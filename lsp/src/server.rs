use dashmap::DashMap;
use dp::parser::{ParseResult, Parser};
use dp::Context;
use miette::Diagnostic as MietteDiagnostic;
use ropey::{LineColumn, Rope};
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    Diagnostic, DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeParams,
    InitializeResult, InitializedParams, MessageType, Position, Range, SemanticToken,
    SemanticTokens, SemanticTokensParams, SemanticTokensResult, Url,
};
use tower_lsp::{Client, LanguageServer};

/// Implementation of Language Server with all needed state
#[derive(Debug)]
pub struct Server {
    /// Client to send messages to
    pub client: Client,
    /// Map of documents and their states
    pub documents_state: DashMap<Url, DocumentState>,
}

/// State for single document
#[derive(Debug)]
pub struct DocumentState {
    /// Text of the document
    pub rope: Rope,
    /// Parser context
    pub context: Context,
    /// Parser result
    pub result: Option<ParseResult>,
}

impl Server {
    /// Create new [`Server`] for `client`
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents_state: DashMap::new(),
        }
    }

    /// Handle change of the document
    pub async fn run_analysis_at(&self, uri: Url, at: usize) {
        self.client
            .log_message(
                MessageType::INFO,
                format!(
                    "running analysis for {}",
                    uri.to_file_path().unwrap().display()
                ),
            )
            .await;

        let mut state = self.documents_state.get_mut(&uri).unwrap();
        let rope = state.rope.clone();

        let rule = state.context.find_rule("Root").unwrap();
        let result = rule.parse_at(rope.to_string().as_str(), at, &mut state.context);
        let errors = result.syntax.errors().collect::<Vec<_>>();

        let mut diags = Vec::new();
        for err in errors {
            if let Some(labels) = err.labels() {
                labels.for_each(|label| {
                    let offset = label.offset();
                    let start = rope.char_to_line_column(offset);
                    let end = rope.char_to_line_column(offset + label.len());
                    let diag = Diagnostic::new_simple(
                        Range::new(
                            Position::new(
                                start.line.try_into().unwrap(),
                                start.column.try_into().unwrap(),
                            ),
                            Position::new(
                                end.line.try_into().unwrap(),
                                end.column.try_into().unwrap(),
                            ),
                        ),
                        format!("{}", label.label().unwrap_or("")),
                    );
                    diags.push(diag);
                });
            }
        }
        self.client
            .publish_diagnostics(uri.clone(), diags, None)
            .await;

        state.result = Some(result);
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Server {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        let config = include_str!("../server.config.json");

        self.client
            .log_message(
                MessageType::INFO,
                format!("server is configured with:\n{config}"),
            )
            .await;

        Ok(serde_json::from_str(&config).unwrap())
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server is initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        self.client
            .log_message(MessageType::INFO, format!("server shutdown",))
            .await;
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let document = &params.text_document;
        let uri = document.uri.clone();

        self.client
            .log_message(
                MessageType::INFO,
                format!("open {}", uri.to_file_path().unwrap().display()),
            )
            .await;

        self.documents_state.insert(
            uri.clone(),
            DocumentState {
                rope: document.text.as_str().into(),
                context: Context::default(),
                result: None,
            },
        );

        self.run_analysis_at(uri, 0).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;

        self.client
            .log_message(
                MessageType::INFO,
                format!("{} has changed", uri.to_file_path().unwrap().display()),
            )
            .await;

        let mut state = self.documents_state.get_mut(&uri).unwrap();
        for change in params.content_changes {
            if let Some(range) = change.range {
                let start = state.rope.line_column_to_char(LineColumn {
                    line: range.start.line as usize,
                    column: range.start.character as usize,
                });
                let end = start + change.range_length.unwrap() as usize;
                state.rope.remove(start..end);
                state.rope.insert(start, &change.text);
            } else {
                state.rope = change.text.into()
            }
        }

        state.context = Context::default();
        // Drop state to avoid deadlock
        drop(state);

        self.run_analysis_at(uri, 0).await;
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;

        let state = self.documents_state.get(&uri).unwrap();
        if state.result.is_none() {
            return Ok(None);
        }
        let rope = state.rope.clone();

        let result = state.result.as_ref().unwrap();
        let mut tokens = Vec::new();
        let mut prev_range = Range::new(Position::new(0, 0), Position::new(0, 0));
        for token in result.syntax.tokens() {
            let range = rope.lsp_range(token.range.clone());
            tokens.push(SemanticToken {
                delta_line: range.start.line - prev_range.start.line,
                delta_start: if range.start.line == prev_range.start.line {
                    range.start.character - prev_range.start.character
                } else {
                    range.start.character
                },
                length: token.range.len() as u32,
                token_type: token.kind.clone() as u32,
                token_modifiers_bitset: 0,
            });
            prev_range = range;
        }
        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }
}

trait LSPPosition {
    /// Convert char index to LSP position
    fn lsp_position(&self, char_index: usize) -> Position;
    /// Convert char range to LSP range
    fn lsp_range(&self, char_range: impl Into<std::ops::Range<usize>>) -> Range {
        let char_range = char_range.into();
        let start = self.lsp_position(char_range.start);
        let end = self.lsp_position(char_range.end);
        Range::new(start, end)
    }
}

impl LSPPosition for Rope {
    fn lsp_position(&self, char_index: usize) -> Position {
        let LineColumn { line, column } = self.char_to_line_column(char_index);
        Position::new(line as u32, column as u32)
    }
}
