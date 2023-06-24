use dashmap::DashMap;
use dp::parser::{self, Parser};
use dp::Context;
use miette::Diagnostic as MietteDiagnostic;
use ropey::Rope;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    Diagnostic, DidOpenTextDocumentParams, InitializeParams, InitializeResult, InitializedParams,
    MessageType, NumberOrString, Position, Range, SemanticTokensParams, SemanticTokensResult, Url,
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
    pub text: Rope,
    /// Parser context
    pub context: Context,
    /// Parser result
    pub result: parser::Result,
}

impl Server {
    /// Create new [`Server`] for `client`
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents_state: DashMap::new(),
        }
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
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let text = params.text_document.text.as_str();
        let rope = Rope::from_str(text);
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        let mut context = dp::Context::default();
        let rule = context.find_rule("Root").expect("rule `Root` not found");
        let result = rule.parse(text, &mut context);

        let mut diags = Vec::new();
        if let Err(err) = &result {
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
            .publish_diagnostics(uri.clone(), diags, Some(version))
            .await;

        self.documents_state.insert(
            uri,
            DocumentState {
                text: rope,
                context,
                result,
            },
        );
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        Ok(None)
    }
}
