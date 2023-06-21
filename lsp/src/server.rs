use dashmap::DashMap;
use dp::parsers::Parser;
use dp::Context;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::{
    InitializeParams, InitializeResult, InitializedParams, MessageType, SemanticTokensParams,
    SemanticTokensResult, Url,
};
use tower_lsp::{Client, LanguageServer};

/// Implementation of Language Server with all needed state
#[derive(Debug)]
pub struct Server {
    /// Client to send messages to
    pub client: Client,
    /// Contexts for each file
    pub contexts: DashMap<Url, Context>,
}

impl Server {
    /// Create new [`Server`] for `client`
    pub fn new(client: Client) -> Self {
        Self {
            client,
            contexts: DashMap::new(),
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

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let mut context = self.contexts.entry(params.text_document.uri).or_default();
        let rule = context.find_rule("Root").expect("rule `Root` not found");
        rule.parse("", &mut context).unwrap();
        Ok(None)
    }
}
