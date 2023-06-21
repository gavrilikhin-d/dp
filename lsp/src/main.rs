use dashmap::DashMap;
use dp_language_server::backend::Backend;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    pretty_env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        ast_map: DashMap::new(),
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;
}
