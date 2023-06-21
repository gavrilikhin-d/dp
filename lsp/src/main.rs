use dp_language_server::Server;
use tower_lsp::LspService;

#[tokio::main]
async fn main() {
    pretty_env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Server::new(client)).finish();

    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}
