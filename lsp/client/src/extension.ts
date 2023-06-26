import {
	workspace,
	ExtensionContext,
} from "vscode";

import {
	Executable,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient;

export const activate = (_context: ExtensionContext) => {
	const run: Executable = {
		command: process.env.SERVER_PATH || "dp-language-server",
		options: {
			env: {
				...process.env,
			},
		},
	}
	const serverOptions: ServerOptions = {
		run,
		debug: run,
	}

	let clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: "file", language: "dp" }],
		synchronize: {
			fileEvents: workspace.createFileSystemWatcher("**/.clientrc"),
		},
	}

	client = new LanguageClient(
		"dp-language-client",
		"DP Language Client",
		serverOptions,
		clientOptions
	)
	client.start()
}

export const deactivate = (): Thenable<void> | undefined => client?.stop()