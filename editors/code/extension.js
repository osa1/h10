const vscode = require('vscode');
const node = require("vscode-languageclient/node");

let client;

function activate(context) {
    console.log('Activating h10 language server');
    const command = '/home/omer/rust/h10/target/debug/h10_language_server';
    const options = { env: { "RUST_BACKTRACE": "1" } };
    const run = {
        command,
        options,
    };
    const serverOptions = {
        run,
        debug: run,
    };
    const clientOptions = {
        documentSelector: [{ scheme: 'file', language: 'h10' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
        }
    };
    client = new node.LanguageClient('h10LanguageServer', 'H10 Language Server', serverOptions, clientOptions);
    client.start();
}

function deactivate() {
    console.log('Deactivating h10 language server');
    if (!client) {
        return undefined;
    }
    return client.stop();
}

module.exports = {
    activate,
    deactivate
}
