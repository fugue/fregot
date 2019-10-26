'use strict';
import {
  ExtensionContext,
  OutputChannel,
  TextDocument,
  window,
  workspace,
} from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  RevealOutputChannelOn,
  ServerOptions,
  TransportKind
} from 'vscode-languageclient';

const clients: Map<string, LanguageClient> = new Map();

export async function activate(context: ExtensionContext) {
  // Register client to check every time a text document gets opened, to
  // support multi-root workspaces.
  workspace.onDidOpenTextDocument(async (document: TextDocument) => await activateFregot(context, document));
  workspace.textDocuments.forEach(async (document: TextDocument) => await activateFregot(context, document));
  // Stop any workspace folders that are removed.
  workspace.onDidChangeWorkspaceFolders(event => {
    for (const folder of event.removed) {
      const client = clients.get(folder.uri.toString());
      if (client) {
        clients.delete(folder.uri.toString());
        client.stop();
      }
    }
  });
}

async function activateFregot(context: ExtensionContext, document: TextDocument) {
  // We are only interested in Rego files.
  if (document.languageId !== 'rego') {
    return;
  }

  const uri = document.uri;
  const folder = workspace.getWorkspaceFolder(uri);
  // Don't handle files outside of a folder.
  if (!folder) {
    return;
  }
  // If the client already has an LSP server, then don't start a new one.
  if (clients.has(folder.uri.toString())) {
    return;
  }

  const runArgs = ['repl'];
  const debugArgs = ['repl'];

  // If the extension is launched in debug mode then the debug server options are used,
  // otherwise the run options are used.
  const serverOptions: ServerOptions = {
    run: { command: 'fregot', transport: TransportKind.stdio, args: runArgs },
    debug: { command: 'fregot', transport: TransportKind.stdio, args: debugArgs }
  };

  // Set a unique name per workspace folder (useful for multi-root workspaces).
  const langName = 'Fregot (' + folder.name + ')';
  const outputChannel: OutputChannel = window.createOutputChannel(langName);
  const clientOptions: LanguageClientOptions = {
    // Use the document selector to only notify the LSP on files inside the folder
    // path for the specific workspace.
    documentSelector: [
      { scheme: 'file', language: 'rego', pattern: `${folder.uri.fsPath}/**/*` },
    ],
    synchronize: {
      // Synchronize the setting section 'languageServerFregot' to the server.
      configurationSection: 'languageServerFregot'
    },
    diagnosticCollectionName: langName,
    revealOutputChannelOn: RevealOutputChannelOn.Never,
    outputChannel,
    outputChannelName: langName,
    // Set the current working directory to be the workspace folder.
    workspaceFolder: folder
  };

  // Create the LSP client.
  const langClient = new LanguageClient(langName, langName, serverOptions, clientOptions, true);

  // Register ClientCapabilities for stuff like window/progress
  langClient.registerProposedFeatures();

  // If the client already has an LSP server, then don't start a new one.
  // We check this again, as there may be multiple parallel requests.
  if (clients.has(folder.uri.toString())) {
    return;
  }

  // Finally start the client and add it to the list of clients.
  langClient.start();
  clients.set(folder.uri.toString(), langClient);
}

/*
 * Deactivate each of the LSP servers.
 */
export function deactivate(): Thenable<void> {
  const promises: Array<Thenable<void>> = [];
  for (const client of clients.values()) {
    promises.push(client.stop());
  }
  return Promise.all(promises).then(() => undefined);
}
