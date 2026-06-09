import * as vscode from 'vscode';
import * as fs from 'fs';
import { format, bundledJarPath } from './formatter';

export function activate(context: vscode.ExtensionContext): void {
    const traceChannel = vscode.window.createOutputChannel('AMDP Pretty Printer');
    context.subscriptions.push(traceChannel);

    // Register as a document formatter for .abap files on disk.
    const formatterProvider: vscode.DocumentFormattingEditProvider = {
        provideDocumentFormattingEdits(
            document: vscode.TextDocument
        ): Promise<vscode.TextEdit[]> {
            return runFormat(document, context.extensionPath, traceChannel);
        }
    };

    context.subscriptions.push(
        vscode.languages.registerDocumentFormattingEditProvider(
            { scheme: 'file', pattern: '**/*.abap' },
            formatterProvider
        )
    );

    // Register the explicit command (Command Palette + Ctrl+0 keybinding).
    context.subscriptions.push(
        vscode.commands.registerCommand('amdp-pretty-printer.formatDocument', async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor) {
                vscode.window.showWarningMessage('AMDP Pretty Printer: No active editor.');
                return;
            }
            const edits = await runFormat(editor.document, context.extensionPath, traceChannel).catch(
                (err: Error) => {
                    vscode.window.showErrorMessage(`AMDP Pretty Printer: ${err.message}`);
                    return [];
                }
            );
            if (edits.length > 0) {
                const wsEdit = new vscode.WorkspaceEdit();
                wsEdit.set(editor.document.uri, edits);
                await vscode.workspace.applyEdit(wsEdit);
            }
        })
    );
}

export function deactivate(): void {
    // nothing to clean up
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

async function runFormat(
    document: vscode.TextDocument,
    extensionPath: string,
    traceChannel: vscode.OutputChannel
): Promise<vscode.TextEdit[]> {
    const config = vscode.workspace.getConfiguration('amdp-pretty-printer');
    const lbRule: number = config.get<number>('lbRule', 4);
    const javaPath: string = config.get<string>('javaPath', 'java');
    const trace: boolean = config.get<boolean>('trace', false);
    const jarPath = bundledJarPath(extensionPath);

    if (!fs.existsSync(jarPath)) {
        throw new Error(
            `Bundled JAR not found at '${jarPath}'.\n` +
            `Build the project first (mvn package in amdp-pretty-printer-java/).`
        );
    }

    const originalText = document.getText();
    const formatted = await format(originalText, lbRule, javaPath, jarPath, trace, traceChannel);

    if (formatted === originalText) {
        return [];
    }

    const fullRange = new vscode.Range(
        document.positionAt(0),
        document.positionAt(originalText.length)
    );
    return [vscode.TextEdit.replace(fullRange, formatted)];
}
