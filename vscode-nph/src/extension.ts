// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as child_process from "child_process";
import * as vscode from "vscode";

function rangeWholeFile(doc: vscode.TextDocument): vscode.Range {
  let lastlinum = doc.lineCount - 1;
  let first = doc.lineAt(0).range.start.character;
  let last = doc.lineAt(lastlinum).range.end.character;
  return new vscode.Range(0, first, lastlinum, last);
}

function getFormattedString(doc: vscode.TextDocument): string {
  const workspaceDir = vscode.workspace.getWorkspaceFolder(doc.uri);

  return child_process
    .execSync("nph -", {
      encoding: "utf-8",
      input: doc.getText(),
      cwd: workspaceDir?.uri.fsPath
    })
    .toString();
}

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: vscode.ExtensionContext) {
  let disposable = vscode.languages.registerDocumentFormattingEditProvider(
    "nim",
    {
      provideDocumentFormattingEdits(
        doc: vscode.TextDocument
      ): vscode.TextEdit[] {
        return [
          vscode.TextEdit.replace(rangeWholeFile(doc), getFormattedString(doc))
        ];
      }
    }
  );
  context.subscriptions.push(disposable);
}

// this method is called when your extension is deactivated
export function deactivate() {}