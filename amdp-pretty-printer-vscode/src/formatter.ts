import * as cp from 'child_process';
import * as path from 'path';
import * as vscode from 'vscode';

const TIMEOUT_MS = 30_000;

/**
 * Formats the given source text by invoking the AMDP Pretty Printer fat-JAR
 * via stdin/stdout.
 *
 * @param text          The source text to format.
 * @param lbRule        Line-break-after-comma rule (0-4).
 * @param javaPath      Path to the Java executable (e.g. "java").
 * @param jarPath       Absolute path to the fat-JAR.
 * @param trace         Whether to pass --trace to the JAR.
 * @param outputChannel Optional channel to write trace (stderr) output to.
 * @returns             The formatted text.
 */
export function format(
    text: string,
    lbRule: number,
    javaPath: string,
    jarPath: string,
    trace = false,
    outputChannel?: vscode.OutputChannel
): Promise<string> {
    return new Promise((resolve, reject) => {
        const args = ['-jar', jarPath, '--stdin', `--lb-rule=${lbRule}`];
        if (trace) { args.push('--trace'); }
        const proc = cp.spawn(javaPath, args, { stdio: ['pipe', 'pipe', 'pipe'] });

        let stdout = '';
        let stderr = '';
        let settled = false;

        const timer = setTimeout(() => {
            if (!settled) {
                settled = true;
                proc.kill();
                reject(new Error(`AMDP Pretty Printer timed out after ${TIMEOUT_MS / 1000} s.`));
            }
        }, TIMEOUT_MS);

        proc.stdout.setEncoding('utf8');
        proc.stdout.on('data', (chunk: string) => { stdout += chunk; });

        proc.stderr.setEncoding('utf8');
        proc.stderr.on('data', (chunk: string) => { stderr += chunk; });

        proc.on('error', (err) => {
            if (!settled) {
                settled = true;
                clearTimeout(timer);
                const hint = (err as NodeJS.ErrnoException).code === 'ENOENT'
                    ? ` — is '${javaPath}' on your PATH? (Java 17+ required)`
                    : '';
                reject(new Error(`Failed to start Java process: ${err.message}${hint}`));
            }
        });

        proc.on('close', (code) => {
            if (!settled) {
                settled = true;
                clearTimeout(timer);
                if (code === 0) {
                    if (trace && outputChannel && stderr.trim()) {
                        outputChannel.appendLine(stderr.trimEnd());
                        outputChannel.show(true);
                    }
                    resolve(stdout);
                } else {
                    reject(new Error(
                        `AMDP Pretty Printer exited with code ${code}.\n${stderr.trim()}`
                    ));
                }
            }
        });

        proc.stdin.write(text, 'utf8');
        proc.stdin.end();
    });
}

/**
 * Returns the absolute path to the bundled fat-JAR inside the extension.
 */
export function bundledJarPath(extensionPath: string): string {
    return path.join(extensionPath, 'lib', 'amdp-pretty-printer-app.jar');
}
