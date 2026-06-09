package com.github.fmabap.amdpprettyprinter;

import com.github.fmabap.amdpprettyprinter.filehandling.FileHandler;
import com.github.fmabap.amdpprettyprinter.filehandling.FileHandler.ReadResult;
import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.ISettings;
import com.github.fmabap.amdpprettyprinter.prettyprinter.PrettyPrinter;
import com.github.fmabap.amdpprettyprinter.prettyprinter.Settings;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * CLI entry point for the AMDP Pretty Printer.
 *
 * <p>
 * Usage:
 * 
 * <pre>
 *   java -jar amdp-pretty-printer-app.jar &lt;source&gt; [target]
 *       [--pattern=&lt;glob&gt;] [--no-recursive] [--lb-rule=&lt;0-4&gt;]
 * </pre>
 * </p>
 *
 * <ul>
 * <li>{@code source} - path to an AMDP source file or directory (required)</li>
 * <li>{@code target} - path for the formatted output file or directory
 * (optional; defaults to {@code source}, i.e. in-place)</li>
 * <li>{@code --pattern=&lt;glob&gt;} - file-name pattern for directory mode
 * (optional; default is {@code *.abap})</li>
 * <li>{@code --no-recursive} - do not descend into subdirectories (optional;
 * recursive traversal is the default)</li>
 * <li>{@code --trace} - print token/rule trace output to stderr (optional;
 * off by default)</li>
 * <li>{@code --lb-rule=&lt;0-4&gt;} - line-break-after-comma rule (optional;
 * default is {@code 4} = dependent on closing bracket, sub-function, and
 * keyword):
 * <ul>
 * <li>{@code 0} - always insert line break after comma</li>
 * <li>{@code 1} - never insert line break after comma</li>
 * <li>{@code 2} - depends on closing bracket only</li>
 * <li>{@code 3} - depends on closing bracket and sub-function</li>
 * <li>{@code 4} - depends on closing bracket, sub-function, and keyword</li>
 * </ul>
 * </li>
 * </ul>
 *
 * <p>
 * The original line endings of the source file are preserved in the output.
 * </p>
 */
public class App {

    private static final String LB_RULE_FLAG = "--lb-rule=";
    private static final String PATTERN_FLAG = "--pattern=";
    private static final String NO_RECURSIVE_FLAG = "--no-recursive";
    private static final String TRACE_FLAG = "--trace";
    private static final String STDIN_FLAG = "--stdin";
    static final String DEFAULT_PATTERN = "*.abap";

    public static void main(String[] args) {
        try {
            CliArgs cliArgs = parse(args);
            run(cliArgs);
        } catch (CliException e) {
            System.err.println("Error: " + e.getMessage());
            System.err.println();
            System.err.println(
                    "Usage: amdp-pretty-printer-app.jar <source> [target] [--pattern=<glob>] [--no-recursive] [--lb-rule=<0-4>]");
            System.err.println("       amdp-pretty-printer-app.jar --stdin [--lb-rule=<0-4>]");
            System.err.println();
            System.err.println("  source              Path to a source file or directory (required without --stdin)");
            System.err.println("  target              Output file or directory (optional; default = source)");
            System.err
                    .println("  --pattern=<glob>    File-name pattern for directory mode (optional; default = *.abap)");
            System.err.println("  --no-recursive      Do not descend into subdirectories (optional)");
            System.err.println("  --lb-rule=N         Line-break-after-comma rule 0-4 (optional; default = 4)");
            System.err.println("  --trace             Print token/rule trace output to stderr (optional)");
            System.err.println(
                    "  --stdin             Read source from stdin, write formatted output to stdout (optional)");
            System.exit(1);
        } catch (IOException | AppException e) {
            System.err.println("Error: " + e.getMessage());
            System.exit(2);
        }
    }

    // ---------------------------------------------------------------
    // Package-private: reachable from tests
    // ---------------------------------------------------------------

    static void run(CliArgs args) throws IOException, AppException {
        if (args.stdin) {
            runStdin(args);
        } else if (Files.isDirectory(args.source)) {
            runDirectory(args);
        } else {
            runFile(args.source, args.target, args.lbRule, args.trace);
        }
    }

    private static void runStdin(CliArgs args) throws IOException, AppException {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
        List<String> lines = new ArrayList<>();
        String line;
        while ((line = reader.readLine()) != null) {
            lines.add(line);
        }
        ISettings settings = new Settings(args.lbRule, args.trace);
        List<String> formatted = new PrettyPrinter().prettyPrint(lines, settings);
        PrintWriter writer = new PrintWriter(new OutputStreamWriter(System.out, StandardCharsets.UTF_8));
        for (String formattedLine : formatted) {
            writer.println(formattedLine);
        }
        writer.flush();
    }

    private static void runFile(Path source, Path target, String lbRule, boolean trace)
            throws IOException, AppException {
        ReadResult readResult = FileHandler.readFile(source);

        ISettings settings = new Settings(lbRule, trace);
        List<String> formatted = new PrettyPrinter().prettyPrint(readResult.getLines(), settings);

        Path effectiveTarget = target != null ? target : source;
        FileHandler.writeLines(effectiveTarget, formatted, readResult.getLineSeparator(),
                readResult.hasTrailingNewline());

        System.out.println("Formatted: " + source
                + (effectiveTarget.equals(source) ? " (in-place)" : " -> " + effectiveTarget));
    }

    private static void runDirectory(CliArgs args) throws IOException {
        PathMatcher matcher = FileSystems.getDefault().getPathMatcher("glob:" + args.pattern);
        ISettings settings = new Settings(args.lbRule, args.trace);
        PrettyPrinter printer = new PrettyPrinter();

        List<String> errors = new ArrayList<>();
        int count = 0;

        try (Stream<Path> stream = args.recursive
                ? Files.walk(args.source)
                : Files.list(args.source)) {

            List<Path> matchedFiles = stream
                    .filter(Files::isRegularFile)
                    .filter(p -> matcher.matches(p.getFileName()))
                    .collect(Collectors.toList());

            for (Path file : matchedFiles) {
                Path targetFile;
                if (args.target != null) {
                    Path relative = args.source.relativize(file);
                    targetFile = args.target.resolve(relative);
                    Path parentDir = targetFile.getParent();
                    if (parentDir != null) {
                        Files.createDirectories(parentDir);
                    }
                } else {
                    targetFile = file; // in-place
                }

                try {
                    ReadResult readResult = FileHandler.readFile(file);
                    List<String> formatted = printer.prettyPrint(readResult.getLines(), settings);
                    FileHandler.writeLines(targetFile, formatted, readResult.getLineSeparator(),
                            readResult.hasTrailingNewline());
                    System.out.println("Formatted: " + file
                            + (targetFile.equals(file) ? " (in-place)" : " -> " + targetFile));
                    count++;
                } catch (IOException | AppException e) {
                    errors.add(file + ": " + e.getMessage());
                }
            }
        }

        System.out.println("Formatted " + count + " file(s).");
        if (!errors.isEmpty()) {
            System.err.println("Errors (" + errors.size() + "):");
            for (String err : errors) {
                System.err.println("  " + err);
            }
        }
    }

    static CliArgs parse(String[] args) throws CliException {
        if (args == null || args.length == 0) {
            throw new CliException("Missing required argument: <source>");
        }

        Path source = null;
        Path target = null;
        String lbRule = ISettings.LB_RULE_DEP_ON_CLS_BR_SF_AND_KEYWRD;
        String pattern = DEFAULT_PATTERN;
        boolean recursive = true;
        boolean trace = false;
        boolean stdin = false;

        for (String arg : args) {
            if (arg.startsWith(LB_RULE_FLAG)) {
                lbRule = arg.substring(LB_RULE_FLAG.length());
                validateLbRule(lbRule);
            } else if (arg.startsWith(PATTERN_FLAG)) {
                pattern = arg.substring(PATTERN_FLAG.length());
                if (pattern.isEmpty()) {
                    throw new CliException("--pattern value must not be empty.");
                }
            } else if (NO_RECURSIVE_FLAG.equals(arg)) {
                recursive = false;
            } else if (TRACE_FLAG.equals(arg)) {
                trace = true;
            } else if (STDIN_FLAG.equals(arg)) {
                stdin = true;
            } else if (arg.startsWith("-")) {
                throw new CliException("Unknown option: " + arg);
            } else if (source == null) {
                source = Paths.get(arg);
            } else if (target == null) {
                target = Paths.get(arg);
            } else {
                throw new CliException("Unexpected argument: " + arg);
            }
        }

        if (!stdin && source == null) {
            throw new CliException("Missing required argument: <source>");
        }
        if (!stdin) {
            if (!Files.exists(source)) {
                throw new CliException("Source not found: " + source);
            }
            if (Files.isRegularFile(source)) {
                // File mode: target must not be an existing directory
                if (target != null && Files.isDirectory(target)) {
                    throw new CliException(
                            "Source is a file but target is a directory: " + target
                                    + ". Specify a target file path instead.");
                }
                // normalise: if target equals source, treat as in-place
                if (target != null && target.toAbsolutePath().equals(source.toAbsolutePath())) {
                    target = null;
                }
            } else if (!Files.isDirectory(source)) {
                throw new CliException("Source is neither a file nor a directory: " + source);
            }
        }

        return new CliArgs(source, target, lbRule, pattern, recursive, trace, stdin);
    }

    private static void validateLbRule(String value) throws CliException {
        switch (value) {
            case ISettings.LB_RULE_ALWAYS_LINE_BREAK:
            case ISettings.LB_RULE_NO_LINE_BREAK:
            case ISettings.LB_RULE_DEP_ON_CLS_BRACKET_ONLY:
            case ISettings.LB_RULE_DEP_ON_CLS_BRACKET_AND_SUB_FU:
            case ISettings.LB_RULE_DEP_ON_CLS_BR_SF_AND_KEYWRD:
                return;
            default:
                throw new CliException(
                        "Invalid --lb-rule value '" + value + "'. Valid values are 0-4.");
        }
    }

    // ---------------------------------------------------------------
    // Inner types
    // ---------------------------------------------------------------

    static final class CliArgs {
        final Path source;
        final Path target; // null means in-place (overwrite source / source dir)
        final String lbRule;
        final String pattern;
        final boolean recursive;
        final boolean trace;
        final boolean stdin;

        CliArgs(Path source, Path target, String lbRule, String pattern, boolean recursive, boolean trace,
                boolean stdin) {
            this.source = source;
            this.target = target;
            this.lbRule = lbRule;
            this.pattern = pattern;
            this.recursive = recursive;
            this.trace = trace;
            this.stdin = stdin;
        }
    }

    static final class CliException extends Exception {
        CliException(String message) {
            super(message);
        }
    }
}
