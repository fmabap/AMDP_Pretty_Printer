package com.github.fmabap.amdpprettyprinter.testcasehelper;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

/**
 * CLI tool that reads a file and prints its content as a Java
 * {@code List<String>} snippet ready to be pasted into a JUnit test case.
 *
 * <p>Usage:
 * <pre>
 *   java -jar amdp-pretty-printer-testcase-helper.jar &lt;file&gt;
 * </pre>
 * </p>
 *
 * <p>Example output:
 * <pre>
 * // @formatter:off
 *         List&lt;String&gt; source = lines(
 *             "METHOD INSERT ...",
 *             "",
 *             "ENDMETHOD."
 *         );
 * // @formatter:on
 * </pre>
 * </p>
 */
public class TestcaseHelper {

    private static final String INDENT = "        ";
    private static final String LINE_INDENT = "            ";

    public static void main(String[] args) throws IOException {
        if (args.length < 1) {
            System.err.println("Usage: testcase-helper <file>");
            System.exit(1);
        }

        List<String> lines = Files.readAllLines(Paths.get(args[0]), StandardCharsets.UTF_8);

        System.out.println("// @formatter:off");
        System.out.println(INDENT + "List<String> source = lines(");

        for (int i = 0; i < lines.size(); i++) {
            String escaped = escapeJavaString(lines.get(i));
            boolean isLast = (i == lines.size() - 1);
            if (isLast) {
                System.out.println(LINE_INDENT + "\"" + escaped + "\"");
            } else {
                System.out.println(LINE_INDENT + "\"" + escaped + "\",");
            }
        }

        System.out.println(INDENT + ");");
        System.out.println("// @formatter:on");
    }

    static String escapeJavaString(String line) {
        return line
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\t", "\\t")
            .replace("\r", "\\r");
    }
}
