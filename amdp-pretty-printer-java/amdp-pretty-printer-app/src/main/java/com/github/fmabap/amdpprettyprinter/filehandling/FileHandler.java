package com.github.fmabap.amdpprettyprinter.filehandling;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Reads and writes source files while preserving the original line endings.
 *
 * <p>
 * Line-ending detection order:
 * <ol>
 * <li>{@code \r\n} (Windows CRLF)</li>
 * <li>{@code \r} (old Mac CR)</li>
 * <li>{@code \n} (Unix LF, default when no line ending is found)</li>
 * </ol>
 * </p>
 */
public final class FileHandler {

    /** Charset used for all file I/O. */
    private static final Charset CHARSET = StandardCharsets.UTF_8;

    private FileHandler() {
        // utility class
    }

    // ---------------------------------------------------------------
    // Public API
    // ---------------------------------------------------------------

    /**
     * Container that holds the lines of a file together with the line-ending
     * string that was detected in the original file content.
     */
    public static final class ReadResult {
        private final List<String> lines;
        private final String lineSeparator;
        private final boolean trailingNewline;

        ReadResult(List<String> lines, String lineSeparator, boolean trailingNewline) {
            this.lines = Collections.unmodifiableList(lines);
            this.lineSeparator = lineSeparator;
            this.trailingNewline = trailingNewline;
        }

        /** Individual lines without any line-ending characters. */
        public List<String> getLines() {
            return lines;
        }

        /**
         * The line-ending sequence detected in the original file
         * ({@code "\r\n"}, {@code "\r"}, or {@code "\n"}).
         */
        public String getLineSeparator() {
            return lineSeparator;
        }

        /**
         * Whether the original file ended with a line-ending character.
         */
        public boolean hasTrailingNewline() {
            return trailingNewline;
        }
    }

    /**
     * Reads {@code file} as UTF-8, detects its line-ending style, and returns
     * the individual lines together with the detected separator.
     *
     * @param file path to the file to read
     * @return {@link ReadResult} containing lines and the original separator
     * @throws IOException if the file cannot be read
     */
    public static ReadResult readFile(Path file) throws IOException {
        byte[] raw = readBytes(file);
        String content = new String(raw, CHARSET);

        String separator = detectSeparator(content);
        boolean trailingNewline = !content.isEmpty() && content.endsWith(separator);
        List<String> lines = splitLines(content, separator);

        return new ReadResult(lines, separator, trailingNewline);
    }

    /**
     * Writes {@code lines} to {@code file} (UTF-8) using {@code lineSeparator}
     * between lines. If the file already exists it is replaced atomically.
     *
     * @param file          destination path
     * @param lines         lines to write (without line-ending characters)
     * @param lineSeparator the line-ending sequence to use
     * @throws IOException if the file cannot be written
     */
    public static void writeLines(Path file, List<String> lines, String lineSeparator)
            throws IOException {
        writeLines(file, lines, lineSeparator, false);
    }

    public static void writeLines(Path file, List<String> lines, String lineSeparator,
            boolean trailingNewline) throws IOException {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < lines.size(); i++) {
            sb.append(lines.get(i));
            if (i < lines.size() - 1) {
                sb.append(lineSeparator);
            }
        }
        if (trailingNewline && lines.size() > 0) {
            sb.append(lineSeparator);
        }
        byte[] bytes = sb.toString().getBytes(CHARSET);
        writeBytes(file, bytes);
    }

    // ---------------------------------------------------------------
    // Private helpers
    // ---------------------------------------------------------------

    private static byte[] readBytes(Path file) throws IOException {
        try (InputStream in = Files.newInputStream(file);
                ByteArrayOutputStream out = new ByteArrayOutputStream()) {
            byte[] buf = new byte[8192];
            int n;
            while ((n = in.read(buf)) != -1) {
                out.write(buf, 0, n);
            }
            return out.toByteArray();
        }
    }

    private static void writeBytes(Path file, byte[] bytes) throws IOException {
        try (OutputStream out = Files.newOutputStream(file)) {
            out.write(bytes);
        }
    }

    /**
     * Detects the dominant line ending in {@code content}.
     * Checks for {@code \r\n} first so that Windows files are not misidentified
     * as old-Mac files.
     */
    private static String detectSeparator(String content) {
        if (content.contains("\r\n")) {
            return "\r\n";
        } else if (content.contains("\r")) {
            return "\r";
        } else {
            return "\n";
        }
    }

    /**
     * Splits {@code content} on the given separator string. A trailing separator
     * does not produce a trailing empty element (mirrors the ABAP SPLIT behaviour).
     */
    private static List<String> splitLines(String content, String separator) {
        List<String> result = new ArrayList<>();
        int start = 0;
        int sepLen = separator.length();
        int idx;

        while ((idx = content.indexOf(separator, start)) != -1) {
            result.add(content.substring(start, idx));
            start = idx + sepLen;
        }

        // remainder after the last separator (or the entire content if no separator)
        String remainder = content.substring(start);
        if (!remainder.isEmpty() || result.isEmpty()) {
            result.add(remainder);
        }

        return result;
    }
}
