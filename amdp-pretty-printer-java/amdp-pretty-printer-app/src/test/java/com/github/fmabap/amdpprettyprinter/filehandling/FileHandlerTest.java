package com.github.fmabap.amdpprettyprinter.filehandling;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Unit tests for {@link FileHandler} – focuses on trailing-newline
 * preservation.
 */
public class FileHandlerTest {

    @Rule
    public TemporaryFolder tmp = new TemporaryFolder();

    // ------------------------------------------------------------------
    // readFile – trailing-newline detection
    // ------------------------------------------------------------------

    @Test
    public void readFile_singleLine_withTrailingLf_detectsTrailingNewline() throws Exception {
        Path file = write("only-line\n");
        FileHandler.ReadResult result = FileHandler.readFile(file);
        assertTrue("single-line file with trailing LF must be detected", result.hasTrailingNewline());
    }

    @Test
    public void readFile_singleLine_withoutTrailingLf_noTrailingNewline() throws Exception {
        Path file = write("only-line");
        FileHandler.ReadResult result = FileHandler.readFile(file);
        assertFalse("single-line file without trailing LF must not be flagged", result.hasTrailingNewline());
    }

    @Test
    public void readFile_emptyFile_noTrailingNewline() throws Exception {
        Path file = write("");
        FileHandler.ReadResult result = FileHandler.readFile(file);
        assertFalse("empty file must not be flagged as having a trailing newline", result.hasTrailingNewline());
    }

    @Test
    public void readFile_onlyNewline_detectsTrailingNewline() throws Exception {
        Path file = write("\n");
        FileHandler.ReadResult result = FileHandler.readFile(file);
        assertTrue("file containing only LF must be detected as having a trailing newline",
                result.hasTrailingNewline());
    }

    @Test
    public void readFile_withTrailingCr_detectsTrailingNewline() throws Exception {
        Path file = writeBytes("line1\rline2\r".getBytes(StandardCharsets.UTF_8));
        FileHandler.ReadResult result = FileHandler.readFile(file);
        assertTrue("trailing CR (old Mac) must be detected", result.hasTrailingNewline());
    }

    @Test
    public void readFile_withoutTrailingCr_noTrailingNewline() throws Exception {
        Path file = writeBytes("line1\rline2".getBytes(StandardCharsets.UTF_8));
        FileHandler.ReadResult result = FileHandler.readFile(file);
        assertFalse("CR file without trailing newline must not be flagged", result.hasTrailingNewline());
    }

    @Test
    public void readFile_withTrailingLf_detectsTrailingNewline() throws Exception {
        Path file = write("line1\nline2\n");
        FileHandler.ReadResult result = FileHandler.readFile(file);
        assertTrue("trailing LF must be detected", result.hasTrailingNewline());
    }

    @Test
    public void readFile_withoutTrailingLf_noTrailingNewline() throws Exception {
        Path file = write("line1\nline2");
        FileHandler.ReadResult result = FileHandler.readFile(file);
        assertFalse("no trailing newline expected", result.hasTrailingNewline());
    }

    @Test
    public void readFile_withTrailingCrLf_detectsTrailingNewline() throws Exception {
        Path file = writeBytes("line1\r\nline2\r\n".getBytes(StandardCharsets.UTF_8));
        FileHandler.ReadResult result = FileHandler.readFile(file);
        assertTrue("trailing CRLF must be detected", result.hasTrailingNewline());
    }

    @Test
    public void readFile_withoutTrailingCrLf_noTrailingNewline() throws Exception {
        Path file = writeBytes("line1\r\nline2".getBytes(StandardCharsets.UTF_8));
        FileHandler.ReadResult result = FileHandler.readFile(file);
        assertFalse("no trailing newline expected", result.hasTrailingNewline());
    }

    // ------------------------------------------------------------------
    // writeLines – trailing-newline round-trip
    // ------------------------------------------------------------------

    @Test
    public void writeLines_singleLine_withTrailingNewline_appendsSeparator() throws Exception {
        Path file = write("only-line\n");
        FileHandler.ReadResult result = FileHandler.readFile(file);

        Path out = tmp.newFile("out_single.abap").toPath();
        FileHandler.writeLines(out, result.getLines(), result.getLineSeparator(),
                result.hasTrailingNewline());

        byte[] written = Files.readAllBytes(out);
        byte[] expected = "only-line\n".getBytes(StandardCharsets.UTF_8);
        assertArrayEquals("single-line file with trailing LF must be preserved", expected, written);
    }

    @Test
    public void writeLines_multipleTrailingNewlines_preserved() throws Exception {
        // "line1\n\n" → lines=["line1",""], trailingNewline=true
        // re-joining gives "line1\n" + trailing "\n" = "line1\n\n"
        String content = "line1\n\n";
        Path file = write(content);
        FileHandler.ReadResult result = FileHandler.readFile(file);

        Path out = tmp.newFile("out_multi_trail.abap").toPath();
        FileHandler.writeLines(out, result.getLines(), result.getLineSeparator(),
                result.hasTrailingNewline());

        byte[] written = Files.readAllBytes(out);
        byte[] expected = content.getBytes(StandardCharsets.UTF_8);
        assertArrayEquals("double trailing newline must be preserved", expected, written);
    }

    @Test
    public void writeLines_withTrailingNewline_appendsSeparator() throws Exception {
        Path file = write("line1\nline2\n");
        FileHandler.ReadResult result = FileHandler.readFile(file);

        Path out = tmp.newFile("out.abap").toPath();
        FileHandler.writeLines(out, result.getLines(), result.getLineSeparator(),
                result.hasTrailingNewline());

        byte[] written = Files.readAllBytes(out);
        byte[] expected = "line1\nline2\n".getBytes(StandardCharsets.UTF_8);
        assertArrayEquals("file with trailing LF must be preserved byte-for-byte", expected, written);
    }

    @Test
    public void writeLines_withoutTrailingNewline_noExtraSeparator() throws Exception {
        Path file = write("line1\nline2");
        FileHandler.ReadResult result = FileHandler.readFile(file);

        Path out = tmp.newFile("out2.abap").toPath();
        FileHandler.writeLines(out, result.getLines(), result.getLineSeparator(),
                result.hasTrailingNewline());

        byte[] written = Files.readAllBytes(out);
        byte[] expected = "line1\nline2".getBytes(StandardCharsets.UTF_8);
        assertArrayEquals("file without trailing newline must not gain one", expected, written);
    }

    @Test
    public void writeLines_crLf_withTrailingNewline_roundTrip() throws Exception {
        String content = "line1\r\nline2\r\n";
        Path file = writeBytes(content.getBytes(StandardCharsets.UTF_8));
        FileHandler.ReadResult result = FileHandler.readFile(file);

        Path out = tmp.newFile("out3.abap").toPath();
        FileHandler.writeLines(out, result.getLines(), result.getLineSeparator(),
                result.hasTrailingNewline());

        byte[] written = Files.readAllBytes(out);
        byte[] expected = content.getBytes(StandardCharsets.UTF_8);
        assertArrayEquals("CRLF file with trailing newline must be preserved", expected, written);
    }

    @Test
    public void writeLines_legacyOverload_noTrailingNewline() throws Exception {
        List<String> lines = List.of("line1", "line2");
        Path out = tmp.newFile("out4.abap").toPath();
        FileHandler.writeLines(out, lines, "\n");

        byte[] written = Files.readAllBytes(out);
        byte[] expected = "line1\nline2".getBytes(StandardCharsets.UTF_8);
        assertArrayEquals("legacy overload must not append trailing newline", expected, written);
    }

    // ------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------

    private Path write(String content) throws Exception {
        return writeBytes(content.getBytes(StandardCharsets.UTF_8));
    }

    private Path writeBytes(byte[] bytes) throws Exception {
        Path file = tmp.newFile().toPath();
        Files.write(file, bytes);
        return file;
    }
}
