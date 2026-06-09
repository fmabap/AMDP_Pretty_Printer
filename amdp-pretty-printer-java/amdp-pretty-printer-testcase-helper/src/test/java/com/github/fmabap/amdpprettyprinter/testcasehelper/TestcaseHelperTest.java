package com.github.fmabap.amdpprettyprinter.testcasehelper;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class TestcaseHelperTest {

    @Rule
    public TemporaryFolder tmpFolder = new TemporaryFolder();

    private PrintStream originalOut;
    private ByteArrayOutputStream capturedOut;

    @Before
    public void redirectStdout() {
        originalOut = System.out;
        capturedOut = new ByteArrayOutputStream();
        System.setOut(new PrintStream(capturedOut, true, StandardCharsets.UTF_8));
    }

    @After
    public void restoreStdout() {
        System.setOut(originalOut);
    }

    // ------------------------------------------------------------------
    // escapeJavaString – unit tests per special character
    // ------------------------------------------------------------------

    @Test
    public void escape_plainLine() {
        assertEquals("SELECT * FROM sflight;", TestcaseHelper.escapeJavaString("SELECT * FROM sflight;"));
    }

    @Test
    public void escape_emptyString() {
        assertEquals("", TestcaseHelper.escapeJavaString(""));
    }

    @Test
    public void escape_backslash() {
        // Input: path\to\file
        // Output: path\\to\\file
        assertEquals("path\\\\to\\\\file", TestcaseHelper.escapeJavaString("path\\to\\file"));
    }

    @Test
    public void escape_doubleQuote() {
        // Input: say "hello"
        // Output: say \"hello\"
        assertEquals("say \\\"hello\\\"", TestcaseHelper.escapeJavaString("say \"hello\""));
    }

    @Test
    public void escape_tab() {
        // Input: col1<TAB>col2
        // Output: col1\tcol2
        assertEquals("col1\\tcol2", TestcaseHelper.escapeJavaString("col1\tcol2"));
    }

    @Test
    public void escape_carriageReturn() {
        // Input: line<CR>
        // Output: line\r
        assertEquals("line\\r", TestcaseHelper.escapeJavaString("line\r"));
    }

    @Test
    public void escape_backslashEscapedFirst() {
        // A backslash followed by a double-quote must not produce \\" but \\\"
        // Input: C:\"folder"
        // Output: C:\\\"folder\"
        assertEquals("C:\\\\\\\"folder\\\"", TestcaseHelper.escapeJavaString("C:\\\"folder\""));
    }

    @Test
    public void escape_combined_abapSqlLine() {
        // Typical ABAP line: delete from "BLA" where path = 'C:\tmp'
        // Expected: delete from \"BLA\" where path = 'C:\\tmp'
        assertEquals(
                "delete from \\\"BLA\\\" where path = 'C:\\\\tmp'",
                TestcaseHelper.escapeJavaString("delete from \"BLA\" where path = 'C:\\tmp'"));
    }

    @Test
    public void escape_multipleTabsAndQuotes() {
        // Input: "col1"<TAB>"col2"<TAB>"col3"
        // Output: \"col1\"\t\"col2\"\t\"col3\"
        assertEquals(
                "\\\"col1\\\"\\t\\\"col2\\\"\\t\\\"col3\\\"",
                TestcaseHelper.escapeJavaString("\"col1\"\t\"col2\"\t\"col3\""));
    }

    @Test
    public void escape_allSpecialsTogether() {
        // Input: "C:\path\to\file"<TAB><CR>
        // Escaping order: \ first, then "
        // Output: \"C:\\path\\to\\file\"\t\r
        assertEquals(
                "\\\"C:\\\\path\\\\to\\\\file\\\"\\t\\r",
                TestcaseHelper.escapeJavaString("\"C:\\path\\to\\file\"\t\r"));
    }

    // ------------------------------------------------------------------
    // Full output structure tests (writing a temp file, calling main)
    // ------------------------------------------------------------------

    /**
     * Writes fileLines to a temp file, calls main, and returns the captured
     * stdout split into lines (OS-agnostic: splits on \n, strips trailing \r).
     */
    private List<String> runMain(String... fileLines) throws IOException {
        Path file = tmpFolder.newFile().toPath();
        Files.write(file, Arrays.asList(fileLines), StandardCharsets.UTF_8);
        TestcaseHelper.main(new String[] { file.toString() });
        String output = capturedOut.toString(StandardCharsets.UTF_8.name());
        return Arrays.stream(output.split("\n", -1))
                .map(s -> s.endsWith("\r") ? s.substring(0, s.length() - 1) : s)
                .collect(Collectors.toList());
    }

    @Test
    public void output_singleLine_structure() throws IOException {
        List<String> out = runMain("ENDMETHOD.");

        assertEquals("// @formatter:off", out.get(0));
        assertEquals("        List<String> source = lines(", out.get(1));
        assertEquals("            \"ENDMETHOD.\"", out.get(2)); // last line: no trailing comma
        assertEquals("        );", out.get(3));
        assertEquals("// @formatter:on", out.get(4));
    }

    @Test
    public void output_multipleLines_commaOnAllButLast() throws IOException {
        List<String> out = runMain("LINE ONE", "LINE TWO", "LINE THREE");

        assertEquals("            \"LINE ONE\",", out.get(2));
        assertEquals("            \"LINE TWO\",", out.get(3));
        assertEquals("            \"LINE THREE\"", out.get(4)); // no comma
        assertEquals("        );", out.get(5));
    }

    @Test
    public void output_emptyLineInMiddle() throws IOException {
        List<String> out = runMain("FIRST", "", "LAST");

        assertEquals("            \"FIRST\",", out.get(2));
        assertEquals("            \"\",", out.get(3));
        assertEquals("            \"LAST\"", out.get(4));
    }

    @Test
    public void output_escapesDoubleQuotes() throws IOException {
        // Input file line: select "CARRID" from sflight;
        // Expected in output: "select \"CARRID\" from sflight;",
        List<String> out = runMain("select \"CARRID\" from sflight;");

        assertEquals("            \"select \\\"CARRID\\\" from sflight;\"", out.get(2));
    }

    @Test
    public void output_escapesBackslash() throws IOException {
        // Input file line: path\to\file
        // Expected in output: "path\\to\\file"
        List<String> out = runMain("path\\to\\file");

        assertEquals("            \"path\\\\to\\\\file\"", out.get(2));
    }

    @Test
    public void output_escapesTab() throws IOException {
        // Input file line: col1<TAB>col2
        // Expected in output: "col1\tcol2"
        List<String> out = runMain("col1\tcol2");

        assertEquals("            \"col1\\tcol2\"", out.get(2));
    }

    @Test
    public void output_combined_backslashAndQuote() throws IOException {
        // Simulates: delete from "BLA" where path = 'C:\tmp'
        // First line gets a comma (not last), second line has no comma.
        List<String> out = runMain(
                "delete from \"BLA\" where path = 'C:\\tmp'",
                "ENDMETHOD.");

        assertEquals(
                "            \"delete from \\\"BLA\\\" where path = 'C:\\\\tmp'\",",
                out.get(2));
        assertEquals("            \"ENDMETHOD.\"", out.get(3));
    }

    @Test
    public void output_combined_allSpecials() throws IOException {
        // Line with tab, backslash, and double-quote together
        List<String> out = runMain("\"C:\\path\"\t<value>");

        assertEquals("            \"\\\"C:\\\\path\\\"\\t<value>\"", out.get(2));
    }
}
