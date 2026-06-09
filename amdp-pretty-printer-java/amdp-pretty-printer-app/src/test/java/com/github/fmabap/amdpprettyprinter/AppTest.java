package com.github.fmabap.amdpprettyprinter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * Unit tests for {@link App} - CLI argument parsing and directory-mode
 * processing.
 */
public class AppTest {

    @Rule
    public TemporaryFolder tmp = new TemporaryFolder();

    // ------------------------------------------------------------------
    // parse() - file mode
    // ------------------------------------------------------------------

    @Test
    public void parse_fileMode_defaults() throws Exception {
        Path file = newAbapFile(tmp.getRoot().toPath(), "zcl_demo.abap", "");
        App.CliArgs args = App.parse(new String[] { file.toString() });

        assertEquals(file.toAbsolutePath(), args.source.toAbsolutePath());
        assertNull("target should be null (in-place)", args.target);
        assertEquals(App.DEFAULT_PATTERN, args.pattern);
        assertTrue("recursive should default to true", args.recursive);
    }

    @Test
    public void parse_fileMode_withTarget() throws Exception {
        Path src = newAbapFile(tmp.getRoot().toPath(), "src.abap", "");
        Path tgt = tmp.getRoot().toPath().resolve("tgt.abap");
        App.CliArgs args = App.parse(new String[] { src.toString(), tgt.toString() });

        assertEquals(tgt.toAbsolutePath(), args.target.toAbsolutePath());
    }

    @Test(expected = App.CliException.class)
    public void parse_fileSrcDirTarget_throws() throws Exception {
        Path src = newAbapFile(tmp.getRoot().toPath(), "src.abap", "");
        Path dir = tmp.newFolder("out").toPath();
        App.parse(new String[] { src.toString(), dir.toString() });
    }

    // ------------------------------------------------------------------
    // parse() - directory mode
    // ------------------------------------------------------------------

    @Test
    public void parse_dirMode_defaults() throws Exception {
        Path dir = tmp.newFolder("src").toPath();
        App.CliArgs args = App.parse(new String[] { dir.toString() });

        assertEquals(dir.toAbsolutePath(), args.source.toAbsolutePath());
        assertNull("target should be null (in-place)", args.target);
        assertEquals(App.DEFAULT_PATTERN, args.pattern);
        assertTrue("recursive should default to true", args.recursive);
    }

    // ------------------------------------------------------------------
    // parse() - flags
    // ------------------------------------------------------------------

    @Test
    public void parse_patternFlag() throws Exception {
        Path dir = tmp.newFolder("src2").toPath();
        App.CliArgs args = App.parse(new String[] { dir.toString(), "--pattern=*.txt" });

        assertEquals("*.txt", args.pattern);
    }

    @Test
    public void parse_noRecursiveFlag() throws Exception {
        Path dir = tmp.newFolder("src3").toPath();
        App.CliArgs args = App.parse(new String[] { dir.toString(), "--no-recursive" });

        assertFalse("recursive should be false", args.recursive);
    }

    @Test
    public void parse_lbRuleFlag() throws Exception {
        Path file = newAbapFile(tmp.getRoot().toPath(), "lb.abap", "");
        App.CliArgs args = App.parse(new String[] { file.toString(), "--lb-rule=1" });

        assertEquals("1", args.lbRule);
    }

    @Test(expected = App.CliException.class)
    public void parse_emptyPattern_throws() throws Exception {
        Path dir = tmp.newFolder("src4").toPath();
        App.parse(new String[] { dir.toString(), "--pattern=" });
    }

    @Test(expected = App.CliException.class)
    public void parse_unknownFlag_throws() throws Exception {
        Path file = newAbapFile(tmp.getRoot().toPath(), "unk.abap", "");
        App.parse(new String[] { file.toString(), "--unknown" });
    }

    @Test(expected = App.CliException.class)
    public void parse_missingSource_throws() throws Exception {
        App.parse(new String[] {});
    }

    @Test(expected = App.CliException.class)
    public void parse_sourceNotFound_throws() throws Exception {
        App.parse(new String[] { "/nonexistent/path/to/file.abap" });
    }

    @Test(expected = App.CliException.class)
    public void parse_invalidLbRule_throws() throws Exception {
        Path file = newAbapFile(tmp.getRoot().toPath(), "bad.abap", "");
        App.parse(new String[] { file.toString(), "--lb-rule=9" });
    }

    // ------------------------------------------------------------------
    // run() - directory mode, in-place
    // ------------------------------------------------------------------

    @Test
    public void run_dirMode_inPlace_formatsOnlyMatchingFiles() throws Exception {
        Path srcDir = tmp.newFolder("inplace").toPath();
        Path abap1 = newAbapFile(srcDir, "a.abap", "SELECT * FROM dummy;\n");
        Path abap2 = newAbapFile(srcDir, "b.abap", "SELECT * FROM dummy;\n");
        Path txt = newAbapFile(srcDir, "c.txt", "SELECT * FROM dummy;\n");

        long abap1Before = Files.getLastModifiedTime(abap1).toMillis();
        long txtBefore = Files.getLastModifiedTime(txt).toMillis();

        // Small sleep so that modification timestamps can differ on fast machines
        Thread.sleep(50);

        App.CliArgs args = new App.CliArgs(srcDir, null, "0", "*.abap", true, false, false);
        App.run(args);

        // Both .abap files must have been touched (re-written)
        assertTrue("a.abap should be rewritten", Files.getLastModifiedTime(abap1).toMillis() >= abap1Before);
        assertTrue("b.abap should be rewritten", Files.getLastModifiedTime(abap2).toMillis() >= abap1Before);
        // .txt file must remain untouched
        assertEquals("c.txt must not be modified",
                txtBefore, Files.getLastModifiedTime(txt).toMillis());
    }

    @Test
    public void run_dirMode_toTargetDir_mirrorsRelativePaths() throws Exception {
        Path srcDir = tmp.newFolder("src_mirror").toPath();
        Path tgtDir = tmp.newFolder("tgt_mirror").toPath();
        newAbapFile(srcDir, "x.abap", "SELECT * FROM dummy;\n");

        App.CliArgs args = new App.CliArgs(srcDir, tgtDir, "0", "*.abap", true, false, false);
        App.run(args);

        assertTrue("x.abap should exist in target dir",
                Files.exists(tgtDir.resolve("x.abap")));
    }

    @Test
    public void run_dirMode_recursive_processesSubfolderFiles() throws Exception {
        Path srcDir = tmp.newFolder("rec_src").toPath();
        Path subDir = Files.createDirectories(srcDir.resolve("sub"));
        newAbapFile(subDir, "nested.abap", "SELECT * FROM dummy;\n");

        Path tgtDir = tmp.newFolder("rec_tgt").toPath();
        App.CliArgs args = new App.CliArgs(srcDir, tgtDir, "0", "*.abap", true, false, false);
        App.run(args);

        assertTrue("nested.abap should be copied into mirrored subdir",
                Files.exists(tgtDir.resolve("sub").resolve("nested.abap")));
    }

    @Test
    public void run_dirMode_noRecursive_ignoresSubfolderFiles() throws Exception {
        Path srcDir = tmp.newFolder("norec_src").toPath();
        Path subDir = Files.createDirectories(srcDir.resolve("child"));
        newAbapFile(srcDir, "top.abap", "SELECT * FROM dummy;\n");
        newAbapFile(subDir, "child.abap", "SELECT * FROM dummy;\n");

        Path tgtDir = tmp.newFolder("norec_tgt").toPath();
        App.CliArgs args = new App.CliArgs(srcDir, tgtDir, "0", "*.abap", false, false, false);
        App.run(args);

        assertTrue("top-level file should be formatted",
                Files.exists(tgtDir.resolve("top.abap")));
        assertFalse("child.abap must NOT be copied when non-recursive",
                Files.exists(tgtDir.resolve("child").resolve("child.abap")));
    }

    @Test
    public void run_dirMode_customPattern_matchesOnlyThatPattern() throws Exception {
        Path srcDir = tmp.newFolder("pat_src").toPath();
        newAbapFile(srcDir, "a.abap", "SELECT * FROM dummy;\n");
        newAbapFile(srcDir, "b.txt", "SELECT * FROM dummy;\n");

        Path tgtDir = tmp.newFolder("pat_tgt").toPath();
        App.CliArgs args = new App.CliArgs(srcDir, tgtDir, "0", "*.txt", true, false, false);
        App.run(args);

        assertFalse("a.abap should NOT be formatted with *.txt pattern",
                Files.exists(tgtDir.resolve("a.abap")));
        assertTrue("b.txt should be formatted",
                Files.exists(tgtDir.resolve("b.txt")));
    }

    @Test
    public void run_dirMode_emptyDir_formatsZeroFiles() throws Exception {
        Path srcDir = tmp.newFolder("empty_src").toPath();
        Path tgtDir = tmp.newFolder("empty_tgt").toPath();
        App.CliArgs args = new App.CliArgs(srcDir, tgtDir, "0", "*.abap", true, false, false);
        App.run(args); // must not throw
        assertEquals("target dir should stay empty", 0,
                Files.list(tgtDir).count());
    }

    // ------------------------------------------------------------------
    // run() - single-file mode (backward compat)
    // ------------------------------------------------------------------

    @Test
    public void run_fileMode_inPlace() throws Exception {
        Path file = newAbapFile(tmp.getRoot().toPath(), "f.abap",
                "SELECT * FROM dummy;\n");
        App.CliArgs args = new App.CliArgs(file, null, "0", App.DEFAULT_PATTERN, true, false, false);
        App.run(args); // must not throw; file is written in-place
        assertTrue(Files.exists(file));
    }

    // ------------------------------------------------------------------
    // run() - trailing-newline preservation (integration)
    // ------------------------------------------------------------------

    @Test
    public void run_fileMode_inPlace_preservesTrailingNewline() throws Exception {
        String content = "SELECT * FROM dummy;\n";
        Path file = newAbapFile(tmp.getRoot().toPath(), "trail_lf.abap", content);
        App.CliArgs args = new App.CliArgs(file, null, "0", App.DEFAULT_PATTERN, true, false, false);
        App.run(args);

        byte[] written = Files.readAllBytes(file);
        byte lastByte = written[written.length - 1];
        assertEquals("trailing LF must be preserved after App.run", (byte) '\n', lastByte);
    }

    @Test
    public void run_fileMode_inPlace_noTrailingNewline_notAdded() throws Exception {
        String content = "SELECT * FROM dummy;";
        Path file = newAbapFile(tmp.getRoot().toPath(), "no_trail.abap", content);
        App.CliArgs args = new App.CliArgs(file, null, "0", App.DEFAULT_PATTERN, true, false, false);
        App.run(args);

        byte[] written = Files.readAllBytes(file);
        byte lastByte = written[written.length - 1];
        assertEquals("no trailing newline must not be added by App.run",
                (byte) ';', lastByte);
    }

    @Test
    public void run_dirMode_inPlace_preservesTrailingNewline() throws Exception {
        Path srcDir = tmp.newFolder("trail_dir").toPath();
        String content = "SELECT * FROM dummy;\n";
        Path file = newAbapFile(srcDir, "trail.abap", content);

        App.CliArgs args = new App.CliArgs(srcDir, null, "0", "*.abap", true, false, false);
        App.run(args);

        byte[] written = Files.readAllBytes(file);
        byte lastByte = written[written.length - 1];
        assertEquals("trailing LF must be preserved in directory mode", (byte) '\n', lastByte);
    }

    // ------------------------------------------------------------------
    // parse() - --trace flag
    // ------------------------------------------------------------------

    @Test
    public void parse_traceFlag_setsTraceTrue() throws Exception {
        Path file = newAbapFile(tmp.getRoot().toPath(), "trace.abap", "");
        App.CliArgs args = App.parse(new String[] { file.toString(), "--trace" });

        assertTrue("trace should be true when --trace is passed", args.trace);
    }

    @Test
    public void parse_noTraceFlag_defaultsFalse() throws Exception {
        Path file = newAbapFile(tmp.getRoot().toPath(), "notrace.abap", "");
        App.CliArgs args = App.parse(new String[] { file.toString() });

        assertFalse("trace should default to false", args.trace);
    }

    @Test
    public void parse_traceWithOtherFlags_allParsed() throws Exception {
        Path file = newAbapFile(tmp.getRoot().toPath(), "trace2.abap", "");
        App.CliArgs args = App.parse(new String[] { file.toString(), "--lb-rule=1", "--trace" });

        assertTrue("trace should be true", args.trace);
        assertEquals("lb-rule should be 1", "1", args.lbRule);
    }

    @Test
    public void run_fileMode_withTrace_doesNotThrow() throws Exception {
        Path file = newAbapFile(tmp.getRoot().toPath(), "trace_run.abap",
                "SELECT * FROM dummy;\n");
        App.CliArgs args = new App.CliArgs(file, null, "0", App.DEFAULT_PATTERN, true, true, false);
        App.run(args); // must not throw
        assertTrue(Files.exists(file));
    }

    // ------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------

    private static Path newAbapFile(Path dir, String name, String content) throws IOException {
        Path file = dir.resolve(name);
        Files.write(file, content.getBytes(StandardCharsets.UTF_8));
        return file;
    }
}
