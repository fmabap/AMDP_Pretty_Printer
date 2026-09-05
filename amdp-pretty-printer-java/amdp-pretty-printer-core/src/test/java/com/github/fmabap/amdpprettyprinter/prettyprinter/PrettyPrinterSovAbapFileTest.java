package com.github.fmabap.amdpprettyprinter.prettyprinter;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import org.junit.Assume;
import org.junit.Test;

/**
 * Regression test running the pretty printer over the real-world AMDP class
 * source {@code tmp/sov.abap} from the repository root.
 *
 * <p>
 * The file is a large AMDP class with SQLScript method bodies, quoted
 * identifiers, comments and nested calls - a good smoke test that tokenizing
 * and re-formatting succeed without any error.
 * </p>
 */
public class PrettyPrinterSovAbapFileTest extends PrettyPrinterTestBase {

    /**
     * Relative path from the core module directory (the Maven/Surefire working
     * directory) to the test file at the repository root.
     */
    private static final Path SOV_ABAP = Paths.get("..", "..", "tmp", "sov.abap")
            .toAbsolutePath()
            .normalize();

    @Test
    public void sovAbapPrintsWithoutException() throws Exception {
        // Skip gracefully when the file is not available (e.g. building the
        // module standalone outside of the repository).
        Assume.assumeTrue("Test file not present: " + SOV_ABAP, Files.isRegularFile(SOV_ABAP));

        List<String> source = Files.readAllLines(SOV_ABAP, StandardCharsets.UTF_8);
        assertFalse("Test file must not be empty", source.isEmpty());

        // Any AppException (or other RuntimeException) thrown here fails the
        // test - this is the actual "no error" assertion.
        List<String> result = new PrettyPrinter().prettyPrint(source, standardSettings());

        assertTrue("Result must not be empty", result != null && !result.isEmpty());
    }
}
