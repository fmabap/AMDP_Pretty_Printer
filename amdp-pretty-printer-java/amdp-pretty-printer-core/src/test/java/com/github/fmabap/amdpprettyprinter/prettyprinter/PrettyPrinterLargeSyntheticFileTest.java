package com.github.fmabap.amdpprettyprinter.prettyprinter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Test;

/**
 * Regression test running the pretty printer over a large, purely synthetic
 * (programmatically generated) AMDP/SQLScript method.
 *
 * <p>
 * Unlike {@link PrettyPrinterSovAbapFileTest}, this source contains no
 * real-world/customer code - it is generated deterministically to reach a
 * token count comparable to large real files (deeply nested brackets, many
 * JOIN variants, UNION ALL, INSERT/UPSERT ... SELECT, a CALL statement, and a
 * long run of same-type tokens) so it exercises the same code paths that can
 * cause deep back-chain recursion in {@code BaseRule}.
 * </p>
 *
 * <p>
 * The expected output is a golden master captured once from the pretty
 * printer and stored as a test resource; it is compared byte-for-byte so any
 * behavioural change introduced while refactoring the recursive position
 * resolution is caught even if it is not covered by the more targeted unit
 * tests.
 * </p>
 */
public class PrettyPrinterLargeSyntheticFileTest extends PrettyPrinterTestBase {

    private static final int BLOCK_COUNT = 6;
    private static final int COLUMN_COUNT = 150;

    private static final String GOLDEN_RESOURCE = "large-synthetic-expected.abap";

    @Test
    public void largeSyntheticFilePrintsWithoutExceptionAndMatchesGolden() throws Exception {
        List<String> source = buildLargeSyntheticSource();

        List<String> actual = new PrettyPrinter().prettyPrint(source, standardSettings());

        assertTrue("Result must not be empty", actual != null && !actual.isEmpty());

        List<String> expected = readGoldenResource();
        assertEquals(expected, actual);
    }

    @Test
    public void largeSyntheticFileIsActuallyLarge() {
        // Sanity check: fail loudly if the generator is accidentally shrunk
        // below a size that exercises deep rule chains.
        List<String> source = buildLargeSyntheticSource();
        assertFalse("Synthetic source must not be empty", source.isEmpty());
        assertTrue("Synthetic source should contain several thousand lines/tokens; got "
                + source.size() + " lines", source.size() > 1500);
    }

    // -----------------------------------------------------------------------
    // Golden resource loading
    // -----------------------------------------------------------------------

    private static List<String> readGoldenResource() throws IOException {
        try (InputStream in = PrettyPrinterLargeSyntheticFileTest.class
                .getResourceAsStream("/" + GOLDEN_RESOURCE)) {
            if (in == null) {
                throw new IOException("Golden resource not found on classpath: " + GOLDEN_RESOURCE);
            }
            String content = new String(in.readAllBytes(), StandardCharsets.UTF_8);
            // Normalise line endings first: Git's autocrlf (or an editor) may have
            // rewritten the checked-in "\n" endings to "\r\n" on Windows, which
            // would otherwise leave a stray trailing '\r' on every split line.
            content = content.replace("\r\n", "\n");
            return java.util.Arrays.asList(content.split("\n", -1));
        }
    }

    // -----------------------------------------------------------------------
    // Synthetic source generator (shared with the golden-file generator tool)
    // -----------------------------------------------------------------------

    static List<String> buildLargeSyntheticSource() {
        List<String> src = new ArrayList<>();

        src.add("  METHOD sel_data");
        src.add("  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT");
        src.add("  OPTIONS READ-ONLY");
        src.add("  USING sflight, spfli, scarr.");
        src.add("");
        src.add("  declare lv_dummy nvarchar(4);");
        src.add("  lv_dummy = 'X';");
        src.add("");
        src.add("  call \"CLASS=>PROCESS\"(");
        src.add("       iv_capid => :iv_capid,");
        src.add("       iv_base_step => :iv_base_step,");
        src.add("       cv_operation_id => cv_operation_id");
        src.add("      );");
        src.add("");

        for (int b = 0; b < BLOCK_COUNT; b++) {
            src.add("  lt_result_" + b + " = select");
            appendColumnList(src);
            src.add("      , concat( concat( concat( col_0000, col_0001 ), col_0002 ), col_0003 ) as deep_nested_" + b);
            src.add("    from sflight as t0");
            src.add("    left join spfli as t1 on t1.carrid = t0.carrid");
            src.add("    right join scarr as t2 on t2.carrid = t0.carrid");
            src.add("    inner join sflight as t3 on t3.carrid = t0.carrid");
            src.add("    cross join spfli as t4");
            src.add("    where t0.mandt = session_context( 'CLIENT' )");
            src.add("    order by col_0000, col_0001");
            src.add("    group by col_0000");
            src.add("    union all");
            src.add("    select");
            appendColumnList(src);
            src.add("    from spfli");
            src.add("    where mandt = session_context( 'CLIENT' );");
            src.add("");
        }

        src.add("  insert into sflight select * from spfli;");
        src.add("");
        src.add("  upsert sflight select * from spfli;");
        src.add("");
        src.add("  ENDMETHOD.");

        return src;
    }

    private static void appendColumnList(List<String> src) {
        for (int c = 0; c < COLUMN_COUNT; c++) {
            String col = String.format("col_%04d", c);
            String suffix = (c == COLUMN_COUNT - 1) ? "" : ",";
            src.add("      " + col + suffix);
        }
    }

    // -----------------------------------------------------------------------
    // One-off golden-file generator (not a @Test; invoked manually via its
    // main method whenever the generator above changes intentionally).
    // -----------------------------------------------------------------------

    public static void main(String[] args) throws Exception {
        List<String> source = buildLargeSyntheticSource();
        List<String> formatted = new PrettyPrinter().prettyPrint(source, new TestSettings(true, false, false, false));
        String content = formatted.stream().collect(Collectors.joining("\n"));
        java.nio.file.Path target = java.nio.file.Paths.get(
                "src", "test", "resources", GOLDEN_RESOURCE);
        Files.createDirectories(target.getParent());
        try {
            Files.writeString(target, content, StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        System.out.println("Wrote golden resource: " + target.toAbsolutePath());
    }
}
