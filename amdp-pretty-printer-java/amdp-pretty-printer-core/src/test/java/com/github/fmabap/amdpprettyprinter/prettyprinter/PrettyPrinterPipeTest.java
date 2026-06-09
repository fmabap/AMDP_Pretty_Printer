package com.github.fmabap.amdpprettyprinter.prettyprinter;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

/**
 * Tests for PIPE Converting.
 *
 */
public class PrettyPrinterPipeTest extends PrettyPrinterTestBase {

    // ------------------------------------------------------------------
    // PIPE Converting
    // ------------------------------------------------------------------

    @Test
    public void pipeSelection() throws Exception {
// @formatter:off
        List<String> source = lines(
            "   METHOD sel_data",
            "   BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "   OPTIONS READ-ONLY.",
            "",
            "lt_bla = select 'Hello' || Char( 32 ) || 'World.' from dummy;",
            "",
            "   ENDMETHOD."
        );
// @formatter:on

// @formatter:off
        List<String> expected = lines(
            "   METHOD sel_data",
            "   BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "   OPTIONS READ-ONLY.",
            "",
            "    lt_bla = SELECT 'Hello' || CHAR( 32 ) || 'World.'",
            "               FROM dummy;",
            "",
            "   ENDMETHOD."
        );
// @formatter:on

        assertEquals(expected, prettyPrint(source, standardSettings()));
    }

}
