package com.github.fmabap.amdpprettyprinter.prettyprinter;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

/**
 * Tests that plain ABAP source (no SQLScript) is returned unchanged by the
 * pretty printer.
 *
 * Converted from ABAP test method: over_take_abap_as_is
 */
public class PrettyPrinterAbapTest extends PrettyPrinterTestBase {

    /**
     * ABAP class source (no AMDP / SQLScript) must be passed through without
     * any formatting changes.
     */
    @Test
    public void overTakeAbapAsIs() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "CLASS",
            "",
            "zcl_app_test_amdp DEFINITION",
            "  PUBLIC",
            "  FINAL",
            "  CREATE PUBLIC .",
            ".",
            "  PUBLIC SECTION.",
            "",
            "    INTERFACES if_amdp_marker_hdb.",
            "",
            "",
            "    CLASS-METHODS write_data.",
            "",
            "  PROTECTED SECTION.",
            "  PRIVATE SECTION.",
            "ENDCLASS.",
            "",
            "",
            "",
            "CLASS zcl_app_test_amdp IMPLEMENTATION.",
            "",
            "  METHOD write_data.",
            "    \"Hello World 'keine Ahnung' 'bla",
            "    WRITE / 'Hello World'. \"Kommentar",
            "  ENDMETHOD.",
            "",
            "ENDCLASS."
        );
        // @formatter:on

        List<String> actual = prettyPrint(source, standardSettings());

        // The source itself is also rtrim-normalised (as in ABAP rtrim_source)
        assertEquals(rtrim(source), actual);
    }
}
