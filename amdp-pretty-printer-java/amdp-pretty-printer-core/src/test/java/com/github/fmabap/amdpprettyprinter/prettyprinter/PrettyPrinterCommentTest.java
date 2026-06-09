package com.github.fmabap.amdpprettyprinter.prettyprinter;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

/**
 * Tests for SQL comment handling in SQLScript methods.
 *
 * Converted from ABAP test methods:
 * comment_with_new_line_indent, correct_sql_comment_token
 */
public class PrettyPrinterCommentTest extends PrettyPrinterTestBase {

    // ------------------------------------------------------------------
    // Comment with keyword that would otherwise trigger extra indent
    // ------------------------------------------------------------------

    /**
     * A SQL comment line containing a keyword like DELETE must not trigger
     * the additional new-line indent that the keyword itself would cause.
     */
    @Test
    public void commentWithNewLineIndent() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "bla = select case ",
            "when a >= 1 then ",
            "-- delete bla",
            "'ABC'",
            "",
            "else",
            "---d dfsa",
            "'CDB'",
            "end as xy",
            "b as d",
            "",
            "endmethod."
        );

        List<String> expected = rtrim(lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "    bla = SELECT ",
            "                 CASE ",
            "                   WHEN a >= 1 THEN ",
            "                     -- delete bla",
            "                     'ABC'",
            "",
            "                   ELSE",
            "                     ---d dfsa",
            "                     'CDB'",
            "                 END AS xy",
            "                 b AS d",
            "",
            "endmethod."
        ));
        // @formatter:on

        assertEquals(expected, prettyPrint(source, standardSettings()));
    }

    // ------------------------------------------------------------------
    // SQL comment tokens after semicolon (;--) must be handled correctly
    // ------------------------------------------------------------------

    /**
     * SQL comment tokens that appear immediately after a semicolon (e.g.
     * "select * from sflight;-- comment") must be recognised as SQL comments
     * and not split or discarded.
     *
     * Also verifies that ABAP comment lines ("..., *...) and string literals
     * containing ";--" are NOT treated as SQL comments.
     */
    @Test
    public void correctSqlCommentToken() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "METHOD test1.",
            "write 'Hello;-- World'.",
            "write 'Hello;--World'.",
            "\"hello;--world.",
            "\"hello ;-- world.",
            "*hello;--world.",
            "*hello ;-- world.",
            "endmethod.",
            "METHOD test",
            " BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            " OPTIONS READ-ONLY.",
            "           select * from  sflight;-- where carrid = 'BLA'",
            "",
            "select * from  sflight; -- where carrid = 'BLA'    ",
            "",
            "select * from  sflight;-- where carrid = 'BLA'      ",
            "",
            "select * from  sflight ;-- where carrid = 'BLA'     ",
            "",
            "select * from  sflight;--",
            "",
            "select * from  sflight ;--",
            "",
            "endmethod.  "
        );

        List<String> expected = rtrim(lines(
            "METHOD test1.",
            "write 'Hello;-- World'.",
            "write 'Hello;--World'.",
            "\"hello;--world.",
            "\"hello ;-- world.",
            "*hello;--world.",
            "*hello ;-- world.",
            "endmethod.",
            "METHOD test",
            " BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            " OPTIONS READ-ONLY.",
            "    SELECT * ",
            "      FROM sflight;-- where carrid = 'BLA'",
            "",
            "    SELECT * ",
            "      FROM sflight; -- where carrid = 'BLA' ",
            "",
            "    SELECT * ",
            "      FROM sflight;-- where carrid = 'BLA' ",
            "",
            "    SELECT * ",
            "      FROM sflight ;-- where carrid = 'BLA' ",
            "",
            "    SELECT * ",
            "      FROM sflight;--",
            "",
            "    SELECT * ",
            "      FROM sflight ;--",
            "",
            "endmethod.  "
        ));
        // @formatter:on

        assertEquals(expected, prettyPrint(source, standardSettings()));
    }

    // ------------------------------------------------------------------
    // SQL comment tokens after semicolon (;--) must be handled correctly
    // ------------------------------------------------------------------

    /**
     * Semicolon after comment must be handled correctly
     */
    @Test
    public void semicolonComment() throws Exception {
     // @formatter:off
        List<String> source = lines(
            "   METHOD sel_data",
            "   BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "   OPTIONS READ-ONLY.",
            "",
            " lt_bla = SELECT 'Hello' AS str",
            "FROM dummy",
            "--comment",
            ";",
            "",
            "",
            "   ENDMETHOD.",
            ""
        );


        List<String> expected = rtrim(lines(
           "   METHOD sel_data",
            "   BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "   OPTIONS READ-ONLY.",
            "",
            "    lt_bla = SELECT 'Hello' AS str",
            "               FROM dummy",
            "                    --comment",
            "                    ; ",
            "",
            "",
            "   ENDMETHOD.",
            ""
        ));
        // @formatter:on
        List<String> actual = prettyPrint(source, standardSettings());
        assertEquals(expected, actual);
    }

    /**
     * Semicolon after comment must be handled correctly
     */
    @Test
    public void semicolonCommentMultiLine() throws Exception {
     // @formatter:off
        List<String> source = lines(
            "   METHOD sel_data",
            "   BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "   OPTIONS READ-ONLY.",
            "",
            " lt_bla = SELECT 'Hello' AS str",
            "FROM dummy",
            "/*comment*/",
            ";",
            "",
            "",
            "   ENDMETHOD.",
            ""
        );


        List<String> expected = rtrim(lines(
           "   METHOD sel_data",
            "   BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "   OPTIONS READ-ONLY.",
            "",
            "    lt_bla = SELECT 'Hello' AS str",
            "               FROM dummy",
            "/*comment*/",
            "                    ; ",
            "",
            "",
            "   ENDMETHOD.",
            ""
        ));
        // @formatter:on
        List<String> actual = prettyPrint(source, standardSettings());
        assertEquals(expected, actual);
    }

    /**
     * Semicolon after comment must be handled correctly
     */
    @Test
    public void semicolonCommentMultiLine2() throws Exception {
     // @formatter:off
        List<String> source = lines(
            "   METHOD sel_data",
            "   BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "   OPTIONS READ-ONLY.",
            "",
            " lt_bla = SELECT 'Hello' AS str",
            "FROM dummy",
            "/*comment1",
            "comment2*/",
            ";",
            "",
            "",
            "   ENDMETHOD.",
            ""
        );


        List<String> expected = rtrim(lines(
           "   METHOD sel_data",
            "   BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "   OPTIONS READ-ONLY.",
            "",
            "    lt_bla = SELECT 'Hello' AS str",
            "               FROM dummy",
            "/*comment1",
            "comment2*/",
            "                    ; ",
            "",
            "",
            "   ENDMETHOD.",
            ""
        ));
        // @formatter:on
        List<String> actual = prettyPrint(source, standardSettings());
        assertEquals(expected, actual);
    }
}
