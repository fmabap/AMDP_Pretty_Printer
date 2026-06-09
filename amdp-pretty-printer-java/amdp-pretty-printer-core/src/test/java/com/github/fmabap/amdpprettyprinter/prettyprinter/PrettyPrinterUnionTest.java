package com.github.fmabap.amdpprettyprinter.prettyprinter;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

/**
 * Tests for UNION / UNION ALL formatting in SQLScript.
 *
 * Converted from ABAP test methods:
 * union, union_all, union_with_join, union_all_with_join
 */
public class PrettyPrinterUnionTest extends PrettyPrinterTestBase {

    // ------------------------------------------------------------------
    // UNION
    // ------------------------------------------------------------------

    @Test
    public void union() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "bla = select * from abc union select * from abc;",
            "",
            "endmethod."
        );

        List<String> expected = rtrim(lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "    bla = SELECT * ",
            "            FROM abc ",
            "           UNION ",
            "          SELECT * ",
            "            FROM abc;",
            "",
            "endmethod."
        ));
        // @formatter:on

        assertEquals(expected, prettyPrint(source, standardSettings()));
    }

    // ------------------------------------------------------------------
    // UNION ALL
    // ------------------------------------------------------------------

    @Test
    public void unionAll() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "bla = select * from abc union all select * from abc;",
            "",
            "endmethod."
        );

        List<String> expected = rtrim(lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "    bla =    SELECT * ",
            "               FROM abc ",
            "          UNION ALL ",
            "             SELECT * ",
            "               FROM abc;",
            "",
            "endmethod."
        ));
        // @formatter:on

        assertEquals(expected, prettyPrint(source, standardSettings()));
    }

    // ------------------------------------------------------------------
    // UNION with JOIN
    // ------------------------------------------------------------------

    @Test
    public void unionWithJoin() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "bla = select * from abc union select * from abc inner join blub on blub.bla = abc.bla;",
            "",
            "endmethod."
        );

        List<String> expected = rtrim(lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "    bla =     SELECT * ",
            "                FROM abc ",
            "               UNION ",
            "              SELECT * ",
            "                FROM abc ",
            "          INNER JOIN blub ",
            "                  ON blub.bla = abc.bla;",
            "",
            "endmethod."
        ));
        // @formatter:on

        assertEquals(expected, prettyPrint(source, standardSettings()));
    }

    // ------------------------------------------------------------------
    // UNION ALL with JOIN
    // ------------------------------------------------------------------

    @Test
    public void unionAllWithJoin() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "bla = select * from abc union all select * from abc inner join blub on blub.bla = abc.bla;",
            "",
            "endmethod."
        );

        List<String> expected = rtrim(lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "    bla =     SELECT * ",
            "                FROM abc ",
            "           UNION ALL ",
            "              SELECT * ",
            "                FROM abc ",
            "          INNER JOIN blub ",
            "                  ON blub.bla = abc.bla;",
            "",
            "endmethod."
        ));
        // @formatter:on

        assertEquals(expected, prettyPrint(source, standardSettings()));
    }
}
