package com.github.fmabap.amdpprettyprinter.prettyprinter;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

/**
 * Tests for SQL DML statement formatting (INSERT, DELETE, SELECT).
 *
 * Converted from ABAP test methods:
 * insert_sql, insert_with_select_sql, delete_sql, select_right
 */
public class PrettyPrinterDmlTest extends PrettyPrinterTestBase {

    // ------------------------------------------------------------------
    // INSERT … SELECT with INNER JOIN
    // ------------------------------------------------------------------

    @Test
    public void insertSql() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "METHOD INSERT PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "            USING MARA .",
            "",
            "  SELECT *    FROM :lt_mara ",
            "  INNER JOIN   :bla AS bla",
            "        ON :bla.matnr = :lt_mara.matnr; ",
            "",
            "ENDMETHOD."
        );

        List<String> expected = rtrim(lines(
            "METHOD INSERT PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "            USING MARA .",
            "",
            "        SELECT * ",
            "          FROM :lt_mara ",
            "    INNER JOIN :bla AS bla ",
            "            ON :bla.matnr = :lt_mara.matnr; ",
            "",
            "ENDMETHOD."
        ));
        // @formatter:on
        List<String> actual = prettyPrint(source, standardSettings());
        assertEquals(expected, actual);
    }

    // ------------------------------------------------------------------
    // INSERT bla SELECT * FROM … with sub-EXISTS
    // ------------------------------------------------------------------

    @Test
    public void insertWithSelectSql() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "",
            "        INSERT bla select * FROM blub",
            "               INNER JOIN bla ",
            "                       ON bla.ha = '1234' ",
            "                      AND bla.blub = 'slfdka'",
            "                    WHERE bla.blub = 'alkjfd'",
            "and exists ( select 1 from blub",
            "INNER JOIN bla ",
            "                       ON bla.ha = blub.abc ",
            "                     AND bla.blub = 'slfdka'",
            "where ab = 'dfkjs' );",
            "",
            "endmethod."
        );

        List<String> expected = rtrim(lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT.",
            "",
            "        INSERT bla ",
            "               SELECT * ",
            "                 FROM blub ",
            "           INNER JOIN bla ",
            "                   ON bla.ha = '1234' ",
            "                  AND bla.blub = 'slfdka' ",
            "                WHERE bla.blub = 'alkjfd'",
            "                  AND EXISTS (     SELECT 1 ",
            "                                     FROM blub",
            "                               INNER JOIN bla ",
            "                                       ON bla.ha = blub.abc ",
            "                                      AND bla.blub = 'slfdka'",
            "                                    WHERE ab = 'dfkjs' ",
            "                             );",
            "",
            "endmethod."
        ));
        // @formatter:on
        List<String> actual = prettyPrint(source, standardSettings());
        assertEquals(expected, actual);
    }

    // ------------------------------------------------------------------
    // INSERT INTO … VALUES
    // ------------------------------------------------------------------

    @Test
    public void insertInto() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "METHOD INSERT PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "            USING BLA .",
            "",
            "   insert into \"BLA\" select * from bla1;",
            "ENDMETHOD."
        );

        List<String> expected = rtrim(lines(
            "METHOD INSERT PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "            USING BLA .",
            "",
            "    INSERT",
            "      INTO \"BLA\" ",
            "           SELECT *",
            "             FROM bla1;",
            "ENDMETHOD."
        ));
        // @formatter:on

        List<String> actual = prettyPrint(source, standardSettings());
        assertEquals(expected, actual);
    }

    // ------------------------------------------------------------------
    // DELETE … WHERE
    // ------------------------------------------------------------------

    @Test
    public void deleteSql() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "METHOD INSERT PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "            USING BLA .",
            "",
            "   delete from \"BLA\" where mandt = :iv_mandt",
            "     and werks = :iv_werks;",
            "ENDMETHOD."
        );

        List<String> expected = rtrim(lines(
            "METHOD INSERT PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "            USING BLA .",
            "",
            "    DELETE ",
            "      FROM \"BLA\" ",
            "     WHERE mandt = :iv_mandt ",
            "       AND werks = :iv_werks;",
            "ENDMETHOD."
        ));
        // @formatter:on

        List<String> actual = prettyPrint(source, standardSettings());
        assertEquals(expected, actual);
    }

    // ------------------------------------------------------------------
    // SELECT with RIGHT() function (line-break after comma, standard)
    // ------------------------------------------------------------------

    @Test
    public void selectRight() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "  OPTIONS READ-ONLY",
            "  USING sflight.",
            "",
            "  lt_bla =  SELECT RIGHT( carrid,  4  )  FROM sflight;",
            "",
            " endmethod. "
        );

        List<String> expected = rtrim(lines(
            "  METHOD sel_data",
            "  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "  OPTIONS READ-ONLY",
            "  USING sflight.",
            "",
            "    lt_bla = SELECT ",
            "                    RIGHT( carrid, ",
            "                           4 ",
            "                         ) ",
            "               FROM sflight; ",
            "",
            " endmethod. "
        ));
        // @formatter:on
        List<String> actual = prettyPrint(source, standardSettings());
        assertEquals(expected, actual);
    }
}
