package com.github.fmabap.amdpprettyprinter.prettyprinter;

import static org.junit.Assert.assertEquals;

import java.util.List;

import org.junit.Test;

/**
 * Tests for the "no line break after comma" settings in SQLScript.
 *
 * Converted from ABAP test methods:
 * no_lb_aft_co_in_si_fu_dep_sub - noLbSfu=true
 * no_lb_aft_co_in_si_fu_dep_cl_b - noLbCbrO=true
 * no_lb_aft_co_in_s_fu_dep_sf_kw - noLbSfuKw=true
 * no_lb_aft_co_left_right - noLbCbrO=true, LEFT/RIGHT functions
 * no_lb_aft_co_select - noLbCbrO=true, SELECT column list
 * no_lb_aft_co_by - noLbCbrO=true, PARTITION BY / ORDER BY
 */
public class PrettyPrinterNoLbAfterCommaTest extends PrettyPrinterTestBase {

    // ------------------------------------------------------------------
    // noLbSfu=true (no line break after comma if simple function depends
    // on a sub-function)
    // ------------------------------------------------------------------

    @Test
    public void noLbAftCoInSiFuDepSub() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "METHOD sel_data",
            "                     BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "                     OPTIONS READ-ONLY",
            "                     USING sflight.",
            "                     ",
            "                        lt_bla1 =  SELECT SUBSTRING( concat( 'Bla','Blub' ),  4, 6  )  FROM sflight;",
            "                     ",
            "                        lt_bla2 =  SELECT SUBSTRING( sflight.connid,  4, 6  )  FROM sflight;",
            "                     ",
            "                        lt_bla3 = SELECT SUBSTR_AFTER ('Hello My Friend','My ') \"substr after\" FROM DUMMY;",
            "                     ",
            "                        lt_bla4 = SELECT SUBSTR_BEFORE ('Hello My Friend','My') \"substr before\" FROM DUMMY;",
            "                     ",
            "                        lt_bla5 = SELECT RPAD ('end', 15, '12345') \"right padded\" FROM DUMMY;",
            "                     ",
            "                        lt_bla6 = SELECT LPAD ('end', 15, '12345') \"lpad\" FROM DUMMY;",
            "                     ",
            "                        lt_bla7 = SELECT CONCAT ('C', 'at') \"concat\" FROM DUMMY;",
            "                     ",
            "                        lt_bla8 = SELECT NULLIF ('diff', 'same') \"nullif\" FROM DUMMY;",
            "                     ",
            "                        lt_bla9 = SELECT IFNULL (NULL, 'same') \"ifnull\" FROM DUMMY;",
            "                     ",
            "                        lt_bla10 =  SELECT SUBSTRING( rtrim(connid),3,4) FROM spfli; ",
            "                     ",
            "                        lt_bla11 =  SELECT CONCAT ('C', concat( 'A','B')) FROM DUMMY; ",
            "                        ",
            "                        lt_bla12 = SELECT SUBSTRING( concat( 'Bla','Blub' ),  4, 6",
            "                                   )  FROM sflight;",
            "                     ",
            "                        lt_bla13 = SELECT CONCAT ( rtrim('BLA '), rtrim('BLUB ') )  FROM DUMMY;",
            "",
            "                     endmethod."
        );

        List<String> expected = rtrim(lines(
            "METHOD sel_data",
            "                     BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "                     OPTIONS READ-ONLY",
            "                     USING sflight.",
            "                     ",
            "    lt_bla1 = SELECT SUBSTRING( CONCAT( 'Bla','Blub' ), ",
            "                                4, ",
            "                                6 ",
            "                              ) ",
            "                FROM sflight; ",
            "",
            "    lt_bla2 = SELECT SUBSTRING( sflight.connid, 4, 6 ) ",
            "                FROM sflight; ",
            "",
            "    lt_bla3 = SELECT SUBSTR_AFTER ('Hello My Friend','My ') \"substr after\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla4 = SELECT SUBSTR_BEFORE ('Hello My Friend','My') \"substr before\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla5 = SELECT RPAD ('end', 15, '12345') \"right padded\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla6 = SELECT LPAD ('end', 15, '12345') \"lpad\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla7 = SELECT CONCAT ('C', 'at') \"concat\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla8 = SELECT NULLIF ('diff', 'same') \"nullif\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla9 = SELECT IFNULL (NULL, 'same') \"ifnull\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla10 = SELECT SUBSTRING( RTRIM(connid),3,4) ",
            "                 FROM spfli; ",
            "",
            "    lt_bla11 = SELECT CONCAT ('C', ",
            "                              CONCAT( 'A','B')",
            "                             ) ",
            "                 FROM dummy; ",
            "",
            "    lt_bla12 = SELECT SUBSTRING( CONCAT( 'Bla','Blub' ), ",
            "                                 4, ",
            "                                 6 ",
            "                               ) ",
            "                 FROM sflight; ",
            "",
            "    lt_bla13 = SELECT CONCAT ( RTRIM('BLA '), RTRIM('BLUB ') ) ",
            "                 FROM dummy; ",
            "",
            "                     endmethod."
        ));
        // @formatter:on

        // noLbSfu = true
        assertEquals(expected, prettyPrint(source, settings(true, true, false, false)));
    }

    // ------------------------------------------------------------------
    // noLbCbrO=true (no line break after comma if simple function depends
    // on closing bracket only - larger test set)
    // ------------------------------------------------------------------

    @Test
    public void noLbAftCoInSiFuDepClB() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "METHOD sel_data",
            "BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "OPTIONS READ-ONLY",
            "USING sflight.",
            "",
            "lt_bla1 =  SELECT SUBSTRING( concat( 'Bla','Blub' ),  4, 6  )  FROM sflight;",
            "",
            "lt_bla2 =  SELECT SUBSTRING( sflight.connid,  4, 6  )  FROM sflight;",
            "",
            "lt_bla3 = SELECT SUBSTR_AFTER ('Hello My Friend','My ') \"substr after\" FROM DUMMY;",
            "",
            "lt_bla4 = SELECT SUBSTR_BEFORE ('Hello My Friend','My') \"substr before\" FROM DUMMY;",
            "",
            "lt_bla5 = SELECT RPAD ('end', 15, '12345') \"right padded\" FROM DUMMY;",
            "",
            "lt_bla6 = SELECT LPAD ('end', 15, '12345') \"lpad\" FROM DUMMY;",
            "",
            "lt_bla7 = SELECT CONCAT ('C', 'at') \"concat\" FROM DUMMY;",
            "",
            "lt_bla8 = SELECT NULLIF ('diff', 'same') \"nullif\" FROM DUMMY;",
            "",
            "lt_bla9 = SELECT IFNULL (NULL, 'same') \"ifnull\" FROM DUMMY;",
            "",
            "lt_bla10 =  SELECT SUBSTRING( rtrim(connid),3,4) FROM spfli; ",
            "",
            "lt_bla11 =  SELECT CONCAT ('C', concat( 'A','B')) FROM DUMMY; ",
            "",
            "lt_bla12 = SELECT SUBSTRING( sflight.connid,  4, 6  ",
            "                 )  FROM sflight;",
            "",
            "lt_bla13 =  SELECT SUBSTRING( rtrim(connid),3,4",
            ") FROM spfli; ",
            "lt_bla14 =  SELECT CONCAT ('C', concat( 'A','B')",
            ") FROM DUMMY; ",
            "",
            "lt_bla15 =  SELECT CONCAT ('C', concat( 'A','B'",
            ")) FROM DUMMY; ",
            "",
            "",
            "endmethod."
        );

        List<String> expected = rtrim(lines(
            "METHOD sel_data",
            "BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "OPTIONS READ-ONLY",
            "USING sflight.",
            "",
            "    lt_bla1 = SELECT SUBSTRING( CONCAT( 'Bla','Blub' ), 4, 6 ) ",
            "                FROM sflight;",
            "",
            "    lt_bla2 = SELECT SUBSTRING( sflight.connid, 4, 6 ) ",
            "                FROM sflight;",
            "",
            "    lt_bla3 = SELECT SUBSTR_AFTER ('Hello My Friend','My ') \"substr after\" ",
            "                FROM dummy;",
            "",
            "    lt_bla4 = SELECT SUBSTR_BEFORE ('Hello My Friend','My') \"substr before\" ",
            "                FROM dummy;",
            "",
            "    lt_bla5 = SELECT RPAD ('end', 15, '12345') \"right padded\" ",
            "                FROM dummy;",
            "",
            "    lt_bla6 = SELECT LPAD ('end', 15, '12345') \"lpad\" ",
            "                FROM dummy;",
            "",
            "    lt_bla7 = SELECT CONCAT ('C', 'at') \"concat\" ",
            "                FROM dummy;",
            "",
            "    lt_bla8 = SELECT NULLIF ('diff', 'same') \"nullif\" ",
            "                FROM dummy;",
            "",
            "    lt_bla9 = SELECT IFNULL (NULL, 'same') \"ifnull\" ",
            "                FROM dummy;",
            "",
            "    lt_bla10 = SELECT SUBSTRING( RTRIM(connid),3,4) ",
            "                 FROM spfli; ",
            "",
            "    lt_bla11 = SELECT CONCAT ('C', CONCAT( 'A','B')) ",
            "                 FROM dummy; ",
            "",
            "    lt_bla12 = SELECT SUBSTRING( sflight.connid, ",
            "                                 4, ",
            "                                 6 ",
            "                               ) ",
            "                 FROM sflight;",
            "",
            "    lt_bla13 = SELECT SUBSTRING( RTRIM(connid),",
            "                                 3,",
            "                                 4",
            "                               ) ",
            "                 FROM spfli; ",
            "    lt_bla14 = SELECT CONCAT ('C', ",
            "                              CONCAT( 'A','B')",
            "                             ) ",
            "                 FROM dummy; ",
            "",
            "    lt_bla15 = SELECT CONCAT ('C', ",
            "                              CONCAT( 'A',",
            "                                      'B'",
            "                                    )",
            "                             ) ",
            "                 FROM dummy; ",
            "",
            "",
            "endmethod."
        ));
        // @formatter:on

        // noLbCbrO = true
        assertEquals(expected, prettyPrint(source, settings(true, false, true, false)));
    }

    // ------------------------------------------------------------------
    // noLbSfuKw=true (no line break after comma if simple function depends
    // on sub-function and keyword)
    // ------------------------------------------------------------------

    @Test
    public void noLbAftCoInSFuDepSfKw() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "METHOD sel_data",
            "                     BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "                     OPTIONS READ-ONLY",
            "                     USING sflight.",
            "                     ",
            "                        lt_bla1 =  SELECT SUBSTRING( concat( 'Bla','Blub' ),  4, 6  )  FROM sflight;",
            "                     ",
            "                        lt_bla2 =  SELECT SUBSTRING( sflight.connid,  4, 6  )  FROM sflight;",
            "                     ",
            "                        lt_bla3 = SELECT SUBSTR_AFTER ('Hello My Friend','My ') \"substr after\" FROM DUMMY;",
            "                     ",
            "                        lt_bla4 = SELECT SUBSTR_BEFORE ('Hello My Friend','My') \"substr before\" FROM DUMMY;",
            "                     ",
            "                        lt_bla5 = SELECT RPAD ('end', 15, '12345') \"right padded\" FROM DUMMY;",
            "                     ",
            "                        lt_bla6 = SELECT LPAD ('end', 15, '12345') \"lpad\" FROM DUMMY;",
            "                     ",
            "                        lt_bla7 = SELECT CONCAT ('C', 'at') \"concat\" FROM DUMMY;",
            "                     ",
            "                        lt_bla8 = SELECT NULLIF ('diff', 'same') \"nullif\" FROM DUMMY;",
            "                     ",
            "                        lt_bla9 = SELECT IFNULL (NULL, 'same') \"ifnull\" FROM DUMMY;",
            "                     ",
            "                        lt_bla10 =  SELECT SUBSTRING( rtrim(connid),3,4) FROM spfli; ",
            "                     ",
            "                        lt_bla11 =  SELECT CONCAT ('C', concat( 'A','B')) FROM DUMMY; ",
            "                        ",
            "                        lt_bla12 = SELECT SUBSTRING( concat( 'Bla','Blub' ),  4, 6",
            "                                   )  FROM sflight;",
            "                     ",
            "                        lt_bla13 = SELECT CONCAT ( rtrim('BLA '), rtrim('BLUB ') )  FROM DUMMY;",
            "",
            "                     endmethod."
        );

        List<String> expected = rtrim(lines(
            "METHOD sel_data",
            "                     BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            "                     OPTIONS READ-ONLY",
            "                     USING sflight.",
            "                     ",
            "    lt_bla1 = SELECT SUBSTRING( CONCAT( 'Bla','Blub' ), ",
            "                                4, ",
            "                                6 ",
            "                              ) ",
            "                FROM sflight; ",
            "",
            "    lt_bla2 = SELECT SUBSTRING( sflight.connid, 4, 6 ) ",
            "                FROM sflight; ",
            "",
            "    lt_bla3 = SELECT SUBSTR_AFTER ('Hello My Friend','My ') \"substr after\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla4 = SELECT SUBSTR_BEFORE ('Hello My Friend','My') \"substr before\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla5 = SELECT RPAD ('end', 15, '12345') \"right padded\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla6 = SELECT LPAD ('end', 15, '12345') \"lpad\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla7 = SELECT CONCAT ('C', 'at') \"concat\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla8 = SELECT NULLIF ('diff', 'same') \"nullif\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla9 = SELECT IFNULL (NULL, 'same') \"ifnull\" ",
            "                FROM dummy; ",
            "",
            "    lt_bla10 = SELECT SUBSTRING( RTRIM(connid),3,4) ",
            "                 FROM spfli; ",
            "",
            "    lt_bla11 = SELECT CONCAT ('C', ",
            "                              CONCAT( 'A','B')",
            "                             ) ",
            "                 FROM dummy; ",
            "",
            "    lt_bla12 = SELECT SUBSTRING( CONCAT( 'Bla','Blub' ), ",
            "                                 4, ",
            "                                 6 ",
            "                               ) ",
            "                 FROM sflight; ",
            "",
            "    lt_bla13 = SELECT CONCAT ( RTRIM('BLA '), ",
            "                               RTRIM('BLUB ') ",
            "                             ) ",
            "                 FROM dummy; ",
            "",
            "                     endmethod."
        ));
        // @formatter:on

        // noLbSfuKw = true
        assertEquals(expected, prettyPrint(source, settings(true, false, false, true)));
    }

    // ------------------------------------------------------------------
    // noLbCbrO=true - LEFT() and RIGHT() functions
    // ------------------------------------------------------------------

    @Test
    public void noLbAftCoLeftRight() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "METHOD test",
            " BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            " OPTIONS READ-ONLY.",
            "",
            "                     lt_bla1 =  SELECT LEFT( carrid,  4  )  FROM sflight;",
            "",
            "                     lt_bla2 =  SELECT RIGHT( carrid,  4  )  FROM sflight;",
            "    ",
            "",
            "endmethod."
        );

        List<String> expected = rtrim(lines(
            "METHOD test",
            " BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            " OPTIONS READ-ONLY.",
            "",
            "    lt_bla1 = SELECT ",
            "                     LEFT( carrid, 4 ) ",
            "                FROM sflight; ",
            "",
            "    lt_bla2 = SELECT ",
            "                     RIGHT( carrid, 4 ) ",
            "                FROM sflight; ",
            "",
            "",
            "endmethod."
        ));
        // @formatter:on

        // noLbCbrO = true
        assertEquals(expected, prettyPrint(source, settings(true, false, true, false)));
    }

    // ------------------------------------------------------------------
    // noLbCbrO=true - SELECT column list and EXISTS sub-query
    // ------------------------------------------------------------------

    @Test
    public void noLbAftCoSelect() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "METHOD test",
            " BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            " OPTIONS READ-ONLY.",
            "    lt_spfli1 = SELECT carrid, connid, countryfr, countryto, ",
            "                  FROM spfli ",
            "                 WHERE mandt = SESSION_CONTEXT( 'CLIENT' );",
            "         and exists ( select bla, blub from dummy where bla = '4' order by bla,blub );",
            "endmethod."
        );

        List<String> expected = rtrim(lines(
            "METHOD test",
            " BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            " OPTIONS READ-ONLY.",
            "    lt_spfli1 = SELECT carrid, ",
            "                       connid, ",
            "                       countryfr, ",
            "                       countryto, ",
            "                  FROM spfli ",
            "                 WHERE mandt = SESSION_CONTEXT( 'CLIENT' ); ",
            "    AND EXISTS (   SELECT bla, ",
            "                          blub ",
            "                     FROM dummy ",
            "                    WHERE bla = '4' ",
            "                 ORDER BY bla,",
            "                          blub ",
            "               );",
            "endmethod."
        ));
        // @formatter:on

        // noLbCbrO = true
        assertEquals(expected, prettyPrint(source, settings(true, false, true, false)));
    }

    // ------------------------------------------------------------------
    // noLbCbrO=true - PARTITION BY / ORDER BY inside OVER()
    // ------------------------------------------------------------------

    @Test
    public void noLbAftCoBy() throws Exception {
        // @formatter:off
        List<String> source = lines(
            "METHOD test",
            " BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            " OPTIONS READ-ONLY.",
            "                         lt_spfli2 = SELECT carrid, connid, countryfr, countryto,",
            "    ROW_NUMBER ( ) OVER( PARTITION BY carrid, connid ORDER BY \"CARRID\", CONNID asc ) AS \"ROW_ID\"",
            "    FROM SPFLI WHERE mandt = session_context( 'CLIENT' );",
            "endmethod."
        );

        List<String> expected = rtrim(lines(
            "METHOD test",
            " BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT",
            " OPTIONS READ-ONLY.",
            "    lt_spfli2 = SELECT carrid, ",
            "                       connid, ",
            "                       countryfr, ",
            "                       countryto, ",
            "                       ROW_NUMBER ( ) OVER( PARTITION BY carrid, ",
            "                                                         connid ",
            "                                            ORDER BY \"CARRID\", ",
            "                                                     connid ASC ",
            "                                          ) AS \"ROW_ID\" ",
            "                  FROM spfli ",
            "                 WHERE mandt = SESSION_CONTEXT( 'CLIENT' );",
            "endmethod."
        ));
        // @formatter:on

        // noLbCbrO = true
        assertEquals(expected, prettyPrint(source, settings(true, false, true, false)));
    }
}
