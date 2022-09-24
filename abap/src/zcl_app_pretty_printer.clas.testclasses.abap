*"* use this source file for your ABAP unit test classes
CLASS test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS over_take_abap_as_is FOR TESTING RAISING cx_static_check.
    METHODS with_sql_script FOR TESTING RAISING cx_static_check.
    METHODS with_sql_script_no_lb_at_comma FOR TESTING RAISING cx_static_check.
    METHODS insert_sql FOR TESTING RAISING cx_static_check.
    METHODS delete_sql FOR TESTING RAISING cx_static_check.
    METHODS select_right FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS test IMPLEMENTATION.

  METHOD over_take_abap_as_is.

    DATA lt_source TYPE sourcetable.
    DATA lt_source_res TYPE sourcetable.
    DATA lr_cut TYPE REF TO zcl_app_pretty_printer.
    DATA lr_ex TYPE REF TO zcx_app_exception.
    DATA lr_settings TYPE REF TO zif_app_settings.

    lt_source = VALUE #( ( |CLASS| )
                         ( || )
                         ( |zcl_app_test_amdp DEFINITION| )
                         ( |  PUBLIC| )
                         ( |  FINAL| )
                         ( |  CREATE PUBLIC .| )
                         ( |.| )
                         ( |  PUBLIC SECTION.| )
                         ( || )
                         ( |    INTERFACES if_amdp_marker_hdb.| )
                         ( || )
                         ( || )
                         ( |    CLASS-METHODS write_data.| )
                         ( || )
                         ( |  PROTECTED SECTION.| )
                         ( |  PRIVATE SECTION.| )
                         ( |ENDCLASS.| )
                         ( || )
                         ( || )
                         ( || )
                         ( |CLASS zcl_app_test_amdp IMPLEMENTATION.| )
                         ( || )
                         ( |  METHOD write_data.| )
                         ( |    "Hello World 'keine Ahnung' 'bla| )
                         ( |    WRITE / 'Hello World'. "Kommentar| )
                         ( |  ENDMETHOD.| )
                         ( || )
                         ( |ENDCLASS.| ) ).

    CREATE OBJECT lr_cut.
    TRY.
        CREATE OBJECT lr_settings TYPE zcl_app_settings.

        lt_source_res = lr_cut->pretty_print(
          it_source   = lt_source
          ir_settings = lr_settings ).


      CATCH zcx_app_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lt_source_res
        exp = lt_source
        msg = 'Tables differs' ).

  ENDMETHOD.




  METHOD with_sql_script.

    DATA lt_source TYPE sourcetable.
    DATA lt_source_res TYPE sourcetable.
    DATA lt_source_res_exp TYPE sourcetable.
    DATA lr_cut TYPE REF TO zcl_app_pretty_printer.
    DATA lr_ex TYPE REF TO zcx_app_exception.
    DATA lr_settings TYPE REF TO zif_app_settings.

    lr_settings ?= cl_abap_testdouble=>create( 'zif_app_settings' ).
    cl_abap_testdouble=>configure_call( lr_settings )->returning( abap_true ).
    lr_settings->is_line_break_at_comma_req( ).

    lt_source = VALUE #(
                         ( |CLASS zcl_app_test_amdp DEFINITION| )
                         ( |  PUBLIC| )
                         ( |  FINAL| )
                         ( |  CREATE PUBLIC .| )
                         ( || )
                         ( |  PUBLIC SECTION.| )
                         ( || )
                         ( |    INTERFACES if_amdp_marker_hdb.| )
                         ( || )
                         ( |    CLASS-METHODS sel_data.| )
                         ( || )
                         ( |    CLASS-METHODS write_data.| )
                         ( || )
                         ( |  PROTECTED SECTION.| )
                         ( |  PRIVATE SECTION.| )
                         ( |ENDCLASS.| )
                         ( || )
                         ( || )
                         ( || )
                         ( |CLASS zcl_app_test_amdp IMPLEMENTATION.| )
                         ( || )
                         ( |  METHOD write_data.| )
                         ( |    "Hello World 'keine Ahnung' 'bla| )
                         ( |    WRITE / 'Hello World'. "Kommentar| )
                         ( |  ENDMETHOD.| )
                         ( || )
                         ( |  METHOD sel_data| )
                         ( |  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT| )
                         ( |  "opt| )
                         ( |* sqlscript| )
                         ( |  OPTIONS READ-ONLY| )
                         ( |  USING sflight. | )
                         ( | -- Flight| )
                         ( | declare lv_bla nvarchar(4);| )
                         ( | lv_bla = 'BlA';                        ---Blub| )
                         ( || )
                         ( | call "CLASS=>PROCESS"( | )
                         ( |          iv_capid     => :IV_CAPID,              --Blub| )
                         ( |     iv_base_step     =>     :IV_BASE_STEP,     | )
                         ( |          cv_operation_id =>     cv_operation_id | )
                         ( |                 ); | )
                         ( || )
                         ( |    lt_carrid2 = select "SFLIGHT"."CARRID" from sflight;| )
                         ( || )
                         ( |    lt_sflight = select * from sflight;| )
                         ( || )
                         ( |    lt_carrid = select "CARRID" from sflight;| )
                         ( || )
                         ( |    lt_carrid2 = select "SFLIGHT"."CARRID" from sflight;| )
                         ( | lt_carrid3 = select max( concat( "SFLIGHT"."CARRID", connid ) )from sflight; | )
                         ( |lt_carrid4 = select max( ( concat( sflight, connid ) ) )from sflight; | )
                         ( |lt_carrid4 = select max( ( concat( sflight, connid )))from sflight; | )
                         ( |lt_carrid5 = select concat( sflight, connid ) from sflight; | )
                         ( |lt_carrid5 = select concat( sflight, connid ) from sflight with hint ( NO_INLINE ); | )
                         ( |lt_carrid5 = select concat( sflight, connid ) from sflight with hint ( NO_INLINE | )
                         ( | ); | )
                         ( |lt_carrid5 = select concat( sflight, connid ) from sflight with hint ( NO_INLINE | )
                         ( | ) ; | )
                         ( |lt_carrid5 = select concat( sflight, connid ) from sflight; | )
                         ( || )
                         ( |*lt_test = select 'Bla Blup Data' from public.dummy;| )
                         ( |    lt_test = select 'Bla Blup Data' from public.dummy;| )
                         ( || )
                         ( |    -- Hello World 'keine Ahnung' 'bla blub'| )
                         ( |    lt_test2 = select 'Bla Blup Data' from public.dummy; /*schaun mer mal */| )
                         ( || )
                         ( |    /* schaun mer| )
                         ( |    noch mehr */| )
                         ( || )
                         ( |    lt_test3 = select 'Bla Blup Data' from public.dummy; /*schaun mer mal */| )
                         ( || )
                         ( |    lt_test3 = select 'Bla Blup Data' from public.dummy; -- schaun mer mal| )
                         ( || )
                         ( |et_exp_part = select :bal, :| )
                         ( |                             :LT_EXP_PART."/DTBOM/SAP_OBJ_KEY_REF", | )
                         ( |                             :LT_EXP_PART."/DTBOM/SAP_OBJ_VRS_REF" | )
                         ( |                        from :LT_EXP_PART | )
                         ( |                  inner join "/DTBOM/30_IC_MAT_TYP_SNR_USE" as snrclsp | )
                         ( |                          on snrclsp.mandt = session_context( 'CLIENT') | )
                         ( |                         and snrclsp.matnr = :LT_EXP_PART.matnr | )
                         ( |                  union  all | )
                         ( |                      select :LT_EXP_PART.run_no, | )
                         ( |                             :LT_EXP_PART."/DTBOM/SAP_OBJ_KEY_REF", | )
                         ( |                             :LT_EXP_PART."/DTBOM/SAP_OBJ_VRS_REF" | )
                         ( |                        from :LT_EXP_PART | )
                         ( |                       where not exists( select 1 | )
                         ( |                                           from "/DTBOM/30_IC_MAT_TYP_SNR_USE" as snrclsp | )
                         ( |                                          where snrclsp.mandt = session_context( 'CLIENT') | )
                         ( |                                            and snrclsp.matnr = :LT_EXP_PART.matnr | )
                         ( |                                       );| )
                         ( || )
                         ( | Lt_test4 =  select carrid, connid, fldate| )
                         ( |    from sflight| )
                         ( |    where sflight.mandt =  SESSION_CONTEXT('CLIENT')| )
                         ( |    and sflight.connid = 'LH' union all| )
                         ( |    select spfli.carrid, spfli.connid, sflight.FLDATE| )
                         ( |    from spfli left outer join sflight| )
                         ( |    on sflight.mandt = spfli.mandt| )
                         ( |    and sflight.CARRID = spfli.carrid| )
                         ( |    and sflight.CONNID = spfli.connid| )
                         ( |    where spfli.mandt = SESSION_CONTEXT('CLIENT')| )
                         ( |    and spfli.carrid = 'AB' union all| )
                         ( |    select spfli.carrid, spfli.connid, sflight.FLDATE| )
                         ( |    from spfli inner join sflight| )
                         ( |    on sflight.mandt = spfli.mandt| )
                         ( |    and sflight.CARRID = spfli.carrid| )
                         ( |    and sflight.CONNID = spfli.connid| )
                         ( |    where spfli.mandt = SESSION_CONTEXT('CLIENT')| )
                         ( |    and spfli.carrid = 'AB';| )
                         ( || )
                         ( |lt_spfli1 = SELECT carrid, connid, countryfr, countryto,| )
                         ( |ROW_NUMBER ( ) OVER( ORDER BY "CARRID", CONNID asc ) AS "ROW_ID"| )
                         ( |FROM SPFLI WHERE mandt = session_context( 'CLIENT' );| )
                         ( || )
                         ( || )
                         ( |lt_spfli2 = SELECT carrid, connid, countryfr, countryto,| )
                         ( |ROW_NUMBER ( ) OVER( PARTITION BY carrid, connid ORDER BY "CARRID", CONNID asc ) AS "ROW_ID"| )
                         ( |FROM SPFLI WHERE mandt = session_context( 'CLIENT' );| )
                         ( |    --ENDMETHOD.| )
                         ( |  ENDMETHOD.| )
                         ( |ENDCLASS.| ) ).


    lt_source_res_exp = VALUE #(
                                 ( |CLASS zcl_app_test_amdp DEFINITION| )
                                 ( |  PUBLIC| )
                                 ( |  FINAL| )
                                 ( |  CREATE PUBLIC .| )
                                 ( || )
                                 ( |  PUBLIC SECTION.| )
                                 ( || )
                                 ( |    INTERFACES if_amdp_marker_hdb.| )
                                 ( || )
                                 ( |    CLASS-METHODS sel_data.| )
                                 ( || )
                                 ( |    CLASS-METHODS write_data.| )
                                 ( || )
                                 ( |  PROTECTED SECTION.| )
                                 ( |  PRIVATE SECTION.| )
                                 ( |ENDCLASS.| )
                                 ( || )
                                 ( || )
                                 ( || )
                                 ( |CLASS zcl_app_test_amdp IMPLEMENTATION.| )
                                 ( || )
                                 ( |  METHOD write_data.| )
                                 ( |    "Hello World 'keine Ahnung' 'bla| )
                                 ( |    WRITE / 'Hello World'. "Kommentar| )
                                 ( |  ENDMETHOD.| )
                                 ( || )
                                 ( |  METHOD sel_data| )
                                 ( |  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT| )
                                 ( |  "opt| )
                                 ( |* sqlscript| )
                                 ( |  OPTIONS READ-ONLY| )
                                 ( |  USING sflight. | )
                                 ( |    -- Flight | )
                                 ( |    DECLARE lv_bla NVARCHAR(4); | )
                                 ( |    lv_bla = 'BlA';                        ---Blub | )
                                 ( || )
                                 ( |    CALL "CLASS=>PROCESS"( | )
                                 ( |             iv_capid        => :iv_capid,              --Blub | )
                                 ( |             iv_base_step    => :iv_base_step, | )
                                 ( |             cv_operation_id => cv_operation_id | )
                                 ( |             ); | )
                                 ( || )
                                 ( |    lt_carrid2 = SELECT "SFLIGHT"."CARRID" | )
                                 ( |                   FROM sflight; | )
                                 ( || )
                                 ( |    lt_sflight = SELECT * | )
                                 ( |                   FROM sflight; | )
                                 ( || )
                                 ( |    lt_carrid = SELECT "CARRID" | )
                                 ( |                  FROM sflight; | )
                                 ( || )
                                 ( |    lt_carrid2 = SELECT "SFLIGHT"."CARRID" | )
                                 ( |                   FROM sflight; | )
                                 ( |    lt_carrid3 = SELECT MAX( concat( "SFLIGHT"."CARRID", | )
                                 ( |                                     connid | )
                                 ( |                                   ) | )
                                 ( |                           ) | )
                                 ( |                   FROM sflight; | )
                                 ( |    lt_carrid4 = SELECT MAX( | )
                                 ( |                             ( concat( sflight, | )
                                 ( |                                       connid | )
                                 ( |                                     ) | )
                                 ( |                             ) | )
                                 ( |                           ) | )
                                 ( |                   FROM sflight; | )
                                 ( |    lt_carrid4 = SELECT MAX( | )
                                 ( |                             ( concat( sflight, | )
                                 ( |                                       connid | )
                                 ( |                                     )| )
                                 ( |                             )| )
                                 ( |                           ) | )
                                 ( |                   FROM sflight; | )
                                 ( |    lt_carrid5 = SELECT concat( sflight, | )
                                 ( |                                connid | )
                                 ( |                              ) | )
                                 ( |                   FROM sflight; | )
                                 ( |    lt_carrid5 = SELECT concat( sflight, | )
                                 ( |                                connid | )
                                 ( |                              ) | )
                                 ( |                   FROM sflight WITH hint ( no_inline ); | )
                                 ( |    lt_carrid5 = SELECT concat( sflight, | )
                                 ( |                                connid | )
                                 ( |                              ) | )
                                 ( |                   FROM sflight WITH hint ( no_inline | )
                                 ( |                                          ); | )
                                 ( |    lt_carrid5 = SELECT concat( sflight, | )
                                 ( |                                connid | )
                                 ( |                              ) | )
                                 ( |                   FROM sflight WITH hint ( no_inline | )
                                 ( |                                          ) ; | )
                                 ( |    lt_carrid5 = SELECT concat( sflight, | )
                                 ( |                                connid | )
                                 ( |                              ) | )
                                 ( |                   FROM sflight; | )
                                 ( || )
                                 ( |*lt_test = select 'Bla Blup Data' from public.dummy; | )
                                 ( |    lt_test = SELECT 'Bla Blup Data' | )
                                 ( |                FROM public.dummy; | )
                                 ( || )
                                 ( |    -- Hello World 'keine Ahnung' 'bla blub' | )
                                 ( |    lt_test2 = SELECT 'Bla Blup Data' | )
                                 ( |                 FROM public.dummy; /*schaun mer mal */ | )
                                 ( || )
                                 ( |    /* schaun mer| )
                                 ( |    noch mehr */ | )
                                 ( || )
                                 ( |    lt_test3 = SELECT 'Bla Blup Data' | )
                                 ( |                 FROM public.dummy; /*schaun mer mal */ | )
                                 ( || )
                                 ( |    lt_test3 = SELECT 'Bla Blup Data' | )
                                 ( |                 FROM public.dummy; -- schaun mer mal| )
                                 ( || )
                                 ( |    et_exp_part =     SELECT :bal, | )
                                 ( |                             : | )
                                 ( |                             :lt_exp_part."/DTBOM/SAP_OBJ_KEY_REF", | )
                                 ( |                             :lt_exp_part."/DTBOM/SAP_OBJ_VRS_REF" | )
                                 ( |                        FROM :lt_exp_part | )
                                 ( |                  INNER JOIN "/DTBOM/30_IC_MAT_TYP_SNR_USE" AS snrclsp | )
                                 ( |                          ON snrclsp.mandt = SESSION_CONTEXT( 'CLIENT') | )
                                 ( |                         AND snrclsp.matnr = :lt_exp_part.matnr | )
                                 ( |                   UNION ALL | )
                                 ( |                      SELECT :lt_exp_part.run_no, | )
                                 ( |                             :lt_exp_part."/DTBOM/SAP_OBJ_KEY_REF", | )
                                 ( |                             :lt_exp_part."/DTBOM/SAP_OBJ_VRS_REF" | )
                                 ( |                        FROM :lt_exp_part | )
                                 ( |                       WHERE NOT EXISTS( SELECT 1 | )
                                 ( |                                           FROM "/DTBOM/30_IC_MAT_TYP_SNR_USE" AS snrclsp | )
                                 ( |                                          WHERE snrclsp.mandt = SESSION_CONTEXT( 'CLIENT') | )
                                 ( |                                            AND snrclsp.matnr = :lt_exp_part.matnr | )
                                 ( |                                       ); | )
                                 ( || )
                                 ( |    lt_test4 =          SELECT carrid, | )
                                 ( |                               connid, | )
                                 ( |                               fldate | )
                                 ( |                          FROM sflight | )
                                 ( |                         WHERE sflight.mandt = SESSION_CONTEXT('CLIENT') | )
                                 ( |                           AND sflight.connid = 'LH' | )
                                 ( |                     UNION ALL | )
                                 ( |                        SELECT spfli.carrid, | )
                                 ( |                               spfli.connid, | )
                                 ( |                               sflight.fldate | )
                                 ( |                          FROM spfli | )
                                 ( |               LEFT OUTER JOIN sflight | )
                                 ( |                            ON sflight.mandt = spfli.mandt | )
                                 ( |                           AND sflight.carrid = spfli.carrid | )
                                 ( |                           AND sflight.connid = spfli.connid | )
                                 ( |                         WHERE spfli.mandt = SESSION_CONTEXT('CLIENT') | )
                                 ( |                           AND spfli.carrid = 'AB' | )
                                 ( |                     UNION ALL | )
                                 ( |                        SELECT spfli.carrid, | )
                                 ( |                               spfli.connid, | )
                                 ( |                               sflight.fldate | )
                                 ( |                          FROM spfli | )
                                 ( |                    INNER JOIN sflight | )
                                 ( |                            ON sflight.mandt = spfli.mandt | )
                                 ( |                           AND sflight.carrid = spfli.carrid | )
                                 ( |                           AND sflight.connid = spfli.connid | )
                                 ( |                         WHERE spfli.mandt = SESSION_CONTEXT('CLIENT') | )
                                 ( |                           AND spfli.carrid = 'AB';| )
                                 ( || )
                                 ( |    lt_spfli1 = SELECT carrid, | )
                                 ( |                       connid, | )
                                 ( |                       countryfr, | )
                                 ( |                       countryto,| )
                                 ( |                       ROW_NUMBER ( ) OVER( ORDER BY "CARRID", | )
                                 ( |                                                     connid ASC | )
                                 ( |                                          ) AS "ROW_ID"| )
                                 ( |                  FROM spfli | )
                                 ( |                 WHERE mandt = SESSION_CONTEXT( 'CLIENT' );| )
                                 ( || )
                                 ( || )
                                 ( |    lt_spfli2 = SELECT carrid, | )
                                 ( |                       connid, | )
                                 ( |                       countryfr, | )
                                 ( |                       countryto,| )
                                 ( |                       ROW_NUMBER ( ) OVER( PARTITION BY carrid, | )
                                 ( |                                                         connid | )
                                 ( |                                            ORDER BY "CARRID", | )
                                 ( |                                                     connid ASC | )
                                 ( |                                          ) AS "ROW_ID"| )
                                 ( |                  FROM spfli | )
                                 ( |                 WHERE mandt = SESSION_CONTEXT( 'CLIENT' ); | )
                                 ( |    --ENDMETHOD. | )
                                 ( |  ENDMETHOD.| )
                                 ( |ENDCLASS.| ) ).

    CREATE OBJECT lr_cut.
    TRY.
        lt_source_res = lr_cut->pretty_print(
          it_source   = lt_source
          ir_settings = lr_settings ).

      CATCH zcx_app_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lt_source_res
        exp = lt_source_res_exp
        msg = 'Tables differs' ).

  ENDMETHOD.

  METHOD with_sql_script_no_lb_at_comma.

    DATA lt_source TYPE sourcetable.
    DATA lt_source_res TYPE sourcetable.
    DATA lt_source_res_exp TYPE sourcetable.
    DATA lr_cut TYPE REF TO zcl_app_pretty_printer.
    DATA lr_ex TYPE REF TO zcx_app_exception.
    DATA lr_settings TYPE REF TO zif_app_settings.

    lr_settings ?= cl_abap_testdouble=>create( 'zif_app_settings' ).
    cl_abap_testdouble=>configure_call( lr_settings )->returning( abap_false ).
    lr_settings->is_line_break_at_comma_req( ).

    lt_source = VALUE #(
                         ( |CLASS zcl_app_test_amdp DEFINITION| )
                         ( |  PUBLIC| )
                         ( |  FINAL| )
                         ( |  CREATE PUBLIC .| )
                         ( || )
                         ( |  PUBLIC SECTION.| )
                         ( || )
                         ( |    INTERFACES if_amdp_marker_hdb.| )
                         ( || )
                         ( |    CLASS-METHODS sel_data.| )
                         ( || )
                         ( |    CLASS-METHODS write_data.| )
                         ( || )
                         ( |  PROTECTED SECTION.| )
                         ( |  PRIVATE SECTION.| )
                         ( |ENDCLASS.| )
                         ( || )
                         ( || )
                         ( || )
                         ( |CLASS zcl_app_test_amdp IMPLEMENTATION.| )
                         ( || )
                         ( |  METHOD write_data.| )
                         ( |    "Hello World 'keine Ahnung' 'bla| )
                         ( |    WRITE / 'Hello World'. "Kommentar| )
                         ( |  ENDMETHOD.| )
                         ( || )
                         ( |  METHOD sel_data| )
                         ( |  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT| )
                         ( |  "opt| )
                         ( |* sqlscript| )
                         ( |  OPTIONS READ-ONLY| )
                         ( |  USING sflight. | )
                         ( | -- Flight| )
                         ( | declare lv_bla nvarchar(4);| )
                         ( | lv_bla = 'BlA';                        ---Blub| )
                         ( || )
                         ( | call "CLASS=>PROCESS"( | )
                         ( |          iv_capid     => :IV_CAPID,              --Blub| )
                         ( |     iv_base_step     =>     :IV_BASE_STEP,     | )
                         ( |          cv_operation_id =>     cv_operation_id | )
                         ( |                 ); | )
                         ( || )
                         ( |    lt_carrid2 = select "SFLIGHT"."CARRID" from sflight;| )
                         ( || )
                         ( |    lt_sflight = select * from sflight;| )
                         ( || )
                         ( |    lt_carrid = select "CARRID" from sflight;| )
                         ( || )
                         ( |    lt_carrid2 = select "SFLIGHT"."CARRID" from sflight;| )
                         ( | lt_carrid3 = select max( concat( "SFLIGHT"."CARRID", connid ) )from sflight; | )
                         ( |lt_carrid4 = select max( ( concat( sflight, connid ) ) )from sflight; | )
                         ( |lt_carrid4 = select max( ( concat( sflight, connid )))from sflight; | )
                         ( |lt_carrid5 = select concat( sflight, connid ) from sflight; | )
                         ( |lt_carrid5 = select concat( sflight, connid ) from sflight with hint ( NO_INLINE ); | )
                         ( |lt_carrid5 = select concat( sflight, connid ) from sflight with hint ( NO_INLINE | )
                         ( | ); | )
                         ( |lt_carrid5 = select concat( sflight, connid ) from sflight with hint ( NO_INLINE | )
                         ( | ) ; | )
                         ( |lt_carrid5 = select concat( sflight, connid ) from sflight; | )
                         ( || )
                         ( |*lt_test = select 'Bla Blup Data' from public.dummy;| )
                         ( |    lt_test = select 'Bla Blup Data' from public.dummy;| )
                         ( || )
                         ( |    -- Hello World 'keine Ahnung' 'bla blub'| )
                         ( |    lt_test2 = select 'Bla Blup Data' from public.dummy; /*schaun mer mal */| )
                         ( || )
                         ( |    /* schaun mer| )
                         ( |    noch mehr */| )
                         ( || )
                         ( |    lt_test3 = select 'Bla Blup Data' from public.dummy; /*schaun mer mal */| )
                         ( || )
                         ( |    lt_test3 = select 'Bla Blup Data' from public.dummy; -- schaun mer mal| )
                         ( || )
                         ( |et_exp_part = select :bal, :| )
                         ( |                             :LT_EXP_PART."/DTBOM/SAP_OBJ_KEY_REF", | )
                         ( |                             :LT_EXP_PART."/DTBOM/SAP_OBJ_VRS_REF" | )
                         ( |                        from :LT_EXP_PART | )
                         ( |                  inner join "/DTBOM/30_IC_MAT_TYP_SNR_USE" as snrclsp | )
                         ( |                          on snrclsp.mandt = session_context( 'CLIENT') | )
                         ( |                         and snrclsp.matnr = :LT_EXP_PART.matnr | )
                         ( |                  union  all | )
                         ( |                      select :LT_EXP_PART.run_no, | )
                         ( |                             :LT_EXP_PART."/DTBOM/SAP_OBJ_KEY_REF", | )
                         ( |                             :LT_EXP_PART."/DTBOM/SAP_OBJ_VRS_REF" | )
                         ( |                        from :LT_EXP_PART | )
                         ( |                       where not exists( select 1 | )
                         ( |                                           from "/DTBOM/30_IC_MAT_TYP_SNR_USE" as snrclsp | )
                         ( |                                          where snrclsp.mandt = session_context( 'CLIENT') | )
                         ( |                                            and snrclsp.matnr = :LT_EXP_PART.matnr | )
                         ( |                                       );| )
                         ( || )
                         ( | Lt_test4 =  select carrid, connid, fldate| )
                         ( |    from sflight| )
                         ( |    where sflight.mandt =  SESSION_CONTEXT('CLIENT')| )
                         ( |    and sflight.connid = 'LH' union all| )
                         ( |    select spfli.carrid, spfli.connid, sflight.FLDATE| )
                         ( |    from spfli left outer join sflight| )
                         ( |    on sflight.mandt = spfli.mandt| )
                         ( |    and sflight.CARRID = spfli.carrid| )
                         ( |    and sflight.CONNID = spfli.connid| )
                         ( |    where spfli.mandt = SESSION_CONTEXT('CLIENT')| )
                         ( |    and spfli.carrid = 'AB' union all| )
                         ( |    select spfli.carrid, spfli.connid, sflight.FLDATE| )
                         ( |    from spfli inner join sflight| )
                         ( |    on sflight.mandt = spfli.mandt| )
                         ( |    and sflight.CARRID = spfli.carrid| )
                         ( |    and sflight.CONNID = spfli.connid| )
                         ( |    where spfli.mandt = SESSION_CONTEXT('CLIENT')| )
                         ( |    and spfli.carrid = 'AB';| )
                         ( || )
                         ( |lt_spfli1 = SELECT carrid, connid, countryfr, countryto,| )
                         ( |ROW_NUMBER ( ) OVER( ORDER BY "CARRID", CONNID asc ) AS "ROW_ID"| )
                         ( |FROM SPFLI WHERE mandt = session_context( 'CLIENT' );| )
                         ( || )
                         ( || )
                         ( |lt_spfli2 = SELECT carrid, connid, countryfr, countryto,| )
                         ( |ROW_NUMBER ( ) OVER( PARTITION BY carrid, connid ORDER BY "CARRID", CONNID asc ) AS "ROW_ID"| )
                         ( |FROM SPFLI WHERE mandt = session_context( 'CLIENT' );| )
                         ( |    --ENDMETHOD.| )
                         ( |  ENDMETHOD.| )
                         ( |ENDCLASS.| ) ).


    lt_source_res_exp = VALUE #(
                              ( |CLASS zcl_app_test_amdp DEFINITION| )
                              ( |  PUBLIC| )
                              ( |  FINAL| )
                              ( |  CREATE PUBLIC .| )
                              ( || )
                              ( |  PUBLIC SECTION.| )
                              ( || )
                              ( |    INTERFACES if_amdp_marker_hdb.| )
                              ( || )
                              ( |    CLASS-METHODS sel_data.| )
                              ( || )
                              ( |    CLASS-METHODS write_data.| )
                              ( || )
                              ( |  PROTECTED SECTION.| )
                              ( |  PRIVATE SECTION.| )
                              ( |ENDCLASS.| )
                              ( || )
                              ( || )
                              ( || )
                              ( |CLASS zcl_app_test_amdp IMPLEMENTATION.| )
                              ( || )
                              ( |  METHOD write_data.| )
                              ( |    "Hello World 'keine Ahnung' 'bla| )
                              ( |    WRITE / 'Hello World'. "Kommentar| )
                              ( |  ENDMETHOD.| )
                              ( || )
                              ( |  METHOD sel_data| )
                              ( |  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT| )
                              ( |  "opt| )
                              ( |* sqlscript| )
                              ( |  OPTIONS READ-ONLY| )
                              ( |  USING sflight. | )
                              ( |    -- Flight | )
                              ( |    DECLARE lv_bla NVARCHAR(4); | )
                              ( |    lv_bla = 'BlA';                        ---Blub | )
                              ( || )
                              ( |    CALL "CLASS=>PROCESS"( iv_capid => :iv_capid,              --Blub | )
                              ( |             iv_base_step => :iv_base_step, | )
                              ( |             cv_operation_id => cv_operation_id | )
                              ( |             ); | )
                              ( || )
                              ( |    lt_carrid2 = SELECT "SFLIGHT"."CARRID" | )
                              ( |                   FROM sflight; | )
                              ( || )
                              ( |    lt_sflight = SELECT * | )
                              ( |                   FROM sflight; | )
                              ( || )
                              ( |    lt_carrid = SELECT "CARRID" | )
                              ( |                  FROM sflight; | )
                              ( || )
                              ( |    lt_carrid2 = SELECT "SFLIGHT"."CARRID" | )
                              ( |                   FROM sflight; | )
                              ( |    lt_carrid3 = SELECT MAX( concat( "SFLIGHT"."CARRID", connid ) ) | )
                              ( |                   FROM sflight; | )
                              ( |    lt_carrid4 = SELECT MAX( | )
                              ( |                             ( concat( sflight, connid ) ) | )
                              ( |                           ) | )
                              ( |                   FROM sflight; | )
                              ( |    lt_carrid4 = SELECT MAX( | )
                              ( |                             ( concat( sflight, connid ))| )
                              ( |                           ) | )
                              ( |                   FROM sflight; | )
                              ( |    lt_carrid5 = SELECT concat( sflight, connid ) | )
                              ( |                   FROM sflight; | )
                              ( |    lt_carrid5 = SELECT concat( sflight, connid ) | )
                              ( |                   FROM sflight WITH hint ( no_inline ); | )
                              ( |    lt_carrid5 = SELECT concat( sflight, connid ) | )
                              ( |                   FROM sflight WITH hint ( no_inline | )
                              ( |                                          ); | )
                              ( |    lt_carrid5 = SELECT concat( sflight, connid ) | )
                              ( |                   FROM sflight WITH hint ( no_inline | )
                              ( |                                          ) ; | )
                              ( |    lt_carrid5 = SELECT concat( sflight, connid ) | )
                              ( |                   FROM sflight; | )
                              ( || )
                              ( |*lt_test = select 'Bla Blup Data' from public.dummy; | )
                              ( |    lt_test = SELECT 'Bla Blup Data' | )
                              ( |                FROM public.dummy; | )
                              ( || )
                              ( |    -- Hello World 'keine Ahnung' 'bla blub' | )
                              ( |    lt_test2 = SELECT 'Bla Blup Data' | )
                              ( |                 FROM public.dummy; /*schaun mer mal */ | )
                              ( || )
                              ( |    /* schaun mer| )
                              ( |    noch mehr */ | )
                              ( || )
                              ( |    lt_test3 = SELECT 'Bla Blup Data' | )
                              ( |                 FROM public.dummy; /*schaun mer mal */ | )
                              ( || )
                              ( |    lt_test3 = SELECT 'Bla Blup Data' | )
                              ( |                 FROM public.dummy; -- schaun mer mal| )
                              ( || )
                              ( |    et_exp_part =     SELECT :bal, : | )
                              ( |                             :lt_exp_part."/DTBOM/SAP_OBJ_KEY_REF", | )
                              ( |                             :lt_exp_part."/DTBOM/SAP_OBJ_VRS_REF" | )
                              ( |                        FROM :lt_exp_part | )
                              ( |                  INNER JOIN "/DTBOM/30_IC_MAT_TYP_SNR_USE" AS snrclsp | )
                              ( |                          ON snrclsp.mandt = SESSION_CONTEXT( 'CLIENT') | )
                              ( |                         AND snrclsp.matnr = :lt_exp_part.matnr | )
                              ( |                   UNION ALL | )
                              ( |                      SELECT :lt_exp_part.run_no, | )
                              ( |                             :lt_exp_part."/DTBOM/SAP_OBJ_KEY_REF", | )
                              ( |                             :lt_exp_part."/DTBOM/SAP_OBJ_VRS_REF" | )
                              ( |                        FROM :lt_exp_part | )
                              ( |                       WHERE NOT EXISTS( SELECT 1 | )
                              ( |                                           FROM "/DTBOM/30_IC_MAT_TYP_SNR_USE" AS snrclsp | )
                              ( |                                          WHERE snrclsp.mandt = SESSION_CONTEXT( 'CLIENT') | )
                              ( |                                            AND snrclsp.matnr = :lt_exp_part.matnr | )
                              ( |                                       ); | )
                              ( || )
                              ( |    lt_test4 =          SELECT carrid, connid, fldate | )
                              ( |                          FROM sflight | )
                              ( |                         WHERE sflight.mandt = SESSION_CONTEXT('CLIENT') | )
                              ( |                           AND sflight.connid = 'LH' | )
                              ( |                     UNION ALL | )
                              ( |                        SELECT spfli.carrid, spfli.connid, sflight.fldate | )
                              ( |                          FROM spfli | )
                              ( |               LEFT OUTER JOIN sflight | )
                              ( |                            ON sflight.mandt = spfli.mandt | )
                              ( |                           AND sflight.carrid = spfli.carrid | )
                              ( |                           AND sflight.connid = spfli.connid | )
                              ( |                         WHERE spfli.mandt = SESSION_CONTEXT('CLIENT') | )
                              ( |                           AND spfli.carrid = 'AB' | )
                              ( |                     UNION ALL | )
                              ( |                        SELECT spfli.carrid, spfli.connid, sflight.fldate | )
                              ( |                          FROM spfli | )
                              ( |                    INNER JOIN sflight | )
                              ( |                            ON sflight.mandt = spfli.mandt | )
                              ( |                           AND sflight.carrid = spfli.carrid | )
                              ( |                           AND sflight.connid = spfli.connid | )
                              ( |                         WHERE spfli.mandt = SESSION_CONTEXT('CLIENT') | )
                              ( |                           AND spfli.carrid = 'AB';| )
                              ( || )
                              ( |    lt_spfli1 = SELECT carrid, connid, countryfr, countryto,| )
                              ( |                       ROW_NUMBER ( ) OVER( ORDER BY "CARRID", connid ASC ) AS "ROW_ID"| )
                              ( |                  FROM spfli | )
                              ( |                 WHERE mandt = SESSION_CONTEXT( 'CLIENT' );| )
                              ( || )
                              ( || )
                              ( |    lt_spfli2 = SELECT carrid, connid, countryfr, countryto,| )
                              ( |                       ROW_NUMBER ( ) OVER( PARTITION BY carrid, connid | )
                              ( |                                            ORDER BY "CARRID", connid ASC | )
                              ( |                                          ) AS "ROW_ID"| )
                              ( |                  FROM spfli | )
                              ( |                 WHERE mandt = SESSION_CONTEXT( 'CLIENT' ); | )
                              ( |    --ENDMETHOD. | )
                              ( |  ENDMETHOD.| )
                              ( |ENDCLASS.| ) ).

    CREATE OBJECT lr_cut.
    TRY.
        lt_source_res = lr_cut->pretty_print(
          it_source   = lt_source
          ir_settings = lr_settings ).

      CATCH zcx_app_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lt_source_res
        exp = lt_source_res_exp
        msg = 'Tables differs' ).

  ENDMETHOD.

  METHOD insert_sql.
    DATA lt_source TYPE sourcetable.
    DATA lt_source_res TYPE sourcetable.
    DATA lt_source_res_exp TYPE sourcetable.
    DATA lr_cut TYPE REF TO zcl_app_pretty_printer.
    DATA lr_ex TYPE REF TO zcx_app_exception.
    DATA lr_settings TYPE REF TO zif_app_settings.

    lr_settings ?= cl_abap_testdouble=>create( 'ZIF_APP_SETTINGS' ).
    cl_abap_testdouble=>configure_call( lr_settings )->returning( abap_true ).
    lr_settings->is_line_break_at_comma_req( ).

    lt_source = VALUE #(
                         ( |METHOD INSERT PROCEDURE FOR HDB LANGUAGE SQLSCRIPT| )
                         ( |            USING MARA .| )
                         ( || )
                         ( |  SELECT *    FROM :lt_mara | )
                         ( |  INNER JOIN   :bla AS bla| )
                         ( |        ON :bla.matnr = :lt_mara.matnr; | )
                         ( || )
                         ( |ENDMETHOD.| ) ).

    lt_source_res_exp = VALUE #(
                                 ( |METHOD INSERT PROCEDURE FOR HDB LANGUAGE SQLSCRIPT| )
                                 ( |            USING MARA .| )
                                 ( || )
                                 ( |        SELECT * | )
                                 ( |          FROM :lt_mara | )
                                 ( |    INNER JOIN :bla AS bla | )
                                 ( |            ON :bla.matnr = :lt_mara.matnr; | )
                                 ( || )
                                 ( |ENDMETHOD.| ) ).


    CREATE OBJECT lr_cut.
    TRY.
        lt_source_res = lr_cut->pretty_print(
          it_source   = lt_source
          ir_settings = lr_settings ).

      CATCH zcx_app_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lt_source_res
        exp = lt_source_res_exp
        msg = 'Tables differs' ).

  ENDMETHOD.


  METHOD delete_sql.
    DATA lt_source TYPE sourcetable.
    DATA lt_source_res TYPE sourcetable.
    DATA lt_source_res_exp TYPE sourcetable.
    DATA lr_cut TYPE REF TO zcl_app_pretty_printer.
    DATA lr_ex TYPE REF TO zcx_app_exception.
    DATA lr_settings TYPE REF TO zif_app_settings.

    lr_settings ?= cl_abap_testdouble=>create( 'ZIF_APP_SETTINGS' ).
    cl_abap_testdouble=>configure_call( lr_settings )->returning( abap_true ).
    lr_settings->is_line_break_at_comma_req( ).

    lt_source = VALUE #(
                         ( |METHOD INSERT PROCEDURE FOR HDB LANGUAGE SQLSCRIPT| )
                         ( |            USING BLA .| )
                         ( || )
                         ( |   delete from "BLA" where mandt = :iv_mandt| )
                         ( |     and werks = :iv_werks;| )
                         ( |ENDMETHOD.| ) ).

    lt_source_res_exp = VALUE #(
                                 ( |METHOD INSERT PROCEDURE FOR HDB LANGUAGE SQLSCRIPT| )
                                 ( |            USING BLA .| )
                                 ( || )
                                 ( |    DELETE | )
                                 ( |      FROM "BLA" where mandt = :iv_mandt | )
                                 ( |       AND werks = :iv_werks;| )
                                 ( |ENDMETHOD.| ) ).


    CREATE OBJECT lr_cut.
    TRY.
        lt_source_res = lr_cut->pretty_print(
          it_source   = lt_source
          ir_settings = lr_settings ).

      CATCH zcx_app_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lt_source_res
        exp = lt_source_res_exp
        msg = 'Tables differs' ).

  ENDMETHOD.
  METHOD select_right.
    DATA lt_source TYPE sourcetable.
    DATA lt_source_res TYPE sourcetable.
    DATA lt_source_res_exp TYPE sourcetable.
    DATA lr_cut TYPE REF TO zcl_app_pretty_printer.
    DATA lr_ex TYPE REF TO zcx_app_exception.
    DATA lr_settings TYPE REF TO zif_app_settings.

    lr_settings ?= cl_abap_testdouble=>create( 'ZIF_APP_SETTINGS' ).
    cl_abap_testdouble=>configure_call( lr_settings )->returning( abap_true ).
    lr_settings->is_line_break_at_comma_req( ).

    lt_source = VALUE #(
                     ( |  METHOD sel_data| )
                     ( |  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT| )
                     ( |  OPTIONS READ-ONLY| )
                     ( |  USING sflight.| )
                     ( || )
                     ( |  lt_bla =  SELECT RIGHT( carrid,  4  )  FROM sflight;| )
                     ( || )
                     ( | endmethod. | ) ).

    lt_source_res_exp = VALUE #(
                                 ( |  METHOD sel_data| )
                                 ( |  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT| )
                                 ( |  OPTIONS READ-ONLY| )
                                 ( |  USING sflight.| )
                                 ( || )
                                 ( |    lt_bla = SELECT | )
                                 ( |                    RIGHT( carrid, | )
                                 ( |                           4 | )
                                 ( |                         ) | )
                                 ( |               FROM sflight; | )
                                 ( || )
                                 ( | endmethod. | ) ).

    CREATE OBJECT lr_cut.
    TRY.
        lt_source_res = lr_cut->pretty_print(
          it_source   = lt_source
          ir_settings = lr_settings ).

      CATCH zcx_app_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text( ) ).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lt_source_res
        exp = lt_source_res_exp
        msg = 'Tables differs' ).

  ENDMETHOD.
ENDCLASS.
