*"* use this source file for your ABAP unit test classes
CLASS lcl_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      scan_test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS lcl_test IMPLEMENTATION.

  METHOD scan_test.

    DATA lt_source TYPE sourcetable.
    DATA lr_cut TYPE REF TO zcl_app_scanner.
    DATA lr_ex TYPE REF TO zcx_app_exception.

    lt_source = VALUE #( ( |CLASS zcl_app_test_amdp DEFINITION| )
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
                         ( |  USING sflight. -- Flight| )
                         ( || )
                         ( |    lt_sflight = select * from sflight;| )
                         ( || )
                         ( |    lt_carrid = select "CARRID" from sflight;| )
                         ( || )
                         ( |    lt_carrid2 = select "SFLIGHT"."CARRID" from sflight;| )
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
                         ( |    --ENDMETHOD.| )
                         ( |  ENDMETHOD.| )
                         ( |ENDCLASS.| ) ).

    CREATE OBJECT lr_cut.
    TRY.
        lr_cut->scan(
          EXPORTING
            it_source = lt_source
        ).
      CATCH zcx_app_exception INTO lr_ex.
        cl_abap_unit_assert=>fail( lr_ex->get_text( ) ).
    ENDTRY.
    "cl_abap_unit_assert=>fail( 'Implement your first test here' ).
  ENDMETHOD.

ENDCLASS.
