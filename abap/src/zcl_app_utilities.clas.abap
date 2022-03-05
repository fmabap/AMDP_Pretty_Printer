"! <p class="shorttext synchronized" lang="en">Pretty Printer Utilities</p>
CLASS zcl_app_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_prev_token_ext
      IMPORTING it_token_ext     TYPE zapp_t_stokesx_ext_st
                ir_token_ext     TYPE REF TO zapp_s_stokesx_ext
      RETURNING VALUE(rr_result) TYPE REF TO zapp_s_stokesx_ext.


    CLASS-METHODS get_next_token_ext
      IMPORTING it_token_ext     TYPE zapp_t_stokesx_ext_st
                ir_token_ext     TYPE REF TO zapp_s_stokesx_ext
      RETURNING VALUE(rr_result) TYPE REF TO zapp_s_stokesx_ext.

    CLASS-METHODS get_statement_of_token
      IMPORTING it_statement        TYPE sstmnt_tab
                ir_token_ext        TYPE REF TO zapp_s_stokesx_ext
      RETURNING VALUE(rr_statement) TYPE REF TO sstmnt
      RAISING   zcx_app_exception.

    CLASS-METHODS is_str_eq_upper_case
      IMPORTING iv_string1       TYPE string
                iv_string2       TYPE string
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_abap_token
      IMPORTING iv_sqlscript     TYPE zapp_d_sqlscript
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_sqlscript_token
      IMPORTING iv_sqlscript     TYPE zapp_d_sqlscript
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_sqlscript_rule
      IMPORTING ir_rule          TYPE REF TO zif_app_rule
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_abap_rule
      IMPORTING ir_rule          TYPE REF TO zif_app_rule
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_comment
      IMPORTING iv_comment       TYPE zapp_d_comment
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_comment_rule
      IMPORTING ir_rule          TYPE REF TO zif_app_rule
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS set_to_0_if_negativ
      CHANGING cv_value TYPE i.

    CLASS-METHODS contains_delimiter_char
      IMPORTING it_delimiter     TYPE zapp_t_delimiter
                iv_char          TYPE char1
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS contains_delimiter_only_space
      IMPORTING it_delimiter     TYPE zapp_t_delimiter
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_delimiter_initial
      IMPORTING it_delimiter     TYPE zapp_t_delimiter
      RETURNING VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS get_space_as_delimiter
      RETURNING VALUE(rt_result) TYPE zapp_t_delimiter.

    CLASS-METHODS conv_source_to_statement
      IMPORTING it_source        TYPE sourcetable
                iv_var           TYPE string DEFAULT 'lt_source'
      RETURNING VALUE(rt_result) TYPE sourcetable.

    CLASS-METHODS conv_source_tab_to_string
      IMPORTING it_source        TYPE sourcetable
      RETURNING VALUE(rv_result) TYPE string.

    CLASS-METHODS get_token_length_wo_delimiter
      IMPORTING ir_rule          TYPE REF TO zif_app_rule
      RETURNING VALUE(rv_result) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_APP_UTILITIES IMPLEMENTATION.


  METHOD contains_delimiter_char.
    DATA lr_delimiter TYPE REF TO string.
    LOOP AT it_delimiter REFERENCE INTO lr_delimiter.
      IF lr_delimiter->* CA iv_char.
        rv_result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD contains_delimiter_only_space.
    DATA lr_delimiter TYPE REF TO string.
    rv_result = abap_true.
    LOOP AT it_delimiter REFERENCE INTO lr_delimiter.
      IF lr_delimiter->* IS NOT INITIAL
      AND NOT lr_delimiter->* CO ` `.
        rv_result = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD conv_source_tab_to_string.
    DATA lr_source TYPE REF TO string.

    LOOP AT it_source REFERENCE INTO lr_source.
      IF sy-tabix = 1.
        rv_result = lr_source->*.
      ELSE.
        CONCATENATE rv_result  cl_abap_char_utilities=>cr_lf lr_source->* INTO rv_result RESPECTING BLANKS.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD conv_source_to_statement.
    DATA lv_source TYPE string.
    DATA lr_source TYPE REF TO string.
    DATA lv_len TYPE i.
    DATA lv_spaces TYPE string.
    DATA lv_lines TYPE i.

    lv_source = iv_var.
    CONCATENATE lv_source ' = VALUE #( ' INTO lv_source RESPECTING BLANKS.
    INSERT lv_source INTO TABLE rt_result.
    DO strlen( lv_source ) TIMES.
      CONCATENATE lv_spaces ` ` INTO lv_spaces.
    ENDDO.

    lv_lines = lines( it_source ).
    LOOP AT it_source REFERENCE INTO lr_source.
      CONCATENATE lv_spaces '( |' lr_source->* '| )' INTO lv_source RESPECTING BLANKS.
      IF sy-tabix = lv_lines.
        CONCATENATE lv_source ' ).' INTO lv_source RESPECTING BLANKS.
      ENDIF.
      INSERT lv_source INTO TABLE rt_result.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_next_token_ext.
    READ TABLE it_token_ext TRANSPORTING NO FIELDS
           WITH TABLE KEY row_col
           COMPONENTS  row = ir_token_ext->row
                       col = ir_token_ext->col.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    READ TABLE it_token_ext REFERENCE INTO rr_result
         INDEX sy-tabix + 1
         USING KEY row_col.

  ENDMETHOD.


  METHOD get_prev_token_ext.
    DATA lv_prev_index   TYPE sytabix.


    READ TABLE it_token_ext TRANSPORTING NO FIELDS
         WITH TABLE KEY row_col
         COMPONENTS  row = ir_token_ext->row
                     col = ir_token_ext->col.
    IF sy-subrc <> 0 OR sy-tabix < 2.
      RETURN.
    ENDIF.

    READ TABLE it_token_ext REFERENCE INTO rr_result
         INDEX sy-tabix - 1
         USING KEY row_col.

  ENDMETHOD.


  METHOD get_space_as_delimiter.
    DATA lv_delimiter TYPE string.
    lv_delimiter = ` `.
    INSERT lv_delimiter INTO TABLE rt_result.
  ENDMETHOD.


  METHOD get_statement_of_token.
    LOOP AT it_statement REFERENCE INTO rr_statement
    WHERE from <= ir_token_ext->org_tab_row
      AND to >= ir_token_ext->org_tab_row.
      EXIT.
    ENDLOOP.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '003'
        WITH ir_token_ext->org_tab_row.
    ENDIF.
  ENDMETHOD.


  METHOD get_token_length_wo_delimiter.
    rv_result = strlen( ir_rule->get_token_up( ) ).
  ENDMETHOD.


  METHOD is_abap_rule.
    DATA lr_token_ext     TYPE REF TO zapp_s_stokesx_ext.
    IF ir_rule IS INITIAL.
      RETURN.
    ENDIF.
    lr_token_ext = ir_rule->get_token_ext( ).
    rv_result = is_abap_token( lr_token_ext->sqlscript ).

  ENDMETHOD.


  METHOD is_abap_token.
    CASE iv_sqlscript.
      WHEN zcl_app_scanner_sqlscript=>cos_sqlscript-none
      OR  zcl_app_scanner_sqlscript=>cos_sqlscript-pending
      OR  zcl_app_scanner_sqlscript=>cos_sqlscript-end_of_pending.
        rv_result = abap_true.
    ENDCASE.
  ENDMETHOD.


  METHOD is_comment.
    IF iv_comment <> zcl_app_scanner_comment=>cos_comment-none.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_comment_rule.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    IF ir_rule IS INITIAL.
      RETURN.
    ENDIF.
    rv_result = ir_rule->is_comment(  ).

  ENDMETHOD.


  METHOD is_delimiter_initial.
    DATA lr_delimiter TYPE REF TO string.
    rv_result = abap_true.
    LOOP AT it_delimiter REFERENCE INTO lr_delimiter.
      IF lr_delimiter->* IS NOT INITIAL.
        rv_result = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_sqlscript_rule.
    DATA lr_token_ext     TYPE REF TO zapp_s_stokesx_ext.
    IF ir_rule IS INITIAL.
      RETURN.
    ENDIF.
    lr_token_ext = ir_rule->get_token_ext( ).
    rv_result = is_sqlscript_token( lr_token_ext->sqlscript ).

  ENDMETHOD.


  METHOD is_sqlscript_token.
    IF iv_sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD is_str_eq_upper_case.
    DATA lv_string1 TYPE string.
    DATA lv_string2 TYPE string.

    lv_string1 = iv_string1.
    TRANSLATE lv_string1 TO UPPER CASE.

    lv_string2 = iv_string2.
    TRANSLATE lv_string2 TO UPPER CASE.

    IF lv_string1 = lv_string2.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD set_to_0_if_negativ.
    IF cv_value < 0.
      cv_value = 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
