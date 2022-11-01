CLASS zcl_app_scanner_sqlscript DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF cos_sqlscript,
                 none           TYPE zapp_d_sqlscript VALUE '',
                 pending        TYPE zapp_d_sqlscript VALUE 'P',
                 end_of_pending TYPE zapp_d_sqlscript VALUE 'E',
                 sqlscript      TYPE zapp_d_sqlscript VALUE 'S',
               END OF cos_sqlscript.
    METHODS set_sqlscript
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
        it_statement      TYPE sstmnt_tab
        it_token_ext      TYPE zapp_t_stokesx_ext_st
      RAISING
        zcx_app_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.



    METHODS is_start_of_sqlscript
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
        it_statement      TYPE sstmnt_tab
        it_token_ext      TYPE zapp_t_stokesx_ext_st
      RETURNING
        VALUE(rv_result)  TYPE abap_bool
      RAISING
        zcx_app_exception.

    METHODS is_end_of_sqlscript_pending
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
        it_statement      TYPE sstmnt_tab
      RETURNING
        VALUE(rv_result)  TYPE abap_bool
      RAISING
        zcx_app_exception.
    METHODS is_sqlscript_still_pending
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_result)  TYPE abap_bool.
    METHODS is_end_of_sqlscript
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_result)  TYPE abap_bool.

    METHODS is_sqlscript
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_result)  TYPE abap_bool.


ENDCLASS.



CLASS zcl_app_scanner_sqlscript IMPLEMENTATION.


  METHOD set_sqlscript.
    DATA lr_statement TYPE REF TO sstmnt.
    IF ir_prev_token_ext IS INITIAL.
      ir_token_ext->sqlscript = cos_sqlscript-none.
      RETURN.
    ENDIF.

    IF is_start_of_sqlscript(
      EXPORTING
        ir_prev_token_ext = ir_prev_token_ext
        ir_token_ext      = ir_token_ext
        it_statement      = it_statement
        it_token_ext      = it_token_ext ) = abap_true.

      zcl_app_utilities=>get_statement_of_token(
        EXPORTING
          it_statement = it_statement
          ir_token_ext = ir_token_ext
        RECEIVING
          rr_statement = lr_statement ).

      IF lr_statement->to = ir_token_ext->org_tab_row.
        ir_token_ext->sqlscript = cos_sqlscript-end_of_pending.
      ELSE.
        ir_token_ext->sqlscript = cos_sqlscript-pending.
      ENDIF.
      RETURN.
    ENDIF.

    IF is_end_of_sqlscript_pending(
      EXPORTING
        ir_prev_token_ext = ir_prev_token_ext
        ir_token_ext      = ir_token_ext
        it_statement      = it_statement ) = abap_true.

      ir_token_ext->sqlscript = cos_sqlscript-end_of_pending.
      RETURN.
    ENDIF.

    IF is_sqlscript_still_pending( ir_prev_token_ext ) = abap_true.

      ir_token_ext->sqlscript = cos_sqlscript-pending.
      RETURN.
    ENDIF.

    IF is_end_of_sqlscript(
         ir_prev_token_ext = ir_prev_token_ext
         ir_token_ext      = ir_token_ext ) = abap_true.

      ir_token_ext->sqlscript = cos_sqlscript-none.
      RETURN.
    ENDIF.

    IF is_sqlscript( ir_prev_token_ext ) = abap_true.

      ir_token_ext->sqlscript = cos_sqlscript-sqlscript.
      RETURN.
    ENDIF.

    ir_token_ext->sqlscript = cos_sqlscript-none.

  ENDMETHOD.

  METHOD is_start_of_sqlscript.
    DATA lr_statement TYPE REF TO sstmnt.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_scanner_comment TYPE REF TO zcl_app_scanner_comment.

    IF ir_prev_token_ext IS INITIAL.
      RETURN.
    ENDIF.

    IF ir_prev_token_ext->sqlscript <> cos_sqlscript-none
       OR zcl_app_utilities=>is_comment( ir_prev_token_ext->comment ) = abap_true.
      RETURN.
    ENDIF.

    CREATE OBJECT lr_scanner_comment.
    IF lr_scanner_comment->is_comment_abap( ir_token_ext = ir_token_ext ) = abap_true.
      RETURN.
    ENDIF.

    IF zcl_app_utilities=>is_str_eq_upper_case(
      EXPORTING
        iv_string1 = ir_token_ext->str
        iv_string2 = 'SQLSCRIPT' ) = abap_false.
      RETURN.
    ENDIF.

    zcl_app_utilities=>get_statement_of_token(
      EXPORTING
        it_statement = it_statement
        ir_token_ext = ir_token_ext
      RECEIVING
        rr_statement = lr_statement ).

    READ TABLE it_token_ext
    REFERENCE INTO lr_token_ext
    WITH TABLE KEY org_tab_row = lr_statement->from ##WARN_OK.
    IF sy-subrc = 0.

      IF zcl_app_utilities=>is_str_eq_upper_case(
        EXPORTING
          iv_string1 = lr_token_ext->str
          iv_string2 = 'METHOD' ) = abap_true.

        rv_result = abap_true.
        RETURN.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD is_end_of_sqlscript_pending.
    DATA: lr_statement TYPE REF TO sstmnt.
    DATA lr_scanner_comment TYPE REF TO zcl_app_scanner_comment.

    IF ir_prev_token_ext->sqlscript <> cos_sqlscript-pending.
      RETURN.
    ENDIF.

    CREATE OBJECT lr_scanner_comment.
    IF lr_scanner_comment->is_comment_abap( ir_token_ext = ir_token_ext ) = abap_true.
      RETURN.
    ENDIF.

    zcl_app_utilities=>get_statement_of_token(
      EXPORTING
        it_statement = it_statement
        ir_token_ext = ir_token_ext
      RECEIVING
        rr_statement = lr_statement ).

    IF lr_statement->to = ir_token_ext->org_tab_row.
      rv_result = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD is_sqlscript_still_pending.
    IF ir_prev_token_ext->sqlscript = cos_sqlscript-pending.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD is_end_of_sqlscript.
    DATA lr_scanner_comment TYPE REF TO zcl_app_scanner_comment.
    IF ir_prev_token_ext->sqlscript <> cos_sqlscript-sqlscript.
      RETURN.
    ENDIF.

    CREATE OBJECT lr_scanner_comment.
    IF lr_scanner_comment->is_comment_sqlscript(
      EXPORTING
        ir_prev_token_ext = ir_prev_token_ext
        ir_token_ext      = ir_token_ext ) = abap_true.
      RETURN.
    ENDIF.

    IF zcl_app_utilities=>is_str_eq_upper_case(
            EXPORTING
              iv_string1 = ir_token_ext->str
              iv_string2 = 'ENDMETHOD' ) = abap_true.
      rv_result = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.




  METHOD is_sqlscript.
    CASE  ir_prev_token_ext->sqlscript.
      WHEN cos_sqlscript-sqlscript
        OR cos_sqlscript-end_of_pending.

        rv_result = abap_true.
        RETURN.

    ENDCASE.
  ENDMETHOD.

ENDCLASS.
