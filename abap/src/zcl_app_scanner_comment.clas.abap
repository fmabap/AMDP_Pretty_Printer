CLASS zcl_app_scanner_comment DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS: BEGIN OF cos_comment,
                 none        TYPE zapp_d_comment VALUE '',
                 multi_line  TYPE zapp_d_comment VALUE 'M',
                 single_line TYPE zapp_d_comment VALUE 'S',
               END OF cos_comment.

    CONSTANTS: BEGIN OF cos_comment_detail,
                 none                           TYPE zapp_d_comment_detail VALUE '',
                 start                          TYPE zapp_d_comment_detail VALUE 'S',
                 part                           TYPE zapp_d_comment_detail VALUE 'P',
                 start_begin_of_line            TYPE zapp_d_comment_detail VALUE 'L',
                 start_begin_of_line_intendable TYPE zapp_d_comment_detail VALUE 'I',
               END OF cos_comment_detail.
    METHODS is_comment_abap
      IMPORTING
        ir_token_ext     TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_comment_sqlscript
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_result)  TYPE abap_bool.
    METHODS set_comment
      IMPORTING
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext.




  PROTECTED SECTION.
  PRIVATE SECTION.



    METHODS set_comment_abap
      IMPORTING
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext.

    METHODS set_comment_sqlscript
      IMPORTING
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext.

    METHODS is_closing_sql_multi_line_com
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_result)  TYPE abap_bool.

    METHODS is_part_of_sql_single_line_com
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_result)  TYPE abap_bool.

    METHODS is_start_sql_single_line_com
      IMPORTING
        ir_token_ext     TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_start_sql_multi_line_com
      IMPORTING
        ir_token_ext     TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_part_of_sql_multi_line_com
      IMPORTING
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
      RETURNING
        VALUE(rv_result)  TYPE abap_bool.

    METHODS set_sql_comment_detail_start
      IMPORTING
        ir_token_ext      TYPE REF TO zapp_s_stokesx_ext
        ir_prev_token_ext TYPE REF TO zapp_s_stokesx_ext.

    METHODS set_sql_start_being_of_line
      IMPORTING
        ir_token_ext TYPE REF TO zapp_s_stokesx_ext.

    METHODS set_abap_start_being_of_line
      IMPORTING
        ir_token_ext TYPE REF TO zapp_s_stokesx_ext.
ENDCLASS.



CLASS zcl_app_scanner_comment IMPLEMENTATION.

  METHOD set_comment.
    ir_token_ext->comment = cos_comment-none.

    IF zcl_app_utilities=>is_abap_token( ir_token_ext->sqlscript ) = abap_true.
      set_comment_abap(
        EXPORTING
          ir_token_ext      = ir_token_ext
          ir_prev_token_ext = ir_prev_token_ext ).

    ELSE.
      set_comment_sqlscript(
        EXPORTING
          ir_token_ext      = ir_token_ext
          ir_prev_token_ext = ir_prev_token_ext ).
    ENDIF.
  ENDMETHOD.

  METHOD set_comment_abap.
    IF is_comment_abap( ir_token_ext ) = abap_true.
      ir_token_ext->comment = cos_comment-single_line.
      IF ir_token_ext->col = 0 AND ir_token_ext->str_up(1) = '*'.
        set_abap_start_being_of_line( ir_token_ext ).
        RETURN.
      ENDIF.
      IF ir_prev_token_ext IS INITIAL.
        set_abap_start_being_of_line( ir_token_ext ).
        RETURN.
      ENDIF.

      IF is_comment_abap( ir_prev_token_ext ) = abap_true
      AND zcl_app_utilities=>is_abap_token( ir_prev_token_ext->sqlscript ) = abap_true
      AND ir_token_ext->row = ir_prev_token_ext->row.
        ir_token_ext->comment_detail = cos_comment_detail-part.
        RETURN.
      ENDIF.
      ir_token_ext->comment_detail = cos_comment_detail-start.
    ENDIF.
  ENDMETHOD.

  METHOD set_comment_sqlscript.

    IF is_part_of_sql_multi_line_com(
            ir_prev_token_ext =  ir_prev_token_ext
            ir_token_ext      = ir_token_ext ) = abap_true.

      ir_token_ext->comment = cos_comment-multi_line.
      ir_token_ext->comment_detail = cos_comment_detail-part.
      RETURN.
    ENDIF.

    IF is_part_of_sql_single_line_com(
         ir_prev_token_ext =  ir_prev_token_ext
         ir_token_ext      = ir_token_ext ) = abap_true.

      ir_token_ext->comment = cos_comment-single_line.
      ir_token_ext->comment_detail = cos_comment_detail-part.
      RETURN.
    ENDIF.

    IF is_start_sql_single_line_com( ir_token_ext ) = abap_true.
      ir_token_ext->comment = cos_comment-single_line.
      set_sql_comment_detail_start(
        EXPORTING
          ir_token_ext      = ir_token_ext
          ir_prev_token_ext = ir_prev_token_ext ).
    ENDIF.

    IF is_start_sql_multi_line_com( ir_token_ext = ir_token_ext ) = abap_true.
      ir_token_ext->comment = cos_comment-multi_line.
      set_sql_comment_detail_start(
        EXPORTING
          ir_token_ext      = ir_token_ext
          ir_prev_token_ext = ir_prev_token_ext ).
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD is_closing_sql_multi_line_com.
    IF ir_prev_token_ext IS INITIAL.
      RETURN.
    ENDIF.
    IF ( ir_prev_token_ext->comment <> cos_comment-multi_line ).
      RETURN.
    ENDIF.
    IF strlen( ir_prev_token_ext->str ) >= 2.
      IF ir_prev_token_ext->str(2) = '*/'.
        rv_result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD is_start_sql_single_line_com.

    IF strlen( ir_token_ext->str ) >= 2.
      IF ir_token_ext->str(2) = '--'.
        rv_result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

    IF strlen( ir_token_ext->str ) >= 1
    AND ir_token_ext->col = 0.
      IF ir_token_ext->str(1) = '*'.
        rv_result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD is_part_of_sql_single_line_com.
    IF ir_prev_token_ext IS INITIAL.
      RETURN.
    ENDIF.
    IF ir_prev_token_ext->row = ir_token_ext->row
        AND ir_prev_token_ext->comment = cos_comment-single_line.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD is_start_sql_multi_line_com.
    IF strlen( ir_token_ext->str ) >= 2.
      IF ir_token_ext->str(2) = '/*'.
        rv_result = abap_true.
        RETURN.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD is_comment_abap.
    IF ir_token_ext->type = 'C'.
      rv_result = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD is_comment_sqlscript.

    IF is_part_of_sql_multi_line_com(
         ir_prev_token_ext = ir_prev_token_ext
         ir_token_ext      = ir_token_ext ) = abap_true.

      rv_result = abap_true.
      RETURN.
    ENDIF.


    IF is_part_of_sql_single_line_com(
         ir_prev_token_ext =  ir_prev_token_ext
         ir_token_ext      = ir_token_ext ) = abap_true.

      rv_result = abap_true.
      RETURN.
    ENDIF.

    IF is_start_sql_single_line_com( ir_token_ext = ir_token_ext ) = abap_true.
      rv_result = abap_true.
      RETURN.
    ENDIF.

    IF is_start_sql_multi_line_com( ir_token_ext = ir_token_ext ) = abap_true.
      rv_result = abap_true.
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD is_part_of_sql_multi_line_com.
    IF ir_prev_token_ext IS INITIAL.
      RETURN.
    ENDIF.

    IF ir_prev_token_ext->comment <> cos_comment-multi_line.
      RETURN.
    ENDIF.

    IF is_closing_sql_multi_line_com( ir_prev_token_ext ) = abap_true.
      RETURN.
    ENDIF.

    rv_result = abap_true.

  ENDMETHOD.

  METHOD set_sql_comment_detail_start.
    IF ir_token_ext->col = 0.
      set_sql_start_being_of_line( ir_token_ext ).
      RETURN.
    ENDIF.
    IF NOT ir_prev_token_ext IS INITIAL.
      IF ir_token_ext->row <> ir_prev_token_ext->row.
        set_sql_start_being_of_line( ir_token_ext ).
        RETURN.
      ELSE.
        ir_token_ext->comment_detail = cos_comment_detail-start.
        RETURN.
      ENDIF.
    ELSE.
      set_sql_start_being_of_line( ir_token_ext ).
      RETURN.
    ENDIF.

  ENDMETHOD.

  METHOD set_sql_start_being_of_line.
    IF ir_token_ext->str_up(1) = '*'.
      ir_token_ext->comment_detail = cos_comment_detail-start_begin_of_line.
    ELSE.
      ir_token_ext->comment_detail = cos_comment_detail-start_begin_of_line_intendable.
    ENDIF.
  ENDMETHOD.
  METHOD set_abap_start_being_of_line.
    IF ir_token_ext->str_up(1) = '*'.
      ir_token_ext->comment_detail = cos_comment_detail-start_begin_of_line.
    ELSE.
      ir_token_ext->comment_detail = cos_comment_detail-start_begin_of_line_intendable.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
