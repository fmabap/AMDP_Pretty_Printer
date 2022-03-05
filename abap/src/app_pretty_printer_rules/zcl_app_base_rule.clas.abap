CLASS zcl_app_base_rule DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_app_rule .
    ALIASES: is_comment FOR zif_app_rule~is_comment.

  PROTECTED SECTION.
    DATA mr_token_ext  TYPE REF TO zapp_s_stokesx_ext.
    DATA mr_context_rule  TYPE REF TO zif_app_rule .
    DATA mr_hl_context_rule  TYPE REF TO zif_app_rule.
    DATA mr_prev_rule  TYPE REF TO zif_app_rule.
    DATA mr_next_rule  TYPE REF TO zif_app_rule.
    DATA mr_t_source  TYPE REF TO sourcetable.
    DATA mr_t_statement  TYPE REF TO sstmnt_tab.
    DATA mr_t_structure  TYPE REF TO sstruc_tab .
    DATA mr_rule_data TYPE REF TO zapp_s_rule.
    DATA mv_default_line_intend TYPE i.
    DATA mv_cur_offset_start_set TYPE abap_bool.
    DATA mv_cur_offset_start TYPE i.
    DATA mv_cur_offset_end_set TYPE abap_bool.
    DATA mv_cur_offset_end TYPE i.
    DATA mv_cur_row_set TYPE abap_bool.
    DATA mv_cur_row TYPE i.
    DATA mv_end_row_set TYPE abap_bool.
    DATA mv_end_row TYPE i.
    DATA mv_add_intend TYPE i.

    METHODS set_cur_offset_end
      IMPORTING iv_cur_offset_end TYPE i.

    METHODS set_end_row
      IMPORTING iv_end_row TYPE i.

    METHODS has_prev_rule_same_type
      RETURNING VALUE(rv_result) TYPE abap_bool.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_base_rule IMPLEMENTATION.


  METHOD has_prev_rule_same_type.
    IF mr_prev_rule IS NOT INITIAL
    AND  zcl_app_utilities=>is_sqlscript_rule( me ) = zcl_app_utilities=>is_sqlscript_rule( mr_prev_rule ).

      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD set_cur_offset_end.
    mv_cur_offset_end = iv_cur_offset_end.
    mv_cur_offset_end_set = abap_true.
  ENDMETHOD.


  METHOD zif_app_rule~set_cur_offset_start.
    mv_cur_offset_start = iv_cur_offset_start.
    mv_cur_offset_start_set = abap_true.
  ENDMETHOD.


  METHOD zif_app_rule~set_cur_row.
    mv_cur_row = iv_cur_row.
    mv_cur_row_set = abap_true.
  ENDMETHOD.


  METHOD set_end_row.
    mv_end_row = iv_end_row.
    mv_end_row_set = abap_true.
  ENDMETHOD.


  METHOD zif_app_rule~finalize_init.
    RETURN.
  ENDMETHOD.


  METHOD zif_app_rule~get_context_rule.
    rr_result = mr_context_rule.
  ENDMETHOD.


  METHOD zif_app_rule~get_cur_offset_end.
    DATA lv_offset TYPE i.
    DATA lv_len TYPE i.
    DATA lt_text TYPE sourcetable.
    DATA lr_text TYPE REF TO string.

    IF mv_cur_offset_end_set = abap_true.
      rv_result = mv_cur_offset_end.
      RETURN.
    ENDIF.

    lv_offset = zif_app_rule~get_cur_offset_start(  ).
    lt_text = zif_app_rule~get_text(  ).

    IF lines( lt_text ) = 1.

      READ TABLE lt_text
      REFERENCE INTO lr_text
      INDEX 1.
      IF sy-subrc = 0.
        lv_len = strlen( lr_text->* ).
        rv_result = lv_offset + lv_len.
      ENDIF.
    ELSE.

      READ TABLE lt_text
      REFERENCE INTO lr_text
      INDEX lines( lt_text ).
      IF sy-subrc = 0.
        rv_result = strlen(  lr_text->* ).
      ENDIF.

    ENDIF.
    set_cur_offset_end( rv_result ).

  ENDMETHOD.


  METHOD zif_app_rule~get_cur_offset_start.

    IF mv_cur_offset_start_set = abap_true.
      rv_result = mv_cur_offset_start.
      RETURN.
    ENDIF.

    IF mr_token_ext->comment_detail = zcl_app_scanner_comment=>cos_comment_detail-start_begin_of_line.
      rv_result = 0.
      zif_app_rule~set_cur_offset_start( rv_result ).
      RETURN.
    ENDIF.

    IF mr_prev_rule IS INITIAL
    OR ( has_prev_rule_same_type(  ) = abap_false
         AND mr_prev_rule->get_cur_row( ) <> zif_app_rule~get_cur_row(  )
       ).
      rv_result = mv_default_line_intend + mv_add_intend.
      zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
      zif_app_rule~set_cur_offset_start( rv_result ).
      RETURN.
    ENDIF.

    CASE mr_token_ext->comment_detail.
      WHEN zcl_app_scanner_comment=>cos_comment_detail-start_begin_of_line_intendable.

        IF mr_prev_rule->has_multline_delimiter( ) = abap_true.
          rv_result = mr_prev_rule->get_cur_offset_end( ) + mv_add_intend.
          zif_app_rule~set_cur_offset_start( rv_result ).
          RETURN.
        ENDIF.

        rv_result = mr_prev_rule->get_new_line_intend( ).
        zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
        rv_result = rv_result + mv_add_intend.
        zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
        zif_app_rule~set_cur_offset_start( rv_result ).
        RETURN.

      WHEN zcl_app_scanner_comment=>cos_comment_detail-start
        OR zcl_app_scanner_comment=>cos_comment_detail-part.

        rv_result = mr_prev_rule->get_cur_offset_end( ) + mv_add_intend.
        zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
        zif_app_rule~set_cur_offset_start( rv_result ).
        RETURN.

    ENDCASE.

    IF mr_prev_rule->is_new_line_req( ) = abap_true
     AND mr_prev_rule->has_multline_delimiter( ) = abap_false
     AND zif_app_rule~is_line_breaking_token( ) = abap_false.
      rv_result = mr_prev_rule->get_new_line_intend( ).
    ELSE.
      rv_result = mr_prev_rule->get_cur_offset_end( ).
    ENDIF.

    zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
    rv_result = rv_result + mv_add_intend.
    zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
    zif_app_rule~set_cur_offset_start( rv_result ).
  ENDMETHOD.


  METHOD zif_app_rule~get_cur_row.

    IF mv_cur_row_set = abap_true.
      rv_result = mv_cur_row.
      RETURN.
    ENDIF.

    IF mr_prev_rule IS INITIAL.
      rv_result = 1.
      zif_app_rule~set_cur_row( rv_result ).
      RETURN.
    ENDIF.

    CASE mr_token_ext->comment_detail.
      WHEN zcl_app_scanner_comment=>cos_comment_detail-start_begin_of_line
        OR zcl_app_scanner_comment=>cos_comment_detail-start_begin_of_line_intendable.
        IF mr_prev_rule->has_multline_delimiter( ) = abap_true.
          rv_result = mr_prev_rule->get_end_row( ).
        ELSE.
          rv_result = mr_prev_rule->get_end_row( ) + 1.
        ENDIF.
        zif_app_rule~set_cur_row( rv_result ).
        RETURN.
      WHEN zcl_app_scanner_comment=>cos_comment_detail-start
       OR zcl_app_scanner_comment=>cos_comment_detail-part.
        rv_result = mr_prev_rule->get_end_row( ).
        zif_app_rule~set_cur_row( rv_result ).
        RETURN.
    ENDCASE.

    IF mr_prev_rule->is_new_line_req( ) = abap_true
       AND zif_app_rule~is_line_breaking_token( ) = abap_false.

      IF mr_prev_rule->has_multline_delimiter( ) = abap_false.
        rv_result = mr_prev_rule->get_end_row( ) + 1.
        zif_app_rule~set_cur_row( rv_result ).
        RETURN.
      ENDIF.
    ENDIF.

    rv_result = mr_prev_rule->get_end_row( ).
    zif_app_rule~set_cur_row( rv_result ).
  ENDMETHOD.


  METHOD zif_app_rule~get_end_row.

    DATA lt_text TYPE sourcetable.

    IF mv_end_row_set = abap_true.
      rv_result = mv_end_row.
      RETURN.
    ENDIF.

    rv_result = zif_app_rule~get_cur_row( ).
    lt_text = zif_app_rule~get_text( ).
    IF lines( lt_text ) > 1.
      rv_result = rv_result + lines( lt_text ) - 1.
    ENDIF.
    set_end_row( iv_end_row = rv_result ).
  ENDMETHOD.


  METHOD zif_app_rule~get_hl_context_rule.
    rr_result = mr_hl_context_rule.
  ENDMETHOD.


  METHOD zif_app_rule~get_new_context.
    DATA lr_context_rule TYPE REF TO zif_app_rule.
    lr_context_rule =  zif_app_rule~get_new_context_rule( ).
    IF NOT lr_context_rule IS INITIAL.
      rv_result = lr_context_rule->get_new_context( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_app_rule~get_new_context_rule.
    rr_result = mr_context_rule.
  ENDMETHOD.


  METHOD zif_app_rule~get_new_hl_context.
    DATA lr_hl_context_rule TYPE REF TO zif_app_rule.
    lr_hl_context_rule =  zif_app_rule~get_new_hl_context_rule( ).
    IF NOT lr_hl_context_rule IS INITIAL.
      rv_result = lr_hl_context_rule->get_new_context( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_app_rule~get_new_hl_context_rule.
    rr_result = mr_hl_context_rule.
  ENDMETHOD.


  METHOD zif_app_rule~get_new_line_intend.
    IF has_prev_rule_same_type(  ) = abap_true.
      IF zif_app_rule~is_end_of_statement( ) = abap_true.
        rv_result = mr_prev_rule->get_new_statement_intend( ).
      ELSE.
        rv_result = mr_prev_rule->get_new_line_intend(  ).

      ENDIF.
    ELSE.
      rv_result = mv_default_line_intend.
    ENDIF.
    rv_result = rv_result + mr_rule_data->new_line_intend_diff.
    zcl_app_utilities=>set_to_0_if_negativ(
      CHANGING
        cv_value = rv_result
    ).

  ENDMETHOD.


  METHOD zif_app_rule~get_new_statement_intend.
    IF has_prev_rule_same_type(  ) = abap_true.
      rv_result = mr_prev_rule->get_new_statement_intend( ).
    ELSE.
      rv_result = mv_default_line_intend.
    ENDIF.
    rv_result = rv_result + mr_rule_data->new_statement_intend_diff.
  ENDMETHOD.


  METHOD zif_app_rule~get_next_rule.
    rr_result = mr_next_rule.
  ENDMETHOD.


  METHOD zif_app_rule~get_prev_rule.
    rr_result = mr_prev_rule.
  ENDMETHOD.


  METHOD zif_app_rule~get_rule_data.
    rr_result = mr_rule_data.
  ENDMETHOD.


  METHOD zif_app_rule~get_text.
    DATA lr_delimiter TYPE REF TO string.
    DATA lr_result TYPE REF TO string.
    IF mr_token_ext->delimiter IS INITIAL.
      INSERT mr_token_ext->str  INTO TABLE rt_result.
      RETURN.
    ENDIF.
    LOOP AT mr_token_ext->delimiter REFERENCE INTO lr_delimiter.
      INSERT INITIAL LINE INTO TABLE rt_result REFERENCE INTO lr_result.
      IF sy-tabix = 1.
        CONCATENATE mr_token_ext->str lr_delimiter->* INTO lr_result->*.
      ELSE.
        lr_result->* = lr_delimiter->*.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_app_rule~get_token_ext.
    rr_result = mr_token_ext.
  ENDMETHOD.


  METHOD zif_app_rule~get_token_up.
    rv_result = mr_token_ext->str_up.
  ENDMETHOD.


  METHOD zif_app_rule~has_multline_delimiter.
    IF lines( mr_token_ext->delimiter ) >= 2.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_app_rule~init.
    mr_token_ext = ir_token_ext.
    mr_context_rule =  ir_context_rule.
    mr_hl_context_rule = ir_hl_context_rule.
    mr_prev_rule = ir_prev_rule.
    mr_t_source = ir_t_source.
    mr_t_statement = ir_t_statement.
    mr_t_structure = ir_t_structure.
    mr_rule_data = ir_rule_data.
  ENDMETHOD.


  METHOD zif_app_rule~is_comment.
    rv_result = zcl_app_utilities=>is_comment( iv_comment = mr_token_ext->comment ).
  ENDMETHOD.


  METHOD zif_app_rule~is_end_of_statement.
    DATA lr_delimiter TYPE REF TO string.
*    DATA lr_next_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lv_statement_end TYPE char1.


    IF is_comment( ) = abap_true.
      IF mr_prev_rule IS NOT INITIAL.
        rv_result = mr_prev_rule->is_end_of_statement( ).
        RETURN.
      ENDIF.
    ENDIF.

*
*      IF mr_next_rule IS NOT INITIAL.
*        lr_next_token_ext = mr_next_rule->get_token_ext(  ).
*        IF lr_next_token_ext->comment_detail <> zcl_app_scanner_comment=>cos_comment_detail-part.
*          rv_result = abap_true.
*        ENDIF.
*      ENDIF.
*      RETURN.
*    ENDIF.

    IF zcl_app_utilities=>is_abap_token( mr_token_ext->sqlscript ) = abap_true.
      lv_statement_end = '.'.
    ELSEIF zcl_app_utilities=>is_sqlscript_token( mr_token_ext->sqlscript ) = abap_true.
      lv_statement_end = ';'.
    ELSE.
      RETURN.
    ENDIF.

    IF zif_app_rule~get_token_up(  ) = lv_statement_end.
      rv_result = abap_true.
      RETURN.
    ENDIF.

    IF mr_token_ext->delimiter IS INITIAL.
      RETURN.
    ENDIF.

    rv_result = zcl_app_utilities=>contains_delimiter_char(
      it_delimiter = mr_token_ext->delimiter
      iv_char      = lv_statement_end
    ).

  ENDMETHOD.


  METHOD zif_app_rule~is_keyword.

  ENDMETHOD.


  METHOD zif_app_rule~is_line_breaking_token.
    IF is_comment( ) = abap_true.
      RETURN.
    ENDIF.
    IF zcl_app_utilities=>is_abap_token( mr_token_ext->sqlscript ).

      CASE zif_app_rule~get_token_up(  ).
        WHEN '.' OR ','.
          rv_result = abap_true.
      ENDCASE.
      RETURN.
    ENDIF.

    IF zcl_app_utilities=>is_sqlscript_token( mr_token_ext->sqlscript ).
      CASE zif_app_rule~get_token_up(  ).
        WHEN ';' OR ','.
          rv_result = abap_true.
          RETURN.
      ENDCASE.
    ENDIF.
  ENDMETHOD.


  METHOD zif_app_rule~is_new_line_req.
    IF is_comment(  ) = abap_true.
      rv_result = abap_true.
      RETURN.
    ENDIF.

    IF zif_app_rule~is_end_of_statement( ) = abap_true.
      rv_result = abap_true.
      RETURN.
    ENDIF.
    rv_result = mr_rule_data->is_new_line_req.
  ENDMETHOD.


  METHOD zif_app_rule~refresh_buffer.
    CLEAR mv_cur_offset_start_set.
    CLEAR mv_cur_offset_start.
    CLEAR mv_cur_offset_end_set.
    CLEAR mv_cur_offset_end.
    CLEAR mv_cur_row_set.
    CLEAR mv_cur_row.
    CLEAR mv_end_row_set.
    CLEAR mv_end_row.

  ENDMETHOD.


  METHOD zif_app_rule~set_additional_intend.
    mv_add_intend = iv_intend.
  ENDMETHOD.

  METHOD zif_app_rule~get_additional_intend.
    rv_intend = mv_add_intend.
  ENDMETHOD.


  METHOD zif_app_rule~set_next_rule.
    mr_next_rule = ir_next_rule.
  ENDMETHOD.


  METHOD zif_app_rule~validate.

  ENDMETHOD.


  METHOD zif_app_rule~is_lb_token_resp_delimiter.
    IF is_comment( ) = abap_true.
      RETURN.
    ENDIF.
    IF zcl_app_utilities=>is_abap_token( mr_token_ext->sqlscript ).

      CASE zif_app_rule~get_token_up(  ).
        WHEN '.' OR ','.
          rv_result = abap_true.
          RETURN.
      ENDCASE.
      rv_result = zcl_app_utilities=>contains_delimiter_char(
        EXPORTING
          it_delimiter = mr_token_ext->delimiter
          iv_char      = '.' ).

      IF rv_result = abap_true.
        RETURN.
      ENDIF.

      rv_result = zcl_app_utilities=>contains_delimiter_char(
        EXPORTING
          it_delimiter = mr_token_ext->delimiter
          iv_char      = ',' ).


      RETURN.
    ENDIF.

    IF zcl_app_utilities=>is_sqlscript_token( mr_token_ext->sqlscript ).
      CASE zif_app_rule~get_token_up(  ).
        WHEN ';' OR ','.
          rv_result = abap_true.
          RETURN.
      ENDCASE.

      rv_result = zcl_app_utilities=>contains_delimiter_char(
        EXPORTING
          it_delimiter = mr_token_ext->delimiter
          iv_char      = ';' ).

      IF rv_result = abap_true.
        RETURN.
      ENDIF.

      rv_result = zcl_app_utilities=>contains_delimiter_char(
        EXPORTING
          it_delimiter = mr_token_ext->delimiter
          iv_char      = ',' ).

    ENDIF.


  ENDMETHOD.

ENDCLASS.
