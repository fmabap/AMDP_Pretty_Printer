CLASS zcl_app_rule_amdp_default DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_base_rule
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~is_new_line_req REDEFINITION.

    METHODS zif_app_rule~init REDEFINITION.
    METHODS zif_app_rule~finalize_init REDEFINITION.
    METHODS zif_app_rule~get_cur_offset_start REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS adjust_delimiter.
ENDCLASS.



CLASS zcl_app_rule_amdp_default IMPLEMENTATION.


  METHOD adjust_delimiter.

    DATA lr_delimiter TYPE REF TO string.
    DATA lv_len TYPE i.
    DATA lv_first_space TYPE abap_bool.
    DATA lv_offset TYPE i.
    DATA lv_delimiter_new TYPE string.
    DATA lt_delimiter_new TYPE sourcetable.
    DATA lv_first_row TYPE abap_bool.

    lv_first_space = abap_true.
    LOOP AT mr_token_ext->delimiter REFERENCE INTO lr_delimiter.

      lv_len = strlen( lr_delimiter->* ).

      DO lv_len TIMES.
        lv_offset = sy-index - 1.
        IF lr_delimiter->*+lv_offset(1) = ` `.
          IF lv_first_space = abap_false.
            CONTINUE.
          ENDIF.
          lv_first_space = abap_false.
        ENDIF.

        CONCATENATE lv_delimiter_new lr_delimiter->*+lv_offset(1) INTO lv_delimiter_new.
      ENDDO.
    ENDLOOP.

    lv_first_row = abap_true.
    DO lines( mr_token_ext->delimiter ) TIMES.
      INSERT INITIAL LINE INTO TABLE lt_delimiter_new REFERENCE INTO lr_delimiter.
      IF lv_first_row = abap_true.
        lv_first_row = abap_false.
        lr_delimiter->* = lv_delimiter_new.
      ENDIF.
    ENDDO.

    mr_token_ext->delimiter = lt_delimiter_new.
  ENDMETHOD.


  METHOD zif_app_rule~finalize_init.
    DATA lr_next_rule TYPE REF TO zif_app_rule.
    DATA lr_next_token_ext TYPE REF TO zapp_s_stokesx_ext.
    super->zif_app_rule~finalize_init( ).

    lr_next_rule = zif_app_rule~get_next_rule( ).
    IF lr_next_rule IS NOT INITIAL.
      lr_next_token_ext = lr_next_rule->get_token_ext( ).
      CASE lr_next_token_ext->comment_detail.
        WHEN  zcl_app_scanner_comment=>cos_comment_detail-part
          OR zcl_app_scanner_comment=>cos_comment_detail-start.
          RETURN.
      ENDCASE.
    ENDIF.

    adjust_delimiter( ).

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

      rv_result = mv_default_line_indent + mv_add_indent.
      zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
      zif_app_rule~set_cur_offset_start( rv_result ).
      RETURN.
    ENDIF.

    CASE mr_token_ext->comment_detail.
      WHEN zcl_app_scanner_comment=>cos_comment_detail-start_begin_of_line_indentable.
        rv_result = mr_prev_rule->get_new_line_indent( ).
        zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
        rv_result = rv_result + mv_add_indent.
        zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
        zif_app_rule~set_cur_offset_start( rv_result ).
        RETURN.
      WHEN zcl_app_scanner_comment=>cos_comment_detail-start
        OR zcl_app_scanner_comment=>cos_comment_detail-part.
        rv_result = mr_prev_rule->get_cur_offset_end( ) + mv_add_indent.
        zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
        zif_app_rule~set_cur_offset_start( rv_result ).
        RETURN.
    ENDCASE.

    IF  ( mr_prev_rule->has_multline_delimiter( ) = abap_true
          AND mr_token_ext->comment_detail <> zcl_app_scanner_comment=>cos_comment_detail-part )
    OR  ( mr_prev_rule->is_new_line_req( ) = abap_true
          AND zif_app_rule~is_line_breaking_token( ) = abap_false ).

      rv_result = mr_prev_rule->get_new_line_indent( ).
    ELSE.
      rv_result = mr_prev_rule->get_cur_offset_end( ).
    ENDIF.

    zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
    rv_result = rv_result + mv_add_indent.
    zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
    zif_app_rule~set_cur_offset_start( rv_result ).
  ENDMETHOD.


  METHOD zif_app_rule~init.
    mv_default_line_indent = 4.

    IF zcl_app_utilities=>is_sqlscript_token( ir_token_ext->sqlscript ) = abap_false.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '009'
        WITH ir_rule_data->rule_name.

    ENDIF.
    super->zif_app_rule~init(
      EXPORTING
        ir_token_ext       = ir_token_ext
        ir_t_source        = ir_t_source
        ir_t_statement     = ir_t_statement
        ir_t_structure     = ir_t_structure
        ir_rule_data       = ir_rule_data
        ir_context_rule    = ir_context_rule
        ir_hl_context_rule = ir_hl_context_rule
        ir_prev_rule       = ir_prev_rule
        ir_settings        = ir_settings ).

  ENDMETHOD.


  METHOD zif_app_rule~is_new_line_req.
    IF super->zif_app_rule~is_new_line_req( ) = abap_true.
      rv_result = abap_true.
      RETURN.
    ENDIF.

    IF mr_settings->is_line_break_at_comma_req( ) = abap_true.
      IF zif_app_rule~get_token_up(  ) = ','.
        rv_result = abap_true.
        RETURN.
      ENDIF.

      IF zcl_app_utilities=>contains_delimiter_char(
           it_delimiter =  mr_token_ext->delimiter
           iv_char      = ','  ) = abap_true.
        rv_result = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
