CLASS zcl_app_rule_amdp_by DEFINITION
  PUBLIC
  FINAL
INHERITING FROM zcl_app_rule_amdp_default_no_c
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~finalize_init REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS is_on_select_level
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_app_exception.

    METHODS adjust_prev_rule
      IMPORTING ir_open_bracket_rule TYPE REF TO zif_app_rule
      RETURNING VALUE(rv_result)     TYPE abap_bool
      RAISING   zcx_app_exception.
ENDCLASS.



CLASS zcl_app_rule_amdp_by IMPLEMENTATION.
  METHOD zif_app_rule~finalize_init.
    DATA lr_open_bracket_rule TYPE REF TO zif_app_rule.
    DATA lv_cur_offset_end TYPE i.

    IF is_comment(  ) = abap_true
    OR mr_prev_rule IS INITIAL
    OR mr_prev_rule->is_comment(  ) = abap_true
    OR mr_prev_rule->get_prev_rule(  ) IS INITIAL
    OR is_on_select_level(  ) = abap_true.
      super->zif_app_rule~finalize_init( ).
      RETURN.
    ENDIF.

    lr_open_bracket_rule = zcl_app_amdp_rule_utilities=>find_prev_open_bracket_rule( ir_start_rule = me ).
    IF lr_open_bracket_rule IS INITIAL.
      super->zif_app_rule~finalize_init( ).
      RETURN.
    ENDIF.

    adjust_prev_rule( lr_open_bracket_rule ).

    super->zif_app_rule~finalize_init( ).
    "respect the new line intend difference of the ORDER Rule at ORDER by for example
    lv_cur_offset_end = zif_app_rule~get_cur_offset_end( ).
    mr_rule_data->new_line_intend_diff = lv_cur_offset_end - mr_prev_rule->get_new_line_intend( ).

  ENDMETHOD.

  METHOD is_on_select_level.
    DATA lv_token TYPE zapp_d_token.
    DATA lt_token TYPE zapp_t_token.
    DATA lt_stop_token TYPE zapp_t_token.
    DATA lr_from_rule TYPE REF TO zif_app_rule.

    lv_token = 'UNION'.
    INSERT  lv_token INTO TABLE lt_stop_token.

    lv_token = 'SELECT'.
    INSERT  lv_token INTO TABLE lt_token.

    lr_from_rule = zcl_app_amdp_rule_utilities=>get_rule_in_stm_on_same_lvl_rw(
      EXPORTING
        ir_start_rule = me
        it_token      = lt_token
        it_stop_token = lt_stop_token
    ).
    IF lr_from_rule IS NOT INITIAL.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD adjust_prev_rule.

    DATA lv_prev_row TYPE i.
    IF mr_prev_rule->get_prev_rule(  ) <> ir_open_bracket_rule.

      IF mr_prev_rule->get_prev_rule( )->get_cur_row( ) = mr_prev_rule->get_prev_rule(  )->get_end_row( ).
        lv_prev_row = mr_prev_rule->get_prev_rule(  )->get_end_row( ) + 1.
      ELSE.
        lv_prev_row = mr_prev_rule->get_prev_rule(  )->get_end_row( ).

      ENDIF.

      mr_prev_rule->refresh_buffer( ).
      mr_prev_rule->set_cur_row( lv_prev_row ).
      mr_prev_rule->set_cur_offset_start( ir_open_bracket_rule->get_cur_offset_end( ) ).

    ENDIF.
  ENDMETHOD.

ENDCLASS.
