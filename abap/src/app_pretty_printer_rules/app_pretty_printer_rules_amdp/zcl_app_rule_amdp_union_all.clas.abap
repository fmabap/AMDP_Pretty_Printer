CLASS zcl_app_rule_amdp_union_all DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM ZCL_APP_RULE_AMDP_NEW_LINE_LFT

  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~finalize_init REDEFINITION.
    METHODS zif_app_rule~is_new_line_req REDEFINITION.
    METHODS zif_app_rule~get_new_line_intend REDEFINITION.
    METHODS zif_app_rule~get_cur_offset_start REDEFINITION.
    METHODS zif_app_rule~get_cur_row REDEFINITION.
  PROTECTED SECTION.


  PRIVATE SECTION.
    METHODS is_union_or_all_of_union_all
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING
                zcx_app_exception.
ENDCLASS.



CLASS ZCL_APP_RULE_AMDP_UNION_ALL IMPLEMENTATION.


  METHOD is_union_or_all_of_union_all.

    IF is_logic_active( ) = abap_false.
      RETURN.
    ENDIF.

    IF mr_token_ext->str_up = 'UNION'
            AND mr_next_rule IS NOT INITIAL
            AND mr_next_rule->get_token_up( ) = 'ALL'.
      RETURN.
    ENDIF.

    IF mr_token_ext->str_up = 'ALL'.
      IF mr_prev_rule IS INITIAL
      OR mr_prev_rule->get_token_up(  ) <> 'UNION'.
        RETURN.
      ENDIF.
    ENDIF.

    rv_result = abap_true.
  ENDMETHOD.


  METHOD zif_app_rule~finalize_init.
    super->zif_app_rule~finalize_init( ).
    IF is_logic_active( ) = abap_false.
      RETURN.
    ENDIF.
    IF mr_token_ext->str_up = 'UNION'
        AND mr_next_rule IS NOT INITIAL
        AND mr_next_rule->get_token_up( ) = 'ALL'.
      mr_rule_data->add_intend = '-3'.
      mv_add_intend = mr_rule_data->add_intend.
      RETURN.
    ENDIF.

    IF mr_token_ext->str_up = 'ALL'.
      IF mr_prev_rule IS INITIAL
      OR mr_prev_rule->get_token_up(  ) <> 'UNION'.
        mv_logic_active = abap_false.
        set_logic_active( ).
        RETURN.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_app_rule~get_cur_offset_start.
    DATA lv_token TYPE zapp_d_token.
    DATA lt_token TYPE zapp_t_token.
    DATA lt_stop_token TYPE zapp_t_token.
    DATA lr_select_rule TYPE REF TO zif_app_rule.

    lv_token = 'UNION'.
    INSERT  lv_token INTO TABLE lt_stop_token.

    IF mr_token_ext->str_up = 'UNION' AND is_logic_active(  ) = abap_true.

      lv_token = 'SELECT'.
      INSERT  lv_token INTO TABLE lt_token.

      lr_select_rule = zcl_app_amdp_rule_utilities=>get_rule_in_stm_on_same_lvl_rw(
        EXPORTING
          ir_start_rule = me
          it_token      = lt_token
          it_stop_token = lt_stop_token
      ).
      IF lr_select_rule IS INITIAL.
        rv_result = super->zif_app_rule~get_cur_offset_start( ).
        RETURN.
      ENDIF.

      rv_result = lr_select_rule->get_cur_offset_start( ) + mv_add_intend.

    ELSEIF mr_token_ext->str_up = 'ALL' AND is_logic_active(  ) = abap_true.
      rv_result = mr_prev_rule->get_cur_offset_end( ).
    ELSE.
      rv_result = super->zif_app_rule~get_cur_offset_start( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_app_rule~get_cur_row.

    IF mr_token_ext->str_up = 'ALL' AND is_union_or_all_of_union_all( ).
      rv_result = mr_prev_rule->get_cur_row( ).

    ELSE.
      rv_result = super->zif_app_rule~get_cur_row( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_app_rule~get_new_line_intend.
    DATA lv_token TYPE zapp_d_token.
    DATA lt_token TYPE zapp_t_token.
    DATA lt_stop_token TYPE zapp_t_token.
    DATA lr_select_rule TYPE REF TO zif_app_rule.
    DATA lr_start_rule  TYPE REF TO zif_app_rule.

    IF is_union_or_all_of_union_all( ).
      lv_token = 'SELECT'.
      INSERT  lv_token INTO TABLE lt_token.

      lv_token = 'UNION'.
      INSERT  lv_token INTO TABLE lt_stop_token.

      IF mr_token_ext->str_up = 'ALL'.
        lr_start_rule = mr_prev_rule.
      ELSE.
        lr_start_rule = me.
      ENDIF.

      lr_select_rule = zcl_app_amdp_rule_utilities=>get_rule_in_stm_on_same_lvl_rw(
        EXPORTING
          ir_start_rule = lr_start_rule
          it_token      = lt_token
          it_stop_token = lt_stop_token
      ).
      IF lr_select_rule IS INITIAL.
        rv_result = super->zif_app_rule~get_new_line_intend(  ).
        RETURN.
      ENDIF.

      rv_result = lr_select_rule->get_cur_offset_start( ) - lr_select_rule->get_additional_intend( ).

    ELSE.
      rv_result = super->zif_app_rule~get_new_line_intend( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_app_rule~is_new_line_req.

    IF is_union_or_all_of_union_all( ).
      rv_result = abap_true.
    ELSE.
      rv_result = super->zif_app_rule~is_new_line_req(  ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
