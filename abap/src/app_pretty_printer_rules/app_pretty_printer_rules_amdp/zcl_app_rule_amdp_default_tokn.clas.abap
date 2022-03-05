CLASS zcl_app_rule_amdp_default_tokn DEFINITION
  PUBLIC
  FINAL
  INHERITING FROM ZCL_APP_RULE_AMDP_NEW_LINE_LFT

  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS is_logic_active REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_rule_amdp_default_tokn IMPLEMENTATION.
  METHOD is_logic_active.
    DATA lv_token TYPE zapp_d_token.
    DATA lt_token TYPE zapp_t_token.
    DATA lt_stop_token TYPE zapp_t_token.
    DATA lr_select_rule TYPE REF TO zif_app_rule.

    rv_result = super->is_logic_active(  ).

    IF rv_result = abap_false.
      RETURN.
    ENDIF.

    lv_token = 'UNION'.
    INSERT  lv_token INTO TABLE lt_stop_token.

    lv_token = 'SELECT'.
    INSERT  lv_token INTO TABLE lt_token.

    lr_select_rule = zcl_app_amdp_rule_utilities=>get_rule_in_stm_on_same_lvl_rw(
      EXPORTING
        ir_start_rule = me
        it_token      = lt_token
        it_stop_token = lt_stop_token
    ).
    IF lr_select_rule IS INITIAL.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
