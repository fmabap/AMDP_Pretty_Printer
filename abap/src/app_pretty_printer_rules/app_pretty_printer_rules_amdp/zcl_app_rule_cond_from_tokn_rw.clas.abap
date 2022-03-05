CLASS zcl_app_rule_cond_from_tokn_rw DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_app_rule_condition.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_rule_cond_from_tokn_rw IMPLEMENTATION.
  METHOD zif_app_rule_condition~is_cond_fulfilled.
    DATA lv_token TYPE zapp_d_token.
    DATA lt_token TYPE zapp_t_token.
    DATA lt_stop_token TYPE zapp_t_token.
    DATA lr_from_rule TYPE REF TO zif_app_rule.

    lv_token = 'UNION'.
    INSERT  lv_token INTO TABLE lt_stop_token.

    lv_token = 'FROM'.
    INSERT  lv_token INTO TABLE lt_token.

    lr_from_rule = zcl_app_amdp_rule_utilities=>get_rule_in_stm_on_same_lvl_rw(
      EXPORTING
        ir_start_rule = ir_rule
        it_token      = lt_token
        it_stop_token = lt_stop_token
    ).
    IF lr_from_rule IS NOT INITIAL.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
