CLASS zcl_app_rule_cond_n_rl_is_no_b DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_app_rule_condition.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_rule_cond_n_rl_is_no_b IMPLEMENTATION.
  METHOD zif_app_rule_condition~is_cond_fulfilled.
    DATA lr_next_rule TYPE REF TO zif_app_rule.

    rv_result = abap_true.
    lr_next_rule = ir_rule->get_next_rule( ).

    IF lr_next_rule IS NOT INITIAL
    AND lr_next_rule->get_token_up( ) = '('.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
