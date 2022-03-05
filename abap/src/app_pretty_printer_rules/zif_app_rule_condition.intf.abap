INTERFACE zif_app_rule_condition
  PUBLIC .

  METHODS is_cond_fulfilled
    IMPORTING ir_rule          TYPE REF TO zif_app_rule
    RETURNING VALUE(rv_result) TYPE abap_bool
    RAISING   zcx_app_exception.
ENDINTERFACE.
