INTERFACE zif_app_rule_provider
  PUBLIC .

  METHODS get_rules
    RETURNING VALUE(rt_result) TYPE ZAPP_T_RULE_SORT
    RAISING   zcx_app_exception.

ENDINTERFACE.
