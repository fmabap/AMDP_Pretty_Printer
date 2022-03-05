INTERFACE zif_app_rule_finder
  PUBLIC .

  METHODS get_rule_data
    IMPORTING is_rule_search type ZAPP_S_RULE_SEARCH
    RETURNING VALUE(rr_rule_data) TYPE REF TO zapp_s_rule
    RAISING zcx_app_exception.

ENDINTERFACE.
