INTERFACE zif_app_settings
  PUBLIC .
  METHODS is_line_break_at_comma_req
    RETURNING VALUE(rv_result) TYPE zapp_d_no_lb_at_comma.

  METHODS is_no_lb_at_co_for_simple_fu
    RETURNING VALUE(rv_result) TYPE abap_bool.

  METHODS is_always_line_break_at_comma
    RETURNING VALUE(rv_result) TYPE abap_bool.

ENDINTERFACE.
