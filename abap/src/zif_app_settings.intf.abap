INTERFACE zif_app_settings
  PUBLIC .
  METHODS is_line_break_at_comma_req
    RETURNING VALUE(rv_result) TYPE zapp_no_lb_at_comma.
ENDINTERFACE.
