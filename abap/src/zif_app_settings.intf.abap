INTERFACE zif_app_settings
  PUBLIC .
  METHODS is_line_break_after_comma_req
    RETURNING VALUE(rv_result) TYPE zapp_d_no_lb_at_comma.

  METHODS is_no_lb_at_co_s_fu_dep_sfu
    RETURNING VALUE(rv_result) TYPE abap_bool.

  METHODS is_no_lb_at_co_s_fu_dep_cbr_o
    RETURNING VALUE(rv_result) TYPE abap_bool.

  METHODS is_always_line_break_aft_comma
    RETURNING VALUE(rv_result) TYPE abap_bool.

  CONSTANTS: BEGIN OF cos_lb_rules_at_comma,
               always_line_break             TYPE zapp_d_lb_after_comma_rule  VALUE '0',
               no_line_break                 TYPE zapp_d_lb_after_comma_rule  VALUE '1',
               dep_on_cls_bracket_and_sub_fu TYPE zapp_d_lb_after_comma_rule  VALUE '2',
               dep_on_cls_bracket_only       TYPE zapp_d_lb_after_comma_rule  VALUE '3',
             END OF cos_lb_rules_at_comma.
ENDINTERFACE.
