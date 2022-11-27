CLASS zcl_app_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_app_settings.
    CLASS-METHODS class_constructor.
    CLASS-METHODS get_settings_from_cust.
    CLASS-METHODS set_settings
      IMPORTING is_settings TYPE zapp_settings.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA ss_settings TYPE zapp_settings.

ENDCLASS.


CLASS zcl_app_settings IMPLEMENTATION.

  METHOD class_constructor.
    get_settings_from_cust( ).

  ENDMETHOD.

  METHOD zif_app_settings~is_line_break_after_comma_req.

    rv_result = xsdbool( ss_settings-lb_after_comma_rule <> zif_app_settings~cos_lb_rules_at_comma-no_line_break ).

  ENDMETHOD.


  METHOD get_settings_from_cust.
    SELECT SINGLE * FROM zapp_settings
        INTO ss_settings
      WHERE uname = sy-uname.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM zapp_settings
        INTO ss_settings
      WHERE uname = '*'.
      IF sy-subrc <> 0.
        CLEAR ss_settings.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD set_settings.
    ss_settings = is_settings.
  ENDMETHOD.


  METHOD zif_app_settings~is_always_line_break_aft_comma.
    rv_result = xsdbool( ss_settings-lb_after_comma_rule = zif_app_settings~cos_lb_rules_at_comma-always_line_break ).
  ENDMETHOD.

  METHOD zif_app_settings~is_no_lb_at_co_s_fu_dep_sfu.
    rv_result = xsdbool( ss_settings-lb_after_comma_rule = zif_app_settings~cos_lb_rules_at_comma-dep_on_cls_bracket_and_sub_fu ).
  ENDMETHOD.

  METHOD zif_app_settings~is_no_lb_at_co_s_fu_dep_cbr_o.
    rv_result = xsdbool( ss_settings-lb_after_comma_rule = zif_app_settings~cos_lb_rules_at_comma-dep_on_cls_bracket_only ).
  ENDMETHOD.

  METHOD zif_app_settings~is_no_lb_at_co_s_fu_dep_sfu_kw.
    rv_result = xsdbool( ss_settings-lb_after_comma_rule = zif_app_settings~cos_lb_rules_at_comma-dep_on_cls_br_sf_and_keywrd ).
  ENDMETHOD.
ENDCLASS.
