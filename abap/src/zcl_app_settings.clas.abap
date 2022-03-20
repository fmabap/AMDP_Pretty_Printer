CLASS zcl_app_settings DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_app_settings.
    CLASS-METHODS class_constructor.

  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA ss_settings TYPE zapp_settings.
    CLASS-DATA ss_settings_user TYPE zapp_settings_u.

    CLASS-METHODS exists_user_specifi_settings
      RETURNING VALUE(rv_result) TYPE abap_bool.

ENDCLASS.



CLASS zcl_app_settings IMPLEMENTATION.

  METHOD class_constructor.

    SELECT SINGLE * FROM zapp_settings
      INTO ss_settings
    WHERE dummy_key = abap_true.

    SELECT SINGLE * FROM zapp_settings_u
      INTO ss_settings_user
    WHERE uname = sy-uname.

  ENDMETHOD.

  METHOD zif_app_settings~is_line_break_at_comma_req.

    IF exists_user_specifi_settings(  ) = abap_true.
      rv_result = xsdbool( ss_settings_user-no_lb_at_comma = abap_false ).
    ELSE.
      rv_result = xsdbool( ss_settings-no_lb_at_comma = abap_false ).
    ENDIF.

  ENDMETHOD.

  METHOD exists_user_specifi_settings.

    IF ss_settings_user IS NOT INITIAL.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
