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

ENDCLASS.


CLASS zcl_app_settings IMPLEMENTATION.

  METHOD class_constructor.

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

  METHOD zif_app_settings~is_line_break_at_comma_req.

    rv_result = xsdbool( ss_settings-no_lb_at_comma = abap_false ).

  ENDMETHOD.


ENDCLASS.
