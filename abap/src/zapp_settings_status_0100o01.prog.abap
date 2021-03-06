*----------------------------------------------------------------------*
***INCLUDE ZAPP_SETTINGS_STATUS_0100O01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  IF gr_settings->exists_settings( ).
    SET PF-STATUS '0100'.
  ELSE.
    SET PF-STATUS '0100' EXCLUDING 'DELETE'.
  ENDIF.
  IF p_set_g = abap_true.
    SET TITLEBAR '0100'.
  ELSE.
    SET TITLEBAR '0200' WITH sy-uname.
  ENDIF.
ENDMODULE.
