*----------------------------------------------------------------------*
***INCLUDE ZAPP_SETTINGS_USER_COMMAND_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE gv_okcode.
    WHEN 'EXIT'.
      gr_settings->handle_exit( zapp_settings ).
    WHEN 'SAVE'.
      gr_settings->save( zapp_settings ).
  ENDCASE.
ENDMODULE.
