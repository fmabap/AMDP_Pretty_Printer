*----------------------------------------------------------------------*
***INCLUDE ZAPP_SETTINGS_USER_COMMAND_I02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE gv_okcode.
    WHEN 'EXIT'.
      gr_settings_user->handle_exit( zapp_settings_u ).
    WHEN 'SAVE'.
      gr_settings_user->save( zapp_settings_u ).
  ENDCASE.
ENDMODULE.
