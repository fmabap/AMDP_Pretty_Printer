INTERFACE zif_app_keyword_scanner

  PUBLIC .
  METHODS scan_keyword
    IMPORTING ir_token_ext TYPE REF TO zapp_s_stokesx_ext
    RAISING   zcx_app_exception.

  METHODS is_keyword
    IMPORTING iv_token_up      TYPE zapp_d_token
    RETURNING VALUE(rv_result) TYPE abap_bool.
ENDINTERFACE.
