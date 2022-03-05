INTERFACE zif_app_keyword_scanner

  PUBLIC .
  METHODS scan_keyword
    importing ir_token_ext type ref to zapp_s_stokesx_ext
    RAISING   zcx_app_exception.
ENDINTERFACE.
