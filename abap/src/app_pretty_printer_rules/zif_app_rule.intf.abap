INTERFACE zif_app_rule
  PUBLIC .

  METHODS get_new_context_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
    RAISING   zcx_app_exception.

  METHODS get_new_hl_context_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
    RAISING   zcx_app_exception.


  METHODS get_context_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
    RAISING   zcx_app_exception.

  METHODS get_hl_context_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
    RAISING   zcx_app_exception.

  METHODS get_prev_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule.


  METHODS get_next_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule.

  METHODS init
    IMPORTING
              ir_token_ext       TYPE REF TO zapp_s_stokesx_ext
              ir_t_source        TYPE REF TO sourcetable
              ir_t_statement     TYPE REF TO sstmnt_tab
              ir_t_structure     TYPE REF TO sstruc_tab
              ir_rule_data       TYPE REF TO zapp_s_rule
              ir_settings        type ref to zif_app_settings
              ir_context_rule    TYPE REF TO zif_app_rule OPTIONAL
              ir_hl_context_rule TYPE REF TO zif_app_rule OPTIONAL
              ir_prev_rule       TYPE REF TO zif_app_rule OPTIONAL
    RAISING   zcx_app_exception.

  METHODS is_new_line_req
    RETURNING VALUE(rv_result) TYPE abap_bool
    RAISING   zcx_app_exception.

  METHODS get_cur_row
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.

  METHODS set_cur_row
    IMPORTING iv_cur_row TYPE i
    RAISING   zcx_app_exception.

  METHODS get_end_row
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.

  METHODS get_cur_offset_start
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.

  METHODS set_cur_offset_start
    IMPORTING iv_cur_offset_start TYPE i
    RAISING   zcx_app_exception.

  METHODS get_cur_offset_end
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.

  METHODS get_text
    RETURNING VALUE(rt_result) TYPE sourcetable
    RAISING   zcx_app_exception.

  METHODS get_new_line_intend
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.

  METHODS get_new_statement_intend
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.

  METHODS set_next_rule
    IMPORTING
              ir_next_rule TYPE REF TO zif_app_rule
    RAISING   zcx_app_exception.

  METHODS get_token_ext
    RETURNING VALUE(rr_result) TYPE REF TO zapp_s_stokesx_ext.

  METHODS validate
    RAISING zcx_app_exception.


  METHODS is_end_of_statement
    RETURNING VALUE(rv_result) TYPE abap_bool
    RAISING   zcx_app_exception.

  METHODS get_new_context
    RETURNING VALUE(rv_result) TYPE zapp_d_context
    RAISING   zcx_app_exception.

  METHODS get_new_hl_context
    RETURNING VALUE(rv_result) TYPE zapp_d_hl_context
    RAISING   zcx_app_exception.

  METHODS get_rule_data
    RETURNING VALUE(rr_result) TYPE REF TO zapp_s_rule
    RAISING   zcx_app_exception.

  METHODS has_multline_delimiter
    RETURNING VALUE(rv_result) TYPE abap_bool
    RAISING   zcx_app_exception.

  METHODS set_additional_intend
    IMPORTING iv_intend TYPE i
    RAISING   zcx_app_exception.

  METHODS get_additional_intend
    RETURNING VALUE(rv_intend) TYPE i
    RAISING   zcx_app_exception.

  METHODS refresh_buffer.

  METHODS is_line_breaking_token
    RETURNING VALUE(rv_result) TYPE abap_bool.

methods is_lb_token_resp_delimiter
RETURNING VALUE(rv_result) TYPE abap_bool.

  METHODS get_token_up
    RETURNING VALUE(rv_result) TYPE zapp_d_token.

  METHODS is_comment
    RETURNING VALUE(rv_result) TYPE abap_bool.

  METHODS finalize_init
    RAISING zcx_app_exception.
ENDINTERFACE.
