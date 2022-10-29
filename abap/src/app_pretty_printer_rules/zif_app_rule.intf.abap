INTERFACE zif_app_rule
  PUBLIC .



"! <p class="shorttext synchronized" lang="en">Get New Context Rule</p>
"!
"! @parameter RR_RESULT | <p class="shorttext synchronized" lang="en">Pretty Printer Rule</p>
METHODS get_new_context_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get New Higher Level Context Rule</p>
"!
"! @parameter RR_RESULT | <p class="shorttext synchronized" lang="en">Pretty Printer Rule</p>
METHODS get_new_hl_context_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
    RAISING   zcx_app_exception.


  "! <p class="shorttext synchronized" lang="en"></p>
  "!
  "! @parameter rr_result | <p class="shorttext synchronized" lang="en">Get Context Rule</p>
  "! @raising zcx_app_exception | <p class="shorttext synchronized" lang="en"></p>
  METHODS get_context_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get Higher Level Context Rule</p>
"!
"! @parameter RR_RESULT | <p class="shorttext synchronized" lang="en">Pretty Printer Rule</p>
METHODS get_hl_context_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get previous rule in the chain</p>
"!
"! @parameter RR_RESULT | <p class="shorttext synchronized" lang="en">Pretty Printer Rule</p>
METHODS get_prev_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule.



"! <p class="shorttext synchronized" lang="en">Get next rule in the chain</p>
"!
"! @parameter RR_RESULT | <p class="shorttext synchronized" lang="en">Pretty Printer Rule</p>
METHODS get_next_rule
    RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule.


"! <p class="shorttext synchronized" lang="en">Init Rule</p>
"!
"! @parameter IR_TOKEN_EXT | <p class="shorttext synchronized" lang="en">Extended STOKESX</p>
"! @parameter IR_T_SOURCE | <p class="shorttext synchronized" lang="en">ABAP4 Sourcetext as table of string</p>
"! @parameter IR_T_STATEMENT | <p class="shorttext synchronized" lang="en">Code Inspector: Anweisungen</p>
"! @parameter IR_T_STRUCTURE | <p class="shorttext synchronized" lang="en">Code Inspector: SSTRUC</p>
"! @parameter IR_RULE_DATA | <p class="shorttext synchronized" lang="en">AMDP Pretty Printer Rule</p>
"! @parameter IR_SETTINGS | <p class="shorttext synchronized" lang="en">AMDP Pretty Printer Settings</p>
"! @parameter IR_CONTEXT_RULE | <p class="shorttext synchronized" lang="en">Pretty Printer Rule</p>
"! @parameter IR_HL_CONTEXT_RULE | <p class="shorttext synchronized" lang="en">Pretty Printer Rule</p>
"! @parameter IR_PREV_RULE | <p class="shorttext synchronized" lang="en">Pretty Printer Rule</p>
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


"! <p class="shorttext synchronized" lang="en">Is new line required</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS is_new_line_req
    RETURNING VALUE(rv_result) TYPE abap_bool
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get current row of the token (start row of the token)</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS get_cur_row
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Set current row in buffer</p>
"!
"! @parameter IV_CUR_ROW | <p class="shorttext synchronized" lang="en"></p>
METHODS set_cur_row
    IMPORTING iv_cur_row TYPE i
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get end row of the token</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS get_end_row
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get current offset (start of the token in the row)</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS get_cur_offset_start
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Set current offset in the buffer</p>
"!
"! @parameter IV_CUR_OFFSET_START | <p class="shorttext synchronized" lang="en"></p>
METHODS set_cur_offset_start
    IMPORTING iv_cur_offset_start TYPE i
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get offset end of the token</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS get_cur_offset_end
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get Token text</p>
"!
"! @parameter RT_RESULT | <p class="shorttext synchronized" lang="en">ABAP4 Sourcetext as table of string</p>
METHODS get_text
    RETURNING VALUE(rt_result) TYPE sourcetable
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get the indent of new lines</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS get_new_line_indent
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get the new statement indent</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS get_new_statement_indent
    RETURNING VALUE(rv_result) TYPE i
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Set the next rule in the chain</p>
"!
"! @parameter IR_NEXT_RULE | <p class="shorttext synchronized" lang="en">Pretty Printer Rule</p>
METHODS set_next_rule
    IMPORTING
              ir_next_rule TYPE REF TO zif_app_rule
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get extended data of the token</p>
"!
"! @parameter RR_RESULT | <p class="shorttext synchronized" lang="en">Extended STOKESX</p>
METHODS get_token_ext
    RETURNING VALUE(rr_result) TYPE REF TO zapp_s_stokesx_ext.


"! <p class="shorttext synchronized" lang="en">Validate rule</p>
"!
METHODS validate
    RAISING zcx_app_exception.



"! <p class="shorttext synchronized" lang="en">Is end of the statement</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS is_end_of_statement
    RETURNING VALUE(rv_result) TYPE abap_bool
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get new context</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Pretty Printer Context</p>
METHODS get_new_context
    RETURNING VALUE(rv_result) TYPE zapp_d_context
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get new higher level context</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Pretty Printer HL Context</p>
METHODS get_new_hl_context
    RETURNING VALUE(rv_result) TYPE zapp_d_hl_context
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get Rule Data</p>
"!
"! @parameter RR_RESULT | <p class="shorttext synchronized" lang="en">AMDP Pretty Printer Rule</p>
METHODS get_rule_data
    RETURNING VALUE(rr_result) TYPE REF TO zapp_s_rule
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Has the token a multiline deliminter</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS has_multline_delimiter
    RETURNING VALUE(rv_result) TYPE abap_bool
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Set additional indent of the token into the buffer</p>
"!
"! @parameter IV_INDENT | <p class="shorttext synchronized" lang="en"></p>
METHODS set_additional_indent
    IMPORTING iv_indent TYPE i
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Get addtional indent of the token</p>
"!
"! @parameter RV_INDENT | <p class="shorttext synchronized" lang="en"></p>
METHODS get_additional_indent
    RETURNING VALUE(rv_indent) TYPE i
    RAISING   zcx_app_exception.


"! <p class="shorttext synchronized" lang="en">Refresh the buffer</p>
"!
METHODS refresh_buffer.


"! <p class="shorttext synchronized" lang="en">Is a line break after the token required</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS is_line_breaking_token
    RETURNING VALUE(rv_result) TYPE abap_bool.


"! <p class="shorttext synchronized" lang="en">Is a line break after the token required respect delimiter</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
methods is_lb_token_resp_delimiter
RETURNING VALUE(rv_result) TYPE abap_bool.


"! <p class="shorttext synchronized" lang="en">Get the token string upper case</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Token</p>
METHODS get_token_up
    RETURNING VALUE(rv_result) TYPE zapp_d_token.


"! <p class="shorttext synchronized" lang="en">Is token a comment</p>
"!
"! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en"></p>
METHODS is_comment
    RETURNING VALUE(rv_result) TYPE abap_bool.


"! <p class="shorttext synchronized" lang="en">Finalize init after each token has been initialized</p>
"!
METHODS finalize_init
    RAISING zcx_app_exception.
ENDINTERFACE.
