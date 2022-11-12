INTERFACE zif_app_settings
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">Is line break after comma required</p>
  "!
  "! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Result</p>
  METHODS is_line_break_after_comma_req
    RETURNING VALUE(rv_result) TYPE zapp_d_no_lb_at_comma.


  "! <p class="shorttext synchronized" lang="en">Is no lb after comma in function dep on sub function</p>
  "!
  "! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Result</p>
  METHODS is_no_lb_at_co_s_fu_dep_sfu
    RETURNING VALUE(rv_result) TYPE abap_bool.



  "! <p class="shorttext synchronized" lang="en">Is no line break after comma in function depending on ) only</p>
  "!
  "! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Result</p>
  METHODS is_no_lb_at_co_s_fu_dep_cbr_o
    RETURNING VALUE(rv_result) TYPE abap_bool.


  "! <p class="shorttext synchronized" lang="en">Is always line break after comma</p>
  "!
  "! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Result</p>
  METHODS is_always_line_break_aft_comma
    RETURNING VALUE(rv_result) TYPE abap_bool.


  "! <p class="shorttext synchronized" lang="en">Is no lb after comma in function dep on sub fu and keyword</p>
  "!
  "! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Result</p>
  METHODS is_no_lb_at_co_s_fu_dep_sfu_kw
    RETURNING VALUE(rv_result) TYPE abap_bool.

  CONSTANTS: BEGIN OF cos_lb_rules_at_comma,
               always_line_break             TYPE zapp_d_lb_after_comma_rule  VALUE '0',
               no_line_break                 TYPE zapp_d_lb_after_comma_rule  VALUE '1',
               dep_on_cls_bracket_only       TYPE zapp_d_lb_after_comma_rule  VALUE '2',
               dep_on_cls_bracket_and_sub_fu TYPE zapp_d_lb_after_comma_rule  VALUE '3',
               dep_on_cls_br_sf_and_keywrd   TYPE zapp_d_lb_after_comma_rule  VALUE '4',
             END OF cos_lb_rules_at_comma.
ENDINTERFACE.
