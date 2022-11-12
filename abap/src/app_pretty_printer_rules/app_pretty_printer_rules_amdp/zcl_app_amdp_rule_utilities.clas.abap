CLASS zcl_app_amdp_rule_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS get_rule_in_stm_on_same_level
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
                it_token         TYPE zapp_t_token
                it_stop_token    TYPE zapp_t_token
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    CLASS-METHODS get_rule_in_stm_on_same_lvl_rw
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
                it_token         TYPE zapp_t_token
                it_stop_token    TYPE zapp_t_token
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    CLASS-METHODS find_prev_open_bracket_rule
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .


    CLASS-METHODS get_next_no_comment_amdp_rule
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    CLASS-METHODS get_prev_no_comment_amdp_rule
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .


    CLASS-METHODS get_1_rule_in_stm_on_same_lvl
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
                iv_token         TYPE zapp_d_token
                it_stop_token    TYPE zapp_t_token OPTIONAL
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    CLASS-METHODS get_1_rl_in_stm_on_same_lvl_rw
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
                iv_token         TYPE zapp_d_token
                it_stop_token    TYPE zapp_t_token OPTIONAL
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .



    "! <p class="shorttext synchronized" lang="en">Is closing bracket of function original in the same line</p>
    "!
    "! @parameter IR_START_RULE | <p class="shorttext synchronized" lang="en">Start rule must be function name before the bracket</p>
    "! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Result</p>
    CLASS-METHODS is_cls_bra_of_fu_in_same_line
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_app_exception .

    "! <p class="shorttext synchronized" lang="en">Contains function sub func. with comma</p>
    "!
    "! @parameter IR_START_RULE | <p class="shorttext synchronized" lang="en">Start rule must be function name before the bracket</p>
    "! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Result</p>
    CLASS-METHODS contains_fu_sub_fu_w_comma
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_app_exception .

    "! <p class="shorttext synchronized" lang="en">Contains function or sub func. SELECT</p>
    "!
    "! @parameter IR_START_RULE | <p class="shorttext synchronized" lang="en">Start rule must be function name before the bracket</p>
    "! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Result</p>
    CLASS-METHODS contains_fu_or_sub_fu_select
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_app_exception .

    "! <p class="shorttext synchronized" lang="en">Contains function keywords or sub func. with comma</p>
    "!
    "! @parameter IR_START_RULE | <p class="shorttext synchronized" lang="en">Start rule must be function name before the bracket</p>
    "! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Result</p>
    CLASS-METHODS contains_fu_kw_or_sfu_w_comma
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_app_exception .

    "! <p class="shorttext synchronized" lang="en">Avoid line break after comma in function</p>
    "!
    "! @parameter IR_START_RULE | <p class="shorttext synchronized" lang="en">Start rule must be function name before the bracket</p>
    CLASS-METHODS avoid_lb_after_comma_in_func
      IMPORTING
                ir_start_rule TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .


    "! <p class="shorttext synchronized" lang="en">Is avoid line break after comma in function active</p>
    "!
    "! @parameter IR_START_RULE | <p class="shorttext synchronized" lang="en">Pretty Printer Rule</p>
    "! @parameter IR_SETTINGS | <p class="shorttext synchronized" lang="en">AMDP Pretty Printer Settings</p>
    "! @parameter RV_RESULT | <p class="shorttext synchronized" lang="en">Result</p>
    CLASS-METHODS is_avd_lb_aft_comma_in_fu_act
      IMPORTING
                ir_start_rule    TYPE REF TO zif_app_rule
                ir_settings      TYPE REF TO zif_app_settings
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_app_exception .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_amdp_rule_utilities IMPLEMENTATION.
  METHOD get_rule_in_stm_on_same_level.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_counter_open_bracket TYPE i.

    lr_rule = ir_start_rule.
    DO.
      lr_rule = lr_rule->get_next_rule( ).

      IF lr_rule IS INITIAL.
        EXIT.
      ENDIF.

      IF zcl_app_utilities=>is_sqlscript_rule( lr_rule ) = abap_false.
        EXIT.
      ENDIF.

      IF lr_rule->is_comment(  ) = abap_true.
        CONTINUE.
      ENDIF.

      CASE lr_rule->get_token_up( ).
        WHEN '('.
          lv_counter_open_bracket = lv_counter_open_bracket + 1.
        WHEN ')'.
          lv_counter_open_bracket = lv_counter_open_bracket - 1.
      ENDCASE.
      IF lv_counter_open_bracket = 0.

        READ TABLE it_token TRANSPORTING NO FIELDS
        WITH KEY table_line = lr_rule->get_token_up( ).
        IF sy-subrc = 0.
          rr_result = lr_rule.
          RETURN.
        ENDIF.

        READ TABLE it_stop_token TRANSPORTING NO FIELDS
        WITH KEY table_line = lr_rule->get_token_up( ).
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
      ENDIF.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD get_rule_in_stm_on_same_lvl_rw.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_counter_open_bracket TYPE i.

    lr_rule = ir_start_rule.
    DO.
      lr_rule = lr_rule->get_prev_rule( ).

      IF lr_rule IS INITIAL.
        EXIT.
      ENDIF.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        EXIT.
      ENDIF.

      IF zcl_app_utilities=>is_sqlscript_rule( lr_rule ) = abap_false.
        EXIT.
      ENDIF.

      IF lr_rule->is_comment(  ) = abap_true.
        CONTINUE.
      ENDIF.

      CASE lr_rule->get_token_up( ).
        WHEN '('.
          lv_counter_open_bracket = lv_counter_open_bracket + 1.
        WHEN ')'.
          lv_counter_open_bracket = lv_counter_open_bracket - 1.
      ENDCASE.
      IF lv_counter_open_bracket = 0.

        READ TABLE it_token TRANSPORTING NO FIELDS
        WITH KEY table_line = lr_rule->get_token_up( ).
        IF sy-subrc = 0.
          rr_result = lr_rule.
          RETURN.
        ENDIF.

        READ TABLE it_stop_token TRANSPORTING NO FIELDS
        WITH KEY table_line = lr_rule->get_token_up( ).
        IF sy-subrc = 0.
          RETURN.
        ENDIF.

      ENDIF.

    ENDDO.
  ENDMETHOD.
  METHOD find_prev_open_bracket_rule.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_counter_prev_closed_bracket TYPE i.

    lr_rule = ir_start_rule.
    DO.
      lr_rule = lr_rule->get_prev_rule( ).
      IF lr_rule IS INITIAL.
        EXIT.
      ENDIF.
      IF zcl_app_utilities=>is_sqlscript_rule( lr_rule ) = abap_false.
        EXIT.
      ENDIF.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        EXIT.
      ENDIF.

      IF lr_rule->is_comment( ) = abap_true.
        CONTINUE.
      ENDIF.

      CASE lr_rule->get_token_up(  ).
        WHEN '('.

          IF lv_counter_prev_closed_bracket = 0.
            rr_result = lr_rule.
            RETURN.
          ENDIF.
          lv_counter_prev_closed_bracket = lv_counter_prev_closed_bracket - 1.
        WHEN ')'.

          lv_counter_prev_closed_bracket = lv_counter_prev_closed_bracket + 1.

      ENDCASE.
    ENDDO.

  ENDMETHOD.


  METHOD get_next_no_comment_amdp_rule.
    DATA lr_rule TYPE REF TO zif_app_rule.

    lr_rule = ir_start_rule.
    DO.
      lr_rule = lr_rule->get_next_rule( ).

      IF lr_rule IS INITIAL.
        RETURN.
      ENDIF.

      IF zcl_app_utilities=>is_sqlscript_rule( lr_rule ) = abap_false.
        RETURN.
      ENDIF.

      IF lr_rule->is_comment(  ) = abap_true.
        CONTINUE.
      ENDIF.
      "Found
      EXIT.
    ENDDO.
    rr_result = lr_rule.
  ENDMETHOD.

  METHOD get_prev_no_comment_amdp_rule.
    DATA lr_rule TYPE REF TO zif_app_rule.

    lr_rule = ir_start_rule.
    DO.
      lr_rule = lr_rule->get_prev_rule( ).

      IF lr_rule IS INITIAL.
        RETURN.
      ENDIF.

      IF zcl_app_utilities=>is_sqlscript_rule( lr_rule ) = abap_false.
        RETURN.
      ENDIF.

      IF lr_rule->is_comment(  ) = abap_true.
        CONTINUE.
      ENDIF.
      "Found
      EXIT.
    ENDDO.
    rr_result = lr_rule.
  ENDMETHOD.

  METHOD get_1_rule_in_stm_on_same_lvl.

    DATA lt_token TYPE zapp_t_token.

    INSERT  iv_token INTO TABLE lt_token.

    rr_result = zcl_app_amdp_rule_utilities=>get_rule_in_stm_on_same_level(
      EXPORTING
        ir_start_rule = ir_start_rule
        it_token      = lt_token
        it_stop_token = it_stop_token
    ).

  ENDMETHOD.

  METHOD get_1_rl_in_stm_on_same_lvl_rw.
    DATA lt_token TYPE zapp_t_token.

    INSERT  iv_token INTO TABLE lt_token.

    rr_result = zcl_app_amdp_rule_utilities=>get_rule_in_stm_on_same_lvl_rw(
      EXPORTING
        ir_start_rule = ir_start_rule
        it_token      = lt_token
        it_stop_token = it_stop_token
    ).

  ENDMETHOD.



  METHOD is_cls_bra_of_fu_in_same_line.

    DATA lr_next_rule TYPE REF TO zif_app_rule.
    DATA lr_close_bracket_rule TYPE REF TO zif_app_rule.
    DATA lr_func_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_close_bracket_token_ext TYPE REF TO zapp_s_stokesx_ext.

    lr_next_rule = zcl_app_amdp_rule_utilities=>get_next_no_comment_amdp_rule( ir_start_rule = ir_start_rule ).
    IF lr_next_rule IS INITIAL.
      RETURN.
    ENDIF.
    IF lr_next_rule->get_token_up( ) <> '('.
      RETURN.
    ENDIF.

    lr_close_bracket_rule = zcl_app_amdp_rule_utilities=>get_1_rule_in_stm_on_same_lvl(
      EXPORTING
        ir_start_rule = ir_start_rule
        iv_token      = ')'
    ).
    IF lr_close_bracket_rule IS INITIAL.
      RETURN.
    ENDIF.

    lr_func_token_ext = ir_start_rule->get_token_ext( ).
    lr_close_bracket_token_ext = lr_close_bracket_rule->get_token_ext( ).

    IF lr_func_token_ext->row = lr_close_bracket_token_ext->row.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD contains_fu_or_sub_fu_select.
    DATA lr_open_bracket_rule TYPE REF TO zif_app_rule.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_counter_open_bracket TYPE i.
    DATA lv_token TYPE zapp_d_token.

    lr_open_bracket_rule = zcl_app_amdp_rule_utilities=>get_next_no_comment_amdp_rule( ir_start_rule = ir_start_rule ).

    IF lr_open_bracket_rule IS INITIAL.
      RETURN.
    ENDIF.

    IF lr_open_bracket_rule->get_token_up( ) <> '('.
      RETURN.
    ENDIF.

    lr_rule = lr_open_bracket_rule.
    lv_counter_open_bracket = 1.
    DO.
      lr_rule = zcl_app_amdp_rule_utilities=>get_next_no_comment_amdp_rule( ir_start_rule = lr_rule ).

      IF lr_rule IS INITIAL.
        RETURN.
      ENDIF.

      lv_token = lr_rule->get_token_up(  ).

      CASE lv_token.
        WHEN 'SELECT'.
          rv_result = abap_true.
          return.
        WHEN '('.
          lv_counter_open_bracket = lv_counter_open_bracket + 1.
        WHEN ')'.
          lv_counter_open_bracket = lv_counter_open_bracket - 1.
          IF lv_counter_open_bracket = 0.
            "End of Function
            RETURN.
          ENDIF.
      ENDCASE.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        RETURN.
      ENDIF.

    ENDDO.

  ENDMETHOD.

  METHOD contains_fu_sub_fu_w_comma.
    DATA lr_open_bracket_rule TYPE REF TO zif_app_rule.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_counter_open_bracket TYPE i.
    DATA lv_token TYPE zapp_d_token.

    lr_open_bracket_rule = zcl_app_amdp_rule_utilities=>get_next_no_comment_amdp_rule( ir_start_rule = ir_start_rule ).

    IF lr_open_bracket_rule IS INITIAL.
      RETURN.
    ENDIF.

    IF lr_open_bracket_rule->get_token_up( ) <> '('.
      RETURN.
    ENDIF.

    lr_rule = lr_open_bracket_rule.
    lv_counter_open_bracket = 1.
    DO.
      lr_rule = zcl_app_amdp_rule_utilities=>get_next_no_comment_amdp_rule( ir_start_rule = lr_rule ).

      IF lr_rule IS INITIAL.
        RETURN.
      ENDIF.

      lv_token = lr_rule->get_token_up(  ).

      CASE lv_token.
        WHEN '('.
          lv_counter_open_bracket = lv_counter_open_bracket + 1.
        WHEN ')'.
          lv_counter_open_bracket = lv_counter_open_bracket - 1.
          IF lv_counter_open_bracket = 0.
            "End of Function
            RETURN.
          ENDIF.
      ENDCASE.
      IF lv_counter_open_bracket > 1.
        IF lv_token = ','.
          rv_result = abap_true.
          RETURN.
        ENDIF.

        lr_token_ext = lr_rule->get_token_ext( ).

        IF zcl_app_utilities=>contains_delimiter_char(
               it_delimiter = lr_token_ext->delimiter
               iv_char      = ',' ) = abap_true.

          rv_result = abap_true.
          RETURN.

        ENDIF.

      ENDIF.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        RETURN.
      ENDIF.

    ENDDO.

  ENDMETHOD.


  METHOD contains_fu_kw_or_sfu_w_comma.
    DATA lr_open_bracket_rule TYPE REF TO zif_app_rule.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_counter_open_bracket TYPE i.
    DATA lv_token TYPE zapp_d_token.
    DATA lr_keyword_scanner_amdp TYPE REF TO zif_app_keyword_scanner.
    DATA lv_key_words TYPE i.


    lr_open_bracket_rule = zcl_app_amdp_rule_utilities=>get_next_no_comment_amdp_rule( ir_start_rule = ir_start_rule ).

    IF lr_open_bracket_rule IS INITIAL.
      RETURN.
    ENDIF.

    IF lr_open_bracket_rule->get_token_up( ) <> '('.
      RETURN.
    ENDIF.

    CREATE OBJECT lr_keyword_scanner_amdp TYPE ('ZCL_APP_KEYWORD_SCANNER_AMDP').


    lr_rule = lr_open_bracket_rule.
    lv_counter_open_bracket = 1.
    DO.
      lr_rule = zcl_app_amdp_rule_utilities=>get_next_no_comment_amdp_rule( ir_start_rule = lr_rule ).

      IF lr_rule IS INITIAL.
        RETURN.
      ENDIF.

      lv_token = lr_rule->get_token_up(  ).

      CASE lv_token.
        WHEN '('.
          lv_counter_open_bracket = lv_counter_open_bracket + 1.
        WHEN ')'.
          lv_counter_open_bracket = lv_counter_open_bracket - 1.
          IF lv_counter_open_bracket = 0.
            "End of Function
            RETURN.
          ENDIF.
      ENDCASE.
      IF lv_counter_open_bracket > 1.
        IF lv_token = ','.
          rv_result = abap_true.
          RETURN.
        ENDIF.

        lr_token_ext = lr_rule->get_token_ext( ).

        IF zcl_app_utilities=>contains_delimiter_char(
               it_delimiter = lr_token_ext->delimiter
               iv_char      = ',' ) = abap_true.

          rv_result = abap_true.
          RETURN.

        ENDIF.
      ELSEIF lv_counter_open_bracket = 1.

        IF lr_keyword_scanner_amdp->is_keyword( iv_token_up = lv_token ) = abap_true.
          lv_key_words = lv_key_words + 1.
          IF lv_key_words = 2.
            rv_result = abap_true.
            RETURN.
          ENDIF.
        ENDIF.

      ENDIF.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        RETURN.
      ENDIF.

    ENDDO.

  ENDMETHOD.

  METHOD avoid_lb_after_comma_in_func.
    DATA lr_open_bracket_rule TYPE REF TO zif_app_rule.
    DATA lr_token_ext TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_counter_open_bracket TYPE i.
    DATA lv_token TYPE zapp_d_token.

    lr_open_bracket_rule = zcl_app_amdp_rule_utilities=>get_next_no_comment_amdp_rule( ir_start_rule = ir_start_rule ).

    IF lr_open_bracket_rule IS INITIAL.
      RETURN.
    ENDIF.

    IF lr_open_bracket_rule->get_token_up( ) <> '('.
      RETURN.
    ENDIF.

    lr_rule = lr_open_bracket_rule.
    lv_counter_open_bracket = 1.

    DO.
      lr_rule = lr_rule->get_next_rule( ).

      IF lr_rule IS INITIAL.
        RETURN.
      ENDIF.

      lv_token = lr_rule->get_token_up(  ).

      IF zcl_app_utilities=>is_sqlscript_rule( lr_rule ) = abap_false.
        RETURN.
      ENDIF.

      IF lr_rule->is_comment(  ) = abap_true.
        CONTINUE.
      ENDIF.

      CASE lv_token.
        WHEN '('.
          lv_counter_open_bracket = lv_counter_open_bracket + 1.
        WHEN ')'.
          lv_counter_open_bracket = lv_counter_open_bracket - 1.
          IF lv_counter_open_bracket = 0.
            "End of Function
            RETURN.
          ENDIF.
      ENDCASE.

      IF lv_counter_open_bracket = 1.
        IF lv_token = ','.
          lr_rule->set_avoid_lb_after_this_token( iv_avoid = abap_true ).
          CONTINUE.
        ENDIF.

        lr_token_ext = lr_rule->get_token_ext( ).

        IF  zcl_app_utilities=>contains_delimiter_char(
               it_delimiter = lr_token_ext->delimiter
               iv_char      = ',' ) = abap_true.

          lr_rule->set_avoid_lb_after_this_token( iv_avoid = abap_true ).
          CONTINUE.
        ENDIF.

      ENDIF.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD is_avd_lb_aft_comma_in_fu_act.
    IF ir_settings->is_no_lb_at_co_s_fu_dep_sfu( ) = abap_false
          AND ir_settings->is_no_lb_at_co_s_fu_dep_cbr_o( ) = abap_false
          AND ir_settings->is_no_lb_at_co_s_fu_dep_sfu_kw( ) = abap_false.
      RETURN.
    ENDIF.

    IF zcl_app_amdp_rule_utilities=>is_cls_bra_of_fu_in_same_line( ir_start_rule ) = abap_false.
      RETURN.
    ENDIF.

    IF zcl_app_amdp_rule_utilities=>contains_fu_or_sub_fu_select( ir_start_rule ) = abap_true.
      RETURN.
    ENDIF.

    IF ir_settings->is_no_lb_at_co_s_fu_dep_sfu( ) = abap_true
       AND zcl_app_amdp_rule_utilities=>contains_fu_sub_fu_w_comma( ir_start_rule ) = abap_true.
      RETURN.
    ENDIF.

    IF ir_settings->is_no_lb_at_co_s_fu_dep_sfu_kw( ) = abap_true
       AND zcl_app_amdp_rule_utilities=>contains_fu_kw_or_sfu_w_comma( ir_start_rule ) = abap_true.
      RETURN.
    ENDIF.

    rv_result = abap_true.
  ENDMETHOD.

ENDCLASS.
