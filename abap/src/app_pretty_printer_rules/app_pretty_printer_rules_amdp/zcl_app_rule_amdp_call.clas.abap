CLASS zcl_app_rule_amdp_call DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_default_no_c
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~finalize_init REDEFINITION.


  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_first_parameter
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception.

    METHODS get_next_parameter
      IMPORTING ir_starting_rule TYPE REF TO zif_app_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception.

    METHODS get_max_parameter_length
      RETURNING VALUE(rv_result) TYPE i
      RAISING   zcx_app_exception.


    METHODS set_add_indent_add_arrows
      IMPORTING iv_max_parameter_length TYPE i
      RAISING   zcx_app_exception.

    METHODS set_add_indent_add_arrow
      IMPORTING ir_parameter            TYPE REF TO zif_app_rule
                iv_max_parameter_length TYPE i
      RAISING   zcx_app_exception.


    METHODS get_arrow_of_parameter
      IMPORTING ir_parameter     TYPE REF TO zif_app_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception.

ENDCLASS.

CLASS zcl_app_rule_amdp_call IMPLEMENTATION.
  METHOD zif_app_rule~finalize_init.

    set_add_indent_add_arrows( get_max_parameter_length( ) ).

  ENDMETHOD.

  METHOD get_first_parameter.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_catch_next TYPE abap_bool.
    DATA lv_counter_next_open_bracket TYPE i.

    lr_rule = me.
    DO.
      lr_rule = lr_rule->get_next_rule( ).
      IF lr_rule IS INITIAL.
        RETURN.
      ENDIF.
      IF zcl_app_utilities=>is_sqlscript_rule( lr_rule ) = abap_false.
        RETURN.
      ENDIF.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        RETURN.
      ENDIF.

      IF lr_rule->is_comment( ) = abap_true.
        CONTINUE.
      ENDIF.

      IF lr_rule->is_lb_token_resp_delimiter( ) = abap_true.
        RETURN.
      ENDIF.
      IF lr_rule->get_token_up( ) = '('.
        lv_catch_next = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_catch_next = abap_true.
        rr_result = lr_rule.
        RETURN.
      ENDIF.

    ENDDO.
  ENDMETHOD.

  METHOD get_next_parameter.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_catch_next TYPE abap_bool.
    DATA lv_counter_open_bracket TYPE i.

    lr_rule = ir_starting_rule.
    DO.
      lr_rule = lr_rule->get_next_rule( ).
      IF lr_rule IS INITIAL.
        RETURN.
      ENDIF.
      IF zcl_app_utilities=>is_sqlscript_rule( lr_rule ) = abap_false.
        RETURN.
      ENDIF.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        RETURN.
      ENDIF.

      IF lr_rule->is_comment( ) = abap_true.
        CONTINUE.
      ENDIF.

      CASE lr_rule->get_token_up( ).
        WHEN '('.
          lv_counter_open_bracket = lv_counter_open_bracket + 1.
        WHEN ')'.
          lv_counter_open_bracket = lv_counter_open_bracket - 1.
      ENDCASE.

      IF lr_rule->is_lb_token_resp_delimiter( ) = abap_true
      AND lv_counter_open_bracket = 0.
        lv_catch_next = abap_true.
        CONTINUE.
      ENDIF.
      IF lv_catch_next = abap_true.
        rr_result = lr_rule.
        RETURN.
      ENDIF.

    ENDDO.
  ENDMETHOD.

  METHOD get_max_parameter_length.
    DATA lr_parameter TYPE REF TO zif_app_rule.
    DATA lv_length TYPE i.

    lr_parameter = get_first_parameter( ).
    IF lr_parameter IS INITIAL.
      RETURN.
    ENDIF.

    rv_result = zcl_app_utilities=>get_token_length_wo_delimiter( lr_parameter ).

    DO.
      lr_parameter = get_next_parameter( lr_parameter ).

      IF lr_parameter IS INITIAL.
        RETURN.
      ENDIF.

      lv_length = zcl_app_utilities=>get_token_length_wo_delimiter( lr_parameter ).

      IF rv_result < lv_length.
        rv_result = lv_length.
      ENDIF.
    ENDDO.

  ENDMETHOD.



  METHOD set_add_indent_add_arrow.
    DATA lr_arrow TYPE REF TO zif_app_rule.
    DATA lv_add_intend TYPE i.
    lr_arrow = get_arrow_of_parameter( ir_parameter ).
    IF lr_arrow IS INITIAL.
      RETURN.
    ENDIF.

    lv_add_intend = iv_max_parameter_length - zcl_app_utilities=>get_token_length_wo_delimiter( ir_parameter ).
    lr_arrow->set_additional_intend( iv_intend = lv_add_intend ).

  ENDMETHOD.

  METHOD get_arrow_of_parameter.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_catch_next TYPE abap_bool.
    DATA lv_counter_open_bracket TYPE i.

    lr_rule = ir_parameter.
    DO.
      lr_rule = lr_rule->get_next_rule( ).
      IF lr_rule IS INITIAL.
        RETURN.
      ENDIF.
      IF zcl_app_utilities=>is_sqlscript_rule( lr_rule ) = abap_false.
        RETURN.
      ENDIF.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        RETURN.
      ENDIF.

      IF lr_rule->is_comment( ) = abap_true.
        CONTINUE.
      ENDIF.

      IF lr_rule->get_token_up( ) = '=>'.
        rr_result = lr_rule.
      ENDIF.
      RETURN.
    ENDDO.
  ENDMETHOD.


  METHOD set_add_indent_add_arrows.
    DATA lr_parameter TYPE REF TO zif_app_rule.
    DATA lv_length TYPE i.

    lr_parameter = get_first_parameter( ).
    IF lr_parameter IS INITIAL.
      RETURN.
    ENDIF.

    set_add_indent_add_arrow(
      EXPORTING
        ir_parameter            = lr_parameter
        iv_max_parameter_length = iv_max_parameter_length ).


    DO.
      lr_parameter = get_next_parameter( lr_parameter ).

      IF lr_parameter IS INITIAL.
        RETURN.
      ENDIF.

      set_add_indent_add_arrow(
        EXPORTING
          ir_parameter            = lr_parameter
          iv_max_parameter_length = iv_max_parameter_length ).

    ENDDO.

  ENDMETHOD.

ENDCLASS.
