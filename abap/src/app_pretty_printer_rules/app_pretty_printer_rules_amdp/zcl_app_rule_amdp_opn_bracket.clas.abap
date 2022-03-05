CLASS zcl_app_rule_amdp_opn_bracket DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_default_no_c
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~get_new_line_intend REDEFINITION.
    METHODS zif_app_rule~finalize_init REDEFINITION.
    METHODS zif_app_rule~get_cur_row REDEFINITION.
    METHODS zif_app_rule~is_new_line_req REDEFINITION.
    METHODS get_mv_special_logic RETURNING VALUE(r_result) TYPE zapp_d_spec_bracket_logic.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_special_logic TYPE zapp_d_spec_bracket_logic.
    METHODS is_new_line_req_for_call_statm
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_app_exception .
ENDCLASS.



CLASS zcl_app_rule_amdp_opn_bracket IMPLEMENTATION.

  METHOD zif_app_rule~get_new_line_intend.


    IF is_logic_active( ) = abap_true.

      IF mv_special_logic = zif_app_amdp_rule_definitions=>cos_open_bracket_special_logic-call_statement.
        rv_result = mr_prev_rule->get_prev_rule( )->get_cur_offset_end( ) + 4.
      ELSE.
        rv_result = zif_app_rule~get_cur_offset_end( ).
      ENDIF.
    ELSE.
      rv_result = super->zif_app_rule~get_new_line_intend( ).
    ENDIF.
  ENDMETHOD.


  METHOD zif_app_rule~get_cur_row.

    IF mv_cur_row_set = abap_true.
      rv_result = mv_cur_row.
      RETURN.
    ENDIF.

    rv_result = super->zif_app_rule~get_cur_row( ).

    IF is_logic_active(  ) = abap_false OR mr_prev_rule IS INITIAL.
      RETURN.
    ENDIF.

    IF mr_prev_rule->get_token_up( ) = zif_app_rule~get_token_up( ).
      rv_result = rv_result + 1.
      zif_app_rule~set_cur_row( rv_result ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_app_rule~finalize_init.
    DATA lr_delimiter TYPE REF TO string.
    super->zif_app_rule~finalize_init( ).

    IF is_logic_active( ) = abap_false.
      RETURN.
    ENDIF.

    READ TABLE mr_token_ext->delimiter
    REFERENCE INTO lr_delimiter
    INDEX 1 .
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF strlen( lr_delimiter->* ) = 0.
      RETURN.
    ENDIF.

    IF lr_delimiter->* CO ` `.
      mr_token_ext->delimiter = zcl_app_utilities=>get_space_as_delimiter( ).
    ENDIF.

    IF mr_prev_rule IS NOT INITIAL
     AND mr_prev_rule->get_prev_rule( ) IS NOT INITIAL
     AND mr_prev_rule->get_prev_rule( )->get_token_up(  ) = 'CALL'.
      mv_special_logic = zif_app_amdp_rule_definitions=>cos_open_bracket_special_logic-call_statement.
    ENDIF.

  ENDMETHOD.

  METHOD zif_app_rule~is_new_line_req.

    DATA lr_rule TYPE REF TO zif_app_rule.
    rv_result = super->zif_app_rule~is_new_line_req( ).

    IF rv_result = abap_true.
      RETURN.
    ENDIF.


    IF mv_special_logic = zif_app_amdp_rule_definitions=>cos_open_bracket_special_logic-call_statement.
      rv_result = is_new_line_req_for_call_statm( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_mv_special_logic.
    r_result = me->mv_special_logic.
  ENDMETHOD.

  METHOD is_new_line_req_for_call_statm.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_counter_next_open_bracket TYPE i.

    lr_rule = me.
    DO.
      lr_rule = lr_rule->get_next_rule( ).
      IF lr_rule IS INITIAL.
        return.
      ENDIF.
      IF zcl_app_utilities=>is_sqlscript_rule( lr_rule ) = abap_false.
        return.
      ENDIF.

      IF lr_rule->is_end_of_statement(  ) = abap_true.
        return.
      ENDIF.

      IF lr_rule->is_comment( ) = abap_true.
        CONTINUE.
      ENDIF.

      IF lr_rule->is_line_breaking_token(  ) = abap_true.
        rv_result = abap_true.
        RETURN.
      ENDIF.

      CASE lr_rule->get_token_up(  ).
        WHEN ')'.

          IF lv_counter_next_open_bracket = 0.
            RETURN.
          ENDIF.
          lv_counter_next_open_bracket = lv_counter_next_open_bracket - 1.
        WHEN '('.
          lv_counter_next_open_bracket = lv_counter_next_open_bracket + 1.
      ENDCASE.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
