CLASS zcl_app_rule_amdp_clse_bracket DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_default_no_c

  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~get_cur_row REDEFINITION.
    METHODS zif_app_rule~get_cur_offset_start REDEFINITION.
    METHODS zif_app_rule~get_new_line_intend REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS find_prev_open_bracket_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .
ENDCLASS.

CLASS zcl_app_rule_amdp_clse_bracket IMPLEMENTATION.
  METHOD find_prev_open_bracket_rule.
    rr_result = zcl_app_amdp_rule_utilities=>find_prev_open_bracket_rule( ir_start_rule = me ).

    IF rr_result IS INITIAL.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '011'
        WITH mr_rule_data->rule_name
        mr_token_ext->row
        mr_token_ext->col.
    ENDIF.
  ENDMETHOD.

  METHOD zif_app_rule~get_cur_row.
    DATA lr_open_bracket_rule TYPE REF TO zcl_app_rule_amdp_opn_bracket.

    IF mv_cur_row_set = abap_true.
      rv_result = mv_cur_row.
      RETURN.
    ENDIF.

    rv_result = super->zif_app_rule~get_cur_row( ).

    IF is_logic_active(  ) = abap_false.
      RETURN.
    ENDIF.

    lr_open_bracket_rule ?= find_prev_open_bracket_rule(  ).
    IF lr_open_bracket_rule->zif_app_rule~get_end_row( ) < rv_result
    AND mr_prev_rule->get_cur_row( ) = rv_result
    AND lr_open_bracket_rule->get_mv_special_logic( ) <> zif_app_amdp_rule_definitions=>cos_open_bracket_special_logic-call_statement.
      rv_result = rv_result + 1.
    ENDIF.

    zif_app_rule~set_cur_row( rv_result ).
  ENDMETHOD.

  METHOD zif_app_rule~get_cur_offset_start.
    DATA lr_open_bracket_rule TYPE REF TO zcl_app_rule_amdp_opn_bracket.

    IF mv_cur_offset_start_set = abap_true.
      rv_result = mv_cur_offset_start.
      RETURN.
    ENDIF.

    IF is_logic_active(  ) = abap_false.
      rv_result = super->zif_app_rule~get_cur_offset_start( ).
      RETURN.
    ENDIF.

    lr_open_bracket_rule ?= find_prev_open_bracket_rule(  ).
    IF lr_open_bracket_rule->get_mv_special_logic( ) <> zif_app_amdp_rule_definitions=>cos_open_bracket_special_logic-call_statement
    AND lr_open_bracket_rule->zif_app_rule~get_end_row( ) <> zif_app_rule~get_cur_row(  ).
      rv_result = lr_open_bracket_rule->zif_app_rule~get_cur_offset_start(  ) + mv_add_intend.
      zif_app_rule~set_cur_offset_start( rv_result ).
    ELSE.
      rv_result = super->zif_app_rule~get_cur_offset_start(  ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_app_rule~get_new_line_intend.
    DATA lr_rule TYPE REF TO zif_app_rule.

    IF is_logic_active(  ) = abap_false OR zif_app_rule~is_end_of_statement( ) = abap_true.
      rv_result = super->zif_app_rule~get_new_line_intend( ).
      RETURN.
    ENDIF.

    lr_rule = find_prev_open_bracket_rule(  ).
    IF lr_rule->get_prev_rule( ) IS INITIAL.
      rv_result = mv_default_line_intend.
    ELSE.
      rv_result = lr_rule->get_prev_rule( )->get_new_line_intend(  ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
