CLASS zcl_app_rule_amdp_control_stru DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_new_line
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~finalize_init REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS finalize_init_end.
    METHODS finalize_init_then.
    METHODS finalize_init_when.
    METHODS finalize_init_else.
    METHODS finalize_init_elseif.
    METHODS finalize_init_case.
    METHODS finalize_init_others
      RAISING zcx_app_exception.

ENDCLASS.



CLASS zcl_app_rule_amdp_control_stru IMPLEMENTATION.
  METHOD zif_app_rule~finalize_init.
    super->zif_app_rule~finalize_init( ).

    IF is_comment(  ) = abap_false. "Set it also if no new line is required
      CASE zif_app_rule~get_token_up( ).
        WHEN 'CASE'.
          finalize_init_case( ).
        WHEN 'END'.
          finalize_init_end( ).
        WHEN 'THEN'.
          finalize_init_then( ).
        WHEN 'WHEN'.
          finalize_init_when( ).
        WHEN 'ELSE'.
          finalize_init_else( ).
        WHEN 'ELSEIF'.
          finalize_init_elseif( ).
        WHEN OTHERS.
          finalize_init_others( ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD finalize_init_end.
    DATA lr_next_rule TYPE REF TO zif_app_rule.
    lr_next_rule = zif_app_rule~get_next_rule( ).
    mr_rule_data->add_indent = -4.
    mv_add_indent = mr_rule_data->add_indent.
    IF lr_next_rule IS NOT INITIAL.
      CASE lr_next_rule->get_token_up(  ).
        WHEN 'IF' OR 'FOR' OR 'WHILE'.
          RETURN.
      ENDCASE.
    ENDIF.
    mr_rule_data->new_line_indent_diff = -4.
    mr_rule_data->new_statement_indent_diff = -4.
  ENDMETHOD.

  METHOD finalize_init_others.
    DATA lr_prev_rule TYPE REF TO zif_app_rule.

    lr_prev_rule = zif_app_rule~get_prev_rule( ).
    IF lr_prev_rule IS NOT INITIAL
      AND lr_prev_rule->get_token_up(  ) = 'END'
      AND lr_prev_rule->is_end_of_statement( ) = abap_false.
      mr_rule_data->new_line_indent_diff = -4.
      mr_rule_data->new_statement_indent_diff = -4.
      mv_logic_active = abap_false.
      RETURN.
    ENDIF.

    mr_rule_data->new_line_indent_diff = 4.
    mr_rule_data->new_statement_indent_diff = 4.
  ENDMETHOD.

  METHOD finalize_init_then.
    mr_rule_data->is_new_line_req = abap_true.
    mv_logic_active = abap_false.
  ENDMETHOD.

  METHOD finalize_init_when.
    mr_rule_data->add_indent = -2.
    mv_add_indent = mr_rule_data->add_indent.
  ENDMETHOD.

  METHOD finalize_init_else.
    mr_rule_data->add_indent = -2.
    mv_add_indent = mr_rule_data->add_indent.
    mr_rule_data->is_new_line_req = abap_true.
  ENDMETHOD.

  METHOD finalize_init_elseif.
    mr_rule_data->add_indent = -2.
    mv_add_indent = mr_rule_data->add_indent.
  ENDMETHOD.

  METHOD finalize_init_case.
    mr_rule_data->new_line_indent_diff = 4.
    mr_rule_data->new_statement_indent_diff = 4.
  ENDMETHOD.

ENDCLASS.
