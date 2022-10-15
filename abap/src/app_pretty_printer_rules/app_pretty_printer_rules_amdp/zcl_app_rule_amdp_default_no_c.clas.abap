CLASS zcl_app_rule_amdp_default_no_c DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_default
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~finalize_init REDEFINITION.
  PROTECTED SECTION.
    METHODS set_logic_active
      RAISING zcx_app_exception.

    METHODS is_logic_active
      RETURNING VALUE(rv_result) TYPE abap_bool
      RAISING   zcx_app_exception.
    DATA mv_logic_active TYPE abap_bool.
  PRIVATE SECTION.

    DATA mv_logic_active_set TYPE abap_bool.

ENDCLASS.



CLASS zcl_app_rule_amdp_default_no_c IMPLEMENTATION.


  METHOD is_logic_active.

    IF mv_logic_active_set = abap_true.
      rv_result = mv_logic_active.
    ELSE.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '015'
        WITH mr_rule_data->rule_name.
    ENDIF.

  ENDMETHOD.


  METHOD set_logic_active.
    IF zif_app_rule~is_comment( ) = abap_true.
      mv_logic_active = abap_false.
    ELSE.
      mv_logic_active = abap_true.
    ENDIF.
    mv_logic_active_set = abap_true.
  ENDMETHOD.


  METHOD zif_app_rule~finalize_init.
    super->zif_app_rule~finalize_init( ).
    set_logic_active( ).

    IF is_logic_active( ) = abap_true.
      mv_add_indent = mr_rule_data->add_indent.
    ELSE.
      "If the token is a comment, then clear the rule_data,
      "that could lead to issues with indent
      CLEAR mr_rule_data->is_new_line_req.
      CLEAR mr_rule_data->new_line_indent_diff.
      CLEAR mr_rule_data->new_statement_indent_diff.
      CLEAR mr_rule_data->add_indent.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
