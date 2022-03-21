CLASS zcl_app_rule_amdp_new_line DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_default_no_c
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~get_cur_row REDEFINITION.
    METHODS zif_app_rule~finalize_init REDEFINITION.
    METHODS zif_app_rule~get_cur_offset_start REDEFINITION.
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_rule_amdp_new_line IMPLEMENTATION.

  METHOD zif_app_rule~get_cur_row.
    IF mv_cur_row_set = abap_true.
      rv_result = mv_cur_row.
      RETURN.
    ENDIF.

    rv_result = super->zif_app_rule~get_cur_row( ).
    IF is_logic_active(  ) = abap_true
        AND mr_prev_rule IS NOT INITIAL.
      IF  mr_prev_rule->get_cur_row( ) = rv_result.
        rv_result = rv_result + 1.
      ENDIF.
    ENDIF.

    zif_app_rule~set_cur_row( rv_result ).
  ENDMETHOD.


  METHOD zif_app_rule~get_cur_offset_start.

    IF mv_cur_offset_start_set = abap_true.
      rv_result = mv_cur_offset_start.
      RETURN.
    ENDIF.

    IF is_logic_active(  ) = abap_false.
      rv_result = super->zif_app_rule~get_cur_offset_start(  ).
      RETURN.
    ENDIF.

    IF has_prev_rule_same_type(  ) = abap_false.
      rv_result = mv_default_line_indent.
    ELSE.
      rv_result = mr_prev_rule->get_new_line_indent( ).
    ENDIF.
    zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
    rv_result = rv_result + mv_add_indent.
    zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).


    zif_app_rule~set_cur_offset_start( rv_result ).

  ENDMETHOD.
  METHOD zif_app_rule~finalize_init.
    super->zif_app_rule~finalize_init( ).

    IF is_logic_active( ) = abap_true.
      IF NOT mr_prev_rule IS INITIAL.
        IF mr_prev_rule->get_token_up( ) = '('.
          mv_logic_active = abap_false.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
