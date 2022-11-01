CLASS zcl_app_rule_amdp_sel_ups_ins DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_default_no_c
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~finalize_init REDEFINITION.
    METHODS zif_app_rule~get_new_line_indent REDEFINITION.
    METHODS zif_app_rule~get_cur_offset_start REDEFINITION.
    METHODS zif_app_rule~get_cur_row REDEFINITION.
    METHODS zif_app_rule~refresh_buffer REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS set_additional_indent
      RAISING
        zcx_app_exception.

    METHODS get_join_rule
      IMPORTING iv_token         TYPE zapp_d_token
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    METHODS get_order_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    METHODS get_default_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    METHODS get_group_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    METHODS get_prev_select_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    METHODS get_prev_ups_ins_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    METHODS get_distinct_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    METHODS get_longest_join_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception.

    METHODS get_union_all_rule
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception .

    DATA mv_add_indent_set TYPE abap_bool.


ENDCLASS.

CLASS zcl_app_rule_amdp_sel_ups_ins IMPLEMENTATION.
  METHOD zif_app_rule~get_new_line_indent.
    IF is_logic_active(  ) = abap_false.
      rv_result = super->zif_app_rule~get_new_line_indent( ).
      RETURN.
    ENDIF.

    rv_result = zif_app_rule~get_cur_offset_end( ).

  ENDMETHOD.

  METHOD zif_app_rule~get_cur_offset_start.
    DATA lr_prev_insert_rule TYPE REF TO zif_app_rule.
    IF mv_cur_offset_start_set = abap_true.
      rv_result = mv_cur_offset_start.
      RETURN.
    ENDIF.

    IF is_logic_active(  ) = abap_true.

      " if we have insert with select then move the select to the begin of a new line
      " with the required indent #5
      lr_prev_insert_rule = get_prev_ups_ins_rule( ).
      IF lr_prev_insert_rule IS NOT INITIAL.
        rv_result = mr_prev_rule->get_new_line_indent( ).
        zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
        rv_result = rv_result + mv_add_indent.
        zcl_app_utilities=>set_to_0_if_negativ( CHANGING cv_value = rv_result ).
        zif_app_rule~set_cur_offset_start( rv_result ).
        RETURN.
      ELSE.
        set_additional_indent( ).
      ENDIF.
    ENDIF.
    rv_result = super->zif_app_rule~get_cur_offset_start( ).

  ENDMETHOD.

  METHOD set_additional_indent.

    DATA lr_prev_select_rule TYPE REF TO zif_app_rule.
    DATA lr_join_rule TYPE REF TO zif_app_rule.
    DATA lr_order_rule TYPE REF TO zif_app_rule.
    DATA lr_group_rule TYPE REF TO zif_app_rule.
    DATA lr_distinct_rule TYPE REF TO zif_app_rule.
    DATA lr_union_all_rule TYPE REF TO zif_app_rule.
    DATA lr_default_rule TYPE REF TO zif_app_rule.

    IF mv_add_indent_set = abap_true.
      RETURN.
    ENDIF.

    lr_prev_select_rule = get_prev_select_rule(  ).
    IF lr_prev_select_rule IS NOT INITIAL.
      mv_add_indent = lr_prev_select_rule->get_additional_indent( ).
      mv_add_indent_set = abap_true.
      RETURN.
    ENDIF.

    lr_join_rule = get_longest_join_rule( ).

    IF lr_join_rule IS NOT INITIAL.
      CASE lr_join_rule->get_token_up( ).
        WHEN 'LEFT'.
          mv_add_indent = 9.
        WHEN 'RIGHT'.
          mv_add_indent = 10.
        WHEN 'CROSS' OR 'INNER'.
          mv_add_indent = 4.
      ENDCASE.

      mv_add_indent_set = abap_true.
      RETURN.

    ENDIF.

    lr_union_all_rule = get_union_all_rule( ).

    IF NOT lr_union_all_rule IS INITIAL.
      mv_add_indent = 3.
      mv_add_indent_set = abap_true.
      RETURN.
    ENDIF.

    lr_distinct_rule = get_distinct_rule( ).
    IF NOT lr_distinct_rule IS INITIAL.
      mv_add_indent = 2.
      mv_add_indent_set = abap_true.
      RETURN.
    ENDIF.

    lr_order_rule = get_order_rule( ).

    IF NOT lr_order_rule IS INITIAL.
      mv_add_indent = 2.
      mv_add_indent_set = abap_true.
      RETURN.
    ENDIF.

    lr_group_rule = get_group_rule( ).

    IF NOT lr_group_rule IS INITIAL.
      mv_add_indent = 2.
      mv_add_indent_set = abap_true.
      RETURN.
    ENDIF.

    lr_default_rule = get_default_rule( ).
    IF NOT lr_default_rule IS INITIAL.
      mv_add_indent = 1.
      mv_add_indent_set = abap_true.
      RETURN.
    ENDIF.

    mv_add_indent_set = abap_true.
  ENDMETHOD.

  METHOD get_join_rule.
    DATA lt_token TYPE zapp_t_token.
    DATA lt_stop_token TYPE zapp_t_token.
    DATA lr_next_rule TYPE REF TO zif_app_rule.
    DATA lr_start_rule TYPE REF TO zif_app_rule.

    INSERT  iv_token INTO TABLE lt_token.
    lr_start_rule = me.
    DO.

      rr_result = zcl_app_amdp_rule_utilities=>get_rule_in_stm_on_same_level(
        EXPORTING
          ir_start_rule = lr_start_rule
          it_token      = lt_token
          it_stop_token = lt_stop_token
      ).

      IF rr_result IS INITIAL.
        RETURN.
      ENDIF.

      CASE rr_result->get_token_up( ).
        WHEN 'LEFT' OR 'RIGHT'.
          lr_next_rule = rr_result->get_next_rule( ).
          IF  lr_next_rule IS NOT INITIAL.
            IF lr_next_rule->get_token_up( ) = '('.
              "the rule doesn't belong to the join condition, it is the function
              lr_start_rule = lr_next_rule.
              CONTINUE.
            ENDIF.
          ENDIF.
      ENDCASE.
      RETURN.
    ENDDO.

  ENDMETHOD.

  METHOD get_order_rule.
    rr_result = zcl_app_amdp_rule_utilities=>get_1_rule_in_stm_on_same_lvl(
      ir_start_rule = me
      iv_token      = 'ORDER'
    ).


  ENDMETHOD.

  METHOD get_default_rule.
    rr_result = zcl_app_amdp_rule_utilities=>get_1_rule_in_stm_on_same_lvl(
      ir_start_rule = me
      iv_token      = 'DEFAULT'
    ).


  ENDMETHOD.

  METHOD get_union_all_rule.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lr_next_rule TYPE REF TO zif_app_rule.

    lr_rule = zcl_app_amdp_rule_utilities=>get_1_rule_in_stm_on_same_lvl(
      ir_start_rule = me
      iv_token      = 'UNION'
    ).

    IF lr_rule IS NOT INITIAL.
      lr_next_rule = lr_rule->get_next_rule( ).
      IF lr_next_rule IS NOT INITIAL.
        IF lr_next_rule->get_token_up( ) = 'ALL'.
          rr_result = lr_rule.
        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_longest_join_rule.

    DATA lv_token TYPE zapp_d_token.
    lv_token = 'RIGHT'.
    rr_result = get_join_rule( lv_token ).

    IF NOT rr_result IS INITIAL.
      RETURN.
    ENDIF.

    lv_token = 'LEFT'.
    rr_result = get_join_rule( lv_token ).

    IF NOT rr_result IS INITIAL.
      RETURN.
    ENDIF.

    lv_token = 'INNER'.
    rr_result = get_join_rule( lv_token ).

    IF NOT rr_result IS INITIAL.
      RETURN.
    ENDIF.

    lv_token = 'CROSS'.
    rr_result = get_join_rule( lv_token ).


  ENDMETHOD.

  METHOD zif_app_rule~refresh_buffer.
    super->zif_app_rule~refresh_buffer( ).
    CLEAR mv_add_indent_set.
  ENDMETHOD.

  METHOD zif_app_rule~finalize_init.
    super->zif_app_rule~finalize_init( ).
    IF is_logic_active( ) = abap_true.
      mr_token_ext->delimiter = zcl_app_utilities=>get_space_as_delimiter( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_group_rule.

    rr_result = zcl_app_amdp_rule_utilities=>get_1_rule_in_stm_on_same_lvl(
      ir_start_rule = me
      iv_token      = 'GROUP'
    ).

  ENDMETHOD.

  METHOD get_distinct_rule.

    rr_result = zcl_app_amdp_rule_utilities=>get_1_rule_in_stm_on_same_lvl(
      ir_start_rule = me
      iv_token      = 'DISTINCT'
    ).


  ENDMETHOD.
  METHOD get_prev_select_rule.

    rr_result = zcl_app_amdp_rule_utilities=>get_1_rl_in_stm_on_same_lvl_rw(
      ir_start_rule = me
      iv_token      = 'SELECT'
    ).

  ENDMETHOD.

  METHOD get_prev_ups_ins_rule.

    DATA lt_token TYPE zapp_t_token.
    DATA lt_stop_token TYPE zapp_t_token.
    DATA lv_token TYPE zapp_d_token.

    lv_token = 'INSERT'.
    INSERT  lv_token INTO TABLE lt_token.

    lv_token = 'UPSERT'.
    INSERT  lv_token INTO TABLE lt_token.

    rr_result = zcl_app_amdp_rule_utilities=>get_rule_in_stm_on_same_lvl_rw(
      EXPORTING
        ir_start_rule = me
        it_token      = lt_token
        it_stop_token = lt_stop_token
    ).

  ENDMETHOD.

  METHOD zif_app_rule~get_cur_row.
    DATA lv_cur_row TYPE i.
    DATA lr_prev_insert_rule TYPE REF TO zif_app_rule.

    IF mv_cur_row_set = abap_true.
      rv_result = mv_cur_row.
      RETURN.
    ENDIF.

    lv_cur_row = super->zif_app_rule~get_cur_row( ).
    " if we have insert with select then move the select to a new line if required
    IF NOT mr_prev_rule IS INITIAL AND is_logic_active(  ) = abap_true.
      IF mr_prev_rule->get_cur_row( ) = lv_cur_row.
        lr_prev_insert_rule = get_prev_ups_ins_rule( ).
        IF lr_prev_insert_rule IS NOT INITIAL.
          lv_cur_row = lv_cur_row + 1.
          zif_app_rule~set_cur_row( lv_cur_row ).
        ENDIF.
      ENDIF.
    ENDIF.
    rv_result = super->zif_app_rule~get_cur_row( ).
  ENDMETHOD.

ENDCLASS.
