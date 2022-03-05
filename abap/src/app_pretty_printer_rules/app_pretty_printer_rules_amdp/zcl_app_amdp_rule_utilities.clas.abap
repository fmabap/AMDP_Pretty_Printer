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
ENDCLASS.
