CLASS zcl_app_rule_abap_dummy DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_base_rule

  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~get_cur_offset_start REDEFINITION.
    METHODS zif_app_rule~get_cur_row REDEFINITION.
    METHODS zif_app_rule~get_text REDEFINITION.
    METHODS zif_app_rule~init REDEFINITION.
    METHODS zif_app_rule~finalize_init REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_rule_abap_dummy IMPLEMENTATION.

  METHOD zif_app_rule~get_cur_offset_start.
    rv_result = mr_token_ext->col.
  ENDMETHOD.


  METHOD zif_app_rule~get_cur_row.
    DATA lr_token_ext_prev  TYPE REF TO zapp_s_stokesx_ext.
    IF mr_prev_rule IS INITIAL.
      rv_result = mr_token_ext->row.
      RETURN.
    ENDIF.

    lr_token_ext_prev = mr_prev_rule->get_token_ext( ).
    IF zcl_app_utilities=>is_abap_token( lr_token_ext_prev->sqlscript ) = abap_true.

      rv_result = mr_prev_rule->get_end_row( ).

    ELSE.
      IF mr_prev_rule->has_multline_delimiter( ) = abap_true.
        rv_result = mr_prev_rule->get_end_row( ).
      ELSE.
        rv_result = mr_prev_rule->get_end_row( ) + 1.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD zif_app_rule~get_text.
    DATA lr_delimiter TYPE REF TO string.
    DATA lr_result TYPE REF TO string.
    IF  mr_token_ext->delimiter_org IS INITIAL.
      INSERT mr_token_ext->str_org  INTO TABLE rt_result.
      RETURN.
    ENDIF.
    LOOP AT mr_token_ext->delimiter_org REFERENCE INTO lr_delimiter.
      INSERT INITIAL LINE INTO TABLE rt_result REFERENCE INTO lr_result.
      IF sy-tabix = 1.
        CONCATENATE mr_token_ext->str_org lr_delimiter->* INTO lr_result->*.
      ELSE.
        lr_result->* = lr_delimiter->*.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zif_app_rule~init.
    IF zcl_app_utilities=>is_abap_token( ir_token_ext->sqlscript ) = abap_false.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '009'
        WITH ir_rule_data->rule_name.

    ENDIF.
    super->zif_app_rule~init(
      EXPORTING
        ir_token_ext       = ir_token_ext
        ir_t_source        = ir_t_source
        ir_t_statement     = ir_t_statement
        ir_t_structure     = ir_t_structure
        ir_rule_data       = ir_rule_data
        ir_context_rule    = ir_context_rule
        ir_hl_context_rule = ir_hl_context_rule
        ir_prev_rule       = ir_prev_rule ).

  ENDMETHOD.

  METHOD zif_app_rule~finalize_init.
    DATA lr_delimiter TYPE REF TO string.
    IF mr_token_ext->sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-end_of_pending
    AND lines( mr_token_ext->delimiter_org ) > 1.

      READ TABLE mr_token_ext->delimiter_org REFERENCE INTO lr_delimiter
      INDEX ( lines( mr_token_ext->delimiter_org ) ).
      IF sy-subrc = 0.
        IF lr_delimiter->* IS INITIAL OR lr_delimiter->* CO ` `.
          CLEAR lr_delimiter->*.
        ELSE.
          INSERT INITIAL LINE INTO TABLE mr_token_ext->delimiter_org.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
