CLASS zcl_app_rule_factory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING ir_settings TYPE REF TO zif_app_settings
      RAISING   zcx_app_exception.

    METHODS get_rule
      IMPORTING ir_t_source      TYPE REF TO sourcetable
                ir_t_statement   TYPE REF TO sstmnt_tab
                ir_t_structure   TYPE REF TO sstruc_tab
                ir_prev_rule     TYPE REF TO zif_app_rule
                ir_token_ext     TYPE REF TO zapp_s_stokesx_ext
      RETURNING VALUE(rr_result) TYPE REF TO zif_app_rule
      RAISING   zcx_app_exception.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mr_rule_finder TYPE REF TO zif_app_rule_finder.
    DATA mr_settings TYPE REF TO zif_app_settings.

    METHODS get_rule_search
      IMPORTING
                ir_token_ext     TYPE REF TO zapp_s_stokesx_ext
                ir_prev_rule     TYPE REF TO zif_app_rule OPTIONAL
      RETURNING VALUE(rs_result) TYPE zapp_s_rule_search
      RAISING   zcx_app_exception.
ENDCLASS.



CLASS zcl_app_rule_factory IMPLEMENTATION.


  METHOD constructor.

    CREATE OBJECT mr_rule_finder TYPE zcl_app_rule_finder.
    mr_settings = ir_settings.
  ENDMETHOD.


  METHOD get_rule.
    DATA lr_rule_data TYPE REF TO zapp_s_rule.
    DATA ls_rule_search TYPE zapp_s_rule_search.
    DATA lr_rule_data_new TYPE REF TO zapp_s_rule.


    ls_rule_search = get_rule_search(
      ir_token_ext = ir_token_ext
      ir_prev_rule = ir_prev_rule ).

    lr_rule_data = mr_rule_finder->get_rule_data( is_rule_search = ls_rule_search ).

    IF lr_rule_data->rule_class IS INITIAL.
      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '006'
        WITH
        lr_rule_data->rule_name.
    ENDIF.

    CREATE OBJECT rr_result TYPE (lr_rule_data->rule_class).

    CREATE DATA lr_rule_data_new.

    lr_rule_data_new->* = lr_rule_data->*.

    IF ir_prev_rule IS INITIAL.
      rr_result->init(
        EXPORTING
          ir_token_ext   = ir_token_ext
          ir_t_source    = ir_t_source
          ir_t_statement = ir_t_statement
          ir_t_structure = ir_t_structure
          ir_rule_data   = lr_rule_data_new
          ir_settings    = mr_settings ).
    ELSE.

      rr_result->init(
        EXPORTING
          ir_token_ext       = ir_token_ext
          ir_t_source        = ir_t_source
          ir_t_statement     = ir_t_statement
          ir_t_structure     = ir_t_structure
          ir_rule_data       = lr_rule_data_new
          ir_settings        = mr_settings
          ir_context_rule    = ir_prev_rule->get_new_context_rule( )
          ir_hl_context_rule = ir_prev_rule->get_new_hl_context_rule( )
          ir_prev_rule       = ir_prev_rule ).
    ENDIF.
  ENDMETHOD.


  METHOD get_rule_search.
    rs_result-token = ir_token_ext->str_up.
    rs_result-sqlscript = ir_token_ext->sqlscript.
    IF ir_prev_rule IS NOT INITIAL.
      rs_result-context = ir_prev_rule->get_new_context( ).
      rs_result-hl_context = ir_prev_rule->get_new_hl_context( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
