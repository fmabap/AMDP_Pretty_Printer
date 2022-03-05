CLASS zcl_app_rule_finder DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zif_app_rule_finder.
    METHODS constructor
      RAISING zcx_app_exception.


  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mt_rule_data TYPE zapp_t_rule_sort.

    METHODS get_rule_data_by_full_spec
      IMPORTING is_rule_search      TYPE zapp_s_rule_search
      RETURNING VALUE(rr_rule_data) TYPE REF TO zapp_s_rule.

    METHODS get_rule_data_w_context
      IMPORTING is_rule_search      TYPE zapp_s_rule_search
      RETURNING VALUE(rr_rule_data) TYPE REF TO zapp_s_rule.


    METHODS get_rule_data_wo_token
      IMPORTING is_rule_search      TYPE zapp_s_rule_search
      RETURNING VALUE(rr_rule_data) TYPE REF TO zapp_s_rule.

    METHODS get_rule_data_w_token
      IMPORTING is_rule_search      TYPE zapp_s_rule_search
      RETURNING VALUE(rr_rule_data) TYPE REF TO zapp_s_rule.

    METHODS get_rule_data_default
      IMPORTING is_rule_search      TYPE zapp_s_rule_search
      RETURNING VALUE(rr_rule_data) TYPE REF TO zapp_s_rule.

ENDCLASS.



CLASS ZCL_APP_RULE_FINDER IMPLEMENTATION.


  METHOD constructor.

    DATA lr_rule_prov TYPE REF TO zif_app_rule_provider.

    CREATE OBJECT lr_rule_prov TYPE zcl_app_rule_provider.
    mt_rule_data = lr_rule_prov->get_rules( ).

  ENDMETHOD.


  METHOD get_rule_data_by_full_spec.

    READ TABLE mt_rule_data REFERENCE INTO rr_rule_data
    WITH TABLE KEY token = is_rule_search-token
             context = is_rule_search-context
             hl_context = is_rule_search-hl_context
             sqlscript = is_rule_search-sqlscript.

  ENDMETHOD.


  METHOD get_rule_data_default.
    DATA lv_hl_context TYPE zapp_d_hl_context.
    DATA lv_context TYPE zapp_d_context.
    DATA lv_token TYPE zapp_d_token.

    READ TABLE mt_rule_data REFERENCE INTO rr_rule_data
    WITH TABLE KEY token = lv_token
             context = lv_context
             hl_context = lv_hl_context
             sqlscript = is_rule_search-sqlscript.
  ENDMETHOD.


  METHOD get_rule_data_wo_token.
    DATA lv_token TYPE zapp_d_token.

    READ TABLE mt_rule_data REFERENCE INTO rr_rule_data
    WITH TABLE KEY token = lv_token
             context = is_rule_search-context
             hl_context = is_rule_search-hl_context
             sqlscript = is_rule_search-sqlscript.
  ENDMETHOD.


  METHOD get_rule_data_w_context.
    DATA lv_hl_context TYPE zapp_d_hl_context.
    READ TABLE mt_rule_data REFERENCE INTO rr_rule_data
    WITH TABLE KEY token = is_rule_search-token
             context = is_rule_search-context
             hl_context = lv_hl_context
             sqlscript = is_rule_search-sqlscript.
  ENDMETHOD.


  METHOD get_rule_data_w_token.
    DATA lv_hl_context TYPE zapp_d_hl_context.
    DATA lv_context TYPE zapp_d_context.
    READ TABLE mt_rule_data REFERENCE INTO rr_rule_data
    WITH TABLE KEY token = is_rule_search-token
             context = lv_context
             hl_context = lv_hl_context
             sqlscript = is_rule_search-sqlscript.
  ENDMETHOD.


  METHOD zif_app_rule_finder~get_rule_data.
    rr_rule_data = get_rule_data_by_full_spec( is_rule_search = is_rule_search ).
    IF rr_rule_data IS NOT INITIAL.
      RETURN.
    ENDIF.

    rr_rule_data = get_rule_data_w_context( is_rule_search = is_rule_search ).
    IF rr_rule_data IS NOT INITIAL.
      RETURN.
    ENDIF.

    rr_rule_data = get_rule_data_wo_token( is_rule_search = is_rule_search ).
    IF rr_rule_data IS NOT INITIAL.
      RETURN.
    ENDIF.

    rr_rule_data = get_rule_data_w_token( is_rule_search = is_rule_search ).
    IF rr_rule_data IS NOT INITIAL.
      RETURN.
    ENDIF.

    rr_rule_data = get_rule_data_default( is_rule_search = is_rule_search ).
    IF rr_rule_data IS NOT INITIAL.
      RETURN.
    ENDIF.

    RAISE EXCEPTION TYPE zcx_app_exception
      MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
      TYPE 'E'
      NUMBER '005'
      WITH
      is_rule_search-token
      is_rule_search-context
      is_rule_search-hl_context
      is_rule_search-sqlscript.

  ENDMETHOD.
ENDCLASS.
