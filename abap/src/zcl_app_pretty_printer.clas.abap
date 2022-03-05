CLASS zcl_app_pretty_printer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS pretty_print
      IMPORTING
        !it_source       TYPE sourcetable
      RETURNING
        VALUE(rt_source) TYPE sourcetable
      RAISING
        zcx_app_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS scan
      IMPORTING
        !it_source    TYPE sourcetable
      EXPORTING
        !et_token_ext TYPE zapp_t_stokesx_ext_st
        !et_statement TYPE sstmnt_tab
        !et_structure TYPE sstruc_tab
      RAISING
        zcx_app_exception .
    METHODS get_and_apply_rules
      IMPORTING
        !it_source       TYPE sourcetable
        !it_statement    TYPE sstmnt_tab
        !it_structure    TYPE sstruc_tab
      CHANGING
        !ct_token_ext    TYPE zapp_t_stokesx_ext_st
      RETURNING
        VALUE(rt_source) TYPE sourcetable
      RAISING
        zcx_app_exception .
    METHODS get_rules
      IMPORTING
        !it_source       TYPE sourcetable
        !it_statement    TYPE sstmnt_tab
        !it_structure    TYPE sstruc_tab
        !ir_rule_factory TYPE REF TO zcl_app_rule_factory
      CHANGING
        !ct_token_ext    TYPE zapp_t_stokesx_ext_st
      RETURNING
        VALUE(rt_result) TYPE zapp_t_rule_instances
      RAISING
        zcx_app_exception .
    METHODS get_source_code_from_rules
      IMPORTING
        !it_rules        TYPE zapp_t_rule_instances
      RETURNING
        VALUE(rt_result) TYPE sourcetable
      RAISING
        zcx_app_exception .
    METHODS get_act_source_row
      IMPORTING
        !ir_rule    TYPE REF TO zif_app_rule
      CHANGING
        !cr_source  TYPE REF TO string
        !ct_source  TYPE sourcetable
        !cv_act_row TYPE int4
      RAISING
        zcx_app_exception .
    METHODS add_rule_to_source
      IMPORTING
        ir_rule    TYPE REF TO zif_app_rule
      CHANGING
        cr_source  TYPE REF TO string
        ct_source  TYPE sourcetable
        cv_act_row TYPE int4
      RAISING
        zcx_app_exception .
    METHODS calc_rule_result
      IMPORTING
        it_rules TYPE zapp_t_rule_instances
      RAISING
        zcx_app_exception .

    METHODS execute_standard_pretty_print
      IMPORTING
        it_source        TYPE sourcetable
      RETURNING
        VALUE(rt_source) TYPE sourcetable
      RAISING
        zcx_app_exception .


ENDCLASS.



CLASS ZCL_APP_PRETTY_PRINTER IMPLEMENTATION.


  METHOD add_rule_to_source.
    DATA lv_len TYPE i.
    DATA lv_spaces TYPE i.
    DATA lr_token_ext  TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_rule_data TYPE REF TO zapp_s_rule.
    DATA lt_text TYPE sourcetable.
    DATA lr_text TYPE REF TO string.

    lv_len = strlen( cr_source->* ).

    lv_spaces = ( ir_rule->get_cur_offset_start( ) - lv_len ).
    IF lv_spaces < 0.
      lr_token_ext = ir_rule->get_token_ext( ).
      lr_rule_data = ir_rule->get_rule_data( ).

      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '007'
        WITH
        lr_rule_data->rule_name
        lr_token_ext->str_up
        lr_token_ext->row
        lr_token_ext->col.

    ENDIF.
    DO lv_spaces TIMES.
      CONCATENATE cr_source->* space INTO cr_source->* RESPECTING BLANKS.
    ENDDO.
    lt_text = ir_rule->get_text(  ).
    LOOP AT  lt_text REFERENCE INTO lr_text.
      IF sy-tabix = 1.
        CONCATENATE cr_source->* lr_text->* INTO cr_source->* RESPECTING BLANKS.
      ELSE.
        INSERT INITIAL LINE INTO TABLE ct_source REFERENCE INTO cr_source.
        cr_source->* = lr_text->*.
        cv_act_row = cv_act_row + 1.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD calc_rule_result.

    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lt_prev_rule_result TYPE zapp_t_rule_result.
    DATA lt_rule_result TYPE zapp_t_rule_result.
    DATA ls_rule_result TYPE zapp_s_rule_result.

    DO 11 TIMES.

      CLEAR lt_rule_result.
      LOOP AT it_rules INTO lr_rule.
        CLEAR ls_rule_result.
        ls_rule_result-rule = lr_rule.
        ls_rule_result-cur_row = lr_rule->get_cur_row( ).
        ls_rule_result-end_row = lr_rule->get_end_row( ).
        ls_rule_result-cur_off_set_start = lr_rule->get_cur_offset_start( ).
        ls_rule_result-text = lr_rule->get_text( ).
        ls_rule_result-cur_off_set_end = lr_rule->get_cur_offset_end( ).
        INSERT ls_rule_result INTO TABLE lt_rule_result.
      ENDLOOP.
      IF lt_prev_rule_result = lt_rule_result.
        RETURN.
      ENDIF.

      lt_prev_rule_result = lt_rule_result.

    ENDDO.

    RAISE EXCEPTION TYPE zcx_app_exception
      MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
      TYPE 'E'
      NUMBER '013'.

  ENDMETHOD.


  METHOD execute_standard_pretty_print.
    DATA lt_normal_app_res TYPE rswsourcet.
    DATA lt_app_res TYPE sourcetable.
    DATA lr_settings TYPE REF TO if_pretty_printer_settings.
    DATA lt_source_code TYPE swbse_max_line_tab.
    DATA lr_case_mode_settings TYPE REF TO if_pretty_printer_settings.


    lr_settings = NEW cl_pretty_printer_wb_settings( ).

    lt_source_code = it_source.
    CALL FUNCTION 'GET_PRETTY_PRINTER_CASE_MODE'
      EXPORTING
        source_code        = lt_source_code
        settings           = lr_settings
      IMPORTING
        case_mode_settings = lr_case_mode_settings.


    TRY.
        lt_normal_app_res = it_source.
        NEW cl_sedi_pretty_printer( )->format_source( EXPORTING i_settings = lr_case_mode_settings CHANGING c_source = lt_normal_app_res ).
      CATCH cx_sedi_pretty_printer.

        RAISE EXCEPTION TYPE zcx_app_exception
          EXPORTING
            textid = cx_adt_rest_data_invalid=>create_textid_from_msg_params( ).
    ENDTRY.
    rt_source = lt_normal_app_res.

  ENDMETHOD.


  METHOD get_act_source_row.
    DATA lv_rule_row TYPE i.
    DATA lr_token_ext  TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_rule_data TYPE REF TO zapp_s_rule.

    lv_rule_row = ir_rule->get_cur_row( ).

    WHILE cv_act_row < lv_rule_row.

      INSERT INITIAL LINE INTO TABLE ct_source REFERENCE INTO cr_source.
      cv_act_row = cv_act_row + 1.

    ENDWHILE.

    IF cv_act_row > lv_rule_row.
      lr_token_ext = ir_rule->get_token_ext( ).
      lr_rule_data = ir_rule->get_rule_data( ).

      RAISE EXCEPTION TYPE zcx_app_exception
        MESSAGE ID 'ZAPP_MC_PRETTY_PRINT'
        TYPE 'E'
        NUMBER '008'
        WITH
        lr_rule_data->rule_name
        lr_token_ext->str_up
        lr_token_ext->row
        lr_token_ext->col.
    ENDIF.

  ENDMETHOD.


  METHOD get_and_apply_rules.
    DATA lr_rule_factory TYPE REF TO zcl_app_rule_factory.
    DATA lt_rules TYPE zapp_t_rule_instances.

    CREATE OBJECT lr_rule_factory.

    lt_rules = get_rules(
      EXPORTING
        it_source       = it_source
        it_statement    = it_statement
        it_structure    = it_structure
        ir_rule_factory = lr_rule_factory
      CHANGING
        ct_token_ext    = ct_token_ext ).

    calc_rule_result( it_rules = lt_rules ).

    rt_source = get_source_code_from_rules( it_rules = lt_rules ).

  ENDMETHOD.


  METHOD get_rules.
    DATA lr_token_ext  TYPE REF TO zapp_s_stokesx_ext.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lr_prev_rule TYPE REF TO zif_app_rule.
    DATA lr_t_source      TYPE REF TO sourcetable.
    DATA lr_t_statement   TYPE REF TO sstmnt_tab.
    DATA lr_t_structure   TYPE REF TO sstruc_tab.

    GET REFERENCE OF it_source INTO  lr_t_source.
    GET REFERENCE OF it_statement INTO  lr_t_statement.
    GET REFERENCE OF it_structure INTO  lr_t_structure.

    LOOP AT ct_token_ext REFERENCE INTO lr_token_ext
    USING KEY row_col.

      lr_rule = ir_rule_factory->get_rule(
        EXPORTING
          ir_t_source    = lr_t_source
          ir_t_statement = lr_t_statement
          ir_t_structure = lr_t_structure
          ir_prev_rule   = lr_prev_rule
          ir_token_ext   = lr_token_ext ).

      IF  lr_prev_rule IS NOT INITIAL.
        lr_prev_rule->set_next_rule( lr_rule ).
      ENDIF.

      INSERT lr_rule INTO TABLE rt_result.
      lr_prev_rule = lr_rule.

    ENDLOOP.

    LOOP AT rt_result INTO lr_rule.
      lr_rule->finalize_init( ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_source_code_from_rules.
    DATA lr_rule TYPE REF TO zif_app_rule.
    DATA lv_act_row TYPE int4.
    DATA lr_source TYPE REF TO string.

    LOOP AT it_rules INTO lr_rule.

      get_act_source_row(
        EXPORTING
          ir_rule    = lr_rule
        CHANGING
          cr_source  = lr_source
          cv_act_row = lv_act_row
          ct_source  = rt_result ).

      add_rule_to_source(
        EXPORTING
          ir_rule    = lr_rule
        CHANGING
          cr_source  = lr_source
          cv_act_row = lv_act_row
          ct_source  = rt_result ).

    ENDLOOP.
  ENDMETHOD.


  METHOD pretty_print.

    DATA lt_token_ext TYPE zapp_t_stokesx_ext_st.
    DATA lt_statement TYPE sstmnt_tab.
    DATA lt_structure TYPE sstruc_tab.
    DATA lt_source TYPE sourcetable.

    lt_source = it_source.

    scan(
      EXPORTING
        it_source    = lt_source
      IMPORTING
        et_token_ext = lt_token_ext
        et_statement = lt_statement
        et_structure = lt_structure
    ).

    rt_source = get_and_apply_rules(
      EXPORTING
        it_source    = lt_source
        it_statement = lt_statement
        it_structure = lt_structure
      CHANGING
        ct_token_ext = lt_token_ext ).

  ENDMETHOD.


  METHOD scan.
    DATA lr_scanner TYPE REF TO zcl_app_scanner.
    CREATE OBJECT lr_scanner.

    CLEAR et_structure.
    CLEAR et_token_ext.
    CLEAR et_statement.

    lr_scanner->scan(
      EXPORTING
        it_source    = it_source
      IMPORTING
        et_token_ext = et_token_ext
        et_statement = et_statement
        et_structure = et_structure
    ).

  ENDMETHOD.
ENDCLASS.
