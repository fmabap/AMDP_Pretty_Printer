*&---------------------------------------------------------------------*
*& Report ZAPP_PRETTY_PRINTER_TEST
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zapp_pretty_printer_test.


*&---------------------------------------------------------------------*
*& Class lcl_logic
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
CLASS lcl_logic DEFINITION FINAL.
  PUBLIC SECTION.
    DATA: mr_cc_input   TYPE REF TO cl_gui_custom_container,
          mr_text_input TYPE REF TO cl_gui_textedit.
    DATA: mr_cc_output   TYPE REF TO cl_gui_custom_container,
          mr_text_output TYPE REF TO cl_gui_textedit.
    DATA: mr_cc_input_stm   TYPE REF TO cl_gui_custom_container,
          mr_text_input_stm TYPE REF TO cl_gui_textedit.
    DATA: mr_cc_output_stm   TYPE REF TO cl_gui_custom_container,
          mr_text_output_stm TYPE REF TO cl_gui_textedit.

    METHODS create_controls.
    METHODS pretty_print.
ENDCLASS.

CLASS lcl_logic IMPLEMENTATION.

  METHOD create_controls.

    CHECK mr_cc_input IS INITIAL.

    CREATE OBJECT mr_cc_input
      EXPORTING
        container_name = 'INPUT_TEXT'.

    CREATE OBJECT mr_text_input
      EXPORTING
        parent = mr_cc_input.

    CREATE OBJECT mr_cc_output
      EXPORTING
        container_name = 'OUTPUT_TEXT'.

    CREATE OBJECT mr_text_output
      EXPORTING
        parent = mr_cc_output.

    CREATE OBJECT mr_cc_input_stm
      EXPORTING
        container_name = 'INPUT_TEXT_STATEMENT'.

    CREATE OBJECT mr_text_input_stm
      EXPORTING
        parent = mr_cc_input_stm.

    CREATE OBJECT mr_cc_output_stm
      EXPORTING
        container_name = 'OUTPUT_TEXT_STATEMENT'.

    CREATE OBJECT mr_text_output_stm
      EXPORTING
        parent = mr_cc_output_stm.

    mr_text_output->set_readonly_mode(
      EXPORTING
        readonly_mode          = cl_gui_textedit=>true
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mr_text_input_stm->set_readonly_mode(
      EXPORTING
        readonly_mode          = cl_gui_textedit=>true
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mr_text_output_stm->set_readonly_mode(
      EXPORTING
        readonly_mode          = cl_gui_textedit=>true
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        OTHERS                 = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD pretty_print.
    DATA lt_source TYPE sourcetable.
    DATA lt_source_res TYPE sourcetable.
    DATA lt_source_stm TYPE sourcetable.
    DATA lt_source_res_stm TYPE sourcetable.
    DATA lr_pretty_printer TYPE REF TO zcl_app_pretty_printer.
    DATA lr_ex TYPE REF TO zcx_app_exception.
    DATA lv_source TYPE string.
    DATA lv_source_res TYPE string.
    DATA lv_source_stm TYPE string.
    DATA lv_source_res_stm TYPE string.
    DATA lr_settings TYPE REF TO zif_app_settings.


    mr_text_output->delete_text( ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mr_text_input_stm->delete_text( ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mr_text_output_stm->delete_text( ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    mr_text_input->get_textstream(
*  EXPORTING
*    only_when_modified     = false            " get text only when modified
      IMPORTING
        text                   = lv_source                 " Text as String with Carriage Returns and Linefeeds
*       is_modified            =                  " modify status of text
      EXCEPTIONS
        error_cntl_call_method = 1                " Error while retrieving a property from TextEdit control
        not_supported_by_gui   = 2                " Method is not supported by installed GUI
        OTHERS                 = 3
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    cl_gui_cfw=>flush( ).

    SPLIT lv_source AT cl_abap_char_utilities=>cr_lf INTO TABLE lt_source.

    lt_source_stm =   zcl_app_utilities=>conv_source_to_statement( lt_source ).
    lv_source_stm = zcl_app_utilities=>conv_source_tab_to_string( lt_source_stm ).

    mr_text_input_stm->set_textstream(
      EXPORTING
        text = lv_source_stm
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    cl_gui_cfw=>flush( ).

    CREATE OBJECT lr_pretty_printer.
    TRY.
        CREATE OBJECT lr_settings TYPE zcl_app_settings.
        lt_source_res = lr_pretty_printer->pretty_print(
          it_source   = lt_source
          ir_settings = lr_settings ).

      CATCH zcx_app_exception INTO lr_ex.
        MESSAGE lr_ex->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

    lv_source_res = zcl_app_utilities=>conv_source_tab_to_string( lt_source_res ).

    mr_text_output->set_textstream(
      EXPORTING
        text = lv_source_res
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    cl_gui_cfw=>flush( ).

    lt_source_res_stm =   zcl_app_utilities=>conv_source_to_statement( it_source = lt_source_res iv_var = 'lt_source_res_exp' ).
    lv_source_res_stm = zcl_app_utilities=>conv_source_tab_to_string( lt_source_res_stm ).

    mr_text_output_stm->set_textstream(
      EXPORTING
        text = lv_source_res_stm
    ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    cl_gui_cfw=>flush( ).
  ENDMETHOD.

ENDCLASS.

INCLUDE zapp_pretty_printer_test_so01.
DATA gr_logic TYPE REF TO lcl_logic.

START-OF-SELECTION.
  CREATE OBJECT gr_logic.
  CALL SCREEN 0100.

  INCLUDE zapp_pretty_printer_test_ui01.

  INCLUDE zapp_pretty_printer_test_co01.
