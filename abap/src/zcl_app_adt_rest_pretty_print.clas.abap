CLASS ZCL_APP_ADT_REST_PRETTY_PRINT DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_rest_resource
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS post  REDEFINITION .
    METHODS get  REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.


ENDCLASS.



CLASS ZCL_APP_ADT_REST_PRETTY_PRINT IMPLEMENTATION.


  METHOD get.
    DATA lv_response TYPE string.
    lv_response = 'Welcome to Pretty Printer, please use POST Method'.

    response->set_body_data(
      content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text( )
      data            = lv_response ).
  ENDMETHOD.


  METHOD post.
    DATA lt_source     TYPE sourcetable.
    DATA lt_app_res TYPE sourcetable.
    DATA lr_ex_pp TYPE REF TO zcx_app_exception.
    data lr_settings type ref to zif_app_settings.


    request->get_body_data(
      EXPORTING
        content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text( )
      IMPORTING
        data            = lt_source ).

    TRY.
        create object lr_settings type zcl_app_settings.
        lt_app_res =  NEW zcl_app_pretty_printer( )->pretty_print(
                                                    it_source   = lt_source
                                                    ir_settings = lr_settings ).
      CATCH zcx_app_exception INTO lr_ex_pp.
        RAISE EXCEPTION TYPE cx_sedi_adt_err_pretty_printer
          EXPORTING
            textid = cx_adt_rest_data_invalid=>create_textid_from_exc_text( exception = lr_ex_pp ).
    ENDTRY.

    response->set_body_data(
      content_handler = cl_adt_rest_cnt_hdl_factory=>get_instance( )->get_handler_for_plain_text( )
      data            = lt_app_res ).

  ENDMETHOD.
ENDCLASS.
