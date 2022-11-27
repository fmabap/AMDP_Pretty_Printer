CLASS zcl_app_adt_disc_pretty_print DEFINITION
  PUBLIC
  INHERITING FROM cl_adt_disc_res_app_base

  CREATE PUBLIC .


  PUBLIC SECTION.
    METHODS if_adt_rest_rfc_application~get_static_uri_path REDEFINITION.
    CONSTANTS co_root_schema TYPE string VALUE 'zapp.pretty.printer'. "#EC NOTEXT
    CONSTANTS co_xml_application TYPE string VALUE 'application/xml' ##NO_TEXT.
  PROTECTED SECTION.

    METHODS get_application_title REDEFINITION.
    METHODS register_resources REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_adt_disc_pretty_print IMPLEMENTATION.


  METHOD get_application_title.
    result = 'AMDP Pretty Printer'(001).
  ENDMETHOD.


  METHOD if_adt_rest_rfc_application~get_static_uri_path.
    result = '/sap/bc/adt/zapp'.
  ENDMETHOD.


  METHOD register_resources.

    registry->register_discoverable_resource(
      url             = '/zapp_pretty_printer'
      handler_class   = 'ZCL_APP_ADT_REST_PRETTY_PRINT'
      description     = 'Pretty Printer'
      category_scheme = co_root_schema
      category_term   = 'pretty_printer' ).

  ENDMETHOD.
ENDCLASS.
