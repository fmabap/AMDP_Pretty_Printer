CLASS ZCL_APP_ADT_DISC_PRETTY_PRINT DEFINITION
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



CLASS ZCL_APP_ADT_DISC_PRETTY_PRINT IMPLEMENTATION.


  METHOD get_application_title.
    result = 'ABAP and AMDP Pretty Printer'(001).
  ENDMETHOD.


  METHOD if_adt_rest_rfc_application~get_static_uri_path.
    result = '/sap/bc/adt/zapp'.
  ENDMETHOD.


  METHOD register_resources.
    DATA: accepted_types     TYPE if_adt_discovery_collection=>ty_accepts.

    DATA(lr_collection) = registry->register_discoverable_resource(
      url             = '/zapp_pretty_printer'
      handler_class   = 'ZCL_APP_ADT_REST_PRETTY_PRINT'
      description     = 'Pretty Printer'
      category_scheme = co_root_schema
      category_term   = 'pretty_printer' ).

  ENDMETHOD.
ENDCLASS.
