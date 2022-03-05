CLASS ZCL_APP_RULE_AMDP_NEW_LINE_LFT DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_new_line
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~get_cur_offset_start REDEFINITION.

  PROTECTED SECTION.
    METHODS set_add_intend
      RAISING zcx_app_exception.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_APP_RULE_AMDP_NEW_LINE_LFT IMPLEMENTATION.

  METHOD zif_app_rule~get_cur_offset_start.

    DATA lt_text TYPE sourcetable.
    DATA lr_text TYPE REF TO string.

    IF mv_cur_offset_start_set = abap_true.
      rv_result = mv_cur_offset_start.
      RETURN.
    ENDIF.

    set_add_intend( ).

    rv_result = super->zif_app_rule~get_cur_offset_start(  ).

  ENDMETHOD.

  METHOD set_add_intend.
    DATA lt_text TYPE sourcetable.
    DATA lr_text TYPE REF TO string.

    IF is_logic_active( ) = abap_true.
      IF mr_rule_data->add_intend is INITIAL.

        lt_text = zif_app_rule~get_text( ).

        READ TABLE lt_text REFERENCE INTO lr_text
        INDEX 1.
        IF sy-subrc = 0.
          mv_add_intend = strlen( lr_text->* ) * -1.
        ENDIF.
      ELSE.
        mv_add_intend = mr_rule_data->add_intend.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
