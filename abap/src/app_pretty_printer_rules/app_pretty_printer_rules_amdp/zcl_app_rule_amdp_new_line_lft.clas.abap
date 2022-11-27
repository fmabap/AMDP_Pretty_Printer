CLASS zcl_app_rule_amdp_new_line_lft DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_new_line
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~get_cur_offset_start REDEFINITION.

  PROTECTED SECTION.
    METHODS set_add_indent
      RAISING zcx_app_exception.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_rule_amdp_new_line_lft IMPLEMENTATION.

  METHOD zif_app_rule~get_cur_offset_start.

    IF mv_cur_offset_start_set = abap_true.
      rv_result = mv_cur_offset_start.
      RETURN.
    ENDIF.

    set_add_indent( ).

    rv_result = super->zif_app_rule~get_cur_offset_start(  ).

  ENDMETHOD.

  METHOD set_add_indent.
    DATA lt_text TYPE sourcetable.
    DATA lr_text TYPE REF TO string.

    IF is_logic_active( ) = abap_true.
      IF mr_rule_data->add_indent IS INITIAL.

        lt_text = zif_app_rule~get_text( ).

        READ TABLE lt_text REFERENCE INTO lr_text
        INDEX 1.
        IF sy-subrc = 0.
          mv_add_indent = strlen( lr_text->* ) * -1.
        ENDIF.
      ELSE.
        mv_add_indent = mr_rule_data->add_indent.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
