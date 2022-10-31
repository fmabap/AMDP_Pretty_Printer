"! AMDP Rule to avoid line break at comma if the closing bracket of this function is in the same row
"! and addition there are no sub functions that contains a comma
"! Example: "substring(rtrim(table.field),3,4)" will be in one row,
"! substring(concat("Bla","Blub"),2,3) will be split in several rows
"! <p class="shorttext synchronized" lang="en">Function in one row</p>
CLASS zcl_app_rule_amdp_func_in_1_ro DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_default_no_c
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS zif_app_rule~finalize_init REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_rule_amdp_func_in_1_ro IMPLEMENTATION.



  METHOD zif_app_rule~finalize_init.

    super->zif_app_rule~finalize_init( ).
    IF is_logic_active(  ) = abap_false.
      RETURN.
    ENDIF.

    IF mr_settings->is_no_lb_at_co_s_fu_dep_sfu( ) = abap_false
       AND mr_settings->is_no_lb_at_co_s_fu_dep_cbr_o( ) = abap_false.
      RETURN.
    ENDIF.

    IF zcl_app_amdp_rule_utilities=>is_cls_bra_of_fu_in_same_line( me ) = abap_false.
      RETURN.
    ENDIF.

    IF mr_settings->is_no_lb_at_co_s_fu_dep_sfu( ) = abap_true
       AND zcl_app_amdp_rule_utilities=>contains_fu_sub_fu_w_co_or_sel( me ) = abap_true.
      RETURN.
    ENDIF.

    zcl_app_amdp_rule_utilities=>avoid_lb_after_comma_in_func( me ).


  ENDMETHOD.

ENDCLASS.
