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

    IF zcl_app_amdp_rule_utilities=>is_avd_lb_aft_comma_in_fu_act(
         ir_start_rule = me
         ir_settings   = mr_settings ) = abap_true.

      zcl_app_amdp_rule_utilities=>avoid_lb_after_comma_in_func( me ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
