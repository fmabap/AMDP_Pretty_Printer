"! <p class="shorttext synchronized" lang="en">Pretty Printer Rule for LEFT and RIGHT</p>
"! Do the inherited logic if the condition is full filled
"! else do the comma rule logic for the string function left and right
CLASS zcl_app_rule_amdp_left_right DEFINITION
  PUBLIC
  INHERITING FROM zcl_app_rule_amdp_nl_lft_cond

  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS zif_app_rule~finalize_init REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_app_rule_amdp_left_right IMPLEMENTATION.

  METHOD zif_app_rule~finalize_init.

    super->zif_app_rule~finalize_init( ).
    IF is_logic_active(  ) = abap_false.
      RETURN.
    ENDIF.

    IF mv_cond_fulfilled_set = abap_false.
      IF zcl_app_amdp_rule_utilities=>is_avd_lb_aft_comma_in_fu_act(
       ir_start_rule = me
       ir_settings   = mr_settings ) = abap_true.

        zcl_app_amdp_rule_utilities=>avoid_lb_after_comma_in_func( me ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
