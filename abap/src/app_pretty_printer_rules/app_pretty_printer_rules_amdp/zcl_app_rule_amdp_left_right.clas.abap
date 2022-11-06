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
      IF mr_settings->is_no_lb_at_co_s_fu_dep_sfu( ) = abap_false
         AND mr_settings->is_no_lb_at_co_s_fu_dep_cbr_o( ) = abap_false
         AND mr_settings->is_no_lb_at_co_s_fu_dep_sfu_kw( ) = abap_false.
        RETURN.
      ENDIF.

      IF zcl_app_amdp_rule_utilities=>is_cls_bra_of_fu_in_same_line( me ) = abap_false.
        RETURN.
      ENDIF.

      IF mr_settings->is_no_lb_at_co_s_fu_dep_sfu( ) = abap_true
         AND zcl_app_amdp_rule_utilities=>contains_fu_sub_fu_w_co_or_sel( me ) = abap_true.
        RETURN.
      ENDIF.

      IF mr_settings->is_no_lb_at_co_s_fu_dep_sfu_kw( ) = abap_true
         AND zcl_app_amdp_rule_utilities=>cnts_fu_kw_or_sfu_w_co_or_sel( me ) = abap_true.
        RETURN.
      ENDIF.

      zcl_app_amdp_rule_utilities=>avoid_lb_after_comma_in_func( me ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
