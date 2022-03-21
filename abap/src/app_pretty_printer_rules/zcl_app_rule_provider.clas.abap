CLASS zcl_app_rule_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_app_rule_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS get_abap_rules
      RETURNING VALUE(rt_result) TYPE zapp_t_rule_sort
      RAISING   zcx_app_exception.

    METHODS get_amdp_rules
      RETURNING VALUE(rt_result) TYPE zapp_t_rule_sort
      RAISING   zcx_app_exception.

    METHODS get_amdp_context_less_rules
      RETURNING VALUE(rt_result) TYPE zapp_t_rule_sort
      RAISING   zcx_app_exception.

    METHODS get_amdp_default_rules
      RETURNING VALUE(rt_result) TYPE zapp_t_rule_sort
      RAISING   zcx_app_exception.

    METHODS get_amdp_select_rules
      RETURNING VALUE(rt_result) TYPE zapp_t_rule_sort
      RAISING   zcx_app_exception.

    METHODS get_amdp_control_rules
      RETURNING VALUE(rt_result) TYPE zapp_t_rule_sort
      RAISING   zcx_app_exception.

    METHODS get_amdp_special_rules
      RETURNING VALUE(rt_result) TYPE zapp_t_rule_sort
      RAISING   zcx_app_exception.
ENDCLASS.



CLASS zcl_app_rule_provider IMPLEMENTATION.
  METHOD zif_app_rule_provider~get_rules.
    INSERT LINES OF get_abap_rules( ) INTO TABLE rt_result.
    INSERT LINES OF get_amdp_rules( ) INTO TABLE rt_result.

  ENDMETHOD.


  METHOD get_abap_rules.
    DATA ls_result TYPE zapp_s_rule.

    CLEAR ls_result.
    ls_result-rule_name = 'DUMMY ABAP SQL N'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-none.
    ls_result-rule_class = 'ZCL_APP_RULE_ABAP_DUMMY'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'DUMMY ABAP SQL P'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-pending.
    ls_result-rule_class = 'ZCL_APP_RULE_ABAP_DUMMY'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'DUMMY ABAP SQL E'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-end_of_pending.
    ls_result-rule_class = 'ZCL_APP_RULE_ABAP_DUMMY'.
    INSERT ls_result INTO TABLE rt_result.

  ENDMETHOD.

  METHOD get_amdp_rules.
    INSERT LINES OF get_amdp_default_rules( ) INTO TABLE rt_result.
    INSERT LINES OF get_amdp_context_less_rules( ) INTO TABLE rt_result.
    INSERT LINES OF get_amdp_select_rules( ) INTO TABLE rt_result.
    INSERT LINES OF get_amdp_control_rules( ) INTO TABLE rt_result.
    INSERT LINES OF get_amdp_special_rules( ) INTO TABLE rt_result.
  ENDMETHOD.

  METHOD get_amdp_default_rules.
    DATA ls_result TYPE zapp_s_rule.
    CLEAR ls_result.
    ls_result-rule_name = 'AMDP DEFAULT'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_DEFAULT'.
    INSERT ls_result INTO TABLE rt_result.
  ENDMETHOD.

  METHOD get_amdp_context_less_rules.
    DATA ls_result TYPE zapp_s_rule.
    CLEAR ls_result.
    ls_result-rule_name = 'AMDP OPEN BRACKET'.
    ls_result-token = '('.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_OPN_BRACKET'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP CLOSE BRACKET'.
    ls_result-token = ')'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CLSE_BRACKET'.
    INSERT ls_result INTO TABLE rt_result.


  ENDMETHOD.

  METHOD get_amdp_select_rules.
    DATA ls_result TYPE zapp_s_rule.
    CLEAR ls_result.
    ls_result-rule_name = 'AMDP SELECT'.
    ls_result-token = 'SELECT'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_SEL_UPS_INS'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP FROM'.
    ls_result-token = 'FROM'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NEW_LINE_LFT'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP INTO'.
    ls_result-token = 'INTO'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NEW_LINE_LFT'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP DEFAULT'.
    ls_result-token = 'DEFAULT'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_DEFAULT_TOKN'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP ON'.
    ls_result-token = 'ON'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NEW_LINE_LFT'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP WHERE'.
    ls_result-token = 'WHERE'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NEW_LINE_LFT'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP ORDER'.
    ls_result-token = 'ORDER'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NL_LFT_COND'.
    ls_result-rule_cond_class = 'ZCL_APP_RULE_COND_FROM_TOKN_RW'.
    ls_result-add_indent = -9.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP GROUP'.
    ls_result-token = 'GROUP'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NL_LFT_COND'.
    ls_result-rule_cond_class = 'ZCL_APP_RULE_COND_FROM_TOKN_RW'.
    ls_result-add_indent = -9.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP HAVING'.
    ls_result-token = 'HAVING'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NL_LFT_COND'.
    ls_result-rule_cond_class = 'ZCL_APP_RULE_COND_FROM_TOKN_RW'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP TOP'.
    ls_result-token = 'TOP'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NL_LFT_COND'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP LIMIT'.
    ls_result-token = 'LIMIT'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NL_LFT_COND'.
    ls_result-rule_cond_class = 'ZCL_APP_RULE_COND_FROM_TOKN_RW'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP OFFSET'.
    ls_result-token = 'OFFSET'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NL_LFT_COND'.
    ls_result-rule_cond_class = 'ZCL_APP_RULE_COND_FROM_TOKN_RW'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP DISTINCT'.
    ls_result-token = 'DISTINCT'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NEW_LINE_LFT'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP LEFT JOIN or LEFT Function'.
    ls_result-token = 'LEFT'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NL_LFT_COND'.
    ls_result-rule_cond_class = 'ZCL_APP_RULE_COND_N_RL_IS_NO_B'.
    ls_result-add_indent = -16.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP RIGHT JOIN or LEFT Function'.
    ls_result-token = 'RIGHT'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NL_LFT_COND'.
    ls_result-rule_cond_class = 'ZCL_APP_RULE_COND_N_RL_IS_NO_B'.
    ls_result-add_indent = -17.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP CROSS JOIN'.
    ls_result-token = 'CROSS'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NEW_LINE_LFT'.
    ls_result-add_indent = -11.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP INNER JOIN'.
    ls_result-token = 'INNER'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NEW_LINE_LFT'.
    ls_result-add_indent = -11.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP AND'.
    ls_result-token = 'AND'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NL_LFT_COND'.
    ls_result-rule_cond_class = 'ZCL_APP_RULE_COND_FROM_TOKN_RW'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP OR'.
    ls_result-token = 'OR'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NL_LFT_COND'.
    ls_result-rule_cond_class = 'ZCL_APP_RULE_COND_FROM_TOKN_RW'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP UNION'.
    ls_result-token = 'UNION'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_UNION_ALL'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP ALL'.
    ls_result-token = 'ALL'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_UNION_ALL'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP DELETE'.
    ls_result-token = 'DELETE'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NEW_LINE'.
    ls_result-new_line_indent_diff = '7'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP INSERT'.
    ls_result-token = 'INSERT'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_SEL_UPS_INS'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP UPSERT'.
    ls_result-token = 'UPSERT'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_SEL_UPS_INS'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP UPDATE'.
    ls_result-token = 'UPDATE'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NEW_LINE'.
    ls_result-new_line_indent_diff = '7'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP SET'.
    ls_result-token = 'SET'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_NEW_LINE_LFT'.
    INSERT ls_result INTO TABLE rt_result.
  ENDMETHOD.

  METHOD get_amdp_control_rules.

    DATA ls_result TYPE zapp_s_rule.
    CLEAR ls_result.
    ls_result-rule_name = 'AMDP CASE'.
    ls_result-token = 'CASE'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CONTROL_STRU'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP IF'.
    ls_result-token = 'IF'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CONTROL_STRU'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP FOR'.
    ls_result-token = 'FOR'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CONTROL_STRU'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP WHILE'.
    ls_result-token = 'WHILE'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CONTROL_STRU'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP WHEN'.
    ls_result-token = 'WHEN'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CONTROL_STRU'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP ELSE'.
    ls_result-token = 'ELSE'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CONTROL_STRU'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP ELSEIF'.
    ls_result-token = 'ELSEIF'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CONTROL_STRU'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP THEN'.
    ls_result-token = 'THEN'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CONTROL_STRU'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP END CASE'.
    ls_result-token = 'END'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CONTROL_STRU'.
    INSERT ls_result INTO TABLE rt_result.

  ENDMETHOD.

  METHOD get_amdp_special_rules.
    DATA ls_result TYPE zapp_s_rule.
    CLEAR ls_result.
    ls_result-rule_name = 'AMDP BY'.
    ls_result-token = 'BY'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_BY'.
    INSERT ls_result INTO TABLE rt_result.

    CLEAR ls_result.
    ls_result-rule_name = 'AMDP CALL'.
    ls_result-token = 'CALL'.
    ls_result-sqlscript = zcl_app_scanner_sqlscript=>cos_sqlscript-sqlscript.
    ls_result-rule_class = 'ZCL_APP_RULE_AMDP_CALL'.
    INSERT ls_result INTO TABLE rt_result.
  ENDMETHOD.

ENDCLASS.
