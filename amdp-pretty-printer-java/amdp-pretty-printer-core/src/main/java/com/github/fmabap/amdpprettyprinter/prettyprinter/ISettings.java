package com.github.fmabap.amdpprettyprinter.prettyprinter;

/**
 * Settings interface for the AMDP Pretty Printer.
 * Converted from ABAP interface ZIF_APP_SETTINGS.
 */
public interface ISettings {

    // Line-break-at-comma rule constants (from ZIF_APP_SETTINGS
    // cos_lb_rules_at_comma)
    String LB_RULE_ALWAYS_LINE_BREAK = "0";
    String LB_RULE_NO_LINE_BREAK = "1";
    String LB_RULE_DEP_ON_CLS_BRACKET_ONLY = "2";
    String LB_RULE_DEP_ON_CLS_BRACKET_AND_SUB_FU = "3";
    String LB_RULE_DEP_ON_CLS_BR_SF_AND_KEYWRD = "4";

    /**
     * Returns true when a line break after comma is required in the current
     * settings. Maps to ABAP XFELD domain value ('X' = true).
     * Converted from ABAP: IS_LINE_BREAK_AFTER_COMMA_REQ.
     */
    boolean isLineBreakAfterCommaReq();

    /**
     * No line break after comma in function depending on sub-function.
     * Converted from ABAP: IS_NO_LB_AT_CO_S_FU_DEP_SFU.
     */
    boolean isNoLbAtCoSFuDepSfu();

    /**
     * No line break after comma in function depending on closing bracket only.
     * Converted from ABAP: IS_NO_LB_AT_CO_S_FU_DEP_CBR_O.
     */
    boolean isNoLbAtCoSFuDepCbrO();

    /**
     * Always insert a line break after comma.
     * Converted from ABAP: IS_ALWAYS_LINE_BREAK_AFT_COMMA.
     */
    boolean isAlwaysLineBreakAftComma();

    /**
     * No line break after comma in function depending on sub-function and keyword.
     * Converted from ABAP: IS_NO_LB_AT_CO_S_FU_DEP_SFU_KW.
     */
    boolean isNoLbAtCoSFuDepSfuKw();

    /**
     * When {@code true}, the token list is printed as a Markdown table to
     * {@link System#err} after tokenisation.
     */
    boolean isTrace();
}
