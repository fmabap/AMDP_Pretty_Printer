package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

/**
 * Input to the rule lookup.
 * Converted from ABAP structure ZAPP_S_RULE_SEARCH.
 */
public class RuleSearch {

    /** Token text (uppercased) to look up (ZAPP_D_TOKEN). */
    public String token = "";

    /** Current context (ZAPP_D_CONTEXT). */
    public String context = "";

    /** Current higher-level context (ZAPP_D_HL_CONTEXT). */
    public String hlContext = "";

    /**
     * {@code true} when the token belongs to a SQLScript section
     * (ZAPP_D_SQLSCRIPT).
     */
    public boolean sqlscript = false;
}
