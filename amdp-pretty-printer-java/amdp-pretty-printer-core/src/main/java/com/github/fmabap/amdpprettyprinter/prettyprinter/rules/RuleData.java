package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

/**
 * AMDP Pretty Printer rule descriptor.
 * Converted from ABAP structure ZAPP_S_RULE.
 */
public class RuleData {

    /** Rule name / identifier (ZAPP_D_RULE). */
    public String ruleName = "";

    /** Token this rule matches - uppercased (ZAPP_D_TOKEN). */
    public String token = "";

    /** Context the rule applies to (ZAPP_D_CONTEXT). */
    public String context = "";

    /** Higher-level context the rule applies to (ZAPP_D_HL_CONTEXT). */
    public String hlContext = "";

    /** {@code true} when the rule is for SQLScript tokens (ZAPP_D_SQLSCRIPT). */
    public boolean sqlscript = false;

    /**
     * Fully-qualified Java class name of the rule implementation
     * (ZAPP_D_RULE_CLASS). Used for dynamic instantiation.
     */
    public String ruleClass = "";

    /** Additional indentation added by this rule (ZAPP_D_ADDITIONAL_INDENT). */
    public int addIndent = 0;

    /**
     * Fully-qualified Java class name of the rule condition implementation
     * (ZAPP_D_RULE_COND_CLASS).
     */
    public String ruleCondClass = "";

    /**
     * Indent difference for new lines inside the rule
     * (ZAPP_D_NEW_LINE_INDENT_DIFF).
     */
    public int newLineIndentDiff = 0;

    /** Indent difference for the next statement (ZAPP_D_NEW_STATEM_INDENT_DIFF). */
    public int newStatementIndentDiff = 0;

    /**
     * {@code true} when the rule forces a new line before the token
     * (ZAPP_D_NEW_LINE_REQ).
     */
    public boolean isNewLineReq = false;
}
