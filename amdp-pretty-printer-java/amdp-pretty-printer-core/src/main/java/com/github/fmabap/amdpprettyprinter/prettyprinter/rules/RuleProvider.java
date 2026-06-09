package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * Provides the complete list of pretty-printer rules.
 * Converted from ABAP class ZCL_APP_RULE_PROVIDER (implements
 * ZIF_APP_RULE_PROVIDER).
 */
public final class RuleProvider implements IRuleProvider {

    private static final String PKG_ABAP = "com.github.fmabap.amdpprettyprinter.prettyprinter.rules.abap.";
    private static final String PKG_AMDP = "com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp.";

    // -----------------------------------------------------------------------
    // IRuleProvider
    // -----------------------------------------------------------------------

    /**
     * Returns all pretty-printer rules (ABAP + AMDP).
     * Converted from ABAP: ZIF_APP_RULE_PROVIDER~GET_RULES.
     */
    @Override
    public List<RuleData> getRules() throws AppException {
        List<RuleData> result = new ArrayList<>();
        result.addAll(getAbapRules());
        result.addAll(getAmdpRules());
        return result;
    }

    // -----------------------------------------------------------------------
    // ABAP rules (converted from GET_ABAP_RULES)
    // -----------------------------------------------------------------------

    /**
     * Returns dummy rules that cover ABAP tokens outside any SQLScript body.
     * <p>
     * In ABAP three entries exist (sqlscript = none / pending / end_of_pending).
     * In the Java model {@code sqlscript} is a boolean, so all three map to
     * {@code false}. The entries are still registered individually so the rule
     * list mirrors the ABAP original; when loaded into {@link RuleFinder}'s
     * HashMap the last entry wins (all three share the same rule class, so the
     * outcome is identical).
     * </p>
     */
    private List<RuleData> getAbapRules() {
        List<RuleData> result = new ArrayList<>();
        RuleData r;

        // cos_sqlscript-none → sqlscript = false
        r = new RuleData();
        r.ruleName = "DUMMY ABAP SQL N";
        r.sqlscript = false;
        r.ruleClass = PKG_ABAP + "AbapDummyRule";
        result.add(r);

        // cos_sqlscript-pending → sqlscript = false (not yet inside SQLScript body)
        r = new RuleData();
        r.ruleName = "DUMMY ABAP SQL P";
        r.sqlscript = false;
        r.ruleClass = PKG_ABAP + "AbapDummyRule";
        result.add(r);

        // cos_sqlscript-end_of_pending → sqlscript = false (transition, still ABAP)
        r = new RuleData();
        r.ruleName = "DUMMY ABAP SQL E";
        r.sqlscript = false;
        r.ruleClass = PKG_ABAP + "AbapDummyRule";
        result.add(r);

        return result;
    }

    // -----------------------------------------------------------------------
    // AMDP rules (converted from GET_AMDP_RULES)
    // -----------------------------------------------------------------------

    private List<RuleData> getAmdpRules() {
        List<RuleData> result = new ArrayList<>();
        result.addAll(getAmdpDefaultRules());
        result.addAll(getAmdpContextLessRules());
        result.addAll(getAmdpSelectRules());
        result.addAll(getAmdpControlRules());
        result.addAll(getAmdpSpecialRules());
        return result;
    }

    // -----------------------------------------------------------------------
    // Converted from GET_AMDP_DEFAULT_RULES
    // -----------------------------------------------------------------------

    private List<RuleData> getAmdpDefaultRules() {
        List<RuleData> result = new ArrayList<>();

        RuleData r = new RuleData();
        r.ruleName = "AMDP DEFAULT";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpFuncInOneRowRule";
        result.add(r);

        return result;
    }

    // -----------------------------------------------------------------------
    // Converted from GET_AMDP_CONTEXT_LESS_RULES
    // -----------------------------------------------------------------------

    private List<RuleData> getAmdpContextLessRules() {
        List<RuleData> result = new ArrayList<>();
        RuleData r;

        r = new RuleData();
        r.ruleName = "AMDP OPEN BRACKET";
        r.token = "(";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpOpenBracketRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP CLOSE BRACKET";
        r.token = ")";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpCloseBracketRule";
        result.add(r);

        return result;
    }

    // -----------------------------------------------------------------------
    // Converted from GET_AMDP_SELECT_RULES
    // -----------------------------------------------------------------------

    private List<RuleData> getAmdpSelectRules() {
        List<RuleData> result = new ArrayList<>();
        RuleData r;

        r = new RuleData();
        r.ruleName = "AMDP SELECT";
        r.token = "SELECT";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpSelectUpsertInsertRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP FROM";
        r.token = "FROM";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP INTO";
        r.token = "INTO";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP DEFAULT";
        r.token = "DEFAULT";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpDefaultTokenRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP ON";
        r.token = "ON";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP WHERE";
        r.token = "WHERE";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP ORDER";
        r.token = "ORDER";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftCondRule";
        r.ruleCondClass = PKG_AMDP + "RuleCondFromTokenReverseWalk";
        r.addIndent = -9;
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP GROUP";
        r.token = "GROUP";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftCondRule";
        r.ruleCondClass = PKG_AMDP + "RuleCondFromTokenReverseWalk";
        r.addIndent = -9;
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP HAVING";
        r.token = "HAVING";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftCondRule";
        r.ruleCondClass = PKG_AMDP + "RuleCondFromTokenReverseWalk";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP TOP";
        r.token = "TOP";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftCondRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP LIMIT";
        r.token = "LIMIT";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftCondRule";
        r.ruleCondClass = PKG_AMDP + "RuleCondFromTokenReverseWalk";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP OFFSET";
        r.token = "OFFSET";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftCondRule";
        r.ruleCondClass = PKG_AMDP + "RuleCondFromTokenReverseWalk";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP DISTINCT";
        r.token = "DISTINCT";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP LEFT JOIN or LEFT Function";
        r.token = "LEFT";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpLeftRightRule";
        r.ruleCondClass = PKG_AMDP + "RuleCondNextRuleIsNoBracket";
        r.addIndent = -16;
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP RIGHT JOIN or RIGHT Function";
        r.token = "RIGHT";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpLeftRightRule";
        r.ruleCondClass = PKG_AMDP + "RuleCondNextRuleIsNoBracket";
        r.addIndent = -17;
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP CROSS JOIN";
        r.token = "CROSS";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftRule";
        r.addIndent = -11;
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP INNER JOIN";
        r.token = "INNER";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftRule";
        r.addIndent = -11;
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP AND";
        r.token = "AND";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftCondRule";
        r.ruleCondClass = PKG_AMDP + "RuleCondFromTokenReverseWalk";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP OR";
        r.token = "OR";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftCondRule";
        r.ruleCondClass = PKG_AMDP + "RuleCondFromTokenReverseWalk";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP UNION";
        r.token = "UNION";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpUnionAllRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP ALL";
        r.token = "ALL";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpUnionAllRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP DELETE";
        r.token = "DELETE";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineRule";
        r.newLineIndentDiff = 7;
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP INSERT";
        r.token = "INSERT";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpSelectUpsertInsertRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP UPSERT";
        r.token = "UPSERT";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpSelectUpsertInsertRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP UPDATE";
        r.token = "UPDATE";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineRule";
        r.newLineIndentDiff = 7;
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP SET";
        r.token = "SET";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpNewLineLeftRule";
        result.add(r);

        return result;
    }

    // -----------------------------------------------------------------------
    // Converted from GET_AMDP_CONTROL_RULES
    // -----------------------------------------------------------------------

    private List<RuleData> getAmdpControlRules() {
        List<RuleData> result = new ArrayList<>();
        RuleData r;

        r = new RuleData();
        r.ruleName = "AMDP CASE";
        r.token = "CASE";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpControlStructureRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP IF";
        r.token = "IF";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpControlStructureRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP FOR";
        r.token = "FOR";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpControlStructureRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP WHILE";
        r.token = "WHILE";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpControlStructureRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP WHEN";
        r.token = "WHEN";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpControlStructureRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP ELSE";
        r.token = "ELSE";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpControlStructureRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP ELSEIF";
        r.token = "ELSEIF";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpControlStructureRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP THEN";
        r.token = "THEN";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpControlStructureRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP END CASE";
        r.token = "END";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpControlStructureRule";
        result.add(r);

        return result;
    }

    // -----------------------------------------------------------------------
    // Converted from GET_AMDP_SPECIAL_RULES
    // -----------------------------------------------------------------------

    private List<RuleData> getAmdpSpecialRules() {
        List<RuleData> result = new ArrayList<>();
        RuleData r;

        r = new RuleData();
        r.ruleName = "AMDP BY";
        r.token = "BY";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpByRule";
        result.add(r);

        r = new RuleData();
        r.ruleName = "AMDP CALL";
        r.token = "CALL";
        r.sqlscript = true;
        r.ruleClass = PKG_AMDP + "AmdpCallRule";
        result.add(r);

        return result;
    }
}
