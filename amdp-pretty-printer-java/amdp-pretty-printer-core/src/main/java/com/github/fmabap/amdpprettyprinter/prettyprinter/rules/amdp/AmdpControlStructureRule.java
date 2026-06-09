package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Handles AMDP / SQLScript control-structure keywords:
 * CASE, END, THEN, WHEN, ELSE, ELSEIF, and others (e.g. FOR, WHILE, IF).
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_CONTROL_STRU.
 * </p>
 */
public class AmdpControlStructureRule extends AmdpNewLineRule {

    /**
     * Calls the parent finalisation and, when the token is not a comment,
     * dispatches to the keyword-specific helper.
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        super.finalizeInit();

        if (!isComment()) {
            switch (getTokenUp()) {
                case "CASE":
                    finalizeInitCase();
                    break;
                case "END":
                    finalizeInitEnd();
                    break;
                case "THEN":
                    finalizeInitThen();
                    break;
                case "WHEN":
                    finalizeInitWhen();
                    break;
                case "ELSE":
                    finalizeInitElse();
                    break;
                case "ELSEIF":
                    finalizeInitElseif();
                    break;
                default:
                    finalizeInitOthers();
                    break;
            }
        }
    }

    // -----------------------------------------------------------------------
    // Private keyword handlers
    // -----------------------------------------------------------------------

    /** Converted from ABAP: FINALIZE_INIT_END. */
    private void finalizeInitEnd() throws AppException {
        IRule nextRule = getNextRule();
        ruleData.addIndent = -4;
        setAdditionalIndent(ruleData.addIndent);
        if (nextRule != null) {
            String nextToken = nextRule.getTokenUp();
            if ("IF".equals(nextToken) || "FOR".equals(nextToken) || "WHILE".equals(nextToken)) {
                return;
            }
        }
        ruleData.newLineIndentDiff = -4;
        ruleData.newStatementIndentDiff = -4;
    }

    /** Converted from ABAP: FINALIZE_INIT_OTHERS. */
    private void finalizeInitOthers() throws AppException {
        IRule prevRuleLocal = getPrevRule();
        if (prevRuleLocal != null
                && "END".equals(prevRuleLocal.getTokenUp())
                && !prevRuleLocal.isEndOfStatement()) {
            ruleData.newLineIndentDiff = -4;
            ruleData.newStatementIndentDiff = -4;
            mvLogicActive = false;
            return;
        }
        ruleData.newLineIndentDiff = 4;
        ruleData.newStatementIndentDiff = 4;
    }

    /** Converted from ABAP: FINALIZE_INIT_THEN. */
    private void finalizeInitThen() {
        ruleData.isNewLineReq = true;
        mvLogicActive = false;
    }

    /** Converted from ABAP: FINALIZE_INIT_WHEN. */
    private void finalizeInitWhen() throws AppException {
        ruleData.addIndent = -2;
        setAdditionalIndent(ruleData.addIndent);
    }

    /** Converted from ABAP: FINALIZE_INIT_ELSE. */
    private void finalizeInitElse() throws AppException {
        ruleData.addIndent = -2;
        setAdditionalIndent(ruleData.addIndent);
        ruleData.isNewLineReq = true;
    }

    /** Converted from ABAP: FINALIZE_INIT_ELSEIF. */
    private void finalizeInitElseif() throws AppException {
        ruleData.addIndent = -2;
        setAdditionalIndent(ruleData.addIndent);
    }

    /** Converted from ABAP: FINALIZE_INIT_CASE. */
    private void finalizeInitCase() {
        ruleData.newLineIndentDiff = 4;
        ruleData.newStatementIndentDiff = 4;
    }
}
