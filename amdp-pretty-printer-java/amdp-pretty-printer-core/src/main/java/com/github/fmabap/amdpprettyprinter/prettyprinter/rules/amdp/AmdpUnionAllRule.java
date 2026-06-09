package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Handles UNION and ALL tokens in UNION ALL expressions.
 * Manages alignment so that SELECT statements after UNION ALL line up with the
 * first SELECT in the statement.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_UNION_ALL.
 * </p>
 */
public final class AmdpUnionAllRule extends AmdpNewLineLeftRule {

    // -----------------------------------------------------------------------
    // IRule - finalizeInit
    // -----------------------------------------------------------------------

    /**
     * Calls parent finalisation; when logic is active for a UNION token, sets
     * the additional indent to {@code -3} (for UNION ALL) or {@code 1} (for plain
     * UNION). Deactivates logic for a standalone ALL token whose predecessor is
     * not UNION.
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        super.finalizeInit();

        if (!isLogicActive()) {
            return;
        }

        if ("UNION".equals(tokenExt.strUp)) {
            if (nextRule != null && "ALL".equals(nextRule.getTokenUp())) {
                ruleData.addIndent = -3;
            } else {
                ruleData.addIndent = 1;
            }
            setAdditionalIndent(ruleData.addIndent);
            return;
        }

        if ("ALL".equals(tokenExt.strUp)) {
            if (prevRule == null || !"UNION".equals(prevRule.getTokenUp())) {
                mvLogicActive = false;
                setLogicActive(); // re-evaluate using the standard logic
            }
        }
    }

    // -----------------------------------------------------------------------
    // IRule - getCurOffsetStart
    // -----------------------------------------------------------------------

    /**
     * For UNION tokens that belong to a UNION ALL: aligns with the preceding
     * SELECT statement's start column plus the additional indent.
     * For ALL tokens (part of UNION ALL): aligns immediately after the UNION token.
     * Falls back to the parent for all other cases.
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_OFFSET_START.
     */
    @Override
    public int getCurOffsetStart() throws AppException {
        List<String> stopTokenList = new ArrayList<>();
        stopTokenList.add("UNION");

        if ("UNION".equals(tokenExt.strUp) && isLogicActive()) {
            List<String> tokenList = new ArrayList<>();
            tokenList.add("SELECT");

            IRule selectRule = AmdpRuleUtilities.getRuleInStmOnSameLvlRw(
                    this, tokenList, stopTokenList);
            if (selectRule == null) {
                return super.getCurOffsetStart();
            }
            int result = selectRule.getCurOffsetStart() + getAdditionalIndent();
            setCurOffsetStart(result);
            return result;

        } else if ("ALL".equals(tokenExt.strUp) && isLogicActive()) {
            int result = prevRule.getCurOffsetEnd();
            setCurOffsetStart(result);
            return result;

        } else {
            return super.getCurOffsetStart();
        }
    }

    // -----------------------------------------------------------------------
    // IRule - getCurRow
    // -----------------------------------------------------------------------

    /**
     * For ALL tokens that belong to a UNION ALL the row equals the row of the
     * preceding UNION token. All other cases delegate to the parent.
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_ROW.
     */
    @Override
    public int getCurRow() throws AppException {
        if ("ALL".equals(tokenExt.strUp) && isUnionOrAllOfUnionAll()) {
            return prevRule.getCurRow();
        }
        return super.getCurRow();
    }

    // -----------------------------------------------------------------------
    // IRule - getNewLineIndent
    // -----------------------------------------------------------------------

    /**
     * For UNION / ALL tokens that belong to a UNION ALL: returns the start column
     * of the preceding SELECT minus its additional indent (so that the next
     * statement starts at the same column as SELECT).
     * Converted from ABAP: ZIF_APP_RULE~GET_NEW_LINE_INDENT.
     */
    @Override
    public int getNewLineIndent() throws AppException {
        if (!isUnionOrAllOfUnionAll()) {
            return super.getNewLineIndent();
        }

        List<String> tokenList = new ArrayList<>();
        tokenList.add("SELECT");

        List<String> stopTokenList = new ArrayList<>();
        stopTokenList.add("UNION");

        IRule startRule = "ALL".equals(tokenExt.strUp) ? prevRule : this;

        IRule selectRule = AmdpRuleUtilities.getRuleInStmOnSameLvlRw(
                startRule, tokenList, stopTokenList);
        if (selectRule == null) {
            return super.getNewLineIndent();
        }

        return selectRule.getCurOffsetStart() - selectRule.getAdditionalIndent();
    }

    // -----------------------------------------------------------------------
    // IRule - isNewLineReq
    // -----------------------------------------------------------------------

    /**
     * Forces a new line for UNION / ALL tokens that belong to a UNION ALL.
     * Converted from ABAP: ZIF_APP_RULE~IS_NEW_LINE_REQ.
     */
    @Override
    public boolean isNewLineReq() throws AppException {
        if (isUnionOrAllOfUnionAll()) {
            return true;
        }
        return super.isNewLineReq();
    }

    // -----------------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------------

    /**
     * Returns {@code true} when this token is the UNION part of a UNION ALL pair
     * (UNION followed by ALL), or the ALL part of that pair (ALL preceded by
     * UNION), and the logic is active.
     * Converted from ABAP: IS_UNION_OR_ALL_OF_UNION_ALL.
     */
    private boolean isUnionOrAllOfUnionAll() throws AppException {
        if (!isLogicActive()) {
            return false;
        }

        if ("UNION".equals(tokenExt.strUp)
                && nextRule != null
                && "ALL".equals(nextRule.getTokenUp())) {
            return false; // UNION without ALL would be a plain UNION
        }

        if ("ALL".equals(tokenExt.strUp)) {
            if (prevRule == null || !"UNION".equals(prevRule.getTokenUp())) {
                return false;
            }
        }

        return true;
    }
}
