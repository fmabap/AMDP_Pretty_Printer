package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Handles the BY keyword in AMDP / SQLScript ORDER BY / GROUP BY expressions.
 * When BY is not at the SELECT level the previous token's position is adjusted
 * so that the column aligns with the corresponding opening bracket.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_BY.
 * </p>
 */
public final class AmdpByRule extends AmdpDefaultNoCommentRule {

    // -----------------------------------------------------------------------
    // IRule - finalizeInit
    // -----------------------------------------------------------------------

    /**
     * Skips the special logic for comment tokens, when no previous rule exists,
     * when the previous rule is a comment, when the previous rule has no
     * predecessor, or when the token is at the SELECT level. Otherwise it
     * locates the nearest unmatched opening bracket and adjusts the previous
     * rule's position to align with it.
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        if (isComment()
                || prevRule == null
                || prevRule.isComment()
                || prevRule.getPrevRule() == null
                || isOnSelectLevel()) {
            super.finalizeInit();
            return;
        }

        IRule openBracketRule = AmdpRuleUtilities.findPrevOpenBracketRule(this);
        if (openBracketRule == null) {
            super.finalizeInit();
            return;
        }

        adjustPrevRule(openBracketRule);
        super.finalizeInit();

        // Respect the new-line indent difference of the ORDER rule for ORDER BY.
        int curOffsetEnd = getCurOffsetEnd();
        ruleData.newLineIndentDiff = curOffsetEnd - prevRule.getNewLineIndent();
    }

    // -----------------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------------

    /**
     * Returns {@code true} when BY is inside a SELECT statement at the same
     * bracket level, not nested inside a function.
     * Converted from ABAP: IS_ON_SELECT_LEVEL.
     */
    private boolean isOnSelectLevel() throws AppException {
        List<String> tokenList = new ArrayList<>();
        tokenList.add("SELECT");

        List<String> stopTokenList = new ArrayList<>();
        stopTokenList.add("UNION");

        IRule fromRule = AmdpRuleUtilities.getRuleInStmOnSameLvlRw(
                this, tokenList, stopTokenList);
        return fromRule != null;
    }

    /**
     * Adjusts the previous rule's row and start-offset when the previous rule
     * is not directly after the opening bracket, so that BY aligns below it.
     * Converted from ABAP: ADJUST_PREV_RULE.
     */
    private void adjustPrevRule(IRule openBracketRule) throws AppException {
        if (prevRule.getPrevRule() != openBracketRule) {
            int prevRow;
            if (prevRule.getPrevRule().getCurRow() == prevRule.getPrevRule().getEndRow()) {
                prevRow = prevRule.getPrevRule().getEndRow() + 1;
            } else {
                prevRow = prevRule.getPrevRule().getEndRow();
            }

            prevRule.refreshBuffer();
            prevRule.setCurRow(prevRow);
            prevRule.setCurOffsetStart(openBracketRule.getCurOffsetEnd());
        }
    }
}
