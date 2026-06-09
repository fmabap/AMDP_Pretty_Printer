package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Handles closing bracket ')' tokens in AMDP / SQLScript expressions.
 * When logic is active the position is derived from the matching opening
 * bracket.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_CLSE_BRACKET.
 * </p>
 */
public class AmdpCloseBracketRule extends AmdpDefaultNoCommentRule {

    // -----------------------------------------------------------------------
    // IRule - getCurRow
    // -----------------------------------------------------------------------

    /**
     * When logic is active and the matching open bracket's end row is before this
     * token's natural row, but the previous rule is on the same row as this token,
     * the row is incremented by one (unless the opening bracket belongs to a CALL
     * statement).
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_ROW.
     */
    @Override
    public int getCurRow() throws AppException {
        int result = super.getCurRow();

        if (!isLogicActive()) {
            return result;
        }

        AmdpOpenBracketRule openBracketRule = (AmdpOpenBracketRule) findPrevOpenBracketRule();

        if (openBracketRule.getEndRow() < result
                && prevRule != null && prevRule.getCurRow() == result
                && openBracketRule.getMvSpecialLogic() != OpenBracketSpecialLogic.CALL_STATEMENT) {
            result = result + 1;
        }

        setCurRow(result);
        return result;
    }

    // -----------------------------------------------------------------------
    // IRule - getCurOffsetStart
    // -----------------------------------------------------------------------

    /**
     * When logic is active and this closing bracket is on a different row from
     * its opening bracket (and the opening bracket is not for a CALL statement),
     * the offset is aligned with the opening bracket's column plus the additional
     * indent. Otherwise falls back to the parent.
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_OFFSET_START.
     */
    @Override
    public int getCurOffsetStart() throws AppException {
        if (!isLogicActive()) {
            return super.getCurOffsetStart();
        }

        AmdpOpenBracketRule openBracketRule = (AmdpOpenBracketRule) findPrevOpenBracketRule();

        if (openBracketRule.getMvSpecialLogic() != OpenBracketSpecialLogic.CALL_STATEMENT
                && openBracketRule.getEndRow() != getCurRow()) {
            int result = openBracketRule.getCurOffsetStart() + getAdditionalIndent();
            setCurOffsetStart(result);
            return result;
        }

        return super.getCurOffsetStart();
    }

    // -----------------------------------------------------------------------
    // IRule - getNewLineIndent
    // -----------------------------------------------------------------------

    /**
     * When logic is active and the token is not an end-of-statement, the new-line
     * indent is taken from the previous rule of the matching open bracket (or the
     * default line indent if the open bracket has no predecessor).
     * Converted from ABAP: ZIF_APP_RULE~GET_NEW_LINE_INDENT.
     */
    @Override
    public int getNewLineIndent() throws AppException {
        if (!isLogicActive() || isEndOfStatement()) {
            return super.getNewLineIndent();
        }

        IRule openBracket = findPrevOpenBracketRule();
        if (openBracket.getPrevRule() == null) {
            return defaultLineIndent;
        }
        return openBracket.getPrevRule().getNewLineIndent();
    }

    // -----------------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------------

    /**
     * Finds and returns the matching opening bracket rule.
     *
     * @throws AppException when no matching opening bracket exists
     *                      Converted from ABAP: FIND_PREV_OPEN_BRACKET_RULE.
     */
    private IRule findPrevOpenBracketRule() throws AppException {
        IRule result = AmdpRuleUtilities.findPrevOpenBracketRule(this);
        if (result == null) {
            throw new AppException(
                    "No matching open bracket found for rule: " + ruleData.ruleName
                            + " at row " + tokenExt.row + " col " + tokenExt.col);
        }
        return result;
    }
}
