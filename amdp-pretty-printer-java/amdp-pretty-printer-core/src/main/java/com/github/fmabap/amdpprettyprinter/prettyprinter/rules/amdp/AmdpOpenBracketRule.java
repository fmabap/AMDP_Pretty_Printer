package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.AppUtilities;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Handles opening bracket '(' tokens in AMDP / SQLScript expressions.
 * Provides logic for computing new-line indent and current row, and sets
 * special logic flags for CALL statements.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_OPN_BRACKET.
 * </p>
 */
public class AmdpOpenBracketRule extends AmdpDefaultNoCommentRule {

    /** Special logic tag for this opening bracket. */
    private OpenBracketSpecialLogic mvSpecialLogic;

    // -----------------------------------------------------------------------
    // Public accessor
    // -----------------------------------------------------------------------

    /** Returns the special logic value set during initialisation. */
    public OpenBracketSpecialLogic getMvSpecialLogic() {
        return mvSpecialLogic;
    }

    // -----------------------------------------------------------------------
    // IRule - getNewLineIndent
    // -----------------------------------------------------------------------

    /**
     * When logic is active the new-line indent is placed after the keyword that
     * precedes the CALL's opening bracket (plus 4 spaces) for CALL statements,
     * or at the end of this bracket otherwise. Falls back to the parent for
     * inactive logic.
     * Converted from ABAP: ZIF_APP_RULE~GET_NEW_LINE_INDENT.
     */
    @Override
    public int getNewLineIndent() throws AppException {
        if (isLogicActive()) {
            if (mvSpecialLogic == OpenBracketSpecialLogic.CALL_STATEMENT) {
                return prevRule.getPrevRule().getCurOffsetEnd() + 4;
            } else {
                return getCurOffsetEnd();
            }
        }
        return super.getNewLineIndent();
    }

    /**
     * Mirrors the fallback condition of {@link #getNewLineIndent()} so the
     * iterative same-type-prefix walk in {@code BaseRule.getNewLineIndent()}
     * knows it may unroll through this rule when its special logic is not
     * active.
     */
    @Override
    protected boolean usesDefaultNewLineIndent() throws AppException {
        return !isLogicActive();
    }

    // -----------------------------------------------------------------------
    // IRule - getCurRow
    // -----------------------------------------------------------------------

    /**
     * When logic is active and the previous token is also an opening bracket,
     * the current row is incremented by one.
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_ROW.
     */
    @Override
    public int getCurRow() throws AppException {
        // Mirror the ABAP pattern: if the cache was already set, return it
        // without re-applying the increment (otherwise +1 accumulates each call).
        if (curRowSet) {
            return curRow;
        }

        int result = super.getCurRow();

        if (!isLogicActive() || prevRule == null) {
            return result;
        }

        if (prevRule.getTokenUp().equals(getTokenUp())) {
            result = result + 1;
            setCurRow(result);
        }

        return result;
    }

    // -----------------------------------------------------------------------
    // IRule - finalizeInit
    // -----------------------------------------------------------------------

    /**
     * After the parent finalisation: normalises an all-whitespace delimiter to a
     * single space, and detects CALL statements to set the special logic flag.
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        super.finalizeInit();

        if (!isLogicActive()) {
            return;
        }

        if (tokenExt.delimiter == null || tokenExt.delimiter.isEmpty()) {
            return;
        }

        String firstDelim = tokenExt.delimiter.get(0);
        if (firstDelim.isEmpty()) {
            return;
        }

        if (firstDelim.chars().allMatch(c -> c == ' ')) {
            tokenExt.delimiter = AppUtilities.getSpaceAsDelimiter();
        }

        // Detect CALL X( ) pattern.
        if (prevRule != null
                && prevRule.getPrevRule() != null
                && "CALL".equals(prevRule.getPrevRule().getTokenUp())) {
            mvSpecialLogic = OpenBracketSpecialLogic.CALL_STATEMENT;
        }
    }

    // -----------------------------------------------------------------------
    // IRule - isNewLineReq
    // -----------------------------------------------------------------------

    /**
     * For CALL statement brackets, a new line is required when any token inside
     * the argument list contains a line-breaking token or comma.
     * Converted from ABAP: ZIF_APP_RULE~IS_NEW_LINE_REQ.
     */
    @Override
    public boolean isNewLineReq() throws AppException {
        boolean result = super.isNewLineReq();
        if (result) {
            return true;
        }

        if (mvSpecialLogic == OpenBracketSpecialLogic.CALL_STATEMENT) {
            return isNewLineReqForCallStatm();
        }

        return false;
    }

    // -----------------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------------

    /**
     * Scans the content of the CALL argument list; returns {@code true} when it
     * contains a line-breaking token.
     * Converted from ABAP: IS_NEW_LINE_REQ_FOR_CALL_STATM.
     */
    private boolean isNewLineReqForCallStatm() throws AppException {
        IRule rule = this;
        int counterNextOpenBracket = 0;

        while (true) {
            rule = rule.getNextRule();
            if (rule == null) {
                return false;
            }
            if (!AppUtilities.isSqlscriptRule(rule)) {
                return false;
            }
            if (rule.isEndOfStatement()) {
                return false;
            }
            if (rule.isComment()) {
                continue;
            }
            if (rule.isLineBreakingToken()) {
                return true;
            }

            String tokenUp = rule.getTokenUp();
            if (")".equals(tokenUp)) {
                if (counterNextOpenBracket == 0) {
                    return false;
                }
                counterNextOpenBracket--;
            } else if ("(".equals(tokenUp)) {
                counterNextOpenBracket++;
            }
        }
    }
}
