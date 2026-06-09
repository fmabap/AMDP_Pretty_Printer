package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * AMDP rule that forces the token onto a new line when the previous token is on
 * the same source row and the rule's logic is active.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_NEW_LINE.
 * </p>
 */
public class AmdpNewLineRule extends AmdpDefaultNoCommentRule {

    // -----------------------------------------------------------------------
    // IRule - getCurRow
    // -----------------------------------------------------------------------

    /**
     * Returns the row of this token. When the logic is active and the previous
     * token sits on the same row, the row is incremented by one to force a line
     * break. The result is cached via {@link #setCurRow(int)}.
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_ROW.
     */
    @Override
    public int getCurRow() throws AppException {
        // Delegate to BaseRule which handles the curRowSet cache; on first call
        // it computes the base row and caches it. We then apply our increment
        // and override the cache.
        int result = super.getCurRow();

        // On subsequent calls super returns the already-incremented cached value,
        // and prevRule.getCurRow() will differ from it, so the condition below
        // evaluates to false — making the method safely idempotent.
        if (isLogicActive() && prevRule != null) {
            if (prevRule.getCurRow() == result) {
                result = result + 1;
            }
        }

        setCurRow(result);
        return result;
    }

    // -----------------------------------------------------------------------
    // IRule - getCurOffsetStart
    // -----------------------------------------------------------------------

    /**
     * Returns the start column offset. When the logic is active the token is
     * placed either at the default line indent (when the previous token has a
     * different type) or at the continuation indent of the previous token.
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_OFFSET_START.
     */
    @Override
    public int getCurOffsetStart() throws AppException {
        if (curOffsetStartSet) {
            return curOffsetStart;
        }

        if (!isLogicActive()) {
            return super.getCurOffsetStart();
        }

        int result;
        if (!hasPrevRuleSameType()) {
            result = defaultLineIndent;
        } else {
            result = prevRule.getNewLineIndent();
        }
        result = Math.max(0, result);
        result = Math.max(0, result + getAdditionalIndent());

        setCurOffsetStart(result);
        return result;
    }

    // -----------------------------------------------------------------------
    // IRule - finalizeInit
    // -----------------------------------------------------------------------

    /**
     * Calls the parent finalisation and then, when the logic is active, deactivates
     * it if the immediately preceding token is an open bracket — the content
     * inside a bracket pair is handled differently.
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        super.finalizeInit();

        if (isLogicActive()) {
            if (prevRule != null && "(".equals(prevRule.getTokenUp())) {
                mvLogicActive = false;
            }
        }
    }
}
