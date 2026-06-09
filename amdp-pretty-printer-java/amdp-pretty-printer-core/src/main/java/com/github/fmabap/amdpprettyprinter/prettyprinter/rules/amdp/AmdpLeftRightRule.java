package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * Handles the LEFT and RIGHT string functions in SQLScript.
 * When the inherited condition is not fulfilled the rule falls back to the
 * "function in one row" logic (avoid line break after comma).
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_LEFT_RIGHT.
 * </p>
 */
public class AmdpLeftRightRule extends AmdpNewLineLeftCondRule {

    /**
     * After parent finalisation: when the logic is active but the condition was
     * not fulfilled, apply the "avoid line break after comma in function" logic
     * if the current settings allow it.
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        super.finalizeInit();

        if (!isLogicActive()) {
            return;
        }

        // Apply fallback when the condition has not yet been evaluated (lazy eval
        // happens later in setAddIndent() called from getCurOffsetStart()).
        // This matches the ABAP: IF mv_cond_fulfilled_set = abap_false.
        if (!mvCondFulfilledSet) {
            if (AmdpRuleUtilities.isAvdLbAftCommaInFuAct(this, settings)) {
                AmdpRuleUtilities.avoidLbAfterCommaInFunc(this);
            }
        }
    }
}
