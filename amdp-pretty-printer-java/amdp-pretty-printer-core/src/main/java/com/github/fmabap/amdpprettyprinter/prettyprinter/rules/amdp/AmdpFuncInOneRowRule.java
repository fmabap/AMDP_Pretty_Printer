package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * AMDP rule for function-call tokens that should appear in a single row when
 * the closing bracket of the function is on the same source line and the
 * function does not contain sub-functions with commas.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_FUNC_IN_1_RO.
 * </p>
 */
public class AmdpFuncInOneRowRule extends AmdpDefaultNoCommentRule {

    /**
     * Calls the parent finalisation and, when the logic is active and the
     * "avoid line-break after comma inside function" setting is applicable,
     * suppresses the line break on all top-level commas of this function.
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        super.finalizeInit();

        if (!isLogicActive()) {
            return;
        }

        if (AmdpRuleUtilities.isAvdLbAftCommaInFuAct(this, settings)) {
            AmdpRuleUtilities.avoidLbAfterCommaInFunc(this);
        }
    }
}
