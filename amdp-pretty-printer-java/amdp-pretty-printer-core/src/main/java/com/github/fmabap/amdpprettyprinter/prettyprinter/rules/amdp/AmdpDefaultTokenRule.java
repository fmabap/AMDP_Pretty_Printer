package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Variant of {@link AmdpNewLineLeftRule} that only activates when the current
 * token is inside a SELECT statement at the same bracket level (reverse walk).
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_DEFAULT_TOKN.
 * </p>
 */
public final class AmdpDefaultTokenRule extends AmdpNewLineLeftRule {

    /**
     * Returns {@code false} when no SELECT keyword exists in the same statement
     * at the same bracket level (walking backward, stopping at UNION).
     * Converted from ABAP: IS_LOGIC_ACTIVE (redefinition).
     */
    @Override
    protected boolean isLogicActive() throws AppException {
        boolean result = super.isLogicActive();
        if (!result) {
            return false;
        }

        List<String> tokenList = new ArrayList<>();
        tokenList.add("SELECT");

        List<String> stopTokenList = new ArrayList<>();
        stopTokenList.add("UNION");

        IRule selectRule = AmdpRuleUtilities.getRuleInStmOnSameLvlRw(
                this, tokenList, stopTokenList);
        return selectRule != null;
    }
}
