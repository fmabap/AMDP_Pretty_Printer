package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRuleCondition;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Condition that is fulfilled when the next rule is NOT an open bracket.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_COND_N_RL_IS_NO_B.
 * </p>
 */
public final class RuleCondNextRuleIsNoBracket implements IRuleCondition {

    @Override
    public boolean isCondFulfilled(IRule rule) throws AppException {
        IRule nextRule = rule.getNextRule();
        if (nextRule != null && "(".equals(nextRule.getTokenUp())) {
            return false;
        }
        return true;
    }
}
