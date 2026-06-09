package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRuleCondition;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Condition that is fulfilled when a FROM keyword exists in the same statement
 * at the same bracket level (walking backward, stopping at UNION).
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_COND_FROM_TOKN_RW.
 * </p>
 */
public final class RuleCondFromTokenReverseWalk implements IRuleCondition {

    @Override
    public boolean isCondFulfilled(IRule rule) throws AppException {
        List<String> tokenList = new ArrayList<>();
        tokenList.add("FROM");

        List<String> stopTokenList = new ArrayList<>();
        stopTokenList.add("UNION");

        IRule fromRule = AmdpRuleUtilities.getRuleInStmOnSameLvlRw(
                rule, tokenList, stopTokenList);
        return fromRule != null;
    }
}
