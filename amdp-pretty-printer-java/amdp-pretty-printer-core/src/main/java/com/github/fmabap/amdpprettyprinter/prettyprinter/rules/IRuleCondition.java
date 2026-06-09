package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * Interface for a rule condition check.
 * Converted from ABAP interface ZIF_APP_RULE_CONDITION.
 */
public interface IRuleCondition {

    /**
     * Checks whether the condition is fulfilled for the given rule.
     * Converted from: IS_COND_FULFILLED.
     *
     * @param rule the rule to evaluate
     * @return {@code true} if the condition is fulfilled, {@code false} otherwise
     * @throws AppException if an error occurs during evaluation
     */
    boolean isCondFulfilled(IRule rule) throws AppException;
}
