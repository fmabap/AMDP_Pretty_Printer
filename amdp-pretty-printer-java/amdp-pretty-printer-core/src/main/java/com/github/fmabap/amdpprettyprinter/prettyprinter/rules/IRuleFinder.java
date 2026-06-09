package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * Rule-finder interface.
 * Converted from ABAP interface ZIF_APP_RULE_FINDER.
 */
public interface IRuleFinder {

    /**
     * Looks up the rule descriptor that matches the given search key.
     *
     * @param ruleSearch token/context search criteria
     * @return matching rule descriptor (never {@code null})
     * @throws AppException when no matching rule exists
     */
    RuleData getRuleData(RuleSearch ruleSearch) throws AppException;
}
