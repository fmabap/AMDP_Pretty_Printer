package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * Rule-provider interface.
 * Converted from ABAP interface ZIF_APP_RULE_PROVIDER.
 */
public interface IRuleProvider {

    /**
     * Returns the full list of pretty-printer rules.
     *
     * @return list of rule descriptors (never {@code null})
     * @throws AppException if the rules cannot be loaded
     */
    List<RuleData> getRules() throws AppException;
}
