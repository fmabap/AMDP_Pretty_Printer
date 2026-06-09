package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRuleCondition;

/**
 * Extends {@link AmdpNewLineLeftRule} with a conditional activation mechanism.
 * The left-aligned indent is only applied when an associated
 * {@link IRuleCondition} is fulfilled (or when none is configured).
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_NL_LFT_COND.
 * </p>
 */
public class AmdpNewLineLeftCondRule extends AmdpNewLineLeftRule {

    // -----------------------------------------------------------------------
    // Protected fields
    // -----------------------------------------------------------------------

    /** {@code true} once the condition has been evaluated. */
    protected boolean mvCondFulfilledSet;

    /** Cached result of the condition evaluation. */
    protected boolean mvCondFulfilled;

    // -----------------------------------------------------------------------
    // Protected helpers - setAddIndent override
    // -----------------------------------------------------------------------

    /**
     * Only applies the left-aligned indent when the configured condition is
     * fulfilled. The condition is evaluated lazily on the first call and
     * cached for subsequent calls.
     *
     * <p>
     * When {@code ruleData.ruleCondClass} is empty the condition is always
     * considered fulfilled.
     * </p>
     * Converted from ABAP: SET_ADD_INDENT (redefinition).
     */
    @Override
    protected void setAddIndent() throws AppException {
        setAdditionalIndent(0);

        if (!isLogicActive()) {
            return;
        }

        if (!mvCondFulfilledSet) {
            mvCondFulfilledSet = true;
            if (ruleData.ruleCondClass == null || ruleData.ruleCondClass.isEmpty()) {
                mvCondFulfilled = true;
            } else {
                try {
                    Class<?> condClass = Class.forName(ruleData.ruleCondClass);
                    IRuleCondition cond = (IRuleCondition) condClass.getDeclaredConstructor().newInstance();
                    mvCondFulfilled = cond.isCondFulfilled(this);
                } catch (ReflectiveOperationException e) {
                    throw new AppException(
                            "Cannot instantiate rule condition class: " + ruleData.ruleCondClass
                                    + " — " + e.getMessage());
                }
            }
        }

        if (!mvCondFulfilled) {
            return;
        }

        super.setAddIndent();
    }
}
