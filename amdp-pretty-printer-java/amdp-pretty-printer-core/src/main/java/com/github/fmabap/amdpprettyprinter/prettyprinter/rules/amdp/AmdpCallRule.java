package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.AppUtilities;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Handles CALL tokens in SQLScript CALL statements by aligning the named
 * parameters (arrows {@code =>}) so they line up vertically.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_CALL.
 * </p>
 */
public class AmdpCallRule extends AmdpDefaultNoCommentRule {

    // -----------------------------------------------------------------------
    // IRule - finalizeInit
    // -----------------------------------------------------------------------

    /**
     * Aligns the {@code =>} arrows of all parameters in this CALL to the
     * position that follows the longest parameter name.
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        setAddIndentAddArrows(getMaxParameterLength());
    }

    // -----------------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------------

    /**
     * Returns the first parameter rule after the opening bracket of the CALL.
     * Converted from ABAP: GET_FIRST_PARAMETER.
     */
    private IRule getFirstParameter() throws AppException {
        IRule rule = this;
        boolean catchNext = false;

        while (true) {
            rule = rule.getNextRule();
            if (rule == null) {
                return null;
            }
            if (!AppUtilities.isSqlscriptRule(rule)) {
                return null;
            }
            if (rule.isEndOfStatement()) {
                return null;
            }
            if (rule.isComment()) {
                continue;
            }
            if (rule.isLbTokenRespDelimiter()) {
                return null;
            }
            if ("(".equals(rule.getTokenUp())) {
                catchNext = true;
                continue;
            }
            if (catchNext) {
                return rule;
            }
        }
    }

    /**
     * Returns the next parameter rule after {@code startingRule}.
     * Converted from ABAP: GET_NEXT_PARAMETER.
     */
    private IRule getNextParameter(IRule startingRule) throws AppException {
        IRule rule = startingRule;
        boolean catchNext = false;
        int counterOpenBracket = 0;

        while (true) {
            rule = rule.getNextRule();
            if (rule == null) {
                return null;
            }
            if (!AppUtilities.isSqlscriptRule(rule)) {
                return null;
            }
            if (rule.isEndOfStatement()) {
                return null;
            }
            if (rule.isComment()) {
                continue;
            }

            String tokenUp = rule.getTokenUp();
            switch (tokenUp) {
                case "(":
                    counterOpenBracket++;
                    break;
                case ")":
                    counterOpenBracket--;
                    break;
                default:
                    break;
            }

            if (rule.isLbTokenRespDelimiter() && counterOpenBracket == 0) {
                catchNext = true;
                continue;
            }
            if (catchNext) {
                return rule;
            }
        }
    }

    /**
     * Returns the maximum upper-case token length across all parameters.
     * Converted from ABAP: GET_MAX_PARAMETER_LENGTH.
     */
    private int getMaxParameterLength() throws AppException {
        IRule parameter = getFirstParameter();
        if (parameter == null) {
            return 0;
        }

        int maxLength = AppUtilities.getTokenLengthWoDelimiter(parameter);

        while (true) {
            parameter = getNextParameter(parameter);
            if (parameter == null) {
                return maxLength;
            }
            int length = AppUtilities.getTokenLengthWoDelimiter(parameter);
            if (maxLength < length) {
                maxLength = length;
            }
        }
    }

    /**
     * Sets the additional indent on the arrow rule for each parameter so that
     * all arrows align at the column after the longest parameter name.
     * Converted from ABAP: SET_ADD_INDENT_ADD_ARROWS.
     */
    private void setAddIndentAddArrows(int maxParameterLength) throws AppException {
        IRule parameter = getFirstParameter();
        if (parameter == null) {
            return;
        }

        setAddIndentAddArrow(parameter, maxParameterLength);

        while (true) {
            parameter = getNextParameter(parameter);
            if (parameter == null) {
                return;
            }
            setAddIndentAddArrow(parameter, maxParameterLength);
        }
    }

    /**
     * Sets the additional indent on the arrow rule for a single parameter.
     * Converted from ABAP: SET_ADD_INDENT_ADD_ARROW.
     */
    private void setAddIndentAddArrow(IRule parameter, int maxParameterLength)
            throws AppException {
        IRule arrow = getArrowOfParameter(parameter);
        if (arrow == null) {
            return;
        }
        int addIndent = maxParameterLength - AppUtilities.getTokenLengthWoDelimiter(parameter);
        arrow.setAdditionalIndent(addIndent);
    }

    /**
     * Returns the {@code =>} rule immediately following a parameter, skipping
     * comments. Returns {@code null} if the first non-comment next token is not
     * {@code =>}.
     * Converted from ABAP: GET_ARROW_OF_PARAMETER.
     */
    private IRule getArrowOfParameter(IRule parameter) throws AppException {
        IRule rule = parameter;

        while (true) {
            rule = rule.getNextRule();
            if (rule == null) {
                return null;
            }
            if (!AppUtilities.isSqlscriptRule(rule)) {
                return null;
            }
            if (rule.isEndOfStatement()) {
                return null;
            }
            if (rule.isComment()) {
                continue;
            }
            // Only the very next non-comment token is inspected.
            if ("=>".equals(rule.getTokenUp())) {
                return rule;
            }
            return null;
        }
    }
}
