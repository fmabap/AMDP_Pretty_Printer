package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.AppUtilities;
import com.github.fmabap.amdpprettyprinter.prettyprinter.ISettings;
import com.github.fmabap.amdpprettyprinter.prettyprinter.TokensExt;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Static utility methods specific to AMDP pretty-printer rules.
 * Converted from ABAP class ZCL_APP_AMDP_RULE_UTILITIES.
 */
public final class AmdpRuleUtilities {

    private AmdpRuleUtilities() {
    }

    // -----------------------------------------------------------------------
    // Navigation helpers
    // -----------------------------------------------------------------------

    /**
     * Walks forward from startRule, stays on the same bracket level, and
     * returns the first rule whose upper-case token appears in the token list
     * (and not in the stop-token list).
     * Converted from ABAP: GET_RULE_IN_STM_ON_SAME_LEVEL.
     */
    public static IRule getRuleInStmOnSameLevel(IRule startRule,
            List<String> tokenList,
            List<String> stopTokenList)
            throws AppException {

        IRule rule = startRule;
        int openBracketCounter = 0;

        while (true) {
            rule = rule.getNextRule();
            if (rule == null) {
                return null;
            }
            if (!AppUtilities.isSqlscriptRule(rule)) {
                return null;
            }
            if (rule.isComment()) {
                continue;
            }

            String tokenUp = rule.getTokenUp();
            if ("(".equals(tokenUp)) {
                openBracketCounter++;
            } else if (")".equals(tokenUp)) {
                openBracketCounter--;
            }

            if (openBracketCounter == 0) {
                if (tokenList != null && tokenList.contains(tokenUp)) {
                    return rule;
                }
                if (stopTokenList != null && stopTokenList.contains(tokenUp)) {
                    return null;
                }
            }

            if (rule.isEndOfStatement()) {
                return null;
            }
        }
    }

    /**
     * Walks backward from startRule (reverse-walk variant).
     * Converted from ABAP: GET_RULE_IN_STM_ON_SAME_LVL_RW.
     */
    public static IRule getRuleInStmOnSameLvlRw(IRule startRule,
            List<String> tokenList,
            List<String> stopTokenList)
            throws AppException {

        IRule rule = startRule;
        int openBracketCounter = 0;

        while (true) {
            rule = rule.getPrevRule();
            if (rule == null) {
                return null;
            }
            if (rule.isEndOfStatement()) {
                return null;
            }
            if (!AppUtilities.isSqlscriptRule(rule)) {
                return null;
            }
            if (rule.isComment()) {
                continue;
            }

            String tokenUp = rule.getTokenUp();
            if ("(".equals(tokenUp)) {
                openBracketCounter++;
            } else if (")".equals(tokenUp)) {
                openBracketCounter--;
            }

            if (openBracketCounter == 0) {
                if (tokenList != null && tokenList.contains(tokenUp)) {
                    return rule;
                }
                if (stopTokenList != null && stopTokenList.contains(tokenUp)) {
                    return null;
                }
            }
        }
    }

    /**
     * Like getRuleInStmOnSameLevel but searches for a single token.
     * Converted from ABAP: GET_1_RULE_IN_STM_ON_SAME_LVL.
     */
    public static IRule get1RuleInStmOnSameLvl(IRule startRule,
            String token,
            List<String> stopTokenList)
            throws AppException {
        List<String> tokenList = new ArrayList<>();
        tokenList.add(token);
        return getRuleInStmOnSameLevel(startRule, tokenList, stopTokenList);
    }

    /**
     * Like getRuleInStmOnSameLvlRw but searches for a single token.
     * Converted from ABAP: GET_1_RL_IN_STM_ON_SAME_LVL_RW.
     */
    public static IRule get1RlInStmOnSameLvlRw(IRule startRule,
            String token,
            List<String> stopTokenList)
            throws AppException {
        List<String> tokenList = new ArrayList<>();
        tokenList.add(token);
        return getRuleInStmOnSameLvlRw(startRule, tokenList, stopTokenList);
    }

    /**
     * Walks backward to find the nearest unmatched '(' before startRule.
     * Converted from ABAP: FIND_PREV_OPEN_BRACKET_RULE.
     */
    public static IRule findPrevOpenBracketRule(IRule startRule)
            throws AppException {

        IRule rule = startRule;
        int closedBracketCounter = 0;

        while (true) {
            rule = rule.getPrevRule();
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
            if ("(".equals(tokenUp)) {
                if (closedBracketCounter == 0) {
                    return rule;
                }
                closedBracketCounter--;
            } else if (")".equals(tokenUp)) {
                closedBracketCounter++;
            }
        }
    }

    /**
     * Returns the next non-comment AMDP rule after startRule.
     * Converted from ABAP: GET_NEXT_NO_COMMENT_AMDP_RULE.
     */
    public static IRule getNextNoCommentAmdpRule(IRule startRule)
            throws AppException {

        IRule rule = startRule;
        while (true) {
            rule = rule.getNextRule();
            if (rule == null) {
                return null;
            }
            if (!AppUtilities.isSqlscriptRule(rule)) {
                return null;
            }
            if (rule.isComment()) {
                continue;
            }
            return rule;
        }
    }

    /**
     * Returns the previous non-comment AMDP rule before startRule.
     * Converted from ABAP: GET_PREV_NO_COMMENT_AMDP_RULE.
     */
    public static IRule getPrevNoCommentAmdpRule(IRule startRule)
            throws AppException {

        IRule rule = startRule;
        while (true) {
            rule = rule.getPrevRule();
            if (rule == null) {
                return null;
            }
            if (!AppUtilities.isSqlscriptRule(rule)) {
                return null;
            }
            if (rule.isComment()) {
                continue;
            }
            return rule;
        }
    }

    // -----------------------------------------------------------------------
    // Function-bracket analysis helpers
    // -----------------------------------------------------------------------

    /**
     * Returns true when the closing bracket of the function starting at
     * startRule is on the same source row as startRule.
     * Converted from ABAP: IS_CLS_BRA_OF_FU_IN_SAME_LINE.
     */
    public static boolean isClsBraOfFuInSameLine(IRule startRule)
            throws AppException {

        IRule nextRule = getNextNoCommentAmdpRule(startRule);
        if (nextRule == null || !"(".equals(nextRule.getTokenUp())) {
            return false;
        }

        IRule closeBracketRule = get1RuleInStmOnSameLvl(startRule, ")", null);
        if (closeBracketRule == null) {
            return false;
        }

        return startRule.getTokenExt().row == closeBracketRule.getTokenExt().row;
    }

    /**
     * Returns true when the function contains a SELECT or BY keyword at the
     * top level (these keywords are exceptions that prevent "function in one row").
     * Converted from ABAP: CONTAINS_FUNCTION_LB_EXCEPTION.
     */
    public static boolean containsFunctionLbException(IRule startRule)
            throws AppException {

        IRule nextRule = getNextNoCommentAmdpRule(startRule);
        if (nextRule == null || !"(".equals(nextRule.getTokenUp())) {
            return false;
        }

        List<String> tokenList = new ArrayList<>();
        tokenList.add("BY");
        tokenList.add("SELECT");

        IRule exceptionRule = getRuleInStmOnSameLevel(nextRule, tokenList, new ArrayList<>());
        return exceptionRule != null;
    }

    /**
     * Returns true when the function contains sub-functions that have a comma.
     * Converted from ABAP: CONTAINS_FU_SUB_FU_W_COMMA.
     */
    public static boolean containsFuSubFuWComma(IRule startRule)
            throws AppException {

        IRule openBracketRule = getNextNoCommentAmdpRule(startRule);
        if (openBracketRule == null || !"(".equals(openBracketRule.getTokenUp())) {
            return false;
        }

        IRule rule = openBracketRule;
        int openBracketCounter = 1;

        while (true) {
            rule = getNextNoCommentAmdpRule(rule);
            if (rule == null) {
                return false;
            }

            String tokenUp = rule.getTokenUp();
            if ("(".equals(tokenUp)) {
                openBracketCounter++;
            } else if (")".equals(tokenUp)) {
                openBracketCounter--;
                if (openBracketCounter == 0) {
                    return false;
                }
            }

            if (openBracketCounter > 1) {
                if (",".equals(tokenUp)) {
                    return true;
                }
                TokensExt tokenExt = rule.getTokenExt();
                if (AppUtilities.containsDelimiterChar(tokenExt.delimiter, ',')) {
                    return true;
                }
            }

            if (rule.isEndOfStatement()) {
                return false;
            }
        }
    }

    /**
     * Returns true when the function contains function keywords or sub-functions
     * with a comma (combined check).
     * Converted from ABAP: CONTAINS_FU_KW_OR_SFU_W_COMMA.
     */
    public static boolean containsFuKwOrSfuWComma(IRule startRule)
            throws AppException {

        IRule openBracketRule = getNextNoCommentAmdpRule(startRule);
        if (openBracketRule == null || !"(".equals(openBracketRule.getTokenUp())) {
            return false;
        }

        IRule rule = openBracketRule;
        int openBracketCounter = 1;
        int keyWords = 0;

        while (true) {
            rule = getNextNoCommentAmdpRule(rule);
            if (rule == null) {
                return false;
            }

            String tokenUp = rule.getTokenUp();
            if ("(".equals(tokenUp)) {
                openBracketCounter++;
            } else if (")".equals(tokenUp)) {
                openBracketCounter--;
                if (openBracketCounter == 0) {
                    return false;
                }
            }

            if (openBracketCounter > 1) {
                if (",".equals(tokenUp)) {
                    return true;
                }
                TokensExt tokenExt = rule.getTokenExt();
                if (AppUtilities.containsDelimiterChar(tokenExt.delimiter, ',')) {
                    return true;
                }
            } else if (openBracketCounter == 1) {
                TokensExt tokenExt = rule.getTokenExt();
                if (tokenExt.isKeyword) {
                    keyWords++;
                    if (keyWords == 2) {
                        return true;
                    }
                }
            }

            if (rule.isEndOfStatement()) {
                return false;
            }
        }
    }

    /**
     * Sets avoidLbAfterThisToken on every comma (or token whose delimiter
     * contains a comma) at the top level of the function call.
     * Converted from ABAP: AVOID_LB_AFTER_COMMA_IN_FUNC.
     */
    public static void avoidLbAfterCommaInFunc(IRule startRule)
            throws AppException {

        IRule openBracketRule = getNextNoCommentAmdpRule(startRule);
        if (openBracketRule == null || !"(".equals(openBracketRule.getTokenUp())) {
            return;
        }

        IRule rule = openBracketRule;
        int openBracketCounter = 1;

        while (true) {
            rule = rule.getNextRule();
            if (rule == null) {
                return;
            }
            if (!AppUtilities.isSqlscriptRule(rule)) {
                return;
            }
            if (rule.isComment()) {
                continue;
            }

            String tokenUp = rule.getTokenUp();
            if ("(".equals(tokenUp)) {
                openBracketCounter++;
            } else if (")".equals(tokenUp)) {
                openBracketCounter--;
                if (openBracketCounter == 0) {
                    return;
                }
            }

            if (openBracketCounter == 1) {
                if (",".equals(tokenUp)) {
                    rule.setAvoidLbAfterThisToken(true);
                    continue;
                }
                TokensExt tokenExt = rule.getTokenExt();
                if (AppUtilities.containsDelimiterChar(tokenExt.delimiter, ',')) {
                    rule.setAvoidLbAfterThisToken(true);
                    continue;
                }
            }

            if (rule.isEndOfStatement()) {
                return;
            }
        }
    }

    /**
     * Returns true when the "avoid line break after comma in function" logic
     * is active for the given rule/settings combination.
     * Converted from ABAP: IS_AVD_LB_AFT_COMMA_IN_FU_ACT.
     */
    public static boolean isAvdLbAftCommaInFuAct(IRule startRule, ISettings settings)
            throws AppException {

        if (!settings.isNoLbAtCoSFuDepSfu()
                && !settings.isNoLbAtCoSFuDepCbrO()
                && !settings.isNoLbAtCoSFuDepSfuKw()) {
            return false;
        }

        if (!isClsBraOfFuInSameLine(startRule)) {
            return false;
        }

        if (containsFunctionLbException(startRule)) {
            return false;
        }

        if (settings.isNoLbAtCoSFuDepSfu() && containsFuSubFuWComma(startRule)) {
            return false;
        }

        if (settings.isNoLbAtCoSFuDepSfuKw()
                && containsFuKwOrSfuWComma(startRule)) {
            return false;
        }

        return true;
    }
}
