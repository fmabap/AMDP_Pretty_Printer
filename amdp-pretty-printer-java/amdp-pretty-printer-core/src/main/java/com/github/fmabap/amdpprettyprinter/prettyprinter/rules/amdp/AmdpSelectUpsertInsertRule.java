package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.AppUtilities;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Handles SELECT, UPSERT, and INSERT tokens in SQLScript statements.
 * Computes the correct column offset so that SELECT columns line up under the
 * first column of the SELECT list, accounting for JOIN, ORDER BY, GROUP BY,
 * DISTINCT, UNION ALL, and nested INSERT … SELECT patterns.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_SEL_UPS_INS.
 * </p>
 */
public class AmdpSelectUpsertInsertRule extends AmdpDefaultNoCommentRule {

    /**
     * Guards the expensive {@link #computeAdditionalIndent()} so it is executed
     * at most once per buffer cycle (reset by {@link #refreshBuffer()}).
     */
    private boolean computedIndentSet;

    // -----------------------------------------------------------------------
    // IRule - finalizeInit
    // -----------------------------------------------------------------------

    /**
     * After parent finalisation, when logic is active, normalises the delimiter
     * to a single space.
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        super.finalizeInit();
        if (isLogicActive()) {
            tokenExt.delimiter = AppUtilities.getSpaceAsDelimiter();
        }
    }

    // -----------------------------------------------------------------------
    // IRule - getNewLineIndent
    // -----------------------------------------------------------------------

    /**
     * When logic is active, the new-line indent equals the end column of this
     * token (so subsequent tokens start directly after SELECT / INSERT / UPSERT).
     * Converted from ABAP: ZIF_APP_RULE~GET_NEW_LINE_INDENT.
     */
    @Override
    public int getNewLineIndent() throws AppException {
        if (!isLogicActive()) {
            return super.getNewLineIndent();
        }
        return getCurOffsetEnd();
    }

    /**
     * Mirrors the fallback condition of {@link #getNewLineIndent()} so the
     * iterative same-type-prefix walk in {@code BaseRule.getNewLineIndent()}
     * knows it may unroll through this rule when its special logic is not
     * active.
     */
    @Override
    protected boolean usesDefaultNewLineIndent() throws AppException {
        return !isLogicActive();
    }

    // -----------------------------------------------------------------------
    // IRule - getCurOffsetStart
    // -----------------------------------------------------------------------

    /**
     * When logic is active and the token is inside an INSERT … SELECT pattern
     * (a preceding INSERT / UPSERT exists at the same bracket level), the offset
     * is derived from the previous rule's new-line indent plus the additional
     * indent. Otherwise the additional indent is computed from the surrounding
     * structure (JOIN type, ORDER BY, GROUP BY, DISTINCT, UNION ALL) before
     * delegating to the parent.
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_OFFSET_START.
     */
    @Override
    public int getCurOffsetStart() throws AppException {
        if (isLogicActive()) {
            IRule prevInsertRule = getPrevUpsInsRule();
            if (prevInsertRule != null) {
                int result = Math.max(0, prevRule.getNewLineIndent());
                result = Math.max(0, result + getAdditionalIndent());
                setCurOffsetStart(result);
                return result;
            } else {
                computeAdditionalIndent();
            }
        }
        return super.getCurOffsetStart();
    }

    // -----------------------------------------------------------------------
    // IRule - getCurRow
    // -----------------------------------------------------------------------

    /**
     * For INSERT … SELECT patterns, when the SELECT is on the same row as the
     * INSERT / UPSERT, the SELECT is pushed to a new row.
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_ROW.
     */
    @Override
    public int getCurRow() throws AppException {
        int curRow = super.getCurRow();

        if (prevRule != null && isLogicActive()) {
            if (prevRule.getCurRow() == curRow) {
                IRule prevInsertRule = getPrevUpsInsRule();
                if (prevInsertRule != null) {
                    curRow = curRow + 1;
                    setCurRow(curRow);
                }
            }
        }

        return super.getCurRow();
    }

    // -----------------------------------------------------------------------
    // IRule - refreshBuffer
    // -----------------------------------------------------------------------

    /**
     * Resets the computed-indent guard so that {@link #computeAdditionalIndent()}
     * is re-evaluated after a buffer refresh.
     * Converted from ABAP: ZIF_APP_RULE~REFRESH_BUFFER.
     */
    @Override
    public void refreshBuffer() {
        super.refreshBuffer();
        computedIndentSet = false;
    }

    // -----------------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------------

    /**
     * Determines and sets the additional indent for this SELECT / UPSERT / INSERT
     * rule based on the surrounding statement structure. The result is cached in
     * {@link #computedIndentSet}.
     * Converted from ABAP: SET_ADDITIONAL_INDENT (private method).
     */
    private void computeAdditionalIndent() throws AppException {
        if (computedIndentSet) {
            return;
        }

        IRule prevSelectRule = getPrevSelectRule();
        if (prevSelectRule != null) {
            setAdditionalIndent(prevSelectRule.getAdditionalIndent());
            computedIndentSet = true;
            return;
        }

        IRule joinRule = getLongestJoinRule();
        if (joinRule != null) {
            int indent;
            switch (joinRule.getTokenUp()) {
                case "LEFT":
                    indent = 9;
                    break;
                case "RIGHT":
                    indent = 10;
                    break;
                default: // CROSS, INNER
                    indent = 4;
                    break;
            }
            setAdditionalIndent(indent);
            computedIndentSet = true;
            return;
        }

        IRule unionAllRule = getUnionAllRule();
        if (unionAllRule != null) {
            setAdditionalIndent(3);
            computedIndentSet = true;
            return;
        }

        IRule distinctRule = getDistinctRule();
        if (distinctRule != null) {
            setAdditionalIndent(2);
            computedIndentSet = true;
            return;
        }

        IRule orderRule = getOrderRule();
        if (orderRule != null) {
            setAdditionalIndent(2);
            computedIndentSet = true;
            return;
        }

        IRule groupRule = getGroupRule();
        if (groupRule != null) {
            setAdditionalIndent(2);
            computedIndentSet = true;
            return;
        }

        IRule defaultRule = getDefaultRule();
        if (defaultRule != null) {
            setAdditionalIndent(1);
            computedIndentSet = true;
            return;
        }

        computedIndentSet = true;
    }

    /**
     * Finds the longest JOIN keyword (RIGHT > LEFT > INNER > CROSS) in the same
     * statement. Returns {@code null} when no JOIN exists.
     * Converted from ABAP: GET_LONGEST_JOIN_RULE.
     */
    private IRule getLongestJoinRule() throws AppException {
        IRule result = getJoinRule("RIGHT");
        if (result != null) {
            return result;
        }
        result = getJoinRule("LEFT");
        if (result != null) {
            return result;
        }
        result = getJoinRule("INNER");
        if (result != null) {
            return result;
        }
        return getJoinRule("CROSS");
    }

    /**
     * Finds a JOIN rule for the given keyword ({@code LEFT}, {@code RIGHT},
     * {@code INNER}, or {@code CROSS}) in the same statement at the same bracket
     * level, skipping function calls (open bracket immediately after LEFT/RIGHT).
     * Converted from ABAP: GET_JOIN_RULE.
     */
    private IRule getJoinRule(String token) throws AppException {
        List<String> tokenList = new ArrayList<>();
        tokenList.add(token);

        IRule startRule = this;
        while (true) {
            IRule result = AmdpRuleUtilities.getRuleInStmOnSameLevel(
                    startRule, tokenList, null);
            if (result == null) {
                return null;
            }

            String tokenUp = result.getTokenUp();
            if ("LEFT".equals(tokenUp) || "RIGHT".equals(tokenUp)) {
                IRule nextRule = result.getNextRule();
                if (nextRule != null && "(".equals(nextRule.getTokenUp())) {
                    // This is a function call (e.g. LEFT(...)), not a JOIN.
                    startRule = nextRule;
                    continue;
                }
            }
            return result;
        }
    }

    /** Converted from ABAP: GET_ORDER_RULE. */
    private IRule getOrderRule() throws AppException {
        return AmdpRuleUtilities.get1RuleInStmOnSameLvl(this, "ORDER", null);
    }

    /** Converted from ABAP: GET_DEFAULT_RULE. */
    private IRule getDefaultRule() throws AppException {
        return AmdpRuleUtilities.get1RuleInStmOnSameLvl(this, "DEFAULT", null);
    }

    /** Converted from ABAP: GET_GROUP_RULE. */
    private IRule getGroupRule() throws AppException {
        return AmdpRuleUtilities.get1RuleInStmOnSameLvl(this, "GROUP", null);
    }

    /** Converted from ABAP: GET_DISTINCT_RULE. */
    private IRule getDistinctRule() throws AppException {
        return AmdpRuleUtilities.get1RuleInStmOnSameLvl(this, "DISTINCT", null);
    }

    /**
     * Returns the UNION rule when followed by ALL (i.e. UNION ALL).
     * Converted from ABAP: GET_UNION_ALL_RULE.
     */
    private IRule getUnionAllRule() throws AppException {
        IRule rule = AmdpRuleUtilities.get1RuleInStmOnSameLvl(this, "UNION", null);
        if (rule != null) {
            IRule nextRule = rule.getNextRule();
            if (nextRule != null && "ALL".equals(nextRule.getTokenUp())) {
                return rule;
            }
        }
        return null;
    }

    /**
     * Finds the nearest preceding SELECT at the same bracket level.
     * Converted from ABAP: GET_PREV_SELECT_RULE.
     */
    private IRule getPrevSelectRule() throws AppException {
        return AmdpRuleUtilities.get1RlInStmOnSameLvlRw(this, "SELECT", null);
    }

    /**
     * Finds the nearest preceding INSERT or UPSERT at the same bracket level.
     * Converted from ABAP: GET_PREV_UPS_INS_RULE.
     */
    private IRule getPrevUpsInsRule() throws AppException {
        List<String> tokenList = new ArrayList<>();
        tokenList.add("INSERT");
        tokenList.add("UPSERT");

        return AmdpRuleUtilities.getRuleInStmOnSameLvlRw(this, tokenList, null);
    }
}
