package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.AppUtilities;
import com.github.fmabap.amdpprettyprinter.prettyprinter.CommentDetail;
import com.github.fmabap.amdpprettyprinter.prettyprinter.ISettings;
import com.github.fmabap.amdpprettyprinter.prettyprinter.TokensExt;

/**
 * Abstract base implementation of {@link IRule}.
 * Converted from ABAP class ZCL_APP_BASE_RULE.
 *
 * <p>
 * Provides the shared logic for position calculation, indentation, line-break
 * predicates, and comment handling that all concrete rule classes inherit.
 * Subclasses must implement the abstract methods declared here (none beyond
 * what {@link IRule} requires).
 * </p>
 */
public abstract class BaseRule implements IRule {

    // -----------------------------------------------------------------------
    // Protected fields (accessible to subclasses)
    // -----------------------------------------------------------------------

    protected TokensExt tokenExt;
    protected IRule contextRule;
    protected IRule hlContextRule;
    protected IRule prevRule;
    protected IRule nextRule;
    protected List<String> tSource;
    protected RuleData ruleData;
    protected ISettings settings;

    protected int defaultLineIndent;
    protected boolean avoidLbAfterThisToken;

    // -- cached position values --

    /** Protected so subclasses can consult the cache before recomputing. */
    protected boolean curOffsetStartSet;
    /** Protected so subclasses can return the cached value directly. */
    protected int curOffsetStart;

    private boolean curOffsetEndSet;
    private int curOffsetEnd;

    protected boolean curRowSet;
    protected int curRow;

    private boolean endRowSet;
    private int endRow;

    private int addIndent;

    // -----------------------------------------------------------------------
    // IRule - initialisation
    // -----------------------------------------------------------------------

    @Override
    public void init(
            TokensExt tokenExt,
            List<String> tSource,
            RuleData ruleData,
            ISettings settings,
            IRule contextRule,
            IRule hlContextRule,
            IRule prevRule) throws AppException {

        this.tokenExt = tokenExt;
        this.contextRule = contextRule;
        this.hlContextRule = hlContextRule;
        this.prevRule = prevRule;
        this.tSource = tSource;
        this.ruleData = ruleData;
        this.settings = settings;
    }

    @Override
    public void finalizeInit() throws AppException {
        // default: nothing to do
    }

    @Override
    public void validate() throws AppException {
        // default: nothing to do
    }

    // -----------------------------------------------------------------------
    // IRule - rule chain
    // -----------------------------------------------------------------------

    @Override
    public IRule getPrevRule() {
        return prevRule;
    }

    @Override
    public IRule getNextRule() {
        return nextRule;
    }

    @Override
    public void setNextRule(IRule nextRule) throws AppException {
        this.nextRule = nextRule;
    }

    // -----------------------------------------------------------------------
    // IRule - context
    // -----------------------------------------------------------------------

    @Override
    public IRule getContextRule() throws AppException {
        return contextRule;
    }

    @Override
    public IRule getHlContextRule() throws AppException {
        return hlContextRule;
    }

    @Override
    public IRule getNewContextRule() throws AppException {
        return contextRule;
    }

    @Override
    public IRule getNewHlContextRule() throws AppException {
        return hlContextRule;
    }

    @Override
    public String getNewContext() throws AppException {
        IRule ctxRule = getNewContextRule();
        if (ctxRule != null) {
            return ctxRule.getNewContext();
        }
        return null;
    }

    @Override
    public String getNewHlContext() throws AppException {
        IRule hlCtxRule = getNewHlContextRule();
        if (hlCtxRule != null) {
            return hlCtxRule.getNewContext();
        }
        return null;
    }

    // -----------------------------------------------------------------------
    // IRule - token / text
    // -----------------------------------------------------------------------

    @Override
    public TokensExt getTokenExt() {
        return tokenExt;
    }

    @Override
    public String getTokenUp() {
        return tokenExt.strUp;
    }

    @Override
    public RuleData getRuleData() throws AppException {
        return ruleData;
    }

    @Override
    public List<String> getText() throws AppException {
        List<String> result = new ArrayList<>();
        if (tokenExt.delimiter == null || tokenExt.delimiter.isEmpty()) {
            result.add(tokenExt.str);
            return result;
        }
        boolean first = true;
        for (String delim : tokenExt.delimiter) {
            if (first) {
                result.add(tokenExt.str + delim);
                first = false;
            } else {
                result.add(delim);
            }
        }
        return result;
    }

    // -----------------------------------------------------------------------
    // IRule - position
    // -----------------------------------------------------------------------

    @Override
    public void setCurRow(int curRow) throws AppException {
        this.curRow = curRow;
        this.curRowSet = true;
    }

    @Override
    public int getCurRow() throws AppException {
        if (curRowSet) {
            return curRow;
        }

        resolveCurRowAncestors();

        if (prevRule == null) {
            setCurRow(1);
            return curRow;
        }

        CommentDetail commentDetail = tokenExt.commentDetail;

        if (commentDetail == CommentDetail.START_BEGIN_OF_LINE
                || commentDetail == CommentDetail.START_BEGIN_OF_LINE_INDENTABLE) {
            int result;
            if (prevRule.hasMultlineDelimiter()) {
                result = prevRule.getEndRow();
            } else {
                result = prevRule.getEndRow() + 1;
            }
            setCurRow(result);
            return curRow;
        }

        if (commentDetail == CommentDetail.START || commentDetail == CommentDetail.PART) {
            setCurRow(prevRule.getEndRow());
            return curRow;
        }

        if (prevRule.isNewLineReq() && !isLineBreakingToken()) {
            if (!prevRule.hasMultlineDelimiter()) {
                setCurRow(prevRule.getEndRow() + 1);
                return curRow;
            }
        }

        setCurRow(prevRule.getEndRow());
        return curRow;
    }

    /**
     * Resolves {@code endRow} (and transitively {@code curRow}) for every
     * not-yet-resolved ancestor of this rule (oldest first), using an
     * explicit heap-allocated worklist instead of recursion. Without this,
     * {@link #getCurRow()} would recurse one Java stack frame per unresolved
     * predecessor - for a long token chain (e.g. a large real-world source
     * file) this can exhaust the JVM stack. After this call every
     * {@code prevRule.getEndRow()} access below is a cheap cache hit.
     *
     * <p>
     * Deliberately keyed on {@code endRowSet} rather than {@code curRowSet}:
     * {@link #getEndRow()} is never overridden and always caches
     * unconditionally, so it is a reliable "fully resolved" marker even for
     * rule classes whose {@code getCurRow()} override does not cache (e.g.
     * {@code AbapDummyRule}) - checking {@code curRowSet} there would treat
     * such a rule as forever unresolved and re-walk its whole prefix on every
     * call.
     * </p>
     */
    protected final void resolveCurRowAncestors() throws AppException {
        Deque<IRule> pending = new ArrayDeque<>();
        IRule cursor = prevRule;
        while (cursor instanceof BaseRule && !((BaseRule) cursor).endRowSet) {
            pending.push(cursor);
            cursor = cursor.getPrevRule();
        }
        while (!pending.isEmpty()) {
            pending.pop().getEndRow();
        }
    }

    @Override
    public int getEndRow() throws AppException {
        if (endRowSet) {
            return endRow;
        }
        int result = getCurRow();
        List<String> text = getText();
        if (text.size() > 1) {
            result = result + text.size() - 1;
        }
        setEndRow(result);
        return endRow;
    }

    private void setEndRow(int endRow) {
        this.endRow = endRow;
        this.endRowSet = true;
    }

    @Override
    public void setCurOffsetStart(int curOffsetStart) throws AppException {
        this.curOffsetStart = curOffsetStart;
        this.curOffsetStartSet = true;
    }

    @Override
    public int getCurOffsetStart() throws AppException {
        if (curOffsetStartSet) {
            return curOffsetStart;
        }

        resolveCurOffsetStartAncestors();

        if (tokenExt.commentDetail == CommentDetail.START_BEGIN_OF_LINE) {
            setCurOffsetStart(0);
            return 0;
        }

        if (prevRule == null
                || (!hasPrevRuleSameType()
                        && prevRule.getCurRow() != getCurRow())) {
            int result = Math.max(0, defaultLineIndent + addIndent);
            setCurOffsetStart(result);
            return result;
        }

        CommentDetail commentDetail = tokenExt.commentDetail;

        if (commentDetail == CommentDetail.START_BEGIN_OF_LINE_INDENTABLE) {
            if (prevRule.hasMultlineDelimiter()) {
                int result = Math.max(0, prevRule.getCurOffsetEnd() + addIndent);
                setCurOffsetStart(result);
                return result;
            }
            int result = Math.max(0, prevRule.getNewLineIndent() + addIndent);
            setCurOffsetStart(result);
            return result;
        }

        if (commentDetail == CommentDetail.START || commentDetail == CommentDetail.PART) {
            int result = Math.max(0, prevRule.getCurOffsetEnd() + addIndent);
            setCurOffsetStart(result);
            return result;
        }

        int result;
        if (prevRule.isNewLineReq()
                && !prevRule.hasMultlineDelimiter()
                && !isLineBreakingToken()) {
            result = prevRule.getNewLineIndent();
        } else {
            result = prevRule.getCurOffsetEnd();
        }

        result = Math.max(0, result + addIndent);
        setCurOffsetStart(result);
        return result;
    }

    /**
     * Resolves {@code curOffsetEnd} (and transitively {@code curOffsetStart})
     * for every not-yet-resolved ancestor of this rule (oldest first),
     * iteratively instead of recursively - see {@link #resolveCurRowAncestors()}
     * for the rationale, including why this is keyed on {@code curOffsetEndSet}
     * (always cached by the never-overridden {@link #getCurOffsetEnd()})
     * rather than {@code curOffsetStartSet}. Also used by
     * {@code AmdpDefaultRule#getCurOffsetStart()}, which reimplements
     * {@code getCurOffsetStart()} for AMDP tokens instead of delegating to it.
     */
    protected final void resolveCurOffsetStartAncestors() throws AppException {
        Deque<IRule> pending = new ArrayDeque<>();
        IRule cursor = prevRule;
        while (cursor instanceof BaseRule && !((BaseRule) cursor).curOffsetEndSet) {
            pending.push(cursor);
            cursor = cursor.getPrevRule();
        }
        while (!pending.isEmpty()) {
            pending.pop().getCurOffsetEnd();
        }
    }

    @Override
    public int getCurOffsetEnd() throws AppException {
        if (curOffsetEndSet) {
            return curOffsetEnd;
        }

        int offset = getCurOffsetStart();
        List<String> text = getText();

        if (text.size() == 1) {
            int result = offset + text.get(0).length();
            setCurOffsetEnd(result);
            return result;
        }

        // multi-line: end offset is the length of the last line
        String lastLine = text.get(text.size() - 1);
        int result = lastLine.length();
        setCurOffsetEnd(result);
        return result;
    }

    private void setCurOffsetEnd(int curOffsetEnd) {
        this.curOffsetEnd = curOffsetEnd;
        this.curOffsetEndSet = true;
    }

    // -----------------------------------------------------------------------
    // IRule - indentation
    // -----------------------------------------------------------------------

    @Override
    public int getNewLineIndent() throws AppException {
        // Walk backward through the run of rules that would just delegate to
        // this default (same-type, non-terminal, no active special logic)
        // implementation, iteratively instead of recursively - a long run of
        // same-type tokens (e.g. many SELECT columns) is never cached (the
        // value may legitimately change across calcRuleResult's convergence
        // iterations), so unlike the row/offset chains this cannot rely on a
        // cache flag to bound recursion depth.
        List<BaseRule> plainChain = new ArrayList<>();
        BaseRule current = this;
        while (current.hasPrevRuleSameType()
                && !current.isEndOfStatement()
                && current.prevRule instanceof BaseRule
                && ((BaseRule) current.prevRule).usesDefaultNewLineIndent()) {
            plainChain.add(current);
            current = (BaseRule) current.prevRule;
        }

        int result;
        if (current.hasPrevRuleSameType()) {
            result = current.isEndOfStatement()
                    ? current.prevRule.getNewStatementIndent()
                    : current.prevRule.getNewLineIndent();
        } else {
            result = current.defaultLineIndent;
        }
        result = Math.max(0, result + current.ruleData.newLineIndentDiff);

        for (int i = plainChain.size() - 1; i >= 0; i--) {
            BaseRule rule = plainChain.get(i);
            result = Math.max(0, result + rule.ruleData.newLineIndentDiff);
        }
        return result;
    }

    /**
     * Whether {@link #getNewLineIndent()} for this rule instance resolves to
     * this default same-type-prefix logic rather than rule-specific special
     * logic. Used by the iterative walk in {@link #getNewLineIndent()} to
     * know how far it may safely keep unrolling instead of falling back to an
     * ordinary (polymorphic, possibly recursive) call. Overridden by rule
     * classes whose {@code getNewLineIndent()} has an active alternative
     * branch, mirroring their existing fallback condition exactly.
     */
    protected boolean usesDefaultNewLineIndent() throws AppException {
        return true;
    }

    @Override
    public int getNewStatementIndent() throws AppException {
        // Same rationale as getNewLineIndent() above; getNewStatementIndent()
        // is never overridden, so no "special logic" boundary check is needed.
        List<BaseRule> plainChain = new ArrayList<>();
        BaseRule current = this;
        while (current.hasPrevRuleSameType() && current.prevRule instanceof BaseRule) {
            plainChain.add(current);
            current = (BaseRule) current.prevRule;
        }

        int result = current.hasPrevRuleSameType()
                ? current.prevRule.getNewStatementIndent()
                : current.defaultLineIndent;
        result += current.ruleData.newStatementIndentDiff;

        for (int i = plainChain.size() - 1; i >= 0; i--) {
            BaseRule rule = plainChain.get(i);
            result += rule.ruleData.newStatementIndentDiff;
        }
        return result;
    }

    @Override
    public void setAdditionalIndent(int indent) throws AppException {
        this.addIndent = indent;
    }

    @Override
    public int getAdditionalIndent() throws AppException {
        return addIndent;
    }

    // -----------------------------------------------------------------------
    // IRule - line-break predicates
    // -----------------------------------------------------------------------

    @Override
    public boolean isNewLineReq() throws AppException {
        if (avoidLbAfterThisToken) {
            return false;
        }
        if (isComment()) {
            return true;
        }
        if (isEndOfStatement()) {
            return true;
        }
        return ruleData.isNewLineReq;
    }

    @Override
    public boolean isLineBreakingToken() {
        if (avoidLbAfterThisToken) {
            return false;
        }
        if (isComment()) {
            return false;
        }
        if (AppUtilities.isAbapToken(tokenExt.sqlscript)) {
            String up = getTokenUp();
            return ".".equals(up) || ",".equals(up);
        }
        if (AppUtilities.isSqlscriptToken(tokenExt.sqlscript)) {
            String up = getTokenUp();
            if (";".equals(up)) {
                return true;
            }
            if (",".equals(up)) {
                return settings.isLineBreakAfterCommaReq();
            }
        }
        return false;
    }

    @Override
    public boolean isLbTokenRespDelimiter() {
        if (avoidLbAfterThisToken) {
            return false;
        }
        if (isComment()) {
            return false;
        }
        if (AppUtilities.isAbapToken(tokenExt.sqlscript)) {
            String up = getTokenUp();
            if (".".equals(up) || ",".equals(up)) {
                return true;
            }
            if (AppUtilities.containsDelimiterChar(tokenExt.delimiter, '.')) {
                return true;
            }
            return AppUtilities.containsDelimiterChar(tokenExt.delimiter, ',');
        }
        if (AppUtilities.isSqlscriptToken(tokenExt.sqlscript)) {
            String up = getTokenUp();
            if (";".equals(up)) {
                return true;
            }
            if (",".equals(up) && settings.isLineBreakAfterCommaReq()) {
                return true;
            }
            if (AppUtilities.containsDelimiterChar(tokenExt.delimiter, ';')) {
                return true;
            }
            if (settings.isLineBreakAfterCommaReq()) {
                return AppUtilities.containsDelimiterChar(tokenExt.delimiter, ',');
            }
        }
        return false;
    }

    // -----------------------------------------------------------------------
    // IRule - end-of-statement / multi-line delimiter
    // -----------------------------------------------------------------------

    @Override
    public boolean isEndOfStatement() throws AppException {
        if (isComment()) {
            if (prevRule != null) {
                return prevRule.isEndOfStatement();
            }
            return false;
        }

        char statementEnd;
        if (AppUtilities.isAbapToken(tokenExt.sqlscript)) {
            statementEnd = '.';
        } else if (AppUtilities.isSqlscriptToken(tokenExt.sqlscript)) {
            statementEnd = ';';
        } else {
            return false;
        }

        if (getTokenUp().equals(String.valueOf(statementEnd))) {
            return true;
        }

        if (tokenExt.delimiter == null || tokenExt.delimiter.isEmpty()) {
            return false;
        }

        return AppUtilities.containsDelimiterChar(tokenExt.delimiter, statementEnd);
    }

    @Override
    public boolean hasMultlineDelimiter() throws AppException {
        return tokenExt.delimiter != null && tokenExt.delimiter.size() >= 2;
    }

    // -----------------------------------------------------------------------
    // IRule - comment
    // -----------------------------------------------------------------------

    @Override
    public boolean isComment() {
        return AppUtilities.isComment(tokenExt.comment);
    }

    // -----------------------------------------------------------------------
    // IRule - misc
    // -----------------------------------------------------------------------

    @Override
    public void setAvoidLbAfterThisToken(boolean avoid) throws AppException {
        this.avoidLbAfterThisToken = avoid;
    }

    @Override
    public void refreshBuffer() {
        curOffsetStartSet = false;
        curOffsetStart = 0;
        curOffsetEndSet = false;
        curOffsetEnd = 0;
        curRowSet = false;
        curRow = 0;
        endRowSet = false;
        endRow = 0;
    }

    // -----------------------------------------------------------------------
    // Protected helpers
    // -----------------------------------------------------------------------

    /**
     * Returns {@code true} when the previous rule exists and carries a token of
     * the same kind (ABAP vs. SQLScript) as this rule.
     * Converted from ABAP: HAS_PREV_RULE_SAME_TYPE.
     */
    protected boolean hasPrevRuleSameType() throws AppException {
        if (prevRule == null) {
            return false;
        }
        return AppUtilities.isSqlscriptRule(this) == AppUtilities.isSqlscriptRule(prevRule);
    }
}
