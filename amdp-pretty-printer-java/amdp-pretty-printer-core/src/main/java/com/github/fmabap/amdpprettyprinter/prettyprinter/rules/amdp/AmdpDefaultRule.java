package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.AppUtilities;
import com.github.fmabap.amdpprettyprinter.prettyprinter.CommentDetail;
import com.github.fmabap.amdpprettyprinter.prettyprinter.ISettings;
import com.github.fmabap.amdpprettyprinter.prettyprinter.TokensExt;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.BaseRule;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.RuleData;

/**
 * Default pretty-printer rule for AMDP / SQLScript tokens.
 * Converted from ABAP class ZCL_APP_RULE_AMDP_DEFAULT.
 *
 * <p>
 * Extends {@link BaseRule} with AMDP-specific indentation logic and
 * delimiter normalisation. The rule may only be applied to SQLScript tokens;
 * ABAP tokens cause an exception during initialisation.
 * </p>
 */
public class AmdpDefaultRule extends BaseRule {

    // -----------------------------------------------------------------------
    // IRule - initialisation
    // -----------------------------------------------------------------------

    /**
     * Validates that the token is a SQLScript token, sets the default line
     * indent to 4, and delegates to the base implementation.
     * Converted from ABAP: ZIF_APP_RULE~INIT.
     */
    @Override
    public void init(
            TokensExt tokenExt,
            List<String> tSource,
            RuleData ruleData,
            ISettings settings,
            IRule contextRule,
            IRule hlContextRule,
            IRule prevRule) throws AppException {

        defaultLineIndent = 4;

        if (!AppUtilities.isSqlscriptToken(tokenExt.sqlscript)) {
            throw new AppException(
                    "Rule " + ruleData.ruleName
                            + " may only be used for SQLScript tokens.");
        }

        super.init(tokenExt, tSource, ruleData, settings, contextRule, hlContextRule, prevRule);
    }

    /**
     * After the base finalisation, normalises the token delimiter unless the
     * immediately following token is a comment continuation or start.
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        super.finalizeInit();

        IRule nextRule = getNextRule();
        if (nextRule != null) {
            CommentDetail nextCommentDetail = nextRule.getTokenExt().commentDetail;
            if (nextCommentDetail == CommentDetail.PART
                    || nextCommentDetail == CommentDetail.START) {
                return;
            }
        }

        adjustDelimiter();
    }

    // -----------------------------------------------------------------------
    // IRule - position
    // -----------------------------------------------------------------------

    /**
     * Computes the start column for this token using AMDP-specific rules.
     *
     * <p>
     * Differences from {@link BaseRule#getCurOffsetStart()}:
     * </p>
     * <ul>
     * <li>For {@code START_BEGIN_OF_LINE_INDENTABLE} comments the multiline-
     * delimiter shortcut is omitted - the new-line indent is always used.</li>
     * <li>The final fallback condition additionally triggers
     * {@code getNewLineIndent()} when the previous token has a multiline
     * delimiter and the current token is not a comment continuation.</li>
     * </ul>
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_OFFSET_START.
     */
    @Override
    public int getCurOffsetStart() throws AppException {
        if (curOffsetStartSet) {
            return curOffsetStart;
        }

        // Ensure the unresolved prefix is computed iteratively, not recursively.
        resolveCurOffsetStartAncestors();

        if (tokenExt.commentDetail == CommentDetail.START_BEGIN_OF_LINE) {
            setCurOffsetStart(0);
            return 0;
        }

        if (prevRule == null
                || (!hasPrevRuleSameType()
                        && prevRule.getCurRow() != getCurRow())) {
            int result = Math.max(0, defaultLineIndent + getAdditionalIndent());
            setCurOffsetStart(result);
            return result;
        }

        CommentDetail commentDetail = tokenExt.commentDetail;

        if (commentDetail == CommentDetail.START_BEGIN_OF_LINE_INDENTABLE) {
            // AMDP default: always use new-line indent (no multiline-delimiter check)
            int result = Math.max(0, prevRule.getNewLineIndent() + getAdditionalIndent());
            setCurOffsetStart(result);
            return result;
        }

        if (commentDetail == CommentDetail.START || commentDetail == CommentDetail.PART) {
            int result = Math.max(0, prevRule.getCurOffsetEnd() + getAdditionalIndent());
            setCurOffsetStart(result);
            return result;
        }

        // AMDP-specific final condition:
        // use getNewLineIndent when prev has a multiline delimiter (and this is not
        // a PART comment) or when a new line is required and this is not a
        // line-breaking token.
        int result;
        if ((prevRule.hasMultlineDelimiter() && tokenExt.commentDetail != CommentDetail.PART)
                || (prevRule.isNewLineReq() && !isLineBreakingToken())) {
            result = prevRule.getNewLineIndent();
        } else {
            result = prevRule.getCurOffsetEnd();
        }

        result = Math.max(0, result + getAdditionalIndent());
        setCurOffsetStart(result);
        return result;
    }

    // -----------------------------------------------------------------------
    // IRule - line-break predicates
    // -----------------------------------------------------------------------

    /**
     * Extends the base new-line requirement with comma awareness: when the
     * settings request a line break after commas, a token that is {@code ,} or
     * whose delimiter contains {@code ,} also requires a new line.
     * Converted from ABAP: ZIF_APP_RULE~IS_NEW_LINE_REQ.
     */
    @Override
    public boolean isNewLineReq() throws AppException {
        if (avoidLbAfterThisToken) {
            return false;
        }

        if (super.isNewLineReq()) {
            return true;
        }

        if (settings.isLineBreakAfterCommaReq()) {
            if (",".equals(getTokenUp())) {
                return true;
            }
            if (AppUtilities.containsDelimiterChar(tokenExt.delimiter, ',')) {
                return true;
            }
        }

        return false;
    }

    // -----------------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------------

    /**
     * Normalises the token delimiter by collapsing all whitespace across every
     * delimiter string into a single space, then rebuilding the delimiter list
     * so that only the first entry carries the combined text and all subsequent
     * entries are empty.
     * Converted from ABAP: ADJUST_DELIMITER.
     */
    private void adjustDelimiter() {
        List<String> delimiter = tokenExt.delimiter;
        if (delimiter == null || delimiter.isEmpty()) {
            return;
        }

        // Concatenate all delimiter characters; keep only the first space encountered
        StringBuilder sb = new StringBuilder();
        boolean firstSpace = true;
        for (String delim : delimiter) {
            for (int i = 0; i < delim.length(); i++) {
                char c = delim.charAt(i);
                if (c == ' ') {
                    if (!firstSpace) {
                        continue; // skip all spaces after the first
                    }
                    firstSpace = false;
                    // fall through - append this first space
                }
                sb.append(c);
            }
        }

        // Rebuild list: same size, first entry = combined text, rest = ""
        List<String> newDelimiter = new ArrayList<>(delimiter.size());
        newDelimiter.add(sb.toString());
        for (int i = 1; i < delimiter.size(); i++) {
            newDelimiter.add("");
        }

        tokenExt.delimiter = newDelimiter;
    }
}
