package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.abap;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.AppUtilities;
import com.github.fmabap.amdpprettyprinter.prettyprinter.ISettings;
import com.github.fmabap.amdpprettyprinter.prettyprinter.Sqlscript;
import com.github.fmabap.amdpprettyprinter.prettyprinter.TokensExt;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.BaseRule;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.RuleData;

/**
 * Pretty-printer rule for ABAP "dummy" tokens (end-of-pending, etc.).
 * Converted from ABAP class ZCL_APP_RULE_ABAP_DUMMY.
 *
 * <p>
 * This rule handles tokens that appear at the ABAP/SQLScript boundary.
 * It uses the original (unformatted) token text and {@code delimiterOrg}
 * instead of the already-formatted variants, and it enforces that it may
 * only be applied to ABAP tokens.
 * </p>
 */
public final class AbapDummyRule extends BaseRule {

    // -----------------------------------------------------------------------
    // IRule - position
    // -----------------------------------------------------------------------

    /**
     * Returns the original column of the token (no reformatting).
     * Converted from ABAP: GET_CUR_OFFSET_START.
     */
    @Override
    public int getCurOffsetStart() throws AppException {
        return tokenExt.col;
    }

    /**
     * Returns the row for this token, taking into account whether the
     * previous token is an ABAP or SQLScript token.
     * Converted from ABAP: GET_CUR_ROW.
     */
    @Override
    public int getCurRow() throws AppException {
        if (prevRule == null) {
            return tokenExt.row;
        }

        TokensExt prevTokenExt = prevRule.getTokenExt();

        if (AppUtilities.isAbapToken(prevTokenExt.sqlscript)) {
            return prevRule.getEndRow();
        } else {
            return prevRule.hasMultlineDelimiter()
                    ? prevRule.getEndRow()
                    : prevRule.getEndRow() + 1;
        }
    }

    // -----------------------------------------------------------------------
    // IRule - text
    // -----------------------------------------------------------------------

    /**
     * Returns the original (unformatted) token text together with the
     * original delimiters ({@code delimiterOrg} instead of {@code delimiter}).
     * Converted from ABAP: GET_TEXT.
     */
    @Override
    public List<String> getText() throws AppException {
        List<String> result = new ArrayList<>();
        if (tokenExt.delimiterOrg.isEmpty()) {
            result.add(tokenExt.strOrg);
            return result;
        }
        for (int i = 0; i < tokenExt.delimiterOrg.size(); i++) {
            String delim = tokenExt.delimiterOrg.get(i);
            if (i == 0) {
                result.add(tokenExt.strOrg + delim);
            } else {
                result.add(delim);
            }
        }
        return result;
    }

    // -----------------------------------------------------------------------
    // IRule - initialisation
    // -----------------------------------------------------------------------

    /**
     * Initialises the rule and enforces that the token must be an ABAP token.
     * Raises {@link AppException} if a non-ABAP token is passed.
     * Converted from ABAP: INIT.
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

        if (!AppUtilities.isAbapToken(tokenExt.sqlscript)) {
            throw new AppException(
                    "Rule " + ruleData.ruleName
                            + " may only be used for ABAP tokens.");
        }

        super.init(tokenExt, tSource, ruleData, settings, contextRule, hlContextRule, prevRule);
    }

    /**
     * Post-initialisation: when the token is an {@code END_OF_PENDING} token with
     * more than one original delimiter, the trailing delimiter is normalised
     * (empty/blank &rarr; cleared; non-empty &rarr; an empty entry is appended).
     * Converted from ABAP: FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        if (tokenExt.sqlscript == Sqlscript.END_OF_PENDING
                && tokenExt.delimiterOrg.size() > 1) {

            int lastIndex = tokenExt.delimiterOrg.size() - 1;
            String lastDelim = tokenExt.delimiterOrg.get(lastIndex);

            if (lastDelim == null
                    || lastDelim.isEmpty()
                    || lastDelim.chars().allMatch(c -> c == ' ')) {
                // Clear the last delimiter (ABAP: CLEAR lr_delimiter->*)
                tokenExt.delimiterOrg.set(lastIndex, "");
            } else {
                // Append an empty delimiter (ABAP: INSERT INITIAL LINE)
                tokenExt.delimiterOrg.add("");
            }
        }
    }
}
