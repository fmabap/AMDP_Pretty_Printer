package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * Looks up the formatting rule that matches a given token / context
 * combination.
 * Converted from ABAP class ZCL_APP_RULE_FINDER (implements
 * ZIF_APP_RULE_FINDER).
 *
 * <p>
 * The ABAP implementation stores rules in a sorted table with a 4-field key
 * (TOKEN, CONTEXT, HL_CONTEXT, SQLSCRIPT) and uses
 * {@code READ TABLE … WITH TABLE KEY}
 * for fast exact-match lookups. In Java this is modelled as a {@link HashMap}
 * keyed on {@link RuleKey} (an immutable value-object covering the same four
 * fields).
 * </p>
 *
 * <p>
 * The lookup cascade (mirrors {@code ZIF_APP_RULE_FINDER~GET_RULE_DATA}) tries
 * progressively less-specific keys until a match is found:
 * </p>
 * <ol>
 * <li>Full spec: token + context + hlContext + sqlscript</li>
 * <li>With context, no HL context: token + context + "" + sqlscript</li>
 * <li>Without token: "" + context + hlContext + sqlscript</li>
 * <li>Token only: token + "" + "" + sqlscript</li>
 * <li>Default: "" + "" + "" + sqlscript</li>
 * </ol>
 * If none of the five attempts yields a match an {@link AppException} is thrown
 * (ZAPP_MC_PRETTY_PRINT message 005).
 */
public final class RuleFinder implements IRuleFinder {

    // -----------------------------------------------------------------------
    // Composite key (replaces the sorted-table key in ABAP)
    // -----------------------------------------------------------------------

    private static final class RuleKey {
        final String token;
        final String context;
        final String hlContext;
        final boolean sqlscript;

        RuleKey(String token, String context, String hlContext, boolean sqlscript) {
            this.token = token != null ? token : "";
            this.context = context != null ? context : "";
            this.hlContext = hlContext != null ? hlContext : "";
            this.sqlscript = sqlscript;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o)
                return true;
            if (!(o instanceof RuleKey))
                return false;
            RuleKey k = (RuleKey) o;
            return sqlscript == k.sqlscript
                    && token.equals(k.token)
                    && context.equals(k.context)
                    && hlContext.equals(k.hlContext);
        }

        @Override
        public int hashCode() {
            return Objects.hash(token, context, hlContext, sqlscript);
        }
    }

    // -----------------------------------------------------------------------
    // State (mirrors DATA mt_rule_data TYPE zapp_t_rule_sort)
    // -----------------------------------------------------------------------

    private final Map<RuleKey, RuleData> ruleMap;

    // -----------------------------------------------------------------------
    // Constructor (mirrors METHOD constructor)
    // -----------------------------------------------------------------------

    /**
     * Loads all rules from {@link RuleProvider}.
     *
     * @throws AppException if the provider cannot supply the rule list
     */
    public RuleFinder() throws AppException {
        IRuleProvider provider = new RuleProvider();
        List<RuleData> rules = provider.getRules();

        ruleMap = new HashMap<>(rules.size() * 2);
        for (RuleData rd : rules) {
            ruleMap.put(new RuleKey(rd.token, rd.context, rd.hlContext, rd.sqlscript), rd);
        }
    }

    // -----------------------------------------------------------------------
    // IRuleFinder implementation
    // -----------------------------------------------------------------------

    /**
     * Looks up the best-matching rule for the given search key.
     *
     * <p>
     * Mirrors ABAP method {@code ZIF_APP_RULE_FINDER~GET_RULE_DATA}.
     * </p>
     *
     * @param ruleSearch search criteria
     * @return matching rule descriptor (never {@code null})
     * @throws AppException if no rule matches (ZAPP_MC_PRETTY_PRINT 005)
     */
    @Override
    public RuleData getRuleData(RuleSearch ruleSearch) throws AppException {

        RuleData result;

        // 1. Full spec: token + context + hlContext + sqlscript
        result = getRuleDataByFullSpec(ruleSearch);
        if (result != null)
            return result;

        // 2. token + context + hlContext="" + sqlscript
        result = getRuleDataWContext(ruleSearch);
        if (result != null)
            return result;

        // 3. token="" + context + hlContext + sqlscript
        result = getRuleDataWoToken(ruleSearch);
        if (result != null)
            return result;

        // 4. token + context="" + hlContext="" + sqlscript
        result = getRuleDataWToken(ruleSearch);
        if (result != null)
            return result;

        // 5. token="" + context="" + hlContext="" + sqlscript (default)
        result = getRuleDataDefault(ruleSearch);
        if (result != null)
            return result;

        throw new AppException(
                "No rule found for token='" + ruleSearch.token
                        + "' context='" + ruleSearch.context
                        + "' hlContext='" + ruleSearch.hlContext
                        + "' sqlscript=" + ruleSearch.sqlscript);
    }

    // -----------------------------------------------------------------------
    // Private lookup helpers (mirror the private ABAP methods)
    // -----------------------------------------------------------------------

    /** Mirrors GET_RULE_DATA_BY_FULL_SPEC. */
    private RuleData getRuleDataByFullSpec(RuleSearch s) {
        return ruleMap.get(new RuleKey(s.token, s.context, s.hlContext, s.sqlscript));
    }

    /** Mirrors GET_RULE_DATA_W_CONTEXT - hlContext is cleared. */
    private RuleData getRuleDataWContext(RuleSearch s) {
        return ruleMap.get(new RuleKey(s.token, s.context, "", s.sqlscript));
    }

    /** Mirrors GET_RULE_DATA_WO_TOKEN - token is cleared. */
    private RuleData getRuleDataWoToken(RuleSearch s) {
        return ruleMap.get(new RuleKey("", s.context, s.hlContext, s.sqlscript));
    }

    /** Mirrors GET_RULE_DATA_W_TOKEN - context and hlContext are cleared. */
    private RuleData getRuleDataWToken(RuleSearch s) {
        return ruleMap.get(new RuleKey(s.token, "", "", s.sqlscript));
    }

    /** Mirrors GET_RULE_DATA_DEFAULT - token, context and hlContext all cleared. */
    private RuleData getRuleDataDefault(RuleSearch s) {
        return ruleMap.get(new RuleKey("", "", "", s.sqlscript));
    }
}
