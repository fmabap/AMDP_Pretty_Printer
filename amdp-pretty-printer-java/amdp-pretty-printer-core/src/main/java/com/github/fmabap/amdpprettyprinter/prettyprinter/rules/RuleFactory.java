package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;
import com.github.fmabap.amdpprettyprinter.prettyprinter.ISettings;
import com.github.fmabap.amdpprettyprinter.prettyprinter.TokensExt;

/**
 * Factory that creates and initialises {@link IRule} instances for each
 * token.
 * Converted from ABAP class ZCL_APP_RULE_FACTORY.
 *
 * <p>
 * Usage pattern (mirrors the ABAP caller):
 * 
 * <pre>
 * RuleFactory factory = new RuleFactory(settings);
 * IRule rule = factory.getRule(source, statements, structures, prevRule, tokenExt);
 * </pre>
 * </p>
 */
public final class RuleFactory {

    // ---------------------------------------------------------------
    // Private state
    // ---------------------------------------------------------------

    /** Looks up the rule descriptor for a token/context combination. */
    private final IRuleFinder ruleFinder;

    /** Active formatting settings forwarded to every rule. */
    private final ISettings settings;

    // ---------------------------------------------------------------
    // Constructor (mirrors METHOD constructor)
    // ---------------------------------------------------------------

    /**
     * Creates a new factory.
     *
     * @param settings active pretty-printer settings
     * @throws AppException if the internal rule finder cannot be created
     */
    public RuleFactory(ISettings settings) throws AppException {
        this.settings = settings;
        this.ruleFinder = new RuleFinder();
    }

    // ---------------------------------------------------------------
    // Public API
    // ---------------------------------------------------------------

    /**
     * Creates, initialises, and returns the rule for {@code tokenExt}.
     *
     * <p>
     * Mirrors ABAP {@code GET_RULE}. The rule's implementation class is
     * looked up dynamically via {@link Class#forName(String)}, exactly as
     * ABAP does with
     * {@code CREATE OBJECT rr_result TYPE (lr_rule_data->rule_class)}.
     * </p>
     *
     * @param source     source lines
     * @param statements statement table (sstmnt_tab)
     * @param structures structure table (sstruc_tab)
     * @param prevRule   previous rule (may be {@code null} for the first token)
     * @param tokenExt   extended token descriptor for the current token
     * @return initialised rule instance
     * @throws AppException if the rule class is missing, unknown, or fails to
     *                      initialise
     */
    public IRule getRule(List<String> source,
            IRule prevRule,
            TokensExt tokenExt) throws AppException {

        // Build search key (mirrors GET_RULE_SEARCH)
        RuleSearch ruleSearch = buildRuleSearch(tokenExt, prevRule);

        // Find rule descriptor
        RuleData ruleData = ruleFinder.getRuleData(ruleSearch);

        // Validate: rule_class must be set (mirrors RAISE EXCEPTION … NUMBER '006')
        if (ruleData.ruleClass == null || ruleData.ruleClass.isEmpty()) {
            throw new AppException(
                    "No rule class configured for rule '" + ruleData.ruleName + "'");
        }

        // Dynamic instantiation - mirrors CREATE OBJECT rr_result TYPE
        // (lr_rule_data->rule_class)
        IRule rule = instantiateRule(ruleData.ruleClass);

        // Deep-copy rule data so each rule instance owns its own descriptor
        RuleData ruleDataCopy = copyRuleData(ruleData);

        // Initialise the rule (with or without context from the previous rule)
        if (prevRule == null) {
            rule.init(tokenExt, source,
                    ruleDataCopy, settings,
                    null, null, null);
        } else {
            rule.init(tokenExt, source,
                    ruleDataCopy, settings,
                    prevRule.getNewContextRule(),
                    prevRule.getNewHlContextRule(),
                    prevRule);
        }

        return rule;
    }

    // ---------------------------------------------------------------
    // Private helpers
    // ---------------------------------------------------------------

    /**
     * Builds a {@link RuleSearch} from the current token and the previous rule.
     * Mirrors ABAP {@code GET_RULE_SEARCH}.
     */
    private RuleSearch buildRuleSearch(TokensExt tokenExt, IRule prevRule)
            throws AppException {

        RuleSearch rs = new RuleSearch();
        rs.token = tokenExt.strUp;
        rs.sqlscript = tokenExt.sqlscript.isSqlscript();

        if (prevRule != null) {
            rs.context = prevRule.getNewContext();
            rs.hlContext = prevRule.getNewHlContext();
        }

        return rs;
    }

    /**
     * Dynamically instantiates an {@link IRule} from a fully-qualified class
     * name.
     * Mirrors ABAP {@code CREATE OBJECT rr_result TYPE (lr_rule_data->rule_class)}.
     */
    private IRule instantiateRule(String className) throws AppException {
        try {
            Class<?> cls = Class.forName(className);
            Object instance = cls.getDeclaredConstructor().newInstance();
            if (!(instance instanceof IRule)) {
                throw new AppException(
                        "Class '" + className + "' does not implement IRule");
            }
            return (IRule) instance;
        } catch (ClassNotFoundException e) {
            throw new AppException("Rule class not found: " + className, e);
        } catch (NoSuchMethodException | InstantiationException
                | IllegalAccessException | InvocationTargetException e) {
            throw new AppException(
                    "Cannot instantiate rule class '" + className + "': " + e.getMessage(), e);
        }
    }

    /** Creates a shallow copy of a {@link RuleData} instance. */
    private RuleData copyRuleData(RuleData src) {
        RuleData copy = new RuleData();
        copy.ruleName = src.ruleName;
        copy.token = src.token;
        copy.context = src.context;
        copy.hlContext = src.hlContext;
        copy.sqlscript = src.sqlscript;
        copy.ruleClass = src.ruleClass;
        copy.addIndent = src.addIndent;
        copy.ruleCondClass = src.ruleCondClass;
        copy.newLineIndentDiff = src.newLineIndentDiff;
        copy.newStatementIndentDiff = src.newStatementIndentDiff;
        copy.isNewLineReq = src.isNewLineReq;
        return copy;
    }
}
