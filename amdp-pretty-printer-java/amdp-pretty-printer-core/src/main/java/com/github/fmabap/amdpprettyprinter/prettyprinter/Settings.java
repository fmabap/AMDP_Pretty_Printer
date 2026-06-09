package com.github.fmabap.amdpprettyprinter.prettyprinter;

/**
 * Immutable, instance-based settings for the AMDP Pretty Printer.
 * Converted from ABAP class ZCL_APP_SETTINGS (singleton) to a plain,
 * thread-safe value object so that the core library can be used safely
 * from multiple threads or multiple callers within the same JVM.
 *
 * <p>
 * Usage:
 * 
 * <pre>
 * // default: always insert line break after comma
 * ISettings s = new Settings();
 *
 * // custom rule
 * ISettings s = new Settings(ISettings.LB_RULE_DEP_ON_CLS_BRACKET_ONLY);
 * </pre>
 * </p>
 */
public final class Settings implements ISettings {

    // ---------------------------------------------------------------
    // Constants (aliases for the ISettings rule constants)
    // ---------------------------------------------------------------
    public static final String LB_ALWAYS = ISettings.LB_RULE_ALWAYS_LINE_BREAK;
    public static final String LB_NO_LINE_BREAK = ISettings.LB_RULE_NO_LINE_BREAK;
    public static final String LB_DEP_ON_CLS_BR_ONLY = ISettings.LB_RULE_DEP_ON_CLS_BRACKET_ONLY;
    public static final String LB_DEP_ON_CLS_BR_AND_SF = ISettings.LB_RULE_DEP_ON_CLS_BRACKET_AND_SUB_FU;
    public static final String LB_DEP_ON_CLS_BR_SF_KW = ISettings.LB_RULE_DEP_ON_CLS_BR_SF_AND_KEYWRD;

    // ---------------------------------------------------------------
    // Instance state
    // ---------------------------------------------------------------
    private final String lbAfterCommaRule;
    private final boolean trace;

    // ---------------------------------------------------------------
    // Constructors
    // ---------------------------------------------------------------

    /**
     * Creates settings with the default rule: dependent on closing bracket,
     * sub-function, and keyword
     * ({@link ISettings#LB_RULE_DEP_ON_CLS_BR_SF_AND_KEYWRD}).
     */
    public Settings() {
        this(LB_DEP_ON_CLS_BR_SF_KW, false);
    }

    /**
     * Creates settings with the given line-break-after-comma rule and tracing
     * disabled.
     *
     * @param lbAfterCommaRule one of the {@code LB_RULE_*} constants defined
     *                         in {@link ISettings}
     * @throws IllegalArgumentException if the value is not a recognised rule
     */
    public Settings(String lbAfterCommaRule) {
        this(lbAfterCommaRule, false);
    }

    /**
     * Creates settings with the given line-break-after-comma rule and trace flag.
     *
     * @param lbAfterCommaRule one of the {@code LB_RULE_*} constants defined
     *                         in {@link ISettings}
     * @param trace            when {@code true}, token output is printed after
     *                         tokenisation
     * @throws IllegalArgumentException if the rule value is not recognised
     */
    public Settings(String lbAfterCommaRule, boolean trace) {
        switch (lbAfterCommaRule) {
            case LB_ALWAYS:
            case LB_NO_LINE_BREAK:
            case LB_DEP_ON_CLS_BR_ONLY:
            case LB_DEP_ON_CLS_BR_AND_SF:
            case LB_DEP_ON_CLS_BR_SF_KW:
                break;
            default:
                throw new IllegalArgumentException(
                        "Unknown lb-after-comma rule: '" + lbAfterCommaRule + "'");
        }
        this.lbAfterCommaRule = lbAfterCommaRule;
        this.trace = trace;
    }

    // ---------------------------------------------------------------
    // ISettings implementation
    // ---------------------------------------------------------------

    @Override
    public boolean isLineBreakAfterCommaReq() {
        return !LB_NO_LINE_BREAK.equals(lbAfterCommaRule);
    }

    @Override
    public boolean isAlwaysLineBreakAftComma() {
        return LB_ALWAYS.equals(lbAfterCommaRule);
    }

    @Override
    public boolean isNoLbAtCoSFuDepSfu() {
        return LB_DEP_ON_CLS_BR_AND_SF.equals(lbAfterCommaRule);
    }

    @Override
    public boolean isNoLbAtCoSFuDepCbrO() {
        return LB_DEP_ON_CLS_BR_ONLY.equals(lbAfterCommaRule);
    }

    @Override
    public boolean isNoLbAtCoSFuDepSfuKw() {
        return LB_DEP_ON_CLS_BR_SF_KW.equals(lbAfterCommaRule);
    }

    @Override
    public boolean isTrace() {
        return trace;
    }
}
