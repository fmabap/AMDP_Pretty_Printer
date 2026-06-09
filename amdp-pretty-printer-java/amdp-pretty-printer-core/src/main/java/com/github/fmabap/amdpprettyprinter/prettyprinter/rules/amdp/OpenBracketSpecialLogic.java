package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

/**
 * Special open-bracket logic identifiers.
 * Converted from ABAP constant group COS_OPEN_BRACKET_SPECIAL_LOGIC
 * inside interface ZIF_APP_AMDP_RULE_DEFINITIONS.
 */
public enum OpenBracketSpecialLogic {

    /** CALL statement context. ABAP value: {@code 'C'}. */
    CALL_STATEMENT("C");

    private final String value;

    OpenBracketSpecialLogic(String value) {
        this.value = value;
    }

    /** Returns the single-character ABAP domain value for this entry. */
    public String getValue() {
        return value;
    }

    /** Looks up an entry by its ABAP domain value. */
    public static OpenBracketSpecialLogic fromValue(String value) {
        for (OpenBracketSpecialLogic entry : values()) {
            if (entry.value.equals(value)) {
                return entry;
            }
        }
        throw new IllegalArgumentException("Unknown OpenBracketSpecialLogic value: " + value);
    }
}
