package com.github.fmabap.amdpprettyprinter.prettyprinter;

/**
 * SQLScript context of a token in the source stream.
 * Converted from ABAP class ZCL_APP_SCANNER_SQLSCRIPT (constants only).
 *
 * <pre>
 * ABAP constant             → enum value
 * cos_sqlscript-none        → NONE
 * cos_sqlscript-pending     → PENDING
 * cos_sqlscript-end_of_pend → END_OF_PENDING
 * cos_sqlscript-sqlscript   → SQLSCRIPT
 * </pre>
 */
public enum Sqlscript {

    /** Normal ABAP token - no SQLScript in sight (cos_sqlscript-none). */
    NONE,

    /**
     * Token is part of the METHOD … BY DATABASE PROCEDURE / FUNCTION declaration,
     * i.e. between LANGUAGE SQLSCRIPT and the terminating '.'.
     * (cos_sqlscript-pending)
     */
    PENDING,

    /**
     * The single token that carries the terminating '.' of the declaration
     * (the last PENDING token). (cos_sqlscript-end_of_pending)
     */
    END_OF_PENDING,

    /** Token is inside the SQLScript body. (cos_sqlscript-sqlscript) */
    SQLSCRIPT;

    /**
     * Returns {@code true} when this value represents a token inside the actual
     * SQLScript body (i.e. {@link #SQLSCRIPT}).
     */
    public boolean isSqlscript() {
        return this == SQLSCRIPT;
    }
}
