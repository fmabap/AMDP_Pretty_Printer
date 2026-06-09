package com.github.fmabap.amdpprettyprinter.prettyprinter;

/**
 * Comment type of a token in the source stream.
 * Converted from ABAP class ZCL_APP_SCANNER_COMMENT (cos_comment constants).
 *
 * <pre>
 * ABAP constant                  → enum value
 * cos_comment-none               → NONE
 * cos_comment-multi_line         → MULTI_LINE
 * cos_comment-single_line        → SINGLE_LINE
 * </pre>
 */
public enum Comment {

    /** Token is not a comment (cos_comment-none). */
    NONE,

    /**
     * Token is part of a multi-line comment, e.g. {@code /* ... * /}
     * (cos_comment-multi_line).
     */
    MULTI_LINE,

    /**
     * Token is a single-line comment, e.g. {@code *}, {@code "}, or {@code --}
     * (cos_comment-single_line).
     */
    SINGLE_LINE;

    /** Returns {@code true} when this value represents any kind of comment. */
    public boolean isComment() {
        return this != NONE;
    }
}
