package com.github.fmabap.amdpprettyprinter.prettyprinter;

/**
 * Comment detail of a token in the source stream.
 * Converted from ABAP class ZCL_APP_SCANNER_COMMENT (cos_comment_detail
 * constants).
 *
 * <pre>
 * ABAP constant                                    → enum value
 * cos_comment_detail-none                          → NONE
 * cos_comment_detail-start                         → START
 * cos_comment_detail-part                          → PART
 * cos_comment_detail-start_begin_of_line           → START_BEGIN_OF_LINE
 * cos_comment_detail-start_begin_of_line_indentabl → START_BEGIN_OF_LINE_INDENTABLE
 * </pre>
 */
public enum CommentDetail {

    /** Token is not a comment or no detail applies (cos_comment_detail-none). */
    NONE,

    /**
     * First token of a comment that starts mid-line (after other tokens on the
     * same row). (cos_comment_detail-start)
     */
    START,

    /**
     * A continuation line of a multi-line comment — not the opening token.
     * (cos_comment_detail-part)
     */
    PART,

    /**
     * First token of a comment that begins at column 0 of its source line.
     * (cos_comment_detail-start_begin_of_line)
     */
    START_BEGIN_OF_LINE,

    /**
     * First token of a comment that begins at the start of a line but may be
     * re-indented by the pretty-printer.
     * (cos_comment_detail-start_begin_of_line_indentabl)
     */
    START_BEGIN_OF_LINE_INDENTABLE;
}
