package com.github.fmabap.amdpprettyprinter.prettyprinter;

import java.util.ArrayList;
import java.util.List;

/**
 * Extended token data structure.
 * Converted from ABAP structure ZAPP_S_STOKESX_EXT.
 */
public class TokensExt {

    /** Token text (formatted/processed) */
    public String str = "";

    /** Row of the token in the source */
    public int row = 0;

    /** Column of the token in the source */
    public int col = 0;

    /** Length of the token */
    public int len = 0;

    /** Token type */
    public String type = "";

    /** Original table row index */
    public int orgTabRow = 0;

    /** SQLScript context of this token. */
    public Sqlscript sqlscript = Sqlscript.NONE;

    /** Comment type of this token. */
    public Comment comment = Comment.NONE;

    /** Comment detail of this token. */
    public CommentDetail commentDetail = CommentDetail.NONE;

    /** Delimiter(s) following the token (formatted) */
    public List<String> delimiter = new ArrayList<>();

    /** Delimiter(s) following the token (original) */
    public List<String> delimiterOrg = new ArrayList<>();

    /** Token text in upper case */
    public String strUp = "";

    /** Original token text */
    public String strOrg = "";

    /**
     * True when this token was identified as a SQL keyword by the keyword scanner.
     */
    public boolean isKeyword = false;

    @Override
    public String toString() {
        return String.format(
                "row=%d col=%d len=%d type=%-2s sql=%-8s cmt=%-8s cmt_det=%-12s kw=%-5s str=%s",
                row, col, len, type,
                sqlscript.name(), comment.name(), commentDetail.name(),
                isKeyword,
                strOrg);
    }
}
