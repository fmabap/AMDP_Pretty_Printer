package com.github.fmabap.amdpprettyprinter.prettyprinter;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Static utility methods for the AMDP Pretty Printer.
 * Converted from ABAP class ZCL_APP_UTILITIES.
 */
public final class AppUtilities {

    private AppUtilities() {
    }

    // -----------------------------------------------------------------------
    // Token navigation
    // -----------------------------------------------------------------------

    /**
     * Returns the previous token in the list relative to {@code token},
     * identified by matching row and col.
     * Returns {@code null} when {@code token} is the first entry or not found.
     * Converted from ABAP: GET_PREV_TOKEN_EXT.
     */
    public static TokensExt getPrevTokenExt(List<TokensExt> tokenExt, TokensExt token) {
        for (int i = 0; i < tokenExt.size(); i++) {
            TokensExt t = tokenExt.get(i);
            if (t.row == token.row && t.col == token.col) {
                if (i < 1) {
                    return null;
                }
                return tokenExt.get(i - 1);
            }
        }
        return null;
    }

    /**
     * Returns the next token in the list relative to {@code token},
     * identified by matching row and col.
     * Returns {@code null} when {@code token} is the last entry or not found.
     * Converted from ABAP: GET_NEXT_TOKEN_EXT.
     */
    public static TokensExt getNextTokenExt(List<TokensExt> tokenExt, TokensExt token) {
        for (int i = 0; i < tokenExt.size(); i++) {
            TokensExt t = tokenExt.get(i);
            if (t.row == token.row && t.col == token.col) {
                if (i + 1 >= tokenExt.size()) {
                    return null;
                }
                return tokenExt.get(i + 1);
            }
        }
        return null;
    }

    // -----------------------------------------------------------------------
    // Token / rule classification
    // -----------------------------------------------------------------------

    /**
     * Returns {@code true} when the sqlscript value represents a pure ABAP token
     * (none, pending, or end-of-pending).
     * Converted from ABAP: IS_ABAP_TOKEN.
     */
    public static boolean isAbapToken(Sqlscript sqlscript) {
        return sqlscript == Sqlscript.NONE
                || sqlscript == Sqlscript.PENDING
                || sqlscript == Sqlscript.END_OF_PENDING;
    }

    /**
     * Returns {@code true} when the sqlscript value represents a SQLScript token.
     * Converted from ABAP: IS_SQLSCRIPT_TOKEN.
     */
    public static boolean isSqlscriptToken(Sqlscript sqlscript) {
        return sqlscript == Sqlscript.SQLSCRIPT;
    }

    /**
     * Returns {@code true} when {@code rule} carries a SQLScript token.
     * Returns {@code false} when {@code rule} is {@code null}.
     * Converted from ABAP: IS_SQLSCRIPT_RULE.
     */
    public static boolean isSqlscriptRule(IRule rule) throws AppException {
        if (rule == null) {
            return false;
        }
        return isSqlscriptToken(rule.getTokenExt().sqlscript);
    }

    /**
     * Returns {@code true} when {@code rule} carries an ABAP token.
     * Returns {@code false} when {@code rule} is {@code null}.
     * Converted from ABAP: IS_ABAP_RULE.
     */
    public static boolean isAbapRule(IRule rule) throws AppException {
        if (rule == null) {
            return false;
        }
        return isAbapToken(rule.getTokenExt().sqlscript);
    }

    /**
     * Returns {@code true} when the comment value indicates this is a comment
     * token.
     * Converted from ABAP: IS_COMMENT.
     */
    public static boolean isComment(Comment comment) {
        return comment != Comment.NONE;
    }

    /**
     * Returns {@code true} when {@code rule} is a comment rule.
     * Returns {@code false} when {@code rule} is {@code null}.
     * Converted from ABAP: IS_COMMENT_RULE.
     */
    public static boolean isCommentRule(IRule rule) {
        if (rule == null) {
            return false;
        }
        return rule.isComment();
    }

    // -----------------------------------------------------------------------
    // Delimiter helpers
    // -----------------------------------------------------------------------

    /**
     * Returns {@code true} when any string in the delimiter list contains
     * {@code ch}.
     * Converted from ABAP: CONTAINS_DELIMITER_CHAR.
     */
    public static boolean containsDelimiterChar(List<String> delimiter, char ch) {
        for (String d : delimiter) {
            if (d.indexOf(ch) >= 0) {
                return true;
            }
        }
        return false;
    }

    /**
     * Returns {@code true} when every non-empty string in the delimiter list
     * consists solely of spaces.
     * Converted from ABAP: CONTAINS_DELIMITER_ONLY_SPACE.
     */
    public static boolean containsDelimiterOnlySpace(List<String> delimiter) {
        for (String d : delimiter) {
            if (!d.isEmpty() && !d.chars().allMatch(c -> c == ' ')) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns {@code true} when every string in the delimiter list is empty.
     * Converted from ABAP: IS_DELIMITER_INITIAL.
     */
    public static boolean isDelimiterInitial(List<String> delimiter) {
        for (String d : delimiter) {
            if (!d.isEmpty()) {
                return false;
            }
        }
        return true;
    }

    /**
     * Returns a delimiter list containing a single space entry.
     * Converted from ABAP: GET_SPACE_AS_DELIMITER.
     */
    public static List<String> getSpaceAsDelimiter() {
        List<String> result = new ArrayList<>();
        result.add(" ");
        return result;
    }

    // -----------------------------------------------------------------------
    // Rule helpers
    // -----------------------------------------------------------------------

    /**
     * Returns the length of the upper-case token text of the given rule,
     * excluding any trailing delimiter.
     * Converted from ABAP: GET_TOKEN_LENGTH_WO_DELIMITER.
     */
    public static int getTokenLengthWoDelimiter(IRule rule) {
        return rule.getTokenUp().length();
    }

    // -----------------------------------------------------------------------
    // String helpers
    // -----------------------------------------------------------------------

    /**
     * Returns {@code true} when both strings are equal when compared
     * case-insensitively (both converted to upper case before comparison).
     * Converted from ABAP: IS_STR_EQ_UPPER_CASE.
     */
    public static boolean isStrEqUpperCase(String s1, String s2) {
        return s1.toUpperCase().equals(s2.toUpperCase());
    }

    /**
     * Returns 0 when {@code value} is negative, otherwise returns {@code value}
     * unchanged.
     * Converted from ABAP: SET_TO_0_IF_NEGATIV (CHANGING parameter → return value).
     */
    public static int setToZeroIfNegative(int value) {
        return value < 0 ? 0 : value;
    }

    /**
     * Joins the source lines into a single string separated by {@code \r\n}.
     * Converted from ABAP: CONV_SOURCE_TAB_TO_STRING.
     */
    public static String convSourceTabToString(List<String> source) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < source.size(); i++) {
            if (i > 0) {
                sb.append("\r\n");
            }
            sb.append(source.get(i));
        }
        return sb.toString();
    }
}
