package com.github.fmabap.amdpprettyprinter.prettyprinter;

import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.RuleData;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;

/**
 * Unit tests for {@link AppUtilities}.
 */
public class AppUtilitiesTest {

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    /** Builds a TokensExt at a given (row, col). */
    private static TokensExt token(int row, int col) {
        TokensExt t = new TokensExt();
        t.row = row;
        t.col = col;
        return t;
    }

    /**
     * Minimal IRule stub exposing just the fields needed for AppUtilities tests.
     * Stub delegates to an inner TokensExt and comment flag.
     */
    private static IRule stubRule(Sqlscript sqlscript, boolean comment) {
        TokensExt te = new TokensExt();
        te.sqlscript = sqlscript;
        te.comment = comment ? Comment.SINGLE_LINE : Comment.NONE;

        return new IRule() {
            @Override
            public TokensExt getTokenExt() {
                return te;
            }

            @Override
            public boolean isComment() {
                return comment;
            }

            @Override
            public String getTokenUp() {
                return "TOKEN";
            }

            // ---------- all remaining interface methods are stubs ----------
            @Override
            public IRule getNewContextRule() {
                return null;
            }

            @Override
            public IRule getNewHlContextRule() {
                return null;
            }

            @Override
            public IRule getContextRule() {
                return null;
            }

            @Override
            public IRule getHlContextRule() {
                return null;
            }

            @Override
            public String getNewContext() {
                return "";
            }

            @Override
            public String getNewHlContext() {
                return "";
            }

            @Override
            public IRule getPrevRule() {
                return null;
            }

            @Override
            public IRule getNextRule() {
                return null;
            }

            @Override
            public void setNextRule(IRule r) {
            }

            @Override
            public void init(TokensExt t, List<String> s, RuleData rd, ISettings set,
                    IRule c, IRule h, IRule p) {
            }

            @Override
            public void finalizeInit() {
            }

            @Override
            public void validate() {
            }

            @Override
            public int getCurRow() {
                return 0;
            }

            @Override
            public void setCurRow(int r) {
            }

            @Override
            public int getEndRow() {
                return 0;
            }

            @Override
            public int getCurOffsetStart() {
                return 0;
            }

            @Override
            public void setCurOffsetStart(int o) {
            }

            @Override
            public int getCurOffsetEnd() {
                return 0;
            }

            @Override
            public int getNewLineIndent() {
                return 0;
            }

            @Override
            public int getNewStatementIndent() {
                return 0;
            }

            @Override
            public void setAdditionalIndent(int i) {
            }

            @Override
            public int getAdditionalIndent() {
                return 0;
            }

            @Override
            public List<String> getText() {
                return Collections.emptyList();
            }

            @Override
            public RuleData getRuleData() {
                return new RuleData();
            }

            @Override
            public boolean isNewLineReq() {
                return false;
            }

            @Override
            public boolean isLineBreakingToken() {
                return false;
            }

            @Override
            public boolean isLbTokenRespDelimiter() {
                return false;
            }

            @Override
            public boolean isEndOfStatement() {
                return false;
            }

            @Override
            public boolean hasMultlineDelimiter() {
                return false;
            }

            @Override
            public void setAvoidLbAfterThisToken(boolean a) {
            }

            @Override
            public void refreshBuffer() {
            }
        };
    }

    // -----------------------------------------------------------------------
    // getPrevTokenExt
    // -----------------------------------------------------------------------

    @Test
    public void getPrevTokenExt_firstToken_returnsNull() {
        TokensExt t1 = token(1, 0);
        TokensExt t2 = token(1, 4);
        List<TokensExt> list = Arrays.asList(t1, t2);

        assertNull(AppUtilities.getPrevTokenExt(list, t1));
    }

    @Test
    public void getPrevTokenExt_middleToken_returnsPrev() {
        TokensExt t1 = token(1, 0);
        TokensExt t2 = token(1, 4);
        TokensExt t3 = token(1, 8);
        List<TokensExt> list = Arrays.asList(t1, t2, t3);

        assertSame(t1, AppUtilities.getPrevTokenExt(list, t2));
    }

    @Test
    public void getPrevTokenExt_tokenNotFound_returnsNull() {
        TokensExt t1 = token(1, 0);
        TokensExt tOther = token(99, 99);
        List<TokensExt> list = Collections.singletonList(t1);

        assertNull(AppUtilities.getPrevTokenExt(list, tOther));
    }

    // -----------------------------------------------------------------------
    // getNextTokenExt
    // -----------------------------------------------------------------------

    @Test
    public void getNextTokenExt_lastToken_returnsNull() {
        TokensExt t1 = token(1, 0);
        TokensExt t2 = token(1, 4);
        List<TokensExt> list = Arrays.asList(t1, t2);

        assertNull(AppUtilities.getNextTokenExt(list, t2));
    }

    @Test
    public void getNextTokenExt_firstToken_returnsNext() {
        TokensExt t1 = token(1, 0);
        TokensExt t2 = token(1, 4);
        List<TokensExt> list = Arrays.asList(t1, t2);

        assertSame(t2, AppUtilities.getNextTokenExt(list, t1));
    }

    @Test
    public void getNextTokenExt_tokenNotFound_returnsNull() {
        TokensExt t1 = token(1, 0);
        TokensExt tOther = token(99, 99);
        List<TokensExt> list = Collections.singletonList(t1);

        assertNull(AppUtilities.getNextTokenExt(list, tOther));
    }

    // -----------------------------------------------------------------------
    // isAbapToken
    // -----------------------------------------------------------------------

    @Test
    public void isAbapToken_none_returnsTrue() {
        assertTrue(AppUtilities.isAbapToken(Sqlscript.NONE));
    }

    @Test
    public void isAbapToken_pending_returnsTrue() {
        assertTrue(AppUtilities.isAbapToken(Sqlscript.PENDING));
    }

    @Test
    public void isAbapToken_endOfPending_returnsTrue() {
        assertTrue(AppUtilities.isAbapToken(Sqlscript.END_OF_PENDING));
    }

    @Test
    public void isAbapToken_sqlscript_returnsFalse() {
        assertFalse(AppUtilities.isAbapToken(Sqlscript.SQLSCRIPT));
    }

    // -----------------------------------------------------------------------
    // isSqlscriptToken
    // -----------------------------------------------------------------------

    @Test
    public void isSqlscriptToken_sqlscript_returnsTrue() {
        assertTrue(AppUtilities.isSqlscriptToken(Sqlscript.SQLSCRIPT));
    }

    @Test
    public void isSqlscriptToken_none_returnsFalse() {
        assertFalse(AppUtilities.isSqlscriptToken(Sqlscript.NONE));
    }

    @Test
    public void isSqlscriptToken_pending_returnsFalse() {
        assertFalse(AppUtilities.isSqlscriptToken(Sqlscript.PENDING));
    }

    // -----------------------------------------------------------------------
    // isSqlscriptRule / isAbapRule
    // -----------------------------------------------------------------------

    @Test
    public void isSqlscriptRule_nullRule_returnsFalse() throws AppException {
        assertFalse(AppUtilities.isSqlscriptRule(null));
    }

    @Test
    public void isSqlscriptRule_sqlscriptRule_returnsTrue() throws AppException {
        assertTrue(AppUtilities.isSqlscriptRule(stubRule(Sqlscript.SQLSCRIPT, false)));
    }

    @Test
    public void isSqlscriptRule_abapRule_returnsFalse() throws AppException {
        assertFalse(AppUtilities.isSqlscriptRule(stubRule(Sqlscript.NONE, false)));
    }

    @Test
    public void isAbapRule_nullRule_returnsFalse() throws AppException {
        assertFalse(AppUtilities.isAbapRule(null));
    }

    @Test
    public void isAbapRule_abapRule_returnsTrue() throws AppException {
        assertTrue(AppUtilities.isAbapRule(stubRule(Sqlscript.NONE, false)));
    }

    @Test
    public void isAbapRule_sqlscriptRule_returnsFalse() throws AppException {
        assertFalse(AppUtilities.isAbapRule(stubRule(Sqlscript.SQLSCRIPT, false)));
    }

    // -----------------------------------------------------------------------
    // isComment / isCommentRule
    // -----------------------------------------------------------------------

    @Test
    public void isComment_none_returnsFalse() {
        assertFalse(AppUtilities.isComment(Comment.NONE));
    }

    @Test
    public void isComment_singleLine_returnsTrue() {
        assertTrue(AppUtilities.isComment(Comment.SINGLE_LINE));
    }

    @Test
    public void isComment_multiLine_returnsTrue() {
        assertTrue(AppUtilities.isComment(Comment.MULTI_LINE));
    }

    @Test
    public void isCommentRule_nullRule_returnsFalse() {
        assertFalse(AppUtilities.isCommentRule(null));
    }

    @Test
    public void isCommentRule_commentRule_returnsTrue() {
        assertTrue(AppUtilities.isCommentRule(stubRule(Sqlscript.NONE, true)));
    }

    @Test
    public void isCommentRule_nonCommentRule_returnsFalse() {
        assertFalse(AppUtilities.isCommentRule(stubRule(Sqlscript.NONE, false)));
    }

    // -----------------------------------------------------------------------
    // containsDelimiterChar
    // -----------------------------------------------------------------------

    @Test
    public void containsDelimiterChar_charPresent_returnsTrue() {
        List<String> delims = Arrays.asList(".", " ");
        assertTrue(AppUtilities.containsDelimiterChar(delims, '.'));
    }

    @Test
    public void containsDelimiterChar_charAbsent_returnsFalse() {
        List<String> delims = Arrays.asList(" ", "  ");
        assertFalse(AppUtilities.containsDelimiterChar(delims, '.'));
    }

    @Test
    public void containsDelimiterChar_emptyList_returnsFalse() {
        assertFalse(AppUtilities.containsDelimiterChar(Collections.emptyList(), '.'));
    }

    // -----------------------------------------------------------------------
    // containsDelimiterOnlySpace
    // -----------------------------------------------------------------------

    @Test
    public void containsDelimiterOnlySpace_allSpaces_returnsTrue() {
        List<String> delims = Arrays.asList(" ", "  ");
        assertTrue(AppUtilities.containsDelimiterOnlySpace(delims));
    }

    @Test
    public void containsDelimiterOnlySpace_hasNonSpace_returnsFalse() {
        List<String> delims = Arrays.asList(" ", ".");
        assertFalse(AppUtilities.containsDelimiterOnlySpace(delims));
    }

    @Test
    public void containsDelimiterOnlySpace_emptyStrings_returnsTrue() {
        // empty strings are skipped — empty list with empty entries is considered "only
        // space"
        List<String> delims = Arrays.asList("", "");
        assertTrue(AppUtilities.containsDelimiterOnlySpace(delims));
    }

    @Test
    public void containsDelimiterOnlySpace_emptyList_returnsTrue() {
        assertTrue(AppUtilities.containsDelimiterOnlySpace(Collections.emptyList()));
    }

    // -----------------------------------------------------------------------
    // isDelimiterInitial
    // -----------------------------------------------------------------------

    @Test
    public void isDelimiterInitial_allEmpty_returnsTrue() {
        List<String> delims = Arrays.asList("", "");
        assertTrue(AppUtilities.isDelimiterInitial(delims));
    }

    @Test
    public void isDelimiterInitial_hasNonEmpty_returnsFalse() {
        List<String> delims = Arrays.asList("", " ");
        assertFalse(AppUtilities.isDelimiterInitial(delims));
    }

    @Test
    public void isDelimiterInitial_emptyList_returnsTrue() {
        assertTrue(AppUtilities.isDelimiterInitial(Collections.emptyList()));
    }

    // -----------------------------------------------------------------------
    // getSpaceAsDelimiter
    // -----------------------------------------------------------------------

    @Test
    public void getSpaceAsDelimiter_returnsSingleSpaceEntry() {
        List<String> result = AppUtilities.getSpaceAsDelimiter();
        assertEquals(1, result.size());
        assertEquals(" ", result.get(0));
    }

    // -----------------------------------------------------------------------
    // isStrEqUpperCase
    // -----------------------------------------------------------------------

    @Test
    public void isStrEqUpperCase_sameCase_returnsTrue() {
        assertTrue(AppUtilities.isStrEqUpperCase("SELECT", "SELECT"));
    }

    @Test
    public void isStrEqUpperCase_differentCase_returnsTrue() {
        assertTrue(AppUtilities.isStrEqUpperCase("select", "SELECT"));
    }

    @Test
    public void isStrEqUpperCase_differentStrings_returnsFalse() {
        assertFalse(AppUtilities.isStrEqUpperCase("SELECT", "FROM"));
    }

    // -----------------------------------------------------------------------
    // setToZeroIfNegative
    // -----------------------------------------------------------------------

    @Test
    public void setToZeroIfNegative_negative_returnsZero() {
        assertEquals(0, AppUtilities.setToZeroIfNegative(-1));
    }

    @Test
    public void setToZeroIfNegative_zero_returnsZero() {
        assertEquals(0, AppUtilities.setToZeroIfNegative(0));
    }

    @Test
    public void setToZeroIfNegative_positive_returnsValue() {
        assertEquals(5, AppUtilities.setToZeroIfNegative(5));
    }

    // -----------------------------------------------------------------------
    // convSourceTabToString
    // -----------------------------------------------------------------------

    @Test
    public void convSourceTabToString_emptyList_returnsEmptyString() {
        assertEquals("", AppUtilities.convSourceTabToString(Collections.emptyList()));
    }

    @Test
    public void convSourceTabToString_singleLine_returnsLine() {
        assertEquals("SELECT 1", AppUtilities.convSourceTabToString(
                Collections.singletonList("SELECT 1")));
    }

    @Test
    public void convSourceTabToString_multipleLines_joinedWithCrLf() {
        List<String> lines = Arrays.asList("line1", "line2", "line3");
        assertEquals("line1\r\nline2\r\nline3", AppUtilities.convSourceTabToString(lines));
    }
}
