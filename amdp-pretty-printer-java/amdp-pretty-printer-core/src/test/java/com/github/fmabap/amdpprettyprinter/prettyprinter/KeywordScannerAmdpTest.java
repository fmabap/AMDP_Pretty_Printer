package com.github.fmabap.amdpprettyprinter.prettyprinter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

/**
 * Unit tests for {@link KeywordScannerAmdp#scanKeyword} and
 * {@link KeywordScannerAmdp#isKeyword}.
 *
 * <p>
 * The core scenarios under test are the four layouts in which the two-character
 * sequence {@code ."} can follow a token, signalling that the token is used as
 * a
 * schema/table qualifier and must therefore be lower-cased even when it matches
 * a
 * SQL keyword:
 * </p>
 * <ol>
 * <li>The delimiter of the current token starts with {@code ."}</li>
 * <li>The delimiter is {@code .} and {@code next1} starts with {@code "}</li>
 * <li>The delimiter is empty, {@code next1} is the bare {@code .} token,
 * and {@code next1}'s delimiter starts with {@code "}</li>
 * <li>The delimiter is empty, {@code next1} is the bare {@code .} token
 * (also empty delimiter), and {@code next2} starts with {@code "}</li>
 * </ol>
 */
public class KeywordScannerAmdpTest {

    private KeywordScannerAmdp scanner;

    @Before
    public void setUp() {
        scanner = new KeywordScannerAmdp();
    }

    // -----------------------------------------------------------------------
    // Helpers
    // -----------------------------------------------------------------------

    /** Creates a minimal SQLScript token with no delimiter. */
    private static TokensExt sqlToken(String str) {
        TokensExt t = new TokensExt();
        t.str = str;
        t.strUp = str.toUpperCase();
        t.sqlscript = Sqlscript.SQLSCRIPT;
        t.comment = Comment.NONE;
        return t;
    }

    /**
     * Creates a minimal SQLScript token whose first delimiter entry is
     * {@code delim}.
     */
    private static TokensExt sqlToken(String str, String delim) {
        TokensExt t = sqlToken(str);
        t.delimiter = Arrays.asList(delim);
        return t;
    }

    /**
     * Runs the scanner on the first token of the given list (index 0).
     * Additional tokens in the list serve as look-ahead.
     */
    private void scan(List<TokensExt> tokens) throws AppException {
        scanner.scanKeyword(tokens, 0);
    }

    /** Convenience: scan a single token with no look-ahead. */
    private void scan(TokensExt t) throws AppException {
        scan(Collections.singletonList(t));
    }

    /** Convenience: scan the first token with one look-ahead token. */
    private void scan(TokensExt t, TokensExt next1) throws AppException {
        scan(Arrays.asList(t, next1));
    }

    /** Convenience: scan the first token with two look-ahead tokens. */
    private void scan(TokensExt t, TokensExt next1, TokensExt next2) throws AppException {
        scan(Arrays.asList(t, next1, next2));
    }

    /**
     * Scans the token at position 1 (i.e. {@code subject}), with {@code prev} at
     * position 0, so the scanner sees a preceding token.
     */
    private void scanWithPrev(TokensExt prev, TokensExt subject) throws AppException {
        scanner.scanKeyword(Arrays.asList(prev, subject), 1);
    }

    // -----------------------------------------------------------------------
    // isKeyword
    // -----------------------------------------------------------------------

    @Test
    public void isKeyword_knownKeyword_returnsTrue() {
        assertTrue(scanner.isKeyword("SELECT"));
        assertTrue(scanner.isKeyword("FROM"));
        assertTrue(scanner.isKeyword("WHERE"));
        assertTrue(scanner.isKeyword("SCHEMA"));
    }

    @Test
    public void isKeyword_unknownIdentifier_returnsFalse() {
        assertFalse(scanner.isKeyword("MY_TABLE"));
        assertFalse(scanner.isKeyword("LT_RESULT"));
    }

    // -----------------------------------------------------------------------
    // scanKeyword – tokens that are skipped
    // -----------------------------------------------------------------------

    @Test
    public void scanKeyword_abapToken_notTouched() throws AppException {
        TokensExt t = sqlToken("select");
        t.sqlscript = Sqlscript.NONE; // not a SQLScript token
        scan(t);
        assertEquals("select", t.str); // unchanged
    }

    @Test
    public void scanKeyword_commentToken_notTouched() throws AppException {
        TokensExt t = sqlToken("SELECT");
        t.comment = Comment.SINGLE_LINE;
        scan(t);
        assertEquals("SELECT", t.str); // unchanged
    }

    @Test
    public void scanKeyword_stringLiteral_notTouched() throws AppException {
        TokensExt t = sqlToken("'hello'");
        scan(t);
        assertEquals("'hello'", t.str); // unchanged
    }

    @Test
    public void scanKeyword_quotedIdentifier_notTouched() throws AppException {
        TokensExt t = sqlToken("\"MY_COL\"");
        scan(t);
        assertEquals("\"MY_COL\"", t.str); // unchanged
    }

    // -----------------------------------------------------------------------
    // scanKeyword – normal keyword / identifier handling
    // -----------------------------------------------------------------------

    @Test
    public void scanKeyword_recognisedKeyword_staysUpperCase() throws AppException {
        TokensExt t = sqlToken("SELECT", " ");
        scan(t);
        assertEquals("SELECT", t.str);
    }

    @Test
    public void scanKeyword_unknownIdentifier_convertedToLowerCase() throws AppException {
        TokensExt t = sqlToken("MY_TABLE", " ");
        scan(t);
        assertEquals("my_table", t.str);
    }

    // -----------------------------------------------------------------------
    // scanKeyword – Case 1: delimiter starts with ."
    // -----------------------------------------------------------------------

    @Test
    public void scanKeyword_case1_delimiterStartsWithDotQuote_keywordLowered() throws AppException {
        // e.g. SCHEMA."MyCol" — delimiter of SCHEMA token is .\"MyCol\"
        TokensExt t = sqlToken("SCHEMA", ".\"MyCol\"");
        scan(t);
        assertEquals("schema", t.str);
    }

    @Test
    public void scanKeyword_case1_delimiterStartsWithDotQuote_nonKeywordLowered() throws AppException {
        // Non-keyword qualifier: my_schema."col"
        TokensExt t = sqlToken("MY_SCHEMA", ".\"col\"");
        scan(t);
        assertEquals("my_schema", t.str);
    }

    // -----------------------------------------------------------------------
    // scanKeyword – Case 2: delimiter is "." and next1 starts with "
    // -----------------------------------------------------------------------

    @Test
    public void scanKeyword_case2_delimDotAndNext1StartsQuote_keywordLowered() throws AppException {
        // Token: SCHEMA — delimiter is "."
        // next1: "MyCol"
        TokensExt t = sqlToken("SCHEMA", ".");
        TokensExt next1 = sqlToken("\"MyCol\"");
        scan(t, next1);
        assertEquals("schema", t.str);
    }

    @Test
    public void scanKeyword_case2_delimDotButNext1DoesNotStartWithQuote_keywordKept() throws AppException {
        // Token: SCHEMA — delimiter is "."
        // next1: col (no quote) => not a qualifier => keyword stays upper
        TokensExt t = sqlToken("SCHEMA", ".");
        TokensExt next1 = sqlToken("col");
        scan(t, next1);
        assertEquals("SCHEMA", t.str);
    }

    // -----------------------------------------------------------------------
    // scanKeyword – Case 3: delimiter empty, next1 is ".", next1's delim starts
    // with "
    // -----------------------------------------------------------------------

    @Test
    public void scanKeyword_case3_emptyDelim_next1Dot_next1DelimQuote_keywordLowered() throws AppException {
        // Token: SCHEMA — delimiter is ""
        // next1: . — delimiter is "\"MyCol\""
        TokensExt t = sqlToken("SCHEMA", "");
        TokensExt next1 = sqlToken(".", "\"MyCol\"");
        scan(t, next1);
        assertEquals("schema", t.str);
    }

    // -----------------------------------------------------------------------
    // scanKeyword – Case 4: delimiter empty, next1 is "." (empty delim), next2
    // starts with "
    // -----------------------------------------------------------------------

    @Test
    public void scanKeyword_case4_emptyDelim_next1Dot_emptyDelim_next2Quote_keywordLowered() throws AppException {
        // Token: SCHEMA — delimiter is ""
        // next1: . — delimiter is ""
        // next2: "MyCol"
        TokensExt t = sqlToken("SCHEMA", "");
        TokensExt next1 = sqlToken(".", "");
        TokensExt next2 = sqlToken("\"MyCol\"");
        scan(t, next1, next2);
        assertEquals("schema", t.str);
    }

    @Test
    public void scanKeyword_case4_emptyDelim_next1NotDot_keywordKept() throws AppException {
        // next1 is not "." so none of the dot-quote cases apply
        TokensExt t = sqlToken("SCHEMA", "");
        TokensExt next1 = sqlToken("col");
        TokensExt next2 = sqlToken("\"MyCol\"");
        scan(t, next1, next2);
        assertEquals("SCHEMA", t.str);
    }

    // -----------------------------------------------------------------------
    // scanKeyword – edge cases
    // -----------------------------------------------------------------------

    @Test
    public void scanKeyword_singleTokenList_noNullPointer() throws AppException {
        TokensExt t = sqlToken("SELECT", "");
        scan(t);
        assertEquals("SELECT", t.str);
    }

    @Test
    public void scanKeyword_emptyDelimAndNext1DotButNoNext2_keywordKept() throws AppException {
        // Dot-token exists but no next2, so Case 4 cannot match
        TokensExt t = sqlToken("SCHEMA", "");
        TokensExt next1 = sqlToken(".", "");
        scan(t, next1);
        assertEquals("SCHEMA", t.str);
    }

    // -----------------------------------------------------------------------
    // scanKeyword – preceded by table-context keyword
    // -----------------------------------------------------------------------

    @Test
    public void scanKeyword_precededByFrom_keywordLowered() throws AppException {
        // SELECT * FROM TABLE — TABLE after FROM is an object name
        TokensExt prev = sqlToken("FROM", " ");
        TokensExt t = sqlToken("TABLE", " ");
        scanWithPrev(prev, t);
        assertEquals("table", t.str);
    }

    @Test
    public void scanKeyword_precededByAs_keywordLowered() throws AppException {
        TokensExt prev = sqlToken("AS", " ");
        TokensExt t = sqlToken("GROUP", " "); // GROUP is a keyword
        scanWithPrev(prev, t);
        assertEquals("group", t.str);
    }

    @Test
    public void scanKeyword_precededByUpdate_keywordLowered() throws AppException {
        TokensExt prev = sqlToken("UPDATE", " ");
        TokensExt t = sqlToken("TABLE", " ");
        scanWithPrev(prev, t);
        assertEquals("table", t.str);
    }

    @Test
    public void scanKeyword_precededByInsert_keywordLowered() throws AppException {
        TokensExt prev = sqlToken("INSERT", " ");
        TokensExt t = sqlToken("INTO", " ");
        scanWithPrev(prev, t);
        assertEquals("INTO", t.str);
    }

    @Test
    public void scanKeyword_precededByDelete_keywordUppered() throws AppException {
        TokensExt prev = sqlToken("DELETE", " ");
        TokensExt t = sqlToken("from", " ");
        scanWithPrev(prev, t);
        assertEquals("FROM", t.str);
    }

    @Test
    public void scanKeyword_precededByUpsert_keywordLowered() throws AppException {
        TokensExt prev = sqlToken("UPSERT", " ");
        TokensExt t = sqlToken("TABLE", " ");
        scanWithPrev(prev, t);
        assertEquals("table", t.str);
    }

    @Test
    public void scanKeyword_precededByOf_keywordLowered() throws AppException {
        TokensExt prev = sqlToken("OF", " ");
        TokensExt t = sqlToken("TABLE", " ");
        scanWithPrev(prev, t);
        assertEquals("table", t.str);
    }

    @Test
    public void scanKeyword_precededByNonTableContextKeyword_keywordKept() throws AppException {
        // WHERE is not a table-context keyword → SELECT after WHERE stays upper
        TokensExt prev = sqlToken("WHERE", " ");
        TokensExt t = sqlToken("SELECT", " ");
        scanWithPrev(prev, t);
        assertEquals("SELECT", t.str);
    }

    @Test
    public void scanKeyword_precededByFromButTokenIsNotKeyword_lowered() throws AppException {
        // Non-keyword after FROM also stays lowercase (already would be lowercase)
        TokensExt prev = sqlToken("FROM", " ");
        TokensExt t = sqlToken("MY_TABLE", " ");
        scanWithPrev(prev, t);
        assertEquals("my_table", t.str);
    }

    @Test
    public void scanKeyword_prevIsAbapToken_keywordKept() throws AppException {
        // If the preceding token is an ABAP token (not SQLScript), it must not trigger
        // lowercasing
        TokensExt prev = sqlToken("FROM", " ");
        prev.sqlscript = Sqlscript.NONE;
        TokensExt t = sqlToken("TABLE", " ");
        scanWithPrev(prev, t);
        assertEquals("TABLE", t.str);
    }
}
