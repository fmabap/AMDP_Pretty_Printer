package com.github.fmabap.amdpprettyprinter.prettyprinter;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Unit tests for {@link TokensExt}.
 */
public class TokensExtTest {

    @Test
    public void defaultConstructor_setsDefaultValues() {
        TokensExt t = new TokensExt();

        assertEquals("", t.str);
        assertEquals(0, t.row);
        assertEquals(0, t.col);
        assertEquals(0, t.len);
        assertEquals("", t.type);
        assertEquals(0, t.orgTabRow);
        assertSame(Sqlscript.NONE, t.sqlscript);
        assertSame(Comment.NONE, t.comment);
        assertSame(CommentDetail.NONE, t.commentDetail);
        assertNotNull(t.delimiter);
        assertTrue(t.delimiter.isEmpty());
        assertNotNull(t.delimiterOrg);
        assertTrue(t.delimiterOrg.isEmpty());
        assertEquals("", t.strUp);
        assertEquals("", t.strOrg);
        assertFalse(t.isKeyword);
    }

    @Test
    public void toString_containsRowAndCol() {
        TokensExt t = new TokensExt();
        t.row = 3;
        t.col = 7;
        t.strOrg = "SELECT";

        String s = t.toString();
        assertTrue(s.contains("row=3"));
        assertTrue(s.contains("col=7"));
        assertTrue(s.contains("SELECT"));
    }

    @Test
    public void toString_containsSqlscriptName() {
        TokensExt t = new TokensExt();
        t.sqlscript = Sqlscript.SQLSCRIPT;

        assertTrue(t.toString().contains("SQLSCRIPT"));
    }

    @Test
    public void toString_containsCommentName() {
        TokensExt t = new TokensExt();
        t.comment = Comment.SINGLE_LINE;

        assertTrue(t.toString().contains("SINGLE_LINE"));
    }

    @Test
    public void fieldAssignment_roundTrips() {
        TokensExt t = new TokensExt();
        t.str = "FROM";
        t.strOrg = "from";
        t.strUp = "FROM";
        t.row = 2;
        t.col = 10;
        t.len = 4;
        t.type = "I";
        t.isKeyword = true;

        assertEquals("FROM", t.str);
        assertEquals("from", t.strOrg);
        assertEquals("FROM", t.strUp);
        assertEquals(2, t.row);
        assertEquals(10, t.col);
        assertEquals(4, t.len);
        assertEquals("I", t.type);
        assertTrue(t.isKeyword);
    }
}
