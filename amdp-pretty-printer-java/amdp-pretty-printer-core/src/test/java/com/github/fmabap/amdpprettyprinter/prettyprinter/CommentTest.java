package com.github.fmabap.amdpprettyprinter.prettyprinter;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Unit tests for the {@link Comment} enum.
 */
public class CommentTest {

    @Test
    public void isComment_noneValue_returnsFalse() {
        assertFalse(Comment.NONE.isComment());
    }

    @Test
    public void isComment_singleLineValue_returnsTrue() {
        assertTrue(Comment.SINGLE_LINE.isComment());
    }

    @Test
    public void isComment_multiLineValue_returnsTrue() {
        assertTrue(Comment.MULTI_LINE.isComment());
    }

    @Test
    public void enumValues_containsThree() {
        assertEquals(3, Comment.values().length);
    }

    @Test
    public void valueOf_none_returnsNone() {
        assertSame(Comment.NONE, Comment.valueOf("NONE"));
    }

    @Test
    public void valueOf_singleLine_returnsSingleLine() {
        assertSame(Comment.SINGLE_LINE, Comment.valueOf("SINGLE_LINE"));
    }
}
