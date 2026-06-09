package com.github.fmabap.amdpprettyprinter.prettyprinter;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Unit tests for the {@link CommentDetail} enum.
 */
public class CommentDetailTest {

    @Test
    public void enumValues_containsFive() {
        assertEquals(5, CommentDetail.values().length);
    }

    @Test
    public void valueOf_none_returnsNone() {
        assertSame(CommentDetail.NONE, CommentDetail.valueOf("NONE"));
    }

    @Test
    public void valueOf_start_returnsStart() {
        assertSame(CommentDetail.START, CommentDetail.valueOf("START"));
    }

    @Test
    public void valueOf_part_returnsPart() {
        assertSame(CommentDetail.PART, CommentDetail.valueOf("PART"));
    }

    @Test
    public void valueOf_startBeginOfLine_returnsCorrectValue() {
        assertSame(CommentDetail.START_BEGIN_OF_LINE,
                CommentDetail.valueOf("START_BEGIN_OF_LINE"));
    }

    @Test
    public void valueOf_startBeginOfLineIndentable_returnsCorrectValue() {
        assertSame(CommentDetail.START_BEGIN_OF_LINE_INDENTABLE,
                CommentDetail.valueOf("START_BEGIN_OF_LINE_INDENTABLE"));
    }

    @Test
    public void ordinal_none_isZero() {
        assertEquals(0, CommentDetail.NONE.ordinal());
    }
}
