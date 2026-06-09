package com.github.fmabap.amdpprettyprinter.prettyprinter;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Unit tests for the {@link Sqlscript} enum.
 */
public class SqlscriptTest {

    @Test
    public void isSqlscript_sqlscriptValue_returnsTrue() {
        assertTrue(Sqlscript.SQLSCRIPT.isSqlscript());
    }

    @Test
    public void isSqlscript_noneValue_returnsFalse() {
        assertFalse(Sqlscript.NONE.isSqlscript());
    }

    @Test
    public void isSqlscript_pendingValue_returnsFalse() {
        assertFalse(Sqlscript.PENDING.isSqlscript());
    }

    @Test
    public void isSqlscript_endOfPendingValue_returnsFalse() {
        assertFalse(Sqlscript.END_OF_PENDING.isSqlscript());
    }

    @Test
    public void enumValues_containsAllFour() {
        Sqlscript[] values = Sqlscript.values();
        assertEquals(4, values.length);
    }

    @Test
    public void valueOf_none_returnNone() {
        assertSame(Sqlscript.NONE, Sqlscript.valueOf("NONE"));
    }
}
