package com.github.fmabap.amdpprettyprinter.prettyprinter;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Unit tests for {@link Settings}.
 */
public class SettingsTest {

    // -----------------------------------------------------------------------
    // Default constructor
    // -----------------------------------------------------------------------

    @Test
    public void defaultConstructor_usesDepOnClsBrSfKwRule() {
        Settings s = new Settings();
        assertTrue(s.isNoLbAtCoSFuDepSfuKw());
    }

    @Test
    public void defaultConstructor_lineBreakAfterCommaReq() {
        // Rule 4 (DEP_ON_CLS_BR_SF_AND_KEYWRD) is NOT "no line break",
        // so isLineBreakAfterCommaReq() must return true.
        Settings s = new Settings();
        assertTrue(s.isLineBreakAfterCommaReq());
    }

    @Test
    public void defaultConstructor_traceDisabled() {
        assertFalse(new Settings().isTrace());
    }

    // -----------------------------------------------------------------------
    // Rule: LB_ALWAYS ("0")
    // -----------------------------------------------------------------------

    @Test
    public void rule_lbAlways_isAlwaysLineBreak() {
        Settings s = new Settings(Settings.LB_ALWAYS);
        assertTrue(s.isAlwaysLineBreakAftComma());
    }

    @Test
    public void rule_lbAlways_lineBreakReq() {
        assertTrue(new Settings(Settings.LB_ALWAYS).isLineBreakAfterCommaReq());
    }

    @Test
    public void rule_lbAlways_otherFlagsOff() {
        Settings s = new Settings(Settings.LB_ALWAYS);
        assertFalse(s.isNoLbAtCoSFuDepSfu());
        assertFalse(s.isNoLbAtCoSFuDepCbrO());
        assertFalse(s.isNoLbAtCoSFuDepSfuKw());
    }

    // -----------------------------------------------------------------------
    // Rule: LB_NO_LINE_BREAK ("1")
    // -----------------------------------------------------------------------

    @Test
    public void rule_noLineBreak_lineBreakNotReq() {
        assertFalse(new Settings(Settings.LB_NO_LINE_BREAK).isLineBreakAfterCommaReq());
    }

    @Test
    public void rule_noLineBreak_alwaysFlagOff() {
        assertFalse(new Settings(Settings.LB_NO_LINE_BREAK).isAlwaysLineBreakAftComma());
    }

    // -----------------------------------------------------------------------
    // Rule: LB_DEP_ON_CLS_BR_ONLY ("2")
    // -----------------------------------------------------------------------

    @Test
    public void rule_depOnClsBrOnly_flagOn() {
        assertTrue(new Settings(Settings.LB_DEP_ON_CLS_BR_ONLY).isNoLbAtCoSFuDepCbrO());
    }

    @Test
    public void rule_depOnClsBrOnly_lineBreakReq() {
        assertTrue(new Settings(Settings.LB_DEP_ON_CLS_BR_ONLY).isLineBreakAfterCommaReq());
    }

    // -----------------------------------------------------------------------
    // Rule: LB_DEP_ON_CLS_BR_AND_SF ("3")
    // -----------------------------------------------------------------------

    @Test
    public void rule_depOnClsBrAndSf_flagOn() {
        assertTrue(new Settings(Settings.LB_DEP_ON_CLS_BR_AND_SF).isNoLbAtCoSFuDepSfu());
    }

    @Test
    public void rule_depOnClsBrAndSf_lineBreakReq() {
        assertTrue(new Settings(Settings.LB_DEP_ON_CLS_BR_AND_SF).isLineBreakAfterCommaReq());
    }

    // -----------------------------------------------------------------------
    // Rule: LB_DEP_ON_CLS_BR_SF_KW ("4")
    // -----------------------------------------------------------------------

    @Test
    public void rule_depOnClsBrSfKw_flagOn() {
        assertTrue(new Settings(Settings.LB_DEP_ON_CLS_BR_SF_KW).isNoLbAtCoSFuDepSfuKw());
    }

    @Test
    public void rule_depOnClsBrSfKw_lineBreakReq() {
        assertTrue(new Settings(Settings.LB_DEP_ON_CLS_BR_SF_KW).isLineBreakAfterCommaReq());
    }

    // -----------------------------------------------------------------------
    // Trace flag
    // -----------------------------------------------------------------------

    @Test
    public void traceEnabled_isTraceReturnsTrue() {
        assertTrue(new Settings(Settings.LB_ALWAYS, true).isTrace());
    }

    @Test
    public void traceDisabled_isTraceReturnsFalse() {
        assertFalse(new Settings(Settings.LB_ALWAYS, false).isTrace());
    }

    // -----------------------------------------------------------------------
    // Invalid rule value
    // -----------------------------------------------------------------------

    @Test(expected = IllegalArgumentException.class)
    public void unknownRule_throwsIllegalArgument() {
        new Settings("UNKNOWN_RULE");
    }

    @Test(expected = IllegalArgumentException.class)
    public void emptyRule_throwsIllegalArgument() {
        new Settings("");
    }
}
