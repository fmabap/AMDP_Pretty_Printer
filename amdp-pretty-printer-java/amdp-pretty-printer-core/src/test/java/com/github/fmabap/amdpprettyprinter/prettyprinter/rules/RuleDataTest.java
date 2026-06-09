package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Unit tests for {@link RuleData}.
 */
public class RuleDataTest {

    @Test
    public void defaultConstructor_setsDefaultValues() {
        RuleData rd = new RuleData();

        assertEquals("", rd.ruleName);
        assertEquals("", rd.token);
        assertEquals("", rd.context);
        assertEquals("", rd.hlContext);
        assertFalse(rd.sqlscript);
        assertEquals("", rd.ruleClass);
        assertEquals(0, rd.addIndent);
        assertEquals("", rd.ruleCondClass);
        assertEquals(0, rd.newLineIndentDiff);
        assertEquals(0, rd.newStatementIndentDiff);
        assertFalse(rd.isNewLineReq);
    }

    @Test
    public void fieldAssignment_roundTrips() {
        RuleData rd = new RuleData();
        rd.ruleName = "AMDP_SELECT";
        rd.token = "SELECT";
        rd.context = "AMDP";
        rd.hlContext = "METHOD";
        rd.sqlscript = true;
        rd.ruleClass = "com.example.MyRule";
        rd.addIndent = 2;
        rd.ruleCondClass = "com.example.MyRuleCond";
        rd.newLineIndentDiff = 1;
        rd.newStatementIndentDiff = -1;
        rd.isNewLineReq = true;

        assertEquals("AMDP_SELECT", rd.ruleName);
        assertEquals("SELECT", rd.token);
        assertEquals("AMDP", rd.context);
        assertEquals("METHOD", rd.hlContext);
        assertTrue(rd.sqlscript);
        assertEquals("com.example.MyRule", rd.ruleClass);
        assertEquals(2, rd.addIndent);
        assertEquals("com.example.MyRuleCond", rd.ruleCondClass);
        assertEquals(1, rd.newLineIndentDiff);
        assertEquals(-1, rd.newStatementIndentDiff);
        assertTrue(rd.isNewLineReq);
    }
}
