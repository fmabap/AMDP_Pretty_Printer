package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * Extends {@link AmdpNewLineRule} with left-aligned indentation: when the rule
 * is active and no explicit {@code addIndent} is configured in the rule data,
 * the additional indent is set to the negative length of the token text so that
 * the token aligns to the left of its natural position.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_NEW_LINE_LFT.
 * </p>
 */
public class AmdpNewLineLeftRule extends AmdpNewLineRule {

    // -----------------------------------------------------------------------
    // IRule - getCurOffsetStart
    // -----------------------------------------------------------------------

    /**
     * Calls {@link #setAddIndent()} to lazily initialise the additional indent
     * before delegating to the parent implementation.
     * Converted from ABAP: ZIF_APP_RULE~GET_CUR_OFFSET_START.
     */
    @Override
    public int getCurOffsetStart() throws AppException {
        if (curOffsetStartSet) {
            return curOffsetStart;
        }
        setAddIndent();
        return super.getCurOffsetStart();
    }

    // -----------------------------------------------------------------------
    // Protected helpers
    // -----------------------------------------------------------------------

    /**
     * Initialises the additional indent when the logic is active.
     * <ul>
     * <li>When {@code ruleData.addIndent == 0}: the additional indent is set to
     * the negative length of the first line of the token text, causing the
     * keyword to start at the column where the keyword itself begins
     * (left-aligned).</li>
     * <li>When {@code ruleData.addIndent != 0}: the configured value is used
     * directly.</li>
     * </ul>
     * Converted from ABAP: SET_ADD_INDENT.
     */
    protected void setAddIndent() throws AppException {
        if (!isLogicActive()) {
            return;
        }

        if (ruleData.addIndent == 0) {
            // Use the negative token length as the indent offset.
            java.util.List<String> text = getText();
            if (!text.isEmpty()) {
                setAdditionalIndent(text.get(0).length() * -1);
            }
        } else {
            setAdditionalIndent(ruleData.addIndent);
        }
    }
}
