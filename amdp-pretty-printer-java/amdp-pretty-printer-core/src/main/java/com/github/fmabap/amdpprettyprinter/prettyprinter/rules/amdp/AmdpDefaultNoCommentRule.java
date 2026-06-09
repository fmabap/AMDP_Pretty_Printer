package com.github.fmabap.amdpprettyprinter.prettyprinter.rules.amdp;

import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * Base AMDP rule that tracks whether the rule's special logic is active.
 * Logic is inactive for comment tokens; comments have their rule-data fields
 * cleared so they do not interfere with indentation.
 *
 * <p>
 * Converted from ABAP class ZCL_APP_RULE_AMDP_DEFAULT_NO_C.
 * </p>
 */
public class AmdpDefaultNoCommentRule extends AmdpDefaultRule {

    // -----------------------------------------------------------------------
    // Protected / private fields
    // -----------------------------------------------------------------------

    /** Whether the rule's special logic is active (= token is not a comment). */
    protected boolean mvLogicActive;

    /** Guard: {@code true} once {@link #setLogicActive()} has been called. */
    private boolean mvLogicActiveSet;

    /**
     * Guard: {@code true} when {@link #setAdditionalIndent(int)} has been called
     * explicitly (from outside {@link #finalizeInit()}), so the default
     * initialisation in {@link #finalizeInit()} will not overwrite it.
     * Converted from ABAP: MV_ADD_INDENT_SET.
     */
    protected boolean mvAddIndentSet;

    // -----------------------------------------------------------------------
    // IRule - setAdditionalIndent override to track explicit calls
    // -----------------------------------------------------------------------

    /**
     * Stores {@code indent} and marks that the additional indent was explicitly
     * set, preventing {@link #finalizeInit()} from overwriting it with the
     * default value from {@code ruleData.addIndent}.
     * Converted from ABAP: ZIF_APP_RULE~SET_ADDITIONAL_INDENT (sets
     * MV_ADD_INDENT_SET).
     */
    @Override
    public void setAdditionalIndent(int indent) throws AppException {
        mvAddIndentSet = true;
        super.setAdditionalIndent(indent);
    }

    // -----------------------------------------------------------------------
    // IRule - finalizeInit
    // -----------------------------------------------------------------------

    /**
     * Calls the parent finalisation, activates or deactivates the logic flag,
     * then either copies {@code ruleData.addIndent} as the additional indent
     * (when logic is active and no explicit indent has been set) or clears the
     * line-break / indent fields (when the token is a comment).
     *
     * <p>
     * The additional indent is initialised without setting {@link #mvAddIndentSet}
     * so that a later explicit call to {@link #setAdditionalIndent(int)} can
     * still override it.
     * </p>
     * Converted from ABAP: ZIF_APP_RULE~FINALIZE_INIT.
     */
    @Override
    public void finalizeInit() throws AppException {
        super.finalizeInit();
        setLogicActive();

        if (isLogicActive()) {
            if (!mvAddIndentSet) {
                // Initialise from ruleData without setting mvAddIndentSet, so a
                // subsequent explicit setAdditionalIndent() call can still override.
                // Calling super bypasses our override which would set mvAddIndentSet.
                super.setAdditionalIndent(ruleData.addIndent);
            }
        } else {
            // Comment token: clear rule-data fields to avoid indentation issues.
            ruleData.isNewLineReq = false;
            ruleData.newLineIndentDiff = 0;
            ruleData.newStatementIndentDiff = 0;
            ruleData.addIndent = 0;
        }
    }

    // -----------------------------------------------------------------------
    // Protected helpers
    // -----------------------------------------------------------------------

    /**
     * Sets {@link #mvLogicActive} to {@code true} when the current token is not
     * a comment, and {@code false} otherwise.
     * Converted from ABAP: SET_LOGIC_ACTIVE.
     */
    protected void setLogicActive() throws AppException {
        mvLogicActive = !isComment();
        mvLogicActiveSet = true;
    }

    /**
     * Returns whether the rule's special logic is active.
     *
     * @throws AppException if {@link #setLogicActive()} has not been called yet
     *                      Converted from ABAP: IS_LOGIC_ACTIVE.
     */
    protected boolean isLogicActive() throws AppException {
        if (mvLogicActiveSet) {
            return mvLogicActive;
        }
        throw new AppException(
                "IS_LOGIC_ACTIVE called before SET_LOGIC_ACTIVE for rule: "
                        + ruleData.ruleName);
    }
}
