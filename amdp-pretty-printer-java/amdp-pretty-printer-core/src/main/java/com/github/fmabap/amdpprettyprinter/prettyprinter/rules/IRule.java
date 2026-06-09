package com.github.fmabap.amdpprettyprinter.prettyprinter.rules;

import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.ISettings;
import com.github.fmabap.amdpprettyprinter.prettyprinter.TokensExt;
import com.github.fmabap.amdpprettyprinter.prettyprinter.AppException;

/**
 * Interface for a single pretty-printer rule/token.
 * Converted from ABAP interface ZIF_APP_RULE.
 *
 * Each token in the source stream is wrapped in one IRule instance.
 * The chain of rules is linked via prev/next references.
 */
public interface IRule {

    // -----------------------------------------------------------------------
    // Context management
    // -----------------------------------------------------------------------

    /**
     * Returns the new context rule after this token. Converted from:
     * GET_NEW_CONTEXT_RULE.
     */
    IRule getNewContextRule() throws AppException;

    /**
     * Returns the new higher-level context rule after this token. Converted from:
     * GET_NEW_HL_CONTEXT_RULE.
     */
    IRule getNewHlContextRule() throws AppException;

    /** Returns the current context rule. Converted from: GET_CONTEXT_RULE. */
    IRule getContextRule() throws AppException;

    /**
     * Returns the current higher-level context rule. Converted from:
     * GET_HL_CONTEXT_RULE.
     */
    IRule getHlContextRule() throws AppException;

    /**
     * Returns the context value after this token. Converted from: GET_NEW_CONTEXT.
     */
    String getNewContext() throws AppException;

    /**
     * Returns the higher-level context value after this token. Converted from:
     * GET_NEW_HL_CONTEXT.
     */
    String getNewHlContext() throws AppException;

    // -----------------------------------------------------------------------
    // Rule chain
    // -----------------------------------------------------------------------

    /** Returns the previous rule in the chain. Converted from: GET_PREV_RULE. */
    IRule getPrevRule();

    /** Returns the next rule in the chain. Converted from: GET_NEXT_RULE. */
    IRule getNextRule();

    /** Sets the next rule in the chain. Converted from: SET_NEXT_RULE. */
    void setNextRule(IRule nextRule) throws AppException;

    // -----------------------------------------------------------------------
    // Initialisation
    // -----------------------------------------------------------------------

    /**
     * Initialises this rule instance with token and context data.
     * Converted from ABAP: INIT.
     *
     * @param tokenExt      Extended token data
     * @param tSource       Full source text (list of lines)
     * @param ruleData      Configuration data for this rule
     * @param settings      Pretty-printer settings
     * @param contextRule   Current context rule (may be null)
     * @param hlContextRule Current HL context rule (may be null)
     * @param prevRule      Previous rule in the chain (may be null)
     */
    void init(TokensExt tokenExt,
            List<String> tSource,
            RuleData ruleData,
            ISettings settings,
            IRule contextRule,
            IRule hlContextRule,
            IRule prevRule) throws AppException;

    /**
     * Called after all tokens have been initialised to allow post-processing.
     * Converted from ABAP: FINALIZE_INIT.
     */
    void finalizeInit() throws AppException;

    /** Validates the rule configuration. Converted from: VALIDATE. */
    void validate() throws AppException;

    // -----------------------------------------------------------------------
    // Position
    // -----------------------------------------------------------------------

    /**
     * Returns the start row (1-based) of this token. Converted from: GET_CUR_ROW.
     */
    int getCurRow() throws AppException;

    /** Stores an override for the current row. Converted from: SET_CUR_ROW. */
    void setCurRow(int curRow) throws AppException;

    /** Returns the end row (1-based) of this token. Converted from: GET_END_ROW. */
    int getEndRow() throws AppException;

    /**
     * Returns the start column offset of this token. Converted from:
     * GET_CUR_OFFSET_START.
     */
    int getCurOffsetStart() throws AppException;

    /**
     * Stores an override for the start column offset. Converted from:
     * SET_CUR_OFFSET_START.
     */
    void setCurOffsetStart(int curOffsetStart) throws AppException;

    /**
     * Returns the end column offset of this token. Converted from:
     * GET_CUR_OFFSET_END.
     */
    int getCurOffsetEnd() throws AppException;

    // -----------------------------------------------------------------------
    // Indent
    // -----------------------------------------------------------------------

    /**
     * Returns the indent to use for continuation lines. Converted from:
     * GET_NEW_LINE_INDENT.
     */
    int getNewLineIndent() throws AppException;

    /**
     * Returns the indent for a new statement. Converted from:
     * GET_NEW_STATEMENT_INDENT.
     */
    int getNewStatementIndent() throws AppException;

    /**
     * Sets an additional offset that is added on top of any computed indent.
     * Converted from: SET_ADDITIONAL_INDENT.
     */
    void setAdditionalIndent(int indent) throws AppException;

    /** Returns the additional indent. Converted from: GET_ADDITIONAL_INDENT. */
    int getAdditionalIndent() throws AppException;

    // -----------------------------------------------------------------------
    // Text / token
    // -----------------------------------------------------------------------

    /**
     * Returns the formatted text of this token (including its trailing delimiter).
     * Multi-line delimiters produce multiple list entries.
     * Converted from: GET_TEXT.
     */
    List<String> getText() throws AppException;

    /** Returns the token string in upper case. Converted from: GET_TOKEN_UP. */
    String getTokenUp();

    /**
     * Returns the underlying extended token data. Converted from: GET_TOKEN_EXT.
     */
    TokensExt getTokenExt();

    /** Returns the rule configuration data. Converted from: GET_RULE_DATA. */
    RuleData getRuleData() throws AppException;

    // -----------------------------------------------------------------------
    // Line-break predicates
    // -----------------------------------------------------------------------

    /**
     * Returns true when a line break is required after this token. Converted from:
     * IS_NEW_LINE_REQ.
     */
    boolean isNewLineReq() throws AppException;

    /**
     * Returns true when this token itself causes a line break. Converted from:
     * IS_LINE_BREAKING_TOKEN.
     */
    boolean isLineBreakingToken();

    /**
     * Like isLineBreakingToken() but also considers trailing delimiters.
     * Converted from: IS_LB_TOKEN_RESP_DELIMITER.
     */
    boolean isLbTokenRespDelimiter();

    /**
     * Returns true when this token ends a statement ('.' or ';'). Converted from:
     * IS_END_OF_STATEMENT.
     */
    boolean isEndOfStatement() throws AppException;

    /**
     * Returns true when the token has a multi-line delimiter. Converted from:
     * HAS_MULTLINE_DELIMITER.
     */
    boolean hasMultlineDelimiter() throws AppException;

    // -----------------------------------------------------------------------
    // Comment / misc
    // -----------------------------------------------------------------------

    /** Returns true when this token is a comment. Converted from: IS_COMMENT. */
    boolean isComment();

    /**
     * Prevents a line break from being inserted after this token.
     * Converted from: SET_AVOID_LB_AFTER_THIS_TOKEN.
     */
    void setAvoidLbAfterThisToken(boolean avoid) throws AppException;

    /**
     * Clears all cached/buffered position values. Converted from: REFRESH_BUFFER.
     */
    void refreshBuffer();
}
