package com.github.fmabap.amdpprettyprinter.prettyprinter;

import java.util.ArrayList;
import java.util.List;

/**
 * Tokeniser for ABAP/SQLScript source code.
 * Converted from ABAP class ZCL_APP_TOKENIZER.
 *
 * <p>
 * Iterates source lines character by character and produces a list of
 * {@link TokensExt} token descriptors. After the raw scan the
 * {@code fillDerived} pass sets {@code strUp}, {@code strOrg},
 * {@code delimiterOrg} and calls the AMDP keyword scanner to case-fold
 * SQLScript identifiers.
 * </p>
 */
public class Tokenizer {

    // -----------------------------------------------------------------------
    // Scanner-state constants
    // -----------------------------------------------------------------------

    /** Normal ABAP / SQLScript code - no special context. */
    private static final int STATE_NORMAL = 0;
    /** Inside a single-quoted string literal ('...'). */
    private static final int STATE_IN_STRING = 1;
    /** Inside a template literal (|...|). */
    private static final int STATE_IN_TEMPLATE = 2;
    /** Inside a single-line comment (* or " or --). */
    private static final int STATE_CMT_LINE = 3;
    /** Inside a multi-line comment (/* ... * /). */
    private static final int STATE_CMT_MULTI = 4;

    // SQL-context constants (track where we are w.r.t. ABAP METHOD … BY DATABASE)
    /** Normal ABAP code - no SQLScript in sight. */
    private static final int SQL_ABAP = 10;
    /** Saw METHOD, waiting for LANGUAGE SQLSCRIPT. */
    private static final int SQL_AWAITING = 11;
    /** Saw LANGUAGE SQLSCRIPT, waiting for statement terminator '.'. */
    private static final int SQL_PENDING = 12;
    /** Inside the SQLScript body (after the terminating '.'). */
    private static final int SQL_CONTENT = 13;

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /**
     * Tokenises the given source lines.
     * Converted from ABAP: TOKENIZE.
     *
     * @param source Source lines (one entry per line).
     * @return Ordered list of token descriptors.
     * @throws AppException if any post-processing step fails.
     */
    public List<TokensExt> tokenize(List<String> source) throws AppException {
        List<TokensExt> result = scanSource(source);
        fillDerived(result);
        return result;
    }

    // -----------------------------------------------------------------------
    // Scanning - main loop
    // -----------------------------------------------------------------------

    /**
     * Iterates all source lines character by character and builds the raw token
     * list. Converted from ABAP: SCAN_SOURCE.
     */
    private List<TokensExt> scanSource(List<String> source) {
        ScanState scanState = new ScanState();
        int numLines = source.size();

        for (int lineIdx = 0; lineIdx < numLines; lineIdx++) {
            String line = source.get(lineIdx);
            int row = lineIdx + 1; // 1-based row (matching ABAP sy-tabix)
            int len = line.length();
            scanState.col = 0;

            while (scanState.col <= len) {
                char ch;
                if (scanState.col < len) {
                    ch = line.charAt(scanState.col);
                } else {
                    // Synthetic newline after each line except the last
                    if (lineIdx + 1 >= numLines) {
                        break;
                    }
                    ch = '\n';
                }

                char nextCh = '\0';
                int nextPos = scanState.col + 1;
                if (nextPos < len) {
                    nextCh = line.charAt(nextPos);
                }

                switch (scanState.tokenState) {
                    case STATE_NORMAL:
                        handleNormalChar(scanState, ch, nextCh, row);
                        break;
                    case STATE_IN_STRING:
                        handleStringChar(scanState, ch, nextCh);
                        break;
                    case STATE_IN_TEMPLATE:
                        handleTemplateChar(scanState, ch);
                        break;
                    case STATE_CMT_LINE:
                        handleCommentLineChar(scanState, ch);
                        break;
                    case STATE_CMT_MULTI:
                        handleCommentMultiChar(scanState, ch, nextCh, row);
                        break;
                    default:
                        break;
                }

                scanState.col++;
            }
        }

        // Flush anything remaining after the last line
        if (scanState.tokenBuf.length() > 0) {
            flushToken(scanState);
        }
        if (!scanState.delimBuf.isEmpty()) {
            flushDelimiter(scanState);
        }

        return scanState.result;
    }

    // -----------------------------------------------------------------------
    // Character handlers
    // -----------------------------------------------------------------------

    /**
     * Handles one character while in the normal (non-string, non-comment) state.
     * Converted from ABAP: HANDLE_NORMAL_CHAR.
     */
    private void handleNormalChar(ScanState scanState, char ch, char nextCh, int row) {

        if (ch == '*' && scanState.col == 0) {
            // '*' at column 0 → full-line ABAP comment
            flushToken(scanState);
            flushDelimiter(scanState);
            scanState.tokenRow = row;
            scanState.tokenCol = scanState.col;
            scanState.tokenComment = Comment.SINGLE_LINE;
            scanState.tokenCmtDetail = CommentDetail.START_BEGIN_OF_LINE;
            scanState.tokenState = STATE_CMT_LINE;
            scanState.tokenBuf.append('*');

        } else if (ch == '"' && scanState.sqlContext != SQL_CONTENT) {
            // '"' → inline ABAP comment (not inside SQLScript body)
            flushToken(scanState);
            flushDelimiter(scanState);
            scanState.tokenRow = row;
            scanState.tokenCol = scanState.col;
            scanState.tokenComment = Comment.SINGLE_LINE;
            scanState.tokenCmtDetail = CommentDetail.START_BEGIN_OF_LINE_INDENTABLE;
            scanState.tokenState = STATE_CMT_LINE;
            scanState.tokenBuf.append('"');

        } else if (ch == '/' && nextCh == '*' && scanState.sqlContext == SQL_CONTENT) {
            // '/*' opens a multi-line SQLScript comment
            flushToken(scanState);
            flushDelimiter(scanState);
            scanState.tokenRow = row;
            scanState.tokenCol = scanState.col;
            scanState.tokenComment = Comment.MULTI_LINE;
            if (scanState.col == 0) {
                scanState.tokenCmtDetail = CommentDetail.START_BEGIN_OF_LINE;
            } else if (!scanState.result.isEmpty() && scanState.result.get(scanState.result.size() - 1).row == row) {
                scanState.tokenCmtDetail = CommentDetail.START;
            } else {
                scanState.tokenCmtDetail = CommentDetail.START_BEGIN_OF_LINE_INDENTABLE;
            }
            scanState.tokenState = STATE_CMT_MULTI;
            scanState.tokenBuf.append("/*");
            scanState.col++; // skip '*'

        } else if (ch == '-' && nextCh == '-') {
            // '--' is a SQLScript inline comment or just a normal token
            if (scanState.sqlContext == SQL_CONTENT
                    || (scanState.sqlContext == SQL_PENDING && containsDelimiterAPoint(scanState.delimBuf))) {
                flushToken(scanState);
                flushDelimiter(scanState);
                scanState.tokenRow = row;
                scanState.tokenCol = scanState.col;
                scanState.tokenComment = Comment.SINGLE_LINE;
                int rsz = scanState.result.size();
                if (rsz == 0 || scanState.result.get(rsz - 1).row != row) {
                    scanState.tokenCmtDetail = CommentDetail.START_BEGIN_OF_LINE_INDENTABLE;
                } else {
                    scanState.tokenCmtDetail = CommentDetail.START;
                }
                scanState.tokenState = STATE_CMT_LINE;
                scanState.tokenBuf.append("--");
                scanState.col++; // skip second '-'
            } else {
                // Not a comment context - treat each '-' as a normal char
                if (scanState.tokenBuf.length() == 0) {
                    flushDelimiter(scanState);
                    scanState.tokenRow = row;
                    scanState.tokenCol = scanState.col;
                }
                scanState.tokenBuf.append(ch);
            }

        } else if (ch == '\'') {
            // '\'' opens a string literal
            flushToken(scanState);
            flushDelimiter(scanState);
            scanState.tokenRow = row;
            scanState.tokenCol = scanState.col;
            scanState.tokenState = STATE_IN_STRING;
            scanState.tokenBuf.append('\'');

        } else if (ch == '|' && scanState.sqlContext != SQL_CONTENT) {
            // '|' opens a template literal
            flushToken(scanState);
            flushDelimiter(scanState);
            scanState.tokenRow = row;
            scanState.tokenCol = scanState.col;
            scanState.tokenState = STATE_IN_TEMPLATE;
            scanState.templateDepth = 0;
            scanState.tokenBuf.append('|');

        } else if (ch == '(' || ch == ')' || ch == '[' || ch == ']') {
            // Brackets are each their own single-character token
            flushToken(scanState);
            flushDelimiter(scanState);
            scanState.tokenRow = row;
            scanState.tokenCol = scanState.col;
            scanState.tokenBuf.append(ch);
            flushToken(scanState);

        } else if (ch == ':' && nextCh == ':' && scanState.sqlContext == SQL_CONTENT) {
            // '::' scope-resolution operator in SQLScript
            flushToken(scanState);
            flushDelimiter(scanState);
            scanState.tokenRow = row;
            scanState.tokenCol = scanState.col;
            scanState.tokenBuf.append("::");
            flushToken(scanState);
            scanState.col++; // skip second ':'

        } else if (scanState.sqlContext == SQL_CONTENT && ch == ',') {
            // In SQLScript body: comma is its own token
            flushToken(scanState);
            flushDelimiter(scanState);
            scanState.tokenRow = row;
            scanState.tokenCol = scanState.col;
            scanState.tokenBuf.append(ch);
            flushToken(scanState);

        } else if (ch == '.' && nextCh == '"' && scanState.sqlContext == SQL_CONTENT) {
            // '."' in SQLScript: '.' becomes its own token (quoted identifier follows)
            flushToken(scanState);
            flushDelimiter(scanState);
            scanState.tokenRow = row;
            scanState.tokenCol = scanState.col;
            scanState.tokenBuf.append(ch);
            flushToken(scanState);

        } else if ((ch == ':' && scanState.sqlContext != SQL_CONTENT)
                || (ch == '.' && scanState.sqlContext != SQL_CONTENT)
                || ch == ',' || ch == ';') {
            // Statement / chain delimiters → accumulate in delimiter buffer
            Boolean isNewLineWithEmptyToken = false;
            if (scanState.tokenBuf.length() == 0 && scanState.result.size() > 0) {
                TokensExt last = scanState.result.get(scanState.result.size() - 1);
                if (last.row != row) {
                    isNewLineWithEmptyToken = true;
                }
            }
            if (isNewLineWithEmptyToken) {
                // Set delimiter as token for new line after single line comment
                // to avoid that the delimiter will be added to the comment
                flushDelimiter(scanState);
                scanState.tokenRow = row;
                scanState.tokenCol = scanState.col;
                scanState.tokenBuf.append(ch);
            } else {
                flushToken(scanState);
                appendToLastDelim(scanState.delimBuf, ch);
            }

        } else if (ch == ' ' || ch == '\t') {
            // Whitespace → end current token, accumulate in delimiter buffer
            flushToken(scanState);
            appendToLastDelim(scanState.delimBuf, ch);

        } else if (ch == '\n') {
            // End of source line
            flushToken(scanState);
            ensureDelimLine(scanState.delimBuf);
            scanState.delimBuf.add("");

        } else {
            // Normal token character
            if (scanState.tokenBuf.length() == 0) {
                flushDelimiter(scanState);
                scanState.tokenRow = row;
                scanState.tokenCol = scanState.col;
            }
            scanState.tokenBuf.append(ch);
        }
    }

    /**
     * Handles one character while inside a single-quoted string literal.
     * Converted from ABAP: HANDLE_STRING_CHAR.
     */
    private void handleStringChar(ScanState scanState, char ch, char nextCh) {
        if (ch == '\'') {
            if (nextCh == '\'') {
                // Escaped single quote inside the string
                scanState.tokenBuf.append("''");
                scanState.col++; // skip second quote
            } else {
                // Closing quote
                scanState.tokenBuf.append('\'');
                flushToken(scanState);
                scanState.tokenState = STATE_NORMAL;
            }
        } else {
            scanState.tokenBuf.append(ch);
        }
    }

    /**
     * Handles one character while inside a template literal (|...|).
     * Converted from ABAP: HANDLE_TEMPLATE_CHAR.
     */
    private void handleTemplateChar(ScanState scanState, char ch) {
        if (ch == '\n') {
            // Multi-line template: keep newline in the buffer
            scanState.tokenBuf.append('\n');
        } else if (ch == '{') {
            scanState.templateDepth++;
            scanState.tokenBuf.append("\\{");
        } else if (ch == '}') {
            scanState.templateDepth--;
            scanState.tokenBuf.append("\\}");
        } else if (ch == '|' && scanState.templateDepth == 0) {
            // Closing '|'
            scanState.tokenBuf.append("\\|");
            flushToken(scanState);
            scanState.tokenState = STATE_NORMAL;
        } else {
            scanState.tokenBuf.append(ch);
        }
    }

    /**
     * Handles one character while inside a single-line comment.
     * Converted from ABAP: HANDLE_COMMENT_LINE_CHAR.
     */
    private void handleCommentLineChar(ScanState scanState, char ch) {
        if (ch == '\n') {
            flushToken(scanState);
            scanState.tokenState = STATE_NORMAL;
            ensureDelimLine(scanState.delimBuf);
            scanState.delimBuf.add("");
        } else {
            scanState.tokenBuf.append(ch);
        }
    }

    /**
     * Handles one character while inside a multi-line comment (/* ... * /).
     * Converted from ABAP: HANDLE_COMMENT_MULTI_CHAR.
     */
    private void handleCommentMultiChar(ScanState scanState, char ch, char nextCh, int row) {
        if (ch == '\n') {
            // Each source line of a multi-line comment is its own token
            flushToken(scanState);
            ensureDelimLine(scanState.delimBuf);
            scanState.delimBuf.add("");
            scanState.tokenComment = Comment.MULTI_LINE;
            scanState.tokenCmtDetail = CommentDetail.PART;

        } else if (ch == '*' && nextCh == '/') {
            // '*/' closes the multi-line comment
            if (scanState.tokenBuf.length() == 0) {
                flushDelimiter(scanState);
                scanState.tokenRow = row;
                scanState.tokenCol = scanState.col;
            }
            scanState.tokenBuf.append("*/");
            flushToken(scanState);
            scanState.tokenState = STATE_NORMAL;
            scanState.col++; // skip '/'

        } else {
            if (scanState.tokenBuf.length() == 0) {
                flushDelimiter(scanState);
                scanState.tokenRow = row;
                scanState.tokenCol = scanState.col;
            }
            scanState.tokenBuf.append(ch);
        }
    }

    // -----------------------------------------------------------------------
    // Flush helpers
    // -----------------------------------------------------------------------

    /**
     * Finalises the current token buffer by creating a {@link TokensExt}
     * and appending it to the result list. Also drives the SQL-context state
     * machine. Converted from ABAP: FLUSH_TOKEN.
     */
    private void flushToken(ScanState scanState) {
        if (scanState.tokenBuf.length() == 0) {
            return;
        }

        String buf = scanState.tokenBuf.toString();
        int newIdx = scanState.result.size(); // 0-based index of the element to be added

        TokensExt tok = new TokensExt();
        tok.str = buf;
        tok.row = scanState.tokenRow;
        tok.col = scanState.tokenCol;
        tok.len = buf.length();
        tok.comment = scanState.tokenComment;
        tok.commentDetail = scanState.tokenCmtDetail;
        tok.orgTabRow = newIdx + 1; // 1-based (matches ABAP lv_sql_newidx)

        scanState.result.add(tok);

        // SQL-context state machine
        String smUp = buf.toUpperCase();
        switch (scanState.sqlContext) {
            case SQL_ABAP:
                if ("METHOD".equals(smUp)) {
                    scanState.sqlContext = SQL_AWAITING;
                }
                if (tok.comment == Comment.NONE) {
                    scanState.lastNcIdx = newIdx;
                }
                break;

            case SQL_AWAITING:
                if ("SQLSCRIPT".equals(smUp) && scanState.lastNcIdx >= 0) {
                    TokensExt prevNc = scanState.result.get(scanState.lastNcIdx);
                    if ("LANGUAGE".equalsIgnoreCase(prevNc.str)) {
                        scanState.sqlContext = SQL_PENDING;
                        tok.sqlscript = Sqlscript.PENDING;
                        scanState.lastNcIdx = newIdx;
                    } else {
                        if (tok.comment == Comment.NONE) {
                            scanState.lastNcIdx = newIdx;
                        }
                    }
                } else {
                    if (tok.comment == Comment.NONE) {
                        scanState.lastNcIdx = newIdx;
                    }
                }
                break;

            case SQL_PENDING:
                tok.sqlscript = Sqlscript.PENDING;
                if (tok.comment == Comment.NONE) {

                    if (smUp.contains(".")) {
                        scanState.sqlContext = SQL_CONTENT;
                        scanState.result.get(newIdx).sqlscript = Sqlscript.END_OF_PENDING;
                    }
                    scanState.lastNcIdx = newIdx;
                }
                break;

            case SQL_CONTENT:
                if ("ENDMETHOD".equals(smUp) || "ENDMETHOD.".equals(smUp)) {
                    scanState.sqlContext = SQL_ABAP;
                } else {
                    tok.sqlscript = Sqlscript.SQLSCRIPT;
                }
                break;

            default:
                break;
        }

        scanState.tokenBuf.setLength(0);
        scanState.tokenComment = Comment.NONE;
        scanState.tokenCmtDetail = CommentDetail.NONE;
    }

    /**
     * Attaches the accumulated delimiter buffer to the last token in the result
     * list. Also drives the SQL-context state machine for the '.' terminator.
     * Converted from ABAP: FLUSH_DELIMITER.
     */
    private void flushDelimiter(ScanState scanState) {
        if (scanState.result.isEmpty()) {
            // No token yet - discard (e.g. leading empty line)
            scanState.delimBuf.clear();
            return;
        }

        TokensExt last = scanState.result.get(scanState.result.size() - 1);
        if (scanState.delimBuf.isEmpty()) {
            scanState.delimBuf.add("");
        }
        last.delimiter = new ArrayList<>(scanState.delimBuf);
        scanState.delimBuf.clear();

        // SQL-context state machine: check for statement terminator '.'
        if (scanState.sqlContext == SQL_AWAITING || scanState.sqlContext == SQL_PENDING) {
            for (String delimLine : last.delimiter) {
                if (delimLine.contains(".")) {
                    if (scanState.sqlContext == SQL_AWAITING) {
                        scanState.sqlContext = SQL_ABAP;
                    } else { // SQL_PENDING
                        if (scanState.lastNcIdx >= 0) {
                            scanState.result
                                    .get(scanState.lastNcIdx).sqlscript = Sqlscript.END_OF_PENDING;
                        }
                        scanState.sqlContext = SQL_CONTENT;
                    }
                    break;
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // Derived-field pass
    // -----------------------------------------------------------------------

    /**
     * Fills the derived fields {@code strUp}, {@code strOrg}, {@code delimiterOrg}
     * on every token and invokes the keyword scanner to case-fold SQLScript
     * identifiers. Converted from ABAP: FILL_DERIVED.
     */
    private void fillDerived(List<TokensExt> tokenExt) throws AppException {
        IKeywordScanner kwScanner = new KeywordScannerAmdp();
        int size = tokenExt.size();
        for (int i = 0; i < size; i++) {
            TokensExt tok = tokenExt.get(i);
            tok.strUp = tok.str.toUpperCase();
            tok.strOrg = tok.str;
            tok.delimiterOrg = new ArrayList<>(tok.delimiter);
            kwScanner.scanKeyword(tokenExt, i);
        }
    }

    // -----------------------------------------------------------------------
    // Delimiter-buffer helpers
    // -----------------------------------------------------------------------

    /**
     * Appends {@code ch} to the last entry of the delimiter buffer, creating a
     * new empty entry first if the buffer is empty.
     */
    private static void appendToLastDelim(List<String> delimBuf, char ch) {
        if (delimBuf.isEmpty()) {
            delimBuf.add(String.valueOf(ch));
        } else {
            int last = delimBuf.size() - 1;
            delimBuf.set(last, delimBuf.get(last) + ch);
        }
    }

    /**
     * Ensures there is at least one entry in the delimiter buffer (mirrors
     * {@code IF ct_delim_buf IS INITIAL. APPEND INITIAL LINE. ENDIF.} in ABAP).
     */
    private static void ensureDelimLine(List<String> delimBuf) {
        if (delimBuf.isEmpty()) {
            delimBuf.add("");
        }
    }

    /**
     * Returns {@code true} when any entry in the delimiter buffer contains a
     * period ('.'). Converted from ABAP: CONTAINS_DELIMITER_A_POINT.
     */
    private static boolean containsDelimiterAPoint(List<String> delimBuf) {
        for (String s : delimBuf) {
            if (s.contains(".")) {
                return true;
            }
        }
        return false;
    }

    // -----------------------------------------------------------------------
    // Mutable scanner state
    // -----------------------------------------------------------------------

    /**
     * Holds all mutable state that the ABAP handler methods receive as
     * {@code CHANGING} parameters.
     */
    private static final class ScanState {
        int tokenState = STATE_NORMAL;
        int sqlContext = SQL_ABAP;

        final StringBuilder tokenBuf = new StringBuilder();
        int tokenRow = 0;
        int tokenCol = 0;
        Comment tokenComment = Comment.NONE;
        CommentDetail tokenCmtDetail = CommentDetail.NONE;

        /** 0-based index of the last non-comment token; -1 = none yet. */
        int lastNcIdx = -1;
        int templateDepth = 0;
        /** Current column (may be advanced by handlers to skip characters). */
        int col = 0;

        final List<String> delimBuf = new ArrayList<>();
        final List<TokensExt> result = new ArrayList<>();
    }
}
