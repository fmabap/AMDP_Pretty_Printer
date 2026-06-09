package com.github.fmabap.amdpprettyprinter.prettyprinter;

import java.util.ArrayList;
import java.util.List;

import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.RuleFactory;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.RuleData;
import com.github.fmabap.amdpprettyprinter.prettyprinter.rules.IRule;

/**
 * Main pretty-printer class.
 * Converted from ABAP class ZCL_APP_PRETTY_PRINTER.
 *
 * <p>
 * Orchestrates tokenisation, rule creation, rule resolution, and source
 * reconstruction for the AMDP Pretty Printer.
 * </p>
 */
public final class PrettyPrinter {

    private static final boolean ANSI_SUPPORTED = isAnsiSupported();

    private static boolean isAnsiSupported() {
        // Explicitly disabled via system property
        if ("false".equalsIgnoreCase(System.getProperty("ansi.enabled"))) {
            return false;
        }
        // Explicitly enabled via system property
        if ("true".equalsIgnoreCase(System.getProperty("ansi.enabled"))) {
            return true;
        }
        // VS Code integrated terminal
        if (System.getenv("TERM_PROGRAM") != null) {
            return true;
        }
        // Standard Unix terminals
        String term = System.getenv("TERM");
        if (term != null && !term.equals("dumb")) {
            return true;
        }
        // Windows: enable Virtual Terminal Processing via kernel32
        if (System.getProperty("os.name", "").toLowerCase().contains("win")) {
            try {
                Class.forName("com.sun.jna.platform.win32.Kernel32");
                return true; // JNA present – could enable VTP; assume supported
            } catch (ClassNotFoundException ignored) {
                // JNA not available; fall through
            }
        }
        return false;
    }

    private static final String ANSI_RESET = ANSI_SUPPORTED ? "\u001B[0m" : "";
    private static final String ANSI_YELLOW = ANSI_SUPPORTED ? "\u001B[33m" : "";
    private static final String ANSI_GREEN = ANSI_SUPPORTED ? "\u001B[32m" : "";
    private static final String ANSI_RED = ANSI_SUPPORTED ? "\u001B[31m" : "";
    private static final String ANSI_CYAN = ANSI_SUPPORTED ? "\u001B[36m" : "";

    // -----------------------------------------------------------------------
    // Public API
    // -----------------------------------------------------------------------

    /**
     * Pretty-prints the given source lines using the provided settings.
     * Converted from ABAP: PRETTY_PRINT.
     *
     * @param source   Source lines to format
     * @param settings Pretty-printer settings
     * @return Formatted source lines
     * @throws AppException if formatting fails
     */
    public List<String> prettyPrint(List<String> source, ISettings settings) throws AppException {

        List<String> workSource = new ArrayList<>(source);
        List<TokensExt> tokenExt = new ArrayList<>();

        Tokenizer tokenizer = new Tokenizer();
        tokenExt = tokenizer.tokenize(workSource);

        if (settings.isTrace()) {
            printTraceTable(tokenExt);
        }

        return getAndApplyRules(workSource, settings, tokenExt);
    }

    // -----------------------------------------------------------------------
    // Private helpers
    // -----------------------------------------------------------------------

    /**
     * Prints the token list as a Markdown table to {@link System#err}.
     * Column widths are computed from the actual data so the table stays compact.
     */
    private static void printTraceTable(List<TokensExt> tokenExt) {

        // Column headers (order matches TokensExt field declaration order)
        String[] headers = { "#", "str", "row", "col", "len", "type", "orgTabRow",
                "sqlscript", "comment", "commentDetail",
                "delimiter", "delimiterOrg", "strUp", "strOrg", "isKeyword" };

        // Collect raw cell values
        String[][] cells = new String[tokenExt.size()][headers.length];
        for (int i = 0; i < tokenExt.size(); i++) {
            TokensExt t = tokenExt.get(i);
            cells[i][0] = String.valueOf(i);
            cells[i][1] = t.str;
            cells[i][2] = String.valueOf(t.row);
            cells[i][3] = String.valueOf(t.col);
            cells[i][4] = String.valueOf(t.len);
            cells[i][5] = t.type;
            cells[i][6] = String.valueOf(t.orgTabRow);
            cells[i][7] = t.sqlscript.name();
            cells[i][8] = t.comment.name();
            cells[i][9] = t.commentDetail.name();
            cells[i][10] = formatList(t.delimiter);
            cells[i][11] = formatList(t.delimiterOrg);
            cells[i][12] = t.strUp;
            cells[i][13] = t.strOrg;
            cells[i][14] = String.valueOf(t.isKeyword);
        }

        // Compute max width per column (at least header width)
        int[] widths = new int[headers.length];
        for (int c = 0; c < headers.length; c++) {
            widths[c] = headers[c].length();
        }
        for (String[] row : cells) {
            for (int c = 0; c < headers.length; c++) {
                widths[c] = Math.max(widths[c], row[c].length());
            }
        }

        // Build separator line
        StringBuilder sep = new StringBuilder("|");
        for (int w : widths) {
            sep.append(" ").append("-".repeat(w)).append(" |");
        }

        // Print header
        StringBuilder header = new StringBuilder("|");
        for (int c = 0; c < headers.length; c++) {
            header.append(String.format(" %-" + widths[c] + "s |", headers[c]));
        }

        System.err.println("TokensExt trace (" + tokenExt.size() + " tokens)");
        System.err.println(header);
        System.err.println(sep);

        // Print data rows
        for (String[] row : cells) {
            StringBuilder line = new StringBuilder("|");
            for (int c = 0; c < headers.length; c++) {
                line.append(String.format(" %-" + widths[c] + "s |", row[c]));
            }
            System.err.println(line);
        }
    }

    /**
     * Formats a list for trace output. Wrapped in square brackets.
     */
    private static String formatList(List<String> list) {
        if (list.size() == 0) {
            return "";
        }
        StringBuilder sb = new StringBuilder();
        for (String entry : list) {
            sb.append('[').append(entry).append(']');
        }
        return sb.toString();
    }

    /** Prints all rules as a Markdown table (initial resolved state). */
    private static void printRuleTable(List<RuleSnapshot> snapshot) {
        String[] headers = {
                "#", "rule",
                // TokensExt originals
                "str", "orgRow", "orgCol", "orgLen", "type", "orgTabRow",
                "sqlscript", "comment", "commentDetail",
                "delimiter", "delimiterOrg", "strUp", "strOrg",
                // resolved
                "curRow", "endRow", "offsetStart", "offsetEnd", "text"
        };
        String[][] cells = new String[snapshot.size()][headers.length];
        for (int i = 0; i < snapshot.size(); i++) {
            RuleSnapshot s = snapshot.get(i);
            cells[i][0] = String.valueOf(i);
            cells[i][1] = s.ruleName;
            cells[i][2] = s.str;
            cells[i][3] = String.valueOf(s.orgRow);
            cells[i][4] = String.valueOf(s.orgCol);
            cells[i][5] = String.valueOf(s.orgLen);
            cells[i][6] = s.type;
            cells[i][7] = String.valueOf(s.orgTabRow);
            cells[i][8] = s.sqlscript;
            cells[i][9] = s.comment;
            cells[i][10] = s.commentDetail;
            cells[i][11] = formatList(s.delimiter);
            cells[i][12] = formatList(s.delimiterOrg);
            cells[i][13] = s.strUp;
            cells[i][14] = s.strOrg;
            cells[i][15] = String.valueOf(s.curRow);
            cells[i][16] = String.valueOf(s.endRow);
            cells[i][17] = String.valueOf(s.curOffsetStart);
            cells[i][18] = String.valueOf(s.curOffsetEnd);
            cells[i][19] = formatList(s.text);
        }
        int[] widths = new int[headers.length];
        for (int c = 0; c < headers.length; c++) {
            widths[c] = headers[c].length();
        }
        for (String[] row : cells) {
            for (int c = 0; c < headers.length; c++) {
                widths[c] = Math.max(widths[c], row[c].length());
            }
        }
        StringBuilder sep = new StringBuilder("|");
        for (int w : widths) {
            sep.append(" ").append("-".repeat(w)).append(" |");
        }
        StringBuilder header = new StringBuilder("|");
        for (int c = 0; c < headers.length; c++) {
            header.append(String.format(" %-" + widths[c] + "s |", headers[c]));
        }
        System.err.println(ANSI_YELLOW + "=== calcRuleResult: initial rule state ("
                + snapshot.size() + " rules) ===" + ANSI_RESET);
        System.err.println(header);
        System.err.println(sep);
        for (String[] row : cells) {
            StringBuilder line = new StringBuilder("|");
            for (int c = 0; c < headers.length; c++) {
                line.append(String.format(" %-" + widths[c] + "s |", row[c]));
            }
            System.err.println(line);
        }
    }

    /** Prints per-rule changes between two consecutive snapshots, coloured. */
    private static void printRuleChanges(int iteration,
            List<RuleSnapshot> prev, List<RuleSnapshot> curr) {

        System.err.println(ANSI_YELLOW + "=== calcRuleResult: iteration "
                + iteration + " changes ===" + ANSI_RESET);

        for (int i = 0; i < curr.size(); i++) {
            RuleSnapshot p = prev.get(i);
            RuleSnapshot c = curr.get(i);
            if (p.equals(c)) {
                continue;
            }
            System.err.print(ANSI_YELLOW + "  Rule [" + c.ruleName + "]" + ANSI_RESET);
            if (p.curRow != c.curRow) {
                System.err.print("  " + ANSI_CYAN + "curRow" + ANSI_RESET
                        + " " + ANSI_RED + p.curRow + ANSI_RESET
                        + "->" + ANSI_GREEN + c.curRow + ANSI_RESET);
            }
            if (p.endRow != c.endRow) {
                System.err.print("  " + ANSI_CYAN + "endRow" + ANSI_RESET
                        + " " + ANSI_RED + p.endRow + ANSI_RESET
                        + "->" + ANSI_GREEN + c.endRow + ANSI_RESET);
            }
            if (p.curOffsetStart != c.curOffsetStart) {
                System.err.print("  " + ANSI_CYAN + "offsetStart" + ANSI_RESET
                        + " " + ANSI_RED + p.curOffsetStart + ANSI_RESET
                        + "->" + ANSI_GREEN + c.curOffsetStart + ANSI_RESET);
            }
            if (p.curOffsetEnd != c.curOffsetEnd) {
                System.err.print("  " + ANSI_CYAN + "offsetEnd" + ANSI_RESET
                        + " " + ANSI_RED + p.curOffsetEnd + ANSI_RESET
                        + "->" + ANSI_GREEN + c.curOffsetEnd + ANSI_RESET);
            }
            if (!p.text.equals(c.text)) {
                System.err.print("  " + ANSI_CYAN + "text" + ANSI_RESET
                        + " " + ANSI_RED + formatList(p.text) + ANSI_RESET
                        + "->" + ANSI_GREEN + formatList(c.text) + ANSI_RESET);
            }
            System.err.println();
        }
    }

    /**
     * Creates all rules for the token stream, resolves their positions, and
     * reconstructs formatted source from them.
     * Converted from ABAP: GET_AND_APPLY_RULES.
     */
    private List<String> getAndApplyRules(
            List<String> source,
            ISettings settings,
            List<TokensExt> tokenExt) throws AppException {

        RuleFactory ruleFactory = new RuleFactory(settings);

        List<IRule> rules = getRules(source, ruleFactory, tokenExt);

        calcRuleResult(rules, settings.isTrace());

        return getSourceCodeFromRules(rules);
    }

    /**
     * Iterates all tokens and builds the rule chain via the rule factory.
     * Converted from ABAP: GET_RULES.
     */
    private List<IRule> getRules(
            List<String> source,
            RuleFactory ruleFactory,
            List<TokensExt> tokenExt) throws AppException {

        List<IRule> result = new ArrayList<>();
        IRule prevRule = null;

        for (TokensExt token : tokenExt) {
            IRule rule = ruleFactory.getRule(source, prevRule, token);

            if (prevRule != null) {
                prevRule.setNextRule(rule);
            }

            result.add(rule);
            prevRule = rule;
        }

        for (IRule rule : result) {
            rule.finalizeInit();
        }

        return result;
    }

    /**
     * Iterates rule resolution until the positions stabilise (or throws if they
     * never do).
     * Converted from ABAP: CALC_RULE_RESULT.
     *
     * <p>
     * The ABAP implementation retries up to 11 times, comparing snapshots of
     * every rule's position/text state. If consecutive snapshots are equal the
     * rules have converged.
     * </p>
     */
    private void calcRuleResult(List<IRule> rules, boolean trace) throws AppException {

        List<RuleSnapshot> prevSnapshot = null;

        for (int iteration = 0; iteration < 11; iteration++) {

            List<RuleSnapshot> snapshot = new ArrayList<>(rules.size());
            for (IRule rule : rules) {
                snapshot.add(new RuleSnapshot(rule));
            }

            if (snapshot.equals(prevSnapshot)) {
                if (trace) {
                    System.out.println(ANSI_YELLOW + "=== calcRuleResult: converged after "
                            + iteration + " iteration(s) ===" + ANSI_RESET);
                }
                return;
            }

            if (trace) {
                if (prevSnapshot == null) {
                    printRuleTable(snapshot);
                } else {
                    printRuleChanges(iteration, prevSnapshot, snapshot);
                }
            }

            prevSnapshot = snapshot;
        }

        throw new AppException("ZAPP_MC_PRETTY_PRINT 013: Rule result did not converge after 11 iterations.");
    }

    /**
     * Reconstructs the formatted source lines from the resolved rule chain.
     * Converted from ABAP: GET_SOURCE_CODE_FROM_RULES.
     */
    private List<String> getSourceCodeFromRules(List<IRule> rules) throws AppException {

        SourceCursor cursor = new SourceCursor();

        for (IRule rule : rules) {
            getActSourceRow(rule, cursor);
            addRuleToSource(rule, cursor);
        }

        cursor.source.replaceAll(line -> line.stripTrailing());
        return cursor.source;
    }

    /**
     * Advances the source cursor to the row required by {@code rule}, inserting
     * empty lines as needed.
     * Converted from ABAP: GET_ACT_SOURCE_ROW.
     *
     * @throws AppException if the cursor has already passed the required row
     */
    private void getActSourceRow(IRule rule, SourceCursor cursor) throws AppException {

        int ruleRow = rule.getCurRow();

        while (cursor.actRow < ruleRow) {
            cursor.insertNewLine();
        }

        if (cursor.actRow > ruleRow) {
            TokensExt tokenExt = rule.getTokenExt();
            RuleData ruleData = rule.getRuleData();

            throw new AppException(
                    String.format("ZAPP_MC_PRETTY_PRINT 008: Rule '%s', token '%s' at row %d col %d: "
                            + "source cursor already past target row.",
                            ruleData.ruleName, tokenExt.strUp, tokenExt.row, tokenExt.col));
        }
    }

    /**
     * Appends the rule's text to the current source line, adding leading spaces
     * up to the rule's start offset. Multi-line texts open new source lines.
     * Converted from ABAP: ADD_RULE_TO_SOURCE.
     *
     * @throws AppException if the rule's start offset is before the current line
     *                      length (overlap)
     */
    private void addRuleToSource(IRule rule, SourceCursor cursor) throws AppException {

        int currentLen = cursor.get().length();
        int spaces = rule.getCurOffsetStart() - currentLen;

        if (spaces < 0) {
            TokensExt tokenExt = rule.getTokenExt();
            RuleData ruleData = rule.getRuleData();

            throw new AppException(
                    String.format("ZAPP_MC_PRETTY_PRINT 007: Rule '%s', token '%s' at row %d col %d: "
                            + "start offset %d is before current line length %d.",
                            ruleData.ruleName, tokenExt.strUp, tokenExt.row, tokenExt.col,
                            rule.getCurOffsetStart(), currentLen));
        }

        // Pad to the required start column
        String padding = " ".repeat(spaces);
        cursor.set(cursor.get() + padding);

        // Append token text (getText() may return multiple lines)
        List<String> text = rule.getText();
        boolean first = true;
        for (String textLine : text) {
            if (first) {
                cursor.set(cursor.get() + textLine);
                first = false;
            } else {
                cursor.insertNewLine(textLine);
            }
        }
    }

    // -----------------------------------------------------------------------
    // Helper types
    // -----------------------------------------------------------------------

    /**
     * Mutable cursor into the source line list being built.
     *
     * <p>
     * Mirrors the ABAP pattern of passing {@code cr_source TYPE REF TO string}
     * together with {@code cv_act_row} as CHANGING parameters.
     * </p>
     */
    private static final class SourceCursor {

        final List<String> source = new ArrayList<>();
        /** 1-based row index, matching ABAP row numbering. 0 = before first line. */
        int actRow = 0;

        /** Returns the content of the current source line. */
        String get() {
            return source.get(actRow - 1);
        }

        /** Replaces the content of the current source line. */
        void set(String value) {
            source.set(actRow - 1, value);
        }

        /** Appends a new empty line and advances the cursor to it. */
        void insertNewLine() {
            source.add("");
            actRow++;
        }

        /** Appends a new line with the given content and advances the cursor. */
        void insertNewLine(String value) {
            source.add(value);
            actRow++;
        }
    }

    /**
     * Immutable snapshot of a rule's position state, used by
     * {@link #calcRuleResult} to detect convergence.
     * Converted from ABAP structure ZAPP_S_RULE_RESULT.
     */
    private static final class RuleSnapshot {

        final String ruleName;
        // TokensExt fields (original, in declaration order)
        final String str;
        final int orgRow;
        final int orgCol;
        final int orgLen;
        final String type;
        final int orgTabRow;
        final String sqlscript;
        final String comment;
        final String commentDetail;
        final List<String> delimiter;
        final List<String> delimiterOrg;
        final String strUp;
        final String strOrg;
        // Resolved rule-result fields
        final int curRow;
        final int endRow;
        final int curOffsetStart;
        final int curOffsetEnd;
        final List<String> text;

        RuleSnapshot(IRule rule) throws AppException {
            this.ruleName = rule.getRuleData().ruleName;
            TokensExt t = rule.getTokenExt();
            this.str = t.str;
            this.orgRow = t.row;
            this.orgCol = t.col;
            this.orgLen = t.len;
            this.type = t.type;
            this.orgTabRow = t.orgTabRow;
            this.sqlscript = t.sqlscript.name();
            this.comment = t.comment.name();
            this.commentDetail = t.commentDetail.name();
            this.delimiter = new ArrayList<>(t.delimiter);
            this.delimiterOrg = new ArrayList<>(t.delimiterOrg);
            this.strUp = t.strUp;
            this.strOrg = t.strOrg;
            this.curRow = rule.getCurRow();
            this.endRow = rule.getEndRow();
            this.curOffsetStart = rule.getCurOffsetStart();
            this.curOffsetEnd = rule.getCurOffsetEnd();
            this.text = new ArrayList<>(rule.getText());
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (!(obj instanceof RuleSnapshot))
                return false;
            RuleSnapshot other = (RuleSnapshot) obj;
            return curRow == other.curRow
                    && endRow == other.endRow
                    && curOffsetStart == other.curOffsetStart
                    && curOffsetEnd == other.curOffsetEnd
                    && text.equals(other.text);
        }

        @Override
        public int hashCode() {
            int h = curRow;
            h = 31 * h + endRow;
            h = 31 * h + curOffsetStart;
            h = 31 * h + curOffsetEnd;
            h = 31 * h + text.hashCode();
            return h;
        }
    }
}
