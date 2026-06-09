package com.github.fmabap.amdpprettyprinter.prettyprinter;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * Keyword scanner for AMDP / SQLScript tokens.
 * Converted from ABAP class ZCL_APP_KEYWORD_SCANNER_AMDP.
 *
 * <p>
 * Implements {@link IKeywordScanner}. Only SQLScript tokens are
 * processed: recognised SQL keywords are kept in upper case; unrecognised
 * tokens are converted to lower case. ABAP tokens and comments are
 * left untouched.
 * </p>
 *
 * <p>
 * The keyword list is the full SQL:2016 / SAP HANA SQLScript reserved-word
 * list taken from the ABAP implementation of GET_KEYWORDS.
 * </p>
 */
public final class KeywordScannerAmdp implements IKeywordScanner {

    // -----------------------------------------------------------------------
    // Keyword set (static, initialised once - mirrors ABAP class_constructor)
    // -----------------------------------------------------------------------

    private static final Set<String> KEYWORDS;

    /**
     * Keywords that introduce a table/view name context: any token immediately
     * following one of these is an object name, not a SQL keyword.
     */
    private static final Set<String> TABLE_CONTEXT_KEYWORDS = Collections.unmodifiableSet(
            new HashSet<>(Arrays.asList("FROM", "AS", "UPDATE", "INSERT", "DELETE", "UPSERT", "OF")));

    static {
        String[] kws = {
                "ABS", "ABSOLUTE", "ACTION", "ADA", "ADD", "ADMIN", "AFTER", "ALL",
                "ALLOCATE", "ALTER", "ALWAYS", "AND", "ANY", "ARE", "ARRAY",
                "ARRAY_AGG", "ARRAY_MAX_CARDINALITY", "AS", "ASC", "ASENSITIVE",
                "ASSERTION", "ASSIGNMENT", "ASYMMETRIC", "AT", "ATOMIC", "ATTRIBUTE",
                "ATTRIBUTES", "AUTHORIZATION", "AVG", "BEFORE", "BEGIN",
                "BEGIN_FRAME", "BEGIN_PARTITION", "BERNOULLI", "BETWEEN", "BIGINT",
                "BINARY", "BLOB", "BOOL", "BOOLEAN", "BOTH", "BREADTH", "BREAK",
                "BY", "CALL", "CALLED", "CARDINALITY", "CASCADE", "CASCADED",
                "CASE", "CAST", "CATALOG_NAME", "CEIL", "CEILING", "CHAIN", "CHAR",
                "CHAR_LENGTH", "CHARACTER", "CHARACTER_LENGTH",
                "CHARACTER_SET_CATALOG", "CHARACTER_SET_NAME", "CHARACTER_SET_SCHEMA",
                "CHARACTERISTICS", "CHARACTERS", "CHECK", "CLASS_ORIGIN", "CLOB",
                "CLOSE", "COALESCE", "COBOL", "COLLATE", "COLLATION",
                "COLLATION_CATALOG", "COLLATION_NAME", "COLLATION_SCHEMA", "COLLECT",
                "COLUMN", "COLUMN_NAME", "COMMAND_FUNCTION", "COMMAND_FUNCTION_CODE",
                "COMMIT", "COMMITTED", "CONCAT", "CONDITION", "CONDITION_NUMBER",
                "CONNECT", "CONNECTION", "CONNECTION_NAME", "CONST", "CONSTRAINT",
                "CONSTRAINT_CATALOG", "CONSTRAINT_NAME", "CONSTRAINT_SCHEMA",
                "CONSTRAINTS", "CONSTRUCTOR", "CONTAINS", "CONTINUE", "CONVERT",
                "CORR", "CORRESPONDING", "COUNT", "COVAR_POP", "COVAR_SAMP",
                "CREATE", "CROSS", "CUBE", "CUME_DIST", "CURRENT",
                "CURRENT_CATALOG", "CURRENT_DATE",
                "CURRENT_DEFAULT_TRANSFORM_GROUP", "CURRENT_PATHCURRENT_ROLE",
                "CURRENT_TRANSFORM_GROUP_FOR_TYPE", "CURRENT_USER", "CURSOR",
                "CURSOR_NAME", "CYCLE", "DATA", "DATE", "DATETIME_INTERVAL_CODE",
                "DATETIME_INTERVAL_PRECISION", "DAY", "DEALLOCATE", "DEC",
                "DECIMAL", "DECLARE", "DEFAULT", "DEFAULTS", "DEFERRABLE",
                "DEFERRED", "DEFINED", "DEFINER", "DEGREE", "DELETE", "DENSE_RANK",
                "DEPTH", "DEREF", "DERIVED", "DESC", "DESCRIBE", "DESCRIPTOR",
                "DETERMINISTIC", "DIAGNOSTICS", "DISCONNECT", "DISPATCH", "DISTINCT",
                "DO", "DOMAIN", "DOUBLE", "DROP", "DYNAMIC", "DYNAMIC_FUNCTION",
                "DYNAMIC_FUNCTION_CODE", "EACH", "EDGE", "ELEMENT", "ELSE",
                "ELSEIF", "END", "END_FRAME", "END_PARTITION", "END-EXEC",
                "ENFORCED", "ENUM", "EQUALS", "ESCAPE", "EVERY", "EXCEPT",
                "EXCLUDE", "EXCLUDING", "EXEC", "EXECUTE", "EXISTS", "EXP",
                "EXPRESSION", "EXTERNAL", "EXTRACT", "FALSE", "FETCH", "FILTER",
                "FINAL", "FIRST", "FIRST_VALUE", "FLAG", "FLOAT", "FLOOR",
                "FOLLOWING", "FOR", "FOREACH", "FOREIGN", "FORTRAN", "FOUND",
                "FRAME_ROW", "FREE", "FROM", "FULL", "FUNCTION", "FUSION",
                "GENERAL", "GENERATED", "GET", "GLOBAL", "GO", "GOTO", "GRANT",
                "GRANTED", "GRAPH", "GROUP", "GROUPING", "GROUPS", "HAVING",
                "HIERARCHY", "HINT", "HOLD", "HOOK", "HOUR", "IDENTITY", "IF", "IFNULL",
                "IGNORE", "IMMEDIATE", "IMMEDIATELY", "IMPLEMENTATION", "IMPORT",
                "IN", "INCLUDE", "INCLUDING", "INCREMENT", "INDICATOR", "INITIALLY",
                "INNER", "INOUT", "INPUT", "INSENSITIVE", "INSERT", "INSTANCE",
                "INSTANTIABLE", "INSTEAD", "INT", "INTEGER", "INTERSECT",
                "INTERSECTION", "INTERVAL", "INTO", "INVOKER", "IS", "ISOLATION",
                "JOIN", "KEY", "KEY_MEMBER", "KEY_TYPE", "LAG", "LANGUAGE", "LARGE",
                "LAST", "LAST_VALUE", "LATERAL", "LEAD", "LEADING", "LEFT",
                "LENGTH", "LEVEL", "LIKE", "LIKE_REGEX", "LIMIT", "LIST", "LN", "LOCAL",
                "LOCALTIME", "LOCALTIMESTAMP", "LOCATOR", "LOWER", "LPAD", "LTRIM",
                "MAP", "MATCH", "MATCHED", "MAX", "MAXVALUE", "MEMBER", "MERGE",
                "MESSAGE_LENGTH", "MESSAGE_OCTET_LENGTH", "MESSAGE_TEXT", "METHOD",
                "MIN", "MINUTE", "MINVALUE", "MOD", "MODIFIES", "MODULE", "MONTH",
                "MORE", "MULTISET", "MUMPS", "NAME", "NAMES", "NAMESPACE",
                "NATIONAL", "NATURAL", "NCHAR", "NCLOB", "NESTING", "NEW", "NEXT",
                "NFC", "NFD", "NFKC", "NFKD", "NO", "NONE", "NORMALIZE",
                "NORMALIZED", "NOT", "NTH_VALUE", "NTILE", "NULL", "NULLABLE",
                "NULLIF", "NULLS", "NUMBER", "NUMERIC", "NVARCHAR", "OBJECT",
                "OCCURRENCES_REGEX", "OCTET_LENGTH", "OCTETS", "OF", "OFFSET",
                "OLD", "ON", "ONLY", "OPEN", "OPTION", "OPTIONS", "OR", "ORDER",
                "ORDERING", "ORDINALITY", "OTHERS", "OUT", "OUTER", "OUTPUT",
                "OVER", "OVERLAPS", "OVERLAY", "OVERRIDING", "PAD", "PARAMETER",
                "PARAMETER_MODE", "PARAMETER_NAME", "PARAMETER_ORDINAL_POSITION",
                "PARAMETER_SPECIFIC_CATALOG", "PARAMETER_SPECIFIC_NAME",
                "PARAMETER_SPECIFIC_SCHEMA", "PARTIAL", "PARTITION", "PASCAL",
                "PATH", "PERCENT", "PERCENT_RANK", "PERCENTILE_CONT",
                "PERCENTILE_DISC", "PERIOD", "PERSISTENT", "PLACING", "PLI",
                "PORTION", "POSITION", "POSITION_REGEX", "POWER", "PRECEDES",
                "PRECEDING", "PRECISION", "PREPARE", "PRESERVE", "PRIMARY",
                "PRIOR", "PRIVILEGES", "PROCEDURE", "PUBLIC", "RANGE", "RANK",
                "READ", "READS", "REAL", "RECURSIVE", "REF", "REFERENCES",
                "REFERENCING", "REGR_AVGX", "REGR_AVGY", "REGR_COUNT",
                "REGR_INTERCEPT", "REGR_R2", "REGR_SLOPE", "REGR_SXX", "REGR_SXY",
                "REGR_SYY", "RELATIVE", "RELEASE", "REPEATABLE", "RESPECT",
                "RESTART", "RESTRICT", "RESULT", "RETURN", "RETURNED_CARDINALITY",
                "RETURNED_LENGTH", "RETURNED_OCTET_LENGTH", "RETURNED_SQLSTATE",
                "RETURNS", "REVOKE", "RIGHT", "ROLE", "ROLLBACK", "ROLLUP",
                "ROUTINE", "ROUTINE_CATALOG", "ROUTINE_NAME", "ROUTINE_SCHEMA",
                "ROW", "ROW_COUNT", "ROW_NUMBER", "ROWS", "RPAD", "RTRIM",
                "SAVEPOINT", "SCALE", "SCHEMA", "SCHEMA_NAME", "SCOPE",
                "SCOPE_CATALOG", "SCOPE_NAME", "SCOPE_SCHEMA", "SCROLL", "SEARCH",
                "SECOND", "SECTION", "SECURITY", "SELECT", "SELF", "SENSITIVE",
                "SEQUENCE", "SERIALIZABLE", "SERVER_NAME", "SESSION",
                "SESSION_USER", "SESSION_CONTEXT", "SET", "SETS", "SIMILAR",
                "SIMPLE", "SIZE", "SMALLINT", "SOME", "SOURCE", "SPACE",
                "SPECIFIC", "SPECIFIC_NAME", "SPECIFICTYPE", "SQL", "SQLEXCEPTION",
                "SQLSTATE", "SQLWARNING", "SQRT", "ST_CIRCULARSTRING",
                "ST_COMPOUNDCURVE", "ST_CURVE", "ST_CURVEPOLYGON",
                "ST_GEOMCOLLECTION", "ST_GEOMETRY", "ST_LINESTRING",
                "ST_MULTICURVE", "ST_MULTILINESTRING", "ST_MULTIPOINT",
                "ST_MULTIPOLYGON", "ST_MULTISURFACE", "ST_POINT", "ST_POLYGON",
                "ST_SURFACE", "START", "STATE", "STATEMENT", "STATIC",
                "STDDEV_POP", "STDDEV_SAMP", "STRUCTURE", "STYLE",
                "SUBCLASS_ORIGIN", "SUBMULTISET", "SUBSTRING", "SUBSTRING_REGEX",
                "SUBSTR_AFTER", "SUBSTR_BEFORE", "SUCCEEDS", "SUM", "SWITCH",
                "SYMMETRIC", "SYSTEM", "SYSTEM_TIME", "SYSTEM_USER", "TABLE",
                "TABLE_NAME", "TABLESAMPLE", "TEMPORARY", "TEXT", "THEN", "TIES",
                "TIME", "TIMESTAMP", "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TO",
                "TOP_LEVEL_COUNT", "TRAILING", "TRANSACTION", "TRANSACTION_ACTIVE",
                "TRANSACTIONS_COMMITTED", "TRANSACTIONS_ROLLED_BACK", "TRANSFORM",
                "TRANSFORMS", "TRANSLATE", "TRANSLATE_REGEX", "TRANSLATION",
                "TREAT", "TREE", "TRIGGER", "TRIGGER_CATALOG", "TRIGGER_NAME",
                "TRIGGER_SCHEMA", "TRIM", "TRIM_ARRAY", "TRUE", "TRUNCATE", "TYPE",
                "UESCAPE", "UNBOUNDED", "UNCOMMITTED", "UNDER", "UNION", "UNIQUE",
                "UNKNOWN", "UNNAMED", "UNNEST", "UPDATE", "UPPER", "USAGE", "USER",
                "USER_DEFINED_TYPE_CATALOG", "USER_DEFINED_TYPE_CODE",
                "USER_DEFINED_TYPE_NAME", "USER_DEFINED_TYPE_SCHEMA", "USING",
                "VALUE", "VALUE_OF", "VALUES", "VAR_POP", "VAR_SAMP", "VARBINARY",
                "VARCHAR", "VARYING", "VERSIONING", "VERTEX", "VIEW", "VOID",
                "WHEN", "WHENEVER", "WHERE", "WHILE", "WIDTH_BUCKET", "WINDOW",
                "WITH", "WITHIN", "WITHOUT", "WORK", "WRITE", "YEAR", "ZONE"
        };
        Set<String> set = new HashSet<>(Arrays.asList(kws));
        KEYWORDS = Collections.unmodifiableSet(set);
    }

    // -----------------------------------------------------------------------
    // IAppKeywordScanner implementation
    // -----------------------------------------------------------------------

    /**
     * Scans a single token and adjusts its {@code str} field:
     * <ul>
     * <li>Only SQLScript tokens that are not comments and not string/quoted
     * literals are processed.</li>
     * <li>Recognised keywords → {@code str} = uppercase ({@code strUp}).</li>
     * <li>Unrecognised identifiers → {@code str} = lowercase.</li>
     * </ul>
     * Converted from ABAP: ZIF_APP_KEYWORD_SCANNER~SCAN_KEYWORD.
     */
    @Override
    public void scanKeyword(java.util.List<TokensExt> tokens, int index) throws AppException {
        TokensExt tokenExt = tokens.get(index);
        // Only process SQLScript tokens (not ABAP tokens)
        if (tokenExt.sqlscript != Sqlscript.SQLSCRIPT) {
            return;
        }
        // Skip comments and empty tokens
        if (tokenExt.comment != Comment.NONE || tokenExt.strUp.isEmpty()) {
            return;
        }
        // Skip string literals and quoted identifiers
        char first = tokenExt.strUp.charAt(0);
        if (first == '\'' || first == '"') {
            return;
        }

        // If the token is used as a schema/table qualifier (e.g. schema."col"),
        // it must not be treated as a keyword even if it appears in the keyword list.
        // The ." sequence may span across the delimiter and/or the next two tokens.
        // Also: a token immediately following a table-context keyword (FROM, AS, …)
        // is an object name, not a SQL keyword.
        TokensExt prev = (index - 1 >= 0) ? tokens.get(index - 1) : null;
        TokensExt next1 = (index + 1 < tokens.size()) ? tokens.get(index + 1) : null;
        TokensExt next2 = (index + 2 < tokens.size()) ? tokens.get(index + 2) : null;

        boolean isTableCtxKw = TABLE_CONTEXT_KEYWORDS.contains(tokenExt.strUp)
                && tokenExt.sqlscript == Sqlscript.SQLSCRIPT
                && !isFollowedByDotQuote(tokenExt, next1, next2);
        if (!isTableCtxKw && (!KEYWORDS.contains(tokenExt.strUp)
                || isFollowedByDotQuote(tokenExt, next1, next2)
                || (!"INTO".equals(tokenExt.strUp) && isPrecededByTableContextKeyword(prev)))) {
            tokenExt.str = tokenExt.str.toLowerCase();
            tokenExt.isKeyword = false;
        } else {
            tokenExt.str = tokenExt.strUp;
            tokenExt.isKeyword = true;
        }
    }

    /**
     * Returns {@code true} when the token is immediately followed by the
     * two-character sequence {@code ."}, regardless of how the tokeniser has
     * split it across the delimiter and the next one or two tokens.
     *
     * <p>
     * The possible layouts are:
     * <ol>
     * <li>delimiter of {@code cur} starts with {@code ."} — both characters
     * are in the delimiter.</li>
     * <li>delimiter is {@code .} and {@code next1} starts with {@code "} —
     * the dot is in the delimiter, the quote opens the next token.</li>
     * <li>delimiter is empty/spaces-only, {@code next1} is the dot token
     * {@code .}, and {@code next1}'s delimiter starts with {@code "} —
     * the quote is already in the delimiter of the dot token.</li>
     * <li>delimiter is empty/spaces-only, {@code next1} is the dot token
     * {@code .}, and {@code next2} starts with {@code "} — both
     * characters are separate tokens with empty delimiters between them.
     * </li>
     * </ol>
     * </p>
     */
    private static boolean isFollowedByDotQuote(TokensExt cur, TokensExt next1, TokensExt next2) {
        String d0 = firstDelim(cur);

        // Case 1: delimiter of current token directly contains ."
        if (d0.startsWith(".\"")) {
            return true;
        }

        // Case 2: delimiter is "." and next token starts with "
        if (d0.equals(".") && next1 != null && next1.str.startsWith("\"")) {
            return true;
        }

        // Cases 3 & 4: delimiter is empty/whitespace-only — look at next tokens
        if (isBlankOrEmpty(d0) && next1 != null && ".".equals(next1.str)) {
            String d1 = firstDelim(next1);
            // Case 3: dot token's delimiter starts with "
            if (d1.startsWith("\"")) {
                return true;
            }
            // Case 4: dot token's delimiter is also empty — check next2
            if (isBlankOrEmpty(d1) && next2 != null && next2.str.startsWith("\"")) {
                return true;
            }
        }

        return false;
    }

    /** Returns the first entry of the delimiter list, or an empty string. */
    private static String firstDelim(TokensExt tok) {
        return (tok.delimiter == null || tok.delimiter.isEmpty()) ? "" : tok.delimiter.get(0);
    }

    /** Returns {@code true} when the string is null, empty, or all whitespace. */
    private static boolean isBlankOrEmpty(String s) {
        return s == null || s.isBlank();
    }

    /**
     * Returns {@code true} when {@code prev} is a non-null SQLScript token whose
     * upper-case text belongs to the set of keywords that introduce a table/view
     * name (FROM, AS, UPDATE, INSERT, DELETE, UPSERT, OF).
     */
    private static boolean isPrecededByTableContextKeyword(TokensExt prev) {
        return prev != null
                && prev.sqlscript == Sqlscript.SQLSCRIPT
                && TABLE_CONTEXT_KEYWORDS.contains(prev.strUp);
    }

    /**
     * Returns {@code true} when {@code tokenUp} (already upper-cased) is a
     * recognised SQLScript / SQL keyword.
     * Converted from ABAP: ZIF_APP_KEYWORD_SCANNER~IS_KEYWORD.
     */
    @Override
    public boolean isKeyword(String tokenUp) {
        return KEYWORDS.contains(tokenUp);
    }
}
