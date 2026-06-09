package com.github.fmabap.amdpprettyprinter.prettyprinter;

/**
 * Interface for a keyword scanner.
 * Converted from ABAP interface ZIF_APP_KEYWORD_SCANNER.
 */
public interface IKeywordScanner {

    /**
     * Marks keywords in the given token by converting the token text to
     * upper case if it matches a known keyword, or to lower case otherwise.
     * Only SQLScript tokens that are not comments and not string/quoted
     * literals are processed.
     * Converted from ABAP: SCAN_KEYWORD.
     *
     * @param tokens The complete token list.
     * @param index  Index of the token to scan within {@code tokens}.
     * @throws AppException if processing fails.
     */
    void scanKeyword(java.util.List<TokensExt> tokens, int index) throws AppException;

    /**
     * Returns {@code true} when the given upper-case token string is a known
     * keyword.
     * Converted from ABAP: IS_KEYWORD.
     *
     * @param tokenUp The upper-case token string.
     * @return {@code true} if the token is a keyword.
     */
    boolean isKeyword(String tokenUp);
}
