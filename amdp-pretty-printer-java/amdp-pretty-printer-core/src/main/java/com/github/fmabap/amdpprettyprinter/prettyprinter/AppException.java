package com.github.fmabap.amdpprettyprinter.prettyprinter;

/**
 * Exception class for the AMDP Pretty Printer.
 * Converted from ABAP class ZCX_APP_EXCEPTION.
 */
public class AppException extends Exception {

    private static final long serialVersionUID = 1L;

    public AppException(String message) {
        super(message);
    }

    public AppException(String message, Throwable cause) {
        super(message, cause);
    }
}
