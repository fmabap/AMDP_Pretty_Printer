package com.github.fmabap.amdpprettyprinter.prettyprinter;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Base class with shared helpers for PrettyPrinter unit tests.
 * Mirrors the ABAP helper method rtrim_source and the common setup pattern.
 */
abstract class PrettyPrinterTestBase {

    /**
     * Convenience factory to create a List<String> from varargs.
     */
    protected static List<String> lines(String... lines) {
        return Arrays.asList(lines);
    }

    /**
     * Removes trailing whitespace from every line in the list.
     * Corresponds to the ABAP helper method rtrim_source.
     */
    protected static List<String> rtrim(List<String> source) {
        return source.stream()
                .map(s -> s.replaceAll("\\s+$", ""))
                .collect(Collectors.toList());
    }

    /**
     * Standard settings used by most tests:
     * lineBreakAfterCommaReq=true, all "no-lb" flags=false.
     */
    protected static ISettings standardSettings() {
        return new TestSettings(true, false, false, false);
    }

    /**
     * Creates a settings object with the given flag values.
     *
     * @param lineBreakAfterCommaReq maps to is_line_break_after_comma_req()
     * @param noLbSfu                maps to is_no_lb_at_co_s_fu_dep_sfu()
     * @param noLbCbrO               maps to is_no_lb_at_co_s_fu_dep_cbr_o()
     * @param noLbSfuKw              maps to is_no_lb_at_co_s_fu_dep_sfu_kw()
     */
    protected static ISettings settings(boolean lineBreakAfterCommaReq,
            boolean noLbSfu,
            boolean noLbCbrO,
            boolean noLbSfuKw) {
        return new TestSettings(lineBreakAfterCommaReq, noLbSfu, noLbCbrO, noLbSfuKw);
    }

    /**
     * Runs the pretty printer and rtrim-normalises both the result and the
     * expected list before returning them for comparison.
     */
    protected static List<String> prettyPrint(List<String> source, ISettings settings)
            throws AppException {
        return rtrim(new PrettyPrinter().prettyPrint(source, settings));
    }
}
