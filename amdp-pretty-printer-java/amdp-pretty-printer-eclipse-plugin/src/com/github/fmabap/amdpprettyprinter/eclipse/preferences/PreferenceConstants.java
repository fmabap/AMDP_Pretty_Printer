package com.github.fmabap.amdpprettyprinter.eclipse.preferences;

/**
 * Preference key constants for the AMDP Pretty Printer plugin.
 */
public final class PreferenceConstants {

	/** Key for the line-break-after-comma rule (maps to {@code ISettings.LB_RULE_*}). */
	public static final String LB_AFTER_COMMA_RULE = "lbAfterCommaRule"; //$NON-NLS-1$

	/** Key for the trace flag; when {@code true} token/rule trace output is printed to stdout. */
	public static final String TRACE = "trace"; //$NON-NLS-1$

	private PreferenceConstants() {
	}
}
