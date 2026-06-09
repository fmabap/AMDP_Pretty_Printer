package com.github.fmabap.amdpprettyprinter.eclipse.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import com.github.fmabap.amdpprettyprinter.eclipse.Activator;
import com.github.fmabap.amdpprettyprinter.prettyprinter.ISettings;

/**
 * Preference page for the AMDP Pretty Printer plugin.
 * Accessible via Window &rarr; Preferences &rarr; AMDP Pretty Printer.
 */
public class AmdpPreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public AmdpPreferencePage() {
		super(GRID);
	}

	@Override
	public void init(IWorkbench workbench) {
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
		setDescription("Configure the line-break-after-comma behaviour of the AMDP Pretty Printer.");
	}

	@Override
	protected void createFieldEditors() {
		addField(new RadioGroupFieldEditor(
			PreferenceConstants.LB_AFTER_COMMA_RULE,
			"Line break after comma rule:",
			1,
			new String[][] {
				{"Always insert line break after comma",                              ISettings.LB_RULE_ALWAYS_LINE_BREAK},
				{"No line break after comma",                                         ISettings.LB_RULE_NO_LINE_BREAK},
				{"Dependent on closing bracket only",                                 ISettings.LB_RULE_DEP_ON_CLS_BRACKET_ONLY},
				{"Dependent on closing bracket and sub-function",                     ISettings.LB_RULE_DEP_ON_CLS_BRACKET_AND_SUB_FU},
				{"Dependent on closing bracket, sub-function and keyword",            ISettings.LB_RULE_DEP_ON_CLS_BR_SF_AND_KEYWRD}
			},
			getFieldEditorParent()
		));

		Label separator = new Label(getFieldEditorParent(), SWT.SEPARATOR | SWT.HORIZONTAL);
		separator.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 2, 1));

		addField(new BooleanFieldEditor(
			PreferenceConstants.TRACE,
			"Enable trace output (prints token/rule details to stdout)",
			getFieldEditorParent()
		));

		Label separator2 = new Label(getFieldEditorParent(), SWT.SEPARATOR | SWT.HORIZONTAL);
		separator2.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 2, 1));

		Label descLabel = new Label(getFieldEditorParent(), SWT.WRAP);
		descLabel.setText(
			"Always insert line break after comma:\n" +
			"    A line break is added after every comma.\n\n" +
			"No line break after comma:\n" +
			"    No line break is ever added after a comma.\n\n" +
			"Dependent on closing bracket only:\n" +
			"    No line break for simple functions if:\n" +
			"    \u2022 the function contains no SELECT or BY statement\n" +
			"    \u2022 the closing bracket is originally on the same row as the function name\n\n" +
			"Dependent on closing bracket and sub-function:\n" +
			"    Like above, and additionally:\n" +
			"    \u2022 a possible sub-function contains no comma\n\n" +
			"Dependent on closing bracket, sub-function and keyword:\n" +
			"    Like above, and additionally:\n" +
			"    \u2022 the function contains at most one keyword in the brackets"
		);
		descLabel.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 2, 1));
	}
}
