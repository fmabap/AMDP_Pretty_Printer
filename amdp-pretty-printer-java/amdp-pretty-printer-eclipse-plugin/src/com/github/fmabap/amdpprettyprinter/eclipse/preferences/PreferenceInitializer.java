package com.github.fmabap.amdpprettyprinter.eclipse.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import com.github.fmabap.amdpprettyprinter.eclipse.Activator;
import com.github.fmabap.amdpprettyprinter.prettyprinter.ISettings;

/**
 * Sets default preference values for the AMDP Pretty Printer plugin.
 * Registered via the {@code org.eclipse.core.runtime.preferences} extension point.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore store = Activator.getDefault().getPreferenceStore();
		store.setDefault(PreferenceConstants.LB_AFTER_COMMA_RULE, ISettings.LB_RULE_DEP_ON_CLS_BR_SF_AND_KEYWRD);
		store.setDefault(PreferenceConstants.TRACE, false);
	}
}
