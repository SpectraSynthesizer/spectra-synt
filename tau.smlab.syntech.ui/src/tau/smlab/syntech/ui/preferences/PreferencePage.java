/*
Copyright (c) since 2015, Tel Aviv University and Software Modeling Lab

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Tel Aviv University and Software Modeling Lab nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Tel Aviv University and Software Modeling Lab 
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE 
GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) 
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT 
OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
*/

package tau.smlab.syntech.ui.preferences;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import tau.smlab.syntech.gamemodel.PlayerModule.TransFuncType;
import tau.smlab.syntech.games.gr1.GR1GameExperiments;
import tau.smlab.syntech.games.rabin.RabinGame;
import tau.smlab.syntech.jtlv.BDDPackage;
import tau.smlab.syntech.jtlv.BDDPackage.BBDPackageVersion;
import tau.smlab.syntech.ui.Activator;

/**
 * This class represents a preference page that is contributed to the
 * Preferences dialog. By subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows us to create a page
 * that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the
 * preference store that belongs to the main plug-in class. That way,
 * preferences can be accessed directly via the preference store.
 */

public class PreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	public PreferencePage() {
		super(GRID);
		setPreferenceStore(Activator.getDefault().getPreferenceStore());
		setDescription("General SYNTECH preferences:");
	}

	/**
	 * Creates the field editors. Field editors are abstractions of the common GUI
	 * blocks needed to manipulate various types of preferences. Each field editor
	 * knows how to save and restore itself.
	 */

	private RadioGroupFieldEditor engine;
	private RadioGroupFieldEditor concCont;
	private RadioGroupFieldEditor opts;
	private RadioGroupFieldEditor reorder;
	private BooleanFieldEditor determinize;
	private BooleanFieldEditor reorderBeforeSave;

	public void createFieldEditors() {
		engine = new RadioGroupFieldEditor(PreferenceConstants.BDD_ENGINE_CHOICE, "BDD engine", 1,
				new String[][] { { "&JTLV package -- pure Java implementation", "JTLV" },
						{ "&CUDD package -- JNI access to C implementation", "CUDD" },
						{ "CUDD package using &ADDs -- JNI access to C implementation", "CUDD_ADD" } },
				getFieldEditorParent(), true);

		opts = new RadioGroupFieldEditor(PreferenceConstants.OPT_CHOICE, "Optimization options", 1,
				new String[][] { { "Disable optimizations", "none" }, { "All optimizations", "all" },
						{ "Algorithms optimizations", "fp_opts" },
						{ "Controlled predecessors optimizations", "cp_opts" } },
				getFieldEditorParent(), true);

		reorder = new RadioGroupFieldEditor(PreferenceConstants.REORDER_CHOICE, "Reorder Strategy", 1,
				new String[][] { { "Disable reorder (not recommended)", "none" }, { "Enable reorder", "reorder" },
						{ "Enable reorder with grouping (variables and their next state copies)", "group" },
				// { "Special reorder strategy", "special"}
				}, getFieldEditorParent(), true);

		determinize = new BooleanFieldEditor(PreferenceConstants.DETERMINIZE,
				"Determinize static symbolic controllers (can be slow)", getFieldEditorParent());
		
		reorderBeforeSave = new BooleanFieldEditor(PreferenceConstants.REORDER_BEFORE_SAVE,
				"Reorder BDD before save to reduce size", getFieldEditorParent());

		concCont = new RadioGroupFieldEditor(PreferenceConstants.CONC_CONT_FORMAT, "Concrete Controller Format", 1,
				new String[][] { { "CMP automaton (Mealy)", "CMP" }, { "JTLV text format", "JTLV" } },
				getFieldEditorParent(), true);

		addField(engine);
		addField(opts);
		addField(reorder);
		addField(determinize);
		addField(reorderBeforeSave);
		addField(concCont);

		// String engineChoice =
		// this.getPreferenceStore().getString(PreferenceConstants.BDD_ENGINE_CHOICE);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}

	public static boolean isReorderEnabled() {
		String val = Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.REORDER_CHOICE);
		return val.equals("reorder");
	}

	// public static boolean isSpecialReorderStrategy() {
	// String val = Activator.getDefault().getPreferenceStore()
	// .getString(PreferenceConstants.REORDER_CHOICE);
	// return val.equals("special");
	// }

	public static boolean isGroupVarSelection() {
		String val = Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.REORDER_CHOICE);
		return val.equals("group");
	}

	public static boolean isDeterminize() {
		return Activator.getDefault().getPreferenceStore().getBoolean(PreferenceConstants.DETERMINIZE);
	}
	
	public static boolean isReorderBeforeSave() {
		return Activator.getDefault().getPreferenceStore().getBoolean(PreferenceConstants.REORDER_BEFORE_SAVE);
	}

	public static BDDPackage getBDDPackageSelection() {
		String val = Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.BDD_ENGINE_CHOICE);
		if (val.equals("JTLV")) {
			return BDDPackage.JTLV;
		} else if (val.equals("CUDD")) {
			return BDDPackage.CUDD;
		} else if (val.equals("CUDD_ADD")) {
			return BDDPackage.CUDD_ADD;
		}

		return null;
	}

	public static BBDPackageVersion getBDDPackageVersionSelection() {
		BDDPackage engineSelection = getBDDPackageSelection();
		if (engineSelection.equals(BDDPackage.JTLV)) {
			return BBDPackageVersion.DEFAULT;
		}

		// CUDD BDD package has been selected
		return BBDPackageVersion.CUDD_3_0;
	}

	public static boolean hasOptSelection() {
		String val = Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.OPT_CHOICE);
		return !val.equals("none");
	}

	public static void setOptSelection() {
		String val = Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.OPT_CHOICE);

		boolean fp_opts = val.equals("all") || val.equals("fp_opts");
		boolean cp_opts = val.equals("all") || val.equals("cp_opts");

		GR1GameExperiments.DETECT_FIX_POINT_EARLY = fp_opts;
		GR1GameExperiments.USE_FIXPOINT_RECYCLE = fp_opts;
		GR1GameExperiments.STOP_WHEN_INITIALS_LOST = fp_opts;
		GR1GameExperiments.SIMULTANEOUS_CONJUNCTION_ABSTRACTION = cp_opts;

		RabinGame.DETECT_FIX_POINT_EARLY = fp_opts;
		RabinGame.USE_FIXPOINT_RECYCLE = fp_opts;
		RabinGame.STOP_WHEN_WIN_FROM_SOME_INITIALS = fp_opts;
		RabinGame.SIMULTANEOUS_CONJUNCTION_ABSTRACTION = cp_opts;

		// TMP - NOTE: the function relprod needed for this optimization is not
		// implemented for ADDs
		if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD)) {
			GR1GameExperiments.SIMULTANEOUS_CONJUNCTION_ABSTRACTION = false;
			RabinGame.SIMULTANEOUS_CONJUNCTION_ABSTRACTION = false;
		}
	}

	public static TransFuncType getTransFuncSelection(boolean isDDMin) {
		String val = Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.OPT_CHOICE);
		if (val.equals("all") || val.equals("cp_opts")) {
			return isDDMin ? TransFuncType.PARTIAL_DECOMPOSED_FUNC : TransFuncType.DECOMPOSED_FUNC;
		}

		return TransFuncType.SINGLE_FUNC;
	}

	// public static boolean getWellSepIncludeSys() {
	// return
	// "SYS".equals(Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.WELL_SEP_SYS));
	// }

	public static String getConcreteControllerFormat() {
		return Activator.getDefault().getPreferenceStore().getString(PreferenceConstants.CONC_CONT_FORMAT);
	}
}