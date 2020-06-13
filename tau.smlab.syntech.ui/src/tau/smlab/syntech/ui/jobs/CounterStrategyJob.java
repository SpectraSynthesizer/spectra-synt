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

package tau.smlab.syntech.ui.jobs;

import java.io.PrintStream;

import org.eclipse.ui.console.IOConsoleOutputStream;

import tau.smlab.syntech.bddgenerator.energy.BDDEnergyReduction;
import tau.smlab.syntech.games.controller.enumerate.ConcreteControllerConstruction;
import tau.smlab.syntech.games.controller.enumerate.printers.MAAMinimizeAutomatonPrinter;
import tau.smlab.syntech.games.controller.enumerate.printers.SimpleTextPrinter;
import tau.smlab.syntech.games.rabin.RabinConcreteControllerConstruction;
import tau.smlab.syntech.games.rabin.RabinGame;
import tau.smlab.syntech.jtlv.BDDPackage;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.ModuleVariableException;
import tau.smlab.syntech.ui.preferences.PreferencePage;

public class CounterStrategyJob extends SyntechJob {

	@Override
	protected void doWork() {

		if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD) && model.getWeights() != null) {
			BDDEnergyReduction.reduce(model.getSys(), model.getWeights(), gi.getEnergyBound(), PreferencePage.isGroupVarSelection());
		}

		// play actual game
		RabinGame rabin = new RabinGame(model);
		if (rabin.checkRealizability()) {
			this.isRealizable = false;

			if(model.getWeights() != null) {
				try {
					BDDEnergyReduction.updateSysIniTransWithEngConstraintsForCounterStrategy(model, gi.getEnergyBound());
				} catch (ModuleVariableException e) {
					e.printStackTrace();
				}
			}

			Env.disableReorder();

			ConcreteControllerConstruction cc = new RabinConcreteControllerConstruction(rabin.getMem(), model);
			IOConsoleOutputStream cout = console.newOutputStream();
			PrintStream out = new PrintStream(cout);
			try {
				if ("CMP".equals(PreferencePage.getConcreteControllerFormat())) {
					MAAMinimizeAutomatonPrinter.REMOVE_DEAD_STATES = false;
					new MAAMinimizeAutomatonPrinter(model).printController(out, cc.calculateConcreteController());
				} else if ("JTLV".equals(PreferencePage.getConcreteControllerFormat())) {
					new SimpleTextPrinter().printController(out, cc.calculateConcreteController());
				}
				out.close();
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else {
			this.isRealizable = true;
			printToConsole("The selected specification is realizable.");
		}
		model.free();
		rabin.free();
	}

	@Override
	public boolean needsBound() {
		return true;
	}

}
