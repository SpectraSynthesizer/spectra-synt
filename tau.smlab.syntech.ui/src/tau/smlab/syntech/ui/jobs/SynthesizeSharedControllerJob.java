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

import java.io.File;
import java.io.IOException;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.bddgenerator.energy.BDDEnergyReduction;
import tau.smlab.syntech.games.controller.symbolic.SymbolicController;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerConstruction;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerJitInfo;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerReaderWriter;
import tau.smlab.syntech.games.gr1.GR1Game;
import tau.smlab.syntech.games.gr1.GR1GameEnergyADD;
import tau.smlab.syntech.games.gr1.GR1GameExperiments;
import tau.smlab.syntech.games.gr1.GR1GameImplC;
import tau.smlab.syntech.games.gr1.GR1SymbolicControllerConstruction;
import tau.smlab.syntech.games.gr1.jit.SymbolicControllerJitInfoConstruction;
import tau.smlab.syntech.jtlv.BDDPackage;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.ui.preferences.PreferencePage;

public class SynthesizeSharedControllerJob extends SyntechJob {

	@Override
	protected void doWork() {
		GR1Game gr1;

		// initialize game based on configuration
		if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD) && model.getWeights() != null) {
			printToConsole("GR1GameEnergyADD (without optimizations)");
			gr1 = new GR1GameEnergyADD(model, gi.getEnergyBound());
		} else {
			PreferencePage.setOptSelection();
			if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD)) {
				printToConsole("GR1GameImplC with memory");
				gr1 = new GR1GameImplC(model);
			} else {
				String GR1SolverMsg = "GR1GameExperiments (" + (PreferencePage.hasOptSelection() ? "with" : "without")
						+ " optimizations)";
				printToConsole(GR1SolverMsg);
				gr1 = new GR1GameExperiments(model);
			}
		}

		// play actual game
		if (gr1.checkRealizability()) {
			this.isRealizable = true;

			if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD) && model.getWeights() != null) {
				((GR1GameEnergyADD) gr1).flattenGameMemoryTerminalsToVariables();
				((GR1GameEnergyADD) gr1).updateSysTransWithEnergyConstraints();
				((GR1GameEnergyADD) gr1).setSysWinningStatesWithFlatCredits();
			}

			SymbolicControllerConstruction cc = new GR1SymbolicControllerConstruction(gr1.getMem(), model);
			SymbolicController ctrl = cc.calculateSymbolicController();

			// if in Energy game then restrict initial states to minimum initial energy
			// credit
			BDD minWinCred = Env.TRUE();
			if (model.getWeights() != null) {
				if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD)) {
					minWinCred = Env.getBDDValue("energyVal", (int) ((GR1GameEnergyADD) gr1).getMinWinInitCred());
				} else {
					minWinCred = BDDEnergyReduction.getMinWinCred(model, gr1.sysWinningStates());
				}
				ctrl.initial().andWith(minWinCred);
			}
			
			SymbolicControllerJitInfoConstruction jitInfoConstruction = new SymbolicControllerJitInfoConstruction(gr1.getMem(), model, minWinCred);
			SymbolicControllerJitInfo jitInfo = jitInfoConstruction.calculateJitSymbollicControllerInfo();
			
			
			String location = specFile.getParent().getLocation().toOSString();
			String outLocation = location + File.separator + "out";
			


			gr1.free();


			try {
				SymbolicControllerReaderWriter.writeSymbolicController(ctrl, model, outLocation, PreferencePage.isReorderBeforeSave());
				SymbolicControllerReaderWriter.writeJitSymbolicController(jitInfo, model, outLocation, PreferencePage.isReorderBeforeSave());
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

			jitInfo.free();

			model.free();

			// clean up the BDDs
			Env.resetEnv();
			return;
		}
		this.isRealizable = false;

		printToConsole("The selected specification is unrealizable.");
		Env.resetEnv();
	}

	@Override
	public boolean needsBound() {
		return true;
	}

}
