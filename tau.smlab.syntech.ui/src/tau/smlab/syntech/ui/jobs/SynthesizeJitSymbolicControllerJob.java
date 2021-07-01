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

import net.sf.javabdd.BDD;
import tau.smlab.syntech.bddgenerator.energy.BDDEnergyReduction;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerJitInfo;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerReaderWriter;
import tau.smlab.syntech.games.gr1.GR1Game;
import tau.smlab.syntech.games.gr1.GR1GameEnergyADD;
import tau.smlab.syntech.games.gr1.GR1GameExperiments;
import tau.smlab.syntech.games.gr1.GR1GameImplC;
import tau.smlab.syntech.games.gr1.GR1StarGameImplC;
import tau.smlab.syntech.games.gr1.jit.SymbolicControllerJitInfoConstruction;
import tau.smlab.syntech.jtlv.BDDPackage;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.ui.preferences.PreferencePage;

public class SynthesizeJitSymbolicControllerJob extends SyntechJob {
	
	protected final String SIZES = "sizes";
	protected final String FIXPOINTS = "fixpoints.bdd";
	protected final String TRANS = "trans.bdd";
	protected final String JUSTICE = "justice.bdd";
	protected final String VARS = "vars.doms";

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
				
				if (model.getSys().hasExistReqs()) {
					
					printToConsole("GR1StarGameImplC with memory");
					gr1 = new GR1StarGameImplC(model);
					
				} else {
					
					printToConsole("GR1GameImplC with memory");
					gr1 = new GR1GameImplC(model);
				}
				
			} else {
				
				String GR1SolverMsg = "GR1GameExperiments (" + (PreferencePage.hasOptSelection() ? "with" : "without") + " optimizations)";
				printToConsole(GR1SolverMsg);
				gr1 = new GR1GameExperiments(model);
			}
		}
		
		long start = System.currentTimeMillis();

		// play actual game
		if (gr1.checkRealizability()) {
			
			long time = System.currentTimeMillis() - start;
			printToConsole("Realizability Check: " + time + "ms");
			
			this.isRealizable = true;
			
						
			if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD) &&
					model.getWeights() != null) {
				((GR1GameEnergyADD)gr1).flattenGameMemoryTerminalsToVariables();
				((GR1GameEnergyADD)gr1).updateSysTransWithEnergyConstraints();
				((GR1GameEnergyADD)gr1).setSysWinningStatesWithFlatCredits();
			}
			
	        try {	
	        	
	            BDD minWinCred = Env.TRUE();
				// if in Energy game then restrict initial states to minimum initial energy credit 
				if (model.getWeights() != null) {
					if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD)) {
						minWinCred = Env.getBDDValue("energyVal", (int)((GR1GameEnergyADD)gr1).getMinWinInitCred());
					} else {
						minWinCred = BDDEnergyReduction.getMinWinCred(model, gr1.sysWinningStates());					
					}
				}
				
				start = System.currentTimeMillis();
				
				SymbolicControllerJitInfoConstruction jitInfoConstruction = new SymbolicControllerJitInfoConstruction(gr1.getMem(), model, minWinCred);
				SymbolicControllerJitInfo jitInfo = jitInfoConstruction.calculateJitSymbollicControllerInfo();
				
				time = System.currentTimeMillis() - start;
				printToConsole("JITS BDDs Preparation: " + time + "ms");
				
				start = System.currentTimeMillis();
				
				String location = specFile.getParent().getLocation().toOSString();
				String outLocation = location + File.separator + "out";
				
				SymbolicControllerReaderWriter.writeJitSymbolicController(jitInfo, model, outLocation, PreferencePage.isReorderBeforeSave());
				jitInfo.free();
				
				time = System.currentTimeMillis() - start;
				printToConsole("Controller save to disk: " + time + "ms");
  
	        } catch (Exception e) {
            	e.printStackTrace();
	        }
		} else {
			
			this.isRealizable = false;
			printToConsole("The selected specification is unrealizable.");

		}
		
		gr1.free();
		model.free();
		Env.resetEnv();
	}
	

	@Override
	public boolean needsBound() {
		return true;
	}

}
