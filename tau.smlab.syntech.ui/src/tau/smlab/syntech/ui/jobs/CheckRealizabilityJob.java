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

import tau.smlab.syntech.bddgenerator.energy.BDDEnergyReduction;
import tau.smlab.syntech.games.gr1.GR1Game;
import tau.smlab.syntech.games.gr1.GR1GameEnergyADD;
import tau.smlab.syntech.games.gr1.GR1GameExperiments;
import tau.smlab.syntech.games.gr1.GR1GameImplC;
import tau.smlab.syntech.games.gr1.GR1GameMemoryless;
import tau.smlab.syntech.games.gr1.GR1StarGameMemoryless;
import tau.smlab.syntech.jtlv.BDDPackage;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.ui.preferences.PreferencePage;
public class CheckRealizabilityJob extends SyntechJob {

	@Override
	protected void doWork() {
		GR1Game gr1;

		// check for existential guarantees
		if(model.getSys().existReqNum() > 0) {
			printToConsole("GR1StarGameMemoryless");
			gr1 = new GR1StarGameMemoryless(model);
		}
		// check for weights or special reordering
		else if (model.getWeights() != null && PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD)) {
			printToConsole("GR1GameEnergyADD (without optimizations)");
			gr1 = new GR1GameEnergyADD(model, gi.getEnergyBound());
		/*} else if (PreferencePage.isSpecialReorderStrategy()) {
			gr1 = new GR1GameReachable(model);
			Env.TRUE().getFactory().autoReorder(BDDFactory.REORDER_NONE);
			Env.TRUE().getFactory().reorder(BDDFactory.REORDER_SIFTITE);*/
		} else {
			PreferencePage.setOptSelection();
//			resetSingleTrans(); //if decomposed transitions are used, then free the single transitions
			if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD)) {
				printToConsole("GR1GameImplC with memory");
				gr1 = new GR1GameImplC(model);
			} else if (PreferencePage.hasOptSelection()) {
				printToConsole("GR1GameExperiments (with optimizations)");
				gr1 = new GR1GameExperiments(model);
			} else {
				printToConsole("GR1GameMemoryless");
				gr1 = new GR1GameMemoryless(model);
			}
		}
		
		this.isRealizable = false;
		String message = "Specification is unrealizable.";

		if (gr1.checkRealizability()) {
			this.isRealizable = true;
			message = "Specification is realizable.";
			if (model.getWeights() != null) {
				message += " System wins with minimal initial credit ";
				if(PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD)) {
					message += ((GR1GameEnergyADD)gr1).getMinWinInitCred();
				}
				else {
					message += BDDEnergyReduction.getMinWinCred(model, gr1.sysWinningStates()).toStringWithDomains(Env.stringer);
				}
			}
		}
		printToConsole(message);
		Env.resetEnv();
	}

	@Override
	public boolean needsBound() {
		return true;
	}

}
