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

import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.Path;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.bddgenerator.BDDEnergyReduction;
import tau.smlab.syntech.games.controller.symbolic.SymbolicController;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerConstruction;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerReaderWriter;
import tau.smlab.syntech.games.gr1.GR1Game;
import tau.smlab.syntech.games.gr1.GR1GameEnergyADD;
import tau.smlab.syntech.games.gr1.GR1GameExperiments;
import tau.smlab.syntech.games.gr1.GR1GameImplC;
import tau.smlab.syntech.games.gr1.GR1SymbolicControllerConstruction;
import tau.smlab.syntech.jtlv.BDDPackage;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.ui.datapreparation.DataPreparation;
import tau.smlab.syntech.ui.preferences.PreferencePage;

public class SynthesizeSymbolicControllerJob extends SyntechJob {

	@Override
	protected void doWork() {
		GR1Game gr1;

		// initialize game based on configuration
		if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD) &&
				model.getWeights() != null) {
			printToConsole("GR1GameEnergyADD (without optimizations)");
			gr1 = new GR1GameEnergyADD(model, gi.getEnergyBound());
		}
		else {
			PreferencePage.setOptSelection();
			if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD)) {
				printToConsole("GR1GameImplC with memory");
				gr1 = new GR1GameImplC(model);
			}
			else {
				String GR1SolverMsg = "GR1GameExperiments (" + (PreferencePage.hasOptSelection() ? "with" : "without") + " optimizations)";
				printToConsole(GR1SolverMsg);
				gr1 = new GR1GameExperiments(model);
			}
		}

		// play actual game
		if (gr1.checkRealizability()) {
			this.isRealizable = true;
			
			if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD) &&
					model.getWeights() != null) {
				((GR1GameEnergyADD)gr1).flattenGameMemoryTerminalsToVariables();
				((GR1GameEnergyADD)gr1).updateSysTransWithEnergyConstraints();
				((GR1GameEnergyADD)gr1).setSysWinningStatesWithFlatCredits();
			}

			//Env.disableReorder(); //commented out as this lead to bad performance of controller construction

			SymbolicControllerConstruction cc = new GR1SymbolicControllerConstruction(gr1.getMem(), model);
			SymbolicController ctrl = cc.calculateSymbolicController();


			// if in Energy game then restrict initial states to minimum initial energy credit 
			if (model.getWeights() != null) {
				BDD minWinCred;
				if (PreferencePage.getBDDPackageSelection().equals(BDDPackage.CUDD_ADD)) {
					minWinCred = Env.getBDDValue("energyVal", (int)((GR1GameEnergyADD)gr1).getMinWinInitCred());
				} else {
					minWinCred = BDDEnergyReduction.getMinWinCred(model, gr1.sysWinningStates());					
				}
				ctrl.initial().andWith(minWinCred);
			}

			gr1.free();

			if (PreferencePage.isDeterminize()) {
				printToConsole("Determinizing the controller.");
				BDD tmp = ctrl.initial();
				ctrl.setInit(det(tmp, model.getSys().moduleUnprimeVars()));
				tmp.free();

				tmp = ctrl.trans();
				ctrl.setTrans(det(tmp, model.getSys().modulePrimeVars()));
				tmp.free();
			}

			model.free();

			// create symbolic controller if not exists
			IFolder outFolder = specFile.getParent().getFolder(new Path("out"));
			if (!outFolder.exists()) {
				try {
					outFolder.create(false, false, null);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}

			// write the actual symbolic controller BDDs and doms
			try {
				SymbolicControllerReaderWriter.writeSymbolicController(ctrl, model, outFolder.getLocation().toString());
				outFolder.refreshLocal(IResource.DEPTH_ONE, null);
			} catch (Exception e) {
				e.printStackTrace();
			}

			// copy spec into output folder of symbolic controller
			try {
				InputStream specZip = DataPreparation.getSpectraFilesAsZip(this.specFile);
				IFile p = outFolder.getFile("spec.zip");
				if (p.exists()) {
					p.delete(true, null);
				}
				p.create(specZip, true, null);
				specZip.close();
			} catch (Exception e) {
				e.printStackTrace();
			}

			// clean up the BDDs
			Env.resetEnv();
//			if (PreferencePage.isSpecialReorderStrategy()) {
//				printToConsole("Now trying to minimize the controller.");
//				Env.TRUE().getFactory().reorderVerbose(2);
//				Env.TRUE().getFactory().autoReorder(BDDFactory.REORDER_SIFT);
//				try {
//					SymbolicController c = SymbolicControllerReaderWriter
//							.readSymbolicController(outFolder.getLocation().toString());
//					Env.TRUE().getFactory().reorder(BDDFactory.REORDER_SIFT);
//					SymbolicControllerReaderWriter.writeSymbolicController(c, outFolder.getLocation().toString());
//				} catch (IOException e) {
//					e.printStackTrace();
//				}
//			}
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

	/**
	 * determinize b
	 * 
	 * @param b
	 * @param vars
	 * @return
	 */
	private BDD det(BDD b, BDDVarSet vars) {
		// set new varorder where vars are at the end
		Env.disableReorder();
		int[] oldOrder = b.getFactory().getVarOrder();
		List<Integer> newOrder = Arrays.stream(oldOrder).boxed().collect(Collectors.toList());
		List<Integer> varsL = Arrays.stream(vars.toArray()).boxed().collect(Collectors.toList());
		newOrder.removeAll(varsL);
		newOrder.addAll(varsL);
		b.getFactory().setVarOrder(newOrder.stream().mapToInt(i->i).toArray());

		BDD det = b.id();
		for (int var : vars.toArray()) {
			BDD tmp = det;
			det = det(tmp, var);
			tmp.free();
		}
		//b.getFactory().setVarOrder(oldOrder);
		return det;
	}

	/**
	 * determinizes b to a fresh BDD
	 * @param b
	 * @param var
	 * @return
	 */
	private BDD det(BDD b, int var) {

		if (b.isZero()) {
			return b.id();
		}

		if (b.isOne()) {
			return b.getFactory().nithVar(var).id();
		}

		// var appears so make decision one option
		// this can only work because of varorder
		if (b.var() == var) {
			BDD blow = b.low();
			if (!blow.isZero()) {
				// take low branch regardless of high branch        
				BDD res = b.getFactory().ithVar(var);
				res = res.ite(Env.FALSE(), blow);
				blow.free();
				return res;
			} else {
				// there is only one choice for b so it is fine
				return b.id();
			}
		}


		// var did not appear
		if (passedVar(b, var)) {
			BDD res = b.getFactory().ithVar(var);
			res = res.ite(Env.FALSE(), b);
			return res;
		} else {
			// determinize children
			BDD res = b.getFactory().ithVar(b.var());
			BDD bhi = b.high();
			BDD high = det(bhi, var);
			bhi.free();
			BDD blo = b.low();
			BDD low = det(blo, var);
			blo.free();
			res = res.ite(high, low);
			high.free();
			low.free();
			return res;
		}
	}

	/**
	 * did var only appear above b?
	 * 
	 * @param b
	 * @param var
	 * @return
	 */
	private boolean passedVar(BDD b, int var) {
		int lvlB = b.getFactory().var2Level(b.var());
		int lvlVar = b.getFactory().var2Level(var);
		return lvlB > lvlVar;
	}

}
