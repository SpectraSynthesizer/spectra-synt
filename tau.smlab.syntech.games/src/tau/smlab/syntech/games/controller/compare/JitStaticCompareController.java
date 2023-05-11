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

package tau.smlab.syntech.games.controller.compare;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import net.sf.javabdd.BDD.BDDIterator;
import tau.smlab.syntech.games.controller.Controller;
import tau.smlab.syntech.games.controller.StaticController;
import tau.smlab.syntech.games.controller.jits.BasicJitController;
import tau.smlab.syntech.jtlv.Env;

public class JitStaticCompareController implements Controller {
	
	private StaticController staticController = new StaticController();
	private BasicJitController jitController;
	
	public JitStaticCompareController(BasicJitController jitController) {
		this.jitController = jitController;
	}
	
	private BDDVarSet envVars;
	private BDDVarSet sysVars;
	
	@Override
	public void load(String folder, String name, Map<String, String[]> sysVars, Map<String, String[]> envVars) {
		
		this.envVars = Env.getEmptySet();
		this.sysVars = Env.getEmptySet();

		for (String sysVar : sysVars.keySet()) {
			if (!"Zn".equals(sysVar)) this.sysVars.unionWith(Env.getVar(sysVar).getDomain().set());
		}
		for (String envVar : envVars.keySet()) {
			this.envVars.unionWith(Env.getVar(envVar).getDomain().set());
		}
		
		jitController.load(folder, name, sysVars, envVars);
		staticController.load(folder, name, sysVars, envVars);
	}
	

	@Override
	public BDD next(BDD currentState, BDD inputs) {
		
		final String JITS_EXCLUDED = "JIT state not in original states!";
		final String ORIG_EXCLUDED = "Original state not in JIT states!";
		
		final int SUCC_DEPTH = 3;
		final int PRED_DEPTH = 3;
		long start, end;
		float sec;
		
		
		start = System.currentTimeMillis();
		
		System.out.println();
		BDD succJits = jitController.kSucc(currentState, SUCC_DEPTH);
		
		end = System.currentTimeMillis();
		sec = (end - start) / 1000F;
		System.out.println("JITS successors: " + sec + " seconds");
		
		start = System.currentTimeMillis();
		
		System.out.println();
		BDD predJits = jitController.kPred(currentState, PRED_DEPTH);
		
		end = System.currentTimeMillis();
		sec = (end - start) / 1000F;
		System.out.println("JITS predecessors: " + sec + " seconds");
		
		
		BDD nextJits = jitController.next(currentState, inputs);
		
		BDD statesToReturn = nextJits.id();
		
		
		start = System.currentTimeMillis();
		
		System.out.println();
		BDD succOrig = staticController.kSucc(currentState, SUCC_DEPTH);
		
		end = System.currentTimeMillis();
		sec = (end - start) / 1000F;
		System.out.println("Static successors: " + sec + " seconds");
		
		start = System.currentTimeMillis();
		
		System.out.println();
		BDD predOrig = staticController.kPred(currentState, PRED_DEPTH);
		
		end = System.currentTimeMillis();
		sec = (end - start) / 1000F;
		System.out.println("Static predecessors: " + sec + " seconds");
		
		
		BDD nextOrig = staticController.next(currentState, inputs);		
		
		
		// Use this spot to compare next states
		System.out.println("Comparing next states: JITS should be in original");
		compareBDDs(nextJits, nextOrig, "nextJits", "nextOrig", JITS_EXCLUDED, ORIG_EXCLUDED, false);
		
		
		System.out.println();
		System.out.println("Comparing successors: JITS should be in original");
		compareBDDs(succJits, succOrig, "succJits", "succOrig", JITS_EXCLUDED, ORIG_EXCLUDED, !jitController.isEager());

		succJits.free();
		succOrig.free();
		
		
		
		System.out.println("Comparing predecessors: JITS should be in original");
		compareBDDs(predJits, predOrig, "predJits", "predOrig", JITS_EXCLUDED, ORIG_EXCLUDED, !jitController.isEager());
		
		predJits.free();
		predOrig.free();
		
		
		return statesToReturn;
	}
	
	
	public void compareBDDs(BDD bdd1, BDD bdd2, String bdd1Name, String bdd2Name, String message1, String message2, Boolean compareBoth) {
		
		List<BDD> bdd1States = null;
		List<BDD> bdd2States = null;
		
		BDD bdd2NotInBdd1 = bdd2.and(bdd1.not()).exist(envVars);
		BDD bdd1NotInBdd2 = bdd1.and(bdd2.not()).exist(envVars);
		
		if (!bdd1NotInBdd2.isZero()) {
			
			System.out.println(String.format("Checking directly with bdd inclusion why %s is not fully inside %s", bdd1Name, bdd2Name));
			System.out.println("Enumerating bdd1 and bdd2");
		
			bdd1States = enumerateBDD(bdd1, sysVars);
			bdd2States = enumerateBDD(bdd2, sysVars);
			
			for (BDD bdd1State : bdd1States) {
				if (!bdd2States.contains(bdd1State)) {
					System.out.println(message1);
					System.out.println(bdd1State);
					System.out.println();
					
					throw new RuntimeException();
//					break;
				}
			}
		}
		
		// Comparing the other direction
		if (compareBoth && !bdd2NotInBdd1.isZero()) {
	
			System.out.println("Compare second part");
			System.out.println(String.format("Checking directly with bdd inclusion why %s is not fully inside %s", bdd2Name, bdd1Name));
							
			if (bdd1States == null || bdd2States == null) {
				System.out.println("Enumerating bdd1 and bdd2");
				bdd1States = enumerateBDD(bdd1, sysVars);
				bdd2States = enumerateBDD(bdd2, sysVars);
			}

			for (BDD bdd2State : bdd2States) {
				if (!bdd1States.contains(bdd2State)) {
					System.out.println(message2);
					System.out.println(bdd2State);
					System.out.println();
					
					throw new RuntimeException();
//						break;
				}
			}
		}
		
		bdd1NotInBdd2.free();
		bdd2NotInBdd1.free();
		
		if (bdd1States != null) {
			for (BDD state : bdd1States) {
				state.free();
			}
		}
		
		if (bdd2States != null) {
			for (BDD state : bdd2States) {
				state.free();
			}
		}
	}


	@Override
	public void free() {
		jitController.free();
		staticController.free();
	}

	@Override
	public void init(BDD currentState) {
		staticController.init(currentState);
		jitController.init(currentState);
	}

	@Override
	public BDD transitions() {
		return jitController.transitions();
	}

	@Override
	public BDD initial() {
		return jitController.initial();
	}

	@Override
	public void saveState() {
		jitController.saveState();
	}

	@Override
	public void loadState() {
		jitController.loadState();
	}


	public static List<BDD> enumerateBDD(BDD bdd, BDDVarSet varSet) {
		BDDIterator iter = bdd.iterator(varSet);
		List<BDD> states = new ArrayList<>();

		while (iter.hasNext()) {
			BDD next = iter.next();
			states.add(next);
		}
		
		return states;
	}

	@Override
	public BDD succ(BDD from) {
		return jitController.succ(from);
	}

	@Override
	public BDD pred(BDD to) {
		return jitController.pred(to);
	}
}
