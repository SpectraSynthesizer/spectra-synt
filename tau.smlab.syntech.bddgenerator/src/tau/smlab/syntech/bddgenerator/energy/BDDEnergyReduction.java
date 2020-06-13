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

package tau.smlab.syntech.bddgenerator.energy;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.bddgenerator.BDDGenerator;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.gamemodel.PlayerModule.TransFuncType;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.ModuleVariableException;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

/**
 * A reduction of games with an energy objective to games where a bounded energy objective is encoded into the
 * statespace and safety constraints.
 * 
 * For every entry in the weight map we add a constraint that the required energy value of the predecessor state is
 * greater or equal to the one of the successor.
 *
 */
public class BDDEnergyReduction {

	private static final String ENERGY_VAL_PRIME = "energyVal'";
	private static final String ENERGY_VAL = "energyVal";

	/**
	 *  The PlayerModule is updated in place with a new variable 'energyVal' that represents initial credits of states.
	 *  The domain of 'energyVal' will be in [0, bound].
	 *  
	 * @param sys the module of the system player
	 * @param weights the weights map of the game model
	 * @param bound the energy upper bound
	 * @param groupVars if true, then the unprimed and primed variables that encode the domain of 'energyVal' will be grouped into a fixed variable block for reordering
	 * @return an empty list (behavior info is not created)
	 */
	public static List<BehaviorInfo> reduce(PlayerModule sys, Map<Integer, BDD> weights, int bound, boolean groupVars) {
		return reduce(sys, weights, bound, groupVars, false);
	}
	
	/**
	 * The PlayerModule is updated in place with a new variable 'energyVal' that represents initial credits of states.
	 * The domain of 'energyVal' will be in [0, bound].
	 * 
	 * 
	* @param sys the module of the system player 
	 * @param weights the weight map of the game model
	 * @param bound the energy upper bound
	 * @param groupVars if true, then the unprimed and primed variables that encode the domain of 'energyVal' will be grouped into a fixed variable block for reordering
	 * @param addBehaviorInfo whether to generate BehaviorInfo(s) of the newly added safety constraints
	 * @return a list of the new behavior info (if addBehaviorInfo was set to true)
	 */
	public static List<BehaviorInfo> reduce(PlayerModule sys, Map<Integer, BDD> weights, int bound, boolean groupVars, boolean addBehaviorInfo) {
		List<BehaviorInfo> info = new ArrayList<BehaviorInfo>();
		if (weights == null) {
			// nothing to do here
			return info;
		}

		try {
			if(groupVars) {
				Env.disableReorder();
			}
			ModuleBDDField energyValField = sys.addVar(ENERGY_VAL, 0, bound, true);
			
			//In the case of decomposed transitions, add domain constraints
			if (sys.getTransFuncType() == TransFuncType.DECOMPOSED_FUNC) {
				sys.addToTransList(energyValField.getDomain().domain().and(energyValField.getOtherDomain().domain()));
			} else if (sys.getTransFuncType() == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
				sys.addToPartTransList(energyValField.getDomain().domain().and(energyValField.getOtherDomain().domain()));
			}
			
			if(groupVars) {
				BDDVarSet energyValDomVars = energyValField.getDomain().set().union(energyValField.getOtherDomain().set());
				Env.TRUE().getFactory().addVarBlock(energyValDomVars, true);
				energyValDomVars.free();
				Env.enableReorder();
			}
		} catch (Exception e) {
			e.printStackTrace();
			return info;
		}

		Variable energyVal = new Variable(ENERGY_VAL, new TypeDef(0, bound));
		// add constraint of energyVal update for every weight 
		for (Integer w : weights.keySet()) {
			if (w > bound) {
				// optimization for weights above bound: all values of y' in [0, c] allowed
				continue;
			} else if (w < -bound) {
				// optimization for weights below negative bound: transition is losing
				 addBddToTrans(sys, weights.get(w).not());
				//sys.conjunctTrans(model.getWeights().get(w).not());
				
				// add trace info for this weight entry
				if (addBehaviorInfo) {
					info.add(new BehaviorInfo(null, weights.get(w).not(), null, null, null, 0, true));
				}
			} else {
				// valPMinusW := energyVal' - w
				Spec valPMinusW = new SpecExp(Operator.SUBSTRACT, new VariableReference(energyVal, ENERGY_VAL_PRIME),
						new PrimitiveValue(w));
				// spec := energyVal' - w <= energyVal
				Spec spec = new SpecExp(Operator.RIGHT_BIGGER_OR_EQUALS, valPMinusW, new VariableReference(energyVal));
				BDD energyValUpdate = BDDGenerator.createBdd(spec, 0);

				BDD transForWeight = weights.get(w).id().impWith(energyValUpdate);
				// add value update to transitions
				addBddToTrans(sys, transForWeight.id());
				//sys.conjunctTrans(transForWeight.id());
				// add trace info for this weight entry
				if (addBehaviorInfo) {
					info.add(new BehaviorInfo(null, transForWeight, null, null, null, 0, true));
				} else {
					transForWeight.free();
				}
			}
		}
		return info;
	}
	
	/**
	 * Adds restrictions required to encode the memory update function of the counter-strategy for the environment.
	 * 
	 * @param model the game model
	 * @param bound the energy upper bound
	 * @throws ModuleVariableException if the energy reduction has not been applied
	 */
	public static void updateSysIniTransWithEngConstraintsForCounterStrategy(GameModel model, int bound) throws ModuleVariableException {
		Map<Integer, BDD> weights = model.getWeights();
		if (weights == null) {
			// nothing to do here
			return;
		}
		if(!model.getSys().hasVar(ENERGY_VAL)) {
			throw new ModuleVariableException(ENERGY_VAL + " system variable does not exist");
		}
		
		
		//update system initial states to 'energyVal=bound'
		model.getSys().conjunctInitial(Env.getBDDValue(ENERGY_VAL, bound));
				
		Variable energyVal = new Variable(ENERGY_VAL, new TypeDef(0, bound));
		// add constraint of energyVal update for every weight 
		for (Integer w : weights.keySet()) {
			if (w >= bound) {
				//Spec nextEnergyLevelEqBound = new SpecExp(Operator.EQUALS, new VariableReference(energyVal, "energyVal'"), new PrimitiveValue(bound));
				//BDD energyLevelUpdate = BDDGenerator.createBdd(nextEnergyLevelEqBound, 0);
				BDD transForWeight = weights.get(w).id().impWith(Env.getBDDValue(ENERGY_VAL_PRIME, bound));
				addBddToTrans(model.getSys(), transForWeight);
			}
			else if (w < -bound) {
				continue;	
			}
			else { // -bound <= w <= bound-1
				
				// valPMinusW := energyVal' - w
				Spec nextEnergyLevel = new SpecExp(Operator.ADD, new VariableReference(energyVal, ENERGY_VAL),
						new PrimitiveValue(w));
				
				// specAtMostBound := energyVal + w <= bound
				Spec specAtMostBound = new SpecExp(Operator.RIGHT_BIGGER_OR_EQUALS, nextEnergyLevel, new PrimitiveValue(bound));
				BDD energyLevelAtMostBound = BDDGenerator.createBdd(specAtMostBound, 0);
				
			    //specEnergyLevelEquals := energyVal' = energyVal + w
				Spec specEnergyLevelEquals = new SpecExp(Operator.EQUALS, new VariableReference(energyVal, ENERGY_VAL_PRIME), nextEnergyLevel);
				BDD energyLevelEquals = BDDGenerator.createBdd(specEnergyLevelEquals, 0);
				
				//energyLevelUpdate := if (energyVal + w <= bound) then  (energyVal' = energyVal + w); otherwise, energyVal' = bound
				BDD energyLevelUpdate = energyLevelAtMostBound.ite(energyLevelEquals, Env.getBDDValue(ENERGY_VAL_PRIME, bound));
				
				BDD transForWeight = weights.get(w).id().impWith(energyLevelUpdate);
				// add value update to transitions
				addBddToTrans(model.getSys(), transForWeight);
			}
		}
		
	}
	

	/**
	 * Computes a BDD representation of the minimal winning credit common to all initial states
	 * 
	 * @param model
	 *          the game model of the Energy game
	 * @param win
	 *          the winning states of the game
	 * @return FALSE if no common winning credit exists and otherwise a BDD with a single minimal valu of variable
	 *         energyVal
	 */
	public static BDD getMinWinCred(GameModel model, BDD win) {
		PlayerModule sys = model.getSys();
		PlayerModule env = model.getEnv();

		ModuleBDDField energyVal = Env.getVar(ENERGY_VAL);
		// get winning energyVal for all winning initial states
		BDD sysWin = win.and(sys.initial());
		BDD minWinCred = env.initial().id().impWith(sysWin.exist(sys.moduleUnprimeVars().minus(energyVal.support())))
				.forAll(env.moduleUnprimeVars());
		sysWin.free();

		if (minWinCred.isZero()) {
			return Env.FALSE();
		}
		// restrict to global minimum of energyVal values
		int[] ivar = energyVal.getDomain().vars();
		for (int n = ivar.length - 1; n >= 0; n--) {
			BDD nithVar = energyVal.getDomain().getFactory().nithVar(ivar[n]);
			if (!minWinCred.and(nithVar).isZero()) {
				minWinCred.andWith(nithVar);
			} else {
				minWinCred.andWith(nithVar.not());
				nithVar.free();
			}
		}
		return minWinCred;
	}
	
	/**
	 * Handles the optimizations of the transition relation
	 * 
	 * @param sys
	 * @param bdd
	 */
	private static void addBddToTrans(PlayerModule sys, BDD bdd) {
		//we always construct a single transition relation. If a decomposition is used, then we add the current safety constraint
		//to the decomposed transitions' list
		sys.conjunctTrans(bdd.id());
		
		if (sys.getTransFuncType() == TransFuncType.DECOMPOSED_FUNC) {
			sys.addToTransList(bdd);
		} else if (sys.getTransFuncType() == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
			sys.addToPartTransList(bdd);
		}
		else { //sys.getTransFuncType() == TransFuncType.SINGLE_FUNC
			bdd.free();
		}
//ORIGINAL IMPLEMENTATION
//		if (sys.getTransFuncType() == TransFuncType.SINGLE_FUNC) {
//			sys.conjunctTrans(bdd);
//		} else {
//			if (PlayerModule.TEST_MODE) {
//				sys.conjunctTrans(bdd.id());
//			}
//
//			if (sys.getTransFuncType() == TransFuncType.DECOMPOSED_FUNC) {
//				sys.addToTransList(bdd);
//			} else if (sys.getTransFuncType() == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
//				sys.addToPartTransList(bdd);
//			}
//		}
	}

}
