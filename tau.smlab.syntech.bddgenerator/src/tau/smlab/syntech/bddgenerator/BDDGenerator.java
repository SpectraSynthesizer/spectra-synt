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

package tau.smlab.syntech.bddgenerator;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDBitVector;
import net.sf.javabdd.BDDDomain;
import net.sf.javabdd.BDDException;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.bddgenerator.energy.BDDEnergyReduction;
import tau.smlab.syntech.bddgenerator.sfa.SFAGeneratorFactory;
import tau.smlab.syntech.bddgenerator.sfa.SFAGeneratorFactory.RegExpSFAGeneratorType;
import tau.smlab.syntech.bddgenerator.sfa.SFAGeneratorFactory.TriggerSFAGeneratorType;
import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.ExistentialConstraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Player;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.model.WeightDefinition;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.gamemodel.SFAModuleConstraint;
import tau.smlab.syntech.gamemodel.PlayerModule.TransFuncType;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;
import tau.smlab.syntech.sfa.SFA;

public class BDDGenerator {

	private static final String CUDDADD_FACTORY = "CUDDFactory$CUDDADDFactory";

	/**
	 * part of spec to create trace info for (BehaviorInfo)
	 *
	 */
	public enum TraceInfo {
		NONE, SYS, ENV, AUX, ALL, SYS_AUX
	}

	/**
	 * create game model without any trace info
	 * 
	 * @param input
	 * @return
	 */
	public static GameModel generateGameModel(GameInput input) {
		return generateGameModel(input, TraceInfo.NONE);
	}

	/**
	 * creates a GameModel from a GameInput and creates BehaviorInfo for all modules mentioned in TraceInfo
	 * 
	 * @param input
	 * @param trace
	 * @return
	 */
	public static GameModel generateGameModel(GameInput input, TraceInfo trace) {
		return generateGameModel(input, trace, false, TransFuncType.SINGLE_FUNC);
	}

	/**
	 * creates a GameModel from a GameInput and creates BehaviorInfo for all modules mentioned in TraceInfo
	 * 
	 * @param input
	 * @param trace
	 * @param groupVars
	 * @return
	 */
	public static GameModel generateGameModel(GameInput input, TraceInfo trace, boolean groupVars) {
		return generateGameModel(input, trace, groupVars, TransFuncType.SINGLE_FUNC);
	}

	public static GameModel generateGameModel(GameInput input, TraceInfo trace, boolean groupVars,
			TransFuncType transFunc) {
		return generateGameModel(input, trace, groupVars, transFunc, true);
	}
	
	
	public static GameModel generateGameModel(GameInput input, TraceInfo trace, boolean groupVars, 
			TransFuncType transFunc, boolean debugLog) {
		return  generateGameModel(input, trace, groupVars, transFunc, debugLog, TriggerSFAGeneratorType
				.SIMPLE, RegExpSFAGeneratorType.SYMBOLIC);
	}
	
	/**
	 * Creates a {@link GameModel} from a {@link GameInput} and creates {@link BehaviorInfo} for all modules mentioned in {@link TraceInfo}.
	 * 
	 * @param input
	 * @param trace
	 * @param groupVars
	 *          groups primed and unprimed BDD variables
	 * @return
	 */
	public static GameModel generateGameModel(GameInput input, TraceInfo trace, boolean groupVars, 
			TransFuncType transFunc, boolean debugLog, TriggerSFAGeneratorType triggerSfaGenType, RegExpSFAGeneratorType regExpSfaGenType) {
		if (debugLog) System.out.println("groupVars = " + groupVars + ", transFunc = " + transFunc);
		GameModel model = new GameModel();

		Player env = input.getEnv();
		Player sys = input.getSys();
		Player aux = input.getAux();

		PlayerModule envMod = new PlayerModule();
		envMod.setName(input.getName() + "_env");
		PlayerModule sysMod = new PlayerModule();
		sysMod.setName(input.getName() + "_sys");

		// NOTE: if grouping variables, reorder must be disabled while the variables are being created.
		if (groupVars) {
			Env.disableReorder();
		}

		// first create all variables (might be used in expressions)
		createModuleVars(envMod, env, false);
		createModuleVars(sysMod, sys, false);
		createModuleVars(sysMod, aux, true);

		int env_vars_all = 0;
		int sys_vars_all = 0;
		if (debugLog) System.out.println("Env domains:");
		for (ModuleBDDField f : envMod.getAllFields()) {
			env_vars_all += f.getDomain().set().size() + f.getOtherDomain().set().size();
			if (debugLog) System.out.println("Domain " + f.getDomain() + ": " + f.getDomain().set()  
					+ ", OtherDomain " + f.getOtherDomain() + ": " + f.getOtherDomain().set());
		}
		if (debugLog) System.out.println("Sys domains:");
		for (ModuleBDDField f : sysMod.getAllFields()) {
			sys_vars_all += f.getDomain().set().size() + f.getOtherDomain().set().size();
			if (debugLog) System.out.println("Domain " + f.getDomain() + ": " + f.getDomain().set()  
					+ ", OtherDomain " + f.getOtherDomain() + ": " + f.getOtherDomain().set());
		}

		if (debugLog) System.out.println("Spec Stats env vars: " + env_vars_all);
		if (debugLog) System.out.println("Spec Stats sys vars: " + sys_vars_all);

		if (groupVars) {
			if (debugLog) System.out.println("Grouping primed and unprimed variables");
			createVarGroups(sysMod);
			createVarGroups(envMod);
			Env.enableReorder();
		}

		envMod.setTransFuncType(transFunc);
		sysMod.setTransFuncType(transFunc);
		if (transFunc == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
			envMod.setTransFuncType(TransFuncType.DECOMPOSED_FUNC);
		}

		// Add the domain restriction for the decomposed option
		if (transFunc == TransFuncType.DECOMPOSED_FUNC || 
				transFunc == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
			envMod.addToTransList(envMod.trans().id());
			sysMod.addToTransList(sysMod.trans().id());
			sysMod.addToPartTransList(sysMod.trans().id());
		}

		// Second, add all constraint
		if (debugLog) System.out.println("Constraints Env: ");
		boolean traceEnv = trace.equals(TraceInfo.ALL) || trace.equals(TraceInfo.ENV);
		model.getEnvBehaviorInfo().addAll(createModuleConstraints(envMod, env, false, traceEnv, false, debugLog));
		if (debugLog) System.out.println("Constraints Sys: ");
		boolean traceSys = trace.equals(TraceInfo.ALL) || trace.equals(TraceInfo.SYS) || trace.equals(TraceInfo.SYS_AUX);
		model.getSysBehaviorInfo().addAll(createModuleConstraints(sysMod, sys, false, traceSys, true, debugLog));
		if (debugLog) System.out.println("Constraints Aux: ");
		boolean traceAux = trace.equals(TraceInfo.ALL) || trace.equals(TraceInfo.AUX) || trace.equals(TraceInfo.SYS_AUX);
		model.getAuxBehaviorInfo().addAll(createModuleConstraints(sysMod, aux, true, traceAux, false, debugLog));
		
		
		// Add existential guarantees
		addExistentialGars(sysMod, sys, RegExpSFAGeneratorType.SYMBOLIC, groupVars, traceSys, model.getSysBehaviorInfo());
		
		// Add trigger constraints
		createTriggerModuleConstraints(envMod, env, sysMod, triggerSfaGenType, regExpSfaGenType, groupVars, traceEnv, traceAux,
				model.getAuxBehaviorInfo(), model.getEnvBehaviorInfo());
		createTriggerModuleConstraints(sysMod, sys, sysMod, triggerSfaGenType, regExpSfaGenType, groupVars, traceSys, traceAux,
				model.getAuxBehaviorInfo(), model.getSysBehaviorInfo());
		
		// Third, add all weights
		model.setWeights(buildWeights(input.getWeightDefs()));
		
		// If either CUDD or JTLV engines are used, apply reduction to energy (this may add safety constraints to the system module)
		if(!Env.getFactoryName().equals(CUDDADD_FACTORY)) {
			model.getAuxBehaviorInfo().addAll(BDDEnergyReduction.reduce(sysMod, model.getWeights(), input.getEnergyBound(), groupVars, traceAux));
		}

		if (transFunc == TransFuncType.DECOMPOSED_FUNC) {
			//			if (debugLog) System.out.println("calcTransQuantList env: ");
			envMod.calcTransQuantList();
			//			if (debugLog) System.out.println("calcTransQuantList sys: ");
			sysMod.calcTransQuantList();
		} else if (transFunc == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
			if (debugLog) System.out.println("incompleteCalcTransQuantList env: ");
			//      envMod.incompleteCalcTransQuantList();
			envMod.calcTransQuantList();
			if (debugLog) System.out.println("incompleteCalcTransQuantList sys: ");
			sysMod.partCalcTransQuantList();

			//      System.out.println("createPartialTransQuantList env: ");
			//      envMod.createPartialTransQuantList();
			//      System.out.println("createPartialTransQuantList sys: ");
			//      sysMod.createPartialTransQuantList();
		}

		model.setEnv(envMod);
		model.setSys(sysMod);


		return model;
	}

	/**
	 * Creates a fixed group for each variable grouping the primed and unprimed versions.
	 * 
	 * @param p
	 */
	private static void createVarGroups(PlayerModule p) {
		for (ModuleBDDField f : p.getAllFields()) {
			BDDVarSet both = f.getDomain().set().union(f.getOtherDomain().set());
			Env.TRUE().getFactory().addVarBlock(both, true);
			both.free();      
		}
	}
	
	/**
	 * Adds given states to set of states in map for specific weight.
	 * 
	 * @param weights
	 *          the weights map
	 * @param w
	 *          the specified weight
	 * @param states
	 *          will be consumed (i.e. freed) and no longer can be used
	 */
	private static void addStatesForWeight(Map<Integer, BDD> weights, Integer w, BDD states) {
		BDD s = weights.get(w);
		if (s == null) {
			weights.put(w, states);
		} else {
			weights.put(w, s.orWith(states));
		}
	}

	/**
	 * Adds the specified weight BDD to the weights map. Returns the updated weights map such that all weights BDDs are
	 * disjoint. Frees the old weights map.
	 * 
	 * @param weightMap
	 *          BDDs consumed and no longer can be used
	 * @param weightValue
	 * @param weightBdd
	 *          *not* consumed and can be used
	 * @param mapUnion
	 * @return
	 */
	private static Map<Integer, BDD> addWeightAsDisjoint(Map<Integer, BDD> weightMap, Integer weightValue, BDD weightBdd,
			BDD mapUnion) {

		if (weightBdd.isZero()) { //if the new BDD is empty (a contradiction in the specification), then just return the same old map
			return weightMap;
		}

		Map<Integer, BDD> disjMap = new HashMap<>();

		BDD remaining = mapUnion.and(weightBdd);
		BDD curVal, curIntersect, curDiff;

		Integer curW;

		curDiff = weightBdd.id().andWith(mapUnion.not());
		if (!curDiff.isZero()) { //make sure we don't add empty BDDs to the map
			addStatesForWeight(disjMap, weightValue, curDiff.id()); //add the new weight such that it is disjoint with all other BDDs in the old weights map
		}
		curDiff.free();

		Iterator<Integer> weightsIt = weightMap.keySet().iterator();
		while (!remaining.isZero() && weightsIt.hasNext()) {

			curW = weightsIt.next();
			curVal = weightMap.get(curW);

			curIntersect = curVal.and(weightBdd);

			curDiff = curVal.id().andWith(curIntersect.not());
			if (!curDiff.isZero()) { //make sure we don't add empty BDDs to the map
				addStatesForWeight(disjMap, curW, curDiff.id());
			}

			curDiff.free();
			curVal.free(); //free BDD entry of the old weightMap

			if (!curIntersect.isZero()) {
				remaining.andWith(curIntersect.not()); //remove the current non empty intersection from remaining
				addStatesForWeight(disjMap, weightValue + curW, curIntersect.id());
			}
			curIntersect.free();
		}

		while (weightsIt.hasNext()) { //if there are any weights which are remaining, then they are all disjoint with the new entry (weightDef), so we just add them to the new map
			curW = weightsIt.next();
			curVal = weightMap.get(curW);
			addStatesForWeight(disjMap, curW, curVal);
		}
		remaining.free();
		return disjMap;
	}

	/**
	 * Builds the weights map such that all states (BDDs) values are disjoint, and overlapping weight definitions have a
	 * single weight value equals to the summation of their given weights values. All transitions which have no explicit
	 * weight definitions are given weight value of 0.
	 * 
	 * @param weightDefs
	 *          list of the weights definitions as defined in the specification
	 * @return The weights map.
	 */
	public static Map<Integer, BDD> buildWeights(List<WeightDefinition> weightDefs) {
		if (weightDefs == null || weightDefs.isEmpty()) {
			return null;
		}
		Spec weightSpec;
		BDD weightBdd, weightMapUnion = Env.FALSE();
		int weightValue, weightTrace;
		Map<Integer, BDD> weightMap = new HashMap<>();
		for (WeightDefinition weightDef : weightDefs) {
			weightSpec = weightDef.getDefinition().getSpec();
			weightTrace = weightDef.getDefinition().getTraceId();
			weightBdd = createBdd(weightSpec, weightTrace);
			weightValue = weightDef.getValue();
			weightMap = addWeightAsDisjoint(weightMap, weightValue, weightBdd, weightMapUnion);
			weightMapUnion.orWith(weightBdd);
		}
		/*
		 * make the weight map complete by adding 0 weight mapped to the complement of the union of all states the map
		 * contains (only if it is a non empty set).
		 */
		if (!weightMapUnion.isOne()) {
			addStatesForWeight(weightMap, 0, weightMapUnion.not());
		}

		weightMapUnion.free();
		return weightMap;
	}

	/**
	 * Adds constraints of the {@link Player} {@code p} to the {@link PlayerModule} {@code m}.
	 * 
	 * If {@code trace == true}, creates {@link BehaviorInfo} for every constraint and configures whether it isAux.
	 * 
	 * @param m
	 * @param p
	 * @param isAux
	 * @param trace
	 * @return
	 */
	private static List<BehaviorInfo> createModuleConstraints(PlayerModule m, Player p, boolean isAux, 
			boolean trace, boolean isSys, boolean debugLog) {
		List<BehaviorInfo> info = new ArrayList<>();

//		if (PlayerModule.TEST_MODE) {
//			System.out.println("TEST_MODE set to true, building also the single transition function");
//		}

		int n_ini = 0;
		int n_safety = 0;
		int n_justice = 0;

		for (Constraint c : p.getConstraints()) {
			// now it is set to use BDD bit vectors for the translation of all kinds of arithmetic expressions
			BDD bdd = createBdd(c.getSpec(), c.getTraceId());
			BehaviorInfo i = new BehaviorInfo();
			i.aux = isAux;
			i.traceId = c.getTraceId();

			switch (c.getKind()) {
			case INI:
				n_ini++;
				i.initial = bdd.id();
				m.conjunctInitial(bdd);
				break;
			case SAFETY:
				n_safety++;
				i.safety = bdd.id();

				//To support legacy code that assumes the existence of a single transition relation,
				//we always construct a single transition relation. If a decomposition is used, then we add the current safety constraint
				//to the decomposed transitions' list

				//if (m.getTransFuncType() == TransFuncType.SINGLE_FUNC) {
				//m.conjunctTrans(bdd);
				m.conjunctTrans(bdd.id());
				//} else {
				//if (PlayerModule.TEST_MODE) {
				//  m.conjunctTrans(bdd.id());
				// }
				if (m.getTransFuncType() == TransFuncType.DECOMPOSED_FUNC) {
					m.addToTransList(bdd);
				} else if (m.getTransFuncType() == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
					m.addToPartTransList(bdd);
				}
				else { //m.getTransFuncType() == TransFuncType.SINGLE_FUNC
					bdd.free();
				}
				break;
			case JUSTICE:
				n_justice++;
				i.justice = bdd.id();
				m.addJustice(bdd, i.traceId);
			default:
				break;
			}
			if (trace) {
				info.add(i);
			} else {
				i.free();
			}
		}

		String type_str = "env";
		if (isAux) {
			type_str = "aux";
		} else  if (isSys) {
			type_str = "sys";
		}
		if (debugLog) System.out.println("Spec Stats "+type_str+" initial: " + n_ini);
		if (debugLog) System.out.println("Spec Stats "+type_str+" safety: " + n_safety);
		if (debugLog) System.out.println("Spec Stats "+type_str+" justices: " + n_justice);
		if (debugLog) System.out.println("Spec Stats "+type_str+" trans size: " + m.trans().nodeCount());

		return info;
	}
	
	
	private static void createTriggerModuleConstraints(PlayerModule pMod, Player p, PlayerModule sysMod,
			TriggerSFAGeneratorType triggerSfaGenType, RegExpSFAGeneratorType regExpSfaGenType, boolean groupVars, boolean traceP, boolean traceAux,
			List<BehaviorInfo> auxBehaviorInfo, List<BehaviorInfo> pBehaviorInfo) {
		SFA triggerSfa;
		SFAModuleConstraint triggerSfaConstraint;
		for(TriggerConstraint trigger : p.getTriggers()) {
			triggerSfa = SFAGeneratorFactory.getGenerator(triggerSfaGenType, regExpSfaGenType, trigger, trigger.getTraceId()).generateTriggerSfa();
			if(!triggerSfa.isTrueStarLanguage()) { //Only add non-trivial trigger constraints
				triggerSfaConstraint = new SFAModuleConstraint.Builder().playerModule(sysMod).groupVars(groupVars).sfa(triggerSfa).traceId(trigger.getTraceId()).build();
				if(traceAux) {
					auxBehaviorInfo.add(new BehaviorInfo(triggerSfaConstraint.getIni().id(), null, null, null, null, trigger.getTraceId(), true));
					auxBehaviorInfo.add(new BehaviorInfo(null, triggerSfaConstraint.getTrans().id(), null, null, null, trigger.getTraceId(), true));
				}
				if(traceP) {
					pBehaviorInfo.add(new BehaviorInfo(null, null, triggerSfaConstraint.getAcceptance().id(), null, null, trigger.getTraceId(), true));
				}
				
				sysMod.conjunctInitial(triggerSfaConstraint.getIni());
				
				sysMod.conjunctTrans(triggerSfaConstraint.getTrans().id());
				if (sysMod.getTransFuncType() == TransFuncType.DECOMPOSED_FUNC) {
					sysMod.addToTransList(triggerSfaConstraint.getTrans());
				}
				else if (sysMod.getTransFuncType() == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
					sysMod.addToPartTransList(triggerSfaConstraint.getTrans());
				}
				else { //sysMod.getTransFuncType() == TransFuncType.SINGLE_FUNC
					triggerSfaConstraint.getTrans().free();
				}
				
				pMod.addJustice(triggerSfaConstraint.getAcceptance());
			}
			triggerSfa.free(); //Free the BDDs (guards) of the trigger sfa
		}
	}
	
	
	private static void addExistentialGars(PlayerModule sysMod, Player sysP, RegExpSFAGeneratorType regExpSfaGenType, boolean groupVars, boolean traceSys, List<BehaviorInfo> sysBehaviorInfo) {
		List<ExistentialConstraint> exConstraints = sysP.getExistentialConstraints();
		BDD specBdd;
		List<BDD> existentialBdds, traceInfoBdds;
		BehaviorInfo i;
		SFAModuleConstraint regExpSfaConstraint;
		SFA regExpSfa;
		for(ExistentialConstraint exC : exConstraints) {
			if(exC.isRegExp()) { //An existential guarantee of a scenario described by a regular expression
				regExpSfa = SFAGeneratorFactory.getGenerator(regExpSfaGenType, exC.getRegExp(), exC.getTraceId()).generateRegExpSfa();
				if(!regExpSfa.acceptsTheEmptyString()) { //Only add existential guarantees that are not satisfied vacuously (i.e., whose regexps' languages do not contain the empty string)
					regExpSfaConstraint = new SFAModuleConstraint.Builder().playerModule(sysMod).groupVars(groupVars).sfa(regExpSfa).restrictIniTrans(false).traceId(exC.getTraceId()).build();
					sysMod.addExistReq(regExpSfaConstraint, exC.getTraceId());
					if(traceSys) {
						sysBehaviorInfo.add(new BehaviorInfo(null, null, null, null, regExpSfaConstraint.clone(), exC.getTraceId(), true));
					}
				}
				regExpSfa.free();
			}
			else { //An existential guarantee of a sequence of nested Finally (F) assertions
				existentialBdds = new ArrayList<>();
				traceInfoBdds = new ArrayList<>();
				for(Spec spec : exC.getSpecs()) {
					specBdd = createBdd(spec, exC.getTraceId());
					existentialBdds.add(specBdd);
					if(traceSys) {
						traceInfoBdds.add(specBdd.id());
					}
				}
				sysMod.addExistReq(existentialBdds, exC.getTraceId());
				if(traceSys) {
					i = new BehaviorInfo(null, null, null, traceInfoBdds, null, exC.getTraceId(), false);
					sysBehaviorInfo.add(i);
				}
			}
		}
	}

	/**
	 * Translates the given specification to a BDD such that BDD vectors are used for the evaluation of all kinds of
	 * arithmetic expressions.
	 * 
	 * Variables referenced in the specification have to exist.
	 * 
	 * Auxiliary variables will be created.
	 * 
	 * @param spec
	 * @param traceId
	 * @return
	 */
	public static BDD createBdd(Spec spec, int traceId) {
		return createBdd(spec, traceId, false);
	}

	/**
	 * Translates the given specification to a BDD.
	 * 
	 * Variables referenced in the specification have to exist.
	 * 
	 * Auxiliary variables will be created.
	 * 
	 * @param spec
	 * @param traceId
	 * @param useLinearExpAlg
	 *          Whether to use the algorithm by Bartzis and Bultan to evaluate linear arithmetic expressions. If set to
	 *          false, BDD vectors are used for the evaluation of all kinds of arithmetic expressions.
	 * @return
	 */
	public static BDD createBdd(Spec spec, int traceId, boolean useLinearExpAlg) {
		if (spec instanceof PrimitiveValue) {
			PrimitiveValue pv = (PrimitiveValue) spec;
			if ("TRUE".equals(pv.getValue())) {
				return Env.TRUE();
			}
			if ("true".equals(pv.getValue())) {
				return Env.TRUE();
			}
			if ("FALSE".equals(pv.getValue())) {
				return Env.FALSE();
			}
			if ("false".equals(pv.getValue())) {
				return Env.FALSE();
			}
			throw new BDDTranslationException("Unable to translate: " + spec + " when expecting a Boolean expression.",
					traceId);

		}
		else if (spec instanceof VariableReference) {
			VariableReference vr = (VariableReference) spec;
			if (vr.isPropSpec()) { //a boolean variable
				return Env.getBDDValue(vr.getReferenceName(), "true").id();
			} else {
				throw new BDDTranslationException("Unable to translate: " + spec + " when expecting a Boolean expression.",
						traceId);
			}
		} else if (spec instanceof SpecExp) {
			SpecExp e = (SpecExp) spec;
			Operator op = e.getOperator();

			if (Operator.EQUALS.equals(op)) { /* equality expression */
				return createEqualsExpression(e.getChildren()[0], e.getChildren()[1], traceId, useLinearExpAlg);
			}
			if (op.isInequality()) { // assuming (op != null) , inequality expression
				return createInequalityExpression(e.getChildren()[0], e.getChildren()[1], op, traceId, useLinearExpAlg);
			}
			// assuming (op != null)
			return createPropExpression(e.getChildren()[0], (op.isUnary() ? null : e.getChildren()[1]), op, traceId,
					useLinearExpAlg);
		}
		throw new BDDTranslationException("Unable to translate: " + spec, traceId);
	}

	protected static BDD createPropExpression(Spec c1, Spec c2, Operator propOp, int traceId, boolean useLinearExpAlg) {
		BDD bddC2 = null;
		BDD bddC1 = createBdd(c1, traceId, useLinearExpAlg);
		if (c2 != null) { /* a binary operator */
			/* first, look at trivial cases we can detect early: && \ || */
			if (Operator.AND.equals(propOp) && bddC1.isZero()) {
				return Env.FALSE();
			}
			if (Operator.OR.equals(propOp) && bddC1.isOne()) {
				return Env.TRUE();
			}
			bddC2 = createBdd(c2, traceId, useLinearExpAlg);
		}
		switch (propOp) {
		case NOT:
			return bddC1.not();
		case PRIME:
			return Env.prime(bddC1);
		case AND:
			return bddC1.andWith(bddC2);
		case OR:
			return bddC1.orWith(bddC2);
		case XOR:
			return bddC1.xorWith(bddC2);
		case IMPLIES:
			return bddC1.impWith(bddC2);
		case IFF:
			return bddC1.biimpWith(bddC2);
		default:
			throw new BDDTranslationException("Unable to translate (unsupported propositional operator): " + c1 + propOp + c2,
					traceId);
		}

	}

	/**
	 * Creates an inequality expression of two specs of any kind.
	 * 
	 * @param c1
	 * @param c2
	 * @param op
	 * @param traceId
	 * @param useLinearExpAlg
	 *          Whether to use the algorithm by Bartzis and Bultan to evaluate linear expressions. If set to false, then
	 *          BDD vectors are used for evaluation.
	 * @return
	 */
	protected static BDD createInequalityExpression(Spec c1, Spec c2, Operator op, int traceId, boolean useLinearExpAlg) {

		if (c1 instanceof PrimitiveValue && c2 instanceof PrimitiveValue) { /*
		 * val1 <= val2, val1 < val2, val1 >= val2, val1
		 * > val2
		 */
			String strVal1 = ((PrimitiveValue) c1).getValue();
			String strVal2 = ((PrimitiveValue) c2).getValue();

			if (((PrimitiveValue) c1).isPropSpec()) {
				throw new BDDTranslationException("Unable to translate: " + c1 + " when expecting an integer expression.",
						traceId);
			}
			if (((PrimitiveValue) c2).isPropSpec()) {
				throw new BDDTranslationException("Unable to translate: " + c2 + " when expecting an integer expression.",
						traceId);
			}

			int val1 = Integer.parseInt(strVal1);
			int val2 = Integer.parseInt(strVal2);

			switch (op) {
			case LEFT_BIGGER_OR_EQUALS:
				return val1 >= val2 ? Env.TRUE() : Env.FALSE();
			case LEFT_BIGGER:
				return val1 > val2 ? Env.TRUE() : Env.FALSE();
			case RIGHT_BIGGER_OR_EQUALS:
				return val1 <= val2 ? Env.TRUE() : Env.FALSE();
			default: //RIGHT_BIGGER
				return val1 < val2 ? Env.TRUE() : Env.FALSE();
			}
		}
		if (c1 instanceof PrimitiveValue && c2 instanceof VariableReference) { /*
		 * val <= var, val < var, val >= var, val >
		 * var
		 */

			/*
			 * we want to swap the expression from val <= var, val < var, val >= var, val > var --to---> var >= val, var >
			 * val, var <= val, var < val
			 */

			Spec swap = c1;
			c1 = c2;
			c2 = swap;

			Operator reverseOp;
			switch (op) {
			case LEFT_BIGGER_OR_EQUALS:
				reverseOp = Operator.RIGHT_BIGGER_OR_EQUALS;
				break;
			case LEFT_BIGGER:
				reverseOp = Operator.RIGHT_BIGGER;
				break;
			case RIGHT_BIGGER_OR_EQUALS:
				reverseOp = Operator.LEFT_BIGGER_OR_EQUALS;
				break;
			default: //RIGHT_BIGGER
				reverseOp = Operator.LEFT_BIGGER;
				break;
			}
			op = reverseOp;

		}

		if (c1 instanceof VariableReference && c2 instanceof PrimitiveValue) { /*
		 * var <= val, var < val, var >= val, var >
		 * val
		 */
			VariableReference r1 = (VariableReference) c1;
			String strVal2 = ((PrimitiveValue) c2).getValue();

			if (r1.isPropSpec()) {
				throw new BDDTranslationException(
						"Unable to translate: " + c2 + " when expecting an integer constant expression.", traceId);
			}

			int val2 = Integer.parseInt(strVal2);

			if (!(r1.getVariable().getType().isInteger())) {
				throw new BDDTranslationException("Unable to translate: " + r1 + " when expecting an integer typed variable.",
						traceId);
			}
			return createSimpleInequalityExpression(r1, val2, op);
		}

		if (useLinearExpAlg &&
				isLinearExpression(c1, traceId, true) && isLinearExpression(c2, traceId, true)) {

			Map<String, Integer> varIdx = new HashMap<>(); /* mapping of variable reference name -> index */
			MutableInt varNum = new MutableInt();
			int[] simpleC1 = flattenLinearExpression(c1, varIdx, varNum);
			int[] simpleC2 = flattenLinearExpression(c2, varIdx, varNum);

			int[] simpleExp;
			switch (op) {
			case LEFT_BIGGER_OR_EQUALS:
				simpleExp = subtractCoefficients(simpleC2, simpleC1);
				simpleExp[0]--; /* we are going to compute strict inequality -> make it equivalent to non strict inequality */
				break;
			case LEFT_BIGGER:
				simpleExp = subtractCoefficients(simpleC2, simpleC1);
				break;
			case RIGHT_BIGGER_OR_EQUALS:
				simpleExp = subtractCoefficients(simpleC1, simpleC2);
				simpleExp[0]--; /* we are going to compute strict inequality -> make it equivalent to non strict inequality */
				break;
			default: //RIGHT_BIGGER
				simpleExp = subtractCoefficients(simpleC1, simpleC2);
				break;
			}

			Map<BDDDomain, Integer> varToCoef = new HashMap<>(); /* mapping of: variable domain --> coefficient */
			Map<Integer, BDDDomain> idxToVar = new HashMap<>(); /* mapping of: variable index --> variable domain */

			int maxBitNum = createInputsForLinearExpressionCreation(simpleExp, varIdx, varToCoef, idxToVar);

			if (varToCoef.keySet().size() == 0) { /* there are no variables left because all of the coefficients are 0 */
				return (simpleExp[0] < 0) ? Env.TRUE() : Env.FALSE();
			}
			return createLinearInequalityExpression(idxToVar, varToCoef, simpleExp[0], varToCoef.keySet().size(), 1, 0,
					maxBitNum);
		}

		/* non linear inequality expression or linear with modulo operator */

		boolean strictIne = false;
		Spec cTmp;
		switch (op) {
		case LEFT_BIGGER_OR_EQUALS:
			cTmp = c2;
			c2 = c1;
			c1 = cTmp;
			break;
		case LEFT_BIGGER:
			cTmp = c2;
			c2 = c1;
			c1 = cTmp;
			strictIne = true;
			break;
		case RIGHT_BIGGER_OR_EQUALS:
			break;
		default: //RIGHT_BIGGER
			strictIne = true;
			break;
		}
		return createGeneralInequalityExpression(strictIne, c1, c2, traceId);
	}

	protected static int createInputsForLinearExpressionCreation(int[] simpleExp, Map<String, Integer> varIdx,
			Map<BDDDomain, Integer> varToCoef, Map<Integer, BDDDomain> idxToVar) {

		/* Generate a mapping of: variable domain --> coefficient */
		/* Generate a mapping of: index --> variable domain */

		int idx, maxBitNum = -1, newIdx = 0;
		BDDDomain varDomain;
		for (String varName : varIdx.keySet()) {
			idx = varIdx.get(varName);
			if (simpleExp[idx] != 0) { /*
			 * a variable with a non-zero coefficient (we can ignore all variables with a
			 * coefficient of value 0)
			 */
				varDomain = Env.getVar(varName).getDomain();
				varToCoef.put(varDomain, simpleExp[idx]);
				idxToVar.put(++newIdx, varDomain);
				maxBitNum = (maxBitNum < varDomain.vars().length) ? varDomain.vars().length : maxBitNum;

			}
		}
		return maxBitNum;
	}

	/**
	 * Computes the BDD of any <i>linear</i> (without modulo) equality expression, i.e, a0 + a1*x1 + a2*x2 + ... + an*xn =
	 * b0 + b1*y1 + b2*y2 + ... + bn*yn, where a0, ... ,an and b0, ... ,bn are integer constants and x1, ... ,xn and y1,
	 * ... ,yn are integer variables. This method is based on an algorithm presented by Bartzis and Bultan (2003).
	 * 
	 * @param idxToVar
	 *          Mapping of indices to the variables of the expression. The minimum index must be 1.
	 * @param varToCoef
	 *          Mapping of variables to their coefficients. Each variable is mapped to its coefficient in the linear
	 *          expression.
	 * @param c
	 *          The carry computed so far. Its initial value is (a0-b0).
	 * @param v
	 *          The number of variables that appear in the linear expression, i.e., with non zero coefficient.
	 * @param i
	 *          The current level, i.e., the current variable index. The minimum index must be 1.
	 * @param j
	 *          The current layer, i.e, the current bit (BDD variable) of the current variable domain, starting from 0.
	 * @param maxBitNum
	 *          The size of the largest domain (number of layers) among all the variable domains that appear in the linear
	 *          expression.
	 * 
	 * @return The BDD which encodes the specified linear equality expression.
	 */
	protected static BDD createLinearEqualsExpression(Map<Integer, BDDDomain> idxToVar,
			Map<BDDDomain, Integer> varToCoef, long c, int v, int i,
			int j, int maxBitNum) {

		int[] varIBits = idxToVar.get(i).vars();

		if (j >= varIBits.length && i < v) { /*
		 * the current variable has no bit in this layer (its length is shorter), but
		 * we can proceed to the next variable in this layer
		 */
			return createLinearEqualsExpression(idxToVar, varToCoef, c, v, i + 1, j, maxBitNum);
		}
		if (j >= varIBits.length && j < maxBitNum - 1) { //&& (i == v) /*proceed to the beginning of the next layer*/
			if (c % 2 == 0) {
				return createLinearEqualsExpression(idxToVar, varToCoef, c / 2, v, 1, j + 1, maxBitNum);
			}
			return Env.FALSE();
		}
		if (j >= varIBits.length) { /*
		 * this is the last variable in the last layer, but the current variable has no bit in
		 * this layer
		 */
			// recursion base
			if (c == 0) {
				return Env.TRUE();
			}
			return Env.FALSE();
		}
		/* j < varIBits.length */

		BDD low, high;
		BDD curBit = idxToVar.get(i).getFactory().ithVar(varIBits[j]);
		int curCoef = varToCoef.get(idxToVar.get(i));

		if (i == v && j == maxBitNum - 1) { // recursion base
			low = (c == 0) ? Env.TRUE() : Env.FALSE();
			high = (c + curCoef == 0) ? Env.TRUE() : Env.FALSE();
		} else if (i == v) { /* last variable in this layer --> proceed to the beginning of the next layer */
			if (c % 2 == 0) {
				low = createLinearEqualsExpression(idxToVar, varToCoef, c / 2, v, 1, j + 1, maxBitNum);
			} else {
				low = Env.FALSE();
			}
			if ((c + curCoef) % 2 == 0) {
				high = createLinearEqualsExpression(idxToVar, varToCoef, (c + curCoef) / 2, v, 1, j + 1, maxBitNum);
			} else {
				high = Env.FALSE();
			}
		} else {
			low = createLinearEqualsExpression(idxToVar, varToCoef, c, v, i + 1, j, maxBitNum);
			high = createLinearEqualsExpression(idxToVar, varToCoef, c + curCoef, v, i + 1, j, maxBitNum);
		}

		BDD result = curBit.ite(high, low);
		low.free();
		high.free();
		return result;
	}

	/**
	 * Computes the BDD of any <i>linear</i> (without modulo) strict inequality expression, i.e, a0 + a1*x1 + a2*x2 + ...
	 * + an*xn < b0 + b1*y1 + b2*y2 + ... + bn*yn, where a0, ... ,an and b0, ... ,bn are integer constants and x1, ... ,xn
	 * and y1, ... ,yn are integer variables. This method is based on an algorithm presented by Bartzis and Bultan (2003).
	 * 
	 * @param idxToVar
	 *          Mapping of indices to the variables of the expression. The minimum index must be 1.
	 * @param varToCoef
	 *          Mapping of variables to their coefficients. Each variable is mapped to its coefficient in the linear
	 *          expression.
	 * @param c
	 *          The carry computed so far. Its initial value is (a0-b0).
	 * @param v
	 *          The number of variables that appear in the linear expression, i.e., with non zero coefficient.
	 * @param i
	 *          The current level, i.e., the current variable index. The minimum index must be 1.
	 * @param j
	 *          The current layer, i.e, the current bit (BDD variable) of the current variable domain, starting from 0.
	 * @param maxBitNum
	 *          The size of the largest domain (number of layers) among all the variable domains that appear in the linear
	 *          expression.
	 * 
	 * @return The BDD which encodes the specified linear inequality expression.
	 */
	protected static BDD createLinearInequalityExpression(Map<Integer, BDDDomain> idxToVar,
			Map<BDDDomain, Integer> varToCoef, long c, int v, int i,
			int j, int maxBitNum) {
		int[] varIBits = idxToVar.get(i).vars();

		if (j >= varIBits.length && i < v) { /*
		 * the current variable has no bit in this layer (its length is shorter), but
		 * we can proceed to the next variable in this layer
		 */
			return createLinearInequalityExpression(idxToVar, varToCoef, c, v, i + 1, j, maxBitNum);
		}
		if (j >= varIBits.length && j < maxBitNum - 1) { //&& (i == v) /*proceed to the beginning of the next layer*/
			if (c % 2 == 0) {
				return createLinearInequalityExpression(idxToVar, varToCoef, c / 2, v, 1, j + 1, maxBitNum);
			}
			return createLinearInequalityExpression(idxToVar, varToCoef, (c - 1) / 2, v, 1, j + 1, maxBitNum);
		}
		if (j >= varIBits.length) { /*
		 * this is the last variable in the last layer, but the current variable has no bit in
		 * this layer
		 */
			// recursion base
			if (c < 0) {
				return Env.TRUE();
			}
			return Env.FALSE();
		}
		/* j < varIBits.length */

		BDD low, high;
		BDD curBit = idxToVar.get(i).getFactory().ithVar(varIBits[j]);
		int curCoef = varToCoef.get(idxToVar.get(i));

		if (i == v && j == maxBitNum - 1) { // recursion base
			low = (c < 0) ? Env.TRUE() : Env.FALSE();
			high = (c + curCoef < 0) ? Env.TRUE() : Env.FALSE();
		} else if (i == v) { /* last variable in this layer --> proceed to the beginning of the next layer */
			if (c % 2 == 0) {
				low = createLinearInequalityExpression(idxToVar, varToCoef, c / 2, v, 1, j + 1, maxBitNum);
			} else {
				low = createLinearInequalityExpression(idxToVar, varToCoef, (c - 1) / 2, v, 1, j + 1, maxBitNum);
			}
			if ((c + curCoef) % 2 == 0) {
				high = createLinearInequalityExpression(idxToVar, varToCoef, (c + curCoef) / 2, v, 1, j + 1, maxBitNum);
			} else {
				high = createLinearInequalityExpression(idxToVar, varToCoef, (c + curCoef - 1) / 2, v, 1, j + 1, maxBitNum);
			}
		} else {
			low = createLinearInequalityExpression(idxToVar, varToCoef, c, v, i + 1, j, maxBitNum);
			high = createLinearInequalityExpression(idxToVar, varToCoef, c + curCoef, v, i + 1, j, maxBitNum);
		}

		BDD result = curBit.ite(high, low);
		low.free();
		high.free();
		return result;
	}

	protected static int[] subtractCoefficients(final int[] subFrom, final int[] toSub) {
		int[] res;
		if (subFrom.length >= toSub.length) {
			res = new int[subFrom.length];
			for (int i = 0; i < toSub.length; i++) {
				res[i] = subFrom[i] - toSub[i];
			}
			for (int i = toSub.length; i < subFrom.length; i++) {
				res[i] = subFrom[i];
			}
		} else { /* subFrom.length < toSub.length */
			res = new int[toSub.length];
			for (int i = 0; i < subFrom.length; i++) {
				res[i] = subFrom[i] - toSub[i];
			}
			for (int i = subFrom.length; i < toSub.length; i++) {
				res[i] -= toSub[i];
			}
		}

		return res;
	}

	protected static class MutableInt {

		int i;

		protected MutableInt(int val) {
			this.i = val;
		}

		protected MutableInt() {
			this.i = 0;
		}

		protected int getIntValue() {
			return this.i;
		}

		protected void incrementInt() {
			this.i++;
		}

	}

	protected static int[] flattenLinearExpression(Spec exp, Map<String, Integer> varIdx, MutableInt varNum) {
		if (exp instanceof PrimitiveValue) {
			int val = Integer.parseInt(((PrimitiveValue) exp).getValue());
			return new int[] { val };
		} else if (exp instanceof VariableReference) {
			Variable var = ((VariableReference) exp).getVariable();
			String varName = ((VariableReference) exp).getReferenceName();
			int varIndex;
			if (varIdx.containsKey(varName)) {
				varIndex = varIdx.get(varName);
			} else {
				varNum.incrementInt();
				varIndex = varNum.getIntValue();
				varIdx.put(varName, varIndex);
			}
			int[] ret = new int[varNum.getIntValue() + 1];
			ret[varIndex] = 1;
			ret[0] = var.getType().getLower(); /* add variable domain info */
			return ret;
		} else {
			SpecExp sExp = (SpecExp) exp;
			int[] leftC = flattenLinearExpression(sExp.getChildren()[0], varIdx, varNum);
			int[] rightC = flattenLinearExpression(sExp.getChildren()[1], varIdx, varNum);
			if (leftC.length == 1 && rightC.length == 1) { /* constant *\+ constant */
				if (Operator.MULTIPLY.equals(sExp.getOperator())) {
					return new int[] { leftC[0] * rightC[0] };
				}
				if (Operator.ADD.equals(sExp.getOperator())) {
					return new int[] { leftC[0] + rightC[0] };
				}
				if (Operator.SUBSTRACT.equals(sExp.getOperator())) {
					return new int[] { leftC[0] - rightC[0] };
				}
			}
			if (leftC.length == 1) { /* constant *\+ linear expression */
				if (Operator.MULTIPLY.equals(sExp.getOperator())) {
					for (int i = 0; i < rightC.length; i++) {
						rightC[i] = leftC[0] * rightC[i];
					}
				} else if (Operator.ADD.equals(sExp.getOperator())) {
					rightC[0] = leftC[0] + rightC[0];
				} else if (Operator.SUBSTRACT.equals(sExp.getOperator())) {
					rightC[0] = leftC[0] - rightC[0];
					for (int i = 1; i < rightC.length; i++) {
						rightC[i] = 0 - rightC[i];
					}
				}
				return rightC;
			}
			if (rightC.length == 1) { /* linear expression *\+ constant */
				if (Operator.MULTIPLY.equals(sExp.getOperator())) {
					for (int i = 0; i < leftC.length; i++) {
						leftC[i] = rightC[0] * leftC[i];
					}
				} else if (Operator.ADD.equals(sExp.getOperator())) {
					leftC[0] = rightC[0] + leftC[0];
				} else if (Operator.SUBSTRACT.equals(sExp.getOperator())) {
					leftC[0] = leftC[0] - rightC[0];
				}
				return leftC;
			}
			int[] smallArr = rightC.length < leftC.length ? rightC : leftC;
			int[] largeArr = rightC.length < leftC.length ? leftC : rightC;
			int[] res = new int[largeArr.length];
			if (Operator.ADD.equals(sExp.getOperator())) {
				for (int i = 0; i < smallArr.length; i++) {
					res[i] = smallArr[i] + largeArr[i];
				}
				for (int i = smallArr.length; i < largeArr.length; i++) {
					res[i] = largeArr[i];
				}
			} else { //subtraction operator
				if (smallArr == leftC) { //small length array is the left child --> smallC - largeC
					for (int i = 0; i < smallArr.length; i++) {
						res[i] = smallArr[i] - largeArr[i];
					}
					for (int i = smallArr.length; i < largeArr.length; i++) {
						res[i] = 0 - largeArr[i];
					}
				} else { //small length array is the right child -- > largeC - smallC
					for (int i = 0; i < smallArr.length; i++) {
						res[i] = largeArr[i] - smallArr[i];
					}
					for (int i = smallArr.length; i < largeArr.length; i++) {
						res[i] = largeArr[i];
					}
				}

			}
			return res;
		}
	}

	protected static boolean isConstantsLinearExpression(Spec exp, int traceId) {
		if (exp instanceof VariableReference) {
			return false;
		}
		if (exp instanceof PrimitiveValue) {
			if (((PrimitiveValue) exp).isPropSpec()) {
				throw new BDDTranslationException(
						"Unable to translate: " + exp + " when expecting an integer constant expression.", traceId);
			}
			return true;
		}
		if (exp instanceof SpecExp) {
			SpecExp sExp = (SpecExp) exp;
			Operator expOp = sExp.getOperator();

			if (expOp.isArithmetic()) {
				return isConstantsLinearExpression(sExp.getChildren()[0], traceId) &&
						isConstantsLinearExpression(sExp.getChildren()[1], traceId);
			}
			throw new BDDTranslationException("Unable to translate: " + exp + " when expecting an arithmetic operator.",
					traceId);
		}
		return false;
	}

	/**
	 * Returns true if {@code exp} is a linear arithmetic expression.
	 * 
	 * @param exp
	 * @param traceId
	 * @param inequalityExpression
	 *          True if {@code exp} is a subexpression of an inequality expression, and false if of an equality
	 *          expression.
	 * @return
	 */
	protected static boolean isLinearExpression(Spec exp, int traceId, boolean inequalityExpression) {

		if (exp instanceof PrimitiveValue) {
			if (((PrimitiveValue) exp).isPropSpec()) {
				if (inequalityExpression) {
					throw new BDDTranslationException(
							"Unable to translate: " + exp + " when expecting an integer constant expression.", traceId);
				}
				return false;
			}
			return true;
		}
		if (exp instanceof VariableReference) {
			VariableReference r = (VariableReference) exp;
			if (!(r.getVariable().getType().isInteger())) {
				if (inequalityExpression) {
					throw new BDDTranslationException(
							"Unable to translate: " + exp + " when expecting an integer typed variable.", traceId);
				}
				return false;
			}
			return true;
		}
		if (!(exp instanceof SpecExp)) {
			throw new BDDTranslationException("Unable to translate: " + exp + " when expecting an instance of SpecExp.",
					traceId);
		}

		SpecExp sExp = (SpecExp) exp;
		if (!sExp.getOperator().isLinearArithmetic()) {
			if (Operator.MULTIPLY.equals(sExp.getOperator())) { /*
			 * check is we have the case of: constants*(linear expression)
			 */
				if (isConstantsLinearExpression(sExp.getChildren()[0], traceId)) {
					return isLinearExpression(sExp.getChildren()[1], traceId, inequalityExpression);
				}
				if (isConstantsLinearExpression(sExp.getChildren()[1], traceId)) { /*
				 * check is we have the case of: (linear
				 * expression)*constants
				 */
					return isLinearExpression(sExp.getChildren()[0], traceId, inequalityExpression);
				}
			}
			return false; /* case of vars*vars --> non linear expression */
		}
		/* + or - operators */
		return isLinearExpression(sExp.getChildren()[0], traceId, inequalityExpression)
				&& isLinearExpression(sExp.getChildren()[1], traceId, inequalityExpression);
	}

	/**
	 * Creates an equals expression of two specs of any kind.
	 * 
	 * @param c1
	 * @param c2
	 * @param useLinearExpAlg
	 *          Whether to use the algorithm by Bartzis and Bultan to evaluate linear expressions. If set to false, then
	 *          BDD vectors are used for evaluation.
	 * @return
	 */
	protected static BDD createEqualsExpression(Spec c1, Spec c2, int traceId, boolean useLinearExpAlg) {
		if (c1 instanceof PrimitiveValue && c2 instanceof PrimitiveValue) {
			if (((PrimitiveValue) c1).getValue().equals(((PrimitiveValue) c2).getValue())) {
				return Env.TRUE();
			} else {
				return Env.FALSE();
			}
		}
		if (c1 instanceof VariableReference && c2 instanceof VariableReference) {
			VariableReference r1 = (VariableReference) c1;
			VariableReference r2 = (VariableReference) c2;
			if (r1.getReferenceName().equals(r2.getReferenceName())) {
				// comparing variable with itself
				return Env.TRUE();
			} else if (!r1.getVariable().getType().isComparable(r1.getVariable().getType())) {
				// comparing incomparable types
				Env.FALSE();
			} else {
				return createTwoVarsEqualsExpression(r1, r2);
			}
		}
		if (c1 instanceof PrimitiveValue && c2 instanceof VariableReference) {
			Spec swap = c1;
			c1 = c2;
			c2 = swap;
		}
		if (c1 instanceof VariableReference && c2 instanceof PrimitiveValue) {
			VariableReference var = (VariableReference) c1;
			PrimitiveValue val = (PrimitiveValue) c2;
			BDD eq = Env.getBDDValue(var.getReferenceName(), val.getValue());
			if (eq == null) {
				// value not in domain
				throw new BDDTranslationException("Value " + val.getValue() + " not in domain of " + var.getReferenceName(),
						traceId);
			} else {
				return eq.id();
			}
		}

		if (isPropExpression(c1) && isPropExpression(c2)) { /*
		 * propositional operators in both sides -> boolean expressions
		 * in both sides
		 */
			BDD c1Bdd = createBdd(c1, traceId, useLinearExpAlg);
			BDD c2Bdd = createBdd(c2, traceId, useLinearExpAlg);

			BDD res = c1Bdd.biimp(c2Bdd);

			c1Bdd.free();
			c2Bdd.free();

			return res;

		}

		if (useLinearExpAlg && isLinearExpression(c1, traceId, false) && isLinearExpression(c2, traceId, false)) {

			Map<String, Integer> varIdx = new HashMap<>(); /* mapping of variable reference name -> index */
			MutableInt varNum = new MutableInt();
			int[] simpleC1 = flattenLinearExpression(c1, varIdx, varNum);
			int[] simpleC2 = flattenLinearExpression(c2, varIdx, varNum);

			int[] simpleExp = subtractCoefficients(simpleC1, simpleC2);

			Map<BDDDomain, Integer> varToCoef = new HashMap<>(); /* mapping of: variable domain --> coefficient */
			Map<Integer, BDDDomain> idxToVar = new HashMap<>(); /* mapping of: variable index --> variable domain */

			int maxBitNum = createInputsForLinearExpressionCreation(simpleExp, varIdx, varToCoef, idxToVar);

			if (varToCoef.keySet().size() == 0) { /* there are no variables left because all of the coefficients are 0 */
				return (simpleExp[0] == 0) ? Env.TRUE() : Env.FALSE();
			}
			return createLinearEqualsExpression(idxToVar, varToCoef, simpleExp[0], varToCoef.keySet().size(), 1, 0,
					maxBitNum);
		}

		/* non linear arithmetic expression (with multiplication of variables) */
		return createGeneralEqualsExpression(c1, c2, traceId);
	}

	/**
	 * Computes the BDD of <i>any</i> inequality (less than or less than equals) expression using BDD vectors. Supports
	 * expressions which contain variables multiplication, addition and subtraction. Also supports modulo operator by
	 * performing modulo division using BDD bit vectors. Note that for linear (without modulo) expressions, one can also
	 * use {@link #createLinearInequalityExpression}.
	 * 
	 * @param strictIne
	 *          Whether the expression is a (strictly) less than expression.
	 * @param c1
	 *          Left child.
	 * @param c2
	 *          Right child.
	 * @param traceId
	 * @return
	 */
	protected static BDD createGeneralInequalityExpression(boolean strictIne, Spec c1, Spec c2, int traceId) {

		BDDBitVector c1BddVector = computeBddVectorFromExpression(c1, traceId);
		BDDBitVector c2BddVector = computeBddVectorFromExpression(c2, traceId);

		BDD res;
		if (strictIne) { /* less-than */
			res = c1BddVector.lt(c2BddVector);
		} else { /* less-than-equals */
			res = c1BddVector.lte(c2BddVector);
		}

		c1BddVector.free();
		c2BddVector.free();

		return res;

	}

	/**
	 * Computes the BDD of <i>any</i> equality expression using BDD vectors. Supports expressions which contain variables
	 * multiplication, addition and subtraction. Also supports modulo operator by performing modulo division using BDD bit
	 * vectors. Note that for linear (without modulo) expressions, one can also use {@link #createLinearEqualsExpression}.
	 * 
	 * @param strictInequality
	 *          Whether the expression is a (strict) less than expression.
	 * @param c1
	 *          Left child.
	 * @param c2
	 *          Right child.
	 * @param traceId
	 * @return
	 */
	protected static BDD createGeneralEqualsExpression(Spec c1, Spec c2, int traceId) {

		BDDBitVector c1BddVector = computeBddVectorFromExpression(c1, traceId);
		BDDBitVector c2BddVector = computeBddVectorFromExpression(c2, traceId);

		BDD res = c1BddVector.eq(c2BddVector);

		c1BddVector.free();
		c2BddVector.free();

		return res;

	}

	/**
	 * Computes a BDD bit vector that encodes all the values to whom the specified arithmetic expression evaluates.
	 * 
	 * @param exp
	 * @param traceId
	 * @return
	 */
	protected static BDDBitVector computeBddVectorFromExpression(Spec exp, int traceId) {
		if (exp instanceof PrimitiveValue) {
			int val = Integer.parseInt(((PrimitiveValue) exp).getValue());
			return Env.createBddVector(BigInteger.valueOf(val));
		}
		if (exp instanceof VariableReference) {
			Variable var = ((VariableReference) exp).getVariable();

			/* now, encode the lower bound of the domain of the variable */

			BDDBitVector lowerBoundVec = Env.createBddVector(BigInteger.valueOf(var.getType().getLower()));
			String varName = ((VariableReference) exp).getReferenceName();
			BDDBitVector resWithoutLB = Env.createBddVector(Env.getVar(varName).getDomain());
			BDDBitVector result = resWithoutLB.add(lowerBoundVec);

			resWithoutLB.free();
			lowerBoundVec.free();
			return result;
		} else {
			SpecExp sExp = (SpecExp) exp;
			BDDBitVector c1 = computeBddVectorFromExpression(sExp.getChildren()[0], traceId);
			BDDBitVector c2 = computeBddVectorFromExpression(sExp.getChildren()[1], traceId);

			Operator op = sExp.getOperator();

			BDDBitVector result;

			switch (op) {
			case ADD:
				result = c1.add(c2);
				break;
			case SUBSTRACT:
				result = c1.sub(c2);
				break;
			case MULTIPLY:
				result = c1.mult(c2);
				break;
			case MOD:
				try {
					if (c2.isConst()) { /* modulo of a constant divisor */
						result = c1.mod(c2.val());
					} else { /* modulo of a non-constant divisor */
						result = c1.mod(c2);
					}
				} catch (BDDException e) {
					throw new BDDTranslationException("Unable to translate: " + exp + " : " + e.getMessage(), traceId);
				}
				break;
			default:
				throw new BDDTranslationException("Unable to translate: " + exp +
						" when expecting an arithmetic operator.", traceId);
			}
			c1.free();
			c2.free();

			return result;
		}
	}

	protected static boolean isPropExpression(Spec exp) {
		if (exp instanceof PrimitiveValue) {
			return ((PrimitiveValue) exp).isPropSpec();
		}
		if (exp instanceof VariableReference) {
			return ((VariableReference) exp).isPropSpec();
		}

		if (!(exp instanceof SpecExp)) {
			return false;
		}

		SpecExp sExp = (SpecExp) exp;
		return sExp.getOperator().isProp();
	}

	/**
	 * Computes a BDD of a simple inequality expression, i.e, 'var < val', 'var <= val', 'var > val', 'var >= val', where
	 * the left operand is a variable reference (of an integer variable) and the right operand is an integer value (a
	 * constant). The time complexity is linear in the number of the BDD variables that encode the domain of referenced
	 * variable.
	 *
	 * @param r1
	 * @param val
	 * @return
	 */
	protected static BDD createSimpleInequalityExpression(VariableReference r1, int val, Operator op) {
		int lowerBound = r1.getVariable().getType().getLower();
		int upperBound = r1.getVariable().getType().getUpper();
		BDDDomain domain = Env.getVar(r1.getReferenceName()).getDomain();

		/* First, check for trivial cases */
		if (Operator.RIGHT_BIGGER_OR_EQUALS.equals(op) && upperBound <= val) {
			if (upperBound <= val) {
				return Env.TRUE();
			}
			if (lowerBound > val) {
				return Env.FALSE();
			}
		}
		if (Operator.RIGHT_BIGGER.equals(op)) {
			if (upperBound < val) {
				return Env.TRUE();
			}
			if (lowerBound >= val) {
				return Env.FALSE();
			}
		}
		if (Operator.LEFT_BIGGER_OR_EQUALS.equals(op)) {
			if (lowerBound >= val) {
				return Env.TRUE();
			}
			if (upperBound < val) {
				return Env.FALSE();
			}
		}
		if (Operator.LEFT_BIGGER.equals(op)) {
			if (lowerBound > val) {
				return Env.TRUE();
			}
			if (upperBound <= val) {
				return Env.FALSE();
			}
		}

		/*
		 * we must take into consideration the offset from 0, i.e, the lower bound of the domain. Thus, we update the right
		 * operand accordingly
		 **/
		val = val - lowerBound;

		if (Operator.RIGHT_BIGGER.equals(op) || Operator.LEFT_BIGGER_OR_EQUALS.equals(op)) {
			/* we need to compute strict inequality --> decrement right operand */
			--val;
		}

		/* encode var <= val */
		int[] ivar = domain.vars();

		BigInteger valWrapper = new BigInteger("" + val);
		BDD inq = Env.TRUE();

		for (int n = 0; n < domain.varNum(); n++) {
			if (valWrapper.testBit(0)) {
				inq.orWith(domain.getFactory().nithVar(ivar[n]));
			} else {
				inq.andWith(domain.getFactory().nithVar(ivar[n]));
			}
			valWrapper = valWrapper.shiftRight(1);
		}

		if (Operator.LEFT_BIGGER_OR_EQUALS.equals(op) || Operator.LEFT_BIGGER.equals(op)) {
			/* negate the BDD because we have a left side operator, and we computed for a right side operator */
			BDD tmpInq = inq.not();
			inq.free();
			inq = tmpInq;
		}

		/*
		 * Note: We do not restrict here the resulting BDD to the domain of the integer variable (to deal with overflows).
		 * It is assumed it is encoded into the transition relation and initial states of the players modules.
		 */
		//inq.andWith(domain.domain());

		return inq;
	}

	/**
	 * Computes an equals expression BDD for two variable references that have a comparable type.
	 * 
	 * @param r1
	 * @param r2
	 * @return
	 */
	protected static BDD createTwoVarsEqualsExpression(VariableReference r1, VariableReference r2) {
		BDD eq = Env.FALSE();
		String v1 = r1.getReferenceName();
		String v2 = r2.getReferenceName();
		Set<String> commonValues = new HashSet<>(Env.getValueNames(v1));
		commonValues.retainAll(Env.getValueNames(v2));
		for (String val : commonValues) {
			BDD b1 = Env.getBDDValue(v1, val);
			BDD b2 = Env.getBDDValue(v2, val);
			eq.orWith(b1.and(b2));
		}
		return eq;
	}

	private static void createModuleVars(PlayerModule m, Player p, boolean aux) {
		for (Variable v : p.getVars()) {
			if (v.getType().isArray()) {
				addArrayVars(m, v.getName(), v.getType().getDimensions(), v.getType(), aux, v.getTraceId());
			} else {
				addVar(m, v.getName(), v.getType(), aux, v.getTraceId());
			}
		}
	}

	/**
	 * Adds variables of an array type by extending the name of the variable with its dimensions.
	 * 
	 * @param m
	 * @param name
	 * @param dimensions
	 * @param type
	 */
	private static void addArrayVars(PlayerModule m, String name, List<Integer> dimensions, TypeDef type, boolean aux, int traceId) {
		if (dimensions.isEmpty()) {
			addVar(m, name, type, aux, traceId);
		} else {
			List<Integer> remainingDimensions = new ArrayList<Integer>();
			for (int i = 1; i < dimensions.size(); i++) {
				remainingDimensions.add(dimensions.get(i));
			}
			for (int i = 0; i < dimensions.get(0); i++) {
				String suffix = "[" + i + "]";
				addArrayVars(m, name + suffix, remainingDimensions, type, aux, traceId);
			}
		}
	}

	/**
	 * Adds a plain variable to the module (NO array support).
	 * 
	 * @param m
	 * @param name
	 * @param type
	 */
	private static void addVar(PlayerModule m, String name, TypeDef type, boolean aux, int traceId) {
		try {
			if (type.isBoolean()) {
				m.addVar(name, aux).setTraceId(traceId);
			} else if (type.isInteger()) {
				m.addVar(name, type.getLower(), type.getUpper(), aux).setTraceId(traceId);
			} else {
				String[] val_names = type.getValues().toArray(new String[0]);
				m.addVar(name, val_names, aux).setTraceId(traceId);
			}
		} catch (Exception e) {
			throw new RuntimeException("Could not add variable " + name + " : " + e.getMessage());
		}
	}
}