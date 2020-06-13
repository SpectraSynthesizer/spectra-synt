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


package tau.smlab.syntech.bddgenerator.sfa.trigger;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import dk.brics.automaton.Automaton;
import dk.brics.automaton.State;
import net.sf.javabdd.BDD;
import tau.smlab.syntech.gameinput.model.Player;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.bddgenerator.BDDGenerator;
import tau.smlab.syntech.bddgenerator.sfa.SFAGeneratorFactory;
import tau.smlab.syntech.bddgenerator.sfa.SFAGeneratorFactory.RegExpSFAGeneratorType;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gameinput.triggers.AutomatonTriggerHelper;
import tau.smlab.syntech.gameinput.triggers.DeprecatedTrigger;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.sfa.SFA;
import tau.smlab.syntech.sfa.SFAState;
import tau.smlab.syntech.sfa.SFAUtil;
import tau.smlab.syntech.sfa.SFAs;
/**
 * 
 * A class for transforming {@link TriggerConstraint}s into {@link SFA}s with "non-overlapping" or "simple" semantics.
 * According to this semantics, as long as the current effect has not been fulfilled, following fulfillments of the initiator may not be granted
 * with an effect.
 * 
 * @author Gal Amram
 * @author Or Pistiner
 */
public class SimpleTriggerSFAGenerator implements TriggerSFAGenerator {

	private TriggerConstraint trigger;
	private int traceId;

	private SFA initSfa, effectSfa, triggerSfa;
	private RegExpSFAGeneratorType regExpSfaGenType;

	public SimpleTriggerSFAGenerator(TriggerConstraint trigger, int traceId, RegExpSFAGeneratorType sfaGeneratorType) {
		this.trigger = trigger;
		this.traceId = traceId;
		this.regExpSfaGenType = sfaGeneratorType;
	}

	public SimpleTriggerSFAGenerator(TriggerConstraint trigger, int traceId) {
		this(trigger, traceId, RegExpSFAGeneratorType.SYMBOLIC);
	}

	@Override
	public SFA generateTriggerSfa() {
		if (this.triggerSfa != null) {
			return this.triggerSfa;
		}

		this.initSfa = SFAGeneratorFactory
				.getGenerator(this.regExpSfaGenType, this.trigger.getInitSpecRegExp(), this.traceId)
				.generateRegExpSfa();

		this.effectSfa = SFAGeneratorFactory
				.getGenerator(this.regExpSfaGenType, this.trigger.getEffectSpecRegExp(), this.traceId)
				.generateRegExpSfa();

		//Check if the trigger this.triggerSfa is vacuously true, i.e., its
		//language is TRUE*
		if (this.initSfa.isEmptyLanguage() || this.effectSfa.acceptsTheEmptyString()) {
			// Either the initiator SFA never accepts or the effect SFA accepts the empty
			// word
			return SFAs.trueStarSimpleSfa();
		}

		//Transform the effect SFA into a deterministic, minimal, and complete SFA
		SFA minEffectSfa = this.effectSfa.minimize();
		this.effectSfa.free();
		this.effectSfa = minEffectSfa;
		this.effectSfa.completeTransitionFunction();

		//Check if the initiator SFA (this.initSfa) accepts the empty word; if that's the case, then
		//there is no need to use this.initSfa to construct this.triggerSfa
		if(this.initSfa.acceptsTheEmptyString()) {
			this.initSfa.free();
			SFAState newFinalState = this.effectSfa.newSfaState(true);
			replaceFinalStatesWith(newFinalState, false);
			SFAUtil.addTransitionsToState(newFinalState, this.effectSfa.getIni().getSucc());
			this.triggerSfa = this.effectSfa;
		}
		else {
			//Transform the initiator SFA into a deterministic, minimal, and complete SFA
			SFA minInitSfa = this.initSfa.minimize();
			this.initSfa.free();
			this.initSfa = minInitSfa;
			this.initSfa.completeTransitionFunction();

			replaceFinalStatesWith(this.effectSfa.getIni(), true);
			makeAllInitSfaStatesFinal();
			replaceFinalStatesWith(this.initSfa.getIni(), false);

			this.triggerSfa = this.initSfa;
		}
		return this.triggerSfa;
	}

	private void makeAllInitSfaStatesFinal() {
		Queue<SFAState> worklist = new LinkedList<>();
		Set<SFAState> seen = new HashSet<>();
		worklist.add(this.initSfa.getIni());
		seen.add(this.initSfa.getIni());
		while (!worklist.isEmpty()) {
			SFAState s = worklist.remove();
			s.setAcceptance(true);

			for (SFAState succ : s.getSucc().keySet()) {
				if (succ != this.effectSfa.getIni() && !seen.contains(succ)) {
					worklist.add(succ);
					seen.add(succ);
				}
			}
		}
	}
	
	
	private void replaceFinalStatesWith(SFAState newFinalState, boolean replaceAtInitSfa) {
		SFA sfa;
		if(replaceAtInitSfa) {
			sfa = this.initSfa;
		}
		else {
			sfa = this.effectSfa;
		}

		Queue<SFAState> worklist = new LinkedList<>(), freeingWorklist = new LinkedList<>();
		Set<SFAState> seen = new HashSet<>();
		Set<SFAState> finalSuccs = new HashSet<>();
		worklist.add(sfa.getIni()); //NOTE: we assume that (!sfa.getIni().isAccepting())
		seen.add(sfa.getIni());
		BDD transGuard;
		while (!worklist.isEmpty()) {
			SFAState s = worklist.remove();
			
			for (SFAState succ : s.getSucc().keySet()) {
				if(succ.isAccepting()) {
					finalSuccs.add(succ);
					if(!seen.contains(succ)) {
						freeingWorklist.add(succ);
						seen.add(succ);
					}
				}
				else if (!seen.contains(succ)) {
					worklist.add(succ);
					seen.add(succ);
				}
			}
			
			//Remove all transitions to final states, and redirect them to the new and only final state
			for(SFAState finalSucc : finalSuccs) {
				transGuard = s.getSucc().get(finalSucc).id();
				s.removeTrans(finalSucc);
				s.addTrans(transGuard, newFinalState);
			}
			finalSuccs.clear();
		}
		
		//Free all the transitions' guards reachable ONLY from the "former" final states
		while (!freeingWorklist.isEmpty()) {
			SFAState s = freeingWorklist.remove();
			
			for (SFAState succ : s.getSucc().keySet()) {				
				transGuard = s.getSucc().get(succ);
				if(!transGuard.isFree()) {
					transGuard.free();
				}
				if (!seen.contains(succ)) { //successor reachable from the initial state ONLY via paths that go through a final state
					freeingWorklist.add(succ);
					seen.add(succ);
				}
			}
		}
	}

	
	
	/*
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * Deprecated code.
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 */
	
	
	
	
	/**
	 * this setting is related to triggers verification : when set to true -
	 * additional checks will be performed to verify that the SFAs are indeed
	 * equivalent to the Brics automata. note these checks require more time.
	 * 
	 * when false (Default) - no such checks will be performed.
	 */
	public static boolean verifySFAsAreEquivalentToBrics = false;

	public static void addTriggerConstraints(PlayerModule m, Player p, boolean isAux, boolean trace,
			List<BehaviorInfo> info) {
		// for (DeprecatedTrigger trigger : p.getTriggers()) {
		// // if naive translation is disabled, each trigger must have both init and
		// effect
		// // (brics, non-det.) automatons
		// if (trigger.hasInitEffectAutomatons()) {
		//
		// List<BehaviorInfo> infoList = createModuleTriggerConstraints(m, trigger,
		// isAux);
		//
		// if (trace) {
		// info.addAll(infoList);
		// } else {
		// for (BehaviorInfo i : infoList) {
		// i.free();
		// }
		// }
		// }
		// }
	}

	/**
	 * translate a trigger expression to one or more constraints, add them to the
	 * player module, and return a new Behavior info.
	 * 
	 * @param trigger
	 *            trigger.hasInitEffectAutomatons() == true must hold
	 */
	private static List<BehaviorInfo> createModuleTriggerConstraints(PlayerModule m, DeprecatedTrigger trigger,
			boolean isAux) {

		int traceId = trigger.getTraceId();

		Automaton initAutomaton = trigger.getInitAutomaton();
		Automaton effectAutomaton = trigger.getEffectAutomaton();
		List<String> specSymbols = trigger.getSymbols();
		List<Boolean> isBoolean = trigger.getBooleans();
		List<Integer> fromList = trigger.getFrom();
		List<Integer> toList = trigger.getTo();

		SFA initSFA, effectSFA, minSFA;

		initSFA = translateBricksToSFA(initAutomaton, specSymbols, isBoolean, fromList, toList, traceId);
		effectSFA = translateBricksToSFA(effectAutomaton, specSymbols, isBoolean, fromList, toList, traceId);

		minSFA = initSFA.minimize();
		initSFA.free();
		initSFA = minSFA;

		minSFA = effectSFA.minimize();
		effectSFA.free();
		effectSFA = minSFA;

		/*
		 * make the deterministic initiator automaton with a complete transition
		 * relation: this is critical to the correctness of of the construction because
		 * otherwise there might be new **deadlocks** because of the automaton!!
		 */
		initSFA.completeTransitionFunction();

		/* verification - see that the SFAs and bricks automata are really equivalent */
		if (verifySFAsAreEquivalentToBrics == true) {

			// verifySFAEquivalentToBricks("initator", initSFA, initAutomaton, true /*
			// minimize*/,
			// traceId, specSymbolsMap);
			// verifySFAEquivalentToBricks("effect", effectSFA, effectAutomaton, true /*
			// minimize*/,
			// traceId, specSymbolsMap);
		}

		// Automaton effectSFAComplement = effectSFA.complement();

		/*
		 * if the initSFA never accepts, or the effect SFA always accepts, Changed: if
		 * the effect SFA accepts the empty word then no need to add any constraints...
		 */
		if (initSFA.isEmptyLanguage() || effectSFA.getIni().isAccepting()) {
			initSFA.free();
			effectSFA.free();
			return new ArrayList<BehaviorInfo>();
		}

		SFA triggerSFA = concatenateSFAs(initSFA, effectSFA, traceId);

		initSFA.free();
		effectSFA.free();

		if (verifySFAsAreEquivalentToBrics == true) {

			/*
			 * determinize and minimize the Brics automata. note that this is the same as
			 * the Naive implementation... - for comparison and validation.
			 */
			initAutomaton.determinize();
			initAutomaton.minimize();
			effectAutomaton.determinize();
			effectAutomaton.minimize();

			// Set<String> alphabet = AutomatonTriggerHelper.createAlphabet(specSymbolsMap);

			// Automaton triggerAutomaton =
			// AutomatonTriggerHelper.triggerConcatenate(initAutomaton, effectAutomaton,
			// alphabet);

			// verifySFAEquivalentToBricks("concatenated", triggerSFA, triggerAutomaton,
			// false /* minimize*/,
			// traceId, specSymbolsMap);
		}

		/* sort the states of the concatenated SFA so all initiator states come first */

		List<? extends SFAState> triggerSFAStates = triggerSFA.reachableStates();

		List<SFAState> initSFAStates = getAcceptingStates(triggerSFAStates, true, traceId);
		List<SFAState> effectSFAStates = getAcceptingStates(triggerSFAStates, false, traceId);

		List<SFAState> sortedTriggerSFAStates = new ArrayList<SFAState>();

		sortedTriggerSFAStates.addAll(initSFAStates);
		sortedTriggerSFAStates.addAll(effectSFAStates);

		/* add SFA "aux" variable. (its value represents the current state) */

		String varName = "trigger_" + traceId + "_states";

		int numSFAStates = sortedTriggerSFAStates.size();
		int minSFAVarValue = 0;
		int maxSAFVarValue = numSFAStates - 1;

		try {

			m.addVar(varName, minSFAVarValue, maxSAFVarValue, true);

		} catch (Exception e) {

			throw new RuntimeException("Could not add Trigger aux variable " + varName + " : " + e.getMessage());
		}

		/*
		 * create two lists of BDDs. first list has at index "i" a BDD that says that
		 * the trigger var is at state "i". second list says the same for the primed
		 * version of the trigger.
		 */

		Variable triggerVar = new Variable(varName, new TypeDef(minSFAVarValue, maxSAFVarValue));
		VariableReference triggerVarRef = new VariableReference(triggerVar);
		VariableReference triggerPrimeVarRef = new VariableReference(triggerVar);

		triggerPrimeVarRef.setReferenceName(triggerVarRef.getReferenceName() + "'");

		List<BDD> triggerInStateBDDs = new ArrayList<BDD>();
		List<BDD> triggerInPrimeStateBDDs = new ArrayList<BDD>();

		for (int i = 0; i < sortedTriggerSFAStates.size(); i++) {

			SpecExp triggerInState = new SpecExp(Operator.EQUALS, triggerVarRef, new PrimitiveValue(i));
			triggerInStateBDDs.add(i, BDDGenerator.createBdd(triggerInState, traceId));

			SpecExp triggerPrimeInState = new SpecExp(Operator.EQUALS, triggerPrimeVarRef, new PrimitiveValue(i));
			triggerInPrimeStateBDDs.add(i, BDDGenerator.createBdd(triggerPrimeInState, traceId));
		}

		/*
		 * add a constraint saying that we start at the initial state of the trigger
		 * automaton
		 */

		List<BehaviorInfo> infoList = new ArrayList<BehaviorInfo>();

		int initialSFAStateIndex = sortedTriggerSFAStates.indexOf(triggerSFA.getIni());
		BDD initialStateBDD = triggerInStateBDDs.get(initialSFAStateIndex);

		BehaviorInfo info = new BehaviorInfo();

		info.aux = isAux;
		info.traceId = traceId;
		info.initial = initialStateBDD.id();

		m.conjunctInitial(initialStateBDD.id());

		infoList.add(info);

		/*
		 * add safety constraints (i.e. that Trigger variable changes follow the trigger
		 * automaton's transitions). go over all state pairs (that have transitions)
		 */
		BDD AccumulatedOrBDD = generateAccumulatedOrTransitionsBDD(sortedTriggerSFAStates, triggerInStateBDDs,
				triggerInPrimeStateBDDs);

		info = new BehaviorInfo();
		info.aux = isAux;
		info.traceId = traceId;
		info.safety = AccumulatedOrBDD.id();

		m.conjunctTrans(AccumulatedOrBDD);

		infoList.add(info);

		/*
		 * add justice constraint (we should be in the accepting states of the trigger
		 * automaton infinitely often). note that we created sortedTriggerSFAStates such
		 * that all accepting states come first. the following spec says that trigger
		 * state index < number of accepting states
		 */
		int numInitiatorStates = initSFAStates.size();
		SpecExp triggerInInitiatorStates = new SpecExp(Operator.RIGHT_BIGGER, triggerVarRef,
				new PrimitiveValue(numInitiatorStates));

		BDD result = BDDGenerator.createBdd(triggerInInitiatorStates, traceId);

		info = new BehaviorInfo();
		info.aux = isAux;
		info.traceId = traceId;
		info.justice = result.id();

		m.addJustice(result, traceId);

		infoList.add(info);

		/* explicitly free BDDs */

		Env.free(triggerInStateBDDs); // freeBDDList(triggerInStateBDDs);
		Env.free(triggerInPrimeStateBDDs); // freeBDDList(triggerInPrimeStateBDDs);

		triggerSFA.free();

		return infoList;
	}

	// private static void verifySFAEquivalentToBricks(String automatonName, SFA
	// sfaAutomaton,
	// Automaton bricsAutomaton,
	// boolean minimize, int traceId, Map<Spec, SpecSymbols> specSymbolsMap) {
	//
	// Automaton comparedBricsAutomaton = bricsAutomaton.clone();
	//
	// if (minimize) {
	//
	// comparedBricsAutomaton.determinize();
	// comparedBricsAutomaton.minimize();
	// }
	//
	// SFA comparedSFA = translateBricksToSFA(comparedBricsAutomaton,
	// specSymbolsMap, traceId);
	//
	// if (!sfaAutomaton.isEquivalent(comparedSFA)) {
	//
	// throw new BDDTranslationException(automatonName + " SFA is not equivalent to
	// Brics automaton",
	// traceId);
	// }
	//
	// FreeSFABDDs(comparedSFA);
	// }
	//
	// public static BDD getBDDFromSymbols(Map<String, Set<String>> symbolStringMap,
	// Set<String> symbols) {
	// Map<Integer, Spec> variableByIndex = new LinkedHashMap<Integer, Spec>();
	// for (SpecSymbols specSymbols : specSymbolsMap.values()) {
	// variableByIndex.put(specSymbols.getIndex(), specSymbols.getSpec());
	// }
	//
	// Env.getBDDValue("a", "true"); // true if 'a' is true
	// BDD a = Env.getBDDValue("a", "false"); // true if 'a' is false
	// String[] vars;
	//
	//
	// int numOfSpecs = variableByIndex.size();
	// List<String> symbolsList = new ArrayList<String>(symbols);
	//
	// Spec accumulatedOr = null;
	// for (int i = 0; i < symbolsList.size(); i++) {
	// Spec accumulatedAnd = null;
	// Spec negatedSpec = null; // for "zeros"
	// String symbol = symbolsList.get(i);
	// for (int j = 0; j < numOfSpecs; j++) { // for each specs
	// Spec spec = variableByIndex.get(j);
	// negatedSpec = new SpecExp(Operator.NOT, spec);
	// if (j == 0) {
	// accumulatedAnd = isOneOnIndex(symbol, j)? // Env.getBDDValue(vars[j], "true")
	// : Env.getBDDValue(vars[j], "false");
	// spec : negatedSpec;
	// } else if (isOneOnIndex(symbol, j)) {
	// accumulatedAnd = new SpecExp(Operator.AND, accumulatedAnd, spec);
	// } else {
	// accumulatedAnd = new SpecExp(Operator.AND, accumulatedAnd, negatedSpec);
	// }
	// }
	// if (i == 0) {
	// accumulatedOr = accumulatedAnd;
	// } else {
	// accumulatedOr = new SpecExp(Operator.OR, accumulatedOr, accumulatedAnd);
	// }
	// }
	// return accumulatedOr;
	// }

	private static SFA translateBricksToSFA(Automaton bricsAutomaton, List<String> specSymbols, List<Boolean> isBoolean,
			List<Integer> fromList, List<Integer> toList, int traceId) {

		SFA sfaAutomaton = SFAs.newSimpleSfa();

		List<State> bricsStates = new ArrayList<State>(bricsAutomaton.getStates());

		State bricksInitialState = bricsAutomaton.getInitialState();

		/*
		 * the following list will contain in index "i" the SFA state equivalent to the
		 * Brics state "i" in the bricsStates list
		 */
		List<SFAState> sfaStates = new ArrayList<SFAState>();

		/* first, create all states */

		for (int i = 0; i < bricsStates.size(); i++) {

			State bricsState = bricsStates.get(i);
			SFAState sfaState = sfaAutomaton.newSfaState(bricsState.isAccept());

			sfaStates.add(i, sfaState);
		}

		/* now, set the init state to match the Brics init state */

		sfaAutomaton.setIni(sfaStates.get(bricsStates.indexOf(bricksInitialState)));

		// finally , add the transitions

		for (State sourceBricsState : bricsStates) {

			SFAState sourceSFAState = sfaStates.get(bricsStates.indexOf(sourceBricsState));

			for (State targetBricsState : bricsStates) {

				Set<Integer> allTransitions = AutomatonTriggerHelper.getAllTransitions(sourceBricsState,
						targetBricsState, specSymbols.size()); // This set includes all transitions between
																// sourceBricsState to targetBricsState

				if (allTransitions.size() > 0) {

					BDD transitionBDD = CreateTrantitionsBdd(specSymbols, isBoolean, fromList, toList, allTransitions);

					SFAState targetSFAState = sfaStates.get(bricsStates.indexOf(targetBricsState));

					if (transitionBDD.isZero()) {

						/* FALSE transitions are ignored. */
						transitionBDD.free();
					} else {

						sourceSFAState.addTrans(transitionBDD, targetSFAState);
					}

				}
			}
		}

		/*
		 * cleanup - if there are any unreachable states , free their BDDs. note that
		 * this can occur here because FALSE transitions are ignored - if a Brics state
		 * could only be reached by FALSE transitions, then the equivalent SFA state
		 * would be unreachable.
		 */

		List<? extends SFAState> reachableStates = sfaAutomaton.reachableStates();

		for (SFAState state : sfaStates) {

			if (!reachableStates.contains(state)) {
				state.free();
			}
		}

		verifyValidSfa(sfaAutomaton);

		return sfaAutomaton;
	}

	/**
	 * Receives a set of transitions, represented as integers, and creates a BDD
	 * that represents this transitions.<br>
	 * Each transition (as integer) encodes an assignment. THe BDD returns 'true'
	 * for any of these assignments.
	 * 
	 * @param specSymbols
	 *            - A list that specifies which symbols appear in the automaton.
	 * @param isBoolean
	 *            - A list that specifies which symbols are boolean
	 * @param fromList
	 *            - Specifies the min value of each variable.
	 * @param toList
	 *            - Specifies the max value of each variable.
	 * @param allTransitions
	 *            - A list of integers, each encodes a transition (assignment).
	 * @return
	 */

	private static BDD CreateTrantitionsBdd(List<String> specSymbols, List<Boolean> isBoolean, List<Integer> fromList,
			List<Integer> toList, Set<Integer> allTransitions) {

		BDD ansBDD = Env.FALSE();
		for (int transition : allTransitions) {
			int[] sequence = AutomatonTriggerHelper.createSequenceFromNum(transition, fromList, toList);
			BDD transitionBDD = createBDDfromSequence(specSymbols, isBoolean, sequence);
			ansBDD = ansBDD.or(transitionBDD); // we add the valuation represented by 'transition' to the BDD we
												// construct
		}
		return ansBDD;
	}

	/**
	 * Receives a sequence (assignment) and returns a BDD that accepts (only) this
	 * assignment.
	 * 
	 * @param specSymbols
	 *            - A list of all variables
	 * @param isBoolean
	 *            - A list that specifies which variables are boolean.
	 * @param sequence
	 *            - An assignment as a sequence of Integers.
	 * @return BDD
	 */

	private static BDD createBDDfromSequence(List<String> specSymbols, List<Boolean> isBoolean, int[] sequence) {

		BDD ansBDD = Env.TRUE();
		for (int i = 0; i < specSymbols.size(); i++) {
			BDD toAdd;
			if (isBoolean.get(i)) { // case of a boolean variable
				if (sequence[i] == 0)
					toAdd = Env.getBDDValue(specSymbols.get(i), "false");
				else
					toAdd = Env.getBDDValue(specSymbols.get(i), "true");
			} else // case of a non-boolean variable
				toAdd = Env.getBDDValue(specSymbols.get(i), sequence[i]);
			ansBDD = ansBDD.and(toAdd);
		}
		return ansBDD;
	}

	/**
	 * create one SFA that represents a two way concatenation of two SFAs. that is -
	 * the concatenated SFA would have equivalent states and transitions, however :
	 * 1. transition from initiator states to accepting initiator states will be
	 * replaced by transitions to the effect automaton's initial state. 2.
	 * similarly, transition from effect states to accepting effect states will be
	 * replaced by transitions to the initiator automaton's initial state.
	 * 
	 * note this means that any state that was accepting (in the initator/effect
	 * automata) and was not an initial state (in the initator/effect automata) will
	 * now become unreachable.
	 * 
	 * note that in the concatenated SFA, a state is accepting iff it came from the
	 * initiator automaton.
	 */
	private static SFA concatenateSFAs(SFA initiator, SFA effect, int traceId) {

		/* special case handling : initiator Automaton's initial state is accepting */

		if (initiator.getIni().isAccepting()) {

			return makeEffectSFATrigger(effect, traceId);
		}

		SFA sfaAutomaton = SFAs.newSimpleSfa();

		/* create copies of initiator and effect automata's states */

		List<? extends SFAState> initiatorStateList = initiator.reachableStates();
		List<? extends SFAState> effectStateList = effect.reachableStates();

		List<SFAState> initiatorStateListCopy = cloneStatesList(initiatorStateList);
		List<SFAState> effectStateListCopy = cloneStatesList(effectStateList);

		/* find initial state(s), and set it in the new automaton */

		int oldInitStateIndex = initiatorStateList.indexOf(initiator.getIni());
		SFAState newInitialState = initiatorStateListCopy.get(oldInitStateIndex);

		int effectInitStateIndex = effectStateList.indexOf(effect.getIni());
		SFAState newEffectInitialState = effectStateListCopy.get(effectInitStateIndex);

		sfaAutomaton.setIni(newInitialState);

		/*
		 * copy transitions, replacing transitions to accepting states with transitions
		 * to initial states (of the other automaton)
		 */

		copyAndRelinkTransitions(initiatorStateList, initiatorStateListCopy, newEffectInitialState,
				true /* initiator */, traceId);

		copyAndRelinkTransitions(effectStateList, effectStateListCopy, newInitialState, false /* effect */, traceId);

		/* cleanup - if there are any unreachable states , free their BDDs */

		List<SFAState> allStates = new ArrayList<SFAState>();

		allStates.addAll(initiatorStateListCopy);
		allStates.addAll(effectStateListCopy);

		List<? extends SFAState> reachableStates = sfaAutomaton.reachableStates();

		for (SFAState state : allStates) {

			if (!reachableStates.contains(state)) {
				state.free();
			}
		}

		verifyValidSfa(sfaAutomaton);

		return sfaAutomaton;
	}

	/*
	 * verify that none of the SFA's BDDs (for reachable state) were freed()
	 */
	private static void verifyValidSfa(SFA sfaAutomaton) {

		List<? extends SFAState> sfaStates = sfaAutomaton.reachableStates();

		for (SFAState state : sfaStates) {

			Map<? extends SFAState, BDD> transitions = state.getSucc();

			for (BDD transitionGuard : transitions.values()) {

				if (transitionGuard.isFree()) {

					throw new RuntimeException("Trigger SFA contains freed BDDs " + transitionGuard.toString());
				}
			}
		}
	}

	/**
	 * Copies all the successors of the initial state to be also the successors of
	 * the new (only) accepting state.
	 * 
	 * @param ini
	 *            the initial state
	 * @param acc
	 *            the accepting state
	 */
	private static void copySuccOfIniToNewAcc(SFAState ini, SFAState acc) {
		/* go over all transitions */
		Map<? extends SFAState, BDD> transitions = ini.getSucc();
		for (SFAState successor : transitions.keySet()) {

			BDD transitionGuard = transitions.get(successor);
			/* clone the original transition */
			acc.addTrans(transitionGuard.id(), successor);
		}
	}

	/**
	 * this function does : 1. (deep) copies transition BDDs in stateList (when the
	 * target state is non accepting), and adds them to the matching states in
	 * stateListCopy
	 * 
	 * 2. however, for transitions to an accepting state, replace them with (deep
	 * copied) transitions to targetAcceptingState (with the same BDD)
	 * 
	 * 3. in addition, if these are initiator states, make them all accepting (in
	 * stateListCopy)
	 * 
	 */
	private static void copyAndRelinkTransitions(List<? extends SFAState> stateList, List<SFAState> stateListCopy,
			SFAState targetAcceptingState, boolean isInitiator, int traceId) {

		if (stateList.size() != stateListCopy.size()) {

			throw new RuntimeException("list sizes differ for " + (isInitiator ? "initiator" : "effect")
					+ " lists for trigger with trace id " + traceId);
		}

		for (int i = 0; i < stateList.size(); i++) {

			SFAState originalState = stateList.get(i);
			SFAState clonedState = stateListCopy.get(i);

			/* go over all transitions */
			Map<? extends SFAState, BDD> transitions = originalState.getSucc();

			for (SFAState originalSuccessor : transitions.keySet()) {

				SFAState clonedSuccessor = stateListCopy.get(stateList.indexOf(originalSuccessor));
				BDD transitionBDD = transitions.get(originalSuccessor);

				if (!originalSuccessor.isAccepting()) {

					/* clone the original transition */
					clonedState.addTrans(transitionBDD.id(), clonedSuccessor);
				} else {

					/* replace the original transition with a transition to targetAcceptingState */
					clonedState.addTrans(transitionBDD.id(), targetAcceptingState);
				}
			}

			/* mark all initiator states as accepting */
			if (isInitiator) {

				/* we assume that by default all states in stateListCopy are non accepting */
				clonedState.flipAcceptance();
			}
		}
	}

	/*
	 * create a trigger automaton for the special case where the initiator
	 * automaton's initial state is accepting, i.e., the initiator accepts the empty
	 * word.
	 * 
	 * In this case, we clone the effect automaton's states while replacing
	 * transitions to accepting states with transitions to a NEW accepting state
	 * ACC. ACC is the only accepting state and it has the same successors as of the
	 * initial state.
	 */
	private static SFA makeEffectSFATrigger(SFA effect, int traceId) {// TODO

		SFA sfaAutomaton = SFAs.newSimpleSfa();

		/* create copies of the effect automata's states */

		List<? extends SFAState> effectStateList = effect.reachableStates();
		List<SFAState> effectStateListCopy = cloneStatesList(effectStateList);

		SFAState newAccState = sfaAutomaton.newSfaState(true); // the new accepting state of the automaton

		/* Set the initial state of the automaton */
		int effectInitStateIndex = effectStateList.indexOf(effect.getIni());
		SFAState newEffectInitialState = effectStateListCopy.get(effectInitStateIndex);
		sfaAutomaton.setIni(newEffectInitialState);

		/*
		 * copy transitions, replacing transitions to old accepting states with
		 * transitions to the new accepting state
		 */

		copyAndRelinkTransitions(effectStateList, effectStateListCopy, newAccState, false /* effect */, traceId);

		/*
		 * Copy successors of the initial state to be also successors of the new
		 * accepting state
		 */
		copySuccOfIniToNewAcc(newEffectInitialState, newAccState);

		/* cleanup - if there are any unreachable states , free their BDDs */

		effectStateListCopy.add(newAccState);

		List<? extends SFAState> reachableStates = sfaAutomaton.reachableStates();

		for (SFAState state : effectStateListCopy) {

			if (!reachableStates.contains(state)) {
				state.free();
			}
		}

		verifyValidSfa(sfaAutomaton);

		return sfaAutomaton;

	}

	/*
	 * create a list of states of the same size as sourceStateList. note that all
	 * cloned states are marked as non-accepting (this can be flipped later).
	 */
	private static List<SFAState> cloneStatesList(List<? extends SFAState> sourceStatesList) {

		List<SFAState> ClonedStateList = new ArrayList<SFAState>();

		for (int i = 0; i < sourceStatesList.size(); i++) {

			ClonedStateList.add(SFAs.newSimpleSfaState(false));
		}

		return ClonedStateList;
	}

	// returns a list of all accepting/non-accepting states (depends on the value of
	// 'accepting')

	private static List<SFAState> getAcceptingStates(List<? extends SFAState> triggerSFAStates, boolean accepting,
			int traceId) {

		List<SFAState> stateList = new ArrayList<SFAState>();

		for (SFAState s : triggerSFAStates) {

			if (s.isAccepting() == accepting) {

				stateList.add(s);
			}
		}

		if (stateList.size() == 0) {

			throw new RuntimeException("got an unexpectedly empty " + (accepting ? "initiator" : "effect")
					+ " list for trigger with trace id " + traceId);
		}

		return stateList;
	}

	/**
	 * create a BDD that represents the OR (disjunction) of all allowed transitions
	 * (BDDs), where an individual transition is of the form : source state (index)
	 * AND transition (BDD) AND NEXT target state (index)
	 */
	private static BDD generateAccumulatedOrTransitionsBDD(List<SFAState> sortedTriggerSFAStates,
			List<BDD> triggerInStateBDDs, List<BDD> triggerInPrimeStateBDDs) {

		BDD AccumulatedOrBDD = Env.FALSE();

		for (SFAState sourceState : sortedTriggerSFAStates) {

			Map<? extends SFAState, BDD> transitions = sourceState.getSucc();

			for (SFAState targetState : transitions.keySet()) {

				int sourceStateIndex = sortedTriggerSFAStates.indexOf(sourceState);
				int targetStateIndex = sortedTriggerSFAStates.indexOf(targetState);

				BDD sourceStateBDD = triggerInStateBDDs.get(sourceStateIndex);
				BDD targetStateBDD = triggerInPrimeStateBDDs.get(targetStateIndex);

				BDD TransitionBDD = transitions.get(targetState);

				/*
				 * the following BDD implies that being at the source state + encountering
				 * "transition" leads to the target state.
				 */
				BDD result1 = TransitionBDD.and(targetStateBDD);
				BDD result = sourceStateBDD.and(result1);

				result1.free();

				BDD previousAccumulatedOrBDD = AccumulatedOrBDD;
				AccumulatedOrBDD = result.or(previousAccumulatedOrBDD);

				result.free();
				previousAccumulatedOrBDD.free();
			}
		}

		return AccumulatedOrBDD;
	}

}