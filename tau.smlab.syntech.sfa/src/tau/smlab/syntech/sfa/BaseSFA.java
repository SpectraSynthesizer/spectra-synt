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

package tau.smlab.syntech.sfa;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.Stack;

//imports for toString() method (returns a DOT representation of the SFA)
import org.eclipse.gef.dot.internal.DotAttributes;
import org.eclipse.gef.dot.internal.DotExport;
import org.eclipse.gef.dot.internal.language.dot.GraphType;
import org.eclipse.gef.graph.Graph;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.sfa.PowerSetIterator.PowerSetIteratorType;


/**
 * 
 * @author Maxim Finkel
 * @author Or Pistiner
 *
 * @param <T> the type of the states the SFA has
 */
@SuppressWarnings("restriction")
public abstract class BaseSFA<T extends BaseSFAState<T>> implements SFA {
	
	final public static String EPS_LABEL = "\u03B5"; //Unicode code point of 'Epsilon'
	
	protected T ini; // the initial state of this automaton
	protected PowerSetIteratorType psIterType; // the type of the powerset (of outgoing transitions) iterator used for by the determinization algorithm
	
	protected BaseSFA(SFAState ini, PowerSetIteratorType psIterType) {
		this.setIni(ini);
		this.psIterType = psIterType;
	}

	protected BaseSFA() {
		this(null, PowerSetIteratorType.EFFICIENT);
	}

	protected BaseSFA(SFAState ini) {
		this(ini, PowerSetIteratorType.EFFICIENT);
	}

	protected BaseSFA(PowerSetIteratorType psIterType) {
		this(null, psIterType);
	}

	protected abstract BaseSFA<T> newSfaInstance();

	protected abstract BaseSFA<T> newSfaInstance(T ini);

	@Override
	public T getIni() {
		return ini;
	}
	
	@Override
	public boolean enablesEpsTrans() {
		return false;
	}
	
	
	@Override
	public abstract T newSfaState(boolean isAccepting);
	
	/**
	 * Returns the type information of the states this automaton has.
	 * 
	 * @return
	 */
	protected abstract Class<T> getStatesType();
	
	/**
	 * Sets the initial state of this automaton.
	 */
	@SuppressWarnings("unchecked")
	@Override
	public void setIni(SFAState ini) {
		if ((ini != null) && (!ini.getClass().equals(this.getStatesType()))) {
			throw new SFAException("The specified state has an invalid type. The initial state must be of type " + this.getStatesType().getSimpleName());
		}
		this.ini = (T) ini;
	}

	@Override
	public PowerSetIteratorType getPsIterType() {
		return psIterType;
	}

	@Override
	public void setPsIterType(PowerSetIteratorType powerSetIteratorType) {
		this.psIterType = powerSetIteratorType;
	}

	@Override
	public Set<T> finalStates() {
		Queue<T> worklist = new LinkedList<>();
		Set<T> seen = new LinkedHashSet<>();
		Set<T> finalStates = new LinkedHashSet<>();
		worklist.add(ini);
		seen.add(ini);
		while (!worklist.isEmpty()) {
			T s = worklist.remove();
			if (s.isAccepting()) {
				finalStates.add(s);
			}
			// add successors which have not been checked yet
			for (T succ : s.getSuccessors()) {
				if (!seen.contains(succ)) {
					worklist.add(succ);
					seen.add(succ);
				}
			}
		}
		return finalStates;
	}

	@Override
	public void free() {
		Queue<T> worklist = new LinkedList<>();
		Set<T> seen = new LinkedHashSet<>();

		worklist.add(ini);
		seen.add(ini);

		while (!worklist.isEmpty()) {
			T s = worklist.remove();

			// free the guards of all outgoing transitions
			for (BDD guard : s.getSucc().values()) {
				if (!guard.isFree()) {
					guard.free();
				}
			}

			// add successors which have not been searched
			for (T succ : s.getSuccessors()) {
				if (!seen.contains(succ)) {
					worklist.add(succ);
					seen.add(succ);
				}
			}
		}
	}

	@Override
	public boolean isDeterministic() {
		Queue<T> worklist = new LinkedList<>();
		Set<T> seen = new LinkedHashSet<>();
		worklist.add(ini);
		seen.add(ini);
		while (!worklist.isEmpty()) {
			T s = worklist.remove();
			// check determinism of outgoing transitions
			if (hasOverlap(s.getSucc().values())) {
				return false;
			}
			// add successors not checked yet
			for (T succ : s.getSuccessors()) {
				if (!seen.contains(succ)) {
					worklist.add(succ);
					seen.add(succ);
				}
			}
		}
		return true;
	}
	
	@Override
	public boolean hasOneFinalState() {
		return false;
	}

	@Override
	public SFAState getFinalState() {
		throw new SFAException("This automaton does not have a single, unique final state.");
	}

	@Override
	public void setFinalState(SFAState finalState) {
		throw new SFAException("This automaton does not have a single, unique final state.");
	}
	
	@Override
	public abstract BaseSFA<T> copy();

	
	@Override
	public BaseSFA<T> eliminateEpsTrans() {
		return this.copy();
	}
	
	@Override
	public boolean hasEpsTrans() {
		return false;
	}
	
	
	@Override
	public BaseSFA<T> determinize() {
		return this.buildDetAutomaton();
	}
	
	/**
	 * Returns a deterministic SFA (DSFA) that is equivalent to this SFA,
	 * which is assumed to be without epsilon transitions.
	 * 
	 * @return
	 */
	protected BaseSFA<T> buildDetAutomaton() {

		// create a fresh copy of this automaton and remove all the dead states from the
		// fresh copy
		BaseSFA<T> copySfa = this.copy();
		copySfa.removeDeadStates();

		// check if the removal of dead states has resulted in a deterministic automaton
		if (copySfa.isDeterministic()) {
			return copySfa; // we have a deterministic automaton so we are done
		}

		// we have that copySfa is non-deterministic so we need to determinize it
		BaseSFA<T> deterministicAutomaton = this.newSfaInstance();

		/*
		 * To distinguish between different states in the DSFA, we need to remember a
		 * mapping between sets of states in this SFA to the new states in the DSFA that
		 * we create.
		 */
		Map<Set<T>, T> stateSubsetsToDeterministicStatesMapping = new HashMap<>();
		Set<T> iniSingletonSet = new HashSet<>(Arrays.asList(copySfa.ini));

		/*
		 * The initial state in the DSFA is a singleton containing the SFA's initial
		 * state.
		 */
		T determinsticAutomatonInitialState = copySfa.ini.cloneWithoutSucc();
		stateSubsetsToDeterministicStatesMapping.put(iniSingletonSet, determinsticAutomatonInitialState);

		deterministicAutomaton.setIni(determinsticAutomatonInitialState);

		/*
		 * We use a stack to do a DFS search on (the copy without dead states of) this
		 * SFA. It will contain the state sets of the DSFA that we create.
		 */
		List<Set<T>> workStack = new Stack<>();
		workStack.add(0, iniSingletonSet);
		while (!workStack.isEmpty()) {
			Map<T, BDD> allTransitionsMap = new HashMap<>();
			Set<T> currentStateSet = workStack.remove(0), tautologySuccs = new HashSet<>();
			for (T state : currentStateSet) {
				for (Map.Entry<T, BDD> transition : state.getSucc().entrySet()) {
					/*
					 * Optimization 1: We don't need to add to the transitions set (denoted by
					 * delta_A(q) in the paper) unsatisfiable transitions, since each subset t of
					 * delta_A(q) that contains that transition will result a unsatisfiable formula
					 * phi_t, since the left conjunction will not be satisfiable.
					 *
					 * Optimization 2: If a subset t of delta_A(q) DOESN'T contains a tautology
					 * transition, then the resulting phi_t will be unsatisfiable, since the right
					 * conjunction will be unsatisfiable. Thus, it suffice to go over delta_A(q)'s
					 * subsets that contain all delta_A(q)'s tautology transitions, which is
					 * equivalent to going over all the subsets of delta_A(q)\{tautologies} and
					 * adding to each subset {tautologies} (the set of all tautology transitions).
					 * 
					 * Optimization 3: If there are multiple transitions to the SAME target state,
					 * then we can transform them into a single transition by taking their
					 * disjunction. This may save (exponentially many but redundant) iterations over
					 * subsets of delta_A(q) that contain different combinations of transitions to
					 * the SAME successor state. In case the disjunction evaluates to a tautology
					 * (i.e., it is trivially true), then we treat the "new" transition as in
					 * Optimization 2.
					 * 
					 */
					if (!transition.getValue().isZero()) {
						if (allTransitionsMap.containsKey(transition.getKey())) {
							allTransitionsMap.get(transition.getKey()).orWith(transition.getValue().id());
							if (allTransitionsMap.get(transition.getKey()).isOne()) {
								// the guard of the transition to the successor state transition.getKey() is
								// TRUE
								allTransitionsMap.get(transition.getKey()).free();
								allTransitionsMap.remove(transition.getKey());
								tautologySuccs.add(transition.getKey());
							}
						} else if (!tautologySuccs.contains(transition.getKey())) {
							// this is the first time a transition to this target/successor state is seen
							if (transition.getValue().isOne()) {
								// tautology transition
								tautologySuccs.add(transition.getKey());
							} else {
								allTransitionsMap.put(transition.getKey(), transition.getValue().id());
							}
						}
					}
				}
			}
			
			/*
			 * Optimization 3: If delta_A(q) contains two transitions with semantically equivalent guards, psi_1 and psi_2, then a subset t that only contains
			 * one of them would yield an unsatisfiable phi_t (since psi_1 and not(psi_2) is unsatisfiable).
			 * Thus, we only consider subsets of delta_A(q) that either contain ALL the transitions in delta_A(q) that have 
			 * equivalent guards or contain none of them. To achieve this, we only iterate over satisfiable subsets of delta_A(q) that contain
			 * at most one (arbitrarily chosen) representative of each subset of equivalent transitions in delta_A(q).
			 *
			 */
			
			//Map each "representative" successor state to all other successor states that have semantically equivalent transitions' guards (BDDs)
			//Note: this might be an empty set			
			Map<T, Set<T>> reprSuccToSameGuardSuccs = new HashMap<>();
			Set<T> sameGuardSuccs, seenSuccs = new HashSet<>();
			BDD succGuard;
			for(T succ : allTransitionsMap.keySet()) {
				if(!seenSuccs.contains(succ)) {
					seenSuccs.add(succ);
					sameGuardSuccs = new HashSet<>();				
					reprSuccToSameGuardSuccs.put(succ, sameGuardSuccs);
					succGuard = allTransitionsMap.get(succ);
					for(T otherSucc : allTransitionsMap.keySet()) {
						if(succ != otherSucc && allTransitionsMap.get(otherSucc).equals(succGuard)) {
							sameGuardSuccs.add(otherSucc);
							seenSuccs.add(otherSucc);
						}
					}
				}
			}
			
			Map<T, BDD> reprTransitionsMap = new HashMap<>();
			for(T repr : reprSuccToSameGuardSuccs.keySet()) {
				reprTransitionsMap.put(repr, allTransitionsMap.get(repr));
			}
			
			allTransitionsMap = reprTransitionsMap;
			
			Set<Map.Entry<T, BDD>> allTransitionsSet = allTransitionsMap.entrySet();
			Iterator<Pair<Set<Map.Entry<T, BDD>>, BDD>> psIter = PowerSetIterator.getIterator(this.psIterType, allTransitionsSet);
			Pair<Set<Map.Entry<T, BDD>>, BDD> nextPair;

			Set<Map.Entry<T, BDD>> transitionsSubset, transitionsSubsetComplement;
			boolean newDeterministicStateIsAccepting;
			Set<T> allSuccStatesSet, missingSameGuardSuccs;
			BDD transitionsGuardsConjunction;
			T detCurrentState = stateSubsetsToDeterministicStatesMapping.get(currentStateSet), detSuccessorState;

			while (psIter.hasNext()) {
				nextPair = psIter.next();
				transitionsSubset = nextPair.getLeft();
				
				/*
				 * An empty transitions set is not interesting since its successor states set is
				 * empty (this corresponds to taking no transitions at all).
				 */
				if (!transitionsSubset.isEmpty() || !tautologySuccs.isEmpty()) {
					transitionsGuardsConjunction = nextPair.getRight();
					transitionsSubsetComplement = SFAUtil.getComplementOfTransSet(allTransitionsSet, transitionsSubset);
					for (Map.Entry<T, BDD> t : transitionsSubsetComplement) {
						if (transitionsGuardsConjunction.isZero()) {
							break;
						}
						transitionsGuardsConjunction.andWith(t.getValue().not());
					}
					if (!transitionsGuardsConjunction.isZero()) {
						allSuccStatesSet = SFAUtil.getTransitionsTargetStates(transitionsSubset);
						
						//Add successor states that we removed/omitted, each of which has a transition guard equivalent to that of
						//a "representative" successor state in allSuccStatesSet (See Optimization 3 above)
						missingSameGuardSuccs = new HashSet<>();
						for(T repr : allSuccStatesSet) {
							missingSameGuardSuccs.addAll(reprSuccToSameGuardSuccs.get(repr));
						}						
						allSuccStatesSet.addAll(missingSameGuardSuccs);
						
						//Add successor states with TRUE (tautology) guards (see Optimization 2 above)
						allSuccStatesSet.addAll(tautologySuccs);
						
						if (!stateSubsetsToDeterministicStatesMapping.containsKey(allSuccStatesSet)) {
							/*
							 * If some state in successorStatesSet is an accepting SFA state, then the
							 * corresponding new DFSA state should be an accepting state (as indicated by
							 * 'newDeterministicStateIsAccepting').
							 */
							newDeterministicStateIsAccepting = SFAUtil.containsAcceptingState(allSuccStatesSet);
							detSuccessorState = this.newSfaState(newDeterministicStateIsAccepting);

							stateSubsetsToDeterministicStatesMapping.put(allSuccStatesSet, detSuccessorState);
							workStack.add(0, allSuccStatesSet);
						} else {
							detSuccessorState = stateSubsetsToDeterministicStatesMapping.get(allSuccStatesSet);
						}
						detCurrentState.addTrans(transitionsGuardsConjunction, detSuccessorState);
					} else {
						transitionsGuardsConjunction.free();
					}
				}
			}
			SFAUtil.freeTransitionsSet(allTransitionsSet);
		}
		copySfa.free();
		return deterministicAutomaton;
	}

	@Override
	public void completeTransitionFunction() {
		// Create a black hole, sink state, from where we are stuck forever.
		T sinkState = this.newSfaState(false);
		sinkState.addTrans(Env.TRUE(), sinkState);

		Queue<T> worklist = new LinkedList<>();
		List<T> seen = new ArrayList<>();
		worklist.add(ini);
		seen.add(ini);
		while (!worklist.isEmpty()) {
			T s = worklist.remove();

			// add successors not checked yet and complete the transition function
			BDD blackHoleTransGuard = Env.TRUE();
			for (Map.Entry<T, BDD> transition : s.getSucc().entrySet()) {
				if (!seen.contains(transition.getKey())) {
					worklist.add(transition.getKey());
					seen.add(transition.getKey());
				}
				blackHoleTransGuard.andWith(transition.getValue().not());
			}

			// if there is an undefined transition for some assignment
			if (!blackHoleTransGuard.isZero()) {
				s.addTrans(blackHoleTransGuard, sinkState);
			} else {
				blackHoleTransGuard.free();
			}
		}
	}


	@Override
	public BaseSFA<T> minimize() {
		if (this.isEmptyLanguage()) {
			T initialState = this.newSfaState(false);
			initialState.addTrans(Env.TRUE(), initialState);
			return this.newSfaInstance(initialState);
		}

		BaseSFA<T> deterministicAutomaton = this.determinize();
		deterministicAutomaton.completeTransitionFunction();

		// Moore�s minimization algorithm (a.k.a. the standard algorithm) lifted to SFAs
		
		// find all reachable states
		List<T> reachableStates = deterministicAutomaton.reachableStates();

		// remember each seen state's index in the seen list
		Map<T, Integer> seenStatesIndices = new HashMap<>();
		for (int i = 0; i < reachableStates.size(); ++i) {
			seenStatesIndices.put(reachableStates.get(i), i);
		}
		
		// initialize the equivalence relation E
		Set<Pair<T, T>> e = new HashSet<>();
		for (int i = 0; i < reachableStates.size(); ++i) {
			for (int j = 0; j < i; ++j) {
				T p = reachableStates.get(i);
				T q = reachableStates.get(j);
				if ((p.isAccepting() && q.isAccepting()) || (!p.isAccepting() && !q.isAccepting())) {
					e.add(new Pair<T, T>(p, q));
				}
			}
		}

		// refine E
		Set<Pair<T, T>> pairsToDelete = new HashSet<>();
		while (true) {
			for (Pair<T, T> pair : e) {
				T p = pair.getLeft();
				T q = pair.getRight();
				for (Map.Entry<T, BDD> pTransition : p.getSucc().entrySet()) {
					for (Map.Entry<T, BDD> qTransition : q.getSucc().entrySet()) {
						T p1 = pTransition.getKey();
						T q1 = qTransition.getKey();
						if (!p1.equals(q1) && !e.contains(new Pair<T, T>(p1, q1))
								&& !e.contains(new Pair<T, T>(q1, p1))) {	
							BDD pTransitionBdd = pTransition.getValue();
							BDD qTransitionBdd = qTransition.getValue();
							BDD conjunctionBdd = pTransitionBdd.and(qTransitionBdd);
							if(!conjunctionBdd.isZero()) {
								pairsToDelete.add(pair);
							}
							conjunctionBdd.free();							
						}
					}
				}
			}

			if (pairsToDelete.isEmpty()) {
				// reached a fixed point!
				break;
			}

			e.removeAll(pairsToDelete);
			pairsToDelete.clear();
		}

		// compute E-equivalence classes using a union find data structure
		QuickUnionPathCompressionUF unionFindDataStructure = new QuickUnionPathCompressionUF(reachableStates.size());
		for (Pair<T, T> pair : e) {
			unionFindDataStructure.union(seenStatesIndices.get(pair.getLeft()), seenStatesIndices.get(pair.getRight()));
		}

		// build the minimal automaton
		Map<T, T> eClassReprStateToMinimalSfaStateMap = new HashMap<>();
		T initialStateRepresentative = reachableStates
				.get(unionFindDataStructure.find(seenStatesIndices.get(deterministicAutomaton.ini)));
		eClassReprStateToMinimalSfaStateMap.put(initialStateRepresentative,
				initialStateRepresentative.cloneWithoutSucc());

		Queue<T> worklist = new LinkedList<>();
		worklist.add(initialStateRepresentative);
		while (!worklist.isEmpty()) {
			T s = worklist.remove();
			// add successors not checked yet
			for (Map.Entry<T, BDD> transition : s.getSucc().entrySet()) {
				T succStateRepresentative = reachableStates
						.get(unionFindDataStructure.find(seenStatesIndices.get(transition.getKey())));
				if (!eClassReprStateToMinimalSfaStateMap.containsKey(succStateRepresentative)) {
					eClassReprStateToMinimalSfaStateMap.put(succStateRepresentative,
							succStateRepresentative.cloneWithoutSucc());
					worklist.add(succStateRepresentative);
				}
				eClassReprStateToMinimalSfaStateMap.get(s).addTrans(transition.getValue().id(),
						eClassReprStateToMinimalSfaStateMap.get(succStateRepresentative));
			}
		}

		BaseSFA<T> minimalAutomaton = this.newSfaInstance();
		minimalAutomaton.setIni(eClassReprStateToMinimalSfaStateMap.get(initialStateRepresentative));
		minimalAutomaton.removeDeadStates();
		return minimalAutomaton;
	}

	@Override
	public List<T> reachableStates() {
		Queue<T> worklist = new LinkedList<>();
		List<T> seen = new ArrayList<>();
		worklist.add(ini);
		seen.add(ini);
		while (!worklist.isEmpty()) {
			T s = worklist.remove();

			// add successors not checked yet
			for (T nextState : s.getSuccessors()) {
				if (!seen.contains(nextState)) {
					worklist.add(nextState);
					seen.add(nextState);
				}
			}
		}
		return seen;
	}

	@Override
	public int numStates() {
		return reachableStates().size();
	}

	/**
	 * Computes this Automaton's complement, by determinizing it and flipping its
	 * states' acceptance.
	 * 
	 * NOTE: IN ORDER FOR THIS ALGORITHM TO WORK, WE MUST MAKE SURE THAT THE
	 * TRANSITION FUNCTION IS COMPLETE, AND THAT WE ACHIVE WITH THE
	 * completeTransitionFunction() METHOD!
	 */
	@Override
	public BaseSFA<T> complement() {
		BaseSFA<T> deterministicAutomaton = this.determinize();
		deterministicAutomaton.completeTransitionFunction();

		Queue<T> worklist = new LinkedList<>();
		List<T> seen = new ArrayList<>();
		worklist.add(deterministicAutomaton.ini);
		seen.add(deterministicAutomaton.ini);
		while (!worklist.isEmpty()) {
			T s = worklist.remove();
			// Flip each accepting (final) state to a non-accepting one, and vice-versa
			s.flipAcceptance();

			// Add successors not checked yet
			for (T nextState : s.getSuccessors()) {
				if (!seen.contains(nextState)) {
					worklist.add(nextState);
					seen.add(nextState);
				}
			}
		}

		return deterministicAutomaton;
	}
	
	@Override
	public void removeDeadStates() {
		// search for all final states, while building the reversed automaton
		Set<T> finalStates = new HashSet<>();
		Map<T, T> originalToReversedStatesMapping = new HashMap<>();

		Queue<T> worklist = new LinkedList<>();
		worklist.add(ini);
		originalToReversedStatesMapping.put(ini, ini.cloneWithoutSucc());
		BDD dummyGuard = Env.TRUE();

		while (!worklist.isEmpty()) {
			T s = worklist.remove();
			if (s.isAccepting()) {
				finalStates.add(s);
			}

			// add successors not checked yet
			for (T nextState : s.getSuccessors()) {
				if (!originalToReversedStatesMapping.containsKey(nextState)) {
					worklist.add(nextState);
					originalToReversedStatesMapping.put(nextState, nextState.cloneWithoutSucc());
				}

				// build reversed edges in the reversed automaton
				// the transitions' guards don't really matter so we add the SAME dummy TRUE
				// guard for all transitions (to save memory and avoid memory leaks in the BDD
				// engine)

				// originalToReversedStatesMapping.get(nextState).addTrans(Env.TRUE(),
				// originalToReversedStatesMapping.get(s));

				originalToReversedStatesMapping.get(nextState).getSucc().put(originalToReversedStatesMapping.get(s),
						dummyGuard);
			}
		}

		// search for reachable states from the final states in the reversed automaton
		List<T> seen = new ArrayList<>();
		Set<T> reachableFromFinalStatesInReversed = new HashSet<>();
		for (T finalState : finalStates) {
			worklist.clear();
			worklist.add(originalToReversedStatesMapping.get(finalState));

			seen.clear();
			seen.add(originalToReversedStatesMapping.get(finalState));

			while (!worklist.isEmpty()) {
				T sRev = worklist.remove();

				// add successors not checked yet
				for (T nextStateRev : sRev.getSuccessors()) {
					if (!seen.contains(nextStateRev)) {
						worklist.add(nextStateRev);
						seen.add(nextStateRev);
					}
				}
			}

			reachableFromFinalStatesInReversed.addAll(seen);
		}

		// free the dummy guard
		dummyGuard.free();

		// delete all states in the original automaton which are reachable from the
		// initial state but from which no accepting state can be reached
		worklist.clear();
		seen.clear();
		worklist.add(ini);
		seen.add(ini);
		while (!worklist.isEmpty()) {
			T s = worklist.remove();

			// mark all transitions to dead states
			Set<T> nextSatesToRemove = new HashSet<>();
			for (T nextState : s.getSuccessors()) {
				if (!reachableFromFinalStatesInReversed.contains(originalToReversedStatesMapping.get(nextState))) {
					nextSatesToRemove.add(nextState);
				}
			}

			// delete all dead transitions
			for (T nextState : nextSatesToRemove) {
				s.removeTrans(nextState);
			}

			// add successors not checked yet
			for (T nextState : s.getSuccessors()) {
				if (!seen.contains(nextState)) {
					worklist.add(nextState);
					seen.add(nextState);
				}
			}
		}
	}

	@Override
	public boolean isEmptyLanguage() {
		if (ini.isAccepting()) {
			return false;
		}

		Queue<T> worklist = new LinkedList<>();
		List<T> seen = new ArrayList<>();
		worklist.add(ini);
		seen.add(ini);
		while (!worklist.isEmpty()) {
			T s = worklist.remove();

			for (T succ : s.getSuccessors()) {
				if (succ.isAccepting()) {
					return false;
				}

				// add the current successor if it has not been searched yet
				if (!seen.contains(succ)) {
					worklist.add(succ);
					seen.add(succ);
				}
			}
		}
		return true;
	}

	@Override
	public boolean isSubsetOf(SFA other) {
		SFA notOtherSfa = other.complement();
		SFA thisAndNotOtherSfa = SFAs.productSfa(this, notOtherSfa);
		boolean isSubset = thisAndNotOtherSfa.isEmptyLanguage();
		
		//Free BDDs
		notOtherSfa.free();
		thisAndNotOtherSfa.free();
		
		return isSubset;
	}

	@Override
	public boolean isEquivalent(SFA other) {
		return this.isSubsetOf(other) && other.isSubsetOf(this);
	}
	
	@Override
	public boolean isTrueStarLanguage() {
		SFA trueStarSfa = SFAs.trueStarSimpleSfa();
		boolean thisIsTrueStar = trueStarSfa.isSubsetOf(this);
		trueStarSfa.free();
		return thisIsTrueStar;
	}
	
	@Override
	public boolean acceptsTheEmptyString() {
		return this.ini.isAccepting();
	}
	
	/*
	 * Creates a string representation of this SFA in the DOT language.
	 */
	@Override
	public String toString() {
		if(ini == null) {return super.toString(); }
		
		Graph.Builder builder = new Graph.Builder().attr(DotAttributes::_setType, GraphType.DIGRAPH).attr(DotAttributes::setRankdir, "LR");
		
		Queue<T> worklist = new LinkedList<>();
		List<T> seen = new ArrayList<>();
		worklist.add(ini);
		seen.add(ini);
		addDotNode(builder, ini, 0);
		
		while (!worklist.isEmpty()) {
			T s = worklist.remove();
			
			// add successors not checked yet
			for (Map.Entry<T, BDD> transition : s.getSucc().entrySet()) {
				if (!seen.contains(transition.getKey())) {
					worklist.add(transition.getKey());
					seen.add(transition.getKey());
					addDotNode(builder, transition.getKey(), seen.size()-1);
				}
				builder.edge(s, transition.getKey()).attr(DotAttributes::setLabel, Env.toNiceString(transition.getValue()));
			}
			
			if(s.hasEpsSuccessors()) {
				for(T epsSucc : s.getEpsSucc()) {
					if (!seen.contains(epsSucc)) {
						worklist.add(epsSucc);
						seen.add(epsSucc);
						addDotNode(builder, epsSucc, seen.size()-1);
					}
					builder.edge(s, epsSucc).attr(DotAttributes::setLabel, BaseSFA.EPS_LABEL);
				}
			}
		}
		return new DotExport().exportDot(builder.build());
	}
	
	/*
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 **************************************************************
	 * Private methods*********************************************
	 **************************************************************
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 */
	private void addDotNode(Graph.Builder builder, T sfaState, int stateIdx) {
		if(sfaState.isAccepting()) {
			builder.node(sfaState).attr(DotAttributes::_setName, "s" + stateIdx).attr(DotAttributes::setLabel, "s" + stateIdx).attr(DotAttributes::setShape, "doublecircle");
		}
		else {
			builder.node(sfaState).attr(DotAttributes::_setName, "s" + stateIdx).attr(DotAttributes::setLabel, "s" + stateIdx).attr(DotAttributes::setShape, "circle"); 
		}
		if(sfaState == ini) {
			T dummyState = this.newSfaState(false);
			builder.node(dummyState).attr(DotAttributes::_setName, "start").attr(DotAttributes::setShape, "point");
			builder.edge(dummyState, sfaState);
		}
	}
	
	
	/**
	 * Checks whether the given collection of BDDs has no overlap between any two.
	 * 
	 * @param values
	 * @return
	 */
	private boolean hasOverlap(Collection<BDD> values) {
		BDD seen = Env.FALSE(), inter;
		for (BDD b : values) {
			inter = b.and(seen);
			if (!inter.isZero()) {
				inter.free();
				seen.free();
				return true;
			}
			inter.free();
			seen.orWith(b.id());
		}
		seen.free();
		return false;
	}
	
	/*
	 * 
	 * 
	 * 
	 * 
	 * ***********************************
	 * Private nested classes
	 * **********************************
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
	 * CODE COPIED FROM:
	 * https://algs4.cs.princeton.edu/15uf/QuickUnionPathCompressionUF.java.html
	 * 
	 * The {@code QuickUnionPathCompressionUF} class represents a union find data
	 * structure. It supports the <em>union</em> and <em>find</em> operations, along
	 * with methods for determining whether two sites are in the same component and
	 * the total number of components.
	 * <p>
	 * This implementation uses quick union (no weighting) with full path
	 * compression. Initializing a data structure with <em>n</em> sites takes linear
	 * time. Afterwards, <em>union</em>, <em>find</em>, and <em>connected</em> take
	 * logarithmic amortized time <em>count</em> takes constant time.
	 * <p>
	 * For additional documentation, see
	 * <a href="https://algs4.cs.princeton.edu/15uf">Section 1.5</a> of
	 * <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
	 * 
	 * @author Robert Sedgewick
	 * @author Kevin Wayne
	 */
	@SuppressWarnings("unused")
	static private class QuickUnionPathCompressionUF {
		private int[] id; // id[i] = parent of i
		private int count; // number of components

		/**
		 * Initializes an empty union�find data structure with n isolated components 0
		 * through n-1.
		 * 
		 * @param n
		 *            the number of sites
		 * @throws java.lang.IllegalArgumentException
		 *             if n < 0
		 */
		QuickUnionPathCompressionUF(int n) {
			count = n;
			id = new int[n];
			for (int i = 0; i < n; i++) {
				id[i] = i;
			}
		}

		/**
		 * Returns the number of components.
		 *
		 * @return the number of components (between {@code 1} and {@code n})
		 */
		int count() {
			return count;
		}

		/**
		 * Returns the component identifier for the component containing site {@code p}.
		 *
		 * @param p
		 *            the integer representing one object
		 * @return the component identifier for the component containing site {@code p}
		 * @throws IllegalArgumentException
		 *             unless {@code 0 <= p < n}
		 */
		int find(int p) {
			int root = p;
			while (root != id[root])
				root = id[root];
			while (p != root) {
				int newp = id[p];
				id[p] = root;
				p = newp;
			}
			return root;
		}

		/**
		 * Returns true if the the two sites are in the same component.
		 *
		 * @param p
		 *            the integer representing one site
		 * @param q
		 *            the integer representing the other site
		 * @return {@code true} if the two sites {@code p} and {@code q} are in the same
		 *         component; {@code false} otherwise
		 * @throws IllegalArgumentException
		 *             unless both {@code 0 <= p < n} and {@code 0 <= q < n}
		 */
		boolean connected(int p, int q) {
			return find(p) == find(q);
		}

		/**
		 * Merges the component containing site {@code p} with the the component
		 * containing site {@code q}.
		 *
		 * @param p
		 *            the integer representing one site
		 * @param q
		 *            the integer representing the other site
		 * @throws IllegalArgumentException
		 *             unless both {@code 0 <= p < n} and {@code 0 <= q < n}
		 */
		void union(int p, int q) {
			int rootP = find(p);
			int rootQ = find(q);
			if (rootP == rootQ)
				return;
			id[rootP] = rootQ;
			count--;
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
	 * Deprecated code for documentation and validation
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
	
//	/**
//	 * Returns a string representation of the automaton, which describes its states
//	 * and transitions.
//	 */
//	@Override
//	public String toString() {
//		Queue<T> worklist = new LinkedList<>();
//		List<T> seen = new ArrayList<>();
//		worklist.add(ini);
//		seen.add(ini);
//		int numTransitions = 0;
//		List<String> acceptingTransitionsList = new ArrayList<>();
//		StringBuilder transitionsStr = new StringBuilder();
//		while (!worklist.isEmpty()) {
//			T s = worklist.remove();
//			if (s.isAccepting()) {
//				acceptingTransitionsList.add("s" + seen.indexOf(s));
//			}
//
//			// add successors not checked yet
//			for (Map.Entry<T, BDD> transition : s.getSucc().entrySet()) {
//				if (!seen.contains(transition.getKey())) {
//					worklist.add(transition.getKey());
//					seen.add(transition.getKey());
//				}
//
//				transitionsStr.append("s" + seen.indexOf(s) + " -> s" + seen.indexOf(transition.getKey()) + " : "
//						+ transition.getValue().toString() + System.lineSeparator());
//				++numTransitions;
//			}
//		}
//
//		StringBuilder prefix = new StringBuilder();
//		prefix.append("Number of states: " + seen.size() + System.lineSeparator());
//		prefix.append("Number of accepting states: " + acceptingTransitionsList.size() + System.lineSeparator());
//		prefix.append("Accepting states:" + System.lineSeparator() + acceptingTransitionsList + System.lineSeparator());
//		prefix.append("Number of transitions: " + numTransitions + System.lineSeparator());
//		prefix.append("Transitions description:" + System.lineSeparator());
//
//		return prefix.append(transitionsStr).toString();
//	}

}
