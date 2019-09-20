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
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import net.sf.javabdd.BDD;

import tau.smlab.syntech.sfa.PowerSetIterator.PowerSetIteratorType;

/**
 * 
 * A class that represents a symbolic finite automaton (SFA). Note that this model does not allow epsilon transitions.
 * 
 * @author Maxim Finkel
 * @author Or Pistiner
 * @author Gal Amram
 * 
 */
public class SimpleSFA extends BaseSFA<SimpleSFAState> {

	protected SimpleSFA() {
		super();
	}
	
	protected SimpleSFA(SFAState ini) {
		super(ini);
	}
	
	protected SimpleSFA(PowerSetIteratorType powerSetIteratorType) {
		super(powerSetIteratorType);
	}
	
	protected SimpleSFA(SFAState ini, PowerSetIteratorType powerSetIteratorType) {
		super(ini, powerSetIteratorType);
	}
	
	@Override
	protected Class<SimpleSFAState> getStatesType() {
		return SimpleSFAState.class;
	}
	
	@Override
	public SimpleSFA copy() {
		SimpleSFA copy = new SimpleSFA(this.ini.cloneWithoutSucc());
		Map<SimpleSFAState, SimpleSFAState> copyStateMappingMap = new HashMap<>();
		copyStateMappingMap.put(this.ini, copy.ini);

		Queue<SimpleSFAState> worklist = new LinkedList<>();
		worklist.add(this.ini);
		while (!worklist.isEmpty()) {
			SimpleSFAState s = worklist.remove();

			// add successors not checked yet
			for (Map.Entry<SimpleSFAState, BDD> transition : s.getSucc().entrySet()) {
				if (!copyStateMappingMap.containsKey(transition.getKey())) {
					worklist.add(transition.getKey());
					copyStateMappingMap.put(transition.getKey(), transition.getKey().cloneWithoutSucc());
				}

				copyStateMappingMap.get(s).addTrans(transition.getValue().id(),
						copyStateMappingMap.get(transition.getKey()));
			}
		}
		return copy;
	}
		
	@Override
	protected SimpleSFA getNewSFAInstance() {
		return new SimpleSFA();
	}

	@Override
	protected SimpleSFA getNewSFAInstance(SimpleSFAState ini) {
		return new SimpleSFA(ini);
	}

	@Override
	public SimpleSFAState getNewState(boolean isAccepting) {
		return new SimpleSFAState(isAccepting);
	}
	
	/**
	 * Returns a string representation of the automaton, which describes its states
	 * and transitions.
	 */
	@Override
	public String toString() {
		Queue<SimpleSFAState> worklist = new LinkedList<>();
		List<SimpleSFAState> seen = new ArrayList<>();
		worklist.add(ini);
		seen.add(ini);
		int numTransitions = 0;
		List<String> acceptingTransitionsList = new ArrayList<>();
		StringBuilder transitionsStr = new StringBuilder();
		while (!worklist.isEmpty()) {
			SimpleSFAState s = worklist.remove();
			if (s.isAccepting()) {
				acceptingTransitionsList.add("s" + seen.indexOf(s));
			}

			// add successors not checked yet
			for (Map.Entry<SimpleSFAState, BDD> transition : s.getSucc().entrySet()) {
				if (!seen.contains(transition.getKey())) {
					worklist.add(transition.getKey());
					seen.add(transition.getKey());
				}

				transitionsStr.append("s" + seen.indexOf(s) + " -> s" + seen.indexOf(transition.getKey()) + " : "
						+ transition.getValue().toString() + System.lineSeparator());
				++numTransitions;
			}
		}

		StringBuilder prefix = new StringBuilder();
		prefix.append("Number of states: " + seen.size() + System.lineSeparator());
		prefix.append("Number of accepting states: " + acceptingTransitionsList.size() + System.lineSeparator());
		prefix.append("Accepting states:" + System.lineSeparator() + acceptingTransitionsList + System.lineSeparator());
		prefix.append("Number of transitions: " + numTransitions + System.lineSeparator());
		prefix.append("Transitions description:" + System.lineSeparator());

		return prefix.append(transitionsStr).toString();
	}
	
//	/**
//	 * Returns the initial state of this automaton.
//	 * 
//	 * @return
//	 */
//	public SimpleSFAState getIni() {
//		return ini;
//	}
//
//	/**
//	 * Sets the initial state of this automaton.
//	 */
//	public void setIni(SFAState ini) {
//		this.ini = ini;
//	}
	
//	/**
//	 * Returns the type of the iterator over subsets of transitions used by {@link #determinize()} method.
//	 * 
//	 * @return
//	 */
//	public PowerSetIteratorType getPowerSetIteratorType() {
//		return powerSetIteratorType;
//	}
//	
//	/**
//	 * Sets the type of the iterator over subsets of transitions used by {@link #determinize()}  method.
//	 * 
//	 * @return
//	 */
//	public void setPowerSetIteratorType(PowerSetIteratorType powerSetIteratorType) {
//		this.powerSetIteratorType = powerSetIteratorType;
//	}

//	/**
//	 * Returns the set of final (accepting) states of this automaton.
//	 * 
//	 * @return
//	 */
//	public Set<SFAState> finalStates() {
//		Queue<SFAState> worklist = new LinkedList<>();
//		Set<SFAState> seen = new LinkedHashSet<>();
//		Set<SFAState> finalStates = new LinkedHashSet<>(); 
//		worklist.add(ini);
//		seen.add(ini);
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//			if(s.isAccepting()) {
//				finalStates.add(s);
//			}
//			// add successors which have not been checked yet
//			for (SFAState succ : s.getSuccessors()) {
//				if (!seen.contains(succ)) {
//					worklist.add(succ);
//					seen.add(succ);
//				}
//			}
//		}
//		return finalStates;
//	}

//	/**
//	 * Frees the BDDs (the transitions' guards) of this automaton.
//	 * 
//	 * @return
//	 */
//	public void free() {
//		Queue<SFAState> worklist = new LinkedList<>();
//		Set<SFAState> seen = new LinkedHashSet<>();
//
//		worklist.add(ini);
//		seen.add(ini);
//
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//
//			// free the guards of all outgoing transitions 
//			for(BDD guard : s.getSucc().values()) {
//				if(!guard.isFree()) {
//					guard.free();
//				}
//			}
//
//			// add successors which have not been searched
//			for (SFAState succ : s.getSuccessors()) {
//				if (!seen.contains(succ)) {
//					worklist.add(succ);
//					seen.add(succ);
//				}
//			}
//		}
//	}

//	/**
//	 * Checks whether automaton is deterministic.
//	 * 
//	 * @return true iff this automaton is deterministic.
//	 */
//	public boolean isDeterministic() {
//		Queue<SFAState> worklist = new LinkedList<>();
//		Set<SFAState> seen = new LinkedHashSet<>();
//		worklist.add(ini);
//		seen.add(ini);
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//			// check determinism of outgoing transitions
//			if (hasOverlap(s.getSucc().values())) {
//				return false;
//			}
//			// add successors not checked yet
//			for (SFAState succ : s.getSuccessors()) {
//				if (!seen.contains(succ)) {
//					worklist.add(succ);
//					seen.add(succ);
//				}
//			}
//		}
//		return true;
//	}




//	@Override
//	public SimpleSFA determinize() {
//		
//		//create a fresh copy of this automaton and remove all the dead states from the fresh copy
//		SimpleSFA copySfa = this.copy();
//		copySfa.removeDeadStates();
//		
//		//check if the removal of dead states has resulted in a deterministic automaton
//		if (copySfa.isDeterministic()) {
//			return copySfa; //we have a deterministic automaton so we are done
//		}
//		//we have that copySfa is non-deterministic so we need to determinize it
//		return new SimpleSFA(super.transformIniToDeterministic(copySfa));
//	}
	

	
//	/**
//	 * Returns a deterministic SFA (DSFA) that is equivalent to this SFA.
//	 * 
//	 * The algorithm follows the work by Veanes et al. (2010).
//	 * 
//	 * The algorithm uses an iterator over subsets of outgoing transitions whose type can be
//	 * checked via {@link #getPowerSetIteratorType} and can be set via {@link #setPowerSetIteratorType}.
//	 *
//	 * @return
//	 */
	/*
	public SFA determinize() {
		
		//create a fresh copy of this automaton and remove all the dead states from the fresh copy
		SFA copySfa = this.copy();
		copySfa.removeDeadStates();
		
		//check if the removal of dead states has resulted in a deterministic automaton
		if (copySfa.isDeterministic()) {
			return copySfa; //we have a deterministic automaton so we are done
		}
		
		
		//we have that copySfa is non-deterministic so we need to determinize it
		
		SFA deterministicAutomaton = new SFA();

		
		 * To distinguish between different states in the DSFA, we need to remember a
		 * mapping between sets of states in this SFA to the new states in the DSFA that
		 * we create.
		 
		Map<Set<SFAState>, SFAState> stateSubsetsToDeterministicStatesMapping = new HashMap<>();
		Set<SFAState> iniSingletonSet = new HashSet<>(Arrays.asList(copySfa.ini));

		
		 * The initial state in the DSFA is a singleton containing the SFA's initial
		 * state.
		 
		SFAState determinsticAutomatonInitialState = copySfa.ini.copyWithoutSucc();
		stateSubsetsToDeterministicStatesMapping.put(iniSingletonSet, determinsticAutomatonInitialState);
		deterministicAutomaton.setIni(determinsticAutomatonInitialState);

		
		 * We use a stack to do a DFS search on (the copy without dead states of) this SFA. It will contain the state
		 * sets of the DSFA that we create.
		 
		List<Set<SFAState>> workStack = new Stack<>();
		workStack.add(0, iniSingletonSet);
		while (!workStack.isEmpty()) {
			Map<SFAState, BDD> allTransitionsMap = new HashMap<>();
			Set<Map.Entry<SFAState, BDD>> tautologyTransitions = new HashSet<>();
			Set<SFAState> currentStateSet = workStack.remove(0), tautologySuccessors = new HashSet<>();
			for (SFAState state : currentStateSet) {
				for (Map.Entry<SFAState, BDD> transition : state.getSucc().entrySet()) {
					
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
					 * Optimization 3: If there are multiple transitions to the SAME target state, then we can transform
					 * them into a single transition by taking their disjunction. This may save (exponentially many but redundant) iterations over
					 * subsets of delta_A(q) that contain different combinations of transitions to the SAME successor state. In case the disjunction
					 * evaluates to a tautology (i.e., it is trivially true), then we treat the "new" transition as in Optimization 2.
					 * 
					 
					if (!transition.getValue().isZero()) {
						if(allTransitionsMap.containsKey(transition.getKey())) {
							allTransitionsMap.get(transition.getKey()).orWith(transition.getValue().id());
							if(allTransitionsMap.get(transition.getKey()).isOne()) {
								//the guard of the transition to the successor state transition.getKey() is TRUE
								allTransitionsMap.get(transition.getKey()).free();
								allTransitionsMap.remove(transition.getKey());
								tautologyTransitions.add(transition);
								tautologySuccessors.add(transition.getKey());
							}
							
						}
						else if(!tautologySuccessors.contains(transition.getKey())) {
							//this is the first time a transition to this target/successor state is seen
							if (transition.getValue().isOne()) {
								//tautology transition
								tautologyTransitions.add(transition);
								tautologySuccessors.add(transition.getKey());
							}
							else {
								allTransitionsMap.put(transition.getKey(), transition.getValue().id());
							}
						}
					}
				}
			}

			Set<Map.Entry<SFAState, BDD>> allTransitionsSet = allTransitionsMap.entrySet();
			Iterator<Pair<Set<Map.Entry<SFAState, BDD>>, BDD>> psIter = PowerSetIteratorFactory.getPowerSetIterator(this.powerSetIteratorType, allTransitionsSet);
			Pair<Set<Map.Entry<SFAState, BDD>>, BDD> nextPair;

			Set<Map.Entry<SFAState, BDD>> transitionsSubset, transitionsSubsetComplement;
			boolean newDeterministicStateIsAccepting;
			Set<SFAState> successorStatesSet;
			BDD transitionsGuardsConjunction;
			SFAState detCurrentState = stateSubsetsToDeterministicStatesMapping.get(currentStateSet), detSuccessorState;

			while (psIter.hasNext()) {
				nextPair = psIter.next();
				transitionsSubset = nextPair.getLeft();
				transitionsSubset.addAll(tautologyTransitions);
				
				 * An empty transitions set is not interesting since its successor states set is
				 * empty (this corresponds to taking no transitions at all).
				 
				if (!transitionsSubset.isEmpty()) {
					transitionsGuardsConjunction = nextPair.getRight();
					transitionsSubsetComplement = SFAUtil.getComplementOfTransSet(allTransitionsSet, transitionsSubset);
					for (Map.Entry<SFAState, BDD> t : transitionsSubsetComplement) {
						if (transitionsGuardsConjunction.isZero()) {
							break;
						}
						transitionsGuardsConjunction.andWith(t.getValue().not());
					}				
					if (!transitionsGuardsConjunction.isZero()) {
						successorStatesSet = SFAUtil.getTransitionsTargetStates(transitionsSubset);
						
						 * If some successor state is an accepting SFA state, then the corresponding new
						 * DFSA state should be an accepting state (as indicated by 'newDeterministicStateIsAccepting').
						 
						newDeterministicStateIsAccepting = SFAUtil.containsAcceptingState(successorStatesSet);
						
						if (!stateSubsetsToDeterministicStatesMapping.containsKey(successorStatesSet)) {
							stateSubsetsToDeterministicStatesMapping.put(successorStatesSet,
									new SFAState(newDeterministicStateIsAccepting));
							workStack.add(0, successorStatesSet);
						}
						detSuccessorState = stateSubsetsToDeterministicStatesMapping.get(successorStatesSet);
						detCurrentState.addTrans(transitionsGuardsConjunction, detSuccessorState);
					}
					else {	
						transitionsGuardsConjunction.free();
					}
				}
			}
			SFAUtil.freeTransitionsSet(allTransitionsSet);
		}
		copySfa.free();
		return deterministicAutomaton;
	}*/
	
	

//	/**
//	 * Completes this' transition function by adding transitions to a "black hole"
//	 * state from every state that has an undefined transition for some assignment
//	 * to the BDDs.
//	 */
//	public void completeTransitionFunction() {
//		// create a black hole state, from where we are stuck forever.
//		SFAState blackHole = new SFAState(false);
//		blackHole.addTrans(Env.TRUE(), blackHole);
//
//		Queue<SFAState> worklist = new LinkedList<>();
//		List<SFAState> seen = new ArrayList<>();
//		worklist.add(ini);
//		seen.add(ini);
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//
//			// add successors not checked yet and complete the transition function
//			BDD blackHoleTransition = Env.TRUE();
//			for (Map.Entry<SFAState, BDD> transition : s.getSucc().entrySet()) {
//				if (!seen.contains(transition.getKey())) {
//					worklist.add(transition.getKey());
//					seen.add(transition.getKey());
//				}
//
//				blackHoleTransition.andWith(transition.getValue().not());
//			}
//
//			// if there is an undefined transition for some assignment
//			if (!blackHoleTransition.isZero()) {
//				s.addTrans(blackHoleTransition, blackHole);
//			}
//			else {
//
//				blackHoleTransition.free();
//			}
//		}
//	}

//	/**
//	 * CODE COPIED FROM:
//	 * https://algs4.cs.princeton.edu/15uf/QuickUnionPathCompressionUF.java.html
//	 * 
//	 * The {@code QuickUnionPathCompressionUF} class represents a union find data
//	 * structure. It supports the <em>union</em> and <em>find</em> operations, along
//	 * with methods for determining whether two sites are in the same component and
//	 * the total number of components.
//	 * <p>
//	 * This implementation uses quick union (no weighting) with full path
//	 * compression. Initializing a data structure with <em>n</em> sites takes linear
//	 * time. Afterwards, <em>union</em>, <em>find</em>, and <em>connected</em> take
//	 * logarithmic amortized time <em>count</em> takes constant time.
//	 * <p>
//	 * For additional documentation, see
//	 * <a href="https://algs4.cs.princeton.edu/15uf">Section 1.5</a> of
//	 * <i>Algorithms, 4th Edition</i> by Robert Sedgewick and Kevin Wayne.
//	 * 
//	 * @author Robert Sedgewick
//	 * @author Kevin Wayne
//	 */
//	static class QuickUnionPathCompressionUF {
//		private int[] id; // id[i] = parent of i
//		private int count; // number of components
//
//		/**
//		 * Initializes an empty union�find data structure with n isolated components 0
//		 * through n-1.
//		 * 
//		 * @param n
//		 *            the number of sites
//		 * @throws java.lang.IllegalArgumentException
//		 *             if n < 0
//		 */
//		QuickUnionPathCompressionUF(int n) {
//			count = n;
//			id = new int[n];
//			for (int i = 0; i < n; i++) {
//				id[i] = i;
//			}
//		}
//
//		/**
//		 * Returns the number of components.
//		 *
//		 * @return the number of components (between {@code 1} and {@code n})
//		 */
//		int count() {
//			return count;
//		}
//
//		/**
//		 * Returns the component identifier for the component containing site {@code p}.
//		 *
//		 * @param p
//		 *            the integer representing one object
//		 * @return the component identifier for the component containing site {@code p}
//		 * @throws IllegalArgumentException
//		 *             unless {@code 0 <= p < n}
//		 */
//		int find(int p) {
//			int root = p;
//			while (root != id[root])
//				root = id[root];
//			while (p != root) {
//				int newp = id[p];
//				id[p] = root;
//				p = newp;
//			}
//			return root;
//		}
//
//		/**
//		 * Returns true if the the two sites are in the same component.
//		 *
//		 * @param p
//		 *            the integer representing one site
//		 * @param q
//		 *            the integer representing the other site
//		 * @return {@code true} if the two sites {@code p} and {@code q} are in the same
//		 *         component; {@code false} otherwise
//		 * @throws IllegalArgumentException
//		 *             unless both {@code 0 <= p < n} and {@code 0 <= q < n}
//		 */
//		boolean connected(int p, int q) {
//			return find(p) == find(q);
//		}
//
//		/**
//		 * Merges the component containing site {@code p} with the the component
//		 * containing site {@code q}.
//		 *
//		 * @param p
//		 *            the integer representing one site
//		 * @param q
//		 *            the integer representing the other site
//		 * @throws IllegalArgumentException
//		 *             unless both {@code 0 <= p < n} and {@code 0 <= q < n}
//		 */
//		void union(int p, int q) {
//			int rootP = find(p);
//			int rootQ = find(q);
//			if (rootP == rootQ)
//				return;
//			id[rootP] = rootQ;
//			count--;
//		}
//	}

//	/**
//	 * Returns a MINIMAL deterministic SFA (DSFA) that is equivalent to this SFA.
//	 * This implementation follows the work by Veanes et al. (2010).
//	 * 
//	 */
//	public SFA minimize() {
//		if (this.isEmptyLanguage()) {
//			return SFAs.emptyLanguageSfa();
//		}
//
//		SFA deterministicAutomaton = this.determinize();
//		deterministicAutomaton.completeTransitionFunction();
//
//		// find all reachable states
//		List<SFAState> reachableStates = deterministicAutomaton.reachableStates();
//
//		// remember each seen state's index in the seen list
//		Map<SFAState, Integer> seenStatesIndices = new HashMap<>();
//		for (int i = 0; i < reachableStates.size(); ++i) {
//			seenStatesIndices.put(reachableStates.get(i), i);
//		}
//
//		// initialize equivalence class E
//		Set<Pair<SFAState, SFAState>> E = new HashSet<>();
//		for (int i = 0; i < reachableStates.size(); ++i) {
//			for (int j = 0; j < i; ++j) {
//				SFAState p = reachableStates.get(i);
//				SFAState q = reachableStates.get(j);
//				if ((p.isAccepting() && q.isAccepting()) || (!p.isAccepting() && !q.isAccepting())) {
//					E.add(new Pair<SFAState, SFAState>(p, q));
//				}
//			}
//		}
//
//		// refine E
//		Set<Pair<SFAState, SFAState>> pairsToDelete = new HashSet<>();
//		while (true) {
//			for (Pair<SFAState, SFAState> pair : E) {
//				SFAState p = pair.getLeft();
//				SFAState q = pair.getRight();
//				for (Map.Entry<SFAState, BDD> pTransition : p.getSucc().entrySet()) {
//					for (Map.Entry<SFAState, BDD> qTransition : q.getSucc().entrySet()) {
//						SFAState p1 = pTransition.getKey();
//						SFAState q1 = qTransition.getKey();
//						BDD pTransitionBDD = pTransition.getValue();
//						BDD qTransitionBDD = qTransition.getValue();
//						BDD conjunctionBDD = pTransitionBDD.and(qTransitionBDD);
//						if (!p1.equals(q1) && !E.contains(new Pair<SFAState, SFAState>(p1, q1))
//								&& !E.contains(new Pair<SFAState, SFAState>(q1, p1))
//								&& !((conjunctionBDD).isZero())) {
//							pairsToDelete.add(pair);
//						}
//
//						conjunctionBDD.free();
//					}
//				}
//			}
//
//			if (pairsToDelete.isEmpty()) {
//				// reached a fixed point!
//				break;
//			}
//
//			E.removeAll(pairsToDelete);
//			pairsToDelete.clear();
//		}
//
//		// compute E-equivalence classes using a union find data structure
//		QuickUnionPathCompressionUF unionFindDataStructure = new QuickUnionPathCompressionUF(reachableStates.size());
//		for (Pair<SFAState, SFAState> pair : E) {
//			unionFindDataStructure.union(seenStatesIndices.get(pair.getLeft()), seenStatesIndices.get(pair.getRight()));
//		}
//
//		// build the minimal automaton
//		Map<SFAState, SFAState> EClassIdStateToMinimalAutomatonStateMap = new HashMap<>();
//		SFAState initialStateRepresentative = reachableStates
//				.get(unionFindDataStructure.find(seenStatesIndices.get(deterministicAutomaton.ini)));
//		EClassIdStateToMinimalAutomatonStateMap.put(initialStateRepresentative,
//				initialStateRepresentative.cloneWithoutSucc());
//
//		Queue<SFAState> worklist = new LinkedList<>();
//		List<SFAState> seen = new ArrayList<>();
//		worklist.add(deterministicAutomaton.ini);
//		seen.add(deterministicAutomaton.ini);
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//
//			// add successors not checked yet
//			for (Map.Entry<SFAState, BDD> transition : s.getSucc().entrySet()) {
//				if (!seen.contains(transition.getKey())) {
//					worklist.add(transition.getKey());
//					seen.add(transition.getKey());
//				}
//
//				SFAState nextStateRepresentative = reachableStates
//						.get(unionFindDataStructure.find(seenStatesIndices.get(transition.getKey())));
//				if (!EClassIdStateToMinimalAutomatonStateMap.containsKey(nextStateRepresentative)) {
//					EClassIdStateToMinimalAutomatonStateMap.put(nextStateRepresentative,
//							nextStateRepresentative.cloneWithoutSucc());
//				}
//
//				SFAState currentStateRepresentative = reachableStates
//						.get(unionFindDataStructure.find(seenStatesIndices.get(s)));
//				EClassIdStateToMinimalAutomatonStateMap.get(currentStateRepresentative).addTrans(transition.getValue(),
//						EClassIdStateToMinimalAutomatonStateMap.get(nextStateRepresentative));
//			}
//		}
//
//		SFA minimalAutomaton = new SFA();
//		minimalAutomaton.setIni(EClassIdStateToMinimalAutomatonStateMap.get(initialStateRepresentative));
//		minimalAutomaton.removeDeadStates();
//		return minimalAutomaton;
//	}

//	/**
//	 * Returns the list of this' reachable states.
//	 */
//	public List<SFAState> reachableStates() {
//		Queue<SFAState> worklist = new LinkedList<>();
//		List<SFAState> seen = new ArrayList<>();
//		worklist.add(ini);
//		seen.add(ini);
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//
//			// add successors not checked yet
//			for (SFAState nextState : s.getSuccessors()) {
//				if (!seen.contains(nextState)) {
//					worklist.add(nextState);
//					seen.add(nextState);
//				}
//			}
//		}
//
//		return seen;
//	}

//	/**
//	 * Returns the number of this' reachable states.
//	 */
//	public int numStates() {
//		return reachableStates().size();
//	}

//	/**
//	 * Computes this Automaton's complement, by determinizing it and flipping its
//	 * states' acceptance.
//	 * 
//	 * NOTE: IN ORDER FOR THIS ALGORITHM TO WORK, WE MUST MAKE SURE THAT THE
//	 * TRANSITION FUNCTION IS COMPLETE, AND THAT WE ACHIVE WITH THE
//	 * completeTransitionFunction() METHOD!
//	 */
//	public SFA complement() {
//		SFA deterministicAutomaton = this.determinize();
//		deterministicAutomaton.completeTransitionFunction();
//
//		Queue<SFAState> worklist = new LinkedList<>();
//		List<SFAState> seen = new ArrayList<>();
//		worklist.add(deterministicAutomaton.ini);
//		seen.add(deterministicAutomaton.ini);
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//			// flip each accepting state to a non-accepting one, and vice-versa
//			s.flipAcceptance();
//
//			// add successors not checked yet
//			for (SFAState nextState : s.getSuccessors()) {
//				if (!seen.contains(nextState)) {
//					worklist.add(nextState);
//					seen.add(nextState);
//				}
//			}
//		}
//
//		return deterministicAutomaton;
//	}

//	/**
//	 * Removes (in place) all this' dead states, i.e. states that do not have any
//	 * reachable final (accepting) states.
//	 */
//	public void removeDeadStates() {
//		// search for all final states, while building the reversed automaton
//		Set<SFAState> finalStates = new HashSet<>();
//		Map<SFAState, SFAState> originalToReversedStatesMapping = new HashMap<>();
//		
//		Queue<SFAState> worklist = new LinkedList<>();
//		worklist.add(ini);
//		originalToReversedStatesMapping.put(ini, ini.cloneWithoutSucc());
//		BDD dummyGuard = Env.TRUE();
//		
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//			if (s.isAccepting()) {
//				finalStates.add(s);
//			}
//
//			// add successors not checked yet
//			for (SFAState nextState : s.getSuccessors()) {
//				if (!originalToReversedStatesMapping.containsKey(nextState)) {
//					worklist.add(nextState);
//					originalToReversedStatesMapping.put(nextState, nextState.cloneWithoutSucc());
//				}
//
//				// build reversed edges in the reversed automaton
//				// the transitions' guards don't really matter so we add the SAME dummy TRUE guard for all transitions (to save memory and avoid memory leaks in the BDD engine)
//				
//				//originalToReversedStatesMapping.get(nextState).addTrans(Env.TRUE(),
//				//		originalToReversedStatesMapping.get(s));
//				
//				originalToReversedStatesMapping.get(nextState).getSucc().put(originalToReversedStatesMapping.get(s), dummyGuard);
//			}
//		}
//		
//		// search for reachable states from the final states in the reversed automaton
//		List<SFAState> seen = new ArrayList<>();
//		Set<SFAState> reachableFromFinalStatesInReversed = new HashSet<>();
//		for (SFAState finalState : finalStates) {
//			worklist.clear();
//			worklist.add(originalToReversedStatesMapping.get(finalState));
//			
//			seen.clear();
//			seen.add(originalToReversedStatesMapping.get(finalState));
//			
//			while (!worklist.isEmpty()) {
//				SFAState sRev = worklist.remove();
//
//				// add successors not checked yet
//				for (SFAState nextStateRev : sRev.getSuccessors()) {
//					if (!seen.contains(nextStateRev)) {
//						worklist.add(nextStateRev);
//						seen.add(nextStateRev);
//					}
//				}
//			}
//
//			reachableFromFinalStatesInReversed.addAll(seen);
//		}
//		
//		//free the dummy guard
//		dummyGuard.free();
//		
//		// delete all states in the original automaton which are reachable from the initial state but from which no accepting state can be reached
//		worklist.clear();
//		seen.clear();
//		worklist.add(ini);
//		seen.add(ini);
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//
//			// mark all transitions to dead states
//			Set<SFAState> nextSatesToRemove = new HashSet<>();
//			for (SFAState nextState : s.getSuccessors()) {
//				if (!reachableFromFinalStatesInReversed.contains(originalToReversedStatesMapping.get(nextState))) {
//					nextSatesToRemove.add(nextState);
//				}
//			}
//
//			// delete all dead transitions
//			for (SFAState nextState : nextSatesToRemove) {
//				s.removeTrans(nextState);
//			}
//
//			// add successors not checked yet
//			for (SFAState nextState : s.getSuccessors()) {
//				if (!seen.contains(nextState)) {
//					worklist.add(nextState);
//					seen.add(nextState);
//				}
//			}
//		}
//	}
	
//	/**
//	 * Returns a new Automaton whose language is the intersection between this and
//	 * other's language.
//	 * 
//	 * This algorithm is inspired by Veanes et al. (2010). 
//	 */
//	public SFA intersectWith(SFA other) {
//		SFA intersectionAutomaton = new SFA();
//
//		/*
//		 * To distinguish between different states in the DSFA, we need to remember a
//		 * mapping between pairs of states in this SFA to the new states in the
//		 * intersection SFA that we create.
//		 */
//		Map<Pair<SFAState, SFAState>, SFAState> statePairsToIntersectionStatesMapping = new HashMap<>();
//		Pair<SFAState, SFAState> intersectionInitialState = new Pair<>(this.ini, other.ini);
//
//		/*
//		 * The initial state in the intersection SFA is a pair containing this and
//		 * other's initial states.
//		 */
//		SFAState intersectionAutomatonInitialState = new SFAState(this.ini.isAccepting() && other.ini.isAccepting());
//		statePairsToIntersectionStatesMapping.put(intersectionInitialState, intersectionAutomatonInitialState);
//		intersectionAutomaton.setIni(intersectionAutomatonInitialState);
//
//		/*
//		 * We use a stack to do a DFS search on the given SFA. It will contain the state
//		 * pairs of the intersection SFA that we create.
//		 */
//		List<Pair<SFAState, SFAState>> workStack = new Stack<>();
//		workStack.add(0, intersectionInitialState);
//		while (!workStack.isEmpty()) {
//			Pair<SFAState, SFAState> currentPair = workStack.remove(0);
//
//			Set<Map.Entry<SFAState, BDD>> thistransitionsSet = currentPair.left.getSucc().entrySet();
//			Set<Map.Entry<SFAState, BDD>> othertransitionsSet = currentPair.right.getSucc().entrySet();
//			for (Map.Entry<SFAState, BDD> leftTransition : thistransitionsSet) {
//				for (Map.Entry<SFAState, BDD> rightTransition : othertransitionsSet) {
//
//					BDD intersectionTransitionBDD = leftTransition.getValue().and(rightTransition.getValue());
//					// if the intersection transition is satisfiable
//					if (!intersectionTransitionBDD.isZero()) {
//						Pair<SFAState, SFAState> targetStatePair = new Pair<>(leftTransition.getKey(),
//								rightTransition.getKey());
//
//						if (!statePairsToIntersectionStatesMapping.containsKey(targetStatePair)) {
//							statePairsToIntersectionStatesMapping.put(targetStatePair, new SFAState(
//									leftTransition.getKey().isAccepting() && rightTransition.getKey().isAccepting()));
//							workStack.add(0, targetStatePair);
//						}
//
//						SFAState currentState = statePairsToIntersectionStatesMapping.get(currentPair);
//						SFAState targetState = statePairsToIntersectionStatesMapping.get(targetStatePair);
//						currentState.addTrans(intersectionTransitionBDD, targetState);
//					}
//					else {
//
//						intersectionTransitionBDD.free();
//					}
//				}
//			}
//		}
//
//		// remove states that can't reach final states
//		intersectionAutomaton.removeDeadStates();
//		return intersectionAutomaton;
//	}

//	/**
//	 * Checks whether this automaton's language is empty, i.e., whether this automaton doesn't accept any
//	 * word.
//	 */
//	public boolean isEmptyLanguage() {
//		if (ini.isAccepting()) {
//			return false;
//		}
//
//		Queue<SFAState> worklist = new LinkedList<>();
//		List<SFAState> seen = new ArrayList<>();
//		worklist.add(ini);
//		seen.add(ini);
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//
//			for(SFAState succ : s.getSuccessors()) {
//				if (succ.isAccepting()) {
//					return false;
//				}
//
//				// add the current successor if it has not been searched yet
//				if (!seen.contains(succ)) {
//					worklist.add(succ);
//					seen.add(succ);
//				}
//			}
//		}
//		return true;
//	}

//	/**
//	 * Checks whether this automaton's language is a subset of the other automaton's language.
//	 */
//	public boolean isSubsetOf(SFA other) {
//		return other.complement().intersectWith(this).isEmptyLanguage();
//	}

//	/**
//	 * Checks whether this and other automatons are equivalent, i.e. accept the same
//	 * language.
//	 */
//	public boolean isEquivalent(SFA other) {
//		return this.isSubsetOf(other) && other.isSubsetOf(this);
//	}



/*
 * 
 * Legacy code for documentation and validation
 * 
 * 
 */
	
//	/**
//	 * Returns a MINIMAL deterministic SFA (DSFA) that is equivalent to this SFA.
//	 * 
//	 * This method is more efficient than the minimize() method since it uses the
//	 * EfficientPowerSetIterator class in the determinization process!
//	 * 
//	 * This algorithm is inspired by Veanes et al. (2010).
//	 */
//	public SFA efficientMinimize() {
//		if (this.isEmptyLanguage()) {
//			SFAState initialState = new SFAState(false);
//			initialState.addTrans(Env.TRUE(), initialState);
//			SFA minimalAutomaton = new SFA();
//			minimalAutomaton.setIni(initialState);
//			return minimalAutomaton;
//		}
//
//		SFA deterministicAutomaton = this.determinize();
//		deterministicAutomaton.completeTransitionFunction();
//
//		// find all reachable states
//		List<SFAState> reachableStates = deterministicAutomaton.reachableStates();
//
//		// remember each seen state's index in the seen list
//		Map<SFAState, Integer> seenStatesIndices = new HashMap<>();
//		for (int i = 0; i < reachableStates.size(); ++i) {
//			seenStatesIndices.put(reachableStates.get(i), i);
//		}
//
//		// initialize equivalence class E
//		Set<Pair<SFAState, SFAState>> E = new HashSet<>();
//		for (int i = 0; i < reachableStates.size(); ++i) {
//			for (int j = 0; j < i; ++j) {
//				SFAState p = reachableStates.get(i);
//				SFAState q = reachableStates.get(j);
//				if ((p.isAccepting() && q.isAccepting()) || (!p.isAccepting() && !q.isAccepting())) {
//					E.add(new Pair<SFAState, SFAState>(p, q));
//				}
//			}
//		}
//
//		// refine E
//		Set<Pair<SFAState, SFAState>> pairsToDelete = new HashSet<>();
//		while (true) {
//			for (Pair<SFAState, SFAState> pair : E) {
//				SFAState p = pair.getLeft();
//				SFAState q = pair.getRight();
//				for (Map.Entry<SFAState, BDD> pTransition : p.getSucc().entrySet()) {
//					for (Map.Entry<SFAState, BDD> qTransition : q.getSucc().entrySet()) {
//						SFAState p1 = pTransition.getKey();
//						SFAState q1 = qTransition.getKey();
//						BDD pTransitionBDD = pTransition.getValue();
//						BDD qTransitionBDD = qTransition.getValue();
//						BDD conjunctionBDD = pTransitionBDD.and(qTransitionBDD);
//
//						if (!p1.equals(q1) && !E.contains(new Pair<SFAState, SFAState>(p1, q1))
//								&& !E.contains(new Pair<SFAState, SFAState>(q1, p1))
//								&& !((conjunctionBDD).isZero())) {
//							pairsToDelete.add(pair);
//						}
//
//						conjunctionBDD.free();
//					}
//				}
//			}
//
//			if (pairsToDelete.isEmpty()) {
//				// reached a fixpoint!
//				break;
//			}
//
//			E.removeAll(pairsToDelete);
//			pairsToDelete.clear();
//		}
//
//		// compute E-equivalence classes using a union find data structure
//		QuickUnionPathCompressionUF unionFindDataStructure = new QuickUnionPathCompressionUF(reachableStates.size());
//		for (Pair<SFAState, SFAState> pair : E) {
//			unionFindDataStructure.union(seenStatesIndices.get(pair.getLeft()), seenStatesIndices.get(pair.getRight()));
//		}
//
//		// build the minimal automaton
//		Map<SFAState, SFAState> EClassIdStateToMinimalAutomatonStateMap = new HashMap<>();
//		SFAState initialStateRepresentative = reachableStates
//				.get(unionFindDataStructure.find(seenStatesIndices.get(deterministicAutomaton.ini)));
//		EClassIdStateToMinimalAutomatonStateMap.put(initialStateRepresentative,
//				new SFAState(initialStateRepresentative.isAccepting()));
//
//		Queue<SFAState> worklist = new LinkedList<>();
//		List<SFAState> seen = new ArrayList<>();
//		worklist.add(deterministicAutomaton.ini);
//		seen.add(deterministicAutomaton.ini);
//		while (!worklist.isEmpty()) {
//			SFAState s = worklist.remove();
//
//			// add successors not checked yet
//			for (Map.Entry<SFAState, BDD> transition : s.getSucc().entrySet()) {
//				if (!seen.contains(transition.getKey())) {
//					worklist.add(transition.getKey());
//					seen.add(transition.getKey());
//				}
//
//				SFAState nextStateRepresentative = reachableStates
//						.get(unionFindDataStructure.find(seenStatesIndices.get(transition.getKey())));
//				if (!EClassIdStateToMinimalAutomatonStateMap.containsKey(nextStateRepresentative)) {
//					EClassIdStateToMinimalAutomatonStateMap.put(nextStateRepresentative,
//							new SFAState(nextStateRepresentative.isAccepting()));
//				}
//
//				SFAState currentStateRepresentative = reachableStates
//						.get(unionFindDataStructure.find(seenStatesIndices.get(s)));
//				EClassIdStateToMinimalAutomatonStateMap.get(currentStateRepresentative).addTrans(transition.getValue(),
//						EClassIdStateToMinimalAutomatonStateMap.get(nextStateRepresentative));
//			}
//		}
//
//		SFA minimalAutomaton = new SFA();
//		minimalAutomaton.setIni(EClassIdStateToMinimalAutomatonStateMap.get(initialStateRepresentative));
//		minimalAutomaton.removeDeadStates();
//		return minimalAutomaton;
//	}
	
//	/**
//	 * Returns a deterministic SFA (DSFA) that is equivalent to this SFA.
//	 * 
//	 * This method is more efficient than the determinize() method since it uses the
//	 * EfficientPowerSetIterator class!
//	 * 
//	 * The algorithm is inspired by Veanes et al. (2010).
//	 */
//	public SFA efficientDeterminize() {
//		this.removeDeadStates();
//
//		if (this.isDeterministic()) {
//			return this.copy();
//		}
//
//		SFA deterministicAutomaton = new SFA();
//
//		/*
//		 * To distinguish between different states in the DSFA, we need to remember a
//		 * mapping between sets of states in this SFA to the new states in the DSFA that
//		 * we create.
//		 */
//		Map<Set<SFAState>, SFAState> stateSubsetsToDetreministicStatesMapping = new HashMap<>();
//		Set<SFAState> iniSingletonSet = new HashSet<>(Arrays.asList(ini));
//
//		/*
//		 * The initial state in the DSFA is a singleton containing the SFA's initial
//		 * state.
//		 */
//		SFAState detreminsticAutomatonInitialState = new SFAState(ini.isAccepting());
//		stateSubsetsToDetreministicStatesMapping.put(iniSingletonSet, detreminsticAutomatonInitialState);
//		deterministicAutomaton.setIni(detreminsticAutomatonInitialState);
//
//		/*
//		 * We use a stack to do a DFS search on the given SFA. It will contain the state
//		 * sets of the DSFA that we create.
//		 */
//		List<Set<SFAState>> workStack = new Stack<>();
//		workStack.add(0, iniSingletonSet);
//		while (!workStack.isEmpty()) {
//			Set<Map.Entry<SFAState, BDD>> transitionsSet = new HashSet<>();
//			Set<Map.Entry<SFAState, BDD>> tautologyTransitions = new HashSet<>();
//			Set<SFAState> currentStateSet = workStack.remove(0);
//			for (SFAState state : currentStateSet) {
//				for (Map.Entry<SFAState, BDD> transition : state.getSucc().entrySet()) {
//					/*
//					 * Optimization 1: We don't need to add to the transitions set (denoted by
//					 * delta_A(q) in the paper) unsatisfiable transitions, since each subset t of
//					 * delta_A(q) that contains that transition will result a unsatisfiable formula
//					 * phi_t, since the left conjunction will not be satisfiable.
//					 */
//					if (!transition.getValue().isZero()) {
//						transitionsSet.add(transition);
//						if (transition.getValue().isOne()) {
//							tautologyTransitions.add(transition);
//						}
//					}
//				}
//			}
//
//			/*
//			 * Optimization 2: If a subset t of delta_A(q) DOESN'T contains a tautology
//			 * transition, then the resulting phi_t will be unsatisfiable, since the right
//			 * conjunction will be unsatisfiable. Thus, it suffice to go over delta_A(q)'s
//			 * subsets that contain all delta_A(q)'s tautology transitions, which is
//			 * equivalent to going over all the subsets of delta_A(q)\{tautologies} and
//			 * adding to each subset {tautologies} (the set of all tautology transitions).
//			 */
//			transitionsSet.removeAll(tautologyTransitions);
//			EfficientPowerSetIterator iterator = new EfficientPowerSetIterator(transitionsSet);
//			//Iterator<Pair<Set<Map.Entry<SFAState, BDD>>, BDD>> iterator = PowerSetIteratorFactory.getPowerSetIterator(this.powerSetIteratorType, transitionsSet);
//			Pair<Set<Map.Entry<SFAState, BDD>>, BDD> nextPair = iterator.next();
//			while (nextPair != null) {
//				Set<Map.Entry<SFAState, BDD>> transitionsSubset = nextPair.getLeft();
//				transitionsSubset.addAll(tautologyTransitions);
//
//				/*
//				 * Empty transitions set is not interesting since its target states set is
//				 * empty.
//				 */
//				if (!transitionsSubset.isEmpty()) {
//					Set<Map.Entry<SFAState, BDD>> transitionsSubsetComplement = new HashSet<>(transitionsSet);
//					transitionsSubsetComplement.removeAll(transitionsSubset);
//
//					Set<SFAState> targetStateSet = new HashSet<>();
//					boolean isAcceptingNewDeterministicState = false;
//					BDD transitionBDD = nextPair.getRight();
//					for (Map.Entry<SFAState, BDD> t : transitionsSubset) {
//						/* Add the transition's target state to the target state set. */
//						targetStateSet.add(t.getKey());
//						/*
//						 * If some target state is an accepting SFA state, then the corresponding new
//						 * DFSA state shall be accepting.
//						 */
//						if (t.getKey().isAccepting()) {
//							isAcceptingNewDeterministicState = true;
//						}
//					}
//					for (Map.Entry<SFAState, BDD> t : transitionsSubsetComplement) {
//						if (transitionBDD.isZero()) {
//							break;
//						}
//						transitionBDD.andWith(t.getValue().not());
//					}
//
//					if (!transitionBDD.isZero()) {
//						if (!stateSubsetsToDetreministicStatesMapping.containsKey(targetStateSet)) {
//							stateSubsetsToDetreministicStatesMapping.put(targetStateSet,
//									new SFAState(isAcceptingNewDeterministicState));
//							workStack.add(0, targetStateSet);
//						}
//
//						SFAState currentState = stateSubsetsToDetreministicStatesMapping.get(currentStateSet);
//						SFAState targetState = stateSubsetsToDetreministicStatesMapping.get(targetStateSet);
//						currentState.addTrans(transitionBDD, targetState);
//					}
//					else {
//
//						transitionBDD.free();
//					}
//				}
//
//				nextPair = iterator.next();
//			}
//		}
//
//		return deterministicAutomaton;
//	}
	
//	/**
//	 * Returns a deterministic SFA (DSFA) that is equivalent to this SFA.
//	 * 
//	 * The algorithm follows the work by Veanes et al. (2010).
//	 * 
//	 * The algorithm uses an iterator over subsets of outgoing transitions whose type can be
//	 * checked via {@link #getPowerSetIteratorType} and can be set via {@link #setPowerSetIteratorType}.
//	 *
//	 * @return
//	 */
//	public SFA determinizeOldIter() {
//		
//		//create a fresh copy of this automaton and remove all the dead states from the fresh copy
//		SFA copySfa = this.copy();
//		copySfa.removeDeadStates();
//		
//		//check if the removal of dead states has resulted in a deterministic automaton
//		if (copySfa.isDeterministic()) {
//			return copySfa; //we have a deterministic automaton so we are done
//		}
//		
//		
//		//we have that copySfa is non-deterministic so we need to determinize it
//		
//		SFA deterministicAutomaton = new SFA();
//
//		/*
//		 * To distinguish between different states in the DSFA, we need to remember a
//		 * mapping between sets of states in this SFA to the new states in the DSFA that
//		 * we create.
//		 */
//		Map<Set<SFAState>, SFAState> stateSubsetsToDeterministicStatesMapping = new HashMap<>();
//		Set<SFAState> iniSingletonSet = new HashSet<>(Arrays.asList(copySfa.ini));
//
//		/*
//		 * The initial state in the DSFA is a singleton containing the SFA's initial
//		 * state.
//		 */
//		SFAState determinsticAutomatonInitialState = copySfa.ini.copyWithoutSucc();
//		stateSubsetsToDeterministicStatesMapping.put(iniSingletonSet, determinsticAutomatonInitialState);
//		deterministicAutomaton.setIni(determinsticAutomatonInitialState);
//
//		/*
//		 * We use a stack to do a DFS search on (the copy without dead states of) this SFA. It will contain the state
//		 * sets of the DSFA that we create.
//		 */
//		List<Set<SFAState>> workStack = new Stack<>();
//		workStack.add(0, iniSingletonSet);
//		while (!workStack.isEmpty()) {
//			Map<SFAState, BDD> allTransitionsMap = new HashMap<>();
//			Set<Map.Entry<SFAState, BDD>> tautologyTransitions = new HashSet<>();
//			Set<SFAState> currentStateSet = workStack.remove(0), tautologySuccessors = new HashSet<>();
//			for (SFAState state : currentStateSet) {
//				for (Map.Entry<SFAState, BDD> transition : state.getSucc().entrySet()) {
//					/*
//					 * Optimization 1: We don't need to add to the transitions set (denoted by
//					 * delta_A(q) in the paper) unsatisfiable transitions, since each subset t of
//					 * delta_A(q) that contains that transition will result a unsatisfiable formula
//					 * phi_t, since the left conjunction will not be satisfiable.
//					 *
//					 * Optimization 2: If a subset t of delta_A(q) DOESN'T contains a tautology
//					 * transition, then the resulting phi_t will be unsatisfiable, since the right
//					 * conjunction will be unsatisfiable. Thus, it suffice to go over delta_A(q)'s
//					 * subsets that contain all delta_A(q)'s tautology transitions, which is
//					 * equivalent to going over all the subsets of delta_A(q)\{tautologies} and
//					 * adding to each subset {tautologies} (the set of all tautology transitions).
//					 * 
//					 * Optimization 3: If there are multiple transitions to the SAME target state, then we can transform
//					 * them into a single transition by taking their disjunction. This may save (exponentially many but redundant) iterations over
//					 * subsets of delta_A(q) that contain different combinations of transitions to the SAME successor state. In case the disjunction
//					 * evaluates to a tautology (i.e., it is trivially true), then we treat the "new" transition as in Optimization 2.
//					 * 
//					 */
//					if (!transition.getValue().isZero()) {
//						if(allTransitionsMap.containsKey(transition.getKey())) {
//							allTransitionsMap.get(transition.getKey()).orWith(transition.getValue().id());
//							if(allTransitionsMap.get(transition.getKey()).isOne()) {
//								//the guard of the transition to the successor state transition.getKey() is TRUE
//								allTransitionsMap.get(transition.getKey()).free();
//								allTransitionsMap.remove(transition.getKey());
//								tautologyTransitions.add(transition);
//								tautologySuccessors.add(transition.getKey());
//							}
//							
//						}
//						else if(!tautologySuccessors.contains(transition.getKey())) {
//							//this is the first time a transition to this target/successor state is seen
//							if (transition.getValue().isOne()) {
//								//tautology transition
//								tautologyTransitions.add(transition);
//								tautologySuccessors.add(transition.getKey());
//							}
//							else {
//								allTransitionsMap.put(transition.getKey(), transition.getValue().id());
//							}
//						}
//					}
//				}
//			}
//
//			Set<Map.Entry<SFAState, BDD>> allTransitionsSet = allTransitionsMap.entrySet();
//			//Iterator<Pair<Set<Map.Entry<SFAState, BDD>>, BDD>> psIter = PowerSetIteratorFactory.getPowerSetIterator(this.powerSetIteratorType, allTransitionsSet);
//			EfficientPowerSetIterator psIter = new EfficientPowerSetIterator(allTransitionsSet);
//			Pair<Set<Map.Entry<SFAState, BDD>>, BDD> nextPair;
//
//			Set<Map.Entry<SFAState, BDD>> transitionsSubset, transitionsSubsetComplement;
//			boolean newDeterministicStateIsAccepting;
//			Set<SFAState> successorStatesSet;
//			BDD transitionsGuardsConjunction;
//			SFAState detCurrentState = stateSubsetsToDeterministicStatesMapping.get(currentStateSet), detSuccessorState;
//
//			nextPair = psIter.next();
//			while (nextPair!=null) {
//				transitionsSubset = nextPair.getLeft();
//				transitionsSubset.addAll(tautologyTransitions);
//				/*
//				 * An empty transitions set is not interesting since its successor states set is
//				 * empty (this corresponds to taking no transitions at all).
//				 */
//				if (!transitionsSubset.isEmpty()) {
//					transitionsGuardsConjunction = nextPair.getRight();
//					transitionsSubsetComplement = getComplementOfTransSet(allTransitionsSet, transitionsSubset);
//					for (Map.Entry<SFAState, BDD> t : transitionsSubsetComplement) {
//						if (transitionsGuardsConjunction.isZero()) {
//							break;
//						}
//						transitionsGuardsConjunction.andWith(t.getValue().not());
//					}				
//					if (!transitionsGuardsConjunction.isZero()) {
//						successorStatesSet = SFAUtils.getTransitionsTargetStates(transitionsSubset);
//						/*
//						 * If some successor state is an accepting SFA state, then the corresponding new
//						 * DFSA state should be an accepting state (as indicated by 'newDeterministicStateIsAccepting').
//						 */
//						newDeterministicStateIsAccepting = SFAUtils.containsAcceptingState(successorStatesSet);
//						
//						if (!stateSubsetsToDeterministicStatesMapping.containsKey(successorStatesSet)) {
//							stateSubsetsToDeterministicStatesMapping.put(successorStatesSet,
//									new SFAState(newDeterministicStateIsAccepting));
//							workStack.add(0, successorStatesSet);
//						}
//						detSuccessorState = stateSubsetsToDeterministicStatesMapping.get(successorStatesSet);
//						detCurrentState.addTrans(transitionsGuardsConjunction, detSuccessorState);
//					}
//					else {	
//						transitionsGuardsConjunction.free();
//					}
//				}
//				nextPair = psIter.next();
//			}
//			SFAUtils.freeTransitionsSet(allTransitionsSet);
//		}
//		copySfa.free();
//		return deterministicAutomaton;
//	}
	
//	/**
//	 * A class for constructing a lazy, memory-efficient, power set iterator that
//	 * uses a binary counter.
//	 */
//	public static class PowerSetIterator<T> implements Iterator<Set<T>>{
//
//		private List<T> setList;
//		private boolean[] nextSubSetBinaryList;
//		private Set<T> nextSubSet = new HashSet<>();
//		private Set<T> nextNextSubSet = new HashSet<>();
//
//		public PowerSetIterator(Set<T> set) {
//			this.setList = new ArrayList<T>(set);
//			this.nextSubSetBinaryList = new boolean[set.size()];
//		}
//
//		@Override
//		public boolean hasNext() {
//			if (this.nextNextSubSet == null) {
//				return false;
//			}
//
//			return true;
//		}
//
//		@Override
//		public Set<T> next() {
//			if (!this.hasNext()) {
//				return null;
//			}
//
//			this.nextSubSet.clear();
//			this.nextSubSet.addAll(this.nextNextSubSet);
//
//			for (int i = 0; i < this.nextSubSetBinaryList.length; ++i) {
//				this.nextSubSetBinaryList[i] = !this.nextSubSetBinaryList[i];
//				if (this.nextSubSetBinaryList[i]) {
//					break;
//				}
//			}
//
//			this.nextNextSubSet.clear();
//			for (int i = 0; i < this.setList.size(); ++i) {
//				if (this.nextSubSetBinaryList[i]) {
//					this.nextNextSubSet.add(this.setList.get(i));
//				}
//			}
//
//			if (this.nextNextSubSet.isEmpty()) {
//				this.nextNextSubSet = null;
//			}
//
//			return this.nextSubSet;
//		}
//	}



//	/**
//	 * A class for constructing a lazy power set iterator over sets of SFA
//	 * transitions, which only generates subsets whose transition formulas
//	 * conjunction is satisfiable.
//	 */
//	public static class EfficientPowerSetIterator {
//
//		private List<Map.Entry<SFAState, BDD>> transitionsSetList;
//		private Set<Map.Entry<SFAState, BDD>> nextSubset = new HashSet<>();
//		private List<Integer> nextSubsetIndices = new ArrayList<>();
//		private List<List<Integer>> prevSizeSubsetIndices = new ArrayList<>();
//		private int currentPrevSizeSubsetIndicesListIndex = 0;
//		private int nextTransitionsSetListIndex = 0;
//		private List<List<Integer>> nextSizeSubsetIndices = new ArrayList<>();
//		private List<BDD> prevSizeSubsetBDDs = new ArrayList<>();
//		private List<BDD> nextSizeSubsetBDDs = new ArrayList<>();
//
//		public EfficientPowerSetIterator(Set<Map.Entry<SFAState, BDD>> transitionsSet) {
//			transitionsSetList = new ArrayList<>(transitionsSet);
//			prevSizeSubsetIndices.add(new ArrayList<>());
//			prevSizeSubsetBDDs.add(Env.TRUE());
//		}
//
//		/*
//		 * This method is basically a flat (iterative) version of a recursive method
//		 * that generates lazily all SATISFIABLE transition subsets of growing size, and
//		 * their formulas' BDD conjunction. Once there are no satisfiable subsets left,
//		 * null is returned.
//		 */
//		public Pair<Set<Map.Entry<SFAState, BDD>>, BDD> next() {
//			while (nextSubsetIndices != null) {
//				if (nextSubsetIndices.isEmpty()) {
//					if (transitionsSetList.isEmpty()) {
//						nextSubsetIndices = null;
//					} else {
//						nextSubsetIndices.add(-1);
//					}
//
//					return new Pair<>(new HashSet<>(), Env.TRUE().id());
//				}
//
//				if (nextTransitionsSetListIndex < transitionsSetList.size()) {
//					BDD nextSizeSubsetBDD = prevSizeSubsetBDDs.get(currentPrevSizeSubsetIndicesListIndex)
//							.and(transitionsSetList.get(nextTransitionsSetListIndex).getValue());
//
//					if (nextSizeSubsetBDD.isZero()) {
//						++nextTransitionsSetListIndex;
//						continue;
//					}
//
//					nextSizeSubsetBDDs.add(nextSizeSubsetBDD);
//
//					nextSubsetIndices = new ArrayList<>();
//					nextSubsetIndices.addAll(prevSizeSubsetIndices.get(currentPrevSizeSubsetIndicesListIndex));
//					nextSubsetIndices.add(nextTransitionsSetListIndex);
//
//					nextSizeSubsetIndices.add(nextSubsetIndices);
//
//					nextSubset.clear();
//					for (int i : nextSubsetIndices) {
//						nextSubset.add(transitionsSetList.get(i));
//					}
//
//					++nextTransitionsSetListIndex;
//					return new Pair<>(nextSubset, nextSizeSubsetBDD.id());
//				}
//
//				while (nextTransitionsSetListIndex == transitionsSetList.size()) {
//					++currentPrevSizeSubsetIndicesListIndex;
//					if (currentPrevSizeSubsetIndicesListIndex == prevSizeSubsetIndices.size()) {
//						if (nextSizeSubsetIndices.isEmpty()) {
//							nextSubsetIndices = null;
//							break;
//						}
//
//						prevSizeSubsetIndices.clear();
//						prevSizeSubsetIndices.addAll(nextSizeSubsetIndices);
//						currentPrevSizeSubsetIndicesListIndex = 0;
//						prevSizeSubsetBDDs.clear();
//						prevSizeSubsetBDDs.addAll(nextSizeSubsetBDDs);
//						nextSizeSubsetIndices.clear();
//						nextSizeSubsetBDDs.clear();
//					}
//
//					List<Integer> currentPrevSizeSubsetIndicesList = prevSizeSubsetIndices
//							.get(currentPrevSizeSubsetIndicesListIndex);
//					nextTransitionsSetListIndex = currentPrevSizeSubsetIndicesList
//							.get(currentPrevSizeSubsetIndicesList.size() - 1) + 1;
//				}
//			}
//
//			return null;
//		}
//
//	}

//	/**
//	 * Returns a deterministic SFA (DSFA) that is equivalent to this SFA.
//	 * 
//	 * This algorithm is inspired by Veanes et al. (2010).
//	 * 
//	 */
//	public SFA determinizeOrg() {
//		this.removeDeadStates();
//
//		if (this.isDeterministic()) {
//			return this.copy();
//		}
//
//		SFA deterministicAutomaton = new SFA();
//
//		/*
//		 * To distinguish between different states in the DSFA, we need to remember a
//		 * mapping between sets of states in this SFA to the new states in the DSFA that
//		 * we create.
//		 */
//		Map<Set<SFAState>, SFAState> stateSubsetsToDetreministicStatesMapping = new HashMap<>();
//		Set<SFAState> iniSingletonSet = new HashSet<>(Arrays.asList(ini));
//
//		/*
//		 * The initial state in the DSFA is a singleton containing the SFA's initial
//		 * state.
//		 */
//		SFAState detreminsticAutomatonInitialState = new SFAState(ini.isAccepting());
//		stateSubsetsToDetreministicStatesMapping.put(iniSingletonSet, detreminsticAutomatonInitialState);
//		deterministicAutomaton.setIni(detreminsticAutomatonInitialState);
//
//		/*
//		 * We use a stack to do a DFS search on the given SFA. It will contain the state
//		 * sets of the DSFA that we create.
//		 */
//		List<Set<SFAState>> workStack = new Stack<>();
//		workStack.add(0, iniSingletonSet);
//		while (!workStack.isEmpty()) {
//			Set<Map.Entry<SFAState, BDD>> transitionsSet = new HashSet<>();
//			Set<Map.Entry<SFAState, BDD>> tautologyTransitions = new HashSet<>();
//			Set<SFAState> currentStateSet = workStack.remove(0);
//			for (SFAState state : currentStateSet) {
//				for (Map.Entry<SFAState, BDD> transition : state.getSucc().entrySet()) {
//					/*
//					 * Optimization 1: We don't need to add to the transitions set (denoted by
//					 * delta_A(q) in the paper) unsatisfiable transitions, since each subset t of
//					 * delta_A(q) that contains that transition will result a unsatisfiable formula
//					 * phi_t, since the left conjunction will not be satisfiable.
//					 */
//					if (!transition.getValue().isZero()) {
//						transitionsSet.add(transition);
//						if (transition.getValue().isOne()) {
//							tautologyTransitions.add(transition);
//						}
//					}
//				}
//			}
//
//			/*
//			 * Optimization 2: If a subset t of delta_A(q) DOESN'T contains a tautology
//			 * transition, then the resulting phi_t will be unsatisfiable, since the right
//			 * conjunction will be unsatisfiable. Thus, it suffice to go over delta_A(q)'s
//			 * subsets that contain all delta_A(q)'s tautology transitions, which is
//			 * equivalent to going over all the subsets of delta_A(q)\{tautologies} and
//			 * adding to each subset {tautologies} (the set of all tautology transitions).
//			 */
//			transitionsSet.removeAll(tautologyTransitions);
//			PowerSetIterator<Map.Entry<SFAState, BDD>> iterator = new PowerSetIterator<>(transitionsSet);
//			while (iterator.hasNext()) {
//				Set<Map.Entry<SFAState, BDD>> transitionsSubset = iterator.next();
//				transitionsSubset.addAll(tautologyTransitions);
//
//				/*
//				 * Empty transitions set is not interesting since its target states set is
//				 * empty.
//				 */
//				if (!transitionsSubset.isEmpty()) {
//					Set<Map.Entry<SFAState, BDD>> transitionsSubsetComplement = new HashSet<>(transitionsSet);
//					transitionsSubsetComplement.removeAll(transitionsSubset);
//
//					Set<SFAState> targetStateSet = new HashSet<>();
//					boolean isAcceptingNewDeterministicState = false;
//					BDD transitionBDD = Env.TRUE();
//					for (Map.Entry<SFAState, BDD> t : transitionsSubset) {
//						if (transitionBDD.isZero()) {
//							break;
//						}
//
//						transitionBDD.andWith(t.getValue().id());
//
//						/* Add the transition's target state to the target state set. */
//						targetStateSet.add(t.getKey());
//						/*
//						 * If some target state is an accepting SFA state, then the corresponding new
//						 * DFSA state shall be accepting.
//						 */
//						if (t.getKey().isAccepting()) {
//							isAcceptingNewDeterministicState = true;
//						}
//					}
//					for (Map.Entry<SFAState, BDD> t : transitionsSubsetComplement) {
//						if (transitionBDD.isZero()) {
//							break;
//						}
//						transitionBDD.andWith(t.getValue().not());
//					}
//
//					if (!transitionBDD.isZero()) {
//						if (!stateSubsetsToDetreministicStatesMapping.containsKey(targetStateSet)) {
//							stateSubsetsToDetreministicStatesMapping.put(targetStateSet,
//									new SFAState(isAcceptingNewDeterministicState));
//							workStack.add(0, targetStateSet);
//						}
//
//						SFAState currentState = stateSubsetsToDetreministicStatesMapping.get(currentStateSet);
//						SFAState targetState = stateSubsetsToDetreministicStatesMapping.get(targetStateSet);
//						currentState.addTrans(transitionBDD, targetState);
//					}
//					else {
//
//						transitionBDD.free();
//					}
//				}
//			}
//		}
//
//		return deterministicAutomaton;
//	}
	
}