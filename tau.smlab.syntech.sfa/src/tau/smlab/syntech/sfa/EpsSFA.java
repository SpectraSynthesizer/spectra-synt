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

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.sfa.PowerSetIterator.PowerSetIteratorType;

/**
 * Epsilon-SFA - Symbolic finite automaton that may have state with outgoing epsilon transitions (moves).
 * 
 * @author Or Pistiner
 *
 */
public class EpsSFA extends BaseSFA<EpsSFAState> {

	final public static String EPS_LABEL = "Epsilon";
	
	public EpsSFA() {
		super();
	}
	
	public EpsSFA(SFAState ini) {
		super(ini);
	}
	
	public EpsSFA(PowerSetIteratorType psIterType) {
		super(psIterType);
	}
	
	public EpsSFA(SFAState ini, PowerSetIteratorType psIterType) {
		super(ini, psIterType);
	}
	
	/**
	 * Computes the epsilon closure sets of all reachable states (from the initial state) in this epsilon SFA.
	 * The epsilon closure of a state S is the set of all states reachable from S by an epsilon labeled path. 
	 * 
	 * @return mapping of each reachable state to its epsilon closure
	 */
	public Map<EpsSFAState, Set<EpsSFAState>> computeEpsClosures() {
		Map<EpsSFAState, Set<EpsSFAState>> epsClosures = new HashMap<>();
		computeEpsClosuresRec(this.getIni(), epsClosures);
		return epsClosures;
	}

	@Override
	public boolean isDeterministic() {
		return !this.hasEpsTrans() && super.isDeterministic();
	}

	public boolean hasEpsTrans() {
		Queue<EpsSFAState> worklist = new LinkedList<>();
		Set<EpsSFAState> seen = new LinkedHashSet<>();
		worklist.add(this.getIni());
		seen.add(this.getIni());
		while (!worklist.isEmpty()) {
			EpsSFAState s = worklist.remove();
			if(s.hasEpsSuccessors()) {
				// a reachable state with an outgoing epsilon transition has been found
				return true;
			}
			// add successors which have not been checked yet (at this point, all successors are non-epsilon ones)
			for (EpsSFAState succ : s.getSuccessors()) {
				if (!seen.contains(succ)) {
					worklist.add(succ);
					seen.add(succ);
				}
			}
		}
		//no reachable epsilon transition has been found
		return false;
	}
	
	@Override
	public boolean enablesEpsTrans() {
		return true;
	}

	/**
	 * Elimination of epsilon transitions (moves). Returns an equivalent EpsSFA which has no epsilon moves.
	 * This implementation is inspired by Veanes et al. (2010).
	 * 
	 * @return
	 */
	@Override
	public EpsSFA eliminateEpsTrans() {
		
		Map<EpsSFAState, Set<EpsSFAState>> epsClosures = computeEpsClosures();
		Map<EpsSFAState, EpsSFAState> origStateToCopyState = new HashMap<>();

		//create a fresh copy of the initial state and determine whether it is a final state in the new automaton
		origStateToCopyState.put(this.getIni(), new EpsSFAState(SFAUtil.containsAcceptingState(epsClosures.get(this.getIni()))));

		//initialize the resulting new automaton
		EpsSFA epsFreeSfa = new EpsSFA();
		epsFreeSfa.setIni(origStateToCopyState.get(this.getIni()));

		Queue<EpsSFAState> worklist = new LinkedList<>();
		worklist.add(this.getIni());


		Set<EpsSFAState> epsClosure, succEpsClosure;
		Map<EpsSFAState, BDD> nonEpsSuccs;
		EpsSFAState origState, copyState, copySucc;
		boolean copySuccIsFinal;

		while (!worklist.isEmpty()) {
			origState = worklist.remove();
			copyState = origStateToCopyState.get(origState);

			//compute all outgoing transitions of the copy state in the new automaton
			epsClosure = epsClosures.get(origState);
			nonEpsSuccs = SFAUtil.getSucc(epsClosure);

			for(EpsSFAState nonEpsSucc : nonEpsSuccs.keySet()) {
				//obtain the copy of nonEpsSucc (i.e., the successor in the new automaton)
				if(origStateToCopyState.containsKey(nonEpsSucc)) {
					copySucc = origStateToCopyState.get(nonEpsSucc);
				}
				else {
					//we have not seen the state nonEpsSucc before; create a fresh copy and add nonEpsSucc to the BFS queue 
					succEpsClosure = epsClosures.get(nonEpsSucc);
					copySuccIsFinal = SFAUtil.containsAcceptingState(succEpsClosure);
					copySucc = new EpsSFAState(copySuccIsFinal);
					origStateToCopyState.put(nonEpsSucc, copySucc);
					worklist.add(nonEpsSucc);
				}
				//add a new transition (move) that goes into the copy of nonEpsSucc
				copyState.addTrans(nonEpsSuccs.get(nonEpsSucc), copySucc);
			}
		}
		return epsFreeSfa;
	}

	@Override
	public EpsSFA determinize() {
		//create an equivalent automaton without epsilon moves
		EpsSFA noEpsAutomaton = this.eliminateEpsTrans();

		//determinize the epsilon-free automaton
		EpsSFA detAutomaton = noEpsAutomaton.determinize();

		//free the BDDs of the epsilon-free automaton 
		noEpsAutomaton.free();

		return detAutomaton;
	}

	@Override
	public EpsSFA copy() {
		EpsSFA copy = new EpsSFA();
		Map<EpsSFAState, EpsSFAState> copyStateMappingMap = new HashMap<>();

		copy.setIni(this.getIni().cloneWithoutSucc());
		copyStateMappingMap.put(this.getIni(), copy.getIni());

		Queue<EpsSFAState> worklist = new LinkedList<>();
		worklist.add(this.getIni());
		
		while (!worklist.isEmpty()) {
			EpsSFAState s = worklist.remove();

			//non-epsilon transitions/successors
			for (Map.Entry<EpsSFAState, BDD> transition : s.getSucc().entrySet()) {
				if (!copyStateMappingMap.containsKey(transition.getKey())) {
					worklist.add(transition.getKey());
					copyStateMappingMap.put(transition.getKey(), transition.getKey().cloneWithoutSucc());
				}

				copyStateMappingMap.get(s).addTrans(transition.getValue().id(),
						copyStateMappingMap.get(transition.getKey()));
			}

			//handle epsilon transitions, if such transitions exist
			if(s.hasEpsSuccessors()) {
				for(EpsSFAState epsSucc : s.getEpsSucc()) {
					if (!copyStateMappingMap.containsKey(epsSucc)) {
						worklist.add(epsSucc);
						copyStateMappingMap.put(epsSucc, epsSucc.cloneWithoutSucc());
					}
					copyStateMappingMap.get(s).addEpsTrans(copyStateMappingMap.get(epsSucc));
				}
			}
		}
		return copy;
	}
	
	@Override
	protected EpsSFA getNewSFAInstance() {
		return new EpsSFA();
	}
	
	@Override
	protected EpsSFA getNewSFAInstance(EpsSFAState ini) {
		return new EpsSFA(ini);
	}
	
	@Override
	public EpsSFAState getNewState(boolean isAccepting) {
		return new EpsSFAState(isAccepting);
	}
	
	@Override
	protected Class<EpsSFAState> getStatesType() {
		return EpsSFAState.class;
	}
	
	@Override
	public String toString() {
		//TODO implement me
		return "";
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

	/**
	 * Traverses through the underlying directed graph of this epsilon SFA in a DFS fashion to compute
	 * the epsilon closure of each reachable state.
	 * 
	 * @param state
	 * @param epsClosures
	 */
	private void computeEpsClosuresRec(EpsSFAState state,
			Map<EpsSFAState,Set<EpsSFAState>> epsClosures) {
		epsClosures.put(state, new LinkedHashSet<EpsSFAState>(Arrays.asList(state)));

		for(EpsSFAState succ : state.getSuccessors()) {
			if(!epsClosures.containsKey(succ)) {
				computeEpsClosuresRec(succ, epsClosures);
			}
			if(succ != state && state.isEpsSuccessor(succ)) {
				epsClosures.get(state).addAll(epsClosures.get(succ));
			}	
		}
	}

}
