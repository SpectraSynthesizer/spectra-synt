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

	/**
	 * 
	 * Use an instance of {@link EpsClosure} to obtain the epsilon closure of each state in this {@link EpsSFA}.
	 *
	 */
	class EpsClosure {

		private Map<EpsSFAState, Set<EpsSFAState>> epsClosures;

		EpsClosure() {
			this.epsClosures = new HashMap<>();
		}
		
		/**
		 * Returns the epsilon closure of the specified state.
		 * 
		 * @param state
		 * @return
		 */
		Set<EpsSFAState> getClosureOf(EpsSFAState state) {
			if(!this.epsClosures.containsKey(state)) {
				this.epsClosures.put(state, state.getEpsClosure());			
			}
			return this.epsClosures.get(state);
		}
	}

	protected EpsSFA() {
		super();
	}

	protected EpsSFA(SFAState ini) {
		super(ini);
	}

	protected EpsSFA(PowerSetIteratorType psIterType) {
		super(psIterType);
	}

	protected EpsSFA(SFAState ini, PowerSetIteratorType psIterType) {
		super(ini, psIterType);
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

		EpsClosure epsClosures = new EpsClosure();
		Map<EpsSFAState, EpsSFAState> origStateToCopyState = new HashMap<>();

		//create a fresh copy of the initial state and determine whether it is a final state in the new automaton
		origStateToCopyState.put(this.getIni(), new EpsSFAState(SFAUtil.containsAcceptingState(epsClosures.getClosureOf(this.getIni()))));

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

			//Compute all outgoing transitions of the copy state in the new automaton
			epsClosure = epsClosures.getClosureOf(origState);
			nonEpsSuccs = SFAUtil.getSucc(epsClosure);

			for(EpsSFAState nonEpsSucc : nonEpsSuccs.keySet()) {
				//Obtain the copy of nonEpsSucc (i.e., the successor in the new automaton)
				if(origStateToCopyState.containsKey(nonEpsSucc)) {
					copySucc = origStateToCopyState.get(nonEpsSucc);
				}
				else {
					//We have not seen the state nonEpsSucc before; create a fresh copy and add nonEpsSucc to the BFS queue 
					succEpsClosure = epsClosures.getClosureOf(nonEpsSucc);
					copySuccIsFinal = SFAUtil.containsAcceptingState(succEpsClosure);
					copySucc = new EpsSFAState(copySuccIsFinal);
					origStateToCopyState.put(nonEpsSucc, copySucc);
					worklist.add(nonEpsSucc);
				}
				//Add a new transition (move) that goes into the copy of nonEpsSucc
				copyState.addTrans(nonEpsSuccs.get(nonEpsSucc), copySucc);
			}
		}
		return epsFreeSfa;
	}

	@Override
	public BaseSFA<EpsSFAState> determinize() {
		//create an equivalent automaton without epsilon moves
		EpsSFA noEpsAutomaton = this.eliminateEpsTrans();

		//determinize the epsilon-free automaton
		BaseSFA<EpsSFAState> detAutomaton = noEpsAutomaton.buildDetAutomaton();

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
	protected EpsSFA newSfaInstance() {
		return new EpsSFA();
	}

	@Override
	protected EpsSFA newSfaInstance(EpsSFAState ini) {
		return new EpsSFA(ini);
	}

	@Override
	public EpsSFAState newSfaState(boolean isAccepting) {
		return new EpsSFAState(isAccepting);
	}

	@Override
	protected Class<EpsSFAState> getStatesType() {
		return EpsSFAState.class;
	}
	
	@Override
	public boolean acceptsTheEmptyString() {
		Queue<EpsSFAState> worklist = new LinkedList<>();
		if(this.ini.isAccepting()) {
			return true;
		}
		Set<EpsSFAState> seen = new LinkedHashSet<>();
		seen.add(this.ini);
		if(this.ini.hasEpsSuccessors()) {
			worklist.add(this.ini);		
		}
		//Search for a final state reachable only via epsilon transitions
		while (!worklist.isEmpty()) {
			EpsSFAState s = worklist.remove();
			for (EpsSFAState epsSucc : s.getEpsSucc()) { //Add epsilon successors not checked yet
				if(epsSucc.isAccepting()) {
					return true;
				}
				if (!seen.contains(epsSucc)) {
					seen.add(epsSucc);
					if(epsSucc.hasEpsSuccessors()) {
						worklist.add(epsSucc);							
					}
				}
			}
		}
		return false;
	}

}
