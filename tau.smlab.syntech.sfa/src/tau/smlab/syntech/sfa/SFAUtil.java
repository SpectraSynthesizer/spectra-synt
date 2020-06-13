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
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.javabdd.BDD;


/**
 * 
 * A class of static methods for SFA related operations. 
 * 
 * @author Or Pistiner
 *
 */
public class SFAUtil {


	private SFAUtil() {};

	/**
	 * Checks whether the specified set of states contains an element which is an accepting state.
	 * 
	 * @param statesSet
	 * @return true iff {@code statesSet} has an accepting state 
	 */
	public static <T extends SFAState> boolean containsAcceptingState(Set<T> statesSet) {
		for(T state : statesSet) {
			if (state.isAccepting()) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns a set of all direct, NON-epsilon successors of all states in {@code statesSet}.
	 * 
	 * @param statesSet
	 * @return
	 */
	public static <T extends SFAState> Set<SFAState> getSuccessors(Set<T> statesSet) {
		Set<SFAState> allSuccs = new LinkedHashSet<>();
		for(T state : statesSet) {
			for(SFAState succ : state.getSucc().keySet()) {
				allSuccs.add(succ);
			}
		}
		return allSuccs;
	}

	/**
	 * Returns the successor map (i.e., copies of outgoing, NON-epsilon transitions) of all states in {@code statesSet}.
	 * Transitions with the same successor state are transformed into a single transition by taking the disjunction of their conditions (guards).
	 * 
	 * @param statesSet
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T extends SFAState> Map<T, BDD> getSucc(Set<T> statesSet) {
		Map<T, BDD> allTrans = new LinkedHashMap<>();
		for(T state : statesSet) {
			for(SFAState succ : state.getSucc().keySet()) {
				if(allTrans.containsKey(succ)) {
					allTrans.get(succ).orWith(state.getSucc().get(succ).id());
				}
				else {
					allTrans.put((T) succ, state.getSucc().get(succ).id());
				}
			}
		}
		return allTrans;
	}

	/**
	 * Returns the set of the target (successor) states of the given set of transitions. That is, technically, returns
	 * the set that contains all the keys of the map entries in the given set. 
	 * 
	 * @param transitionsSet set of transitions
	 * @return
	 */
	public static <T extends SFAState> Set<T> getTransitionsTargetStates(Set<Map.Entry<T, BDD>> transitionsSet) {
		Set<T> successorsStatesSet = new LinkedHashSet<>();

		for (Map.Entry<T, BDD> t : transitionsSet) {
			//Add the transition's successor state to the successor states' set
			successorsStatesSet.add(t.getKey());
		}
		return successorsStatesSet;
	}

	/**
	 * Frees all the guards of the transitions in the given set.
	 * 
	 * @param transitionsSet
	 */
	public static <T extends SFAState> void freeTransitionsSet(Set<Map.Entry<T, BDD>> transitionsSet) {
		BDD guard;
		for (Map.Entry<T, BDD> t : transitionsSet) {
			guard = t.getValue();
			if(!guard.isFree()) {
				guard.free();
			}
		}
	}

	/**
	 * 
	 * Create a list of states of the same size as the specified list of states.
	 * Each state is cloned without its successors. The cloned state has no successors.
	 * It is marked as final (resp. enables outgoing epsilon transitions) iff the corresponding state in {@code statesList} is marked as final
	 * (resp. allows such transitions).
	 * 
	 * @param statesList
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T extends SFAState> List<T> cloneStatesList(List<T> statesList) {

		List<T> clonedStatesList = new ArrayList<T>();

		for (int i = 0; i < statesList.size(); i++) {
			clonedStatesList.add((T)statesList.get(i).cloneWithoutSucc());		
		}

		return clonedStatesList;
	}
	
	/**
	 * Returns a copy of the specified state. The returned copy is an accepting state iff the specified state is accepting.
	 * The returned copy allows adding epsilon transitions iff the specified state allows that.
	 * The returned copy has the SAME non-epsilon outgoing transitions. That is, all BDD guards are copied, while the SAME successor states are used.
	 * Outgoing epsilon transitions are not copied.
	 * 
	 * @param state
	 * @return
	 */
	public static SFAState cloneStateWithSameSucc(SFAState state) {
		SFAState stateClone = state.cloneWithoutSucc();
		for(SFAState succ : state.getSucc().keySet()) {
			stateClone.addTrans(state.getSucc().get(succ).id(), succ);
		}
		return stateClone;
	}

	/**
	 * Returns the set {@code universe} \ {@code transSet}, namely the complement of {@code transSet} w.r.t. {@code universe}.
	 * @param universe
	 * @param transSet
	 * @return
	 */
	public static <T extends SFAState> Set<Map.Entry<T, BDD>> getComplementOfTransSet(Set<Map.Entry<T, BDD>> universe, Set<Map.Entry<T, BDD>> transSet) {
		Set<Map.Entry<T, BDD>> compTransSet = new HashSet<>(universe);
		compTransSet.removeAll(transSet);
		return compTransSet;
	}
	
	/**
	 * Adds the specified outgoing transitions to the given state.
	 * 
	 * @param state
	 * @param transitions
	 */
	public static <T extends SFAState> void addTransitionsToState(T state, Map<? extends T, BDD> transitions) {
		for(T succ : transitions.keySet()) {
			state.addTrans(transitions.get(succ).id(), succ);
		}
	}
}
