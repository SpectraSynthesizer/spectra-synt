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

import java.util.Map;
import java.util.Set;

import net.sf.javabdd.BDD;

/**
 * 
 * An interface that represents a state of an SFA.
 * 
 * @author Maxim Finkel
 * @author Or Pistiner
 * @author Gal Amram
 * 
 */
public interface SFAState {
	
	/**
	 * 
	 * An exception thrown due to an invalid SFAState operation.
	 *
	 */
	public static class SFAStateException extends RuntimeException {

		/**
		 * 
		 */
		private static final long serialVersionUID = -3538711506175144666L;

		public SFAStateException() {super();}
		public SFAStateException(String msg) {super(msg);}
	}
	
	/**
	 * Checks whether there are any epsilon transitions (moves) that go out from this state.
	 * @return
	 */
	boolean hasEpsSuccessors();

	/**
	 * Returns the set of (direct) successors reachable by epsilon moves.
	 *  
	 * @return
	 */
	Set<? extends SFAState> getEpsSucc();

	/**
	 * Checks whether this SFAState allows adding outgoing epsilon transitions (moves).
	 * @return
	 */
	boolean enablesEpsTrans();

	/**
	 * If this SFAState allows adding outgoing epsilon transitions, adds an epsilon transition (move) to the specified successor state, {@code tgt}.
	 * 
	 * @param tgt
	 * @throws SFAStateException if {@link #enablesEpsTrans()} returns false
	 * @see {@link #enablesEpsTrans()}
	 */
	void addEpsTrans(SFAState tgt);

	/**
	 * Checks whether {@code succ} is a (direct) successor reachable from this state by an epsilon transition (move).
	 * @param succ
	 * @return
	 */
	boolean isEpsSuccessor(SFAState succ);

	/**
	 * Checks if this state is an accepting one.
	 */
	boolean isAccepting();

	/**
	 * Sets whether this state is an accepting (final) state.
	 * 
	 * @param isAccepting whether this state should be an accepting one
	 */
	void setAcceptance(boolean isAccepting);
	
	/**
	 * Changes (flips) the acceptance of this state.
	 */
	void flipAcceptance();

	/**
	 * Returns a copy of this SFAState. The returned copy is an accepting state iff this SFAState is accepting.
	 * The returned copy allows adding epsilon transitions iff this SFAState allows that.
	 * The successor map of this SFAState is not copied, i.e., the successor map of the returned copy is empty.
	 * @return
	 */
	SFAState cloneWithoutSucc();

	/**
	 * Returns the successor(s) map of this state.
	 * Each map entry represents a transition (move) where its key is a successor state and its value is the guard (condition).
	 * 
	 * @return
	 */
	Map<? extends SFAState, BDD> getSucc();

	/**
	 * Adds a (non-epsilon) transition with the specified guard to the successor state {@code tgt}, which must have the same type as this state has.
	 * To keep the automaton 'clean', if the specified guard is the FALSE BDD, then no transition is added while the FALSE guard is consumed.
	 * 
	 * @param guard
	 *            is consumed (freed) and cannot be used afterwards
	 * @param tgt
	 */
	void addTrans(BDD guard, SFAState tgt);

	/**
	 * Removes the transition to the specified successor state {@code tgt}, if such a transition exists. 
	 * If the transition is not an epsilon transition, its guard (BDD) is consumed (freed).
	 * 
	 * @param tgt
	 */
	void removeTrans(SFAState tgt);

	/**
	 * Returns all direct successor states of this state.
	 * 
	 * @return
	 */
	Set<? extends SFAState> getSuccessors();
	
	/**
	 * Frees the guards (BDDs) of all outgoing (non-epsilon) transitions of this state.
	 * 
	 */
	void free();
}
