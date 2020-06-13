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

import java.util.List;
import java.util.Set;

import tau.smlab.syntech.sfa.PowerSetIterator.PowerSetIteratorType;
/**
 * 
 * An interface that represents a Symbolic Finite Automaton.
 * 
 * @author Maxim Finkel
 * @author Or Pistiner
 * @author Gal Amram
 * 
 */
public interface SFA {
	
	/**
	 * 
	 * An exception thrown due to an invalid SFA operation.
	 *
	 */
	public static class SFAException extends RuntimeException {

		/**
		 * 
		 */
		private static final long serialVersionUID = 5000796717972170546L;
		
		public SFAException() {super();}
		public SFAException(String msg) {super(msg);}
		
	}
	
	/**
	 * A generic class of an ordered pair.
	 */
	public static class Pair<L, R> {

		private L left;
		private R right;

		public Pair(L left, R right) {
			this.left = left;
			this.right = right;
		}

		public L getLeft() {
			return left;
		}

		public R getRight() {
			return right;
		}

		public void setLeft(L left) {
			this.left = left;
		}

		public void setRight(R right) {
			this.right = right;
		}

		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + ((left == null) ? 0 : left.hashCode());
			result = prime * result + ((right == null) ? 0 : right.hashCode());
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Pair<?, ?> other = (Pair<?, ?>) obj;
			if (left == null) {
				if (other.left != null)
					return false;
			} else if (!left.equals(other.left))
				return false;
			if (right == null) {
				if (other.right != null)
					return false;
			} else if (!right.equals(other.right))
				return false;
			return true;
		}
	}
	
	/**
	 * Returns the initial state of this automaton.
	 * 
	 * @return the initial state
	 */
	SFAState getIni();
	
	/**
	 * Returns a new state of the type used by this automaton.
	 * 
	 * @param isAccepting whether the new state should be an accepting state
	 * @return
	 */
	SFAState newSfaState(boolean isAccepting);
	
	/**
	 * Sets the initial state of this automaton. The specified state must be of the type this automaton enables.
	 * 
	 * @param ini the initial state
	 * @throws SFAException if the specified state is not of the type this automaton enables
	 */
	void setIni(SFAState ini);
	
	/**
	 * Returns the type of the iterator over subsets of transitions used by {@link #determinize()} method.
	 * 
	 * @return
	 */
	PowerSetIteratorType getPsIterType();
	
	/**
	 * Sets the type of the iterator over subsets of transitions used by {@link #determinize()}  method.
	 * 
	 * @return
	 */
	void setPsIterType(PowerSetIteratorType powerSetIteratorType);

	/**
	 * Returns the set of final (accepting) states of this automaton.
	 * 
	 * @return
	 */
	Set<? extends SFAState> finalStates();

	/**
	 * Frees the BDDs of the transitions' guards in this automaton.
	 * 
	 * @return
	 */
	void free();

	/**
	 * Checks whether automaton is deterministic.
	 * 
	 * @return true iff this automaton is deterministic.
	 */
	boolean isDeterministic();
	
	/**
	 * Returns a structurally equivalent automaton to this automaton.
	 * 
	 * @return
	 */
	SFA copy();
	
	/**
	 * Checks whether there is an epsilon move (transition) reachable
	 * from the initial state of this automaton.
	 * 
	 * @return
	 */
	boolean hasEpsTrans();
	
	/**
	 * Checks whether this automaton enables adding outgoing epsilon moves (transitions).
	 * 
	 * @return
	 */
	boolean enablesEpsTrans();
	
	/**
	 * Elimination of epsilon transitions (moves). Returns an equivalent SFA that has no epsilon moves and enables
	 * adding epsilon transitions iff this automaton enables that.
	 * Implementations should follow the algorithm by Veanes et al. (2010).
	 * 
	 * @return
	 */
	SFA eliminateEpsTrans();

	/**
	 * Returns a deterministic SFA (DSFA) that is equivalent to this SFA.
	 * 
	 * Implementations should follow the algorithm by Veanes et al. (2010),
	 * and use an iterator over subsets of outgoing transitions whose type can be
	 * checked via {@link #getPsIterType} and set via {@link #setPsIterType}.
	 *
	 * @return
	 */
	SFA determinize();
	
	/**
	 * Completes this automaton's transition function by adding transitions to a (non-final) sink
	 * state from every state that has an undefined transition for some guard/condition.
	 * 
	 */
	void completeTransitionFunction();
	
	/**
	 * Returns a MINIMAL deterministic SFA (DSFA) that is equivalent to this SFA.
	 * The algorithm follows the work by Veanes et al. (2010).
	 * 
	 * @return
	 */
	SFA minimize();
	
	/**
	 * Returns the list of this' reachable states.
	 * 
	 * @return
	 */
	List<? extends SFAState> reachableStates();
	
	/**
	 * Returns the number of this automaton's reachable states.
	 * 
	 * @return
	 */
	int numStates();
	
	/**
	 * Returns this automaton's complement, which is constructed by determinizing this automaton and then flipping its
	 * states' acceptance.
	 * 
	 * @return
	 */
	SFA complement();
	
	/**
	 * Removes (in place) all this' dead states, i.e. states that do not have any
	 * reachable final (accepting) states.
	 * 
	 */
	void removeDeadStates();
	
	/**
	 * Checks whether this automaton's language is empty, i.e., whether this automaton doesn't accept any
	 * word.
	 * 
	 * @return
	 */
	boolean isEmptyLanguage();
	
	/**
	 * Checks whether this automaton's language is a subset of the other automaton's language.
	 * 
	 * @param other
	 * @return
	 */
	boolean isSubsetOf(SFA other);
	
	/**
	 * Checks whether this automaton's language is that of the TRUE* regular expression, i.e., whether this automaton accepts all finite words.
	 * 
	 * @return
	 */
	boolean isTrueStarLanguage();
	
	/**
	 * Checks whether this automaton accepts the empty string.
	 * 
	 * @return
	 */
	boolean acceptsTheEmptyString();
	
	/**
	 * Checks whether this and other automatons are equivalent, i.e., whether they accept the same
	 * language.
	 * 
	 * @param other
	 * @return true iff both automatons accept the same language.
	 */
	boolean isEquivalent(SFA other);
	
	/**
	 * Checks whether this automaton an OFSFA, i.e., enables having exactly one final state.
	 * 
	 * @return
	 */
	boolean hasOneFinalState();
	
	/**
	 * If this automaton enables having exactly one final state, returns the only final state of this automaton.
	 * 
	 * @throws SFAException if this automaton may have any number of final states.
	 * @return the only final state of this automaton
	 * @see #hasOneFinalState()
	 */
	SFAState getFinalState();
	
	/**
	 * If this automaton enables having exactly one final state, sets the only final state of this automaton to the specified state.
	 * 
	 * @param finalState
	 * @throws SFAException if either the specified state is not of the type this automaton enables, or this automaton may have any number of final states.
	 * @see #hasOneFinalState()
	 * @see #getFinalState()
	 */
	void setFinalState(SFAState finalState);
}
