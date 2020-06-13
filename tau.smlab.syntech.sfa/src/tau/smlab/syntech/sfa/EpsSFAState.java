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

import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.Queue;
import java.util.Set;

/**
 * A class that represents a state of an Epsilon-SFA, an SFA that may have states with outgoing epsilon transitions (moves).
 * 
 * @author Or Pistiner
 *
 */
public class EpsSFAState extends BaseSFAState<EpsSFAState> {

	/**
	 * Epsilon successors. Each such successor is reachable from this state by an epsilon move.
	 */
	private Set<EpsSFAState> epsSucc;

	/**
	 * Constructs a new state of an SFA with outgoing epsilon transitions (moves).
	 */
	protected EpsSFAState(boolean isAccepting) {
		super(isAccepting);
	}

	@Override
	public boolean hasEpsSuccessors() {
		return this.epsSucc != null;
	}

	@Override
	public void removeTrans(SFAState tgt) {
		super.removeTrans(tgt);

		if(this.isEpsSuccessor(tgt)) {
			//Handle the case of an epsilon transition
			this.epsSucc.remove(tgt);
			if(epsSucc.isEmpty()) { //Mark that no epsilon successors are left
				this.epsSucc = null;
			}
		}
	}

	@Override
	public boolean isEpsSuccessor(SFAState succ) {
		return this.hasEpsSuccessors() && this.epsSucc.contains(succ);
	}

	@Override
	public EpsSFAState cloneWithoutSucc() {
		return new EpsSFAState(this.isAccepting());
	}

	@Override
	public Set<EpsSFAState> getEpsSucc() {
		if (!this.hasEpsSuccessors()) {
			return super.getEpsSucc();
		}
		return this.epsSucc;
	}

	@Override
	public boolean enablesEpsTrans() {
		return true;
	}

	@Override
	public void addEpsTrans(SFAState tgt) {
		if(!(tgt instanceof EpsSFAState)) {
			throw new SFAStateException("EpsSFAState can only have successor states that are instances of EpsSFAState");
		}

		if(!this.hasEpsSuccessors()) {
			this.epsSucc = new LinkedHashSet<EpsSFAState>();
		}
		this.epsSucc.add((EpsSFAState)tgt);
	}

	/**
	 * Returns all direct successor states of this state, including those reachable by epsilon moves.
	 * 
	 * @return
	 */
	@Override
	public Set<EpsSFAState> getSuccessors() {
		Set<EpsSFAState> allSuccessors;
		if(this.hasEpsSuccessors()) {
			allSuccessors = new LinkedHashSet<EpsSFAState>(this.epsSucc);
		}
		else {
			allSuccessors = new LinkedHashSet<EpsSFAState>();
		}
		allSuccessors.addAll(super.getSuccessors());
		return allSuccessors;
	}

	/**
	 * Computes the epsilon closure of this state.
	 * The epsilon closure is the set of all states reachable from this state by an epsilon labeled path.
	 * 
	 * @return the epsilon closure of this state
	 */
	public Set<EpsSFAState> getEpsClosure() {
		Queue<EpsSFAState> worklist = new LinkedList<>();
		Set<EpsSFAState> epsClosure = new LinkedHashSet<>();
		epsClosure.add(this);
		if(this.hasEpsSuccessors()) {
			worklist.add(this);		
		}
		while (!worklist.isEmpty()) {
			EpsSFAState s = worklist.remove();
			for (EpsSFAState epsSucc : s.getEpsSucc()) { //Add epsilon successors not checked yet
				if (!epsClosure.contains(epsSucc)) {
					epsClosure.add(epsSucc);
					if(epsSucc.hasEpsSuccessors()) {
						worklist.add(epsSucc);							
					}
				}
			}
		}
		return epsClosure;
	}

	@Override
	protected Class<EpsSFAState> getSuccessorsType() {
		return EpsSFAState.class;
	}
}