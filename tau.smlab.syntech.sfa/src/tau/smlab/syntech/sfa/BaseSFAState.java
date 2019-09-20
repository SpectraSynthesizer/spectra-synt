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

import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import net.sf.javabdd.BDD;


/**
 * 
 * An abstract class that represents a state of an SFA.
 *
 */
public abstract class BaseSFAState<T extends BaseSFAState<T>> implements SFAState {
	
	/**
	 * Outgoing transitions of this state. A transition is a map entry {@code <key,value>} where {@code key} is a successor state and {@code value}
	 * is the BDD that encodes its guard (condition). 
	 */
	protected Map<T, BDD> succ = new LinkedHashMap<>();

	/**
	 * Marks whether this state is an accepting state.
	 */
	protected boolean isAccepting;
	
	/**
	 * Constructs a new state of an SFA automaton.
	 */
	protected BaseSFAState(boolean isAccepting) {
		this.isAccepting = isAccepting;
	}
	
	@Override
	public Set<T> getEpsSucc() {
		return new LinkedHashSet<T>();
	}
	
	@Override
	public boolean isAccepting() {
		return this.isAccepting;
	}
	
	@Override
	public void setAcceptance(boolean isAccepting) {
		this.isAccepting = isAccepting;
	}

	@Override
	public void flipAcceptance() {
		this.isAccepting = !isAccepting;
	}

	@Override
	public Map<T, BDD> getSucc() {
		return this.succ;
	}

	@Override
	public void removeTrans(SFAState tgt) {
		if(this.succ.containsKey(tgt)) {
			this.succ.remove(tgt).free();
		}
	}
	
	@Override
	public void free() {
		for(BDD guard : this.succ.values()) {
			if(!guard.isFree()) {
				guard.free();
			}
		}
	}
	
	@Override
	public abstract T cloneWithoutSucc();

	@Override
	public Set<T> getSuccessors() {
		return new LinkedHashSet<T>(succ.keySet());
	}

	@SuppressWarnings("unchecked")
	@Override
	public void addTrans(BDD guard, SFAState tgt) {
		if(guard == null) {
			throw new SFAStateException("Cannot add an outgoing transition with a null guard.");
		}
		
		if(tgt == null || !this.getSuccessorsType().equals(tgt.getClass())) {
			//we only allow successor states that have the same type as that of this state
			throw new SFAStateException(this.getClass().getSimpleName() + " can only have successor states which are instances of " + this.getSuccessorsType().getSimpleName() + ".");
		}
		
		// don't add an unnecessary FALSE transition
		if (guard.isZero()) {
			guard.free();
			return;
		}
		
		BDD currGuard = succ.get(tgt);
		if (currGuard == null) {
			succ.put((T) tgt, guard.id());
		} else {
			currGuard.orWith(guard.id());
		}
		guard.free();
	}
	
	/**
	 * Returns the type information of the successor states. All successor states should have the same
	 * type as that of this state.
	 * 
	 * @return
	 */
	protected abstract Class<T> getSuccessorsType(); 
}