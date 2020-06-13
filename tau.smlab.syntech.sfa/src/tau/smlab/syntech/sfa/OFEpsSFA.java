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
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.sfa.PowerSetIterator.PowerSetIteratorType;

/**
 * 
 * An Epsilon-SFA that has exactly One Final state, which has no outgoing transitions.
 * 
 * @author Or Pistiner
 *
 */
public class OFEpsSFA extends EpsSFA {
	
	private EpsSFAState finalState; //The unique, only final state of this automaton
	
	protected OFEpsSFA() {
		this(null, null, PowerSetIteratorType.EFFICIENT);
	}

	protected OFEpsSFA(SFAState ini) {
		this(ini, null, PowerSetIteratorType.EFFICIENT);
	}
	
	protected OFEpsSFA(SFAState ini, SFAState finalState) {
		this(ini, finalState, PowerSetIteratorType.EFFICIENT);
	}

	protected OFEpsSFA(PowerSetIteratorType psIterType) {
		this(null, null, psIterType);
	}

	protected OFEpsSFA(SFAState ini, PowerSetIteratorType psIterType) {
		this(ini, null, psIterType);
	}
	
	protected OFEpsSFA(SFAState ini, SFAState finalState, PowerSetIteratorType psIterType) {
		super(ini, psIterType);
		if(finalState != null) {
			this.setFinalState(finalState);
		}
	}
	
	@Override
	public EpsSFAState getFinalState() {
		return this.finalState;
	}
	
	@Override
	public void setFinalState(SFAState finalState) {
		if(!(finalState instanceof EpsSFAState)) {
			throw new SFAException("The specified state has an invalid type. The final state must be an instance of EpsSFAState.");
		}
		if(!finalState.isAccepting()) {
			finalState.flipAcceptance();
		}
		this.finalState = (EpsSFAState) finalState;
	}
	
	@Override
	public OFEpsSFA copy() {
		OFEpsSFA copy = new OFEpsSFA();
		Map<EpsSFAState, EpsSFAState> copyStateMappingMap = new HashMap<>();

		copy.setIni(this.getIni().cloneWithoutSucc());
		copyStateMappingMap.put(this.getIni(), copy.getIni());
		
		Queue<EpsSFAState> worklist = new LinkedList<>();
		worklist.add(this.getIni());
		
		while (!worklist.isEmpty()) {
			EpsSFAState s = worklist.remove();
			
			if(s == this.finalState) {
				//the only accepting state is reachable from the initial state
				copy.finalState = copyStateMappingMap.get(s);
			}
			
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
		
		if(!copyStateMappingMap.containsKey(this.finalState)) {
			//the final state is not reachable from the initial state
			copy.finalState = this.finalState.cloneWithoutSucc();
		}
		return copy;
	}
}
