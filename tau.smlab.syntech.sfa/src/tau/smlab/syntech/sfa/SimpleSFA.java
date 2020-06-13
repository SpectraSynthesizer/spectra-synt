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
	protected SimpleSFA newSfaInstance() {
		return new SimpleSFA();
	}

	@Override
	protected SimpleSFA newSfaInstance(SimpleSFAState ini) {
		return new SimpleSFA(ini);
	}

	@Override
	public SimpleSFAState newSfaState(boolean isAccepting) {
		return new SimpleSFAState(isAccepting);
	}
}