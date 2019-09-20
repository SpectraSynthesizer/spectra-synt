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
import java.util.Random;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.ModuleException;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.gamemodel.PlayerModule.TransFuncType;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.ModuleVariableException;

public class SFAConstraint {

	private static final String VAR_NAME_PREFIX = "sfa_states_";

	private final SFA sfa;
	private final String varName;
	private final int varNameNonce;
	private final PlayerModule module;
	
	private boolean constraintCreated, constraintAddedToModule;
	private BDD constraint;

	/**
	 * @param module the player module
	 * @param sfa assumed to be an SFA without epsilon transitions reachable from the initial state
	 */
	public SFAConstraint(PlayerModule module, SFA sfa) {
		this(module, sfa, VAR_NAME_PREFIX);
	}

	/**
	 * 
	 * @param module the player module
	 * @param sfa assumed to be an SFA without epsilon transitions reachable from the initial state
	 * @param varNamePrefix the prefix of the variable name that will be added to the specified player module.
	 * The name of the new module variable is {@code varNamePrefix} + random int.
	 */
	public SFAConstraint(PlayerModule module, SFA sfa, String varNamePrefix) {
		this.module = module;
		this.sfa = sfa;
		this.varNameNonce = new Random().nextInt();
		this.varName = varNamePrefix + this.varNameNonce;
	}

	/**
	 * Creates ...
	 */
	public void createConstraint() {
		if(constraintCreated) {
			return;
		}
		try {
			module.addVar(varName, 0, sfa.numStates()-1, true);
		} catch (ModuleException | ModuleVariableException e) {
			throw new SFA.SFAException("Could not create SFA aux variable " + varName + " : " + e.getMessage());
		}

		//Perform a BFS search on this' SFA and encode all of its states and transitions
		Map<SFAState, Integer> statesToIdMapping = new HashMap<>();
		statesToIdMapping.put(sfa.getIni(), 0);
		int nextId = 1;
		Queue<SFAState> worklist = new LinkedList<>();
		worklist.add(sfa.getIni());
		BDD result = Env.FALSE(), currTransBdd, currStateBdd;
		SFAState currState;
		while (!worklist.isEmpty()) {
			currState = worklist.remove();
			currStateBdd = Env.getBDDValue(varName, statesToIdMapping.get(currState));
			for (SFAState succState : currState.getSucc().keySet()) {
				if (!statesToIdMapping.containsKey(succState)) {
					statesToIdMapping.put(succState, nextId);
					nextId++;
					worklist.add(succState);
				}		
				currTransBdd = (currStateBdd.and(currState.getSucc().get(succState))).andWith(Env.getBDDValue(varName+"'", statesToIdMapping.get(succState)).id());
				result.orWith(currTransBdd);
			}
		}
		constraint = result;
		constraintCreated = true;
	}
	
	public void addConstraintToModule() {
		if(constraintAddedToModule) {
			return;
		}
		createConstraint();
		addConstraintToModuleTrans();
		constraintAddedToModule = true;
	}

	public BDD getConstraint() {
		return constraint;
	}

	public SFA getSfa() {
		return sfa;
	}

	public PlayerModule getModule() {
		return module;
	}

	/**
	 * Returns the name of the module variable that encodes the states of this' sfa.
	 * @return
	 */
	public String getVarName() {
		return this.varName;
	}
	
	/*
	 * 
	 * Private methods.
	 * 
	 * 
	 */
	private void addConstraintToModuleTrans() {
		module.conjunctTrans(constraint.id());
		if (module.getTransFuncType() == TransFuncType.DECOMPOSED_FUNC) {
			module.addToTransList(constraint);
		} else if (module.getTransFuncType() == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
			module.addToPartTransList(constraint);
		}
	}
}