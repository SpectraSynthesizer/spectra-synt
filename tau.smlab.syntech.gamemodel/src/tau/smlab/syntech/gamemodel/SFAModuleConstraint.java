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

package tau.smlab.syntech.gamemodel;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;
import tau.smlab.syntech.sfa.SFA;
import tau.smlab.syntech.sfa.SFAState;

/**
 * 
 * A class that represents an "SFA constraint" of a player module.
 * That is, this class encapsulates the BDD encoding of an associated Symbolic Finite Automaton (SFA) that does not have any reachable epsilon transitions.
 * This encoding consists of the following three BDDs: (1) A BDD that asserts being at the SFA's initial state over unprimed variables;
 * (2) A BDD that encodes the SFA's transition function over unprimed and primed variables; (3) A BDD that encodes the set of final/accepting states over unprimed variables.
 * The underlying (Boolean BDD) variables encode the domain of an integer typed variable. Each integer value in that domain uniquely represents a state in the associated SFA.
 * 
 * @author Or Pistiner
 *
 */
public class SFAModuleConstraint {

	private final PlayerModule m;
	private final String statesVarName;
	private final boolean restrictIniTrans, groupVars;
	private final int traceId;
	
	private ModuleBDDField statesVar;
	private BDD ini, trans, acceptance, //The set of the final (accepting) states of this' SFA 
	transToAcceptance; //The set of all transitions that go into an accepting state 
	
	/**
	 * 
	 * Constructor only for cloning.
	 *
	 */
	private SFAModuleConstraint(PlayerModule m, String statesVarName, boolean restrictIniTrans, boolean groupVars, int traceId,
			ModuleBDDField statesVar, BDD ini, BDD trans, BDD acceptance, BDD transToAcceptance) {
		this.m = m;
		this.statesVarName = statesVarName;
		this.restrictIniTrans = restrictIniTrans;
		this.groupVars = groupVars;
		this.traceId = traceId;
		this.statesVar = statesVar;
		this.ini = ini;
		this.trans = trans;
		this.acceptance = acceptance;
		this.transToAcceptance = transToAcceptance;
	}
	
	/**
	 * 
	 * Constructor only invoked by a {@link Builder}.
	 * 
	 */
	private SFAModuleConstraint(PlayerModule m, SFA sfa, String statesVarName, boolean restrictIniTrans, boolean groupVars, int traceId) {
		this.m = m;
		this.statesVarName = statesVarName;
		this.restrictIniTrans = restrictIniTrans;
		this.groupVars = groupVars;
		this.traceId = traceId;
		setIniTransAcceptance(sfa);
	}
	
	public void free() {
		if(transToAcceptance != null && !transToAcceptance.isFree()) {
			transToAcceptance.free();
		}
		if(acceptance != null && !acceptance.isFree()) {
			acceptance.free();
		}
		if(trans != null && !trans.isFree()) {
			trans.free();
		}
		if(ini != null && !ini.isFree()) {
			ini.free();
		}
	}
	
	/**
	 * Returns the set of all pairs (q,s), where q is a state of this' SFA and s is a character (i.e., an assignment),
	 * such that (q,s) is a transition that goes into a final (accepting) state.
	 * Therefore, s is the last character of a word in the language of this' SFA.
	 *	
	 * @return
	 */
	public BDD getTransToAcceptance() {
		return transToAcceptance;
	}
	
	public BDD getAcceptance() {
		return acceptance;
	}

	public PlayerModule getPlayerModule() {
		return m;
	}
	
	public BDD getTrans() {
		return this.trans;
	}

	public BDD getIni() {
		return this.ini;
	}

	public boolean isRestrictIniTrans() {
		return restrictIniTrans;
	}

	public boolean isGroupVars() {
		return groupVars;
	}
	
	public ModuleBDDField getStatesVar() {
		return statesVar;
	}
	
	/**
	 * Returns the name of the module variable that encodes the states of this' SFA.
	 * 
	 * @return
	 */
	public String getStatesVarName() {
		return this.statesVarName;
	}
	
	public int getTraceId() {
		return traceId;
	}
	
	@Override
	public SFAModuleConstraint clone() {
		 return new SFAModuleConstraint(this.m, this.statesVarName, this.restrictIniTrans,
				 this.groupVars, this.traceId, this.statesVar, this.ini.id(), this.trans.id(), this.acceptance.id(), this.transToAcceptance.id());
	}
	
	@Override
	public String toString() {
		StringBuilder resBuilder = new StringBuilder();
		resBuilder.append("SFAModuleConstraint encoded over the variable ").append(this.statesVarName).append(System.lineSeparator());
		resBuilder.append("Initial state: ").append(Env.toNiceString(this.ini)).append(System.lineSeparator());
		resBuilder.append("Transitions:").append(System.lineSeparator()).append(Env.toNiceString(this.trans)).append(System.lineSeparator());
		resBuilder.append("Final states: ").append(Env.toNiceString(this.acceptance));
		return resBuilder.toString();
	}

	private void setIniTransAcceptance(SFA sfa) {
		try {
			if(this.groupVars) {
				Env.disableReorder();
			}
			//Try to add a fresh aux integer variable that encodes the states of this' SFA 
			this.statesVar = this.m.addVar(this.statesVarName, 0, sfa.numStates()-1, true, this.restrictIniTrans);
			this.statesVar.setTraceId(this.traceId);

			if(this.groupVars) {
				BDDVarSet statesVarDomVars = this.statesVar.getDomain().set().union(this.statesVar.getOtherDomain().set());
				Env.TRUE().getFactory().addVarBlock(statesVarDomVars, true);
				statesVarDomVars.free();
				Env.enableReorder();
			}
		}
		catch (Exception e) {
			throw new RuntimeException("Could not add aux variable " + this.statesVarName + " : " + e.getMessage());
		}
		
		//Initial: ('new aux states variable = 0')
		Map<SFAState, Integer> statesToIdMapping = new HashMap<>();
		Set<Integer> finalStatesIds = new HashSet<>();
		statesToIdMapping.put(sfa.getIni(), 0);
		this.ini = Env.getBDDValue(this.statesVarName, 0).id();
		if(sfa.getIni().isAccepting()) {
			finalStatesIds.add(0);
		}

		//Perform a BFS search on this' SFA and encode all of its transitions
		int nextId = 1;
		Queue<SFAState> worklist = new LinkedList<>();
		worklist.add(sfa.getIni());
		BDD trans = Env.FALSE(), currTransBdd, currStateBdd;
		SFAState currState;
		while (!worklist.isEmpty()) {
			currState = worklist.remove();
			currStateBdd = Env.getBDDValue(this.statesVarName, statesToIdMapping.get(currState));
			for (SFAState succState : currState.getSucc().keySet()) {
				if (!statesToIdMapping.containsKey(succState)) {
					statesToIdMapping.put(succState, nextId);
					if(succState.isAccepting()) {
						finalStatesIds.add(nextId);
					}
					++nextId;
					worklist.add(succState);
				}
				currTransBdd = (currStateBdd.and(currState.getSucc().get(succState))).
						andWith(Env.getBDDValue(this.statesVarName+"'", statesToIdMapping.get(succState)).id());
				trans.orWith(currTransBdd);
			}
		}
		this.trans = trans;
		this.setAcceptance(finalStatesIds);
	}
	
	private void setAcceptance(Set<Integer> finalStatesIds) {
		BDD accepting = Env.FALSE();
		for(int finalStateId : finalStatesIds) {
			accepting.orWith(Env.getBDDValue(this.statesVarName, finalStateId).id());
		}
		this.acceptance = accepting;
		System.out.println("IN ACP: " + this.acceptance);
		
		BDD primedFinalStates = Env.prime(this.acceptance);
		this.transToAcceptance = this.trans.relprod(primedFinalStates, this.statesVar.getOtherDomain().set());
		primedFinalStates.free();
	}
	
	/**
	 * 
	 * Make use of the {@link Builder} to construct a new {@link SFAModuleConstraint}.
	 *
	 */
	public static class Builder {
		
		private static int VAR_NAME_IDX = 0;

		private static final String VAR_NAME_PREFIX = "sfa_states_";
		
		private PlayerModule m;
		private SFA sfa;
		private String varNamePrefix;
		private boolean restrictIniTrans, groupVars;
		private int traceId;
		
		/**
		 * Constructs a new {@link Builder} with a default variable name prefix.
		 */
		public Builder() {
			this.varNamePrefix = VAR_NAME_PREFIX;
			this.restrictIniTrans = true;
			this.groupVars = false;
		}
		
		/**
		 * Sets the SFA that the resulting {@link SFAModuleConstraint} will encode.
		 * 
		 * @param sfa assumed to be an SFA without epsilon transitions reachable from the initial state
		 * @return
		 */
		public Builder sfa(SFA sfa) {
			this.sfa = sfa;
			return this;
		}
		
		/**
		 * Sets whether to create a fixed variable block for reordering, consisting of all the newly created primed and unprimed variables.
		 * By default, set to {@code false}.
		 * 
		 * @param groupVars
		 * @return
		 */
		public Builder groupVars(boolean groupVars) {
			this.groupVars = groupVars;
			return this;
		}
		
		/**
		 * Sets the prefix of the variable's name that will be created to encode the states of the SFA associated
		 * with the resulting {@link SFAModuleConstraint}. If a prefix is not set, a default one will be used.
		 * 
		 * @param varNamePrefix the prefix of the variable's name
		 * @return
		 */
		public Builder varNamePrefix(String varNamePrefix) {
			this.varNamePrefix = varNamePrefix;
			return this;
		}
		
		/**
		 * Sets whether to restrict the initial states and transitions of the player module with the domain of variable that
		 * encodes the states of the SFA associated with the resulting {@link SFAModuleConstraint}. By default, set to {@code true}.
		 * 
		 * @param restrictIniTrans
		 * @return
		 */
		public Builder restrictIniTrans(boolean restrictIniTrans) {
			this.restrictIniTrans = restrictIniTrans;
			return this;
		}
		
		/**
		 * 
		 * The states of the SFA associated with the resulting {@link SFAModuleConstraint} will be encoded over a new integer variable
	     * controlled by the specified player module {@code m}.
		 * 
		 * @param m the player module
		 * @return
		 */
		public Builder playerModule(PlayerModule m) {
			this.m = m;
			return this;
		}
		
		public Builder traceId(int traceId) {
			this.traceId = traceId;
			return this;
		}
		
		/**
		 * 
		 * Constructs a new {@link SFAModuleConstraint} from the values which have been supplied to this {@link Builder}.
		 * 
		 * @return
		 */
		public SFAModuleConstraint build() {
			SFAModuleConstraint sfaConstraint = new SFAModuleConstraint(this.m, this.sfa, this.varNamePrefix + VAR_NAME_IDX,
					this.restrictIniTrans, this.groupVars, traceId);
			++VAR_NAME_IDX;
			return sfaConstraint;
		}
		
	}

}