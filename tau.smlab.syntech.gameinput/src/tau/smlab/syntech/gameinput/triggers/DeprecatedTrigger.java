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

package tau.smlab.syntech.gameinput.triggers;

import java.util.List;

import dk.brics.automaton.Automaton;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;

	/** The trigger class
	 * @param traceId
	 * @param init - The trigger's init regular expression
	 * @param effect - The trigger's effect regular expression
	 * @param initAutomaton - A bricks automaton that matches the regular expression: init
	 * @param effectAutomaton - A bricks automaton that matches the regular expression: effect
	 * @param symbols -  a list of all variables that appear in the automatons, init and effect
	 * @param isBoolean - specifies which variables in 'symbols' are boolean
	 * @param from - specifies for each variable the minimal value of its range. 
	 * <br> For a variable var, this value is stored in from.symbols.getindexof(var).
	 * @param to - specifies for each variable the maximal value of its range. 
	 * <br> For a variable var, this value is stored in to.symbols.getindexof(var).<p>
	 * Hence, if var is stored at index 'i' in the list symbols, var get values in the range from[i]...to[i].
	 */

@Deprecated
public class DeprecatedTrigger {

	private SpecRegExp init;
	private Automaton initAutomaton;
	private SpecRegExp effect;
//	private Map<Spec, SpecSymbols> specSymbolsMap; 
	private Automaton effectAutomaton;
	private int traceId;
	private List<String> symbols;
	private List<Boolean> isBoolean;
	private List<Integer> from;
	private List<Integer> to;
	
	
//	private String initString;
//	private String effectString;
//	private Map<VarDecl, String> varSymbolMap;
	
	
	

//	public Trigger(SpecRegExp init, SpecRegExp effect, int traceId) {
//		this.init = init;
//		this.effect = effect;
//		this.traceId = traceId;
//	}
	
// A constructor. Receives the init and effect regular expressions as SpecRegExps, 
//	and a list that includes all (names of the) boolean variables that appear in the regular expressions
	
	public DeprecatedTrigger(SpecRegExp init, SpecRegExp effect, List<String> symbols, List<Boolean> isBoolean,
			List<Integer> from , List<Integer> to, int traceId ) {
		this.init = init;
		this.effect = effect;
		this.symbols = symbols;
		this.isBoolean = isBoolean;
		this.from = from;
		this.to = to;
		this.traceId = traceId;
	}
	
// to create the trigger with Strings that match brics regular expressions 
	
//	public Trigger(String init, String effect, Map<VarDecl, String> varSymolMap, int traceId) {
//		this.initString = init;
//		this.effectString = effect;
//		this.traceId = traceId;
//		this.varSymbolMap = varSymolMap;
//	}
	
	
	 /**
   * @param traceId
   * @param init
   * @param effect
   */
//   public Trigger(SpecWrapper init, SpecWrapper effect, int traceId) {
//     this.init = init.getRegExp();
//     this.effect = effect.getRegExp();
//     this.traceId = traceId;
//   }
	
//	public Map<Spec, SpecSymbols> getSpecSymbolsMap() {
//		return specSymbolsMap;
//	}

	public void setTraceId(int traceId) {
	    this.traceId = traceId;
	 }
	
//	public void setSpecSymbolsMap(Map<Spec, SpecSymbols> specSymbolsMap) {
//		this.specSymbolsMap = specSymbolsMap;
//	}

	public SpecRegExp getInitiator() {
		return this.init;
	}
	
	public Automaton getInitAutomaton() {
		return initAutomaton;
	}

	public void setInitAutomaton(Automaton initAutomaton) {
		this.initAutomaton = initAutomaton;
	}

	public Automaton getEffectAutomaton() {
		return effectAutomaton;
	}

	public void setEffectAutomaton(Automaton effectAutomaton) {
		this.effectAutomaton = effectAutomaton;
	}

	public SpecRegExp getEffect() {
		return this.effect;
	}
	
	public int getTraceId() {
		return this.traceId;
	}
	
	public boolean hasInitEffectAutomatons() {
	  return this.effectAutomaton != null &&
	      this.initAutomaton != null;
	}
	
	public List<String> getSymbols() {
		return this.symbols;
	}
	
	public List<Boolean> getBooleans() {
		return this.isBoolean;
	}
	
	public List<Integer> getFrom() {
		return this.from;
	}
	
	public List<Integer> getTo() {
		return this.to;
	}
}