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

package tau.smlab.syntech.gameinputtrans.translator;

import java.util.List;

import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Trigger;
import tau.smlab.syntech.gameinput.triggers.AutomatonTriggerHelper;


public class TriggerTranslator implements Translator {

	GameInput input;

	/** 
	 * this global setting controls how Spectra trigger expressions
	 * are handled.
	 * 
	 * if false (Default) then trigger expressions are
	 * evaluated in the BDD generator (when a game input is translated
	 * to a game model). this is the more efficient implementation.
	 * 
	 * if true, then trigger expressions are translated to simple
	 * game input constraints (naive implementation).
	 * 
	 * we keep both implementation for research purposes.	
	 */
	public static boolean translateTriggersToConstraints = false;


	@Override
	public void translate(GameInput input) {

		this.input = input;

		if (!translateTriggersToConstraints) {
		  addInitEffectAutomatons(input.getSys().getTriggers()); //sys player
		  addInitEffectAutomatons(input.getEnv().getTriggers()); //env player	
		  return;
		}

		
		
		
		
		
		// *****************  when 'translateTriggersToConstraints' is true  *********
		
		
		
		
		
		/*// sys 
		List<Trigger> sysTriggers = input.getSys().getTriggers();
		for (Trigger trigger : sysTriggers) {
			translateTrigger(input, input.getSys(), trigger);
		}

		// env
		List<Trigger> envTriggers = input.getEnv().getTriggers();
		for (Trigger trigger : envTriggers) {
			translateTrigger(input, input.getEnv(), trigger);
		}*/
	}

	/**
	 * 
	 * generate an alpha-bet for the Brics automata -
	 * by considering each step in the initiator and effect regexps
	 * as an (atomic) proposition, and taking the power set of all
	 * such atomic propositions.
	 */
	/*public Map<Spec, SpecSymbols> getSpecSymbolsMap(Trigger trigger) {

		List<Spec> specs = getAllSpecs(trigger);

		return AutomatonTriggerHelper.translateSpecs(specs);
	}
*/
	/*###############################################################################
	 * ############################## private methods ###############################*/
	


	private void addInitEffectAutomatons(List<Trigger> triggers) {
		List<String> symbols; 
		List<Integer> fromList;
		List<Integer> toList;
		for (Trigger trigger : triggers) {
			
			symbols = trigger.getSymbols();
			fromList = trigger.getFrom();
			toList = trigger.getTo();

			trigger.setInitAutomaton(AutomatonTriggerHelper.getBricsAutomaton(trigger.getInitiator(),
					symbols,fromList,toList));
			trigger.setEffectAutomaton(AutomatonTriggerHelper.getBricsAutomaton(trigger.getEffect(),
					symbols,fromList,toList));
		}
	}

	
	/**
	 * Creates an array of symbols that match the variables and their negation.   
	 * @param binaryWords - the array of symbols. 
	 * For i<size: binaryWords[i]s the symbols that match the variable in symbols[i] of the trigger, 
	 * and binaryWords[i+size] matchs the negation of this symbol.
	 * binaryWords[2size] matches the 'T' symbol (all non-0 symbols)
	 * binaryWords[2size+1] matches the 'F' symbol (0); 
	 * @param size
	 *//*
	
	
	private void CreateBinaryWords(String[] binaryWords, int size) {
		
		for (int location =0 ; location < binaryWords.length; location ++) {
			binaryWords[location]="(";
		}
		
		double maxString = Math.pow(2,size);
		for (int i=0; i< maxString; i++) { // i  is a candidate integer
			String binaryValue= Integer.toBinaryString(i);		
			char c = (char) (i + 48); // A char representing i. We add 48 for readability. This way 0 matches the symbol '0' and so on... 
			
			int toComplete= size - binaryValue.length(); // if the binary value is shorter than size, we add 0's
	
			for (int k=0; k<toComplete; k++) {
				binaryValue = "0" + binaryValue;}
			
			// for each symbol, we check if 'i' matches the symbol, and if so, add it to the sequence of matching binary words
			// otherwise, add it the location that matches the negation of the symbol
			for (int location = 0 ; location < size ; location ++) {
				
				if (binaryValue.charAt(size-location-1) == '1') { // check if we should add the binary value of i to binaryWords[location]
				
					if (binaryWords[location] != "(") {
						binaryWords[location] =   binaryWords[location] +  "|"  + c ;
					}
					else {
						binaryWords[location] =  binaryWords[location]  +  c ;
					}
				}
				
				else { //otherwise, we add the binary word to location+size - the cell that matches the negation of the sumbol
					if (binaryWords[location+size] != "(") {
						binaryWords[location+size] =   binaryWords[location+size] +  "|"  + c ;
					}
					else {
						binaryWords[location+size] =  binaryWords[location+size]  +  c ;
					}
				}
			}
			// If the string is different form 0's, it matches the 'T' symbol. Otherwise, it matches 'F'
			if (i!= 0) {
				if (binaryWords[2*size] != "(") {
					binaryWords[2*size] =  binaryWords[2*size] + "|" +  c ;
				}
				else {
					binaryWords[2*size] =  binaryWords[2*size]  +  c ;
				}	
			}
			else {
				binaryWords[2*size+1] = binaryWords[2*size+1] +  c ;
			}
		}
		
		for (int location = 0 ; location < 2*size+2 ; location ++) {
			binaryWords[location] =  binaryWords[location]  + ")";
		}
		
	}*/

	// These are used when 'translateTriggersToConstraints' is true
	
	
	
/*	private void translateTrigger(GameInput input, Player player, Trigger trigger) {

		Map<Spec, SpecSymbols> specSymbolsMap = getSpecSymbolsMap(trigger);

		if (isAlwaysAccepting(trigger.getEffect())) { // Trigger is always satisfied and therefore no need to translate it.
			return; 
		}

		Automaton initAutomaton = AutomatonTriggerHelper.getBricsAutomaton(trigger.getInitiator(), specSymbolsMap);
		Automaton effectAutomaton = AutomatonTriggerHelper.getBricsAutomaton(trigger.getEffect(), specSymbolsMap);

		// determinize and minimize the initial automata
		initAutomaton.determinize();
		initAutomaton.minimize();
		effectAutomaton.determinize();
		effectAutomaton.minimize();

		Set<String> alphabet = AutomatonTriggerHelper.createAlphabet(specSymbolsMap);

		Automaton triggerAutomaton = AutomatonTriggerHelper.triggerConcatenate(initAutomaton, effectAutomaton, alphabet);

		if (triggerAutomaton == null) return;
		if (!triggerAutomaton.isDeterministic()) {
			throw new RuntimeException("Trigger automaton is not deterministic");
		}

		// Create aux variable that represents the trigger automaton (the states are its values)
		String varName = "trigger_" + trigger.getTraceId() + "_states";
		Variable triggerVar = new Variable(varName, new TypeDef(0, triggerAutomaton.getNumberOfStates()));
		VariableReference triggerVarRef = new VariableReference(triggerVar);
		VariableReference triggerPrimeVarRef = new VariableReference(triggerVar);
		triggerPrimeVarRef.setReferenceName(triggerVarRef.getReferenceName() + "'");
		input.getAux().addVar(triggerVar);

		// Translate automaton to an aux constraint	  
		List<State> triggerStates = new ArrayList<State>(triggerAutomaton.getStates());
		List<SpecExp> triggerInStateSpecs = new ArrayList<SpecExp>(); // SpecExp i is: triggerVarRef == i (trigger in state i)
		List<SpecExp> triggerPrimeInStateSpecs = new ArrayList<SpecExp>(); // SpecExp i is: triggerVarRef' == i (trigger in state i on next step)
		for (int i = 0; i < triggerStates.size(); i++) {
			SpecExp triggerInState = new SpecExp(Operator.EQUALS, triggerVarRef, new PrimitiveValue(i));
			triggerInStateSpecs.add(i, triggerInState);
			SpecExp triggerPrimeInState = new SpecExp(Operator.EQUALS, triggerPrimeVarRef, new PrimitiveValue(i));
			triggerPrimeInStateSpecs.add(i, triggerPrimeInState);
		}

		List<SpecExp> transitionsSpecs = new ArrayList<SpecExp>(); 
		for (State s1 : triggerStates) {
			for (State s2 : triggerStates) {
				Set<String> allTransitions = AutomatonTriggerHelper.getAllTransitions(s1, s2);
				if (allTransitions.size() > 0) {
					Spec transitionSpec = AutomatonTriggerHelper.getSpecFromSymbols(specSymbolsMap, allTransitions);
					SpecExp condition = new SpecExp(Operator.AND, triggerInStateSpecs.get(triggerStates.indexOf(s1)), transitionSpec);
					SpecExp transitionSpecExp = new SpecExp(Operator.AND, condition, triggerPrimeInStateSpecs.get(triggerStates.indexOf(s2)));
					transitionsSpecs.add(transitionSpecExp);  
				}
			}
		}

		SpecExp accumulatedlOrAutomaton = null;
		if (transitionsSpecs.size() < 1) {
			throw new RuntimeException("Empty trigger automaton");
		}
		for (int i = 0; i < transitionsSpecs.size(); i++) {
			if (i == 0) {
				accumulatedlOrAutomaton = transitionsSpecs.get(0);
			} else {
				accumulatedlOrAutomaton = new SpecExp(Operator.OR, accumulatedlOrAutomaton, transitionsSpecs.get(i));
			}
		}
		SpecExp triggerAutomatonSpec = accumulatedlOrAutomaton;
		String automatonConstName = varName + "_transitions";
		Constraint triggerAutomatonConstraint = new Constraint(Kind.SAFETY, triggerAutomatonSpec, automatonConstName, trigger.getTraceId());
		input.getAux().addConstraint(triggerAutomatonConstraint);

		SpecExp initialStateSpec = triggerInStateSpecs.get(triggerStates.indexOf(triggerAutomaton.getInitialState()));
		String initStateConstName = varName + "_initState";
		Constraint initialStateConstraint = new Constraint(Kind.INI, initialStateSpec, initStateConstName, trigger.getTraceId());
		input.getAux().addConstraint(initialStateConstraint);

		// Translate trigger condition to a player constraint
		List<State> initStates = new ArrayList<State>(); // States from initiator automaton
		for (State state : triggerAutomaton.getStates()) {
			if (state.isAccept()) { // implementation sign for an "initiator state"
				initStates.add(state);
			}
		}
		SpecExp accumulatedOrTrigger = null;
		if (triggerInStateSpecs.size() < 1) {
			throw new RuntimeException("Empty trigger automaton");
		}
		for (int i = 0; i < initStates.size(); i++) {
			State currState = initStates.get(i);
			SpecExp triggerInStateSpec = triggerInStateSpecs.get(triggerStates.indexOf(currState));
			if (i == 0) {
				accumulatedOrTrigger = triggerInStateSpec;
			} else {
				accumulatedOrTrigger = new SpecExp(Operator.OR, accumulatedOrTrigger, triggerInStateSpec);
			}
		}  

		String conditionConstName = varName + "_acceptance";
		Constraint triggerConditionConstraint = new Constraint(Kind.JUSTICE, accumulatedOrTrigger, conditionConstName, trigger.getTraceId());
		player.addConstraint(triggerConditionConstraint);
	}
*/
	/*private boolean mustOccurAtLeastOnce(SpecRegExpStep step) {

		switch (step.getQuantifier()) {

		case ZERO_OR_MORE :
		case ZERO_OR_ONCE :
			return false;

		case EXACT_REPETITION :
		case AT_LEAST :
		case RANGE :
			return (step.getFrom() > 0);  		

		default :
			return true;
		}
	}

	private boolean isAlwaysAccepting(SpecRegExp regExpGi) {
		for (SpecRegExpStep step : regExpGi.getSteps()) {
			if (mustOccurAtLeastOnce(step)) {

				return false; // May be a false negative mistake}
				// TODO: Check if this is correct
			}
		}
		return true; // if all steps are kleened (and could happen 0 times) 
	}

	private List<Spec> getAllSpecs(Trigger trigger) {
		Set<Spec> allSpecs = new LinkedHashSet<Spec>();
		List<SpecRegExpStep> allSteps = new ArrayList<SpecRegExpStep>(trigger.getInitiator().getSteps());
		allSteps.addAll(trigger.getEffect().getSteps());
		for (SpecRegExpStep step : allSteps) {
			allSpecs.add(step.getSpec());
		}
		List<Spec> res = new ArrayList<Spec>();
		res.addAll(allSpecs);
		return res;
	}*/

}
