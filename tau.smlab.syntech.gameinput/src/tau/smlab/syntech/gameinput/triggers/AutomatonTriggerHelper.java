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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import dk.brics.automaton.Automaton;
import dk.brics.automaton.RegExp;
import dk.brics.automaton.State;
import dk.brics.automaton.Transition;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;

/**
 * Provides helper functions for finite brics automaton trigger construction related operations.
 * 
 * @author Gal Amram
 *
 */
@Deprecated
public class AutomatonTriggerHelper {


	
	/**
	 * To create a brics automaton out of spectra-regexp w.r.t. a list of symbols, specsymbols.
	 * @param modelRegExp - a regular expression, represented as a "creation tree".
	 * @param specSymbols - a list of variable names
	 * @param fromList - specifies for each variable the minimal value it can store
	 * @param toList - specifies for each variable the maximal value it can store
	 * @return
	 */

	public static Automaton getBricsAutomaton(SpecRegExp modelRegExp, List<String> specSymbols, 
			List<Integer> fromList, List<Integer> toList) {

		String regexpString = translateRegExp2(modelRegExp, specSymbols, fromList, toList); // Creates a String that matches the regexp
		RegExp regExp = new RegExp(regexpString);

		Automaton extended =  regExp.toAutomaton(); // This automaton matches the regular expression, but it might include
											// too many symbols due to complementations. Thus, we intersect with an automaton that accept Sigma*
		String allSymbols = createAllSymbols(fromList,toList); // Creates a regexp whose language is Sigma* 
		RegExp sigmaStar = new RegExp(allSymbols);
		Automaton sigmaStarAutomaton = sigmaStar.toAutomaton();
		Automaton reduced = sigmaStarAutomaton.intersection(extended);
		return reduced;
	}

	
	/**
	 *  Creates a string to be translate to brics regexp that includes all possible symbols in the alphabet
	 * @param size - the number of symbols
	 * @return
	 */

	private static String createAllSymbols(List<Integer> fromList, List<Integer> toList) {
		
		double numOfSeq =1;
		for (int i=0; i<fromList.size(); i++)
			numOfSeq=numOfSeq*(toList.get(i)+1-fromList.get(i));
		
		String ans = "";
		for (double i=0; i< numOfSeq; i ++) {
			ans = ans + ((char) (i+192))  + "|";}
		ans = ans.substring(0, ans.length()-1);		
		return "(" + ans + ")*";
	}




	/**
	 * @param A - an automaton that represents the initial condition of a trigger.
	 * @param B - an automaton that represents the effect of of the trigger.
	 * @return - A new automaton that is the concatenation of A with B, 
	 * but with the accepting states of A actually replaced by the initial state of B.
	 * If B's initial state is accepting - returns null (no need to translate the trigger
	 * as it is always satisfied).
	 */
	public static Automaton triggerConcatenate(Automaton A, Automaton B, Set<String> alphabet) {
		if (!A.isDeterministic() || !B.isDeterministic()) {
			throw new RuntimeException("Automaton is not deterministic");
		}
		if (B.getInitialState().isAccept()) {
			return null;
		}

		Automaton result = new Automaton();
		State initInitState = A.getInitialState();
		State triggerInitialState = null;

		if (initInitState.isAccept()) {
			return makeEffectTrigger(B, alphabet);
		}

		// start clone A 
		List<State> statesA = new ArrayList<State>(A.getStates());
		List<State> newStatesA = new ArrayList<State>();

		for (int i=0; i < statesA.size(); i++) {
			State newState = new State();
			newState.setAccept(true); // To recognize which states came from the initiator automaton
			newStatesA.add(newState);
			if (statesA.get(i).equals(A.getInitialState())) {
				triggerInitialState = newState; 
				result.setInitialState(triggerInitialState);
			}
		}

		// clone B (with accepting states replaced by A initial state)
		List<State> statesB = new ArrayList<State>(B.getStates());
		List<State> newStatesB = new ArrayList<State>();

		for (int i=0; i < statesB.size(); i++) {
			State newState = new State();
			newStatesB.add(newState);
		}

		for (int i=0; i < statesB.size(); i++) {
			State stateB = statesB.get(i);
			State newState = newStatesB.get(i);

			if (stateB.isAccept()) { 
				// pass
			} else {
				for (Transition trans : stateB.getTransitions()) {
					char min = trans.getMin();
					char max = trans.getMax();
					Transition newTrans;
					State destB = trans.getDest();
					if (destB.isAccept()) {
						newTrans = new Transition(min, max, newStatesA.get(statesA.indexOf(initInitState)));
					} else {
						newTrans = new Transition(min, max, newStatesB.get(statesB.indexOf(destB)));
					}
					newState.addTransition(newTrans);
				}
			}
		}

		State initEffectState = newStatesB.get(statesB.indexOf(B.getInitialState()));

		// continue clone A and combine (without accepting states)
		// A-transitions into an accepting state are replaced by transitions into B's initial state
		for (int i=0; i < statesA.size(); i++) {

			State stateA = statesA.get(i);
			State newState = newStatesA.get(i);

			if (stateA.isAccept()) {
				// pass
			} else {
				for (Transition trans : stateA.getTransitions()) {
					char min = trans.getMin();
					char max = trans.getMax();
					Transition newTrans;
					State destA = trans.getDest();
					if (destA.isAccept()) {
						newTrans = new Transition(min, max, initEffectState);
					} else {
						newTrans = new Transition(min, max, newStatesA.get(statesA.indexOf(destA)));
					}
					newState.addTransition(newTrans);
				}  
			}
		}

		// add garbage state transitions to itself
		State garbageState = new State();
		garbageState.setAccept(true);
		for (String symbol : alphabet) {
			Transition trans = new Transition(symbolToChar(symbol), symbolToChar(symbol), garbageState);
			garbageState.addTransition(trans);
		}
		
		
		// for every non-accepting state x of A with no defined transition for symbol y, 
		// add transition to garbage state
		for (State state : result.getStates()) {
			if( (newStatesA.contains(state)) && 
					(!statesA.get(newStatesA.indexOf(state)).isAccept()) ){
			
			Set<Character> covered = new LinkedHashSet<Character>();
			for (Transition trans : state.getTransitions()) {
				for (char c = trans.getMin(); c <= trans.getMax(); c++) {
					covered.add(c);
				}
			}
			for (String symbol : alphabet) {
				char c = symbolToChar(symbol);
				if (!covered.contains(c)) {
					Transition trans = new Transition(c, c, garbageState);
					state.addTransition(trans);
				}
			}
		}
		}

		return result;
	}
	
	/**
	 * @param s1 - an automaton state
	 * @param s2 - an automaton state 
	 * @return returns the set of all symbols of transitions from state s1 to state s2, 
	 * represented as binary strings
	 */
	public static Set<Integer> getAllTransitions(State s1, State s2, int size) {
		Set<Integer> allTransitions = new LinkedHashSet<Integer>();
		for (Transition trans : s1.getTransitions()) {
			if (trans.getDest().equals(s2)) {
				for (int i = (int) trans.getMin()-192; i <= (int) trans.getMax()-192; i++) {
					allTransitions.add(i);
				}
			}
		}
		return allTransitions;
	}
	
	
	public static Spec getSpecFromSymbols(Map<Spec, SpecSymbols> specSymbolsMap, Set<String> symbols) {
		Map<Integer, Spec> specByIndex = new LinkedHashMap<Integer, Spec>();
		for (SpecSymbols specSymbols : specSymbolsMap.values()) {
			specByIndex.put(specSymbols.getIndex(), specSymbols.getSpec());
		}
		int numOfSpecs = specByIndex.size();
		List<String> symbolsList = new ArrayList<String>(symbols);

		Spec accumulatedOr = null;
		for (int i = 0; i < symbolsList.size(); i++) {
			Spec accumulatedAnd = null;
			Spec negatedSpec = null; // for "zeros"
			String symbol = symbolsList.get(i);
			for (int j = 0; j < numOfSpecs; j++) { // for each specs
				Spec spec = specByIndex.get(j);
				negatedSpec = new SpecExp(Operator.NOT, spec);
				if (j == 0) {
					accumulatedAnd = isOneOnIndex(symbol, j)? spec : negatedSpec;
				} else if (isOneOnIndex(symbol, j)) {
					accumulatedAnd = new SpecExp(Operator.AND, accumulatedAnd, spec);
				} else {
					accumulatedAnd = new SpecExp(Operator.AND, accumulatedAnd, negatedSpec);
				}
			}
			if (i == 0) {
				accumulatedOr = accumulatedAnd;
			} else {
				accumulatedOr = new SpecExp(Operator.OR, accumulatedOr, accumulatedAnd);  
			}
		}
		return accumulatedOr;
	}
	
	public static Set<String> createAlphabet(Map<Spec, SpecSymbols> specSymbolsMap) {

		Set<String> alphabet = new LinkedHashSet<String>();

		for (SpecSymbols specSymbols : specSymbolsMap.values()) {
			alphabet.addAll(specSymbols.getSymbols());
		}

		alphabet.add(("0"));

		return alphabet;
	}
	
	/***
	 * Translates the specifications - from predicates to binary strings,
	 * to generate an alphabet for an automaton.
	 * The alphabet generated is the binary strings from 1 to (pow(2, specs.size())-1).
	 * The translation of spec of index i is the set of binary strings
	 * with 1 in place i.
	 */
	public static Map<Spec, SpecSymbols> translateSpecs(List<Spec> specs) {

		double numOfSymbols = Math.pow(2, specs.size()); 
		List<String> allSymbols = new ArrayList<String>();
		Map<Spec, SpecSymbols> specSymbolsMap = new LinkedHashMap<Spec, SpecSymbols>();

		for (int i = 1; i < numOfSymbols; i++) {
			String symbol = Integer.toBinaryString(i);
			allSymbols.add(symbol);
		}
		for (int i = 0; i < specs.size(); i++) {
			Set<String> specSymbols = new LinkedHashSet<String>();
			Spec currSpec = specs.get(i);
			for (String symbol : allSymbols) {
				if (isOneOnIndex(symbol, i)) {
					specSymbols.add(symbol);
				}
			}
			specSymbolsMap.put(currSpec, new SpecSymbols(currSpec, i, specSymbols));
		}
		return specSymbolsMap;
	}
	
	/*###############################################################################
	 * ############################## private functions ###############################*/

	/**
	 * A method for cases where the initiator always accepts, 
	 * and then the effect should always happen.
	 * The method convert the effect automaton in the following way:
	 * Adds a new accepting state
	 * Replaces its old accepting states by the new accepting state.
	 * copy outgoing edges of the initial state to the new accepting state 
	 * The method throws an exception if the initial state of the effect automaton is not accepting
	 * (this case should be handled before the call).
	 */
	
	private static Automaton makeEffectTrigger(Automaton effectAutomaton, Set<String> alphabet) {
		if (effectAutomaton.getInitialState().isAccept()) {
			throw new RuntimeException("Accepting initial state of effect automaton was not handled");
		}

		// create new accepting state	
		State newAcceptState = new State();
		newAcceptState.setAccept(true);
		
		// clone the automaton states
		List<State> originalStates = new ArrayList<State>(effectAutomaton.getStates());
		List<State> newStates = new ArrayList<State>();
		State newInitial = null;
		for (State originalState : originalStates) {
			State newState = new State();
			newStates.add(newState);
			if (effectAutomaton.getInitialState().equals(originalState)) {
				newInitial = newState;	
			}			  
		}
		
		// copy transitions.
		// Transitions into old accepting states are redirected into the new accepting state	
		for (int i = 0; i < newStates.size(); i++) {
			State originalState = originalStates.get(i);
			State newState = newStates.get(i);
			if (!originalState.isAccept()) {
				for (Transition trans : originalState.getTransitions()) {
					State dest = trans.getDest().isAccept()? newAcceptState : newStates.get(originalStates.indexOf(trans.getDest()));
					Transition newTrans = new Transition(trans.getMin(), trans.getMax(), dest);
					newState.addTransition(newTrans);
				}
			}
		}
		
		// Copy transitions of the initial state to the new accepting state
		for (Transition trans : newInitial.getTransitions()) {
			Transition newTrans = new Transition(trans.getMin(), trans.getMax(), trans.getDest());
			newAcceptState.addTransition(newTrans);
		}

		// return the constructed automaton
		Automaton result = new Automaton();
		result.setInitialState(newInitial);
		return result;
	}
	
	
//	// translates a spectra regular expression into a String that represents a brics regular expression
//
//	@SuppressWarnings("unused")
//	private static String translateRegExp(SpecRegExp regexp,List<String> symbols, String[] binaryWords) {
//		
//			if (regexp.getRegExpKind() == Quantifier.EMPTY_STR ) {
//				return ( "()" );
//			}
//			
//			if (regexp.getRegExpKind() == Quantifier.VAR) {
//				String name =regexp.getName();
//				int index = symbols.indexOf(name);
//				return ( binaryWords[index] );
//			}
//			
//			
//			if (regexp.getRegExpKind() == Quantifier.NEGVAR) {
//				String name = regexp.getName();
//				int index = symbols.indexOf(name);
//				return ( binaryWords[2*index] );
//			}
//			
//			if (regexp.getRegExpKind() == Quantifier.VAL) {
//				if (regexp.getName().charAt(0) == 'T') { 
//				  return (binaryWords[symbols.size()]); }
//				if (regexp.getName().charAt(0) == 'F') {
//					return (binaryWords[symbols.size()+1]); }
//			}
//			
//			if (regexp.getRegExpKind() == Quantifier.ZERO_OR_MORE)   { 
//				return ("(" + translateRegExp(regexp.getLeft(), symbols, binaryWords) + ")*");
//			}
//			
//			if (regexp.getRegExpKind() == Quantifier.ZERO_OR_ONE)   { 
//				return ("(" + translateRegExp(regexp.getLeft(), symbols, binaryWords) + ")?");
//			}
//			
//			if (regexp.getRegExpKind() == Quantifier.ONE_OR_MORE)   { 
//				return ("(" + translateRegExp(regexp.getLeft(), symbols, binaryWords) + ")+");
//			}
//			
//			if (regexp.getRegExpKind() == Quantifier.EXACT_REPETITION)   { 
//				return ("(" + translateRegExp(regexp.getLeft(), symbols, binaryWords)  + "){" + 
//						regexp.getFrom() +"}");
//			}
//			
//			if (regexp.getRegExpKind() == Quantifier.AT_LEAST)   { 
//				return ("(" +  translateRegExp(regexp.getLeft(), symbols, binaryWords) + "){" +
//						regexp.getFrom() +",}");
//			}
//
//			if (regexp.getRegExpKind() == Quantifier.RANGE)   { 
//				return ("(" + translateRegExp(regexp.getLeft(), symbols, binaryWords) +
//						"){" + regexp.getFrom() + "," + regexp.getTo() +  "}");
//			}
//			
//			if (regexp.getRegExpKind() == Quantifier.COMPLEMENT )   { 
//				return ("~(" + translateRegExp(regexp.getLeft(), symbols, binaryWords) + ")");
//			}
//			
//			if(regexp.getRegExpKind() == Quantifier.CONCAT) {
//					return ("(" + translateRegExp(regexp.getLeft(), symbols, binaryWords) + ")" +
//						   "(" + translateRegExp(regexp.getRight(), symbols, binaryWords) + ")");
//			}
//			
//			if(regexp.getRegExpKind() == Quantifier.UNION) {
//				return ("(" + translateRegExp(regexp.getLeft(), symbols, binaryWords) + ")" + "|" +
//					   "(" + translateRegExp(regexp.getRight(), symbols, binaryWords) + ")");
//			}
//			
//			if(regexp.getRegExpKind() == Quantifier.INTERSECTION) {
//				return ("(" + translateRegExp(regexp.getLeft(), symbols, binaryWords) + ")" + "&" +
//					   "(" + translateRegExp(regexp.getRight(), symbols, binaryWords) + ")");
//			}
//			return null;
//	}
	
	
	// translates a spectra regular expression into a String that represents a brics regular expression
	// This method supports non-binary variables
	
	
	private static String translateRegExp2(SpecRegExp regexp,List<String> symbols, 
			List<Integer> fromList, List<Integer> toList) {
		
		return "";
		
	}
	
	/*private static String translateRegExp2(SpecRegExp regexp,List<String> symbols, 
			List<Integer> fromList, List<Integer> toList) {
		
		Set<Integer> values;
		
		if (regexp.getRegExpKind() == Quantifier.EMPTY_STR ) {
			return "()";
		}
		
		
		if ( (regexp.getRegExpKind() == Quantifier.VAR) || (regexp.getRegExpKind() == Quantifier.NEGVAR) ) {
			String name =regexp.getName();
			int index = symbols.indexOf(name);
			values = regexp.getValues();  
			return satisfyingAssignments(symbols,fromList,toList,index,values);
		}

		
		if (regexp.getRegExpKind() == Quantifier.VAL) {
			if (regexp.getName().charAt(0) == 'T') { 
			  return ( createNonZeroSequences(fromList,toList) ); }
			if (regexp.getName().charAt(0) == 'F') {
				char symbol = (char) 192;
				return ("(" + symbol + ")"); }
		}
		
		
		if (regexp.getRegExpKind() == Quantifier.ZERO_OR_MORE)   { 
			return "(" + translateRegExp2(regexp.getLeft(), symbols, fromList, toList) + ")*";
		}
		
		
		if (regexp.getRegExpKind() == Quantifier.ZERO_OR_ONE)   { 
			return "(" + translateRegExp2(regexp.getLeft(), symbols, fromList, toList) + ")?";
		}
		
		
		if (regexp.getRegExpKind() == Quantifier.ONE_OR_MORE)   { 
			return "(" + translateRegExp2(regexp.getLeft(), symbols, fromList, toList) + ")+";
		}
		
		
		if (regexp.getRegExpKind() == Quantifier.EXACT_REPETITION)   { 
			return "(" + translateRegExp2(regexp.getLeft(), symbols, fromList, toList)  + "){" + 
					regexp.getFrom() +"}";
		}
		
		
		if (regexp.getRegExpKind() == Quantifier.AT_LEAST)   { 
			return "(" +  translateRegExp2(regexp.getLeft(), symbols, fromList, toList) + "){" +
					regexp.getFrom() +",}";
		}

		
		if (regexp.getRegExpKind() == Quantifier.RANGE)   { 
			return "(" + translateRegExp2(regexp.getLeft(), symbols, fromList, toList) +
					"){" + regexp.getFrom() + "," + regexp.getTo() +  "}";
		}
		
		
		if (regexp.getRegExpKind() == Quantifier.COMPLEMENT )   { 
			return "~(" + translateRegExp2(regexp.getLeft(), symbols, fromList, toList) + ")";
		}
		
		
		if(regexp.getRegExpKind() == Quantifier.CONCAT) {
			String left = translateRegExp2(regexp.getLeft(), symbols, fromList, toList);
			String right = translateRegExp2(regexp.getRight(), symbols, fromList, toList);
			return "(" + left + ")" + "(" + right + ")";
		}
		
		
		if(regexp.getRegExpKind() == Quantifier.UNION) {
			String left = translateRegExp2(regexp.getLeft(), symbols, fromList, toList);
			String right = translateRegExp2(regexp.getRight(), symbols, fromList, toList);
			return "(" + left + ")" + "|" + "(" + right + ")";
		}
		
		
		if(regexp.getRegExpKind() == Quantifier.INTERSECTION) {
			String left = translateRegExp2(regexp.getLeft(), symbols, fromList, toList);
			String right = translateRegExp2(regexp.getRight(), symbols, fromList, toList);
			return "(" + left + ")" + "&" + "(" + right + ")";
		}
		return null;
}*/
	
	/**
	 * A method to construct a string that is a disjunction of all symbols representing non-zero sequences.
	 * @param fromList - the minimal values of all variables.
	 * @param toList - the maximal values of all variables.
	 * @return A disjunction of all sequences that matches the value 'T' (as a String).
	 */
	
	
	private static String createNonZeroSequences(List<Integer> fromList, List<Integer> toList) {
		
		int[] sequence = new int[fromList.size()];
		for (int i=0; i<sequence.length; i++) 
			sequence[i] = fromList.get(i);
		
		
		int[] convert=createConvertionArray(fromList, toList);
		
		sequence =nextSequence(sequence, fromList, toList);
		String ans = "(";		
		
		boolean done = true;
		int i=0;
		while(done && i<sequence.length) {  // check if the sequence is different from the zero sequence
			done = done & (sequence[i] == fromList.get(i) );
			i++;
		}
		
		while(!done) {
			
			char numToChar = sequenceToChar(sequence, fromList, convert);
			ans = ans + numToChar + "|";
			
			sequence = nextSequence(sequence,fromList,toList);
			
			// check if all sequences have been traversed. This holds if 'sequence' stores the first sequence
			done = true;
			i=0;
			while(done && i<sequence.length) {
				done = done & (sequence[i] == fromList.get(i) );
				i++;
			}
		}
		
		// a correction to ans
		if (ans.length()>1)
			ans = ans.substring(0, ans.length()-1) + ")";
		else
			ans = ans + ")";
		return ans;
	}


	/**
	 * This method receives a variable name, its range and possible values, 
	 * and returns a disjunction of all corresponding sequences (assignments).
	 * 
	 * @param symbols - a List of all variable names that appear in the trigger regular expressions 
	 * @param fromList - a list that specifies for each variable its minimal value
	 * @param toList - a list that specifies for each variable its maximal value
	 * @param index - the index in which the discussed variable appears 
	 * @param values -  the set of all permitted values for the discussed variable
	 * @return
	 */
	private static String satisfyingAssignments(List<String> symbols, List<Integer> fromList, 
			List<Integer> toList, int index, Set<Integer> values) {
		
		int[] convert = createConvertionArray(fromList, toList);
			
		// sequence stores the assignment we are about to check. We start with the first sequence
		int[] sequence = new int[fromList.size()]; 
		for (int i = 0; i< sequence.length; i++) {
			sequence[i] =fromList.get(i);
		}
		
		String ans ="(";
		boolean done;
		do {  // Traversing all sequences and add those in which sequence[index] belongs to values
			
			if ( values.contains(sequence[index]) ){ // if we should add the current sequence		
					char numToChar = sequenceToChar(sequence, fromList, convert);
					ans = ans + numToChar + "|";
			}
			// Precede to the next sequence
			sequence = nextSequence(sequence,fromList,toList);
			
			// check if all sequence have been traversed. This holds if 'sequence' stores the first sequence
			done = true;
			int i=0;
			while(done && i<sequence.length) {
				done = done & (sequence[i] == fromList.get(i) );
				i++;
			}
		} while(!done);
		
		// a correction to ans
		if (ans.length()>1)
			ans = ans.substring(0, ans.length()-1) + ")";
		else
			ans = ans + ")";
		return ans;
	}
	
	
	/**
	 * convert uses to transform sequence into a number w.r.t. the lexicographic ordering.<br>
	 * Specifically, convert[j] denotes how much entry j "worth". Thus, the transformation is: SUM_j(sequence[j]*convert[j])
	 * @param fromList - specifies the minimal value of each variable.
	 * @param toList - specifies the minimal value of each variable.
	 * @return
	 */

	private static int[] createConvertionArray(List<Integer> fromList, List<Integer> toList) {

		int[] convert  = new int[fromList.size()];
		int[] maxNum = new int[fromList.size()];
		convert[convert.length-1]=1;
				
		// maxNum[i] stores the maximal integer can be produced from the subsequence that start from i.
		// Namely, the subsequence for which the jth letter equals to toList[j].
		// This can be computed as follows: maxNum[i] = SUM_{j=i}^{convert[length-1]}( (toList[j]-fromList[j])*convert[i] )
		// Thus, a shorter way is: maxNum[i] = maxNum[i+1] + (toList[i]-fromLsit[i])*convert[i]
		maxNum[maxNum.length-1]=toList.get(maxNum.length-1)-fromList.get(maxNum.length-1); 
				
		// The computation of convert and MaxNum
		for(int i=convert.length-2; i>=0 ; i--) {
			convert[i] = maxNum[i+1]+1;
			maxNum[i]=( (toList.get(i)-fromList.get(i))*convert[i] ) + maxNum[i+1];
		}
		
		return convert;
	}


	/**
	 * The method gets a sequence and returns the next sequence lexicographically,<br>
	 *  when the ith char is taken from the range fromList[i]..toList[i] (in unicode). 
	 * @param sequence - the sequence we want its successor
	 * @param fromList - entry fromList[i] stores the minimal value that the ith variable can hold
	 * @param toList - entry toList[i] stores the maximal value that the ith variable can hold
	 * @return the next sequence
	 */
	
	private static int[] nextSequence(int[] sequence, List<Integer> fromList, List<Integer> toList) {

		int[] ans = new int[sequence.length];
		
	//  copy the sequence	
		for(int i=0; i<sequence.length; i++)
			ans[i]=sequence[i];
		
		int index = ans.length-1;
		boolean done = false;
		
		// Finding the first symbol (from right to left) that is smaller than the possible maximal value
		do {  
			if (index>=0){
				if ( ans[index] == toList.get(index)  )
					index  = index -1;
				else 
					done = true;
			}
		} while ((done == false) && (index>=0));
		
		// Increasing this symbol by 1 
		if (index>=0) ans[index]++; 
		
		// changing all following symbols to their minimal value 
		for (int i=index+1; i<sequence.length; i++)  
			ans[i] = fromList.get(i);
		
		// returning the result
		return ans;
	}
	
	
	private static char sequenceToChar(int[] sequence, List<Integer> fromList, int[] convert) {
		
		
		int num = 192;
		for (int j = sequence.length-1; j>=0 ;j--) {
			num = num + (sequence[j]-fromList.get(j))*convert[j];
		}					
		// cast the integer to a unicode char
		char ans = (char) num;		
		return ans;
	}

	
	@SuppressWarnings("unused")
	private static String escapedSymbolToChar(String symbol) {

		char symbolChar = symbolToChar(symbol);
		String res = "";

		if (needEscape(symbolChar)) {

			res = "\\";
		}

		res += symbolChar;

		return res;
	}

	/**
	 * 
	 * returns true iff this character needs an escape sequence (\)
	 * when presented in the context of a regexp.
	 * 
	 * @param symbolChar
	 * @return
	 */
	private static boolean needEscape(char symbolChar) {

		boolean res;

		switch (symbolChar) {

		case '|' :
		case '&' :
		case '?' :
		case '*' :
		case '+' :
		case '{' :
		case '}' :
		case '~' :
		case '[' :
		case ']' :
		case '.' :
		case '#' :
		case '@' :
		case '"' :
		case '(' :
		case ')' :
		case '<' :
		case '>' :
		case '\\' :
			res = true;
			break;

		default :
			res = false;
			break;
		}


		return res;
	}

	private static char symbolToChar(String symbol) {
		return (char) Integer.parseInt(symbol, 2);
	}
	

	private static boolean isOneOnIndex(String symbol, int index) {
		if (symbol.length()-1 < index) return false;
		int stringIndex = (index == 0 ? symbol.length()-1 : symbol.length()-index-1);
		return symbol.charAt(stringIndex) == '1';
	}

	public static int[] createSequenceFromNum(int target, List<Integer> fromList, List<Integer> toList) {
		
		int[] convert = createConvertionArray(fromList, toList);
		int[] ans = new int[fromList.size()];
		
		for (int i = 0; i<ans.length; i++) {
			int j = toList.get(i);
			while ( (j-fromList.get(i))*convert[i] > target)
				j--;
			
			ans[i]=j;
			target = target - ((ans[i]-fromList.get(i))*convert[i]);
		}
		
		return ans;
	}


}
