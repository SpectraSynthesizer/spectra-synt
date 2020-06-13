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

package tau.smlab.syntech.bddgenerator.sfa.regexp;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import dk.brics.automaton.Automaton;
import dk.brics.automaton.RegExp;
import dk.brics.automaton.State;
import dk.brics.automaton.Transition;
import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDDomain;
import tau.smlab.syntech.bddgenerator.BDDGenerator;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.sfa.SFA;
import tau.smlab.syntech.sfa.SFAState;
import tau.smlab.syntech.sfa.SFAs;

/**
 * 
 * A class for transforming regular expressions into Symbolic finite automatons (SFAs) via the Brics package.</br>
 * 
 * Essentially, instances of {@link SpecRegExp} are translated to regular expressions that are supported by Brics.
 * This translation entails an exponential blowup of the alphabet.
 * The resulting Brics automaton is translated to an equivalent SFA.</br>
 * 
 * <p>NOTE: Use this generator for evaluation/validation purposes only. To obtain better performance, use {@link SymRegExpSFAGenerator}.</p>
 * 
 * <p>NOTE: Due to Brics, this generator only supports instances of {@link SpecRegExp} that use at most 16 Boolean variables.</p>
 * 
 * @author Or Pistiner
 * @author Gal Amram
 *
 */
public class BricsRegExpSFAGenerator implements RegExpSFAGenerator {

	private static final String BRICS_EMPTY_LANGUAGE = "#";
	private static final String BRICS_EMPTY_STRING = "()";
	private static final String TRUE = "true";
	private static final String FALSE = "false";
	
	/*
	 * 
	 * In a string that represents a regular expression of Brics,
	 * there are RESERVED Unicode characters that have special meanings.
	 * In order to use these characters just as any character, one has to escape them (using '\').
	 * To avoid the need of escaping, we MAY translate the SpecRegExp to a Brics RegExp that only uses
	 * NON-RESERVED Unicode characters.
	 * This may be done by only using characters whose value is at least a "safe" threshold value, MIN_UNICODE_CHAR.
	 * However, BY DEFAULT, we perform escaping to make use of all the Unicode BMP characters (0x0000-0xFFFF).
	 * 
	 */
	private static final int MIN_UNICODE_CHAR = 0; //Default value of 0 (i.e., perform escaping); may be changed to, e.g., 0xA0; (Unicode code point of "Non-breaking space")
	
	
	private SFA sfa; //The SFA that would be created
	
	private BricsRegExpBuilder regExpBuilder;
	private Automaton bricsAutomaton; //The Brics automaton that would be created
	
	private List<RegExpVarReference> regExpVars; //List of all the variable references that appear in this.regExpBuilder.specRegExp
	private List<Integer> fromList, toList; //The domain of the variable regExpVars[i] ranges from fromList[i] to toList[i] (inclusive)
	
	private int[] varAssgUnicodeConvertArr;
	private Map<Integer, VarAssignment> unicodeToVarAssgMapping;
	
	/**
	 * Returns a new SFAGenerator that relies on the Brics package to translate the specified Spectra regular expression {@code specRegExp}
	 * to an SFA. Essentially, {@code specRegExp} is translated to a regular expression of Brics, which is then translated
	 * to an equivalent Brics automaton. Finally, that Brics automaton is transformed into an SFA by translating the transitions' labels
	 * (Unicode characters) to predicates (BDDs).
	 * 
	 * </p> NOTE: In contrast to {@link SymRegExpSFAGenerator}, this generator generates an SFA whose transitions' guards also encode
	 * the domain information/restrictions of don't care variables if they appear in {@code specRegExp}.
	 * 
	 * @param specRegExp the Spectra regular expression
	 * @param traceId the trace ID of {@code specRegExp}
	 */
	public BricsRegExpSFAGenerator(SpecRegExp specRegExp, int traceId) {
		this.initializeSpecRegExpAttr(specRegExp);
		this.regExpBuilder = new BricsRegExpBuilder().specRegExp(specRegExp).traceId(traceId);
		this.unicodeToVarAssgMapping = new HashMap<>();
	}

	public BricsRegExpSFAGenerator(SpecRegExp specRegExp, int traceId, boolean min) {
		this.initializeSpecRegExpAttr(specRegExp);
		this.regExpBuilder = new BricsRegExpBuilder().specRegExp(specRegExp).traceId(traceId);
		this.unicodeToVarAssgMapping = new HashMap<>();
	}

	@Override
	public SFA generateRegExpSfa() {
		return generateRegExpSfa(BricsAutKind.NFA);
	}

	@Override
	public SFA generateRegExpDSfa(boolean minimal) {
		return generateRegExpSfa(minimal ? BricsAutKind.MINIMAL_DFA : BricsAutKind.DFA);
	}

	@Override
	public void freeRegExpSfa() {
		if(this.sfa != null) {
			this.sfa.free();
		}
		this.sfa = null;
	}

	enum BricsAutKind {NFA, DFA, MINIMAL_DFA};

	private SFA generateRegExpSfa(BricsAutKind bricsAutKind) {
		if(this.sfa != null) {
			return this.sfa;
		}

		Automaton regExpBricsAut = this.regExpBuilder.build().toAutomaton(false); //Construct a non-deterministic Brics automaton

		if(this.regExpBuilder.specRegExp.containsComplement()) {
			//NOTE: The regular expression contains a complement operator, thus the constructed Brics automaton (regExpBricsAut)
			//may accept (finite) words over an alphabet with Unicode characters that do not correspond/map to any variable assignment.
			//This is because Brics performs complementation where the alphabet is the whole Unicode BMP plane (0x0000-0xFFFF).
			//As a solution, we intersect regExpBricsAut with the automaton trueStarBricsAut that accepts all words whose
			//letters are only the Unicode characters that correspond to variable assignments.

			SpecRegExp trueStar = SpecRegExp.newZeroOrMoreRepRegExp(SpecRegExp.newBooleanConstRegExp(true)); //TRUE*
			Automaton trueStarBricsAut = new BricsRegExpBuilder().specRegExp(trueStar).build().toAutomaton(false);
			this.bricsAutomaton = regExpBricsAut.intersection(trueStarBricsAut);			
		}
		else {
			this.bricsAutomaton = regExpBricsAut;		
		}

		switch(bricsAutKind) {
		case DFA:
			this.bricsAutomaton.determinize(); //Determinize the automaton
			break;
		case MINIMAL_DFA:
			this.bricsAutomaton.minimize(); //Determinize and minimize the automaton
			//Default minimization algorithm used: Hopcroft's O(n log n) algorithm (can be changed via setMinimization(..) method)
			break;
		default:
			break;
		} 

		this.sfa = this.translateBricsAutomatonToSfa();

		return this.sfa;
	}
	
	private void initializeSpecRegExpAttr(SpecRegExp specRegExp) {
		this.regExpVars = buildRegExpVarRefList(specRegExp);//specRegExp.getVariables();
		this.fromList = new ArrayList<Integer>();
		this.toList = new ArrayList<Integer>();
		
		Variable var;
		for(RegExpVarReference varRef : this.regExpVars) {
			var = varRef.getVariable();
			if(var.getType().isBoolean()) { //A Boolean variable
				this.fromList.add(0);
				this.toList.add(1);
			}
			else if(var.getType().isInteger()) { //An integer variable	
				this.fromList.add(var.getType().getLower());
				this.toList.add(var.getType().getUpper());
			}
			else { //An Enum variable
				this.fromList.add(0);
				this.toList.add(var.getType().getValues().size()-1);
			}
		}
		this.createVarAssgUnicodeConversionArr();
	}
	
	private List<RegExpVarReference> buildRegExpVarRefList(SpecRegExp specRegExp) {
		Map<String, Variable> refToVarMap = specRegExp.getRefToVarMapping();
		List<RegExpVarReference> regExpVarRefList = new ArrayList<>();
		for(String refName : refToVarMap.keySet()) {
			regExpVarRefList.add(new RegExpVarReference(refName, refToVarMap.get(refName)));
		}
		return regExpVarRefList;
	}
	
	/**
	 * Initializes the {@code varAssgUnicodeConvertArr} attribute of this generator.
	 * This attribute points to an array whose size equals the number of variables that appear
	 * in the regular expression of this generator (i.e., {@code this.regExpVars.size()}).
	 * This regular expression is translated to that in Brics. Specifically, each atomic subexpression
	 * of the form '[VAR in {set of values}]' is mapped to a range of Unicode BMP characters, [CHAR1_FROM-CHAR1_TO CHAR2_FROM-CHAR2_TO ...].
	 * Notice that, in fact, '[VAR in {set of values}]' represents a set of assignments to the variables that appear in the regular expression
	 * where VAR may be assigned either value in {set of values}, while each of the other variables (if exist) may be assigned any value
	 * in its domain ("don't cares").
	 * Each character range is computed using varAssgUnicodeConvertArr. Essentially, {@code varAssgUnicodeConvertArr[i]} represents
	 * the "radix" or "base" of the variable {@code this.regExpVars[i]}, a generalization of positional numeral systems, such as Binary,
	 * where {@code this.varAssgUnicodeConvertArr[this.varAssgUnicodeConvertArr.length-1] = 1} is that of the least significant position/bit/variable
	 * and {@code this.varAssgUnicodeConvertArr[0]} is that of the most significant one. Informally, we have that {@code this.varAssgUnicodeConvertArr[i]} is
	 * the successor of the maximal Unicode code point (an integer) that can be generated from an assignment to
	 * the variables thus far, namely {@code this.regExpVars[i+1] ... this.regExpVars[this.regExpVars.size()-1]}. That is,
	 * provided that the domain the variable {@code regExpVars[i]} ranges from {@code this.fromList[i]} to {@code this.toList[i]},
	 * then {@code this.varAssgUnicodeConvertArr[i] = 1 + maxNum[i+1]}, where
	 *  {@code maxNum[maxNum.length-1] = toList[maxNum.length-1]-this.fromList[maxNum.length-1]} and
	 *  {@code maxNum[i]=((toList[i]-fromList[i]) * this.varAssgUnicodeConvertArr[i]) + maxNum[i+1]}.
	 * Note that {@code maxNum[i]} is generated from the assignment where each of the variables {@code this.regExpVars[i] ... this.regExpVars[j] ... this.regExpVars[this.regExpVars.size()-1]}
	 * is assigned the maximal possible value {@code this.toList[j]}. To obtain smaller Unicode code points,
	 * the minimal possible value {@code this.fromList[j]} is subtracted from {@code this.toList[j]} (i.e., the values in the domain of each variable are "normalized").
	 * 
	 */
	private void createVarAssgUnicodeConversionArr() {
		this.varAssgUnicodeConvertArr  = new int[this.regExpVars.size()];
		if(!this.regExpVars.isEmpty()) {
			int[] maxNum = new int[this.regExpVars.size()];
			this.varAssgUnicodeConvertArr[this.varAssgUnicodeConvertArr.length-1]=1;

			//maxNum[i] stores the maximal integer that can be produced from a subsequence (assignment) that start from i and ends
			// at maxNum[varAssgUnicodeConvertArr.length-1]. This integer is produced from the subsequence for which the j-th element equals to toList[j]
			//and can be computed as follows: maxNum[i] = SUM_{j=i}^{varAssgUnicodeConvertArr.length-1} ((toList[j]-fromList[j]) * varAssgUnicodeConvertArr[i]).
			//Thus, inductively, maxNum[i] = maxNum[i+1] + (toList[i]-fromLsit[i])*varAssgUnicodeConvertArr[i].
			maxNum[maxNum.length-1]=this.toList.get(maxNum.length-1)-this.fromList.get(maxNum.length-1); 

			//The computation of varAssgUnicodeConvertArr and maxNum
			for(int i=this.varAssgUnicodeConvertArr.length-2; i>=0 ; i--) {
				this.varAssgUnicodeConvertArr[i] = maxNum[i+1]+1;
				maxNum[i]=((this.toList.get(i)-this.fromList.get(i)) * this.varAssgUnicodeConvertArr[i]) + maxNum[i+1];
			}
		}
	}

	private SFA translateBricsAutomatonToSfa() {
		SFA equivSfa = SFAs.newSimpleSfa(SFAs.newSimpleSfaState(this.bricsAutomaton.getInitialState().isAccept()));
		Map<State, SFAState> bricsStatesToSfaStatesMapping = new HashMap<>();
		bricsStatesToSfaStatesMapping.put(this.bricsAutomaton.getInitialState(), equivSfa.getIni());
		
		Queue<State> worklist = new LinkedList<>();
		worklist.add(this.bricsAutomaton.getInitialState());		
		
		while (!worklist.isEmpty()) {
			State bricsState = worklist.remove();
			
			// Add successors not checked yet
			for (Transition bricsTransition : bricsState.getTransitions()) {
				if (!bricsStatesToSfaStatesMapping.containsKey(bricsTransition.getDest())) {
					worklist.add(bricsTransition.getDest());
					bricsStatesToSfaStatesMapping.put(bricsTransition.getDest(), equivSfa.newSfaState(bricsTransition.getDest().isAccept()));
				}
				bricsStatesToSfaStatesMapping.get(bricsState).addTrans(createTransitionGuardBdd(bricsTransition), bricsStatesToSfaStatesMapping.get(bricsTransition.getDest()));
			}
		}
		return equivSfa;
	}
	
	/**
	 * Translates the specified Brics transition interval (i.e., a range of Unicode BMP characters) to the guard of the corresponding SFA transition.
	 * That is, returns the BDD that encodes the set of variable assignments that the specified transition interval represents.
	 * 
	 * @param transition Brics transition interval
	 * @return
	 */
	private BDD createTransitionGuardBdd(Transition transition) {
		List<VarAssignment> guardVarAssgns = getGuardVarAssgs(transition);

		BDD res = Env.FALSE();
		for(int i = 0 ; i < guardVarAssgns.size(); i++) {
			res.orWith(guardVarAssgns.get(i).toBdd());
		}
		return res;
	}

	private List<VarAssignment> getGuardVarAssgs(Transition transition) {		
		List<VarAssignment> decodedVarAssgns = new ArrayList<>();
		for(int unicodeCode = (int) transition.getMin(); unicodeCode <= (int) transition.getMax(); unicodeCode++) {
			decodedVarAssgns.add(decodeVarAssg(unicodeCode));
		}
		return decodedVarAssgns;
	}
	
	/**
	 * 
	 * An inner class that represents a variable reference that appears in the {@link SpecRegExp} of this generator.
	 * Each such reference may appear in an atomic subexpression. Two references with the same reference name are considered equal.
	 * Note that two references may not be equal but still reference the same variable in case that variable is an array (e.g., the references 'arr[0]' and 'arr[11]').
	 *
	 */
	class RegExpVarReference {
		
		private Variable variable;
		private String refName;
		
		RegExpVarReference(String refName, Variable variable) {
			this.refName = refName;
			this.variable = variable;
		}
		
		public Variable getVariable() {
			return this.variable;
		}
		
		public String getReferenceName() {
			return this.refName;
		}
		
		@Override
		public boolean equals(Object other) {
			if(this == other) {
				return true;
			}
			if(!(other instanceof RegExpVarReference)) {
				return false;
			}
			RegExpVarReference otherVarRef = (RegExpVarReference) other;
			if(this.refName != null) {
				return this.refName.equals(otherVarRef.refName);
			}
			else {
				return otherVarRef.refName == null;
			}
		}
	}
	
	/**
	 * 
	 * An enum that represents values, each of which may be assigned to a BDD variable.
	 *
	 * @see DomainAssignment
	 */
	enum DomainAssignmentValue {TRUE, FALSE, DONTCARE}
	
	/**
	 * 
	 * An inner class that represent an assignment to the BDD variables that encode a given {@link BDDDomain}.
	 *
	 */
	class DomainAssignment {

		private BDDDomain dom;
		private int regExpVarIdx;
		private DomainAssignmentValue[] domAssgn;
		private Map<Integer,Integer> bddVarIdxToPosition;

		DomainAssignment(BDDDomain dom, int regExpVarIdx) {
			this.dom = dom;
			this.regExpVarIdx = regExpVarIdx;
			this.domAssgn = new DomainAssignmentValue[dom.varNum()];
			Arrays.fill(domAssgn, DomainAssignmentValue.DONTCARE); //initialize assignment to DONT CARES

			this.bddVarIdxToPosition = new HashMap<>();
			int[] domVarsIdx = dom.vars();
			for(int i = 0 ; i < dom.varNum(); i++) {
				this.bddVarIdxToPosition.put(domVarsIdx[i], i);
			}
		}

		public void assignValueToBddVar(DomainAssignmentValue value, int bddVarIdx) {
			if(!this.bddVarIdxToPosition.containsKey(bddVarIdx)) {
				throw new RegExpSFAGeneratorException("The BDD variable with the specified index "
						+ bddVarIdx + " does not encode the BDD domain " + this.dom.toString(), BricsRegExpSFAGenerator.this.regExpBuilder.traceId);
			}
			//Assign the given value to the variable
			this.domAssgn[this.bddVarIdxToPosition.get(bddVarIdx)] = value;
		}

		public DomainAssignmentValue getValueOfBddVar(int bddVarIdx) {
			if(!this.bddVarIdxToPosition.containsKey(bddVarIdx)) {
				throw new RegExpSFAGeneratorException("The BDD variable with the specified index "
						+ bddVarIdx + " does not encode the BDD domain " + this.dom.toString(), BricsRegExpSFAGenerator.this.regExpBuilder.traceId);
			}

			return this.domAssgn[this.bddVarIdxToPosition.get(bddVarIdx)];
		}

		public DomainAssignmentValue getValueAtPosition(int position) {
			if(position < 0 || position >= this.domAssgn.length) {
				throw new RegExpSFAGeneratorException("The specified position "
						+ position + " is invalid. The number of BDD variables that encode this' domain is " + this.dom.varNum(), BricsRegExpSFAGenerator.this.regExpBuilder.traceId);
			}
			return this.domAssgn[position];
		}

		public int getDomainBddVarNum() {
			return this.dom.varNum();
		}

		public int[] getDomainBddVarIdx() {
			return this.dom.vars();
		}
		
		/**
		 * Returns the (sorted) set of values in this' domain, each of which is encoded by
		 * this assignment.
		 *   
		 * @return
		 */
		public SortedSet<Integer> getValuesSortedSet() {
			BigInteger newValTrue, newValFalse;
			Set<BigInteger> bigIntegersSet = new HashSet<>(), bigIntegersTmpSet = new HashSet<>();
			bigIntegersSet.add(BigInteger.ZERO);
			for(int i = this.domAssgn.length-1; i >= 0; i--) {
				if(DomainAssignmentValue.TRUE.equals(this.domAssgn[i])) {
					for(BigInteger value : bigIntegersSet) {
						newValTrue = value.add(BigInteger.ONE);
						if(i > 0) {
							newValTrue = newValTrue.shiftLeft(1);
						}
						bigIntegersTmpSet.add(newValTrue);
					}
				}
				else if(DomainAssignmentValue.FALSE.equals(this.domAssgn[i])) {
						for(BigInteger value : bigIntegersSet) {
							if(i > 0) {
								newValFalse = value.shiftLeft(1);
								bigIntegersTmpSet.add(newValFalse);
							}
							else {
								bigIntegersTmpSet.add(value);
							}
						}
				}
				else if(DomainAssignmentValue.DONTCARE.equals(this.domAssgn[i])) {
					for(BigInteger value : bigIntegersSet) {
						newValTrue = value.add(BigInteger.ONE);
						if(i > 0) {
							newValTrue = newValTrue.shiftLeft(1);
							
							newValFalse = value.shiftLeft(1);
							bigIntegersTmpSet.add(newValFalse);
						}
						else {
							bigIntegersTmpSet.add(value);
						}
						bigIntegersTmpSet.add(newValTrue);
					}
				}
				bigIntegersSet.clear();
				bigIntegersSet.addAll(bigIntegersTmpSet);
				bigIntegersTmpSet.clear();	
			}

			int domMinVal = BricsRegExpSFAGenerator.this.fromList.get(this.regExpVarIdx);
			SortedSet<Integer> res = new TreeSet<>();
			for(BigInteger value : bigIntegersSet) {
				res.add(value.intValue()+domMinVal);
			}
			return res;
		}
		
		@Override
		public String toString() {
			return Arrays.toString(this.domAssgn);
		}
	}

	/**
	 * 
	 * An inner class that represents an assignment to the variables that appear in the regular expression of
	 * this generator, each of which is an instance of {@link RegExpVarReference}. 
	 *
	 */
	class VarAssignment {
		
		private int[] varAssg;
		private Integer unicodeEncoding;
		
		VarAssignment(int[] varAssg) {
			this.varAssg = varAssg;
		}
		
		VarAssignment(int[] varAssg, int unicodeEncoding) {
			this.varAssg = varAssg;
			this.unicodeEncoding = unicodeEncoding;
		}
		
		public int getVarValue(RegExpVarReference var) {
			int varIdx;
			if((varIdx = BricsRegExpSFAGenerator.this.regExpVars.indexOf(var)) != -1) {
				//The specified variable occurs in the regular expression of this generator
				return this.varAssg[varIdx];
			}
			return -1;
		}
		
		public int getVarValue(int varIdx) {
			if(varIdx < 0 || varIdx >= this.varAssg.length) {
				return -1;
			}
			return this.varAssg[varIdx];
		}
		
		/**
		 * Returns the Unicode encoding of this variable assignment.
		 * This encoding is the character that represents this assignment
		 * in the alphabet of the Brics automaton constructed by this generator.
		 * 
		 * @return
		 */
		public int getUnicodeEncoding() {
			if(this.unicodeEncoding != null) {
				return this.unicodeEncoding;
			}
			int varAssgUnicode = BricsRegExpSFAGenerator.MIN_UNICODE_CHAR;
			for (int j = this.varAssg.length-1; j>=0 ;j--) {
				varAssgUnicode += ((this.varAssg[j]-BricsRegExpSFAGenerator.this.fromList.get(j)) * BricsRegExpSFAGenerator.this.varAssgUnicodeConvertArr[j]);
			}					
			this.unicodeEncoding = varAssgUnicode;
			return this.unicodeEncoding;
		}
		
		/**
		 * 
		 * Returns the BDD that encodes this variable assignment.
		 * 
		 * @return
		 */
		BDD toBdd() {
			BDD res = Env.TRUE(), currAssignment;
			TypeDef ithVarType;
			String ithVarRefName;
			for (int i = 0; i < BricsRegExpSFAGenerator.this.regExpVars.size(); i++) {
				ithVarType = BricsRegExpSFAGenerator.this.regExpVars.get(i).getVariable().getType();
				ithVarRefName = BricsRegExpSFAGenerator.this.regExpVars.get(i).getReferenceName();
				if (ithVarType.isBoolean()) { //The case of a Boolean variable
					if (this.varAssg[i] == 0) {
						currAssignment = Env.getBDDValue(ithVarRefName, FALSE).id();
					}
					else { //'varAssignment[i] == 1'
						currAssignment = Env.getBDDValue(ithVarRefName, TRUE).id();
					}
				}
				else if (ithVarType.isInteger()) { //The case of an Integer variable
					currAssignment = Env.getBDDValue(ithVarRefName, varAssg[i]).id();					
				}
				else { //The case of an Enum variable
					currAssignment = Env.getBDDValue(ithVarRefName, Env.stringer.elementName(Env.getVar(ithVarRefName).getDomain(), BigInteger.valueOf(varAssg[i]))).id();	
				}
				res.andWith(currAssignment);
			}
			return res;
		}
		
		@Override
		public String toString() {
			StringBuilder resBuilder = new StringBuilder();
			for(int i=0; i < this.varAssg.length; i++) {
				resBuilder.append(BricsRegExpSFAGenerator.this.regExpVars.get(i).getReferenceName());
				resBuilder.append("=");
				resBuilder.append(this.varAssg[i]);
				if(i < this.varAssg.length-1) {
					resBuilder.append(" ");
				}
			}
			return resBuilder.toString();
		}
	}
	
	private VarAssignment decodeVarAssg(int varAssgUnicodeEncoding) {
		if(this.unicodeToVarAssgMapping.containsKey(varAssgUnicodeEncoding)) {
			//Return the cached (decoded) variable assignment
			return this.unicodeToVarAssgMapping.get(varAssgUnicodeEncoding);
		}
		
		int[] varAssg = new int[this.regExpVars.size()];
		int remainingUnicodeValue = varAssgUnicodeEncoding - MIN_UNICODE_CHAR;
		
		for (int i = 0; i < varAssg.length; i++) {
			int currVarValue = this.toList.get(i);
			while (((currVarValue-this.fromList.get(i)) * this.varAssgUnicodeConvertArr[i]) > remainingUnicodeValue) {
				currVarValue--;
			}
			
			varAssg[i] = currVarValue;
			remainingUnicodeValue -= ((varAssg[i]-this.fromList.get(i)) * this.varAssgUnicodeConvertArr[i]);
		}
		
		VarAssignment decodedVarAssg = new VarAssignment(varAssg, varAssgUnicodeEncoding);
		
		//Cache the decoded assignment
		this.unicodeToVarAssgMapping.put(varAssgUnicodeEncoding, decodedVarAssg);
		
		return decodedVarAssg;
	}
		
	/**
	 * 
	 * Nested class that represents a range [CHAR_FROM-CHAR_TO] of Unicode BMP characters.
	 *
	 */
	class UnicodeCharClass {
		
		private int from;
		private int to;
		
		public UnicodeCharClass(int from, int to) {
			this.setFrom(from);
			this.setTo(to);
		}
		
		public int getFrom() {
			return this.from;
		}
		
		public int getTo() {
			return this.to;
		}
		
		public void setFrom(int from) {
			checkCodePointIsBmp(from + MIN_UNICODE_CHAR);
			this.from = from;
		}
		
		public void setTo(int to) {
			checkCodePointIsBmp(to + MIN_UNICODE_CHAR);
			this.to = to;
		}
		
		@Override
		public String toString() { 
			char[] fromCharPair = Character.toChars(this.from + MIN_UNICODE_CHAR);
			String fromStr = new String(fromCharPair);
			if(Character.isISOControl(this.from + MIN_UNICODE_CHAR) || needsEscaping(fromCharPair)) {
				fromStr = "\\" + fromStr;
			}
			if(this.from == this.to) {
				return fromStr;
			}
			char[] toCharPair = Character.toChars(this.to + MIN_UNICODE_CHAR);
			String toStr = new String(toCharPair);
			if(Character.isISOControl(this.to + MIN_UNICODE_CHAR) || needsEscaping(toCharPair)) {
				toStr = "\\" + toStr;
			}
			return fromStr + "-" + toStr;
		}
		
		/**
		 * 
		 * Checks whether the specified unicode code point needs to be escaped, i.e.,
		 * whether it is reserved.
		 * </p>In a string that represents a regular expression of Brics,
		 * there are RESERVED Unicode characters that have special meanings.
		 * In order to use these characters just as any character, one has to escape them (using '\').
		 * 
		 * @param unicodeChar code point either Basic Multilingual Plane or supplementary
		 * @return
		 */
		private boolean needsEscaping(char[] unicodeChar) {
			return unicodeChar.length == 1 && needsEscaping(unicodeChar[0]);	
		}
			
		/**
		 *
		 * Checks whether the specified unicode code point needs to be escaped, i.e.,
		 * whether it is reserved.
		 * </p>In a string that represents a regular expression of Brics,
		 * there are RESERVED Unicode characters that have special meanings.
		 * In order to use these characters just as any character, one has to escape them (using '\').
		 * 
		 * @param bmpUnicodeCodePoint Basic Multilingual Plane code point
		 * @return
		 */
		private boolean needsEscaping(char bmpUnicodeCodePoint) {

			boolean res;

			switch (bmpUnicodeCodePoint) {
			case '|' :
			case ' ':
			case '-':
			case ',':
			case '^':
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
			//case '@' : a symbol for 'any string' - uncomment if added to the enabled syntax of Brics
			case '"' :
			case '(' :
			case ')' :
			//case '<' : symbols for 'numeric interval <m,n>' and 'automaton identifier' -  uncomment if either is added to the enabled syntax of Brics
			//case '>' :
			case '\\' :
				res = true;
				break;
			default :
				res = false;
				break;
			}
			return res;
		}
		
		/**
		 * Checks whether the specified character (Unicode code point) is in the Basic Multilingual Plane (BMP).
		 * Such code points can be represented using a single char and are the only ones supported by Brics.
		 * In case {@code codePoint} is not in the BMP, an {@link RegExpSFAGeneratorException} exception is thrown.
		 * 
		 * @throws RegExpSFAGeneratorException
		 * @param codePoint
		 */
		private void checkCodePointIsBmp(int codePoint) {
			if(!Character.isBmpCodePoint(codePoint)) {
				throw new RegExpSFAGeneratorException("The state " + BricsRegExpSFAGenerator.this.decodeVarAssg(codePoint).toString()
						+ " is translated to the non BMP Unicode code point " + codePoint + System.lineSeparator()
						+ "Since Brics only supports BMP characters, the specified regular expression "
						+  BricsRegExpSFAGenerator.this.regExpBuilder.specRegExp.toString() + " cannot be translated to a Brics automaton",
						BricsRegExpSFAGenerator.this.regExpBuilder.traceId);
			}
		}
	}
	
	/**
	 * Nested class for translating Spectra regular expressions (SpecRegExp) to equivalent ones in Brics.
	 *
	 */
	class BricsRegExpBuilder {

		private SpecRegExp specRegExp; //The Spectra regular expression
		private int traceId; //The trace ID of the constraint where the Spectra regular expression appears
		private boolean minimizeCharClassesNum = true; //If true, then tries to minimize the number of distinct Unicode character ranges generated in each character class (atomic) expression 

		public BricsRegExpBuilder specRegExp(SpecRegExp specRegExp) {
			this.specRegExp = specRegExp;
			return this;
		}
		
		public BricsRegExpBuilder minimizeCharClassesNum(boolean minimize) {
			this.minimizeCharClassesNum = minimize;
			return this;
		}

		public BricsRegExpBuilder traceId(int traceId) {
			this.traceId = traceId;
			return this;
		}

		public RegExp build() {
			//Enable complement  ('~'), intersection ('&'), and the empty language ('#') in the Brics RegExp syntax
			return new RegExp(this.buildBricsRegExp(), RegExp.COMPLEMENT|RegExp.INTERSECTION|RegExp.EMPTY);
		}


		private String buildBricsRegExp() {
			if(this.specRegExp == null) {
				throw new RegExpSFAGeneratorException("The Spectra regular expression of this BricsRegExpBuilder is null and thus cannot be translated to a that of Brics",
						this.traceId);
			}

			if(this.specRegExp.isEmptyString()) { //regExp = '()' 
				return BRICS_EMPTY_STRING;
			}
			if(this.specRegExp.isTrueBooleanConst()) { //regExp = 'TRUE' 
				return createCharClassExpression(new HashMap<>());
			}
			if(this.specRegExp.isFalseBooleanConst()) { //regExp = 'FALSE' 
				return BRICS_EMPTY_LANGUAGE;
			}
			if(this.specRegExp.isVariable()) { //regExp = 'var in {set of values}' 
				String varRefName = this.specRegExp.getVariableReferenceName();
				if(Env.getVar(varRefName) == null) {
					throw new RegExpSFAGeneratorException("Unable to build a Brics regular expression from the SpecRegExp "
							+ this.specRegExp + " since the BDD field " + varRefName + " does not exist", this.traceId);
				}
				SortedSet<Integer> sortedVarValues = new TreeSet<>();
				for(String value : this.specRegExp.getValues()) {
					if(this.specRegExp.isBooleanVariable()) {
						if (TRUE.equalsIgnoreCase(value)) {
							sortedVarValues.add(1);
						}
						else { //FALSE.equalsIgnoreCase(value)
							sortedVarValues.add(0);
						}
					}
					else if(this.specRegExp.isIntegerVariable()) {
						sortedVarValues.add(Integer.valueOf(value).intValue());
					}
					else { //Enum variable - add the ordinal/index of the enum value to 'sortedVarValues'
						int enumValueIdx = Env.stringer.get_module_value_loc(Env.getVar(varRefName).getDomain(), value).intValue();
						if(enumValueIdx == -1) {
							throw new RegExpSFAGeneratorException("Unable to build a Brics regular expression from the SpecRegExp "
									+ this.specRegExp + " since " + value + " is not in the domain of the variable " + varRefName, this.traceId);
						}
						sortedVarValues.add(enumValueIdx);
					}
				}
				Map<Integer, SortedSet<Integer>> varIdxToPermittedValues = new HashMap<>();
				varIdxToPermittedValues.put(BricsRegExpSFAGenerator.this.regExpVars.indexOf(new RegExpVarReference(this.specRegExp.getVariableReferenceName(), this.specRegExp.getVariable())), sortedVarValues);
				return createCharClassExpression(varIdxToPermittedValues);
			}
			if(this.specRegExp.isPredicate()) {
				return createCharClassExpression(this.specRegExp.getPredicate());
			}
			
			if(!this.specRegExp.hasLeft()) {
				throw new RegExpSFAGeneratorException("Unable to build a Brics regular expression from the SpecRegExp "
						+ this.specRegExp + " since its kind is not supported", this.traceId);
			}

			//Perform recursive call on the left sub-expression (might be the only sub expression in case this is an unary expression)
			String leftRegExp = new BricsRegExpBuilder().specRegExp(this.specRegExp.getLeft()).traceId(this.traceId).buildBricsRegExp();

			if(this.specRegExp.isRepetition()) {
				int leastRepNum = this.specRegExp.getQuantifier().getLeastRepetitionsNum();
				switch(this.specRegExp.getQuantifierType()) {
				case AT_LEAST: //'{n,}'
					return "(" + leftRegExp + "){" + leastRepNum + ",}";
				case EXACT_REPETITION: //'{n}'
					return "(" + leftRegExp + "){" + leastRepNum + "}";
				case ONE_OR_MORE: //'+'
					return "(" + leftRegExp + ")+";
				case RANGE: //'{n,m}'
					return "(" + leftRegExp + "){" + leastRepNum + "," + this.specRegExp.getQuantifier().getMaxRepetitionsNum() + "}";
				case ZERO_OR_MORE: //'*'
					return "(" + leftRegExp + ")*";
				case ZERO_OR_ONE: //'?'
					return "(" + leftRegExp + ")?";
				default:
					throw new RegExpSFAGeneratorException("Unable to build a Brics regular expression from the repetition SpecRegExp " + this.specRegExp + " since the quantifier " +
							this.specRegExp.getQuantifierType() + " is not supported", this.traceId);
				}
			}
			
			if(this.specRegExp.isComplementation()) { //'~'
				return "~(" + leftRegExp + ")";
			}

			if(!this.specRegExp.hasRight()) {
				throw new RegExpSFAGeneratorException("Unable to build a Brics regular expression from the unsupported unary SpecRegExp " + this.specRegExp, this.traceId);
			}

			String rightRegExp = new BricsRegExpBuilder().specRegExp(this.specRegExp.getRight()).traceId(this.traceId).buildBricsRegExp();

			if(this.specRegExp.isUnion()) {
				return "(" + leftRegExp + ")" + "|" + "(" + rightRegExp + ")";
			}

			if(this.specRegExp.isIntersection()) {
				return "(" + leftRegExp + ")" + "&" + "(" + rightRegExp + ")";
			}

			if(this.specRegExp.isConcat()) {
				return "(" + leftRegExp + ")(" + rightRegExp + ")";
			}
			throw new RegExpSFAGeneratorException("Unable to build a Brics regular expression from the unsupported binary SpecRegExp " + this.specRegExp, this.traceId);
		}
		
		/**
		 * Returns a string that encodes a Brics character class regular expression, i.e., a string of the form {@code "[Unicode character-Unicode character...]"}.
		 * The generated ranges of Unicode characters represent the set of variable assignments that satisfy the given {@code predicate}.
		 * 
		 * @param predicate the predicate/Boolean term atomic {@link SpecRegExp}
		 * @return
		 */
		private String createCharClassExpression(Spec predicate) {
			/*
			 * 
			 * First, translate the predicate to the Boolean function (BDD) that it represents
			 * 
			 */
			BDD predBdd = BDDGenerator.createBdd(predicate, this.traceId);
						
			if(predBdd.isOne()) {
				//The constant 'TRUE' regular expression
				predBdd.free();
				return createCharClassExpression(new HashMap<>());
			}
			
			Map<Integer, DomainAssignment> regExpVarIdxToDomAssgn = new HashMap<>();
			Map<Integer, DomainAssignment> supportBddVarIdxToDomAssgn = new HashMap<>();
			/*
			 * regExpVarIdxToDomAssgn maps regexp variable indices to assignments to the BDD variables that encode the domains of those variables
			 * 
			 * supportBddVarIdxToDomAssgn maps BDD variables' indices to assignments to the domains that those BDD variables encode
			 */
			fillDomAssgnMapsRestrictPredBddToDoms(predBdd, regExpVarIdxToDomAssgn, supportBddVarIdxToDomAssgn);
		
			if(predBdd.isZero()) {
				//The constant 'FALSE' regular expression
				predBdd.free();
				return BRICS_EMPTY_LANGUAGE;
			}
			
			Set<UnicodeCharClass> bddCharClasses = createBddCharClasses(predBdd, regExpVarIdxToDomAssgn, supportBddVarIdxToDomAssgn);
			predBdd.free();
			
			StringBuilder resBuilder = new StringBuilder("[");
			
			for(UnicodeCharClass charClass : bddCharClasses) {
				resBuilder.append(charClass.toString());
			}
			
			resBuilder.append("]");

			return resBuilder.toString();
		}
		
		/**
		 * @see #bddCharClassesRec
		 * @param predBdd the transition predicate
		 * @param regExpVarIdxToDomAssgn
		 * @param supportBddVarIdxToDomAssgn
		 * @return A set of Unicode BMP character ranges that correspond to a Disjoint Sum of Products (DSOP) representation of {@code predBdd}
		 */
		private Set<UnicodeCharClass> createBddCharClasses(BDD predBdd, Map<Integer, DomainAssignment> regExpVarIdxToDomAssgn,
				Map<Integer, DomainAssignment> supportBddVarIdxToDomAssgn) {
			Set<UnicodeCharClass> bddCharClasses = new HashSet<>();
			bddCharClassesRec(predBdd, regExpVarIdxToDomAssgn, supportBddVarIdxToDomAssgn, bddCharClasses, new HashSet<>());
			return bddCharClasses;
		}
		
		/**
		 * Traverses all the paths {@code p} that end with a '1' terminal node in the specified BDD {@code currBddVar}. The traversal is done in a DFS fashion.
		 * Each such a path {@code p} corresponds to one or more states or assignments that {@code currBddVar} encodes. When {@code p} is found,
		 * this method translates {@code p} to a range of Unicode BMP characters. Each Unicode character uniquely represents an assignment.
		 * All the ranges of Unicode characters are added to {@code bddCharClasses}. Note that two different paths that end with a '1' terminal node,
		 * represent two disjoint sets of assignments as they disagree on the Boolean value assigned to a BDD variable.  
		 * As a result, {@code bddCharClasses} represents {@code currBddVar} as a Disjoint Sum of Products (DSOP), each of which is encoded by a Unicode character.
		 *   
		 * 
		 * @param currBddVar the subBDD traversed
		 * @param regExpVarIdxToDomAssgn maps regexp variable indices to assignments to the BDD variables that encode the domains of those variables
		 * @param supportBddVarIdxToDomAssgn maps BDD variables' indices (in the support of {@code currBddVar}) to assignments to the domains that those BDD variables encode
		 * @param bddCharClasses an empty mutable set that will be filled with elements
		 * @param currBddCharClasses an empty mutable set
		 */
		private void bddCharClassesRec(BDD currBddVar, Map<Integer, DomainAssignment> regExpVarIdxToDomAssgn,
				Map<Integer, DomainAssignment> supportBddVarIdxToDomAssgn, Set<UnicodeCharClass> bddCharClasses, Set<UnicodeCharClass> currBddCharClasses) {
			if(currBddVar.isOne()) {
				Map<Integer, SortedSet<Integer>> varIdxToPermittedValues = new HashMap<>();
				for(int regExpVarIdx : regExpVarIdxToDomAssgn.keySet()) {
					varIdxToPermittedValues.put(regExpVarIdx, regExpVarIdxToDomAssgn.get(regExpVarIdx).getValuesSortedSet());
				}
				charClassesRec(varIdxToPermittedValues, BricsRegExpSFAGenerator.this.regExpVars.size()-1, currBddCharClasses);
				if(this.minimizeCharClassesNum) {
					addCurrBddCharClasses(bddCharClasses, currBddCharClasses);
				}
				else {
					bddCharClasses.addAll(currBddCharClasses);
					currBddCharClasses.clear();
				}
			}
			else {
				BDD lowChild = currBddVar.low();
				if(!lowChild.isZero()) {
					supportBddVarIdxToDomAssgn.get(currBddVar.var()).assignValueToBddVar(DomainAssignmentValue.FALSE, currBddVar.var());
					bddCharClassesRec(lowChild, regExpVarIdxToDomAssgn, supportBddVarIdxToDomAssgn, bddCharClasses, currBddCharClasses);
				}
				lowChild.free();
				
				BDD highChild = currBddVar.high();
				if(!highChild.isZero()) {
					supportBddVarIdxToDomAssgn.get(currBddVar.var()).assignValueToBddVar(DomainAssignmentValue.TRUE, currBddVar.var());
					bddCharClassesRec(highChild, regExpVarIdxToDomAssgn, supportBddVarIdxToDomAssgn, bddCharClasses, currBddCharClasses);
				}
				highChild.free();
				
				supportBddVarIdxToDomAssgn.get(currBddVar.var()).assignValueToBddVar(DomainAssignmentValue.DONTCARE, currBddVar.var());
			}
		}
		
		/**
		 * Adds the new character classes in {@code currBddCharClasses} to {@code bddCharClasses}.
		 * To minimize the number of distinct character classes/ranges (i.e., the size of {@code bddCharClasses}), this method tries to merge each new character range
		 * with existing character ranges in {@code bddCharClasses}. 
		 * 
		 * @param bddCharClasses changed in place
		 * @param currBddCharClasses all of its elements are removed
		 */
		private void addCurrBddCharClasses(Set<UnicodeCharClass> bddCharClasses,
				Set<UnicodeCharClass> currBddCharClasses) {
			//Try to merge the new character classes with the existing ones
			boolean foundLowerCharClassToMerge, foundHigherCharClassToMerge;
			Set<UnicodeCharClass> remainingCurrBddCharClasses = new HashSet<>(currBddCharClasses);
			UnicodeCharClass lowerOldCharClass = null, higherOldCharClass = null;
			for(UnicodeCharClass newCharClass : currBddCharClasses) {
				foundLowerCharClassToMerge = false;
				foundHigherCharClassToMerge = false;
				for(UnicodeCharClass oldCharClass : bddCharClasses) {
					if(oldCharClass.getFrom() == newCharClass.getTo()+1) {
						higherOldCharClass = oldCharClass;
						foundHigherCharClassToMerge = true;
					}
					else if(oldCharClass.getTo()+1 == newCharClass.getFrom()) {
						lowerOldCharClass = oldCharClass;
						foundLowerCharClassToMerge = true;
					}
				}
				if(foundHigherCharClassToMerge && !foundLowerCharClassToMerge) {
					higherOldCharClass.setFrom(newCharClass.getFrom());
				}
				else if(foundLowerCharClassToMerge && !foundHigherCharClassToMerge) {
					lowerOldCharClass.setTo(newCharClass.getTo());
				}
				else if(foundLowerCharClassToMerge && foundHigherCharClassToMerge) {					
					bddCharClasses.remove(higherOldCharClass);
					bddCharClasses.remove(lowerOldCharClass);
					bddCharClasses.add(new UnicodeCharClass(lowerOldCharClass.getFrom(), higherOldCharClass.getTo()));
				}
				if(foundLowerCharClassToMerge || foundHigherCharClassToMerge) {
					remainingCurrBddCharClasses.remove(newCharClass);
				}
			}
			bddCharClasses.addAll(remainingCurrBddCharClasses);
			currBddCharClasses.clear();
		}
		
		/**
		 * Fills the given empty maps {@code regExpVarIdxToDomAssgn} and {@code supportBddVarIdxToDomAssgn}.
		 * Restricts {@code predBdd} with the domain information/constraints of the {@link BDDDomain}s in the support of {@code predBdd}.
		 * 
		 * <p>After this method returns, {@code regExpVarIdxToDomAssgn}
		 * maps regexp variable indices to assignments to the BDD variables that encode the domains of those variables,
		 * and {@code supportBddVarIdxToDomAssgn} maps BDD variables' indices to assignments to the domains that those BDD variables encode.
		 * The keys of both maps only consist of variables in the support of {@code predBdd}.</p>
		 * 
		 * @param predBdd
		 * @param regExpVarIdxToDomAssgn an empty map that will be filled with entries
		 * @param supportBddVarIdxToDomAssgn an empty map that will be filled with entries
		 */
		private void fillDomAssgnMapsRestrictPredBddToDoms(BDD predBdd, Map<Integer, DomainAssignment> regExpVarIdxToDomAssgn,
				Map<Integer, DomainAssignment> supportBddVarIdxToDomAssgn) {
			int[] predSupport = predBdd.support().toArray();
			RegExpVarReference regExpVar;
			BDDDomain regExpVarDom;
			int[] regExpVarDomVars;
			boolean foundRegExpVar;
			DomainAssignment regExpVarDomAssgn;
			for(int i = 0; i < predSupport.length; i++) {
				if(supportBddVarIdxToDomAssgn.containsKey(predSupport[i])) {
					continue;
				}
				foundRegExpVar = false;
				for(int j = 0; j < BricsRegExpSFAGenerator.this.regExpVars.size() && !foundRegExpVar; j++) {
					if(regExpVarIdxToDomAssgn.containsKey(j)) {
						continue;
					}
					regExpVar = BricsRegExpSFAGenerator.this.regExpVars.get(j);
					regExpVarDom = Env.getVar(regExpVar.getReferenceName()).getDomain();
					regExpVarDomVars = regExpVarDom.vars();
					for(int k = 0; k < regExpVarDomVars.length && !foundRegExpVar; k++) {
						if(regExpVarDomVars[k] == predSupport[i]) {
							regExpVarIdxToDomAssgn.put(j, new DomainAssignment(regExpVarDom, j));
							predBdd.andWith(regExpVarDom.domain());
							foundRegExpVar = true;
						}
					}
					if(foundRegExpVar) {
						regExpVarDomAssgn = regExpVarIdxToDomAssgn.get(j);
						for(int k = 0; k < regExpVarDomVars.length; k++) {
							supportBddVarIdxToDomAssgn.put(regExpVarDomVars[k], regExpVarDomAssgn);
						}
					}
				}
				if(!foundRegExpVar) {
					throw new RegExpSFAGeneratorException("Could not find the BDDDomain that the BDD variable of index " + i + " encodes", this.traceId);
				}
			}
		}
			
		/**
		 * Returns a string that encodes a Brics character class regular expression, i.e., a string of the form {@code "[Unicode character-Unicode character...]"}.
		 * The generated ranges of Unicode characters represent the set of variable assignments defined by the given 'var in {set of values}' atomic SpecRegExp, which is specified by {@code varIdxToPermittedValues}.
		 * 
		 * @param varIdxToPermittedValues 'var in {set of values}' atomic SpecRegExp represented by mapping the index of the variable var
		 * to the {set of values}. If {@code varIdxToPermittedValues} is empty, then it represents the 'TRUE' regular expression.
		 *
		 * @return
		 */
		private String createCharClassExpression(Map<Integer, SortedSet<Integer>> varIdxToPermittedValues) {
			Set<UnicodeCharClass> charClasses = new HashSet<>();
			if(!BricsRegExpSFAGenerator.this.regExpVars.isEmpty()) {
				charClassesRec(varIdxToPermittedValues, BricsRegExpSFAGenerator.this.regExpVars.size()-1, charClasses);
			}
			else {
				charClasses.add(new UnicodeCharClass(MIN_UNICODE_CHAR, MIN_UNICODE_CHAR));
			}
			
			StringBuilder resBuilder = new StringBuilder("[");
			
			for(UnicodeCharClass charClass : charClasses) {
				resBuilder.append(charClass.toString());
			}
			
			resBuilder.append("]");

			return resBuilder.toString();
		}
		
		/**
		 * Recursively fills the set {@code charClasses} with the (BMP) Unicode character ranges that encode the specified 'var in {set of values}' atomic SpecRegExp that
		 * {@code varIdxToPermittedValues} represents.
		 * 
		 * @param varIdxToPermittedValues 'var in {set of values}' atomic SpecRegExp represented by mapping the index of the variable var
		 * to the {set of values}
		 * @param varIdx the index of the variable currently under consideration
		 * @param charClasses
		 */
		private void charClassesRec(Map<Integer, SortedSet<Integer>> varIdxToPermittedValues, int varIdx, Set<UnicodeCharClass> charClasses) { 
			SortedSet<Integer> varPermittedValues;	
			if(varIdxToPermittedValues.containsKey(varIdx)) { //The current variable cannot be assigned all the values in its domain
				varPermittedValues = varIdxToPermittedValues.get(varIdx);
			}
			else { //The current variable can assigned all the values in its domain
				varPermittedValues = IntStream.rangeClosed(BricsRegExpSFAGenerator.this.fromList.get(varIdx), BricsRegExpSFAGenerator.this.toList.get(varIdx)).boxed().collect(Collectors.toCollection(TreeSet:: new));
			}
			if(varIdx == BricsRegExpSFAGenerator.this.regExpVars.size()-1) { //The least significant variable (i.e., the base case of the recursion)
				int elementOrd = 0;
				UnicodeCharClass prevCharClass = null;
				for(int value : varPermittedValues) {
					if(elementOrd > 0 && (prevCharClass.getTo() + 1) == (value - BricsRegExpSFAGenerator.this.fromList.get(varIdx))) {
						//Extend the previous interval by incrementing its upper bound
						prevCharClass.setTo(value - BricsRegExpSFAGenerator.this.fromList.get(varIdx));
					}
					else {
						//Create a new interval of the form '[value,value]', and add it to the recursion memory
						prevCharClass = new UnicodeCharClass(value - BricsRegExpSFAGenerator.this.fromList.get(varIdx), value - BricsRegExpSFAGenerator.this.fromList.get(varIdx));
						charClasses.add(prevCharClass);
					}
					elementOrd++;
				}
			}
			else { //varIdx < this.regExpVars.size()-1
				UnicodeCharClass extendableCharClass = null, newCharClass; //An interval whose upper bound may be extended to a strictly greater one when the iteration reaches the next value of the current variable 
				Set<UnicodeCharClass> nextCharClasses = new HashSet<>();
				for(int value : varPermittedValues) {
					for(UnicodeCharClass charClass : charClasses) {
						int weightedValue = (value - BricsRegExpSFAGenerator.this.fromList.get(varIdx)) * BricsRegExpSFAGenerator.this.varAssgUnicodeConvertArr[varIdx];
						newCharClass = new UnicodeCharClass(charClass.getFrom() + weightedValue, charClass.getTo() + weightedValue);
						if(extendableCharClass != null && ((extendableCharClass.getTo() + 1) == newCharClass.getFrom())) {
							extendableCharClass.setTo(newCharClass.getTo()); //Merge the new char class with the extendable one
						}
						else {
							nextCharClasses.add(newCharClass); //Cannot extend an existing char class, thus add the new one to the set
							if(newCharClass.getTo() == weightedValue + (BricsRegExpSFAGenerator.this.varAssgUnicodeConvertArr[varIdx]-1)) { //The newly added char class may be extended when reaching the (next) iteration of the successive value
								extendableCharClass = newCharClass;
							}	
						}
					}
				}

				charClasses.clear();
				charClasses.addAll(nextCharClasses);
			}
			if(varIdx > 0) {
				charClassesRec(varIdxToPermittedValues, --varIdx, charClasses);
			}
		}

	}
}
