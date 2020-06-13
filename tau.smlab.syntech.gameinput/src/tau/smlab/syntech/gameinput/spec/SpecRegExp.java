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

package tau.smlab.syntech.gameinput.spec;

import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;

/**
 * A class that represents a regular expression.
 * 
 * 
 * @author Or Pistiner
 * @author Gal Amram
 * 
 */

public class SpecRegExp implements Spec {

	/**
	 * 
	 */
	private static final long serialVersionUID = 5557869841294255345L;
	
	private static final String FALSE = "false";
	private static final String TRUE = "true";

	private SpecRegExp left; //the left (resp. only) sub regular expression in case this is a binary (resp. an unary) expression
	private SpecRegExp right; //the right sub regular expression in case this is a binary expression
	private List<String> values; //if this is a variable or a Boolean constant regular expression, these are the values this expression accepts
	private VariableReference varRef; //if this is a variable regular expression, that is the variable reference that appears in this expression
	private Spec predicate; //if this is a Boolean term (formula/predicate) regular expression, that is the predicate that appears in the expression
	private RegExpKind kind; //the kind (i.e., either the main operator or the base kind) of this regular expression
	private RegExpQuantifier quantifier; //if this is a 'REPEAT' unary regular expression, that is its quantifier

	public enum RegExpKind {

		UNION(true, false), INTERSECTION(true, false), CONCAT(true, false), REPEAT(false, false), COMPLEMENT(false, false), EMPTY_STR(false, true),
		VAR(false, true), PREDICATE(false, true), BOOLEAN_CONST(false, true);
		
		
		private static final List<RegExpKind> NULLARY_KINDS =
				Collections.unmodifiableList(Arrays.stream(RegExpKind.values()).filter(RegExpKind::isNullary).collect(Collectors.toList()));
		private static final List<RegExpKind> UNARY_KINDS = 
				Collections.unmodifiableList(Arrays.stream(RegExpKind.values()).filter(RegExpKind::isUnary).collect(Collectors.toList()));
		private static final List<RegExpKind> BINARY_KINDS = 
				Collections.unmodifiableList(Arrays.stream(RegExpKind.values()).filter(RegExpKind::isBinary).collect(Collectors.toList()));
		
		private boolean isNullary; //true if this kind of regular expression is nullary
		private boolean isBinary; //true if this kind of regular expression is binary
		
		private RegExpKind(boolean isBinary, boolean isNullary) {
			this.isNullary = isNullary;
			this.isBinary = isBinary;
		}

		public boolean isNullary() {
			return this.isNullary;
		}

		public boolean isBinary() {
			return this.isBinary;
		}

		public boolean isUnary() {
			return !this.isBinary && !this.isNullary;
		}
		
		public static List<RegExpKind> nullaryKinds() {
			return NULLARY_KINDS;
		}
		
		public static List<RegExpKind> unaryKinds() {
			return UNARY_KINDS;
		}
		
		public static List<RegExpKind> binaryKinds() {
			return BINARY_KINDS;
		}
		
	}

	/**
	 * 
	 * An enum class that represents types of regular expressions' quantifier operators.
	 *
	 */
	public enum QuantifierType {

		ZERO_OR_MORE, ONE_OR_MORE, ZERO_OR_ONE, EXACT_REPETITION,
		AT_LEAST, RANGE;
	};

	/**
	 * A class that represents regular expressions' quantifier operators.
	 *
	 */
	public static class RegExpQuantifier implements Spec {
		/**
		 * 
		 */
		private static final long serialVersionUID = 3852513999102523898L;
		
		private QuantifierType type;
		private int least, most;


		private RegExpQuantifier(QuantifierType type, int least, int most) {
			this.type = type;
			this.least = least;
			this.most = most;
		}

		public QuantifierType getType() {
			return this.type;
		}

		public int getLeastRepetitionsNum() {
			return this.least;
		}

		public int getMaxRepetitionsNum() {
			return this.most;
		}

		public void setLeastRepetitionsNum(int least) {
			this.least = least;
		}

		public void setMaxRepetitionsNum(int most) {
			this.most = most;
		}

		/**
		 * 
		 * @return Kleene star '*' quantifier 
		 */
		public static RegExpQuantifier zeroOrMoreRepetitions() {
			return new RegExpQuantifier(QuantifierType.ZERO_OR_MORE, 0, Integer.MAX_VALUE);
		}

		/**
		 * 
		 * @return '+' quantifier
		 */
		public static RegExpQuantifier oneOrMoreRepetitions() {
			return new RegExpQuantifier(QuantifierType.ONE_OR_MORE, 1, Integer.MAX_VALUE);
		}

		/**
		 * 
		 * @return '?' quantifier
		 */
		public static RegExpQuantifier zeroOrOneRepetition() {
			return new RegExpQuantifier(QuantifierType.ZERO_OR_ONE, 0, 1);
		}

		/**
		 * 
		 * @param exactNum the number of occurrences/repetitions
		 * @return '{{@code exactNum}}' quantifier
		 */
		public static RegExpQuantifier exactRepetitions(int exactNum) {
			return new RegExpQuantifier(QuantifierType.EXACT_REPETITION, exactNum, exactNum);
		}

		/**
		 * 
		 * @param atLeastNum the least number of occurrences/repetitions
		 * @return '{{@code atLeastNum},}' quantifier
		 */
		public static RegExpQuantifier atLeastRepetitions(int atLeastNum) {
			return new RegExpQuantifier(QuantifierType.AT_LEAST, atLeastNum, Integer.MAX_VALUE);
		}

		/**
		 * 
		 * @param atLeastNum the least number of occurrences/repetitions
		 * @param atMostNum the maximal number of occurrences/repetitions (inclusive)
		 * @return  '{{@code atLeastNum},{@code atMostNum}}' quantifier
		 */
		public static RegExpQuantifier repetitionsInRange(int atLeastNum, int atMostNum) {
			return new RegExpQuantifier(QuantifierType.RANGE, atLeastNum, atMostNum);
		}

		@Override
		public String toString() {
			switch(this.type) {
			case AT_LEAST:
				return "{" + this.least + ",}";
			case EXACT_REPETITION:
				return "{" + this.least + "}";
			case ONE_OR_MORE:
				return "+";
			case RANGE:
				return "{" + this.least + "," + this.most + "}";
			case ZERO_OR_MORE:
				return "*";
			case ZERO_OR_ONE:
				return "?";
			default:
				break;			
			}
			return "INVALID QUANTIFIER TYPE";
		}


		@Override
		public boolean equals(Object obj) {
			if (this == obj) {
				return true;
			}
			if (obj == null) {
				return false;
			}
			if (this.getClass() != obj.getClass()) {
				return false;
			}
			RegExpQuantifier other = (RegExpQuantifier) obj;
			if(this.type == null) {
				if(other.type != null) {
					return false;
				}
				return true; //both quantifiers are of type null;
			}
			else {
				if(!this.type.equals(other.type)) {
					return false;
				}
				//both quantifiers have the same type
				return this.least == other.least &&
						this.most == other.most;
			}
		}

		@Override
		public boolean isPastLTLSpec() {
			return false;
		}

		@Override
		public boolean isPropSpec() {
			return false;
		}

		@Override
		public boolean hasTemporalOperators() {
			return false;
		}

		@Override
		public RegExpQuantifier clone() throws CloneNotSupportedException {
			return new RegExpQuantifier(this.type, this.least, this.most);
		}
	}

	/**
	 * 
	 * Use a {@link RandomGenerator} to randomly generate
	 * instances of {@link SpecRegExp}. 
	 * 
	 * 
	 * @author Or Pistiner
	 *
	 */
	public static class RandomGenerator  {
		
		private static final boolean GENERATE_PREDICATE_KIND = true;
		
		private static final List<RegExpKind> REGEXP_NULLARY_KINDS = GENERATE_PREDICATE_KIND ? RegExpKind.nullaryKinds() : Arrays.asList(new RegExpKind[] {RegExpKind.EMPTY_STR, RegExpKind.BOOLEAN_CONST, RegExpKind.VAR});
		private static final int NULLARY_KIND_NUM = REGEXP_NULLARY_KINDS.size();

		private static final List<RegExpKind> REGEXP_KINDS = initRegExpKinds();
		private static final int REGEXP_KIND_NUM = REGEXP_KINDS.size();

		private static final List<QuantifierType> QUANTIFIER_TYPES = Arrays.asList(QuantifierType.values());
		private static final int QUANTIFIER_TYPE_NUM = QUANTIFIER_TYPES.size();
		
		enum SpecKind {INEQ_OP(false), EQ_OP(false), ARITH_OP(false), PROP_OP(false), INT_VAR(true), INT_CONST(true), BOOLEAN_VAR(true), BOOLEAN_CONST(true);
			
			private static Operator[] propOps = {Operator.AND, Operator.OR, Operator.IFF, Operator.IMPLIES, Operator.NOT, Operator.XOR};
			private static Operator[] arithOps = {Operator.ADD, Operator.SUBSTRACT, Operator.MULTIPLY};
			private static Operator[] arithOpsWithMod = {Operator.ADD, Operator.SUBSTRACT, Operator.MOD, Operator.MULTIPLY};
			private static Operator[] ineqOps = Operator.inqOp;
			
			private boolean isNullary;
			
			private SpecKind(boolean isNullary) {
				this.isNullary = isNullary;
			}
			
			public boolean isNullary() {
				return this.isNullary;
			}
			
			public static Operator[] getOps(SpecKind opKind) {
				return getOps(opKind, false);
			}
			
			public static Operator[] getOps(SpecKind opKind, boolean excludeMod) {
				if(INEQ_OP.equals(opKind)) {
					return ineqOps;
				}
				if(ARITH_OP.equals(opKind)) {
					return excludeMod ? arithOps : arithOpsWithMod;
				}
				if(PROP_OP.equals(opKind)) {
					return propOps;
				}
				return null;
			}
		}
		
		private final List<Variable> variables, booleanVariables, intVariables, nonZeroIntVariables, enumVariables;
		private final List<SpecKind> propSpecKinds, nullaryPropSpecKinds, arithSpecKinds, nullaryArithSpecKinds;
		
		private final Random random;
		private int maxDepth, predMaxDepth, maxLeastRepNum, maxRepRange, maxIntConst;
	
		private static List<RegExpKind> initRegExpKinds() {
			List<RegExpKind> regExpKinds = new ArrayList<>();

			regExpKinds.addAll(RegExpKind.unaryKinds());
			regExpKinds.addAll(RegExpKind.binaryKinds());
			regExpKinds.addAll(REGEXP_NULLARY_KINDS);

			return regExpKinds;	
		}

		/**
		 * Returns a new {@link RandomGenerator} that randomly generates
		 * instances of {@link SpecRegExp}, which may have atomic 
		 * subexpressions that exclusively use the variables in {@code variables}.
		 * 
		 * @param variables
		 * @param maxDepth the maximal depth of the generated regular expressions
		 * @param predMaxDepth the maximal depth of each predicate that appears in the generated regular expression
		 * @param maxLeastRepNum the maximal value that may appear in repetition quantifiers as the least number of repetitions
		 * @param maxRepRange the maximal interval size that "repetitions in range" {@link QuantifierType#RANGE} quantifiers may have 
		 */		
		public RandomGenerator(int maxDepth, int predMaxDepth, List<Variable> variables,
				int maxLeastRepNum, int maxRepRange) {
			this.variables = variables;
			
			this.booleanVariables = variables.stream().filter(var -> var.getType().isBoolean()).collect(Collectors.toList());
			this.intVariables = variables.stream().filter(var -> var.getType().isInteger()).collect(Collectors.toList());
			this.nonZeroIntVariables = variables.stream().filter(var -> (var.getType().isInteger() && var.getType().getLower() > 0)).collect(Collectors.toList());
			this.enumVariables = variables.stream().filter(var -> (!var.getType().isInteger() && !var.getType().isBoolean())).collect(Collectors.toList());
			
			this.propSpecKinds = new ArrayList<>();
			this.arithSpecKinds = new ArrayList<>();
			this.nullaryPropSpecKinds = new ArrayList<>();
			this.nullaryArithSpecKinds = new ArrayList<>();
			this.fillSpecKindsLists();
			this.maxIntConst = getMaxIntConstValue();
			
			this.random = new Random();
			this.maxDepth = maxDepth;
			this.predMaxDepth = predMaxDepth;
			this.maxLeastRepNum = maxLeastRepNum;
			this.maxRepRange = maxRepRange;
		}
		
		private int getMaxIntConstValue() {
			if(this.intVariables.isEmpty()) {return 0;}
			int minMaxUpperValue = this.intVariables.get(0).getType().getUpper();
			for(int i = 1; i < this.intVariables.size(); i++) {
				if(this.intVariables.get(i).getType().getUpper() < minMaxUpperValue) {
					minMaxUpperValue = this.intVariables.get(i).getType().getUpper();
				}
			}
			return minMaxUpperValue;
		}
		
		private void fillSpecKindsLists() {
			this.propSpecKinds.add(SpecKind.BOOLEAN_CONST);
			this.nullaryPropSpecKinds.add(SpecKind.BOOLEAN_CONST);
			
			if(!this.enumVariables.isEmpty() || !this.intVariables.isEmpty() || !this.booleanVariables.isEmpty()) {
				this.propSpecKinds.add(SpecKind.PROP_OP);
			}
			if(!this.enumVariables.isEmpty() || !this.intVariables.isEmpty()) {
				this.propSpecKinds.add(SpecKind.EQ_OP);
			}
			if(!this.intVariables.isEmpty()) {
				this.propSpecKinds.add(SpecKind.INEQ_OP);
				
				this.arithSpecKinds.add(SpecKind.INT_CONST);
				this.nullaryArithSpecKinds.add(SpecKind.INT_CONST); 
				this.arithSpecKinds.add(SpecKind.INT_VAR);
				this.nullaryArithSpecKinds.add(SpecKind.INT_VAR); 
				this.arithSpecKinds.add(SpecKind.ARITH_OP);
			}
			if(!this.booleanVariables.isEmpty()) {
				this.propSpecKinds.add(SpecKind.BOOLEAN_VAR);
				this.nullaryPropSpecKinds.add(SpecKind.BOOLEAN_VAR);
			}
		}

		public SpecRegExp nextSpecRegExp() {
			SpecRegExp res = this.genRandRegExp(this.maxDepth);
			if(res == null) {
				System.out.println("null regexp");
			}
			return res;
		}

		public List<Variable> getVariables() {
			return variables;
		}

		public int getMaxDepth() {
			return maxDepth;
		}

		public void setMaxDepth(int maxDepth) {
			this.maxDepth = maxDepth;
		}

		public int getMaxLeastRepNum() {
			return maxLeastRepNum;
		}

		public void setMaxLeastRepNum(int maxLeastRepNum) {
			this.maxLeastRepNum = maxLeastRepNum;
		}

		public int getMaxRepRange() {
			return maxRepRange;
		}

		public void setMaxRepRange(int maxRepRange) {
			this.maxRepRange = maxRepRange;
		}

		private SpecRegExp genRandRegExp(int maxDepth) {
			if(maxDepth == 0) {
				return genRandNullaryRegExp(REGEXP_NULLARY_KINDS.get(this.random.nextInt(NULLARY_KIND_NUM)));
			}
			RegExpKind regExpKind = REGEXP_KINDS.get(this.random.nextInt(REGEXP_KIND_NUM));
			if(regExpKind.isNullary()) {
				return genRandNullaryRegExp(regExpKind);
			}
			if(regExpKind.isUnary()) {
				return genRandUnaryRegExp(maxDepth, regExpKind);
			}
			if(regExpKind.isBinary()) {
				return genRandBinaryRegExp(maxDepth, regExpKind);
			}
			throw new RuntimeException("The regexp kind " + regExpKind + " is unsupported");
		}

		private SpecRegExp genRandNullaryRegExp(RegExpKind nullaryKind) {
			switch(nullaryKind) {
			case EMPTY_STR:
				return SpecRegExp.newEmptyStringRegExp();
			case BOOLEAN_CONST:
				return SpecRegExp.newBooleanConstRegExp(this.random.nextBoolean());
			case VAR:
				VariableReference varRef = new VariableReference(variables.get(this.random.nextInt(this.variables.size())));
				TypeDef variableType = varRef.getVariable().getType();
				List<String> values = new ArrayList<>();
				int varDomainSize;
				if(variableType.isBoolean()) {
					varDomainSize = 2;
					values.add(String.valueOf(this.random.nextBoolean()));
				}
				else if(variableType.isInteger()) {
					for(int value = variableType.getLower(); value <= variableType.getUpper(); value++) {
						if(this.random.nextBoolean()) {
							values.add(String.valueOf(value));
						}
					}
					varDomainSize = variableType.getUpper()-variableType.getLower()+1;
				}
				else { //Enum
					for(String value : variableType.getValues()) {
						if(this.random.nextBoolean()) {
							values.add(value);
						}
					}
					varDomainSize = variableType.getValues().size();
				}
				return SpecRegExp.newVariableRegExp(varRef, values, getAcceptValues(values, varDomainSize));
			case PREDICATE:
				return SpecRegExp.newPredicateRegExp(genRandPredicate(this.predMaxDepth));
			default:
				throw new RuntimeException("The nullary regexp kind " + nullaryKind + " is unsupported");
			}
		}
		
		private Spec genRandPredicate(int predMaxDepth) {
			SpecKind propKind = predMaxDepth > 0 ? this.propSpecKinds.get(this.random.nextInt(this.propSpecKinds.size())) :
				this.nullaryPropSpecKinds.get(this.random.nextInt(this.nullaryPropSpecKinds.size()));
			if(propKind.isNullary()) { //predMaxDepth >= 0
				switch(propKind) {
				case BOOLEAN_CONST:
					return new PrimitiveValue(this.random.nextBoolean() ? TRUE : FALSE);
				case BOOLEAN_VAR:
					return new VariableReference(this.booleanVariables.get(this.random.nextInt(this.booleanVariables.size())));
				default:
					throw new RuntimeException("The nullary propositional spec kind " + propKind + " is unsupported");
				}
			}
			else { //predMaxDepth > 0
				switch(propKind) {
				case EQ_OP:
					boolean enumComparison = this.intVariables.isEmpty() || (!this.enumVariables.isEmpty() && this.random.nextBoolean());
					if(enumComparison) { //Create a spec of the form 'enumVar = enumConstantValue'
						Variable enumVariable = this.enumVariables.get(this.random.nextInt(this.enumVariables.size()));
						String enumValue = enumVariable.getType().getValues().get(this.random.nextInt(enumVariable.getType().getValues().size()));
						return new SpecExp(Operator.EQUALS, new VariableReference(enumVariable), new PrimitiveValue(enumValue));
					}
					else { //Integer comparison: create a spec of the form 'arithmetic expression = integer constant'
						Spec leftArithExp = genRandArithmeticSpec(predMaxDepth-1, false);
						int randValue = randEvalArithSpec(leftArithExp); //choose a random value to which the LHS can evaluate
						return new SpecExp(Operator.EQUALS, leftArithExp, new PrimitiveValue(randValue));
					}				
				case INEQ_OP: //Integer comparison: create a spec of the form 'arithmetic expression (> | < | >= | <=) integer constant'
					Operator ineqOp = SpecKind.getOps(SpecKind.INEQ_OP)[this.random.nextInt(SpecKind.getOps(SpecKind.INEQ_OP).length)];
					Spec leftArithExp = genRandArithmeticSpec(predMaxDepth-1, false);
					int randValue = randEvalArithSpec(leftArithExp); //choose a random value to which the LHS can evaluate
					return new SpecExp(ineqOp, leftArithExp, new PrimitiveValue(randValue));
				case PROP_OP:
					Operator propOp = SpecKind.getOps(SpecKind.PROP_OP)[this.random.nextInt(SpecKind.getOps(SpecKind.PROP_OP).length)];
					Spec leftPropSpec = genRandPredicate(predMaxDepth-1);
					if(propOp.isBinary()) {
						return new SpecExp(propOp, leftPropSpec, genRandPredicate(predMaxDepth-1));
					}
					//Unary propositional operator
					return new SpecExp(propOp, leftPropSpec);
				default:
					throw new RuntimeException("The propositional spec operator kind " + propKind + " is unsupported");
				}
			}
		}
		
		private Spec genRandArithmeticSpec(int predMaxDepth, boolean modOpDivisor) {
			SpecKind arithKind;
			if(modOpDivisor && this.nonZeroIntVariables.isEmpty()) {
				arithKind = SpecKind.INT_CONST;
			}
			else {
				arithKind = (predMaxDepth > 0 && !modOpDivisor) ? this.arithSpecKinds.get(this.random.nextInt(this.arithSpecKinds.size())) :
				this.nullaryArithSpecKinds.get(this.random.nextInt(this.nullaryArithSpecKinds.size()));
			}
			if(arithKind.isNullary()) { //predMaxDepth >= 0
				switch(arithKind) {
				case INT_CONST:
					int intVal = this.random.nextInt(this.maxIntConst+1);
					if(modOpDivisor && intVal == 0) { //Avoid a zero divisor
						intVal+=1;
					}
					return new PrimitiveValue(intVal);
				case INT_VAR:
					Variable intVariable;
					if(modOpDivisor) {
						intVariable = this.nonZeroIntVariables.get(this.random.nextInt(this.nonZeroIntVariables.size()));
					}
					else {
						intVariable = this.intVariables.get(this.random.nextInt(this.intVariables.size()));
					}
					return new VariableReference(intVariable);
				default:
					throw new RuntimeException("The arithmetic spec operator kind " + arithKind + " is unsupported");
				}
			}
			else { //predMaxDepth > 0
				switch(arithKind) {
				case ARITH_OP:
					Operator arithOp = SpecKind.getOps(SpecKind.ARITH_OP)[this.random.nextInt(SpecKind.getOps(SpecKind.ARITH_OP).length)];
					Spec leftArithSpec = genRandArithmeticSpec(predMaxDepth-1, false);
					Spec rightArithSpec = genRandArithmeticSpec(predMaxDepth-1, Operator.MOD.equals(arithOp));
					return new SpecExp(arithOp, leftArithSpec, rightArithSpec);
				default:
					throw new RuntimeException("The arithmetic spec operator kind " + arithKind + " is unsupported");
				}
			}
		}
		
		private int randEvalArithSpec(Spec arithSpec) {
			if (arithSpec instanceof PrimitiveValue) {
				PrimitiveValue intVal = (PrimitiveValue) arithSpec;
				try {
					return Integer.parseInt(intVal.getValue());
				} catch (NumberFormatException e) {
					throw new RuntimeException("The arithmetic spec " + arithSpec + " cannot be evaluated");
				}
			}
			else if (arithSpec instanceof VariableReference) {
				VariableReference varRef = (VariableReference) arithSpec;
				int domSize = varRef.getVariable().getType().getUpper() - varRef.getVariable().getType().getLower() + 1;
				return this.random.nextInt(domSize) + varRef.getVariable().getType().getLower();
			}
			else if (arithSpec instanceof SpecExp) {
				SpecExp specExp = (SpecExp) arithSpec;
				Operator arithOp = specExp.getOperator();

				int leftRandEval = randEvalArithSpec(specExp.getChildren()[0]);
				int rightRandEval = randEvalArithSpec(specExp.getChildren()[1]);

				if (Operator.MOD.equals(arithOp)) {
					return leftRandEval % rightRandEval;
				}
				if (Operator.ADD.equals(arithOp)) {
					return leftRandEval + rightRandEval;
				}
				if (Operator.SUBSTRACT.equals(arithOp)) {
					return leftRandEval - rightRandEval;
				}
				if (Operator.MULTIPLY.equals(arithOp)) {
					return leftRandEval * rightRandEval;
				}
			}
			throw new RuntimeException("The arithmetic spec " + arithSpec + " cannot be evaluated");
		}

		private boolean getAcceptValues(List<String> values, int varDomainSize) {
			boolean acceptValues;
			if(values.size() == varDomainSize) {
				acceptValues = true;
			}
			else if (values.isEmpty()){
				acceptValues = false;
			}
			else {
				acceptValues = this.random.nextBoolean();
			}
			return acceptValues;
		}

		private SpecRegExp genRandBinaryRegExp(int maxDepth, RegExpKind binaryKind) {
			SpecRegExp leftSubExp = genRandRegExp(maxDepth-1);
			SpecRegExp rightSubExp = genRandRegExp(maxDepth-1);

			switch(binaryKind) {
			case UNION:
				return SpecRegExp.newUnionRegExp(leftSubExp, rightSubExp);
			case INTERSECTION:
				return SpecRegExp.newIntersectionRegExp(leftSubExp, rightSubExp);
			case CONCAT:
				return SpecRegExp.newConcatRegExp(leftSubExp, rightSubExp);
			default:
				throw new RuntimeException("The binary regexp kind " + binaryKind + " is unsupported");
			}
		}


		private SpecRegExp genRandUnaryRegExp(int maxDepth, RegExpKind unaryKind) {
			SpecRegExp subExp = genRandRegExp(maxDepth-1);
			switch(unaryKind) {
			case REPEAT:
				return genRandRepRegExp(subExp);
			case COMPLEMENT:
				return SpecRegExp.newComplementationRegExp(subExp);
			default:
				throw new RuntimeException("The unary regexp kind " + unaryKind + " is unsupported");
			}
		}

		private SpecRegExp genRandRepRegExp(SpecRegExp subRegExp) {
			QuantifierType quantifierType = QUANTIFIER_TYPES.get(this.random.nextInt(QUANTIFIER_TYPE_NUM));
			int repRangeSize = -1, leastRepNum = -1; 
			switch(quantifierType) {
			case ZERO_OR_MORE:
				return SpecRegExp.newZeroOrMoreRepRegExp(subRegExp);	
			case ONE_OR_MORE:
				return SpecRegExp.newOneOrMoreRepRegExp(subRegExp);
			case ZERO_OR_ONE:
				return SpecRegExp.newZeroOrOneRepRegExp(subRegExp);
			case RANGE:
				repRangeSize = 1 + this.random.nextInt(this.maxRepRange);
			default:
				leastRepNum = this.random.nextInt(this.maxLeastRepNum+1);	
			}
			switch(quantifierType) {
			case RANGE:
				return SpecRegExp.newRepInRangeRegExp(subRegExp, leastRepNum, leastRepNum + repRangeSize - 1);
			case AT_LEAST:
				return SpecRegExp.newAtLeastRepRegExp(subRegExp, leastRepNum);
			case EXACT_REPETITION:
				return SpecRegExp.newExactRepRegExp(subRegExp, leastRepNum);
			default:
				throw new RuntimeException("The regexp repetition quantifier " + quantifierType + " is unsupported");
			}
		}
	}


	/*
	 * 
	 * Public methods
	 * 
	 */

	public RegExpKind getRegExpKind(){
		return this.kind;
	}


	public boolean isNullaryRegExp() {
		if(this.kind != null) {
			return this.kind.isNullary();
		}
		return false;
	}

	public boolean isBinaryRegExp() {
		if(this.kind != null) {
			return this.kind.isBinary() && this.left != null && this.right != null;
		}
		return false;
	}

	public boolean isUnaryRegExp() {
		if(this.kind != null) {
			return this.kind.isUnary();
		}
		return false;
	}

	public boolean hasLeft() {
		return this.left != null;
	}

	public boolean hasRight() {
		return this.right != null;
	}

	/**
	 * Checks whether this regular expression consists of a variable and asserts that this variable
	 * should be equal to at least one value.
	 * 
	 * @return
	 */
	public boolean isVariable() {
		return RegExpKind.VAR.equals(this.kind)
				&& this.varRef != null
				&& this.varRef.getReferenceName() != null
				&& this.varRef.getVariable() != null
				&& this.varRef.getVariable().getName() != null
				&& this.values != null
				&& !this.values.isEmpty();
	}

	public boolean isPredicate() {
		return RegExpKind.PREDICATE.equals(this.kind)
				&& this.predicate != null;
	}

	public Spec getPredicate() {
		return this.predicate;
	}

	public void setPredicate(Spec predicate) {
		this.predicate = predicate;
	}
	
	/**
	 * Serializes this {@link SpecRegExp} object in the specified file.
	 * 
	 * @param fileName the file name
	 */
	public void save(String fileName) throws IOException {   
		//Saving of this regexp in a file 
		FileOutputStream file = new FileOutputStream(fileName);
		ObjectOutputStream out = new ObjectOutputStream(file);
		
		//Serialization of this regexp 
		out.writeObject(this);

		out.close();
		file.close(); 
	}
	
	
	/**
	 * 
	 * Deserializes the {@link SpecRegExp} object serialized in the file with the name {@code fileName}.
	 * 
	 * 
	 * @param fileName the file name
	 * @return The {@link SpecRegExp} object serialized in the file
	 * @throws IOException
	 * @throws ClassNotFoundException
	 */
	public static SpecRegExp loadRegExp(String fileName) throws IOException, ClassNotFoundException {
		//Reading the object from a file 
		FileInputStream file = new FileInputStream(fileName); 
		ObjectInputStream in = new ObjectInputStream(file);
		
		//Deserialization of the current regexp object
		SpecRegExp regExp = (SpecRegExp)in.readObject();
		
		in.close(); 
		file.close();
		
		return regExp;
	}
	
	/**
	 * Returns a list that consists of all the subexpressions of the form '[Boolean term (assertion)]'
	 * in this regular expression.
	 * 
	 * @return
	 */
	public List<SpecRegExp> getPredicateSubExps() {
		List<SpecRegExp> predicateExps = new ArrayList<>();
		this.addAllPredicateSubExps(predicateExps);
		return predicateExps;
	}

	/**
	 * Returns a list that consists of all the variables referenced in the atomic subexpressions
	 * of this regular expression. Each variable appears in the returned list exactly once.
	 * 
	 * @return
	 */
	public List<Variable> getVariables() {
		List<Variable> regExpVars = new ArrayList<>();
		this.addAllVariables(regExpVars);
		return regExpVars;
	}
	
	/**
	 * <p>Returns a mapping of all the variable references (reference names) that appear in this regular expression
	 * to the referenced variable objects.</p>
	 * 
	 * <p>Note: The reference name may be different than the name of the referenced variable in case the variable
	 * is an array. For example, 'arrVar[0][2]' references the two-dimensional array variable 'arrVar'.</p> 
	 * 
	 * @return
	 */
	public Map<String, Variable> getRefToVarMapping() {
		Map<String, Variable> refToVar = new HashMap<>();
		fillRefToVarMapping(refToVar);
		return refToVar;
		
	}
	
	/**
	 * Checks whether this regular expression contains a subexpression with a complementation/negation operator.
	 * 
	 * @return
	 */
	public boolean containsComplement() {
		if(RegExpKind.COMPLEMENT.equals(this.kind)) {
			return true;
		}
		if(this.hasLeft() && this.left.containsComplement()) {
			return true;
		}
		if(this.hasRight() && this.right.containsComplement()) {
			return true;
		}
		return false;
	}

	/**
	 * Checks whether this regular expression consists of a Boolean constant, either TRUE or FALSE.
	 * 
	 * @return
	 */
	public boolean isBooleanConst() {
		return isBooleanConstKind() && (hasSingleBooleanValue(true) || hasSingleBooleanValue(false));
	}

	/**
	 * Checks whether this regular expression is the TRUE Boolean constant.
	 * 
	 * @return
	 */
	public boolean isTrueBooleanConst() {
		return isBooleanConstKind() && hasSingleBooleanValue(true);
	}

	/**
	 * Checks whether this regular expression is the FALSE Boolean constant.
	 * 
	 * @return
	 */
	public boolean isFalseBooleanConst() {
		return isBooleanConstKind() && hasSingleBooleanValue(false);
	}

	public boolean isEmptyString() {
		return RegExpKind.EMPTY_STR.equals(this.kind);
	}

	public boolean isComplementation() {
		return isUnaryKind(RegExpKind.COMPLEMENT);
	}

	/**
	 * Checks whether this regular expression asserts a repetition (via a quantifier) of a
	 * sub-expression.
	 * 
	 * @return
	 * @see #getQuantifier()
	 * @see #getQuantifierType()
	 * @see QuantifierType
	 * @see RegExpQuantifier
	 */
	public boolean isRepetition() {
		return isUnaryKind(RegExpKind.REPEAT) && this.quantifier != null;
	}
	/**
	 * Checks whether this regular expression is a concatenation of two regular expressions.
	 * 
	 * @return
	 */
	public boolean isConcat() {
		return isBinaryKind(RegExpKind.CONCAT);
	}

	/**
	 * Checks whether this regular expression is a union of two regular expressions.
	 * 
	 * @return
	 */
	public boolean isUnion() {
		return isBinaryKind(RegExpKind.UNION);
	}

	/**
	 * Checks whether this regular expression is an intersection of two regular expressions.
	 * 
	 * @return
	 */
	public boolean isIntersection() {
		return isBinaryKind(RegExpKind.INTERSECTION);
	}

	public QuantifierType getQuantifierType() {
		return isRepetition() ? this.quantifier.type : null;
	}

	public RegExpQuantifier getQuantifier() {
		return this.quantifier;
	}

	public SpecRegExp getLeft(){
		return left;
	}

	public SpecRegExp getRight(){
		return right;
	}

	public Variable getVariable() {
		return this.isVariable() ? this.varRef.getVariable() : null;
	}
	
	public VariableReference getVariableReference() {
		return this.varRef;
	}

	public String getVariableName() {
		return this.isVariable() ? this.varRef.getVariable().getName() : null;
	}
	
	public String getVariableReferenceName() {
		return this.isVariable() ? this.varRef.getReferenceName() : null;
	}

	/**
	 * Checks whether this regular expression consists of a Boolean variable and asserts that this variable
	 * should be equal to either TRUE or FALSE (or both).
	 * 
	 * @return
	 */
	public boolean isBooleanVariable() {
		return this.isVariable()
				&& this.varRef.getVariable().getType() != null 
				&& this.varRef.getVariable().getType().isBoolean()
				&& allValuesAreBooleans();
	}

	/**
	 * Checks whether this regular expression consists of an integer variable and asserts that this variable
	 * should be equal to an element in a non-empty set of integer values.
	 * @return
	 */
	public boolean isIntegerVariable() {
		return this.isVariable()
				&& this.varRef.getVariable().getType() != null 
				&& this.varRef.getVariable().getType().isInteger()
				&& allValuesAreInts();
	}

	/**
	 * Checks whether this regular expression consists of an enumeration variable and asserts that this variable
	 * should be equal to an element in a non-empty set of values.
	 * @return
	 */
	public boolean isEnumVariable() {
		return this.isVariable()
				&& this.varRef.getVariable().getType() != null 
				&& !this.varRef.getVariable().getType().isInteger()
				&& !this.varRef.getVariable().getType().isBoolean();
	}

	/**
	 * If this is a regular expression which is a Boolean constant,
	 * returns the underlying Boolean value. Otherwise, returns {@code null}.
	 * @return
	 */
	public String getBooleanValue() {
		if(this.isBooleanConst()) {
			return this.values.get(0);
		}
		return null;
	}

	public Set<String> getValues(){
		return new HashSet<>(this.values);
	}

	@Override
	public boolean isPastLTLSpec() {
		return false;
	}

	@Override
	public boolean isPropSpec() {
		return false;
	}

	@Override
	public boolean hasTemporalOperators() {
		return false;
	}

	@Override
	public SpecRegExp clone() throws CloneNotSupportedException {
		return new SpecRegExp(this.left != null ? this.left.clone() : null, this.right != null ? this.right.clone() : null, this.values != null ? new ArrayList<>(this.values) : null,
				this.varRef != null ? this.varRef.clone() : null, this.predicate != null ? this.predicate.clone() : null, this.kind,
						this.quantifier != null ? this.quantifier.clone() : null);
	}

	@Override
	public String toString() {
		if(this.isEmptyString()) { //regExp = '()' 
			return "()";
		}
		if(this.isTrueBooleanConst()) { //regExp = 'TRUE' 
			return "true";
		}
		if(this.isFalseBooleanConst()) { //regExp = 'FALSE' 
			return "false";
		}
		if(this.isVariable()) { //regExp = '[var in {set of values}]' 
			StringBuilder res = new StringBuilder();
			res.append("[");
			res.append(this.getVariableReferenceName());
			res.append(" in {");
			this.values.subList(0, this.values.size()-1).forEach(s -> res.append(s + ", "));
			res.append(this.values.get(this.values.size()-1));
			res.append("}]");
			return res.toString();
		}
		if(this.isPredicate()) { //regExp = '[Boolean term (assertion)]'
			return "[" + this.predicate.toString() + "]";
		}

		if(!this.hasLeft()) {
			return "INVALID REGEXP: expected non-null left subexpession attribute";
		}

		String leftRes = this.left.toString();

		if(this.isRepetition()) {
			return "(" + leftRes + ")" + this.quantifier.toString();
		}

		if(this.isComplementation()) {
			return "~(" + leftRes + ")";
		}

		if(!this.hasRight()) {
			return "INVALID REGEXP: expected non-null right subexpession attribute";
		}

		String rightRes = this.right.toString();

		if(this.isConcat()) {
			return "(" + leftRes + ") (" + rightRes + ")";
		}

		if(this.isUnion()) {
			return "(" + leftRes + ")|(" + rightRes + ")";
		}

		if(this.isIntersection()) {
			return "(" + leftRes + ")&(" + rightRes + ")";
		}

		return "INVALID REGEXP KIND: cannot generate string for this regular expression";

	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (this.getClass() != obj.getClass()) {
			return false;
		}
		SpecRegExp other = (SpecRegExp) obj;

		if(this.kind != other.kind) {return false;}

		//this.kind == other.kind
		if(this.kind == null || RegExpKind.EMPTY_STR.equals(this.kind)) {return true;}

		if(RegExpKind.BOOLEAN_CONST.equals(this.kind)) { //Both kinds are Boolean constants
			if(this.values != null) {
				return this.values.equals(other.values);
			}
			else {
				return other.values == null;
			}
		}
		if(RegExpKind.VAR.equals(this.kind)) { //Both kinds are variables
			if(this.varRef != null) {
				if(other.varRef == null) {
					return false;
				}
				if(this.varRef.getReferenceName() != null) {
					if(!this.varRef.getReferenceName().equals(other.varRef.getReferenceName())) {
						return false;
					}
					if(this.values != null) {
						if(other.values == null) {
							return false;
						}
						//Compare the two values' lists, but ignore the elements' order
						Set<String> thisValSet = new HashSet<>(this.values);
						Set<String> otherValSet = new HashSet<>(other.values);
						return thisValSet.equals(otherValSet);
					}
					else {
						return other.values == null;
					}
				}
				else {
					return other.varRef.getReferenceName() == null;
				}
			}
			else {
				return other.varRef == null;
			}
		}
		if(RegExpKind.PREDICATE.equals(this.kind)) {  //Both kinds are predicates
			if(this.predicate != null) {
				return this.predicate.equals(other.predicate);
			}
			else {
				return other.predicate == null;
			}
		}
		if(this.kind.isUnary()) { //Both kinds are unary
			if(RegExpKind.REPEAT.equals(this.kind)) { //Check that both have the same repetition quantifier
				if(this.quantifier != null) {
					if(!this.quantifier.equals(other.quantifier)) {return false;}
				}
				else if(other.quantifier != null) {return false;}
			}
			//Check that the (left and only) subexpressions are equal
			if(this.left != null) {
				return this.left.equals(other.left);
			}
			else {
				return this.left == null;
			}
		}
		else { //Both kinds are binary
			boolean leftEquals;

			//Check that the left subexpressions are equal
			if(this.left != null) {
				leftEquals = this.left.equals(other.left);
			}
			else {
				leftEquals = (other.left == null);
			}
			
			if(!leftEquals) {
				//The left subexpressions are not equal
				return false;
			}

			//Check that the right subexpressions are equal
			if(this.right != null) {
				return this.right.equals(other.right);
			}
			else {
				return other.right == null;
			}
		}
	}

	/*
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * Static factory methods
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 */

	public static SpecRegExp newVariableRegExp(VariableReference varRef, List<String> values, boolean acceptValues) {
		if(varRef == null || varRef.getVariable() == null || varRef.getVariable().getType() == null || values == null || (acceptValues && values.isEmpty())) {
			throw new RuntimeException("Could not create a new variable SpecRegExp object."
					+ " The given arguments would lead to the creation of an invalid regular expression");
		}
		SpecRegExp newRegExp = new SpecRegExp(varRef, values, acceptValues);
		if(!acceptValues && newRegExp.values.isEmpty()) {
			//the complementation of the values set has resulted in an empty set of values
			//this is an invalid regular expression (as it requires that the variable should not be equal to any value)
			return null;
		}
		return newRegExp;
	}

	public static SpecRegExp newPredicateRegExp(Spec predicate) {
		if(predicate == null){
			throw new RuntimeException("Could not create a new predicate SpecRegExp object."
					+ " The predicate (a Spec object) that has been specified is null"); 
		}
		return new SpecRegExp(predicate);
	}

	public static SpecRegExp newBooleanConstRegExp(boolean booleanValue) {
		return new SpecRegExp(booleanValue);
	}

	public static SpecRegExp newEmptyStringRegExp() {
		return new SpecRegExp();
	}

	public static SpecRegExp newUnionRegExp(SpecRegExp leftSubExp, SpecRegExp rightSubExp) {
		if(leftSubExp == null || rightSubExp == null) {
			throw new RuntimeException("Could not create a new union SpecRegExp object. "
					+ (leftSubExp == null ? "The left" : "The right") + " subexpression that has been specified is null"); 
		}
		return new SpecRegExp(leftSubExp, rightSubExp, RegExpKind.UNION);
	}

	public static SpecRegExp newConcatRegExp(SpecRegExp leftSubExp, SpecRegExp rightSubExp) {
		if(leftSubExp == null || rightSubExp == null) {
			throw new RuntimeException("Could not create a new concatenation SpecRegExp object. "
					+ (leftSubExp == null ? "The left" : "The right") + " subexpression that has been specified is null"); 
		}
		return new SpecRegExp(leftSubExp, rightSubExp, RegExpKind.CONCAT);
	}

	public static SpecRegExp newIntersectionRegExp(SpecRegExp leftSubExp, SpecRegExp rightSubExp) {
		if(leftSubExp == null || rightSubExp == null) {
			throw new RuntimeException("Could not create a new intersection SpecRegExp object. "
					+ (leftSubExp == null ? "The left" : "The right") + " subexpression that has been specified is null"); 
		}
		return new SpecRegExp(leftSubExp, rightSubExp, RegExpKind.INTERSECTION);
	}

	public static SpecRegExp newComplementationRegExp(SpecRegExp subExp) {
		if(subExp == null) {
			throw new RuntimeException("Could not create a new complement SpecRegExp object."
					+ " The subexpression that has been specified is null"); 
		}
		return new SpecRegExp(subExp, RegExpKind.COMPLEMENT);
	}

	public static SpecRegExp newZeroOrOneRepRegExp(SpecRegExp subExp) {
		if(subExp == null) {
			throw new RuntimeException("Could not create a new 'zero or one' repetitions SpecRegExp object."
					+ " The quantified subexpression that has been specified is null"); 
		}
		return new SpecRegExp(subExp);
	}

	public static SpecRegExp newZeroOrMoreRepRegExp(SpecRegExp subExp) {
		if(subExp == null) {
			throw new RuntimeException("Could not create a new 'zero or more' repetitions SpecRegExp object."
					+ " The quantified subexpression that has been specified is null"); 
		}
		return new SpecRegExp(subExp, true);
	}

	public static SpecRegExp newOneOrMoreRepRegExp(SpecRegExp subExp) {
		if(subExp == null) {
			throw new RuntimeException("Could not create a new 'one or more' repetitions SpecRegExp object."
					+ " The quantified subexpression that has been specified is null"); 
		}
		return new SpecRegExp(subExp, false);
	}

	public static SpecRegExp newExactRepRegExp(SpecRegExp subExp, int repNum) {
		if(subExp == null) {
			throw new RuntimeException("Could not create a new 'exact' repetitions SpecRegExp object."
					+ " The quantified subexpression that has been specified is null"); 
		}
		if(repNum < 0) {
			throw new RuntimeException("Could not create a new 'exact' repetitions SpecRegExp object."
					+ " The number of repetitions that has been specified is negative (must be non-negative)"); 
		}
		return new SpecRegExp(subExp, true, repNum);
	}

	public static SpecRegExp newAtLeastRepRegExp(SpecRegExp subExp, int repNum) {
		if(subExp == null) {
			throw new RuntimeException("Could not create a new 'at least' repetitions SpecRegExp object."
					+ " The quantified subexpression that has been specified is null"); 
		}
		if(repNum < 0) {
			throw new RuntimeException("Could not create a new 'at least' repetitions SpecRegExp object."
					+ " The number of repetitions that has been specified is negative (must be non-negative)"); 
		}
		return new SpecRegExp(subExp, false, repNum);
	}

	public static SpecRegExp newRepInRangeRegExp(SpecRegExp subExp, int leastRepNum, int maxRepNum) {
		//if(subExp == null || leastRepNum < 0 || maxRepNum < leastRepNum) {return null;}
		if(subExp == null) {
			throw new RuntimeException("Could not create a new 'repetitions in range' SpecRegExp object."
					+ " The quantified subexpression that has been specified is null"); 
		}
		if(leastRepNum < 0) {
			throw new RuntimeException("Could not create a new 'repetitions in range' SpecRegExp object."
					+ " The least number of repetitions that has been specified is negative (must be non-negative)"); 
		}
		if(maxRepNum < leastRepNum) {
			throw new RuntimeException("Could not create a new 'repetitions in range' SpecRegExp object."
					+ " The maximal number of repetitions that has been specified is smaller than the least one"); 
		}
		return new SpecRegExp(subExp, leastRepNum, maxRepNum);
	}

	/*
	 * 
	 * 
	 * 
	 * 
	 * 
	 * Private constructors
	 * 
	 * 
	 * 
	 * 
	 * 
	 */

	/**
	 * 
	 * 
	 * Constructor for a regular expression that consists of a Boolean term/formula/predicate.
	 * 
	 * @param predicate the Boolean predicate/formula
	 */
	private SpecRegExp(Spec predicate) {
		this.kind = RegExpKind.PREDICATE;
		this.predicate = predicate;
	}

	/**
	 * 
	 * Constructor for a regular expression that specifies a variable and a set of values, either of which should be assigned to that variable.
	 * 
	 * @param varRef the variable reference
	 * @param values non-empty set of the values, each of which is ASSUMED to be in the domain of {@code variable}
	 * @param acceptValues {@code true} (resp. {@code false}) if either (resp. neither) of the values in {@code values} should be accepted by the regular expression
	 */
	private SpecRegExp(VariableReference varRef, List<String> values, boolean acceptValues) {
		this.kind = RegExpKind.VAR; 
		this.varRef = varRef;
		if(acceptValues) {
			this.values = values;
		}
		else { //we assume that (varRef != null && varRef.getVariable() != null && varRef.getVariable().getType() != null && values != null)
			//this.values should be the complement of the given list of values (w.r.t. the domain of the variable)
			if(varRef.getVariable().getType().isBoolean()) {
				//Boolean variable
				this.values = new ArrayList<>();
				this.values.add(TRUE);
				this.values.add(FALSE);
			}
			else if (varRef.getVariable().getType().isInteger()) {
				this.values = IntStream.rangeClosed(varRef.getVariable().getType().getLower(), varRef.getVariable().getType().getUpper()).
						mapToObj(String::valueOf).collect(Collectors.toList());
			}
			else { //Enum variable
				this.values = new ArrayList<>(varRef.getVariable().getType().getValues());					
			}
			//this.values now contains all the values in the domain of the given variable
			//thus to obtain the complement list, we remove all the values that are in the given list
			this.values.removeAll(values);
		}
	}

	/**
	 * 
	 * Constructor for a regular expression of a constant Boolean value, either 'true' or 'false'.
	 * 
	 * @param booleanValue the Boolean value
	 */
	private SpecRegExp(boolean booleanValue) {
		this.kind = RegExpKind.BOOLEAN_CONST;
		String constValue = booleanValue ? TRUE : FALSE;
		this.values = new ArrayList<>();
		this.values.add(constValue);
	}

	/**
	 * 
	 * Constructor for the empty string regular expression.
	 *
	 */
	private SpecRegExp() {
		this.kind = RegExpKind.EMPTY_STR;
	}

	/**
	 * Constructor for an unary regular expression.
	 * 
	 * @param left the sub regular expression
	 * @param kind the kind of the unary operator, e.g., complementation
	 */
	private SpecRegExp(SpecRegExp left, RegExpKind kind) {
		this(left, null, kind);
	}

	/**
	 * 
	 * Constructor for a binary regular expression.
	 * 
	 * @param left the left sub regular expression
	 * @param right the right sub regular expression
	 * @param kind the kind of the binary operator, e.g., union, intersection, concatenation.
	 */
	private SpecRegExp(SpecRegExp left, SpecRegExp right, RegExpKind kind) {
		this(left, right, null, null, null, kind, null);
	}

	/**
	 * 
	 * Constructor for a regular expression of any kind.
	 * 
	 * @param left the left sub regular expression
	 * @param right the right sub regular expression
	 * @param values set of the values
	 * @param varRef the variable reference
	 * @param predicate the Boolean predicate/formula
	 * @param kind the kind of the regular expression
	 * @param quantifier the repetitions quantifier
	 */
	private SpecRegExp(SpecRegExp left, SpecRegExp right, List<String> values,
			VariableReference varRef, Spec predicate, RegExpKind kind, RegExpQuantifier quantifier) {
		this.left = left;
		this.right = right;
		this.values = values;
		this.varRef = varRef;
		this.predicate = predicate;
		this.kind = kind;
		this.quantifier = quantifier;
	}


	/**
	 * Constructor for an unary regular expression with a 'zero or one repetitions' quantifier ('?').
	 * 
	 * @param left the sub regular expression
	 */
	private SpecRegExp(SpecRegExp left) {
		this(left, RegExpKind.REPEAT);
		this.quantifier = RegExpQuantifier.zeroOrOneRepetition();
	}

	/**
	 * Constructor for an unary regular expression with either of the following two quantifiers:<br>
	 * 
	 * - 'Zero or more repetitions' (Kleene iteration, '*')<br>
	 * - 'One or more repetitions' ('+')
	 * 
	 * @param left the sub regular expression
	 * @param zeroOrMore whether the regular expression has a Kleene iteration quantifier
	 */
	private SpecRegExp(SpecRegExp left, boolean zeroOrMore) {
		this(left, RegExpKind.REPEAT);
		if(zeroOrMore) {
			this.quantifier = RegExpQuantifier.zeroOrMoreRepetitions();
		}
		else {
			this.quantifier = RegExpQuantifier.oneOrMoreRepetitions();
		}
	}

	/**
	 * Constructor for an unary regular expression with either of the following quantifiers:<br>
	 * 
	 * - 'Exact repetitions' ('{{@code n}}') <br>
	 * - 'At least repetitions' ('{{@code n},}')
	 * 
	 * @param left the sub regular expression
	 * @param isExact whether the regular expression has an 'exact repetitions' quantifier
	 * @param n the number of repetitions
	 */
	private SpecRegExp(SpecRegExp left, boolean isExact, int n) {
		this(left, RegExpKind.REPEAT);
		if(isExact) {
			this.quantifier = RegExpQuantifier.exactRepetitions(n);
		}
		else {
			this.quantifier = RegExpQuantifier.atLeastRepetitions(n);
		}
	}

	/**
	 * Constructor for an unary regular expression with 'repetitions in range' ('{{@code n},{@code m}}') quantifier.
	 * 
	 * 
	 * @param left the sub regular expression
	 * @param n the least number of repetitions
	 * @param m the maximal number of repetitions (inclusive)
	 */
	private SpecRegExp(SpecRegExp left, int n, int m) {
		this(left, RegExpKind.REPEAT);
		this.quantifier = RegExpQuantifier.repetitionsInRange(n, m);
	}

	/*
	 * 
	 * Private methods
	 * 
	 * 
	 * 
	 */

	private void addAllPredicateSubExps(List<SpecRegExp> predicateExps) {
		if(this.isPredicate()) {
			if(!predicateExps.contains(this)) { //regExp = '[Boolean term (assertion)]'
				predicateExps.add(this);
			}
		}
		else {
			if(this.hasLeft()) {
				this.left.addAllPredicateSubExps(predicateExps);
			}
			if(this.hasRight()) {
				this.right.addAllPredicateSubExps(predicateExps);
			}
		}
	}

	private void addAllVariables(List<Variable> regExpVars) {
		if(this.isVariable()) { //regExp = '[variable in {set of values}]'
			if(!regExpVars.contains(this.varRef.getVariable())) { 
				regExpVars.add(this.varRef.getVariable());
			}
		}
		else if(this.isPredicate()) { //regExp = '[Boolean term (instance of Spec)]'
			this.addAllSpecVariables(this.predicate, regExpVars);
		}
		else {
			if(this.hasLeft()) {
				this.left.addAllVariables(regExpVars);
			}
			if(this.hasRight()) {
				this.right.addAllVariables(regExpVars);
			}
		}
	}
	
	private void addAllSpecVariables(Spec spec, List<Variable> regExpVars) {
		if(spec instanceof VariableReference) {
			VariableReference vr = (VariableReference) spec;
			Variable var = vr.getVariable();
			if(var != null && !regExpVars.contains(var)) {
				regExpVars.add(var);
			}
		}
		else if(spec instanceof SpecExp) {
			SpecExp specExp = (SpecExp) spec;
			for(Spec childSpec : specExp.getChildren()) {
				addAllSpecVariables(childSpec, regExpVars);
			}
		}
	}
	
	private void fillRefToVarMapping(Map<String, Variable> refToVar) {
		if(this.isVariable()) { //regExp = '[variable in {set of values}]'
			if(!refToVar.containsKey(this.varRef.getReferenceName())) { 
				refToVar.put(this.varRef.getReferenceName(), this.varRef.getVariable());
			}
		}
		else if(this.isPredicate()) { //regExp = '[Boolean term (instance of Spec)]'
			this.fillSpecRefToVarMapping(this.predicate, refToVar);
		}
		else {
			if(this.hasLeft()) {
				this.left.fillRefToVarMapping(refToVar);
			}
			if(this.hasRight()) {
				this.right.fillRefToVarMapping(refToVar);
			}
		}
	}
	
	private void fillSpecRefToVarMapping(Spec spec, Map<String, Variable> refToVar) {
		if(spec instanceof VariableReference) {
			VariableReference varRef = (VariableReference) spec;
			if(!refToVar.containsKey(varRef.getReferenceName())) {
				refToVar.put(varRef.getReferenceName(), varRef.getVariable());
			}
		}
		else if(spec instanceof SpecExp) {
			SpecExp specExp = (SpecExp) spec;
			for(Spec childSpec : specExp.getChildren()) {
				fillSpecRefToVarMapping(childSpec, refToVar);
			}
		}
	}

	private boolean hasSingleBooleanValue(boolean trueValue) {
		return this.values != null && this.values.size() == 1 && (trueValue ? TRUE.equalsIgnoreCase(this.values.get(0)) : FALSE.equalsIgnoreCase(this.values.get(0)));
	}

	private boolean isBooleanConstKind() {
		return RegExpKind.BOOLEAN_CONST.equals(this.kind);
	}

	private boolean isBinaryKind(RegExpKind kind) {
		return kind.equals(this.kind) && this.left != null && this.right != null;
	}

	private boolean isUnaryKind(RegExpKind kind) {
		return kind.equals(this.kind) && this.left != null;
	}

	private boolean allValuesAreInts() {
		for(String val : this.values) {
			try {
				Integer.valueOf(val);
			} catch (NumberFormatException e) {
				return false;
			}
		}
		return true;
	}

	private boolean allValuesAreBooleans() {
		for(String val : this.values) {
			if(!(TRUE.equalsIgnoreCase(val) || FALSE.equalsIgnoreCase(val))) {
				return false;
			}
		}
		return true;
	}
}
