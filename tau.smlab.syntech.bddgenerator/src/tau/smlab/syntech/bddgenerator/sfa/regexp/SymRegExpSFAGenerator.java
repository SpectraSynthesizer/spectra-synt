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

import net.sf.javabdd.BDD;
import tau.smlab.syntech.bddgenerator.BDDGenerator;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.sfa.SFA;
import tau.smlab.syntech.sfa.SFAs;

/**
 * 
 * A class for transforming regular expressions into Symbolic finite automatons (SFAs) using 
 * only symbolic BDD operations. This implementation adapts Thompson's construction algorithm.
 * 
 * @author Or Pistiner
 *
 */
public class SymRegExpSFAGenerator implements RegExpSFAGenerator {

	private SpecRegExp specRegExp; //the regular expression
	private SFA sfa; //the SFA that would be created
	private int traceId; //the trace ID of the constraint where the regular expression appears
	private boolean isPredicate; //indicates whether the regular expression defines a "predicate" language, i.e., a language that only consists
								 //of words of length 1 that may be characterized by a Boolean predicate/assertion

	/**
	 * 
	 * @param specRegExp the regular expression
	 * @param traceId
	 */
	public SymRegExpSFAGenerator(SpecRegExp specRegExp, int traceId) {
		this.traceId = traceId;
		this.isPredicate = false;
		this.specRegExp = specRegExp;
	}
	
	@Override
	public SFA generateRegExpDSfa(boolean minimal) {
		generateRegExpSfa();
		SFA resDSfa;
		if(minimal) {
			resDSfa = this.sfa.minimize();
		}
		else {
			resDSfa = this.sfa.determinize();
		}
		this.sfa.free();
		this.sfa = resDSfa;
		return this.sfa;
	}
	
	@Override
	public SFA generateRegExpSfa() {
		if(this.sfa == null) {
			SFA oFSfa = this.buildRegExpOFSfa();
			this.sfa = oFSfa.eliminateEpsTrans();
			this.sfa.removeDeadStates();
			oFSfa.free();
		}
		return this.sfa;
	}

	@Override
	public void freeRegExpSfa() {
		if(this.sfa != null) {
			this.sfa.free();
		}
		this.sfa = null;
	}

	/**
	 * Checks whether the Epsilon-SFA of this generator either has been freed or
	 * has not been created.
	 * 
	 * @see #generateRegExpSfa()
	 * @see #free()
	 * 
	 * @return
	 */
	public boolean isFree() {
		return this.sfa == null;
	}

	/*
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 *
	 * 
	 * Private methods
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 */

	/**
	 * Returns an OFSFA whose language is that of the regular expression of this generator.
	 * Note that the returned OFSFA may be non-deterministic and have epsilon transitions.
	 * 
	 * @return
	 */
	private SFA buildRegExpOFSfa() {

		if(this.specRegExp == null) {
			throw new RegExpSFAGeneratorException("The regular expression of this transformer is null and thus cannot be translated to an SFA",
					this.traceId);
		}

		if(this.specRegExp.isEmptyString()) { //regExp = '()' 
			return SFAs.emptyWordOFSfa();
		}
		if(this.specRegExp.isTrueBooleanConst()) { //regExp = 'TRUE'
			this.isPredicate = true;
			return SFAs.predicateOFSfa(Env.TRUE());
		}
		if(this.specRegExp.isFalseBooleanConst()) { //regExp = 'FALSE'
			this.isPredicate = true;
			return SFAs.predicateOFSfa(Env.FALSE());
		}
		if(this.specRegExp.isVariable()) { //regExp = 'var in {set of values}' 
			String varName = this.specRegExp.getVariableReferenceName();
			BDD currValue;
			BDD valuesAssrt = Env.FALSE();
			for(String value : this.specRegExp.getValues()) {
				currValue = Env.getBDDValue(varName, value);
				if(currValue == null) {
					throw new RegExpSFAGeneratorException("Unable to transform the regular expression " + this.specRegExp + " into an Epsilon-SFA since "
							+ value + " is not in the domain of the variable " + varName, this.traceId);
				}
				valuesAssrt.orWith(currValue.id());
			}
			this.isPredicate = true;
			return SFAs.predicateOFSfa(valuesAssrt);	
		}
		if(this.specRegExp.isPredicate()) { //regExp = 'Boolean term (i.e., assertion)'
			this.isPredicate = true;
			BDD predicate = BDDGenerator.createBdd(this.specRegExp.getPredicate(), this.traceId);
			return SFAs.predicateOFSfa(predicate);
		}

		if(!this.specRegExp.hasLeft()) {
			throw new RegExpSFAGeneratorException("Unable to transform the regular expression " + this.specRegExp + " into an Epsilon-SFA as it is expected to have a left sub-expression", this.traceId);
		}

		//Perform a recursive call on the left subexpression (might be the only subexpression in case this is an unary expression)
		SymRegExpSFAGenerator leftSfaGenerator = new SymRegExpSFAGenerator(this.specRegExp.getLeft(), this.traceId);
		SFA leftOFSfa = leftSfaGenerator.buildRegExpOFSfa();

		if(this.specRegExp.isRepetition()) {
			int leastRepNum = this.specRegExp.getQuantifier().getLeastRepetitionsNum();
			switch(this.specRegExp.getQuantifierType()) {
			case AT_LEAST: //'{n,}'
				return SFAs.nOrMoreOFSfa(leftOFSfa, leastRepNum);
			case EXACT_REPETITION: //'{n}'
				return SFAs.exactNOFSfa(leftOFSfa, leastRepNum);
			case ONE_OR_MORE: //'+'
				return SFAs.oneOrMoreOFSfa(leftOFSfa);
			case RANGE: //'{n,m}'
				return SFAs.nToMOFSfa(leftOFSfa, leastRepNum, this.specRegExp.getQuantifier().getMaxRepetitionsNum());
			case ZERO_OR_MORE: //'*'
				return SFAs.kleeneClosureOFSfa(leftOFSfa);
			case ZERO_OR_ONE: //'?'
				return SFAs.zeroOrOneOFSfa(leftOFSfa);
			default:
				throw new RegExpSFAGeneratorException("Unable to transform the repetition regular expression " + this.specRegExp + " into an Epsilon-SFA since the quantifier " +
						this.specRegExp.getQuantifierType() + " is not supported", this.traceId);
			}
		}
		
		if(this.specRegExp.isComplementation()) {
			return SFAs.complementOFSfa(leftOFSfa);
		}
		

		if(!this.specRegExp.hasRight()) {
			throw new RegExpSFAGeneratorException("Unable to transform the unary regular expression " + this.specRegExp + " into an Epsilon-SFA. This regular expression is not supported", this.traceId);
		}
		
		//Perform a recursive call on the right subexpression
		SymRegExpSFAGenerator rightSfaGenerator = new SymRegExpSFAGenerator(this.specRegExp.getRight(), this.traceId);
		SFA rightOFSfa = rightSfaGenerator.buildRegExpOFSfa();

		if(this.specRegExp.isUnion()) {
			if(leftSfaGenerator.isPredicate && rightSfaGenerator.isPredicate) {
				this.isPredicate = true;
				return SFAs.unionPredicateOFSfa(leftOFSfa, rightOFSfa);
			}
			else {
				return SFAs.unionOFSfa(leftOFSfa, rightOFSfa);
			}
		}

		if(this.specRegExp.isIntersection()) {
			if(leftSfaGenerator.isPredicate && rightSfaGenerator.isPredicate) {
				this.isPredicate = true;
				return SFAs.productPredicateOFSfa(leftOFSfa, rightOFSfa);
			}
			else {
				return SFAs.productOFSfa(leftOFSfa, rightOFSfa);
			}
		}

		if(this.specRegExp.isConcat()) {
			return SFAs.concatOFSfa(leftOFSfa, rightOFSfa);
		}
		throw new RegExpSFAGeneratorException("Unable to transform the binary regular expression " + this.specRegExp + " into an Epsilon-SFA. This regular expression is not supported", this.traceId);
	}

}
