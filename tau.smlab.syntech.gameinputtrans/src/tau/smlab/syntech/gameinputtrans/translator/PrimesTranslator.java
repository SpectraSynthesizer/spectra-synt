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

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.gameinput.model.WeightDefinition;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gameinputtrans.TranslationException;

/**
 * should be run only after all past LTL, predicates, patterns, and defines have
 * been instantiated
 * 
 * will only translate primes in main specifications
 */
public class PrimesTranslator implements Translator {

	@Override
	public void translate(GameInput input) {
		for (Constraint c : input.getSys().getConstraints()) {
			c.setSpec(replacePrimes(c.getSpec(), c.getTraceId()));
		}
		for (TriggerConstraint c : input.getSys().getTriggers()) {
			c.setSpec(detectPrimes(c.getInitSpecRegExp(), c.getTraceId()));
			c.setSpec(detectPrimes(c.getEffectSpecRegExp(), c.getTraceId()));
		}
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replacePrimes(c.getSpec(), c.getTraceId()));
		}
		for (Constraint c : input.getAux().getConstraints()) {
			c.setSpec(replacePrimes(c.getSpec(), c.getTraceId()));
		}
		for (WeightDefinition wd : input.getWeightDefs()) {
			wd.getDefinition().setSpec(replacePrimes(wd.getDefinition().getSpec(), wd.getDefinition().getTraceId()));
		}
	}

	/**
	 * look for prime operators and replace them
	 * 
	 * @param spec
	 * @return
	 */
	private Spec replacePrimes(Spec spec, int traceId) {
		if (spec instanceof SpecExp) {
			SpecExp e = (SpecExp) spec;
			if (Operator.PRIME.equals(e.getOperator())) {
				Spec pSpec = primeVarAllReferences(e.getChildren()[0], traceId);
				return replacePrimes(pSpec, traceId);
			} else {
				for (int i = 0; i < e.getChildren().length; i++) {
					e.getChildren()[i] = replacePrimes(e.getChildren()[i], traceId);
				}
			}
		}
		return spec;
	}
	
	/**
	 * detect primes in triggers if there are any left after predicate translations
	 * @param spec
	 * @param traceId
	 * @return
	 */
	private Spec detectPrimes(Spec spec, int traceId) {
		if (spec instanceof SpecRegExp) {
			SpecRegExp e = (SpecRegExp) spec;
			
			detectPrimes(e.getPredicate(), traceId);
			detectPrimes(e.getLeft(), traceId);
			detectPrimes(e.getRight(), traceId);
			
		} else if (spec instanceof SpecExp) {
			SpecExp e = (SpecExp) spec;
			if (Operator.PRIME.equals(e.getOperator())) {
				throw new TranslationException("Regular expressions can't have primed ('next') variables", traceId);
			} else {
				for (int i = 0; i < e.getChildren().length; i++) {
					detectPrimes(e.getChildren()[i], traceId);
				}
			}
		}
		return spec;
	}

	/**
	 * replace reference names with primed versions
	 * 
	 * @param spec
	 */
	private Spec primeVarAllReferences(Spec spec, int traceId) {

		if (spec instanceof VariableReference) {
			String refName = ((VariableReference) spec).getReferenceName();
			if (refName.endsWith("'")) {
				throw new TranslationException("Cannot prime primed variable.", traceId);
			}
			return new VariableReference(((VariableReference) spec).getVariable(), refName + "'");
		} else if (spec instanceof SpecExp) {
			SpecExp e = (SpecExp) spec;
			for (int i = 0; i < e.getChildren().length; i++) {
				e.getChildren()[i] = primeVarAllReferences(e.getChildren()[i], traceId);
			}
		}
		return spec;
	}

}
