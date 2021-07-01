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

import java.util.ArrayList;
import java.util.List;

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.Define;
import tau.smlab.syntech.gameinput.model.DefineArray;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Predicate;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.spec.DefineReference;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PredicateInstance;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.QuantifiedSpec;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecHelper;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gameinputtrans.TranslationException;

public class QuantifierTranslator implements Translator {

	@Override
	public void translate(GameInput input) {
		// guarantees
		for (Constraint c : input.getSys().getConstraints()) {
			c.setSpec(replaceQuantifiers(c.getSpec(), c.getTraceId()));
		}

		// assumptions
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replaceQuantifiers(c.getSpec(), c.getTraceId()));
		}

		// auxiliary constraints
		for (Constraint c : input.getAux().getConstraints()) {
			c.setSpec(replaceQuantifiers(c.getSpec(), c.getTraceId()));
		}
		
		// predicates
		for (Predicate pred : input.getPredicates()) {
			pred.setSpec(replaceQuantifiers(pred.getExpression(), pred.getTraceId()));
		}

		// defines
		for (Define define : input.getDefines()) {
			
			if (define.getExpression() != null) {
				define.setExpression(replaceQuantifiers(define.getExpression(), 0));
			} else {
				define.setDefineArray(replaceQuantifiersInDefineArrays(define.getDefineArray()));
			}
		}

		// Clear Domain variables list
		// input.getDomainVars().clear();
	}

	/**
	 * @param spec
	 * @return the Spec that represent the result of the reduction from
	 *         QuantifiedSpec to SpecExp
	 */
	private Spec replaceQuantifiers(Spec spec, int traceId) {

		if (spec instanceof SpecExp) {
			SpecExp specExp = (SpecExp) spec;
			for (int i = 0; i < specExp.getChildren().length; i++) {
				specExp.getChildren()[i] = replaceQuantifiers(specExp.getChildren()[i], traceId);
			}

			return spec;
		}

		if (!(spec instanceof QuantifiedSpec)) {
			return spec;
		}
		QuantifiedSpec qSpec = (QuantifiedSpec) spec;

		// need to translate the inner quantifier first.
		qSpec.setTempExpr(replaceQuantifiers(qSpec.getTempExpr(), traceId));

		// we don't want to translate an expression
		// that doesn't contains the domain var of the current quantifier expression
//		if (!getVarsListOfSpec(qSpec.getTempExpr()).contains(qSpec.getDomainVar())) {
//			// in that case, we can just ignore the current quantifier
//			return qSpec.getTempExpr();
//		}

		Operator op = qSpec.getExprOperator(); // op=AND for FORALL and op=OR for EXISTS

		// we get a list of all the values that the current domain var can get
		List<PrimitiveValue> values = qSpec.getDomainVar().getType().getPrimitivesList();

		Spec total = null;
		try {
			for (PrimitiveValue val : values) { // for every value of the domain var
				// we try to replace the domain var with the current value
				Spec newSpec = translateSpec(qSpec.getTempExpr(), qSpec.getDomainVar(), val, traceId);
	
				// connects the current new_spec to the whole result spec
				if (total == null) {
					total = newSpec;
				} else {
					total = new SpecExp(op, total, newSpec);
				}
			}
		} catch (Exception e) {
			throw new TranslationException(e.getMessage(), traceId);
		}

		return total;
	}

	/**
	 * @param spec
	 * @param domainVar
	 * @param primVal
	 * @return the Spec input after replacing all appearances of domainVar with
	 *         primVal
	 */
	private Spec translateSpec(Spec spec, Variable domainVar, PrimitiveValue primVal, int traceId) throws CloneNotSupportedException {
		if (spec instanceof VariableReference) {
			// if we arrived to VariableReference we check if it's a domain var
			VariableReference varRef = (VariableReference) spec;

			if (varRef.getVariable().getName().equals(domainVar.getName())) {
				// if it's the current domain var, we return the primitive value instead
				return new PrimitiveValue(primVal.getValue());
			} else if (varRef.getIndexSpecs() != null) {
				try {					
					if (varRef.getIndexVars().containsKey(domainVar.getName())) {
						VariableReference newVarRef = varRef.clone();
						for (int i = 0; i < newVarRef.getIndexSpecs().size(); i++) {
							Spec interpreted = SpecHelper.interpretWithVariable(newVarRef.getIndexSpecs().get(i), domainVar, primVal);
							newVarRef.getIndexSpecs().set(i, interpreted);
						}
						newVarRef.getIndexVars().remove(domainVar.getName());
						
						SpecHelper.updateRefName(newVarRef);
				        return newVarRef;
					}
				} catch (Exception e) {
					throw new TranslationException(e.getMessage(), traceId);
				}
			}
			
			return varRef;
			
		} else if (spec instanceof DefineReference) {
			DefineReference defRef = (DefineReference) spec;
			
			try {
				if (defRef.getIndexSpecs() != null && defRef.getIndexVars().containsKey(domainVar.getName())) {
					DefineReference newDefRef = defRef.clone();
					
					SpecHelper.updateDefineReference(newDefRef, domainVar, primVal);

					return newDefRef;
				}
			} catch (Exception e) {
				throw new TranslationException(e.getMessage(), traceId);
			}
			
			return defRef;
			
		// if we arrived to PredicateInstance, we want to replace all the appearances of
		// the current domain var in the parameters of the PredicateInstance
		} else if (spec instanceof PredicateInstance) {
			PredicateInstance pi = ((PredicateInstance) spec).clone();
			List<Spec> params = pi.getParameters();
			List<Spec> translatedParams = new ArrayList<Spec>();
			for (int i = 0; i < params.size(); i++) {
				translatedParams.add(translateSpec(params.get(i), domainVar, primVal, traceId));
			}
			pi.setParameters(translatedParams);
			return pi;
		} else if (spec instanceof SpecExp) {

			// if we got here, the current spec is a SpecExp we want to run on it's children array
			// and replace all the appearances of the current domain var in it with primVal.
			SpecExp specExp = ((SpecExp) spec).clone();
			for (int i = 0; i < specExp.getChildren().length; i++) {
				specExp.getChildren()[i] = translateSpec(specExp.getChildren()[i], domainVar, primVal, traceId);
			}
			return specExp;
		}
		
		return spec;
	}
	
	private DefineArray replaceQuantifiersInDefineArrays(DefineArray defArray) {

		List<Spec> newSpec = null;
		if (defArray.getExpressions() != null) {
			newSpec = new ArrayList<>();
			for (Spec exp : defArray.getExpressions()) {
				newSpec.add(replaceQuantifiers(exp, 0));
			}
		}
		
		List<DefineArray> newDefArray = null;
		if (defArray.getDefineArray() != null) {
			newDefArray = new ArrayList<>();
			for (DefineArray innerArray : defArray.getDefineArray()) {
				newDefArray.add(replaceQuantifiersInDefineArrays(innerArray));
			}
		}
		
		return new DefineArray(newSpec, newDefArray);
	}
}
