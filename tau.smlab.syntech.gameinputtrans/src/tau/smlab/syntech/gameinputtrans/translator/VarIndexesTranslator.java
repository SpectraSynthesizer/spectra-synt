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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.ExistentialConstraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.spec.DefineReference;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecHelper;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gameinputtrans.TranslationException;


/**
 * Depends on QuantifierTranslator
 * 
 *
 */
public class VarIndexesTranslator implements Translator {

	@Override
	public void translate(GameInput input) {
		// guarantees
		for (Constraint c : input.getSys().getConstraints()) {
			// first, replace all the var indexes inside PastLTL expressions
			c.setSpec(replaceVarIndexesInsidePastLTL(c.getSpec(), c.getTraceId()));

			// then, replace all the var indexes inside next expression (true)
			// and finaly, replace all the remain var indexes of the expression (false)
			c.setSpec(replaceVarIndexes(replaceVarIndexes(c.getSpec(), true, c.getTraceId()), false, c.getTraceId()));
		}
		
		// sys existential constraints
		for (ExistentialConstraint exC : input.getSys().getExistentialConstraints()) {
			if(exC.isRegExp()) {
				SpecRegExp regExp = exC.getRegExp();
				for(SpecRegExp predRegExp : regExp.getPredicateSubExps()) {  //we assume that there are no 'next' operators and no PastLTL operators
					predRegExp.setPredicate(replaceVarIndexes(predRegExp.getPredicate(), false, exC.getTraceId()));
				}
			}
			else {
				for(int i = 0; i < exC.getSize() ; i++) {
					exC.replaceSpec(i, replaceVarIndexesInsidePastLTL(exC.getSpec(i), exC.getTraceId()));
					exC.replaceSpec(i, replaceVarIndexes(exC.getSpec(i), false, exC.getTraceId())); //we assume that there are no 'next' operators
				}
			}
		}
		
		// sys triggers
		replaceVarIndexesInTriggers(input.getSys().getTriggers());

		// assumptions
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replaceVarIndexesInsidePastLTL(c.getSpec(), c.getTraceId()));
			c.setSpec(replaceVarIndexes(replaceVarIndexes(c.getSpec(), true, c.getTraceId()), false, c.getTraceId()));

		}
		
		// env triggers
		replaceVarIndexesInTriggers(input.getEnv().getTriggers());
		
		// auxiliary constraints
		for (Constraint c : input.getAux().getConstraints()) {
			c.setSpec(replaceVarIndexesInsidePastLTL(c.getSpec(), c.getTraceId()));
			c.setSpec(replaceVarIndexes(replaceVarIndexes(c.getSpec(), true, c.getTraceId()), false, c.getTraceId()));
		}

	}
	
	private void replaceVarIndexesInTriggers(List<TriggerConstraint> moduleTriggers) {
		SpecRegExp initSpecRegExp, effectSpecRegExp;
		for(TriggerConstraint trigger : moduleTriggers) {
			initSpecRegExp = trigger.getInitSpecRegExp();
			for(SpecRegExp predRegExp : initSpecRegExp.getPredicateSubExps()) {
				predRegExp.setPredicate(replaceVarIndexes(predRegExp.getPredicate(), false, trigger.getTraceId()));
			}
			effectSpecRegExp = trigger.getEffectSpecRegExp();
			for(SpecRegExp predRegExp : effectSpecRegExp.getPredicateSubExps()) {
				predRegExp.setPredicate(replaceVarIndexes(predRegExp.getPredicate(), false, trigger.getTraceId()));
			}
		}
	}

	/**
	 * @param spec
	 * @nextReplace
	 * @return the result spec after replacing all the var indexes the input spec or
	 *         only on its next expressions
	 */
	private Spec replaceVarIndexes(Spec spec, boolean nextsReplace, int traceId) {

		// first we get all the vars indexes that in the spec (or only on it's next
		// expressions)
		Set<Variable> vars = getVarsIndexesListOfSpec(spec, nextsReplace, false);
		if (vars.isEmpty()) {
			return spec;
		}

		// for every var index we want to get a list of all the values that it can get
		for (Variable var : vars) {
			List<PrimitiveValue> values = var.getType().getPrimitivesList();

			Spec total = null;
			
			try {

				// for every value that the current var index can get
				for (PrimitiveValue val : values) {
	
					// we create a new Variable Reference for the index and it will be inside
					// a Equality SpecExp with the it's current value
					// that way we can do the reduction from "array[x]" to "array[val]&(x=val)" for
					// every val that x can get with OR between them
					VariableReference vRef = new VariableReference(var, var.getName());
					Spec indexVal = new SpecExp(Operator.EQUALS, vRef, val);
	
					// if we replace indexes inside next now, we want to do the reduction
					// from "next(array[x]=y)" to "next(array[val]=y)&next(x=val)",
					// again for all val with OR between them
	
					// we wanted to do the reduction from
					// "next(array[x]=y)" to "next(array[val]=y & x=val)"
					// but next can contains a non-boolean expression like "next(array[x])"
					// and then the second reduction has no meaning
					// and the first will transform "next(array[x])" to
					// "next(array[val])&next(x=val)"
					// for every val with OR between the expressions
					if (nextsReplace) {
						indexVal = new SpecExp(Operator.PRIME, indexVal);
					}
					// we replace all the appearences of the index var inside the spec with val
					Spec newSpec = translateSpec(spec, var, val, nextsReplace, false, traceId);
	
					if (total == null) {
						total = new SpecExp(Operator.AND, newSpec, indexVal);
					} else {
						total = new SpecExp(Operator.OR, total, new SpecExp(Operator.AND, newSpec, indexVal));
					}
				}
				spec = total;
			} catch (Exception e) {
				throw new TranslationException(e.getMessage(), traceId);
			}
		}
		return spec;
	}

	/**
	 * @param spec
	 * @param indexVar
	 * @param primVal
	 * @param nextTranslation
	 * @param insideOfNext
	 * @return the result spec after replacing all the indexVar appearances in spec
	 *         with the value of primVal and doing that only inside of Next
	 *         expressions if told to.
	 */
	private Spec translateSpec(Spec spec, Variable var, 
			PrimitiveValue primVal, boolean nextTranslation, boolean insideOfNext, int traceId) throws CloneNotSupportedException {
		if (spec instanceof VariableReference) {
			VariableReference varRef = (VariableReference) spec;
			if (varRef.getIndexSpecs() != null) {
				if (nextTranslation == insideOfNext) {
					try {
						if (varRef.getIndexVars().containsKey(var.getName())) {
							VariableReference newVarRef = varRef.clone();
							for (int i = 0; i < newVarRef.getIndexSpecs().size(); i++) {
								Spec interpreted = SpecHelper.interpretWithVariable(newVarRef.getIndexSpecs().get(i), var, primVal);
								newVarRef.getIndexSpecs().set(i, interpreted);
							}
							newVarRef.getIndexVars().remove(var.getName());
							
							SpecHelper.updateRefName(newVarRef);
					        return newVarRef;
						}
					} catch (Exception e) {
						throw new TranslationException(e.getMessage(), traceId);
					}
				}
			}
			
		} else if (spec instanceof DefineReference) {
			DefineReference defRef = (DefineReference) spec;
			
			try {					
				if (defRef.getIndexSpecs() != null && defRef.getIndexVars().containsKey(var.getName())) {
					DefineReference newDefRef = defRef.clone();
					
					SpecHelper.updateDefineReference(newDefRef, var, primVal);

					return newDefRef;
				}
			} catch (Exception e) {
				throw new TranslationException(e.getMessage(), traceId);
			}
			
			return defRef;
		} else if (spec instanceof SpecExp) {
			SpecExp specExp = ((SpecExp) spec).clone();
			Operator op = specExp.getOperator();

			if (!op.isPastLTLOp()) {
				for (int i = 0; i < specExp.getChildren().length; i++) {
					specExp.getChildren()[i] = translateSpec(specExp.getChildren()[i], var, primVal, nextTranslation,
							op.equals(Operator.PRIME) || insideOfNext, traceId);
				}
			}
			
			return specExp;
		}

		return spec;
	}

	/**
	 * @param spec
	 * @return the result spec after replacing all the var indexes inside of PastLTL
	 *         expression in the input spec.
	 */
	private Spec replaceVarIndexesInsidePastLTL(Spec spec, int traceId) {
		// PastLTL operator can get only boolean values
		// so the reduction will take the form of:
		// PREV(array[x]=y) to PREV(array[val]=y & (x=val)) for every val that
		// x can get with OR between the expressions.
		// same for next inside PastLTL operator.

		// Stopping condition
		if (!(spec instanceof SpecExp)) {
			return spec;
		}

		SpecExp specExp = (SpecExp) spec;

		if (specExp.getOperator().isPastLTLOp()) {
			for (int i = 0; i < specExp.getChildren().length; i++) {
				// for nested past ltl op
				specExp.getChildren()[i] = replaceVarIndexesInsidePastLTL(specExp.getChildren()[i], traceId);
				// for next inside past ltl											
				specExp.getChildren()[i] = replaceVarIndexes(specExp.getChildren()[i], true, traceId);
				specExp.getChildren()[i] = replaceVarIndexes(specExp.getChildren()[i], false, traceId);
			}
		} else {
			for (int i = 0; i < specExp.getChildren().length; i++) {
				specExp.getChildren()[i] = replaceVarIndexesInsidePastLTL(specExp.getChildren()[i], traceId);
			}
		}
		return specExp;
	}

	/**
	 * @param spec
	 * @return a list of all the var indexes in that in the input spec without
	 *         double indexes according to the conditions in the inputs
	 */
	private Set<Variable> getVarsIndexesListOfSpec(Spec spec, boolean getOnlyIndexesInNext, boolean insideOfNext) {

		Set<Variable> vars = new HashSet<Variable>();

		if (spec instanceof VariableReference) {
			VariableReference vr = (VariableReference) spec;
			if (getOnlyIndexesInNext == insideOfNext) {
				if (vr.getIndexVars() != null) {
					vars.addAll(vr.getIndexVars().values());
				}
			}
		} else if (spec instanceof DefineReference) {
			DefineReference dr = (DefineReference) spec;
			if (dr.getIndexVars() != null) {
				vars.addAll(dr.getIndexVars().values());
			}
		}
		
		if (spec instanceof SpecExp) {
			SpecExp se = (SpecExp) spec;
			Operator op = se.getOperator();
			if (!op.isPastLTLOp()) {
				for (Spec s : se.getChildren()) {
					vars.addAll(getVarsIndexesListOfSpec(s, getOnlyIndexesInNext, op.equals(Operator.PRIME) || insideOfNext));
				}
			}
		}
		return vars;
	}
}