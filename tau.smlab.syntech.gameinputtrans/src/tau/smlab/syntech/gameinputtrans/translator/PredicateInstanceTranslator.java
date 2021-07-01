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
import tau.smlab.syntech.gameinput.model.Counter;
import tau.smlab.syntech.gameinput.model.Define;
import tau.smlab.syntech.gameinput.model.DefineArray;
import tau.smlab.syntech.gameinput.model.ExistentialConstraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Monitor;
import tau.smlab.syntech.gameinput.model.PatternConstraint;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.model.WeightDefinition;
import tau.smlab.syntech.gameinput.spec.DefineReference;
import tau.smlab.syntech.gameinput.spec.PredicateInstance;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecHelper;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gameinputtrans.TranslationException;

/**
 * instantiate all predicates
 * 
 * go over all specs (in guarantees, assumptions, auxiliary constraints, weight
 * definition, patterns?)
 * 
 * replace all PredicateInstance elements with a copy of the spec of the
 * predicate and then replace all parameters referenced inside the predicate
 * spec by the parameters defined in PredicateInstance
 * 
 * fially delete all predicates
 *
 */
public class PredicateInstanceTranslator implements Translator {

	@Override
	public void translate(GameInput input) {

		if (noWorkToDo(input)) {
			return;
		}

		// guarantees
		for (Constraint c : input.getSys().getConstraints()) {
			c.setSpec(replacePredicateInstances(c.getSpec()));
		}

		// sys existential constraints
		for (ExistentialConstraint exC : input.getSys().getExistentialConstraints()) {
			if (exC.isRegExp()) {
				SpecRegExp regExp = exC.getRegExp();
				for (SpecRegExp predRegExp : regExp.getPredicateSubExps()) {
					predRegExp.setPredicate(replacePredicateInstances(predRegExp.getPredicate()));
				}
			} else {
				for (int i = 0; i < exC.getSize(); i++) {
					exC.replaceSpec(i, replacePredicateInstances(exC.getSpec(i)));
				}
			}
		}
		
		//sys triggers
		replacePredicateInstancesInTriggers(input.getSys().getTriggers());

		// assumptions
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replacePredicateInstances(c.getSpec()));
		}
		
		//env triggers
		replacePredicateInstancesInTriggers(input.getEnv().getTriggers());

		// auxiliary constraints
		for (Constraint c : input.getAux().getConstraints()) {
			c.setSpec(replacePredicateInstances(c.getSpec()));
		}

		// weight definitions
		for (WeightDefinition wd : input.getWeightDefs()) {
			Constraint c = wd.getDefinition();
			c.setSpec(replacePredicateInstances(c.getSpec()));
		}

		// going over defines
		for (Define def : input.getDefines()) {
			if (def.getExpression() != null) {
				def.setExpression(replacePredicateInstances(def.getExpression()));
			} else {
				def.setDefineArray(replacePredicateInDefineArrays(def.getDefineArray()));
			}
		}
		
		for (Counter counter : input.getCounters()) {
			if (counter.getDecPred() != null) {
				counter.getDecPred().setContent(replacePredicateInstances(counter.getDecPred().getContent()));
			}
			if (counter.getIncPred() != null) {
				counter.getIncPred().setContent(replacePredicateInstances(counter.getIncPred().getContent()));
			}
			if (counter.getIniPred() != null) {
				counter.getIniPred().setContent(replacePredicateInstances(counter.getIniPred().getContent()));
			}
			if (counter.getResetPred() != null) {
				counter.getResetPred().setContent(replacePredicateInstances(counter.getResetPred().getContent()));
			}
		}

		// going over monitors
		for (Monitor mon : input.getMonitors()) {
			for (Constraint c : mon.getExpressions()) {
				c.setSpec(replacePredicateInstances(c.getSpec()));
			}
		}

		// pattern parameters
		for (PatternConstraint pc : input.getEnv().getPatterns()) {
			List<Spec> ps = pc.getParameters();
			for (int i = 0; i < pc.getParameters().size(); i++) {
				ps.set(i, replacePredicateInstances(ps.get(i)));
			}
		}
		for (PatternConstraint pc : input.getSys().getPatterns()) {
			List<Spec> ps = pc.getParameters();
			for (int i = 0; i < pc.getParameters().size(); i++) {
				ps.set(i, replacePredicateInstances(ps.get(i)));
			}
		}

		// clear all predicates
		input.getPredicates().clear();
	}
	
	private DefineArray replacePredicateInDefineArrays(DefineArray defArray) {

		List<Spec> newSpec = null;
		if (defArray.getExpressions() != null) {
			newSpec = new ArrayList<>();
			for (Spec exp : defArray.getExpressions()) {
				newSpec.add(replacePredicateInstances(exp));
			}
		}
		
		List<DefineArray> newDefArray = null;
		if (defArray.getDefineArray() != null) {
			newDefArray = new ArrayList<>();
			for (DefineArray innerArray : defArray.getDefineArray()) {
				newDefArray.add(replacePredicateInDefineArrays(innerArray));
			}
		}
		
		return new DefineArray(newSpec, newDefArray);
	}

	private boolean noWorkToDo(GameInput input) {
		return input.getPredicates() == null || input.getPredicates().isEmpty();
	}

	private Spec replacePredicateInstances(Spec spec) {
		if (spec instanceof PredicateInstance) {
			PredicateInstance pi = (PredicateInstance) spec;
			Spec clonedPredicateInstanceExpression = null;
			try {
				// clone so we don't destroy the original predicate expression - may be needed
				// for other entities too
				clonedPredicateInstanceExpression = pi.getPredicate().getExpression().clone();
			} catch (CloneNotSupportedException e) {
			}
			spec = removeParameterReferencesInPredicate(clonedPredicateInstanceExpression,
					pi.getPredicate().getParamsList(), pi.getParameters());
			// check if there are more instances of predicates after instantiation (nested
			// predicates)
			spec = replacePredicateInstances(spec);
		} else if (spec instanceof SpecExp) {
			SpecExp se = (SpecExp) spec;
			for (int i = 0; i < se.getChildren().length; i++) {
				se.getChildren()[i] = replacePredicateInstances(se.getChildren()[i]);
			}
		}
		return spec;
	}

	private Spec removeParameterReferencesInPredicate(Spec predicateSpec, List<Variable> formalParams,
			List<Spec> actualParamsValues) {
		if (predicateSpec instanceof VariableReference) {
			VariableReference varRef = (VariableReference) predicateSpec;

			if (formalParams.contains(varRef.getVariable())) {
				// varRef points to formal parameters
				int indexOfParam = formalParams.indexOf(varRef.getVariable());
				try {
					String arrayCoord = varRef.getReferenceName().substring(varRef.getVariable().getName().length());
					predicateSpec = actualParamsValues.get(indexOfParam).clone();
					if (arrayCoord.length() != 0 && predicateSpec instanceof VariableReference) {
						((VariableReference) predicateSpec)
								.setReferenceName(((VariableReference) predicateSpec).getReferenceName() + arrayCoord);
					}
				} catch (CloneNotSupportedException e) {
					e.printStackTrace();
				}
			} else if (varRef.getIndexSpecs() != null) {
				List<Spec> newIndexSpec = new ArrayList<Spec>();
				for (Spec indexSpec : varRef.getIndexSpecs()) {
					indexSpec = removeParameterReferencesInPredicate(indexSpec, formalParams, actualParamsValues);
					newIndexSpec.add(indexSpec);
				}
				varRef.setIndexSpecs(newIndexSpec);
				
				for (Variable var : formalParams) {
					if (varRef.getIndexVars().containsValue(var)) {
						varRef.getIndexVars().remove(var.getName());
					}
				}
				
				try {
					SpecHelper.updateRefName(varRef);
				} catch (Exception e) {
					throw new TranslationException(e.getMessage(), 0);
				}
			}
		} else if (predicateSpec instanceof DefineReference) {
			
			DefineReference defRef = (DefineReference) predicateSpec;
			
			if (defRef.getIndexSpecs() != null) {
				
				try {
					DefineReference newDefRef = defRef.clone();
					
					List<Spec> newIndexSpec = new ArrayList<Spec>();
					for (Spec indexSpec : newDefRef.getIndexSpecs()) {
						indexSpec = removeParameterReferencesInPredicate(indexSpec, formalParams, actualParamsValues);
						newIndexSpec.add(indexSpec);
					}
					newDefRef.setIndexSpecs(newIndexSpec);
					
					for (Variable var : formalParams) {
						if (newDefRef.getIndexVars().containsValue(var)) {
							newDefRef.getIndexVars().remove(var.getName());
						}
					}
					
					if (newDefRef.getIndexVars().isEmpty()) {
						for (int i = 0; i < newDefRef.getIndexSpecs().size(); i++) {
							Integer indexValue = SpecHelper.calculateSpec(newDefRef.getIndexSpecs().get(i));
							if (indexValue < 0 || indexValue >= newDefRef.getIndexDimensions().get(i)) {
								throw new Exception(String.format("Index %d out of bounds for variable %s", indexValue,
										newDefRef.getDefine().getName()));
							}
						}
					}
					
					predicateSpec = newDefRef;
					
				} catch (Exception e) {
					throw new TranslationException(e.getMessage(), 0);
				}
			}
		
			// else: varRef points to a global variable. no need to replace
		} else if (predicateSpec instanceof SpecExp) {
			SpecExp se = (SpecExp) predicateSpec;
			for (int i = 0; i < se.getChildren().length; i++) {
				se.getChildren()[i] = removeParameterReferencesInPredicate(se.getChildren()[i], formalParams,
						actualParamsValues);
			}
		} else if (predicateSpec instanceof PredicateInstance) {
			// replace references in params of nested predicates
			List<Spec> params = ((PredicateInstance) predicateSpec).getParameters();
			for (int i = 0; i < params.size(); i++) {
				params.set(i, removeParameterReferencesInPredicate(params.get(i), formalParams, actualParamsValues));
			}
		}
		return predicateSpec;
	}
	
	private void replacePredicateInstancesInTriggers(List<TriggerConstraint> moduleTriggers) {
		SpecRegExp initSpecRegExp, effectSpecRegExp;
		for(TriggerConstraint trigger : moduleTriggers) {
			initSpecRegExp = trigger.getInitSpecRegExp();
			for(SpecRegExp predRegExp : initSpecRegExp.getPredicateSubExps()) {
				predRegExp.setPredicate(replacePredicateInstances(predRegExp.getPredicate()));
			}
			effectSpecRegExp = trigger.getEffectSpecRegExp();
			for(SpecRegExp predRegExp : effectSpecRegExp.getPredicateSubExps()) {
				predRegExp.setPredicate(replacePredicateInstances(predRegExp.getPredicate()));
			}
		}
	}

}