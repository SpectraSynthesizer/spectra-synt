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
import tau.smlab.syntech.gameinput.model.DefineArray;
import tau.smlab.syntech.gameinput.model.ExistentialConstraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Pattern;
import tau.smlab.syntech.gameinput.model.Predicate;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.gameinput.model.WeightDefinition;
import tau.smlab.syntech.gameinput.spec.DefineReference;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecHelper;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;
import tau.smlab.syntech.gameinputtrans.TranslationException;

/**
 * iterate over all specs (in guarantees, assumptions, auxiliary constraints,
 * weight definition, patterns?, predicates)
 * 
 * if spec contains a DefineReference replace it by the spec of the define
 * 
 * finally delete the list of defines
 * 
 * Depends on : VarIndexesTranslator, QuantifierTranslator
 *
 */

public class DefinesTranslator implements Translator {

	@Override
	public void translate(GameInput input) {

		if (noWorkToDo(input)) {
			return;
		}
		// guarantees
		for (Constraint c : input.getSys().getConstraints()) {
			c.setSpec(replaceDefines(c.getSpec(), c.getTraceId()));
		}

		// sys existential constraints
		for (ExistentialConstraint exC : input.getSys().getExistentialConstraints()) {
			if(exC.isRegExp()) {
				SpecRegExp regExp = exC.getRegExp();
				for(SpecRegExp predRegExp : regExp.getPredicateSubExps()) {
					predRegExp.setPredicate(replaceDefines(predRegExp.getPredicate(), 0));
				}
			}
			else {
				for(int i = 0; i < exC.getSize() ; i++) {
					exC.replaceSpec(i, replaceDefines(exC.getSpec(i), exC.getTraceId()));
				}
			}
		}
		
		// sys triggers
		replaceDefinesInTriggers(input.getSys().getTriggers());

		// assumptions
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replaceDefines(c.getSpec(), c.getTraceId()));
		}
		
		// env triggers
		replaceDefinesInTriggers(input.getEnv().getTriggers());

		// auxiliary constraints
		for (Constraint c : input.getAux().getConstraints()) {
			c.setSpec(replaceDefines(c.getSpec(), c.getTraceId()));
		}

		// weight definition
		for (WeightDefinition wd : input.getWeightDefs()) {
			Constraint c = wd.getDefinition();
			c.setSpec(replaceDefines(c.getSpec(), c.getTraceId()));
		}

		// patterns
		for (Pattern patt : input.getPatterns()) {
			for (Constraint c : patt.getExpressions()) {
				c.setSpec(replaceDefines(c.getSpec(), c.getTraceId()));
			}
		}

		// predicates
		for (Predicate pred : input.getPredicates()) {
			pred.setSpec(replaceDefines(pred.getExpression(), pred.getTraceId()));

		}
		
		for (Counter counter : input.getCounters()) {
			if (counter.getDecPred() != null) {
				counter.getDecPred().setContent(replaceDefines(counter.getDecPred().getContent(), counter.getTraceId()));
			}
			if (counter.getIncPred() != null) {
				counter.getIncPred().setContent(replaceDefines(counter.getIncPred().getContent(), counter.getTraceId()));
			}
			if (counter.getIniPred() != null) {
				counter.getIniPred().setContent(replaceDefines(counter.getIniPred().getContent(), counter.getTraceId()));
			}
			if (counter.getResetPred() != null) {
				counter.getResetPred().setContent(replaceDefines(counter.getResetPred().getContent(), counter.getTraceId()));
			}
		}

		// clear the list of the defines
		input.getDefines().clear();
	}

	private void replaceDefinesInTriggers(List<TriggerConstraint> moduleTriggers) {
		SpecRegExp initSpecRegExp, effectSpecRegExp;
		for(TriggerConstraint trigger : moduleTriggers) {
			initSpecRegExp = trigger.getInitSpecRegExp();
			for(SpecRegExp predRegExp : initSpecRegExp.getPredicateSubExps()) {
				predRegExp.setPredicate(replaceDefines(predRegExp.getPredicate(), 0));
			}
			effectSpecRegExp = trigger.getEffectSpecRegExp();
			for(SpecRegExp predRegExp : effectSpecRegExp.getPredicateSubExps()) {
				predRegExp.setPredicate(replaceDefines(predRegExp.getPredicate(), 0));
			}
		}
	}

	private boolean noWorkToDo(GameInput input) {
		return input.getDefines() == null || input.getDefines().isEmpty();
	}

	private Spec replaceDefines(Spec spec, int traceId) {
		if (spec instanceof DefineReference) {
			DefineReference dr = (DefineReference) spec;
			// important: spec = dr.getDefine().getExpression() isn't enough! there might be nested defines. 
			try {
				
				DefineReference newdr = dr.clone();

				if (newdr.getIndexSpecs() != null) {
					List<Spec> indexSpecs = new ArrayList<>();
					for (Spec indexSpec : newdr.getIndexSpecs()) {
						indexSpecs.add(replaceDefines(indexSpec, traceId));	
					}
					newdr.setIndexSpecs(indexSpecs);
				}
				
				// Single define
				if (newdr.getDefine().getExpression() != null) {
					newdr.getDefine().setExpression(replaceDefines(newdr.getDefine().getExpression(), traceId));
					return newdr.getDefine().getExpression();
				}
				
				// Multiple define
				newdr.getDefine().setDefineArray(replaceDefinesInDefineArrays(newdr.getDefine().getDefineArray(), traceId));
				
				if (newdr.getIndexVars().isEmpty()) {
					return extractSpecFromDefine(newdr.getIndexSpecs(), 0, newdr.getDefine().getDefineArray());
				}
				
				return newdr;
				
			} catch (Exception e) {
				throw new TranslationException(e.getMessage(), traceId);
			}
			
		} else if (spec instanceof SpecExp) {
			
			try {
				SpecExp se = ((SpecExp) spec).clone();
				for (int i = 0; i < se.getChildren().length; i++) {
					se.getChildren()[i] = replaceDefines(se.getChildren()[i], traceId);
				}
				
				return se;
			} catch (Exception e) {
				throw new TranslationException(e.getMessage(), traceId);
			}
		}
		return spec;
	}
	
	private Spec extractSpecFromDefine(List<Spec> indexSpec, int index, DefineArray defArray) throws Exception {
		
		Integer indexValue = SpecHelper.calculateSpec(indexSpec.get(index));
		
		if (index == indexSpec.size() - 1) {
			
			if (defArray.getExpressions() == null || defArray.getExpressions().size() == 0) {
				throw new Exception("Invalid access to a define array, too few []");
			}
			
			if (defArray.getExpressions().size() <= indexValue) {
				throw new Exception("Invalid access to a define array, out of bounds");
			}
			
			return defArray.getExpressions().get(indexValue);
		} else {
			
			if (defArray.getDefineArray() == null || defArray.getDefineArray().size() == 0) {
				throw new Exception("Invalid access to a define array, too many []");
			}
			
			if (defArray.getDefineArray().size() <= indexValue) {
				throw new Exception("Invalid access to a define array, out of bounds");
			}
			
			return extractSpecFromDefine(indexSpec, index + 1, defArray.getDefineArray().get(indexValue));
		}
	}
	
	private DefineArray replaceDefinesInDefineArrays(DefineArray defArray, int traceId) {

		List<Spec> newSpec = null;
		if (defArray.getExpressions() != null) {
			newSpec = new ArrayList<>();
			for (Spec exp : defArray.getExpressions()) {
				newSpec.add(replaceDefines(exp, traceId));
			}
		}
		
		List<DefineArray> newDefArray = null;
		if (defArray.getDefineArray() != null) {
			newDefArray = new ArrayList<>();
			for (DefineArray innerArray : defArray.getDefineArray()) {
				newDefArray.add(replaceDefinesInDefineArrays(innerArray, traceId));
			}
		}
		
		return new DefineArray(newSpec, newDefArray);
	}

}
