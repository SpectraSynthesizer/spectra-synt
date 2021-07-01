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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.Constraint.Kind;
import tau.smlab.syntech.gameinput.model.Counter;
import tau.smlab.syntech.gameinput.model.Define;
import tau.smlab.syntech.gameinput.model.DefineArray;
import tau.smlab.syntech.gameinput.model.ExistentialConstraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Monitor;
import tau.smlab.syntech.gameinput.model.PatternConstraint;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.model.WeightDefinition;
import tau.smlab.syntech.gameinput.spec.CounterReference;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;
import tau.smlab.syntech.gameinput.spec.SpecTraceable;
import tau.smlab.syntech.gameinput.spec.VariableReference;

public class CounterTranslator implements Translator {

	protected Map<String, List<SpecTraceable>> counterPredicates = new HashMap<>();
	protected Map<String, Set<Integer>> traceIdsByCounter = new HashMap<>();
	protected List<String> counterNameList = new ArrayList<>();

	@Override
	public void translate(GameInput input) {

		if (noWorkToDo(input)) {
			return;
		}
		// translate counters
		Map<String, Variable> counterVars = translateCounters(input);

		// replace the counter references in
		// guarantees
		for (Constraint c : input.getSys().getConstraints()) {
			c.setSpec(replaceCounterRefs(counterVars, c.getSpec()));
		}
		
		// sys triggers
		replaceCounterRefsInTriggers(counterVars, input.getSys().getTriggers());
		
		// sys existential constraints
		for (ExistentialConstraint exC : input.getSys().getExistentialConstraints()) {
			if(exC.isRegExp()) {
				SpecRegExp regExp = exC.getRegExp();
				for(SpecRegExp predRegExp : regExp.getPredicateSubExps()) {
					predRegExp.setPredicate(replaceCounterRefs(counterVars, predRegExp.getPredicate()));
				}
			}
			else {
				for(int i = 0; i < exC.getSize() ; i++) {
					exC.replaceSpec(i, replaceCounterRefs(counterVars, exC.getSpec(i)));
				}
			}
		}

		// assumptions
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replaceCounterRefs(counterVars, c.getSpec()));
		}
		
		// env triggers
		replaceCounterRefsInTriggers(counterVars, input.getEnv().getTriggers());

		// auxiliary constraints
		for (Constraint c : input.getAux().getConstraints()) {
			c.setSpec(replaceCounterRefs(counterVars, c.getSpec()));
		}

		// defines
		for (Define d : input.getDefines()) {
			
			if (d.getExpression() != null) {
				d.setExpression(replaceCounterRefs(counterVars, d.getExpression()));
			} else {
				d.setDefineArray(replaceCounterInDefineArrays(counterVars, d.getDefineArray()));
			}
		}

		// weight definition
		for (WeightDefinition wd : input.getWeightDefs()) {
			Constraint c = wd.getDefinition();
			c.setSpec(replaceCounterRefs(counterVars, c.getSpec()));
		}


		// going over monitors
		for (Monitor mon : input.getMonitors()) {
			for (Constraint c : mon.getExpressions()) {
				c.setSpec(replaceCounterRefs(counterVars, c.getSpec()));
			}
		}

		// pattern parameters
		for (PatternConstraint pc : input.getEnv().getPatterns()) {
			List<Spec> ps = pc.getParameters();
			for (int i = 0; i < pc.getParameters().size(); i++) {
				ps.set(i, replaceCounterRefs(counterVars, ps.get(i)));
			}
		}
		for (PatternConstraint pc : input.getSys().getPatterns()) {
			List<Spec> ps = pc.getParameters();
			for (int i = 0; i < pc.getParameters().size(); i++) {
				ps.set(i, replaceCounterRefs(counterVars, ps.get(i)));
			}
		}

		input.getCounters().clear();
	}

	/**
	 * replace the counter references by references to the new variables
	 * 
	 * @param monVars
	 * @param spec
	 * @return
	 */
	private Spec replaceCounterRefs(Map<String, Variable> counterVars, Spec spec) {
		if (spec instanceof CounterReference) {
			// replace counter reference by aux var
			String name = ((CounterReference) spec).getCounter().getName();
			return new VariableReference(counterVars.get(name));
		} else if (spec instanceof SpecExp) {
			// replace references in all children
			SpecExp se = (SpecExp) spec;
			for (int i = 0; i < se.getChildren().length; i++) {
				se.getChildren()[i] = replaceCounterRefs(counterVars, se.getChildren()[i]);
			}
		}
		// nothing to do
		return spec;
	}

	/**
	 * Translate all counters by creating auxiliary variables and creating auxiliary constraints.
	 * 
	 * @param input
	 * @return
	 */
	private Map<String, Variable> translateCounters(GameInput input) {

		Map<String, Variable> counterVars = new HashMap<>();
		Set<Integer> traceIdsSet;

		for (Counter count : input.getCounters()) {

			//maintain a list of all counter names
			counterNameList.add(count.getName());

			// create auxiliary variable for counter
			TypeDef t = new TypeDef(count.getLower(), count.getUpper());
			Variable counterVar = new Variable(count.getName(), t);
			input.getAux().addVar(counterVar);
			counterVars.put(count.getName(), counterVar);

			//maintain a traceIds set of the counter's constraints
			traceIdsSet = new HashSet<>();
			traceIdsByCounter.put(count.getName(), traceIdsSet);
			traceIdsSet.add(count.getTraceId()); // safety constraints have the TraceId of the counter

			// add the counter's predicates to a counter-to-predicates mapping
			List<SpecTraceable> predicates = new ArrayList<SpecTraceable>();
			if (count.getDecPred() != null) {
				predicates.add(count.getDecPred());
			}
			if (count.getResetPred() != null) {
				predicates.add(count.getResetPred());
			}
			if (count.getIncPred() != null) {
				predicates.add(count.getIncPred());
			}
			counterPredicates.put(count.getName(), predicates);

			// add initial constraint to AUX player
			if (count.getIniPred() != null) {
				Constraint c = new Constraint(Kind.INI, count.getIniPred().getContent(), count.getName() + ".ini",
						count.getIniPred().getTraceId());

				// add counter's initial constraints to AUX player
				input.getAux().addConstraint(c);

				//add the current constraint c and its traceId
				traceIdsSet.add(c.getTraceId());
			}

			Spec step = new PrimitiveValue(1);
			Spec max = new PrimitiveValue(count.getUpper());
			Spec min = new PrimitiveValue(count.getLower());
			Spec counterVarRef = new VariableReference(counterVar, counterVar.getName());
			Spec nextCounterVarRef = new SpecExp(Operator.PRIME, counterVarRef); // next operator

			// create and add increment constraint
			if (count.getIncPred() != null) {
				Spec specIncVal = new SpecExp(Operator.EQUALS, nextCounterVarRef,
						new SpecExp(Operator.ADD, counterVarRef, step));

				Spec overflow = null;
				Spec atMax = new SpecExp(Operator.EQUALS, counterVarRef, max);

				switch (count.getOverFlowMethod()) {
				case FALSE:
					overflow = new PrimitiveValue("FALSE");
					break;
				case KEEP: // increment of max value stays at max value
					Spec nextAtMax = new SpecExp(Operator.EQUALS, nextCounterVarRef, max);
					overflow = new SpecExp(Operator.AND, atMax, nextAtMax);
					break;
				case MODULO:
					Spec nextAtMin = new SpecExp(Operator.EQUALS, nextCounterVarRef, min);
					overflow = new SpecExp(Operator.AND, atMax, nextAtMin);
					break;
				default:
					break;
				}

				Spec incPred = count.getIncPred().getContent();
				Spec spIncFinalExp = new SpecExp(Operator.IMPLIES, incPred,
						new SpecExp(Operator.OR, specIncVal, overflow));
				Constraint incConst = new Constraint(Kind.SAFETY, spIncFinalExp, count.getName() + ".inc", count.getIncPred().getTraceId());
				input.getAux().addConstraint(incConst);
			}

			// create and add decrement constraint.
			if (count.getDecPred() != null) {
				Spec specDecVal = new SpecExp(Operator.EQUALS, nextCounterVarRef,
						new SpecExp(Operator.SUBSTRACT, counterVarRef, step));

				Spec underflow = null;
				Spec atMin = new SpecExp(Operator.EQUALS, counterVarRef, min);

				switch (count.getUnderFlowMethod()) {
				case FALSE:
					underflow = new PrimitiveValue("FALSE");
					break;
				case KEEP: // decrement of min value stays at min value
					Spec nextAtMin = new SpecExp(Operator.EQUALS, nextCounterVarRef, min);
					underflow = new SpecExp(Operator.AND, atMin, nextAtMin);
					break;
				case MODULO:
					Spec nextAtMax = new SpecExp(Operator.EQUALS, nextCounterVarRef, max);
					underflow = new SpecExp(Operator.AND, atMin, nextAtMax);
					break;
				default:
					break;
				}

				Spec decPred = count.getDecPred().getContent();
				Spec spDecFinalExp = new SpecExp(Operator.IMPLIES, decPred,
						new SpecExp(Operator.OR, specDecVal, underflow));
				Constraint decConst = new Constraint(Kind.SAFETY, spDecFinalExp, count.getName() + ".dec", count.getDecPred().getTraceId());
				input.getAux().addConstraint(decConst);
			}

			// create and add reset constraints.
			if (count.getResetPred() != null) {
				Spec spReset = new SpecExp(Operator.IMPLIES, count.getResetPred().getContent(),
						new SpecExp(Operator.EQUALS, nextCounterVarRef, min));
				Constraint resetConst = new Constraint(Kind.SAFETY, spReset, count.getName() + ".reset", count.getResetPred().getTraceId());
				input.getAux().addConstraint(resetConst);
			}

			// create constraint for not changing counter value while its predicates are false.
			// (not (incPred) and not (resetPred) and not (decPred)) ==> myCounter=next(myCounter)
			Spec alwaysTrue = new PrimitiveValue("true");
			Spec notInc = alwaysTrue;
			if (count.getIncPred() != null) {
				notInc = new SpecExp(Operator.NOT, count.getIncPred().getContent());
			}
			Spec notDec = alwaysTrue;
			if (count.getDecPred() != null) {
				notDec = new SpecExp(Operator.NOT, count.getDecPred().getContent());
			}
			Spec notReset = alwaysTrue;
			if (count.getResetPred() != null) {
				notReset = new SpecExp(Operator.NOT, count.getResetPred().getContent());
			}

			Spec notAll = new SpecExp(Operator.AND, new SpecExp(Operator.AND, notInc, notDec), notReset);
			Spec unmodifyExp = new SpecExp(Operator.EQUALS, nextCounterVarRef, counterVarRef);
			Spec spIFF = new SpecExp(Operator.IMPLIES, notAll, unmodifyExp);

			Constraint unmodifyConst = new Constraint(Kind.SAFETY, spIFF, count.getName() + ".noChange", count.getTraceId());
			input.getAux().addConstraint(unmodifyConst);
		}

		return counterVars;
	}

	public List<SpecTraceable> getCounterPredicates(String counterName) {
		return counterPredicates.get(counterName);
	}

	public Set<Integer> getTraceIdsOfCounter(String countName) {
		return traceIdsByCounter.get(countName);
	}

	public List<String> getCountersNames() {
		return counterNameList;
	}
	
	private DefineArray replaceCounterInDefineArrays(Map<String, Variable> counterVars, DefineArray defArray) {

		List<Spec> newSpec = null;
		if (defArray.getExpressions() != null) {
			newSpec = new ArrayList<>();
			for (Spec exp : defArray.getExpressions()) {
				newSpec.add(replaceCounterRefs(counterVars, exp));
			}
		}
		
		List<DefineArray> newDefArray = null;
		if (defArray.getDefineArray() != null) {
			newDefArray = new ArrayList<>();
			for (DefineArray innerArray : defArray.getDefineArray()) {
				newDefArray.add(replaceCounterInDefineArrays(counterVars, innerArray));
			}
		}
		
		return new DefineArray(newSpec, newDefArray);
	}

	private boolean noWorkToDo(GameInput input) {
		return input.getCounters() == null || input.getCounters().isEmpty();
	}
	
	private void replaceCounterRefsInTriggers(Map<String, Variable> counterVars, List<TriggerConstraint> moduleTriggers) {
		SpecRegExp initSpecRegExp, effectSpecRegExp;
		for(TriggerConstraint trigger : moduleTriggers) {
			initSpecRegExp = trigger.getInitSpecRegExp();
			for(SpecRegExp predRegExp : initSpecRegExp.getPredicateSubExps()) {
				predRegExp.setPredicate(replaceCounterRefs(counterVars, predRegExp.getPredicate()));
			}
			effectSpecRegExp = trigger.getEffectSpecRegExp();
			for(SpecRegExp predRegExp : effectSpecRegExp.getPredicateSubExps()) {
				predRegExp.setPredicate(replaceCounterRefs(counterVars, predRegExp.getPredicate()));
			}
		}
	}

}
