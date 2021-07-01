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
import tau.smlab.syntech.gameinput.model.Define;
import tau.smlab.syntech.gameinput.model.DefineArray;
import tau.smlab.syntech.gameinput.model.ExistentialConstraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Monitor;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.model.WeightDefinition;
import tau.smlab.syntech.gameinput.spec.MonitorReference;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;

public class MonitorTranslator implements Translator {

	protected Map<String, List<Constraint>> monitorConstraints = new HashMap<>();;
	protected Map<String, Set<Integer>> traceIdsByMonitor = new HashMap<>();
	protected List<String> monitorNameList = new ArrayList<>();

	@Override
	public void translate(GameInput input) {

		if (noWorkToDo(input)) {
			return;
		}
		// translate monitors
		Map<String, Variable> monVars = translateMonitors(input);

		// replace the monitor references in
		// guarantees
		for (Constraint c : input.getSys().getConstraints()) {
			c.setSpec(replaceMonRefs(monVars, c.getSpec()));
		}

		// sys existential constraints
		for (ExistentialConstraint exC : input.getSys().getExistentialConstraints()) {
			if(exC.isRegExp()) {
				SpecRegExp regExp = exC.getRegExp();
				for(SpecRegExp predRegExp : regExp.getPredicateSubExps()) {
					predRegExp.setPredicate(replaceMonRefs(monVars, predRegExp.getPredicate()));
				}
			}
			else {
				for(int i = 0; i < exC.getSize() ; i++) {
					exC.replaceSpec(i, replaceMonRefs(monVars, exC.getSpec(i)));
				}
			}
		}
		
		//sys triggers
		replaceMonRefsInTriggers(monVars, input.getSys().getTriggers());

		// assumptions
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replaceMonRefs(monVars, c.getSpec()));
		}
		
		//env triggers
		replaceMonRefsInTriggers(monVars, input.getEnv().getTriggers());

		// auxiliary constraints
		for (Constraint c : input.getAux().getConstraints()) {
			c.setSpec(replaceMonRefs(monVars, c.getSpec()));
		}

		// defines
		for (Define d : input.getDefines()) {
			if (d.getExpression() != null) {
				d.setExpression(replaceMonRefs(monVars, d.getExpression()));
			} else {
				d.setDefineArray(replaceMonitorInDefineArrays(monVars, d.getDefineArray()));
			}
		}

		// weight definition
		for (WeightDefinition wd : input.getWeightDefs()) {
			Constraint c = wd.getDefinition();
			c.setSpec(replaceMonRefs(monVars, c.getSpec()));
		}

		input.getMonitors().clear();
	}

	/**
	 * replace the monitor references by references to the new variables
	 * @param monVars
	 * @param spec
	 * @return
	 */
	private Spec replaceMonRefs(Map<String, Variable> monVars, Spec spec) {
		if (spec instanceof MonitorReference) {
			// replace monitor reference by aux var
			String name = ((MonitorReference) spec).getMonitor().getName();
			return new VariableReference(monVars.get(name));
		} else if (spec instanceof SpecExp) {
			// replace references in all children
			SpecExp se = (SpecExp) spec;
			for (int i = 0; i < se.getChildren().length; i++) {
				se.getChildren()[i] = replaceMonRefs(monVars, se.getChildren()[i]);
			}
		}
		// nothing to do
		return spec;
	}

	/**
	 * Translate all monitors by creating auxiliary variables and creating auxiliary constraints.
	 * 
	 * @param input
	 * @return
	 */
	private Map<String, Variable> translateMonitors(GameInput input) {

		Map<String, Variable> monVars = new HashMap<>();
		Set<Integer> traceIdsSet;
		List<Constraint> monConstList;

		for (Monitor mon : input.getMonitors()) {

			//maintain a list of all monitor names
			monitorNameList.add(mon.getName());

			// create auxiliary variable for monitor
			Variable v = new Variable(mon.getName(), mon.getType());
			input.getAux().addVar(v);
			monVars.put(mon.getName(), v);

			//maintain a traceIds set of the monitor's constraints
			traceIdsSet = new HashSet<>();
			traceIdsByMonitor.put(mon.getName(), traceIdsSet);

			// maintain a list of the monitor's constraints
			monConstList = new ArrayList<>();
			monitorConstraints.put(mon.getName(), monConstList);

			for (Constraint c : mon.getExpressions()) {
				// add monitoring constraints to AUX player
				input.getAux().addConstraint(c);

				//add the current constraint c and its traceId
				traceIdsSet.add(c.getTraceId());
				monConstList.add(c);
			}
		}
		return monVars;
	}

	public List<Constraint> getMonitorConstraints(String MonName) {
		return monitorConstraints.get(MonName);
	}

	public Set<Integer> getTraceIdsOfMonitor(String monName) {
		return traceIdsByMonitor.get(monName);
	}

	public List<String> getMonitorsNames() {
		return monitorNameList;
	}
	
	private DefineArray replaceMonitorInDefineArrays(Map<String, Variable> monVars, DefineArray defArray) {

		List<Spec> newSpec = null;
		if (defArray.getExpressions() != null) {
			newSpec = new ArrayList<>();
			for (Spec exp : defArray.getExpressions()) {
				newSpec.add(replaceMonRefs(monVars, exp));
			}
		}
		
		List<DefineArray> newDefArray = null;
		if (defArray.getDefineArray() != null) {
			newDefArray = new ArrayList<>();
			for (DefineArray innerArray : defArray.getDefineArray()) {
				newDefArray.add(replaceMonitorInDefineArrays(monVars, innerArray));
			}
		}
		
		return new DefineArray(newSpec, newDefArray);
	}

	private boolean noWorkToDo(GameInput input) {
		return input.getMonitors() == null || input.getMonitors().isEmpty();
	}
	
	private void replaceMonRefsInTriggers(Map<String, Variable> monVars, List<TriggerConstraint> moduleTriggers) {
		SpecRegExp initSpecRegExp, effectSpecRegExp;
		for(TriggerConstraint trigger : moduleTriggers) {
			initSpecRegExp = trigger.getInitSpecRegExp();
			for(SpecRegExp predRegExp : initSpecRegExp.getPredicateSubExps()) {
				predRegExp.setPredicate(replaceMonRefs(monVars, predRegExp.getPredicate()));
			}
			effectSpecRegExp = trigger.getEffectSpecRegExp();
			for(SpecRegExp predRegExp : effectSpecRegExp.getPredicateSubExps()) {
				predRegExp.setPredicate(replaceMonRefs(monVars, predRegExp.getPredicate()));
			}
		}
	}

}
