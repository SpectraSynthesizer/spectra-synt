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
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Monitor;
import tau.smlab.syntech.gameinput.model.PatternConstraint;
import tau.smlab.syntech.gameinput.spec.IfThenElseInstance;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;


public class IfThenElseInstanceTranslator implements Translator {

	@Override
	public void translate(GameInput input) {

		if (noWorkToDo(input)) {
			return;
		}

		// guarantees
		for (Constraint c : input.getSys().getConstraints()) {
			c.setSpec(replaceIfThenElseInstances(c.getSpec()));
		}

		// assumptions
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replaceIfThenElseInstances(c.getSpec()));
		}

		// auxiliary constraints
		for (Constraint c : input.getAux().getConstraints()) {
			c.setSpec(replaceIfThenElseInstances(c.getSpec()));
		}

		// going over defines
		for (Define def : input.getDefines()) {
			if (def.getExpression() != null) {
				def.setExpression(replaceIfThenElseInstances(def.getExpression()));
			} else {
				def.setDefineArray(replaceIfThenElseInDefineArrays(def.getDefineArray()));
			}
		}

		for (Counter counter : input.getCounters()) {
			if (counter.getDecPred() != null) {
				counter.getDecPred().setContent(replaceIfThenElseInstances(counter.getDecPred().getContent()));
			}
			if (counter.getIncPred() != null) {
				counter.getIncPred().setContent(replaceIfThenElseInstances(counter.getIncPred().getContent()));
			}
			if (counter.getIniPred() != null) {
				counter.getIniPred().setContent(replaceIfThenElseInstances(counter.getIniPred().getContent()));
			}
			if (counter.getResetPred() != null) {
				counter.getResetPred().setContent(replaceIfThenElseInstances(counter.getResetPred().getContent()));
			}
		}

		// going over monitors
		for (Monitor mon : input.getMonitors()) {
			for (Constraint c : mon.getExpressions()) {
				c.setSpec(replaceIfThenElseInstances(c.getSpec()));
			}
		}

		// pattern parameters
		for (PatternConstraint pc : input.getEnv().getPatterns()) {
			List<Spec> ps = pc.getParameters();
			for (int i = 0; i < pc.getParameters().size(); i++) {
				ps.set(i, replaceIfThenElseInstances(ps.get(i)));
			}
		}
		for (PatternConstraint pc : input.getSys().getPatterns()) {
			List<Spec> ps = pc.getParameters();
			for (int i = 0; i < pc.getParameters().size(); i++) {
				ps.set(i, replaceIfThenElseInstances(ps.get(i)));
			}
		}

	}

	private DefineArray replaceIfThenElseInDefineArrays(DefineArray defArray) {

		List<Spec> newSpec = null;
		if (defArray.getExpressions() != null) {
			newSpec = new ArrayList<>();
			for (Spec exp : defArray.getExpressions()) {
				newSpec.add(replaceIfThenElseInstances(exp));
			}
		}

		List<DefineArray> newDefArray = null;
		if (defArray.getDefineArray() != null) {
			newDefArray = new ArrayList<>();
			for (DefineArray innerArray : defArray.getDefineArray()) {
				newDefArray.add(replaceIfThenElseInDefineArrays(innerArray));
			}
		}

		return new DefineArray(newSpec, newDefArray);
	}

	private boolean noWorkToDo(GameInput input) {
		return false;
	}

	private Spec replaceIfThenElseInstances(Spec spec) {
		if (spec instanceof IfThenElseInstance) {
			IfThenElseInstance pi = (IfThenElseInstance) spec;

			spec = translateIfThenElse(pi);
			// check if there are more instances of predicates after instantiation (nested predicates)
			spec = replaceIfThenElseInstances(spec);
		} else if (spec instanceof SpecExp) {
			SpecExp se = (SpecExp) spec;
			for (int i = 0; i < se.getChildren().length; i++) {
				se.getChildren()[i] = replaceIfThenElseInstances(se.getChildren()[i]);
			}
		}
		return spec;
	}
	
	private Spec translateIfThenElse(IfThenElseInstance ifThenElse) {
		
		Spec ifThen = new SpecExp(Operator.IMPLIES, ifThenElse.getIfPart(), ifThenElse.getThenPart());
		Spec elseThen = new SpecExp(Operator.IMPLIES, new SpecExp(Operator.NOT, ifThenElse.getIfPart()), ifThenElse.getElsePart());
		return new SpecExp(Operator.AND, ifThen, elseThen);
	}

}