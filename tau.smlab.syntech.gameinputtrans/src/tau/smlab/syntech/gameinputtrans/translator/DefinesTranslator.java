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
import tau.smlab.syntech.gameinput.model.ExistentialConstraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Pattern;
import tau.smlab.syntech.gameinput.model.Predicate;
import tau.smlab.syntech.gameinput.model.WeightDefinition;
import tau.smlab.syntech.gameinput.spec.DefineReference;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;

/**
 * iterate over all specs (in guarantees, assumptions, auxiliary constraints,
 * weight definition, patterns?, predicates)
 * 
 * if spec contains a DefineReference replace it by the spec of the define
 * 
 * finally delete the list of defines
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
			c.setSpec(replaceDefines(c.getSpec()));
		}

		// sys existential constraints
		for (ExistentialConstraint exC : input.getSys().getExistentialConstraints()) {
			for(int i = 0; i < exC.getSize() ; i++) {
				exC.replaceSpec(i, replaceDefines(exC.getSpec(i)));
			}
		}

		// assumptions
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replaceDefines(c.getSpec()));
		}

		// auxiliary constraints
		for (Constraint c : input.getAux().getConstraints()) {
			c.setSpec(replaceDefines(c.getSpec()));
		}

		// weight definition
		for (WeightDefinition wd : input.getWeightDefs()) {
			Constraint c = wd.getDefinition();
			c.setSpec(replaceDefines(c.getSpec()));
		}

		// patterns
		for (Pattern patt : input.getPatterns()) {
			for (Constraint c : patt.getExpressions()) {
				c.setSpec(replaceDefines(c.getSpec()));
			}
		}

		// predicates
		for (Predicate pred : input.getPredicates()) {
			pred.setSpec(replaceDefines(pred.getExpression()));

		}

		// clear the list of the defines
		input.getDefines().clear();
	}

	private boolean noWorkToDo(GameInput input) {
		return input.getDefines() == null || input.getDefines().isEmpty();
	}

	private Spec replaceDefines(Spec spec) {
		if (spec instanceof DefineReference) {
			DefineReference dr = (DefineReference) spec;
			// important: spec = dr.getDefine().getExpression() isn't enough! there might be nested defines. 
			try {
				spec = replaceDefines(dr.getDefine().getExpression().clone());
			} catch (CloneNotSupportedException e) {
				e.printStackTrace();
			}
		} else if (spec instanceof SpecExp) {
			SpecExp se = (SpecExp) spec;
			for (int i = 0; i < se.getChildren().length; i++) {
				se.getChildren()[i] = replaceDefines(se.getChildren()[i]);
			}

		}
		return spec;
	}

}
