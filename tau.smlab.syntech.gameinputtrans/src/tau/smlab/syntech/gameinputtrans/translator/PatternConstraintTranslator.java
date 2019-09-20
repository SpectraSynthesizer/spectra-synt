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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.Constraint.Kind;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.PatternConstraint;
import tau.smlab.syntech.gameinput.model.Player;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;

/**
 * iterate over all PatternConstraints of sys and env
 * 
 * for every constraint create all pattern variables with a unique name inside
 * the AUX module
 * 
 * add all initial and safety constraints of the pattern to the AUX module but
 * uniquely instantiated with the new variables and the expression given in the
 * parameters
 * 
 * add justice constraint to the module that had the PatternConstraint (again
 * instantiated with the new variables and expressions of parameters)
 *
 */
public class PatternConstraintTranslator implements Translator {

	enum PlayerType {
		SYS, ENV
	};

	@Override
	public void translate(GameInput input) {

		if (noWorkToDo(input)) {
			return;
		}

		// sys patternConstraint
		handlePatternConstraints(input.getAux(), input.getSys().getPatterns(), input.getSys(), PlayerType.SYS);

		// env patternConstraint
		handlePatternConstraints(input.getAux(), input.getEnv().getPatterns(), input.getEnv(), PlayerType.ENV);

		input.getSys().getPatterns().clear();
		input.getEnv().getPatterns().clear();
		input.getPatterns().clear();
	}

	private void handlePatternConstraints(Player auxPlayer, List<PatternConstraint> pattConstraintsList, Player player,
			PlayerType playerType) {
		Map<Variable, Variable> originalPatVarToUniqueAuxVar = new HashMap<>();
		for (PatternConstraint pc : pattConstraintsList) {
			originalPatVarToUniqueAuxVar.clear();
			handlePatternVars(auxPlayer, playerType, originalPatVarToUniqueAuxVar, pc);

			for (Constraint constraint : pc.getPattern().getExpressions()) {
				Spec copyOfConstraintSpec = null;
				try {
					// clone so we don't destroy the original expression - may be needed for other
					// entities too
					copyOfConstraintSpec = constraint.getSpec().clone();
				} catch (CloneNotSupportedException e) {
				}
				Spec spec = replaceVarReferences(copyOfConstraintSpec, originalPatVarToUniqueAuxVar,
						pc.getPattern().getParamsList(), pc.getParameters());
				Constraint constraintWithReplacedSpec = new Constraint(constraint.getKind(), spec, constraint.getName(),
						pc.getTraceId());

				if (constraint.getKind().equals(Kind.INI) || constraint.getKind().equals(Kind.SAFETY)) {
					auxPlayer.addConstraint(constraintWithReplacedSpec);

				} else if (constraint.getKind().equals(Kind.JUSTICE)) {
					player.addConstraint(constraintWithReplacedSpec);
				}
			}
		}
	}

	private void handlePatternVars(Player auxPlayer, PlayerType playerType,
			Map<Variable, Variable> originalPatVarToUniqueAuxVar, PatternConstraint patternConstraint) {
		for (Variable pattVar : patternConstraint.getPattern().getVarsList()) {
			String uniqueAuxVarName = playerType + "_CONSTRAINT." + Integer.toString(patternConstraint.getTraceId())
					+ "." + patternConstraint.getPattern().getPatternName() + "." + pattVar.getName();
			Variable auxVar = new Variable(uniqueAuxVarName, pattVar.getType());
			originalPatVarToUniqueAuxVar.put(pattVar, auxVar);
			auxPlayer.addVar(auxVar);
		}
	}

	private Spec replaceVarReferences(Spec spec, Map<Variable, Variable> originalPatVarToUniqueAuxVar,
			List<Variable> formalParams, List<Spec> actualParamsValues) {
		if (spec instanceof VariableReference) {
			VariableReference varRef = (VariableReference) spec;

			if (formalParams.contains(varRef.getVariable())) {
				// varRef points to formal parameters
				int indexOfParam = formalParams.indexOf(varRef.getVariable());
				try {
					spec = actualParamsValues.get(indexOfParam).clone();
				} catch (CloneNotSupportedException e) {
					e.printStackTrace();
				}
			} else if (originalPatVarToUniqueAuxVar.containsKey(varRef.getVariable())) {
				// varRef points to a pattern var. replace with the aux unique var
				Variable auxVar = originalPatVarToUniqueAuxVar.get(varRef.getVariable());
				spec = new VariableReference(auxVar, auxVar.getName());
			}

		} else if (spec instanceof SpecExp) {
			SpecExp se = (SpecExp) spec;
			for (int i = 0; i < se.getChildren().length; i++) {
				se.getChildren()[i] = replaceVarReferences(se.getChildren()[i], originalPatVarToUniqueAuxVar,
						formalParams, actualParamsValues);
			}
		}
		return spec;
	}

	private boolean noWorkToDo(GameInput input) {
		return input.getPatterns() == null || input.getPatterns().isEmpty();
	}

}
