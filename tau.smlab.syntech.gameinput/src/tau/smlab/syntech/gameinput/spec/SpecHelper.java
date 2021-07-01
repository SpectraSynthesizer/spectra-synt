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

package tau.smlab.syntech.gameinput.spec;

import java.util.Map;
import tau.smlab.syntech.gameinput.model.Variable;

public class SpecHelper {

	// Get underlying variable of temporal expression inside index. We assume that
	// there is only one variable
	// in each index since it was verified during type check phase
	public static void getUnderlyingVariables(Spec index, Map<String, Variable> vars) {

		if (index instanceof VariableReference) {
			Variable var = ((VariableReference) index).getVariable();
			vars.put(var.getName(), var);
		} else if (index instanceof SpecExp) {
			SpecExp indexExp = (SpecExp) index;
			for (Spec child : indexExp.getChildren()) {
				getUnderlyingVariables(child, vars);
			}
		}
	}
	
	public static void updateDefineReference(DefineReference defRef, Variable var, PrimitiveValue primVal) throws Exception {
		
		for (int i = 0; i < defRef.getIndexSpecs().size(); i++) {
			Spec interpreted = SpecHelper.interpretWithVariable(defRef.getIndexSpecs().get(i), var, primVal);
			defRef.getIndexSpecs().set(i, interpreted);
		}
		defRef.getIndexVars().remove(var.getName());
		
		if (defRef.getIndexVars().isEmpty()) {
			for (int i = 0; i < defRef.getIndexSpecs().size(); i++) {
				Integer indexValue = SpecHelper.calculateSpec(defRef.getIndexSpecs().get(i));
				if (indexValue < 0 || indexValue >= defRef.getIndexDimensions().get(i)) {
					throw new Exception(String.format("Index %d out of bounds for variable %s", indexValue,
							defRef.getDefine().getName()));
				}
			}
		}
	}	

	public static void updateRefName(VariableReference varRef) throws Exception {
		String refName = varRef.getVariable().getName();
		if (varRef.getIndexVars().isEmpty()) {
			for (int i = 0; i < varRef.getIndexSpecs().size(); i++) {
				Integer indexValue = SpecHelper.calculateSpec(varRef.getIndexSpecs().get(i));
				if (indexValue < 0 || indexValue >= varRef.getIndexDimensions().get(i)) {
					throw new Exception(String.format("Index %d out of bounds for variable %s", indexValue,
							varRef.getVariable().getName()));
				}
				refName += String.format("[%d]", indexValue);
			}
		}
		varRef.setReferenceName(refName);
	}

	public static Spec interpretWithVariable(Spec spec, Variable var, PrimitiveValue value) throws Exception {
		// Assign value to varRef
		if (spec instanceof VariableReference) {
			VariableReference varRef = (VariableReference) spec;
			if (varRef.getVariable().equals(var)) {
				return value;
			} else {
				return varRef;
			}
		} else if (spec instanceof PrimitiveValue) {
			return spec;
		} else if (spec instanceof DefineReference) {
			DefineReference def = (DefineReference) spec;
			// For the time being, cannot interpret define arrays inside index spec
			if (def.getDefine().getExpression() != null) {
				def.getDefine().setExpression((interpretWithVariable(def.getDefine().getExpression(), var, value)));
				return def;
			} else {
				throw new Exception("Spec cannot be interpreted with variable, has invalid inner specs");
			}

		} else if (spec instanceof SpecExp) {
			SpecExp exp = (SpecExp) spec;
			return new SpecExp(exp.getOperator(), interpretWithVariable(exp.getChildren()[0], var, value),
					interpretWithVariable(exp.getChildren()[1], var, value));
		} else {
			throw new Exception("Spec cannot be interpreted with variable, has invalid inner specs");
		}
	}

	public static Integer calculateSpec(Spec spec) throws Exception {
		if (spec instanceof PrimitiveValue) {
			PrimitiveValue intVal = (PrimitiveValue) spec;
			try {
				return Integer.parseInt(intVal.getValue());
			} catch (NumberFormatException e) {
				throw new Exception("Spec cannot be calculated, is non integer");
			}
		} else if (spec instanceof DefineReference) {
			DefineReference def = (DefineReference) spec;
			if (def.getDefine().getExpression() != null) {
				return calculateSpec(def.getDefine().getExpression());
			} else {
				throw new Exception("Spec cannot be calculated, contains define references");
			}

		} else if (spec instanceof SpecExp) {
			SpecExp specExp = (SpecExp) spec;
			Operator op = specExp.getOperator();

			Integer res1 = calculateSpec(specExp.getChildren()[0]);
			Integer res2 = calculateSpec(specExp.getChildren()[1]);

			if (Operator.MOD.equals(op)) {
				return ((res1 % res2) + res2) % res2;
			} else if (Operator.ADD.equals(op)) {
				return res1 + res2;
			} else if (Operator.SUBSTRACT.equals(op)) {
				return res1 - res2;
			} else if (Operator.MULTIPLY.equals(op)) {
				return res1 * res2;
			} else if (Operator.DIVIDE.equals(op)) {
				return res1 / res2;
			} else {
				throw new Exception("Spec cannot be calculated, is non integer");
			}
		} else if (spec instanceof VariableReference) {
			throw new Exception("Spec cannot be calculated, contains variable references");
		} else {
			throw new Exception("Spec cannot be calculated, is non integer");
		}
	}
}
