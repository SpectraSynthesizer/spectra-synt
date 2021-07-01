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
import tau.smlab.syntech.gameinput.model.Define;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Player;
import tau.smlab.syntech.gameinput.model.Predicate;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.model.Constraint.Kind;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;

public class MinMaxFunctionsTranslator implements Translator {

	@Override
	public void translate(GameInput input) {		
		// guarantees
		Constraint[] sysConstraints = new Constraint[input.getSys().getConstraints().size()];
		input.getSys().getConstraints().toArray(sysConstraints);
		
		for (Constraint c : sysConstraints) {
			c.setSpec(replaceMinMaxOperators(c.getSpec(), input, c.getTraceId(), input.getSys()));
		}

		// assumptions
		Constraint[] envConstraints = new Constraint[input.getEnv().getConstraints().size()];
		input.getEnv().getConstraints().toArray(envConstraints);
		for (Constraint c : envConstraints) {
			c.setSpec(replaceMinMaxOperators(c.getSpec(), input, c.getTraceId(), input.getEnv()));
		}
		
		// auxiliary constraints
		Constraint[] auxConstraints = new Constraint[input.getAux().getConstraints().size()];
		input.getAux().getConstraints().toArray(auxConstraints);
		for (Constraint c : auxConstraints) {
			c.setSpec(replaceMinMaxOperators(c.getSpec(), input, c.getTraceId(), input.getAux()));
		}
		
		// predicates
		Predicate[] predicatesConstraints = new Predicate[input.getPredicates().size()];
		input.getPredicates().toArray(predicatesConstraints);
		for (Predicate c : predicatesConstraints) {
			c.setSpec(replaceMinMaxOperators(c.getExpression(), input, c.getTraceId(), input.getAux()));
		}
		
		// defines
		Define[] defConstraints = new Define[input.getDefines().size()];
		input.getDefines().toArray(defConstraints);
		for (Define define : defConstraints) {
			define.setExpression(replaceMinMaxOperators(define.getExpression(), input, 0,input.getAux()));
		}
	}
	
	private Spec replaceMinMaxOperators(Spec spec, GameInput input, int traceId, Player player) {
		if (spec instanceof SpecExp) {
			SpecExp specExp = (SpecExp) spec; 
			
			Operator arrayOp = specExp.getOperator();
						
			if (arrayOp == Operator.MIN || arrayOp == Operator.MAX) {
					
				VariableReference arrayVar = (VariableReference) specExp.getChildren()[0];
				Variable minMaxAuxVar = null;
				
				// Create auxiliary variable with the same range, if not already exist				
				if (input.getAux().getVars().stream().filter(o -> o.getName().equals(arrayVar.getReferenceName() + "." + arrayOp)).findFirst().isPresent()) {
					minMaxAuxVar = input.getAux().getVars().stream().filter(o -> o.getName().equals(arrayVar.getReferenceName() +  "." + arrayOp)).findFirst().get();
				} else {
					TypeDef t = new TypeDef(arrayVar.getVariable().getType().getLower(), arrayVar.getVariable().getType().getUpper());
					minMaxAuxVar = new Variable(arrayVar.getReferenceName() + "." + arrayOp, t);
					input.getAux().addVar(minMaxAuxVar);	
				}
				
				Spec specAuxVar = new VariableReference(minMaxAuxVar);
				
				// Create gar G exist (arr[0] = myMin | arr[1] = myMin ...)
				VariableReference currentElement = new VariableReference(new Variable(arrayVar.getReferenceName()+ "[0]", arrayVar.getVariable().getType()));
				Spec containerOr = new SpecExp(Operator.EQUALS, currentElement, specAuxVar);
				
				for (int index = 1; index < arrayVar.getIndexDimensions().get(0); index++) {
					currentElement = new VariableReference(new Variable(arrayVar.getReferenceName()+ "["+ index +"]", arrayVar.getVariable().getType()));
					Spec existElemMinMx = new SpecExp(Operator.EQUALS, currentElement, specAuxVar);
					
					containerOr = new SpecExp(Operator.OR, containerOr, existElemMinMx);
				}
				
				Constraint existConst = new Constraint(Kind.SAFETY, containerOr, arrayVar.getReferenceName() + ".exists", traceId);
				player.addConstraint(existConst);
				
				// Create gar G forall (myMin <= arr[0] & myMin <= arr[1] ...)
				Operator op = arrayOp == Operator.MIN ? Operator.RIGHT_BIGGER_OR_EQUALS : Operator.LEFT_BIGGER_OR_EQUALS;
				
				VariableReference currentArrElement = new VariableReference(new Variable(arrayVar.getReferenceName()+ "[0]", arrayVar.getVariable().getType()));
				Spec containerAnd = new SpecExp(op, specAuxVar, currentArrElement);
				
				for (int index = 1; index < arrayVar.getIndexDimensions().get(0); index++) {
					currentArrElement = new VariableReference(new Variable(arrayVar.getReferenceName()+ "["+ index +"]", arrayVar.getVariable().getType()));
					Spec forallElemMinMx = new SpecExp(op, currentElement, specAuxVar);
					
					containerAnd = new SpecExp(Operator.AND, containerAnd, forallElemMinMx);
				}
				
				Constraint forallConst = new Constraint(Kind.SAFETY, containerAnd, arrayVar.getReferenceName() + ".forall", traceId);
				player.addConstraint(forallConst);	
				
				// translate the original expression to the use of the aux var.  
				return specAuxVar;	
				
			} else {
				for (int i = 0; i < specExp.getChildren().length; i++) {
					specExp.getChildren()[i] = replaceMinMaxOperators(specExp.getChildren()[i], input, traceId, player);
				}	
			}
		}

		return spec;
	}

}
