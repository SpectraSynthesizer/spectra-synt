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
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.QuantifiedSpec;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecWrapper;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gameinputtrans.TranslationException;

public class ArrayFunctionsTranslator implements Translator {
	
	private static final Map<Operator, Operator> ARRAY_TO_UNIT_OPERATORS = Collections.unmodifiableMap(new HashMap<Operator, Operator>() {{
		put(Operator.SUM_OF, Operator.ADD);
		put(Operator.PROD_OF, Operator.MULTIPLY);
		put(Operator.AND_OF, Operator.AND);
		put(Operator.OR_OF, Operator.OR);
	}});
	
	
	@Override
	public void translate(GameInput input) {
		// guarantees
		for (Constraint c : input.getSys().getConstraints()) {
			c.setSpec(replaceArrayOperators(c.getSpec(), c.getTraceId()));
		}

		// assumptions
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replaceArrayOperators(c.getSpec(), c.getTraceId()));
		}

	}
		
	private Spec replaceArrayOperators(Spec spec, int traceId) {
		if (spec instanceof SpecExp) {
			SpecExp specExp = (SpecExp) spec; 
			
			Operator arrayOp = specExp.getOperator();
						
			if (ARRAY_TO_UNIT_OPERATORS.containsKey(arrayOp) && (specExp.getChildren()[0] instanceof VariableReference)) {
				
				VariableReference array = (VariableReference) specExp.getChildren()[0];
				
				Spec newSpec = new VariableReference(new Variable(array.getReferenceName() + "[0]", array.getVariable().getType()));
				
				for (int index = 1; index < array.getIndexDimensions().get(0); index++) { 
					
					VariableReference currentElement = new VariableReference(new Variable(array.getReferenceName()+ "["+ index +"]", array.getVariable().getType())); 
					
					newSpec = new SpecExp(ARRAY_TO_UNIT_OPERATORS.get(arrayOp), currentElement, newSpec);										 
				}
				
				return newSpec;
			} else {
				for (int i = 0; i < specExp.getChildren().length; i++) {
					specExp.getChildren()[i] = replaceArrayOperators(specExp.getChildren()[i], traceId);
				}	
			}
		}

		return spec;
	}

}
