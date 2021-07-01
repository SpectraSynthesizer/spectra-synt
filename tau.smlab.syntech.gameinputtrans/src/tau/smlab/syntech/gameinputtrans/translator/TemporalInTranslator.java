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
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.spec.InExpSpec;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;

public class TemporalInTranslator implements Translator {

	@Override
	public void translate(GameInput input) {
		// guarantees
		for (Constraint c : input.getSys().getConstraints()) {
			c.setSpec(replaceInOperator(c.getSpec(), c.getTraceId()));
		}

		// assumptions
		for (Constraint c : input.getEnv().getConstraints()) {
			c.setSpec(replaceInOperator(c.getSpec(), c.getTraceId()));
		}
	} 
	
	private Spec replaceInOperator(Spec spec, int traceId) {
		if (spec instanceof SpecRegExp) {
			SpecRegExp regexpSpec = (SpecRegExp)spec; 
			Spec predicate = regexpSpec.getPredicate();
		
			if (predicate instanceof InExpSpec) {
				InExpSpec inSpec = (InExpSpec) predicate;
								
				regexpSpec.setPredicate(trsanlateInOperator(inSpec));
				
				return regexpSpec;
			}
		}
		else if (spec instanceof InExpSpec) {
			return trsanlateInOperator((InExpSpec)spec);
		}
		else if (spec instanceof SpecExp) {
			SpecExp specExp = (SpecExp) spec; 
			for (int i = 0; i < specExp.getChildren().length; i++) {
				specExp.getChildren()[i] = replaceInOperator(specExp.getChildren()[i], traceId);
			}	
		}
		return spec;
	}

	private Spec trsanlateInOperator(InExpSpec inSpec) {
		Operator mainOp = Operator.OR;
						
		if (inSpec.isNot()) {
			mainOp = Operator.AND;
		}
		
		SpecExp firstEqualExp = new SpecExp(Operator.EQUALS, inSpec.getVariable(), new PrimitiveValue(inSpec.getSetOfvalues().get(0)));
				
		SpecExp translatedSpec = inSpec.isNot() ? new SpecExp(Operator.NOT, firstEqualExp) : firstEqualExp; 

		for (int i = 1; i < inSpec.getSetOfvalues().size(); i++) {
			String value_element = inSpec.getSetOfvalues().get(i);
            SpecExp equalExp = new SpecExp(Operator.EQUALS, inSpec.getVariable(), new PrimitiveValue(value_element));
			SpecExp finalExp = inSpec.isNot() ? new SpecExp(Operator.NOT, equalExp) : equalExp;
			translatedSpec = new SpecExp(mainOp, translatedSpec, finalExp);
        }
		
		return translatedSpec;
	}

}
