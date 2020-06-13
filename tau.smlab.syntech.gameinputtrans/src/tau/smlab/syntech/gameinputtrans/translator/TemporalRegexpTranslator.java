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
import tau.smlab.syntech.gameinput.model.Constraint.Kind;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Player;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;

public class TemporalRegexpTranslator implements Translator {

	@Override
	public void translate(GameInput input) {
			
		List<Player> players = new ArrayList<>();
		players.add(input.getEnv()); // assumptions
		players.add(input.getSys()); // guarantees
		
		for (Player player: players) {
			List<Constraint> constraints = new ArrayList<>();
			
			for (Constraint c : player.getConstraints()) {
				TriggerConstraint triggerConstraint = replaceTemporalRegexp(c);
				if (triggerConstraint == null) {
					constraints.add(c);
				} else {
					player.addTrigger(triggerConstraint);
				}
			}
			
			player.setConstraints(constraints);
		}
	}
		
	private TriggerConstraint replaceTemporalRegexp(Constraint c)
	{
		 if (c.getSpec() instanceof SpecExp) {
		      SpecExp e = (SpecExp) c.getSpec();
		      if (Operator.REGEXP.equals(e.getOperator())) {
		    	if (Kind.SAFETY.equals(c.getKind())) {
		    		return replaceSafetyRegex(c.getName(), e.getChildren()[0], c.getTraceId());	
		    	} else if (Kind.JUSTICE.equals(c.getKind())) {
		    		return replaceLivnessRegex(c.getName(), e.getChildren()[0], c.getTraceId());	
		    	}
		      }
		    }
		return null;
	}
	
	private TriggerConstraint replaceSafetyRegex(String constraintName, Spec spec, int traceId) {
		// search for G(regex) and replace with  true* |=> reg - trig [true]* |=> regexp;

		SpecRegExp initSpecRegExp = SpecRegExp.newZeroOrMoreRepRegExp(SpecRegExp.newPredicateRegExp(new PrimitiveValue("true"))); 
		SpecRegExp effectSpecRegExp = (SpecRegExp) spec;
		
		return new TriggerConstraint(constraintName, initSpecRegExp, effectSpecRegExp, traceId);		
	}
	
	private TriggerConstraint replaceLivnessRegex(String constraintName, Spec spec, int traceId) {
		// search for GF(regex) and replace with true* |=> true* reg - trig [true]* |=> [true]*regex  
		
		SpecRegExp initSpecRegExp = SpecRegExp.newZeroOrMoreRepRegExp(SpecRegExp.newPredicateRegExp(new PrimitiveValue("true"))); 
		SpecRegExp effectSpecRegExp = SpecRegExp.newConcatRegExp(initSpecRegExp, (SpecRegExp) spec);
		
		return new TriggerConstraint(constraintName, initSpecRegExp, effectSpecRegExp, traceId);
	}

}
