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

package tau.smlab.syntech.gameinput.kind;

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.model.Constraint.Kind;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gameinput.spec.SpecExp;

public class PitermanSyntacticReduction {
	
private static final String envVariable = "~envHelp";
	
	
	public static void doReduction(GameInput gi) {
		if (gi.getEnv().getConstraints().stream().noneMatch(c -> c.getKind() == Kind.SAFETY || c.getKind() == Kind.STATE_INV))
			return;
		
		Variable envHelpVariable = new Variable(envVariable, new TypeDef(), -1);
		gi.getEnv().addVar(envHelpVariable);
		
		for (Constraint c : gi.getEnv().getConstraints()) {
			if (c.getKind() == Kind.SAFETY || c.getKind() == Kind.STATE_INV) {
				c.setSpec(new SpecExp(Operator.OR, c.getSpec(), new VariableReference(envHelpVariable, envHelpVariable.getName() + "'")));
			}
		}
		for (Constraint c : gi.getSys().getConstraints()) {
			if (c.getKind() == Kind.SAFETY || c.getKind() == Kind.STATE_INV) {
				c.setSpec(new SpecExp(Operator.OR, c.getSpec(), new VariableReference(envHelpVariable)));
			}
		}
		
		SpecExp envInit = new SpecExp(Operator.NOT, new VariableReference(envHelpVariable));
		gi.getEnv().addConstraint(new Constraint(Kind.INI, envInit, "env init", -1));
		SpecExp envSafety = new SpecExp(Operator.IMPLIES, new VariableReference(envHelpVariable), new VariableReference(envHelpVariable, envHelpVariable.getName() + "'"));
		gi.getEnv().addConstraint(new Constraint(Kind.SAFETY, envSafety, "env safety", -1));
		SpecExp envLiveness = new SpecExp(Operator.NOT, new VariableReference(envHelpVariable));
		gi.getEnv().addConstraint(new Constraint(Kind.JUSTICE, envLiveness, "env liveness", -1));
		
	}

}
