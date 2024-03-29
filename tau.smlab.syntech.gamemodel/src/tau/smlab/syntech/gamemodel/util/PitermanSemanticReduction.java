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

package tau.smlab.syntech.gamemodel.util;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.ModuleException;
import tau.smlab.syntech.jtlv.ModuleVariableException;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

public class PitermanSemanticReduction {
	
private static final String envVariable = "~envHelp";
	
	public static void doReduction(GameModel gm) {
		try {
			if (gm.getEnv().trans().isOne())
				return;
			
			ModuleBDDField envHelp = gm.getEnv().addVar(envVariable, false);
			BDD envHelpIsFalse = envHelp.getDomain().ithVar(0);
			BDD envHelpIsTrue = envHelp.getDomain().ithVar(1);
			BDD envHelpPrimeIsTrue = envHelp.other().getDomain().ithVar(1);
			
			gm.getEnv().conjunctInitial(envHelpIsFalse.id());
			gm.getEnv().trans().orWith(envHelpPrimeIsTrue.id());
			gm.getEnv().conjunctTrans(envHelpIsTrue.imp(envHelpPrimeIsTrue));
			gm.getEnv().addJustice(envHelpIsFalse.id());
			
			gm.getSys().trans().orWith(envHelpIsTrue.id());
			gm.getSys().trans().orWith(envHelpIsFalse.and(envHelpPrimeIsTrue));
			
		} catch (ModuleException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ModuleVariableException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
