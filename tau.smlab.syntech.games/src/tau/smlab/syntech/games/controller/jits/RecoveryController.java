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

package tau.smlab.syntech.games.controller.jits;

import java.util.Map;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;

/**
 * 
 * @author ilia
 *
 */
public class RecoveryController extends AbstractJitController {
		
	public RecoveryController(JitController jitController) {
		super(jitController);
	}

	@Override
	public void load(String folder, String name, Map<String, String[]> sysVars, Map<String, String[]> envVars) {
		jitController.load(folder, name, sysVars, envVars);
		
		BDD Z = Env.unprime(getJitContext().Y(0, getJitContext().rank(0) - 1));
		
		// Trans and ini are taken only from sys
		jitController.getJitContext().setTrans(jitController.getJitContext().getSysTrans());
		jitController.getJitContext().setIni(jitController.getJitContext().getSysIni().exist(Env.globalPrimeVars()).and(Z));
		
		Z.free();
		
	}
	
	@Override
	public BDD next(BDD currentState, BDD inputs) {
		
		if (currentState.and(jitController.getJitContext().getEnvTrans()).and(Env.prime(inputs)).isZero()) {
			System.out.println("Environment safety violation detected with given inputs. Trying to continue with execution");
		}
		
		BDD next = jitController.next(currentState, inputs);
		if (next.isZero()) {
			throw new IllegalArgumentException("The inputs are a safety violation for the environment and it is impossible for the system to recover");
		}
		
		return next;
	}

	@Override
	public void init(BDD currentState) {
		
		if (jitController.getJitContext().getEnvIni().and(currentState).isZero()) {
			System.out.println("Environment safety violation detected with given inputs. Trying to continue with execution");
		}
		
		jitController.init(currentState);
	}
	
	@Override
	public void saveState() {
		this.controller.saveState();
	}

	@Override
	public void loadState() {
		this.controller.loadState();
	}

	@Override
	public BDD succ(BDD from) {
		return jitController.succ(from);
	}

	@Override
	public BDD pred(BDD to) {
		return jitController.pred(to);
	}
}
