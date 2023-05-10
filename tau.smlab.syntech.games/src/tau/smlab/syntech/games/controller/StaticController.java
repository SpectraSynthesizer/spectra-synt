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

package tau.smlab.syntech.games.controller;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.games.controller.symbolic.SymbolicController;
import tau.smlab.syntech.jtlv.Env;

/**
 * 	
 * A class for the execution of static (original) controllers
 *
 */
public class StaticController implements Controller {

	private SymbolicController ctrl;

	public StaticController() {
	}
	
	private BDDVarSet envVars;
	private BDDVarSet sysVars;
	

	@Override
	public void load(String folder, String name, Map<String, String[]> sysVars, Map<String, String[]> envVars) {

		try {
			
			String prefix;
			if (name == null) {
				prefix = folder + File.separator;
			} else {
				prefix = folder + File.separator + name + ".";
			}
			
			BDD init = Env.loadBDD(prefix + "controller.init.bdd");
			BDD trans = Env.loadBDD(prefix + "controller.trans.bdd");
			
			init = init.exist(Env.globalPrimeVars());
		    ctrl = new SymbolicController();
		    ctrl.setTrans(trans);
		    ctrl.setInit(init);
		    
		    
			this.envVars = Env.getEmptySet();
			this.sysVars = Env.getEmptySet();
			for (String sysVar : sysVars.keySet()) {
				if (!"Zn".equals(sysVar)) this.sysVars.unionWith(Env.getVar(sysVar).getDomain().set());
			}
			for (String envVar : envVars.keySet()) {
				this.envVars.unionWith(Env.getVar(envVar).getDomain().set());
			}
		    
		    
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public BDD pred(BDD to) {
		return Env.pred(transitions(), to.id());
	}
	
	public BDD succ(BDD from) {
		return Env.succ(from.id(), transitions());
	}
	

	@Override
	public BDD next(BDD currentState, BDD inputs) {
		return ctrl.succ(currentState).and(inputs);	
	}

	@Override
	public void free() {
		ctrl.free();
	}

	@Override
	public BDD transitions() {
		return ctrl.trans();
	}
	
	@Override 
	public BDD initial() {
		return ctrl.initial();
	}

	@Override
	public void init(BDD currentState) {
		
	}

	@Override
	public void saveState() {
	}

	@Override
	public void loadState() {
	}

}
