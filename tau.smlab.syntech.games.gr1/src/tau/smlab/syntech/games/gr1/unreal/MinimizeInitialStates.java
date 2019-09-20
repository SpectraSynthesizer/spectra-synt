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

package tau.smlab.syntech.games.gr1.unreal;

import java.util.ArrayList;
import java.util.List;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.ModuleException;
import tau.smlab.syntech.games.util.AbstractDdmin;
import tau.smlab.syntech.jtlv.Env;

public class MinimizeInitialStates extends AbstractDdmin<BehaviorInfo> {

	private BDD winSysStates;
	private BDD iniSysStates = Env.TRUE();
	private GameModel model;

	public MinimizeInitialStates(GameModel model, BDD winSys) {
		this.winSysStates = winSys;
		this.model = model;
	}

	/**
	 * check for system winning from initial states
	 * 
	 * @return true if sys is not winning from all the given initial states
	 */
	@Override
	protected boolean check(List<BehaviorInfo> part) {

		try {
			buildSysInitial(part);
		} catch (ModuleException e) {
			throw new RuntimeException(e);
		}

		BDD sysWin = winSysStates.and(iniSysStates);
		BDD result = model.getEnv().initial().id().impWith(sysWin.exist(model.getSys().moduleUnprimeVars()))
				.forAll(model.getEnv().moduleUnprimeVars());
		sysWin.free();

		boolean allIni = result.isOne();
		result.free();
		iniSysStates.free();

		return !allIni;
	}

	public void buildSysInitial(List<BehaviorInfo> part) throws ModuleException {

		iniSysStates = Env.TRUE();

		ArrayList<BehaviorInfo> gars = new ArrayList<BehaviorInfo>();
		gars.addAll(model.getAuxBehaviorInfo());
		gars.addAll(part);

		for (BehaviorInfo gar : gars) {
			if (gar.isInitial()) {
				iniSysStates.andWith(gar.initial.id());
			}
		}
	}

}
