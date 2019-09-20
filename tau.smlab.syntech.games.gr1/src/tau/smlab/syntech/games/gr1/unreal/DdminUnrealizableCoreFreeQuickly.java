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
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.games.gr1.GR1Game;
import tau.smlab.syntech.games.gr1.GR1GameMemoryless;
import tau.smlab.syntech.games.util.AbstractDdmin;
import tau.smlab.syntech.jtlv.Env;

/**
 * minimizes set of guarantees to keep system unrealizable
 * 
 */
public class DdminUnrealizableCoreFreeQuickly extends AbstractDdmin<BehaviorInfo> {
	private GameModel model;
	private BDD auxIni = Env.TRUE();
	private BDD auxTrans = Env.TRUE();
	private PlayerModule sys;

	/**
	 * auxiliaries are always added to the system module
	 * 
	 * @param cu
	 * @param aux
	 */
	public DdminUnrealizableCoreFreeQuickly(GameModel model) {
		this.model = model;
		this.sys = model.getSys();
		for (BehaviorInfo b : model.getAuxBehaviorInfo()) {
			if (b.isInitial()) {
				auxIni.andWith(b.initial);
			} else if (b.isSafety()) {
				auxTrans.andWith(b.safety);
			}
		}
		model.getAuxBehaviorInfo().clear();
		for (BehaviorInfo b : model.getEnvBehaviorInfo()) {
			b.free();
		}
		model.getEnvBehaviorInfo().clear();
	}

	/**
	 * check for unrealizability
	 * 
	 * @return true if sys is unrealizable
	 */
	@Override
	protected boolean check(List<BehaviorInfo> part) {

		try {
			buildSys(part);
		} catch (ModuleException e) {
			throw new RuntimeException(e);
		}

		boolean unrealizable;
		GR1Game rg = new GR1GameMemoryless(model);
		unrealizable = !rg.checkRealizability();
		rg.free();

		return unrealizable;
	}

	/**
	 * resets and then adds guarantees to sys module
	 * 
	 * @param part
	 * @throws ModuleException
	 */
	public void buildSys(List<BehaviorInfo> part) throws ModuleException {
		sys.reset();

		ArrayList<BehaviorInfo> gars = new ArrayList<BehaviorInfo>();
		gars.addAll(part);
		sys.conjunctInitial(auxIni.id());
		sys.conjunctTrans(auxTrans.id());
		for (BehaviorInfo gar : gars) {
			if (gar.isInitial()) {
				sys.conjunctInitial(gar.initial.id());
			}
			if (gar.isSafety()) {
				sys.conjunctTrans(gar.safety.id());
			}
			if (gar.isJustice()) {
				sys.addJustice(gar.justice.id());
			}
		}
		sys.trans();
	}

	@Override
	protected void dispose(List<BehaviorInfo> elements) {
		for (BehaviorInfo b : elements) {
			b.free();
		}
	}

}
