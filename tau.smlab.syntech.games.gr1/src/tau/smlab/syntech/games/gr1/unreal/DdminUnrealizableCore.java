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

import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.ModuleException;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.gamemodel.PlayerModule.TransFuncType;
import tau.smlab.syntech.games.gr1.GR1Game;
import tau.smlab.syntech.games.gr1.GR1GameExperiments;
import tau.smlab.syntech.games.gr1.GR1GameImplC;
import tau.smlab.syntech.games.rabin.RabinGame;
import tau.smlab.syntech.games.rabin.RabinGameExperiments;
import tau.smlab.syntech.games.rabin.RabinGameImplC;
import tau.smlab.syntech.games.util.AbstractDdmin;

/**
 * minimizes set of guarantees to keep system unrealizable
 * 
 * @author ringert
 * 
 */
public class DdminUnrealizableCore extends AbstractDdmin<BehaviorInfo> {
	protected GameModel model;
	protected List<BehaviorInfo> aux;
	protected PlayerModule sys;

	/**
	 * include aux in system module creation
	 */
	private boolean includeAux = true;

	enum GameType {
		GR1_GAME, RABIN_GAME
	}

	static public GameType gameType = GameType.GR1_GAME;
	static public boolean cImpl = false;

	/**
	 * auxiliaries are always added to the system module
	 * 
	 * @param cu
	 * @param aux
	 */
	public DdminUnrealizableCore(GameModel model) {
		this.model = model;
		this.sys = model.getSys();
		this.aux = model.getAuxBehaviorInfo();
	}

	/**
	 * check for unrealizability
	 * 
	 * @return true if sys is unrealizable
	 */
	@Override
	protected boolean check(List<BehaviorInfo> part) {

		long start = System.nanoTime();
		try {
			buildSys(part);
			System.out.println("time for buildSys for part " + part + ": " + (System.nanoTime() - start));
		} catch (ModuleException e) {
			throw new RuntimeException(e);
		}

		boolean unrealizable = false;
		if (gameType.equals(GameType.GR1_GAME)) {
			GR1Game rg;
			if (cImpl)
				rg = new GR1GameImplC(model);
			else
				rg = new GR1GameExperiments(model);

			unrealizable = !rg.checkRealizability();
			rg.free();
		} else if (gameType.equals(GameType.RABIN_GAME)) {
			RabinGame rg;
			if (cImpl)
				rg = new RabinGameImplC(model);
			else
				rg = new RabinGameExperiments(model);

			unrealizable = rg.checkRealizability();
			rg.free();
		}

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
		if (includeAux) {
			gars.addAll(aux);
		}
		gars.addAll(part);

		for (BehaviorInfo gar : gars) {
			if (gar.isInitial()) {
				sys.conjunctInitial(gar.initial.id());
			}
			if (gar.isSafety()) {
				if (PlayerModule.TEST_MODE) {
					sys.conjunctTrans(gar.safety.id());
				}

				switch (sys.getTransFuncType()) {
				case SINGLE_FUNC:
					sys.conjunctTrans(gar.safety.id());
					break;
				case DECOMPOSED_FUNC:
					sys.addToTransList(gar.safety.id());
					break;
				case PARTIAL_DECOMPOSED_FUNC:
					sys.addToTransList(gar.safety.id());
					break;
				default:
					System.err.println("unknown type " + sys.getTransFuncType());
					break;
				}
			}
			if (gar.isJustice()) {
				sys.addJustice(gar.justice.id(), gar.traceId);
			}
		}

		if (sys.getTransFuncType() == TransFuncType.DECOMPOSED_FUNC) {
			sys.calcTransQuantList();
		} else if (sys.getTransFuncType() == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
			sys.createPartialTransQuantList();
		}
	}

	public List<BehaviorInfo> getAuxCore() {
		includeAux = false;
		List<BehaviorInfo> res = minimize(model.getAuxBehaviorInfo());
		includeAux = true;
		return res;
	}

	@Override
	protected void dispose(List<BehaviorInfo> elements) {
		for (BehaviorInfo b : elements) {
			b.free();
		}
	}

}
