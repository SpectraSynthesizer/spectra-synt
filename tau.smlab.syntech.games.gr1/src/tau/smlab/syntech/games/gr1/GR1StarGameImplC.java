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

package tau.smlab.syntech.games.gr1;

import java.lang.reflect.Array;
import java.util.function.Function;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.jtlv.Env;

public class GR1StarGameImplC extends GR1GameImplC {

	public GR1StarGameImplC(GameModel model) {
		super(model);
	}

	private <T> T[] getSfaAttributeArray(Class<T[]> clazz, Function<Integer, T> sfaAttributeGetter) {
		T[] sfaAttribute = clazz.cast(Array.newInstance(clazz.getComponentType(), sys.existRegExpReqNum()));
		for (int i = 0, k = 0; k < sys.existReqNum(); k++) {
			if (sys.existReqHasRegExp(k)) {
				sfaAttribute[i] = sfaAttributeGetter.apply(k);
				i++;
			}
		}
		return sfaAttribute;
	}

	protected BDD[] getExistGarsSfaIniArray() {
		BDD[] sfaIni = getSfaAttributeArray(BDD[].class,
				existReqIdx -> sys.existReqAt(existReqIdx).getRegExpSfaConstraint().getIni());
		if (LOG_VERBOSE) {
			System.out.println("sys.existRegExpReqNum() = " + sys.existRegExpReqNum());
			for (int i = 0, k = 0; k < sys.existReqNum(); k++) {
				if (sys.existReqHasRegExp(k)) {
					System.out.println("Exist Req." + k + " initial state: " + Env.toNiceSignleLineString(sfaIni[i]));
					i++;
				}
			}
		}
		return sfaIni;
	}

	protected BDD[] getExistGarsSfaTransArray() {
		BDD[] sfaTrans = getSfaAttributeArray(BDD[].class,
				existReqIdx -> sys.existReqAt(existReqIdx).getRegExpSfaConstraint().getTrans());
		if (LOG_VERBOSE) {
			System.out.println("sys.existRegExpReqNum() = " + sys.existRegExpReqNum());
			for (int i = 0, k = 0; k < sys.existReqNum(); k++) {
				if (sys.existReqHasRegExp(k)) {
					System.out.println("Exist Req." + k + " transitions:" + System.lineSeparator()
							+ Env.toNiceSignleLineString(sfaTrans[i]) + System.lineSeparator());
					i++;
				}
			}
		}
		return sfaTrans;
	}

	protected BDD[] getExistGarsSfaTransToAccArray() {
		BDD[] sfaTransToAcc = getSfaAttributeArray(BDD[].class,
				existReqIdx -> sys.existReqAt(existReqIdx).getRegExpSfaConstraint().getTransToAcceptance());
		if (LOG_VERBOSE) {
			System.out.println("sys.existRegExpReqNum() = " + sys.existRegExpReqNum());
			for (int i = 0, k = 0; k < sys.existReqNum(); k++) {
				if (sys.existReqHasRegExp(k)) {
					System.out.println("Exist Req." + k + " transitions to accepting states: "
							+ Env.toNiceSignleLineString(sfaTransToAcc[i]));
					i++;
				}
			}
		}
		return sfaTransToAcc;
	}

	protected BDDVarSet[] getExistGarsSfaUnprimeStatesVars() {
		BDDVarSet[] sfaUnprimeStatesVars = getSfaAttributeArray(BDDVarSet[].class,
				existReqIdx -> sys.existReqAt(existReqIdx).getRegExpSfaConstraint().getStatesVar().getDomain().set());
		if (LOG_VERBOSE) {
			System.out.println("sys.existRegExpReqNum() = " + sys.existRegExpReqNum());
			for (int k = 0; k < sys.existReqNum(); k++) {
				if (sys.existReqHasRegExp(k)) {
					System.out.println("Exist Req." + k + " unprime states variable: "
							+ sys.existReqAt(k).getRegExpSfaConstraint().getStatesVarName());
				}
			}
		}
		return sfaUnprimeStatesVars;
	}

	protected BDDVarSet[] getExistGarsSfaPrimeStatesVars() {
		BDDVarSet[] sfaPrimeStatesVars = getSfaAttributeArray(BDDVarSet[].class, existReqIdx -> sys
				.existReqAt(existReqIdx).getRegExpSfaConstraint().getStatesVar().getOtherDomain().set());
		if (LOG_VERBOSE) {
			System.out.println("sys.existRegExpReqNum() = " + sys.existRegExpReqNum());
			for (int k = 0; k < sys.existReqNum(); k++) {
				System.out.println("Exist Req." + k + " primed states variable: "
						+ sys.existReqAt(k).getRegExpSfaConstraint().getStatesVar().getOtherDomain().getName());
			}
		}
		return sfaPrimeStatesVars;
	}

	protected void setJusticeIterMemFromC() {
		if (WITH_MEMORY) {
			mem.x_mem = new BDD[sys.justiceNum()][env.justiceNum()][];
			mem.y_mem = new BDD[sys.justiceNum()][];

			int[] justiceIterNum = (Env.TRUE().getFactory()).getGr1StarJusticeIterNum();

			BDD[] xMem = (Env.TRUE().getFactory()).getGr1StarXMem();
			int xMemIdx = 0;
			for (int j = 0; j < sys.justiceNum(); j++) {
				for (int i = 0; i < env.justiceNum(); i++) {
					mem.x_mem[j][i] = new BDD[justiceIterNum[j]];
					for (int cy = 0; cy < justiceIterNum[j]; cy++) {
						mem.x_mem[j][i][cy] = xMem[xMemIdx];
						xMemIdx++;
					}
				}
			}

			BDD[] yMem = (Env.TRUE().getFactory()).getGr1StarYMem();
			int yMemIdx = 0;
			for (int j = 0; j < sys.justiceNum(); j++) {
				mem.y_mem[j] = new BDD[justiceIterNum[j]];
				for (int cy = 0; cy < justiceIterNum[j]; cy++) {
					mem.y_mem[j][cy] = yMem[yMemIdx];
					yMemIdx++;
				}
			}
		}
	}

	protected void setExistentialMemFromC() {
		if (WITH_MEMORY && sys.existRegExpReqNum() > 0) {
			mem.fulfill_exist_gar_mem = new BDD[sys.existRegExpReqNum()][];
			mem.towards_exist_gar_mem = new BDD[sys.existRegExpReqNum()][];

			int[] fExistIterNum = (Env.TRUE().getFactory()).getGr1StarFulfillExistIterNum();

			BDD[] fExistMem = (Env.TRUE().getFactory()).getGr1StarFulfillExistMem();
			int fExistIdx = 0;
			for (int k = 0; k < sys.existRegExpReqNum(); k++) {
				mem.fulfill_exist_gar_mem[k] = new BDD[fExistIterNum[k]];
				for (int i = 0; i < fExistIterNum[k]; i++) {
					mem.fulfill_exist_gar_mem[k][i] = fExistMem[fExistIdx];
					fExistIdx++;
				}
			}

			int[] tExistIterNum = (Env.TRUE().getFactory()).getGr1StarTowardsExistIterNum();

			BDD[] tExistMem = (Env.TRUE().getFactory()).getGr1StarTowardsExistMem();
			int tExistIdx = 0;
			for (int k = 0; k < sys.existRegExpReqNum(); k++) {
				mem.towards_exist_gar_mem[k] = new BDD[tExistIterNum[k]];
				for (int i = 0; i < tExistIterNum[k]; i++) {
					mem.towards_exist_gar_mem[k][i] = tExistMem[tExistIdx];
					tExistIdx++;
				}
			}

			int envJVIterNum = (Env.TRUE().getFactory()).getGr1StarEnvJViolationIterNum();
			mem.envJusticeViolation_mem = new BDD[env.justiceNum()][envJVIterNum];
			BDD[] envJVMem = (Env.TRUE().getFactory()).getGR1StarEnvJViolationMem();

			int envJVIdx = 0;
			for (int i = 0; i < env.justiceNum(); i++) {
				for (int cy = 0; cy < envJVIterNum; cy++) {
					mem.envJusticeViolation_mem[i][cy] = envJVMem[envJVIdx];
					envJVIdx++;
				}
			}
		}
	}

	protected boolean invokeNativeGameSolver() {
		return (Env.TRUE().getFactory()).gr1StarGame(getSysJArray(), getEnvJArray(), getExistGarsSfaIniArray(),
				getExistGarsSfaTransArray(), getExistGarsSfaTransToAccArray(), getExistGarsSfaUnprimeStatesVars(),
				getExistGarsSfaPrimeStatesVars(), sys.initial(), env.initial(), sys.trans(), env.trans(),
				sys.moduleUnprimeVars(), env.moduleUnprimeVars(), sys.modulePrimeVars(), env.modulePrimeVars(),
				Env.allCouplesPairing(), sys.getPartTransArray(), env.getPartTransArray(), sys.getQuantSetArray(),
				env.getQuantSetArray(), DETECT_FIX_POINT_EARLY, STOP_WHEN_INITIALS_LOST, USE_FIXPOINT_RECYCLE,
				SIMULTANEOUS_CONJUNCTION_ABSTRACTION, WITH_MEMORY);
	}

	@Override
	public boolean checkRealizability() {
		/*
		 * System.out.println("GR1StarGameImplC.checkRealizability: start");
		 * 
		 * System.out.println("\tWITH_MEMORY = " + WITH_MEMORY);
		 * System.out.println("\tDETECT_FIX_POINT_EARLY = " + DETECT_FIX_POINT_EARLY);
		 * System.out.println("\tUSE_FIXPOINT_RECYCLE = " + USE_FIXPOINT_RECYCLE);
		 * System.out.println("\tSTOP_WHEN_INITIALS_LOST = " + STOP_WHEN_INITIALS_LOST);
		 * System.out.println("\tSIMULTANEOUS_CONJUNCTION_ABSTRACTION = " +
		 * SIMULTANEOUS_CONJUNCTION_ABSTRACTION);
		 */

		boolean isRealizble = invokeNativeGameSolver();

		setJusticeIterMemFromC();
		setExistentialMemFromC();

		// System.out.println("GR1StarGameImplC.checkRealizability: end");
		// System.out.println("Nodes num: " + Env.TRUE().getFactory().getNodeNum());
		// System.out.println("Nodes table size: " +
		// Env.TRUE().getFactory().getNodeTableSize());

		mem.setWin((Env.FALSE().getFactory()).getGr1StarWinningStates());
		mem.setComplete(true);
		return isRealizble;
	}

}
