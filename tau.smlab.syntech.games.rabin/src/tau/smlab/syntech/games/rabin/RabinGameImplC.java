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

package tau.smlab.syntech.games.rabin;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.jtlv.Env;

public class RabinGameImplC extends RabinGameExperiments {

	public static boolean LOG_VERBOSE = false;

	public RabinGameImplC(GameModel m) {
		super(m);
	}

	protected BDD[] getSysJArray() {
		BDD[] sysJ = new BDD[sys.justiceNum()];
		for (int i = 0; i < sys.justiceNum(); i++) {
			sysJ[i] = sys.justiceAt(i);
		}

		if (LOG_VERBOSE) {
			System.out.println("sys.justiceNum() = " + sys.justiceNum());
			for (int j = 0; j < sys.justiceNum(); j++) {
				System.out.println("J" + j + ": " + Env.toNiceSignleLineString(sys.justiceAt(j)));
			}
		}

		return sysJ;

	}

	protected BDD[] getEnvJArray() {
		BDD[] envJ = new BDD[env.justiceNum()];
		for (int i = 0; i < env.justiceNum(); i++) {
			envJ[i] = env.justiceAt(i);
		}

		if (LOG_VERBOSE) {
			System.out.println("env.justiceNum() = " + env.justiceNum());
			for (int j = 0; j < env.justiceNum(); j++) {
				System.out.println("J" + j + ": " + Env.toNiceSignleLineString(env.justiceAt(j)));
			}
		}

		return envJ;
	}

	protected boolean setGameMemFromC(boolean envWins) {
		int zSize = (Env.TRUE().getFactory()).getRabinZSize();
		BDD[] zMem = (Env.TRUE().getFactory()).getRabinZMem();
		System.out.println("zSize = " + zSize);
		System.out.println("zMem.length = " + zMem.length);

		for (int cz = 0; cz < zSize; cz++) {
			mem.addZ(zMem[cz]);
		}

		if (STOP_WHEN_WIN_FROM_SOME_INITIALS && envWins) {
			return false;
		}

		int[] xSizes = (Env.TRUE().getFactory()).getRabinXSizes();
		BDD[] xMem = (Env.TRUE().getFactory()).getRabinXMem();

		System.out.println("xSizes.length = " + xSizes.length);
		System.out.println("xMem.length = " + xMem.length);

		int xSizesIdx = 0;
		int[][] cx = new int[zSize][env.justiceNum()];

		for (int cz = 0; cz < zSize; cz++) {
			for (int i = 0; i < env.justiceNum(); i++) {
				if (LOG_VERBOSE)
					System.out.println("xSizesIdx = " + xSizesIdx);
				cx[cz][i] = xSizes[xSizesIdx];
				if (LOG_VERBOSE)
					System.out.println("cx[" + cz + "][" + i + "] = " + cx[cz][i]);
				xSizesIdx++;
			}
		}

		int xMemIdx = 0;
		for (int cz = 0; cz < zSize; cz++) {
			mem.addXLayer(env.justiceNum());
			for (int i = 0; i < env.justiceNum(); i++) {
				if (LOG_VERBOSE)
					System.out.println("cx[" + cz + "][" + i + "] = " + cx[cz][i]);
				for (int k = 0; k < cx[cz][i]; k++) {
					if (LOG_VERBOSE)
						System.out.println("xMemIdx = " + xMemIdx);
					mem.addX(i, xMem[xMemIdx]);
					xMemIdx++;
				}
			}
		}

		return true;
	}

	@Override
	public boolean checkRealizability() {
		System.out.println("RabinGameImplC.checkRealizability: start");

		// parameters: sys + env - justices, transitions, primed vars and a pairing of
		// primed+unprimed vars
		boolean envWins = (Env.TRUE().getFactory()).rabinGame(getSysJArray(), getEnvJArray(), sys.initial(),
				env.initial(), sys.trans(), env.trans(), sys.moduleUnprimeVars(), env.moduleUnprimeVars(),
				sys.modulePrimeVars(), env.modulePrimeVars(), Env.allCouplesPairing(), sys.getPartTransArray(),
				env.getPartTransArray(), sys.getQuantSetArray(), env.getQuantSetArray(), DETECT_FIX_POINT_EARLY,
				STOP_WHEN_WIN_FROM_SOME_INITIALS, USE_FIXPOINT_RECYCLE, SIMULTANEOUS_CONJUNCTION_ABSTRACTION);

//		System.out.println("is env winning = " + envWins);

		boolean completeMem = setGameMemFromC(envWins);

		System.out.println("RabinGameImplC.checkRealizability: end");
		System.out.println("Nodes num: " + Env.TRUE().getFactory().getNodeNum());
		System.out.println("Nodes table size: " + Env.TRUE().getFactory().getNodeTableSize());

		mem.setWin(mem.getZ(mem.sizeZ() - 1));
		mem.setComplete(completeMem);
		return envWins;
	}
}
