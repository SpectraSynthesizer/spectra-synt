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

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.jtlv.Env;

public class GR1GameImplC extends GR1GameExperiments {

	public GR1GameImplC(GameModel model) {
		super(model);
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

	protected void setGameMemFromC() {
		setGameMemFromC(true, true);
	}

	protected void setGameMemFromC(boolean getXMem, boolean getYMem) {
		mem.x_mem = new BDD[sys.justiceNum()][env.justiceNum()][];
		mem.y_mem = new BDD[sys.justiceNum()][];

		mem.z_mem = (Env.TRUE().getFactory()).getZMem();
		if (LOG_VERBOSE)
			System.out.println("zMem.length = " + mem.z_mem.length);

		int[] attrSizes = (Env.TRUE().getFactory()).getAttrSizes();
		if (LOG_VERBOSE)
			System.out.println("attrSizes.length = " + attrSizes.length);

		if (getXMem) {
			BDD[] xMem = (Env.TRUE().getFactory()).getXMem();
			if (LOG_VERBOSE)
				System.out.println("xMem.length = " + xMem.length);
			int xMemIdx = 0;
			for (int j = 0; j < sys.justiceNum(); j++) {
				if (LOG_VERBOSE)
					System.out.println("attrSizes[" + j + "] = " + attrSizes[j]);
				for (int i = 0; i < env.justiceNum(); i++) {
					if (LOG_VERBOSE)
						System.out.println("i = " + i);
					if (LOG_VERBOSE)
						System.out.println("attrSizes[" + j + "] = " + attrSizes[j]);
					mem.x_mem[j][i] = new BDD[attrSizes[j]];
					for (int cy = 0; cy < attrSizes[j]; cy++) {
						if (LOG_VERBOSE)
							System.out.println("xMemIdx = " + xMemIdx);
						if (LOG_VERBOSE)
							System.out.println("cy = " + cy);
						mem.x_mem[j][i][cy] = xMem[xMemIdx];
						xMemIdx++;
					}
				}
			}
		}

		if (getYMem) {
			BDD[] yMem = (Env.TRUE().getFactory()).getYMem();
			if (LOG_VERBOSE)
				System.out.println("yMem.length = " + yMem.length);
			int yMemIdx = 0;
			for (int j = 0; j < sys.justiceNum(); j++) {
				mem.y_mem[j] = new BDD[attrSizes[j]];
				for (int cy = 0; cy < attrSizes[j]; cy++) {
					if (LOG_VERBOSE)
						System.out.println("yMemIdx = " + yMemIdx);
					if (LOG_VERBOSE)
						System.out.println("cy = " + cy);
					mem.y_mem[j][cy] = yMem[yMemIdx];
					yMemIdx++;
				}
			}
		}

	}

	@Override
	public boolean checkRealizability() {
		System.out.println("GR1GameImplC.checkRealizability: start");

		// parameters: sys + env - justices, transitions, primed vars and a pairing of
		// primed+unprimed vars
		boolean isRealizble = (Env.TRUE().getFactory()).gr1Game(getSysJArray(), getEnvJArray(), sys.initial(),
				env.initial(), sys.trans(), env.trans(), sys.moduleUnprimeVars(), env.moduleUnprimeVars(),
				sys.modulePrimeVars(), env.modulePrimeVars(), Env.allCouplesPairing(), sys.getPartTransArray(),
				env.getPartTransArray(), sys.getQuantSetArray(), env.getQuantSetArray(), DETECT_FIX_POINT_EARLY,
				STOP_WHEN_INITIALS_LOST, USE_FIXPOINT_RECYCLE, SIMULTANEOUS_CONJUNCTION_ABSTRACTION);

//		System.out.println("isRealizble = " + isRealizble);

		if (STOP_WHEN_INITIALS_LOST && !isRealizble) {
			System.out.println("GR1GameImplC.checkRealizability: end");
			System.out.println("Nodes num: " + Env.TRUE().getFactory().getNodeNum());
			System.out.println("Nodes table size: " + Env.TRUE().getFactory().getNodeTableSize());
			mem.setWin(Env.FALSE());
			return false;
		}

		setGameMemFromC();

		System.out.println("GR1GameImplC.checkRealizability: end");
		System.out.println("Nodes num: " + Env.TRUE().getFactory().getNodeNum());
		System.out.println("Nodes table size: " + Env.TRUE().getFactory().getNodeTableSize());

		mem.setWin(mem.z_mem[sys.justiceNum() - 1]);
		mem.setComplete(true);
		return isRealizble;
	}

}
