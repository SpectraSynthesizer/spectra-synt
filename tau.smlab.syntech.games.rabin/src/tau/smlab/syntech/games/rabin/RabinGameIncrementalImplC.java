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

import java.util.Vector;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.games.GameIncrementalMemory;
import tau.smlab.syntech.jtlv.Env;

public class RabinGameIncrementalImplC extends RabinGameImplC {

	private RabinGameIncrementalMemory incMem;

	public RabinGameIncrementalImplC(GameModel m) {
		super(m);
		incMem = new RabinGameIncrementalMemory();
	}

	public GameIncrementalMemory GetGameIncrementalMemory() {
		return incMem;
	}

	public RabinMemory getMemoryCopy() {
		Vector<BDD> zMem = new Vector<BDD>();
		Vector<Vector<Vector<BDD>>> xMem = new Vector<Vector<Vector<BDD>>>();

		for (int i = 0; i < mem.sizeZ(); i++) {
			zMem.add(i, mem.getZ(i).id());
		}

		for (int i = 0; i < mem.getXMem().size(); i++) {
			Vector<Vector<BDD>> veci = new Vector<Vector<BDD>>();
			for (int j = 0; j < mem.getXMem().elementAt(i).size(); j++) {
				Vector<BDD> vecj = new Vector<BDD>();
				for (int k = 0; k < mem.getXMem().elementAt(i).elementAt(j).size(); k++) {
					// vecj.add(k, mem.getXMem().elementAt(i).elementAt(j).elementAt(k).id());
					vecj.add(k, Env.FALSE());
				}
				veci.add(j, vecj);
			}

			xMem.add(i, veci);
		}

		// Vector<Vector<BDD>> v1 = new Vector<Vector<BDD>>();
		// Vector<BDD> v2 = new Vector<BDD>();
		// v2.add(Env.FALSE());
		// v1.add(v2);
		// xMem.add(v1);
		RabinMemory memCopy = new RabinMemory(zMem, xMem);
		return memCopy;
	}

	@Override
	public boolean checkRealizability() {
		System.out.println("RabinGameIncrementalImplC.checkRealizability: start");

		int typeBitmap = incMem.getIncTypeBitmap();
//		System.out.println("typeBitmap = " + typeBitmap);
//		System.out.println("incMem.prevMem == null : " + (incMem.prevMem == null));
		BDD[] prev_xMem = { Env.FALSE() };
		BDD[] prev_zMem = { Env.FALSE() };
		int sizeD1 = 1;
		int sizeD2 = 1;
		int[] sizeD3 = { 1 };
		if ((typeBitmap != 0) && (incMem.prevMem != null)) {
			sizeD1 = incMem.prevMem.sizeZ();
			sizeD2 = env.justiceNum();
//			System.out.println("sizeD1 = " + sizeD1 + ", sizeD2 = " + sizeD2);
			Vector<BDD> xMemVec = new Vector<BDD>();
			Vector<Integer> sizeD3Vec = new Vector<Integer>();
			prev_zMem = new BDD[sizeD1];

			for (int cz = 0; cz < sizeD1; cz++) {
				prev_zMem[cz] = incMem.prevMem.getZ(cz);
				for (int i = 0; i < sizeD2; i++) {
					int cx = incMem.prevMem.getXMem().get(cz).get(i).size();
					sizeD3Vec.add(cx);
					for (int k = 0; k < cx; k++) {
						xMemVec.add(incMem.prevMem.getX(cz, i, k));
					}
				}
			}

			prev_xMem = new BDD[xMemVec.size()];
			for (int i = 0; i < xMemVec.size(); i++) {
				prev_xMem[i] = xMemVec.get(i);
			}

			sizeD3 = new int[sizeD3Vec.size()];
			for (int i = 0; i < sizeD3Vec.size(); i++) {
				sizeD3[i] = sizeD3Vec.get(i);
			}
		}

		STOP_WHEN_WIN_FROM_SOME_INITIALS = false;
		// parameters: sys + env - justices, transitions, primed vars and a pairing of
		// primed+unprimed vars
		boolean envWins = (Env.TRUE().getFactory()).rabinGameWithIncData(getSysJArray(), getEnvJArray(), sys.initial(),
				env.initial(), sys.trans(), env.trans(), sys.moduleUnprimeVars(), env.moduleUnprimeVars(),
				sys.modulePrimeVars(), env.modulePrimeVars(), Env.allCouplesPairing(), false, false, false,
				SIMULTANEOUS_CONJUNCTION_ABSTRACTION, typeBitmap, incMem.startZ, prev_zMem, prev_xMem,
				incMem.leastRemovedJusticeIdx, sizeD1, sizeD2, sizeD3);

//		System.out.println("is env winning = " + envWins);

		boolean completeMem = setGameMemFromC(envWins);

		System.out.println("RabinGameIncrementalImplC.checkRealizability: end");
		System.out.println("Nodes num: " + Env.TRUE().getFactory().getNodeNum());
		System.out.println("Nodes table size: " + Env.TRUE().getFactory().getNodeTableSize());

		mem.setWin(mem.getZ(mem.sizeZ() - 1));
		mem.setComplete(completeMem);
		return envWins;
	}

}
