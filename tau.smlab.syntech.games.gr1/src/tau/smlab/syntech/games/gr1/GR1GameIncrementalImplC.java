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

import java.util.Vector;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.jtlv.Env;

public class GR1GameIncrementalImplC extends GR1GameIncremental {

	public GR1GameIncrementalImplC(GameModel model) {
		super(model);

		incMem.startZ = Env.TRUE();
		incMem.prevZMem = new BDD[] { Env.TRUE() };
		incMem.prevFirstZIterMem = new BDD[] { Env.TRUE() };
		incMem.prevXMem = new BDD[][][] { { { Env.TRUE() } } };
	}

	@Override
	public boolean checkRealizability() {
		System.out.println("GR1GameIncrementalImplC.checkRealizability: start");

		int typeBitmap = incMem.getIncTypeBitmap();

		// TODO: TMP
		int[] prev_cy_mem = { 0 };
		// int[] prev_cy_mem = new int[prevZMem.length];
		// for (int i = 0; i < prevZMem.length; i++) prev_cy_mem[i]=0;

		BDD[] prev_xMem = { Env.TRUE() };
		// TODO: TMP
		if (typeBitmap != 0) {
			prev_cy_mem = new int[incMem.prevZMem.length];
			Vector<BDD> xMemVec = new Vector<BDD>();
			for (int j = 0; j < incMem.prevZMem.length; j++) {
				for (int i = 0; i < incMem.prevXMem[0].length; i++) {
					prev_cy_mem[j] = incMem.prevXMem[j][i].length;
					for (int cy = 0; cy < prev_cy_mem[j]; cy++) {
						xMemVec.add(incMem.prevXMem[j][i][cy]);
					}
				}
			}
			prev_xMem = new BDD[xMemVec.size()];
			for (int i = 0; i < xMemVec.size(); i++) {
				prev_xMem[i] = xMemVec.get(i);
			}
		}

		boolean isRealizble = (Env.TRUE().getFactory()).gr1GameWithIncData(getSysJArray(), getEnvJArray(),
				sys.initial(), env.initial(), sys.trans(), env.trans(), sys.moduleUnprimeVars(),
				env.moduleUnprimeVars(), sys.modulePrimeVars(), env.modulePrimeVars(), Env.allCouplesPairing(), false,
				false /* eun */, false, SIMULTANEOUS_CONJUNCTION_ABSTRACTION, typeBitmap, incMem.startZ,
				incMem.prevZMem, incMem.prevFirstZIterMem, prev_xMem, incMem.leastRemovedJusticeIdx,
				incMem.prevZMem.length, incMem.prevXMem[0].length, prev_cy_mem);

//		System.out.println("isRealizble = " + isRealizble);

		setGameMemFromC();
		firstZIterMem = (Env.TRUE().getFactory()).getZMemFirstItr();

		System.out.println("GR1GameIncrementalImplC.checkRealizability: end");
		System.out.println("Nodes num: " + Env.TRUE().getFactory().getNodeNum());
		System.out.println("Nodes table size: " + Env.TRUE().getFactory().getNodeTableSize());

		mem.setWin(mem.z_mem[sys.justiceNum() - 1]);
		mem.setComplete(true);
		return isRealizble;
	}

}
