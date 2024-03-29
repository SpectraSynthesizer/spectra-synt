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

import java.util.ArrayList;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.games.GameSolver;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

public class GR1Game extends GameSolver {

	protected GR1Memory mem;

	public GR1Memory getMem() {
		return mem;
	}

	public GR1Game(GameModel model) {
		super(model);
		mem = new GR1Memory();
		if (sys.justiceNum() == 0) {
			sys.addJustice(Env.TRUE());
		}
		if (env.justiceNum() == 0) {
			env.addJustice(Env.TRUE());
		}
	}

	@Override
	public boolean checkRealizability() {
		//System.out.println("-----------------------------------------------------------------------");
		mem.x_mem = new BDD[sys.justiceNum()][env.justiceNum()][50];
		mem.y_mem = new BDD[sys.justiceNum()][50];
		mem.z_mem = new BDD[sys.justiceNum()];
		
		mem.z_fixpoints = new ArrayList<>();

		BDD x = null, y, z;
		FixPoint iterZ, iterY, iterX;
		int cy = 0;

		z = Env.TRUE();
		for (iterZ = new FixPoint(false); iterZ.advance(z);) {

			for (int j = 0; j < sys.justiceNum(); j++) {
				
				cy = 0;
				y = Env.FALSE();
				for (iterY = new FixPoint(false); iterY.advance(y);) {
					BDD start = sys.justiceAt(j).id().andWith(env.yieldStates(sys, z)).orWith(env.yieldStates(sys, y));
					y = Env.FALSE();
					for (int i = 0; i < env.justiceNum(); i++) {
						BDD negp = env.justiceAt(i).not();
						if (x != null)
							x.free();
						x = z.id();
						for (iterX = new FixPoint(false); iterX.advance(x);) {

							BDD sysCtrl = env.yieldStates(sys, x);
							BDD sysCtrlAndNotJustice = sysCtrl.and(negp);
							sysCtrl.free();
							x = sysCtrlAndNotJustice.or(start);
							sysCtrlAndNotJustice.free();
						}
						mem.x_mem[j][i][cy] = x.id();
						BDD oy = y;
						y = y.or(x);
						oy.free();
					}
					start.free();
					mem.y_mem[j][cy] = y.id();
					cy++;
					if (cy % 50 == 0) {
						mem.x_mem = mem.extend_size(mem.x_mem, cy);
						mem.y_mem = mem.extend_size(mem.y_mem, cy);
					}
				}
				z = y.id();
				mem.z_mem[j] = z.id();
			}
			
			mem.z_fixpoints.add(z.id());
		}
		mem.x_mem = mem.extend_size(mem.x_mem, 0);
		mem.y_mem = mem.extend_size(mem.y_mem, 0);

		mem.setWin(z);
		mem.setComplete(true);
		return sysWinAllInitial(z);
	}
	
//	private BDD X_gfp(BDD top, BDD F, BDD start) {
//		FixPoint iterX;
//		BDD ret = top.id();
//		for (iterX = new FixPoint(false); iterX.advance(ret);) {
//
//			ret = env.yieldStates(sys, ret).and(F).or(start);
//		}
//		return ret;
//	}
//	
//	private BDD Y_lfp(BDD top, BDD F) {
//		FixPoint iterY;
//		BDD ret = Env.FALSE();
//		for (iterY = new FixPoint(false); iterY.advance(ret);) {
//			BDD start = F.orWith(env.yieldStates(sys, ret));
//
//			BDD tmpRet = Env.FALSE();
//			for (int i = 0; i < env.justiceNum(); i++) {
//				tmpRet = tmpRet.or(X_gfp(top, env.justiceAt(i).not(), start));
//			}
//			start.free();
//			ret = tmpRet.id();
//		}
//		return ret;
//	}
//	
//	private BDD Z_gfp() {
//		FixPoint iterZ;
//		BDD ret = Env.TRUE();
//		
//		for (iterZ = new FixPoint(false); iterZ.advance(ret);) {
//
//			for (int j = 0; j < sys.justiceNum(); j++) {
//				BDD F = sys.justiceAt(j).id().andWith(env.yieldStates(sys, ret));
//				ret = Y_lfp(ret, F);
//			}
//		}
//		return ret;
//	}

	@Override
	public void free() {
		mem.free();
	}

	public boolean sysWinAllInitial() {
		return this.sysWinAllInitial(mem.getWin());
	}

	public BDD sysWinningStates() {
		return mem.getWin();
	}
}
