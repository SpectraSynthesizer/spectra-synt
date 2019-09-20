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
import tau.smlab.syntech.jtlv.lib.FixPoint;

public class GR1GameCooperative extends GR1Game {

	public static boolean DETECT_FIX_POINT_EARLY = true;
	public static boolean USE_FIXPOINT_RECYCLE = true;
	/**
	 * stop as soon as initial states are lost from Z (system cannot win all
	 * initials)
	 * 
	 * WARNING if activated not all winning states are computed
	 */
	public static boolean STOP_WHEN_INITIALS_LOST = true;

	public GR1GameCooperative(GameModel model) {
		super(model);
	}

	@Override
	public boolean checkRealizability() {

		// following Ehlers, Koenighofer, Bloem 2015
		// Part 0: add assumptions as guarantees
		for (int i = 0; i < env.justiceNum(); i++) {
			try {
				sys.addJustice(env.justiceAt(i));
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		}

		mem.x_mem = new BDD[sys.justiceNum()][env.justiceNum()][50];
		mem.y_mem = new BDD[sys.justiceNum()][50];
		mem.z_mem = new BDD[sys.justiceNum()];

		BDD[][] recycleFixX = new BDD[sys.justiceNum()][env.justiceNum()];
		boolean firstFixZ = true;

		BDD x = null, y, z;
		FixPoint iterZ, iterY, iterX;
		int cy = 0;

		z = Env.TRUE();

		// following Ehlers, Koenighofer, Bloem 2015
		// Part 1: removing system successors that deadlock environment
		sys.conjunctTrans(Env.prime(env.trans().exist(Env.globalPrimeVars())));

		for (iterZ = new FixPoint(false); iterZ.advance(z);) {

			for (int j = 0; j < sys.justiceNum(); j++) {

				cy = 0;
				y = Env.FALSE();

				boolean firstFixY = true;

				BDD yieldZandJj = env.yieldStates(sys, z).andWith(sys.justiceAt(j).id());

				for (iterY = new FixPoint(false); iterY.advance(y);) {
					BDD yieldY = env.yieldStates(sys, y);
					BDD start = yieldZandJj.or(yieldY);
					BDD yieldYorJj = null;
					yieldYorJj = yieldY.orWith(sys.justiceAt(j).id());
					y = Env.FALSE();
					for (int i = 0; i < env.justiceNum(); i++) {

						BDD negp = env.justiceAt(i).not();

						if (x != null) {
							x.free();
						}

						if (USE_FIXPOINT_RECYCLE && firstFixY && !firstFixZ) {
							x = recycleFixX[j][i].and(z);
						} else {
							x = z.id();
						}

						for (iterX = new FixPoint(false); iterX.advance(x);) {

							BDD sysCtrl = env.yieldStates(sys, x);
							BDD sysCtrlAndNotJustice = sysCtrl.and(negp);
							sysCtrl.free();
							x = sysCtrlAndNotJustice.or(start);
							sysCtrlAndNotJustice.free();

							// following Ehlers, Koenighofer, Bloem 2015
							// Part 2: restrict x to positions where progress is made
							BDD bothTrans = env.trans().and(sys.trans());

							BDD R = Env.FALSE();
							for (FixPoint iterR = new FixPoint(false); iterR.advance(R);) {
								R = Env.pred(bothTrans, R).or(yieldYorJj).and(x);
							}
							bothTrans.free();
							x.andWith(R);
						}

						mem.x_mem[j][i][cy] = x.id();
						if (USE_FIXPOINT_RECYCLE) {
							recycleFixX[j][i] = mem.x_mem[j][i][cy];
						}
						BDD oy = y;
						y = y.or(x);
						oy.free();
						negp.free();
					}
					start.free();
					if (yieldYorJj != null) {
						yieldYorJj.free();
					}
					mem.y_mem[j][cy] = y.id();
					cy++;
					if (cy % 50 == 0) {
						mem.x_mem = mem.extend_size(mem.x_mem, cy);
						mem.y_mem = mem.extend_size(mem.y_mem, cy);
					}
					firstFixY = false;
				}

				yieldZandJj.free();

				z = y.id();
				if (STOP_WHEN_INITIALS_LOST && !sysWinAllInitial(z)) {
					mem.setWin(Env.FALSE());
					return false;
				}
				if (DETECT_FIX_POINT_EARLY && !firstFixZ) {
					// check if we will reach a fix-point after the end of this iteration
					if (mem.z_mem[j].equals(z)) {
						z.free();
						z = mem.z_mem[sys.justiceNum() - 1];
						break;
					}
				}
				mem.z_mem[j] = z.id();
			}

			// following Ehlers, Koenighofer, Bloem 2015
			// Part 3: make sure winning states have cooperative way to satisfy assumptions
			BDD bothTrans = env.trans().and(sys.trans());
			for (int i = 0; i < env.justiceNum(); i++) {
				BDD R = Env.FALSE();
				for (FixPoint iterR = new FixPoint(false); iterR.advance(R);) {
					R = Env.pred(bothTrans, R).or(env.justiceAt(i)).and(z);
				}
				z.andWith(R);
			}
			bothTrans.free();

			firstFixZ = false;
		}

		mem.x_mem = mem.extend_size(mem.x_mem, 0);
		mem.y_mem = mem.extend_size(mem.y_mem, 0);

		mem.setWin(z);
		mem.setComplete(true);
		return sysWinAllInitial(z);
	}

}
