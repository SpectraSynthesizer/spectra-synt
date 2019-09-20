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

public class GR1GameMemoryless extends GR1Game {

	public static boolean DETECT_FIX_POINT_EARLY = true;
	public static boolean USE_FIXPOINT_RECYCLE = true;
	/**
	 * stop as soon as initial states are lost from Z (system cannot win all
	 * initials)
	 * 
	 * WARNING if activated not all winning states are computed
	 */
	public static boolean STOP_WHEN_INITIALS_LOST = true;

	public GR1GameMemoryless(GameModel model) {
		super(model);
	}

	@Override
	public boolean checkRealizability() {
		if (USE_FIXPOINT_RECYCLE) {
			mem.x_mem = new BDD[sys.justiceNum()][env.justiceNum()][50];
		}
		if (DETECT_FIX_POINT_EARLY) {
			mem.z_mem = new BDD[sys.justiceNum()];
		}
		boolean firstFixZ = true;

		BDD x = null, y, z;
		FixPoint iterZ, iterY, iterX;
		int cy = 0;

		z = Env.TRUE();

		BDD forceFalse = env.yieldStates(sys, Env.FALSE());
		BDD forceTrue = env.yieldStates(sys, Env.TRUE());

		for (iterZ = new FixPoint(true); iterZ.advance(z);) {

			for (int j = 0; j < sys.justiceNum(); j++) {

				cy = 0;
				y = Env.FALSE();

				BDD yieldZandJj = yield(z, forceTrue, forceFalse).andWith(sys.justiceAt(j).id());

				for (iterY = new FixPoint(true); iterY.advance(y);) {
					BDD yieldY = yield(y, forceTrue, forceFalse);
					BDD start = yieldZandJj.or(yieldY);
					yieldY.free();

					y = Env.FALSE();
					for (int i = 0; i < env.justiceNum(); i++) {

						BDD negp = env.justiceAt(i).not();

						if (x != null) {
							x.free();
						}

						if (USE_FIXPOINT_RECYCLE && !firstFixZ) {
							x = bestCandidate(cy, mem.x_mem[j][i]).and(z);
						} else {
							x = z.id();
						}

						for (iterX = new FixPoint(true); iterX.advance(x);) {

							BDD sysCtrl = yield(x, forceTrue, forceFalse);
							BDD sysCtrlAndNotJustice = sysCtrl.and(negp);
							sysCtrl.free();
							x = sysCtrlAndNotJustice.or(start);
							sysCtrlAndNotJustice.free();
						}

						if (USE_FIXPOINT_RECYCLE) {
							if (mem.x_mem[j][i][cy] != null && !mem.x_mem[j][i][cy].isFree()) {
								mem.x_mem[j][i][cy].free();
							}
							mem.x_mem[j][i][cy] = x.id();
						}
						BDD oy = y;
						y = y.or(x);
						oy.free();
						negp.free();
					}
					start.free();
					if (USE_FIXPOINT_RECYCLE) {
						cy++;
						if (cy % 50 == 0) {
							mem.x_mem = mem.extend_size(mem.x_mem, cy);
						}
					}
				} // end of Y fixed-point

				yieldZandJj.free();

				z = y.id();
				if (STOP_WHEN_INITIALS_LOST && !sysWinAllInitial(z)) {
					z.free();
					if (DETECT_FIX_POINT_EARLY || USE_FIXPOINT_RECYCLE) {
						mem.free();
					}
					forceFalse.free();
					forceTrue.free();
					mem.setWin(Env.FALSE());
					return false;
				}
				if (DETECT_FIX_POINT_EARLY && !firstFixZ) {
					// check if we will reach a fix-point after the end of this iteration
					if (mem.z_mem[j].equals(z)) {
						z.free();
						z = mem.z_mem[sys.justiceNum() - 1].id();
						break;
					}
				}
				if (DETECT_FIX_POINT_EARLY) {
					if (!firstFixZ) {
						mem.z_mem[j].free();
					}
					mem.z_mem[j] = z.id();
				}
			}

			firstFixZ = false;
		}

		if (DETECT_FIX_POINT_EARLY || USE_FIXPOINT_RECYCLE) {
			mem.free();
		}
		forceFalse.free();
		forceTrue.free();
		boolean win = sysWinAllInitial(z);
		mem.setWin(z);
		// z.free();
		return win;
	}

	/**
	 * find best entry for recycling: either the cy itself or the closest one from
	 * below
	 * 
	 * @param cy
	 * @param bdds
	 * @return
	 */
	private BDD bestCandidate(int cy, BDD[] bdds) {
		// check if entry exists to recycle
		if (cy < bdds.length) {
			if (bdds[cy] != null) {
				return bdds[cy];
			}
		}
		return Env.TRUE();
	}

	/**
	 * some shortcuts to not evaluate yieldStates in case of TRUE or FALSE
	 * 
	 * @param x
	 * @param forceTrue
	 * @param forceFalse
	 * @return
	 */
	private BDD yield(BDD x, BDD forceTrue, BDD forceFalse) {
		if (x.isZero()) {
			return forceFalse.id();
		}

		if (x.isOne()) {
			return forceTrue.id();
		}

		return env.yieldStates(sys, x);
	}
}
