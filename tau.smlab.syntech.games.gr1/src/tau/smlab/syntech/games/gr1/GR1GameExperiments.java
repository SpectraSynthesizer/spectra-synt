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

public class GR1GameExperiments extends GR1Game {

	public static boolean LOG_VERBOSE = false;

	// flags for initialize game optimizations
	public static boolean WITH_MEMORY = true;
	/**
	 * requires WITH_MEMORY
	 */
	public static boolean DETECT_FIX_POINT_EARLY = false;

	/**
	 * requires WITH_MEMORY
	 */
	public static boolean USE_FIXPOINT_RECYCLE = false;
	private boolean CHECK_FIXPOINT_RECYCLE_CORRECTNESS = false;

	/**
	 * stop as soon as initial states are lost from Z (system cannot win all
	 * initials)
	 * 
	 * WARNING if activated not all winning states are computed
	 */
	public static boolean STOP_WHEN_INITIALS_LOST = false;

	/**
	 * Use CUDD function to conjunct and abstract in parallel
	 */
	public static boolean SIMULTANEOUS_CONJUNCTION_ABSTRACTION = false;

	public GR1GameExperiments(GameModel model) {
		super(model);
	}

	@Override
	public boolean checkRealizability() {

		if (WITH_MEMORY) {
			mem.x_mem = new BDD[sys.justiceNum()][env.justiceNum()][50];
			mem.y_mem = new BDD[sys.justiceNum()][50];
			mem.z_mem = new BDD[sys.justiceNum()];
		}

		if (LOG_VERBOSE) {
			System.out.println("GR1GameExperiments.checkRealizability with flags:");
			System.out.println("\tWITH_MEMORY = " + WITH_MEMORY);
			System.out.println("\tDETECT_FIX_POINT_EARLY = " + DETECT_FIX_POINT_EARLY);
			System.out.println("\tUSE_FIXPOINT_RECYCLE = " + USE_FIXPOINT_RECYCLE);
			System.out.println("\tSTOP_WHEN_INITIALS_LOST = " + STOP_WHEN_INITIALS_LOST);
			System.out.println("\tSIMULTANEOUS_CONJUNCTION_ABSTRACTION = " + SIMULTANEOUS_CONJUNCTION_ABSTRACTION);
		}

		env.setSCA(SIMULTANEOUS_CONJUNCTION_ABSTRACTION);
		sys.setSCA(SIMULTANEOUS_CONJUNCTION_ABSTRACTION);

		int zIters = 0;
		int yIters = 0;
		int xIters = 0;

		boolean firstFixZ = true;

		BDD x = null, y, z;
		FixPoint iterZ, iterY, iterX;
		int cy = 0;

		z = Env.TRUE();

		BDD forceFalse = env.yieldStates(sys, Env.FALSE());
		BDD forceTrue = env.yieldStates(sys, Env.TRUE());

		for (iterZ = new FixPoint(false); iterZ.advance(z);) {

			zIters++;

			for (int j = 0; j < sys.justiceNum(); j++) {
				// System.out.println("Z loop " + zIters + ": j = " + j);

				cy = 0;
				y = Env.FALSE();

				BDD yieldZandJj = yield(z, forceTrue, forceFalse).andWith(sys.justiceAt(j).id());

				for (iterY = new FixPoint(false); iterY.advance(y);) {
					// System.out.println("y.equals(sys.justiceAt("+j+")) = " +
					// y.equals(sys.justiceAt(j)));
					BDD yieldY = yield(y, forceTrue, forceFalse);
					// System.out.println("yieldY.isOne() = " + yieldY.isOne());
					BDD start = yieldZandJj.or(yieldY);
					// System.out.println("start.equals(sys.justiceAt("+j+")) = " +
					// start.equals(sys.justiceAt(j)));
					yieldY.free();
					yIters++;

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

						for (iterX = new FixPoint(false); iterX.advance(x);) {
							// System.out.println("x.equals(sys.justiceAt("+j+")) = " +
							// x.equals(sys.justiceAt(j)));

							xIters++;

							BDD sysCtrl = yield(x, forceTrue, forceFalse);
							BDD sysCtrlAndNotJustice = sysCtrl.and(negp);
							sysCtrl.free();
							x = sysCtrlAndNotJustice.or(start);
							sysCtrlAndNotJustice.free();
						}

						if (CHECK_FIXPOINT_RECYCLE_CORRECTNESS && USE_FIXPOINT_RECYCLE) {
							if (!firstFixZ && !x.and(bestCandidate(cy, mem.x_mem[j][i])).equals(x)) {
								System.err.println("NOT correct x_mem[j][i][bestCy]");
							}
						}

						if (WITH_MEMORY) {
							if (mem.x_mem[j][i][cy] != null)
								mem.x_mem[j][i][cy].free();
							mem.x_mem[j][i][cy] = x.id();
						}

						BDD oy = y;
						y = y.or(x);
						oy.free();
						negp.free();
					} // end of justice i

					start.free();
					if (WITH_MEMORY) {
						if (mem.y_mem[j][cy] != null)
							mem.y_mem[j][cy].free();
						mem.y_mem[j][cy] = y.id();

						cy++;
						if (cy % 50 == 0) {
							mem.y_mem = mem.extend_size(mem.y_mem, cy);
							mem.x_mem = mem.extend_size(mem.x_mem, cy);
						}
					}
				} // end of Y fixed-point

				if (WITH_MEMORY) {
					for (int i = 0; i < env.justiceNum(); i++) {
						for (int k = cy; k < mem.x_mem[j][i].length; k++) {
							mem.x_mem[j][i][k] = null;
						}
					}
					for (int k = cy; k < mem.y_mem[j].length; k++) {
						mem.y_mem[j][k] = null;
					}
				}

				yieldZandJj.free();

				z = y.id();
				if (STOP_WHEN_INITIALS_LOST && !sysWinAllInitial(z)) {
					mem.setWin(Env.FALSE());
					// System.out.println("sysWinAllInitial is false - detected early
					// unrealizable");
					if (LOG_VERBOSE) {
						System.out.println("GR1GameExperiments.checkRealizability end");
						System.out.println("Nodes num: " + z.getFactory().getNodeNum());
						System.out.println("Nodes table size: " + z.getFactory().getNodeTableSize());
//						System.out.println("z iterations = " + zIters);
//						System.out.println("y iterations = " + yIters);
//						System.out.println("x iterations = " + xIters);
					}
					return false;
				}
				if (WITH_MEMORY) {
					if (DETECT_FIX_POINT_EARLY && !firstFixZ) {
						// check if we will reach a fix-point after the end of this iteration
						if (mem.z_mem[j].equals(z)) {
							z.free();
							z = mem.z_mem[sys.justiceNum() - 1].id();
							break;
						}
					}

					if (mem.z_mem[j] != null)
						mem.z_mem[j].free();
					mem.z_mem[j] = z.id();
				}
			}

			firstFixZ = false;
		}

		if (WITH_MEMORY) {
			mem.x_mem = mem.extend_size(mem.x_mem, 0);
			mem.y_mem = mem.extend_size(mem.y_mem, 0);
		}
		forceTrue.free();
		forceFalse.free();

		if (LOG_VERBOSE) {
			System.out.println("GR1GameExperiments.checkRealizability end");
			System.out.println("Nodes num: " + z.getFactory().getNodeNum());
			System.out.println("Nodes table size: " + z.getFactory().getNodeTableSize());
//			System.out.println("z iterations = " + zIters);
//			System.out.println("y iterations = " + yIters);
//			System.out.println("x iterations = " + xIters);
		}

		mem.setWin(z);
		mem.setComplete(true);
		return sysWinAllInitial(z);
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