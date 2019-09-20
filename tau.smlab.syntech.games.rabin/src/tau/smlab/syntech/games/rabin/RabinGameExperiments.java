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
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

public class RabinGameExperiments extends RabinGame {

	public static boolean USE_START_Z = true;

	private BDD startZ = null;
	private BDD[] z_mem;

	public RabinGameExperiments(GameModel m) {
		super(m);
	}

	public void setStartZ(BDD startZ) {
		this.startZ = startZ;
	}

	public BDD[] getLastZMemoryCopy() {
		BDD[] z_mem_cpy = new BDD[sys.justiceNum()];
		for (int j = 0; j < sys.justiceNum(); j++) {
			z_mem_cpy[j] = z_mem[j].id();
		}

		return z_mem_cpy;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean checkRealizability() {
		System.out.println("RabinGameExperiments::checkRealizability with flags:");
		System.out.println("\tDETECT_FIX_POINT_EARLY = " + DETECT_FIX_POINT_EARLY);
		System.out.println("\tUSE_FIXPOINT_RECYCLE = " + USE_FIXPOINT_RECYCLE);
		System.out.println("\tSTOP_WHEN_WIN_FROM_SOME_INITIALS = " + STOP_WHEN_WIN_FROM_SOME_INITIALS);

		this.mem = new RabinMemory();
		z_mem = new BDD[sys.justiceNum()];

		int zIters = 0;
		int yIters = 0;
		int xIters = 0;

		Vector<BDD>[][] x_recycle = null;
		if (USE_FIXPOINT_RECYCLE) {
			x_recycle = new Vector[sys.justiceNum()][env.justiceNum()];
			for (int j = 0; j < sys.justiceNum(); j++) {
				for (int i = 0; i < env.justiceNum(); i++) {
					x_recycle[j][i] = new Vector<BDD>();
				}
			}
		}

		BDD x, y, z;
		FixPoint iterX, iterY, iterZ;

		z = Env.FALSE();
		boolean firstZ = true;

		if (startZ != null) {
//			System.out.println("use startZ: startZ.isZero = " + startZ.isZero());
			z = startZ;
		}

		for (iterZ = new FixPoint(false); iterZ.advance(z);) {
			zIters++;
			for (int j = 0; j < sys.justiceNum(); j++) {
				mem.addXLayer(env.justiceNum());

				BDD nextZ = env.controlStates(sys, z);
				BDD notJj = sys.justiceAt(j).not();

				y = Env.TRUE();
				int cy = 0;
				for (iterY = new FixPoint(false); iterY.advance(y);) {
					yIters++;
					mem.clearXLayer();
					BDD nextYandNotJj = env.controlStates(sys, y).andWith(notJj.id());

					y = Env.TRUE();
					for (int i = 0; i < env.justiceNum(); i++) {
						BDD pre = nextZ.id().orWith(env.justiceAt(i).and(nextYandNotJj));

						if (USE_FIXPOINT_RECYCLE && !firstZ) {
							// read X for corresponding Y
							if (x_recycle[j][i].size() <= cy) {
								x = x_recycle[j][i].lastElement().or(pre);
							} else {
								x = x_recycle[j][i].get(cy).or(pre);
							}
						} else {
							x = Env.FALSE();
						}

						for (iterX = new FixPoint(false); iterX.advance(x);) {
							xIters++;
							x = pre.id().orWith(notJj.id().andWith(env.controlStates(sys, x)));
							mem.addX(i, x);
						}

						if (USE_FIXPOINT_RECYCLE) {
							if (x_recycle[j][i].size() > cy) {
								x_recycle[j][i].get(cy).free();
							} else {
								// extend vector
								x_recycle[j][i].setSize(cy + 1);
							}
							// store fixed-point x for current y iteration
							x_recycle[j][i].set(cy, x.id());
						}
						y = y.and(x);
						pre.free();
					}
					nextYandNotJj.free();
					cy++;
				}

				if (USE_FIXPOINT_RECYCLE) {
					// shrink memory to size cy
					for (int i = 0; i < env.justiceNum(); i++) {
						if (x_recycle[j][i].size() > cy) {
							for (int k = cy; k < x_recycle[j][i].size(); k++) {
								x_recycle[j][i].get(k).free();
							}
							x_recycle[j][i].setSize(cy);
						}
					}
				}

				z = z.or(y);
				mem.addZ(z);
				z_mem[j] = z.id();

				nextZ.free();
				notJj.free();

				// breaking as early as possible
				if (DETECT_FIX_POINT_EARLY && mem.sizeZ() > sys.justiceNum()) { // at least one full loop
					int currZpos = mem.sizeZ() - 1;
					if (z.equals(mem.getZ(currZpos - sys.justiceNum()))) {
//						System.out.println("Stops early - found early fixed point");
//						System.out.println("currZpos = " + currZpos + ", efp pos = " + (currZpos - sys.justiceNum()));
//						System.out.println("Stops early - found early fixed point, j = " + j + ", sys justice num = "
//								+ sys.justiceNum());
//						int lastZpos = (mem.sizeZ() / sys.justiceNum()) * sys.justiceNum() - 1;
//						System.out.println("is z equals last zmem(" + lastZpos + ") = " + z.equals(mem.getZ(lastZpos)));
						// fixpoint reached because last iteration over all
						// system justices
						// did not add anything
						break; // jump to return below
					}
				}

				// NOTE: check if we win from some initials and stop early
				if (STOP_WHEN_WIN_FROM_SOME_INITIALS && envWinAllInitial(z.id())) {
					if (USE_FIXPOINT_RECYCLE) {
						free(x_recycle);
					}
					System.out.println("Stops early - env wins from some initial states");
					mem.setWin(z.id());
					mem.setComplete(false);
					System.out.println("Nodes num: " + z.getFactory().getNodeNum());
					System.out.println("Nodes table size: " + z.getFactory().getNodeTableSize());
//					System.out.println("z iterations = " + zIters);
//					System.out.println("y iterations = " + yIters);
//					System.out.println("x iterations = " + xIters);
					return true;
				}
			}

			firstZ = false;
		}

		mem.setWin(z.id());
		mem.setComplete(true);

		if (USE_FIXPOINT_RECYCLE) {
			free(x_recycle);
		}

		System.out.println("RabinGame.checkRealizability end");
		System.out.println("Nodes num: " + z.getFactory().getNodeNum());
		System.out.println("Nodes table size: " + z.getFactory().getNodeTableSize());
//		System.out.println("z iterations = " + zIters);
//		System.out.println("y iterations = " + yIters);
//		System.out.println("x iterations = " + xIters);

//		for (int j = 1; j < sys.justiceNum(); j++) {
//			System.out.println("check: z[" + (j - 1) + "] == z[" + j + "] : " + z_mem[j].equals(z_mem[j - 1]));
//		}
		return envWinAllInitial(z.id());
	}

	private void free(Vector<BDD>[][] x_recycle) {
		for (Vector<BDD>[] x : x_recycle) {
			for (Vector<BDD> y : x) {
				Env.free(y);
			}
		}
	}

	/**
	 * frees all BDDs in the x and z memory
	 */
	@Override
	public void free() {
		super.free();
		Env.free(z_mem);
	}

}
