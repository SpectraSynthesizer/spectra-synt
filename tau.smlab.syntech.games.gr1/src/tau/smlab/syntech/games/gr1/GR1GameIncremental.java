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
import tau.smlab.syntech.games.GameIncrementalMemory;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

public class GR1GameIncremental extends GR1GameImplC {

	protected BDD[] firstZIterMem;

	protected GR1GameIncrementalMemory incMem;

	// protected BDD startZ = null;
	// protected BDD[] prevZMem;
	// protected BDD[] prevFirstZIterMem;
	// protected BDD[][][] prevXMem;
	//
	// public boolean NEW_INI_ADDED = false;
	// public boolean NEW_SAFETY_ADDED = false;
	// public boolean NEW_JUSTICE_ADDED = false;
	// public boolean PREV_INI_REMOVED = false;
	// public boolean PREV_SAFETY_REMOVED = false;
	// public boolean PREV_JUSTICE_REMOVED = false;
	//
	// public int leastRemovedJusticeIdx = 0;

	public GR1GameIncremental(GameModel model) {
		super(model);
		incMem = new GR1GameIncrementalMemory();
	}

	// public void setStartZ(BDD startZ)
	// {
	// this.startZ = startZ;
	// }
	//
	// public void setPrevZMemory(BDD[] prevMem)
	// {
	// this.prevZMem = prevMem;
	// }
	//
	// public void setPrevFirstZIterMem(BDD[] prevFirstZIterMem)
	// {
	// this.prevFirstZIterMem = prevFirstZIterMem;
	// }
	//
	// public void setPrevXMem(BDD[][][] xMem)
	// {
	// this.prevXMem = xMem;
	// }

	public GameIncrementalMemory GetGameIncrementalMemory() {
		return incMem;
	}

	public BDD[] getZMemoryCopy() {
		return incMem.getZMemoryCopy(mem.z_mem, sys.justiceNum());
	}

	// public BDD[] getZMemoryCopy(BDD[] z_mem)
	// {
	// BDD[] z_mem_copy = new BDD[sys.justiceNum()];
	// for (int j = 0; j < z_mem.length; j++)
	// {
	// z_mem_copy[j] = z_mem[j].id();
	// }
	//
	// return z_mem_copy;
	// }

	public BDD[][][] getXMemoryCopy() {
		return incMem.getXMemoryCopy(mem.x_mem, 0, sys.justiceNum(), env.justiceNum());
	}

	// public BDD[][][] getXMemoryCopy(BDD[][][] x_mem, int x_currSize)
	// {
	// BDD[][][] x_mem_copy = new BDD[sys.justiceNum()][env.justiceNum()][];
	//
	// for (int j = 0; j < x_mem.length; j++)
	// {
	// for (int i = 0; i < x_mem[0].length; i++)
	// {
	// int cySize = x_mem[j][i].length;
	// if (x_currSize > cySize) cySize = x_currSize;
	// x_mem_copy[j][i] = new BDD[cySize];
	// for (int k = 0; k < x_mem[j][i].length; k++)
	// {
	// x_mem_copy[j][i][k] = x_mem[j][i][k].id();
	// }
	// }
	// }
	//
	// return x_mem_copy;
	// }

	public BDD[][] getPartialXMemoryCopy() {
		BDD[][] x_mem = new BDD[sys.justiceNum()][env.justiceNum()];
		for (int j = 0; j < sys.justiceNum(); j++) {
			for (int i = 0; i < env.justiceNum(); i++) {
				int lastXIdx = mem.x_mem[j][i].length - 1;
				x_mem[j][i] = mem.x_mem[j][i][lastXIdx].id();
			}
		}

		return x_mem;
	}

	public BDD[] getFirstZIterMem() {
		BDD[] z_mem_copy = new BDD[sys.justiceNum()];
		for (int j = 0; j < firstZIterMem.length; j++) {
			z_mem_copy[j] = firstZIterMem[j].id();
		}

		return z_mem_copy;
	}

	// public void addToStartZ(BDD z, int idx)
	// {
	// System.out.println("Add to startZ: startZ.isOne = " + startZ.isOne() +
	// ", segMem.zMem["+idx+"].isOne = " + z);
	// BDD tmp = startZ;
	// startZ = tmp.and(z);
	// tmp.free();
	// }

	@Override
	public boolean checkRealizability() {

		mem.x_mem = new BDD[sys.justiceNum()][env.justiceNum()][50];
		mem.y_mem = new BDD[sys.justiceNum()][50];
		mem.z_mem = new BDD[sys.justiceNum()];
		firstZIterMem = new BDD[sys.justiceNum()];

		int zIterCount = 0;
		@SuppressWarnings("unused")
		int yIterCount = 0;
		@SuppressWarnings("unused")
		int xIterCount = 0;

		if ((incMem.PREV_INI_REMOVED && !incMem.PREV_SAFETY_REMOVED && !incMem.PREV_JUSTICE_REMOVED)
				|| (incMem.NEW_INI_ADDED && !incMem.NEW_SAFETY_ADDED && !incMem.NEW_JUSTICE_ADDED)) {
			System.out.println("Only ini states changed: PREV_INI_REMOVED=" + incMem.PREV_INI_REMOVED
					+ ", NEW_INI_ADDED=" + incMem.NEW_INI_ADDED);
			BDD z = incMem.prevZMem[sys.justiceNum() - 1].id();
			mem.x_mem = incMem.getXMemoryCopy(incMem.prevXMem, 0, sys.justiceNum(), env.justiceNum());
			mem.z_mem = incMem.getZMemoryCopy(incMem.prevZMem, sys.justiceNum());
			firstZIterMem = incMem.getZMemoryCopy(incMem.prevFirstZIterMem, sys.justiceNum());

			mem.setWin(z);
			mem.setComplete(true);
			return sysWinAllInitial(z);
		}

		// if (!PREV_SAFETY_REMOVED && PREV_JUSTICE_REMOVED && leastRemovedJusticeIdx >
		// 0)
		// {
		// System.out.println("previous justice was removed, leastRemovedJusticeIdx = "
		// + leastRemovedJusticeIdx);
		//// mem.x_mem = new
		// BDD[sys.justiceNum()][env.justiceNum()][previousMem.x_mem.length];
		//// mem.y_mem = new BDD[sys.justiceNum()][previousMem.y_mem.length];
		//// mem.z_mem = new BDD[sys.justiceNum()];
		// for (int j = 0; j < leastRemovedJusticeIdx; j++)
		// {
		// mem.z_mem[j] = prevFirstZIterMem[j].id(); //previousMem.z_mem[j].id();
		// if (previousMem.y_mem[j].length / 50 > 1) {
		// mem.y_mem = mem.extend_size(mem.y_mem, previousMem.y_mem[j].length);
		// }
		// for (int k = 0; k < previousMem.y_mem[j].length; k++) {
		// mem.y_mem[j][k] = previousMem.y_mem[j][k].id();
		// }
		// for (int i = 0; i < env.justiceNum(); i++) {
		// if (previousMem.x_mem[j][i].length / 50 > 1) {
		// mem.x_mem = mem.extend_size(mem.x_mem, previousMem.x_mem[j][i].length);
		// }
		// for (int k = 0; k < previousMem.x_mem[j][i].length; k++) {
		// mem.x_mem[j][i][k] = previousMem.x_mem[j][i][k].id();
		// }
		// }
		// }
		// }

		// NOTE: The justices were removed only from the end of the list
		// if (!PREV_SAFETY_REMOVED && leastRemovedJusticeIdx == sys.justiceNum() &&
		// leastRemovedJusticeIdx != 0)
		// {
		// System.out.println("leastRemovedJusticeIdx = sys.justiceNum() = " +
		// leastRemovedJusticeIdx);
		// mem.setWin(mem.z_mem[sys.justiceNum() - 1]);
		// mem.setComplete(previousMem.isComplete());
		// mem.x_mem = mem.extend_size(mem.x_mem, 0);
		// mem.y_mem = mem.extend_size(mem.y_mem, 0);
		// //TODO: need to recompute or maybe not?
		// return sysWinAllInitial(mem.z_mem[sys.justiceNum() - 1]);
		// }

		BDD x = null, y, z;
		FixPoint iterZ, iterY, iterX;
		int cy = 0;

		boolean firstZItr = true;

		z = Env.TRUE();
		int jStartIdx = 0;

		// if (!PREV_SAFETY_REMOVED && PREV_JUSTICE_REMOVED && leastRemovedJusticeIdx >
		// 0)
		// {
		// System.out.println("previous justice was removed, leastRemovedJusticeIdx = "
		// + leastRemovedJusticeIdx);
		// for (int j = 0; j < leastRemovedJusticeIdx; j++)
		// {
		// mem.z_mem[j] = prevFirstZIterMem[j].id();
		// firstZIterMem[j] = prevFirstZIterMem[j].id();
		// }
		//
		// z = prevFirstZIterMem[leastRemovedJusticeIdx-1];
		// jStartIdx = leastRemovedJusticeIdx;
		// }

		if (incMem.NEW_JUSTICE_ADDED && !incMem.NEW_SAFETY_ADDED && sys.justiceNum() != 1) {
			System.out.println("New justice(s) was added, to an existing list of justices, "
					+ "current sys.justiceNum() = " + sys.justiceNum());
			jStartIdx = incMem.prevZMem.length;
			mem.z_mem = incMem.getZMemoryCopy(incMem.prevZMem, sys.justiceNum());
			// mem.x_mem = getXMemoryCopy(prevXMem, 50);
			for (int j = 0; j < incMem.prevXMem.length; j++) {
				for (int i = 0; i < incMem.prevXMem[j].length; i++) {
					for (int k = 0; k < incMem.prevXMem[j][i].length; k++) {
						mem.x_mem[j][i][k] = incMem.prevXMem[j][i][k].id();
					}
				}
			}

			firstZIterMem = incMem.getZMemoryCopy(incMem.prevFirstZIterMem, sys.justiceNum());
			z = incMem.prevZMem[incMem.prevZMem.length - 1];
			System.out.println(
					"New justice was added, starting from jStartIdx=" + jStartIdx + " and z.isOne=" + z.isOne());
		} else if (incMem.NEW_JUSTICE_ADDED || incMem.NEW_SAFETY_ADDED) {
			System.out.println("use startZ");
			z = incMem.startZ;
		}

		System.out.println("is starting Z from One = " + z.isOne());

		for (iterZ = new FixPoint(false); iterZ.advance(z);) {
			zIterCount++;

			System.out.println("sys.justiceNum() = " + sys.justiceNum());

			if (firstZItr && !incMem.PREV_SAFETY_REMOVED && incMem.PREV_JUSTICE_REMOVED
					&& incMem.leastRemovedJusticeIdx > 0) {
				System.out.println(
						"previous justice was removed, leastRemovedJusticeIdx = " + incMem.leastRemovedJusticeIdx);
				for (int k = 0; k < incMem.leastRemovedJusticeIdx; k++) {
					mem.z_mem[k] = incMem.prevFirstZIterMem[k].id();
					firstZIterMem[k] = incMem.prevFirstZIterMem[k].id();
				}
				if (z != null)
					z.free();
				z = incMem.prevFirstZIterMem[incMem.leastRemovedJusticeIdx - 1];
				jStartIdx = incMem.leastRemovedJusticeIdx;
				System.out.println("starting first z iteration from jStartIdx = " + jStartIdx);
			}

			for (int j = jStartIdx; j < sys.justiceNum(); j++) {
				System.out.println("j = " + j);
				cy = 0;
				y = Env.FALSE();

				if (incMem.PREV_SAFETY_REMOVED && !incMem.PREV_JUSTICE_REMOVED) {
					System.out.println("use prevZMem[" + j + "] for current Y");
					y = incMem.prevZMem[j].id();
					if (y.isZero()) {
						System.out.println("Y = FALSE");
					}
				}

				BDD yieldToJ = sys.justiceAt(j).id().andWith(env.yieldStates(sys, z));

				for (iterY = new FixPoint(false); iterY.advance(y);) {
					yIterCount++;
					BDD start = yieldToJ.id().orWith(env.yieldStates(sys, y));

					y = Env.FALSE();

					for (int i = 0; i < env.justiceNum(); i++) {
						BDD negp = env.justiceAt(i).not();
						if (x != null) {
							x.free();
						}

						if (incMem.NEW_SAFETY_ADDED && j < incMem.prevXMem.length /* prevPartialXMem.length */) {
							System.out.println("use prevXMem[" + j + "][" + i + "][" + cy + "] for current X");
							int k = cy;
							if (incMem.prevXMem[j][i].length <= cy) {
								System.out.println("\tprevXMem[j][i].length = " + incMem.prevXMem[j][i].length + " <= "
										+ cy + " = cy");
								k = incMem.prevXMem[j][i].length - 1;
							}

							x = incMem.prevXMem[j][i][k].and(z.id());

							// NOTE: log for checks
							// System.out.println("\tx.isOne = " + x.isOne());
							// System.out.println("\tz.isOne = " + z.isOne());
							// System.out.println("\tprevXMem.isOne = " + prevXMem[j][i][k].isOne());
							// System.out.println("\tx in z = " + z.and(x).equals(x));
							// System.out.println("\tz in x = " + z.and(x).equals(z));
							// System.out.println("\tz equals x = " + z.equals(x));

							// if (prevPartialXMem[j][i] == null) {
							// System.out.println("\tis null, can't use it");
							// x = z.id();
							// } else {
							// x = prevPartialXMem[j][i].and(z.id());
							// System.out.println("\tx.isOne = " + x.isOne());
							// System.out.println("\tz.isOne = " + z.isOne());
							// System.out.println("\tprevPartialXMem[j][i].isOne = " +
							// prevPartialXMem[j][i].isOne());
							// if (z.equals(prevPartialXMem[j][i]))
							// {
							// System.out.println("\tequals current z");
							// }
							// if (z.equals(x))
							// {
							// System.out.println("\tx equals current z - prevPartialXMem did not minimized
							// the possible states");
							// }
							// }
						} else {
							x = z.id();
						}
						for (iterX = new FixPoint(false); iterX.advance(x);) {
							xIterCount++;
							BDD sysCtrl = env.yieldStates(sys, x);
							BDD sysCtrlAndNotJustice = sysCtrl.and(negp);
							sysCtrl.free();
							x = sysCtrlAndNotJustice.or(start);
							sysCtrlAndNotJustice.free();
						}

						// NOTE: log for checks
						// if (NEW_SAFETY_ADDED && j < prevXMem.length ) {
						// int k = cy;
						// if (prevXMem[j][i].length <= cy) k = prevXMem[j][i].length - 1;
						//
						// System.out.println("\tx new fixed point: x.isOne = " + x.isOne());
						// System.out.println("\tx new fixed point: x in prev x = " +
						// x.and(prevXMem[j][i][k]).equals(x));
						// System.out.println("\tx new fixed point: x in prev x.and(z) = "
						// + x.and((prevXMem[j][i][k].and(z.id()))).equals(x));
						// }

						// System.out.println("x iters # " + xIterCount);
						xIterCount = 0;

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

				for (int i = 0; i < env.justiceNum(); i++) {
					for (int k = cy; k < mem.x_mem[j][i].length; k++) {
						mem.x_mem[j][i][k] = null;
					}
				}
				for (int k = cy; k < mem.y_mem[j].length; k++) {
					mem.y_mem[j][k] = null;
				}

				// System.out.println("y iters # " + yIterCount);
				yIterCount = 0;

				z = y.id();
				if (DETECT_FIX_POINT_EARLY && !firstZItr) {
					// check if we will reach a fix-point after the end of this iteration
					if (mem.z_mem[j].equals(z)) {
						z.free();
						z = mem.z_mem[sys.justiceNum() - 1].id();
						break;
					}
				}

				mem.z_mem[j] = z.id();
				if (firstZItr) {
					firstZIterMem[j] = z.id();
				}

				yieldToJ.free();
			}

			jStartIdx = 0;
			if (firstZItr) {
				firstZItr = false;
			}
		}

		System.out.println("z iters # " + zIterCount);

		mem.x_mem = mem.extend_size(mem.x_mem, 0);
		mem.y_mem = mem.extend_size(mem.y_mem, 0);

		mem.setWin(z);
		mem.setComplete(true);
		return sysWinAllInitial(z);
	}

	@Override
	public void free() {
		Env.free(firstZIterMem);
		mem.free();
	}

	public boolean sysWinAllInitial() {
		return this.sysWinAllInitial(mem.getWin());
	}

	public BDD sysWinningStates() {
		return mem.getWin();
	}

}
