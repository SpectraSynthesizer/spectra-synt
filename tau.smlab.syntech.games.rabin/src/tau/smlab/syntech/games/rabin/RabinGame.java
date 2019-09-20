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
import tau.smlab.syntech.games.GameSolver;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

/**
 * winning states of game memory are winning for the environment!
 *
 */

public class RabinGame extends GameSolver {

	public static boolean DETECT_FIX_POINT_EARLY = true;
	public static boolean STOP_WHEN_WIN_FROM_SOME_INITIALS = true;
	public static boolean USE_FIXPOINT_RECYCLE = true;
	/**
	 * Use CUDD function to conjunct and abstract in parallel
	 */
	public static boolean SIMULTANEOUS_CONJUNCTION_ABSTRACTION = false;

	protected RabinMemory mem;

	public RabinGame(GameModel m) {
		super(m);
		mem = new RabinMemory();
		if (sys.justiceNum() == 0) {
			sys.addJustice(Env.TRUE());
		}
		if (env.justiceNum() == 0) {
			env.addJustice(Env.TRUE());
		}
	}

	@Override
	public boolean checkRealizability() {

		this.mem = new RabinMemory();

		BDD x, y, z;
		FixPoint iterX, iterY, iterZ;
		boolean firstFixZ = true;

		z = Env.FALSE();
		for (iterZ = new FixPoint(false); iterZ.advance(z);) {
			for (int j = 0; j < sys.justiceNum(); j++) {
				mem.addXLayer(env.justiceNum());

				BDD nextZ = env.controlStates(sys, z);
				BDD notJj = sys.justiceAt(j).not();

				y = Env.TRUE();
				for (iterY = new FixPoint(false); iterY.advance(y);) {
					mem.clearXLayer();
					BDD nextYandNotJj = env.controlStates(sys, y).andWith(notJj.id());

					y = Env.TRUE();
					for (int i = 0; i < env.justiceNum(); i++) {
						BDD pre = nextZ.id().orWith(env.justiceAt(i).and(nextYandNotJj));
						if (USE_FIXPOINT_RECYCLE && !firstFixZ) {
							x = mem.getXMem().get(mem.getXMem().size() - sys.justiceNum() - 1).get(i).lastElement()
									.or(pre);
						} else {
							x = Env.FALSE();
						}
						for (iterX = new FixPoint(false); iterX.advance(x);) {
							x = pre.id().orWith(notJj.id().andWith(env.controlStates(sys, x)));
							mem.addX(i, x);
						}
						y = y.and(x);
						pre.free();
					}
					nextYandNotJj.free();
				} // end y fix
				z = z.or(y);
				mem.addZ(z);

				nextZ.free();
				notJj.free();

				// breaking as early as possible
				if (DETECT_FIX_POINT_EARLY && mem.sizeZ() > sys.justiceNum()) { // at least one full loop
					int currZpos = mem.sizeZ() - 1;
					if (z.equals(mem.getZ(currZpos - sys.justiceNum()))) {
//						System.out.println("Stops early - found early fixed point");
						// fixpoint reached because last iteration over all
						// system justices
						// did not add anything
						break; // jump to return below
					}
				}

				// NOTE: check if we win from some initials and stop early
				if (STOP_WHEN_WIN_FROM_SOME_INITIALS && envWinAllInitial(z.id())) {
//					System.out.println("Stops early - env wins from some initial states");
					mem.setWin(z.id());
					mem.setComplete(false);
					return true;
				}
			}
			firstFixZ = false;
		}

		mem.setWin(z.id());
		mem.setComplete(true);
		return envWinAllInitial(z.id());
	}

	/**
	 * check existence of an env initial s.t. for all sys initial env wins
	 * 
	 * @return
	 */
	public boolean envWinAllInitial() {
		BDD envWinIni = envWinIni();
		boolean win = !envWinIni.isZero();
		envWinIni.free();
		return win;
	}

	private BDD envWinIni() {
		BDD sysDeadOrEnvWin = sys.initial().imp(mem.getWin());
		BDD envWinIni = env.initial().id().andWith(sysDeadOrEnvWin.forAll(sys.moduleUnprimeVars()));
		sysDeadOrEnvWin.free();
		return envWinIni;
	}

	public BDD getInitialStates() {
		BDD envWinIni = envWinIni();
		BDD ini = envWinIni.andWith(sys.initial().id());
		return ini;
	}

	@Override
	public void free() {
		mem.free();
	}

	public RabinMemory getMem() {
		return mem;
	}

}
