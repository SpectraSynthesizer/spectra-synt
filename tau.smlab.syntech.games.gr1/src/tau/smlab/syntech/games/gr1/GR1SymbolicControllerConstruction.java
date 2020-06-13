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
import tau.smlab.syntech.games.controller.symbolic.SymbolicController;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerConstruction;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

public class GR1SymbolicControllerConstruction extends SymbolicControllerConstruction {

	private GR1Memory mem;

	public GR1SymbolicControllerConstruction(GR1Memory mem, GameModel m) {
		super(mem, m);
		// not sure if needed
		this.mem = mem;
	}

	/**
	 * check if inclusion of y cells is necessary
	 * 
	 * FIXME check whether this si correctly implemented
	 */
	public static boolean STOP_EARLY_ON_Y = false;

	/**
	 * this is simply Fig. 3 of the GR(1) journal paper BJP+12
	 */
	public SymbolicController calculateSymbolicController() {	
		
		BDD Z = mem.z_mem[mem.z_mem.length - 1];

		BDD[] collectedZ = new BDD[sys.justiceNum()];

		ModuleBDDField Zn = null;
		SymbolicController ctrl = new SymbolicController();
		try {
			int upper = Math.max(this.sys.justiceNum() - 1, 1);
			Zn = sys.addVar("Zn", 0, upper, true);
			ctrl.setInit(this.env.initial().id().andWith(Z.id()).andWith(this.sys.initial().id())
					.andWith(Zn.getDomain().ithVar(0)));
		} catch (Exception e) {
			e.printStackTrace();
		}
		

		BDD tr12 = this.sys.trans().and(this.env.trans());

		// rho3 transitions avoid satisfying the justice assumption of the
		// environment and stay at same distance from satisfying current justice
		// guarantee
		for (int j = 0; j < sys.justiceNum(); j++) {
			BDD low = Env.FALSE();
			BDD ZnVarj = Zn.getDomain().ithVar(j);
			for (int r = 0; r < maxR(mem.x_mem[j]); r++) {
				for (int i = 0; i < this.env.justiceNum(); i++) {
					if (r < mem.x_mem[j][i].length) {
						BDD rho3 = ZnVarj.and(mem.x_mem[j][i][r]).andWith(low.not()).andWith(env.justiceAt(i).not())
								.andWith(tr12.id()).andWith(Env.prime(mem.x_mem[j][i][r])).andWith(Env.prime(ZnVarj));
						low.orWith(mem.x_mem[j][i][r].id());
						ctrl.disjunctTrans(rho3);
						rho3.free();
					}
				}
			}
			ZnVarj.free();
			low.free();
		}

		// rho2 transitions get us close to the state satisfying the current justice
		// guarantee
		// TODO here we just go lower in y vector can we ensure fastests decrease?
		for (int j = 0; j < sys.justiceNum(); j++) {
			BDD low = mem.y_mem[j][0].id();
			BDD ZnVarj = Zn.getDomain().ithVar(j);
			if (STOP_EARLY_ON_Y) {
				collectedZ[j] = Env.FALSE();
			} else {
				collectedZ[j] = Z.id();
			}
			for (int r = 1; r < mem.y_mem[j].length; r++) {
				if (STOP_EARLY_ON_Y) {
					if (r > 1 && j > 0) {
						// if all states of previous justice are contained stop checking
						// however compute complete attractor for j=0 to also capture all initial states
						int prevJ = (j + sys.justiceNum() - 1) % sys.justiceNum();
						BDD rest = mem.y_mem[prevJ][0].and(mem.y_mem[j][r - 1].not());
						if (rest.isZero()) {
							rest.free();
							break;
						}
						rest.free();
					}
					collectedZ[j].orWith(mem.y_mem[j][r].id());
				}
				BDD rho2 = ZnVarj.and(mem.y_mem[j][r]).andWith(low.not()).andWith(tr12.id()).andWith(Env.prime(low))
						.andWith(Env.prime(ZnVarj));
				low.orWith(mem.y_mem[j][r].id());
				ctrl.disjunctTrans(rho2);
				rho2.free();
			}
			ZnVarj.free();
			low.free();
		}

		// rho1 transitions are taken when the current justice guarantee is
		// satisfied. Successors are winning states for the next justice guarantee
		// TODO see if we can get closest to satisfying next justice
		for (int j = 0; j < sys.justiceNum(); j++) {
			int next_j = (j + 1) % sys.justiceNum();
			BDD ZnVarNextJ = Zn.getDomain().ithVar(next_j);
			BDD rho1 = Zn.getDomain().ithVar(j).andWith(sys.justiceAt(j).id()).andWith(tr12.id())
					.andWith(Env.prime(Z.id())).andWith(Env.prime(ZnVarNextJ));
			ctrl.disjunctTrans(rho1);
			rho1.free();
			ZnVarNextJ.free();
		}

		Env.free(collectedZ);
		tr12.free();

		return ctrl;
	}

	/**
	 * find maximal size of array in second dimension
	 * 
	 * @param bdds
	 * @return
	 */
	private int maxR(BDD[][] bdds) {
		int maxr = 0;
		for (int i = 0; i < bdds.length; i++) {
			if (bdds[i].length > maxr) {
				maxr = bdds[i].length;
			}
		}
		return maxr;
	}

}
