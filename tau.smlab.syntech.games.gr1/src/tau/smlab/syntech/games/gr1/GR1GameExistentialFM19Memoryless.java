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

import java.util.List;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

/**
 * 
 * Realizability checking for GR(1)* specifications as presented in FM'19.
 * 
 * 
 * @author Gal Amram
 * @author Or Pistiner
 * 
 */
public class GR1GameExistentialFM19Memoryless extends GR1Game {

	public GR1GameExistentialFM19Memoryless(GameModel model) {
		super(model);
	}

	@Override
	public boolean checkRealizability() {
		BDD z = Env.TRUE(), nextZ, V;
		FixPoint iterZ;

		V = (model.getSys().existReqNum() > 0) ? computeJusticeAssumptionViolation() : Env.FALSE();

		for (iterZ = new FixPoint(true); iterZ.advance(z);) {
			nextZ = Env.TRUE();
			for (int k = 0; k < model.getSys().existReqNum(); k++) {
				nextZ.andWith(computeExistentialBdd(z, model.getSys().existReqAt(k).getExistFinallyAssrts()));
			}

			nextZ.andWith(computeJusticeIteration(z));
			nextZ.orWith(V.id());

			z = nextZ;
		}
		mem.setWin(z);
		mem.setComplete(true);
		iterZ.free();
		return sysWinAllInitial(z);
	}

	/**
	 * GR(1) part: justice assumptions and justice guarantees
	 * 
	 * @param z
	 * @return
	 */
	private BDD computeJusticeIteration(BDD z) {
		BDD x, y, zApprox = z.id();
		FixPoint iterY, iterX;
		for (int j = 0; j < sys.justiceNum(); j++) {
			BDD yieldZandJj = sys.justiceAt(j).id().andWith(env.yieldStates(sys, zApprox));
			y = Env.FALSE();
			for (iterY = new FixPoint(true); iterY.advance(y);) {
				BDD start = yieldZandJj.id().orWith(env.yieldStates(sys, y));
				y = Env.FALSE();
				for (int i = 0; i < env.justiceNum(); i++) {
					BDD negp = env.justiceAt(i).not();
					x = zApprox.id();
					for (iterX = new FixPoint(true); iterX.advance(x);) {
						BDD sysCtrl = env.yieldStates(sys, x);
						BDD sysCtrlAndNotJustice = sysCtrl.and(negp);
						sysCtrl.free();
						x = sysCtrlAndNotJustice.or(start);
						sysCtrlAndNotJustice.free();
					}
					y.orWith(x);
					iterX.free();
					negp.free();
				}
				start.free();
			}
			zApprox.free();
			zApprox = y;
			iterY.free();
			yieldZandJj.free();
		}
		return zApprox;
	}

	/**
	 * Existential guarantees
	 * 
	 * @param z
	 * @param assertions
	 * @return
	 */
	private BDD computeExistentialBdd(BDD z, List<BDD> assertions) {
		BDD currAssrt, nextTarget = z.id(), y, oldNextTarget;
		if(assertions != null) { 
			FixPoint iterY;
			for (int i = assertions.size() - 1; i >= 0; i--) {
				currAssrt = assertions.get(i);
				y = Env.FALSE();
				for (iterY = new FixPoint(true); iterY.advance(y);) {
					y = currAssrt.and(nextTarget).orWith(z.id().andWith(env.pred(sys, y)));
				}
				oldNextTarget = nextTarget;
				nextTarget = y;
				oldNextTarget.free();
				iterY.free();
			}
		}
		return nextTarget;
	}

	/**
	 * Justice assumptions violation
	 * 
	 * @return
	 */
	private BDD computeJusticeAssumptionViolation() {
		BDD x, y, cPreY, sysCtrl, sysCtrlAndNotJustice, negJe;
		FixPoint iterY, iterX;
		y = Env.FALSE();
		for (iterY = new FixPoint(true); iterY.advance(y);) {
			cPreY = env.yieldStates(sys, y);
			y = Env.FALSE();
			for (int i = 0; i < env.justiceNum(); i++) {
				negJe = env.justiceAt(i).not();
				x = Env.TRUE();
				for (iterX = new FixPoint(true); iterX.advance(x);) {
					sysCtrl = env.yieldStates(sys, x);
					sysCtrlAndNotJustice = sysCtrl.and(negJe);
					sysCtrl.free();
					x = sysCtrlAndNotJustice.or(cPreY);
					sysCtrlAndNotJustice.free();
				}
				y.orWith(x);
				iterX.free();
				negJe.free();
			}
			cPreY.free();
		}
		iterY.free();
		return y;
	}
}
