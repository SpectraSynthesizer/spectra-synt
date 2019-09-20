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

package tau.smlab.syntech.modelchecker;

import java.util.Vector;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.jtlv.CoreUtil;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

public class FeasibilityChecker {
	
	public void FeasiblityChecker() {}
	
	/**
	 * <p>
	 * Eliminate states from scc which have no successors within scc. scc is
	 * supposed to be a strongly connected component, however, it also contains
	 * chains of states which exit the scc, but are not in it. This procedure
	 * eliminates these chains.
	 * </p>
	 * 
	 * @param scc
	 *            The set of states to find scc for.
	 * @return The scc of the given set of states.
	 */
	private BDD elimSuccChains(PlayerModule module, BDD scc) {
		BDD res = scc.id();
		for (FixPoint iscc = new FixPoint(true /*freeOnAdvance*/); iscc.advance(res);) {
			res = res.and(Env.pred(module.trans(), res));
		}
		return res;
	}
	
	/**
	 * <p>
	 * Get the subset of states which has successor to the other given set of
	 * states.
	 * </p>
	 * 
	 * @param module
	 * 			  The relevant PlayerModule
	 * @param state
	 *            The first set of states.
	 * @param to
	 *            The second set of states.
	 * @return the subset of "state" which has successors to "to".
	 */
	public BDD hasSuccessorsTo(PlayerModule module, BDD state, BDD to) {
		return state.and(Env.prime(to)).and(module.trans()).exist(Env.globalPrimeVars());
	}

	
	
	/**
	 * <p>
	 * This is essentially algorithm "FEASIBLE", from the article: <br>
	 * Yonit Ketsen, Amir Pnueli, Li-on Raviv, Elad Shahar, "Model checking with
	 * strong fairness".
	 * </p>
	 * <p>
	 * The line numbers are the line numbers of that algorithm. Read the article
	 * for further details.
	 * </p>
	 * 
	 * 
	 * @return Returns a set of states which induce a graph which contains all
	 *         the fair SCS's reachable from an initial state.
	 */
	public BDD check(PlayerModule module) {
		// saving the previous restriction state.
		BDD trans = module.trans().id();

		// Line 2
		BDD res = Env.allSucc(module.initial().id(), module.trans().id());
		BDD reachable = res.id();

		// Determine whether fairness conditions refer to primed variables.
		// If so, they are treated differently.
		boolean[] has_primed_j = new boolean[module.justiceNum()];
		BDDVarSet primes = Env.globalPrimeVars();
		for (int i = 0; i < has_primed_j.length; i++) {
			has_primed_j[i] = !module.justiceAt(i).biimp(
					module.justiceAt(i).exist(primes)).isUniverse();
		}

		// Line 3
		module.conjunctTrans(res.id());

		// Lines 4-11
		for (FixPoint ires = new FixPoint(true /*freeOnAdvance*/); ires.advance(res);) {
			// Lines 6-7
			// Eliminate states from res which have no successors within res.
			res = elimSuccChains(module, res);
			module.conjunctTrans(res.id());

			// Lines 8-9
			// Ensure fulfillment of justice requirements.
			// Remove from "res" all states which are not R^*-successors
			// of some justice state.
			for (int i = module.justiceNum() - 1; i >= 0; i--) {
				BDD localj = has_primed_j[i] ? hasSuccessorsTo(module, module.justiceAt(i), res) : module.justiceAt(i);
				res = Env.allPred(module.trans().id(), res.and(localj));
				module.conjunctTrans(res.id());
			}
		}
		module.resetTrans();
		module.conjunctTrans(trans.id());

		// Line 12
		module.conjunctTrans(reachable);

		res = Env.allPred(module.trans().id(), res);
		module.resetTrans();
		module.conjunctTrans(trans.id());

		// Line 13
		return res;
	}
	
	/**
	 * <p>
	 * Compute the shortest path from source to destination. <br>
	 * Algorithm from: Yonit Kesten, Amir Pnueli, Li-on Raviv, Elad Shahar,
	 * "Model Checking with Strong Fairness".
	 * </p>
	 * 
	 * @param source
	 *            The states to start look for path.
	 * @param dest
	 *            The states to reach in the path.
	 * @return An array representing a path, null if the algorithm could not
	 *         find a path.
	 */
	public BDD[] shortestPath(PlayerModule module, BDD source, BDD dest) {
		BDD statesPassed = Env.FALSE(), last = dest.id();
		Vector<BDD> blurredPath = new Vector<BDD>();
		// blurredPath.add(last);

		while (last.and(source).isZero()) { // found source
			blurredPath.add(last);
			statesPassed = statesPassed.id().or(last);
			last = Env.pred(module.trans().id(), last).and(statesPassed.not());
			if (last.isZero()) // failed to find a path.
				return null;
		}
		blurredPath.add(last);

		BDD[] res = new BDD[blurredPath.size()];

		BDD blurredSt = blurredPath.elementAt(blurredPath.size() - 1);
		res[0] = CoreUtil.satOne(source.and(blurredSt), module.moduleUnprimeVars());
		for (int i = 1; i < res.length; i++) {
			blurredSt = blurredPath.elementAt(blurredPath.size() - i - 1);
			res[i] = CoreUtil.satOne(Env.succ(res[i - 1], module.trans().id()).and(blurredSt),
					module.moduleUnprimeVars());
		}

		return res;
	}

	
}
