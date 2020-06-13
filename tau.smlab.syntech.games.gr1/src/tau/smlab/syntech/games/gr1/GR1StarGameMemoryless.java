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
import tau.smlab.syntech.gamemodel.SFAModuleConstraint;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

/**
 * 
 * Realizability checking for GR(1)* specifications, namely GR(1) specifications that additionally may have existential guarantees with regular expressions.
 * 
 * @author Or Pistiner
 *
 */
public class GR1StarGameMemoryless extends GR1Game {

	public GR1StarGameMemoryless(GameModel model) {
		super(model);
	}
	
	@Override
	public boolean checkRealizability() {
		BDD z = Env.TRUE(), nextZ, V;
		FixPoint iterZ;

		V = model.getSys().hasExistReqs() ? computeJusticeAssumptionViolation() : Env.FALSE();

		for (iterZ = new FixPoint(true); iterZ.advance(z);) {
			nextZ = Env.TRUE();
			for (int k = 0; k < model.getSys().existReqNum(); k++) {
				if(model.getSys().existReqHasRegExp(k)) {
					nextZ.andWith(computeExistWinningStates(z, model.getSys().existReqAt(k).getRegExpSfaConstraint()));
				}
				else {
					nextZ.andWith(computeExistWinningStates(z, model.getSys().existReqAt(k).getExistFinallyAssrts()));
				}
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
	 * 
	 * Existential guarantees of regular expressions.
	 * 
	 * @param z
	 * @param regExpSfaConstraint
	 * @return
	 */
	private BDD computeExistWinningStates(BDD z, SFAModuleConstraint regExpSfaConstraint) {
		//1. Compute the set of all pairs (q,s) where q is an sfa state in regExpSfaConstraint and s is a state in Z,
		//and (q,s) is an sfa transition that goes into a final state of regExpSfaConstraint. Thus, s is the last character of a word in the language of regExpSfaConstraint.

		BDD lengthOneSfaWordSuffix = regExpSfaConstraint.getTransToAcceptance().and(z);
		
		//2. Using fixed point iteration, compute the set of all pairs (q,s) such that the two player modules,
		//provided that they start playing from s while respecting their possible transitions,
		//can choose together a finite sequence of states (i.e., inputs and outputs) 's...' that results in a suffix of a word in the language of regExpSfaConstraint.
		
		BDD lengthKSfaWordSuffix = Env.FALSE();
		for (FixPoint lfpIter = new FixPoint(true); lfpIter.advance(lengthKSfaWordSuffix);) {
			lengthKSfaWordSuffix = lengthOneSfaWordSuffix.or(z.id().andWith(env.pred(sys, lengthKSfaWordSuffix, regExpSfaConstraint)));
		}
		lengthOneSfaWordSuffix.free();
		
		//3. Compute the set of all states s in Z from which the two player modules can choose together
		//a finite sequence of states 's...' that is a word in the language of regExpSfaConstraint.
		BDD zStatesStartingSfaWords = lengthKSfaWordSuffix.relprod(regExpSfaConstraint.getIni(), regExpSfaConstraint.getStatesVar().getDomain().set());
		lengthKSfaWordSuffix.free();
		
		//4. Compute the set of all states t in Z from which the two player modules can choose together
		//a finite sequence of states 't...s...' where 's...' is a word in the language of regExpSfaConstraint.
		//That is, 't...s...' is a word in TRUE* L(regExpSfaConstraint).
		
		BDD zStatesStartingTrueStarSfaWords = Env.FALSE();
		for (FixPoint lfpIter = new FixPoint(true); lfpIter.advance(zStatesStartingTrueStarSfaWords);) {
			zStatesStartingTrueStarSfaWords = zStatesStartingSfaWords.or(z.id().andWith(env.pred(sys, zStatesStartingTrueStarSfaWords)));
		}
		zStatesStartingSfaWords.free();
		
		return zStatesStartingTrueStarSfaWords;
		
	}
	
	/**
	 * 
	 * Existential guarantees of nested Finally (F) operators (as presented in FM'19).
	 * 
	 * @param z
	 * @param assertions
	 * @return
	 */
	private BDD computeExistWinningStates(BDD z, List<BDD> assertions) {
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
