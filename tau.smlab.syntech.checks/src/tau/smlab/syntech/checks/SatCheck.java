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

package tau.smlab.syntech.checks;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

/**
 * checks satisfiability of various constructs
 *
 */
public class SatCheck {

  /**
   * 
   * Checks if a Player module is consistent (has any valid traces)
   * 
   * This is an implementation of a generalized Buchi model checking. <br>
   * 
   * <li>implements a check whether we reached a fixed point early</li>
   * <li>computes all winning states</li>
   * 
   * @param m
   *          the player module to check
   * @return true iff there exists a trace that satisfies the player module
   */
	public static boolean checkSat(PlayerModule m) {
		if (m.trans().isZero() || m.initial().isZero()) {
			return false;
		}
		// if there is no justice add one in an ad-hoc way, to avoid a possible false positive
		boolean noJust = m.justiceNum()==0;
		if (noJust) {
			m.addJustice(Env.TRUE());
		}
		BDD Z = Env.TRUE();
		// don't free on advance because we update for every justice
		int iterationsWithoutLoss = 0;
		while (iterationsWithoutLoss < m.justiceNum()) {
			for (int i = 0; i < m.justiceNum(); i++) {
				BDD justAndToWin = m.justiceAt(i).id().andWith(m.pred(Z));
				BDD X = justAndToWin.id(); // instead of starting from FALSE we start somewhere in the middle
				FixPoint fX = new FixPoint(true);
				while (fX.advance(X)) {
					X = justAndToWin.id().orWith(m.pred(X));
				}
				if (X.equals(Z)) {
					iterationsWithoutLoss++;
				} else {
					iterationsWithoutLoss = 0;
				}
				Z.free();
				justAndToWin.free();
				Z = X;
			}
		}
		if (noJust) {
			m.resetJustice();
		}
		BDD winIni = Z.and(m.initial());
		Z.free();
		boolean sat = !winIni.isZero();
		winIni.free();
		return sat;
	}

  /**
   * determines whether "lhs => rhs"
   * 
   * result if false if the BDDs are incomparable or rhs => lhs
   * 
   * @param lhs
   * @param rhs
   * @return true iff lhs => rhs
   */
  public static boolean implies(BDD lhs, BDD rhs) {
    BDD imp = lhs.imp(rhs);
    boolean implies = imp.isOne();
    imp.free();
    return implies;
  }

  /**
   * determines whether "lhs => rhs" with respect to domain constraints of player modules
   * 
   * result if false if the BDDs are incomparable or rhs => lhs
   * 
   * @param lhs
   * @param rhs
   * @param doms domain restrictions of variables used in lhs 
   * @return true iff lhs => rhs wrt. doms
   */
  public static boolean impliesWithDoms(BDD lhs, BDD rhs, BDD doms) {
    BDD imp = doms.id().impWith(lhs.imp(rhs));
    boolean implies = imp.isOne();
    imp.free();
    return implies;
  }

  
}
