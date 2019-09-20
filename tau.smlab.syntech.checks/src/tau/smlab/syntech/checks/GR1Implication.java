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

import java.util.ArrayList;
import java.util.List;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDDomain;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

/** 
 * 
 * @author shalom
 *
 * A class for GR(1) implication that uses satisfiability check with F and FG conjuncts
 */

public class GR1Implication {
	
	/**
	 * Imply all suffix behaviors
	 * 
	 * @param pref
	 * @param suff
	 * @return
	 */
	public static boolean imply(List<BehaviorInfo> pref, List<BehaviorInfo> suff) {
		for (BehaviorInfo s: suff) {
			if (!imply(pref, s)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * main method for implication check between a set of BehaviorInfo, and a single one.
	 * 
	 * @param pref
	 * @param suff
	 * @return
	 */
	public static boolean imply(List<BehaviorInfo> pref, BehaviorInfo suff) {
		boolean result = false;
		if (suff.isInitial()) {
			result = checkImpIni(pref, suff.initial);
		}
		if (suff.isSafety()) {
			result = checkImpSafe(pref, suff.safety);
		}
		if (suff.isJustice()) {
			result = checkImpJust(pref, suff.justice);
		}
		return result;
	}
	
	/**
	 * the states in the suffix include all winning states of the prefix
	 * 
	 * @param pref
	 * @param ini
	 * @return implication result for ini
	 */
	private static boolean checkImpIni(List<BehaviorInfo> pref, BDD ini) {
		BDD init = BDDBuilder.getIni(pref);
		BDD reverse = ini.notWithDoms();
		BDD allIni = init.and(reverse);
		init.free();
		reverse.free();
		if (allIni.isZero()) { // no need for win region
			return true;
		}
		BDD win = BDDBuilder.getWinRegion(pref);
		win.andWith(allIni);
		boolean ret = win.isZero();
		win.free();
		return ret;
	}
	
	/**
	 * In order to compute prefix -> safe we try to compute the winning states for prefix and "F (not safe)"
	 * if this not satisfiable the implication holds
	 * for the F we want to reach two winning states that have a "not safe" edge between them
	 * @param pref
	 * @param safe
	 * @return implication result for safety
	 */
	private static boolean checkImpSafe(List<BehaviorInfo> pref, BDD safe) {
		BDD trans = BDDBuilder.getTrans(pref);
		BDD win = BDDBuilder.getWinRegion(pref);
		BDD unsafe = safe.notWithDoms();
		BDD primeWin = Env.prime(win);
		win.andWith(unsafe);
		win.andWith(primeWin);
		win.andWith(trans.id());
		BDD quanWin = win.exist(Env.globalPrimeVars());
		win.free();
		BDD ini = BDDBuilder.getIni(pref);
		boolean ret = !canReach(ini, trans, quanWin);
		quanWin.free();
		ini.free();
		trans.free();
		return ret;
	}
	
	/** 
	 * In order to compute prefix -> just we try to compute the winning states for prefix and "FG (not just)"
	 * if this not satisfiable the implication holds 
	 * for the FG we see if we can reach winning states that keep "not just" so we add it to the transitions
	 * @param pref
	 * @param just
	 * @return
	 */
	private static boolean checkImpJust(List<BehaviorInfo> pref, BDD just) {
		BDD origTrans = BDDBuilder.getTrans(pref);
		BDD notJust = just.notWithDoms();
		BDD trans = origTrans.and(notJust);
		notJust.free();
		BDD winWhileNotJust = genBuchiWin(trans, collectJust(pref));
		trans.free();
		BDD ini = BDDBuilder.getIni(pref);
		boolean result = !canReach(ini, origTrans, winWhileNotJust);
		ini.free();
		winWhileNotJust.free();
		origTrans.free();
		return result;
	}
	
	
	/** compute win region according to a set of safeties and justices
	 * 
	 * @param		pref the set of safeties and justices
	 * @return		Winning region
	 */
	protected static BDD computeWinRegion(List<BehaviorInfo> pref) {
		BDD trans = BDDBuilder.getTrans(pref);
		BDD win = genBuchiWin(trans, collectJust(pref));
		trans.free();
		return win;
	}
	
	/**
	 * Get all justices and make sure there is at least one which is needed for the fp computation
	 * @param pref
	 * @return the justices
	 */
	protected static List<BDD> collectJust(List<BehaviorInfo> pref) {
	    List<BDD> buchis = new ArrayList<BDD>();
		for (BehaviorInfo b : pref) {
			if (b.isJustice()) {
				buchis.add(b.justice);
			}
	    }
		// make sure there is at least one justice
		if (buchis.isEmpty()) {
			buchis.add(Env.TRUE());
		}
		return buchis;
	}

	/**
	 * Get winning region on a generalized buchi
	 * @param trans 		the transitions
	 * @param buchis		the justices
	 * @return 				Winning region
	 */
	
	protected static BDD genBuchiWin(BDD trans, List<BDD> buchis) {
		if (trans.isZero()) { // no win region when the transition relation is empty.
			return Env.FALSE();
		}
	    BDD Z = Env.TRUE();

	    FixPoint zFix = new FixPoint(true);
	    while (zFix.advance(Z)) {
	      Z = Z.id();
	      for (BDD buchi : buchis) {
	        BDD start = buchi.id().andWith(Env.pred(trans, Z));
	        BDD nextZ = Z.id().andWith(CouldAsmHelp.reachBwd(trans, start));
	        Z.free();
	        Z = nextZ;
	        start.free();
	      }
	    }
	    // clear region of domain external values
	    for (BDDDomain d : Z.support().getDomains()) {
	        Z.andWith(d.domain());
	    }
	    return Z;
	}
	
/*	protected static BDD genBuchiWin(BDD trans, List<BDD> buchis) {
	// don't free on advance because we update for every justice
	    BDD Z = Env.TRUE();
		int iterationsWithoutLoss = 0;
		while (iterationsWithoutLoss < buchis.size()) {
			for (BDD buchi : buchis) {
				BDD justAndToWin = buchi.id().andWith(Env.pred(trans, Z));
				BDD X = CouldAsmHelp.reachBwd(trans, justAndToWin); // instead of starting from FALSE we start somewhere in the middle
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
		return Z;
	}*/
	/**
	 * Look for earliest intersection while reaching back in order to save iterations
	 * 
	 * @param toReg			the region to reach backwards to
	 * @param trans 		transition relation
	 * @param fromReg		the region to reach backwards from
	 * @return
	 */
	private static boolean canReach(BDD toReg, BDD trans, BDD fromReg) {
		boolean hasReached = intersect(toReg, fromReg); 
	    BDD attr = Env.FALSE(); // no states
	    FixPoint f = new FixPoint(true);
	    while (!hasReached && f.advance(attr)) {
	      attr = fromReg.id().orWith(Env.pred(trans, attr));
	      hasReached = intersect(toReg, attr);
	    }
	    return hasReached;
	}
	
	private static boolean intersect(BDD a, BDD b) {
		BDD both = a.and(b);
		boolean r = !both.isZero();
		both.free();
		return r;
	}
}
