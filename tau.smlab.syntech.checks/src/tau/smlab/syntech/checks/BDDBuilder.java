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

/**
 * Compute and keep popular BDDs for reuse
 * 
 * @author shalom
 *
 */

public class BDDBuilder {
	private static BDD keptWinRegion = null;
	private static BDD keptTrans = null;
	private static BDD keptIni = null;
	private static List<BehaviorInfo> justBehaviors = null;	
	private static List<BehaviorInfo> transBehaviors = null;
	private static List<BehaviorInfo> iniBehaviors = null;
	private static boolean keepBDDs = false;
	
	private enum bType {INI, TRANS, JUST};
	
	/**
	 * Set behaviors and activate the keepBDDs option
	 * @param behaviors
	 */
	public static void setBehaviors(List<BehaviorInfo> behaviors) {
		justBehaviors = new ArrayList<BehaviorInfo>();	
		transBehaviors = new ArrayList<BehaviorInfo>();	
		iniBehaviors = new ArrayList<BehaviorInfo>();	

		iniBehaviors = allOfType(behaviors, bType.INI);
		transBehaviors = allOfType(behaviors, bType.TRANS);
		justBehaviors = allOfType(behaviors, bType.JUST);

		keptWinRegion = null;
		keptTrans = null;
		keptIni = null;
		keepBDDs = true;
	}
	
	/**
	 * get a copy of the win region when possible
	 * 
	 * @param behaviors
	 * @return
	 */
	public static BDD getWinRegion(List<BehaviorInfo> behaviors) {
		if (keepBDDs && 
				eq(allOfType(behaviors, bType.TRANS), transBehaviors) && 
				eq(allOfType(behaviors, bType.JUST), justBehaviors)) {
			if (keptWinRegion==null) {
				keptWinRegion = GR1Implication.computeWinRegion(behaviors);
			}
			return keptWinRegion.id();
		} else {
			return GR1Implication.computeWinRegion(behaviors);
		}
	}

	/**
	 * get a copy of the transition relation when possible

	 * @param behaviors
	 * @return
	 */
	public static BDD getTrans(List<BehaviorInfo> behaviors) {
		if (keepBDDs && eq(allOfType(behaviors, bType.TRANS), transBehaviors)) {
			if (keptTrans==null) {
				keptTrans = computeTrans(behaviors);
			}
			return keptTrans.id();
		} else {
			return computeTrans(behaviors);
		}
	}
	
	/**
	 * get a copy of the initial states when possible

	 * @param behaviors
	 * @return
	 */
	public static BDD getIni(List<BehaviorInfo> behaviors) {
		if (keepBDDs && eq(allOfType(behaviors, bType.INI), iniBehaviors)) {
			if (keptIni==null) {
				keptIni = computeIni(behaviors);
			}
			return keptIni.id();
		} else {
			return computeIni(behaviors);
		}
	}
	
	/**
	 * Remove all behaviors and BDDs and lower the keepBDDs flag
	 */
	public static void release() {
		if (keptWinRegion!=null) {
			keptWinRegion.free();
		}
		if (keptTrans!=null) {
			keptTrans.free();
		}
		if (keptIni!=null) {
			keptIni.free();
		}
		keptWinRegion = null;
		keptTrans = null;
		keptIni = null;
		justBehaviors = null;
		transBehaviors = null;
		iniBehaviors = null;
		keepBDDs = false;
	}
	
	private static boolean eq(List<BehaviorInfo> a, List<BehaviorInfo> b) {
		return a.containsAll(b) && b.containsAll(a);
	}
	
	/** 
	 * get the initial states of the prefix
	 * @param pref
	 * @return init states
	 */
	private static BDD computeIni(List<BehaviorInfo> pref) {
		BDD result = Env.TRUE();
		for (BehaviorInfo b : pref) {
			if (b.isInitial()) {
				result.andWith(b.initial.id());
			}
		}
		return clear(result);
	}
	/**
	 * compute transitions of the prefix
	 * @param pref
	 * @return Transition function
	 */
	private static BDD computeTrans(List<BehaviorInfo> pref) {
		BDD result = Env.TRUE();
		for (BehaviorInfo b : pref) {
			if (b.isSafety()) {
				result.andWith(b.safety.id());
			}
		}
		return clear(result);
	}

	private static List<BehaviorInfo> allOfType(List<BehaviorInfo> pref, bType t) {
	    List<BehaviorInfo> all = new ArrayList<BehaviorInfo>();
		for (BehaviorInfo b : pref) {
			if (b.isJustice() && t==bType.JUST) {
				all.add(b);
			}
			if (b.isSafety() && t==bType.TRANS) {
				all.add(b);
			}
			if (b.isInitial() && t==bType.INI) {
				all.add(b);
			}
	    }
		return all;
	}
	
	/**
	 * clear BDD from irrelevant domain values caused by domains which are not a power of 2
	 * 
	 * @param b
	 * @return
	 */
	private static BDD clear(BDD b) {
		for (BDDDomain d : b.support().getDomains()) {
			b.andWith(d.domain());
		}
		return b;
	}
}
