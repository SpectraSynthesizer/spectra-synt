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

package tau.smlab.syntech.games.controller.symbolic;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;

/**
 * characterization of a set of possible controllers
 * 
 * default: initial = FALSE and trans = FALSE
 * 
 * @author ringert
 * 
 */
public class SymbolicController {

	private BDD initial = Env.FALSE();
	private BDD trans = Env.FALSE();

	public SymbolicController() {
	}

	/**
	 * creates a new controller from given BDDs (consumed)
	 * 
	 * @param ini   (consumed)
	 * @param trans (consumed)
	 */
	public SymbolicController(BDD ini, BDD trans) {
		this.initial = ini;
		this.trans = trans;
	}

	/**
	 * 
	 * @return direct reference to BDD that describes inital states
	 */
	public BDD initial() {
		return initial;
	}

	/**
	 * sets the inital states
	 * 
	 * @param init is used (don't modify or free)
	 */
	public void setInit(BDD init) {
		this.initial = init;
	}

	/**
	 * 
	 * @return direct reference to BDD that describes transitions
	 */
	public BDD trans() {
		return trans;
	}

	/**
	 * sets the transitions
	 * 
	 * @param trans is used (don't modify or free)
	 */
	public void setTrans(BDD trans) {
		this.trans = trans;
	}

	/**
	 * adds moreTrans as a disjunction to current transitions
	 * 
	 * @param moreTrans (BDD not freed)
	 */
	public void disjunctTrans(BDD moreTrans) {
		trans.orWith(moreTrans.id());
	}

	public void disjunctTransWith(BDD moreTrans) {
		trans.orWith(moreTrans);
	}

	/**
	 * restricts trans of controller to trans2
	 * 
	 * @param moreTrans (BDD not freed)
	 */
	public void conjunctTrans(BDD trans2) {
		trans.andWith(trans2.id());
	}

	public void conjunctTransWith(BDD trans2) {
		trans.andWith(trans2);
	}

	/**
	 * <p>
	 * This procedure return all states which the controller can reach in a single
	 * step from given a set of state.
	 * </p>
	 * 
	 * @param from The set of state to start from.
	 * @return The set of states which the controller can reach in a single step
	 *         from the given states.
	 */
	public BDD succ(BDD from) {
		return Env.succ(from, trans);
	}

	@Override
	public String toString() {
		String ret = "Initial states:\n";
		ret += Env.toNiceString(initial);
		ret += "\n\nTransitions:\n";
		ret += Env.toNiceString(trans);
		return ret;
	}

	public void free() {
		initial.free();
		trans.free();
	}
}
