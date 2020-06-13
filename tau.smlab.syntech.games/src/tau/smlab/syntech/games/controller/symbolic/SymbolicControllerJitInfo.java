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
 * BDDs needed to store information to run the Just-in-time symbolic controller
 * 
 * @author Ilia Shevrin
 *
 */
public class SymbolicControllerJitInfo {

	private BDD fixpoints = Env.FALSE();
	private BDD safeties = Env.FALSE();
	private BDD justices = Env.FALSE();
	private int[] ranks;

	public SymbolicControllerJitInfo(BDD fixpoints, BDD safeties, BDD justices, int[] ranks) {
		this.fixpoints = fixpoints;
		this.safeties = safeties;
		this.justices = justices;
		this.ranks = ranks;
	}

	public BDD fixpoints() {
		return fixpoints;
	}
	
	public BDD safeties() {
		return safeties;
	}

	public BDD justices() {
		return justices;
	}
	
	public int ranks(int j) {
		return ranks[j];
	}

	@Override
	public String toString() {
		String ret = "Fixpoints:\n";
		ret += Env.toNiceString(fixpoints);
		ret += "\nSafeties:\n";
		ret += Env.toNiceString(safeties);
		ret += "\nJustices:\n";
		ret += Env.toNiceString(justices);
		return ret;
	}

	public void free() {
		fixpoints.free();
		safeties.free();
		justices.free();
	}
}
