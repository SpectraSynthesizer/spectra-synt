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
public class SymbolicControllerExistentialJitInfo extends SymbolicControllerJitInfo {

	private BDD fulfill = Env.FALSE();
	private BDD towards = Env.FALSE();
	private BDD envViolation = Env.FALSE();
	private int[] fulfillRanks;
	private int[] towardsRanks;
	private int envViolationRank;

	public SymbolicControllerExistentialJitInfo(BDD fixpoints, BDD safeties, BDD justices, int[] ranks,
			BDD fulfill, BDD towards, BDD envViolation, int[] fulfillRanks, int[] towardsRanks, int envViolationRank) {
		super(fixpoints, safeties, justices, ranks);
		this.fulfill = fulfill;
		this.towards = towards;
		this.envViolation = envViolation;
		this.fulfillRanks = fulfillRanks;
		this.towardsRanks = towardsRanks;
		this.envViolationRank = envViolationRank;
	}

	public BDD fulfill() {
		return fulfill;
	}
	
	public BDD towards() {
		return towards;
	}

	public BDD envViolation() {
		return envViolation;
	}
	
	public int fulfillRanks(int j) {
		return fulfillRanks[j];
	}
	
	public int towardsRanks(int j) {
		return towardsRanks[j];
	}
	
	public int envViolationRank() {
		return envViolationRank;
	}

	@Override
	public String toString() {
		String ret = super.toString();
		ret += "\nFulfill:\n";
		ret += Env.toNiceString(fulfill);
		ret += "\nTowards:\n";
		ret += Env.toNiceString(towards);
		ret += "\nEnv Violation:\n";
		ret += Env.toNiceString(envViolation);
		return ret;
	}

	public void free() {
		super.free();
		fulfill.free();
		towards.free();
		envViolation.free();
	}
}
