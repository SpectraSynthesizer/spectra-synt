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

package tau.smlab.syntech.gamemodel;

import java.util.List;

import net.sf.javabdd.BDD;

public class BehaviorInfo {

	public BehaviorInfo() {
	}

	public BehaviorInfo(BDD initial, BDD safety, BDD justice, List<BDD> existentialFAssrts, SFAModuleConstraint existentialRegExp, int traceId, boolean aux) {
		this.initial = initial;
		this.safety = safety;
		this.justice = justice;
		this.existentialFAssrts = existentialFAssrts;
		this.existentialRegExp = existentialRegExp;
		this.traceId = traceId;
		this.aux = aux;
	}
	
	public SFAModuleConstraint existentialRegExp;
	public BDD initial;
	public BDD safety;
	public BDD justice;
	public List<BDD> existentialFAssrts;
	public int traceId;
	public boolean aux;

	public boolean isJustice() {
		return justice != null;
	}

	public boolean isExistential() {
		return this.existentialFAssrts != null || this.existentialRegExp != null;
	}

	public boolean isSafety() {
		return safety != null;
	}

	public boolean isInitial() {
		return initial != null;
	}

	@Override
	public String toString() {
		String s = isInitial() ? "ini" : "err";
		s = isJustice() ? "justice" : s;
		s = isSafety() ? "safety" : s;
		s = isExistential() ? "existential" : s;
		return "TraceId (" + s + ") = " + traceId;
	}

	public void free() {
		if (isInitial()) {
			initial.free();
		}
		if (isSafety()) {
			safety.free();
		}
		if (isJustice()) {
			justice.free();
		}
		if(isExistential()) {
			if(this.existentialFAssrts != null) {
				for(BDD exBdd : this.existentialFAssrts) {
					if(!exBdd.isFree()) {
						exBdd.free();
					}
				}
			}
			else { //this.existentialRegExp != null
				this.existentialRegExp.free();
			}
		}
	}

}
