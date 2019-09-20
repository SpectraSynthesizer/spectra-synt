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

import java.util.List;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.BehaviorInfo;

public class GR1Inference {

	/**
	 * checks simple inference (implication)
	 * Check that:
	 * 1. ini -> ini
	 * 2. safety -> safety
	 * 3. For every justice of the suffix there is a justice or safety of the prefix that implies it 
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean infer(List<BehaviorInfo> a, List<BehaviorInfo> b ) {
		// compare inits
		BDD ainit = BDDBuilder.getIni(a);
		BDD binit = BDDBuilder.getIni(b);
		ainit.impWith(binit);
		if (!ainit.isOne())
			return false;
		ainit.free();
		
		// compare safeties

		BDD asafe = BDDBuilder.getTrans(a);
		BDD bsafe = BDDBuilder.getTrans(b);
		asafe.impWith(bsafe);
		if (!asafe.isOne())
			return false;
		asafe.free();
		
		// compare just -- all just in b must have an implying one in a
		for (BehaviorInfo ba : b) {
			boolean found = false;
			if (ba.isJustice()) { // must find match in a
				for (BehaviorInfo ref : a) {
					if ((ref.isJustice() && ref.justice.imp(ba.justice).isOne()) 
						|| (ref.isSafety()) && ref.safety.imp(ba.justice).isOne()) {
						found = true;
						break;
					}
				}
				if (!found) {
					return false;
				}
			}
		}
		return true;
	}

}
