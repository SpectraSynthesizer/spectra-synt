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

import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.jtlv.Env;

public class GR1GameBuilder {

	public static GR1Game getDefault(GameModel model, int energyBound) {
		boolean hasADD = Env.getBDDPackageInfo().contains("CUDDADDFactory");
		if (hasADD) {
			return new GR1GameEnergyADD(model, energyBound);
		} else {
			// in this case BDDGenerator took care of reduction already; we use regular GR1
			return getDefault(model);
		}
	}

	public static GR1Game getDefault(GameModel model) {
		// are we using pure Java? (this check also works correctly if the user chose a
		// native CUDD but Java is as fallback)
		boolean hasJNI = !Env.getBDDPackageInfo().contains("JTLVJavaFactory");

		// use GR(1)* synthesis
		if (model.getSys().existReqNum() > 0) {
			if (hasJNI) {
				return new GR1StarGameImplC(model);
			} else {
				new GR1StarGameMemoryless(model);
			}
		}
		if (hasJNI) {
			return new GR1GameImplC(model);
		}

		return new GR1GameExperiments(model);
	}

	/**
	 * Enable/disable optimization (if true, not all winning states are calculated!) 
	 * 
	 * TODO the current way this works is very ugly refactor this and make it not
	 * static all over
	 * 
	 * @param b
	 */
	public static void stopWhenInitialsLost(boolean b) {
		GR1GameExperiments.STOP_WHEN_INITIALS_LOST = b;
	}
}
