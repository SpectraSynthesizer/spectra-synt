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

public class GR1SeparatedGameImplC extends GR1GameImplC{

	public GR1SeparatedGameImplC(GameModel model) {
		super(model);
		// TODO Auto-generated constructor stub
	}
	
	@Override
	public boolean checkRealizability() {
		System.out.println("GR1SeparatedGameImplC.checkRealizability: start");


		boolean isRealizble = (Env.TRUE().getFactory()).gr1SeparatedGame(getSysJArray(), getEnvJArray(), sys.initial(),
				env.initial(), sys.trans(), env.trans(), sys.moduleUnprimeVars(), env.moduleUnprimeVars(),
				sys.modulePrimeVars(), env.modulePrimeVars(), Env.allCouplesPairing(), sys.getPartTransArray(),
				env.getPartTransArray(), sys.getQuantSetArray(), env.getQuantSetArray(), DETECT_FIX_POINT_EARLY,
				STOP_WHEN_INITIALS_LOST, USE_FIXPOINT_RECYCLE, SIMULTANEOUS_CONJUNCTION_ABSTRACTION);


		if (STOP_WHEN_INITIALS_LOST && !isRealizble) {
			System.out.println("GR1SeparatedGameImplC.checkRealizability: end");
			System.out.println("Nodes num: " + Env.TRUE().getFactory().getNodeNum());
			System.out.println("Nodes table size: " + Env.TRUE().getFactory().getNodeTableSize());
			mem.setWin(Env.FALSE());
			return false;
		}

		setGameMemFromC();

		System.out.println("GR1SeparatedGameImplC.checkRealizability: end");
		System.out.println("Nodes num: " + Env.TRUE().getFactory().getNodeNum());
		System.out.println("Nodes table size: " + Env.TRUE().getFactory().getNodeTableSize());

		mem.setWin(mem.z_mem[sys.justiceNum() - 1]);
		mem.setComplete(true);
		return isRealizble;
	}

}
