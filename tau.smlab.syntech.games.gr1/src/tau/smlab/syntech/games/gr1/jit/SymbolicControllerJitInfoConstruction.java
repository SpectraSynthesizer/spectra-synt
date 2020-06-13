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

package tau.smlab.syntech.games.gr1.jit;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerExistentialJitInfo;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerJitInfo;
import tau.smlab.syntech.games.gr1.GR1Memory;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

public class SymbolicControllerJitInfoConstruction {

	private GR1Memory mem;
	private PlayerModule sys;
	private PlayerModule env;
	private int maxRank;
	private int maxFulfillRank;
	private int maxTowardsRank;
	private int[] ranks;
	private int[] fulfillRanks;
	private int[] towardsRanks;
	private BDD minWinCred;

	public SymbolicControllerJitInfoConstruction(GR1Memory mem, GameModel m, BDD minWinCred) {

		this.mem = mem;
		this.env = m.getEnv();
		this.sys = m.getSys();
		this.ranks = new int[sys.justiceNum()];
		this.fulfillRanks = new int[sys.existReqNum()];
		this.towardsRanks = new int[sys.existReqNum()];

		int maxRank = 0;
		for (int j = 0; j < sys.justiceNum(); j++) {
			ranks[j] = this.mem.getRank(j);
			if (this.mem.getRank(j) - 1 > maxRank) {
				maxRank = this.mem.getRank(j) - 1;
			}
		}
		
		int maxFulfillRank = 0;
		int maxTowardsRank = 0;
		for (int exj = 0; exj < sys.existReqNum(); exj++) {
			fulfillRanks[exj] = this.mem.getFulfillRank(exj);
			if (this.mem.getFulfillRank(exj) - 1 > maxFulfillRank) {
				maxFulfillRank = this.mem.getFulfillRank(exj) - 1;
			}
			towardsRanks[exj] = this.mem.getTowardsRank(exj);
			if (this.mem.getTowardsRank(exj) - 1 > maxTowardsRank) {
				maxTowardsRank = this.mem.getTowardsRank(exj) - 1;
			}
		}

		this.maxRank = maxRank;
		this.minWinCred = minWinCred;
		this.maxFulfillRank = maxFulfillRank;
		this.maxTowardsRank = maxTowardsRank;
	}

	public SymbolicControllerJitInfo calculateJitSymbollicControllerInfo() {

		ModuleBDDField In = null;
		ModuleBDDField Jn = null;
		ModuleBDDField Rn = null;
		ModuleBDDField util = null;
		
		ModuleBDDField exJn = null; // Existential guarantees
		ModuleBDDField Fn = null; // Fulfill ranks
		ModuleBDDField Tn = null; // Towards ranks
		ModuleBDDField vIn = null; // Env. violation justice assumptions
		ModuleBDDField vRn = null; // Env. violation ranks
		
		System.out.println("Calculating controller info for storage");

		try {
			// TODO: possible collision in names
			In = sys.addVar("util_In", 0, Math.max(env.justiceNum() - 1, 1), true);
			Jn = sys.addVar("util_Jn", 0, Math.max(sys.justiceNum() - 1, 1), true);
			Rn = sys.addVar("util_Rn", 0, Math.max(maxRank, 1), true);
			util = sys.addVar("util_0", true);
			
			if (sys.hasExistReqs()) {
				exJn = sys.addVar("util_exJn", 0, Math.max(sys.existReqNum() - 1, 1), true);
				Fn = sys.addVar("util_Fn", 0, Math.max(maxFulfillRank - 1, 1), true);
				Tn = sys.addVar("util_Tn", 0, Math.max(maxTowardsRank - 1, 1), true);
				vIn = sys.addVar("util_vIn", 0, Math.max(env.justiceNum() - 1, 1), true);
				vRn = sys.addVar("util_vRn", 0, Math.max(maxRank, 1), true);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		// Save fixpoints of controller

//        BDD fixpoints = Env.TRUE();
//        
//        for (int j = 0; j < sys.justiceNum(); j++) {
//            for (int k = 0; k < kvals[j]; k++) {
//            	BDD currRow = Env.TRUE();
//                for (int i = 0; i < env.justiceNum(); i++) {
//
//                	BDD currX = In.getDomain().ithVar(i).and(Jn.getDomain().ithVar(j)).and(Kn.getDomain().ithVar(k));
//                	currX.impWith(mem.getX(j, k, i).id());
//                	
////                	System.out.println(System.currentTimeMillis() + " AFTER PREPARING X " + j + " " + k + " " + i);
//                	
//                	currRow.andWith(currX.id());
//                	currX.free();
//                	
//                	
////                	System.out.println(System.currentTimeMillis() + " AFTER CONJ X " + j + " " + k + " " + i);
//
//                }
//                fixpoints.andWith(currRow.id());
//                currRow.free();
//            }
//        }



		// Save safeties of controller

//        BDD safeties = Env.TRUE();
        
//        BDD initial = sys.initial().and(env.initial());
//        BDD transitions = sys.trans().and(env.trans());
        
//        initial.andWith(minWinCred);
    
        	            
//        BDD sysIni = util.getDomain().ithVar(0).and(Jn.getDomain().ithVar(0)).impWith(sys.initial().and(minWinCred).id());
//        safeties.andWith(sysIni.id());
//        sysIni.free();
//
//        BDD sysTrans = util.getDomain().ithVar(0).and(Jn.getDomain().ithVar(1)).impWith(sys.trans().id());
//        safeties.andWith(sysTrans.id());
//        sysTrans.free();
//        
//        BDD envIni = util.getDomain().ithVar(1).and(In.getDomain().ithVar(0)).impWith(env.initial().id());
//        safeties.andWith(envIni.id());
//        envIni.free();
//
//        BDD envTrans = util.getDomain().ithVar(1).and(In.getDomain().ithVar(1)).impWith(env.trans().id());
//        safeties.andWith(envTrans.id());
//        envTrans.free();
        


		// Save justices of controller

//        BDD justices = Env.TRUE();
//                    
//        for (int j = 0; j < sys.justiceNum(); j++) {
//        	
//        	BDD currGar = (util.getDomain().ithVar(0).and(Jn.getDomain().ithVar(j))).impWith(sys.justiceAt(j).id());
//        	justices.andWith(currGar.id());
//        	currGar.free();
//        }
//        
//        for (int i = 0; i < env.justiceNum(); i++) {
//        	
//        	BDD currAsm = (util.getDomain().ithVar(1).and(In.getDomain().ithVar(i))).impWith(env.justiceAt(i).id());
//        	justices.andWith(currAsm.id());
//        	currAsm.free();
//        }


		
		if (sys.hasExistReqs()) {
			
			BDD fixpoints = (Env.TRUE().getFactory()).getFixpointsStarBDD(Jn.getDomain().vars(), In.getDomain().vars(),
					Rn.getDomain().vars());
			
			System.out.println("GR(1)* Calculated fixpoints BDD");
			
			BDD safeties = (Env.TRUE().getFactory()).getTransBDD(sys.initial().and(minWinCred), env.initial(), sys.trans(), env.trans(),
					Jn.getDomain().vars(), In.getDomain().vars(), util.getDomain().vars()[0]);
			
			System.out.println("GR(1)* Calculated safeties BDD");
			
			BDD justices = (Env.TRUE().getFactory()).getJusticesStarBDD(sys.getJustices(), env.getJustices(),
					Jn.getDomain().vars(), In.getDomain().vars(), util.getDomain().vars()[0]);
			
			System.out.println("GR(1)* Calculated justices * BDD");
			
			BDD fulfill = (Env.TRUE().getFactory()).getFulfillBDD(exJn.getDomain().vars(), Fn.getDomain().vars());
			BDD towards = (Env.TRUE().getFactory()).getTowardsBDD(exJn.getDomain().vars(), Tn.getDomain().vars());
			BDD envViolation = (Env.TRUE().getFactory()).getEnvViolationBDD(vIn.getDomain().vars(), vRn.getDomain().vars());
			
			System.out.println("GR(1)* Calculated fulfill and towards BDDs");
			
			return new SymbolicControllerExistentialJitInfo(fixpoints, safeties, justices, ranks, fulfill, towards, envViolation, fulfillRanks, towardsRanks, this.mem.getEnvViolationRank());
		} else {
			
			BDD fixpoints = (Env.TRUE().getFactory()).getFixpointsBDD(Jn.getDomain().vars(), In.getDomain().vars(),
					Rn.getDomain().vars());
			
			System.out.println("GR(1) Calculated fixpoints BDD");
			
			BDD safeties = (Env.TRUE().getFactory()).getTransBDD(sys.initial().and(minWinCred), env.initial(), sys.trans(), env.trans(),
					Jn.getDomain().vars(), In.getDomain().vars(), util.getDomain().vars()[0]);
			
			System.out.println("GR(1) Calculated safeties BDD");
			
			BDD justices = (Env.TRUE().getFactory()).getJusticesBDD(sys.getJustices(), env.getJustices(),
					Jn.getDomain().vars(), In.getDomain().vars(), util.getDomain().vars()[0]);
			
			System.out.println("GR(1) Calculated justices BDD");
			
			return new SymbolicControllerJitInfo(fixpoints, safeties, justices, ranks);
		}

	}
}
