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

package tau.smlab.syntech.games.controller.jits;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Map;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;

public class ExistentialJitController extends AbstractJitController {
	
	@Override
	public void free() {
		jitController.free();
		Env.free(fulfill);
		Env.free(towards);
		safeZone.free();
	}

	private BDD[][] fulfill;
	private BDD[][] towards;
	private BDD safeZone;
	
	private BDD[][] envViolationX;
	private BDD[] envViolationY;
	private int envViolationRank;
	
	private boolean envViolationPhase = false;
	private boolean exphase = false;
	
	private int exn;
	private int exjx = 0;
	private int towardsRank;
	private int fulfillRank;

	public ExistentialJitController(JitController jitController) {
		super(jitController);
	}
	
	private void loadFulfill(BDD fulfill, int[] fulfill_ranks, int exn) {

		BDD[][] F = new BDD[exn][];
		
		for (int exj = 0; exj < exn; exj++) {
			F[exj] = new BDD[fulfill_ranks[exj]];
			for (int r = 0; r < fulfill_ranks[exj]; r++) {
				
				BDD temp = Env.getVar("util_exJn").getDomain().ithVar(exj);
				temp.andWith(Env.getVar("util_Fn").getDomain().ithVar(r));
				
				BDD fulfillBDD = fulfill.restrict(temp);
				F[exj][r] = Env.prime(fulfillBDD);
				fulfillBDD.free();
				temp.free();
			}
		}
		
		this.fulfill = F;
	}
	
	private void loadTowards(BDD towards, int[] towards_ranks, int exn) {

		BDD[][] T = new BDD[exn][];
		
		for (int exj = 0; exj < exn; exj++) {
			T[exj] = new BDD[towards_ranks[exj]];
			for (int r = 0; r < towards_ranks[exj]; r++) {
				
				BDD temp = Env.getVar("util_exJn").getDomain().ithVar(exj);
				temp.andWith(Env.getVar("util_Tn").getDomain().ithVar(r));
				
				BDD towardsBDD = towards.restrict(temp);
				T[exj][r] = Env.prime(towardsBDD);
				towardsBDD.free();
				temp.free();
			}
		}
		
		this.towards = T;
		
		safeZone = Env.TRUE();
		for (int exj = 0; exj < exn; exj++) {
			safeZone.andWith(this.towards[exj][towardsRank(exj) - 1].id());
		}
	}
	
	private void loadEnvViolation(BDD envViolation, int m, int rank) {

		BDD[][] EV = new BDD[m][];
		BDD[] EVR = new BDD[rank];
		
		for (int i = 0; i < m; i++) {
			EV[i] = new BDD[rank];
			for (int r = 0; r < rank; r++) {
				
				BDD temp = Env.getVar("util_vIn").getDomain().ithVar(i);
				temp.andWith(Env.getVar("util_vRn").getDomain().ithVar(r));
				
				BDD envViolationBDD = envViolation.restrict(temp);
				EV[i][r] = Env.prime(envViolationBDD);
				envViolationBDD.free();
				temp.free();
			}
		}
		
		for (int r = 0; r < rank; r++) {
			EVR[r] = Env.FALSE();
			for (int i = 0; i < m; i++) {
				EVR[r].orWith(EV[i][r].id());
			}
		}
		
		this.envViolationX = EV;
		this.envViolationY = EVR;
		this.envViolationRank = rank - 1;
	}

	@Override
	public void load(String folder, String name, Map<String, String[]> sysVars, Map<String, String[]> envVars) {
		jitController.load(folder, name, sysVars, envVars);
		
		try {
			
			String prefix;
			if (name == null) {
				prefix = folder + File.separator;
			} else {
				prefix = folder + File.separator + name + ".";
			}
			
			BufferedReader sizesReader = new BufferedReader(new FileReader(prefix + "existential_sizes"));
			
			exn = Integer.parseInt(sizesReader.readLine());
			int[] fulfill_ranks = new int[exn];
			int[] towards_ranks = new int[exn];
			int env_violation_rank;
			for (int f = 0; f < exn; f++) {
				fulfill_ranks[f] = Integer.parseInt(sizesReader.readLine());
			}
			for (int t = 0; t < exn; t++) {
				towards_ranks[t] = Integer.parseInt(sizesReader.readLine());
			}
			env_violation_rank = Integer.parseInt(sizesReader.readLine());
			sizesReader.close();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Read Existential Sizes");

			
			// Extract fulfill BDD
			
			BDD fulfill = Env.loadBDD(prefix + "fulfill.bdd");
			loadFulfill(fulfill, fulfill_ranks, exn);	
			fulfill.free();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Load Fulfill BDD");
			
			
			// Extract towards BDD
			
			BDD towards = Env.loadBDD(prefix + "towards.bdd");
			loadTowards(towards, towards_ranks, exn);
			towards.free();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Load Towards BDD");
			
			
			// Extract environment violation BDD

			BDD envViolation = Env.loadBDD(prefix + "envviolation.bdd");
			loadEnvViolation(envViolation, jitController.getJusticeAsm().size(), env_violation_rank);
			envViolation.free();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Load Env Violation BDD");
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		Env.deleteVar("util_exJn");
		Env.deleteVar("util_Fn");
		Env.deleteVar("util_Tn");
		Env.deleteVar("util_vIn");
		Env.deleteVar("util_vRn");
	}
	
	private int fulfillRank(int exj) {
		return fulfill[exj].length;
	}
	
	private int towardsRank(int exj) {
		return towards[exj].length;
	}
	
	private BDD nextStatesEnvViolation(BDD currStatePrimed, BDD currAndTrans) {
		
		BDD temp;
		BDD nextStates = Env.FALSE();
		
		// Find lowest rank
		BDD candidate = Env.FALSE();
		if (envViolationRank > 0) {
			candidate = currAndTrans.and(envViolationY[envViolationRank-1]);
		}
		
		if (candidate.isZero()) {
			
			for (int i = 0; i < getJusticeAsm().size(); i++) {
				temp = envViolationX[i][envViolationRank].and(currStatePrimed);
				
				if (!temp.isZero()) {
					temp.free();
					nextStates = currAndTrans.and(envViolationX[i][envViolationRank]);
					break;
				}
				
				temp.free();
			}
			
		} else {
			
			// It is guaranteed to stop on some k because Y[jx][rank] = Z
			for (int r = 0; r < envViolationY.length; r++) {
				temp = currAndTrans.and(envViolationY[r]);
				
				if (!temp.isZero()) {
					nextStates = temp;
					envViolationRank = r;
					break;
				}
				
				temp.free();
			}
		}
		
		candidate.free();
		
		return nextStates;
	}
	
	@Override
	public BDD next(BDD currentState, BDD inputs) {
		
		BDD nextStates = Env.FALSE();
		
		// Phase 2
		if (exphase) {
			
			System.out.println(String.format("phase 2 - exjx %d, towards %d, fulfill %d", exjx, towardsRank, fulfillRank));
			
			BDD currAndTrans = getJitContext().getTrans().id();
			currAndTrans.andWith(currentState.id());
			currAndTrans.andWith(Env.prime(inputs));
			
			if (towardsRank == 0 && fulfillRank == 0) {
				
				exjx = (exjx + 1) % exn;
				
				exphase = false;
				nextStates = prepareReturnToPhaseOne(currAndTrans);
			
			} else if (towardsRank == 0 && fulfillRank > 0) {
				nextStates = currAndTrans.and(fulfill[exjx][fulfillRank - 1]);
				fulfillRank--;
				
				// Asssume nextStates is FALSE here, move to the next existential guarantee
				if (nextStates.isZero()) {
					
					System.out.println(String.format("phase 2 - could not satisfy existential gar %s, move to next", exjx));

					exjx = (exjx + 1) % exn;
					
					exphase = false;
					nextStates = prepareReturnToPhaseOne(currAndTrans);
				
				}
				
			} else {
				nextStates = getLowestTowards(currAndTrans);
			}
			
			currAndTrans.free();
		    BDD primedNextStates = nextStates.exist(Env.globalUnprimeVars());
		    nextStates.free();
		    nextStates = Env.unprime(primedNextStates);
		    primedNextStates.free();
		    
		    return nextStates;
		
		} else { // Phase 1
			
			// Check if there is at least one ex. gar. that cannot be reached from current state. This means that it will never be reached and
			// the system cannot satisfy its guarantee GEF(regex). The only way to win that is left for the system is to force environment
			// to violate assumptions
			BDD currStatePrimed = Env.prime(currentState);
			if (getEnvViolationPhase(currStatePrimed)) {
				
				System.out.println("phase 1 - in env violation phase");
				BDD currAndTrans = getJitContext().getTrans().id();
				currAndTrans.andWith(currentState.id());
				currAndTrans.andWith(Env.prime(inputs));
				nextStates = nextStatesEnvViolation(currStatePrimed, currAndTrans);
				
				currStatePrimed.free();
				currAndTrans.free();
			    BDD primedNextStates = nextStates.exist(Env.globalUnprimeVars());
			    nextStates.free();
			    nextStates = Env.unprime(primedNextStates);
			    primedNextStates.free();
			    return nextStates;
			}
			
			currStatePrimed.free();
			
			int oldJx = jitController.getJitState().getJx();
			BDD nextStatesOriginal = jitController.next(currentState, inputs);
			int newJx = jitController.getJitState().getJx();
			
			System.out.println(String.format("phase 1 - old jx %d, new jx %d", oldJx, newJx));
			
			// Justice guarantees cycle is satisfied
			if (newJx < oldJx) {
				
				// Maybe switching phase, recalculating next states
				
				BDD currAndTrans = getJitContext().getTrans().id();
				currAndTrans.andWith(currentState.id());
				currAndTrans.andWith(Env.prime(inputs));
			
				exphase = true;
				fulfillRank = fulfillRank(exjx) - 1;
				nextStates = getLowestTowards(currAndTrans);

				
				currAndTrans.free();
			    BDD primedNextStates = nextStates.exist(Env.globalUnprimeVars());
			    nextStates.free();
			    nextStates = Env.unprime(primedNextStates);
			    primedNextStates.free();
			    return nextStates;
				
			} else {
				return nextStatesOriginal;
			}
		}
	}
	
	private boolean getEnvViolationPhase(BDD currentState) {
		if (envViolationPhase) return true;
		BDD temp = currentState.and(safeZone);
		if (temp.isZero()) { // Not in safe zone
			envViolationPhase = true;
		}
		temp.free();
		return envViolationPhase;
	}
	
	private BDD prepareReturnToPhaseOne(BDD currAndTrans) {
		
		BDD temp;
		BDD next = Env.FALSE();
		for (int r = 0; r < jitController.getJitContext().rank(0); r++) {
			temp = currAndTrans.and(jitController.getJitContext().Y(0, r));
			
			if (!temp.isZero()) {
				next = temp;
				jitController.getJitState().setRank(r);
				break;
			}
			
			temp.free();
		}
		
		return next;
	}
	
	private BDD getLowestTowards(BDD currAndTrans) {
		
		BDD temp;
		BDD lowest = Env.FALSE();
		for (int r = 0; r < towardsRank(exjx); r++) {
			temp = currAndTrans.and(towards[exjx][r]);
			
			if (!temp.isZero()) {
				lowest = temp;
				towardsRank = r;
				break;
			}
			
			temp.free();
		}
		
		return lowest;
	}
	
	@Override
	public void saveState() {
		this.controller.saveState();
	}

	@Override
	public void loadState() {
		this.controller.loadState();
	}

	@Override
	public BDD succ(BDD from) {
		return jitController.succ(from);
	}

	@Override
	public BDD pred(BDD to) {
		return jitController.pred(to);
	}
}
