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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;

/**
 * A class for the execution of symbolic controllers in a Just-in-time fashion
 * @author ilia
 *
 */
public class BasicJitController implements JitController {

	private JitContext jitContext;
	private JitState jitState;
		
	private List<BDD> justiceGar = new ArrayList<>();
	private List<BDD> justiceAsm = new ArrayList<>();
	
	private List<Integer> skipped;
	
	private BDD[] rho1;
	private BDD[][] rho2;
	private BDD[][][] rho3;
	
	private boolean eager;
	
	public BasicJitController(boolean eager, Integer ... skipped) {
		this.eager = eager;
		this.skipped = Arrays.asList(skipped);
	}
	
	public BasicJitController(Integer ... skipped) {
		this(false, skipped);
	}
	
	public boolean isEager() {
		return eager;
	}
	
	
	@Override
	public BDD pred(BDD to) {

		BDD prevStates = Env.FALSE();
		
		BDD toPrimed = Env.prime(to);
		BDD currAndTrans = jitContext.getTrans().id();
		currAndTrans.andWith(toPrimed.id());
		
		// Env deadlock
		if (currAndTrans.isZero()) {
			return prevStates;
		}
		
		
		for (int jx = 0; jx < justiceGar.size(); jx++) {
						
			BDD nextZn = Env.prime(Env.getVar("Zn").getDomain().ithVar(jx));
			
			BDD tempZ = toPrimed.and(nextZn);
			if (tempZ.isZero()) {
				tempZ.free();
				continue;
			}
			
			tempZ.free();
			
			int rank = jitContext.rank(jx);
			int prevJx = (jx-1+justiceGar.size()) % justiceGar.size();

			prevStates.orWith(getTransition(prevJx).and(toPrimed));
			
			for (int r = 0; r < rank; r++) {
				
				BDD tempY = jitContext.Y(jx, r).and(toPrimed);
					
				if (tempY.isZero()) {
					tempY.free();
					continue;
				}
					
				if (r+1 < rank) {
					prevStates.orWith(getTransition(jx, r+1).and(toPrimed));
				}
				
				for (int i = 0; i < justiceAsm.size(); i++) {
									
					BDD tempX = jitContext.X(jx, i, r).and(toPrimed);
					if (!tempX.isZero()) {
						prevStates.orWith(getTransition(jx, r, i).and(toPrimed));
					}
					
					tempX.free();
				}
			}
			
			nextZn.free();
		}
		
		toPrimed.free();
		currAndTrans.free();
		
	    BDD primedNextState = prevStates.exist(Env.globalPrimeVars());
	    prevStates.free();
	    prevStates = primedNextState;
		
		return prevStates;
		
	}
	
	
	@Override
	public BDD succ(BDD from) {
		
		System.out.println("ACTIVE NODE COUNT: " + Env.TRUE().getFactory().getNodeNum());
		System.out.println("TOTAL NODE COUNT: " + Env.TRUE().getFactory().getNodeTableSize());
		
		BDD succStates = Env.FALSE();
		
		BDD currAndTrans = jitContext.getTrans().id();
		currAndTrans.andWith(from.id());
		
		// Env deadlock
		if (currAndTrans.isZero()) {
			return succStates;
		}
		
		BDD fromPrimed = Env.prime(from);
		
		for (int jx = 0; jx < justiceGar.size(); jx++) {
						
			BDD prevZn = Env.getVar("Zn").getDomain().ithVar(jx);
			
			BDD tempZ = from.and(prevZn);
			if (tempZ.isZero()) {
				tempZ.free();
				continue;
			}
			
			tempZ.free();
						
			int rank = jitContext.rank(jx);

			succStates.orWith(getTransition(jx).and(from));
			
			for (int r = 0; r < rank; r++) {
								
				BDD tempY = jitContext.Y(jx, r).and(fromPrimed);
				
				if (tempY.isZero()) {
					tempY.free();
					continue;
				}
					
				if (r > 0) {
					succStates.orWith(getTransition(jx, r).and(from));
				}
				
				for (int i = 0; i < justiceAsm.size(); i++) {
					
					BDD tempX = jitContext.X(jx, i, r).and(fromPrimed);
					if (!tempX.isZero()) {
						succStates.orWith(getTransition(jx, r, i).and(from));
					}
					
					tempX.free();
				}
			}
			
			prevZn.free();
		}

		fromPrimed.free();
		currAndTrans.free();
		
	    BDD primedNextState = succStates.exist(Env.globalUnprimeVars());
	    succStates.free();
	    succStates = Env.unprime(primedNextState);
	    primedNextState.free();

	    return succStates;
	}
	
	
	@Override
	public BDD next(BDD currentState, BDD inputs) {
		
		BDD currAndTrans = jitContext.getTrans().id();
		currAndTrans.andWith(currentState.id());
		currAndTrans.andWith(Env.prime(inputs));
		BDD nextStates = Env.FALSE();
		
		if (jitState.getRank() == 0) {
			
			// We divide between guarantee satisfaction and assumption violation
			// This is in contrast to the original construction where the controller could decide to move to next zn
			// or to stay in same zn and violate some assumption. We want to be more eager so we decide that once
			// the controller can move to next zn it will do it
			
			BDD currAndJustice = justiceGar.get(jitState.getJx()).and(currentState);
			if (!currAndJustice.isZero()) {
				
				//System.out.println("rho1 with j=" + jitState.getJx());
				jitState.updateJx();
				
				// It is guaranteed to stop on some r because Y[j][r_j] = Z
				
				BDD temp;
				for (int r = 0; r < jitContext.rank(jitState.getJx()); r++) {
					temp = currAndTrans.and(jitContext.Y(jitState.getJx(), r));
					
					if (!temp.isZero()) {
						nextStates = temp;
						jitState.setRank(r);
						//System.out.println("rho1 with new j=" + jitState.getJx() + " and next r=" + jitState.getRank());
						break;
					}
					
					temp.free();
				}
				
			} else {
					
				BDD currentStatePrimed = Env.prime(currentState);
				BDD temp;
				
				for (int i = 0; i < justiceAsm.size(); i++) {
					temp = jitContext.X(jitState.getJx(), i, 0).and(currentStatePrimed);
					
					if (!temp.isZero()) {
						temp.free();
						nextStates = currAndTrans.and(jitContext.X(jitState.getJx(), i, 0));
						//System.out.println("rho3 with j=" + jitState.getJx() + " and r=0 and i=" + i);
						break;
					}
					
					temp.free();
				}
				
				currentStatePrimed.free();
			}
			
			currAndJustice.free();

		} else {
			
			// Find lowest rank
			BDD candidate = currAndTrans.and(jitContext.Y(jitState.getJx(), jitState.getRank()-1));
			
			if (candidate.isZero()) {

				BDD currentStatePrimed = Env.prime(currentState);
				BDD temp;
				
				for (int i = 0; i < justiceAsm.size(); i++) {
					temp = jitContext.X(jitState.getJx(), i, jitState.getRank()).and(currentStatePrimed);
					
					if (!temp.isZero()) {
						temp.free();
						nextStates = currAndTrans.and(jitContext.X(jitState.getJx(), i, jitState.getRank()));
						//System.out.println("rho3 with j=" + jitState.getJx() + " and r=" + jitState.getRank() + " and i=" + i);
						break;
					}
					
					temp.free();
				}

				currentStatePrimed.free();	
				
			} else {
				
				// It is guaranteed to stop on some r because Y[j][r_j] = Z
				BDD temp;
				for (int r = 0; r < jitContext.rank(jitState.getJx()); r++) {
					temp = currAndTrans.and(jitContext.Y(jitState.getJx(), r));
					
					if (!temp.isZero()) {
						//System.out.println("rho2 with j=" + jitState.getJx() + " and r=" + jitState.getRank() + " and next r="+ r);
						nextStates = temp;
						jitState.setRank(r);
						break;
					}
					
					temp.free();
				}
			}
			
			candidate.free();
		}

		nextStates.andWith(Env.prime(Env.getVar("Zn").getDomain().ithVar(jitState.getJx())));
		currAndTrans.free();
	    BDD primedNextStates = nextStates.exist(Env.globalUnprimeVars());
	    nextStates.free();
	    nextStates = Env.unprime(primedNextStates);
	    primedNextStates.free();
	    return nextStates;
	}
	
	protected void loadTransAndInitial(BDD trans) {
		
		BDD temp = Env.getVar("util_0").getDomain().ithVar(0);
		temp.andWith(Env.getVar("util_Jn").getDomain().ithVar(0));
		
		temp.andWith(Env.getVar("util_In").getDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Rn").getDomain().ithVar(0));
		temp.andWith(Env.getVar("util_0").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Jn").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_In").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Rn").getOtherDomain().ithVar(0));
		BDD sysIni = trans.restrict(temp);
		temp.free();
		
		temp = Env.getVar("util_0").getDomain().ithVar(1);
		temp.andWith(Env.getVar("util_In").getDomain().ithVar(0));
		
		temp.andWith(Env.getVar("util_Jn").getDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Rn").getDomain().ithVar(0));
		temp.andWith(Env.getVar("util_0").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Jn").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_In").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Rn").getOtherDomain().ithVar(0));
		BDD envIni = trans.restrict(temp);
		temp.free();
		
		temp = Env.getVar("util_0").getDomain().ithVar(0);
		temp.andWith(Env.getVar("util_Jn").getDomain().ithVar(1));
		
		temp.andWith(Env.getVar("util_In").getDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Rn").getDomain().ithVar(0));
		temp.andWith(Env.getVar("util_0").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Jn").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_In").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Rn").getOtherDomain().ithVar(0));
		BDD sysTrans = trans.restrict(temp);
		temp.free();
		
		temp = Env.getVar("util_0").getDomain().ithVar(1);
		temp.andWith(Env.getVar("util_In").getDomain().ithVar(1));
		
		temp.andWith(Env.getVar("util_Jn").getDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Rn").getDomain().ithVar(0));
		temp.andWith(Env.getVar("util_0").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Jn").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_In").getOtherDomain().ithVar(0));
		temp.andWith(Env.getVar("util_Rn").getOtherDomain().ithVar(0));
		BDD envTrans = trans.restrict(temp);
		temp.free();
		
		jitContext = new JitContext(envIni, envTrans, sysIni, sysTrans);
	}
	
	protected void loadFixpoints(BDD fixpoints, int[] ranks, int n, int m) {

		BDD[][][] X = new BDD[n-skipped.size()][m][];
		BDD[][][] Xunprimed = new BDD[n-skipped.size()][m][];
		int sk = 0;
		
		for (int j = 0; j < n; j++) {
			
			if (!skipped.contains(j)) {
				for (int i = 0; i < m; i++) {
					Xunprimed[j-sk][i] = new BDD[ranks[j]];
					X[j-sk][i] = new BDD[ranks[j]];
					for (int r = 0; r < ranks[j]; r++) {
						
						BDD temp = Env.getVar("util_In").getDomain().ithVar(i);
						temp.andWith(Env.getVar("util_Jn").getDomain().ithVar(j));
						temp.andWith(Env.getVar("util_Rn").getDomain().ithVar(r));
						
						temp.andWith(Env.getVar("util_0").getDomain().ithVar(0));
						temp.andWith(Env.getVar("util_0").getOtherDomain().ithVar(0));
						temp.andWith(Env.getVar("util_Jn").getOtherDomain().ithVar(0));
						temp.andWith(Env.getVar("util_In").getOtherDomain().ithVar(0));
						temp.andWith(Env.getVar("util_Rn").getOtherDomain().ithVar(0));
						
						BDD XBDD = fixpoints.restrict(temp);
						Xunprimed[j-sk][i][r] = XBDD.id();
						X[j-sk][i][r] = Env.prime(XBDD);
						XBDD.free();
						temp.free();
					}
				}
			} else {
				sk++;
			}
		}
		
		// Extract Y from X on current r
		sk = 0;
		BDD[][] Y = new BDD[n-skipped.size()][];
		BDD[][] Yunprimed = new BDD[n-skipped.size()][];
		for (int j = 0; j < n; j++) {
			
			if (!skipped.contains(j)) {
				Y[j-sk] = new BDD[ranks[j]];
				Yunprimed[j-sk] = new BDD[ranks[j]];
				for (int r = 0; r < ranks[j]; r++) {
					Y[j-sk][r] = Env.FALSE();
					Yunprimed[j-sk][r] = Env.FALSE();
					for (int i = 0; i < m; i++) {
						Y[j-sk][r].orWith(X[j-sk][i][r].id());
						Yunprimed[j-sk][r].orWith(Xunprimed[j-sk][i][r].id());
					}
				}
			} else {
				sk++;
			}
		}
		
		jitContext.setMX(X);
		jitContext.setMY(Y);
		jitContext.setMXunprimed(Xunprimed);
		jitContext.setMYunprimed(Yunprimed);
	}
	
	protected void loadJustices(BDD justices, int n, int m) {
		
		BDD justice;
		for (int j = 0; j < n; j++) {
			
			if (!skipped.contains(j)) {
				BDD temp = Env.getVar("util_0").getDomain().ithVar(0);
				temp.andWith(Env.getVar("util_Jn").getDomain().ithVar(j));
				
				temp.andWith(Env.getVar("util_In").getDomain().ithVar(0));
				temp.andWith(Env.getVar("util_Rn").getDomain().ithVar(0));
				temp.andWith(Env.getVar("util_0").getOtherDomain().ithVar(0));
				temp.andWith(Env.getVar("util_Jn").getOtherDomain().ithVar(0));
				temp.andWith(Env.getVar("util_In").getOtherDomain().ithVar(0));
				temp.andWith(Env.getVar("util_Rn").getOtherDomain().ithVar(0));
				
				justice = justices.restrict(temp);
				temp.free();
	            
	            justiceGar.add(justice);	
			}
		}
		
		for (int i = 0; i < m; i++) {
			
			BDD temp = Env.getVar("util_0").getDomain().ithVar(1);
			temp.andWith(Env.getVar("util_In").getDomain().ithVar(i));
			
			temp.andWith(Env.getVar("util_Jn").getDomain().ithVar(0));
			temp.andWith(Env.getVar("util_Rn").getDomain().ithVar(0));
			temp.andWith(Env.getVar("util_0").getOtherDomain().ithVar(0));
			temp.andWith(Env.getVar("util_Jn").getOtherDomain().ithVar(0));
			temp.andWith(Env.getVar("util_In").getOtherDomain().ithVar(0));
			temp.andWith(Env.getVar("util_Rn").getOtherDomain().ithVar(0));
			
			justice = justices.restrict(temp);
			temp.free();
            
			justiceAsm.add(justice);
		}
	}
	
	@Override
	public void load(String folder, String name, Map<String, String[]> sysVars, Map<String, String[]> envVars) {
		
		if (Env.getVar("Zn") == null) {
			throw new IllegalStateException("You are using an old version of the JIT controller. Please re-synthesize");
		}
		
		try {
			
			String prefix;
			if (name == null) {
				prefix = folder + File.separator;
			} else {
				prefix = folder + File.separator + name + ".";
			}
			
			BufferedReader sizesReader = new BufferedReader(new FileReader(prefix + "sizes"));
			
			int n = Integer.parseInt(sizesReader.readLine());
			int m = Integer.parseInt(sizesReader.readLine());
			int[] ranks = new int[n];
			for (int j = 0; j < n; j++) {
				ranks[j] = Integer.parseInt(sizesReader.readLine());
			}
			
			sizesReader.close();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Read Sizes");

			
			// Extract justices
			
			BDD justices = Env.loadBDD(prefix + "justice.bdd");
			loadJustices(justices, n, m);	
			justices.free();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Loaded Justice BDD");
			
			
			// Extract trans and init
			
			BDD trans = Env.loadBDD(prefix + "trans.bdd");
			loadTransAndInitial(trans);
			trans.free();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Loaded Transition BDD");
			
			
			// Extract X from fixpoints BDD

			BDD fixpoints = Env.loadBDD(prefix + "fixpoints.bdd");
			loadFixpoints(fixpoints, ranks, n, m);
			fixpoints.free();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Loaded Fixed-Points BDD");
			
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		// Delete vars even though they are not really removed from bdd engine.
		// At least they won't show up in next states enumeration
		Env.deleteVar("util_In");
		Env.deleteVar("util_Jn");
		Env.deleteVar("util_Rn");
		Env.deleteVar("util_0");
		
		for (int j = 0; j < justiceGar.size(); j++) {
			if (jitContext.rank(j) == 0) {
				throw new IllegalStateException("The controller is invalid. Please make sure the specification is well-separated and satisfiable");
			}
		}
		
		BDD Z = Env.unprime(jitContext.Y(0, jitContext.rank(0) - 1));
		
		jitContext.setTrans(jitContext.getSysTrans().and(jitContext.getEnvTrans()));
		jitContext.setIni(jitContext.getSysIni().and(jitContext.getEnvIni()));
		
		BDD tempIni = jitContext.getIni().exist(Env.globalPrimeVars());
		jitContext.getIni().free();
		
		jitContext.setIni(tempIni.and(Z));
		tempIni.free();
		Z.free();
		
		rho1 = new BDD[justiceGar.size()];
		rho2 = new BDD[justiceGar.size()][];
		rho3 = new BDD[justiceGar.size()][][];
		
		for (int jx = 0; jx < justiceGar.size(); jx++) {
						
			rho2[jx] = new BDD[jitContext.rank(jx)];
			rho3[jx] = new BDD[jitContext.rank(jx)][];
			
			for (int r = 0; r < jitContext.rank(jx); r++) {
				
				rho3[jx][r] = new BDD[justiceAsm.size()];

			}
		}
	}
	
	
	private BDD getTransition(int jx) {
		
//		System.out.println("Before adding Z justice=" + jx);
		
		if (rho1[jx] == null) {
			
			BDD transition = jitContext.getTrans().and(Env.getVar("Zn").getDomain().ithVar(jx));
			int newJx = (jx+1) % justiceGar.size();
			rho1[jx] = transition
					.and(justiceGar.get(jx))
					.and(Env.prime(Env.getVar("Zn").getDomain().ithVar(newJx)))
					.and(jitContext.Y(newJx, jitContext.rank(newJx)-1));
		}
		
//		System.out.println("Adding Z justice=" + jx);
		
		return rho1[jx];
	}
	
	private BDD getTransition(int jx, int rx) {
		
//		System.out.println("Before adding Y rank=" + rx + " justice=" + jx);
		
		if (rho2[jx][rx] == null) {
			
			BDD transition = jitContext.getTrans().and(Env.getVar("Zn").getDomain().ithVar(jx));
			transition.andWith(Env.prime(Env.getVar("Zn").getDomain().ithVar(jx)));
			
			rho2[jx][rx] = transition
					.and(jitContext.Yunprimed(jx, rx-1).not())
					.and(jitContext.Yunprimed(jx, rx))
					.and(jitContext.Y(jx, rx-1));
			
			if (eager) {
				rho2[jx][rx] = rho2[jx][rx].and(justiceGar.get(jx).not());
			}
		}
		
//		System.out.println("Adding Y rank=" + rx + " justice=" + jx);
		
		return rho2[jx][rx];
	}
	
	private BDD getTransition(int jx, int rx, int ix) {
		
//		System.out.println("Before adding X=" + ix + " rank=" + rx + " justice=" + jx);
		
		if (rho3[jx][rx][ix] == null) {
			
			BDD transition = jitContext.getTrans().and(Env.getVar("Zn").getDomain().ithVar(jx));
			transition.andWith(Env.prime(Env.getVar("Zn").getDomain().ithVar(jx)));

			BDD currTransition = transition.and(jitContext.Yunprimed(jx, rx-1).not());	
			BDD lowX = Env.FALSE();
			
			for (int i = 0; i <= ix; i++) {
				
				if (rho3[jx][rx][i] == null) {
					rho3[jx][rx][i] = currTransition
							.and(lowX.not())
							.and(jitContext.Xunprimed(jx, i, rx))
							.and(jitContext.X(jx, i, rx))
							.and(justiceAsm.get(i).not());
					
					if (eager) {
						rho3[jx][rx][i] = rho3[jx][rx][i].and(justiceGar.get(jx).not()).and(jitContext.Y(jx, rx-1).not());
					}
				}
				
				lowX = lowX.or(jitContext.Xunprimed(jx, i, rx));

			}
			
			lowX.free();
			currTransition.free();
		}
		
//		System.out.println("Adding X=" + ix + " rank=" + rx + " justice=" + jx);
		
		return rho3[jx][rx][ix];
	}
	

	@Override
	public List<BDD> getJusticeGar() {
		return justiceGar;
	}

	@Override
	public List<BDD> getJusticeAsm() {
		return justiceAsm;
	}

	@Override
	public JitContext getJitContext() {
		return jitContext;
	}

	@Override
	public void free() {
		
		jitContext.free();
		
		for (int i = 0; i < justiceAsm.size(); i++) {
			justiceAsm.get(i).free();
		}
		
		for (int i = 0; i < justiceGar.size(); i++) {
			justiceGar.get(i).free();
		}
	}

	@Override
	public BDD transitions() {
		return jitContext.getTrans();
	}

	@Override
	public JitState getJitState() {
		return jitState;
	}

	@Override
	public BDD initial() {
		return jitContext.getIni();
	}

	@Override
	public void init(BDD currentState) {
		
		jitState = new JitState();
		BDD currentStatePrimed = Env.prime(currentState);
						
		boolean found = false;
		BDD temp;
		for (int r = 0; r < jitContext.rank(0); r++) {
			temp = currentStatePrimed.and(jitContext.Y(0, r));
			
			if (!temp.isZero()) {
				found = true;
				temp.free();
				jitState.setRank(r);
				break;
			}
			
			temp.free();
		}
		
		if (!found) {
			throw new IllegalArgumentException("Probably initial environment inputs violate the initial assumptions");
		}
		
		currentStatePrimed.free();
		
		jitState.setGoalFinder(new NextJusticeGoalFinder() {
			
			@Override
			public int findNextJusticeGoal(int jx) {
				return (jx + 1) % justiceGar.size();
			}
		});
		
		jitContext.getEnvIni().free();
		jitContext.getSysIni().free();
	}
	
	private int savedRank = 0;
	private int savedJx = 0;

	@Override
	public void saveState() {
		this.savedJx = this.jitState.getJx();
		this.savedRank = this.jitState.getRank();
	}

	@Override
	public void loadState() {
		this.jitState.setJx(this.savedJx);
		this.jitState.setRank(this.savedRank);
	}
}
