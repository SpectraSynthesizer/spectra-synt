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
import java.util.List;
import java.util.Map;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;

/**
 * A class for the execution of symbolic controllers in a Just-in-time fashion
 * @author ilia
 *
 */
public class BasicJitControllerImplC implements JitController {

	private BDD transitions;
	private BDD initial;
	
	@Override
	public BDD next(BDD currentState, BDD inputs) {
		
		return (Env.TRUE().getFactory()).nextStatesJits(currentState, inputs, Env.allCouplesPairing(), Env.globalUnprimeVars());
	}
	
	@Override
	public void load(String folder, String name, Map<String, String[]> sysVars, Map<String, String[]> envVars) {
				
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
			
			
			int[] jindices = Env.getVar("util_Jn").getDomain().vars();
			int[] iindices = Env.getVar("util_In").getDomain().vars();
			int[] rindices = Env.getVar("util_Rn").getDomain().vars();
			int utilindex = Env.getVar("util_0").getDomain().vars()[0];
			
			// Extract justices
			
			BDD justices = Env.loadBDD(prefix + "justice.bdd");
			(Env.TRUE().getFactory()).loadJusticesJits(justices, jindices, iindices, utilindex, n, m);
			justices.free();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Loaded Justice BDD");
			
			
			
			// Extract trans and init
			
			BDD trans = Env.loadBDD(prefix + "trans.bdd");
			(Env.TRUE().getFactory()).loadTransJits(trans, jindices, iindices, utilindex);
			trans.free();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Loaded Transition BDD");
			
			
			// Extract X from fixpoints BDD
			
			BDD fixpoints = Env.loadBDD(prefix + "fixpoints.bdd");
			(Env.TRUE().getFactory()).loadFixpointsJits(fixpoints, jindices, iindices, rindices, ranks, Env.allCouplesPairing(), Env.globalPrimeVars());
			fixpoints.free();
			System.out.println(Env.TRUE().getFactory().getNodeNum() + " - Loaded Fixed-Points BDD");

			
			
			
			initial = (Env.TRUE().getFactory()).getInitialJits();
			transitions = (Env.TRUE().getFactory()).getTransitionsJits();
			
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		// Delete vars even though they are not really removed from bdd engine.
		// At least they won't show up in next states enumeration
		Env.deleteVar("util_In");
		Env.deleteVar("util_Jn");
		Env.deleteVar("util_Rn");
		Env.deleteVar("util_0");
	}

	@Override
	public List<BDD> getJusticeGar() {
		return null;
	}

	@Override
	public List<BDD> getJusticeAsm() {
		return null;
	}

	@Override
	public JitContext getJitContext() {
		return null;
	}

	@Override
	public void free() {
		(Env.TRUE().getFactory()).freeControllerJits();
	}

	@Override
	public BDD transitions() {
		return transitions;
	}

	@Override
	public JitState getJitState() {
		return null;
	}

	@Override
	public BDD initial() {
		return initial;
	}

	@Override
	public void init(BDD currentState) {
		
		int result = (Env.TRUE().getFactory()).initControllerJits(currentState, Env.allCouplesPairing());
		
		if (result == -1) {
			throw new IllegalArgumentException("Illegal rank reached. Probably initial environment inputs violate the initial assumptions");
		}
	}

	@Override
	public void saveState() {
	}

	@Override
	public void loadState() {
	}

	@Override
	public BDD succ(BDD from) {
		return null;
	}

	@Override
	public BDD pred(BDD to) {
		return null;
	}

}
