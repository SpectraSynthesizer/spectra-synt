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

package tau.smlab.syntech.games.controller;

import java.util.Map;
import java.util.function.Function;

import net.sf.javabdd.BDD;

public interface Controller {
	
	/**
	 * Loads the controller according to the provided folder and name
	 * @param folder with synthesized BDD files
	 */
	void load(String folder, String name, Map<String, String[]> sysVars, Map<String, String[]> envVars);
	
	/**
	 * Performs the initial step of the controller
	 * @param currentState BDD of assignments to system and environment variables in the current state
	 */
	void init(BDD currentState);
	
	/**
	 * Returns a BDD of next states based on current state and environment inputs
	 * @param currentState BDD of assignments to system and environment variables in the current state
	 * @param inputs BDD of assignments to environment variables in the next state
	 * 
	 * @return a BDD of next states based on current state and environment inputs
	 */
	BDD next(BDD currentState, BDD inputs);
	
	/**
	 * Frees the controller
	 */
	void free();
	
	/**
	 * Returns the BDD of safety transitions defined in the specification
	 * @return BDD of safety transitions defined in the specification
	 */
	BDD transitions();
	
	/**
	 * Returns the BDD of initial transitions defined in the specification
	 * @return BDD of initial transitions defined in the specification
	 */
	BDD initial();
	
	
	void saveState();
	void loadState();
	
	BDD succ(BDD from);
	BDD pred(BDD to);
	
	
	default BDD kSucc(BDD from, int k) {
		return kIteration(from.id(), k, this::succ);
	}
	
	default BDD kPred(BDD to, int k) {
		return kIteration(to.id(), k, this::pred);
	}
	
	default BDD kIteration(BDD start, int k, Function<BDD, BDD> func) {
		
		int counter = 0;
		BDD prev = start.id();
		BDD next = start;
		
		do {
			prev.free();
			prev = next;		
			BDD tmp = func.apply(prev);
			next = next.or(tmp);
			tmp.free();
			counter++;
			
		} while (!prev.equals(next) & (counter != k));
		
		return next;
	}
}
