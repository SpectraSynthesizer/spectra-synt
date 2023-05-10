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

import java.io.File;
import java.io.IOException;
import java.util.Map;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.logs.BDDLogWriter;

public class LoggingController extends AbstractController {

	private BDDLogWriter log;
	
	public LoggingController(Controller controller) {
		super(controller);
	}

	@Override
	public void load(String folder, String name, Map<String, String[]> sysVars, Map<String, String[]> envVars) {
		
		controller.load(folder, name, sysVars, envVars);
		
		// Creates folder ./logs if it doesn't exist. Prepares the log object to write a BDD log
		File lp = new File("logs");
		if (!lp.exists()) {
			lp.mkdir();
		}
		String ts = "" + System.currentTimeMillis();
		String logName = lp.getPath() + "/" + ts + ".gol";
		try {
			log = new BDDLogWriter(logName, Env.globalUnprimeVars());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	@Override
	public BDD next(BDD currentState, BDD inputs) {
		
		BDD nextStates = controller.next(currentState, inputs);
		
		//try to log the current state
		try {
			log.write(currentState);
		} catch (IOException e) {
			System.err.println("There was an error writing to log during controller execution: " + e.getMessage());
		}
		
		return nextStates;
	}

	@Override
	public void init(BDD currentState) {
		controller.init(currentState);
	}

	@Override
	public void saveState() {
		controller.saveState();
		
	}

	@Override
	public void loadState() {
		controller.loadState();
	}

	@Override
	public BDD kSucc(BDD from, int k) {
		return controller.kSucc(from, k);
	}
	
	@Override
	public BDD kPred(BDD to, int k) {
		return controller.kPred(to, k);
	}

	@Override
	public BDD succ(BDD from) {
		return controller.succ(from);
	}

	@Override
	public BDD pred(BDD to) {
		return controller.pred(to);
	}

}
