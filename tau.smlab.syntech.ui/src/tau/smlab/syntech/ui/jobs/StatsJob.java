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

package tau.smlab.syntech.ui.jobs;

public class StatsJob extends SyntechJob {

	@Override
	protected void doWork() {

		clearMarkers();

		printToConsole("");
		
		printToConsole("Environment variables count: " + model.getEnv().getAllFields().size());
		printToConsole("System variables count: " + model.getSys().getAllFields().size());
		printToConsole("Of which auxiliary variables: " + model.getSys().getAuxFields().size());
		printToConsole("");
		
		printToConsole("Environment initial constraints count: " + model.getEnvBehaviorInfo().stream().filter(b -> b.isInitial()).count());
		printToConsole("Environment safety constraints count: " + model.getEnvBehaviorInfo().stream().filter(b -> b.isSafety()).count());
		printToConsole("Environment justice constraints count: " + model.getEnvBehaviorInfo().stream().filter(b -> b.isJustice()).count());
		printToConsole("");
		
		printToConsole("System initial constraints count: " + model.getSysBehaviorInfo().stream().filter(b -> b.isInitial()).count());
		printToConsole("System safety constraints count: " + model.getSysBehaviorInfo().stream().filter(b -> b.isSafety()).count());
		printToConsole("System justice constraints count: " + model.getSysBehaviorInfo().stream().filter(b -> b.isJustice()).count());
		printToConsole("");
		
		printToConsole("Auxiliary initial constraints count: " + model.getAuxBehaviorInfo().stream().filter(b -> b.isInitial()).count());
		printToConsole("Auxiliary safety constraints count: " + model.getAuxBehaviorInfo().stream().filter(b -> b.isSafety()).count());
		printToConsole("");
		
		printToConsole("Environment initial BDD node count: " + model.getEnv().initial().nodeCount());
		printToConsole("Environment transitions BDD node count: " + model.getEnv().trans().nodeCount());
		printToConsole("");
		
		printToConsole("System initial BDD node count: " + model.getSys().initial().nodeCount());
		printToConsole("System transitions BDD node count: " + model.getSys().trans().nodeCount());
		printToConsole("");
		
		model.free();
	}

}
