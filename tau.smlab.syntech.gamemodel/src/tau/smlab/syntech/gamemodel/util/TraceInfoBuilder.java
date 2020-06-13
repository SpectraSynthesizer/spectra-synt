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

package tau.smlab.syntech.gamemodel.util;

import java.util.ArrayList;
import java.util.List;

import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;

/**
 * This is an abstract base class for building games based on lists of traces
 * 
 * @author shalom
 *
 */
public abstract class TraceInfoBuilder {

	protected GameModel gm = null;
	protected List<BehaviorInfo> env = null;
	protected List<BehaviorInfo> sys = null;
	protected List<BehaviorInfo> aux = null;
	
	List<Integer> traceList = null;
	
	public TraceInfoBuilder(GameModel gm) {
		this.gm = gm;
		env = new ArrayList<BehaviorInfo>(gm.getEnvBehaviorInfo());
		sys = new ArrayList<BehaviorInfo>(gm.getSysBehaviorInfo());
		aux = new ArrayList<BehaviorInfo>(gm.getAuxBehaviorInfo());
		createTraceList();
	}
	
	/**
	 * return the list of all traces which are relevant to the module
	 */
	public List<Integer> getTraceList() {
		return traceList;
	}
	
	/**
	 * restore the modle according to original traces
	 * 
	 * @return restored model
	 */
	public GameModel restore() {
		GameBuilderUtil.buildEnv(gm, env);
		
		List<BehaviorInfo> gameAux = gm.getAuxBehaviorInfo();
		gameAux.clear();
		gameAux.addAll(aux);	
		GameBuilderUtil.buildSys(gm, sys);
		
		return gm;
	}
	
	/**
	 * create a list of all traces which are relevant to the module
	 */
	protected abstract void createTraceList();
	
	/**
	 * Build and return a model based on the one in the class which according to the subset of the trace list
	 * @param taken
	 * @return
	 */
	public abstract GameModel build(List<Integer> taken);
}
