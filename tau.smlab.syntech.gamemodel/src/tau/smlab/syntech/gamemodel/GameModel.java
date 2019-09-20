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

package tau.smlab.syntech.gamemodel;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.jtlv.Env;

public class GameModel {

	private PlayerModule sys;
	private PlayerModule env;

	private Map<Integer, BDD> weights;

	private List<BehaviorInfo> auxBehaviorInfo = new ArrayList<BehaviorInfo>();
	private List<BehaviorInfo> sysBehaviorInfo = new ArrayList<BehaviorInfo>();
	private List<BehaviorInfo> envBehaviorInfo = new ArrayList<BehaviorInfo>();

	public PlayerModule getSys() {
		return sys;
	}

	public void setSys(PlayerModule sys) {
		this.sys = sys;
	}

	public PlayerModule getEnv() {
		return env;
	}

	public void setEnv(PlayerModule env) {
		this.env = env;
	}

	public Map<Integer, BDD> getWeights() {
		return weights;
	}

	public void setWeights(Map<Integer, BDD> weights) {
		this.weights = weights;
	}

	public List<BehaviorInfo> getEnvBehaviorInfo() {
		return envBehaviorInfo;
	}

	public void addEnvBehaviorInfo(BehaviorInfo envBehaviorInfo) {
		this.envBehaviorInfo.add(envBehaviorInfo);
	}

	public List<BehaviorInfo> getSysBehaviorInfo() {
		return sysBehaviorInfo;
	}

	public void addSysBehaviorInfo(BehaviorInfo sysBehaviorInfo) {
		this.sysBehaviorInfo.add(sysBehaviorInfo);
	}

	public List<BehaviorInfo> getAuxBehaviorInfo() {
		return auxBehaviorInfo;
	}

	public void addAuxBehaviorInfo(BehaviorInfo auxBehaviorInfo) {
		this.auxBehaviorInfo.add(auxBehaviorInfo);
	}

	public void updateSingleTransFunc() {
		sys.createTransFromPartTrans();
		env.createTransFromPartTrans();
	}

	public void resetSingleTransFunc() {
		sys.resetSingleTrans();
		env.resetSingleTrans();
	}

	public void free() {
		Env.free(auxBehaviorInfo);
		Env.free(sysBehaviorInfo);
		Env.free(envBehaviorInfo);
		if(this.weights != null) {
			Env.free(this.weights);
		}
		sys.free();
		env.free();
	}

}
