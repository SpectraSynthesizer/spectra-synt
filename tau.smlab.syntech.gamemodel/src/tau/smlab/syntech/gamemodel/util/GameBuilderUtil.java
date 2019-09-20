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

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.ModuleException;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

public class GameBuilderUtil {

  public static void buildSys(GameModel m, List<BehaviorInfo> part) {
    try {
  	  build(m.getSysBehaviorInfo(), m.getSys(), part, m.getAuxBehaviorInfo());		
    } catch (ModuleException e) {
  	  e.printStackTrace();
    }	  
  }

  public static void buildEnv(GameModel m, List<BehaviorInfo> part) {
    try {
  	  build(m.getEnvBehaviorInfo(), m.getEnv(), part, new ArrayList<BehaviorInfo>());		
    } catch (ModuleException e) {
  	  e.printStackTrace();
    }
  }
  
  private static void build(List<BehaviorInfo> bi, PlayerModule pm, List<BehaviorInfo> part, List<BehaviorInfo> aux) throws ModuleException {
  
    bi.clear();
    bi.addAll(part);
    pm.reset();
    part.addAll(aux);	  
    
    for (BehaviorInfo bhave : part) {
  	  if (bhave.isInitial()) {
  		  pm.conjunctInitial(bhave.initial.id());
  	  }
  	  if (bhave.isSafety()) {
  		  pm.conjunctTrans(bhave.safety.id());
  	  }
  	  if (bhave.isJustice()) {
  		  pm.addJustice(bhave.justice.id(), bhave.traceId);
  	  }
    }
  }
  
  public static void buildQuantifiedSys(GameModel m, List<BehaviorInfo> part, List<ModuleBDDField> vars) {
    PlayerModule pm = m.getSys();
	  
	pm.reset();
	  
	// quantify over core behavior and add
	for (BehaviorInfo b : part) {
	  if (b.isJustice()) {
		BDD q = b.justice.id();
		q = quanAll(q, vars);
		pm.addJustice(q, b.traceId);
	  }
	  if (b.isSafety()) {
		BDD q = b.safety.id();
		q = quanAll(q, vars);
		pm.conjunctTrans(q.id());
	  }
	  if (b.isInitial()) {
		BDD q = b.initial.id();
		q = quanAll(q, vars);
		pm.conjunctInitial(q.id());	 
	  }
	}
		
	// add aux gars as are
	for (BehaviorInfo g : m.getAuxBehaviorInfo()) {
	  if (g.isInitial()) {
	    pm.conjunctInitial(g.initial.id());
	  }
	  if (g.isSafety()) {
	    pm.conjunctTrans(g.safety.id());
	  }
	  if (g.isJustice()) {
	    pm.addJustice(g.justice.id(), g.traceId);
	  }
	} 
  }
  
	/**
	 * quantify over a set of variables
	 * 
	 * @param bdd the BDD
	 * @param vars the vars for existetial quantification
	 * @return
	 */
  private static BDD quanAll(BDD bdd, List<ModuleBDDField> vars) {
	for (ModuleBDDField v:vars) {
	  bdd = bdd.exist(v.support());
	}
	return bdd;
  }
}
