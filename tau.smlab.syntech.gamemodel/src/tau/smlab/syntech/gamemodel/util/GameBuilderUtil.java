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
import tau.smlab.syntech.gamemodel.PlayerModule.TransFuncType;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

public class GameBuilderUtil {

	/**
	 * Use this method to build the system PlayerModule of the given GameModel. 
	 * Aux behaviors are automatically used.
	 * 
	 * @param m		game model
	 * @param part  the behaviors
	 */
  public static void buildSys(GameModel m, List<BehaviorInfo> part) {
	List<BehaviorInfo> bvs = new ArrayList<BehaviorInfo>(part);
	bvs.addAll(m.getAuxBehaviorInfo());
	m.getSysBehaviorInfo().clear();
	m.getSysBehaviorInfo().addAll(part);
    try {
  	  build(m.getSys(), bvs);		
    } catch (ModuleException e) {
  	  e.printStackTrace();
    }	  
  }

  	/**
  	 * Use this method to build the environment PlayerModule of the given GameModel. 
  	 * 
  	 * @param m		game model
  	 * @param part	the behaviors
  	 */
  public static void buildEnv(GameModel m, List<BehaviorInfo> part) {
	List<BehaviorInfo> bvs = new ArrayList<BehaviorInfo>(part);
	m.getEnvBehaviorInfo().clear();
	m.getEnvBehaviorInfo().addAll(part);
    try {
  	  build(m.getEnv(), bvs);		
    } catch (ModuleException e) {
  	  e.printStackTrace();
    }
  }
  
  /**
   * Build the PlayerModule using the given behaviors. Take into consideration the type of safety the module uses
   * 
   * @param module 
   * @param beavs
   * @throws ModuleException
   */
	private static void build(PlayerModule module, List<BehaviorInfo> beavs) throws ModuleException {

		module.reset();

		for (BehaviorInfo b : beavs) {
			if (b.isInitial()) {
				module.conjunctInitial(b.initial.id());
			}
			if (b.isSafety()) {
				switch (module.getTransFuncType()) {
				case SINGLE_FUNC:
					module.conjunctTrans(b.safety.id());
					break;
				case DECOMPOSED_FUNC:
					module.addToTransList(b.safety.id());
					break;
				case PARTIAL_DECOMPOSED_FUNC:
					module.addToTransList(b.safety.id());
					break;
				default:
					System.err.println("unknown type " + module.getTransFuncType());
					break;
				}
			}
			if (b.isJustice()) {
				module.addJustice(b.justice.id(), b.traceId);
			}
		}

		if (module.getTransFuncType() == TransFuncType.DECOMPOSED_FUNC) {
			module.calcTransQuantList();
		} else if (module.getTransFuncType() == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
			module.createPartialTransQuantList();
		}
	}
  
  public static void buildQuantifiedSys(GameModel m, List<ModuleBDDField> vars) {
    PlayerModule module = m.getSys();
	  
    module.reset();
	
	List<BehaviorInfo> part = new ArrayList<BehaviorInfo>();
	part.addAll(m.getSysBehaviorInfo());
	part.addAll(m.getAuxBehaviorInfo());
	  
	// quantify over core behavior and add
	for (BehaviorInfo b : part) {
	  if (b.isJustice()) {
		BDD q = b.justice.id();
		q = quanAll(q, vars);
		module.addJustice(q, b.traceId);
	  }
	  if (b.isSafety()) {
		BDD q = b.safety.id();
		q = quanAll(q, vars);		
		switch (module.getTransFuncType()) {
		case SINGLE_FUNC:
			module.conjunctTrans(q.id());
			break;
		case DECOMPOSED_FUNC:
			module.addToTransList(q.id());
			break;
		case PARTIAL_DECOMPOSED_FUNC:
			module.addToTransList(q.id());
			break;
		default:
			System.err.println("unknown type " + module.getTransFuncType());
			break;
		}
	  }
	  if (b.isInitial()) {
		BDD q = b.initial.id();
		q = quanAll(q, vars);
		module.conjunctInitial(q.id());	 
	  }
	}
	
	if (module.getTransFuncType() == TransFuncType.DECOMPOSED_FUNC) {
		module.calcTransQuantList();
	} else if (module.getTransFuncType() == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
		module.createPartialTransQuantList();
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
