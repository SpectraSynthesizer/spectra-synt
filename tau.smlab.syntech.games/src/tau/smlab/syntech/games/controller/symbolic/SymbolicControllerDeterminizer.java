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

package tau.smlab.syntech.games.controller.symbolic;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.jtlv.Env;

public class SymbolicControllerDeterminizer {

  /**
   * Creates a deterministic version of the specified symbolic controller {@code ctrl}, such that
   * for any (initial) state there is at most one assignment to the (non primed) variables controlled by
   * {@code detPlayer} module.
   * 
   * @param ctrl non-deterministic symbolic controller
   * @param detPlayer the player module by which the controller would become deterministic
   * @param responderModule whether {@code detPlayer} is the system (responder) player or the environment player
   * @return A deterministic version of the specified symbolic controller relative to the given player module.
   */
  public static SymbolicController determinize(SymbolicController ctrl, PlayerModule detPlayer, boolean responderModule) {
    if(responderModule) {
      return determinize(ctrl, Env.union(detPlayer.getAllFields()), Env.unionPrime(detPlayer.getAllFields()));
    }
    
    //remove variables of the responder module from the controller
    BDD transEx = ctrl.trans().exist(Env.globalPrimeVars().minus(Env.unionPrime(detPlayer.getAllFields())));
    BDD iniEx = ctrl.initial().exist(Env.globalUnprimeVars().minus(Env.union(detPlayer.getAllFields())));
    
    SymbolicController ctrlEx = new SymbolicController(iniEx, transEx);
    
    //create a deterministic version of the controller with the removed variables of the responder module
    SymbolicController detCtrl = determinize(ctrlEx, Env.union(detPlayer.getAllFields()), Env.unionPrime(detPlayer.getAllFields()));
    
    ctrlEx.free();
    
    SymbolicController resCtrl = new SymbolicController();
    
    //add valid choices of the responder module to the new deterministic controller
    resCtrl.setInit(ctrl.initial().and(detCtrl.initial()));
    resCtrl.setTrans(ctrl.trans().and(detCtrl.trans()));
    
    detCtrl.free();
    
    return resCtrl;
  }
  /**
   * Creates a deterministic version of the specified symbolic controller {@code ctrl}, such that
   * for any (initial) state there is at most one assignment to the variables (iniDetVars) transDetVars that
   * evaluates to true.
   * 
   * @param ctrl non-deterministic symbolic controller
   * @param transDetVars the variables set according to which the transition relation would be deterministic
   * @param iniDetVars   the variables set according to which the initial states would be deterministic
   * @return A deterministic version of the specified symbolic controller relative to the two given variables sets.
   */
  public static SymbolicController determinize(SymbolicController ctrl, BDDVarSet transDetVars, BDDVarSet iniDetVars) {
    BDD detTrans, trans = ctrl.trans();
    BDD detIni, ini = ctrl.initial();
    
    detTrans = trans.determinizeController(transDetVars);
    detIni = ini.determinizeController(iniDetVars);
    
    return new SymbolicController(detIni, detTrans);
  }
}
