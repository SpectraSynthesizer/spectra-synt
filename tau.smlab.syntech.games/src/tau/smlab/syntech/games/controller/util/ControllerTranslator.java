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

package tau.smlab.syntech.games.controller.util;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.ModuleException;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.games.controller.enumerate.EnumStateI;
import tau.smlab.syntech.games.controller.enumerate.EnumStrategyI;
import tau.smlab.syntech.games.controller.symbolic.SymbolicController;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.ModuleVariableException;

/**
 * translates a concrete controller to a symbolic controller
 *
 */
public class ControllerTranslator {

  /**
   * 
   * @param concrete
   *          the concrete strategy of player p
   * @param p
   *          PlayerModule, e.g., system from GameModel.getSys()
   * @return
   */
  public static SymbolicController translateWithJusticeMem(EnumStrategyI concrete, PlayerModule p) {

    BDD ini = Env.FALSE();
    BDD trans = Env.FALSE();

    createMem(p);

    for (EnumStateI t : concrete.enumController()) {
      EnumStateI s = (EnumStateI) t;
      for (EnumStateI tucc : s.getSuccessors()) {
        EnumStateI succ = (EnumStateI) tucc;
        BDD nxt = Env.prime(getBDDWithRank(succ));
        nxt.andWith(getBDDWithRank(s));
        trans.orWith(nxt);
      }

      if (s.isInitial()) {
        ini.orWith(getBDDWithRank(s));
      }
    }

    return new SymbolicController(ini, trans);
  }

  private static void createMem(PlayerModule p) {
    if (p.justiceNum() > 0) {
      try {
        int upper = Math.max(p.justiceNum()-1, 1);
        p.addVar("Zn", 0, upper, true);
      } catch (ModuleException | ModuleVariableException e) {
        e.printStackTrace();
      }
    }
  }

  private static BDD getBDDWithRank(EnumStateI s) {
    return s.getData().and(Env.getBDDValue("Zn", "" + s.get_rank_info().memVal()));
  }

}
