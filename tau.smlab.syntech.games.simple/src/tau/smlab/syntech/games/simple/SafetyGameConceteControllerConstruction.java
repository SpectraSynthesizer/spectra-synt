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

package tau.smlab.syntech.games.simple;

import java.util.Stack;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDD.BDDIterator;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.games.AbstractGamesException;
import tau.smlab.syntech.games.GameMemory;
import tau.smlab.syntech.games.controller.enumerate.ConcreteControllerConstruction;
import tau.smlab.syntech.games.controller.enumerate.EnumStateI;
import tau.smlab.syntech.games.controller.enumerate.EnumStrategyI;
import tau.smlab.syntech.games.controller.enumerate.EnumStrategyImpl;
import tau.smlab.syntech.jtlv.CoreUtil;

public class SafetyGameConceteControllerConstruction extends ConcreteControllerConstruction {

  private GameMemory mem;

  public SafetyGameConceteControllerConstruction(GameMemory mem, GameModel m) {
    super(mem, m);
    this.mem = mem;
  }

  @Override
  public EnumStrategyI calculateConcreteController() throws AbstractGamesException {
    Stack<EnumStateI> st_stack = new Stack<EnumStateI>();

    BDDVarSet envUnprimedVars = env.moduleUnprimeVars();
    BDDVarSet sysUnprimedVars = sys.moduleUnprimeVars();

    EnumStrategyImpl aut = new EnumStrategyImpl();

    for (BDDIterator it = env.initial().iterator(envUnprimedVars); it.hasNext();) {
      BDD envIni = it.nextBDD();
      BDD iniWin = envIni.andWith(getWinningInitialStates());
      BDD oneIni = CoreUtil.satOne(iniWin, envUnprimedVars.union(sysUnprimedVars));
      st_stack.push(aut.getState(oneIni, null));
    }

    System.out.println("Initial states of environment: " + st_stack.size());

    // iterating over the stacks.
    while (!st_stack.isEmpty()) {
      // making a new entry.
      EnumStateI new_state = st_stack.pop();
      BDD p_st = new_state.getData();

      BDD good_suc = env.succ(p_st).andWith(mem.getWin().id());
      BDD one_cand = CoreUtil.satOne(good_suc, envUnprimedVars.union(sysUnprimedVars));
      good_suc.free();
      // add succ
      EnumStateI succ = aut.addSuccessorState(new_state, one_cand, null);
      if (succ != null) {
        // if a new state was created.
        st_stack.push(succ);
      }
    }

    return aut;
  }

  private BDD getWinningInitialStates() {
    return mem.getWin().and(sys.initial());
  }

}
