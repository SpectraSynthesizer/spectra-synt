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

package tau.smlab.syntech.games;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule;

public abstract class GameSolver {

  protected GameModel model;
  protected PlayerModule env;
  protected PlayerModule sys;

  public GameSolver(GameModel m) {
    this.model = m;
    env = m.getEnv();
    sys = m.getSys();
  }

  /**
   * check whether player wins
   * 
   * @return
   */
  abstract public boolean checkRealizability();

  abstract public void free();

  /**
   * check whether the system player wins from all initial states if <code>winSys</code> are its winning states
   * 
   * @param winSys
   * @return
   */
  public boolean sysWinAllInitial(BDD winSys) {
    BDD sysWin = winSys.and(sys.initial());
    BDD result = env.initial().id().impWith(sysWin.exist(sys.moduleUnprimeVars()))
        .forAll(env.moduleUnprimeVars());
    sysWin.free();

    boolean allIni = result.isOne();
    result.free();
    return allIni;
  }

  /**
   * check whether the environment player wins if <code>win</code> are the winning states
   * 
   * @param winEnv
   * @return
   */
  public boolean envWinAllInitial(BDD winEnv) {
    BDD envWin = env.initial().id().andWith(sys.initial().id().impWith(winEnv).forAll(sys.moduleUnprimeVars()));

    boolean winSomeIni = !envWin.isZero();
    envWin.free();
    return winSomeIni;
  }
}
