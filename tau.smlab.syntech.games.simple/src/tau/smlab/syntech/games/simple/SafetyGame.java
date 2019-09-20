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

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.games.GameMemory;
import tau.smlab.syntech.games.GameSolver;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

public class SafetyGame extends GameSolver {

  protected GameMemory mem;

  public GameMemory getMem() {
    return mem;
  }

  public SafetyGame(GameModel model) {
    super(model);
    mem = new GameMemory();    
  }

  @Override
  public boolean checkRealizability() {
    BDD z = Env.TRUE();
    FixPoint zFix = new FixPoint(true);
    while(zFix.advance(z)) {
      z = env.yieldStates(sys, z);
      if (!sysWinAllInitial(z)) {
        zFix.free();
        mem.setWin(z);
        mem.setComplete(false);
        return false;
      }
    }
    mem.setWin(z);
    mem.setComplete(true);
    return sysWinAllInitial(z);
  }

  @Override
  public void free() {
    mem.free();
  }

  public boolean sysWinAllInitial() {
    return this.sysWinAllInitial(mem.getWin());
  }

  public BDD sysWinningStates() {
    return mem.getWin();
  }

}
