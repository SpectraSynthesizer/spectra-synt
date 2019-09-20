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

package tau.smlab.syntech.checks;

import java.util.ArrayList;
import java.util.List;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.lib.FixPoint;

/**
 * assuming the user has an unrealizable spec this check will determine whether it makes sense to add assumptions that
 * allow "good" executions: where the environment can satisfy all assumptions and where the system can satisfy all
 * guarantees
 * 
 * This gives no guarantee that a corresponding strategy exists.
 *
 */
public class CouldAsmHelp {

  /**
   * assuming the user has an unrealizable spec this check will determine whether it makes sense to add assumptions that
   * allow "good" executions: where the environment can satisfy all assumptions and where the system can satisfy all
   * guarantees
   * 
   * This gives no guarantee that a corresponding strategy exists.
   * 
   * @param m
   *          the game model with both players
   * @return true if there is a common way to satisfy asms and gars
   */
  public static boolean couldAsmHelp(GameModel m) {
    BDD ini = m.getSys().initial().and(m.getEnv().initial());
    BDD trans = m.getSys().trans().and(m.getEnv().trans());
    List<BDD> buchis = new ArrayList<BDD>();
    for (int i = 0; i < m.getSys().justiceNum(); i++) {
      buchis.add(m.getSys().justiceAt(i));
    }
    for (int i = 0; i < m.getEnv().justiceNum(); i++) {
      buchis.add(m.getEnv().justiceAt(i));
    }
    if (buchis.isEmpty()) {
      buchis.add(Env.TRUE());
    }
    boolean commonSat = commonGeneralizedBuchiRealizable(ini, trans, buchis);
    ini.free();
    trans.free();
    return commonSat;
  }

  /**
   * compute a cooperative solution to the generalized Buchi game
   * 
   * @param ini
   * @param trans
   * @param buchis
   * @return
   */
  private static boolean commonGeneralizedBuchiRealizable(BDD ini, BDD trans, List<BDD> buchis) {
    BDD Z = Env.TRUE();

    FixPoint zFix = new FixPoint(true);
    while (zFix.advance(Z)) {
      Z = Z.id();
      for (BDD buchi : buchis) {
        BDD start = buchi.id().andWith(Env.pred(trans, Z));
        BDD nextZ = Z.id().andWith(reachBwd(trans, start));
        Z.free();
        Z = nextZ;
        start.free();

        BDD iniWin = ini.and(Z);
        if (iniWin.isZero()) {
          iniWin.free();
          return false;
        }
        iniWin.free();
      }
    }
    return true;
  }

  /**
   * compute all backwards reachable states
   * 
   * @param trans
   * @param to
   * @return
   */
  public static BDD reachBwd(BDD trans, BDD to) {
    BDD attr = Env.FALSE(); // no states
    FixPoint f = new FixPoint(true);
    while (f.advance(attr)) {
      attr = to.id().orWith(Env.pred(trans, attr));
    }
    return attr;
  }

}
