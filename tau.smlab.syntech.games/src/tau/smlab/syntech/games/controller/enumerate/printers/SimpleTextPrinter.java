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

package tau.smlab.syntech.games.controller.enumerate.printers;

import java.io.PrintStream;
import java.util.Vector;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.games.controller.enumerate.EnumStateI;
import tau.smlab.syntech.games.controller.enumerate.EnumStrategyI;
import tau.smlab.syntech.jtlv.Env;

public class SimpleTextPrinter implements EnumStrategyPrinter {

  @Override
  public void printController(PrintStream out, EnumStrategyI c) {
    for (int i = 0; i < c.numOfStates(); i++) {
      EnumStateI s = c.getState(i);
      if (s.isInitial())
        out.print("Initial ");
      BDD prn = s.getData();
      out.print("State " + i + " "
          + prn.toStringWithDomains(Env.stringer) + "\n");
      Vector<EnumStateI> succ = s.getSuccessors();
      if (succ.isEmpty()) {
        out.print("\tWith no successors.");
      } else {
        EnumStateI[] all_succ = new EnumStateI[succ.size()];
        succ.toArray(all_succ);
        out.print("\tWith successors : " + all_succ[0].getStateId());
        for (int j = 1; j < all_succ.length; j++)
          out.print(", " + all_succ[j].getStateId());
      }
      if (c.isCalcStats()) {
        out.print("\tDist From Ini: " + s.getDistFromIni());
      }
      out.println();
    }
  }

}
