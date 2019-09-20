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

package tau.smlab.syntech.gameinputtrans.translator;

import java.util.ArrayList;
import java.util.List;

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.Constraint.Kind;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Monitor;
import tau.smlab.syntech.gameinput.model.Pattern;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;

/**
 * iterate over all state invariants (in guarantees, assumptions, monitors, and patterns)
 * 
 * state invariant is translated to initial an next() version safety constraint
 * 
 * finally delete the state invariants
 *
 */

public class StateInvTranslator implements Translator {

  @Override
  public void translate(GameInput input) {

    // guarantees
    replaceStateInv(input.getSys().getConstraints());
    // assumptions
    replaceStateInv(input.getEnv().getConstraints());

    // patterns
    for (Pattern patt : input.getPatterns()) {
      replaceStateInv(patt.getExpressions());
    }
    // monitors
    for (Monitor mon : input.getMonitors()) {
      replaceStateInv(mon.getExpressions());
    }

  }

  private void replaceStateInv(List<Constraint> cons) {
    List<Constraint> stateInvs = new ArrayList<Constraint>();
    List<Constraint> translated = new ArrayList<Constraint>();
    
    for (Constraint c : cons) {
      if (Kind.STATE_INV.equals(c.getKind())) {
        stateInvs.add(c);
        Constraint ini = new Constraint(Kind.INI, c.getSpec(), c.getName(), c.getTraceId());
        translated.add(ini);
        Spec clone = null;
        try {
          clone = c.getSpec().clone();
        } catch (CloneNotSupportedException e) {
          throw new RuntimeException(e);
        }
        Constraint safety = new Constraint(Kind.SAFETY, new SpecExp(Operator.PRIME, clone), c.getName(), c.getTraceId());
        translated.add(safety);
      }
    }
    
    cons.removeAll(stateInvs);
    cons.addAll(translated);
  }

}
