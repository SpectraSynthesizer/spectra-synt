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
import tau.smlab.syntech.gameinput.spec.VariableReference;

/**
 * This translator must run twice, once before most translators to replace alw
 * with G.
 * 
 * Then again after most translators to check whether any previous alw is a
 * state invariant, i.e., has no primes. The state invariants in a former alw
 * are translated to an initial and a safety constraint.
 * 
 * Operates on traceIds in case other translators change object identities of
 * constraints
 *
 * Uses the operator PRIME internally, i.e., needs to run before
 * RemovePrimesTranslator.
 */
public class StateInvTranslator implements Translator {
  /**
   * translators first or second run
   */
  private boolean before;
  /**
   * constraints that are potential state invariants (or use of always instead of
   * G)
   */
  private static List<Integer> potentialStateInvIDs;
  private GameInput input;

  public StateInvTranslator(boolean before) {
    this.before = before;
    if (before) {
      potentialStateInvIDs = new ArrayList<>();
    }
  }

  @Override
  public void translate(GameInput input) {
    this.input = input;
    // guarantees
    replaceStateInv(input.getSys().getConstraints());
    // assumptions
    replaceStateInv(input.getEnv().getConstraints(), true);
    // auxiliary created by other translators
    replaceStateInv(input.getAux().getConstraints());

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
    replaceStateInv(cons, false);
  }

  private void replaceStateInv(List<Constraint> cons, boolean assumption) {
    List<Constraint> stateInvs = new ArrayList<Constraint>();
    List<Constraint> translated = new ArrayList<Constraint>();

    for (Constraint c : cons) {
      if (Kind.STATE_INV.equals(c.getKind())) {
        // must be before; we simply change alw to G
        potentialStateInvIDs.add(c.getTraceId());
        translated.add(new Constraint(Kind.SAFETY, c.getSpec(), c.getName(), c.getTraceId()));
        stateInvs.add(c);
      } else if (!before && Kind.SAFETY.equals(c.getKind())) {
        // after most other translators check for primes and translate true state
        // invariants
        if (!(assumption && containsSysVarRef(c.getSpec()))) {
          if (potentialStateInvIDs.contains(c.getTraceId()) && !containsNextOrPrime(c.getSpec())) {
            stateInvs.add(c);
            Constraint ini = new Constraint(Kind.INI, c.getSpec(), c.getName(), c.getTraceId());
            translated.add(ini);
            Spec clone = null;
            try {
              clone = c.getSpec().clone();
            } catch (CloneNotSupportedException e) {
              throw new RuntimeException(e);
            }
            Constraint safety = null;
            if ((assumption && containsSysVarRef(c.getSpec()) || (!assumption && containsEnvVarRef(c.getSpec())))){
              safety = new Constraint(Kind.SAFETY, clone, c.getName(), c.getTraceId());
            } else {
              safety = new Constraint(Kind.SAFETY, new SpecExp(Operator.PRIME, clone), c.getName(), c.getTraceId());
            }
            translated.add(safety);
          }
        }
      }
    }

    cons.removeAll(stateInvs);
    cons.addAll(translated);
  }

  /**
   * check whether the spec references a variable of the system module or aux
   * module
   * 
   * @param spec
   * @return
   */
  private boolean containsSysVarRef(Spec spec) {
    if (spec instanceof SpecExp) {
      SpecExp e = (SpecExp) spec;
      for (int i = 0; i < e.getChildren().length; i++) {
        if (containsSysVarRef(e.getChildren()[i])) {
          return true;
        }
      }
    } else if (spec instanceof VariableReference) {
      VariableReference v = (VariableReference) spec;
      if (input.getSys().getVars().contains(v.getVariable()) || input.getAux().getVars().contains(v.getVariable())) {
        return true;
      }
    }
    return false;
  }
  
  /**
   * check whether the spec references a variable of the environment module
   * 
   * @param spec
   * @return
   */
  private boolean containsEnvVarRef(Spec spec) {
    if (spec instanceof SpecExp) {
      SpecExp e = (SpecExp) spec;
      for (int i = 0; i < e.getChildren().length; i++) {
        if (containsEnvVarRef(e.getChildren()[i])) {
          return true;
        }
      }
    } else if (spec instanceof VariableReference) {
      VariableReference v = (VariableReference) spec;
      if (input.getEnv().getVars().contains(v.getVariable())) {
        return true;
      }
    }
    return false;
  }

  /**
   * recursively checks SpecExp or VariableReferences for primes
   * 
   * @param spec
   * @return
   */
  private boolean containsNextOrPrime(Spec spec) {
    if (spec instanceof SpecExp) {
      SpecExp e = (SpecExp) spec;
      if (Operator.PRIME.equals(e.getOperator())) {
        return true;
      } else {
        for (int i = 0; i < e.getChildren().length; i++) {
          if (containsNextOrPrime(e.getChildren()[i])) {
            return true;
          }
        }
      }
    } else if (spec instanceof VariableReference) {
      VariableReference v = (VariableReference) spec;
      if (v.getReferenceName().endsWith("'")) {
        return true;
      }
    }
    return false;
  }

}
