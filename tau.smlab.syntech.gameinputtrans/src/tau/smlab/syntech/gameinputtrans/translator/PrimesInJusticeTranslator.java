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

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.model.Constraint.Kind;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;

/**
 * Replaces Justice asm and gar with primes by aux variables
 *
 */
public class PrimesInJusticeTranslator implements Translator {

  @Override
  public void translate(GameInput input) {
    for (Constraint c : input.getSys().getConstraints()) {
      if (c.isJustice() && containsPrimes(c.getSpec())) {
        c.setSpec(replacePrimes(c.getSpec(), c.getTraceId(), input));
      }
    }
    for (Constraint c : input.getEnv().getConstraints()) {
      if (c.isJustice() && containsPrimes(c.getSpec())) {
        c.setSpec(replacePrimes(c.getSpec(), c.getTraceId(), input));
      }
    }
  }

  /**
   * check whether the given spec contains references to primed variables or the prime operator
   * 
   * @param spec
   * @return
   */
  private boolean containsPrimes(Spec spec) {
    if (spec instanceof VariableReference) {
      return ((VariableReference) spec).getReferenceName().endsWith("'");
    } else if (spec instanceof SpecExp) {
      SpecExp e = ((SpecExp) spec);
      if (e.getOperator().equals(Operator.PRIME)) {
        return true;
      }
      for (Spec s : e.getChildren()) {
        if (containsPrimes(s)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Replaces a complete justice with primes by a new variable that is true iff the justice is satisfied
   * 
   * @param spec
   * @param traceId
   * @param input
   * @return
   */
  private Spec replacePrimes(Spec spec, int traceId, GameInput input) {

    // create new var
    String varName = "PRIMED_JUSTICE_" + traceId;
    Variable aux = new Variable(varName, new TypeDef());
    input.getAux().addVar(aux);

    VariableReference auxRef = new VariableReference(aux);
    VariableReference auxRefP = new VariableReference(aux, aux.getName() + "'");

    // initially the new justive var is false
    SpecExp iniSpec = new SpecExp(Operator.NOT, auxRef);
    Constraint ini = new Constraint(Kind.INI, iniSpec, null, traceId);
    input.getAux().addConstraint(ini);

    // the justice var is next true iff justice holds
    SpecExp safetySpec = new SpecExp(Operator.IFF, auxRefP, spec);
    Constraint safety = new Constraint(Kind.SAFETY, safetySpec, null, traceId);
    input.getAux().addConstraint(safety);

    return auxRef;
  }

}
