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

import tau.smlab.syntech.gameinput.model.GameInput;

public class DefaultTranslators {
  public static List<Translator> getDefaultTranslators() {
    List<Translator> ts = new ArrayList<Translator>();
    ts.add(new MinMaxFunctionsTranslator());
    ts.add(new TemporalInTranslator());
    ts.add(new ArrayFunctionsTranslator());
    ts.add(new TemporalRegexpTranslator());
    ts.add(new QuantifierTranslator()); //The translator of QuantifiedSpecs
    ts.add(new StateInvTranslator(true));
    ts.add(new PredicateInstanceTranslator());
    ts.add(new PatternConstraintTranslator());
    ts.add(new MonitorTranslator());
    ts.add(new CounterTranslator());
    ts.add(new VarIndexesTranslator()); //The translator of all the complex indexes in arrays
    ts.add(new DefinesTranslator());
    ts.add(new PastLTLTranslator());
    ts.add(new StateInvTranslator(false)); // second part of translator to be aware of unfolded primes
    ts.add(new PrimesTranslator());
    ts.add(new PrimesInJusticeTranslator());
    return ts;
  }
  
}
