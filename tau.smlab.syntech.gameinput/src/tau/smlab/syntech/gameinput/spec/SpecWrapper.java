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

package tau.smlab.syntech.gameinput.spec;

import java.util.List;

import tau.smlab.syntech.gameinput.model.Pattern;
import tau.smlab.syntech.gameinput.spec.Spec;

public class SpecWrapper {

  private Spec spec;
  private boolean hasPatternReference;
  private Pattern pattern;
  private List<Spec> parameters;

  
  /**
   * Use this constructor to create a regular spec (without pattern reference)
   * @param spec
   */
  public SpecWrapper(Spec spec)
  {
    this.spec = spec;
    this.hasPatternReference = false;
    this.pattern = null;
    this.parameters = null;
  }
  
  /**
   * Use this constructor to create a spec that describes a pattern reference
   * @param pattern
   * @param parameters the passed parameters to the pattern (must be a boolean expression)
   */
  public SpecWrapper(Pattern pattern, List<Spec> parameters)
  {
    this.spec = null;
    this.hasPatternReference = true;
    this.pattern = pattern;
    this.parameters = parameters;
  }
  
  public Spec getSpec() {
    return spec;
  }

  public boolean isHasPatternReference() {
    return hasPatternReference;
  }

  public Pattern getPattern() {
    return pattern;
  }

  public List<Spec> getParameters() {
    return parameters;
  }
}
