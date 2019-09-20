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

package tau.smlab.syntech.gameinput.model;

import tau.smlab.syntech.gameinput.spec.Spec;
// import tau.smlab.syntech.spectra.TemporalExpression;

/**
 * Represents a step of a regular expression.
 * Each step is constructed of two elements:
 * (1) A specification that may contain boolean and past-temporal operators.
 * (2) Whether the step has the Kleene-star operator (*).
 *
 */
public class SpecRegExpStep {
	
  public enum Quantifier {
	  
	  ONCE,
	  ZERO_OR_MORE,
	  ZERO_OR_ONCE,
	  AT_LEAST_ONCE,
	  EXACT_REPETITION,
	  AT_LEAST,
	  RANGE
  };
  
  private Spec spec;
  private Quantifier quantifierType;
  private int from;
  private int to;
  
  
  
  
  
  
  /* constructor for the once , zero or more (*) , question mark (?) and plus (+)
   * quantifier types */
  public SpecRegExpStep(Spec spec, Quantifier quantifier)
  {
	    this.spec = spec;
	    this.quantifierType = quantifier;
	    this.from = -1;
	    this.to = -1;
  }
  
  /* constructor for the exact repetition ({n}) and at least ({n,}) 
   * quantifier types */
  public SpecRegExpStep(Spec spec, Quantifier quantifier, int from)
  {
	    this.spec = spec;
	    this.quantifierType = quantifier;
	    this.from = from;
	    this.to = -1;
  }
  
  /* constructor for the range ({n,m}) quantifier type */
  public SpecRegExpStep(Spec spec, Quantifier quantifier, int from , int to)
  {
    this.spec = spec;
    this.quantifierType = quantifier;
    this.from = from;
    this.to = to;
  }
  
  public Spec getSpec() {
    return this.spec;
  }
  
  public void setSpec(Spec spec) {
	  this.spec = spec;
  }
  
  public Quantifier getQuantifier() {
	  
	  return this.quantifierType;
  }
  
  public int getFrom() {
	  
	  return this.from;
  }
  
  public int getTo() {
	  
	  return this.to;
  }
}
