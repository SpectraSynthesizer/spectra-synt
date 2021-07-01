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

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;

/**
 * Specification encapsulating a BDD.
 */
public class SpecBDD implements Spec {
  private BDD val = null;
  private String identifying_expr = "";

  /**
   * <p>
   * A constructor for a leaf BDD specification, without a string representation.
   * </p>
   * 
   * @param v
   *          The evaluated BDD.
   */
  public SpecBDD(BDD v) {
    this.val = v;
  }

  /**
   * <p>
   * Getter for the BDD value.
   * </p>
   * 
   * @return The BDD value.
   * 
   * @see SpecBDD#toBDD()
   */
  public BDD getVal() {
    return this.val;
  }

  @Override
  public boolean isPastLTLSpec() {
    return false;
  }

  @Override
  public boolean isPropSpec() {
    return true;
  }

  @Override
  public boolean hasTemporalOperators() {
    return false;
  }

  @Override
  public String toString() {
    if (this.getVal() == null)
      if (this.identifying_expr != "")
        return "\"" + this.identifying_expr + "\"";
      else
        return "[!$#! Cannot Identify BDD Expression]";
    return "#[" + Env.toNiceSignleLineString(val) + "]";
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SpecBDD)) {
      return false;
    }
    SpecBDD otherBDD = (SpecBDD) other;
    if ((this.val == null) || (otherBDD.val == null)) {
      return false;
    }
    return this.val.equals(otherBDD.val);
  }

  @Override
  public SpecBDD clone() throws CloneNotSupportedException {
    return new SpecBDD(this.getVal().id());
  }
  
  public boolean isOne() {
    return val.isOne();
  }
  public boolean isZero() {
    return val.isZero();
  }
}
