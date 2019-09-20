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

package tau.smlab.syntech.jtlv.lib;

import net.sf.javabdd.BDD;

/**
 * <p>
 * A template for fix-point using for loop.
 * </p>
 * 
 * @version {@value edu.wis.jtlv.env.Env#version}
 * @author yaniv sa'ar.
 * 
 */
public class FixPoint {

  private BDD previous;
  private boolean freeOnAdvance;

  /**
   * 
   * @param freeOnAdvance if set the advance method will free the BDD
   */
  public FixPoint(boolean freeOnAdvance) {
    previous = null;
    this.freeOnAdvance = freeOnAdvance;
  }

  /**
   * Given the element to fix-point on, determine whether it has changed since the last time a check was performed. If
   * it had changed, then another iteration is allowed.
   * 
   * <p>
   * <b>NOTE</b> that the fix-point is on the object's equality. If the Underlying current object is changed, then it
   * might affect this iterator. (e.g. if you are using BDD.*With API, then you are changing the object himself)
   * </p>
   * 
   * @param curr
   *          The current value of the element to perform fix-point upon.
   * @return true, if the element has changed since the last time, otherwise false.
   */
  public boolean advance(BDD curr) {
    if (previous != null) {
      if (curr.equals(previous)) {
        return false;
      }
      if (freeOnAdvance) {
        previous.free();
      }
    }
    previous = curr;
    return true;
  }

  public void free() {
    previous.free();
  }
}
