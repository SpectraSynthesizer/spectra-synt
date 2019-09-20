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

// BDDPairing.java, created Jan 29, 2003 9:50:57 PM by jwhaley
// Copyright (C) 2003 John Whaley
// Licensed under the terms of the GNU LGPL; see COPYING for details.
package net.sf.javabdd;

/**
 * Encodes a table of variable pairs.  This is used for replacing variables in a
 * BDD.
 * 
 * @author John Whaley
 * @version $Id: BDDPairing.java,v 1.2 2009/10/18 19:30:54 uid228351 Exp $
 */
public abstract class BDDPairing {

  /**
   * Returns true if this is a table of ADD pairs; otherwise, returns false;
   * 
   * @return true if this is a table of ADD pairs.
   */
    public boolean isADDPairing() {
      return false;
    }
    /**
     * Adds the pair (oldvar, newvar) to this table of pairs. This results in
     * oldvar being substituted with newvar in a call to BDD.replace().
     * 
     * Compare to bdd_setpair.
     */
    public abstract void set(int oldvar, int newvar);

    /**
     * Like set(), but with a whole list of pairs.
     * 
     * Compare to bdd_setpairs.
     */
    public void set(int[] oldvar, int[] newvar) {
        if (oldvar.length != newvar.length)
            throw new BDDException();

        for (int n = 0; n < oldvar.length; n++)
            this.set(oldvar[n], newvar[n]);
    }
    
    /**
     * Adds the pair (oldvar, newvar) to this table of pairs. This results in
     * oldvar being substituted with newvar in a call to bdd.replace().  The
     * variable oldvar is substituted with the BDD newvar.  The possibility to
     * substitute with any BDD as newvar is utilized in BDD.compose(), whereas
     * only the topmost variable in the BDD is used in BDD.replace().
     * <p>If this is a table of ADD pairs (as opposed to BDD pairs) then newvar must be an ADD, otherwise 
     * the behavior is undefined.</p>
     * 
     * Compare to bdd_setbddpair.
     */
    public abstract void set(int oldvar, BDD newvar);

    /**
     * Like set(), but with a whole list of pairs.
     * 
     * Compare to bdd_setbddpairs.
     */
    public void set(int[] oldvar, BDD[] newvar) {
        if (oldvar.length != newvar.length)
            throw new BDDException();

        for (int n = 0; n < newvar.length; n++)
            this.set(oldvar[n], newvar[n]);
    }
    
    /**
     * Defines each variable in the finite domain block p1 to be paired with the
     * corresponding variable in p2.
     * 
     * Compare to fdd_setpair.
     */
    public void set(BDDDomain p1, BDDDomain p2) {
        int[] ivar1 = p1.vars();
        int[] ivar2 = p2.vars();
        this.set(ivar1, ivar2);
    }

    /**
     * Like set(), but with a whole list of pairs.
     * 
     * Compare to fdd_setpairs.
     */
    public void set(BDDDomain[] p1, BDDDomain[] p2) {
        if (p1.length != p2.length)
            throw new BDDException();

        for (int n = 0; n < p1.length; n++)
            if (p1[n].varNum() != p2[n].varNum())
                throw new BDDException();

        for (int n = 0; n < p1.length; n++) {
            this.set(p1[n], p2[n]);
        }
    }

    /**
     * Resets this table of pairs by setting all substitutions to their default
     * values (that is, no change).
     * 
     * Compare to bdd_resetpair.
     */
    public abstract void reset();

}
