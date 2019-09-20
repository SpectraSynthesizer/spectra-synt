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

// BDDVarSet.java, created Jul 13, 2006 8:53:13 PM by jwhaley
// Copyright (C) 2004-2006 John Whaley <jwhaley@alum.mit.edu>
// Licensed under the terms of the GNU LGPL; see COPYING for details.
package net.sf.javabdd;


/**
 * <p>Some BDD methods, namely <tt>exist()</tt>, <tt>forall()</tt>, <tt>unique()</tt>, 
 * <tt>relprod()</tt>, <tt>applyAll()</tt>, <tt>applyEx()</tt>, <tt>applyUni()</tt>, 
 * and <tt>satCount()</tt> take a BDDVarSet argument.</p>
 * 
 * @author jwhaley
 * @version $Id: BDDVarSet.java,v 1.2 2009/10/18 19:30:54 uid228351 Exp $
 */
public abstract class BDDVarSet {

  /**
   * <p>Returns the factory that created this BDDVarSet.</p>
   * 
   * @return factory that created this BDDVarSet
   */
  public abstract BDDFactory getFactory();

  public abstract BDD toBDD();

  public abstract BDDVarSet id();
  public abstract void free();

  public abstract int size();
  public abstract boolean isEmpty();

  public abstract int[] toArray();
  public abstract int[] toLevelArray();

  public String toString() {
    //return Arrays.toString(toArray());
    int[] a = toArray();
    StringBuffer sb = new StringBuffer(a.length * 4 + 2);
    sb.append('[');
    for (int i = 0; i < a.length; ++i) {
      if (i != 0) sb.append(',');
      sb.append(a[i]);
    }
    sb.append(']');
    return sb.toString();
  }

  /**
   * <p>Scans this BDD and copies the stored variables into an array of BDDDomains.
   * The domains returned are guaranteed to be in ascending order.</p>
   * 
   * <p>Compare to fdd_scanset.</p>
   * 
   * @return int[]
   */
  public BDDDomain[] getDomains() {
    int[] fv;
    BDDDomain[] varset;
    int fn;
    int num, n, m, i;

    fv = this.toArray();
    fn = fv.length;

    BDDFactory factory = getFactory();

    for (n = 0, num = 0; n < factory.numberOfDomains(); n++) {
      BDDDomain dom = factory.getDomain(n);
      int[] ivar = dom.vars();
      boolean found = false;
      for (m = 0; m < dom.varNum() && !found; m++) {
        for (i = 0; i < fn && !found; i++) {
          if (ivar[m] == fv[i]) {
            num++;
            found = true;
          }
        }
      }
    }

    varset = new BDDDomain[num];

    for (n = 0, num = 0; n < factory.numberOfDomains(); n++) {
      BDDDomain dom = factory.getDomain(n);
      int[] ivar = dom.vars();
      boolean found = false;
      for (m = 0; m < dom.varNum() && !found; m++) {
        for (i = 0; i < fn && !found; i++) {
          if (ivar[m] == fv[i]) {
            varset[num++] = dom;
            found = true;
          }
        }
      }
    }

    return varset;
  }

  /**
   * <p>Returns a new BDDVarSet that is the union of the current BDDVarSet
   * and the given BDDVarSet.  This constructs a new set; neither the current
   * nor the given BDDVarSet is modified.</p>
   * 
   * @param b  BDDVarSet to union with
   * @return  a new BDDVarSet that is the union of the two sets
   */
  public abstract BDDVarSet union(BDDVarSet b);

  /**
   * <p>Returns a new BDDVarSet that is the union of the current BDDVarSet
   * and the given variable.  This constructs a new set; the current BDDVarSet
   * is not modified.</p>
   * 
   * @param b  variable to add to set
   * @return  a new BDDVarSet that includes the given variable
   */
  public abstract BDDVarSet union(int var);

  /**
   * <p>Modifies this BDDVarSet to include all of the vars in the given set.
   * This modifies the current set in place and consumes the given set.</p>
   * 
   * @param b  BDDVarSet to union in
   * @return this
   */
  public abstract BDDVarSet unionWith(BDDVarSet b);

  /**
   * <p>Modifies this BDDVarSet to include the given variable.  This modifies
   * the current set in place.</p>
   * 
   * @param b  variable to add to set
   * @return this
   */
  public abstract BDDVarSet unionWith(int var);

  /**
   * <p>Returns a new BDDVarSet that is the union of the current BDDVarSet
   * and the given BDDVarSet.  This constructs a new set; neither the current
   * nor the given BDDVarSet is modified.</p>
   * 
   * @param b  BDDVarSet to union with
   * @return  a new BDDVarSet that is the union of the two sets
   */
  public BDDVarSet intersect(BDDVarSet b) {
    BDDVarSet res = this.getFactory().emptySet();
    BDDDomain[] aDoms = this.getDomains();
    BDDDomain[] bDoms = b.getDomains();
    for (int i = 0; i < aDoms.length; i++) {
      for (int j = 0; j < bDoms.length; j++) {
        if (aDoms[i].getIndex() == bDoms[j].getIndex()) {
          res = res.unionWith(aDoms[i].set());
          break;
        }
      }
    }
    return res;
  }
  
  /**
   * <p>Modifies this BDDVarSet to include all of the vars in the given set.
   * This modifies the current set in place and consumes the given set.</p>
   * 
   * @param b  BDDVarSet to union in
   * @return this
   */
  public abstract BDDVarSet intersectWith(BDDVarSet b);
  
  /**
   * <p>Returns a new BDDVarSet that is the remainder when removing the 
   * variables in the given BDDVarSet from the current BDDVarSet. 
   * This constructs a new set; neither the current
   * nor the given BDDVarSet is modified.</p>
   * 
   * @param b  BDDVarSet to remove
   * @return  a new BDDVarSet that is current setminus given
   */
  public BDDVarSet minus(BDDVarSet b) {
    BDDVarSet res = this.getFactory().emptySet();
    BDDDomain[] aDoms = this.getDomains();
    BDDDomain[] bDoms = b.getDomains();
    for (int i = 0; i < aDoms.length; i++) {
      boolean notInSecond = true;
      for (int j = 0; j < bDoms.length; j++) {
        if (aDoms[i].getIndex() == bDoms[j].getIndex()) {
          notInSecond = false;
          break; 
        }
      }
      if (notInSecond) {
        res = res.unionWith(aDoms[i].set());
      }
    }
    return res;
  }
  
  /* (non-Javadoc)
   * @see java.lang.Object#hashCode()
   */
  public abstract int hashCode();

  /**
   * Returns true if the sets are equal.
   * 
   * @param that other set
   * @return true if the sets are equal
   */
  public abstract boolean equals(BDDVarSet that);

  /* (non-Javadoc)
   * @see java.lang.Object#equals(java.lang.Object)
   */
  public final boolean equals(Object o) {
    if (o instanceof BDDVarSet)
      return equals((BDDVarSet) o);
    return false;
  }

  /** 
   * Default implementation of BDDVarSet based on BDDs.
   * 
   * @author jwhaley
   * @version $Id: BDDVarSet.java,v 1.2 2009/10/18 19:30:54 uid228351 Exp $
   */
  public static class DefaultImpl extends BDDVarSet {

    /**
     * BDD representation of the set of variables.
     * Treated like a linked list of variables.
     */
    protected BDD b;

    /**
     * Construct a BDDVarSet backed by the given BDD.
     * Ownership of the given BDD is transferred to this BDDVarSet,
     * so you should not touch it after construction!
     * 
     * @param b BDD to use in constructing BDDVarSet
     */
    public DefaultImpl(BDD b) {
      this.b = b;
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#free()
     */
    public void free() {
      if (b != null) {
        b.free();
        b = null;
      }
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#getFactory()
     */
    public BDDFactory getFactory() {
      return b.getFactory();
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#id()
     */
    public BDDVarSet id() {
      return new DefaultImpl(b.id());
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#intersectWith(net.sf.javabdd.BDDVarSet)
     */
    public BDDVarSet intersectWith(BDDVarSet s) {
      DefaultImpl i = (DefaultImpl) s;
      DefaultImpl res = (DefaultImpl) this.intersect(i);
      this.b.free();
      this.b = res.b.id();
      res.free();
      if(this != i) {
        i.free();
      }
      //DefaultImpl i = (DefaultImpl) s;
      //b.orWith(i.b); i.b = null;
      return this;
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#isEmpty()
     */
    public boolean isEmpty() {
      return b.isOne();
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#size()
     */
    public int size() {
      int result = 0;
      BDD r = b.id();
      while (!r.isOne()) {
        if (r.isZero()) throw new BDDException("varset contains zero");
        ++result;
        BDD q = r.high();
        r.free();
        r = q;
      }
      r.free();
      return result;
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#toArray()
     */
    public int[] toArray() {
      int[] result = new int[size()];
      int i = -1;
      BDD r = b.id();
      while (!r.isOne()) {
        if (r.isZero()) throw new BDDException("varset contains zero");
        result[++i] = r.var();
        BDD q = r.high();
        r.free();
        r = q;
      }
      r.free();
      return result;
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#toBDD()
     */
    public BDD toBDD() {
      return b.id();
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#toLevelArray()
     */
    public int[] toLevelArray() {
      int[] result = new int[size()];
      int i = -1;
      BDD r = b.id();
      while (!r.isOne()) {
        if (r.isZero()) throw new BDDException("varset contains zero");
        result[++i] = r.level();
        BDD q = r.high();
        r.free();
        r = q;
      }
      r.free();
      return result;
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#union(net.sf.javabdd.BDDVarSet)
     */
    public BDDVarSet union(BDDVarSet s) {
      DefaultImpl i = (DefaultImpl) s;
      return new DefaultImpl(b.and(i.b));
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#union(int)
     */
    public BDDVarSet union(int var) {
      BDD ith = b.getFactory().ithVar(var);
      DefaultImpl j = new DefaultImpl(b.and(ith));
      ith.free();
      return j;
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#unionWith(net.sf.javabdd.BDDVarSet)
     */
    public BDDVarSet unionWith(BDDVarSet s) {
      DefaultImpl i = (DefaultImpl) s;
      b.andWith(i.b); i.b = null;
      return this;
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#unionWith(int)
     */
    public BDDVarSet unionWith(int var) {
      b.andWith(b.getFactory().ithVar(var));
      return this;
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#hashCode()
     */
    public int hashCode() {
      return b.hashCode();
    }

    /* (non-Javadoc)
     * @see net.sf.javabdd.BDDVarSet#equals(net.sf.javabdd.BDDVarSet)
     */
    public boolean equals(BDDVarSet s) {
      if (s instanceof DefaultImpl)
        return equals((DefaultImpl) s);
      return false;
    }

    public boolean equals(DefaultImpl s) {
      return b.equals(s.b);
    }

  }

}
