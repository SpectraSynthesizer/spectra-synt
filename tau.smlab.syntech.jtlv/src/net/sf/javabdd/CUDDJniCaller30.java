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

package net.sf.javabdd;

import net.sf.javabdd.BDDFactory.ReorderMethod;

public class CUDDJniCaller30 implements CUDDCaller {
	
	private static final String CUDD_LIB_3_0 = "cudd";
	
	private long info = 0;   // CUDD manager
	private long zero = 0;      // constant CUDD BDD (logical) zero
	private long one = 0;       // constant CUDD BDD and ADD one
	private long addZero = 0; // constant CUDD ADD (arithmetic) zero
	
	private static native void registerNatives(); 
	
	private native void initialize0(int nodenum, int cachesize);
	private native boolean isInitialized0();
	private native void done0();
	private native int varNum0();
	private native int setVarNum0(int num, boolean ADD);
	private native long ithVar0(int var, boolean ADD);
	private native int level2Var0(int level);
	private native int var2Level0(int var);
	private native void setVarOrder0(int[] neworder);
	private native int getAllocNum0();
	private native int getNodeNum0();
	private native int getCacheSize0();
	private native int var0(long b);
	private native long high0(long b, boolean ADD);
	private native long low0(long b, boolean ADD);
	private native long not0(long b, boolean ADD);
	private native long ite0(long b, long c, long d, boolean ADD);
	private native long relprod0(long b, long c, long d);
	private native long compose0(long b, long c, int var, boolean ADD);
	private native long exist0(long b, long c, boolean ADD);
	private native long forAll0(long b, long c, boolean ADD);
	private native long restrict0(long b, long var);
	private native long restrictWith0(long b, long c, boolean deref_other);
	private native long simplify0(long b, long var, boolean ADD);
	private native long support0(long b, boolean is_add);
	private native long apply0(long b, long c, int opr, boolean ADD, boolean apply_with, boolean deref_other);
	private native long satOne0(long b, long c, boolean ADD);
	private native int nodeCount0(long b);
	private native double pathCount0(long b);
	private native double satCount0(long b);
	private native void addRef(long p);
	private native void delRef(long p, boolean ADD);
	private native long veccompose0(long b, long p, boolean ADD);
	private native long replace0(long b, long p, boolean ADD);
	private native long alloc(boolean ADD);
	private native void set0(long p, int oldvar, int newvar, boolean ADD);
	private native void set2(long p, int oldvar, long newbdd);
	private native void reset0(long ptr, boolean ADD);
	private native void free0(long ptr);
	private native boolean isZeroOneADD0(long ptr);
	private native long addConst0(double constval);
	private native boolean isAddConst0(long ptr);
	private native long addFindMax0(long ptr);
	private native long addFindMin0(long ptr);
	private native double retrieveConstValue0(long ptr);
	private native long  addAdditiveNeg0(long ptr);
	private native long  addApplyLog0(long ptr);
	private native void  reorder0(ReorderMethod method);
	private native void  autoReorder0(ReorderMethod method, boolean setmax, int maxval);
	private native ReorderMethod  getreordermethod0();
	private native void  autoReorder1();
	private native int  reorderVerbose0(int v);
	private native void  addVarBlock0(int first, int last, boolean fixed);
	private native void  clearVarBlocks0();
	private native void  printStat0();
	private native long  toADD0(long b);
	private native  long  toBDD0(long b);
	private native  long  toBDDThreshold(long b, double threshold);
	private native  long  toBDDStrictThreshold(long b, double threshold);
	private native  long  toBDDInterval(long b, double lower, double upper);
	private native  long  toBDDIthBit(long b, int bit);
	private native int[] varSupportIndex0(long b);
	private native void  printSet0(long b, int printMode);
	private native long logicZero0();
	private native long arithmeticZero0();
	private native long arithmeticLogicOne0();
	private native long arithmeticPlusInfinity0();
	private native long arithmeticMinusInfinity0();
	private native long replaceWith0(long b, long c, boolean ADD);
	private native long addMinimizeVal0(long ptr, double val);
	private native long addAbstractMin0(long b, long c);
	private native long addAbstractMax0(long b, long c);
	private native int reorderTimes0();
	private native void printVarTree0();
	private native long addEngSubtract0(long a, long b, double maxEnergy);
	private native long convertToZeroOneADDByThres0(long a, long opType, double thresValue);
	private native void printDot0(long a);
	private native long arithmeticExist0(long b, long c);
	private native long determinizeController0(long b, long c);
	private native int getSize0(long b);
	private native boolean reorderEnabled0();
	
	static {
		System.loadLibrary(CUDD_LIB_3_0);
		registerNatives();
	}
	
	@Override
	public void initialize0Caller(int nodenum, int cachesize) {
		initialize0(nodenum, cachesize);

	}

	@Override
	public boolean isInitialized0Caller() {
		return isInitialized0();
	}

	@Override
	public void done0Caller() {
		done0();

	}

	@Override
	public int varNum0Caller() {
		return varNum0();
	}

	@Override
	public int setVarNum0Caller(int num, boolean ADD) {
		return setVarNum0(num, ADD);
	}

	@Override
	public long ithVar0Caller(int var, boolean ADD) {
		return ithVar0(var, ADD);
	}

	@Override
	public int level2Var0Caller(int level) {
		return level2Var0(level);
	}

	@Override
	public int var2Level0Caller(int var) {
		return var2Level0(var);
	}

	@Override
	public void setVarOrder0Caller(int[] neworder) {
		setVarOrder0(neworder);

	}

	@Override
	public int getAllocNum0Caller() {
		return getAllocNum0();
	}

	@Override
	public int getNodeNum0Caller() {
		return getNodeNum0();
	}

	@Override
	public int getCacheSize0Caller() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public int var0Caller(long b) {
		return var0(b);
	}

	@Override
	public long high0Caller(long b, boolean ADD) {
		return high0(b, ADD);
	}

	@Override
	public long low0Caller(long b, boolean ADD) {
		return low0(b, ADD);
	}

	@Override
	public long not0Caller(long b, boolean ADD) {
		return not0(b, ADD);
	}

	@Override
	public long ite0Caller(long b, long c, long d, boolean ADD) {
		return ite0(b, c, d, ADD);
	}

	@Override
	public long relprod0Caller(long b, long c, long d) {
		return relprod0(b, c, d);
	}

	@Override
	public long compose0Caller(long b, long c, int var, boolean ADD) {
		return compose0(b, c, var, ADD);
	}

	@Override
	public long exist0Caller(long b, long c, boolean ADD) {
		return exist0(b, c, ADD);
	}

	@Override
	public long forAll0Caller(long b, long c, boolean ADD) {
		return forAll0(b, c, ADD);
	}

	@Override
	public long restrict0Caller(long b, long var) {
		 return restrict0(b, var);
	}

	@Override
	public long restrictWith0Caller(long b, long c, boolean deref_other) {
		return restrictWith0(b, c, deref_other);
	}

	@Override
	public long simplify0Caller(long b, long var, boolean ADD) {
		return simplify0(b, var, ADD);
	}

	@Override
	public long support0Caller(long b, boolean is_add) {
		return support0(b, is_add);
	}

	@Override
	public long apply0Caller(long b, long c, int opr, boolean ADD, boolean apply_with, boolean deref_other) {
		return apply0(b, c, opr, ADD, apply_with, deref_other);
	}

	@Override
	public long satOne0Caller(long b, long c, boolean ADD) {
		return satOne0(b, c, ADD);
	}

	@Override
	public int nodeCount0Caller(long b) {
		return nodeCount0(b);
	}

	@Override
	public double pathCount0Caller(long b) {
		return pathCount0(b);
	}

	@Override
	public double satCount0Caller(long b) {
		return satCount0(b);
	}

	@Override
	public void addRefCaller(long p) {
		addRef(p);

	}

	@Override
	public void delRefCaller(long p, boolean ADD) {
		delRef(p, ADD);

	}

	@Override
	public long veccompose0Caller(long b, long p, boolean ADD) {
		return veccompose0(b, p, ADD);
	}

	@Override
	public long replace0Caller(long b, long p, boolean ADD) {
		return replace0(b, p, ADD);
	}

	@Override
	public long allocCaller(boolean ADD) {
		return alloc(ADD);
	}

	@Override
	public void set0Caller(long p, int oldvar, int newvar, boolean ADD) {
		set0(p, oldvar, newvar, ADD);

	}

	@Override
	public void set2Caller(long p, int oldvar, long newbdd) {
		set2(p, oldvar, newbdd);

	}

	@Override
	public void reset0Caller(long ptr, boolean ADD) {
		reset0(ptr, ADD);

	}

	@Override
	public void free0Caller(long ptr) {
		free0(ptr);

	}

	@Override
	public boolean isZeroOneADD0Caller(long ptr) {
		return isZeroOneADD0(ptr);
	}

	@Override
	public long addConst0Caller(double constval) {
		return addConst0(constval);
	}

	@Override
	public boolean isAddConst0Caller(long ptr) {
		return isAddConst0(ptr);
	}

	@Override
	public long addFindMax0Caller(long ptr) {
		return addFindMax0(ptr);
	}

	@Override
	public long addFindMin0Caller(long ptr) {
		return addFindMin0(ptr);
	}

	@Override
	public double retrieveConstValue0Caller(long ptr) {
		return retrieveConstValue0(ptr);
	}

	@Override
	public long addAdditiveNeg0Caller(long ptr) {
		return addAdditiveNeg0(ptr);
	}

	@Override
	public long addApplyLog0Caller(long ptr) {
		return addApplyLog0(ptr);
	}

	@Override
	public void reorder0Caller(ReorderMethod method) {
		reorder0(method);

	}

	@Override
	public void autoReorder0Caller(ReorderMethod method, boolean setmax, int maxval) {
		autoReorder0(method, setmax, maxval);

	}

	@Override
	public ReorderMethod getreordermethod0Caller() {
		return getreordermethod0();
	}

	@Override
	public void autoReorder1Caller() {
		autoReorder1();

	}

	@Override
	public int reorderVerbose0Caller(int v) {
		return reorderVerbose0(v);
	}

	@Override
	public void addVarBlock0Caller(int first, int last, boolean fixed) {
		addVarBlock0(first, last, fixed);

	}

	@Override
	public void clearVarBlocks0Caller() {
		clearVarBlocks0();

	}

	@Override
	public void printStat0Caller() {
		printStat0();

	}

	@Override
	public long toADD0Caller(long b) {
		return toADD0(b);
	}

	@Override
	public long toBDD0Caller(long b) {
		return toBDD0(b);
	}

	@Override
	public long toBDDThresholdCaller(long b, double threshold) {
		return toBDDThreshold(b, threshold);
	}

	@Override
	public long toBDDStrictThresholdCaller(long b, double threshold) {
		return toBDDStrictThreshold(b, threshold);
	}

	@Override
	public long toBDDIntervalCaller(long b, double lower, double upper) {
		return toBDDInterval(b, lower, upper);
	}

	@Override
	public long toBDDIthBitCaller(long b, int bit) {
		return toBDDIthBit(b, bit);
	}

	@Override
	public int[] varSupportIndex0Caller(long b) {
		return varSupportIndex0(b);
	}

	@Override
	public void printSet0Caller(long b, int printMode) {
		printSet0(b, printMode);

	}

	@Override
	public long logicZero0Caller() {
		return logicZero0();
	}

	@Override
	public long arithmeticZero0Caller() {
		return arithmeticZero0();
	}

	@Override
	public long arithmeticLogicOne0Caller() {
		return arithmeticLogicOne0();
	}

	@Override
	public long arithmeticPlusInfinity0Caller() {
		return arithmeticPlusInfinity0();
	}

	@Override
	public long arithmeticMinusInfinity0Caller() {
		return arithmeticMinusInfinity0();
	}

	@Override
	public long replaceWith0Caller(long b, long c, boolean ADD) {
		return replaceWith0(b, c, ADD);
	}

	@Override
	public long addMinimizeVal0Caller(long ptr, double val) {
		return addMinimizeVal0(ptr, val);
	}

	@Override
	public long addAbstractMin0Caller(long b, long c) {
		return addAbstractMin0(b, c);
	}

	@Override
	public long addAbstractMax0Caller(long b, long c) {
		return addAbstractMax0(b, c);
	}

	@Override
	public int reorderTimes0Caller() {
		return reorderTimes0();
	}

	@Override
	public void printVarTree0Caller() {
		printVarTree0();

	}

	@Override
	public long addEngSubtract0Caller(long a, long b, double maxEnergy) {
		return addEngSubtract0(a, b, maxEnergy);
	}

	@Override
	public long convertToZeroOneADDByThres0Caller(long a, long opType, double thresValue) {
		return convertToZeroOneADDByThres0(a, opType, thresValue);
	}

	@Override
	public void printDot0Caller(long a) {
		printDot0(a);

	}

	@Override
	public long arithmeticExist0Caller(long b, long c) {
		return arithmeticExist0(b, c);
	}

	@Override
	public long determinizeController0Caller(long b, long c) {
		return determinizeController0(b, c);
	}
	
	@Override
	public boolean reorderEnabled0Caller() {
	  return reorderEnabled0();
	}

	@Override
	public int getSize0Caller(long b) {
		return getSize0(b);
	}
		
  @Override
  public long getBddZero() {
    return this.zero;
  }
  @Override
  public long getAddZero() {
    return this.addZero;
  }
  @Override
  public long getManager() {
    return this.info;
  }
  @Override
  public long getOne() {
    return this.one;
  }
}