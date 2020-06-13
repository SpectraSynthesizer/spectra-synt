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

// CUDDFactory.java, created Jan 29, 2003 9:50:57 PM by jwhaley
// Copyright (C) 2003 John Whaley
// Licensed under the terms of the GNU LGPL; see COPYING for details.

/* modified to allow multiple managers -- not thread-safe. 
   - Kedar Namjoshi, August 2009
 */

package net.sf.javabdd;

import java.math.BigInteger;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import tau.smlab.syntech.jtlv.Env;

/**
 * <p>
 * An implementation of BDDFactory that relies on the CUDD library through a
 * native interface. You can use this by calling the "CUDDFactory.init()" method
 * with the desired arguments. This will return you an instance of the
 * BDDFactory class that you can use. In this version, it can be called with an
 * optional 'ADDFactory' argument. Call "done()" on that instance when you are
 * finished.
 * </p>
 * 
 * <p>
 * CUDD does not have much of the functionality that BuDDy has, and it has not
 * been well-tested. Furthermore, it is slower than BuDDy. Therefore, it is
 * recommended that you use the BuDDy library instead.
 * </p>
 * 
 * <p>
 * This class (and the CUDD library) do NOT support multithreading. Furthermore,
 * there can be only one instance active at a time. You can only call "init()"
 * again after you have called "done()" on the original instance. It is not
 * recommended to call "init()" again after calling "done()" unless you are
 * _completely_ sure that all BDD objects that reference the old factory have
 * been freed.
 * </p>
 * 
 * <p>
 * If you really need multiple BDD factories, consider using the JavaFactory
 * class for the additional BDD factories --- JavaFactory can have multiple
 * factory instances active at a time.
 * </p>
 * 
 * <p>
 * This is an *updated* version of BDDfactory that also supports ADD\MTBDD
 * operations. It is important to note that the CUDD library had already
 * supported ADD operations before this update was done, but it was not
 * available through a java native interface.
 * </p>
 * 
 * @see net.sf.javabdd.BDDFactory
 * @see net.sf.javabdd.BuDDyFactory
 * 
 * @author John Whaley
 * @version $Id: CUDDFactory.java,v 1.3 2015/09/15 19:30:54 uid228351 Exp $
 */
public class CUDDFactory extends BDDFactory {

	/**
	 * name of the CUDD dll to load try default "cudd" or the experimental "cudd3"
	 */
	private static final String CUDD_LIB_3_0 = "cudd";
	public static final String CUDD_3_0 = "3.0";
	private static String currVer;

	public static BDDFactory init(int nodenum, int cachesize) {
		CUDDFactory resF;
		if (!CUDDFactoryDone) {
			return bddInstance;
		}
		loadLibrary();
		resF = new CUDDFactory();
		resF.initialize(nodenum / 256, cachesize);
		return resF;
	}

	private static void loadLibrary() {
		System.loadLibrary(CUDD_LIB_3_0);
		currVer = CUDD_3_0;
		registerNatives();
	}

	private static native void registerNatives();

	private CUDDFactory() {
	}

	private static CUDDFactory bddInstance = null;
	private static CUDDADDFactory addInstance = null;
	private static boolean CUDDFactoryDone = true;
	private static boolean CUDDADDFactoryDone = true;

	/*
	 * the names below should be kept consistent with those used in cudd_jni.c in
	 * the top-level directory.
	 */
	private static long info = 0; // CUDD manager
	private static long zero = 0; // constant CUDD BDD (logical) zero
	private static long one = 0; // constant CUDD BDD and ADD one
	private static long addZero = 0; // constant CUDD ADD (arithmetic) zero
	/* --------------------- BEGIN native methods -------------------- */
	/*
	 * ALL native methods are declared here, so they can access the fields above.
	 * Declarations in the inner classes cannot access these fields.
	 */

	private static native void initialize0(int nodenum, int cachesize);

	private static native boolean isInitialized0();

	private static native void done0();

	private static native int varNum0();

	private static native int setVarNum0(int num, boolean ADD);

	private static native long ithVar0(int var, boolean ADD);

	private static native int level2Var0(int level);

	private static native int var2Level0(int var);

	private static native void setVarOrder0(int[] neworder);

	private static native int getAllocNum0();

	private static native int getNodeNum0();

	private static native int getCacheSize0();

	private static native int var0(long b);

	private static native long high0(long b, boolean ADD);

	private static native long low0(long b, boolean ADD);

	private static native long not0(long b, boolean ADD);

	private static native long ite0(long b, long c, long d, boolean ADD);

	private static native long relprod0(long b, long c, long d);

	private static native long compose0(long b, long c, int var, boolean ADD);

	private static native long exist0(long b, long c, boolean ADD);

	private static native long forAll0(long b, long c, boolean ADD);

	private static native long restrict0(long b, long var);

	private static native long restrictWith0(long b, long c, boolean deref_other);

	private static native long simplify0(long b, long var, boolean ADD);

	private static native long support0(long b, boolean is_add);

	private static native long apply0(long b, long c, int opr, boolean ADD, boolean apply_with, boolean deref_other);

//	private static native long satOne0(long b, long c, boolean ADD);
	private static native long satOne0(long b, boolean ADD);

	private static native int nodeCount0(long b);

	private static native double pathCount0(long b);

	private static native double satCount0(long b);

	private static native void addRef(long p);

	private static native void delRef(long p, boolean ADD);

	private static native long veccompose0(long b, long p, boolean ADD);

	private static native long replace0(long b, long p, boolean ADD);

	private static native long alloc(boolean ADD);

	private static native void set0(long p, int oldvar, int newvar, boolean ADD);

	private static native void set2(long p, int oldvar, long newbdd);

	private static native void reset0(long ptr, boolean ADD);

	private static native void free0(long ptr);

	private static native boolean isZeroOneADD0(long ptr);

	private static native long addConst0(double constval);

	private static native boolean isAddConst0(long ptr);

	private static native long addFindMax0(long ptr);

	private static native long addFindMin0(long ptr);

	private static native double retrieveConstValue0(long ptr);

	private static native long addAdditiveNeg0(long ptr);

	private static native long addApplyLog0(long ptr);

	private static native void reorder0(ReorderMethod method);

	private static native void autoReorder0(ReorderMethod method, boolean setmax, int maxval);

	private static native ReorderMethod getreordermethod0();

	private static native void autoReorder1();

	private static native int reorderVerbose0(int v);

	private static native void addVarBlock0(int first, int last, boolean fixed);

	private static native void clearVarBlocks0();

	private static native void printStat0();

	private static native long toADD0(long b);

	private static native long toBDD0(long b);

	private static native long toBDDThreshold(long b, double threshold);

	private static native long toBDDStrictThreshold(long b, double threshold);

	private static native long toBDDInterval(long b, double lower, double upper);

	private static native long toBDDIthBit(long b, int bit);

	private static native int[] varSupportIndex0(long b);

	private static native void printSet0(long b, int printMode);

	private static native long logicZero0();

	private static native long arithmeticZero0();

	private static native long arithmeticLogicOne0();

	private static native long arithmeticPlusInfinity0();

	private static native long arithmeticMinusInfinity0();

	private static native long replaceWith0(long b, long c, boolean ADD);

	private static native long addMinimizeVal0(long ptr, double val);

	private static native long addAbstractMin0(long b, long c);

	private static native long addAbstractMax0(long b, long c);

	private static native int reorderTimes0();

	private static native void printVarTree0();

	private static native long addEngSubtract0(long a, long b, double maxEnergy);

	private static native long convertToZeroOneADDByThres0(long a, long opType, double thresValue);

	private static native void printDot0(long a);

	private static native long arithmeticExist0(long b, long c);

	private static native long determinizeController0(long b, long c, boolean ADD);

	private static native int getSize0(long b);

	private static native boolean reorderEnabled0();

	private static native boolean equalBDDs0(long bdd1, long bdd2);

	private static native int[] getAttrSizes0();

	private static native long[] getXMem0();

	private static native long[] getYMem0();

	private static native long[] getZMem0();

	private static native long[] getZMemFirstItr0();
		
	
	private static native long getJusticesBDD0(long[] sysJ, long[] envJ, int[] jindices, int[] iindices, int utilindex);
	
	private static native long getJusticesStarBDD0(long[] sysJ, long[] envJ, int[] jindices, int[] iindices, int utilindex);
	
	private static native long getTransBDD0(long sysIni, long envIni, long sysTrans, long envTrans, 
			int[] jindices, int[] iindices, int utilindex);
	
	private static native long getFixpointsBDD0(int[] jindices, int[] iindices, int[] kindices);
	
	private static native long getFixpointsStarBDD0(int[] jindices, int[] iindices, int[] kindices);
	
	private static native long getFulfillBDD0(int[] exjindices, int[] findices);
	
	private static native long getTowardsBDD0(int[] exjindices, int[] tindices);
	
	private static native long getEnvViolationBDD0(int[] iindices, int[] kindices);
	
	private static native void loadFixpointsJits0(long fixpoints, int[] jindices, int[] iindices, int[] kindices, int[] ranks, long pairs, long primeVars);
	
	private static native void loadTransJits0(long trans, int[] jindices, int[] iindices, int utilindex);
	
	private static native void loadJusticesJits0(long justices, int[] jindices, int[] iindices, int utilindex, int n, int m);
	
	private static native long nextStatesJits0(long current, long inputs, long pairs, long unprimeVars);
	
	private static native int initControllerJits0(long inputs);
	
	private static native void freeControllerJits0();
	
	private static native long getTransitionsJits0();
	
	private static native long getInitialJits0();
	
	private static native boolean gr1Game0(long[] sysJ, long[] envJ, long sysIni, long envIni, long sysTrans,
			long envTrans, long sysUnprime, long envUnprime, long sysPrime, long envPrime, long pairs,
			long[] sysTransList, long[] envTransList, long[] sysQuantSets, long[] envQuantSets, boolean efp,
			boolean eun, boolean fpr, boolean sca);

	private static native boolean gr1GameInc0(long[] sysJ, long[] envJ, long sysIni, long envIni, long sysTrans,
			long envTrans, long sysUnprime, long envUnprime, long sysPrime, long envPrime, long pairs, boolean efp,
			boolean eun, boolean fpr, boolean sca, int incBitmap, long incStartZ, long[] incZMem,
			long[] incZMemFirstItr, long[] incXMem, int jIdx, int incSizeD1, int incSizeD2, int[] incSizeD3);

	private static native long[] getRabinXMem0();

	private static native long[] getRabinZMem0();

	private static native int getRabinZSize0();

	private static native int[] getRabinXSizes0();

	private static native boolean rabinGame0(long[] sysJ, long[] envJ, long sysIni, long envIni, long sysTrans,
			long envTrans, long sysUnprime, long envUnprime, long sysPrime, long envPrime, long pairs,
			long[] sysTransList, long[] envTransList, long[] sysQuantSets, long[] envQuantSets, boolean efp,
			boolean eun, boolean fpr, boolean sca);

	private static native boolean rabinGameInc0(long[] sysJ, long[] envJ, long sysIni, long envIni, long sysTrans,
			long envTrans, long sysUnprime, long envUnprime, long sysPrime, long envPrime, long pairs, boolean efp,
			boolean eun, boolean fpr, boolean sca, int incBitmap, long incStartZ, long[] incZMem, long[] incXMem,
			int jIdx, int incSizeD1, int incSizeD2, int[] incSizeD3);
	
	
	/* ************************ GR(1)* *********************************************** */
	
	private static native long getGR1StarWinningStates0();
	
	private static native int[] getGR1StarJusticeIterNum0();
	
	private static native int[] getGR1StarTowardsExistIterNum();
	
	private static native int[] getGR1StarFulfillExistIterNum();
	
	private static native int getGR1StarEnvJViolationIterNum();
	
	private static native long[] getGR1StarYMem0();
	
	private static native long[] getGR1StarXMem0();
	
	private static native long[] getGR1StarFulfillExistMem0();
	
	private static native long[] getGR1StarTowardsExistMem0();
	
	private static native long[] getGR1StarEnvJViolationMem0();
	
	private static native boolean gr1StarGame0(long[] sysJ, long[] envJ,
			long[] sfaIni, long[] sfaTrans, long[] sfaTransToAcc, long[] sfaUnprime, long[] sfaPrime,
			long sysIni, long envIni, long sysTrans,
			long envTrans, long sysUnprime, long envUnprime, long sysPrime, long envPrime, long pairs,
			long[] sysTransList, long[] envTransList, long[] sysQuantSets, long[] envQuantSets, boolean efp,
			boolean eun, boolean fpr, boolean sca, boolean mem);
	
	/* ***************************************** */
	
	/*
	 * private static native boolean negCycleCheck0(long A, long S, long primedVars,
	 * long allPairs);
	 */
	///////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////
	///////////////////////////////////////////////////////////////////////////////
	/* --------------------- END native methods -------------------- */

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#zero() If it is a BDD factory, then the
	 * logical zero is returned; Otherwise, if it is an ADD factory, the arithmetic
	 * zero is returned.
	 */
	public BDD zero() {
		BDD res = new CUDDBDD(logicZero0());
		return res;
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#one()
	 */
	public BDD one() {
		BDD res = new CUDDBDD(arithmeticLogicOne0());
		return res;
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#initialize(int, int)
	 */
	protected void initialize(int nodenum, int cachesize) {
		if (CUDDFactoryDone) {
			CUDDFactoryDone = false;
			bddInstance = this;
			if (CUDDADDFactoryDone && addInstance == null && !isInitialized0()) { // CUDDFactory has not been
																					// initialized
				initialize0(nodenum, cachesize);
			}
		}
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#isInitialized()
	 */
	public boolean isInitialized() {
		return ((!CUDDFactoryDone) && (bddInstance != null) && isInitialized0());
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#done()
	 */
	public void done() {
		if (!CUDDFactoryDone) {
			CUDDFactoryDone = true;
			bddInstance = null;
			if (CUDDADDFactoryDone) {
				done0();
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#setError(int)
	 */
	public void setError(int code) {
		// TODO Implement this.
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#clearError()
	 */
	public void clearError() {
		// TODO Implement this.
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#setMaxNodeNum(int)
	 */
	public int setMaxNodeNum(int size) {
		// TODO Implement this.
		System.err.println("Warning: setMaxNodeNum() not yet implemented");
		return 1000000;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#setNodeTableSize(int)
	 */
	public int setNodeTableSize(int size) {
		// TODO Implement this.
		System.err.println("Warning: setNodeTableSize() not yet implemented");
		return getNodeTableSize();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#setCacheSize(int)
	 */
	public int setCacheSize(int size) {
		// TODO Implement this.
		System.err.println("Warning: setCacheSize() not yet implemented");
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#setMinFreeNodes(double)
	 */
	public double setMinFreeNodes(double x) {
		// TODO Implement this.
		System.err.println("Warning: setMinFreeNodes() not yet implemented");
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#setMaxIncrease(int)
	 */
	public int setMaxIncrease(int x) {
		// TODO Implement this.
		System.err.println("Warning: setMaxIncrease() not yet implemented");
		return 50000;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#setCacheRatio(double)
	 */
	public double setCacheRatio(double x) {
		// TODO Implement this.
		System.err.println("Warning: setCacheRatio() not yet implemented");
		return 0;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#setIncreaseFactor(double)
	 */
	public double setIncreaseFactor(double x) {
		// TODO Implement this.
		System.err.println("Warning: setIncreaseFactor() not yet implemented");
		return 0;
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#varNum()
	 */
	public int varNum() {
		return varNum0();
	}
	// private static native int varNum0();

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#setVarNum(int)
	 */
	public int setVarNum(int num) {
		return setVarNum0(num, isADD());
	}
	// private static native int setVarNum0(int num);

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#duplicateVar(int)
	 */
	public int duplicateVar(int var) {
		// TODO Implement this.
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#ithVar(int)
	 */
	public BDD ithVar(int var) {
		long id = ithVar0(var, isADD());
		BDD res = new CUDDBDD(id);
		return res;
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#nithVar(int)
	 */
	public BDD nithVar(int var) {
		BDD b = ithVar(var);
		BDD c = b.not();
		b.free();
		return c;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#swapVar(int, int)
	 */
	public void swapVar(int v1, int v2) {
		// TODO Implement this.
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#makePair()
	 */
	public BDDPairing makePair() {
		return new CUDDBDDPairing();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#printAll()
	 */
	public void printAll() {
		// TODO Implement this.
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#printTable(net.sf.javabdd.BDD)
	 */
	@Override
	public void printTable(BDD b) {
		if (!(b instanceof CUDDBDD || b instanceof CUDDADD)) {
			return;
		}
		CUDDBDD bdd = (CUDDBDD) b;
		printSet0(bdd._ddnode_ptr, 4);
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#level2Var(int)
	 */
	public int level2Var(int level) {
		return level2Var0(level);
	}
	// private static native int level2Var0(int level);

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#var2Level(int)
	 */
	public int var2Level(int var) {
		return var2Level0(var);
	}
	// private static native int var2Level0(int var);

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see
	 * net.sf.javabdd.BDDFactory#reorder(net.sf.javabdd.BDDFactory.ReorderMethod)
	 */
	public void reorder(ReorderMethod method) {
		reorder0(method);
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#autoReorder(net.sf.javabdd.BDDFactory.
	 * ReorderMethod)
	 */
	public void autoReorder(ReorderMethod method) {
		autoReorder0(method, false, 0);
	}

	/*
	 * (non-Javadoc) - DONE By default max is set to infinity, i.e. 2^31-1
	 * (0x7FFFFFFF, which is the maximum value an int can have). max must be a
	 * non-negative integer.
	 * 
	 * @see net.sf.javabdd.BDDFactory#autoReorder(net.sf.javabdd.BDDFactory.
	 * ReorderMethod, int)
	 */
	@Override
	public void autoReorder(ReorderMethod method, int max) {
		if (max < 0) {
			return;
		} /*
			 * make sure max is non-negative, so we don't have problems when converting from
			 * signed to unsigned int in CUDD
			 */
		autoReorder0(method, true, max);
	}

	@Override
	public boolean reorderEnabled() {
		return reorderEnabled0();
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#getReorderMethod()
	 */
	public ReorderMethod getReorderMethod() {
		return getreordermethod0();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#getReorderTimes()
	 */
	public int getReorderTimes() {
		return reorderTimes0();
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#disableReorder()
	 */
	public void disableReorder() {
		autoReorder0(BDDFactory.REORDER_NONE, false, 0);
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#enableReorder() Auto reordering is disabled by
	 * default. The default (initial) reordering method is REORDER_SIFT. The
	 * reordering method is left unchanged
	 */
	public void enableReorder() {
		autoReorder1();
	}

	/*
	 * (non-Javadoc) - DONE There are only two verbose levels: 0 == no information,
	 * non-zero == full information.
	 * 
	 * @see net.sf.javabdd.BDDFactory#reorderVerbose(int)
	 */
	public int reorderVerbose(int v) {
		return reorderVerbose0(v);
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#setVarOrder(int[])
	 */
	public void setVarOrder(int[] neworder) {
		setVarOrder0(neworder);
	}
	// private static native void setVarOrder0(int[] neworder);

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#addVarBlock(net.sf.javabdd.BDD, boolean)
	 */
	public void addVarBlock(BDD var, boolean fixed) {
		// TODO Implement this.
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#addVarBlock(int, int, boolean)
	 */
	public void addVarBlock(int first, int last, boolean fixed) {
		addVarBlock0(first, last, fixed);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#varBlockAll()
	 */
	public void varBlockAll() {
		// TODO Implement this.
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#clearVarBlocks()
	 */
	public void clearVarBlocks() {
		clearVarBlocks0();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#printOrder()
	 */
	public void printOrder() {
		printVarTree0();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#nodeCount(java.util.Collection)
	 */
	public int nodeCount(Collection<BDD> r) {
		// TODO Implement this.
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#getNodeTableSize()
	 */
	public int getNodeTableSize() {
		return getAllocNum0();
	}
	// private static native int getAllocNum0();

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#getNodeNum()
	 */
	public int getNodeNum() {
		return getNodeNum0();
	}
	// private static native int getNodeNum0();

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#getCacheSize()
	 */
	public int getCacheSize() {
		return getCacheSize0();
	}
	// private static native int getCacheSize0();

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#reorderGain()
	 */
	public int reorderGain() {
		// TODO Implement this.
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#printStat()
	 */
	public void printStat() {
		printStat0();
		System.out.flush();
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#createDomain(int, BigInteger)
	 */
	protected BDDDomain createDomain(int a, BigInteger b) {
		return new CUDDBDDDomain(a, b);
	}

	public static class CUDDADDFactory extends CUDDFactory {

		private enum ThresholdOp {

			SLT(1), SGT(2), LT(3), GT(4);

			private long opNum;

			private ThresholdOp(long opNum) {
				this.opNum = opNum;
			}

			private long getOpNumber() {
				return this.opNum;
			}

		}

		public static BDDFactory init(int nodenum, int cachesize) {
			CUDDFactory resF;
			if (!CUDDADDFactoryDone) {
				return addInstance;
			}
			loadLibrary();
			resF = new CUDDFactory.CUDDADDFactory();
			resF.initialize(nodenum / 256, cachesize);
			return resF;
		}

		@Override
		public String getVersion() {
			return "CUDD-ADD " + currVer;
		}

		@Override
		public ADD plusInfinity() {
			long res = arithmeticPlusInfinity0();
			return new CUDDADD(res);
		}

		@Override
		public ADD minusInfinity() {
			long res = arithmeticMinusInfinity0();
			return new CUDDADD(res);
		}

		@Override
		public ADD constant(double value) {
			if (value == 0.0 || value == 1.0) {
				return (ADD) super.constant(value);
			}
			long ddnode = addConst0(value);
			return new CUDDADD(ddnode);
		}

		@Override
		public boolean isADD() {
			return true;
		}

		@Override
		/*
		 * (non-Javadoc) not multi threaded safe!
		 * 
		 * @see net.sf.javabdd.CUDDFactory#initialize(int, int)
		 */
		protected void initialize(int nodenum, int cachesize) {
			if (CUDDADDFactoryDone) {
				// if (addInstance != null) {
				// return; // CUDDADDFactory already initialized
				// }
				CUDDADDFactoryDone = false;
				addInstance = this;
				if (CUDDFactoryDone && bddInstance == null && !isInitialized0()) { // avoid calling initialize0() more
																					// than once
					initialize0(nodenum, cachesize);
				}
			}
		}

		@Override
		public boolean isInitialized() {
			return ((!CUDDADDFactoryDone) && (addInstance != null) && isInitialized0());
		}

		@Override
		public ADD zero() {
			ADD res = new CUDDADD(arithmeticZero0());
			return res;
		}

		@Override
		public ADD one() {
			ADD res = new CUDDADD(arithmeticLogicOne0());
			return res;
		}

		@Override
		public ADD ithVar(int var) {
			long id = ithVar0(var, isADD());
			return new CUDDADD(id);
		}

		@Override
		public ADD nithVar(int var) {
			ADD b = this.ithVar(var);
			ADD c = b.not();
			b.free();
			return c;
		}

		@Override
		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDDFactory#makePair()
		 */
		public CUDDADDPairing makePair() {
			return new CUDDADDPairing();
		}

		/*
		 * (non-Javadoc) - DONE
		 * 
		 * @see net.sf.javabdd.BDDFactory#createDomain(int, BigInteger)
		 */
		@Override
		protected BDDDomain createDomain(int a, BigInteger b) {
			return new CUDDADDDomain(a, b);
		}

		@Override
		public void done() {
			if (!CUDDADDFactoryDone) { // && addInstance != null) {
				CUDDADDFactoryDone = true;
				addInstance = null;
				if (CUDDFactoryDone) {
					done0();
				}
			}
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDDFactory#createBitVector(int)
		 */
		@Override
		protected BDDBitVector createBitVector(int a) {
			return new CUDDADDBitVector(a);
		}

	}

	/*
	 * (non-Javadoc) An implementation of a BDD class, used by the CUDD interface.
	 */
	private class CUDDBDD extends AbstractBDD {

		/** The pointer used by the BDD library. */
		protected long _ddnode_ptr;

		/** An invalid id, for use in invalidating BDDs. */
		final long INVALID_BDD = -1;

		private CUDDBDD(long ddnode) {
			this._ddnode_ptr = ddnode;
		}

		public void printSet() {
			printSet0(this._ddnode_ptr, 2);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#getFactory()
		 */
		public BDDFactory getFactory() {
			return bddInstance;
		}

		@Override
		protected void finalize() throws Throwable {
			// if(this._ddnode_ptr != INVALID_BDD) {
			/// delRef(_ddnode_ptr, this.isADD());
			// _ddnode_ptr = INVALID_BDD;
			// }
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#isZero()
		 */
		public boolean isZero() {
			return this._ddnode_ptr == zero;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#isOne()
		 */
		public boolean isOne() {
			return this._ddnode_ptr == one;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#toADD()
		 */
		@Override
		public ADD toADD() {
			if (CUDDADDFactoryDone) {
				throw new BDDException("CUDD_ADD Factory must be initialized before calling toADD() method!");
			}
			if (this.isADD()) {
				return ((ADD) this).id();
			}
			long res = toADD0(this._ddnode_ptr);
			return new CUDDADD(res);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#var()
		 */
		public int var() {
			if (isZero() || isOne())
				throw new BDDException("cannot get var of terminal");
			return var0(_ddnode_ptr);
		}
		// private static native int var0(long b);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#high()
		 */
		public BDD high() {
			long b = high0(_ddnode_ptr, this.isADD());
			if (b == INVALID_BDD) {
				return null;
			}
			return new CUDDBDD(b);
		}
		// private static native long high0(long b);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#low()
		 */
		public BDD low() {
			long b = low0(_ddnode_ptr, this.isADD());
			if (b == INVALID_BDD) {
				return null;
			}
			return new CUDDBDD(b);
		}
		// private static native long low0(long b);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#id()
		 */
		public BDD id() {
			BDD res = new CUDDBDD(this._ddnode_ptr);
			addRef(this._ddnode_ptr);
			return res;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#not()
		 */
		public BDD not() {
			long b = not0(_ddnode_ptr, false);
			return new CUDDBDD(b);
		}
		// private static native long not0(long b);

		protected long iteCaller(BDD thenBDD, BDD elseBDD) {
			CUDDBDD c = (CUDDBDD) thenBDD;
			CUDDBDD d = (CUDDBDD) elseBDD;
			return ite0(_ddnode_ptr, c._ddnode_ptr, d._ddnode_ptr, this.isADD());
		}

		/*
		 * (non-Javadoc) - DONE
		 * 
		 * @see net.sf.javabdd.BDD#ite(net.sf.javabdd.BDD, net.sf.javabdd.BDD)
		 */
		public BDD ite(BDD thenBDD, BDD elseBDD) { // if-then-else operator: Cudd_bddIte, computes: this*then +
													// this'*else
			long b = iteCaller(thenBDD, elseBDD);
			return new CUDDBDD(b);
		}
		// private static native long ite0(long b, long c, long d);

		/*
		 * (non-Javadoc) - TO DO
		 * 
		 * @see net.sf.javabdd.BDD#relprod(net.sf.javabdd.BDD, net.sf.javabdd.BDDVarSet)
		 */
		public BDD relprod(BDD that, BDDVarSet var) {
			CUDDBDD c = (CUDDBDD) that;
			CUDDBDD d = (CUDDBDD) ((BDDVarSet.DefaultImpl) var).b;
			long b = relprod0(_ddnode_ptr, c._ddnode_ptr, d._ddnode_ptr);
			return new CUDDBDD(b);
		}
		// private static native long relprod0(long b, long c, long d);

		protected long composeCaller(BDD that, int var) {
			CUDDBDD c = (CUDDBDD) that;
			return compose0(_ddnode_ptr, c._ddnode_ptr, var, this.isADD());
		}

		/*
		 * (non-Javadoc) - DONE
		 * 
		 * @see net.sf.javabdd.BDD#compose(net.sf.javabdd.BDD, int)
		 */
		public BDD compose(BDD that, int var) {
			long b = composeCaller(that, var);
			return new CUDDBDD(b);
		}
		// private static native long compose0(long b, long c, int var);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#constrain(net.sf.javabdd.BDD)
		 */
		public BDD constrain(BDD that) {
			// TODO Implement this.
			throw new UnsupportedOperationException();
		}

		protected long existCaller(BDDVarSet var) {
			CUDDBDD c = (CUDDBDD) ((BDDVarSet.DefaultImpl) var).b;
			return exist0(_ddnode_ptr, c._ddnode_ptr, this.isADD());
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#exist(net.sf.javabdd.BDDVarSet)
		 */
		public BDD exist(BDDVarSet var) {
			long b = existCaller(var);
			return new CUDDBDD(b);
		}

		protected long forAllCaller(BDDVarSet var) {
			CUDDBDD c = (CUDDBDD) ((BDDVarSet.DefaultImpl) var).b;
			return forAll0(_ddnode_ptr, c._ddnode_ptr, this.isADD());
		}
		// private static native long exist0(long b, long c);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#forAll(net.sf.javabdd.BDDVarSet)
		 */
		public BDD forAll(BDDVarSet var) {
			long b = forAllCaller(var);
			return new CUDDBDD(b);
		}
		// private static native long forAll0(long b, long c);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#unique(net.sf.javabdd.BDDVarSet)
		 */
		public BDD unique(BDDVarSet var) {
			// TODO Implement this.
			throw new UnsupportedOperationException();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#restrict(net.sf.javabdd.BDD)
		 */
		@Override
		public BDD restrict(BDD var) {
			CUDDBDD c = (CUDDBDD) var;
			long b = restrict0(_ddnode_ptr, c._ddnode_ptr);
			return new CUDDBDD(b);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#restrictWith(net.sf.javabdd.BDD)
		 */
		@Override
		public BDD restrictWith(BDD that) {
			CUDDBDD c = (CUDDBDD) that;
			long b = restrictWith0(this._ddnode_ptr, c._ddnode_ptr, (this != c));
			if (this != c) {
				c._ddnode_ptr = INVALID_BDD;
			}
			this._ddnode_ptr = b;
			return this;
		}

		/*
		 * 
		 */
		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#simplify(net.sf.javabdd.BDDVarSet)
		 */
		public BDD simplify(BDDVarSet d) {
			CUDDBDD c = (CUDDBDD) d.toBDD();
			long b = simplify0(_ddnode_ptr, c._ddnode_ptr, this.isADD());
			return new CUDDBDD(b);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#support()
		 */
		public BDDVarSet support() {
			long b = support0(_ddnode_ptr, this.isADD());
			return new BDDVarSet.DefaultImpl(new CUDDBDD(b));
		}
		// private static native long support0(long b);

		protected long applyCaller(BDD that, BDDFactory.BDDOp opr) {
			CUDDBDD c = (CUDDBDD) that;
			return apply0(_ddnode_ptr, c._ddnode_ptr, opr.id, this.isADD(), false, false);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#apply(net.sf.javabdd.BDD,
		 * net.sf.javabdd.BDDFactory.BDDOp)
		 */
		public BDD apply(BDD that, BDDFactory.BDDOp opr) { // Cudd_addApply
			long b = applyCaller(that, opr);
			return new CUDDBDD(b);
		}
		// private static native long apply0(long b, long c, int opr);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#applyWith(net.sf.javabdd.BDD,
		 * net.sf.javabdd.BDDFactory.BDDOp)
		 */
		public BDD applyWith(BDD that, BDDFactory.BDDOp opr) {
			CUDDBDD c = (CUDDBDD) that;
			long b = apply0(_ddnode_ptr, c._ddnode_ptr, opr.id, this.isADD(), true, (this != c));
			if (this != c) {
				c._ddnode_ptr = INVALID_BDD;
			}
			_ddnode_ptr = b;
			return this;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#applyAll(net.sf.javabdd.BDD,
		 * net.sf.javabdd.BDDFactory.BDDOp, net.sf.javabdd.BDDVarSet)
		 */
		public BDD applyAll(BDD that, BDDOp opr, BDDVarSet var) {
			// TODO Implement this.
			throw new UnsupportedOperationException();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#applyEx(net.sf.javabdd.BDD,
		 * net.sf.javabdd.BDDFactory.BDDOp, net.sf.javabdd.BDDVarSet)
		 */
		public BDD applyEx(BDD that, BDDOp opr, BDDVarSet var) {
			// TODO Implement this.
			throw new UnsupportedOperationException();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#applyUni(net.sf.javabdd.BDD,
		 * net.sf.javabdd.BDDFactory.BDDOp, net.sf.javabdd.BDDVarSet)
		 */
		public BDD applyUni(BDD that, BDDOp opr, BDDVarSet var) {
			// TODO Implement this.
			throw new UnsupportedOperationException();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#satOne(BDDVarSet var)
		 */
		@Override
		public BDD satOne(BDDVarSet var) {
//			CUDDBDD c = (CUDDBDD) ((BDDVarSet.DefaultImpl) var).b;
//			long b = satOne0(_ddnode_ptr, c._ddnode_ptr, this.isADD());
			long b = satOne0(_ddnode_ptr, this.isADD());
			return new CUDDBDD(b);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#nodeCount()
		 */
		public int nodeCount() {
			return nodeCount0(_ddnode_ptr);
		}
		// private static native int nodeCount0(long b);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#pathCount()
		 */
		public double pathCount() {
			return pathCount0(_ddnode_ptr);
		}
		// private static native double pathCount0(long b);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#satCount()
		 */
		public double satCount() {
			return satCount0(_ddnode_ptr);
		}
		// private static native double satCount0(long b);

		///////////////////////////////////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////
		///////////////////////////////////////////////////////////////////////
		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#varProfile()
		 */
		public int[] varProfile() {
			int[] profile = varSupportIndex0(_ddnode_ptr);
			return profile;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#free()
		 */
		public void free() {
			delRef(_ddnode_ptr, this.isADD());
			_ddnode_ptr = INVALID_BDD;
		}

		protected long veccomposeCaller(BDDPairing pair) {
			CUDDBDDPairing p = (CUDDBDDPairing) pair;
			return veccompose0(_ddnode_ptr, p._ptr, this.isADD());
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#veccompose(net.sf.javabdd.BDDPairing)
		 */
		public BDD veccompose(BDDPairing pair) {
			long b = veccomposeCaller(pair);
			return new CUDDBDD(b);
		}
		// private static native long veccompose0(long b, long p);

		protected long replaceCaller(BDDPairing pair) {
			CUDDBDDPairing p = (CUDDBDDPairing) pair;
			return replace0(_ddnode_ptr, p._ptr, this.isADD());
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#replace(net.sf.javabdd.BDDPairing)
		 */
		public BDD replace(BDDPairing pair) {
			long b = this.replaceCaller(pair);
			return new CUDDBDD(b);
		}
		// private static native long replace0(long b, long p);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#replaceWith(net.sf.javabdd.BDDPairing)
		 */
		public BDD replaceWith(BDDPairing pair) {
			CUDDBDDPairing p = (CUDDBDDPairing) pair;
			long b = replaceWith0(_ddnode_ptr, p._ptr, this.isADD());
			_ddnode_ptr = b;
			return this;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#equals(net.sf.javabdd.BDD)
		 */
		public boolean equals(BDD that) {
			if (that == null) {
				return false;
			}

			return (this._ddnode_ptr == ((CUDDBDD) that)._ddnode_ptr);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#hashCode()
		 */
		public int hashCode() {
			return (int) this._ddnode_ptr;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#toVarSet()
		 */
		public BDDVarSet toVarSet() {
			return new BDDVarSet.DefaultImpl(new CUDDBDD(this._ddnode_ptr));
		}

		@Override
		public boolean isFree() {
			return _ddnode_ptr == INVALID_BDD;
		}

		@Override
		public long rootId() {
			return _ddnode_ptr;
		}

		@Override
		public BDD determinizeController(BDDVarSet d) {
			CUDDBDD c = (CUDDBDD) ((BDDVarSet.DefaultImpl) d).b;
			long res = determinizeController0(_ddnode_ptr, c._ddnode_ptr, false);
			return new CUDDBDD(res);
		}

	}

	/*
	 * (non-Javadoc) Implementation of a MTBDD class. CUDD library calls it ADD.
	 */
	private class CUDDADD extends CUDDBDD implements ADD {

		private CUDDADD(long ddnode) {
			super(ddnode);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#support()
		 */
		@Override
		public BDDVarSet support() {
			long b = support0(_ddnode_ptr, this.isADD());
			return new BDDVarSet.DefaultImpl(new CUDDADD(b));
		}

		@Override
		public ADD simplify(BDDVarSet d) {
			CUDDADD c = (CUDDADD) d.toBDD();
			long b = simplify0(_ddnode_ptr, c._ddnode_ptr, this.isADD());
			return new CUDDADD(b);
		}

		@Override
		public ADD restrict(ADD var) {
			CUDDADD c = (CUDDADD) var;
			long b = restrict0(_ddnode_ptr, c._ddnode_ptr);
			return new CUDDADD(b);
		}

		@Override
		public ADD restrictWith(ADD that) {
			CUDDADD c = (CUDDADD) that;
			long b = restrictWith0(this._ddnode_ptr, c._ddnode_ptr, (this != c));
			if (this != c) {
				c._ddnode_ptr = INVALID_BDD;
			}
			this._ddnode_ptr = b;
			return this;
		}

		@Override
		public ADD minimizeValue(double value) {
			long res = addMinimizeVal0(this._ddnode_ptr, value);
			return new CUDDADD(res);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#toVarSet()
		 */
		@Override
		public BDDVarSet toVarSet() {
			return new BDDVarSet.DefaultImpl(new CUDDADD(this._ddnode_ptr));
		}

		@Override
		public ADD satOne(BDDVarSet var) {
//			CUDDADD c = (CUDDADD) ((BDDVarSet.DefaultImpl) var).b;
//			long b = satOne0(_ddnode_ptr, c._ddnode_ptr, this.isADD());
			long b = satOne0(_ddnode_ptr, this.isADD());
			return new CUDDADD(b);
		}

		@Override
		public ADD id() {
			ADD res = new CUDDADD(this._ddnode_ptr);
			addRef(this._ddnode_ptr);
			return res;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#isZero()
		 */
		@Override
		public boolean isZero() {
			return this._ddnode_ptr == addZero;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.CUDDFactory.CUDDBDD#not()
		 */
		public ADD not() {
			long b = not0(_ddnode_ptr, this.isADD());
			return new CUDDADD(b);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#low()
		 */
		@Override
		public ADD low() {
			long b = low0(_ddnode_ptr, this.isADD());
			if (b == INVALID_BDD) {
				return null;
			}
			return new CUDDADD(b);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#high()
		 */
		@Override
		public ADD high() {
			long b = high0(_ddnode_ptr, this.isADD());
			if (b == INVALID_BDD) {
				return null;
			}
			return new CUDDADD(b);
		}

		@Override
		public ADD replace(BDDPairing pair) {
			long b = this.replaceCaller(pair);
			return new CUDDADD(b);
		}

		@Override
		public ADD replaceWith(BDDPairing pair) {
			return (ADD) super.replaceWith(pair);
		}

		@Override
		public BDDFactory getFactory() {
			return addInstance;
		}

		@Override
		public boolean isConstantADD() {
			return isAddConst0(this._ddnode_ptr);
		}

		@Override
		public ADD ite(BDD thenBDD, BDD elseBDD) {
			long b = iteCaller(thenBDD, elseBDD);
			return new CUDDADD(b);
		}

		@Override
		public boolean isADD() {
			return true;
		}

		@Override
		public ADD findMax() {
			return new CUDDADD(addFindMax0(this._ddnode_ptr));
		}

		@Override
		public ADD findMin() {
			return new CUDDADD(addFindMin0(this._ddnode_ptr));
		}

		@Override
		public boolean isZeroOneADD() {
			return isZeroOneADD0(this._ddnode_ptr);
		}

		@Override
		public ADD additiveNot() {
			return new CUDDADD(addAdditiveNeg0(this._ddnode_ptr));
		}

		@Override
		public ADD applyLog() {
			return new CUDDADD(addApplyLog0(this._ddnode_ptr));
		}

		@Override
		public ADD toZeroOneSLTThreshold(double threshold) {
			long result = convertToZeroOneADDByThres0(this._ddnode_ptr, CUDDADDFactory.ThresholdOp.SLT.getOpNumber(),
					threshold);
			return new CUDDADD(result);
		}

		@Override
		public ADD toZeroOneSGTThreshold(double threshold) {
			long result = convertToZeroOneADDByThres0(this._ddnode_ptr, CUDDADDFactory.ThresholdOp.SGT.getOpNumber(),
					threshold);
			return new CUDDADD(result);
		}

		@Override
		public ADD toZeroOneLTThreshold(double threshold) {
			long result = convertToZeroOneADDByThres0(this._ddnode_ptr, CUDDADDFactory.ThresholdOp.LT.getOpNumber(),
					threshold);
			return new CUDDADD(result);
		}

		@Override
		public ADD toZeroOneGTThreshold(double threshold) {
			long result = convertToZeroOneADDByThres0(this._ddnode_ptr, CUDDADDFactory.ThresholdOp.GT.getOpNumber(),
					threshold);
			return new CUDDADD(result);
		}

		@Override
		public ADD apply(BDD that, BDDFactory.BDDOp opr) {
			long b = this.applyCaller(that, opr);
			return new CUDDADD(b);
		}

		@Override
		public ADD applyWith(BDD that, BDDFactory.BDDOp opr) {
			return (ADD) super.applyWith(that, opr);
		}

		public ADD forAll(BDDVarSet var) {
			long b = this.forAllCaller(var);
			return new CUDDADD(b);
		}

		public ADD exist(BDDVarSet var) {
			long b = existCaller(var);
			return new CUDDADD(b);
		}

		@Override
		public double getConstantValue() {
			if (this.isConstantADD()) {
				return retrieveConstValue0(this._ddnode_ptr);
			}
			return Double.NaN;
		}

		@Override
		public ADD compose(BDD that, int var) {
			long b = composeCaller(that, var);
			return new CUDDADD(b);
		}

		@Override
		public ADD veccompose(BDDPairing pair) {
			long b = veccomposeCaller(pair);
			return new CUDDADD(b);
		}

		/*
		 * 
		 * 
		 * methods of conversion from ADD to BDD
		 * 
		 * 
		 * 
		 */

		@Override
		public BDD toBDD() {
			if (CUDDFactoryDone) {
				throw new BDDException("CUDD BDD Factory must be initialized before calling toBDD() method!");
			}
			long res = toBDD0(this._ddnode_ptr);
			return new CUDDBDD(res);
		}

		@Override
		public BDD toBDDByThreshold(double threshold) {
			if (CUDDFactoryDone) {
				throw new BDDException(
						"CUDD BDD Factory must be initialized before calling toBDDByThreshold() method!");
			}
			long res = toBDDThreshold(this._ddnode_ptr, threshold);
			return new CUDDBDD(res);
		}

		@Override
		public BDD toBDDByStrictThreshold(double threshold) {
			if (CUDDFactoryDone) {
				throw new BDDException(
						"CUDD BDD Factory must be initialized before calling toBDDByStrictThreshold() method!");
			}
			long res = toBDDStrictThreshold(this._ddnode_ptr, threshold);
			return new CUDDBDD(res);
		}

		@Override
		public BDD toBDDByInterval(double lower, double upper) {
			if (CUDDFactoryDone) {
				throw new BDDException("CUDD BDD Factory must be initialized before calling toBDDByInterval() method!");
			}
			long res = toBDDInterval(this._ddnode_ptr, lower, upper);
			return new CUDDBDD(res);
		}

		@Override
		public BDD toBDDByIthBit(int bit) {
			if (CUDDFactoryDone) {
				throw new BDDException("CUDD BDD Factory must be initialized before calling toBDDByIthBit() method!");
			}
			long res = toBDDIthBit(this._ddnode_ptr, bit);
			return new CUDDBDD(res);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#relprod(net.sf.javabdd.BDD, net.sf.javabdd.BDDVarSet)
		 */
		@Override
		public BDD relprod(BDD that, BDDVarSet var) {
			throw new UnsupportedOperationException();
		}

		@Override
		public ADD and(ADD that) {
			return this.apply(that, BDDFactory.and);
		}

		@Override
		public ADD or(ADD that) {
			if (this.isZeroOneADD()) {
				return this.apply(that, BDDFactory.or);
			}
			return this.apply(that, BDDFactory.plus);
		}

		@Override
		public ADD andWith(ADD that) {
			return this.applyWith(that, BDDFactory.and);
		}

		@Override
		public ADD orWith(ADD that) {
			if (this.isZeroOneADD()) {
				return this.applyWith(that, BDDFactory.or);
			}
			return this.applyWith(that, BDDFactory.plus);
		}

		@Override
		public ADD abstractMin(BDDVarSet d) {
			CUDDADD c = (CUDDADD) ((BDDVarSet.DefaultImpl) d).b;
			long res = addAbstractMin0(_ddnode_ptr, c._ddnode_ptr);
			return new CUDDADD(res);
		}

		@Override
		public ADD abstractSum(BDDVarSet d) {
			CUDDADD c = (CUDDADD) ((BDDVarSet.DefaultImpl) d).b;
			long res = arithmeticExist0(_ddnode_ptr, c._ddnode_ptr);
			return new CUDDADD(res);
		}

		@Override
		public ADD abstractMax(BDDVarSet d) {
			CUDDADD c = (CUDDADD) ((BDDVarSet.DefaultImpl) d).b;
			long res = addAbstractMax0(_ddnode_ptr, c._ddnode_ptr);
			return new CUDDADD(res);
		}

		private void getTerminalValuesRecur(Set<Double> values, Set<CUDDADD> mem) {
			Double constVal;

			if (this.isConstantADD()) {
				constVal = this.getConstantValue();
				// if(!values.contains(constVal)) {
				values.add(constVal);
				// }
				return;
			}
			CUDDADD lowChild = (CUDDADD) this.low();
			CUDDADD highChild = (CUDDADD) this.high();

			if (!mem.contains(lowChild)) {
				mem.add(lowChild);
				lowChild.getTerminalValuesRecur(values, mem);
			}

			if (!mem.contains(highChild)) {
				mem.add(highChild);
				highChild.getTerminalValuesRecur(values, mem);
			}

			lowChild.free();
			highChild.free();
		}

		@Override
		public double[] getTerminalValues() {
			Set<Double> values = new HashSet<Double>();
			Set<CUDDADD> mem = new HashSet<CUDDADD>();
			getTerminalValuesRecur(values, mem);

			double[] result = new double[values.size()];
			Iterator<Double> valsIter = values.iterator();

			for (int i = 0; valsIter.hasNext(); i++) {
				result[i] = valsIter.next().doubleValue();
			}

			return result;
		}

		@Override
		public void printDot() {
			printDot0(this._ddnode_ptr);
		};

		@Override
		public ADD determinizeController(BDDVarSet d) {
			CUDDADD c = (CUDDADD) ((BDDVarSet.DefaultImpl) d).b;
			long res = determinizeController0(_ddnode_ptr, c._ddnode_ptr, true);
			return new CUDDADD(res);
		}

		@Override
		public ADD engSubtract(ADD that, double maxEnergy) {
			CUDDADD b = (CUDDADD) that;
			long res = addEngSubtract0(this._ddnode_ptr, b._ddnode_ptr, maxEnergy);
			return new CUDDADD(res);
		}

		@Override
		public void printTerminalValues() {
			double[] res = this.getTerminalValues();
			for (int i = 0; i < res.length; i++) {
				System.out.print(res[i] + ", ");
			}
			System.out.println(System.lineSeparator());
			System.out.flush();
		}

	}

	/*
	 * (non-Javadoc) An implementation of a BDDDomain, used by the CUDD interface.
	 */
	private class CUDDBDDDomain extends BDDDomain {

		private CUDDBDDDomain(int index, BigInteger range) {
			super(index, range);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDDDomain#getFactory()
		 */
		public BDDFactory getFactory() {
			return bddInstance;
		}

	}

	/*
	 * (non-Javadoc) An implementation of a BDDDomain for ADDs, used by the CUDD
	 * (ADD) interface.
	 */
	private class CUDDADDDomain extends CUDDBDDDomain {

		private CUDDADDDomain(int index, BigInteger range) {
			super(index, range);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDDDomain#getFactory()
		 */
		public BDDFactory getFactory() {
			return addInstance;
		}

	}

	/*
	 * (non-Javadoc) An implementation of a BDDPairing, used by the CUDD interface.
	 */
	private class CUDDBDDPairing extends BDDPairing {

		long _ptr;

		private CUDDBDDPairing() {
			_ptr = alloc(isADDPairing());
		}

		// private static native long alloc();

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDDPairing#set(int, int)
		 */
		public void set(int oldvar, int newvar) {
			set0(_ptr, oldvar, newvar, isADDPairing());
		}
		// private static native void set0(long p, int oldvar, int newvar);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDDPairing#set(int, net.sf.javabdd.BDD)
		 */
		public void set(int oldvar, BDD newvar) {
			CUDDBDD c = (CUDDBDD) newvar;
			set2(_ptr, oldvar, c._ddnode_ptr);
		}
		// private static native void set2(long p, int oldvar, long newbdd);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDDPairing#reset()
		 */
		public void reset() {
			reset0(_ptr, isADDPairing());
		}
		// private static native void reset0(long ptr);

		/**
		 * Free the memory allocated for this pair.
		 */
		// private static native void free0(long ptr);
	}

	private class CUDDADDPairing extends CUDDBDDPairing {

		private CUDDADDPairing() {
			super();
		}

		@Override
		public boolean isADDPairing() {
			return true;
		}

		@Override
		public void set(int oldvar, BDD newvar) {
			if (!newvar.isADD()) {
				throw new BDDException("newvar must be an ADD");
			}
			super.set(oldvar, newvar);
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#createBitVector(int)
	 */
	protected BDDBitVector createBitVector(int a) {
		return new CUDDBDDBitVector(a);
	}

	/*
	 * (non-Javadoc) An implementation of a BDDBitVector, used by the CUDD
	 * interface.
	 */
	private class CUDDBDDBitVector extends BDDBitVector {

		private CUDDBDDBitVector(int a) {
			super(a);
		}

		@Override
		public BDDFactory getFactory() {
			return bddInstance;
		}

	}

	private class CUDDADDBitVector extends CUDDBDDBitVector {

		private CUDDADDBitVector(int a) {
			super(a);
		}

		@Override
		public BDDFactory getFactory() {
			return addInstance;
		}

	}
	
	@Override
	public BDD getGr1StarWinningStates() {
		return new CUDDBDD(getGR1StarWinningStates0());
	}
	
	@Override
	public int[] getGr1StarJusticeIterNum() {
		return getGR1StarJusticeIterNum0();
	}
	
	@Override
	public BDD[] getGr1StarXMem() {
		long[] xMem = getGR1StarXMem0();
		BDD[] res = new BDD[xMem.length];
		for (int i = 0; i < xMem.length; i++) {
			res[i] = new CUDDBDD(xMem[i]);
		}
		return res;
	}
	
	@Override
	public BDD[] getGr1StarYMem() {
		long[] yMem = getGR1StarYMem0();
		BDD[] res = new BDD[yMem.length];
		for (int j = 0; j < yMem.length; j++) {
			res[j] = new CUDDBDD(yMem[j]);
		}
		return res;
	}
	
	@Override
	public int[] getGr1StarTowardsExistIterNum() {
		return getGR1StarTowardsExistIterNum();
	}
	
	@Override
	public int[] getGr1StarFulfillExistIterNum() {
		return getGR1StarFulfillExistIterNum();
	}
	
	@Override
	public int getGr1StarEnvJViolationIterNum() {
		return getGR1StarEnvJViolationIterNum();
	}
	
	@Override
	public BDD[] getGr1StarFulfillExistMem() {
		long[] fExistMem = getGR1StarFulfillExistMem0();
		BDD[] res = new BDD[fExistMem.length];
		for(int i = 0; i < fExistMem.length; i++) {
			res[i] = new CUDDBDD(fExistMem[i]);
		}
		return res;
	}
	
	@Override
	public BDD[] getGr1StarTowardsExistMem() {
		long[] tExistMem = getGR1StarTowardsExistMem0();
		BDD[] res = new BDD[tExistMem.length];
		for(int i = 0; i < tExistMem.length; i++) {
			res[i] = new CUDDBDD(tExistMem[i]);
		}
		return res;
	}
	
	@Override
	public BDD[] getGR1StarEnvJViolationMem() {
		long[] envJVMem = getGR1StarEnvJViolationMem0();
		BDD[] res = new BDD[envJVMem.length];
		for(int i = 0; i < envJVMem.length; i++) {
			res[i] = new CUDDBDD(envJVMem[i]);
		}
		return res;
	}
	
	public int[] getAttrSizes() {
		return getAttrSizes0();
	}

	public BDD[] getXMem() {
		long[] xMem = getXMem0();
		if (xMem.length == 0) {
			//System.out.println("no getXMem");
			BDD[] res = new BDD[1];
			res[0] = Env.FALSE();
			return res;
		}

		BDD[] res = new BDD[xMem.length];
		for (int i = 0; i < xMem.length; i++) {
			res[i] = new CUDDBDD(xMem[i]);
		}

		return res;
	}

	public BDD[] getYMem() {
		long[] yMem = getYMem0();
		BDD[] res = new BDD[yMem.length];
		for (int i = 0; i < yMem.length; i++) {
			res[i] = new CUDDBDD(yMem[i]);
		}

		return res;
	}

	public BDD[] getZMem() {
		long[] zMem = getZMem0();
		BDD[] res = new BDD[zMem.length];
		for (int i = 0; i < zMem.length; i++) {
			res[i] = new CUDDBDD(zMem[i]);
		}

		return res;
	}

	public BDD[] getZMemFirstItr() {
		long[] zMem = getZMemFirstItr0();
		BDD[] res = new BDD[zMem.length];
		for (int i = 0; i < zMem.length; i++) {
			res[i] = new CUDDBDD(zMem[i]);
		}

		return res;
	}
	
	@Override
	public boolean gr1StarGame(BDD[] sysJ, BDD[] envJ,
			BDD[] sfaIni, BDD[] sfaTrans, BDD[] sfaTransToAcc, BDDVarSet[] sfaUnprimeStateVars, BDDVarSet[] sfaPrimeStateVars,
			BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans,
			BDDVarSet sysUnprimeVars, BDDVarSet envUnprimeVars, BDDVarSet sysPrimeVars, BDDVarSet envPrimeVars,
			BDDPairing pairs, BDD[] sysTransList, BDD[] envTransList, BDDVarSet[] sysQuantSets,
			BDDVarSet[] envQuantSets, boolean efp, boolean eun, boolean fpr, boolean sca, boolean mem) {
		
		CUDDBDDPairing p = (CUDDBDDPairing) pairs;

		CUDDBDD sysIniBDD = (CUDDBDD) sysIni;
		CUDDBDD envIniBDD = (CUDDBDD) envIni;

		CUDDBDD sysTransBDD = (CUDDBDD) sysTrans;
		CUDDBDD envTransBDD = (CUDDBDD) envTrans;

		long[] sysJPtrArr = new long[sysJ.length];
		for (int i = 0; i < sysJ.length; i++) {
			sysJPtrArr[i] = ((CUDDBDD) sysJ[i])._ddnode_ptr;
		}

		long[] envJPtrArr = new long[envJ.length];
		for (int i = 0; i < envJ.length; i++) {
			envJPtrArr[i] = ((CUDDBDD) envJ[i])._ddnode_ptr;
		}
		
		long[] sfaIniPtrArr = new long[sfaIni.length];
		for (int i = 0; i < sfaIni.length; i++) {
			sfaIniPtrArr[i] = ((CUDDBDD) sfaIni[i])._ddnode_ptr;
		}
		
		long[] sfaTransPtrArr = new long[sfaTrans.length];
		for (int i = 0; i < sfaTrans.length; i++) {
			sfaTransPtrArr[i] = ((CUDDBDD) sfaTrans[i])._ddnode_ptr;
		}
		
		long[] sfaTransToAccPtrArr = new long[sfaTransToAcc.length];
		for (int i = 0; i < sfaTransToAcc.length; i++) {
			sfaTransToAccPtrArr[i] = ((CUDDBDD) sfaTransToAcc[i])._ddnode_ptr;
		}
		
		long[] sfaUnprimeVarsPtrArr = new long[sfaUnprimeStateVars.length];
		for(int i = 0 ; i < sfaUnprimeStateVars.length; i++) {
			sfaUnprimeVarsPtrArr[i] = ((CUDDBDD) ((BDDVarSet.DefaultImpl) sfaUnprimeStateVars[i]).b)._ddnode_ptr;
		}
		
		long[] sfaPrimeVarsPtrArr = new long[sfaPrimeStateVars.length];
		for(int i = 0 ; i < sfaPrimeStateVars.length; i++) {
			sfaPrimeVarsPtrArr[i] = ((CUDDBDD) ((BDDVarSet.DefaultImpl) sfaPrimeStateVars[i]).b)._ddnode_ptr;
		}
		
		CUDDBDD sysUnp = (CUDDBDD) ((BDDVarSet.DefaultImpl) sysUnprimeVars).b;
		CUDDBDD envUnp = (CUDDBDD) ((BDDVarSet.DefaultImpl) envUnprimeVars).b;

		CUDDBDD sysP = (CUDDBDD) ((BDDVarSet.DefaultImpl) sysPrimeVars).b;
		CUDDBDD envP = (CUDDBDD) ((BDDVarSet.DefaultImpl) envPrimeVars).b;

		long[] sysTransListArr = new long[sysTransList.length];
		for (int i = 0; i < sysTransList.length; i++) {
			sysTransListArr[i] = ((CUDDBDD) sysTransList[i])._ddnode_ptr;
		}

		long[] envTransListArr = new long[envTransList.length];
		for (int i = 0; i < envTransList.length; i++) {
			envTransListArr[i] = ((CUDDBDD) envTransList[i])._ddnode_ptr;
		}

		long[] sysQuantSetsArr = new long[sysQuantSets.length];
		for (int i = 0; i < sysQuantSets.length; i++) {
			sysQuantSetsArr[i] = ((CUDDBDD) ((BDDVarSet.DefaultImpl) sysQuantSets[i]).b)._ddnode_ptr;
		}

		long[] envQuantSetsArr = new long[envQuantSets.length];
		for (int i = 0; i < envQuantSets.length; i++) {
			envQuantSetsArr[i] = ((CUDDBDD) ((BDDVarSet.DefaultImpl) envQuantSets[i]).b)._ddnode_ptr;
		}

		return gr1StarGame0(sysJPtrArr, envJPtrArr,
				sfaIniPtrArr, sfaTransPtrArr, sfaTransToAccPtrArr, sfaUnprimeVarsPtrArr, sfaPrimeVarsPtrArr,
				sysIniBDD._ddnode_ptr, envIniBDD._ddnode_ptr, sysTransBDD._ddnode_ptr,
				envTransBDD._ddnode_ptr, sysUnp._ddnode_ptr, envUnp._ddnode_ptr, sysP._ddnode_ptr, envP._ddnode_ptr,
				p._ptr, sysTransListArr, envTransListArr, sysQuantSetsArr, envQuantSetsArr, efp, eun, fpr, sca, mem);
	}
	
	
	public BDD getFulfillBDD(int[] exjindices, int[] findices) {
		
		long fulfill = getFulfillBDD0(exjindices, findices);
		return new CUDDBDD(fulfill);
	}
	
	public BDD getTowardsBDD(int[] exjindices, int[] tindices) {
		
		long towards = getTowardsBDD0(exjindices, tindices);
		return new CUDDBDD(towards);
	}
	
	public BDD getEnvViolationBDD(int[] iindices, int[] kindices) {
		
		long towards = getEnvViolationBDD0(iindices, kindices);
		return new CUDDBDD(towards);
	}
	
	public BDD getJusticesBDD(BDD[] sysJ, BDD[] envJ, int[] jindices, int[] iindices, int utilindex) {
		
		long[] sysJPtrArr = new long[sysJ.length];
		for (int i = 0; i < sysJ.length; i++) {
			sysJPtrArr[i] = ((CUDDBDD) sysJ[i])._ddnode_ptr;
		}

		long[] envJPtrArr = new long[envJ.length];
		for (int i = 0; i < envJ.length; i++) {
			envJPtrArr[i] = ((CUDDBDD) envJ[i])._ddnode_ptr;
		}
		
		long justice = getJusticesBDD0(sysJPtrArr, envJPtrArr, jindices, iindices, utilindex);
		return new CUDDBDD(justice);
	}
	
	public BDD getJusticesStarBDD(BDD[] sysJ, BDD[] envJ, int[] jindices, int[] iindices, int utilindex) {
		
		long[] sysJPtrArr = new long[sysJ.length];
		for (int i = 0; i < sysJ.length; i++) {
			sysJPtrArr[i] = ((CUDDBDD) sysJ[i])._ddnode_ptr;
		}

		long[] envJPtrArr = new long[envJ.length];
		for (int i = 0; i < envJ.length; i++) {
			envJPtrArr[i] = ((CUDDBDD) envJ[i])._ddnode_ptr;
		}
		
		long justice = getJusticesStarBDD0(sysJPtrArr, envJPtrArr, jindices, iindices, utilindex);
		return new CUDDBDD(justice);
	}
	
	public BDD getTransBDD(BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans, int[] jindices, int[] iindices, int utilindex) {
		
		CUDDBDD sysIniBDD = (CUDDBDD) sysIni;
		CUDDBDD envIniBDD = (CUDDBDD) envIni;

		CUDDBDD sysTransBDD = (CUDDBDD) sysTrans;
		CUDDBDD envTransBDD = (CUDDBDD) envTrans;
		
		long trans = getTransBDD0(sysIniBDD._ddnode_ptr, envIniBDD._ddnode_ptr, sysTransBDD._ddnode_ptr, envTransBDD._ddnode_ptr, 
				jindices, iindices, utilindex);
		return new CUDDBDD(trans);
	}
	
	public BDD getFixpointsBDD(int[] jindices, int[] iindices, int[] kindices) {
		
		long fixpoints = getFixpointsBDD0(jindices, iindices, kindices);
		return new CUDDBDD(fixpoints);
	}
	
	public BDD getFixpointsStarBDD(int[] jindices, int[] iindices, int[] kindices) {
		
		long fixpoints = getFixpointsStarBDD0(jindices, iindices, kindices);
		return new CUDDBDD(fixpoints);
	}
	
	public void loadFixpointsJits(BDD fixpoints, int[] jindices, int[] iindices, int[] kindices, int[] ranks, BDDPairing pairs, BDDVarSet primeVars) {
		
		CUDDBDDPairing p = (CUDDBDDPairing) pairs;
		CUDDBDD fixpointsBDD = (CUDDBDD) fixpoints;
		CUDDBDD primeVarsBDD = (CUDDBDD) ((BDDVarSet.DefaultImpl) primeVars).b;
		loadFixpointsJits0(fixpointsBDD._ddnode_ptr, jindices, iindices, kindices, ranks, p._ptr, primeVarsBDD._ddnode_ptr);
	}
	
	public void loadTransJits(BDD trans, int[] jindices, int[] iindices, int utilindex) {
		
		CUDDBDD transBDD = (CUDDBDD) trans;
		loadTransJits0(transBDD._ddnode_ptr, jindices, iindices, utilindex);
	}
	
	public void loadJusticesJits(BDD justices, int[] jindices, int[] iindices, int utilindex, int n, int m) {
		
		CUDDBDD justicesBDD = (CUDDBDD) justices;
		loadJusticesJits0(justicesBDD._ddnode_ptr, jindices, iindices, utilindex, n, m);
	}
	
	public BDD nextStatesJits(BDD current, BDD inputs, BDDPairing pairs, BDDVarSet unprimeVars) {
		
		CUDDBDD currentBDD = (CUDDBDD) current;
		CUDDBDD inputsBDD = (CUDDBDD) inputs;
		CUDDBDDPairing p = (CUDDBDDPairing) pairs;
		CUDDBDD unprimeVarsBDD = (CUDDBDD) ((BDDVarSet.DefaultImpl) unprimeVars).b;
		long nextStates = nextStatesJits0(currentBDD._ddnode_ptr, inputsBDD._ddnode_ptr, p._ptr, unprimeVarsBDD._ddnode_ptr);
		return new CUDDBDD(nextStates);
	}
	
	public int initControllerJits(BDD inputs) {
		
		CUDDBDD inputsBDD = (CUDDBDD) inputs;
		return initControllerJits0(inputsBDD._ddnode_ptr);
	}
	
	public void freeControllerJits() {
		
		freeControllerJits0();
	}
	
	public BDD getTransitionsJits() {
		
		long trans = getTransitionsJits0();
		return new CUDDBDD(trans);
	}
	
	public BDD getInitialJits() {
		
		long ini = getInitialJits0();
		return new CUDDBDD(ini);
	}

	public boolean gr1Game(BDD[] sysJ, BDD[] envJ, BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans,
			BDDVarSet sysUnprimeVars, BDDVarSet envUnprimeVars, BDDVarSet sysPrimeVars, BDDVarSet envPrimeVars,
			BDDPairing pairs, BDD[] sysTransList, BDD[] envTransList, BDDVarSet[] sysQuantSets,
			BDDVarSet[] envQuantSets, boolean efp, boolean eun, boolean fpr, boolean sca) {
		//System.out.println("gr1 game in CUDD");

		CUDDBDDPairing p = (CUDDBDDPairing) pairs;

		CUDDBDD sysIniBDD = (CUDDBDD) sysIni;
		CUDDBDD envIniBDD = (CUDDBDD) envIni;

		CUDDBDD sysTransBDD = (CUDDBDD) sysTrans;
		CUDDBDD envTransBDD = (CUDDBDD) envTrans;

		long[] sysJPtrArr = new long[sysJ.length];
		for (int i = 0; i < sysJ.length; i++) {
			sysJPtrArr[i] = ((CUDDBDD) sysJ[i])._ddnode_ptr;
		}

		long[] envJPtrArr = new long[envJ.length];
		for (int i = 0; i < envJ.length; i++) {
			envJPtrArr[i] = ((CUDDBDD) envJ[i])._ddnode_ptr;
		}

		CUDDBDD sysUnp = (CUDDBDD) ((BDDVarSet.DefaultImpl) sysUnprimeVars).b;
		CUDDBDD envUnp = (CUDDBDD) ((BDDVarSet.DefaultImpl) envUnprimeVars).b;

		CUDDBDD sysP = (CUDDBDD) ((BDDVarSet.DefaultImpl) sysPrimeVars).b;
		CUDDBDD envP = (CUDDBDD) ((BDDVarSet.DefaultImpl) envPrimeVars).b;

		long[] sysTransListArr = new long[sysTransList.length];
		for (int i = 0; i < sysTransList.length; i++) {
			sysTransListArr[i] = ((CUDDBDD) sysTransList[i])._ddnode_ptr;
		}

		long[] envTransListArr = new long[envTransList.length];
		for (int i = 0; i < envTransList.length; i++) {
			envTransListArr[i] = ((CUDDBDD) envTransList[i])._ddnode_ptr;
		}

		long[] sysQuantSetsArr = new long[sysQuantSets.length];
		for (int i = 0; i < sysQuantSets.length; i++) {
			sysQuantSetsArr[i] = ((CUDDBDD) ((BDDVarSet.DefaultImpl) sysQuantSets[i]).b)._ddnode_ptr;
		}

		long[] envQuantSetsArr = new long[envQuantSets.length];
		for (int i = 0; i < envQuantSets.length; i++) {
			envQuantSetsArr[i] = ((CUDDBDD) ((BDDVarSet.DefaultImpl) envQuantSets[i]).b)._ddnode_ptr;
		}

		return gr1Game0(sysJPtrArr, envJPtrArr, sysIniBDD._ddnode_ptr, envIniBDD._ddnode_ptr, sysTransBDD._ddnode_ptr,
				envTransBDD._ddnode_ptr, sysUnp._ddnode_ptr, envUnp._ddnode_ptr, sysP._ddnode_ptr, envP._ddnode_ptr,
				p._ptr, sysTransListArr, envTransListArr, sysQuantSetsArr, envQuantSetsArr, efp, eun, fpr, sca);
	}

	public boolean gr1GameWithIncData(BDD[] sysJ, BDD[] envJ, BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans,
			BDDVarSet sysUnprimeVars, BDDVarSet envUnprimeVars, BDDVarSet sysPrimeVars, BDDVarSet envPrimeVars,
			BDDPairing pairs, boolean efp, boolean eun, boolean fpr, boolean sca, int incBitmap, BDD incStartZ,
			BDD[] incZMem, BDD[] incZMemFirstItr, BDD[] incXMem, int jIdx, int incSizeD1, int incSizeD2,
			int[] incSizeD3) {
		//System.out.println("gr1 game with inc data in CUDD");

		CUDDBDDPairing p = (CUDDBDDPairing) pairs;

		CUDDBDD sysIniBDD = (CUDDBDD) sysIni;
		CUDDBDD envIniBDD = (CUDDBDD) envIni;

		CUDDBDD sysTransBDD = (CUDDBDD) sysTrans;
		CUDDBDD envTransBDD = (CUDDBDD) envTrans;

		long[] sysJPtrArr = new long[sysJ.length];
		for (int i = 0; i < sysJ.length; i++) {
			sysJPtrArr[i] = ((CUDDBDD) sysJ[i])._ddnode_ptr;
		}

		long[] envJPtrArr = new long[envJ.length];
		for (int i = 0; i < envJ.length; i++) {
			envJPtrArr[i] = ((CUDDBDD) envJ[i])._ddnode_ptr;
		}

		CUDDBDD sysUnp = (CUDDBDD) ((BDDVarSet.DefaultImpl) sysUnprimeVars).b;
		CUDDBDD envUnp = (CUDDBDD) ((BDDVarSet.DefaultImpl) envUnprimeVars).b;

		CUDDBDD sysP = (CUDDBDD) ((BDDVarSet.DefaultImpl) sysPrimeVars).b;
		CUDDBDD envP = (CUDDBDD) ((BDDVarSet.DefaultImpl) envPrimeVars).b;

		CUDDBDD incStartZBDD = (CUDDBDD) incStartZ;

		long[] incZMemArr = new long[incZMem.length];
		for (int i = 0; i < incZMem.length; i++) {
			incZMemArr[i] = ((CUDDBDD) incZMem[i])._ddnode_ptr;
		}

		long[] incZMemFirstItrArr = new long[incZMemFirstItr.length];
		for (int i = 0; i < incZMemFirstItr.length; i++) {
			incZMemFirstItrArr[i] = ((CUDDBDD) incZMemFirstItr[i])._ddnode_ptr;
		}

		long[] incXMemArr = new long[incXMem.length];
		for (int i = 0; i < incXMem.length; i++) {
			incXMemArr[i] = ((CUDDBDD) incXMem[i])._ddnode_ptr;
		}

		return gr1GameInc0(sysJPtrArr, envJPtrArr, sysIniBDD._ddnode_ptr, envIniBDD._ddnode_ptr,
				sysTransBDD._ddnode_ptr, envTransBDD._ddnode_ptr, sysUnp._ddnode_ptr, envUnp._ddnode_ptr,
				sysP._ddnode_ptr, envP._ddnode_ptr, p._ptr, efp, eun, fpr, sca, incBitmap, incStartZBDD._ddnode_ptr,
				incZMemArr, incZMemFirstItrArr, incXMemArr, jIdx, incSizeD1, incSizeD2, incSizeD3);
	}

	public int getRabinZSize() {
		//System.out.println("calling getRabinZSize");
		return getRabinZSize0();
	}

	public int[] getRabinXSizes() {
		//System.out.println("calling getRabinXSizes");
		return getRabinXSizes0();
	}

	public BDD[] getRabinXMem() {
		//System.out.println("calling getRabinXMem");
		long[] xMem = getRabinXMem0();
		if (xMem.length == 0) {
			//System.out.println("no getRabinXMem");
			BDD[] res = new BDD[1];
			res[0] = Env.FALSE();
			return res;
		}

		BDD[] res = new BDD[xMem.length];
		for (int i = 0; i < xMem.length; i++) {
			res[i] = new CUDDBDD(xMem[i]);
		}

		return res;
	}

	public BDD[] getRabinZMem() {
		//System.out.println("calling getRabinZMem");
		long[] zMem = getRabinZMem0();
		BDD[] res = new BDD[zMem.length];

		for (int i = 0; i < zMem.length; i++) {
			res[i] = new CUDDBDD(zMem[i]);
		}

		return res;
	}

	public boolean rabinGame(BDD[] sysJ, BDD[] envJ, BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans,
			BDDVarSet sysUnprimeVars, BDDVarSet envUnprimeVars, BDDVarSet sysPrimeVars, BDDVarSet envPrimeVars,
			BDDPairing pairs, BDD[] sysTransList, BDD[] envTransList, BDDVarSet[] sysQuantSets,
			BDDVarSet[] envQuantSets, boolean efp, boolean eun, boolean fpr, boolean sca) {
		//System.out.println("rabin game in CUDD");

		CUDDBDDPairing p = (CUDDBDDPairing) pairs;

		CUDDBDD sysIniBDD = (CUDDBDD) sysIni;
		CUDDBDD envIniBDD = (CUDDBDD) envIni;

		CUDDBDD sysTransBDD = (CUDDBDD) sysTrans;
		CUDDBDD envTransBDD = (CUDDBDD) envTrans;

		long[] sysJPtrArr = new long[sysJ.length];
		for (int i = 0; i < sysJ.length; i++) {
			sysJPtrArr[i] = ((CUDDBDD) sysJ[i])._ddnode_ptr;
		}

		long[] envJPtrArr = new long[envJ.length];
		for (int i = 0; i < envJ.length; i++) {
			envJPtrArr[i] = ((CUDDBDD) envJ[i])._ddnode_ptr;
		}

		CUDDBDD sysUnp = (CUDDBDD) ((BDDVarSet.DefaultImpl) sysUnprimeVars).b;
		CUDDBDD envUnp = (CUDDBDD) ((BDDVarSet.DefaultImpl) envUnprimeVars).b;

		CUDDBDD sysP = (CUDDBDD) ((BDDVarSet.DefaultImpl) sysPrimeVars).b;
		CUDDBDD envP = (CUDDBDD) ((BDDVarSet.DefaultImpl) envPrimeVars).b;

		long[] sysTransListArr = new long[sysTransList.length];
		for (int i = 0; i < sysTransList.length; i++) {
			sysTransListArr[i] = ((CUDDBDD) sysTransList[i])._ddnode_ptr;
		}

		long[] envTransListArr = new long[envTransList.length];
		for (int i = 0; i < envTransList.length; i++) {
			envTransListArr[i] = ((CUDDBDD) envTransList[i])._ddnode_ptr;
		}

		long[] sysQuantSetsArr = new long[sysQuantSets.length];
		for (int i = 0; i < sysQuantSets.length; i++) {
			sysQuantSetsArr[i] = ((CUDDBDD) ((BDDVarSet.DefaultImpl) sysQuantSets[i]).b)._ddnode_ptr;
		}

		long[] envQuantSetsArr = new long[envQuantSets.length];
		for (int i = 0; i < envQuantSets.length; i++) {
			envQuantSetsArr[i] = ((CUDDBDD) ((BDDVarSet.DefaultImpl) envQuantSets[i]).b)._ddnode_ptr;
		}

		return rabinGame0(sysJPtrArr, envJPtrArr, sysIniBDD._ddnode_ptr, envIniBDD._ddnode_ptr, sysTransBDD._ddnode_ptr,
				envTransBDD._ddnode_ptr, sysUnp._ddnode_ptr, envUnp._ddnode_ptr, sysP._ddnode_ptr, envP._ddnode_ptr,
				p._ptr, sysTransListArr, envTransListArr, sysQuantSetsArr, envQuantSetsArr, efp, eun, fpr, sca);
	}

	public boolean rabinGameWithIncData(BDD[] sysJ, BDD[] envJ, BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans,
			BDDVarSet sysUnprimeVars, BDDVarSet envUnprimeVars, BDDVarSet sysPrimeVars, BDDVarSet envPrimeVars,
			BDDPairing pairs, boolean efp, boolean eun, boolean fpr, boolean sca, int incBitmap, BDD incStartZ,
			BDD[] incZMem, BDD[] incXMem, int jIdx, int incSizeD1, int incSizeD2, int[] incSizeD3) {
		//System.out.println("rabin game inc in CUDD");

		CUDDBDDPairing p = (CUDDBDDPairing) pairs;

		CUDDBDD sysIniBDD = (CUDDBDD) sysIni;
		CUDDBDD envIniBDD = (CUDDBDD) envIni;

		CUDDBDD sysTransBDD = (CUDDBDD) sysTrans;
		CUDDBDD envTransBDD = (CUDDBDD) envTrans;

		long[] sysJPtrArr = new long[sysJ.length];
		for (int i = 0; i < sysJ.length; i++) {
			sysJPtrArr[i] = ((CUDDBDD) sysJ[i])._ddnode_ptr;
		}

		long[] envJPtrArr = new long[envJ.length];
		for (int i = 0; i < envJ.length; i++) {
			envJPtrArr[i] = ((CUDDBDD) envJ[i])._ddnode_ptr;
		}

		CUDDBDD sysUnp = (CUDDBDD) ((BDDVarSet.DefaultImpl) sysUnprimeVars).b;
		CUDDBDD envUnp = (CUDDBDD) ((BDDVarSet.DefaultImpl) envUnprimeVars).b;

		CUDDBDD sysP = (CUDDBDD) ((BDDVarSet.DefaultImpl) sysPrimeVars).b;
		CUDDBDD envP = (CUDDBDD) ((BDDVarSet.DefaultImpl) envPrimeVars).b;

		CUDDBDD incStartZBDD = (CUDDBDD) incStartZ;

		long[] incZMemArr = new long[incZMem.length];
		for (int i = 0; i < incZMem.length; i++) {
			incZMemArr[i] = ((CUDDBDD) incZMem[i])._ddnode_ptr;
		}

		long[] incXMemArr = new long[incXMem.length];
		for (int i = 0; i < incXMem.length; i++) {
			incXMemArr[i] = ((CUDDBDD) incXMem[i])._ddnode_ptr;
		}

		return rabinGameInc0(sysJPtrArr, envJPtrArr, sysIniBDD._ddnode_ptr, envIniBDD._ddnode_ptr,
				sysTransBDD._ddnode_ptr, envTransBDD._ddnode_ptr, sysUnp._ddnode_ptr, envUnp._ddnode_ptr,
				sysP._ddnode_ptr, envP._ddnode_ptr, p._ptr, efp, eun, fpr, sca, incBitmap, incStartZBDD._ddnode_ptr,
				incZMemArr, incXMemArr, jIdx, incSizeD1, incSizeD2, incSizeD3);
	}

	public void main(String[] args) {
		BDDFactory bdd = init(1000000, 100000);

		//System.out.println("One: " + one);
		//System.out.println("Zero: " + zero);

		BDDDomain[] doms = bdd.extDomain(new int[] { 50, 10, 15, 20, 15 });

		BDD b = bdd.one();
		for (int i = 0; i < doms.length - 1; ++i) {
			b.andWith(doms[i].ithVar(i));
		}

		for (int i = 0; i < bdd.numberOfDomains(); ++i) {
			BDDDomain d = bdd.getDomain(i);
			int[] ivar = d.vars();
			//System.out.print("Domain #" + i + ":");
			for (int j = 0; j < ivar.length; ++j) {
				System.out.print(' ');
				System.out.print(j);
				System.out.print(':');
				System.out.print(ivar[j]);
			}
			System.out.println();
		}

		BDDPairing p = bdd.makePair(doms[2], doms[doms.length - 1]);
		System.out.println("Pairing: " + p);

		//System.out.println("Before replace(): " + b);
		BDD c = b.replace(p);
		//System.out.println("After replace(): " + c);

		c.printDot();
	}

	public final String REVISION = "$Revision: 1.3 $";

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#getVersion()
	 */
	public String getVersion() {
		return "CUDD " + currVer;
	}
}