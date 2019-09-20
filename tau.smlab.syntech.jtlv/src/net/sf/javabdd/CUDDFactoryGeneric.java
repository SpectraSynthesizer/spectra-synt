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

/**
 * <p>
 * An implementation of BDDFactory that relies on the CUDD library through a
 * native interface. You can use this by calling the "CUDDFactoryJNI.init()"
 * method with the desired arguments. This will return you an instance of the
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
public class CUDDFactoryGeneric extends BDDFactory {

	public static final String CUDD_3_0 = "3.0";
	public static final String CUDD_3_0_PIPE = "3.0_PIPE";
	protected static String cuddVer;
	protected CUDDCaller caller;

	public static BDDFactory init(int nodenum, int cachesize) {
		return init(null, nodenum, cachesize);
	}

	public static BDDFactory init(String packageVer, int nodenum, int cachesize) {
		CUDDFactoryGeneric resF;
		if (!CUDDADDFactoryDone) {
			throw new BDDException("Cannot initialize CUDD BDD and CUDD ADD factories at the same time");
		}
		if (!CUDDFactoryDone) {
			return bddInstance;
		}
		setCuddVersion(packageVer);
		resF = new CUDDFactoryGeneric();
		resF.initialize(packageVer, nodenum / 256, cachesize);
		return resF;
	}

	private static void setCuddVersion(String packageVer) {
		String loadedVer = CUDD_3_0;
		if (CUDD_3_0.equals(packageVer)) {
			loadedVer = CUDD_3_0;
		} else if (CUDD_3_0_PIPE.equals(packageVer)) {
			loadedVer = CUDD_3_0_PIPE;
		}
		cuddVer = loadedVer;
	}

	protected void loadCUDDCaller(String packageVer) {
		if (CUDD_3_0.equals(packageVer)) {
			this.caller = new CUDDJniCaller30();
		} else if (CUDD_3_0_PIPE.equals(packageVer)) {
			this.caller = new CUDDPipeCaller();
		} else {// Default: load CUDD 3.0
			this.caller = new CUDDJniCaller30();
		}
	}

	private CUDDFactoryGeneric() {
	}

	private static CUDDFactoryGeneric bddInstance = null;
	private static CUDDADDFactoryGeneric addInstance = null;
	private static boolean CUDDFactoryDone = true;
	private static boolean CUDDADDFactoryDone = true;

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#zero() If it is a BDD factory, then the
	 * logical zero is returned; Otherwise, if it is an ADD factory, the arithmetic
	 * zero is returned.
	 */
	public BDD zero() {
		BDD res = new CUDDBDD(caller.logicZero0Caller());
		return res;
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#one()
	 */
	public BDD one() {
		BDD res = new CUDDBDD(caller.arithmeticLogicOne0Caller());
		return res;
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#initialize(int, int)
	 */
	@Override
	protected void initialize(int nodenum, int cachesize) {
		this.initialize(cuddVer, nodenum, cachesize);
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#initialize(int, int)
	 */
	protected void initialize(String packageVer, int nodenum, int cachesize) {
		if (CUDDFactoryDone) {
			CUDDFactoryDone = false;
			bddInstance = this;
			this.loadCUDDCaller(packageVer);
			if (CUDDADDFactoryDone && addInstance == null && !caller.isInitialized0Caller()) { // CUDDFactory has not
																								// been initialized
				caller.initialize0Caller(nodenum, cachesize);
			}
		}
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#isInitialized()
	 */
	public boolean isInitialized() {
		return ((!CUDDFactoryDone) && (bddInstance != null) && caller.isInitialized0Caller());
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#done()
	 */
	public void done() {
		if (!CUDDFactoryDone) {
			CUDDFactoryDone = true;
			if (CUDDADDFactoryDone) {
				caller.done0Caller();
			}
			bddInstance.caller = null;
			bddInstance = null;
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

	@Override
	public boolean reorderEnabled() {
		return caller.reorderEnabled0Caller();
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
		return caller.varNum0Caller();
	}
	// private static native int varNum0();

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#setVarNum(int)
	 */
	public int setVarNum(int num) {
		return caller.setVarNum0Caller(num, isADD());
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
		long id = caller.ithVar0Caller(var, isADD());
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
		caller.printSet0Caller(bdd._ddnode_ptr, 4);
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#level2Var(int)
	 */
	public int level2Var(int level) {
		return caller.level2Var0Caller(level);
	}
	// private static native int level2Var0(int level);

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#var2Level(int)
	 */
	public int var2Level(int var) {
		return caller.var2Level0Caller(var);
	}
	// private static native int var2Level0(int var);

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see
	 * net.sf.javabdd.BDDFactory#reorder(net.sf.javabdd.BDDFactory.ReorderMethod)
	 */
	public void reorder(ReorderMethod method) {
		caller.reorder0Caller(method);
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#autoReorder(net.sf.javabdd.BDDFactory.
	 * ReorderMethod)
	 */
	public void autoReorder(ReorderMethod method) {
		caller.autoReorder0Caller(method, false, 0);
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
		caller.autoReorder0Caller(method, true, max);
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#getReorderMethod()
	 */
	public ReorderMethod getReorderMethod() {
		return caller.getreordermethod0Caller();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#getReorderTimes()
	 */
	public int getReorderTimes() {
		return caller.reorderTimes0Caller();
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#disableReorder()
	 */
	public void disableReorder() {
		caller.autoReorder0Caller(BDDFactory.REORDER_NONE, false, 0);
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#enableReorder() Auto reordering is disabled by
	 * default. The default (initial) reordering method is REORDER_SIFT. The
	 * reordering method is left unchanged
	 */
	public void enableReorder() {
		caller.autoReorder1Caller();
	}

	/*
	 * (non-Javadoc) - DONE There are only two verbose levels: 0 == no information,
	 * non-zero == full information.
	 * 
	 * @see net.sf.javabdd.BDDFactory#reorderVerbose(int)
	 */
	public int reorderVerbose(int v) {
		return caller.reorderVerbose0Caller(v);
	}

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#setVarOrder(int[])
	 */
	public void setVarOrder(int[] neworder) {
		caller.setVarOrder0Caller(neworder);
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
		caller.addVarBlock0Caller(first, last, fixed);
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
		caller.clearVarBlocks0Caller();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#printOrder()
	 */
	public void printOrder() {
		caller.printVarTree0Caller();
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
		return caller.getAllocNum0Caller();
	}
	// private static native int getAllocNum0();

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#getNodeNum()
	 */
	public int getNodeNum() {
		return caller.getNodeNum0Caller();
	}
	// private static native int getNodeNum0();

	/*
	 * (non-Javadoc) - DONE
	 * 
	 * @see net.sf.javabdd.BDDFactory#getCacheSize()
	 */
	public int getCacheSize() {
		return caller.getCacheSize0Caller();
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
		caller.printStat0Caller();
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

	public static class CUDDADDFactoryGeneric extends CUDDFactoryGeneric {

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
			CUDDFactoryGeneric resF;
			if (!CUDDFactoryDone) {
				throw new BDDException("Cannot initialize CUDD BDD and CUDD ADD factories at the same time");
			}
			if (!CUDDADDFactoryDone) {
				return addInstance;
			}
			resF = new CUDDFactoryGeneric.CUDDADDFactoryGeneric();
			resF.initialize(nodenum / 256, cachesize);
			return resF;
		}

		@Override
		public String getVersion() {
			return "CUDD-ADD " + cuddVer;
		}

		@Override
		public ADD plusInfinity() {
			long res = caller.arithmeticPlusInfinity0Caller();
			return new CUDDADD(res);
		}

		@Override
		public ADD minusInfinity() {
			long res = caller.arithmeticMinusInfinity0Caller();
			return new CUDDADD(res);
		}

		@Override
		public ADD constant(double value) {
			if (value == 0.0 || value == 1.0) {
				return (ADD) super.constant(value);
			}
			long ddnode = caller.addConst0Caller(value);
			return new CUDDADD(ddnode);
		}

		@Override
		public boolean isADD() {
			return true;
		}

		/*
		 * @Override public boolean thereIsNegativeCycle(ADD transFromSrcStates, ADD
		 * arena, BDDVarSet primedVariables, BDDPairing primedUnprimedPairs) { boolean
		 * res = negCycleCheck0(((CUDDADD) arena)._ddnode_ptr, ((CUDDADD)
		 * transFromSrcStates)._ddnode_ptr,
		 * ((CUDDADD)primedVariables.toBDD())._ddnode_ptr, ((CUDDADDPairing)
		 * primedUnprimedPairs)._ptr);
		 * 
		 * return res; }
		 */

		@Override
		/*
		 * (non-Javadoc) not multi threaded safe!
		 * 
		 * @see net.sf.javabdd.CUDDFactory#initialize(int, int)
		 */
		protected void initialize(String packageVer, int nodenum, int cachesize) {
			if (CUDDADDFactoryDone) {
				CUDDADDFactoryDone = false;
				addInstance = this;
				this.loadCUDDCaller(packageVer);
				if (CUDDFactoryDone && bddInstance == null && !caller.isInitialized0Caller()) { // avoid calling
																								// initialize0() more
																								// than once
					caller.initialize0Caller(nodenum, cachesize);
				}
			}
		}

		@Override
		public boolean isInitialized() {
			return ((!CUDDADDFactoryDone) && (addInstance != null) && caller.isInitialized0Caller());
		}

		@Override
		public ADD zero() {
			ADD res = new CUDDADD(caller.arithmeticZero0Caller());
			return res;
		}

		@Override
		public ADD one() {
			ADD res = new CUDDADD(caller.arithmeticLogicOne0Caller());
			return res;
		}

		@Override
		public ADD ithVar(int var) {
			long id = caller.ithVar0Caller(var, isADD());
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
			if (!CUDDADDFactoryDone) {
				CUDDADDFactoryDone = true;
				if (CUDDFactoryDone) {
					caller.done0Caller();
				}
				addInstance.caller = null;
				addInstance = null;
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
			caller.printSet0Caller(this._ddnode_ptr, 2);
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
			return this._ddnode_ptr == caller.getBddZero();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#isOne()
		 */
		public boolean isOne() {
			return this._ddnode_ptr == caller.getOne();
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
			long res = caller.toADD0Caller(this._ddnode_ptr);
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
			return caller.var0Caller(_ddnode_ptr);
		}
		// private static native int var0(long b);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#high()
		 */
		public BDD high() {
			long b = caller.high0Caller(_ddnode_ptr, this.isADD());
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
			long b = caller.low0Caller(_ddnode_ptr, this.isADD());
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
			caller.addRefCaller(this._ddnode_ptr);
			return res;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#not()
		 */
		public BDD not() {
			long b = caller.not0Caller(_ddnode_ptr, false);
			return new CUDDBDD(b);
		}
		// private static native long not0(long b);

		protected long iteCaller(BDD thenBDD, BDD elseBDD) {
			CUDDBDD c = (CUDDBDD) thenBDD;
			CUDDBDD d = (CUDDBDD) elseBDD;
			return caller.ite0Caller(_ddnode_ptr, c._ddnode_ptr, d._ddnode_ptr, this.isADD());
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
			long b = caller.relprod0Caller(_ddnode_ptr, c._ddnode_ptr, d._ddnode_ptr);
			return new CUDDBDD(b);
		}
		// private static native long relprod0(long b, long c, long d);

		protected long composeCaller(BDD that, int var) {
			CUDDBDD c = (CUDDBDD) that;
			return caller.compose0Caller(_ddnode_ptr, c._ddnode_ptr, var, this.isADD());
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
			return caller.exist0Caller(_ddnode_ptr, c._ddnode_ptr, this.isADD());
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
			return caller.forAll0Caller(_ddnode_ptr, c._ddnode_ptr, this.isADD());
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
			long b = caller.restrict0Caller(_ddnode_ptr, c._ddnode_ptr);
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
			long b = caller.restrictWith0Caller(this._ddnode_ptr, c._ddnode_ptr, (this != c));
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
			long b = caller.simplify0Caller(_ddnode_ptr, c._ddnode_ptr, this.isADD());
			return new CUDDBDD(b);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#support()
		 */
		public BDDVarSet support() {
			long b = caller.support0Caller(_ddnode_ptr, this.isADD());
			return new BDDVarSet.DefaultImpl(new CUDDBDD(b));
		}
		// private static native long support0(long b);

		protected long applyCaller(BDD that, BDDFactory.BDDOp opr) {
			CUDDBDD c = (CUDDBDD) that;
			return caller.apply0Caller(_ddnode_ptr, c._ddnode_ptr, opr.id, this.isADD(), false, false);
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
			long b = caller.apply0Caller(_ddnode_ptr, c._ddnode_ptr, opr.id, this.isADD(), true, (this != c));
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
		 * @see net.sf.javabdd.BDD#satOne()
		 */
		public BDD satOne(BDDVarSet var) {
			CUDDBDD c = (CUDDBDD) ((BDDVarSet.DefaultImpl) var).b;
			long b = caller.satOne0Caller(_ddnode_ptr, c._ddnode_ptr, this.isADD());
			return new CUDDBDD(b);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#nodeCount()
		 */
		public int nodeCount() {
			return caller.nodeCount0Caller(_ddnode_ptr);
		}
		// private static native int nodeCount0(long b);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#pathCount()
		 */
		public double pathCount() {
			return caller.pathCount0Caller(_ddnode_ptr);
		}
		// private static native double pathCount0(long b);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#satCount()
		 */
		public double satCount() {
			return caller.satCount0Caller(_ddnode_ptr);
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
			int[] profile = caller.varSupportIndex0Caller(_ddnode_ptr);
			return profile;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#free()
		 */
		public void free() {
			caller.delRefCaller(_ddnode_ptr, this.isADD());
			_ddnode_ptr = INVALID_BDD;
		}

		protected long veccomposeCaller(BDDPairing pair) {
			CUDDBDDPairing p = (CUDDBDDPairing) pair;
			return caller.veccompose0Caller(_ddnode_ptr, p._ptr, this.isADD());
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
			return caller.replace0Caller(_ddnode_ptr, p._ptr, this.isADD());
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
			long b = caller.replaceWith0Caller(_ddnode_ptr, p._ptr, this.isADD());
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
			return this._ddnode_ptr == ((CUDDBDD) that)._ddnode_ptr;
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
			long b = caller.support0Caller(_ddnode_ptr, this.isADD());
			return new BDDVarSet.DefaultImpl(new CUDDADD(b));
		}

		@Override
		public ADD simplify(BDDVarSet d) {
			CUDDADD c = (CUDDADD) d.toBDD();
			long b = caller.simplify0Caller(_ddnode_ptr, c._ddnode_ptr, this.isADD());
			return new CUDDADD(b);
		}

		@Override
		public ADD restrict(ADD var) {
			CUDDADD c = (CUDDADD) var;
			long b = caller.restrict0Caller(_ddnode_ptr, c._ddnode_ptr);
			return new CUDDADD(b);
		}

		@Override
		public ADD restrictWith(ADD that) {
			CUDDADD c = (CUDDADD) that;
			long b = caller.restrictWith0Caller(this._ddnode_ptr, c._ddnode_ptr, (this != c));
			if (this != c) {
				c._ddnode_ptr = INVALID_BDD;
			}
			this._ddnode_ptr = b;
			return this;
		}

		@Override
		public ADD minimizeValue(double value) {
			long res = caller.addMinimizeVal0Caller(this._ddnode_ptr, value);
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
			CUDDADD c = (CUDDADD) ((BDDVarSet.DefaultImpl) var).b;
			long b = caller.satOne0Caller(_ddnode_ptr, c._ddnode_ptr, this.isADD());
			return new CUDDADD(b);
		}

		@Override
		public ADD id() {
			ADD res = new CUDDADD(this._ddnode_ptr);
			caller.addRefCaller(this._ddnode_ptr);
			return res;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#isZero()
		 */
		@Override
		public boolean isZero() {
			return this._ddnode_ptr == caller.getAddZero();
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.CUDDFactory.CUDDBDD#not()
		 */
		public ADD not() {
			long b = caller.not0Caller(_ddnode_ptr, this.isADD());
			return new CUDDADD(b);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDD#low()
		 */
		@Override
		public ADD low() {
			long b = caller.low0Caller(_ddnode_ptr, this.isADD());
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
			long b = caller.high0Caller(_ddnode_ptr, this.isADD());
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
			return caller.isAddConst0Caller(this._ddnode_ptr);
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
			return new CUDDADD(caller.addFindMax0Caller(this._ddnode_ptr));
		}

		@Override
		public ADD findMin() {
			return new CUDDADD(caller.addFindMin0Caller(this._ddnode_ptr));
		}

		@Override
		public boolean isZeroOneADD() {
			return caller.isZeroOneADD0Caller(this._ddnode_ptr);
		}

		@Override
		public ADD additiveNot() {
			return new CUDDADD(caller.addAdditiveNeg0Caller(this._ddnode_ptr));
		}

		@Override
		public ADD applyLog() {
			return new CUDDADD(caller.addApplyLog0Caller(this._ddnode_ptr));
		}

		@Override
		public ADD toZeroOneSLTThreshold(double threshold) {
			long result = caller.convertToZeroOneADDByThres0Caller(this._ddnode_ptr,
					CUDDADDFactoryGeneric.ThresholdOp.SLT.getOpNumber(), threshold);
			return new CUDDADD(result);
		}

		@Override
		public ADD toZeroOneSGTThreshold(double threshold) {
			long result = caller.convertToZeroOneADDByThres0Caller(this._ddnode_ptr,
					CUDDADDFactoryGeneric.ThresholdOp.SGT.getOpNumber(), threshold);
			return new CUDDADD(result);
		}

		@Override
		public ADD toZeroOneLTThreshold(double threshold) {
			long result = caller.convertToZeroOneADDByThres0Caller(this._ddnode_ptr,
					CUDDADDFactoryGeneric.ThresholdOp.LT.getOpNumber(), threshold);
			return new CUDDADD(result);
		}

		@Override
		public ADD toZeroOneGTThreshold(double threshold) {
			long result = caller.convertToZeroOneADDByThres0Caller(this._ddnode_ptr,
					CUDDADDFactoryGeneric.ThresholdOp.GT.getOpNumber(), threshold);
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
				return caller.retrieveConstValue0Caller(this._ddnode_ptr);
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
			long res = caller.toBDD0Caller(this._ddnode_ptr);
			return new CUDDBDD(res);
		}

		@Override
		public BDD toBDDByThreshold(double threshold) {
			if (CUDDFactoryDone) {
				throw new BDDException(
						"CUDD BDD Factory must be initialized before calling toBDDByThreshold() method!");
			}
			long res = caller.toBDDThresholdCaller(this._ddnode_ptr, threshold);
			return new CUDDBDD(res);
		}

		@Override
		public BDD toBDDByStrictThreshold(double threshold) {
			if (CUDDFactoryDone) {
				throw new BDDException(
						"CUDD BDD Factory must be initialized before calling toBDDByStrictThreshold() method!");
			}
			long res = caller.toBDDStrictThresholdCaller(this._ddnode_ptr, threshold);
			return new CUDDBDD(res);
		}

		@Override
		public BDD toBDDByInterval(double lower, double upper) {
			if (CUDDFactoryDone) {
				throw new BDDException("CUDD BDD Factory must be initialized before calling toBDDByInterval() method!");
			}
			long res = caller.toBDDIntervalCaller(this._ddnode_ptr, lower, upper);
			return new CUDDBDD(res);
		}

		@Override
		public BDD toBDDByIthBit(int bit) {
			if (CUDDFactoryDone) {
				throw new BDDException("CUDD BDD Factory must be initialized before calling toBDDByIthBit() method!");
			}
			long res = caller.toBDDIthBitCaller(this._ddnode_ptr, bit);
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
			long res = caller.addAbstractMin0Caller(_ddnode_ptr, c._ddnode_ptr);
			return new CUDDADD(res);
		}

		@Override
		public ADD abstractSum(BDDVarSet d) {
			CUDDADD c = (CUDDADD) ((BDDVarSet.DefaultImpl) d).b;
			long res = caller.arithmeticExist0Caller(_ddnode_ptr, c._ddnode_ptr);
			return new CUDDADD(res);
		}

		@Override
		public ADD abstractMax(BDDVarSet d) {
			CUDDADD c = (CUDDADD) ((BDDVarSet.DefaultImpl) d).b;
			long res = caller.addAbstractMax0Caller(_ddnode_ptr, c._ddnode_ptr);
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
			caller.printDot0Caller(this._ddnode_ptr);
		};

		@Override
		public ADD determinizeController(BDDVarSet d) {
			CUDDADD c = (CUDDADD) ((BDDVarSet.DefaultImpl) d).b;
			long res = caller.determinizeController0Caller(_ddnode_ptr, c._ddnode_ptr);
			return new CUDDADD(res);
		}

		@Override
		public ADD engSubtract(ADD that, double maxEnergy) {
			CUDDADD b = (CUDDADD) that;
			long res = caller.addEngSubtract0Caller(this._ddnode_ptr, b._ddnode_ptr, maxEnergy);
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
			_ptr = caller.allocCaller(isADDPairing());
		}

		// private static native long alloc();

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDDPairing#set(int, int)
		 */
		public void set(int oldvar, int newvar) {
			caller.set0Caller(_ptr, oldvar, newvar, isADDPairing());
		}
		// private static native void set0(long p, int oldvar, int newvar);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDDPairing#set(int, net.sf.javabdd.BDD)
		 */
		public void set(int oldvar, BDD newvar) {
			CUDDBDD c = (CUDDBDD) newvar;
			caller.set2Caller(_ptr, oldvar, c._ddnode_ptr);
		}
		// private static native void set2(long p, int oldvar, long newbdd);

		/*
		 * (non-Javadoc)
		 * 
		 * @see net.sf.javabdd.BDDPairing#reset()
		 */
		public void reset() {
			caller.reset0Caller(_ptr, isADDPairing());
		}
		// private static native void nativeCaller.reset0Caller(long ptr);

		/**
		 * Free the memory allocated for this pair.
		 */
		// private static native void nativeCaller.free0Caller(long ptr);
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

	public void main(String[] args) {
		BDDFactory bdd = init(1000000, 100000);

		System.out.println("One: " + caller.getOne());
		System.out.println("Zero: " + caller.getBddZero());

		BDDDomain[] doms = bdd.extDomain(new int[] { 50, 10, 15, 20, 15 });

		BDD b = bdd.one();
		for (int i = 0; i < doms.length - 1; ++i) {
			b.andWith(doms[i].ithVar(i));
		}

		for (int i = 0; i < bdd.numberOfDomains(); ++i) {
			BDDDomain d = bdd.getDomain(i);
			int[] ivar = d.vars();
			System.out.print("Domain #" + i + ":");
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

		System.out.println("Before replace(): " + b);
		BDD c = b.replace(p);
		System.out.println("After replace(): " + c);

		c.printDot();
	}

	public final String REVISION = "$Revision: 1.3 $";

	/*
	 * (non-Javadoc)
	 * 
	 * @see net.sf.javabdd.BDDFactory#getVersion()
	 */
	public String getVersion() {
		return "CUDD " + cuddVer;
	}

}
