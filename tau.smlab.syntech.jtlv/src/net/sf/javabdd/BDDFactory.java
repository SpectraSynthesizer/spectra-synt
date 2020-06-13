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

// BDDFactory.java, created Jan 29, 2003 9:50:57 PM by jwhaley
// Copyright (C) 2003 John Whaley
// Licensed under the terms of the GNU LGPL; see COPYING for details.
package net.sf.javabdd;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigInteger;
import java.security.AccessControlException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.ModuleVariableException;

/**
 * <p>
 * Interface for the creation and manipulation of BDDs.
 * </p>
 * 
 * @see net.sf.javabdd.BDD
 * 
 * @author John Whaley
 * @version $Id: BDDFactory.java,v 1.2 2009/10/18 19:30:54 uid228351 Exp $
 */
public abstract class BDDFactory {

	public static final String getProperty(String key, String def) {
		try {
			return System.getProperty(key, def);
		} catch (AccessControlException e) {
			return def;
		}
	}

	/**
	 * <p>
	 * Initializes a BDD factory with the given initial node table size and
	 * operation cache size. Tries to use the "buddy" native library; if it fails,
	 * it falls back to the "java" library.
	 * </p>
	 * 
	 * @param nodenum
	 *            initial node table size
	 * @param cachesize
	 *            operation cache size
	 * @return BDD factory object
	 */
	public static BDDFactory init(int nodenum, int cachesize) {
		String bddpackage = getProperty("bdd", "buddy");
		return init(bddpackage, null, nodenum, cachesize);
	}

	/**
	 * <p>
	 * Initializes a BDD factory of the given type with the given initial node table
	 * size and operation cache size. The type is a string that can be "buddy",
	 * "cudd", "cal", "j", "java", "jdd", "test", "typed", or a name of a class that
	 * has an init() method that returns a BDDFactory. If it fails, it falls back to
	 * the "java" factory.
	 * </p>
	 * 
	 * @param bddpackage
	 *            BDD package string identifier
	 * @param nodenum
	 *            initial node table size
	 * @param cachesize
	 *            operation cache size
	 * @return BDD factory object
	 */
	public static BDDFactory init(String bddpackage, int nodenum, int cachesize) {
		return init(bddpackage, null, nodenum, cachesize);
	}

	/**
	 * <p>
	 * Initializes a BDD factory of the given type with the given initial node table
	 * size and operation cache size. The type is a string that can be "buddy",
	 * "cudd", "cal", "j", "java", "jdd", "test", "typed", or a name of a class that
	 * has an init() method that returns a BDDFactory. If it fails, it falls back to
	 * the "java" factory.
	 * </p>
	 * 
	 * @param bddpackage
	 *            BDD package string identifier
	 * @param bddpackageVer
	 *            BDD package version string identifier. If null, the default
	 *            version is used
	 * @param nodenum
	 *            initial node table size
	 * @param cachesize
	 *            operation cache size
	 * @return BDD factory object
	 */
	public static BDDFactory init(String bddpackage, String bddpackageVer, int nodenum, int cachesize) {
		try {
			if (bddpackage.equals("net.sf.javabdd.CUDDFactory")) {
				if (bddpackageVer != null && bddpackageVer.equals("3.0_PIPE")) {
					return CUDDFactoryGeneric.init("3.0_PIPE", nodenum, cachesize);
				}
				return CUDDFactory.init(nodenum, cachesize);
			}
			if (bddpackage.equals("net.sf.javabdd.CUDDFactory$CUDDADDFactory")) {
				if (bddpackageVer != null && bddpackageVer.equals("3.0_PIPE")) {
					return CUDDFactoryGeneric.CUDDADDFactoryGeneric.init("3.0_PIPE", nodenum, cachesize);
				}
				return CUDDFactory.CUDDADDFactory.init(nodenum, cachesize);
			}
		} catch (LinkageError e) {
			System.out.println("Could not load BDD package " + bddpackage + ": " + e.getLocalizedMessage());
		}
		try {
			Class<?> c = Class.forName((bddpackage.equals("cuddadd")) ? "cudd" : bddpackage);
			Method m = (bddpackage.equals("cuddadd"))
					? c.getMethod("init", new Class[] { int.class, int.class, boolean.class })
					: c.getMethod("init", new Class[] { int.class, int.class });
			return (BDDFactory) ((bddpackage.equals("cuddadd"))
					? m.invoke(null, new Object[] { new Integer(nodenum), new Integer(cachesize), new Boolean(true) })
					: m.invoke(null, new Object[] { new Integer(nodenum), new Integer(cachesize) }));
		} catch (Exception e) {
		}
		// falling back to default java implementation.
		return JTLVJavaFactory.init(nodenum, cachesize);
	}

	/**
	 * Logical 'and' (BDDs and ADDs; for non 0-1 ADDs this operator is equivalent to
	 * multiplication).
	 */
	public static final BDDOp and = new BDDOp(0, "and");

	/**
	 * Logical 'xor' (BDDs and 0-1 ADDs only).
	 */
	public static final BDDOp xor = new BDDOp(1, "xor");

	/**
	 * Logical 'or' (BDDs and 0-1 ADDs only).
	 */
	public static final BDDOp or = new BDDOp(2, "or");

	/**
	 * Logical 'nand' (BDDs and 0-1 ADDs only).
	 */
	public static final BDDOp nand = new BDDOp(3, "nand");

	/**
	 * Logical 'nor' (BDDs and 0-1 ADDs only).
	 */
	public static final BDDOp nor = new BDDOp(4, "nor");

	/**
	 * Logical 'implication' (BDDs and 0-1 ADDs only).
	 */
	public static final BDDOp imp = new BDDOp(5, "imp");

	/**
	 * Logical 'bi-implication' (BDDs and 0-1 ADDs only).
	 */
	public static final BDDOp biimp = new BDDOp(6, "biimp");

	/**
	 * Set difference (BDDs and 0-1 ADDs only).
	 */
	public static final BDDOp diff = new BDDOp(7, "diff");

	/**
	 * Less than (BDDs and ADDs).
	 */
	public static final BDDOp less = new BDDOp(8, "less");

	/**
	 * Inverse implication (BDDs and 0-1 ADDs only).
	 */
	public static final BDDOp invimp = new BDDOp(9, "invimp");

	/**
	 * Arithmetic multiplication (ADDs only).
	 */
	public static final BDDOp times = new BDDOp(10, "times");
	/**
	 * Arithmetic division (ADDs only).
	 */
	public static final BDDOp div = new BDDOp(11, "div");
	/**
	 * Arithmetic addition (ADDs only).
	 */
	public static final BDDOp plus = new BDDOp(12, "plus");
	/**
	 * Arithmetic subtraction (ADDs only).
	 */
	public static final BDDOp minus = new BDDOp(13, "minus");
	/**
	 * Maximum (ADDs only).
	 */
	public static final BDDOp max = new BDDOp(14, "max");
	/**
	 * Minimum (ADDs only).
	 */
	public static final BDDOp min = new BDDOp(15, "min");
	/**
	 * <p>
	 * Energy (or Mean Payoff) games Arithmetic subtraction (ADDs only). This
	 * operation is an arithmetic subtraction that is defined differently in the
	 * following cases:
	 * </p>
	 * +INF - y = +INF , for all finite y <br>
	 * y - (+INF) = 0 , for all finite y <br>
	 * y - (-INF) = +INF , for all finite, non negative, y <br>
	 * +INF - (+INF) = 0 <br>
	 * +INF - (-INF) = +INF <br>
	 * x - y = max {x-y, 0} , for all finite, non negative x, and for all y
	 */
	public static final BDDOp engMinus = new BDDOp(16, "engMinus");
	/**
	 * <p>
	 * Energy (or Mean Payoff) games Arithmetic addition (ADDs only). This operation
	 * is an arithmetic addition that is defined differently in the following cases:
	 * </p>
	 * +INF + y = +INF , for all finite y <br>
	 * y + (+INF) = +INF , for all finite y <br>
	 * y + (-INF) = -INF , for all finite y <br>
	 * (-INF) + y = -INF , for all finite y <br>
	 * +INF + (+INF) = +INF <br>
	 * -INF + (-INF) = -INF <br>
	 * +INF + (-INF) = 0 <br>
	 * (-INF) + (+INF) = 0 <br>
	 */
	public static final BDDOp engPlus = new BDDOp(17, "engPlus");
	/**
	 * <p>
	 * Agreement operator (ADDs only). This operator is similar to xnor, such that
	 * <br>
	 * ADD1 op ADD2 = 1 if ADD1==ADD2, and ADD1 op ADD2 = 0 otherwise.
	 * </p>
	 */
	public static final BDDOp agree = new BDDOp(18, "agree");

	/**
	 * <p>
	 * Enumeration class for binary operations on BDDs. Use the static fields in
	 * BDDFactory to access the different binary operations.
	 * </p>
	 */
	public static class BDDOp {
		final int id;
		final String name;

		private BDDOp(int id, String name) {
			this.id = id;
			this.name = name;
		}

		public String toString() {
			return name;
		}
	}

	/**
	 * <p>
	 * Construct a new BDDFactory.
	 * </p>
	 */
	protected BDDFactory() {
		System.out.println(this.getInfo());
	}

	/**
	 * <p>
	 * Get info string of BDD Factory
	 * </p>
	 */
	public String getInfo() {
		String s = this.getClass().toString();
		s = s.substring(s.lastIndexOf('.') + 1);
		return "Using BDD Package: " + s + ", Version: " + this.getVersion();
	}

	/**
	 * <p>
	 * Returns true if this is a ZDD factory, false otherwise.
	 * </p>
	 */
	public boolean isZDD() {
		return false;
	}

	/**
	 * <p>
	 * Returns true if this is an ADD (a.k.a MTBDD) factory, false otherwise.
	 * </p>
	 */
	public boolean isADD() {
		return false;
	}

	/**
	 * <p>
	 * Get a constant BDD by value: <br>
	 * If this is a BDD factory, for value of 0 the constant false BDD is returned.
	 * Otherwise, for any non-zero value the constant true BDD is returned.<br>
	 * If this is an ADD factory, the constant ADD for the value is returned (not
	 * implemented by default).
	 * </p>
	 * <h4>Implementation notes:</h4> This method is not implemented in the
	 * {@code abstract BDDFactory class} for ADDs and must be overridden for ADD
	 * factories.
	 * 
	 * @param value
	 *            the constant BDD\ADD value
	 * @return constant BDD\ADD for the value
	 */
	/*
	 * default implementation: for an ADD factory this method should be overridden
	 */
	public BDD constant(double value) {
		if (value == 0.0) {
			return zero();
		}
		return one();
	}

	/**
	 * <p>
	 * Get the constant plus infinity ADD.
	 * </p>
	 * <h4>Implementation notes:</h4> This method throws by default
	 * {@code UnsupportedOperationException} and must be overridden for ADD
	 * factories.
	 * 
	 * @throws UnsupportedOperationException
	 * @return constant plus infinity ADD
	 */
	public ADD plusInfinity() {
		throw new UnsupportedOperationException();
	}

	/**
	 * <p>
	 * Get the constant minus infinity ADD.
	 * </p>
	 * <h4>Implementation notes:</h4> This method throws by default
	 * {@code UnsupportedOperationException} and must be overridden for ADD
	 * factories.
	 * 
	 * @throws UnsupportedOperationException
	 * @return constant minus infinity ADD
	 */
	public ADD minusInfinity() {
		throw new UnsupportedOperationException();
	}

	/**
	 * <p>
	 * Get the constant false BDD.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_false.
	 * </p>
	 */
	public abstract BDD zero();

	/**
	 * <p>
	 * Get the constant true BDD.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_true.
	 * </p>
	 */
	public abstract BDD one();

	/**
	 * <p>
	 * Get the constant universe BDD. (The universe BDD differs from the one BDD in
	 * ZDD mode.)
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_true.
	 * </p>
	 */
	public BDD universe() {
		return one();
	}

	/**
	 * <p>
	 * Get an empty BDDVarSet.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_true.
	 * </p>
	 */
	public BDDVarSet emptySet() {
		return new BDDVarSet.DefaultImpl(one());
	}

	/**
	 * <p>
	 * Build a cube from an array of variables.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_buildcube.
	 * </p>
	 */
	public BDD buildCube(int value, List<BDD> variables) {
		BDD result = universe();
		Iterator<BDD> i = variables.iterator();
		while (i.hasNext()) {
			BDD var = i.next();
			if ((value & 0x1) != 0)
				var = var.id();
			else
				var = var.not();
			result.andWith(var);
			value >>= 1;
		}
		return result;
	}

	/**
	 * <p>
	 * Build a cube from an array of variables.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_ibuildcube./p>
	 */
	public BDD buildCube(int value, int[] variables) {
		BDD result = universe();
		for (int z = 0; z < variables.length; z++, value >>= 1) {
			BDD v;
			if ((value & 0x1) != 0)
				v = ithVar(variables[variables.length - z - 1]);
			else
				v = nithVar(variables[variables.length - z - 1]);
			result.andWith(v);
		}
		return result;
	}

	/**
	 * <p>
	 * Builds a BDD variable set from an integer array. The integer array
	 * <tt>varset</tt> holds the variable numbers. The BDD variable set is
	 * represented by a conjunction of all the variables in their positive form.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_makeset.
	 * </p>
	 */
	public BDDVarSet makeSet(int[] varset) {
		BDDVarSet res = emptySet();
		int varnum = varset.length;
		for (int v = varnum - 1; v >= 0; --v) {
			res.unionWith(varset[v]);
		}
		return res;
	}

	/**** STARTUP / SHUTDOWN ****/

	/**
	 * <p>
	 * Compare to bdd_init.
	 * </p>
	 * 
	 * @param nodenum
	 *            the initial number of BDD nodes
	 * @param cachesize
	 *            the size of caches used by the BDD operators
	 */
	protected abstract void initialize(int nodenum, int cachesize);

	/**
	 * <p>
	 * Returns true if this BDD factory is initialized, false otherwise.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_isrunning.
	 * </p>
	 * 
	 * @return true if this BDD factory is initialized
	 */
	public abstract boolean isInitialized();

	/**
	 * <p>
	 * Reset the BDD factory to its initial state. Everything is reallocated from
	 * scratch. This is like calling done() followed by initialize().
	 * </p>
	 */
	public void reset() {
		int nodes = getNodeTableSize();
		int cache = getCacheSize();
		domain = null;
		fdvarnum = 0;
		firstbddvar = 0;
		done();
		initialize(nodes, cache);
	}

	/**
	 * <p>
	 * This function frees all memory used by the BDD package and resets the package
	 * to its uninitialized state. The BDD package is no longer usable after this
	 * call.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_done.
	 * </p>
	 */
	public abstract void done();

	/**
	 * <p>
	 * Sets the error condition. This will cause the BDD package to throw an
	 * exception at the next garbage collection.
	 * </p>
	 * 
	 * @param code
	 *            the error code to set
	 */
	public abstract void setError(int code);

	/**
	 * <p>
	 * Clears any outstanding error condition.
	 * </p>
	 */
	public abstract void clearError();

	/**** CACHE/TABLE PARAMETERS ****/

	/**
	 * <p>
	 * Set the maximum available number of BDD nodes.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_setmaxnodenum.
	 * </p>
	 * 
	 * @param size
	 *            maximum number of nodes
	 * @return old value
	 */
	public abstract int setMaxNodeNum(int size);

	/**
	 * <p>
	 * Set minimum percentage of nodes to be reclaimed after a garbage collection.
	 * If this percentage is not reclaimed, the node table will be grown. The range
	 * of x is 0..1. The default is .20.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_setminfreenodes.
	 * </p>
	 * 
	 * @param x
	 *            number from 0 to 1
	 * @return old value
	 */
	public abstract double setMinFreeNodes(double x);

	/**
	 * <p>
	 * Set maximum number of nodes by which to increase node table after a garbage
	 * collection.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_setmaxincrease.
	 * </p>
	 * 
	 * @param x
	 *            maximum number of nodes by which to increase node table
	 * @return old value
	 */
	public abstract int setMaxIncrease(int x);

	/**
	 * <p>
	 * Set factor by which to increase node table after a garbage collection. The
	 * amount of growth is still limited by <tt>setMaxIncrease()</tt>.
	 * </p>
	 * 
	 * @param x
	 *            factor by which to increase node table after GC
	 * @return old value
	 */
	public abstract double setIncreaseFactor(double x);

	/**
	 * <p>
	 * Sets the cache ratio for the operator caches. When the node table grows,
	 * operator caches will also grow to maintain the ratio.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_setcacheratio.
	 * </p>
	 * 
	 * @param x
	 *            cache ratio
	 */
	public abstract double setCacheRatio(double x);

	/**
	 * <p>
	 * Sets the node table size.
	 * </p>
	 * 
	 * @param n
	 *            new size of table
	 * @return old size of table
	 */
	public abstract int setNodeTableSize(int n);

	/**
	 * <p>
	 * Sets cache size.
	 * </p>
	 * 
	 * @return old cache size
	 */
	public abstract int setCacheSize(int n);

	/**** VARIABLE NUMBERS ****/

	/**
	 * <p>
	 * Returns the number of defined variables.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_varnum.
	 * </p>
	 */
	public abstract int varNum();

	/**
	 * <p>
	 * Set the number of used BDD variables. It can be called more than one time,
	 * but only to increase the number of variables.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_setvarnum.
	 * </p>
	 * 
	 * @param num
	 *            new number of BDD variables
	 * @return old number of BDD variables
	 */
	public abstract int setVarNum(int num);

	/**
	 * <p>
	 * Add extra BDD variables. Extends the current number of allocated BDD
	 * variables with num extra variables.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_extvarnum.
	 * </p>
	 * 
	 * @param num
	 *            number of BDD variables to add
	 * @return old number of BDD variables
	 */
	public int extVarNum(int num) {
		int start = varNum();
		if (num < 0 || num > 0x3FFFFFFF)
			throw new BDDException();
		setVarNum(start + num);
		return start;
	}

	/**
	 * <p>
	 * Returns a BDD representing the I'th variable. (One node with the children
	 * true and false.) The requested variable must be in the (zero-indexed) range
	 * defined by <tt>setVarNum</tt>.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_ithvar.
	 * </p>
	 * 
	 * @param var
	 *            the variable number
	 * @return the I'th variable on success, otherwise the constant false BDD
	 */
	public abstract BDD ithVar(int var);

	/**
	 * <p>
	 * Returns a BDD representing the negation of the I'th variable. (One node with
	 * the children false and true.) The requested variable must be in the
	 * (zero-indexed) range defined by <tt>setVarNum</tt>.<br>
	 * If it is a MTBDD \ ADD, the negation is the complement ADD, i.e. every zero
	 * terminal becomes one, and every non-zero terminal becomes zero.
	 * </p>
	 * <p>
	 * Compare to bdd_nithvar.
	 * </p>
	 * 
	 * @param var
	 *            the variable number
	 * @return the negated I'th variable on success, otherwise the constant false
	 *         BDD
	 */
	public abstract BDD nithVar(int var);

	/**** INPUT / OUTPUT ****/

	/**
	 * <p>
	 * Prints all used entries in the node table.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_printall.
	 * </p>
	 */
	public abstract void printAll();

	/**
	 * <p>
	 * Prints the node table entries used by a BDD.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_printtable.
	 * </p>
	 */
	public abstract void printTable(BDD b);

	/**
	 * <p>
	 * Loads a BDD from a file.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_load.
	 * </p>
	 */
	public BDD load(String filename) throws IOException {
		BufferedReader r = null;
		try {
			r = new BufferedReader(new FileReader(filename));
			BDD result = load(r);
			return result;
		} finally {
			if (r != null)
				try {
					r.close();
				} catch (IOException e) {
				}
		}
	}
	// TODO: error code from bdd_load (?)

	/**
	 * <p>
	 * Loads a BDD from the given input, translating BDD variables according to the
	 * given map.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_load.
	 * </p>
	 * 
	 * @param ifile
	 *            reader
	 * @param translate
	 *            variable translation map
	 * @return BDD
	 */
	public BDD load(BufferedReader ifile) throws IOException {

		Map<Long, BDD> map = new HashMap<>();
		map.put(0l, zero());
		map.put(1l, universe());
		tokenizer = null;

		long vnum = Long.parseLong(readNext(ifile));

		// Check for constant true / false
		if (vnum == 0) {
			long r = Long.parseLong(readNext(ifile));
			return r == 0 ? zero() : universe();
		}

		if (vnum > varNum()) {
			System.err.println(
					"Warning! Missing some variables. Have " + varNum() + " need " + vnum + ". Filling with dummys.");
			while (vnum > varNum()) {
				try {
					Env.newVar("dummy" + Math.random());
				} catch (ModuleVariableException e) {
					throw new RuntimeException(e);
				}
			}
		} else if (vnum < varNum()) {
			System.err.println("Warning! Env has more vars than expected by BDD.");
		}

		disableReorder(); // FIXME remember whether to enable later

		int[] order = new int[(int) vnum];
		for (int i = 0; i < vnum; i++) {
			order[i] = Integer.parseInt(readNext(ifile));
		}
		setVarOrder(order);

		BDD root = null;

		while (!doneReading(ifile)) {
			long key = Long.parseLong(readNext(ifile));
			int var = Integer.parseInt(readNext(ifile));
			long lowi = Long.parseLong(readNext(ifile));
			long highi = Long.parseLong(readNext(ifile));

			BDD low, high;

			low = map.get(lowi);
			high = map.get(highi);

			if (low == null || high == null || var < 0)
				throw new BDDException("Incorrect file format");

			BDD b = ithVar(var);
			root = b.ite(high, low);
			b.free();

			map.put(key, root);
		}
		BDD tmproot = root.id();

		Env.free(map);

		// FIXME do not always turn it on (only on demand or if it was already enabled)
		enableReorder();
		return tmproot;
	}

	/**
	 * not done reading if the tokenizer still has tokens or if the file has more
	 * content to be read
	 * 
	 * @param ifile
	 * @return true if more tokens or characters; false otherwise
	 */
	private boolean doneReading(BufferedReader ifile) {
		if (tokenizer != null && tokenizer.hasMoreTokens()) {
			return false;
		}
		try {
			if (ifile.ready()) {
				return false;
			}
		} catch (IOException e) {
		}
		return true;
	}

	/**
	 * Used for tokenization during loading.
	 */
	protected StringTokenizer tokenizer;

	/**
	 * Read the next token from the file.
	 * 
	 * @param ifile
	 *            reader
	 * @return next string token
	 */
	protected String readNext(BufferedReader ifile) throws IOException {
		while (tokenizer == null || !tokenizer.hasMoreTokens()) {
			String s = ifile.readLine();
			if (s == null)
				throw new BDDException("Incorrect file format");
			tokenizer = new StringTokenizer(s);
		}
		return tokenizer.nextToken();
	}

	/**
	 * <p>
	 * Saves a BDD to a file.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_save.
	 * </p>
	 */
	public void save(String filename, BDD var) throws IOException {
		BufferedWriter is = null;
		try {
			is = new BufferedWriter(new FileWriter(filename));

			save(is, var);
		} finally {
			if (is != null)
				try {
					is.close();
				} catch (IOException e) {
				}
		}
	}
	// TODO: error code from bdd_save (?)

	/**
	 * <p>
	 * Saves a BDD to an output writer.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_save.
	 * </p>
	 */
	public void save(BufferedWriter out, BDD r) throws IOException {

		// special encoding of TRUE and FALSE use 0 variables and then 1 (TRUE) or 0
		// (FALSE)
		if (r.isOne() || r.isZero()) {
			out.write("0 " + (r.isOne() ? 1 : 0) + "\n");
			return;
		}

		out.write(varNum() + "\n");
		for (int i : getVarOrder()) {
			out.write(i + " ");
		}
		out.write("\n");

		Set<Long> visited = new HashSet<>();
		save_rec(out, visited, r.id());
	}

	/**
	 * Helper function for save().
	 */
	protected long save_rec(BufferedWriter out, Set<Long> visited, BDD root) throws IOException {
		if (root.isZero()) {
			root.free();
			return 0;
		}
		if (root.isOne()) {
			root.free();
			return 1;
		}
		long i = root.rootId();
		if (visited.contains(i)) {
			root.free();
			return i;
		}
		long v = i;
		visited.add(i);

		BDD h = root.high();

		BDD l = root.low();

		int rootvar = root.var();
		root.free();

		long lo = save_rec(out, visited, l);

		long hi = save_rec(out, visited, h);

		out.write(i + " ");
		out.write(rootvar + " ");
		out.write(lo + " ");
		out.write(hi + "\n");

		return v;
	}

	// TODO: bdd_blockfile_hook
	// TODO: bdd_versionnum, bdd_versionstr

	/**** REORDERING ****/

	/**
	 * <p>
	 * Convert from a BDD level to a BDD variable.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_level2var.
	 * </p>
	 */
	public abstract int level2Var(int level);

	/**
	 * <p>
	 * Convert from a BDD variable to a BDD level.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_var2level.
	 * </p>
	 */
	public abstract int var2Level(int var);

	/**
	 * No reordering.
	 */
	public static final ReorderMethod REORDER_NONE = new ReorderMethod(0, "NONE");

	/**
	 * Reordering using a sliding window of 2.
	 */
	public static final ReorderMethod REORDER_WIN2 = new ReorderMethod(1, "WIN2");

	/**
	 * Reordering using a sliding window of 2, iterating until no further progress.
	 */
	public static final ReorderMethod REORDER_WIN2ITE = new ReorderMethod(2, "WIN2ITE");

	/**
	 * Reordering using a sliding window of 3.
	 */
	public static final ReorderMethod REORDER_WIN3 = new ReorderMethod(5, "WIN3");

	/**
	 * Reordering using a sliding window of 3, iterating until no further progress.
	 */
	public static final ReorderMethod REORDER_WIN3ITE = new ReorderMethod(6, "WIN3ITE");

	/**
	 * Reordering where each block is moved through all possible positions. The best
	 * of these is then used as the new position. Potentially a very slow but good
	 * method.
	 */
	public static final ReorderMethod REORDER_SIFT = new ReorderMethod(3, "SIFT");

	/**
	 * Same as REORDER_SIFT, but the process is repeated until no further progress
	 * is done. Can be extremely slow.
	 */
	public static final ReorderMethod REORDER_SIFTITE = new ReorderMethod(4, "SIFTITE");

	/**
	 * Selects a random position for each variable. Mostly used for debugging
	 * purposes.
	 */
	public static final ReorderMethod REORDER_RANDOM = new ReorderMethod(7, "RANDOM");

	/**
	 * Enumeration class for method reordering techniques. Use the static fields in
	 * BDDFactory to access the different reordering techniques.
	 */
	public static class ReorderMethod {
		final int id;
		final String name;

		private ReorderMethod(int id, String name) {
			this.id = id;
			this.name = name;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			return name;
		}
	}

	/**
	 * <p>
	 * Reorder the BDD with the given method.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_reorder.
	 * </p>
	 */
	public abstract void reorder(ReorderMethod m);

	/**
	 * <p>
	 * Enables automatic reordering. If method is REORDER_NONE then automatic
	 * reordering is disabled.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_autoreorder.
	 * </p>
	 */
	public abstract void autoReorder(ReorderMethod method);

	/**
	 * <p>
	 * Enables automatic reordering with the given (maximum) number of reorderings.
	 * If method is REORDER_NONE then automatic reordering is disabled.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_autoreorder_times.
	 * </p>
	 */
	public abstract void autoReorder(ReorderMethod method, int max);

	/**
	 * <p>
	 * Returns the current reorder method as defined by autoReorder.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_getreorder_method.
	 * </p>
	 * 
	 * @return ReorderMethod
	 */
	public abstract ReorderMethod getReorderMethod();

	/**
	 * <p>
	 * Returns the number of allowed reorderings left. This value can be defined by
	 * autoReorder.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_getreorder_times.
	 * </p>
	 */
	public abstract int getReorderTimes();

	/**
	 * <p>
	 * Disable automatic reordering until enableReorder is called. Reordering is
	 * enabled by default as soon as any variable blocks have been defined.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_disable_reorder.
	 * </p>
	 */
	public abstract void disableReorder();

	/**
	 * <p>
	 * Enable automatic reordering after a call to disableReorder.<br>
	 * The reordering method is left unchanged.
	 * </p>
	 * 
	 * 
	 * <p>
	 * Compare to bdd_enable_reorder.
	 * </p>
	 */
	public abstract void enableReorder();

	/**
	 * Returns true if automatic reordering is enabled.
	 * 
	 * @return Whether automatic reordering is enabled
	 */
	public abstract boolean reorderEnabled();

	/**
	 * <p>
	 * Enables verbose information about reordering. A value of zero means no
	 * information, one means some information and greater than one means lots of
	 * information.
	 * </p>
	 * 
	 * @param v
	 *            the new verbose level
	 * @return the old verbose level
	 */
	public abstract int reorderVerbose(int v);

	/**
	 * <p>
	 * This function sets the current variable order to be the one defined by
	 * neworder. The variable parameter neworder is interpreted as a sequence of
	 * variable indices and the new variable order is exactly this sequence. The
	 * array must contain all the variables defined so far. If, for instance the
	 * current number of variables is 3 and neworder contains [1; 0; 2] then the new
	 * variable order is v1<v0<v2.
	 * </p>
	 * 
	 * <p>
	 * Note that this operation must walk through the node table many times, and
	 * therefore it is much more efficient to call this when the node table is
	 * small.
	 * </p>
	 * 
	 * @param neworder
	 *            new variable order
	 */
	public abstract void setVarOrder(int[] neworder);

	/**
	 * <p>
	 * Gets the current variable order.
	 * </p>
	 * 
	 * @return variable order
	 */
	public int[] getVarOrder() {
		int n = varNum();
		int[] result = new int[n];
		for (int i = 0; i < n; ++i) {
			result[i] = level2Var(i);
		}
		return result;
	}

	/**
	 * <p>
	 * Make a new BDDPairing object.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_newpair.
	 * </p>
	 */
	public abstract BDDPairing makePair();

	/**
	 * Make a new pairing that maps from one variable to another.
	 * 
	 * @param oldvar
	 *            old variable
	 * @param newvar
	 *            new variable
	 * @return BDD pairing
	 */
	public BDDPairing makePair(int oldvar, int newvar) {
		BDDPairing p = makePair();
		p.set(oldvar, newvar);
		return p;
	}

	/**
	 * Make a new pairing that maps from one variable to another BDD.
	 * 
	 * @param oldvar
	 *            old variable
	 * @param newvar
	 *            new BDD
	 * @return BDD pairing
	 */
	public BDDPairing makePair(int oldvar, BDD newvar) {
		BDDPairing p = makePair();
		p.set(oldvar, newvar);
		return p;
	}

	/**
	 * Make a new pairing that maps from one BDD domain to another.
	 * 
	 * @param oldvar
	 *            old BDD domain
	 * @param newvar
	 *            new BDD domain
	 * @return BDD pairing
	 */
	public BDDPairing makePair(BDDDomain oldvar, BDDDomain newvar) {
		BDDPairing p = makePair();
		p.set(oldvar, newvar);
		return p;
	}

	/**
	 * <p>
	 * Swap two variables.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_swapvar.
	 * </p>
	 */
	public abstract void swapVar(int v1, int v2);

	/**** VARIABLE BLOCKS ****/

	/**
	 * <p>
	 * Adds a new variable block for reordering.
	 * </p>
	 * 
	 * <p>
	 * Creates a new variable block with the variables in the variable set var. The
	 * variables in var must be contiguous.
	 * </p>
	 * 
	 * <p>
	 * The fixed parameter sets the block to be fixed (no reordering of its child
	 * blocks is allowed) or free.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_addvarblock.
	 * </p>
	 */
	public void addVarBlock(BDDVarSet var, boolean fixed) {
		int[] v = var.toArray();
		int first, last;
		if (v.length < 1)
			throw new BDDException("Invalid parameter for addVarBlock");

		first = last = v[0];

		for (int n = 1; n < v.length; n++) {
			if (v[n] < first)
				first = v[n];
			if (v[n] > last)
				last = v[n];
		}

		addVarBlock(first, last, fixed);
	}
	// TODO: handle error code for addVarBlock.

	/**
	 * <p>
	 * Adds a new variable block for reordering.
	 * </p>
	 * 
	 * <p>
	 * Creates a new variable block with the variables numbered first through last,
	 * inclusive.
	 * </p>
	 * 
	 * <p>
	 * The fixed parameter sets the block to be fixed (no reordering of its child
	 * blocks is allowed) or free.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_intaddvarblock.
	 * </p>
	 */
	public abstract void addVarBlock(int first, int last, boolean fixed);
	// TODO: handle error code for addVarBlock.
	// TODO: fdd_intaddvarblock (?)

	/**
	 * <p>
	 * Add a variable block for all variables.
	 * </p>
	 * 
	 * <p>
	 * Adds a variable block for all BDD variables declared so far. Each block
	 * contains one variable only. More variable blocks can be added later with the
	 * use of addVarBlock -- in this case the tree of variable blocks will have the
	 * blocks of single variables as the leafs.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_varblockall.
	 * </p>
	 */
	public abstract void varBlockAll();

	/**
	 * <p>
	 * Clears all the variable blocks that have been defined by calls to
	 * addVarBlock.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_clrvarblocks.
	 * </p>
	 */
	public abstract void clearVarBlocks();

	/**
	 * <p>
	 * Prints an indented list of the variable blocks.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_printorder.
	 * </p>
	 */
	public abstract void printOrder();

	/**** BDD STATS ****/

	/**
	 * Get the BDD library version.
	 * 
	 * @return version string
	 */
	public abstract String getVersion();

	/**
	 * <p>
	 * Counts the number of shared nodes in a collection of BDDs. Counts all
	 * distinct nodes that are used in the BDDs -- if a node is used in more than
	 * one BDD then it only counts once.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_anodecount.
	 * </p>
	 */
	public abstract int nodeCount(Collection<BDD> r);

	/**
	 * <p>
	 * Get the number of allocated nodes. This includes both dead and active nodes.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_getallocnum.
	 * </p>
	 */
	public abstract int getNodeTableSize();

	/**
	 * <p>
	 * Get the number of active nodes in use. Note that dead nodes that have not
	 * been reclaimed yet by a garbage collection are counted as active.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_getnodenum.
	 * </p>
	 */
	public abstract int getNodeNum();

	/**
	 * <p>
	 * Get the current size of the cache, in entries.
	 * </p>
	 * 
	 * @return size of cache
	 */
	public abstract int getCacheSize();

	/**
	 * <p>
	 * Calculate the gain in size after a reordering. The value returned is
	 * (100*(A-B))/A, where A is previous number of used nodes and B is current
	 * number of used nodes.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_reorder_gain.
	 * </p>
	 */
	public abstract int reorderGain();

	/**
	 * <p>
	 * Print cache statistics.
	 * </p>
	 * 
	 * <p>
	 * Compare to bdd_printstat.
	 * </p>
	 */
	public abstract void printStat();

	/*
	 * public boolean thereIsNegativeCycle(ADD sourceStates, ADD arena, BDDVarSet
	 * primedVariables, BDDPairing primedUnprimedPairs) { return false; }
	 */

	/**
	 * Stores statistics about garbage collections.
	 * 
	 * @author jwhaley
	 * @version $Id: BDDFactory.java,v 1.2 2009/10/18 19:30:54 uid228351 Exp $
	 */
	public static class GCStats {
		public int nodes;
		public int freenodes;
		public long time;
		public long sumtime;
		public int num;

		protected GCStats() {
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			StringBuffer sb = new StringBuffer();
			sb.append("Garbage collection #");
			sb.append(num);
			sb.append(": ");
			sb.append(nodes);
			sb.append(" nodes / ");
			sb.append(freenodes);
			sb.append(" free");

			sb.append(" / ");
			sb.append((float) time / (float) 1000);
			sb.append("s / ");
			sb.append((float) sumtime / (float) 1000);
			sb.append("s total");
			return sb.toString();
		}
	}

	/**
	 * Singleton object for GC statistics.
	 */
	protected GCStats gcstats = new GCStats();

	/**
	 * <p>
	 * Return the current GC statistics for this BDD factory.
	 * </p>
	 * 
	 * @return GC statistics
	 */
	public GCStats getGCStats() {
		return gcstats;
	}

	/**
	 * Stores statistics about reordering.
	 * 
	 * @author jwhaley
	 * @version $Id: BDDFactory.java,v 1.2 2009/10/18 19:30:54 uid228351 Exp $
	 */
	public static class ReorderStats {

		public long time;
		public int usednum_before, usednum_after;

		protected ReorderStats() {
		}

		public int gain() {
			if (usednum_before == 0)
				return 0;

			return (100 * (usednum_before - usednum_after)) / usednum_before;
		}

		public String toString() {
			StringBuffer sb = new StringBuffer();
			sb.append("Went from ");
			sb.append(usednum_before);
			sb.append(" to ");
			sb.append(usednum_after);
			sb.append(" nodes, gain = ");
			sb.append(gain());
			sb.append("% (");
			sb.append((float) time / 1000f);
			sb.append(" sec)");
			return sb.toString();
		}
	}

	/**
	 * Singleton object for reorder statistics.
	 */
	protected ReorderStats reorderstats = new ReorderStats();

	/**
	 * <p>
	 * Return the current reordering statistics for this BDD factory.
	 * </p>
	 * 
	 * @return reorder statistics
	 */
	public ReorderStats getReorderStats() {
		return reorderstats;
	}

	/**
	 * Stores statistics about the operator cache.
	 * 
	 * @author jwhaley
	 * @version $Id: BDDFactory.java,v 1.2 2009/10/18 19:30:54 uid228351 Exp $
	 */
	public static class CacheStats {
		public int uniqueAccess;
		public int uniqueChain;
		public int uniqueHit;
		public int uniqueMiss;
		public int opHit;
		public int opMiss;
		public int swapCount;

		protected CacheStats() {
		}

		void copyFrom(CacheStats that) {
			this.uniqueAccess = that.uniqueAccess;
			this.uniqueChain = that.uniqueChain;
			this.uniqueHit = that.uniqueHit;
			this.uniqueMiss = that.uniqueMiss;
			this.opHit = that.opHit;
			this.opMiss = that.opMiss;
			this.swapCount = that.swapCount;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#toString()
		 */
		public String toString() {
			StringBuffer sb = new StringBuffer();
			String newLine = getProperty("line.separator", "\n");
			sb.append(newLine);
			sb.append("Cache statistics");
			sb.append(newLine);
			sb.append("----------------");
			sb.append(newLine);

			sb.append("Unique Access:  ");
			sb.append(uniqueAccess);
			sb.append(newLine);
			sb.append("Unique Chain:   ");
			sb.append(uniqueChain);
			sb.append(newLine);
			sb.append("=> Ave. chain = ");
			if (uniqueAccess > 0)
				sb.append(((float) uniqueChain) / ((float) uniqueAccess));
			else
				sb.append((float) 0);
			sb.append(newLine);
			sb.append("Unique Hit:     ");
			sb.append(uniqueHit);
			sb.append(newLine);
			sb.append("Unique Miss:    ");
			sb.append(uniqueMiss);
			sb.append(newLine);
			sb.append("=> Hit rate =   ");
			if (uniqueHit + uniqueMiss > 0)
				sb.append(((float) uniqueHit) / ((float) uniqueHit + uniqueMiss));
			else
				sb.append((float) 0);
			sb.append(newLine);
			sb.append("Operator Hits:  ");
			sb.append(opHit);
			sb.append(newLine);
			sb.append("Operator Miss:  ");
			sb.append(opMiss);
			sb.append(newLine);
			sb.append("=> Hit rate =   ");
			if (opHit + opMiss > 0)
				sb.append(((float) opHit) / ((float) opHit + opMiss));
			else
				sb.append((float) 0);
			sb.append(newLine);
			sb.append("Swap count =    ");
			sb.append(swapCount);
			sb.append(newLine);
			return sb.toString();
		}
	}

	/**
	 * Singleton object for cache statistics.
	 */
	protected CacheStats cachestats = new CacheStats();

	/**
	 * <p>
	 * Return the current cache statistics for this BDD factory.
	 * </p>
	 * 
	 * @return cache statistics
	 */
	public CacheStats getCacheStats() {
		return cachestats;
	}

	// TODO: bdd_sizeprobe_hook
	// TODO: bdd_reorder_probe

	/**** FINITE DOMAINS ****/

	protected BDDDomain[] domain;
	protected int fdvarnum;
	protected int firstbddvar;

	/**
	 * <p>
	 * Implementors must implement this factory method to create BDDDomain objects
	 * of the correct type.
	 * </p>
	 */
	protected BDDDomain createDomain(int a, BigInteger b) {
		return new BDDDomain(a, b) {
			public BDDFactory getFactory() {
				return BDDFactory.this;
			}
		};
	}

	/**
	 * <p>
	 * Creates a new finite domain block of the given size. Allocates log 2
	 * (|domainSize|) BDD variables for the domain.
	 * </p>
	 */
	public BDDDomain extDomain(long domainSize) {
		return extDomain(BigInteger.valueOf(domainSize));
	}

	public BDDDomain extDomain(BigInteger domainSize) {
		return extDomain(new BigInteger[] { domainSize })[0];
	}

	/**
	 * <p>
	 * Extends the set of finite domain blocks with domains of the given sizes. Each
	 * entry in domainSizes is the size of a new finite domain which later on can be
	 * used for finite state machine traversal and other operations on finite
	 * domains. Each domain allocates log 2 (|domainSizes[i]|) BDD variables to be
	 * used later. The ordering is interleaved for the domains defined in each call
	 * to extDomain. This means that assuming domain D0 needs 2 BDD variables x1 and
	 * x2 , and another domain D1 needs 4 BDD variables y1, y2, y3 and y4, then the
	 * order then will be x1, y1, x2, y2, y3, y4. The new domains are returned in
	 * order. The BDD variables needed to encode the domain are created for the
	 * purpose and do not interfere with the BDD variables already in use.
	 * </p>
	 * 
	 * <p>
	 * Compare to fdd_extdomain.
	 * </p>
	 */
	public BDDDomain[] extDomain(int[] dom) {
		BigInteger[] a = new BigInteger[dom.length];
		for (int i = 0; i < a.length; ++i) {
			a[i] = BigInteger.valueOf(dom[i]);
		}
		return extDomain(a);
	}

	public BDDDomain[] extDomain(long[] dom) {
		BigInteger[] a = new BigInteger[dom.length];
		for (int i = 0; i < a.length; ++i) {
			a[i] = BigInteger.valueOf(dom[i]);
		}
		return extDomain(a);
	}

	public BDDDomain[] extDomain(BigInteger[] domainSizes) {
		int offset = fdvarnum;
		int binoffset;
		int extravars = 0;
		int n, bn;
		boolean more;
		int num = domainSizes.length;

		/* Build domain table */
		if (domain == null) /* First time */ {
			domain = new BDDDomain[num];
		} else /* Allocated before */ {
			if (fdvarnum + num > domain.length) {
				int fdvaralloc = domain.length + Math.max(num, domain.length);
				BDDDomain[] d2 = new BDDDomain[fdvaralloc];
				System.arraycopy(domain, 0, d2, 0, domain.length);
				domain = d2;
			}
		}

		/* Create bdd variable tables */
		for (n = 0; n < num; n++) {
			domain[n + fdvarnum] = createDomain(n + fdvarnum, domainSizes[n]);
			extravars += domain[n + fdvarnum].varNum();
		}

		binoffset = firstbddvar;
		int bddvarnum = varNum();
		if (firstbddvar + extravars > bddvarnum) {
			setVarNum(firstbddvar + extravars);
		}

		/* Set correct variable sequence (interleaved) */
		for (bn = 0, more = true; more; bn++) {
			more = false;

			for (n = 0; n < num; n++) {
				if (bn < domain[n + fdvarnum].varNum()) {
					more = true;
					domain[n + fdvarnum].ivar[bn] = binoffset++;
				}
			}
		}

		if (isZDD()) {
			// Need to rebuild varsets for existing domains.
			for (n = 0; n < fdvarnum; n++) {
				domain[n].var.free();
				domain[n].var = makeSet(domain[n].ivar);
			}
		}
		for (n = 0; n < num; n++) {
			domain[n + fdvarnum].var = makeSet(domain[n + fdvarnum].ivar);
		}

		fdvarnum += num;
		firstbddvar += extravars;

		BDDDomain[] r = new BDDDomain[num];
		System.arraycopy(domain, offset, r, 0, num);
		return r;
	}

	/**
	 * <p>
	 * This function takes two finite domain blocks and merges them into a new one,
	 * such that the new one is encoded using both sets of BDD variables.
	 * </p>
	 * 
	 * <p>
	 * Compare to fdd_overlapdomain.
	 * </p>
	 */
	public BDDDomain overlapDomain(BDDDomain d1, BDDDomain d2) {
		BDDDomain d;
		int n;

		int fdvaralloc = domain.length;
		if (fdvarnum + 1 > fdvaralloc) {
			fdvaralloc += fdvaralloc;
			BDDDomain[] domain2 = new BDDDomain[fdvaralloc];
			System.arraycopy(domain, 0, domain2, 0, domain.length);
			domain = domain2;
		}

		d = domain[fdvarnum];
		d.realsize = d1.realsize.multiply(d2.realsize);
		d.ivar = new int[d1.varNum() + d2.varNum()];

		for (n = 0; n < d1.varNum(); n++)
			d.ivar[n] = d1.ivar[n];
		for (n = 0; n < d2.varNum(); n++)
			d.ivar[d1.varNum() + n] = d2.ivar[n];

		d.var = makeSet(d.ivar);
		// bdd_addref(d.var);

		fdvarnum++;
		return d;
	}

	/**
	 * <p>
	 * Returns a BDD defining all the variable sets used to define the variable
	 * blocks in the given array.
	 * </p>
	 * 
	 * <p>
	 * Compare to fdd_makeset.
	 * </p>
	 */
	public BDDVarSet makeSet(BDDDomain[] v) {
		BDDVarSet res = emptySet();
		int n;

		for (n = 0; n < v.length; n++) {
			res.unionWith(v[n].set());
		}

		return res;
	}

	/**
	 * <p>
	 * Clear all allocated finite domain blocks that were defined by extDomain() or
	 * overlapDomain().
	 * </p>
	 * 
	 * <p>
	 * Compare to fdd_clearall.
	 * </p>
	 */
	public void clearAllDomains() {
		domain = null;
		fdvarnum = 0;
		firstbddvar = 0;
	}

	/**
	 * <p>
	 * Returns the number of finite domain blocks defined by calls to extDomain().
	 * </p>
	 * 
	 * <p>
	 * Compare to fdd_domainnum.
	 * </p>
	 */
	public int numberOfDomains() {
		return fdvarnum;
	}

	/**
	 * <p>
	 * Returns the ith finite domain block, as defined by calls to extDomain().
	 * </p>
	 */
	public BDDDomain getDomain(int i) {
		if (i < 0 || i >= fdvarnum)
			throw new IndexOutOfBoundsException();
		return domain[i];
	}

	// TODO: fdd_file_hook, fdd_strm_hook

	/**
	 * <p>
	 * Creates a variable ordering from a string. The resulting order can be passed
	 * into <tt>setVarOrder()</tt>. Example: in the order "A_BxC_DxExF", the bits
	 * for A are first, followed by the bits for B and C interleaved, followed by
	 * the bits for D, E, and F interleaved.
	 * </p>
	 * 
	 * <p>
	 * Obviously, domain names cannot contain the 'x' or '_' characters.
	 * </p>
	 * 
	 * @param reverseLocal
	 *            whether to reverse the bits of each domain
	 * @param ordering
	 *            string representation of ordering
	 * @return int[] of ordering
	 * @see net.sf.javabdd.BDDFactory#setVarOrder(int[])
	 */
	public int[] makeVarOrdering(boolean reverseLocal, String ordering) {

		int varnum = varNum();

		int nDomains = numberOfDomains();
		int[][] localOrders = new int[nDomains][];
		for (int i = 0; i < localOrders.length; ++i) {
			localOrders[i] = new int[getDomain(i).varNum()];
		}

		for (int i = 0; i < nDomains; ++i) {
			BDDDomain d = getDomain(i);
			int nVars = d.varNum();
			for (int j = 0; j < nVars; ++j) {
				if (reverseLocal) {
					localOrders[i][j] = nVars - j - 1;
				} else {
					localOrders[i][j] = j;
				}
			}
		}

		BDDDomain[] doms = new BDDDomain[nDomains];

		int[] varorder = new int[varnum];

		// System.out.println("Ordering: "+ordering);
		StringTokenizer st = new StringTokenizer(ordering, "x_", true);
		int numberOfDomains = 0, bitIndex = 0;
		boolean[] done = new boolean[nDomains];
		for (int i = 0;; ++i) {
			String s = st.nextToken();
			BDDDomain d;
			for (int j = 0;; ++j) {
				if (j == numberOfDomains())
					throw new BDDException("bad domain: " + s);
				d = getDomain(j);
				if (s.equals(d.getName()))
					break;
			}
			if (done[d.getIndex()])
				throw new BDDException("duplicate domain: " + s);
			done[d.getIndex()] = true;
			doms[i] = d;
			if (st.hasMoreTokens()) {
				s = st.nextToken();
				if (s.equals("x")) {
					++numberOfDomains;
					continue;
				}
			}
			bitIndex = fillInVarIndices(doms, i - numberOfDomains, numberOfDomains + 1, localOrders, bitIndex,
					varorder);
			if (!st.hasMoreTokens()) {
				break;
			}
			if (s.equals("_"))
				numberOfDomains = 0;
			else
				throw new BDDException("bad token: " + s);
		}

		for (int i = 0; i < doms.length; ++i) {
			if (!done[i]) {
				throw new BDDException("missing domain #" + i + ": " + getDomain(i));
			}
		}

		while (bitIndex < varorder.length) {
			varorder[bitIndex] = bitIndex;
			++bitIndex;
		}

		int[] test = new int[varorder.length];
		System.arraycopy(varorder, 0, test, 0, varorder.length);
		Arrays.sort(test);
		for (int i = 0; i < test.length; ++i) {
			if (test[i] != i)
				throw new BDDException(test[i] + " != " + i);
		}

		return varorder;
	}

	/**
	 * Helper function for makeVarOrder().
	 */
	static int fillInVarIndices(BDDDomain[] doms, int domainIndex, int numDomains, int[][] localOrders, int bitIndex,
			int[] varorder) {
		// calculate size of largest domain to interleave
		int maxBits = 0;
		for (int i = 0; i < numDomains; ++i) {
			BDDDomain d = doms[domainIndex + i];
			maxBits = Math.max(maxBits, d.varNum());
		}
		// interleave the domains
		for (int bitNumber = 0; bitNumber < maxBits; ++bitNumber) {
			for (int i = 0; i < numDomains; ++i) {
				BDDDomain d = doms[domainIndex + i];
				if (bitNumber < d.varNum()) {
					int di = d.getIndex();
					int local = localOrders[di][bitNumber];
					if (local >= d.vars().length) {
						System.out.println("bug!");
					}
					if (bitIndex >= varorder.length) {
						System.out.println("bug2!");
					}
					varorder[bitIndex++] = d.vars()[local];
				}
			}
		}
		return bitIndex;
	}

	/**** BIT VECTORS ****/

	/**
	 * <p>
	 * Implementors must implement this factory method to create BDDBitVector
	 * objects of the correct type.
	 * </p>
	 */
	protected BDDBitVector createBitVector(int a) {
		return new BDDBitVector(a) {
			public BDDFactory getFactory() {
				return BDDFactory.this;
			}
		};
	}

	/**
	 * <p>
	 * Implementors must implement this factory method to create BDDBitVector
	 * objects of the correct type.
	 * </p>
	 */
	protected BDDBitVector createBitVector(BigInteger i) {
		return new BDDBitVector(i) {
			public BDDFactory getFactory() {
				return BDDFactory.this;
			}
		};
	}

	/**
	 * <p>
	 * Build a bit vector that is constant true or constant false.
	 * </p>
	 * 
	 * <p>
	 * Compare to bvec_true, bvec_false.
	 * </p>
	 */
	public BDDBitVector buildVector(int bitnum, boolean b) {
		BDDBitVector v = createBitVector(bitnum);
		v.initialize(b);
		return v;
	}

	/**
	 * <p>
	 * Build a bit vector that corresponds to a constant value.
	 * </p>
	 * 
	 * <p>
	 * Compare to bvec_con.
	 * </p>
	 */
	public BDDBitVector constantVector(int bitnum, long val) {
		BDDBitVector v = createBitVector(bitnum);
		v.initialize(val);
		return v;
	}

	/**
	 * Build a bit vector that corresponds to a constant value. The size of the
	 * vector is determined such that it suffices for holding the value.
	 * 
	 * @param val
	 * @return
	 */
	public BDDBitVector constantVector(BigInteger val) {
		BDDBitVector v = createBitVector(val);
		return v;
	}

	public BDDBitVector constantVector(int bitnum, BigInteger val) {
		BDDBitVector v = createBitVector(bitnum);
		v.initialize(val);
		return v;
	}

	/**
	 * <p>
	 * Build a bit vector using variables offset, offset+step, offset+2*step, ... ,
	 * offset+(bitnum-1)*step.
	 * </p>
	 * 
	 * <p>
	 * Compare to bvec_var.
	 * </p>
	 */
	public BDDBitVector buildVector(int bitnum, int offset, int step) {
		BDDBitVector v = createBitVector(bitnum);
		v.initialize(offset, step);
		return v;
	}

	/**
	 * <p>
	 * Build a bit vector using variables from the given BDD domain.
	 * </p>
	 * 
	 * <p>
	 * Compare to bvec_varfdd.
	 * </p>
	 */
	public BDDBitVector buildVector(BDDDomain d) {
		BDDBitVector v = createBitVector(d.varNum());
		v.initialize(d);
		return v;
	}

	/**
	 * <p>
	 * Build a bit vector using the given variables.
	 * </p>
	 * 
	 * <p>
	 * compare to bvec_varvec.
	 * </p>
	 */
	public BDDBitVector buildVector(int[] var) {
		BDDBitVector v = createBitVector(var.length);
		v.initialize(var);
		return v;
	}

	/**** CALLBACKS ****/

	protected List<Object[]> gc_callbacks, reorder_callbacks, resize_callbacks;

	/**
	 * <p>
	 * Register a callback that is called when garbage collection is about to occur.
	 * </p>
	 * 
	 * @param o
	 *            base object
	 * @param m
	 *            method
	 */
	public void registerGCCallback(Object o, Method m) {
		if (gc_callbacks == null)
			gc_callbacks = new LinkedList<Object[]>();
		registerCallback(gc_callbacks, o, m);
	}

	/**
	 * <p>
	 * Unregister a garbage collection callback that was previously registered.
	 * </p>
	 * 
	 * @param o
	 *            base object
	 * @param m
	 *            method
	 */
	public void unregisterGCCallback(Object o, Method m) {
		if (gc_callbacks == null)
			throw new BDDException();
		if (!unregisterCallback(gc_callbacks, o, m))
			throw new BDDException();
	}

	/**
	 * <p>
	 * Register a callback that is called when reordering is about to occur.
	 * </p>
	 * 
	 * @param o
	 *            base object
	 * @param m
	 *            method
	 */
	public void registerReorderCallback(Object o, Method m) {
		if (reorder_callbacks == null)
			reorder_callbacks = new LinkedList<Object[]>();
		registerCallback(reorder_callbacks, o, m);
	}

	/**
	 * <p>
	 * Unregister a reorder callback that was previously registered.
	 * </p>
	 * 
	 * @param o
	 *            base object
	 * @param m
	 *            method
	 */
	public void unregisterReorderCallback(Object o, Method m) {
		if (reorder_callbacks == null)
			throw new BDDException();
		if (!unregisterCallback(reorder_callbacks, o, m))
			throw new BDDException();
	}

	/**
	 * <p>
	 * Register a callback that is called when node table resizing is about to
	 * occur.
	 * </p>
	 * 
	 * @param o
	 *            base object
	 * @param m
	 *            method
	 */
	public void registerResizeCallback(Object o, Method m) {
		if (resize_callbacks == null)
			resize_callbacks = new LinkedList<Object[]>();
		registerCallback(resize_callbacks, o, m);
	}

	/**
	 * <p>
	 * Unregister a reorder callback that was previously registered.
	 * </p>
	 * 
	 * @param o
	 *            base object
	 * @param m
	 *            method
	 */
	public void unregisterResizeCallback(Object o, Method m) {
		if (resize_callbacks == null)
			throw new BDDException();
		if (!unregisterCallback(resize_callbacks, o, m))
			throw new BDDException();
	}

	protected void gbc_handler(boolean pre, GCStats s) {
		if (gc_callbacks == null) {
			bdd_default_gbchandler(pre, s);
		} else {
			doCallbacks(gc_callbacks, new Integer(pre ? 1 : 0), s);
		}
	}

	protected static void bdd_default_gbchandler(boolean pre, GCStats s) {
		if (pre) {
			if (s.freenodes != 0)
				System.err.println(
						"Starting GC cycle  #" + (s.num + 1) + ": " + s.nodes + " nodes / " + s.freenodes + " free");
		} else {
			System.err.println(s.toString());
		}
	}

	void reorder_handler(boolean b, ReorderStats s) {
		if (b) {
			s.usednum_before = getNodeNum();
			s.time = System.currentTimeMillis();
		} else {
			s.time = System.currentTimeMillis() - s.time;
			s.usednum_after = getNodeNum();
		}
		if (reorder_callbacks == null) {
			bdd_default_reohandler(b, s);
		} else {
			doCallbacks(reorder_callbacks, new Boolean(b), s);
		}
	}

	protected void bdd_default_reohandler(boolean prestate, ReorderStats s) {
		int verbose = 1;
		if (verbose > 0) {
			if (prestate) {
				System.out.println("Start reordering");
			} else {
				System.out.println("End reordering. " + s);
			}
		}
	}

	protected void resize_handler(int oldsize, int newsize) {
		if (resize_callbacks == null) {
			bdd_default_reshandler(oldsize, newsize);
		} else {
			doCallbacks(resize_callbacks, new Integer(oldsize), new Integer(newsize));
		}
	}

	protected static void bdd_default_reshandler(int oldsize, int newsize) {
		int verbose = 1;
		if (verbose > 0) {
			System.out.println("Resizing node table from " + oldsize + " to " + newsize);
		}
	}

	protected void registerCallback(List<Object[]> callbacks, Object o, Method m) {
		if (!Modifier.isPublic(m.getModifiers()) && !m.isAccessible()) {
			throw new BDDException("Callback method not accessible");
		}
		if (!Modifier.isStatic(m.getModifiers())) {
			if (o == null) {
				throw new BDDException("Base object for callback method is null");
			}
			if (!m.getDeclaringClass().isAssignableFrom(o.getClass())) {
				throw new BDDException("Base object for callback method is the wrong type");
			}
		}
		callbacks.add(new Object[] { o, m });
	}

	protected boolean unregisterCallback(List<Object[]> callbacks, Object o, Method m) {
		if (callbacks != null) {
			for (Iterator<Object[]> i = callbacks.iterator(); i.hasNext();) {
				Object[] cb = i.next();
				if (o == cb[0] && m.equals(cb[1])) {
					i.remove();
					return true;
				}
			}
		}
		return false;
	}

	protected void doCallbacks(List<Object[]> callbacks, Object arg1, Object arg2) {
		if (callbacks != null) {
			for (Iterator<Object[]> i = callbacks.iterator(); i.hasNext();) {
				Object[] cb = i.next();
				Object o = cb[0];
				Method m = (Method) cb[1];
				try {
					switch (m.getParameterTypes().length) {
					case 0:
						m.invoke(o, new Object[] {});
						break;
					case 1:
						m.invoke(o, new Object[] { arg1 });
						break;
					case 2:
						m.invoke(o, new Object[] { arg1, arg2 });
						break;
					default:
						throw new BDDException("Wrong number of arguments for " + m);
					}
				} catch (IllegalArgumentException e) {
					e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				} catch (InvocationTargetException e) {
					if (e.getTargetException() instanceof RuntimeException)
						throw (RuntimeException) e.getTargetException();
					if (e.getTargetException() instanceof Error)
						throw (Error) e.getTargetException();
					e.printStackTrace();
				}
			}
		}
	}

	public int[] getAttrSizes() {
		System.out.println("getAttrSizes() not implemented");
		return null;
	}

	public BDD[] getXMem() {
		System.out.println("getXMem() not implemented");
		return null;
	}

	public BDD[] getYMem() {
		System.out.println("getYMem() not implemented");
		return null;
	}

	public BDD[] getZMem() {
		System.out.println("getZMem() not implemented");
		return null;
	}

	public BDD[] getZMemFirstItr() {
		System.out.println("getZMemFirstItr() not implemented");
		return null;
	}
	
	public BDD getFulfillBDD(int[] exjindices, int[] findices) {
		System.out.println("getFulfillBDD not implemented");
		return null;
	}
	
	public BDD getTowardsBDD(int[] exjindices, int[] tindices) {
		System.out.println("getTowardsBDD not implemented");
		return null;
	}
	
	public BDD getEnvViolationBDD(int[] iindices, int[] kindices) {
		System.out.println("getEnvViolationBDD not implemented");
		return null;
	}
	
	public BDD getGr1StarWinningStates() {
		System.out.println("getGr1StarWinningStates() not implemented");
		return null;
	}

	public boolean gr1StarGame(BDD[] sysJ, BDD[] envJ,
			BDD[] sfaIni, BDD[] sfaTrans, BDD[] sfaTransToAcc, BDDVarSet[] sfaUnprimeStateVars, BDDVarSet[] sfaPrimeStateVars,
			BDD sysIni, BDD envIni, BDD sysTrans,
			BDD envTrans, BDDVarSet sysUnprimeVars, BDDVarSet envUnprimeVars, BDDVarSet sysPrimeVars,
			BDDVarSet envPrimeVars, BDDPairing pairs, BDD[] sysTransList, BDD[] envTransList,
			BDDVarSet[] sysQuantSets, BDDVarSet[] envQuantSets, boolean efp,
			boolean eun, boolean fpr, boolean sca, boolean mem) {
		System.out.println("gr1 star game not implemented");
		return false;
	}
	
	public boolean gr1Game(BDD[] sysJ, BDD[] envJ, BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans,
			BDDVarSet sysUnprimeVars, BDDVarSet envUnprimeVars, BDDVarSet sysPrimeVars, BDDVarSet envPrimeVars,
			BDDPairing pairs, BDD[] sysTransList, BDD[] envTransList, BDDVarSet[] sysQuantSets,
			BDDVarSet[] envQuantSets, boolean efp, boolean eun, boolean fpr, boolean sca) {
		System.out.println("gr1 game not implemented");
		return false;
	}

	public boolean gr1GameWithIncData(BDD[] sysJ, BDD[] envJ, BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans,
			BDDVarSet sysUnprimeVars, BDDVarSet envUnprimeVars, BDDVarSet sysPrimeVars, BDDVarSet envPrimeVars,
			BDDPairing pairs, boolean efp, boolean eun, boolean fpr, boolean sca, int incBitmap, BDD incStartZ,
			BDD[] incZMem, BDD[] incZMemFirstItr, BDD[] incXMem, int jIdx, int incSizeD1, int incSizeD2,
			int[] incSizeD3) {
		System.out.println("gr1GameWithIncData not implemented");
		return false;
	}

	public int getRabinZSize() {
		System.out.println("getRabinZSize() not implemented");
		return 0;
	}

	public int[] getRabinXSizes() {
		System.out.println("getRabinXSizes() not implemented");
		return null;
	}

	public BDD[] getRabinXMem() {
		System.out.println("getRabinXMem() not implemented");
		return null;
	}

	public BDD[] getRabinZMem() {
		System.out.println("getRabinZMem() not implemented");
		return null;
	}

	public boolean rabinGame(BDD[] sysJ, BDD[] envJ, BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans,
			BDDVarSet sysUnprimeVars, BDDVarSet envUnprimeVars, BDDVarSet sysPrimeVars, BDDVarSet envPrimeVars,
			BDDPairing pairs, BDD[] sysTransList, BDD[] envTransList, BDDVarSet[] sysQuantSets,
			BDDVarSet[] envQuantSets, boolean efp, boolean eun, boolean fpr, boolean sca) {
		System.out.println("rabin game not implemented");
		return false;
	}

	public boolean rabinGameWithIncData(BDD[] sysJ, BDD[] envJ, BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans,
			BDDVarSet sysUnprimeVars, BDDVarSet envUnprimeVars, BDDVarSet sysPrimeVars, BDDVarSet envPrimeVars,
			BDDPairing pairs, boolean efp, boolean eun, boolean fpr, boolean sca, int incBitmap, BDD incStartZ,
			BDD[] incZMem, BDD[] incXMem, int jIdx, int incSizeD1, int incSizeD2, int[] incSizeD3) {
		System.out.println("rabinGameWithIncData not implemented");
		return false;
	}
	
	public BDD getJusticesBDD(BDD[] sysJ, BDD[] envJ, int[] jindices, int[] iindices, int utilindex) {
		System.out.println("getJusticesBDD not implemented");
		return null;
	}
	
	public BDD getTransBDD(BDD sysIni, BDD envIni, BDD sysTrans, BDD envTrans, int[] jindices, int[] iindices, int utilindex) {
		System.out.println("getTransBDD not implemented");
		return null;
	}
	
	public BDD getFixpointsBDD(int[] jindices, int[] iindices, int[] kindices) {
		System.out.println("getFixpointsBDD not implemented");
		return null;
	}
	
	public BDD getFixpointsStarBDD(int[] jindices, int[] iindices, int[] kindices) {
		System.out.println("getFixpointsStarBDD not implemented");
		return null;
	}
	
	public BDD getJusticesStarBDD(BDD[] sysJ, BDD[] envJ, int[] jindices, int[] iindices, int utilindex) {
		System.out.println("getJusticesStarBDD not implemented");
		return null;
	}
	
	
	
	
	// GR(1) controller execution funcions
	
	public void loadFixpointsJits(BDD fixpoints, int[] jindices, int[] iindices, int[] kindices, int[] ranks, BDDPairing pairs, BDDVarSet primeVars) {
		System.out.println("loadFixpointsJits not implemented");
	}
	
	public void loadTransJits(BDD trans, int[] jindices, int[] iindices, int utilindex) {
		System.out.println("loadTransJits not implemented");
	}
	
	public void loadJusticesJits(BDD justices, int[] jindices, int[] iindices, int utilindex, int n, int m) {
		System.out.println("loadJusticesJits not implemented");
	}
	
	public BDD nextStatesJits(BDD current, BDD inputs, BDDPairing pairs, BDDVarSet unprimeVars) {
		System.out.println("nextStatesJits not implemented");
		return null;
	}
	
	public int initControllerJits(BDD inputs, BDDPairing pairs) {
		System.out.println("initControllerJits not implemented");
		return 0;
	}
	
	public void freeControllerJits() {
		System.out.println("freeControllerJits not implemented");
	}
	
	public BDD getTransitionsJits() {
		System.out.println("getTransitionsJits not implemented");
		return null;
	}
	
	public BDD getInitialJits() {
		System.out.println("getInitialJits not implemented");
		return null;
	}
	
	
	
	

	public BDD[] getGr1StarXMem() {
		System.out.println("getGr1StarXMem() game not implemented");
		return null;
	}

	public BDD[] getGr1StarYMem() {
		System.out.println("getGr1StarYMem() game not implemented");
		return null;
	}

	public int[] getGr1StarTowardsExistIterNum() {
		System.out.println("getGr1StarTowardsExistIterNum() game not implemented");
		return null;
	}

	public int[] getGr1StarFulfillExistIterNum() {
		System.out.println("getGr1StarFulfillExistIterNum() game not implemented");
		return null;
	}

	public int getGr1StarEnvJViolationIterNum() {
		System.out.println("getGr1StarEnvJViolationIterNum() game not implemented");
		return 0;
	}

	public BDD[] getGr1StarFulfillExistMem() {
		System.out.println("getGr1StarFulfillExistMem() game not implemented");
		return null;
	}

	public BDD[] getGr1StarTowardsExistMem() {
		System.out.println("getGr1StarTowardsExistMem() game not implemented");
		return null;
	}

	public BDD[] getGR1StarEnvJViolationMem() {
		System.out.println("getGR1StarEnvJViolationMem() game not implemented");
		return null;
	}

	public int[] getGr1StarJusticeIterNum() {
		System.out.println("getGr1StarJusticeIterNum() game not implemented");
		return null;
	}



}
