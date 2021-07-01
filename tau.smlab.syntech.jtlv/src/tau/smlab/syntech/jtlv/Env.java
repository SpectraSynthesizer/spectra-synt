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

package tau.smlab.syntech.jtlv;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Method;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Vector;

import net.sf.javabdd.ADD;
import net.sf.javabdd.BDD;
import net.sf.javabdd.BDD.BDDToString;
import net.sf.javabdd.BDDBitVector;
import net.sf.javabdd.BDDDomain;
import net.sf.javabdd.BDDException;
import net.sf.javabdd.BDDFactory;
import net.sf.javabdd.BDDPairing;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

/**
 * <p>
 * The main JTLV environment Facade, which supplies the API to basic
 * functionalities through delegators to the private internal managers. The
 * environment is somewhat corresponding to the Java "System" environment.
 * </p>
 * 
 * <p>
 * The two main field which Env is delegating with are: The JTLVBDDManager,
 * which is responsible of communicating with the actual BDD implementation
 * (through JavaBDD package). The JTLVBDDManagerPairing, which is responsible of
 * all BDD naming and coupling operations (i.e. prime and unprimed versions of
 * all fields).
 * </p>
 * 
 * @version {@value edu.wis.jtlv.env.Env#version}
 * @author yaniv sa'ar.
 * 
 */
public final class Env {

	// a static environment, should not be instantiated.
	private Env() {
	}

	/**
	 * <p>
	 * JTLV version count.
	 * </p>
	 */
	public static final String version = "1.4.2.SYNTECH";

	/**
	 * <p>
	 * Responsible of communicating with the actual BDD implementation.
	 * </p>
	 * 
	 */
	private static JTLVBDDManager bdd_manager;

	/**
	 * <p>
	 * Responsible of all BDD naming and coupling operations (i.e. prime and unprime
	 * versions of all fields).
	 * </p>
	 * 
	 */
	private static JTLVBDDManagerPairing bdd_namer;

	/**
	 * <p>
	 * An object responsible for associating BDD variables to their names.
	 * </p>
	 */
	public static JTLVBDDToString stringer;

	static {
		reset_pool_and_managers();
	}

	private static void reset_pool_and_managers() {
		reset_pool_and_managers(null);
	}

	private static void reset_pool_and_managers(BDDFactory f) {

		// /////////////////////////////////
		// constructing the BDD manager
		int node_num, cache_size;
		String str_start_node_num = System.getProperty("bdd_table_size");
		String str_start_cache_size = System.getProperty("bdd_cache_size");
		try {
			node_num = Integer.parseInt(str_start_node_num);
		} catch (NumberFormatException nfe) {
			node_num = 50000;
		}
		try {
			cache_size = Integer.parseInt(str_start_cache_size);
		} catch (NumberFormatException nfe) {
			cache_size = 10000;
		}

		if (bdd_manager == null) { // no previous initiation.
			if (f == null)
				bdd_manager = new JTLVBDDManager(node_num, cache_size);
			else
				bdd_manager = new JTLVBDDManager(f);
		} else {
			if (f == null)
				bdd_manager.reset_factory(node_num, cache_size);
			else
				bdd_manager.reset_factory(f);
		}

		// /////////////////////////////////
		// constructing the pairs manager
		bdd_namer = new JTLVBDDManagerPairing();

		// /////////////////////////////////
		// re-initiating other vars
		stringer = new JTLVBDDToString();

	}

	// ////////////////////////////////////////////////////////////////////////
	// /////////////// general Functionalities ////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////

	/**
	 * <p>
	 * Reset the environment. In particular reset the bdd factory and all Modules
	 * and fields declared in the environment.
	 * </p>
	 * 
	 */
	public static void resetEnv() {
		Env.reset_pool_and_managers();
	}

	/**
	 * <p>
	 * Reset the environment. In particular reset the bdd factory and all Modules
	 * and fields declared in the environment.
	 * </p>
	 * 
	 */
	public static void resetEnv(BDDFactory f) {
		Env.reset_pool_and_managers(f);
	}

	// ////////////////////////////////////////////////////////////////////////
	// /////////////// BDD Functionalities ////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////

	/**
	 * <p>
	 * The factory package which is currently used by the environment to allocate
	 * new BDD fields.
	 * </p>
	 * 
	 * @return The BDD factory name.
	 * 
	 */
	public static String getFactoryName() {
		return bdd_manager.factoryName();
	}

	/**
	 * <p>
	 * Register a procedure to be done at every garbage collection.
	 * </p>
	 * 
	 * @param m_gc The method to perform.
	 * 
	 * @see edu.wis.jtlv.env.Env#beQuiet()
	 * @see edu.wis.jtlv.env.Env#doOnResize(Method)
	 * @see edu.wis.jtlv.env.Env#doOnReorder(Method)
	 */
	public static void doOnGC(Method m_gc) {
		bdd_manager.doOnGC(m_gc);
	}

	/**
	 * <p>
	 * Register a procedure to be done at every resize to the BDD table.
	 * </p>
	 * 
	 * @param m_resize The method to perform.
	 * 
	 * @see edu.wis.jtlv.env.Env#beQuiet()
	 * @see edu.wis.jtlv.env.Env#doOnGC(Method)
	 * @see edu.wis.jtlv.env.Env#doOnReorder(Method)
	 */
	public static void doOnResize(Method m_resize) {
		bdd_manager.doOnResize(m_resize);
	}

	/**
	 * <p>
	 * Register a procedure to be done at every reorder to the BDD table.
	 * </p>
	 * 
	 * @param m_reorder The method to perform.
	 * 
	 * @see edu.wis.jtlv.env.Env#beQuiet()
	 * @see edu.wis.jtlv.env.Env#doOnGC(Method)
	 * @see edu.wis.jtlv.env.Env#doOnResize(Method)
	 */
	public static void doOnReorder(Method m_reorder) {
		bdd_manager.doOnReorder(m_reorder);
	}

	/**
	 * <p>
	 * Turning off the entire factory verbose (garbage collection, resize, reorder).
	 * </p>
	 * 
	 * @see edu.wis.jtlv.env.Env#doOnGC(Method)
	 * @see edu.wis.jtlv.env.Env#doOnResize(Method)
	 * @see edu.wis.jtlv.env.Env#doOnReorder(Method)
	 */
	public static void beQuiet() {
		try {
			Method m = Env.class.getMethod("NOP", (Class[]) null);
			doOnGC(m);
			doOnResize(m);
			doOnReorder(m);
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		}
	}

	/**
	 * <p>
	 * An empty procedure for shutting down the factory verbose.
	 * </p>
	 * 
	 * @see edu.wis.jtlv.env.Env#beQuiet()
	 */
	public static void NOP() {
	}

	/**
	 * Creates a new constant BDDBitVector of length bitnum which has the specified
	 * value.
	 * 
	 * @param bitnum The length of the created BDD bit vector in bits.
	 * @param value  The value the created BDD bit vector holds.
	 * @return
	 */
	public static BDDBitVector createBddVector(int bitnum, long value) {
		return bdd_manager.buildVector(bitnum, value);
	}

	/**
	 * Creates a new constant BDDBitVector of length bitnum which has the specified
	 * value.
	 * 
	 * @param bitnum The length of the created BDD bit vector in bits.
	 * @param value  The value the created BDD bit vector holds.
	 * @return
	 */
	public static BDDBitVector createBddVector(int bitnum, BigInteger value) {
		return bdd_manager.buildVector(bitnum, value);
	}

	/**
	 * Creates a new constant BDDBitVector which has the specified value. The length
	 * of the vector is determined such that it is sufficient for the encoding of
	 * the specified value.
	 * 
	 * @param value The value the created BDD bit vector holds.
	 * @return
	 */
	public static BDDBitVector createBddVector(BigInteger value) {
		return bdd_manager.buildVector(value);
	}

	/**
	 * Creates a new BDDBitVector such that the i'th bit is the variable of index
	 * {@code var[i]}. The length of the vector is {@code var.length}.
	 * 
	 * @param var The indices of the variables the created vector holds.
	 * @return
	 */
	public static BDDBitVector createBddVector(int[] var) {
		return bdd_manager.buildVector(var);
	}

	/**
	 * Creates a new BDDBitVector such that the i'th bit is the i'th variable that
	 * encodes the specified domain.
	 * 
	 * @param domain The domain whose variables become the bits of the created
	 *               vector.
	 * @return
	 */
	public static BDDBitVector createBddVector(BDDDomain domain) {
		return bdd_manager.buildVector(domain);
	}

	/**
	 * <p>
	 * The constant FALSE BDD.
	 * </p>
	 * 
	 * @return The constant FALSE BDD.
	 * 
	 * @see edu.wis.jtlv.env.Env#TRUE()
	 */
	public static BDD FALSE() {
		return bdd_manager.FALSE();
	}

	/**
	 * <p>
	 * The constant TRUE BDD.
	 * </p>
	 * 
	 * @return The constant TRUE BDD.
	 * 
	 * @see edu.wis.jtlv.env.Env#FALSE()
	 */
	public static BDD TRUE() {
		return bdd_manager.TRUE();
	}

	/**
	 * <p>
	 * The plus infinity constant ADD. Supported only if the actual DD is an ADD;
	 * Otherwise, the behavior of this method is undefined.
	 * </p>
	 * 
	 * @return The constant plus infinity ADD.
	 * 
	 * @see edu.wis.jtlv.env.Env#MINUS_INF()
	 * @see edu.wis.jtlv.env.Env#CONST()
	 */
	public static ADD PLUS_INF() {
		return bdd_manager.PLUS_INF();
	}

	/**
	 * <p>
	 * The minus infinity constant ADD. Supported only if the actual DD is an ADD;
	 * Otherwise, the behavior of this method is undefined.
	 * </p>
	 * 
	 * @return The constant minus infinity ADD.
	 * 
	 * @see edu.wis.jtlv.env.Env#PLUS_INF()
	 * @see edu.wis.jtlv.env.Env#CONST()
	 */
	public static ADD MINUS_INF() {
		return bdd_manager.MINUS_INF();
	}

	/**
	 * <p>
	 * The constant ADD of the given value. Supported only if the actual DD is an
	 * ADD; Otherwise, the behavior of this method is undefined.
	 * </p>
	 * 
	 * @param value the value of the constant ADD
	 * @return The constant ADD of the given value.
	 * 
	 * @see edu.wis.jtlv.env.Env#PLUS_INF()
	 * @see edu.wis.jtlv.env.Env#MINUS_INF()
	 */
	public static ADD CONST(double value) {
		return bdd_manager.CONST(value);
	}

	/**
	 * <p>
	 * Save a BDD to the file system.
	 * </p>
	 * 
	 * @param filename The file to save the BDD to.
	 * @param to_save  The BDD to save.
	 * @throws IOException If the was a problem with creating the file.
	 * 
	 * @see edu.wis.jtlv.env.Env#loadBDD(String)
	 */
	public static void saveBDD(String filename, BDD to_save, boolean reorder) throws IOException {
		bdd_manager.save(filename, to_save, reorder);
	}

	public static void saveBDD(BufferedWriter writer, BDD to_save, boolean reorder) throws IOException {
		bdd_manager.save(writer, to_save, reorder);
	}

	/**
	 * <p>
	 * Load a BDD from the file system.
	 * </p>
	 * 
	 * <p>
	 * <b> Make sure that the system is configured with the same BDD package which
	 * saved the BDD to the file!! </b>
	 * </p>
	 * 
	 * @param filename The file to load the BDD from.
	 * @return The BDD which was loaded.
	 * @throws IOException If the was a problem with reading the file.
	 * 
	 * @see edu.wis.jtlv.env.Env#saveBDD(String, BDD)
	 */
	public static BDD loadBDD(String filename) throws IOException {
		return bdd_manager.load(filename);
	}

	public static BDD loadBDD(BufferedReader reader) throws IOException {
		return bdd_manager.load(reader);
	}

	/**
	 * <p>
	 * Search and retrieve the BDD field by the given path and field name.
	 * </p>
	 * 
	 * @param preface The path to the field.
	 * @param name    The field name.
	 * @return The BDD field, null if not found.
	 * 
	 * @see edu.wis.jtlv.env.Env#newVar(String, String, int)
	 * @see edu.wis.jtlv.env.Env#newVar(String, String)
	 * @see edu.wis.jtlv.env.Env#allocBDD(int, int)
	 */
	public static ModuleBDDField getVar(String name) {
		return bdd_namer.get_var(name);
	}

	/**
	 * <p>
	 * Create and allocate a new BDD <b>field</b> with the given domain size, i.e.
	 * the number of values that this BDD contains. (the actual number values
	 * rounded to the power of 2)
	 * </p>
	 * 
	 * <p>
	 * With this field instantiation The BDD is created with an associated
	 * facilities in the environment.<br>
	 * For creating a BDD without the environment field facilities, please refer to
	 * {@link edu.wis.jtlv.env.Env#allocBDD(int, int)}.
	 * </p>
	 * 
	 * @param preface     The path to the field.
	 * @param name        The field name.
	 * @param values_size The size of the domain.
	 * @return The newly created BDD field.
	 * @throws ModuleVariableException If the given name for the new field to create
	 *                                 is illegal (for instance duplication).
	 * 
	 * @see edu.wis.jtlv.env.Env#getVar(String, String)
	 * @see edu.wis.jtlv.env.Env#newVar(String, String)
	 * @see edu.wis.jtlv.env.Env#allocBDD(int, int)
	 */
	public static ModuleBDDField newVar(String name, int values_size) throws ModuleVariableException {
		ModuleBDDField res = bdd_namer.new_var(name, values_size);
		return res;
	}

	/**
	 * <p>
	 * Create and allocate a new BDD <b>field</b> with 2 values domain.
	 * </p>
	 * 
	 * <p>
	 * With this field instantiation The BDD is created with an associated
	 * facilities in the environment.<br>
	 * For creating a BDD without the environment field facilities, please refer to
	 * {@link edu.wis.jtlv.env.Env#allocBDD(int,int)}.
	 * </p>
	 * 
	 * @param preface The path to the field.
	 * @param name    The field name.
	 * @return The newly created BDD couple variable.
	 * @throws ModuleVariableException If the given name for the new field to create
	 *                                 is illegal (for instance duplication).
	 * 
	 * @see edu.wis.jtlv.env.Env#getVar(String, String)
	 * @see edu.wis.jtlv.env.Env#newVar(String, String, int)
	 * @see edu.wis.jtlv.env.Env#allocBDD(int, int)
	 */
	public static ModuleBDDField newVar(String name) throws ModuleVariableException {
		return newVar(name, 2);
	}

	/**
	 * <p>
	 * Allocate a new BDD with the given domain size, i.e. the number of values that
	 * this BDD contains. (the actual number values rounded to the power of 2)
	 * </p>
	 * 
	 * <p>
	 * This procedure does not create the entire BDD field facilities. For creating
	 * a BDD field, please refer to
	 * {@link edu.wis.jtlv.env.Env#newVar(String, String)} or
	 * {@link edu.wis.jtlv.env.Env#newVar(String, String, int)}
	 * </p>
	 * 
	 * @param thread_idx  The factory to create this BDD for.
	 * @param values_size The size of the domain.
	 * @return A new domain to be used.
	 * 
	 * @see edu.wis.jtlv.env.Env#getVar(String, String)
	 * @see edu.wis.jtlv.env.Env#newVar(String, String)
	 * @see edu.wis.jtlv.env.Env#newVar(String, String, int)
	 */
	private static BDDDomain allocBDD(int values_size) {
		return bdd_manager.allocBDD(values_size);
	}

	// ////////////////////////////////////////////////////////////////////////
	// /////////////// String Functionalities /////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////

	/**
	 * <p>
	 * Prepare a string representing the If-Then-Else form of the given BDD. This
	 * kind of printing is better by an order of magnitude with respect to simple
	 * BDD.toString, which is exponential to the size of the BDD.<br>
	 * The naming is done respective to the given module, i.e. the path leading to
	 * the variable from the given module.<br>
	 * The print is done with line indentation.
	 * </p>
	 * 
	 * <p>
	 * <b> It is advise to use this procedure for printing big BDDs!! </b>
	 * </p>
	 * 
	 * @param con The module context of the BDD.
	 * @param b   The BDD to prepare string to.
	 * @return The string representing the BDD.
	 * 
	 * @see edu.wis.jtlv.env.Env#toNiceString(BDD)
	 * @see edu.wis.jtlv.env.Env#toNiceString(Module, BDD, String)
	 * @see edu.wis.jtlv.env.Env#toNiceString(BDD, String)
	 * @see edu.wis.jtlv.env.Env#toNiceSignleLineString(Module, BDD)
	 * @see edu.wis.jtlv.env.Env#toNiceSignleLineString(BDD)
	 */
	public static String toNiceString(BDD b) {
		// printing is always done at the main factory
		return bdd_manager.toNiceString(b);
	}

	/**
	 * <p>
	 * Prepare a string representing the If-Then-Else form of the given BDD. This
	 * kind of printing is better by an order of magnitude with respect to simple
	 * BDD.toString, which is exponential to the size of the BDD.<br>
	 * The naming is done respective to the given module, i.e. the path leading to
	 * the variable from the given module.<br>
	 * The print is done with line indentation starting with the given initial
	 * indentation.
	 * </p>
	 * 
	 * <p>
	 * <b> It is advise to use this procedure for printing big BDDs!! </b>
	 * </p>
	 * 
	 * @param con         The module context of the BDD.
	 * @param b           The BDD to prepare string to.
	 * @param startIndent The initial indentation.
	 * @return The string representing the BDD.
	 * 
	 * @see edu.wis.jtlv.env.Env#toNiceString(Module, BDD)
	 * @see edu.wis.jtlv.env.Env#toNiceString(BDD)
	 * @see edu.wis.jtlv.env.Env#toNiceString(BDD, String)
	 * @see edu.wis.jtlv.env.Env#toNiceSignleLineString(Module, BDD)
	 * @see edu.wis.jtlv.env.Env#toNiceSignleLineString(BDD)
	 */
	public static String toNiceString(BDD b, String startIndent) {
		// printing is always done at the main factory
		return bdd_manager.toNiceString(b, startIndent);
	}

	/**
	 * <p>
	 * Prepare a string representing the If-Then-Else form of the given BDD. This
	 * kind of printing is better by an order of magnitude with respect to simple
	 * BDD.toString, which is exponential to the size of the BDD.<br>
	 * The naming is done respective to the given module, i.e. the path leading to
	 * the variable from the given module.<br>
	 * The print is done in a single line.
	 * </p>
	 * 
	 * <p>
	 * <b> It is advise to use this procedure for printing big BDDs!! </b>
	 * </p>
	 * 
	 * @param con The module context of the BDD.
	 * @param b   The BDD to prepare string to.
	 * @return The string representing the BDD.
	 * 
	 * @see edu.wis.jtlv.env.Env#toNiceString(Module, BDD)
	 * @see edu.wis.jtlv.env.Env#toNiceString(BDD)
	 * @see edu.wis.jtlv.env.Env#toNiceString(Module, BDD, String)
	 * @see edu.wis.jtlv.env.Env#toNiceString(BDD, String)
	 * @see edu.wis.jtlv.env.Env#toNiceSignleLineString(BDD)
	 */
	public static String toNiceSignleLineString(BDD b) {
		// printing is always done at the main factory
		if (b == null) {
			return "null";
		}
		if (b.nodeCount() > 32) {
			return "BDD is too large to print";
		}
		String res = bdd_manager.toNiceSignleLineString(b);
		return res;
	}

	// ////////////////////////////////////////////////////////////////////////
	// /////////////// Naming and Coupling Functionalities ////////////////////
	// ////////////////////////////////////////////////////////////////////////

	/**
	 * <p>
	 * The empty set of fields.
	 * </p>
	 * 
	 * @return The empty set of fields.
	 */
	public static BDDVarSet getEmptySet() {
		return bdd_manager.emptySet();
	}

	// A workaround (better implemented in JTLVBDDFactory, as of 1.3.2)
	private static BDDVarSet intersect(BDDVarSet a, BDDVarSet b) {
		return bdd_manager.intersect(a, b);
	}

	/**
	 * <p>
	 * Given a BDD, returns the same BDD in its prime version of the variables.
	 * </p>
	 * 
	 * @param unp_bdd The unprimed BDD.
	 * @return The primed version of the given BDD.
	 * @throws BDDException When trying to prime BDD with primed fields.
	 * 
	 * @see edu.wis.jtlv.env.Env#prime(BDD, ModuleBDDField[])
	 * @see edu.wis.jtlv.env.Env#unprime(BDD)
	 * @see edu.wis.jtlv.env.Env#unprime(BDD, ModuleBDDField[])
	 * @see edu.wis.jtlv.env.Env#containPrimeVars(BDD)
	 * @see edu.wis.jtlv.env.Env#containUnprimeVars(BDD)
	 */
	public static BDD prime(BDD unp_bdd) throws BDDException {
		return bdd_namer.prime(unp_bdd);
	}

	/**
	 * <p>
	 * Given a BDD, returns the same BDD with the prime version of the given array
	 * of fields.
	 * </p>
	 * 
	 * @deprecated due to performance issues. Try to use prime(BDD) where ever
	 *             possible.
	 * 
	 * @param unp_bdd The given BDD.
	 * @param couples The fields to prime.
	 * @return A BDD with primed version of the given fields.
	 * @throws BDDException When trying to prime already primed fields.
	 * 
	 * @see edu.wis.jtlv.env.Env#prime(BDD)
	 * @see edu.wis.jtlv.env.Env#unprime(BDD)
	 * @see edu.wis.jtlv.env.Env#unprime(BDD, ModuleBDDField[])
	 * @see edu.wis.jtlv.env.Env#containPrimeVars(BDD)
	 * @see edu.wis.jtlv.env.Env#containUnprimeVars(BDD)
	 */
	public static BDD prime(BDD unp_bdd, ModuleBDDField[] couples) throws BDDException {
		return bdd_namer.prime(unp_bdd, couples);
	}

	/**
	 * <p>
	 * Given a BDD, returns the same BDD in its unprime version of the fields.
	 * </p>
	 * 
	 * @param p_bdd The primed BDD.
	 * @return The unprimed version of the given BDD.
	 * @throws BDDException When trying to unprime BDD with unprime fields.
	 * 
	 * @see edu.wis.jtlv.env.Env#prime(BDD)
	 * @see edu.wis.jtlv.env.Env#prime(BDD, ModuleBDDField[])
	 * @see edu.wis.jtlv.env.Env#unprime(BDD, ModuleBDDField[])
	 * @see edu.wis.jtlv.env.Env#containPrimeVars(BDD)
	 * @see edu.wis.jtlv.env.Env#containUnprimeVars(BDD)
	 */
	public static BDD unprime(BDD p_bdd) throws BDDException {
		return bdd_namer.unprime(p_bdd);
	}

	/**
	 * <p>
	 * Given a BDD, returns the same BDD with the unprime version of the given array
	 * of fields.
	 * 
	 * </p>
	 * 
	 * @deprecated due to performance issues. Try to use unprime(BDD) where ever
	 *             possible.
	 * 
	 * @param p_bdd   The given BDD.
	 * @param couples The fields to unprime.
	 * @return A BDD with unprimed version of the given fields.
	 * @throws BDDException When trying to unprime already unprimed fields.
	 * 
	 * @see edu.wis.jtlv.env.Env#prime(BDD)
	 * @see edu.wis.jtlv.env.Env#prime(BDD, ModuleBDDField[])
	 * @see edu.wis.jtlv.env.Env#unprime(BDD)
	 * @see edu.wis.jtlv.env.Env#containPrimeVars(BDD)
	 * @see edu.wis.jtlv.env.Env#containUnprimeVars(BDD)
	 */
	public static BDD unprime(BDD p_bdd, ModuleBDDField[] couples) throws BDDException {
		return bdd_namer.unprime(p_bdd, couples);
	}

	/**
	 * <p>
	 * Check whether the given BDD contains a prime version of some field.
	 * </p>
	 * 
	 * @param set The BDD to check.
	 * @return true if the given BDD contains prime version of some field, false
	 *         otherwise.
	 * 
	 * @see edu.wis.jtlv.env.Env#containUnprimeVars(BDD)
	 * @see edu.wis.jtlv.env.Env#prime(BDD)
	 * @see edu.wis.jtlv.env.Env#prime(BDD, ModuleBDDField[])
	 * @see edu.wis.jtlv.env.Env#unprime(BDD)
	 * @see edu.wis.jtlv.env.Env#unprime(BDD, ModuleBDDField[])
	 */
	public static boolean containPrimeVars(BDD set) {
		return bdd_namer.has_prime_vars(set);
	}

	/**
	 * <p>
	 * Check whether the given BDD contains an unprime version of some field.
	 * </p>
	 * 
	 * @param set The BDD to check.
	 * @return true if the given BDD contains unprime version of some field, false
	 *         otherwise.
	 * 
	 * @see edu.wis.jtlv.env.Env#containPrimeVars(BDD)
	 * @see edu.wis.jtlv.env.Env#prime(BDD)
	 * @see edu.wis.jtlv.env.Env#prime(BDD, ModuleBDDField[])
	 * @see edu.wis.jtlv.env.Env#unprime(BDD)
	 * @see edu.wis.jtlv.env.Env#unprime(BDD, ModuleBDDField[])
	 */
	public static boolean containUnprimeVars(BDD set) {
		return bdd_namer.has_unprime_vars(set);
	}

	/**
	 * <p>
	 * Get all primed variables in the system.
	 * </p>
	 * 
	 * @return All primed variables in the system.
	 * 
	 * @see edu.wis.jtlv.env.Env#globalUnprimeVars()
	 * @see edu.wis.jtlv.env.Env#globalVarsMinus(BDDVarSet)
	 */
	public static BDDVarSet globalPrimeVars() {
		return bdd_namer.globalPrimeVars();
	}

	/**
	 * <p>
	 * Get all unprimed variables in the system.
	 * </p>
	 * 
	 * @return All unprimed variables in the system.
	 * 
	 * @see edu.wis.jtlv.env.Env#globalPrimeVars()
	 * @see edu.wis.jtlv.env.Env#globalVarsMinus(BDDVarSet)
	 */
	public static BDDVarSet globalUnprimeVars() {
		return bdd_namer.globalUnprimeVars();
	}

	/**
	 * <p>
	 * Get the BDDPairing - the variables that represent primed and unprimed var
	 * </p>
	 * 
	 */
	public static BDDPairing allCouplesPairing() throws BDDException {
		return bdd_namer.all_couples_pairing();
	}

	/**
	 * <p>
	 * Get the set of all variables (including the prime and unprime versions),
	 * except the given set of variables.
	 * </p>
	 * 
	 * @param minus The variable set to remove.
	 * @return A set of all variables except the given ones.
	 * 
	 * @see edu.wis.jtlv.env.Env#globalPrimeVars()
	 * @see edu.wis.jtlv.env.Env#globalUnprimeVars()
	 */
	public static BDDVarSet globalVarsMinus(BDDVarSet minus) {
		return bdd_namer.globalVarsMinus(minus);
	}

	/**
	 * <p>
	 * Given a set of state and a transitions, the procedure return all states which
	 * can lead in a single step to the given states.
	 * </p>
	 * 
	 * @param trans The given transitions.
	 * @param to    The set of state to be reach.
	 * @return The set of states which can lead in a single step to the given
	 *         states.
	 * 
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	public static BDD pred(BDD trans, BDD to) {
		return pred(trans, to, Env.globalPrimeVars());
	}

	/**
	 * <p>
	 * Given a set of state, transitions, and a set of variables, the procedure
	 * return all states which can lead in a single step over the set of variables,
	 * to the given states.
	 * </p>
	 * 
	 * <p>
	 * Note that the returned set of states can contain prime version of the
	 * variable (not in primeVars).
	 * </p>
	 * 
	 * @param trans     The given transitions.
	 * @param to        The set of state to be reach.
	 * @param primeVars The set of variable qualified for the step.
	 * @return The set of states which can lead in a single step to the given
	 *         states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	private static BDD pred(BDD trans, BDD to, BDDVarSet primeVars) {
		BDD prime_to = Env.prime(to);
		prime_to.andWith(trans.id());
		
		BDD res = prime_to.exist(primeVars);
		prime_to.free();
		
		return res;
	}

	/**
	 * <p>
	 * Given a set of state and a transitions, the procedure return all states which
	 * can be reached in a single step from the given states.
	 * </p>
	 * 
	 * @param from  The given set of state.
	 * @param trans The given transitions.
	 * @return The set of states which can be reached in a single step from the
	 *         given states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	public static BDD succ(BDD from, BDD trans) {
		return succ(from, trans, Env.globalUnprimeVars());
	}

	/**
	 * <p>
	 * Given a set of state, transitions, and a set of variables, the procedure
	 * return all states which can be reached in a single step over the set of
	 * variables, to the given states.
	 * </p>
	 * 
	 * <p>
	 * Note that the returned set of states can contain prime version of the
	 * variable (not in primeVars).
	 * </p>
	 * 
	 * @param from
	 * @param trans
	 * @param unprimeVars The set of variable qualified for the step.
	 * @return The set of states which can be reached in a single step from the
	 *         given states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	private static BDD succ(BDD from, BDD trans, BDDVarSet unprimeVars) {
		BDD tmp = from.and(trans);
		BDD prime_to = tmp.exist(unprimeVars);
		tmp.free();
		tmp = Env.unprime(prime_to);
		prime_to.free();
		return tmp;
	}

	/**
	 * <p>
	 * Given a set of state and a transitions, the procedure return all states which
	 * can lead in any number of steps to given states.
	 * </p>
	 * 
	 * @param trans The given transitions.
	 * @param to    The set of state to be reach.
	 * @return The set of states which can lead in any number of steps to the given
	 *         states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	public static BDD allPred(BDD trans, BDD to) {
		return kPred(trans, to, Env.globalPrimeVars(), -1);
	}

	@SuppressWarnings("unused")
	private static BDD allPred(BDD trans, BDD to, BDDVarSet primeVars) {
		return kPred(trans, to, primeVars, -1);
	}

	/**
	 * <p>
	 * Given a set of state and a transitions, the procedure return all states which
	 * can lead in maximum k number of steps to given states.
	 * </p>
	 * 
	 * @param trans The given transitions.
	 * @param to    The set of state to be reach.
	 * @param k     The number of maximum steps allowed.
	 * @return The set of states which can lead in k number of steps to the given
	 *         states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	public static BDD kPred(BDD trans, BDD to, int k) {
		return kPred(trans, to, Env.globalPrimeVars(), k);
	}

	private static BDD kPred(BDD trans, BDD to, BDDVarSet primeVars, int k) {
		int counter = 1;
		BDD oldPred, newPred = to;
		do {
			oldPred = newPred;
			newPred = oldPred.or(pred(trans, oldPred, primeVars));
			counter++;
		} while (!oldPred.equals(newPred) & (counter != k));
		return newPred;
	}

	/**
	 * <p>
	 * Given a set of state and a transitions, the procedure return all states which
	 * can lead in any number of steps to given states, excluding the given states
	 * themselves (in some cases this might be more efficient the allPred, and then
	 * conjuncting out the negation).
	 * </p>
	 * 
	 * @param trans The given transitions.
	 * @param to    The set of state to be reach.
	 * @return The set of states which can lead in any number of steps to the given
	 *         states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	public static BDD allDeltaPred(BDD trans, BDD to) {
		// test for spell chek
		return kDeltaPred(trans, to, Env.globalPrimeVars(), -1);
	}

	@SuppressWarnings("unused")
	private static BDD allDeltaPred(BDD trans, BDD to, BDDVarSet primeVars) {
		return kDeltaPred(trans, to, primeVars, -1);
	}

	/**
	 * <p>
	 * Given a set of state and a transitions, the procedure return all states which
	 * can lead in maximum k number of steps to given states, excluding the given
	 * states themselves (in some cases this might be more efficient the kPred, and
	 * then conjuncting out the negation).
	 * </p>
	 * 
	 * @param trans The given transitions.
	 * @param to    The set of state to be reach.
	 * @param k     The number of maximum steps allowed.
	 * @return The set of states which can lead in any number of steps to the given
	 *         states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	public static BDD kDeltaPred(BDD trans, BDD to, int k) {
		return kDeltaPred(trans, to, Env.globalPrimeVars(), k);
	}

	private static BDD kDeltaPred(BDD trans, BDD to, BDDVarSet primeVars, int k) {
		int counter = 1;
		BDD deltaPred = pred(trans, to, primeVars).and(to.not());
		BDD oldPred, newPred = deltaPred;
		if (k == 1)
			return newPred;
		else if (k == 0) // just so it will be close
			return Env.FALSE();
		// else
		do {
			oldPred = newPred;
			deltaPred = pred(deltaPred, trans, primeVars).and(oldPred.not()).and(to.not());
			newPred = oldPred.or(deltaPred);
			counter++;
		} while ((!oldPred.equals(newPred)) & (counter != k));
		return newPred;
	}

	/**
	 * <p>
	 * Given a set of state and a transitions, the procedure return all states which
	 * can be reached in a any number of steps from these states.
	 * </p>
	 * 
	 * @param from  (freed) The given set of state.
	 * @param trans The given transitions.
	 * @return The set of states which can be reached in any number of steps from
	 *         the given states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	public static BDD allSucc(BDD from, BDD trans) {
		return kSucc(from, trans, Env.globalUnprimeVars(), -1);
	}

	@SuppressWarnings("unused")
	private static BDD allSucc(BDD from, BDD trans, BDDVarSet unprimeVars) {
		return kSucc(from, trans, unprimeVars, -1);
	}

	/**
	 * <p>
	 * Given a set of state and a transitions, the procedure return all states which
	 * can be reached in maximum k number of steps from these states.
	 * </p>
	 * 
	 * frees from!
	 * 
	 * @param trans The given transitions.
	 * @param from  The given set of state (is freed).
	 * @param k     The number of maximum steps allowed.
	 * @return The set of states which can be reached in maximum k number of steps
	 *         from the given states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	public static BDD kSucc(BDD from, BDD trans, int k) {
		return kSucc(from, trans, Env.globalUnprimeVars(), k);
	}

	/**
	 * Frees from!
	 * 
	 * @param from        (is freed)
	 * @param trans
	 * @param unprimeVars
	 * @param k
	 * @return
	 */
	private static BDD kSucc(BDD from, BDD trans, BDDVarSet unprimeVars, int k) {
		int counter = 1;
		BDD oldSucc = from.id(), newSucc = from;
		do {
			oldSucc.free();
			oldSucc = newSucc;
			BDD tmp = succ(oldSucc, trans, unprimeVars);
			newSucc = oldSucc.or(tmp);
			tmp.free();
			counter++;
		} while (!oldSucc.equals(newSucc) & (counter != k));
		return newSucc;
	}

	/**
	 * <p>
	 * Given a set of state and a transitions, the procedure return all states which
	 * can be reached in any number of steps from these states, excluding the given
	 * states themselves (in some cases this might be more efficient the allSucc,
	 * and then conjuncting out the negation).
	 * </p>
	 * 
	 * @param trans The given transitions.
	 * @param from  The given set of state.
	 * @return The set of states which can be reached in any number of steps from
	 *         the given states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kDeltaSucc(BDD, BDD, int)
	 */
	public static BDD allDeltaSucc(BDD from, BDD trans) {
		return kDeltaSucc(from, trans, Env.globalUnprimeVars(), -1);
	}

	@SuppressWarnings("unused")
	private static BDD allDeltaSucc(BDD from, BDD trans, BDDVarSet unprimeVars) {
		return kDeltaSucc(from, trans, unprimeVars, -1);
	}

	/**
	 * <p>
	 * Given a set of state and a transitions, the procedure return all states which
	 * can be reached in maximum k number of steps from these states, excluding the
	 * given states themselves (in some cases this might be more efficient the
	 * allSucc, and then conjuncting out the negation).
	 * </p>
	 * 
	 * @param trans The given transitions.
	 * @param from  The given set of state.
	 * @param k     The number of maximum steps allowed.
	 * @return The set of states which can be reached in maximum k number of steps
	 *         from the given states.
	 * 
	 * @see edu.wis.jtlv.env.Env#pred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#succ(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kPred(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#kSucc(BDD, BDD, int)
	 * @see edu.wis.jtlv.env.Env#allDeltaPred(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#allDeltaSucc(BDD, BDD)
	 * @see edu.wis.jtlv.env.Env#kDeltaPred(BDD, BDD, int)
	 */
	public static BDD kDeltaSucc(BDD from, BDD trans, int k) {
		return kDeltaSucc(from, trans, Env.globalUnprimeVars(), k);
	}

	private static BDD kDeltaSucc(BDD from, BDD trans, BDDVarSet unprimeVars, int k) {
		int counter = 1;
		BDD deltaSucc = succ(from, trans, unprimeVars).and(from.not());
		BDD oldSucc, newSucc = deltaSucc;
		if (k == 1)
			return newSucc;
		else if (k == 0) // just so it will be close
			return Env.FALSE();
		// else
		do {
			oldSucc = newSucc;
			deltaSucc = succ(deltaSucc, trans, unprimeVars).and(oldSucc.not()).and(from.not());
			newSucc = oldSucc.or(deltaSucc);
			counter++;
		} while ((!oldSucc.equals(newSucc)) & (counter != k));
		return newSucc;
	}

	// ////////////////////////////////////////////////////////////////////////
	// /////////// Other Functionalities that Should be Hidden ////////////////
	// ////////////////////////////////////////////////////////////////////////
	/**
	 * <p>
	 * Creates a pair object which can then be used to associate two BDD fields.
	 * </p>
	 * 
	 * @param i The factory to make the pair to.
	 * @return A new BDD pair.
	 * @throws ArrayIndexOutOfBoundsException If addressed a non existing factory.
	 * 
	 */
	private static BDDPairing makePair() {
		return bdd_manager.makePair();
	}

	/**
	 * <p>
	 * Given a BDD domain, search and returns the corresponding BDD field.
	 * </p>
	 * 
	 * @param dom The domain to look his variable for.
	 * @return The variable corresponding to the given domain.
	 * 
	 */
	private static ModuleBDDField getVarForDomain(BDDDomain dom) {
		return bdd_namer.getVarForDomain(dom);
	}
	// ////////////////////////////////////////////////////////////////////////
	// /////////////// Internal Classes ///////////////////////////////////////
	// ////////////////////////////////////////////////////////////////////////

	/**
	 * <p>
	 * The JTLVBDDManager is responsible for communicating with all JavaBDD
	 * functionalities.
	 * </p>
	 * 
	 * <p>
	 * Its only holder is the Env, and all operation should be done through the Env.
	 * </p>
	 * 
	 * @see edu.wis.jtlv.env.Env
	 * 
	 * @version {@value edu.wis.jtlv.env.Env#version}
	 * @author yaniv sa'ar.
	 */
	private static final class JTLVBDDManager {
		private static int print_order_domain_threshold = 32;
		private static int range_thrashold = 3;

		static {
			// init 64
			if (System.getProperty("java.vm.name").contains("64")) {
				System.setProperty("buddylib", "buddy64");
				// for some reason the property is not used for CAL and CUDD.
				System.setProperty("cuddlib", "cudd64");
				System.setProperty("callib", "cal64");
			}
		}

		private BDDFactory factory;
		private String factory_name;
		private String package_name;
		private String package_version;
		private int nodenum;
		private int cachesize;

		private JTLVBDDManager() {
		}

		private JTLVBDDManager(BDDFactory f) {
			tear_down();
			reset_factory(f);
		}

		private JTLVBDDManager(int node_num, int cache_size) {
			reset_factory(node_num, cache_size);
		}

		// ////////////////////////////
		// factory methods...
		private void reset_factory(BDDFactory f) {
			this.tear_down();
			this.nodenum = -1;
			this.cachesize = -1;
			this.factory_name = "user_factory";
			// no setup...
			this.factory = f;
		}

		private void reset_factory(int node_num, int cache_size) {
			this.tear_down();
			this.nodenum = node_num;
			this.cachesize = cache_size;
			this.factory_name = prompt2name();
			this.package_name = "net.sf.javabdd." + factory_name;
			this.package_version = BDDFactory.getProperty("bddver", "default");
			this.factory = BDDFactory.init(package_name, package_version, nodenum, cachesize);
		}

		private static String prompt2name() {
			// default is BuDDy...
			String bddpackage = BDDFactory.getProperty("bdd", "jtlv");
			if (bddpackage.equals("cudd"))
				return "CUDDFactory";
			if (bddpackage.equals("cuddadd"))
				return "CUDDFactory$CUDDADDFactory";
			if (bddpackage.equals("jtlv"))
				return "JTLVJavaFactory";

			System.err.println("Failed to find package '" + bddpackage + "' initializing JTLVBDDFactory instead.");
			return "JTLVBDDFactory";
		}

		private void tear_down() {
			destroy_factory();
		}

		private void destroy_factory() {
			if (this.factory == null)
				return;
			this.factory.done();
			this.factory = null;
		}

		public void doOnGC(Method m_gc) {
			this.factory.registerGCCallback(this, m_gc);
		}

		public void doOnResize(Method m_resize) {
			this.factory.registerResizeCallback(this, m_resize);
		}

		public void doOnReorder(Method m_reorder) {
			this.factory.registerReorderCallback(this, m_reorder);
		}

		// /////////////////////////////////

		private BDDBitVector buildVector(BDDDomain domain) {
			return this.factory.buildVector(domain);
		}

		private BDDBitVector buildVector(int[] var) {
			return this.factory.buildVector(var);
		}

		private BDDBitVector buildVector(BigInteger val) {
			return this.factory.constantVector(val);
		}

		private BDDBitVector buildVector(int bitnum, BigInteger val) {
			return this.factory.constantVector(bitnum, val);
		}

		private BDDBitVector buildVector(int bitnum, long val) {
			return this.factory.constantVector(bitnum, val);
		}

		private BDD TRUE() {
			return this.factory.one();
		}

		private BDD FALSE() {
			return this.factory.zero();
		}

		private ADD PLUS_INF() {
			return this.factory.plusInfinity();
		}

		private ADD MINUS_INF() {
			return this.factory.minusInfinity();
		}

		private ADD CONST(double val) {
			return (ADD) this.factory.constant(val);
		}

		private BDDDomain allocBDD(int values_size) {
			return this.factory.extDomain(values_size);
		}

		private BDDVarSet emptySet() {
			return this.factory.emptySet();
		}

		private BDDPairing makePair() {
			return this.factory.makePair();
		}

		/**
		 * @return the factory_name
		 */
		private String factoryName() {
			return factory_name;
		}

		private void save(String filename, BDD to_save, boolean reorder) throws IOException {
			if (reorder) {
				this.factory.reorder(BDDFactory.REORDER_SIFT);
			}
			this.factory.save(filename, to_save);
		}

		private void save(BufferedWriter writer, BDD to_save, boolean reorder) throws IOException {
			if (reorder) {
				this.factory.reorder(BDDFactory.REORDER_SIFT);
			}
			this.factory.save(writer, to_save);
		}

		private BDD load(String filename) throws IOException {
			return this.factory.load(filename);
		}

		private BDD load(BufferedReader reader) throws IOException {
			return this.factory.load(reader);
		}

		private String toNiceString(BDD b) {
			return toNiceStringHelper(b, true, "");
		}

		private String toNiceString(BDD b, String startIndent) {
			return toNiceStringHelper(b, true, startIndent);
		}

		private String toNiceSignleLineString(BDD b) {
			return toNiceStringHelper(b, false, "");
		}

		private String toNiceStringHelper(BDD b, boolean withIndent, String currIndent) {
			// restricting the values, to the ones possible in the domains.
			BDDDomain[] all_doms = b.support().getDomains();
			BDD non_existing_vals = Env.FALSE();
			BDD restrToDom = b.id();
			for (int i = 0; i < all_doms.length; i++) {
				restrToDom.andWith(all_doms[i].domain());
				non_existing_vals.orWith(all_doms[i].domain().not());
			}
			return toNiceStringHelper2(restrToDom, non_existing_vals, withIndent, currIndent);
		}

		private String toNiceStringHelper2(BDD b, BDD non_existing_vals, boolean withIndent, String currIndent) {
			BDD tmp = b.or(non_existing_vals);
			if (tmp.isUniverse()) {
				return "TRUE";
			}
			tmp.free();
			tmp = b.and(non_existing_vals.not());
			if (tmp.isZero()) {
				return "FALSE";
			}
			tmp.free();

			// all relevant domains...
			BDDDomain[] all_doms = b.support().getDomains();
			Arrays.sort(all_doms, new BDDDomainComparator());
			BDDDomain curDom = all_doms[0];

			// ////////////////////////////////////////////////////////////////////
			// preparing
			String res = "";
			String pre = ((withIndent) ? currIndent : ""); // prefix indent...
			String suf = ((withIndent) ? "\n" : " "); // suffix line break...
			String fName = getFieldName(curDom);
			BigInteger[] allVals = getDomainLegitValues(curDom, b);
			Arrays.sort(allVals, new BigIntegerComparator());

			// ////////////////////////////////////////////////////////////////////
			// now we are starting to work on this domain....

			// if this is the last domain, then shortening it...
			if (all_doms.length == 1) {
				res = pre + getValueStr(fName, curDom, allVals);
				return res;
			}

			// first checking if I can do a simple grouping of everything...
			// i.e. all values has the same rest BDD.
			if (valsCanBeGrouped(b, curDom, allVals)) {
				// if (false) {
				BDD rest = b.and(curDom.ithVar(allVals[0])).exist(curDom.set());
				res += pre + "(" + getValueStr(fName, curDom, allVals) + " and";

				String toAdd = toNiceStringHelper2(rest, non_existing_vals, withIndent, currIndent);
				// removing tabes
				while (toAdd.startsWith("\t")) {
					toAdd = toAdd.substring(1);
				}

				// making new line for the next indent in line...
				return res + " " + toAdd + ")";
			}

			// otherwise, enumarating everything.
			// first conjuncting out all not possible values...
			BigInteger[] notVals = opVals(allVals, curDom.size().intValue());
			if (notVals.length > 0)
				res += pre + getNotValueStr(fName, curDom, notVals) + " and" + suf;

			// then conjuncting in all possible values...
			boolean isFirst = true;
			for (BigInteger val : allVals) {
				BDD rest = b.and(curDom.ithVar(val)).exist(curDom.set());
				if (rest.or(non_existing_vals).isUniverse()) {
					rest.free();
					continue;
				}
				res += (isFirst ? "" : " and" + suf) + pre + "(" + fName;
				res += "=" + Env.stringer.elementName(curDom, val) + " -> ";

				// making new line for the next indent in line...
				boolean doIndent = (all_doms.length > 2);
				res += doIndent ? suf : "";
				res += toNiceStringHelper2(rest, non_existing_vals, doIndent & withIndent, currIndent + "\t");
				rest.free();
				res += ")";
				isFirst = false;
			}
			return res;
		}

		/**
		 * check whether BDD b is the same when restricting it to any value from allVals
		 * 
		 * @param b
		 * @param dom
		 * @param allVals
		 * @return
		 */
		private boolean valsCanBeGrouped(BDD b, BDDDomain dom, BigInteger[] allVals) {
			BDD toCompare = b.and(dom.ithVar(allVals[0])).exist(dom.set());
			for (int i = 1; i < allVals.length; i++) {
				BDD toComparei = b.and(dom.ithVar(allVals[i])).exist(dom.set());
				if (!toComparei.equals(toCompare)) {
					toCompare.free();
					toComparei.free();
					return false;
				}
				toComparei.free();
			}
			toCompare.free();
			return true;
		}

		private BigInteger[] getDomainLegitValues(BDDDomain curDom, BDD b) {
			BDD iter_b = b.id();
			Vector<BigInteger> res_vec = new Vector<BigInteger>();

			while (!iter_b.isZero()) {
				BigInteger toAdd = iter_b.scanVar(curDom);
				res_vec.add(toAdd);
				BDD val_bdd = curDom.ithVar(toAdd);
				iter_b.andWith(val_bdd.not());
				val_bdd.free();
			}
			iter_b.free();

			BigInteger[] res = new BigInteger[res_vec.size()];
			res_vec.toArray(res);
			return res;
		}

		private String getFieldName(BDDDomain dom) {
			ModuleBDDField coup = Env.getVarForDomain(dom);
			return coup.getName();
		}

		// all_vals is always sorted...
		private String getValueStr(String field, BDDDomain dom, BigInteger[] allVals) {
			if (allVals.length > (dom.size().intValue() / 2))
				return getNotValueStr(field, dom, opVals(allVals, dom.size().intValue()));
			if (allVals.length == 1)
				return field + "=" + Env.stringer.elementName(dom, allVals[0]);
			String res = field + "=" + "{";
			for (int i = 0; i < allVals.length; i++) {
				res += (i == 0) ? "" : ", ";
				int until = rangeUntil(allVals, i);
				if (until != -1)
					res += Env.stringer.elementNames(dom, allVals[i], allVals[until]);
				else
					res += Env.stringer.elementName(dom, allVals[i]);
			}
			res += "}";
			return res;
		}

		private int rangeUntil(BigInteger[] all_vals, int start_idx) {
			int range_elem_count = 0;
			for (int i = start_idx; i < all_vals.length - 1; i++)
				if (all_vals[i].intValue() + 1 == all_vals[i + 1].intValue())
					range_elem_count++;
				else if (range_elem_count >= range_thrashold)
					return i;
				else
					return -1;
			if (range_elem_count >= range_thrashold)
				return all_vals.length - 1;
			else
				return -1;
		}

		// all_vals is always sorted...
		private String getNotValueStr(String field, BDDDomain dom, BigInteger[] allVals) {
			if (allVals.length > (dom.size().intValue() / 2))
				return getValueStr(field, dom, opVals(allVals, dom.size().intValue()));
			if (allVals.length == 1)
				return field + "!=" + Env.stringer.elementName(dom, allVals[0]);
			String res = field + "!=" + "{";
			for (int i = 0; i < allVals.length; i++) {
				res += (i == 0) ? "" : ", ";
				int until = rangeUntil(allVals, i);
				if (until != -1) {
					res += Env.stringer.elementNames(dom, allVals[i], allVals[until]);
				} else {
					res += Env.stringer.elementName(dom, allVals[i]);
				}
			}
			res += "}";
			return res;
		}

		private BigInteger[] opVals(BigInteger[] cur_vals, int max) {
			Vector<BigInteger> res_vec = new Vector<BigInteger>(max);
			for (int i = 0; i < max; i++) {
				boolean exists = false;
				for (int j = 0; j < cur_vals.length; j++)
					if (cur_vals[j].intValue() == i)
						exists = true;
				if (!exists)
					res_vec.add(new BigInteger("" + i));
			}
			BigInteger[] res = new BigInteger[res_vec.size()];
			res_vec.toArray(res);
			Arrays.sort(res, new BigIntegerComparator());
			return res;
		}

		private class BDDDomainComparator implements Comparator<BDDDomain> {
			public int compare(BDDDomain arg0, BDDDomain arg1) {
				int left = enumDomain(arg0);
				int right = enumDomain(arg1);
				return (left - right);
			}

			public int enumDomain(BDDDomain dom) {
				if (dom.size().intValue() <= print_order_domain_threshold)
					return dom.getIndex();
				else
					// factory.varNum() won't be changed during a single
					// print...
					return (int) (Math.pow(2, factory.varNum())) + dom.getIndex();
			}
		}

		private class BigIntegerComparator implements Comparator<BigInteger> {
			public int compare(BigInteger arg0, BigInteger arg1) {
				return (int) (arg0.longValue() - arg1.longValue());
			}
		}

		// a workaround for a bug in javaBDD
		private BDDVarSet intersect(BDDVarSet a, BDDVarSet b) {
			BDDVarSet res = this.emptySet();
			BDDDomain[] aDoms = a.getDomains();
			BDDDomain[] bDoms = b.getDomains();
			for (int i = 0; i < aDoms.length; i++) {
				for (int j = 0; j < bDoms.length; j++) {
					if (aDoms[i].getIndex() == bDoms[j].getIndex())
						res = res.id().union(aDoms[i].set());
				}
			}
			return res;
		}

	}

	// private static boolean debugCopyBDD = false;

	/**
	 * <p>
	 * The JTLVBDDManagerPairing is responsible for the BDD coupling, i.e. the prime
	 * and unprime versions of the fields.
	 * </p>
	 * 
	 * <p>
	 * Its only holder is the Env, and all operation should be done through the Env.
	 * </p>
	 * 
	 * @version {@value edu.wis.jtlv.env.Env#version}
	 * @author yaniv sa'ar.
	 * 
	 */
	private static final class JTLVBDDManagerPairing {
		// //////////////////////////////////
		// the current implementation is to order the bdd as unprime variable
		// first, and the immediately followed variable is its primed version.
		protected Vector<ModuleBDDField> all_couples;

		private JTLVBDDManagerPairing() {
			all_couples = new Vector<ModuleBDDField>(100);
		}

		/**
		 * The main implementation for creating a variable.
		 * 
		 * @param preface        module name.
		 * @param name_with_tick variable name, can also be in prime notation (e.g. x').
		 * @param values_size
		 * @return the couple bdd created for this name.
		 * @throws ModuleVariableException if the name already exists.
		 */
		private ModuleBDDField new_var(String name_with_tick, int values_size) throws ModuleVariableException {
			// preparing a proper name.
			if (is_prime_name(name_with_tick)) {
				throw new ModuleVariableException("Cannot declare a variable with \'.");
			}
			String name = this.to_simple_name(name_with_tick);

			// preparing the actual bdds.
			if (search_couple(this.to_simple_name(name)) != null) {
				throw new ModuleVariableException("variable \"" + name + "\" already exists.");
			}

			BDDDomain[] couple = new BDDDomain[2];
			couple[0] = Env.allocBDD(values_size);
			couple[1] = Env.allocBDD(values_size);

			// making the pair
			ModuleBDDField new_pair = new ModuleBDDField(couple[0], couple[1], name);

			all_couples.add(new_pair);

			// returning the relevant couple.
			return new_pair;
		}

		private ModuleBDDField getVarForDomain(BDDDomain dom) {
			for (ModuleBDDField coup : this.all_couples) {
				if (dom.getIndex() == coup.getDomain().getIndex())
					return coup;
				if (dom.getIndex() == coup.other().getDomain().getIndex())
					return coup.other();
			}
			return null;
		}

		// ////////////////////////////////////
		// getter without creation
		private ModuleBDDField search_couple(String name) {
			ModuleBDDField[] iter = new ModuleBDDField[this.all_couples.size()];
			all_couples.toArray(iter);
			for (int i = 0; i < iter.length; i++) {
				if ((iter[i].getName().equals(name))) {
					return iter[i];
				}
			}
			return null;
		}

		private ModuleBDDField get_var(String name) {
			ModuleBDDField res;
			try {
				res = search_couple(this.to_simple_name(name));
				if (res == null)
					return null;
				return this.is_prime_name(name) ? res.prime() : res;
			} catch (ModuleVariableException e) {
				return null;
			}
		}

		// ////////////////////////////////////
		// until here all was single var BDD manipulations.
		// here on there are general BDD operations.

		private boolean is_prime_name(String name) throws ModuleVariableException {
			if (name.indexOf("'") != name.lastIndexOf("'")) {
				throw new ModuleVariableException("ERROR: cannot name a variable: \"" + name + "\"");
			}
			return (name.indexOf("'") != -1);
		}

		private String to_simple_name(String name) throws ModuleVariableException {
			return is_prime_name(name) ? name.substring(0, name.indexOf("'")) : name;
		}

		// /////////////////////// pairings ///////////////////////
		private ModuleBDDField[] all_couples_arr = null;

		private ModuleBDDField[] all_couples_arr() {
			if (all_couples_arr != null)
				if (all_couples_arr.length == all_couples.size())
					return all_couples_arr;

			all_couples_arr = new ModuleBDDField[all_couples.size()];
			all_couples.toArray(all_couples_arr);
			return all_couples_arr;
		}

		private BDDPairing all_couples_pairing = null;
		private int all_couples_pairing_size = 0;

		private BDDPairing all_couples_pairing() {
			ModuleBDDField[] pairs = all_couples_arr();
			if (pairs != null) {
				if (pairs.length == all_couples_pairing_size) {
					return all_couples_pairing;
				}
			}
			all_couples_pairing = this.mk_pairs(pairs);
			all_couples_pairing_size = pairs.length;
			return all_couples_pairing;
		}

		private BDD all_buddy_couples_pairing = null;
		private int all_buddy_couples_pairing_size = 0;

		private BDD all_buddy_couples_pairing() {
			ModuleBDDField[] pairs = all_couples_arr();
			if (pairs != null) {
				if (pairs.length == all_buddy_couples_pairing_size) {
					if (all_buddy_couples_pairing == null) {
						all_buddy_couples_pairing = Env.TRUE();
					}
					return all_buddy_couples_pairing;
				}
			}
			all_buddy_couples_pairing_size = pairs.length;
			all_buddy_couples_pairing = this.mk_buddy_pairs(pairs);
			return all_buddy_couples_pairing;
		}

		/**
		 * @deprecated
		 * 
		 * @param unp_bdd
		 * @param pairs
		 * @return
		 * @throws BDDException
		 */
		private BDD prime(BDD unp_bdd, ModuleBDDField[] pairs) throws BDDException {
			// FIXME_DONE: performance issue....
			if (has_prime_vars(unp_bdd, pairs))
				throw new BDDException("ERROR: Cannot prime primed " + "variables: \n\t" + get_prime_vars(pairs));
			BDD res = null;
			if (Env.getFactoryName() == "BuDDyFactory") {
				// workaround due to some kind of a bug in pairing with buddy
				res = unp_bdd.id().and(all_buddy_couples_pairing());
				res = res.exist(get_unprime_vars(pairs));
			} else {
				// mk_pairs is a speedup consumer...
				res = unp_bdd.id().replace(this.mk_pairs(pairs));
			}
			return res;
		}

		private BDD prime(BDD unp_bdd) throws BDDException {
			ModuleBDDField[] pairs = all_couples_arr();
			if (has_prime_vars(unp_bdd, pairs))
				throw new BDDException("ERROR: Cannot prime primed " + "variables: \n\t" + get_prime_vars(pairs));
			BDD res = null;
			if (Env.getFactoryName() == "BuDDyFactory") {
				// workaround due to some kind of a bug in pairing with buddy
				res = unp_bdd.id().and(all_buddy_couples_pairing());
				res = res.exist(get_unprime_vars(pairs));
			} else {
				res = unp_bdd.id().replace(all_couples_pairing());
			}
			return res;
		}

		/**
		 * @deprecated
		 * 
		 * @param p_bdd
		 * @param pairs
		 * @return
		 * @throws BDDException
		 */
		private BDD unprime(BDD p_bdd, ModuleBDDField[] pairs) throws BDDException {
			// FIXME_DONE: performance issue....
			if (has_unprime_vars(p_bdd, pairs))
				throw new BDDException("ERROR: Cannot unprime unprimed " + "variables: \n\t" + get_unprime_vars(pairs));
			BDD res = null;
			if (Env.getFactoryName() == "BuDDyFactory") {
				// workaround due to some kind of a bug in pairing with buddy
				res = p_bdd.id().and(all_buddy_couples_pairing());
				res = res.exist(get_prime_vars(pairs));
			} else {
				// mk_pairs is a speedup consumer...
				res = p_bdd.id().replace(this.mk_pairs(pairs));
			}
			return res;
		}

		private BDD unprime(BDD p_bdd) throws BDDException {
			ModuleBDDField[] pairs = all_couples_arr();
			if (has_unprime_vars(p_bdd, pairs))
				throw new BDDException("ERROR: Cannot unprime unprimed " + "variables: \n\t" + get_unprime_vars(pairs));
			BDD res = null;
			if (Env.getFactoryName() == "BuDDyFactory") {
				// workaround due to some kind of a bug in pairing with buddy
				res = p_bdd.id().and(all_buddy_couples_pairing());
				res = res.exist(get_prime_vars(pairs));
			} else {
				res = p_bdd.id().replace(all_couples_pairing());
			}
			return res;
		}

		private BDDVarSet globalPrimeVars() {
			return get_prime_vars(all_couples_arr());
		}

		private BDDVarSet globalUnprimeVars() {
			return get_unprime_vars(all_couples_arr());
		}

		private BDDVarSet globalPrimeVarsMinus(BDDVarSet primeMinus) {
			Vector<ModuleBDDField> all_less = new Vector<ModuleBDDField>();
			ModuleBDDField[] all = all_couples_arr();

			for (int i = 0; i < all.length; i++) {
				BDDVarSet var = all[i].getOtherDomain().set();
				if (Env.intersect(primeMinus, var).isEmpty())
					all_less.add(all[i]);
			}

			ModuleBDDField[] all_less_arr = new ModuleBDDField[all_less.size()];
			all_less.toArray(all_less_arr);
			return get_prime_vars(all_less_arr);
		}

		private BDDVarSet globalUnprimeVarsMinus(BDDVarSet unprimeMinus) {
			Vector<ModuleBDDField> all_less = new Vector<ModuleBDDField>();
			ModuleBDDField[] all = all_couples_arr();

			for (int i = 0; i < all.length; i++) {
				BDDVarSet var = all[i].getDomain().set();
				if (Env.intersect(unprimeMinus, var).isEmpty())
					all_less.add(all[i]);
			}

			ModuleBDDField[] all_less_arr = new ModuleBDDField[all_less.size()];
			all_less.toArray(all_less_arr);
			return get_unprime_vars(all_less_arr);
		}

		private BDDVarSet globalVarsMinus(BDDVarSet minus) {
			BDDVarSet minusPrime = Env.intersect(minus, this.globalPrimeVars());
			BDDVarSet minusUnprime = Env.intersect(minus, this.globalUnprimeVars());

			return this.globalPrimeVarsMinus(minusPrime).union(this.globalUnprimeVarsMinus(minusUnprime));
		}

		private boolean has_prime_vars(BDD set, ModuleBDDField[] pairs) {
			int[] var_prof = set.varProfile();
			for (int j = 0; j < pairs.length; j++) {
				ModuleBDDField p = pairs[j].isPrime() ? pairs[j] : pairs[j].other();
				int[] vars = p.getDomain().vars();
				for (int i = 0; i < vars.length; i++)
					if (var_prof[vars[i]] > 0)
						return true;
			}
			return false;
		}

		@SuppressWarnings("unused")
		private boolean bad_has_prime_vars(BDD set, ModuleBDDField[] pairs) {
			BDDVarSet all_vars = set.support();
			for (int j = 0; j < pairs.length; j++) {
				BDDVarSet p_var = pairs[j].isPrime() ? pairs[j].support() : pairs[j].prime().support();
				// there is a bug with the intersect
				BDDVarSet un = all_vars.union(p_var);
				if (un.size() != (p_var.size() + all_vars.size()))
					return true;
			}
			return false;
		}

		private boolean has_prime_vars(BDD set) {
			return has_prime_vars(set, all_couples_arr());
		}

		private BDDVarSet get_prime_vars(ModuleBDDField[] pairs) {
			BDDVarSet res = Env.getEmptySet();
			for (int j = 0; j < pairs.length; j++) {
				BDDVarSet p_var = pairs[j].isPrime() ? pairs[j].support() : pairs[j].prime().support();
				res = res.id().union(p_var);
			}
			return res;
		}

		private boolean has_unprime_vars(BDD set, ModuleBDDField[] pairs) {
			int[] var_prof = set.varProfile();
			for (int j = 0; j < pairs.length; j++) {
				ModuleBDDField up = pairs[j].isPrime() ? pairs[j].other() : pairs[j];
				int[] vars = up.getDomain().vars();
				for (int i = 0; i < vars.length; i++)
					if (var_prof[vars[i]] > 0)
						return true;
			}
			return false;
		}

		@SuppressWarnings("unused")
		private boolean bad_has_unprime_vars(BDD set, ModuleBDDField[] pairs) {
			BDDVarSet all_vars = set.support();
			for (int j = 0; j < pairs.length; j++) {
				BDDVarSet up_var = pairs[j].isPrime() ? pairs[j].unprime().support() : pairs[j].support();
				BDDVarSet un = all_vars.id().union(up_var);
				if (un.size() != (up_var.size() + all_vars.size()))
					return true;
			}
			return false;
		}

		private boolean has_unprime_vars(BDD set) {
			return has_unprime_vars(set, all_couples_arr());
		}

		private BDDVarSet get_unprime_vars(ModuleBDDField[] pairs) {
			BDDVarSet res = Env.getEmptySet();
			for (int j = 0; j < pairs.length; j++) {
				BDDVarSet up_var = pairs[j].isPrime() ? pairs[j].unprime().support() : pairs[j].support();
				res = res.id().union(up_var);
			}
			return res;
		}

		private BDD mk_buddy_pairs(ModuleBDDField[] all_couples) {
			BDD res = Env.TRUE();
			for (int i = 0; i < all_couples.length; i++) {
				res = res.id().and(all_couples[i].getDomain().buildEquals(all_couples[i].getOtherDomain()));
			}
			return res;
		}

		private BDDPairing mk_pairs(ModuleBDDField[] all_couples) {
			BDDPairing res = Env.makePair();
			res.reset();
			for (int i = 0; i < all_couples.length; i++) {
				res.set(all_couples[i].getDomain().vars(), all_couples[i].getOtherDomain().vars());
				res.set(all_couples[i].getOtherDomain().vars(), all_couples[i].getDomain().vars());
			}
			return res;
		}

	}

	/**
	 * <p>
	 * The JTLVBDDToString is responsible for naming of the values in the BDD
	 * domain.
	 * </p>
	 * 
	 * <p>
	 * Its holder is variable stringer at Env, and all operation should be done
	 * through the Env (unless another instance of this object is created else where
	 * by the user...).
	 * </p>
	 * 
	 * @version {@value edu.wis.jtlv.env.Env#version}
	 * @author yaniv sa'ar.
	 * 
	 */
	public static class JTLVBDDToString extends BDDToString implements Serializable {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		/**
		 * BDDs of variable values by variable and value
		 */
		private Map<String, Map<String, BDD>> allValues;

		private HashMap<Integer, String[]> dom2vals;
		private HashMap<Integer, Integer[]> dom2range;

		public JTLVBDDToString() {
			dom2vals = new HashMap<Integer, String[]>(100);
			dom2range = new HashMap<Integer, Integer[]>(100);
			allValues = new HashMap<>();
		}

		public void register_domain_module_values(BDDDomain dom, String[] values) {
			dom2vals.put(dom.getIndex(), values);
		}

		public void register_domain_module_values(BDDDomain dom, int range_start, int range_size) {
			dom2range.put(dom.getIndex(), new Integer[] { range_start, range_size });
		}

		public boolean domain_has_module_value(BDDDomain dom, String value) {
			int dom_idx = dom.getIndex();
			String[] all_vals = dom2vals.get(dom_idx);
			if (all_vals == null)
				return false;
			for (int i = 0; i < all_vals.length; i++) {
				if (all_vals[i].equals(value)) {
					return true;
				}
			}
			return false;
		}

		public BigInteger get_module_value_loc(BDDDomain dom, String value) {
			int dom_idx = dom.getIndex();
			String[] all_vals = dom2vals.get(dom_idx);
			if (all_vals == null)
				return new BigInteger("-1");
			for (int i = 0; i < all_vals.length; i++) {
				if (all_vals[i].equals(value)) {
					String val = "" + i;
					return new BigInteger(val);
				}
			}
			return new BigInteger("-1");
		}

		private static boolean print_as_range = false;

		// implementations
		public String elementName(BDDDomain dom, BigInteger j) {
			return elementName(dom.getIndex(), j);
		}

		public String elementName(int i, BigInteger j) {
			String res = "";
			int val_idx = j.intValue();
			String[] all_vals = dom2vals.get(i);
			if (all_vals == null) {
				// looking for range domain
				Integer[] range = dom2range.get(i);
				if (range != null) {
					int range_val = range[0] + val_idx;
					res += range_val;
				} else if (val_idx == 0) {
					res = "false";
				} else if (val_idx == 1) {
					res = "true";
				} else {
					res += "#?#";
				}
			} else {
				if (val_idx < all_vals.length) {
					res = all_vals[val_idx];
				} else {
					res = "#?#";
				}
			}
			return res;
		}

		public String elementNames(BDDDomain dom, BigInteger lo, BigInteger hi) {
			return elementNames(dom.getIndex(), lo, hi);
		}

		public String elementNames(int i, BigInteger lo, BigInteger hi) {
			int lo_idx = lo.intValue();
			int hi_idx = hi.intValue();
			if (lo_idx == hi_idx) {
				return elementName(i, lo);
			}

			if (print_as_range) {
				return "{" + elementName(i, lo) + ".." + elementName(i, hi) + "}";
			}

			String res = "{";
			for (int iter = lo_idx; iter <= hi_idx; iter++) {
				res += elementName(i, new BigInteger("" + lo_idx));
				res += (iter != hi_idx) ? ", " : "}";
			}
			return res;
		}

	}

	/**
	 * For a variable <code>varName</code> and value <code>valName</code> get the
	 * BDD
	 * 
	 * @param varName variable name
	 * @param valName name of the value
	 * @return
	 */
	public static BDD getBDDValue(String varName, String valName) {
		try {
			return stringer.allValues.get(varName).get(valName);
		} catch (NullPointerException n) {
			return null;
		}
	}

	/**
	 * chooses and returns one possible value of variable varName that b allows
	 * 
	 * @param b
	 * @param varName
	 * @return
	 */
	public static String getValName(BDD b, String varName) {
		ModuleBDDField field = getVar(varName);
		BDD oneVal = CoreUtil.satOne(b, field.support());
		String assignment = oneVal.toStringWithDomains(stringer).replace("<", "").replace(">", "");
		return assignment.split(":")[1];
	}

	/**
	 * For a variable <code>varName</code> and value <code>val</code> get the BDD
	 * 
	 * @param varName variable name
	 * @param val     value of the variable
	 * @return
	 */
	public static BDD getBDDValue(String varName, int val) {
		return getBDDValue(varName, Integer.toString(val));
	}

	public static void addBDDValueLookup(String var, String val, BDD ithVar) {
		Map<String, BDD> vals = Env.stringer.allValues.get(var);
		if (vals == null) {
			vals = new HashMap<>();
			Env.stringer.allValues.put(var, vals);
		}
		vals.put(val, ithVar);
	}

	/**
	 * frees an iterable of (iterable of)* BDDs
	 * 
	 * @param v
	 */
	public static void free(Iterable<?> v) {
		if (v != null) {
			for (Object o : v) {
				if (o != null) {
					if (o instanceof Iterable<?>) {
						free((Iterable<?>) o);
					} else if (o instanceof BDD) {
						((BDD) o).free();
					}
				}
			}
		}
	}

	public static void free(BDD[] bs) {
		if (bs != null) {
			for (BDD b : bs) {
				if (b != null) {
					b.free();
				}
			}
		}
	}

	public static void free(BDD[][] bs) {
		if (bs != null) {
			for (BDD[] b : bs) {
				if (b != null) {
					free(b);
				}
			}
		}
	}

	public static void free(BDD[][][] bs) {
		if (bs != null) {
			for (BDD[][] b : bs) {
				if (b != null) {
					free(b);
				}
			}
		}
	}

	public static void free(Map<?, BDD> map) {
		free(map.values());
	}

	/**
	 * returns the list of values as Strings
	 * 
	 * Boolean values are "true" and "false"
	 * 
	 * returns an empty list if variable does not exist
	 * 
	 * @param varName
	 * @return
	 */
	public static List<String> getValueNames(String varName) {
		Map<String, BDD> vals = stringer.allValues.get(varName);
		if (vals != null) {
			return new ArrayList<>(vals.keySet());
		}
		return new ArrayList<>();
	}

	public static void disableReorder() {
		bdd_manager.factory.disableReorder();
	}

	public static void enableReorder() {
		bdd_manager.factory.enableReorder();
	}

	/**
	 * checks whether the variables in the support of the BDD intersect with the
	 * ones in vars
	 * 
	 * @param b
	 * @param vars
	 * @return
	 */
	public static boolean hasVars(BDD b, BDDVarSet vars) {
		BDDVarSet bVars = b.support();
		bVars.intersectWith(vars.id());
		if (bVars.isEmpty()) {
			return false;
		}
		bVars.free();
		return true;
	}

	/**
	 * creates a BDD where the values of all variables agree with those of their
	 * primed version
	 * 
	 * @return
	 */
	public BDD getIdleBDD() {
		BDD idle = Env.TRUE();
		for (ModuleBDDField f : bdd_namer.all_couples) {
			idle.andWith(f.getDomain().buildEquals(f.getOtherDomain()));
		}
		return idle;
	}

	/**
	 * compute a union of the varsets of the unprimed fields
	 * 
	 * @param fields
	 * @return
	 */
	public static BDDVarSet unionUnprime(List<ModuleBDDField> fields) {
		BDDVarSet v = Env.getEmptySet();
		for (ModuleBDDField f : fields) {
			v.unionWith(f.getDomain().set());
		}
		return v;
	}

	/**
	 * compute a union of the varsets of the primed fields
	 * 
	 * @param fields
	 * @return
	 */
	public static BDDVarSet unionPrime(List<ModuleBDDField> fields) {
		BDDVarSet v = Env.getEmptySet();
		for (ModuleBDDField f : fields) {
			v.unionWith(f.getOtherDomain().set());
		}
		return v;
	}

	/**
	 * compute a union of the varsets of the unprimed and primed fields
	 * 
	 * @param fields
	 * @return
	 */
	public static BDDVarSet union(List<ModuleBDDField> fields) {
		BDDVarSet v = Env.getEmptySet();
		for (ModuleBDDField f : fields) {
			v.unionWith(f.getDomain().set());
			v.unionWith(f.getOtherDomain().set());
		}
		return v;
	}

	public static BDDVarSet unionSets(List<BDDVarSet> vars) {
		BDDVarSet v = Env.getEmptySet();
		for (BDDVarSet s : vars) {
			v.unionWith(s.id());
		}
		return v;
	}

	/**
	 * Deletes the registered variable name from JTLV.
	 * 
	 * Don't use when variable is still in use in some BDD.
	 * 
	 * Best not to use it at all!
	 * 
	 * @param name
	 */
	public static void deleteVar(String name) {
		ModuleBDDField f = getVar(name);
		if (f != null) {
			bdd_namer.all_couples.remove(f);
			bdd_namer.all_couples.remove(f.other());
		}
	}

	public static String getBDDPackageInfo() {
		return bdd_manager.factory.getInfo();
	}
	
	/**
	 * Returns a BDD that represents a satisfying assignment to the given set of states. The assignment is random and refers to
	 * the specified Boolean variables.
	 * 
	 * @param states The set of states
	 * @param booleanVars
	 *          The Boolean variables the assignment should refer to
	 * @return A random assignment to the specified variables that satisfies the given set of states.
	 */
	public static BDD randomSat(BDD states, BDDVarSet booleanVars) {

		if (states.isZero()) {
			return Env.FALSE();
		}

		// Now we are sure we have a non trivially FALSE BDD

		BDD satRes = Env.TRUE(), satCheck;
		Random random = new Random();
		boolean randChoice;
		int[] varProfile = booleanVars.toBDD().varProfile();
		for (int i = 0; i < varProfile.length; i++) {
			if (varProfile[i] > 0) {
				randChoice = random.nextBoolean();
				if (randChoice) {
					satCheck = states.and(satRes).andWith(states.getFactory().ithVar(i));
				} else {
					satCheck = states.and(satRes).andWith(states.getFactory().nithVar(i));
				}
				if (!satCheck.isZero()) {
					satRes.andWith(randChoice ? states.getFactory().ithVar(i) : states.getFactory().nithVar(i));
				} else {
					satRes.andWith(randChoice ? states.getFactory().nithVar(i) : states.getFactory().ithVar(i));
				}
				satCheck.free();
				
				if (satRes.isZero()) {
					System.out.println("WAT?");
				}
			}
		}
		
		return satRes;
	}
	
	/**
	 * Returns the current value of the specified variable domain
	 *
	 * @param bdd the bdd
	 * @param domain the domain of the variable
	 * @return
	 * @throws IllegalArgumentException if the variable name does not exist in the Env
	 */
	public static String getValueByDomain(BDD bdd, BDDDomain domain) {

		int[] domVarIdx = domain.vars();
		int[] varPolarity = new int[domVarIdx.length];
		BDD ithVar, valCheck, res = Env.TRUE();
		for (int i = 0 ; i < domVarIdx.length ; i++) {
			ithVar = bdd.getFactory().ithVar(domVarIdx[i]);
			valCheck = bdd.and(res).andWith(ithVar.id());
			if (!valCheck.isZero()) {
				res.andWith(ithVar);
				varPolarity[i] = 1;
			} else {
				res.andWith(bdd.getFactory().nithVar(domVarIdx[i]));
				varPolarity[i] = 0;
				ithVar.free();
			}
			valCheck.free();
		}
		res.free();
		BigInteger pos = BigInteger.ZERO;
		for (int i = domVarIdx.length - 1 ; i >= 0 ; i--) {
			pos = pos.shiftLeft(1);
			if(varPolarity[i] == 1) {
				pos = pos.setBit(0);
			}
		}

		return Env.stringer.elementName(domain.getIndex(), pos);
	}
}
