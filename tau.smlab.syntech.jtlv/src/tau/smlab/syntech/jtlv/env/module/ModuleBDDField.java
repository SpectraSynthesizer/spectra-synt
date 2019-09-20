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

package tau.smlab.syntech.jtlv.env.module;

import net.sf.javabdd.BDDDomain;
import net.sf.javabdd.BDDException;
import net.sf.javabdd.BDDVarSet;

/**
 * <p>
 * JTLVBDDField is an object representing a field variable in JTLV environment.
 * On one hand, this object encapsulate the BDD domain, which does not
 * necessarily have two boolean values. On the other hand, this object also
 * encapsulate both prime and unprime versions of the variables.
 * </p>
 * 
 * @version {@value edu.wis.jtlv.env.Env#version}
 * @author yaniv sa'ar.
 * 
 */
public class ModuleBDDField extends ModuleEntity {
	/**
	 * <p>
	 * The domain of this field.
	 * </p>
	 */
	protected BDDDomain main;

	/**
	 * <p>
	 * Identify whether this field is the prime or the unprime version of the
	 * field.
	 * </p>
	 */
	protected boolean is_prime;

	/**
	 * <p>
	 * The other version of this field. i.e., if this is the unprime version
	 * then this.pair is the prime one, and wise versa.
	 * </p>
	 */
	protected ModuleBDDField pair;
	
	private int traceId;

	/**
	 * <p>
	 * The main public constructor for JTLVBDDField. Given a name, a
	 * domain, and a corresponding domain, a new BDD field is created with a
	 * corresponding prime version of the field.
	 * </p>
	 * 
	 * @param unprime
	 *            The domain to which we are constructing a field.
	 * @param prime
	 *            The other corresponding domain.
	 * @param name
	 *            A name for this field.
	 * 
	 * @see edu.wis.jtlv.env.Env#newVar(String, String)
	 * @see edu.wis.jtlv.env.Env#newVar(String, String, int)
	 */
	public ModuleBDDField(BDDDomain unprime, BDDDomain prime,
			String name) {
		this.main = unprime;
		this.main.setName(name);
		this.name = name;
		this.is_prime = false;
		this.pair = new ModuleBDDField(prime, this, name + "'");
	}

	/**
	 * <p>
	 * The corresponding private constructor for creating the prime version of
	 * the field.
	 * </p>
	 * 
	 * @param prime
	 *            The domain to which we are constructing a field.
	 * @param main_pair
	 *            The other JTLVBDDField which has invoked this instance.
	 * @param name
	 *            A name for this field.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#JTLVBDDField(BDDDomain,
	 *      BDDDomain, String, String)
	 */
	private ModuleBDDField(BDDDomain prime, ModuleBDDField main_pair,
			String name) {
		this.main = prime;
		this.main.setName(name);
		this.name = name;
		this.is_prime = true;
		this.pair = main_pair;
	}

	/**
	 * <p>
	 * Return the other version of the field, regardless of which instance this
	 * is.
	 * </p>
	 * 
	 * @return The other version of the field.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#prime()
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#unprime()
	 */
	public ModuleBDDField other() {
		return this.pair;
	}

	/**
	 * <p>
	 * Get the prime version of this field.
	 * </p>
	 * 
	 * @return The prime version of this field.
	 * @throws BDDException
	 *             If this is a prime version of the field.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#other()
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#unprime()
	 */
	public ModuleBDDField prime() throws BDDException {
		if (is_prime) {
			throw new BDDException("Cannot prime primed variables.");
		}
		return this.other();
	}

	/**
	 * <p>
	 * Get the unprime version of this field.
	 * </p>
	 * 
	 * @return The unprime version of this field.
	 * @throws BDDException
	 *             If this is an unprime version of the field.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#other()
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#prime()
	 */
	public ModuleBDDField unprime() throws BDDException {
		if (!is_prime) {
			throw new BDDException("Cannot unprime unprimed variables.");
		}
		return this.other();
	}

	/**
	 * <p>
	 * Get the set of BDD variables which construct the domain for this field.
	 * </p>
	 * 
	 * @return The set of BDD variables.
	 */
	public BDDVarSet support() {
		// return this.getDomain().set();
		return this.getDomain().ithVar(0).support();
	}

	/**
	 * <p>
	 * Check whether this is a prime version of the field representation.
	 * </p>
	 * 
	 * @return true if this is the prime version of the field, false otherwise.
	 */
	public boolean isPrime() {
		return is_prime;
	}

	/**
	 * <p>
	 * Getter for the domain of this field.
	 * </p>
	 * 
	 * @return The domain of this field.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#getOtherDomain()
	 */
	public BDDDomain getDomain() {
		return this.main;
	}

	/**
	 * <p>
	 * Getter for the domain of the other corresponding field.
	 * </p>
	 * 
	 * @return The domain of the other corresponding field.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#getDomain()
	 */
	public BDDDomain getOtherDomain() {
		return this.other().getDomain();
	}

	/**
	 * <p>
	 * Check whether this object's domain is comparable to the give object
	 * domain.
	 * </p>
	 * 
	 * @param other
	 *            The other object to compare this filed to.
	 * @return true if the given object's domain is comparable to this domain,
	 *         false otherwise.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#equals(Object)
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#strongEquals(Object)
	 */
	public boolean comparable(ModuleBDDField other) {
		return this.getDomain().size().equals((other.getDomain().size()));
	}

	public int getTraceId() {
	  return traceId;
	}
  
	public void setTraceId(int traceId) {
    this.traceId = traceId;
  }

}
