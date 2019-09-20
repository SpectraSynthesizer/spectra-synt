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

// BDDBitVector.java, created Jul 14, 2003 9:50:57 PM by jwhaley
// Copyright (C) 2003 John Whaley
// Licensed under the terms of the GNU LGPL; see COPYING for details.
// Note: re-written to support vectors operations of different lengths and of a negative sign in Oct - Nov, 2016.
package net.sf.javabdd;
import java.math.BigInteger;

/**
 * <p>Bit vector implementation for BDDs.</p>
 * 
 * @author John Whaley
 * @version $Id: BDDBitVector.java,v 1.2 2009/10/18 19:30:54 uid228351 Exp $
 */

public abstract class BDDBitVector {

	protected BDD[] bitvec;

	protected BDD sign;


	/**
	 * Creates a new BDDBitVector instance of the specified length. The new vector is not initialized. However, the sign BDD is 
	 * initialized to zero, i.e., a positive sign.
	 * @param bitnum the length (number of bits) of the new BDDBitVector.
	 */
	protected BDDBitVector(int bitnum) {
		bitvec = new BDD[bitnum];
		sign = this.getFactory().zero();
	}

	/**
	 * Creates a new BDDBitVector instance initialized with the specified value. 
	 * The vector's length is sufficient for the bit representation of the specified value
	 * and equals to {@code floor(log_2(val))+1}. If {@code val} holds a value out of the range of {@code long} type, the 
	 * new vector would have the value of 0. 
	 * 
	 * @param val the value which the new BDDBitVector holds.
	 */
	protected BDDBitVector(BigInteger val) {
		long lVal;
		int bitnum = 0;

		try {
			lVal = val.longValueExact();
		}
		catch (ArithmeticException ae) {
			lVal = 0;
		}
		long sVal = lVal;
		if(lVal < 0) {
			sVal = -lVal;
		}

		do {
			bitnum++;
			sVal >>= 1;
		}
		while(sVal != 0);

		bitvec = new BDD[bitnum];
		sign = this.getFactory().zero();

		initialize(lVal);
	}

	protected void initialize(boolean isTrue) {
		BDDFactory bdd = getFactory();
		for (int n = 0; n < bitvec.length; n++)
			if (isTrue)
				bitvec[n] = bdd.one();
			else
				bitvec[n] = bdd.zero();
	}

	protected void initialize(int val) {
		BDDFactory bdd = getFactory();
		if(val < 0) { 
			sign.free(); 
			sign = bdd.one();
		}
		for (int n = 0; n < bitvec.length; n++) {
			if ((val & 0x1) != 0)
				bitvec[n] = bdd.one();
			else
				bitvec[n] = bdd.zero();
			val >>= 1;
		}
	}

	protected void initialize(long val) {
		BDDFactory bdd = getFactory();
		if(val < 0) { 
			sign.free(); 
			sign = bdd.one();
		}
		for (int n = 0; n < bitvec.length; n++) {
			if ((val & 0x1) != 0)
				bitvec[n] = bdd.one();
			else
				bitvec[n] = bdd.zero();
			val >>= 1;
		}

	}

	protected void initialize(BigInteger val) {
		BDDFactory bdd = getFactory();
		if(val.intValue() < 0) { 
			sign.free(); 
			sign = bdd.one();
		}
		for (int n = 0; n < bitvec.length; n++) {
			if (val.testBit(0))
				bitvec[n] = bdd.one();
			else
				bitvec[n] = bdd.zero();
			val = val.shiftRight(1);
		}
	}

	protected void initialize(int offset, int step) {
		BDDFactory bdd = getFactory();
		for (int n = 0 ; n < bitvec.length ; n++)
			bitvec[n] = bdd.ithVar(offset+n*step);
	}

	protected void initialize(BDDDomain d) {
		initialize(d.vars());
	}

	protected void initialize(int[] var) {
		BDDFactory bdd = getFactory();
		for (int n=0 ; n < bitvec.length ; n++)
			bitvec[n] = bdd.ithVar(var[n]);
	}

	public abstract BDDFactory getFactory();
	/**
	 * Returns a copy of this BDDBitVector.
	 * @return copy of this BDDBitVector.
	 */
	public BDDBitVector copy() {
		BDDFactory bdd = getFactory();
		BDDBitVector dst = bdd.createBitVector(bitvec.length);
		dst.sign = sign.id();

		for (int n = 0; n < bitvec.length; n++)
			dst.bitvec[n] = bitvec[n].id();

		return dst;
	}

	public void replaceWith(BDDBitVector that) {
		//if (bitvec.length != that.bitvec.length)
		//	throw new BDDException();
		this.free();
		this.bitvec = that.bitvec;
		this.sign = that.sign;
		that.bitvec = null;
		that.sign = null;
	}

	/**
	 * Returns true if this BDDBitVector is a constant bit vector, i.e., every BDD bit is a constant boolean function (TRUE or FALSE BDD).
	 * @return true if this BDDBitVector is a constant bit vector.
	 */
	public boolean isConst() {
		for (int n = 0; n < bitvec.length; n++) {
			BDD b = bitvec[n];
			if (!b.isOne() && !b.isZero()) return false;
		}
		return true;
	}

	/**
	 * Returns the decimal value this BDDBitVector encodes if this BDDBitVector is a constant bit vector, and otherwise 0.
	 * @return The decimal value this BDDBitVector encodes.
	 */
	public long val() {
		int n;
		long val = 0L;
		if(this.sign.isOne()) { /*negative value*/
			val = -1L;
		}
		for (n = bitvec.length - 1; n >= 0; n--)
			if (bitvec[n].isOne())
				val = (val << 1) | 1L;
			else if (bitvec[n].isZero())
				val = val << 1;
			else /*non constant BDD Bit Vector*/
				return 0;

		return val;
	}

	/**
	 * Frees this BDDBitVector.
	 */
	public void free() {
		for (int n = 0; n < bitvec.length; n++) {
			bitvec[n].free();
		}
		bitvec = null;
		this.sign.free();
	}

	/**
	 * Returns true if this BDDBitVector encodes only negative values.
	 * @return true if this BDDBitVector encodes only negative values.
	 */
	public boolean isNegative() {
		return this.sign.isOne();
	}


	private static BDDBitVector removeTwosComplements(BDDBitVector vector) {

		BDDFactory bdd = vector.getFactory();

		BDDBitVector compVector = bdd.createBitVector(vector.size());

		/*first, create a two's complement representation of vector*/
		/*invert all bits*/
		for (int i = 0 ; i < vector.size() ; i++) {
			compVector.bitvec[i] = vector.bitvec[i].not();
		}

		compVector.sign = vector.sign.not();

		/*increment by one*/
		BDDBitVector oneVector = bdd.createBitVector(1);
		oneVector.initialize(1);
		BDDBitVector incCompVector = compVector.add(oneVector);

		oneVector.free();
		compVector.free();

		/*if the sign bit is 1 (negative) cancel two's complement form (i.e., use positive form); 
		 * else (sign bit is 0), use positive form as always.*/
		BDDBitVector iteCompVector = bdd.createBitVector(vector.size());

		/*make all representations in a positive form*/
		for (int i = 0 ; i < vector.size() ; i++) {
			iteCompVector.bitvec[i] = vector.sign.ite(incCompVector.bitvec[i], vector.bitvec[i]);
		}

		iteCompVector.sign = bdd.zero(); /*now all representations are positive*/

		incCompVector.free();

		return iteCompVector;
	}


	private static BDDBitVector restoreTwosComplements(BDD resSign, BDDBitVector posVector) {

		BDDFactory bdd = resSign.getFactory();

		BDDBitVector compVector = bdd.createBitVector(posVector.size());

		/*first, create a two's complement representation of posVector*/
		/*invert all bits*/
		for (int i = 0 ; i < posVector.size() ; i++) {
			compVector.bitvec[i] = posVector.bitvec[i].not();
		}

		compVector.sign = bdd.one();

		/*increment by one*/
		BDDBitVector oneVector = bdd.createBitVector(1);
		oneVector.initialize(1);
		BDDBitVector incCompVector = compVector.add(oneVector);

		oneVector.free();
		compVector.free();

		BDD notZeroRes = bdd.zero();

		for(int i = 0 ; i < posVector.size(); i++) {
			notZeroRes.orWith(posVector.bitvec[i].id());
		}

		BDD negAndNotZeroRes = resSign.and(notZeroRes); /*a boolean function that is true iff the number is negative and not zero (thus 
		surely negative : this fix was added to handle cases of a negative number multiplied by zero)*/

		/*if the sign bit is 1 (negative) add\restore two's complement form (i.e., use negative form); 
		 * else (sign bit is 0 -> positive or zero), use positive form as always.*/
		BDDBitVector iteCompVector = bdd.createBitVector(posVector.size());

		for (int i = 0 ; i < posVector.size() ; i++) {
			iteCompVector.bitvec[i] = negAndNotZeroRes.ite(incCompVector.bitvec[i], posVector.bitvec[i]);
		}

		iteCompVector.sign = negAndNotZeroRes; /*now all representations are positive*/

		incCompVector.free();

		return iteCompVector;
	}

	/**
	 * Performs bitwise multiplication of this and that BDDBitVectors. Both vectors can be of different lengths and of different signs (positive\negative). 
	 * @param that
	 * @return
	 */
	public BDDBitVector mult(BDDBitVector that) {

		BDDFactory bdd = this.getFactory();

		int longArgLen = java.lang.Math.max(this.size(), that.size());
		BDDBitVector shorterVector = (this.size() != longArgLen) ? this : that;
		BDDBitVector longerVector = (this.size() == longArgLen) ? this : that;

		BDDBitVector res = bdd.createBitVector(shorterVector.size() + longerVector.size());
		res.initialize(0);

		BDDBitVector posShorterVector = shorterVector;
		if(!shorterVector.sign.isZero()) {
			posShorterVector =  removeTwosComplements(shorterVector);
		}

		BDDBitVector posLongerVector;
		if(!longerVector.sign.isZero()) {
			posLongerVector =  removeTwosComplements(longerVector);
		}
		else {
			posLongerVector = longerVector.copy();

		}

		BDDBitVector resIfShorterIsOne;
		int n = 0;
		for (; n < posShorterVector.size() ; n++) {
			resIfShorterIsOne = res.add(posLongerVector);

			for(int i = 0 ; i < res.size() ; i++) {
				res.bitvec[i] = posShorterVector.bitvec[n].ite(resIfShorterIsOne.bitvec[i], res.bitvec[i]);
			}

			resIfShorterIsOne.free();

			BDDBitVector tmp1 = posLongerVector.shlWithExtraBits(1, bdd.zero()); 
			posLongerVector.free();
			posLongerVector = tmp1;
		}

		res.sign.free();
		res.sign = shorterVector.sign.xor(longerVector.sign);

		if(shorterVector.sign.isZero() && longerVector.sign.isZero()) {
			return res;
		}

		return restoreTwosComplements(res.sign, res);

	}


	/**
	 *  Performs bitwise addition of this and that BDDBitVectors. Both vectors can be of different lengths and of different signs (positive\negative). 
	 * @param that
	 * @return
	 */
	public BDDBitVector add(BDDBitVector that) {

		BDDFactory bdd = this.getFactory();

		int longArgLen = java.lang.Math.max(this.size(), that.size());
		BDDBitVector shorterVector = (this.size() != longArgLen) ? this : that;
		BDDBitVector longerVector = (this.size() == longArgLen) ? this : that;

		BDD c = bdd.zero(); /*set initial carry to zero*/
		BDDBitVector res = bdd.createBitVector(longArgLen+1);
		res.sign.free();

		int n = 0;
		for (; n < shorterVector.size(); n++) {


			/* bitvec[n] = l[n] ^ r[n] ^ c; */
			res.bitvec[n] = bitvec[n].xor(that.bitvec[n]);
			res.bitvec[n].xorWith(c.id());

			/* c = (l[n] & r[n]) | (c & (l[n] | r[n])); */
			BDD tmp1 = bitvec[n].or(that.bitvec[n]);
			tmp1.andWith(c);
			BDD tmp2 = bitvec[n].and(that.bitvec[n]);
			tmp2.orWith(tmp1);
			c = tmp2;
		}

		for (; n < longArgLen; n++) {
			/* bitvec[n] = longVector[n] ^ shortVectorSignExtension ^ c; */
			res.bitvec[n] = longerVector.bitvec[n].xor(shorterVector.sign);
			res.bitvec[n].xorWith(c.id());

			/* c = (longVector[n] & shortVectorSignExtension) | (c & (longVector[n] | shortVectorSignExtension)); */
			BDD tmp1 = longerVector.bitvec[n].or(shorterVector.sign);
			tmp1.andWith(c);
			BDD tmp2 = longerVector.bitvec[n].and(shorterVector.sign);
			tmp2.orWith(tmp1);
			c = tmp2;			
		}

		/*calculate the MSB*/
		res.bitvec[n] = longerVector.sign.xor(shorterVector.sign);
		res.bitvec[n].xorWith(c.id());

		/* c = (longerVector.sign & shorterVector.sign) | (c & (longerVector.sign | shorterVector.sign)); */
		BDD tmp1 = longerVector.sign.or(shorterVector.sign);
		tmp1.andWith(c);
		BDD tmp2 = longerVector.sign.and(shorterVector.sign);
		tmp2.orWith(tmp1);
		c = tmp2;

		/*calculate the sign extension*/
		res.sign =  longerVector.sign.xor(shorterVector.sign);
		res.sign.xorWith(c);

		return res;
	}

	/**
	 *  Performs bitwise subtraction of this and that BDDBitVectors. Both vectors can be of different lengths and of different signs (positive\negative). 
	 * @param that
	 * @return
	 */
	public BDDBitVector sub(BDDBitVector that) {

		BDDFactory bdd = getFactory();

		int longArgLen = java.lang.Math.max(this.size(), that.size());
		BDDBitVector shorterVector = (this.size() != longArgLen) ? this : that;
		BDDBitVector longerVector = (this.size() == longArgLen) ? this : that;

		BDD c = bdd.zero();
		BDDBitVector res = bdd.createBitVector(longerVector.size() + 1);
		res.sign.free();

		int n;
		BDD tmp1, tmp2;
		for (n = 0; n < shorterVector.size(); n++) {
			/* bitvec[n] = l[n] ^ r[n] ^ c; */
			res.bitvec[n] = bitvec[n].xor(that.bitvec[n]);
			res.bitvec[n].xorWith(c.id());

			/* c = (l[n] & r[n] & c) | (!l[n] & (r[n] | c)); */
			tmp1 = that.bitvec[n].or(c);
			tmp2 = this.bitvec[n].apply(tmp1, BDDFactory.less);
			tmp1.free();
			tmp1 = this.bitvec[n].and(that.bitvec[n]);
			tmp1.andWith(c);
			tmp1.orWith(tmp2);

			c = tmp1;
		}

		BDD l, r;
		for (; n < longerVector.size(); n++) {
			/* bitvec[n] = r.sign ^ l[n] ^ c; */
			res.bitvec[n] = longerVector.bitvec[n].xor(c);
			res.bitvec[n].xorWith(shorterVector.sign.id());

			if(longerVector == that) {
				l = this.sign;
				r = that.bitvec[n];
			}
			else {
				l = this.bitvec[n];
				r = that.sign;
			}
			tmp1 = r.or(c);
			tmp2 = l.apply(tmp1, BDDFactory.less);
			tmp1.free();
			tmp1 = l.and(r);
			tmp1.andWith(c);
			tmp1.orWith(tmp2);
			c = tmp1;
		}

		res.bitvec[n] = c.xor(this.sign);
		res.bitvec[n].xorWith(that.sign.id());

		/* c = (l.sign & r.sign & c) | (!l.sign & (r.sign | c)); */
		tmp1 = that.sign.or(c);
		tmp2 = this.sign.apply(tmp1, BDDFactory.less);
		tmp1.free();
		tmp1 = this.sign.and(that.sign);
		tmp1.andWith(c);
		tmp1.orWith(tmp2);
		c = tmp1;

		res.sign = c.xor(this.sign);
		res.sign.xorWith(that.sign.id());

		return res;
	}

	/**
	 * Returns a BDD that encodes less than equal (lte) boolean function of this BDDBitVector and the specified BDDBitVector, i.e, this <= r.
	 * @param r
	 * @return
	 */
	public BDD lte(BDDBitVector r)
	{
		BDDFactory bdd = getFactory();

		BDDBitVector cmpResTmp = this.sub(r);

		BDDBitVector oneVec = bdd.createBitVector(BigInteger.valueOf(1)); 
		BDDBitVector cmpRes = cmpResTmp.sub(oneVec);

		cmpResTmp.free();
		oneVec.free();

		BDD res = cmpRes.sign.id();
		cmpRes.free();
		return res;
	}

	/**
	 * Returns a BDD that encodes less than (lt) boolean function of this BDDBitVector and the specified BDDBitVector, i.e, this < r.
	 * @param r
	 * @return
	 */
	public BDD lt(BDDBitVector r)
	{

		BDDBitVector cmpRes = this.sub(r);

		BDD res = cmpRes.sign.id();
		cmpRes.free();
		return res;
	}

	/**
	 * Returns a BDD that encodes equals (eq) boolean function of this BDDBitVector and the specified BDDBitVector, i.e, this = r.
	 * @param r
	 * @return
	 */
	public BDD eq(BDDBitVector r)
	{

		BDDBitVector cmpRes = this.sub(r);

		BDD res = cmpRes.sign.id();

		for(int i = 0 ; i < cmpRes.size() ; i ++) {
			res.orWith(cmpRes.bitvec[i].id());
		}

		BDD resTmp = res.not();

		res.free();
		cmpRes.free();

		res = resTmp;

		return res;
	}

	/**
	 * Left shift. The vector's length grows.
	 * @param pos
	 * @param c
	 * @return
	 */
	public BDDBitVector shlWithExtraBits(int pos, BDD c) {

		if (pos < 0) {
			throw new BDDException();
		}

		BDDFactory bdd = getFactory();
		BDDBitVector res = bdd.createBitVector(bitvec.length + pos);

		int n;
		for (n = 0; n < pos; n++) {
			res.bitvec[n] = c.id();
		}

		for (n = pos; n < bitvec.length + pos; n++) {
			res.bitvec[n] = bitvec[n - pos].id();
		}

		res.sign = this.sign.id();

		return res;
	}

	/**
	 * Conditional shift left. If the condition if true, then the value will be the left shifted, and otherwise
	 * the same value as no left shift has been performed. The vector's length grows.
	 * @param pos
	 * @param c
	 * @param cond
	 * @return
	 */
	public BDDBitVector condShlWithExtraBits(int pos, BDD c, BDD cond) {

		if (pos < 0) {
			throw new BDDException();
		}

		BDDFactory bdd = getFactory();
		BDDBitVector res = bdd.createBitVector(bitvec.length + pos);

		int n;
		for (n = 0; n < pos; n++) {
			res.bitvec[n] = cond.ite(c, this.bitvec[n]);
		}

		for (n = pos; n < bitvec.length + pos; n++) {
			if(n < bitvec.length) {
				res.bitvec[n] = cond.ite(bitvec[n - pos], this.bitvec[n]);
			}
			else {
				res.bitvec[n] = cond.ite(bitvec[n - pos], this.sign);
			}
		}

		res.sign = this.sign.id();

		return res;
	}

	/**
	 * Right shift. The vector's length becomes shorter.
	 * @param pos
	 * @return
	 */
	public BDDBitVector shrWithLessBits(int pos) {
		if (pos < 0) {
			throw new BDDException();
		}

		int maxnum = Math.max(0, bitvec.length - pos);

		BDDFactory bdd = getFactory();
		BDDBitVector res = bdd.createBitVector(maxnum);

		for (int n = 0 ; n < maxnum ; n++) {
			res.bitvec[n] = bitvec[n + pos].id();
		}

		res.sign = this.sign.id();

		return res;
	}

	public int size() {
		return bitvec.length;
	}

	public BDD getBit(int n) {
		return bitvec[n];
	}

	/**
	 * Left shift. The vector's length remains the same.
	 * @param pos
	 * @param c
	 * @return
	 */
	public BDDBitVector shl(int pos, BDD c) {
		if (pos < 0)
			throw new BDDException();

		int minnum = Math.min(bitvec.length, pos);

		BDDFactory bdd = getFactory();
		BDDBitVector res = bdd.createBitVector(bitvec.length);

		int n;
		for (n = 0; n < minnum; n++)
			res.bitvec[n] = c.id();

		for (n = minnum; n < bitvec.length; n++)
			res.bitvec[n] = bitvec[n - pos].id();

		res.sign = this.sign.id();

		return res;
	}
	/**
	 * Right shift. The vector's length remains the same.
	 * @param pos
	 * @param c
	 * @return
	 */
	public BDDBitVector shr(int pos, BDD c) {	
		if (pos < 0)
			throw new BDDException();

		int maxnum = Math.max(0, bitvec.length - pos);

		BDDFactory bdd = getFactory();
		BDDBitVector res = bdd.createBitVector(bitvec.length);

		int n;
		for (n = maxnum ; n < bitvec.length ; n++)
			res.bitvec[n] = c.id();

		for (n = 0 ; n < maxnum ; n++)
			res.bitvec[n] = bitvec[n+pos].id();

		res.sign = this.sign.id();

		return res;
	}


	private static boolean powerOfTwo(long c) {
		boolean haveSeenOne = false;
		while(c != 0) {
			if((c & 1L) != 0) {
				if(haveSeenOne) {
					return false;
				}
				haveSeenOne = true;
			}
			c >>= 1;
		}
		return haveSeenOne;
	}
	/**
	 * Computes {@code this modulo c}
	 * @param c the modulo value
	 * @return
	 */
	public BDDBitVector mod (long c) {
		if(c <= 0L) {
			throw new BDDException("modulo divisor must be positive");
		}
		BDDFactory bdd = this.getFactory();
		BDDBitVector cBitVec = bdd.createBitVector(BigInteger.valueOf(c));
		BDDBitVector result =  mod(cBitVec, false, true);
		cBitVec.free();
		return result;
	}
	
	public BDDBitVector mod (BDDBitVector divisor) {
		return mod (divisor, true, false);
	}
	
	/**
	 * Computes {@code this modulo divisor}. The divisor must have positive values only. If non positive value 
	 * is found a {@link#BDDException} is thrown. However, {@code this} can be of any integer value.
	 * @param divisor
	 * @param checkZeroDivisor Whether to check if the divisor can get the value of zero.
	 * @param constDivisor Whether the divisor is a constant.
	 * @return The modulo result.
	 */
	public BDDBitVector mod (BDDBitVector divisor, boolean checkZeroDivisor, boolean constDivisor) {
		if(!divisor.sign.isZero()) { /*check if the divisor can get negative values*/
			throw new BDDException("modulo divisor must be positive");
		}
		BDDFactory bdd = getFactory();

		BDDBitVector result;
		if(this.sign.isZero()) { /*the dividend only has non-negative values*/
			if(constDivisor && powerOfTwo(divisor.val())) { 
				/*check if the divisor is a constant of a power of 2. 
				If so, optimize the computation*/

				int bitnum = 0;
				long tmpC = divisor.val()-1;
				
				while(tmpC != 0 && bitnum < this.bitvec.length) {
					bitnum++;
					tmpC >>= 1;	
				}
				
				result = bdd.createBitVector(bitnum);

				for(int i = 0; i < result.bitvec.length; i++) {
					result.bitvec[i] = this.bitvec[i].id();
				}
			}
			else {
				/*perform modulo division, and return the remainder which is also the modulo*/
				result = this.divmod(divisor, checkZeroDivisor, false, constDivisor);
			}
		}
		else { /*the dividend has negative value(s)*/

			/*Compute the remainder (this is the modulo only if this >= 0): 
			 * this % c = this - (this / c) * c;
			 * 
			 * */

			BDDBitVector posThis = removeTwosComplements(this); 			

			BDDBitVector posDivRes = posThis.divmod(divisor, checkZeroDivisor, true, constDivisor);

			BDDBitVector divRes = restoreTwosComplements(this.sign, posDivRes);

			posDivRes.free();
			posThis.free();

			BDDBitVector resTmp1 = divRes.mult(divisor);
			BDDBitVector remainder = this.sub(resTmp1);

			resTmp1.free();
			divRes.free();

			if(!remainder.sign.isZero()) { /*We have negative remainder(s)*/

				/*Compute modulo in case of a negative remainder (possible only if this < 0): remainder + c */

				BDDBitVector resIfRNeg = remainder.add(divisor); 

				result = bdd.createBitVector(remainder.bitvec.length);

				for(int i = 0 ; i < result.bitvec.length ; i ++) {
					result.bitvec[i] = remainder.sign.ite(resIfRNeg.bitvec[i], remainder.bitvec[i]);
				}
				remainder.free();
				resIfRNeg.free();

			}
			else {
				result = remainder;
			}
		}

		return result;
	}
	
	private BDD isNonZeroBdd(BDDBitVector vector) {
		BDDFactory bdd = getFactory();
		BDD res = bdd.zero();
		for(int i = 0 ; i < vector.bitvec.length ; i ++) {
			res.orWith(vector.bitvec[i].id());
		}
		return res;
	}
	
	/**
	 *  Performs bitwise (modulo 2) long division of this BDDBitVector (the dividend) and divisor (the divisor). 
	 *  Both vectors can be different lengths. However, both vectors must encode <i>positive</i> numbers only.
	 * 
	 * @param divisor The divisor.
	 * @param retRes If true, returns the quotient; otherwise, returns the remainder.
	 * @param checkZeroDivisor Whether to check if the divisor can have a value of 0.
	 * @param constDivisor Whether the divisor is a constant.
	 * @return The result or the remainder of the division, depending on the value of {@code retRes}.
	 */
	public BDDBitVector divmod(BDDBitVector divisor, boolean checkZeroDivisor, boolean retRes, boolean constDivisor) {
		if (!divisor.sign.isZero()) { /*check if the divisor can get negative values*/
			throw new BDDException("divisor must be positive");
		}

		BDDFactory bdd = getFactory();

		if(checkZeroDivisor) { 	/*check the divisor cannot have a value of 0 */
			
			BDD zeroCheck = isNonZeroBdd(divisor);
			
			if(!zeroCheck.isOne()) {
				throw new BDDException("Divisor cannot have a value of 0");
			}
			zeroCheck.free();

		}

		/*set the result (quotient) to be 0 in the same length (number of bits) of the dividend*/
		BDDBitVector quotient = bdd.buildVector(bitvec.length, false);

		/*set the initial remainder to be the dividend*/
		BDDBitVector remainder = this.copy();

		BDDBitVector product = divisor.copy(), productTmp, termTmp;
		BDD prodLteDiv = product.lt(this);//product.lte(this);

		BDDBitVector oneConstVector = bdd.constantVector(BigInteger.valueOf(1L));
		BDDBitVector term = oneConstVector;

		/* the divisor multiplied by 2, resulting in product = divisor * 2^i, such that product * 2 > dividend
		 * 
		 * */

		/* term = 2^i, such that divisor * 2^(i+1) > divided
		 * 
		 * */

		while(!prodLteDiv.isZero()) {
			/*shift left only products such that product < dividend*/
			if(constDivisor) {
				productTmp = product.shlWithExtraBits(1, bdd.zero());
			}
			else {
				productTmp = product.condShlWithExtraBits(1, bdd.zero(), prodLteDiv);
			}

			product.free();
			product = productTmp;

			/*shift left only terms such that term < dividend*/
			if(constDivisor) {
				termTmp = term.shlWithExtraBits(1, bdd.zero());
			}
			else {
				termTmp = term.condShlWithExtraBits(1, bdd.zero(), prodLteDiv);				
			}

			term.free();
			term = termTmp;

			prodLteDiv.free();
			prodLteDiv = product.lt(this);
		}


		BDD prodLteRemainder = product.lte(remainder);
		BDD termIsNonZero = isNonZeroBdd(term); /*get the bdd that evaluates to true when the term vector
		gets a non zero value*/
		
		prodLteRemainder.andWith(termIsNonZero.id());
		
		BDDBitVector quotientIfPLteR, remainderIfPLteR, quotientTmp, remainderTmp;//,productTmp, termTmp;
		
		
		
		while(!termIsNonZero.isZero()) { /*term >= 1*/

			if(!prodLteRemainder.isZero()) { /*product <= remainder*/

				quotientIfPLteR = quotient.add(term);
				quotientTmp = bdd.createBitVector(quotientIfPLteR.bitvec.length);

				/*quotientIfPLteR.bitvec.length > quotient.bitvec.length*/

				for(int i = 0 ; i < quotientTmp.bitvec.length ; i++) {
					if(i < quotient.bitvec.length) {
						quotientTmp.bitvec[i] = prodLteRemainder.ite(quotientIfPLteR.bitvec[i], quotient.bitvec[i]);
					} 
					else {
						quotientTmp.bitvec[i] = prodLteRemainder.ite(quotientIfPLteR.bitvec[i], quotient.sign);
					}
				}


				remainderIfPLteR = remainder.sub(product);
				remainderTmp = bdd.createBitVector(remainderIfPLteR.bitvec.length);

				/*remainderIfPLteR.bitvec.length > remainder.bitvec.length*/
				for(int i = 0 ; i < remainderTmp.bitvec.length ; i++) {
					if(i < remainder.bitvec.length) {
						remainderTmp.bitvec[i] = prodLteRemainder.ite(remainderIfPLteR.bitvec[i], remainder.bitvec[i]);
					} 
					else {
						remainderTmp.bitvec[i] = prodLteRemainder.ite(remainderIfPLteR.bitvec[i], remainder.sign);
					}
				}

				quotientIfPLteR.free();
				quotient.free();
				remainderIfPLteR.free();
				remainder.free();

				quotient = quotientTmp;
				remainder = remainderTmp;

			}

			/*divide (right shift) product and term by 2*/
			productTmp = product.shrWithLessBits(1);
			product.free();
			product = productTmp;

			termTmp = term.shrWithLessBits(1);
			term.free();
			term = termTmp;

			prodLteRemainder.free();
			prodLteRemainder = product.lte(remainder);
			termIsNonZero.free();
			termIsNonZero = isNonZeroBdd(term);
			prodLteRemainder.andWith(termIsNonZero.id());
		}

		return (retRes ? quotient : remainder);
	}

	///////////////////////////////////////////////////////////////////////////////////////////
	//////////////////////////// commented out methods of the original implementation /////////
	///////////////////////////////////////////////////////////////////////////////////////////

	/*BDD lte(BDDBitVector r) // less than equal 
	{
		if (this.bitvec.length != r.bitvec.length)
			throw new BDDException();

		BDDFactory bdd = getFactory();
		BDD p = bdd.one();
		for (int n = 0 ; n < bitvec.length ; n++) {
			 p = (!l[n] & r[n]) |
	 *     bdd_apply(l[n], r[n], bddop_biimp) & p; 

			BDD tmp1 = bitvec[n].apply(r.bitvec[n], BDDFactory.less);
			BDD tmp2 = bitvec[n].apply(r.bitvec[n], BDDFactory.biimp);
			tmp2.andWith(p);
			tmp1.orWith(tmp2);
			p = tmp1;
		}
		return p;
	}*/

	/*	public BDDBitVector map2(BDDBitVector that, BDDFactory.BDDOp op) {
	if (bitvec.length != that.bitvec.length)
		throw new BDDException();

	BDDFactory bdd = getFactory();
	BDDBitVector res = bdd.createBitVector(bitvec.length);
	for (int n=0 ; n < bitvec.length ; n++)
		res.bitvec[n] = bitvec[n].apply(that.bitvec[n], op);

	return res;
	}
	 */



	/*	public BDDBitVector add(BDDBitVector that) {

	if (bitvec.length != that.bitvec.length)
		throw new BDDException();

	BDDFactory bdd = getFactory();

	BDD c = bdd.zero();
	BDDBitVector res = bdd.createBitVector(bitvec.length);

	for (int n = 0; n < res.bitvec.length; n++) {
		 bitvec[n] = l[n] ^ r[n] ^ c; 
		res.bitvec[n] = bitvec[n].xor(that.bitvec[n]);
		res.bitvec[n].xorWith(c.id());

		 c = (l[n] & r[n]) | (c & (l[n] | r[n])); 
		BDD tmp1 = bitvec[n].or(that.bitvec[n]);
		tmp1.andWith(c);
		BDD tmp2 = bitvec[n].and(that.bitvec[n]);
		tmp2.orWith(tmp1);
		c = tmp2;
	}
	c.free();

	return res;
}
	 */


	/*	public BDDBitVector sub(BDDBitVector that) {

	if (bitvec.length != that.bitvec.length)
		throw new BDDException();

	BDDFactory bdd = getFactory();

	BDD c = bdd.zero();
	BDDBitVector res = bdd.createBitVector(bitvec.length);

	for (int n = 0; n < res.bitvec.length; n++) {
		 bitvec[n] = l[n] ^ r[n] ^ c; 
		res.bitvec[n] = bitvec[n].xor(that.bitvec[n]);
		res.bitvec[n].xorWith(c.id());

		 c = (l[n] & r[n] & c) | (!l[n] & (r[n] | c)); 
		BDD tmp1 = that.bitvec[n].or(c);
		BDD tmp2 = this.bitvec[n].apply(tmp1, BDDFactory.less);
		tmp1.free();
		tmp1 = this.bitvec[n].and(that.bitvec[n]);
		tmp1.andWith(c);
		tmp1.orWith(tmp2);

		c = tmp1;
	}
	c.free();

	return res;
}
	 */


	/*	public BDDBitVector coerce(int bitnum) {
	BDDFactory bdd = getFactory();
	BDDBitVector dst = bdd.createBitVector(bitnum);
	int minnum = Math.min(bitnum, bitvec.length);
	int n;
	for (n = 0; n < minnum; n++)
		dst.bitvec[n] = bitvec[n].id();
	for (; n < minnum; n++)
		dst.bitvec[n] = bdd.zero();
	return dst;
}
	 */

	/*
		static void div_rec(BDDBitVector divisor,
		BDDBitVector remainder,
		BDDBitVector result,
		int step) {
	BDD isSmaller = divisor.lte(remainder);
	BDDBitVector newResult = result.shl(1, isSmaller);
	BDDFactory bdd = divisor.getFactory();
	BDDBitVector zero = bdd.buildVector(divisor.bitvec.length, false);
	BDDBitVector sub = bdd.buildVector(divisor.bitvec.length, false);

	for (int n = 0; n < divisor.bitvec.length; n++)
		sub.bitvec[n] = isSmaller.ite(divisor.bitvec[n], zero.bitvec[n]);

	BDDBitVector tmp = remainder.sub(sub);
	BDDBitVector newRemainder =
			tmp.shl(1, result.bitvec[divisor.bitvec.length - 1]);

	if (step > 1)
		div_rec(divisor, newRemainder, newResult, step - 1);

	tmp.free();
	sub.free();
	zero.free();
	isSmaller.free();

	result.replaceWith(newResult);
	remainder.replaceWith(newRemainder);
	}

	 */

	/*
	public void replaceWith(BDDBitVector that) {
	if (bitvec.length != that.bitvec.length)
		throw new BDDException();
	free();
	this.bitvec = that.bitvec;
	that.bitvec = null;
	}
	 */

	/*		public BDDBitVector divmod(long c, boolean which) {
			if (c <= 0L)
				throw new BDDException();
			BDDFactory bdd = getFactory();
			BDDBitVector divisor = bdd.constantVector(bitvec.length, c);
			BDDBitVector tmp = bdd.buildVector(bitvec.length, false);
			BDDBitVector tmpremainder = tmp.shl(1, bitvec[bitvec.length-1]);
			BDDBitVector result = this.shl(1, bdd.zero());

			BDDBitVector remainder;

			div_rec(divisor, tmpremainder, result, divisor.bitvec.length);
			remainder = tmpremainder.shr(1, bdd.zero());

			tmp.free();
			tmpremainder.free();
			divisor.free();

			if (which) {
				remainder.free();
				return result;
			} else {
				result.free();
				return remainder;
			}
		}*/
}
