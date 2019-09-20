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
/**
 *<p>Algebraic Decision Diagrams (ADDs) {@code Interface}</p>.
 */
public interface ADD extends BDD {

	@Override
	public ADD id();

	@Override
	public ADD replace(BDDPairing pair);

	@Override
	public ADD replaceWith(BDDPairing pair);
	/**
	 * {@inheritDoc}
	 * <p>If this is a non 0-1 ADD, it applies addition instead of logical 'or' operation (as is applied to BDDs).</p> 
	 */
	@Override
	public ADD exist(BDDVarSet var); 
	/**
	 * {@inheritDoc}
	 * <p>If this is an ADD, it applies multiplication instead of logical 'and' operation (as is applied to BDDs).</p> 
	 */
	@Override
	public ADD forAll(BDDVarSet var);

	@Override
	public ADD apply(BDD that, BDDFactory.BDDOp opr);

	@Override
	public ADD applyWith(BDD that, BDDFactory.BDDOp opr);

	@Override
	public ADD low();

	@Override
	public ADD high();
	/**<p> Returns true if this is a constant ADD.<p>
	 * 
	 * @return true if this is a constant ADD 
	 */
	public boolean isConstantADD();
	/**<p> If this ADD is a constant ADD, then its value is returned. Otherwise, NaN is returned.<p>
	 * 
	 * @return the value of this constant ADD
	 */
	public double getConstantValue();
	/**<p> Finds the maximum terminal (discriminant) of this ADD.<p>
	 * 
	 * @return the maximum constant ADD of this ADD 
	 */
	public ADD findMax(); 
	/**<p> Finds the minimum terminal (discriminant) of this ADD.<p>
	 * 
	 * @return the minimum constant ADD of this ADD 
	 */
	public ADD findMin(); 
	/**
	 * <p>Applies the natural logarithm to all the terminal nodes (the discriminant) of this ADD </p>
	 * 
	 * @return the natural logarithm of this ADD
	 */
	public ADD applyLog(); 
	/**
	 * <p>Returns true if this is a zero-one ADD, 
	 * i.e. each of its terminal nodes (discriminants) is zero or one.</p>
	 * 
	 * @return true if this ADD is a zero-one ADD
	 */
	public boolean isZeroOneADD(); 
	/**
	 * <p>Negates this ADD by exchanging all references to a constant with value v with
	 * references to a constant whose value is the additive inverse (-v).</p>
	 * 
	 * <p>Compare to add_not.</p>
	 * 
	 * @return the additive inverse (negated) ADD
	 */
	public ADD additiveNot();
	/**
	 * <p> Computes the complement ADD: the complement of 0 is 1 and the complement of everything else is 0.</p>
	 * 
	 *@return the complement ADD 
	 */
	@Override
	public ADD not();
	/**
	 * <p>if-then-else operator. This method assumes that this ADD is a 0-1 ADD.
	 * <br>Computes: (this * then) + (not(this) * else)</p>
	 * 
	 *@return the result of the if-then-else operator on this 0-1 ADD and the other two ADDs 
	 */
	@Override
	public ADD ite(BDD thenBDD, BDD elseBDD);
	/**
	 * {@inheritDoc}
	 * <p>This method assumes that ADD (i.e. g) is a 0-1 ADD.</p>
	 */
	@Override
	public ADD compose(BDD that, int var);
	/**
	 * {@inheritDoc}
	 * <p>Each pair must consist of a variable and a 0-1 ADD; Otherwise, the result is undefined.</p>
	 */
	@Override
	public ADD veccompose(BDDPairing pair);
	/**
	 * <p>Returns true if this ADD is the arithmetic zero ADD.</p>
	 * 
	 * @return true if this ADD is the (arithmetic) zero ADD
	 */
	@Override
	public boolean isZero();
	/**
	 * <p>Converts an ADD to a BDD by replacing all discriminants (terminal nodes) different from 0 with 1.</p>
	 *  
	 * @return the converted BDD
	 */
	public BDD toBDD();
	/**
	 * <p>Converts this ADD to a BDD by replacing all
	 * discriminants greater than or equal to threshold with 1, and all other
	 * discriminants with 0.</p>
	 * 
	 * @param threshold the value of the threshold
	 * @return the converted BDD by the given threshold
	 */
	public BDD toBDDByThreshold(double threshold);  
	/**
	 * <p>Converts this ADD to a BDD by replacing all
	 * discriminants STRICTLY greater than threshold with 1, and all other
	 * discriminants with 0.</p>
	 * 
	 * @param threshold the value of the threshold
	 * @return the STRICTLY converted BDD by the given threshold
	 */  
	public BDD toBDDByStrictThreshold(double threshold);  
	/**
	 * <p>Converts this ADD to a BDD by replacing all
	 * discriminants greater than or equal to lower and less than or equal to
	 * upper with 1, and all other discriminants with 0.</p>
	 * 
	 * @param upper the upper endpoint of the interval
	 * @param lower the upper endpoint of the interval
	 * @return the STRICTLY converted BDD by the given threshold
	 */  
	public BDD toBDDByInterval(double lower, double upper);  
	/**
	 * <p>Converts this ADD to a BDD by replacing all
	 * discriminants whose i'th bit is equal to 1 with 1, and all other
	 * discriminants with 0.</p><p>The i'th bit refers to the integer
	 * representation of the leaf value. If the value (which is of type double) has a fractional
	 * part, it is ignored.</p><p>Repeated calls to this procedure allow one to
	 * transform an integer-valued ADD into an array of BDDs, one for each
	 * bit of the leaf values.</p>
	 * 
	 * @param bit the i'th bit 
	 * @return the converted BDD by the i'th bit
	 */
	public BDD toBDDByIthBit(int bit);
	/**
	 * <p>Returns the logical 'and' of two 0-1 ADDs, or the arithmetic multiplication of two non 0-1 ADDs. This is a shortcut for calling
	 * "apply" with the "and" operator.</p>
	 * 
	 * <p>Compare to bdd_and.</p>
	 * 
	 * @param that BDD to 'and' with
	 * @return the logical 'and' of two BDDs
	 */
	public ADD and(ADD that);
	/**
	 * <p>Returns the logical 'or' of two 0-1 ADDs, or the arithmetic addition of two non 0-1 ADDs. This is a shortcut for calling
	 * "apply" with the "or" \ "plus" operator.</p>
	 * 
	 * <p>Compare to bdd_or.</p>
	 * 
	 * @param that the BDD to 'or' with
	 * @return the logical 'or' of two BDDs
	 */
	public ADD or(ADD that);
	/**
	 * <p>Makes this ADD be the logical 'and' of two 0-1 ADDs, or the multiplication of two non 0-1 ADDs.  The "that" ADD is
	 * consumed, and can no longer be used.  This is a shortcut for calling
	 * "applyWith" with the "and" operator.</p>
	 * 
	 * <p>Compare to bdd_and and bdd_delref.</p>
	 * 
	 * @param that the BDD to 'and' with
	 */
	public ADD andWith(ADD that);
  /**
   * <p>
   * Finds one satisfying variable assignment of this 0-1 ADD which restricts each variable of the set <tt>var</tt>.
   * The only variables mentioned in the result are those of <tt>var</tt>. All other variables would be undefined, i.e., don't
   * cares, in the resulting ADD.</p>
   * 
   * <h4>Note:</h4><p>This ADD is assumed to be a 0-1 ADD. In case this is not a 0-1 ADD, the behavior of this method is undefined.
   * </p>
   * 
   * @param var
   *          BDDVarSet the set of variables that are mentioned in the result
   * @return one satisfying variable assignment
   * @see net.sf.javabdd.BDDDomain#set()
   */
  public ADD satOne(BDDVarSet var);
	/**
	 * <p>Coudert and Madre's restrict function.  Tries to simplify the ADD f by
	 * restricting it to the domain covered by d.  No checks are done to see if
	 * the result is actually smaller than the input.  This can be done by the
	 * user with a call to nodeCount().</p>
	 * 
	 * <p>Compare to bdd_simplify.</p>
	 * 
	 * @param d BDDVarSet containing the variables in the domain
	 * @return the result of the simplify operation
	 */
	public ADD simplify(BDDVarSet d);
	/**
	 * <p>Turns the given value's terminal node into a MINUS INFINITY terminal node, 
	 * <br>i.e. makes the terminal node that has the given value, the minimum terminal node of this ADD.<br>
	 * If no terminal node has the given value, then the same ADD (as this) is returned.</p>
	 * 
	 * @param value the value whose terminal node (if exists) becomes MINUS INFINITY
	 * @return the resulting ADD
	 */
	public ADD minimizeValue(double value);
	/**
	 * <p>Returns the abstraction of this ADD by the given variable set. Each discriminant of the returned ADD is the result 
	 * of applying the MINIMUM operator over the discriminants of all the minterms of the variable set.</p>
	 * 
	 * <p>This method can be used instead of All operator for ADDs (i.e. universal quantification)</p>
	 * 
	 * 
	 * @param d the variable set to abstract
	 * @return the abstracted ADD
	 */
	public ADD abstractMin(BDDVarSet d);
	/**
	 * <p>Returns the abstraction of this ADD by the given variable set. Each discriminant of the returned ADD is the result 
	 * of applying the MAXIMUM operator over the discriminants of all the minterms of the variable set.  
	 * <p>This method can be used instead of Exists operator for ADDs (i.e. existential quantification)</p>  
	 *
	 * @param d the variable set to abstract
	 * @return the abstracted ADD
	 */
	public ADD abstractMax(BDDVarSet d);
	/**
	 * <p>Returns the logical 'or' of two 0-1 ADDs, or the arithmetic addition of two non 0-1 ADDs. The "that" ADD is
	 * consumed, and can no longer be used.  This is a shortcut for calling
	 * "applyWith" with the "or"\"plus" operator.</p>
	 * 
	 * <p>Compare to bdd_or.</p>
	 * 
	 * @param that the ADD to 'or' with
	 * @return the 'or' of two ADDs
	 */
	public ADD orWith(ADD that);
	/**
	 * <p>Returns the engMinus of this ADD and that ADD (i.e. this - that), s.t. if x+y > maxEnergy, then x+y = +INF, for all finite x and y values.</p>
	 * <p>This is not the same as calling "applyWith" with the "engMinus" operator, due to the absence of maxEnergy bound.</p>
	 * 
	 * @param that the ADD to 'engMinus' with
	 * @param maxEnergy the maximum energy bound
	 * @return the 'engMinus' of two ADDs constrained with the maxEnergy bound
	 */
	public ADD engSubtract(ADD that, double maxEnergy);
	/**
	 * <p>Converts this ADD into a 0-1 ADD by threshold, such that if the terminal node's value is *strictly less then* threshold, then it become 1; Otherwise,
	 * it becomes 0.</p>  
	 * 
	 * @param threshold the threshold value
	 * @return the converted 0-1 ADD
	 */
	public ADD toZeroOneSLTThreshold(double threshold);
	/**
	 * <p>Converts this ADD into a 0-1 ADD by threshold, such that if the terminal node's value is *strictly greater then* threshold, then it become 1; Otherwise,
	 * it becomes 0.</p>  
	 * 
	 * @param threshold the threshold value
	 * @return the converted 0-1 ADD
	 */
	public ADD toZeroOneSGTThreshold(double threshold);
	/**
	 * <p>Converts this ADD into a 0-1 ADD by threshold, such that if the terminal node's value is *less then* threshold, then it become 1; Otherwise,
	 * it becomes 0.</p>  
	 * 
	 * @param threshold the threshold value
	 * @return the converted 0-1 ADD
	 */
	public ADD toZeroOneLTThreshold(double threshold);
	/**
	 * <p>Converts this ADD into a 0-1 ADD by threshold, such that if the terminal node's value is *greater then* threshold, then it become 1; Otherwise,
	 * it becomes 0.</p>  
	 * 
	 * @param threshold the threshold value
	 * @return the converted 0-1 ADD
	 */
	public ADD toZeroOneGTThreshold(double threshold);
	/**
	 * <p>Prints all the terminal node's values of this ADD</p>  
	 *
	 */
	public void printTerminalValues();
	/**
	 * <p>Returns the abstraction of this ADD by the given variable set. Each discriminant of the returned ADD is the result 
	 * of applying the addition operator over the discriminants of all the minterms of the variable set.  
	 * <p>This method can be used instead of Exists operator for ADDs (i.e. existential quantification)</p>  
	 *
	 * @param d the variable set to abstract
	 * @return the abstracted ADD
	 */
	public ADD abstractSum(BDDVarSet d);
	/**
	 * <p>Restrict a set of variables to constant values.  Restricts the variables
	 * in this ADD to constant 1 if they are included in their positive form
	 * in var, and constant 0 if they are included in their negative form.</p>
	 * 
	 * <p><i>Note that this is quite different than Coudert and Madre's restrict
	 * function.</i></p>
	 * 
	 * <p>Compare to bdd_restrict.</p>
	 * 
	 * @param var ADD containing the variables to be restricted
	 * @return the result of the restrict operation
	 * @see net.sf.javabdd.ADD#simplify(BDDVarSet)
	 */
	public ADD restrict(ADD var);
	/**
	 * <p>Mutates this ADD to restrict a set of variables to constant values.
	 * Restricts the variables in this ADD to constant 1 if they are included
	 * in their positive form in that, and constant 0 if they are included in
	 * their negative form.  The "that" ADD is consumed, and can no longer be used.</p>
	 * 
	 * <p><i>Note that this is quite different than Coudert and Madre's restrict
	 * function.</i></p>
	 * 
	 * <p>Compare to add_restrict and bdd_delref.</p>
	 * 
	 * @param that ADD containing the variables to be restricted
	 */
	public ADD restrictWith(ADD that);
	/**
	 * <p>Returns a deterministic version of this 0-1 ADD. It is assumed that this ADD represents a controller or a strategy.<br>
	 * The controller is deterministic such that for every combination of variable assignments that are not in d, there is at most one assignment of variables in d 
	 * that goes to 1 constant, i.e there is at most one valid choice for the module that controls the variables that appear in d.</p>
	 * 
	 * @param d ADD containing the variables of the module whose choices should be deterministic
	 * @return A deterministic version of this controller
	 */
	public ADD determinizeController(BDDVarSet d);
	/**
	 * <p>Return an array that contains all the terminal node's values of this ADD</p>  
	 *
	 */
	public double[] getTerminalValues();
}