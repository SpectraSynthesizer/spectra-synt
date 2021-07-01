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

package tau.smlab.syntech.gameinput.spec;

/**
 * <p>
 * All kinds of operators.
 * </p>
 * 
 * @version {@value edu.wis.jtlv.env.Env#version}
 * @author yaniv sa'ar.
 * 
 * @see edu.wis.jtlv.env.spec.SpecExp
 */
public enum Operator {
	// UNARY............
	// Prop
	NOT, PRIME,  //PRIME stands for next
	REGEXP,
	SUM_OF, AND_OF, OR_OF, PROD_OF,
	MIN, MAX,
	// LTL
	HISTORICALLY, ONCE, PREV,

	// BINARY............
	
	IN,
	// Prop
	AND, OR, XOR, IFF, IMPLIES, EQUALS,LEFT_BIGGER, LEFT_BIGGER_OR_EQUALS, RIGHT_BIGGER, RIGHT_BIGGER_OR_EQUALS, ADD, SUBSTRACT,
	MULTIPLY, DIVIDE, MOD,
	// PAST LTL
	SINCE, TRIGGERED,
	
	// FUTURE LTL
	FINALLY, GLOBALLY,
	
    // Quantifiers
    FORALL, EXISTS;

	
	// Extra Prop - Binary
	// EQ, NEQ, LT, GT, LE, GE, SETIN, UNION, LSHIFT, RSHIFT, MOD, PLUS, MINUS,
	// TIMES, DIVIDE, CONCATENATION;

	// ////////////////////////////////////////////////////////////////////////
	// Groupings //////////////////////////////////////////////////////////////
	// number of operands...
	public static final Operator[] unaryOp = { NOT, PRIME,
			HISTORICALLY, ONCE, PREV, FINALLY, GLOBALLY};
	public static final Operator[] binaryOp = { AND, OR, XOR, IFF,
			IMPLIES,SINCE, TRIGGERED, EQUALS, LEFT_BIGGER, LEFT_BIGGER_OR_EQUALS, RIGHT_BIGGER, RIGHT_BIGGER_OR_EQUALS, ADD, SUBSTRACT, MULTIPLY, DIVIDE, MOD};
	//inequality operands
	public static final Operator[] inqOp = {LEFT_BIGGER, LEFT_BIGGER_OR_EQUALS, RIGHT_BIGGER, RIGHT_BIGGER_OR_EQUALS};
	
	//arithmetic operands
	public static final Operator[] arithOp = {ADD, SUBSTRACT, MULTIPLY, DIVIDE, MOD};
	
	public static final Operator[] linearArithOp = {ADD, SUBSTRACT};
	
	// is it propositional, or TL operator.
	public static final Operator[] propOp = { NOT, AND, OR, XOR, IFF,
			IMPLIES, EQUALS, LEFT_BIGGER, LEFT_BIGGER_OR_EQUALS, RIGHT_BIGGER, RIGHT_BIGGER_OR_EQUALS};
	public static final Operator[] PastLTLOp = { HISTORICALLY,
			ONCE, PREV, SINCE, TRIGGERED };
	
    // quantifying operators
    public static final Operator[] quantifierOp = {FORALL, EXISTS};
	
	private boolean in(Operator[] set) {
		for (Operator op : set)
			if (this == op)
				return true;
		return false;
	}

	// ////////////////////////////////////////////////////////////////////////
	/**
	 * <p>Is this a linear arithmetic operator.</p>
	 * 
	 * @return true, if this is a linear arithmetic operator.
	 */
	public boolean isLinearArithmetic() {
		return this.in(linearArithOp);
	}
	/**
	 * <p>Is this an arithmetic operator.</p>
	 * 
	 * @return true, if this is an arithmetic operator.
	 */
	public boolean isArithmetic() {
		return this.in(arithOp);
	}
	/**
	 * <p>Is this an inequality operator.</p>
	 * 
	 * @return true, if this is an inequality operator.
	 */
	public boolean isInequality() {
		return this.in(inqOp);
	}
	/**
	 * <p>
	 * Is this a first order operator.
	 * </p>
	 * 
	 * @return true, if this a first order operator.
	 */
	public boolean isProp() {
		return this.in(propOp);
	}

	/**
	 * <p>
	 * Is this a LTL operator.
	 * </p>
	 * 
	 * @return true, if this a LTL operator.
	 */
	public boolean isLTLOp() {
		return this.in(PastLTLOp) || this == GLOBALLY || this == FINALLY || this == PRIME;
	}

	/**
	 * <p>
	 * Is this a Past LTL operator.
	 * </p>
	 * 
	 * @return true, if this a Past LTL operator.
	 */
	public boolean isPastLTLOp() {
		return this.in(PastLTLOp);
	}

	/**
	 * <p>
	 * Is this a Temporal operator.
	 * </p>
	 * 
	 * @return true, if this a Temporal operator.
	 */
	public boolean isTemporalOp() {
		return this.in(PastLTLOp);
	}

	/**
	 * <p>
	 * Is this an unary operator.
	 * </p>
	 * 
	 * @return true, if this an unary operator.
	 */
	public boolean isUnary() {
		return this.in(unaryOp);
	}

	/**
	 * <p>
	 * Is this a binary operator.
	 * </p>
	 * 
	 * @return true, if this a binary operator.
	 */
	public boolean isBinary() {
		return this.in(binaryOp);
	}
	
    /**
     * <p>
     * Is this an quantifying operator.
     * </p>
     * 
     * @return true, if this a quantifying operator.
     */
    public boolean isQuantifierOp() {
        return this.in(quantifierOp);
    }

	/**
	 * <p>
	 * Getter for the number of operands to this operator.
	 * </p>
	 * 
	 * @return The number of operands to this operator.
	 */
	public int numOfOperands() {
		if (this.in(unaryOp))
			return 1;
		if (this.in(binaryOp))
			return 2;
		return -1;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Enum#toString()
	 */
	public String toString() {
		return super.toString();
	}

	/**
	 * <p>
	 * Parse an operator from a string.
	 * </p>
	 * 
	 * @param op_str
	 *            The operator string.
	 * @return The operator object.
	 */
	public static Operator operatorFromString(String op_str) {
		for (Operator op : unaryOp)
			if (op.toString() == op_str)
				return op;
		for (Operator op : binaryOp)
			if (op.toString() == op_str)
				return op;
		return null;
	}
}
