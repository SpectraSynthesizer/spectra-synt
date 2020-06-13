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
 * Specification expression.
 * </p>
 * 
 * @author Yaniv Sa'ar.
 * 
 */
public class SpecExp implements Spec {
	/**
	 * 
	 */
	private static final long serialVersionUID = -6833233099417793276L;
	
	private Operator theOp;
	private Spec[] elements;

	/**
	 * <p>
	 * Constructor for an unary specification.
	 * </p>
	 * 
	 * @param op
	 *          The operator.
	 * @param e1
	 *          The sub specification.
	 */
	public SpecExp(Operator op, Spec e1) {
		this.theOp = op;
		this.elements = new Spec[] { e1 };
	}

	public SpecExp(Operator op, SpecWrapper e1) {
		this(op, e1.getSpec());
	}

	/**
	 * <p>
	 * Constructor for a binary specification.
	 * </p>
	 * 
	 * @param op
	 *          The operator.
	 * @param e1
	 *          The first sub specification.
	 * @param e2
	 *          The second sub specification.
	 */
	public SpecExp(Operator op, Spec e1, Spec e2) {
		this.theOp = op;
		this.elements = new Spec[] { e1, e2 };
	}

	public SpecExp(Operator op, SpecWrapper e1, SpecWrapper e2) {
		this(op, e1.getSpec(), e2.getSpec());
	}

	public String toString() {
		if (this.theOp.isUnary()) {
			return theOp.toString() + "(" + elements[0].toString() + ")";
		} else {
			return elements[0].toString() + " " + theOp.toString() + " " + elements[1].toString();
		}
	}

	/**
	 * <p>
	 * The operator representing this node.
	 * </p>
	 * 
	 * @return An operator representing this node.
	 */
	public Operator getOperator() {
		return theOp;
	};

	/**
	 * <p>
	 * Get the children specification of this node.
	 * <p>
	 * 
	 * @return The children specification.
	 */
	public Spec[] getChildren() {
		return this.elements;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see edu.wis.jtlv.env.spec.Spec#isPastLTLSpec()
	 */
	public boolean isPastLTLSpec() {
		// checking that all children are prop.
		for (Spec s : this.getChildren()) {
			if (s.isPastLTLSpec()) {
				return true;
			}
		}
		// checking that I'm prop or pastLTL
		return this.getOperator().isPastLTLOp();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see edu.wis.jtlv.env.spec.Spec#isPropSpec()
	 */
	public boolean isPropSpec() {
		// checking that all children are prop.
		for (Spec s : this.getChildren())
			if (!s.isPropSpec())
				return false;
		// checking that I'm prop
		return this.getOperator().isProp();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see edu.wis.jtlv.env.spec.Spec#hasTemporalOperators()
	 */
	public boolean hasTemporalOperators() {
		// if one of my elements is temporal.
		for (Spec s : this.getChildren())
			if (s.hasTemporalOperators())
				return true;
		// or I'm temporal.
		return this.theOp.isTemporalOp();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object other) {
		if (!(other instanceof SpecExp))
			return false;
		SpecExp otherExp = (SpecExp) other;
		if (this.getOperator() != otherExp.getOperator())
			return false;

		Spec[] this_children = this.getChildren();
		Spec[] other_children = otherExp.getChildren();
		if (this_children.length != other_children.length)
			return false;

		for (int i = 0; i < this_children.length; i++)
			if (!this_children[i].equals(other_children[i]))
				return false;

		return true;
	}

	public SpecExp clone() throws CloneNotSupportedException {
		int elementsSize = this.elements.length;
		if (elementsSize == 1) {
			return new SpecExp(this.theOp, this.elements[0].clone());
		} else {
			return new SpecExp(this.theOp, this.elements[0].clone(), this.elements[1].clone());
		}
	}
}
