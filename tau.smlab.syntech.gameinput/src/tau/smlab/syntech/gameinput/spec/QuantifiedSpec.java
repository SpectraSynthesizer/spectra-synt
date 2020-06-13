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

import tau.smlab.syntech.gameinput.model.Variable;

//This class implements a Spec that represents a QuantifierExpr
public class QuantifiedSpec implements Spec {
	/**
	 * 
	 */
	private static final long serialVersionUID = -355517893664017916L;
	
	Operator op; // The operator of the quantifier expression (forall or exists)
	private Variable domainVar; // The domain var of the quantifier expression
	private Spec tmpExpr; // The inner expression of the quantifier expression

	public QuantifiedSpec(Operator op, Variable domainVar, Spec tmpExpr) {
		this.op = op;
		this.domainVar = domainVar;
		this.tmpExpr = tmpExpr;
	}

	public Operator getOperator() {
		return this.op;
	}

	public void setOperator(Operator op) {
		this.op = op;
	}

	public Operator getExprOperator() {
		return (this.getOperator() == Operator.FORALL) ? Operator.AND : Operator.OR;
	}

	public Variable getDomainVar() {
		return this.domainVar;
	}

	public void setDomainVar(Variable domainVar) {
		this.domainVar = domainVar;
	}

	public Spec getTempExpr() {
		return this.tmpExpr;
	}

	public void setTempExpr(Spec tmpExpr) {
		this.tmpExpr = tmpExpr;
	}

	public String toString() {
		return this.op.toString() + " " + this.domainVar.toString() + " expr : " + this.tmpExpr.toString();
	}

	public boolean isPastLTLSpec() {
		return false;
	}

	@Override
	public boolean isPropSpec() {
		return false;
	}

	@Override
	public boolean hasTemporalOperators() {
		return false;
	}

	public QuantifiedSpec clone() throws CloneNotSupportedException {
		return new QuantifiedSpec(this.op, this.domainVar, this.tmpExpr);
	}
}
