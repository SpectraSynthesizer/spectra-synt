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

import java.util.List;

import tau.smlab.syntech.gameinput.model.Variable;

public class InExpSpec implements Spec {
	/**
	 * 
	 */
	private static final long serialVersionUID = -8081328673447088736L;
	
	private Spec variable; // The main var of the expression
	private boolean isNot;
	private List<String> setOfvalues;
	
	public InExpSpec(Spec variable, boolean isNot, List<String> setOfvalues) {
		this.variable = variable;
		this.setNot(isNot);
		this.setSetOfvalues(setOfvalues);
	}

	public String toString() {
		return this.getVariable().toString() + (this.isNot() ? " not " : " ") + Operator.IN.toString() + " expr : " + this.getSetOfvalues();
	}

	@Override
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

	@Override
	public Spec clone() throws CloneNotSupportedException {
		return new InExpSpec(this.variable, this.isNot(), this.getSetOfvalues());
	}

	public Spec getVariable() {
		return variable;
	}

	public void setVariable(Spec variable) {
		this.variable = variable;
	}

	public boolean isNot() {
		return isNot;
	}

	public void setNot(boolean isNot) {
		this.isNot = isNot;
	}

	public List<String> getSetOfvalues() {
		return setOfvalues;
	}

	public void setSetOfvalues(List<String> setOfvalues) {
		this.setOfvalues = setOfvalues;
	}

}
