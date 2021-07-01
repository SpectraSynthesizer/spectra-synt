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

import java.util.ArrayList;
import java.util.List;

import tau.smlab.syntech.gameinput.model.Predicate;

public class PredicateInstance implements Spec {
	/**
	 * 
	 */
	private static final long serialVersionUID = -2518428787086751295L;
	
	private Predicate predicate;
	private List<Spec> parameters;

	public PredicateInstance(Predicate predicate, List<Spec> parameters)
	{
		this.predicate = predicate;
		this.parameters = parameters;
	}

	public String toString()
	{
		return "<PredicateInstance: " + this.predicate.getPredicateName() + ">";
	}

	public Predicate getPredicate() {
		return predicate;
	}

	public void setPredicate(Predicate predicate) {
		this.predicate = predicate;
	}

	public List<Spec> getParameters() {
		return parameters;
	}

	public void setParameters(List<Spec> parameters) {
		this.parameters = parameters;
	}

	@Override
	public boolean isPastLTLSpec() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean isPropSpec() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean hasTemporalOperators() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public PredicateInstance clone() throws CloneNotSupportedException {
		List<Spec> newParameters = new ArrayList<>(this.parameters);
		return new PredicateInstance(this.predicate, newParameters);
	}
}
