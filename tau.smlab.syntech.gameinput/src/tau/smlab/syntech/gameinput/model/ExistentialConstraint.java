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

package tau.smlab.syntech.gameinput.model;

import java.util.ArrayList;
import java.util.List;

import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;
/**
 * 
 * @author Or Pistiner
 *
 */
public class ExistentialConstraint extends Constraint {
	/**
	 * 
	 */
	private static final long serialVersionUID = -336461559302985367L;

	private List<Spec> specs;
	private boolean isRegExp; //Whether this existential constraint is of the form 'GEF regExp'
	private SpecRegExp regExp; //The regular expression of this existential constraint

	/**
	 * 
	 * Returns a new existential constraint of the form 'GE (F(g1 and F (g2 and F (g3 and ..))))'
	 * where each gi is a {@link tau.smlab.syntech.gameinput.spec.Spec} object (represents an assertion). The returned
	 * existential constraint has an empty list of assertions gi.
	 * 
	 * @param name
	 * @param traceId
	 */
	public ExistentialConstraint(String name, int traceId) {
		super(Kind.EXISTS, null, name, traceId);
		this.isRegExp = false;
		this.specs = new ArrayList<>();
	}

	/**
	 * 
	 * Return a new existential constraint of the form 'GEF {@code regExp}'.
	 * 
	 * @param name
	 * @param regExp the regular expression
	 * @param traceId
	 */
	public ExistentialConstraint(String name, SpecRegExp regExp, int traceId) {
		super(Kind.EXISTS, null, name, traceId);
		this.isRegExp = true;
		this.regExp = regExp;
	}

	public boolean isRegExp() {
		return this.isRegExp;
	}


	@Override
	public Kind getKind() {
		return Kind.EXISTS;
	}

	public List<Spec> getSpecs() {
		return this.specs;
	}

	public int getSize() {
		return isRegExp ? 0 : this.specs.size();
	}

	public SpecRegExp getRegExp() {
		return this.regExp;
	}

	/**
	 * 
	 * Adds the specified {@code spec} to the list of existential assertions.
	 * 
	 * @param spec the existential assertion to add
	 * @return true if {@code spec} has been successfully added to the list
	 */
	public boolean addSpec(Spec spec) {
		if(!isRegExp) {
			return this.specs.add(spec);
		}
		return false;
	}

	/**
	 * 
	 * Returns the spec at the specified position in the list of existential assertions.
	 * If the specified position is out of bounds, {@code null} is returned.
	 * 
	 * @param specIdx the position
	 * @return
	 */
	public Spec getSpec(int specIdx) {
		if(!isRegExp && specIdx >= 0 && specIdx < specs.size()) {
			return this.specs.get(specIdx);
		}
		return null;
	}

	/**
	 * 
	 * Replaces the spec at the specified position in the list of existential assertions with the specified spec.
	 *
	 * @param specIdx the position
	 * @param spec the spec
	 * @return the spec previously at the specified position or {@code null} if the specified position is out of bounds
	 */
	public Spec replaceSpec(int specIdx, Spec spec) {
		if(!isRegExp && specIdx >= 0 && specIdx < specs.size()) {
			return this.specs.set(specIdx, spec);
		}
		return null;
	}

	/**
	 * 
	 * Replaces the regular expression of this existential constraint with the specified one.
	 *  
	 * @param regExp the new regular expression
	 * @return the previous regular expression
	 */
	public SpecRegExp replaceRegExp(SpecRegExp regExp) {
		if(isRegExp) {
			SpecRegExp replacedRegExp = this.regExp;
			this.regExp = regExp;
			return replacedRegExp;
		}
		return null;
	}
}
