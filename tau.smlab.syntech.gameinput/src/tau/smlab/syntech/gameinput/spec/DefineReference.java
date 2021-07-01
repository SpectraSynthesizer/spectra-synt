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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import tau.smlab.syntech.gameinput.model.Define;
import tau.smlab.syntech.gameinput.model.Variable;

public class DefineReference implements Spec {
	/**
	 * 
	 */
	private static final long serialVersionUID = 7153031562445721013L;
	
	private Define define;
	private Map<String, Variable> indexVars = new HashMap<>(); // a set of complex (non-Integer) indexes, like vars and domain vars
	private List<Spec> indexSpecs;
	private List<Integer> indexDimensions;
	
	public DefineReference(Define define) {
		this.define = define;
	}
	
	public DefineReference(Define define, Map<String, Variable> indexVars, List<Spec> indexSpecs, List<Integer> indexDimensions) {
		this.define = define;
		this.indexVars = indexVars;
		this.indexSpecs = indexSpecs;
		this.indexDimensions = indexDimensions;
	}

	public Map<String, Variable> getIndexVars() {
		return indexVars;
	}

	public void setIndexVars(Map<String, Variable> indexVars) {
		this.indexVars = indexVars;
	}

	public List<Spec> getIndexSpecs() {
		return indexSpecs;
	}

	public void setIndexSpecs(List<Spec> indexSpecs) {
		this.indexSpecs = indexSpecs;
	}

	public List<Integer> getIndexDimensions() {
		return indexDimensions;
	}

	public void setIndexDimension(List<Integer> indexDimensions) {
		this.indexDimensions = indexDimensions;
	}

	public String toString()
	{
		return "<DefineReference: " + this.define.getName() +">";
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

	public Define getDefine() {
		return define;
	}

	public void setDefine(Define define) {
		this.define = define;
	}
	
	public DefineReference clone() throws CloneNotSupportedException {
		Map<String, Variable> indexVars = (this.indexVars == null) ? null : new HashMap<String, Variable>(this.indexVars); // SHALLOW COPY
		ArrayList<Spec> indexSpecs; // DEEP COPY
		if (this.indexSpecs == null) {
			indexSpecs = null;
		} else {
			indexSpecs = new ArrayList<Spec>();
			for (Spec indexSpec : this.indexSpecs) {
				indexSpecs.add(indexSpec.clone());
			}
		}
		ArrayList<Integer> indexDimensions = (this.indexDimensions == null) ? null : new ArrayList<Integer>(this.indexDimensions); // SHALLOW COPY
		return new DefineReference(this.define, indexVars, indexSpecs, indexDimensions);
	}

}
