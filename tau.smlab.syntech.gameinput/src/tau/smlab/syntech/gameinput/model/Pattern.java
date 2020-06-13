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

import java.io.Serializable;
import java.util.List;

public class Pattern implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1232427056171750765L;
	
	private int traceId;
	private String patternName;
	private List<Constraint> expressions;
	private List<Variable> paramsList;
	private List<Variable> varsList;

	/**
	 * 
	 * @param patternName
	 * @param expressions
	 * @param varsList
	 * @param paramsList must be boolean
	 * @param traceId
	 */
	public Pattern(String patternName, List<Constraint> expressions, List<Variable> varsList, List<Variable> paramsList, int traceId)
	{
		this.patternName = patternName;
		this.expressions = expressions;
		this.varsList =  varsList;
		this.paramsList = paramsList;
		this.traceId = traceId;
	}

	public int getTraceId() {
		return traceId;
	}

	public String getPatternName() {
		return patternName;
	}

	public List<Constraint> getExpressions() {
		return expressions;
	}

	public List<Variable> getParamsList() {
		return paramsList;
	}

	public List<Variable> getVarsList() {
		return varsList;
	}


}
