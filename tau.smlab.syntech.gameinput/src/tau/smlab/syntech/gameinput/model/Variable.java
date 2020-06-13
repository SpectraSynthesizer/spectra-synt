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

public class Variable implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -1555452952492538184L;
	
	private String name;
	private TypeDef type;
	private int traceId;
	private boolean isDomainVar; // a boolean field that is true iff the variable is a domain var.

	public Variable(String name, TypeDef type) {
		this(name, type, 0, false);
	}

	@Override
	public boolean equals(Object obj) {
		return name.equals(((Variable)obj).name);
	}

	public Variable(String name, TypeDef type, int traceId) {
		this(name, type, traceId, false);
	}

	public Variable(String name, TypeDef type, boolean isDomainVar) {
		this(name, type, 0, isDomainVar);
	}

	public Variable(String name, TypeDef type, int traceId, boolean isDomainVar) {
		this.name = name;
		this.type = type;
		this.traceId = traceId;
		this.isDomainVar = isDomainVar;
	}

	public String toString() {
		return "VariableName: " + name + " VariableType: " + type + " IsDomainVar? " + isDomainVar;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public TypeDef getType() {
		return type;
	}

	public void setType(TypeDef type) {
		this.type = type;
	}

	public int getTraceId() {
		return traceId;
	}

	public void setTraceId(int traceId) {
		this.traceId = traceId;
	}

	public boolean isDomainVar() {
		return isDomainVar;
	}

}
