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

import tau.smlab.syntech.gameinput.spec.SpecTraceable;

public class Counter implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 2420031291078508480L;

	public enum OverFlowMethod {
		FALSE, KEEP, MODULO
	};

	// FALSE = Assumption to not exceed bounds.
	// KEEP = Keep Min/ Max value when trying to exceed bounds.
	// MODULO = Modulo(Max value)

	private int traceId;

	private String name;
	private SpecTraceable iniPred;
	private SpecTraceable incPred;
	private SpecTraceable decPred;
	private SpecTraceable resetPred;
	private OverFlowMethod overFlowMethod;
	private OverFlowMethod underFlowMethod;
	private int lower;
	private int upper;

	/**
	 * @param traceId
	 * @param name
	 * @param incPred
	 * @param decPred
	 * @param resetPred
	 * @param overFlowMethod
	 * @param range
	 * @param iniPred
	 */
	public Counter(int traceId, String name, SpecTraceable incPred, SpecTraceable decPred,
			SpecTraceable resetPred, OverFlowMethod overFlowMethod, OverFlowMethod underFlowMethod,
			int lower, int upper, SpecTraceable iniPred) {
		super();
		this.traceId = traceId;
		this.name = name;
		this.iniPred = iniPred;
		this.incPred = incPred;
		this.decPred = decPred;
		this.resetPred = resetPred;
		this.overFlowMethod = overFlowMethod;
		this.underFlowMethod = underFlowMethod;
		this.lower = lower;
		this.upper = upper;
	}

	public Counter(int traceId, String name) {
		this.name = name;
		this.traceId = traceId;
	}

	public void setTraceId(int traceId) {
		this.traceId = traceId;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setIncPred(SpecTraceable incPredicate) {
		this.incPred = incPredicate;
	}

	public void setDecPred(SpecTraceable decPredicate) {
		this.decPred = decPredicate;
	}

	public void setResetPred(SpecTraceable resetPredicate) {
		this.resetPred = resetPredicate;
	}

	public void setOverFlowMethod(OverFlowMethod overFlowMethod) {
		this.overFlowMethod = overFlowMethod;
	}

	public void setUnderFlowMethod(OverFlowMethod underFlowMethod) {
		this.underFlowMethod = underFlowMethod;
	}

	public void setLower(int lower) {
		this.lower = lower;
	}

	public void setUpper(int upper) {
		this.upper = upper;
	}

	public void setIniPred(SpecTraceable initialConstraint) {
		this.iniPred = initialConstraint;
	}

	public int getTraceId() {
		return traceId;
	}

	public String getName() {
		return name;
	}

	public SpecTraceable getIncPred() {
		return incPred;
	}

	public SpecTraceable getResetPred() {
		return resetPred;
	}

	public SpecTraceable getDecPred() {
		return decPred;
	}

	public int getLower() {
		return lower;
	}

	public int getUpper() {
		return upper;
	}

	public OverFlowMethod getOverFlowMethod() {
		return overFlowMethod;
	}

	public OverFlowMethod getUnderFlowMethod() {
		return underFlowMethod;
	}

	public SpecTraceable getIniPred() {
		return iniPred;
	}
}
