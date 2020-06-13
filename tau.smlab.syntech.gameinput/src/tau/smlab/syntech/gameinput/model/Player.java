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
import java.util.ArrayList;
import java.util.List;

public class Player implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -5085525976634287802L;
	
	private List<Variable> vars = new ArrayList<>();
	private List<Constraint> constraints = new ArrayList<>();
	private List<PatternConstraint> patterns = new ArrayList<>();
	private List<TriggerConstraint> triggers = new ArrayList<>();
	private List<ExistentialConstraint> existentialConstraints = new ArrayList<>();


	public Player() {
		this.vars = new  ArrayList<>();
		this.constraints = new ArrayList<>();
		this.patterns = new ArrayList<>();
		this.triggers = new ArrayList<>();
		this.existentialConstraints = new ArrayList<>();
	}

	public String toString()
	{
		return "Player:[vars: " + vars + " constraints: " + constraints + "patterns: " + patterns+"]";
	}
	public List<Variable> getVars() {
		return vars;
	}

	public void setVars(List<Variable> vars) {
		this.vars = vars;
	}

	public List<Constraint> getConstraints() {
		return constraints;
	}

	public void setConstraints(List<Constraint> constraints) {
		this.constraints = constraints;
	}

	public List<PatternConstraint> getPatterns() {
		return patterns;
	}

	public void setPatterns(List<PatternConstraint> patterns) {
		this.patterns = patterns;
	}

	public void setExistentialConstraints(List<ExistentialConstraint> existentialConstraints) {
		this.existentialConstraints = existentialConstraints;
	}

	public List<ExistentialConstraint> getExistentialConstraints() {
		return existentialConstraints;
	}

	public void addVar(Variable variable)
	{
		this.vars.add(variable);
	}

	public void addConstraint(Constraint constraint)
	{
		this.constraints.add(constraint);
	}

	public void addPatternConstraint(PatternConstraint patternConstraint)
	{
		this.patterns.add(patternConstraint);
	}

	public void addExistentialConstraint(ExistentialConstraint existentialConstraint)
	{
		this.existentialConstraints.add(existentialConstraint);
	}

	public void addTrigger(TriggerConstraint trigger) {
		this.triggers.add(trigger);
	}

	public List<TriggerConstraint> getTriggers() {
		return triggers;
	}
}
