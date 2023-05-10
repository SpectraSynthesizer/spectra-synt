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

/**
 * represents the input for a specification
 *
 */
public class GameInput implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = -3356883533820650863L;

	private String name;

	private Player sys;
	private Player env;
	private Player aux;

	private List<Define> defines = new ArrayList<>();
	private List<Predicate> predicates = new ArrayList<>();
	private List<Pattern> patterns = new ArrayList<>();
	private List<Monitor> monitors = new ArrayList<>();
	private List<Counter> counters = new ArrayList<>();
	private List<WeightDefinition> weightDefs = new ArrayList<>();
	private List<TriggerConstraint> triggers = new ArrayList<>();
	private List<Variable> domainVars = new ArrayList<>();
	private List<RegexpTestModel> regtestExpressions = new ArrayList<>();
	private int bound;

	public GameInput(String name)
	{
		this.name = name;
	}

	public GameInput(String name, Player sysPlayer, Player envPlayer, Player auxPlayer)
	{
		this.name = name;
		this.sys = sysPlayer;
		this.env = envPlayer;
		this.aux = auxPlayer;
	}

	public String toString()
	{
		return "GameInput:[name: " + name + ", sys: " + sys + ", env: " + env + ", aux: " + aux + ", defines: " + defines + ", predicates: " + predicates + ", patterns: " + patterns +"]";
	}

	public Player getSys() {
		return sys;
	}

	public void setSys(Player sys) {
		this.sys = sys;
	}

	public Player getEnv() {
		return env;
	}

	public void setEnv(Player env) {
		this.env = env;
	}

	public Player getAux() {
		return aux;
	}

	public void setAux(Player aux) {
		this.aux = aux;
	}

	public List<Define> getDefines() {
		return defines;
	}

	public void setDefines(List<Define> defines) {
		this.defines = defines;
	}

	public List<Predicate> getPredicates() {
		return predicates;
	}

	public void setPredicates(List<Predicate> predicates) {
		this.predicates = predicates;
	}

	public List<Pattern> getPatterns() {
		return patterns;
	}

	public void setPatterns(List<Pattern> patterns) {
		this.patterns = patterns;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<WeightDefinition> getWeightDefs() {
		return weightDefs;
	}

	public void setWeightDefs(List<WeightDefinition> weightDefs) {
		this.weightDefs = weightDefs;
	}

	public void setEnergyBound(int bound) {
		this.bound = bound;
	}

	public int getEnergyBound() {
		return this.bound;
	}

	public List<Monitor> getMonitors() {
		return monitors;
	}

	public void setMonitors(List<Monitor> monitors) {
		this.monitors = monitors;
	}

	public List<Counter> getCounters() {
		return counters;
	}

	public void setCounters(List<Counter> counters) {
		this.counters = counters;
	}

	public List<TriggerConstraint> getTriggers() {
		return triggers;
	}

	public void setTriggers(List<TriggerConstraint> giTriggers) {
		this.triggers = giTriggers;
	}

	public List<Variable> getDomainVars() {
		return domainVars;
	}

	public void setDomainVars(List<Variable> domainVars) throws NonDomainVarException {
		for (Variable domainVar : domainVars) {
			if(!domainVar.isDomainVar()) {
				throw new NonDomainVarException("domainVars list does not contain only Domain Variables");
			}
		}
		this.domainVars = domainVars;
	}

	private class NonDomainVarException extends Exception {
		private static final long serialVersionUID = 1L;

		public NonDomainVarException(String msg) {
			super(msg);
		}
	}
	
	public void setRegtestExpressions(List<RegexpTestModel> regtestExpressions) {
		this.regtestExpressions = regtestExpressions;
	}

	public List<RegexpTestModel> getRegtestExpressions() {
		return regtestExpressions;
	}
	
	public void addregtestExperssion(RegexpTestModel regtestExpression)
	{
		this.regtestExpressions.add(regtestExpression);
	}
}
