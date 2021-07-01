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

package tau.smlab.syntech.gameinputtrans.translator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.Constraint.Kind;
import tau.smlab.syntech.gameinput.model.ExistentialConstraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.Player;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.model.WeightDefinition;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gameinputtrans.TranslationException;

/**
 * translate every operator to a variable added to AUX the SpecExp with the past operator becomes a VariableReference to
 * the new variable the variable value is defined by initial and safety constraints that are added to the AUX module
 * (details what constraints to add are in code below)
 * 
 * https://smlab.unfuddle.com/svn/smlab_synthesis1/trunk/AspectLTL/net.games.core/src/net/games/core/parser/SpecificationParser.java
 * 
 * Depends on DefinesTranslator, CounterTranslator, MonitorTranslator, PatternConstraintTranslator, PredicateInstanceTranslator
 * 
 */
public class PastLTLTranslator implements Translator {

	private static int runningNumber = 0;
	private Player auxPlayer;
	private Map<Integer, VariableReference> pasLTLSpecsHashMap;
	private List<Constraint> auxPastConstraints;
	private Map<VariableReference, List<Constraint>> varRefToConstraints;
	@Override
	public void translate(GameInput input) {

		auxPlayer = input.getAux();
		pasLTLSpecsHashMap = new HashMap<>();
		auxPastConstraints = new ArrayList<Constraint>();
		varRefToConstraints = new HashMap<>();

		// sys constraints
		for (Constraint c : input.getSys().getConstraints()) {
			if (c.getSpec().isPastLTLSpec()) {
				try {
					c.setSpec(replacePastOperators(c.getSpec(), c.getTraceId()));
				} catch (CloneNotSupportedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}

		// sys existential constraints
		for (ExistentialConstraint exC : input.getSys().getExistentialConstraints()) {
			Spec currSpec;
			if(!exC.isRegExp()) {
				for(int i = 0; i < exC.getSize() ; i++) {
					currSpec = exC.getSpec(i);
					if (currSpec.isPastLTLSpec()) {
						try {
							exC.replaceSpec(i, replacePastOperators(currSpec, exC.getTraceId()));
						} catch (CloneNotSupportedException e) {
							// TODO Auto-generated catch block
							e.printStackTrace();
						}
					}
				}
			}
		}

		// env constraints
		for (Constraint c : input.getEnv().getConstraints()) {
			if (c.getSpec().isPastLTLSpec()) {
				try {
					c.setSpec(replacePastOperators(c.getSpec(), c.getTraceId()));
				} catch (CloneNotSupportedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}

		// aux constraints
		for (Constraint c : input.getAux().getConstraints()) {
			if (c.getSpec().isPastLTLSpec()) {
				try {
					c.setSpec(replacePastOperators(c.getSpec(), c.getTraceId()));
				} catch (CloneNotSupportedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}

		// weights
		for (WeightDefinition w : input.getWeightDefs()) {
			if (w.getDefinition().getSpec().isPastLTLSpec()) {
				try {
					w.getDefinition().setSpec(replacePastOperators(w.getDefinition().getSpec(), w.getDefinition().getTraceId()));
				} catch (CloneNotSupportedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}

		input.getAux().getConstraints().addAll(auxPastConstraints);
	}

	private Spec replacePastOperators(Spec spec, int traceId) throws CloneNotSupportedException {

		if (spec instanceof SpecExp) {
			int specHash = -1;
			SpecExp se = (SpecExp) spec;
			if (se.getOperator().isPastLTLOp()) {
				specHash= getHashCode(spec, traceId);
				VariableReference hashedReference = pasLTLSpecsHashMap.get(specHash);
				if (hashedReference != null) {
					return hashedReference;
				}
			}

			for (int i = 0; i < se.getChildren().length; i++) {
				se.getChildren()[i] = replacePastOperators(se.getChildren()[i], traceId);
			}
			if (se.getOperator().isPastLTLOp()) {        
				// prepare common variables
				Variable aux = addFreshAuxVariable(se.getOperator(), traceId);
				Spec p_aux = new SpecExp(Operator.PRIME, new VariableReference(aux));
				Spec safetySpec;
				switch (se.getOperator()) {
				case PREV:
					// negation of aux var
					Spec iniSpec = new SpecExp(Operator.NOT, new VariableReference(aux));
					auxPastConstraints.add(new Constraint(Kind.INI, iniSpec, null, traceId));

					// update of aux var
					safetySpec = new SpecExp(Operator.IFF, p_aux, se.getChildren()[0]);
					auxPastConstraints.add(new Constraint(Kind.SAFETY, safetySpec, null, traceId));
					break;
				case ONCE:
					iniSpec = new SpecExp(Operator.IFF, new VariableReference(aux), se.getChildren()[0]);
					auxPastConstraints.add(new Constraint(Kind.INI, iniSpec, null, traceId));

					Spec auxOrPChild = new SpecExp(Operator.OR, new VariableReference(aux), new SpecExp(Operator.PRIME, se.getChildren()[0].clone()));
					safetySpec = new SpecExp(Operator.IFF, p_aux, auxOrPChild);
					auxPastConstraints.add(new Constraint(Kind.SAFETY, safetySpec, null, traceId));
					break;
				case HISTORICALLY:
					iniSpec = new SpecExp(Operator.IFF, new VariableReference(aux), se.getChildren()[0]);
					auxPastConstraints.add(new Constraint(Kind.INI, iniSpec, null, traceId));

					Spec auxAndPChild = new SpecExp(Operator.OR, new VariableReference(aux), new SpecExp(Operator.PRIME, se.getChildren()[0].clone()));
					safetySpec = new SpecExp(Operator.IFF, p_aux, auxAndPChild);
					auxPastConstraints.add(new Constraint(Kind.SAFETY, safetySpec, null, traceId));
					break;          
				case SINCE:
					// aux initially set by second child
					iniSpec = new SpecExp(Operator.IFF, new VariableReference(aux), se.getChildren()[1]);
					auxPastConstraints.add(new Constraint(Kind.INI, iniSpec, null, traceId));

					Spec primedChild1 = new SpecExp(Operator.PRIME, se.getChildren()[0].clone());
					Spec primedChild2 = new SpecExp(Operator.PRIME, se.getChildren()[1].clone());
					Spec child1AndAux = new SpecExp(Operator.AND, new VariableReference(aux), primedChild1);
					Spec child2_Or_Child1AndAux = new SpecExp(Operator.OR,primedChild2, child1AndAux);
					safetySpec = new SpecExp(Operator.IFF, p_aux, child2_Or_Child1AndAux);
					auxPastConstraints.add(new Constraint(Kind.SAFETY, safetySpec, null, traceId));
					break;
				case TRIGGERED:
					Spec child1OrChild2 = new SpecExp(Operator.OR, se.getChildren()[0], se.getChildren()[1]);
					iniSpec = new SpecExp(Operator.IFF, new VariableReference(aux), child1OrChild2);
					auxPastConstraints.add(new Constraint(Kind.INI, iniSpec, null, traceId));

					primedChild1 = new SpecExp(Operator.PRIME, se.getChildren()[0].clone());
					primedChild2 = new SpecExp(Operator.PRIME, se.getChildren()[1].clone());
					child1AndAux = new SpecExp(Operator.AND, new VariableReference(aux), primedChild1);
					child2_Or_Child1AndAux = new SpecExp(Operator.OR,primedChild2, child1AndAux);
					safetySpec = new SpecExp(Operator.IFF, p_aux, child2_Or_Child1AndAux);
					auxPastConstraints.add(new Constraint(Kind.SAFETY, safetySpec, null, traceId));
					break;

				default:
					throw new TranslationException("Unsupported PastLTL operator " + se.getOperator(), traceId);
				}
				VariableReference variableReference = new VariableReference(aux);
				pasLTLSpecsHashMap.put(specHash, variableReference);
				varRefToConstraints.put(variableReference, new ArrayList<>(auxPastConstraints.subList
						(auxPastConstraints.size()-2, auxPastConstraints.size())));
				return variableReference;
			}
		}

		return spec;
	}

	private int getHashCode(Spec spec, int traceId) {
		if (spec instanceof VariableReference)
		{
			VariableReference variableReference = (VariableReference)spec;
			return variableReference.getReferenceName().hashCode();
		}
		else if (spec instanceof PrimitiveValue)
		{
			PrimitiveValue primitiveValue = (PrimitiveValue)spec;
			return primitiveValue.getValue().hashCode();
		}
		else if (! (spec instanceof SpecExp))
		{
			throw new TranslationException("PastLTL Spec is not VariableReference / PrimitiveValue / SpecExp", traceId);
		}
		SpecExp specExp = (SpecExp)spec;
		int hashCode = specExp.getOperator().hashCode();
		for (int i = 0; i <specExp.getChildren().length; i++)
		{
			Spec child = specExp.getChildren()[i];
			hashCode *= 31;
			hashCode += getHashCode(child, traceId);
		}

		return hashCode;
	}

	private Variable addFreshAuxVariable(Operator operator, int traceId) {
		String varName = operator.toString() + "_" + traceId + "_" + runningNumber++;
		Variable aux = new Variable(varName, new TypeDef());
		auxPlayer.addVar(aux);
		return aux;
	}

	public List<Constraint> getConstraintsOfVarRef(VariableReference varRef) {
		return this.varRefToConstraints.get(varRef);
	}

}
