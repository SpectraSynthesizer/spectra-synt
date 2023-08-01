package tau.smlab.syntech.gameinputtrans.translator;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.Constraint.Kind;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.pl.Feature;
import tau.smlab.syntech.gameinput.pl.FeatureConstraint;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;

public class FeatureTranslator implements Translator {
	
	boolean addFeatureModelConstraint;
	
	public FeatureTranslator(boolean featureModelExplicit) {
		addFeatureModelConstraint = featureModelExplicit;
	}

	@Override
	public void translate(GameInput input) {

		if (input.getFeatures() == null || input.getFeatures().isEmpty()) return;
		

		Map<Feature, Variable> variables = createFeatureVariables(input);
		
		for (Constraint c : input.getSys().getConstraints()) {
			
			List<Variable> relevantVariables = variables.keySet()
					.stream()
					.filter(f -> f.getGars().contains(c.getName()))
					.map(f -> variables.get(f))
					.toList();
			c.setSpec(addFeatureImplication(c.getSpec(), relevantVariables));
		}
		
		for (Constraint c : input.getEnv().getConstraints()) {
			
			List<Variable> relevantVariables = variables.keySet()
					.stream()
					.filter(f -> f.getAsms().contains(c.getName()))
					.map(f -> variables.get(f))
					.toList();
			c.setSpec(addFeatureImplication(c.getSpec(), relevantVariables));
		}
		
		setFeatureConstraints(input, variables);
		
		input.setFeatures(null);
		input.setFeatureModel(null);
	}
	
	private Map<Feature, Variable> createFeatureVariables(GameInput gi) {
		
		Map<Feature, Variable> variables = new HashMap<>();
		for (Feature f : gi.getFeatures()) {
			
			Variable v = new Variable(f.getName(), new TypeDef());
			gi.getAux().addVar(v);
			variables.put(f, v);
		}
		
		return variables;
	}
	
	private void setFeatureConstraints(GameInput gi, Map<Feature, Variable> features) {
		
		for (Variable feature : features.values()) {
			
			SpecExp spec = new SpecExp(Operator.EQUALS, new VariableReference(feature), new SpecExp(Operator.PRIME, new VariableReference(feature)));
			
			Constraint featureIsStatic = new Constraint(Kind.STATE_INV, spec, String.format("feature%sIsStatic", feature.getName()), feature.getTraceId());
			gi.getSys().addConstraint(featureIsStatic);
		}
		if (addFeatureModelConstraint) {
			Spec featureSpec = transformFeatureModelToSpec(gi.getFeatureModel(), features);
			Constraint featureModel = new Constraint(Kind.STATE_INV, featureSpec, "featureModel", gi.getFeatureModel().getTraceId());
			gi.getAux().addConstraint(featureModel);
		}
	}
	
	private Spec transformFeatureModelToSpec(FeatureConstraint featureModel, Map<Feature, Variable> variables) {
		
		if (featureModel.getFeature() != null) {
			return new VariableReference(variables.get(featureModel.getFeature()));
		} else { // getElements() != null
			if (featureModel.getElements().length == 1) {
				return new SpecExp(featureModel.getTheOp(), 
						transformFeatureModelToSpec(featureModel.getElements()[0], variables));
			} else {
				return new SpecExp(featureModel.getTheOp(), 
						transformFeatureModelToSpec(featureModel.getElements()[0], variables),
						transformFeatureModelToSpec(featureModel.getElements()[1], variables));
			}
		}
	}
	
	private Spec addFeatureImplication(Spec s, List<Variable> features) {
		
		if (features.isEmpty()) {
			return s;
		}
		
		Spec leftSide = new PrimitiveValue("FALSE");
		
		for (Variable feature : features) {
			leftSide = new SpecExp(Operator.OR, leftSide, new VariableReference(feature));
		}
		
		return new SpecExp(Operator.IMPLIES, leftSide, s);
	}

}
