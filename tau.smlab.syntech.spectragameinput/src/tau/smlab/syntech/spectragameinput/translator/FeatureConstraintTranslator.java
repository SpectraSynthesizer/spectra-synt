package tau.smlab.syntech.spectragameinput.translator;

import java.util.HashMap;

import tau.smlab.syntech.gameinput.pl.Feature;
import tau.smlab.syntech.gameinput.pl.FeatureConstraint;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.spectra.FeatureAndExpr;
import tau.smlab.syntech.spectra.FeatureIffExpr;
import tau.smlab.syntech.spectra.FeatureImpExpr;
import tau.smlab.syntech.spectra.FeatureOrExpr;
import tau.smlab.syntech.spectragameinput.SpectraTranslationException;

public class FeatureConstraintTranslator {
	
	public static HashMap<String, Feature> featureMap = new HashMap<>();

	public static FeatureConstraint parseConstraint(tau.smlab.syntech.spectra.FeatureExpr exp, Tracer tracer) throws SpectraTranslationException {

		if (exp instanceof FeatureImpExpr) {
			
			return parseConstraint((FeatureImpExpr) exp, tracer);
			
		} else if (exp instanceof FeatureIffExpr) {
			
			return parseConstraint((FeatureIffExpr) exp, tracer);
			
		} else if (exp instanceof FeatureOrExpr) {
			
			return parseConstraint((FeatureOrExpr) exp, tracer);
			
		} else if (exp instanceof FeatureAndExpr) {
			
			return parseConstraint((FeatureAndExpr) exp, tracer);
			
		} else {
		
			if (exp.getExpr() != null) {
				
				return new FeatureConstraint(Operator.NOT, parseConstraint(exp.getExpr(), tracer), tracer.getTrace(exp));
				
			} else if (exp.getFeature() != null) {
				
				return new FeatureConstraint(featureMap.get(exp.getFeature().getName()), tracer.getTrace(exp));
				
			} else {
				
				throw new SpectraTranslationException("Invalid Feature Constraint");
				
			}

		}
	}
	
	private static FeatureConstraint parseConstraint(FeatureImpExpr exp, Tracer tracer) throws SpectraTranslationException {

		return new FeatureConstraint(Operator.IMPLIES,
				parseConstraint(exp.getLeft(), tracer),
				parseConstraint(exp.getImplication(), tracer), tracer.getTrace(exp));
	}
	
	private static FeatureConstraint parseConstraint(FeatureIffExpr exp, Tracer tracer) throws SpectraTranslationException {

		FeatureConstraint fc = parseConstraint(exp.getElements().get(0), tracer);
		for (int i = 1; i < exp.getElements().size(); i++) {
			fc = new FeatureConstraint(Operator.IFF, fc, parseConstraint(exp.getElements().get(i), tracer), tracer.getTrace(exp));
		}
		return fc;
	}
	
	private static FeatureConstraint parseConstraint(FeatureOrExpr exp, Tracer tracer) throws SpectraTranslationException {

		FeatureConstraint fc = parseConstraint(exp.getElements().get(0), tracer);
		for (int i = 1; i < exp.getElements().size(); i++) {
			fc = new FeatureConstraint(Operator.OR, fc, parseConstraint(exp.getElements().get(i), tracer), tracer.getTrace(exp));
		}
		return fc;
	}
	
	private static FeatureConstraint parseConstraint(FeatureAndExpr exp, Tracer tracer) throws SpectraTranslationException {

		FeatureConstraint fc = parseConstraint(exp.getElements().get(0), tracer);
		for (int i = 1; i < exp.getElements().size(); i++) {
			fc = new FeatureConstraint(Operator.AND, fc, parseConstraint(exp.getElements().get(i), tracer), tracer.getTrace(exp));
		}
		return fc;
	}
}
