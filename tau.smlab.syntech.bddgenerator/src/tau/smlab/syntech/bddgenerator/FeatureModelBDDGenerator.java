package tau.smlab.syntech.bddgenerator;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gameinput.pl.Feature;
import tau.smlab.syntech.gameinput.pl.FeatureConstraint;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.jtlv.Env;

public class FeatureModelBDDGenerator {
	
	public static String getFeatureVarName(Feature feature) {
		return String.format("feature_%s", feature.getName());
	}
	
	public static BDD createBdd(FeatureConstraint constraint, int traceId) {
		
		if (constraint.getFeature() != null) {
			return Env.getBDDValue(getFeatureVarName(constraint.getFeature()), "true").id();
		} else {
			
			Operator op = constraint.getTheOp();
			BDD bdd0 = createBdd(constraint.getElements()[0], traceId);
			BDD res = null;
			
			if (Operator.NOT.equals(op)) {
				
				res = bdd0.not();
				
			} else {
				BDD bdd1 = createBdd(constraint.getElements()[1], traceId);
				
				if (Operator.IFF.equals(op)) {
					res = bdd0.biimp(bdd1);
				} else if (Operator.AND.equals(op)) {
					res = bdd0.and(bdd1);
				} else if (Operator.OR.equals(op)) {
					res = bdd0.or(bdd1);
				} else if (Operator.IMPLIES.equals(op)) {
					res = bdd0.imp(bdd1);
				}
				
				bdd1.free();
			}
			
			bdd0.free();
			return res;
		}
	}

}
