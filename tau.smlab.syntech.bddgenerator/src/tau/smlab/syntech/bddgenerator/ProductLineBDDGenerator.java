package tau.smlab.syntech.bddgenerator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDD.BDDIterator;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.gameinput.pl.Feature;
import tau.smlab.syntech.gameinput.pl.FeatureConstraint;
import tau.smlab.syntech.gameinput.pl.Product;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.ModuleVariableException;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

public class ProductLineBDDGenerator {
	
	public static String getFeatureVarName(Feature feature) {
		return String.format("feature_%s", feature.getName());
	}
	
	private static BDD createBdd(FeatureConstraint constraint) {
		
		if (constraint.getFeature() != null) {
			return Env.getVar(getFeatureVarName(constraint.getFeature())).support().toBDD();
		} else {
			
			Operator op = constraint.getTheOp();
			BDD bdd0 = createBdd(constraint.getElements()[0]);
			BDD res = null;
			
			if (Operator.NOT.equals(op)) {
				
				res = bdd0.not();
				
			} else {
				BDD bdd1 = createBdd(constraint.getElements()[1]);
				
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
	
	public static List<Product> createProducts(List<Feature> features, FeatureConstraint featureModel) {
		
		// Handle the product line features
		BDDVarSet featureVarSet = Env.getEmptySet();
		Map<ModuleBDDField, Feature> featureFields = new HashMap<>();
		try {
			for (Feature f : features) {
				ModuleBDDField featureField = Env.newVar(getFeatureVarName(f));
				featureFields.put(featureField, f);
				featureVarSet.unionWith(featureField.support());
			}
		} catch (ModuleVariableException e) {
			e.printStackTrace();
		}
		
		BDD featureModelBDD = ProductLineBDDGenerator.createBdd(featureModel);
		BDDIterator productsIterator = featureModelBDD.iterator(featureVarSet);
		List<Product> products = new ArrayList<>();
		
		while (productsIterator.hasNext()) {
			BDD product = productsIterator.next();
			Product p = new Product();
			
			for (ModuleBDDField field : featureFields.keySet()) {
				String val = Env.getValueByDomain(product, field.getDomain());
				if ("true".equals(val)) {
					Feature f = featureFields.get(field);
					p.getFeatures().add(f);
					p.getGars().addAll(f.getGars());
					p.getAsms().addAll(f.getAsms());
				}
			}
			products.add(p);
			product.free();
		}
		featureModelBDD.free();
		
		for (ModuleBDDField field : featureFields.keySet()) {
			Env.deleteVar(field.getName());
		}
		
		return products;
	}

}
