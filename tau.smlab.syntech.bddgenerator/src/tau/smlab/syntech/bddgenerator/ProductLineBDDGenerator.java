package tau.smlab.syntech.bddgenerator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import net.sf.javabdd.BDD.BDDIterator;
import tau.smlab.syntech.gameinput.model.GameInput;
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
	
	public static BDD createBdd(FeatureConstraint constraint) {
		
		if (constraint.getFeature() != null) {
			return Env.getBDDValue(getFeatureVarName(constraint.getFeature()), "true").id();
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
	
	public static List<Product> createProducts(GameInput input) {
		
		// Handle the product line features
		BDDVarSet featureVarSet = Env.getEmptySet();
		Map<ModuleBDDField, Feature> featureFields = new HashMap<>();
		try {
			for (Feature f : input.getFeatures()) {
				ModuleBDDField featureField = Env.newVar(ProductLineBDDGenerator.getFeatureVarName(f));
				featureFields.put(featureField, f);
				featureVarSet.unionWith(featureField.support());
			}
		} catch (ModuleVariableException e) {
			e.printStackTrace();
		}
		
		BDD featureModelBDD = ProductLineBDDGenerator.createBdd(input.getFeatureModel());
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
	
	
	public static Map<Product, List<Product>> createProductLattice(List<Product> products) {
		
		Map<Product, List<Product>> lattice = new HashMap<>();
		
		for (Product p0 : products) {
			for (Product p1 : products) {
				
				if (p0.subsumes(p1)) {
					
					if (products.stream().noneMatch(p -> !p.equals(p0) && !p.equals(p1) && p0.subsumes(p) && p.subsumes(p1))) {
						if (!lattice.containsKey(p0)) {
							lattice.put(p0, new ArrayList<>());
						}
						lattice.get(p0).add(p1);
					}
				}
				
			}
		}
		
		return lattice;
	}
	
	public static List<Product> getRoots(Map<Product, List<Product>> lattice) {
		Set<Product> allSubsumed = lattice.values().stream().flatMap(List<Product>::stream).collect(Collectors.toSet());
		return lattice.keySet().stream().filter(p -> !allSubsumed.contains(p)).collect(Collectors.toList());
	}

}
