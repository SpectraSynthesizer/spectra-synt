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

package tau.smlab.syntech.bddgenerator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDD.BDDIterator;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.gameinput.model.Player;
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
	
	public static List<Product> createProducts(List<Feature> features, FeatureConstraint featureModel,
			Player sys, Player env) {
		
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
					p.getGars().addAll(sys.getConstraints().stream().filter(c -> f.getGars().contains(c.getName())).collect(Collectors.toList()));
					p.getAsms().addAll(env.getConstraints().stream().filter(c -> f.getAsms().contains(c.getName())).collect(Collectors.toList()));
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
