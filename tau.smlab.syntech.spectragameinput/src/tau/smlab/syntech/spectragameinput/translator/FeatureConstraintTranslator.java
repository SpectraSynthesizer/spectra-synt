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
				
				return new FeatureConstraint(Operator.NOT, parseConstraint(exp.getExpr(), tracer));
				
			} else if (exp.getFeature() != null) {
				
				return new FeatureConstraint(featureMap.get(exp.getFeature().getName()));
				
			} else {
				
				throw new SpectraTranslationException("Invalid Feature Constraint");
				
			}

		}
	}
	
	private static FeatureConstraint parseConstraint(FeatureImpExpr exp, Tracer tracer) throws SpectraTranslationException {

		return new FeatureConstraint(Operator.IMPLIES,
				parseConstraint(exp.getLeft(), tracer),
				parseConstraint(exp.getImplication(), tracer));
	}
	
	private static FeatureConstraint parseConstraint(FeatureIffExpr exp, Tracer tracer) throws SpectraTranslationException {

		FeatureConstraint fc = parseConstraint(exp.getElements().get(0), tracer);
		for (int i = 1; i < exp.getElements().size(); i++) {
			fc = new FeatureConstraint(Operator.IFF, fc, parseConstraint(exp.getElements().get(i), tracer));
		}
		return fc;
	}
	
	private static FeatureConstraint parseConstraint(FeatureOrExpr exp, Tracer tracer) throws SpectraTranslationException {

		FeatureConstraint fc = parseConstraint(exp.getElements().get(0), tracer);
		for (int i = 1; i < exp.getElements().size(); i++) {
			fc = new FeatureConstraint(Operator.OR, fc, parseConstraint(exp.getElements().get(i), tracer));
		}
		return fc;
	}
	
	private static FeatureConstraint parseConstraint(FeatureAndExpr exp, Tracer tracer) throws SpectraTranslationException {

		FeatureConstraint fc = parseConstraint(exp.getElements().get(0), tracer);
		for (int i = 1; i < exp.getElements().size(); i++) {
			fc = new FeatureConstraint(Operator.AND, fc, parseConstraint(exp.getElements().get(i), tracer));
		}
		return fc;
	}
}
