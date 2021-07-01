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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.EcoreUtil2;

import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;

import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.model.Constraint.Kind;
import tau.smlab.syntech.gameinput.model.Define;
import tau.smlab.syntech.gameinput.model.DefineArray;
import tau.smlab.syntech.gameinput.model.ExistentialConstraint;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.model.PatternConstraint;
import tau.smlab.syntech.gameinput.model.Player;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.model.WeightDefinition;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;
import tau.smlab.syntech.gameinput.spec.SpecWrapper;
import tau.smlab.syntech.spectra.Decl;
import tau.smlab.syntech.spectra.DefineDecl;
import tau.smlab.syntech.spectra.EXGar;
import tau.smlab.syntech.spectra.LTLAsm;
import tau.smlab.syntech.spectra.LTLGar;
import tau.smlab.syntech.spectra.Model;
import tau.smlab.syntech.spectra.Pattern;
import tau.smlab.syntech.spectra.PatternParam;
import tau.smlab.syntech.spectra.Predicate;
import tau.smlab.syntech.spectra.RegExp;
import tau.smlab.syntech.spectra.Subrange;
import tau.smlab.syntech.spectra.TemporalExpression;
import tau.smlab.syntech.spectra.Trigger;
import tau.smlab.syntech.spectra.TypeConstant;
import tau.smlab.syntech.spectra.TypedParam;
import tau.smlab.syntech.spectra.Var;
import tau.smlab.syntech.spectra.VarDecl;
import tau.smlab.syntech.spectra.VarType;
import tau.smlab.syntech.spectra.WeightDef;
import tau.smlab.syntech.spectragameinput.SpectraTranslationException;
import tau.smlab.syntech.typesystem.TypeSystemUtils;

public class Spectra2GameInputTranslator {

	public static GameInput translate(Model model) throws SpectraTranslationException {

		EntitiesMapper entitiesMapper = new EntitiesMapper();

		Tracer tracer = new Tracer();
		Player sysPlayer = new Player();
		Player envPlayer = new Player();
		Player auxPlayer = new Player();

		// Variables
		EcoreUtil2.getAllContentsOfType(model, VarDecl.class)
				.forEach(varDec -> addVarToPlayer(entitiesMapper, tracer, sysPlayer, envPlayer, auxPlayer, varDec));

		// WeightDefinition
		List<WeightDefinition> giWeightDefsList = Lists
				.newArrayList(Iterators.filter(model.getElements().iterator(), WeightDef.class)).stream()
				.map(spectraWeight -> getGiWeight(entitiesMapper, tracer, spectraWeight)).collect(Collectors.toList());

		// keep track of all QuantifierVars
		// List<Variable> allQuantifierVars = new ArrayList<Variable>();

		// Gars
		Iterators.filter(model.getElements().iterator(), LTLGar.class)
				.forEachRemaining(gar -> addGar(entitiesMapper, tracer, sysPlayer, gar));

		// Assumptions
		Iterators.filter(model.getElements().iterator(), LTLAsm.class)
				.forEachRemaining(asm -> addAsm(entitiesMapper, tracer, envPlayer, asm));

		// Existential Gars
		Iterators.filter(model.getElements().iterator(), EXGar.class)
				.forEachRemaining(exGar -> addExGar(entitiesMapper, tracer, sysPlayer, exGar));

		GameInput gi = new GameInput(model.getName(), sysPlayer, envPlayer, auxPlayer);
		gi.setDefines(entitiesMapper.getDefineNameToDefineMapping().getAllDefines());
		gi.setPredicates(entitiesMapper.getPredicateNameToPredicateMapping().getAllPredicates());
		gi.setPatterns(entitiesMapper.getPatternNameToPatternMapping().getAllPatterns());
		gi.setMonitors(entitiesMapper.getMonitorNameToMonitorMapping().getAllMonitors());
		gi.setCounters(entitiesMapper.getCounterNameToCounterMapping().getAllCounters());
		gi.setWeightDefs(giWeightDefsList);
		return gi;
	}

	private static void addVarToPlayer(EntitiesMapper entitiesMapper, Tracer tracer, Player sysPlayer, Player envPlayer,
			Player auxPlayer, VarDecl varDec) {
		Pattern patternContainer = EcoreUtil2.getContainerOfType(varDec, Pattern.class);
		if (patternContainer == null) {
			// check that varDec isn't declared inside a pattern
			if (isSysVar(varDec)) {
				addVarToPlayer(sysPlayer, varDec, entitiesMapper.getVariableNameToVariableMapping(), tracer);
			} else if (isEnvVar(varDec)) {
				addVarToPlayer(envPlayer, varDec, entitiesMapper.getVariableNameToVariableMapping(), tracer);
			} else if (isAuxVar(varDec)) {
				addVarToPlayer(auxPlayer, varDec, entitiesMapper.getVariableNameToVariableMapping(), tracer);
			}
		}
	}

	private static WeightDefinition getGiWeight(EntitiesMapper entitiesMapper, Tracer tracer, WeightDef spectraWeight) {
		tracer.addTrace(spectraWeight);
		try {
			int weightValue = (spectraWeight.getNegative() == null ? spectraWeight.getValue()
					: (-1) * spectraWeight.getValue());
			Spec weightSpec;
			try {
				weightSpec = SpectraASTToSpecGenerator
						.getConstraintSpec(spectraWeight.getDefinition(), entitiesMapper, tracer, null, null, null)
						.getSpec();
			} catch (SpectraTranslationException e) {
				e.setTraceId(tracer.getTrace(spectraWeight));
				throw e;
			}
			Constraint weightConstraint = new Constraint(Kind.WEIGHT, weightSpec, spectraWeight.getName(),
					tracer.getTrace(spectraWeight));
			return new WeightDefinition(spectraWeight.getName(), weightValue, weightConstraint);
		} catch (SpectraTranslationException e) {
			e.setTraceId(tracer.getTrace(spectraWeight));
			throw new RuntimeException(e);
		}
	}

	private static void addGar(EntitiesMapper entitiesMapper, Tracer tracer, Player sysPlayer, LTLGar gar) {
		tracer.addTrace(gar);
		try {
			if (gar.getTemporalExpr() != null) {
				if (gar.getParams() != null) {
					addParameterizedConstraint(entitiesMapper, tracer, sysPlayer, gar, gar.getName(),
							gar.getParams().getParams());
				} else {
					addSingleConstraint(entitiesMapper, tracer, sysPlayer, gar, gar.getName(), null);
				}
			} else { // Case of a trigger constraint
				TriggerConstraint trig = computeTriggerConstraint(entitiesMapper, tracer, tracer.getTrace(gar),
						gar.getTrig(), gar.getName());
				sysPlayer.addTrigger(trig);
			}	
		} catch (SpectraTranslationException e) {
			e.setTraceId(tracer.getTrace(gar));
			throw new RuntimeException(e);
		}
	}

	private static void addAsm(EntitiesMapper entitiesMapper, Tracer tracer, Player envPlayer, LTLAsm asm) {
		tracer.addTrace(asm);
		try {
			if (asm.getTemporalExpr() != null) {
				if (asm.getParams() != null) {
					addParameterizedConstraint(entitiesMapper, tracer, envPlayer, asm, asm.getName(),
							asm.getParams().getParams());
				} else {
					addSingleConstraint(entitiesMapper, tracer, envPlayer, asm, asm.getName(), null);
				}
			} else { // Case of a trigger constraint
				TriggerConstraint trig = computeTriggerConstraint(entitiesMapper, tracer, tracer.getTrace(asm),
						asm.getTrig(), asm.getName());
				envPlayer.addTrigger(trig);
			}

		} catch (SpectraTranslationException e) {
			e.setTraceId(tracer.getTrace(asm));
			throw new RuntimeException(e);
		}
	}

	private static void addParameterizedConstraint(EntitiesMapper entitiesMapper, Tracer tracer, Player player,
			Decl constraint, String name, List<TypedParam> parameters) throws SpectraTranslationException {
		List<List<PrimitiveValue>> primitivesLists = parameters.stream()
				.map(param -> getTypeDef(param.getType()).getPrimitivesList()).collect(Collectors.toList());

		int valuesCounter = 0;
		for (List<PrimitiveValue> values : Lists.cartesianProduct(primitivesLists)) {
			Map<String, PrimitiveValue> variables = new HashMap<>();

			for (int i = 0; i < parameters.size(); i++) {
				variables.put(parameters.get(i).getName(), values.get(i));
			}

			// Need to add the constraint several times to the tracer, to get a unique trace id each time
			// the constraint is duplicated
			tracer.addTrace(constraint);
			addSingleConstraint(entitiesMapper, tracer, player, constraint, name + valuesCounter, variables);
			valuesCounter++;
		}
	}

	private static void addSingleConstraint(EntitiesMapper entitiesMapper, Tracer tracer, Player player,
			Decl constraint, String name, Map<String, PrimitiveValue> params) throws SpectraTranslationException {
		TemporalExpression expr = null;
		if (constraint instanceof LTLAsm) {
			expr = ((LTLAsm) constraint).getTemporalExpr();
		} else if (constraint instanceof LTLGar) {
			expr = ((LTLGar) constraint).getTemporalExpr();
		}

		// INI, SAFETY, or JUSTICE
		SpecWrapper specWrapper = SpectraASTToSpecGenerator.getConstraintSpec(expr, entitiesMapper, tracer, null, null,
				params);
		if (!specWrapper.isHasPatternReference()) { // case of a non-pattern constraint
			Kind constraintKind = getConstraintKind(constraint);
			player.addConstraint(
					new Constraint(constraintKind, specWrapper.getSpec(), name, tracer.getTrace(constraint)));
		} else { // case of a pattern constraint
			player.addPatternConstraint(new PatternConstraint(specWrapper.getPattern().getPatternName(),
					specWrapper.getPattern(), specWrapper.getParameters(), tracer.getTrace(constraint)));
		}
	}

	private static void addExGar(EntitiesMapper entitiesMapper, Tracer tracer, Player sysPlayer, EXGar exGar) {
		ExistentialConstraint exConstraint;
		SpecWrapper specWrapper;
		tracer.addTrace(exGar);
		try {
			if (exGar.getElements() != null && !exGar.getElements().isEmpty()) {
				exConstraint = new ExistentialConstraint(exGar.getName(), tracer.getTrace(exGar));
				for (EObject exp : exGar.getElements()) {
					specWrapper = SpectraASTToSpecGenerator.getConstraintSpec((TemporalExpression) exp, entitiesMapper,
							tracer, null, null, null);
					exConstraint.addSpec(specWrapper.getSpec());
				}
			} else if (exGar.getRegExpPointer() != null) {
				// TODO check if the regexp has already been mapped using the tracer
				exConstraint = new ExistentialConstraint(exGar.getName(), SpectraASTToSpecGenerator
						.getConstraintSpecRegExp(entitiesMapper, tracer, exGar.getRegExpPointer().getExp()),
						tracer.getTrace(exGar));
			} else { // exGar.getRegExp() != null
				exConstraint = new ExistentialConstraint(exGar.getName(),
						SpectraASTToSpecGenerator.getConstraintSpecRegExp(entitiesMapper, tracer, exGar.getRegExp()),
						tracer.getTrace(exGar));
			}
			sysPlayer.addExistentialConstraint(exConstraint);
		} catch (SpectraTranslationException e) {
			e.setTraceId(tracer.getTrace(exGar));
			throw new RuntimeException(e);
		}
	}

	public static tau.smlab.syntech.gameinput.model.Pattern computePattern(EntitiesMapper entitiesMapper, Tracer tracer,
			Pattern pattern) throws SpectraTranslationException {
		tracer.addTrace(pattern);

		List<Variable> paramsList = new ArrayList<>();
		for (PatternParam patternParam : pattern.getParams().getParams()) {
			paramsList.add(new Variable(patternParam.getName(), new TypeDef(), tracer.addTrace(patternParam)));
		}

		List<Variable> varsList = new ArrayList<>();
		for (VarDecl varDecl : pattern.getVarDeclList()) {
			TypeDef typeDef = getTypeDef(varDecl.getType());
			varsList.add(new Variable(varDecl.getName(), typeDef, tracer.addTrace(varDecl)));
		}

		List<Variable> varsAndParamsList = new ArrayList<>();
		varsAndParamsList.addAll(varsList);
		varsAndParamsList.addAll(paramsList);

		List<Constraint> expressions = new ArrayList<>();
		for (TemporalExpression initial : pattern.getInitial()) {
			tracer.addTrace(initial);
			Spec initalSpec;
			try {
				initalSpec = SpectraASTToSpecGenerator
						.getConstraintSpec(initial, entitiesMapper, tracer, null, varsAndParamsList, null).getSpec();
			} catch (SpectraTranslationException e) {
				e.setTraceId(tracer.getTrace(initial));
				throw e;
			}
			expressions.add(new Constraint(Kind.INI, initalSpec, null, tracer.getTrace(initial)));
		}
		for (TemporalExpression safety : pattern.getSafety()) {
			tracer.addTrace(safety);
			Spec safetySpec;
			try {
				safetySpec = SpectraASTToSpecGenerator
						.getConstraintSpec(safety, entitiesMapper, tracer, null, varsAndParamsList, null).getSpec();
			} catch (SpectraTranslationException e) {
				e.setTraceId(tracer.getTrace(safety));
				throw e;
			}
			expressions.add(new Constraint(Kind.SAFETY, safetySpec, null, tracer.getTrace(safety)));
		}
		for (TemporalExpression stateInv : pattern.getStateInv()) {
			tracer.addTrace(stateInv);
			Spec safetySpec;
			try {
				safetySpec = SpectraASTToSpecGenerator
						.getConstraintSpec(stateInv, entitiesMapper, tracer, null, varsAndParamsList, null).getSpec();
			} catch (SpectraTranslationException e) {
				e.setTraceId(tracer.getTrace(stateInv));
				throw e;
			}
			expressions.add(new Constraint(Kind.STATE_INV, safetySpec, null, tracer.getTrace(stateInv)));
		}
		for (TemporalExpression justice : pattern.getJustice()) {
			tracer.addTrace(justice);
			Spec justiceSpec;
			try {
				justiceSpec = SpectraASTToSpecGenerator
						.getConstraintSpec(justice, entitiesMapper, tracer, null, varsAndParamsList, null).getSpec();
			} catch (SpectraTranslationException e) {
				e.setTraceId(tracer.getTrace(justice));
				throw e;
			}

			expressions.add(new Constraint(Kind.JUSTICE, justiceSpec, null, tracer.getTrace(justice)));
		}

		tau.smlab.syntech.gameinput.model.Pattern gameInputPattern = new tau.smlab.syntech.gameinput.model.Pattern(
				pattern.getName(), expressions, varsList, paramsList, tracer.getTrace(pattern));
		return gameInputPattern;
	}

	public static TriggerConstraint computeTriggerConstraint(EntitiesMapper entitiesMapper, Tracer tracer, int traceId,
			Trigger trig, String trigName) throws SpectraTranslationException {

		RegExp init = null, effect = null;
		SpecRegExp initSpecRegExp, effectSpecRegExp;

		// Create SpecRegExp for the trigger initiator
		if (trig.getInitPointer() != null) {
			init = trig.getInitPointer().getExp();
		} else if (trig.getInitRegExp() != null) {
			init = trig.getInitRegExp();
		}

		if (init == null) {
			throw new SpectraTranslationException("trigger has no initial regexp");
		}

		initSpecRegExp = SpectraASTToSpecGenerator.getConstraintSpecRegExp(entitiesMapper, tracer, init);

		// Create SpecRegExp for the trigger effect
		if (trig.getEffectPointer() != null) {
			effect = trig.getEffectPointer().getExp();
		} else if (trig.getEffectRegExp() != null) {
			effect = trig.getEffectRegExp();
		}

		if (effect == null) {
			throw new SpectraTranslationException("trigger has no effect regexp");
		}

		effectSpecRegExp = SpectraASTToSpecGenerator.getConstraintSpecRegExp(entitiesMapper, tracer, effect);

		// Create the trigger
		TriggerConstraint spectraTrig = new TriggerConstraint(trigName, initSpecRegExp, effectSpecRegExp, traceId);

		return spectraTrig;
	}

	public static tau.smlab.syntech.gameinput.model.Predicate computePredicate(EntitiesMapper entitiesMapper,
			Tracer tracer, Predicate predicate) throws SpectraTranslationException {
		tracer.addTrace(predicate);

		List<Variable> paramsList = new ArrayList<>();
		if (predicate.getParams() != null) {
			for (TypedParam typedParam : predicate.getParams().getParams()) {
				TypeDef paramType = getTypeDef(typedParam.getType());
				paramsList.add(new Variable(typedParam.getName(), paramType, tracer.addTrace(typedParam)));
			}
		}

		Spec predicateExpression;
		try {
			predicateExpression = SpectraASTToSpecGenerator
					.getConstraintSpec(predicate.getBody(), entitiesMapper, tracer, paramsList, null, null).getSpec();
		} catch (SpectraTranslationException e) {
			e.setTraceId(tracer.addTrace(predicate.getBody()));
			throw e;
		}
		tau.smlab.syntech.gameinput.model.Predicate gameInputPredicate = new tau.smlab.syntech.gameinput.model.Predicate(
				predicate.getName(), predicateExpression, paramsList, tracer.getTrace(predicate));

		return gameInputPredicate;
	}

	public static Define computeDefine(EntitiesMapper entitiesMapper, Tracer tracer, DefineDecl define)
			throws SpectraTranslationException {
		try {
			Spec singleDefine = null;
			DefineArray defineArray = null;
			if (define.getSimpleExpr() != null) {
				singleDefine = SpectraASTToSpecGenerator
						.getConstraintSpec(define.getSimpleExpr(), entitiesMapper, tracer, null, null, null).getSpec();
			} else {
				defineArray = computeDefineArray(entitiesMapper, tracer, define.getInnerArray());
			}
			return new Define(define.getName(), singleDefine, defineArray);
		} catch (SpectraTranslationException e) {
			if (e.getTraceId() == 0) {
				e.setTraceId(tracer.addTrace(define.getSimpleExpr()));
			}
			throw e;
		}
	}
	
	private static DefineArray computeDefineArray(EntitiesMapper entitiesMapper, Tracer tracer, tau.smlab.syntech.spectra.DefineArray defineArray)
			throws SpectraTranslationException {
		int i = 0;
		try {
			List<Spec> defSpecs = new ArrayList<>();
			for (TemporalExpression exp : defineArray.getSimpleExprs()) {
				defSpecs.add(SpectraASTToSpecGenerator
						.getConstraintSpec(exp, entitiesMapper, tracer, null, null, null).getSpec());
				i++;
			}
			List<DefineArray> innerDefArrays = new ArrayList<>();
			for (tau.smlab.syntech.spectra.DefineArray innerArray : defineArray.getInnerArrays()) {
				innerDefArrays.add(computeDefineArray(entitiesMapper, tracer, innerArray));
			}
			return new DefineArray(defSpecs, innerDefArrays);
		} catch (SpectraTranslationException e) {
			e.setTraceId(tracer.addTrace(defineArray.getSimpleExprs().get(i)));
			throw e;
		}
	}

	private static boolean isEnvVar(VarDecl varDec) {
		Var var = EcoreUtil2.getContainerOfType(varDec, Var.class);
		if (var.getKind().getName().equals("ENV")) {
			return true;
		}
		return false;
	}

	private static boolean isSysVar(VarDecl varDec) {
		Var var = EcoreUtil2.getContainerOfType(varDec, Var.class);
		if (var.getKind().getName().equals("SYS")) {
			return true;
		}
		return false;
	}

	private static boolean isAuxVar(VarDecl varDec) {
		Var var = EcoreUtil2.getContainerOfType(varDec, Var.class);
		if (var.getKind().getName().equals("AUX")) {
			return true;
		}
		return false;
	}

	private static Kind getConstraintKind(Decl constraint) {

		if (constraint instanceof LTLAsm) {
			LTLAsm asm = (LTLAsm) constraint;
			if (asm.getTrig() != null) {
				return Kind.TRIGGER;
			}
			if (asm.getJustice() != null) {
				return Kind.JUSTICE;
			}
			if (asm.getSafety() != null) {
				return Kind.SAFETY;
			}
			if (asm.getStateInv() != null) {
				return Kind.STATE_INV;
			}
			return Kind.INI;
		} else if (constraint instanceof LTLGar) {
			LTLGar gar = (LTLGar) constraint;
			if (gar.getTrig() != null) {
				return Kind.TRIGGER;
			}
			if (gar.getJustice() != null) {
				return Kind.JUSTICE;
			}
			if (gar.getSafety() != null) {
				return Kind.SAFETY;
			}
			if (gar.getStateInv() != null) {
				return Kind.STATE_INV;
			}
			return Kind.INI;
		}

		return null;
	}

	private static void addVarToPlayer(Player player, VarDecl varDec, VariableNameToVariableMapping variableMapping,
			Tracer tracer) {
		TypeDef typeDef = getTypeDef(varDec.getType());
		variableMapping.put(varDec.getName(), new Variable(varDec.getName(), typeDef, tracer.addTrace(varDec)));
		player.addVar(variableMapping.get(varDec.getName()));
	}

	public static TypeDef getTypeDef(VarType varType) {
		List<Integer> dimensions = TypeSystemUtils.sizeDefineToInt(varType.getDimensions());
		dimensions = ((dimensions.size() == 0) ? null : dimensions);
		Subrange subrange = varType.getSubr();
		EList<TypeConstant> consts = varType.getConst();

		if (subrange != null) {
			// it's an integer
			return new TypeDef(TypeSystemUtils.sizeDefineToInt(subrange.getFrom()),
					TypeSystemUtils.sizeDefineToInt(subrange.getTo()), dimensions);
		} else if (consts != null && consts.size() > 0) {
			// the var has user-defined values
			List<String> constNames = getConstNames(consts);
			return new TypeDef(constNames, dimensions);
		} else if (varType.getName() != null && varType.getName().equals("boolean")) {

			return new TypeDef(dimensions);
		}

		else if (varType.getType() != null) {
			// add dimensions of variable declaration
			TypeDef ref = getTypeDef(varType.getType().getType());
			if (dimensions != null) {
				List<Integer> refDim = ref.getDimensions();
				if (refDim == null) {
					refDim = new ArrayList<>();
				}
				dimensions.addAll(refDim);
				ref.setDimensions(dimensions);
			}
			return ref;
		}
		// shouldn't happen
		return null;
	}

	private static List<String> getConstNames(EList<TypeConstant> consts) {
		List<String> constNames = new ArrayList<>();
		for (TypeConstant tc : consts) {
			constNames.add(tc.getName());
		}
		return constNames;
	}

	/**
	 * create Monitor constraints
	 * 
	 * @param entitiesMapper
	 * @param tracer
	 * @param sm
	 * @return
	 * @throws SpectraTranslationException
	 */
	public static List<Constraint> computeMonitorExpressions(EntitiesMapper entitiesMapper, Tracer tracer,
			tau.smlab.syntech.spectra.Monitor sm) throws SpectraTranslationException {

		List<Constraint> exps = new ArrayList<>();

		for (TemporalExpression t : sm.getInitial()) {
			tracer.addTrace(t);
			Spec s;
			try {
				s = SpectraASTToSpecGenerator.getConstraintSpec(t, entitiesMapper, tracer, null, null, null).getSpec();
			} catch (SpectraTranslationException e) {
				e.setTraceId(tracer.getTrace(t));
				throw e;
			}
			exps.add(new Constraint(Kind.INI, s, "", tracer.getTrace(t)));
		}
		for (TemporalExpression t : sm.getSafety()) {
			tracer.addTrace(t);
			Spec s;
			try {
				s = SpectraASTToSpecGenerator.getConstraintSpec(t, entitiesMapper, tracer, null, null, null).getSpec();
			} catch (SpectraTranslationException e) {
				e.setTraceId(tracer.getTrace(t));
				throw e;
			}
			exps.add(new Constraint(Kind.SAFETY, s, "", tracer.getTrace(t)));
		}
		for (TemporalExpression t : sm.getStateInv()) {
			tracer.addTrace(t);
			Spec s;
			try {
				s = SpectraASTToSpecGenerator.getConstraintSpec(t, entitiesMapper, tracer, null, null, null).getSpec();
			} catch (SpectraTranslationException e) {
				e.setTraceId(tracer.getTrace(t));
				throw e;
			}
			exps.add(new Constraint(Kind.STATE_INV, s, "", tracer.getTrace(t)));
		}

		return exps;
	}

}
