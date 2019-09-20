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

package tau.smlab.syntech.games.gr1;

import java.util.Map;
import java.util.Map.Entry;

import net.sf.javabdd.ADD;
import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDFactory;
import tau.smlab.syntech.gameinput.spec.SpecException;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

public class BoundedEnergyLattice {

	protected static String ENG_VAR_NAME = "energyVal";

	protected int upperBound;
	protected ADD totalWeightFunc;
	protected double infVal;
	protected PlayerModule env;
	protected PlayerModule sys;
	protected ADD engValDom;

	/**
	 * A class that represents the semantics of Energy-mu-calculus w.r.t a Bounded
	 * Energy Weighted Game Structure (EGS). Each instance computes the total weight
	 * function of the EGS from the weight definition of the game model, and the
	 * upper bound given as input. Supports all the operations of the De-Morgan
	 * function algebra: meet (max), join (min), and negation. Features the Energy
	 * controllable predecessor operators of each of the players.
	 * 
	 * @param upperBound
	 *            - The (non-negative integer) upper bound of the Bounded Energy
	 *            objective
	 * @param model
	 *            - The game model that includes a weight definition from which the
	 *            total weight function is built
	 * @throws SpecException
	 *             - If the game model has a null weight definition
	 */
	public BoundedEnergyLattice(GameModel model, int upperBound) throws SpecException {
		this.upperBound = upperBound;
		this.infVal = getBottomValue();
		this.sys = model.getSys();
		this.env = model.getEnv();
		this.engValDom = null;
		if (model.getWeights() != null) {
			totalWeightFunc = this.computeTotalWeightFunction(model.getWeights());
		} else {
			throw new SpecException(
					"Cannot create an instance of an Energy lattice without specifying a weight definition.");
		}
	}

	public int getUpperBound() {
		return this.upperBound;
	}

	protected ADD computeTotalWeightFunction(Map<Integer, BDD> weightsMap) {
		ADD weightFunc = (ADD) Env.FALSE();
		ADD transTimesWeight;

		// first, compute the sum of (weight_i * trans_i)
		// added optimizations for weight values above upperBound and below -upperBound
		// that might reduce the number of terminal nodes of the resulting ADD

		for (Entry<Integer, BDD> curWeight : weightsMap.entrySet()) {
			if (curWeight.getKey() > this.upperBound) {
				// truncate weight value above upperBound to upperBound
				transTimesWeight = (ADD) curWeight.getValue().apply(Env.CONST(this.upperBound), BDDFactory.times);
			} else if (curWeight.getKey() < -this.upperBound) {
				// mark weight value below -upperBound as losing for the system player (-INF)
				transTimesWeight = (ADD) curWeight.getValue().apply(Env.MINUS_INF(), BDDFactory.times);
			} else { // curWeight.getKey() is in [-upperBound, upperBound]
				transTimesWeight = (ADD) curWeight.getValue().apply(Env.CONST(curWeight.getKey()), BDDFactory.times);
			}
			weightFunc.applyWith(transTimesWeight, BDDFactory.plus);
		}
		// make the weight function total by mapping illegal transition for each of the
		// players to designated +INF and -INF values
		ADD sysWeightedTrans = (ADD) env.trans().and(sys.trans().not()).ite(Env.MINUS_INF(), weightFunc);
		weightFunc.free();
		weightFunc = (ADD) env.trans().not().ite(Env.PLUS_INF(), sysWeightedTrans);

		sysWeightedTrans.free();

		return weightFunc;

	}

	/**
	 * Returns the bottom element of the Energy lattice. That is, the constant
	 * function that maps all states to +INF.
	 * 
	 * @return An ADD representing the +INF constant function.
	 */
	public ADD getBottomEngFunc() {
		return Env.PLUS_INF();
	}

	/**
	 * Returns the top element of the Energy lattice. That is, the constant function
	 * that maps all states to 0.
	 * 
	 * @return An ADD representing the 0 constant function.
	 */
	public ADD getTopEngFunc() {
		return (ADD) Env.FALSE();
	}

	/**
	 * @return The total weight function of the EGS.
	 */
	public ADD getTotalWeightFunc() {
		return this.totalWeightFunc;
	}

	/**
	 * Performs a join operation, i.e., the minimum operation, taken pointwise.
	 * 
	 * @param engFunc1
	 * @param engFunc2
	 * @return
	 */
	public ADD join(ADD engFunc1, ADD engFunc2) {
		return engFunc1.apply(engFunc2, BDDFactory.min);
	}

	/**
	 * Performs a join operation, i.e., the minimum operation, taken pointwise.
	 * "engFunc1" becomes the result of the operation. "engFunc2" is consumed and no
	 * longer can be used.
	 * 
	 * @param engFunc1
	 * @param engFunc2
	 * @return
	 */
	public ADD joinWith(ADD engFunc1, ADD engFunc2) {
		return engFunc1.applyWith(engFunc2, BDDFactory.min);
	}

	/**
	 * Performs a meet operation, i.e., the maximum operation, taken pointwise.
	 * 
	 * @param engFunc1
	 * @param engFunc2
	 * @return
	 */
	public ADD meet(ADD engFunc1, ADD engFunc2) {
		return engFunc1.apply(engFunc2, BDDFactory.max);
	}

	/**
	 * Performs a meet operation, i.e., the maximum operation, taken pointwise.
	 * "engFunc1" becomes the result of the operation. "engFunc2" is consumed and no
	 * longer can be used.
	 * 
	 * @param engFunc1
	 * @param engFunc2
	 * @return
	 */
	public ADD meetWith(ADD engFunc1, ADD engFunc2) {
		return engFunc1.applyWith(engFunc2, BDDFactory.max);
	}

	/**
	 * Returns the complement of "engFunc". The negation operation neg is defined on
	 * each terminal node with value val as follows: if val == 0, then neg(val) ==
	 * +INF if val == +INF then neg(val) == 0 if val > 0 and val <= c then neg(val)
	 * == upperBound+1 - val
	 * 
	 * @param engFunc
	 * @return
	 */
	public ADD negate(ADD engFunc) {
		// TODO: Change to a native implementation (for better performance). Currently,
		// implemented on the Java level.

		// create two sets of states: mapped to 0 and +INF, respectively
		ADD zeroEngStates = engFunc.toZeroOneLTThreshold(0);
		ADD infEngStates = engFunc.toZeroOneGTThreshold(this.infVal);

		// perform negation for states that are mapped to val in (0,upperBound] by
		// mapping each to (upperBound+1 - val)
		ADD upperBoundPlusOne = Env.CONST(this.upperBound + 1);
		ADD negFinitePositiveEngValues = upperBoundPlusOne.apply(engFunc, BDDFactory.minus);

		// perform negation by mapping 0 energy states to +INF, and vice versa.
		ADD negNonZeroEngStates = infEngStates.ite(Env.FALSE(), negFinitePositiveEngValues);
		ADD negEngFunc = zeroEngStates.ite(Env.PLUS_INF(), negNonZeroEngStates);

		upperBoundPlusOne.free();
		zeroEngStates.free();
		infEngStates.free();
		negFinitePositiveEngValues.free();
		negNonZeroEngStates.free();

		return negEngFunc;
	}

	/**
	 * Computes the Energy controllable predecessor operator of the system player.
	 * Informally, it computes for every state the minimum initial credit required
	 * for the system player to force the environment player into "engFunc" in a
	 * single step, and to maintain the Bounded Energy objective.
	 * 
	 * @param engFunc
	 * @return
	 */
	public ADD sysECpre(ADD engFunc) {
		ADD primedEngFunc = (ADD) Env.prime(engFunc);
		ADD reqEng = primedEngFunc.engSubtract(this.totalWeightFunc, this.upperBound);
		ADD reqEngAbsSysMin = reqEng.abstractMin(sys.modulePrimeVars());
		ADD sysECPreRes = reqEngAbsSysMin.abstractMax(env.modulePrimeVars());

		primedEngFunc.free();
		reqEng.free();
		reqEngAbsSysMin.free();
		return sysECPreRes;
	}

	/**
	 * Computes the Energy controllable predecessor operator of the environment
	 * player. Informally, it computes for every state the maximum initial credit
	 * that allows the environment player to force the system player into "engFunc"
	 * in a single step, or to violate the Bounded Energy objective.
	 * 
	 * @param engFunc
	 * @return
	 */
	public ADD envECpre(ADD engFunc) {
		// TODO: Change to a native implementation (for better performance). Currently,
		// implemented on the Java level.
		// the implementation should be of the dual function of engSubtract(...)
		ADD negEngFunc = this.negate(engFunc);
		ADD sysECPreRes = this.sysECpre(negEngFunc);
		ADD envECpreRes = this.negate(sysECPreRes);

		negEngFunc.free();
		sysECPreRes.free();

		return envECpreRes;
	}

	/**
	 * Translates the specified 0-1 (Boolean) ADD to Energy mu-calculus semantics,
	 * i.e., replaces all 0 terminals by +INF, and all 1 terminals by 0.
	 * 
	 * @param booleanFunc
	 *            0-1 ADD
	 * @return
	 */
	public ADD toEnergySemantics(ADD booleanFunc) {
		return booleanFunc.ite(Env.FALSE(), Env.PLUS_INF());
	}

	/**
	 * Translates the specified 0-1 (Boolean) ADD to Energy mu-calculus semantics,
	 * and negates it in a single step. That is, replaces all 1 terminals by +INF
	 * terminals, while all 0 terminals remain the same.
	 * 
	 * @param booleanFunc
	 *            0-1 ADD
	 * @return
	 */
	public ADD toEnergyNegatedSemantics(ADD booleanFunc) {
		return booleanFunc.ite(Env.PLUS_INF(), Env.FALSE());
	}

	/**
	 * Derives the set of states winning for the system from the function (ADD) that
	 * maps each state to the minimum initial credit for which the system wins from
	 * that state.
	 * 
	 * @param sysWinningEng
	 * @return
	 */
	public BDD deriveSysWinningStates(ADD sysWinningEng) {
		return sysWinningEng.toZeroOneSLTThreshold(this.infVal);
	}

	/**
	 * Derives the set of states winning for the environment from the function (ADD)
	 * that maps each state to an Energy lattice value x where (upperBound-x) is the
	 * maximum initial credit for which the environment wins from that state.
	 * 
	 * @param envWinningEng
	 * @return
	 */
	public BDD deriveEnvWinningStates(ADD envWinningEng) {
		return envWinningEng.toZeroOneLTThreshold(0);
	}

	/**
	 * Translates the specified Energy function ADD to a 0-1 ADD that encodes the
	 * terminal values in Boolean (BDD) variables added to its domain.
	 * 
	 * @param engFunc
	 * @return
	 */
	public BDD flattenEnergyValuesToEngVariable(ADD engFunc) {

		if (this.engValDom == null) {
			this.engValDom = this.mapEngVarToTerminalValues();
		}

		ADD negResult = this.engValDom.apply(engFunc, BDDFactory.less);
		ADD result = negResult.not();

		// restrict the assignments of ENG_VAR_NAME to its exact domain size
		BDD dom = Env.getVar(ENG_VAR_NAME).getDomain().domain();
		result.andWith(dom);

		negResult.free();

		return result;
	}

	private ADD mapEngVarToTerminalValues() {
		if (!sys.hasVar("energyVal")) {
			try {
				sys.addVar(ENG_VAR_NAME, 0, this.upperBound, true);
			} catch (Exception e) {
				throw new RuntimeException("Could not add variable " + ENG_VAR_NAME + " : " + e.getMessage());
			}
		}

		ModuleBDDField energyVal = Env.getVar(ENG_VAR_NAME);

		int[] ivar = energyVal.getDomain().vars();

		// create an ADD that enumerates all the values in the domain of ENG_VAR_NAME.
		// assignments to variable ENG_VAR_NAME correspond to the terminal nodes of the
		// ADD.
		// iterate over the domain of ENG_VAR_NAME from least to most significant bit:
		// in the i'th iteration, add 2^i to the ADD engValDom (initialized to the
		// constant function 0)
		ADD ithVar, engValDom = (ADD) Env.FALSE();
		int ithBitVal = 1;
		for (int n = 0; n < ivar.length; n++) {
			ithVar = (ADD) energyVal.getDomain().getFactory().ithVar(ivar[n]);
			ithVar.applyWith(Env.CONST(ithBitVal), BDDFactory.times);
			engValDom.applyWith(ithVar, BDDFactory.plus);
			ithBitVal = ithBitVal << 1;
		}

		// Constrain to the exact domain size - an alternative to the BDD based domain
		// restriction

		// ADD upperBoundFunc = Env.CONST(this.upperBound+1);
		// ADD terminalsInDom = engValDom.apply(upperBoundFunc, BDDFactory.less);
		// ADD result = terminalsInDom.ite(engValDom, Env.CONST(-1));
		// upperBoundFunc.free();
		// engValDom.free();
		// terminalsInDom.free();
		// return result;

		return engValDom;
	}

	/**
	 * Converts an Energy mu-calculus ADD of the environment player to maximum
	 * initial credits. That is, each terminal node x is replaced with (upperBound -
	 * x).
	 * 
	 * @param engFunc
	 * @return
	 */
	public ADD toMaximumInitialCredits(ADD engFunc) {
		ADD upperBoundFunc = Env.CONST(upperBound);
		ADD maxEngFunc = upperBoundFunc.apply(engFunc, BDDFactory.minus);
		upperBoundFunc.free();
		return maxEngFunc;
	}

	/**
	 * Checks if the specified value equals the lattice bottom element's value,
	 * i.e., +INF.
	 * 
	 * @param value
	 * @return
	 */
	public boolean notBottomValue(double value) {
		return value != this.infVal;
	}

	private double getBottomValue() {
		ADD bottom = Env.PLUS_INF();
		double infVal = bottom.getConstantValue();
		bottom.free();
		return infVal;
	}
}
