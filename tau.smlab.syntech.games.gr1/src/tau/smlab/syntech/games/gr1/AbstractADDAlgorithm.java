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
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.jtlv.Env;

public abstract class AbstractADDAlgorithm {

	protected PlayerModule env;
	protected PlayerModule sys;
	protected Map<Integer, BDD> weights;

	public AbstractADDAlgorithm(PlayerModule env, PlayerModule sys, Map<Integer, BDD> weights) {
		this.env = env;
		this.sys = sys;
		this.weights = weights;
	}

	/**
	 * weighted arena
	 */
	private ADD arena;

	/**
	 * the winning region, consists of all states that are winning for the system
	 */
	private ADD winningRegion;

	/**
	 * the required energy level from states in the winning region
	 * 
	 */
	private ADD winningRegionEng;

	/**
	 * maximal initial energy (a bound for the algorithm)
	 */
	private double maxEng;

	public double getMaxEng() {
		return maxEng;
	}

	public void setMaxEng(double maxEng) {
		this.maxEng = maxEng;
	}

	public ADD getWeightedArena() {
		return this.arena;
	}

	public ADD getWinningRegion() {
		return this.winningRegion;
	}

	/**
	 * check that new energy values are the same as old energy values
	 * 
	 * @return
	 */
	protected boolean reachedFixPoint(ADD previous, ADD current) {
		if (current != null && previous != null) {
			return current.equals(previous);
		}
		return false;
	}

	protected void printInitialEnergy() {
		double[] initEngVals, engVals;
		ADD initStatesEng, nonInitStatesEng;
		ADD states = this.winningRegionEng;
		String printStr = "Winning region required energy levels";
		if (states != null) {
			initStatesEng = (ADD) (env.initial().and(sys.initial())).ite(states, Env.PLUS_INF());
			nonInitStatesEng = (ADD) (env.initial().and(sys.initial())).ite(Env.PLUS_INF(), states);
			engVals = nonInitStatesEng.getTerminalValues();
			initEngVals = initStatesEng.getTerminalValues();
			System.out.println("Initial states required energy levels:");
			for (int i = 0; i < initEngVals.length; i++) {
				if (initEngVals[i] != Env.PLUS_INF().getConstantValue()) {
					System.out.print(initEngVals[i] + ", ");
				}
			}
			System.out.println(System.lineSeparator());
			System.out.println(printStr);
			for (int i = 0; i < engVals.length; i++) {
				if (engVals[i] != Env.PLUS_INF().getConstantValue()) {
					System.out.print(engVals[i] + ", ");
				}
			}
			System.out.println(System.lineSeparator());
			initStatesEng.free();
		}
	}

	/**
	 * create arena with:
	 * <ul>
	 * <li>system deadlocks: -INF</li>
	 * <li>environment deadlocks: +INF</li>
	 * <li>all other transitions: weight as defined</li>
	 * <ul>
	 * 
	 * @return
	 */
	private ADD computeWeightedArena() {

		ADD deadlocks = (ADD) env.trans().ite(Env.MINUS_INF(), Env.PLUS_INF());

		ADD weightedArena = (ADD) Env.FALSE();
		for (Entry<Integer, BDD> w : weights.entrySet()) {
			ADD tmp = (ADD) w.getValue().ite(Env.CONST(w.getKey()), weightedArena);
			weightedArena.free();
			weightedArena = tmp;
		}

		BDD both = env.trans().and(sys.trans());
		arena = (ADD) both.ite(weightedArena, deadlocks);
		weightedArena.free();

		return arena;
	}

	public ADD computeWinningRegion() {

		this.arena = computeWeightedArena();

		winningRegionEng = computeWinningRegionEng();
		winningRegion = (ADD) toStates(winningRegionEng);

		return winningRegion;
	}

	protected BDD toStates(ADD s) {
		return s.toZeroOneSLTThreshold(Env.PLUS_INF().getConstantValue());
	}

	abstract protected ADD computeWinningRegionEng();

	/**
	 * computes the states from which the system can force to current on the given
	 * arena respecting the bound maxEng
	 * 
	 * @param current
	 * @param arena
	 * @return
	 */
	protected ADD cPre(ADD current) {
		ADD pCurrent = (ADD) Env.prime(current);

		ADD accEngByTarget = pCurrent.engSubtract(arena, maxEng);
		ADD accEngSysMin = accEngByTarget.abstractMin(sys.modulePrimeVars());
		ADD pre = accEngSysMin.abstractMax(env.modulePrimeVars());

		pCurrent.free();
		accEngByTarget.free();
		accEngSysMin.free();
		return pre;
	}

	public void free() {
		arena.free();
		winningRegion.free();
		winningRegionEng.free();
	}

	/**
	 * returns an ADD where states in BDD have value 0 and all others infinity
	 * 
	 * @param states
	 * @return
	 */
	protected ADD getADDForGoodStates(BDD states) {
		BDD neg = states.not();
		ADD res = ((ADD) neg).apply(Env.PLUS_INF(), BDDFactory.times);
		neg.free();
		return res;
	}

	/**
	 * intersection of sets of states represented by the ADD (implemented via
	 * maximum)
	 * 
	 * @param a1
	 * @param a2
	 * @return
	 */
	protected ADD inter(ADD a1, ADD a2) {
		return a1.apply(a2, BDDFactory.max);
	}

	/**
	 * intersection of sets of states represented by the ADD (implemented via
	 * maximum)
	 * 
	 * @param a1
	 * @param a2
	 * @return
	 */
	protected ADD interWith(ADD a1, ADD a2) {
		return a1.applyWith(a2, BDDFactory.max);
	}

	/**
	 * union of sets of states represented by the ADD (implemented via minimum)
	 * 
	 * @param a1
	 * @param a2
	 * @return
	 */
	protected ADD union(ADD a1, ADD a2) {
		return a1.apply(a2, BDDFactory.min);
	}

	/**
	 * union of sets of states represented by the ADD (implemented via minimum)
	 * 
	 * @param a1
	 * @param a2
	 * @return
	 */
	protected ADD unionWith(ADD a1, ADD a2) {
		return a1.applyWith(a2, BDDFactory.min);
	}

}
