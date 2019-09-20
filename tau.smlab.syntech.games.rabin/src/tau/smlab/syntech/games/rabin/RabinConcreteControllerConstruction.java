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

package tau.smlab.syntech.games.rabin;

import java.util.Stack;
import java.util.Vector;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import net.sf.javabdd.BDD.BDDIterator;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.games.AbstractGamesException;
import tau.smlab.syntech.games.GamesStrategyException;
import tau.smlab.syntech.games.controller.enumerate.ConcreteControllerConstruction;
import tau.smlab.syntech.games.controller.enumerate.EnumStateI;
import tau.smlab.syntech.games.controller.enumerate.EnumStrategyI;
import tau.smlab.syntech.games.controller.enumerate.EnumStrategyImpl;
import tau.smlab.syntech.jtlv.Env;

public class RabinConcreteControllerConstruction extends ConcreteControllerConstruction {
	protected RabinMemory mem;

	public RabinConcreteControllerConstruction(RabinMemory mem, GameModel m) {
		super(mem, m);
		this.mem = mem;
	}

	@Override
	public EnumStrategyI calculateConcreteController() throws AbstractGamesException {
		return this.calculateConcreteController(false);
	}

	public EnumStrategyI calculateConcreteController(boolean calcLongestSimplePath) throws AbstractGamesException {
		System.out.println("extract strategy - Start");

		if (mem.getWin() == null || mem.getWin().isFree()) {
			throw new GamesStrategyException("BDD of winning states invalid.");
		}

		Stack<EnumStateI> st_stack = new Stack<EnumStateI>();
		EnumStrategyI aut = new EnumStrategyImpl(calcLongestSimplePath);

		BDD sysDeadOrEnvWin = sys.initial().imp(mem.getWin());
		BDD allIni = env.initial().id().andWith(sysDeadOrEnvWin.forAll(sys.moduleUnprimeVars()));
		sysDeadOrEnvWin.free();
		if (allIni.isZero()) {
			throw new GamesStrategyException("No environment winning states.");
		}
		// select one env choice
		BDD ini = (BDD) allIni.iterator(env.moduleUnprimeVars()).next();
		allIni.free();
		// add all system choices
		ini.andWith(sys.initial().id());

		for (BDDIterator iter = ini.iterator(allUnprime()); iter.hasNext();) {
			BDD cand = iter.nextBDD();
			int iniZ = mem.getZRank(cand);
			int iniK = 0;
			int iniX = mem.getXRank(iniZ, iniK, cand);
			RabinRankInfo iniR = new RabinRankInfo(iniZ, iniK, iniX);
			st_stack.push(aut.getState(cand, iniR));
		}

		try {
			// int state_counter = 0;
			while (!st_stack.isEmpty()) {
				// System.out.println("num of states is: " + state_counter++);
				EnumStateI st = st_stack.pop();
				st_stack.addAll(getSysStepsToEnvChoice(aut, st));
			}
		} catch (AbstractGamesException e) {
			System.err.println("failed generating counter strategy");
			e.printStackTrace();
		}
		System.out.println("extract strategy - End");
		return aut;
	}

	// returns a vector of states representation of the next environment step
	// from a given state, and the possible set of system responses.
	private Vector<EnumStateI> getSysStepsToEnvChoice(EnumStrategyI aut, EnumStateI state)
			throws AbstractGamesException {
		if (!(state.get_rank_info() instanceof RabinRankInfo))
			throw new GamesStrategyException("cannot build rabin automata for" + "streett state");
		RabinRankInfo rri = (RabinRankInfo) state.get_rank_info();

		// int zRank = rri.get_row_count();
		// int kGoal = rri.get_env_goal();
		// int kRank = rri.get_env_goal_count();
		//
		// System.out.println("is state " + state.get_id() + " winning? "
		// + !state.get_state().and(player1_winning).isZero() + " env"
		// + kGoal + ":" + kRank + " sys rank:" + zRank);
		// System.out.println("for state " + state.get_id()
		// + " the row ranks is :" + zRank + " the conjunct rank is:"
		// + kRank);
		//

		// ////////////////////////////////////////////////////////////////////
		// ////////////////////////////////////////////////////////////////////
		// ////////////////////////////////////////////////////////////////////
		// The general idea is the first try to move as low as possible in the
		// vector of z (the lowest possible zRank). If not possible then trying
		// to move as low as possible in the relevant vector of K, and if
		// reached the lowest K, moving to the next K. Namely:
		// 1) lowering Z. // not a relevant value... can return immediately.
		// 2) if reached lowest K moving to the next K.
		// 3) try to lower K.
		// 4) not found good choice yet.
		// int priority = 4;
		// ////////////////////////////////////////////////////////////////////

		Vector<EnumStateI> ret = null;
		BDD envSuccs = env.succ(state.getData());
		BDD bestSuccs = Env.FALSE();
		int bestKind = 4;
		for (BDDIterator iter = envSuccs.iterator(env.moduleUnprimeVars()); iter.hasNext();) {
			BDD envSucc = (BDD) iter.next();

			BDD sysSuccs = sys.succ(state.getData()).and(envSucc);
			BDD succSysWin = sysSuccs.and(mem.getWin().not());
			if (!succSysWin.isZero()) {
				succSysWin.free();
				continue; // not one of the relevant environment choices.
			}
			succSysWin.free();

			int kind = getBestKind(state, rri, sysSuccs);

			if (kind == -1)
				// not a relevant env choice - one of the sys choices leads to a higher z-rank
				continue;

			if (kind == 1)
				// all sys choices for this env choice lead to a lower z-rank - this env choice
				// can force to a lower z-rank
				return calcResponses(aut, state, rri, sysSuccs, kind);

			if (kind < bestKind) {
				bestKind = kind;
				bestSuccs = sysSuccs;
			}
		}

		if (bestKind == -1)
			throw new GamesStrategyException(
					"No environment successor found from" + " state " + state.getData().toString());

		ret = calcResponses(aut, state, rri, bestSuccs, bestKind);

		if (ret == null)
			throw new GamesStrategyException(
					"No environment successor found from" + " state " + state.getData().toString());

		return ret;
		// throw new
		// GameException("we have a problem in succs of state "
		// + state.get_state().toString() + ". One of the next "
		// + "step Z rank is not decreasing");
		// throw new GameException("we have a problem in succs of state "
		// + state.get_state().toString() + ". One of the next "
		// + "step X rank is not decreasing (the Z is identical)");
	}

	// Kinds:
	// 1 - all system choices for this env choice lead to a lower z-rank
	// 2 - all system choices for this env choice either lead to a lower z-rank or
	// not,
	// and at least one sys choice doesn't lead to lower z-rank and we've reached
	// the final cell
	// (cell 0) in the current x-matrix row.
	// 3 - At least one sys choice for this env choice doesn't lead to lower z-rank
	// and we're in
	// the middle of the current x-matrix row, and this sys choice leads to some
	// lower x-cell in this
	// row
	// -1 - at least one sys choice leads to a higher z-rank
	private int getBestKind(EnumStateI state, RabinRankInfo rri, BDD succs) {
		int ret_kind = 1;
		for (BDDIterator iter = succs.iterator(allUnprime()); iter.hasNext();) {
			BDD cand = iter.nextBDD();

			int nextZ, nextK, nextX;
			nextZ = mem.getZRank(cand);
			if (nextZ > rri.get_row_count())
				return -1;

			if (nextZ < rri.get_row_count())
				continue;

			if (rri.get_env_goal_count() == 0) {
				// TODO - I think this internal if is redundant - add some assert to ensure
				// it indeed can't be > 2 and remove this internal if (just set ret_kind = 2)
				if (ret_kind <= 2)
					ret_kind = 2;
				continue;
			}

			nextK = rri.get_env_goal();
			nextX = mem.getXRank(nextZ, nextK, cand);
			if (nextX >= rri.get_env_goal_count())
				return -1;
			ret_kind = 3;
		}

		return ret_kind;
	}

	/**
	 * @param aut
	 * @param state
	 * @param rri
	 * @param succs
	 * @param kind
	 *            the priority from which this next step is allowed (or below). 1 -
	 *            dec row, 2 - next K, 3 - dec K.
	 * @param monitoring_expr
	 * @return The vector of next states, or null if not a valid set of succ.
	 * @throws GameException
	 */
	private Vector<EnumStateI> calcResponses(EnumStrategyI aut, EnumStateI state, RabinRankInfo rri, BDD succs,
			int kind) throws AbstractGamesException {
		Vector<EnumStateI> ret = new Vector<EnumStateI>();
		for (BDDIterator iter = succs.iterator(allUnprime()); iter.hasNext();) {
			BDD cand = iter.nextBDD();

			int nextZ, nextK, nextX;
			RabinRankInfo nextR;

			nextZ = mem.getZRank(cand);
			// if cannot move forward
			if (nextZ > rri.get_row_count())
				return null;

			// else if reducing Z rank
			if (nextZ < rri.get_row_count()) {
				nextK = 0;
				nextX = mem.getXRank(nextZ, nextK, cand);
				nextR = new RabinRankInfo(nextZ, nextK, nextX);
				EnumStateI newSt = aut.addSuccessorState(state, cand, nextR);
				if (newSt != null) // really a new state.
					ret.add(newSt);
				continue;
			}

			if (kind <= 1)
				return null;

			// else nextZRank == rri.get_row_count(), namely, cannot reduce Z,
			// and if we reached the K goal (suppose to satisfy the environment
			// justice K)
			if (rri.get_env_goal_count() == 0) {
				nextK = (rri.get_env_goal() + 1) % env.justiceNum();
				nextX = mem.getXRank(nextZ, nextK, cand);
				nextR = new RabinRankInfo(nextZ, nextK, nextX);
				EnumStateI newSt = aut.addSuccessorState(state, cand, nextR);
				if (newSt != null) // really a new state.
					ret.add(newSt);
				continue;
			}

			if (kind <= 2)
				return null;

			// else still did not reach the K goal
			nextK = rri.get_env_goal();
			nextX = mem.getXRank(nextZ, nextK, cand);
			if (nextX >= rri.get_env_goal_count())
				return null;
			nextR = new RabinRankInfo(nextZ, nextK, nextX);
			EnumStateI newSt = aut.addSuccessorState(state, cand, nextR);
			if (newSt != null) // really a new state.
				ret.add(newSt);
			continue;
		}

		return ret;
	}

	private BDDVarSet allUnprime() {
		return sys.moduleUnprimeVars().union(env.moduleUnprimeVars());
	}

}
