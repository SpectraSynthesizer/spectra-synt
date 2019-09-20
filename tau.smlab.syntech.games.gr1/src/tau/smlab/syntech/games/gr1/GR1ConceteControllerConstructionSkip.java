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

import java.util.Stack;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDD.BDDIterator;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.games.AbstractGamesException;
import tau.smlab.syntech.games.controller.enumerate.ConcreteControllerConstruction;
import tau.smlab.syntech.games.controller.enumerate.EnumStateI;
import tau.smlab.syntech.games.controller.enumerate.EnumStrategyI;
import tau.smlab.syntech.games.controller.enumerate.EnumStrategyImpl;
import tau.smlab.syntech.jtlv.CoreUtil;
import tau.smlab.syntech.jtlv.Env;

public class GR1ConceteControllerConstructionSkip extends ConcreteControllerConstruction {

	private GR1Memory mem;
	private GR1StrategyType kind;

	public GR1ConceteControllerConstructionSkip(GR1Memory mem, GameModel m, GR1StrategyType kind) {
		super(mem, m);
		this.mem = mem;
		this.kind = kind;
	}

	public GR1ConceteControllerConstructionSkip(GR1Memory mem, GameModel m) {
		super(mem, m);
		this.mem = mem;
		this.kind = GR1StrategyType.ZYX;
	}

	@Override
	public EnumStrategyI calculateConcreteController() throws AbstractGamesException {
		return this.calculateConcreteController(false);
	}

	public EnumStrategyI calculateConcreteController(boolean calcLongestSimplePath) throws AbstractGamesException {
		if (!(kind instanceof GR1StrategyType))
			return null;
		int strategy_kind = ((GR1StrategyType) kind).old_value();
		Stack<EnumStateI> st_stack = new Stack<EnumStateI>();
		Stack<Integer> j_stack = new Stack<Integer>();
		Stack<Integer> cy_stack = new Stack<Integer>();

		BDDVarSet envUnprimedVars = env.moduleUnprimeVars();
		BDDVarSet sysUnprimedVars = sys.moduleUnprimeVars();

		EnumStrategyImpl aut = new EnumStrategyImpl(calcLongestSimplePath);

		System.out.println("calculateNewStrategyNewSkip start with node num = " + Env.TRUE().getFactory().getNodeNum());

		// for all initial env states find one possible sys state and add these on
		// the stack with rank 0

		for (BDDIterator it = env.initial().iterator(envUnprimedVars); it.hasNext();) {
			// TODO make this faster by keeping envIni symbolic: search "closest" envIni
			// states, then iterate (this will save individual searches for "closest")
			BDD envIni = it.nextBDD();
			BDD iniWin = envIni.andWith(getWinningInitialStates());
			int cy = 0;
			BDD closest = Env.FALSE();
			// get to the closest state to go to next justice
			while (closest.isZero()) {
				closest.free();
				closest = iniWin.and(mem.y_mem[0][cy++]);
			}
			iniWin.free();
			BDD oneIni = CoreUtil.satOne(closest, envUnprimedVars.union(sysUnprimedVars));
			closest.free();
			st_stack.push(aut.getState(oneIni, new GR1RankInfo(0)));
			cy_stack.push(cy);
			j_stack.push(new Integer(0));
		}

		System.out.println("Initial states of environment: " + st_stack.size());

		// iterating over the stacks.
		while (!st_stack.isEmpty()) {
			// making a new entry.
			EnumStateI new_state = st_stack.pop();
			BDD p_st = new_state.getData();
			int p_j = j_stack.pop();
			int p_cy = cy_stack.pop();

			int succ_cy = 0;

			assert p_cy >= 0 : "Couldn't find p_cy";

			int p_i = -1;
			for (int i = 0; i < env.justiceNum(); i++) {
				BDD b = p_st.and(mem.x_mem[p_j][i][p_cy]);
				if (!b.isZero()) {
					p_i = i;
					b.free();
					break;
				}
				b.free();
			}
			assert p_i >= 0 : "Couldn't find p_i";

			BDD all_succs = env.succ(p_st);

			// For each env successor, find a strategy successor
			for (BDDIterator iter_succ = all_succs.iterator(envUnprimedVars); iter_succ.hasNext();) {
				BDD primed_cur_succ = Env.prime(iter_succ.nextBDD());
				BDD next_s = sys.trans().and(p_st).andWith(primed_cur_succ)
						.exist(envUnprimedVars.union(sysUnprimedVars));
				BDD next_op = Env.unprime(next_s);
				next_s.free();
				BDD candidate = Env.FALSE();
				int jcand = p_j;

				int local_kind = strategy_kind;
				while (candidate.isZero() & (local_kind >= 0)) {
					// a - first successor option in the strategy.
					// just satisfied a justice goal go to next
					if ((local_kind == 3) | (local_kind == 7) | (local_kind == 10) | (local_kind == 13)
							| (local_kind == 18) | (local_kind == 21)) {
						int next_p_j = (p_j + 1) % sys.justiceNum();

						BDD justiceAndSt = p_st.and(sys.justiceAt(p_j));
						if (!justiceAndSt.isZero()) {
							BDD opt = next_op.and(mem.z_mem[next_p_j]);
							if (!opt.isZero()) {
								// prefer existing states
								BDD optEx = opt.and(aut.getStatesOfRank(new GR1RankInfo(next_p_j)));
								if (!optEx.isZero()) {
									opt.free();
									opt = optEx;
								}
								int steps = 0;
								BDD closest = Env.FALSE();
								// get to the closest state to go to next justice
								while (closest.isZero()) {
									closest.free();
									closest = opt.and(mem.y_mem[next_p_j][steps++]);
								}
								candidate = closest;
								jcand = next_p_j;
								succ_cy = steps;
							}
							opt.free();
						}
						justiceAndSt.free();
					}
					// b - second successor option in the strategy.
					// get closest to satisfying current system justice goal
					if ((local_kind == 2) | (local_kind == 5) | (local_kind == 11) | (local_kind == 15)
							| (local_kind == 17) | (local_kind == 22)) {
						if (p_cy > 0) {
							int look_r = 0;
							// look for the fairest r.
							BDD opt = next_op.and(mem.y_mem[p_j][look_r]);
							while (opt.isZero() & (look_r < p_cy)) {
								look_r++;
								opt.free();
								opt = next_op.and(mem.y_mem[p_j][look_r]);
							}

							if ((look_r != p_cy) && (!opt.isZero())) {
								candidate = opt.id();
								succ_cy = look_r;
							}
							opt.free();
						}
					}

					// c - third successor option in the strategy.
					// preventing env justice goal
					if ((local_kind == 1) | (local_kind == 6) | (local_kind == 9) | (local_kind == 14)
							| (local_kind == 19) | (local_kind == 23)) {
						BDD notJustSt = p_st.id().andWith(env.justiceAt(p_i).not());
						if (!notJustSt.isZero()) {
							BDD opt = next_op.and(mem.x_mem[p_j][p_i][p_cy]);
							if (!opt.isZero()) {
								candidate = opt.id();
								succ_cy = p_cy;
								// does not always succeed to prevent, e.g., when justice
								// of env satisfied by successor already
								BDD notJustCand = candidate.id().andWith(env.justiceAt(p_i).not());
								notJustCand.free();
							}
							opt.free();
						}
						notJustSt.free();
					}

					// no successor was found yet.
					assert ((local_kind != 0) & (local_kind != 4) & (local_kind != 8) & (local_kind != 12)
							& (local_kind != 16) & (local_kind != 20)) : "No successor was found";

					local_kind--;
				}

				BDD candGoalEx = candidate.and(aut.getStatesOfRank(new GR1RankInfo(jcand)))
						.andWith(sys.justiceAt(jcand).id());
				BDD candGoalNew = candidate.and(sys.justiceAt(jcand));
				BDD candEx = candidate.and(aut.getStatesOfRank(new GR1RankInfo(jcand)));

				if (!candGoalEx.isZero()) {
					// 1st option take one that reaches goal if possible and exists
					candidate.free();
					candidate = candGoalEx;
					candGoalNew.free();
					candEx.free();
				} else if (!candGoalNew.isZero()) {
					// 2nd option take one that reaches goal if possible (even multiple)
					BDD skip = sys.justiceAt(jcand).id();
					int skipToJustice = (jcand + 1) % sys.justiceNum();
					// TODO check about the BDD z of winning states
					BDD skipTo = candGoalNew.and(sys.justiceAt(skipToJustice));
					while (jcand != skipToJustice && !skipTo.isZero()) {
						skip.andWith(sys.justiceAt(skipToJustice).id());
						skipToJustice = (skipToJustice + 1) % sys.justiceNum();
						skipTo = candidate.id().andWith(skip.id()).andWith(sys.justiceAt(skipToJustice).id());
					}
					candidate.andWith(skip);
					skipTo.free();
					candGoalEx.free();
					candGoalNew.free();
					candEx.free();
					jcand = (skipToJustice + sys.justiceNum() - 1) % sys.justiceNum();
					succ_cy = 0;
				} else if (!candEx.isZero()) {
					// 3rd option take an existing one
					candidate.free();
					candidate = candEx;
					candGoalEx.free();
					candGoalNew.free();
				}

				BDD one_cand = CoreUtil.satOne(candidate, envUnprimedVars.union(sysUnprimedVars));
				candidate.free();
				BDD tmp = one_cand.and(sys.justiceAt(jcand));
				tmp.free();
				// add succ
				EnumStateI succ = aut.addSuccessorState(new_state, one_cand, new GR1RankInfo(jcand));
				if (succ != null) {
					// if a new state was created.
					st_stack.push(succ);
					cy_stack.push(succ_cy);
					j_stack.push(jcand);
				}
			}
			all_succs.free();
		}

		System.out.println("calculateNewStrategyNewSkip end with node num = " + Env.TRUE().getFactory().getNodeNum());

		return aut;
	}

	private BDD getWinningInitialStates() {
		return mem.getWin().and(sys.initial());
	}

}
