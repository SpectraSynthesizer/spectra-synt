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

package tau.smlab.syntech.games.controller.enumerate.printers;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import dk.brics.automaton.Automaton;
import dk.brics.automaton.State;
import dk.brics.automaton.Transition;
import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.games.controller.enumerate.EnumStateI;
import tau.smlab.syntech.games.controller.enumerate.EnumStrategyI;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

public class MAAMinimizeAutomatonPrinter implements EnumStrategyPrinter {
	private PlayerModule sys;
	private PlayerModule env;
	/**
	 * if true states without successors will be removed, i.e., (I) for counter
	 * strategies losing moves are not possible and (II) for strategies system will
	 * not abort doing something useful once environment does something bad
	 */
	public static boolean REMOVE_DEAD_STATES = false;
	public static boolean ADD_LIVENESS_GOALS_INFO = false;

	private Map<String, Character> triggers = new HashMap<String, Character>();

	public MAAMinimizeAutomatonPrinter(GameModel m) {
		this.env = m.getEnv();
		this.sys = m.getSys();
	}

	@Override
	public void printController(PrintStream out, EnumStrategyI c) {
		out.println("ports:");

		for (String varName : getRelevantVarNames(env)) {
			out.print("  in " + varName + " = {");
			List<String> vals = Env.getValueNames(varName);
			for (int i = 0; i < vals.size(); i++) {
				out.print(vals.get(i));
				if (i < vals.size() - 1) {
					out.print(",");
				} else {
					out.println("}");
				}
			}
		}
		out.println();

		for (String varName : getRelevantVarNames(sys)) {
			out.print("  out " + varName + " = {");
			List<String> vals = Env.getValueNames(varName);
			for (int i = 0; i < vals.size(); i++) {
				out.print(vals.get(i));
				if (i < vals.size() - 1) {
					out.print(",");
				} else {
					out.println("}");
				}
			}
		}
		out.println();

		out.println("automaton:");

		Map<Integer, State> states = new HashMap<Integer, State>();
		for (int i = 0; i < c.numOfStates(); i++) {
			State s = new State();
			if (!REMOVE_DEAD_STATES || hasSuccessors(c.getState(i))) {
				s.setAccept(true);
				states.put(i, s);
			}
		}
		State initial = new State();
		State dead = new State();
		dead.setAccept(true);
		

		for (int i = 0; i < c.numOfStates(); i++) {
			State s = states.get(i);
			if (s != null) {
				EnumStateI es = c.getState(i);
				if (es.isInitial()) {
					// add pseudo state to be initial state
					Transition t = new Transition(getTrigger(es), s);
					initial.addTransition(t);
				}
				for (EnumStateI esucc : es.getSuccessors()) {
					State succ = states.get(esucc.getStateId());
					if (succ != null) {
						Transition t = new Transition(getTrigger(esucc), succ);
						s.addTransition(t);
					}
				}
			}
		}

		Automaton a = new Automaton();
		a.setInitialState(initial);

		int before = c.numOfStates();
		int deadStates = c.numOfStates() - a.getNumberOfStates() + 1;

		if (!a.isDeterministic()) {
			throw new RuntimeException("automaton not deterministic");
		}

		a.minimize();

		out.println("// number of states reduced from " + before + " to " + a.getStates().size()
				+ " by DFA minimization of strategy.");
		if (REMOVE_DEAD_STATES) {
			out.println("// removed " + deadStates + " states without successors before minimization");
		}

		out.print("state INI[initial]");
		int i = 0;
		Map<State, String> miniStates = new HashMap<State, String>();
		for (State s : a.getStates()) {
			String name = null;
			if (s.equals(a.getInitialState())) {
				// already printed
				name = "INI";
			} else if (s.getTransitions().size() == 0) {
				name = "DEAD";
				if (!miniStates.containsValue("DEAD")) {
					out.print(", " + name);
				}
			} else {
				name = "S" + i;
				out.print(", " + name);
				i++;
			}
			miniStates.put(s, name);
		}

		out.print(";");
		out.println();
		out.println();

		for (State s : a.getStates()) {
			for (Transition t : s.getTransitions()) {
				String transitionLabel = "";
				for (char trigger = t.getMin(); trigger <= t.getMax(); trigger++) {
					out.print(miniStates.get(s) + " -> " + miniStates.get(t.getDest()) + " ");
					for (Entry<String, Character> e : triggers.entrySet()) {
						if (e.getValue().equals(trigger)) {
							out.println(e.getKey());
							transitionLabel += e.getKey() + " ";
						}
					}
				}
				transitionLabel = transitionLabel.substring(0, transitionLabel.length() - 2);
			}
		}
	}

	/**
	 * computes names of variables that are not aux vars
	 * 
	 * @param p
	 * @return
	 */
	private List<String> getRelevantVarNames(PlayerModule p) {
		List<String> varNames = new ArrayList<String>();
		for (ModuleBDDField f : p.getNonAuxFields()) {
			varNames.add(f.getName());
		}
		return varNames;
	}

	private char getTrigger(EnumStateI esucc) {
		String trigger = getIO(esucc);
		if (!triggers.containsKey(trigger)) {
			triggers.put(trigger, (char) triggers.size());
		}
		return triggers.get(trigger);
	}

	private String getIO(EnumStateI s) {

		List<String> inputs = getRelevantVarNames(env);
		List<String> outputs = getRelevantVarNames(sys);

		String io = "{" + getSelectedAssignmentStrings(s, inputs) + "}";
		io += " / {" + getSelectedAssignmentStrings(s, outputs) + "};";
		if (ADD_LIVENESS_GOALS_INFO) {
			io += " // " + s.get_rank_info().rankInfoString().split("@")[0];
		}
		return io;
	}

	/**
	 * prints the selected vars of the state for input varNames = {"mLeft", "lift"}
	 * we can get "mLeft:STOP, lift:NIL"
	 * 
	 * @param s        current state
	 * @param varNames set of variable names to extract (empty set gives all)
	 * @return
	 */
	public static String getSelectedAssignmentStrings(EnumStateI s, List<String> varNames) {
		String res = "";

		if (varNames.isEmpty()) {
			return res;
		}

		BDD prn = s.getData();
		String all = prn.toStringWithDomains(Env.stringer).replace("<", "").replace(">", "");
		String[] asgns = all.split(", ");
		for (String asgn : asgns) {
			if (varNames.contains(asgn.split(":")[0])) {
				if (res.length() > 0) {
					res += ", ";
				}
				res += asgn;
			}
		}

		return res;
	}

	private boolean hasSuccessors(EnumStateI state) {
		return state.getSuccessors().size() > 0;
	}

}
