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

package tau.smlab.syntech.games.controller.symbolic;

import java.util.Map;

import net.sf.javabdd.ADD;
import net.sf.javabdd.BDD;
import net.sf.javabdd.BDD.BDDIterator;
import net.sf.javabdd.BDDFactory;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecBDD;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.ModuleVariableException;
import tau.smlab.syntech.modelchecker.CounterExampleException;
import tau.smlab.syntech.modelchecker.LTLModelChecker;
import tau.smlab.syntech.modelchecker.ModelCheckException;

public class SymbolicControllerChecker {

  public static boolean checkCompletenessForEnvIterate(SymbolicController ctrl, GameModel model) {

    PlayerModule env = model.getEnv();
    PlayerModule sys = model.getSys();

    // 1) check that all inital assignments to environment variables have a
    // corresponding inital state in the controller
    BDD result = env.initial().id().impWith(ctrl.initial().exist(sys.moduleUnprimeVars()))
        .forAll(env.moduleUnprimeVars());

    if (!result.isOne()) {
      result.free();
      return false;
    }
    result.free();

    // 2) check that for all reachable states in the controller it is enabled
    // for all next assignments to environment variables
    BDD reachable = Env.allSucc(ctrl.initial().id(), ctrl.trans().id());

    for (BDDIterator sit = reachable.iterator(Env.globalUnprimeVars()); sit.hasNext();) {
      BDD s = sit.nextBDD();
      BDD ctrlSucc = s.and(ctrl.trans());
      BDD envSucc = s.and(env.trans());
      BDD envChoices = envSucc.exist(sys.modulePrimeVars());
      envSucc.free();
      BDD ctrlEnvChoices = ctrlSucc.exist(sys.modulePrimeVars());
      ctrlSucc.free();
      BDD envNotCovered = envChoices.andWith(ctrlEnvChoices.not());
      if (!envNotCovered.isZero()) {
        System.out.println("State " + Env.toNiceSignleLineString(s));
        System.out.println(
            "Successors in Controller: " + Env.toNiceSignleLineString(ctrlEnvChoices.exist(Env.globalUnprimeVars())));
        System.out.println("EnvSuccessors not in Controller: "
            + Env.toNiceSignleLineString(envNotCovered.exist(Env.globalUnprimeVars())));
        s.free();
        envNotCovered.free();
        ctrlEnvChoices.free();
        return false;
      }
      s.free();
      envNotCovered.free();
      ctrlEnvChoices.free();
    }

    return true;
  }

  /**
   * Check whether from every state of the controller the environment can make all
   * choices
   * 
   * @param ctrl
   * @param model
   * @return
   */
  public static boolean checkCompletenessForEnv(SymbolicController ctrl, GameModel model) {

    PlayerModule env = model.getEnv();
    PlayerModule sys = model.getSys();

    // 1) check that all inital assignments to environment variables have a
    // corresponding inital state in the controller
    BDD result = env.initial().id().impWith(ctrl.initial().exist(sys.moduleUnprimeVars()))
        .forAll(env.moduleUnprimeVars());

    if (!result.isOne()) {
      System.out.println("initial states are not complete");
      result.free();
      return false;
    }
    result.free();

    // 2) check that for all reachable states in the controller it is enabled
    // for all next assignments to environment variables
    BDD reachable = Env.allSucc(ctrl.initial().id(), ctrl.trans().id());

    return reachable.andWith(env.trans().id()).impWith(ctrl.trans().exist(sys.modulePrimeVars()))
        .forAll(env.modulePrimeVars()).isOne();
  }

  /**
   * Check whether every reachable state (by controller and system) has an
   * environment successor or is a deadlock state for the system
   * 
   * @param ctrl
   * @param model
   * @return true if env has successors for all valid system choices
   */
  public static boolean checkCompletenessForSys(SymbolicController ctrl, GameModel model) {

    PlayerModule env = model.getEnv();
    PlayerModule sys = model.getSys();

    // 1) check that there is at least one valid initial env choice
    BDD result = ctrl.initial().exist(env.moduleUnprimeVars());

    if (result.isZero()) {
      System.out.println("no inital env choice");
      return false;
    }
    result.free();

    // 2) check that for all reachable states in the controller it is enabled
    BDD reachable = Env.allSucc(ctrl.initial().and(sys.initial()), sys.trans().and(ctrl.trans()));

    // remove states where env has a successor
    reachable.andWith(ctrl.trans().exist(env.modulePrimeVars().union(sys.modulePrimeVars())).not());
    // remove dead states
    reachable.andWith(env.controlStates(sys, Env.FALSE()).not());
    return reachable.isZero();
  }

  public static boolean checkStrategyIsWinningForSys(SymbolicController ctrl, ADD weightedArena, double worstInitEng) {

    ADD sourceStates = (ADD) ctrl.initial().id();
    ADD strategyTrans = (ADD) ctrl.trans().and(Env.allSucc(sourceStates.id(), ctrl.trans().id()));

    /* map every reachable state to -INF and every unreachable state to +INF */
    ADD strategyTransTmp = strategyTrans.not();
    strategyTrans.free();
    strategyTrans = strategyTransTmp;

    strategyTrans.applyWith(Env.CONST(2.0), BDDFactory.times);
    strategyTrans.applyWith(Env.TRUE(), BDDFactory.minus);
    strategyTrans.applyWith(Env.PLUS_INF(), BDDFactory.times);

    /*
     * restrict the arena to have only transitions from states that are reachable
     * from the initial states if sys plays by the strategy. Every outgoing legal
     * transition from a reachable source state, would have a finite or +INF weight;
     * Otherwise, the transition would have a +INF weight.
     */
    ADD strategyArena = weightedArena.apply(strategyTrans, BDDFactory.max);

    strategyTrans.free();

    /*
     * If there is a transition with -INF weight that is reachable from the initial
     * states when the system plays according to ctrl strategy, then it is loosing
     */
    ADD minimumCheck = strategyArena.findMin();
    if (minimumCheck.equals(Env.MINUS_INF())) {
      System.out.println(
          "The strategy is loosing for the system because there is a -INF transition reachable from an initial state");
      minimumCheck.free();
      return false;
    }
    minimumCheck.free();

    System.out.println(
        "Weights of reachable transitions from all initial states, if the system plays according to the constructed strategy:");
    strategyArena.printTerminalValues();

    System.out.println(
        "Checking that the system can play according to this strategy, without going into a negative cycle...");

    /*
     * Check that there is no negative cycle reachable from every initial state if
     * the system plays according to ctrl strategy
     */
    if (thereIsNegativeCycle(sourceStates, strategyArena, worstInitEng)) {
      System.out.println(
          "Error: there is a reachable negative cycle if the system plays according to this controller, i.e. it is loosing for the system!!");
      return false;
    }

    System.out.println("We don't have any negative cycles, so this controller is winning for the system!");

    return true;
  }

  /**
   * An implementation of symbolic Bellman Ford algorithm for calculating the
   * shortest paths weights from all given source states.
   * 
   * @param sourceStates
   * @param arena        the weighted arena
   * @return true if there is a negative cycle reachable from one of the source
   *         states; false, otherwise.
   */
  public static boolean thereIsNegativeCycle(ADD sourceStates, ADD arena, double worstInitEng) {

    /*
     * We add a new sink state, that has 0 weighted transitions to all initial
     * states. Therefore, at we initialize the shortest path estimate of each
     * initial state to 0; All other states are initialized to +INF.
     */
    ADD curDistEstimates = sourceStates.ite(Env.FALSE(), Env.PLUS_INF());

    ADD prevDistEstimates = null;
    ADD distSum, relaxPrimed, relaxUnprimed, worstInitEngCheck;

    /* V is the size of the state space */
    long V = 1;
    V <<= (2 * Env.globalPrimeVars().size());

    System.out.println("V = " + V);
    worstInitEng = (-1) * worstInitEng;
    for (long i = 0; i < V && (prevDistEstimates == null || !curDistEstimates.equals(prevDistEstimates)); i++) {
      if (i == V - 1) { // V's (last) iteration iff there is a negative cycle in the arena!
        return true;
      }
      /*
       * for efficiency of computation: if there is a negative cycle, we will end up
       * with worst required energy that is higher than the declared one; If so, we
       * should stop now (before reaching the V's iteration).
       */
      worstInitEngCheck = curDistEstimates.findMin();
      if (worstInitEngCheck
          .getConstantValue() < worstInitEng) { /*
                                                 * the strategy seems to be wrong, or there might be a negative cycle
                                                 */
        System.out.println(
            "the strategy requires energy from the initial states which is higher than the declared worst needed energy level!");
        System.out.println("The worst energy level that was found: " + (-1) * worstInitEngCheck.getConstantValue()
            + ", but the declared energy is " + (-1) * worstInitEng);
        return true;
      }
      worstInitEngCheck.free();
      // curDistEstimates.printTerminalValues();
      if (prevDistEstimates != null) {
        prevDistEstimates.free();
      }
      prevDistEstimates = curDistEstimates;

      /*
       * for all states v, d(v) is the shortest path estimate from one of the initial
       * states, i.e. it is the most energy consuming path to state v from one of the
       * initial states.
       */

      /*
       * Relax operation: for every target state v, do: d*(v) = min(d(u) + w(u,v)) for
       * all predecessors of v
       */
      distSum = arena.apply(prevDistEstimates, BDDFactory.plus);
      relaxPrimed = distSum.abstractMin(Env.globalUnprimeVars());
      relaxUnprimed = (ADD) Env.unprime(relaxPrimed);

      /*
       * for every target state v, do: d(v) = min(d(v), d*(v)); this is the last step
       * of the relax operation for each edge in the graph.
       */
      curDistEstimates = relaxUnprimed.apply(prevDistEstimates, BDDFactory.min);

      distSum.free();
      relaxPrimed.free();
      relaxUnprimed.free();
    }
    return false;

  }

  public static int checkMaxAccumulatedWeightUpToLimit(SymbolicController ctrl, Map<Integer, BDD> weights, int limit) {
    // TODO implement fixed point algorithm over reachable states but divided by
    // accumulated weights

    // compute a downwards closed set to guarantee fixpoint (to make it
    // downwardclosed keep only the state with the highest accumulated value)

    return -1;
  }

  public static boolean checkCompletenessForEnvUpTo(SymbolicController ctrl, GameModel model, BDD upTo) {

    PlayerModule env = model.getEnv();
    PlayerModule sys = model.getSys();

    // 1) check that all inital assignments to environment variables have a
    // corresponding inital state in the controller
    BDD result = env.initial().id().andWith(upTo.not()).impWith(ctrl.initial().exist(sys.moduleUnprimeVars()))
        .forAll(env.moduleUnprimeVars());

    if (!result.isOne()) {
      result.free();
      return false;
    }
    result.free();

    // 2) check that for all reachable states in the controller it is enabled
    // for all next assignments to environment variables
    BDD reachable = Env.allSucc(ctrl.initial().id(), ctrl.trans().id()).andWith(upTo.not());

    return reachable.andWith(env.trans().id()).impWith(ctrl.trans().exist(sys.modulePrimeVars()))
        .forAll(env.modulePrimeVars()).isOne();
  }

  /**
   * Model-checks controller against GR(1) specification (strict realizability
   * semantics)
   * 
   * @param ctrl
   * @param m
   * @return false in case of any exceptions (including counter-examples)
   */
  public static boolean checkGR1Spec(SymbolicController ctrl, GameModel m) {
    try {
      checkGR1SpecWC(ctrl, m);
    } catch (Exception e) {
      return false;
    }
    return true;
  }

  /**
   * Model-checks controller against GR(1) specification (strict realizability
   * semantics)
   * 
   * @param ctrl
   * @param m
   * @throws ModelCheckException
   * @throws CounterExampleException (is a ModelCheckException) in case GR(1) spec
   *                                 is violated by controller
   * @throws ModuleVariableException
   */
  public static void checkGR1SpecWC(SymbolicController ctrl, GameModel m)
      throws ModelCheckException, CounterExampleException, ModuleVariableException {
    PlayerModule sys = m.getSys();
    if (sys.justiceNum() == 0) {
      sys.addJustice(Env.TRUE());
    }
    PlayerModule env = m.getEnv();
    if (env.justiceNum() == 0) {
      env.addJustice(Env.TRUE());
    }

    LTLModelChecker c = null;

    PlayerModule mod = new PlayerModule();
    mod.setName("symbolicGraph");
    mod.resetInitial();
    mod.conjunctInitial(ctrl.initial().id());
    mod.resetTrans();
    mod.conjunctTrans(ctrl.trans().id());
    c = new LTLModelChecker(mod);

    Spec sysJ = allJustice(sys);
    Spec envJ = allJustice(env);

    Spec rhoS = new SpecBDD(sys.trans());
    Spec rhoE = new SpecBDD(env.trans());

    Spec thetaS = new SpecBDD(sys.initial());
    Spec thetaE = new SpecBDD(env.initial());

    Spec implI = new SpecExp(Operator.IMPLIES, thetaE, thetaS);

    Spec HrhoE = null;
    // workaround of HISTORICALLY limitation in case of primes in rhoE
    if (Env.containPrimeVars(env.trans())) {
      HrhoE = new SpecExp(Operator.HISTORICALLY, new SpecExp(Operator.PRIME, new SpecExp(Operator.PREV, rhoE)));
    } else {
      HrhoE = new SpecExp(Operator.HISTORICALLY, rhoE);
    }
    Spec implS = new SpecExp(Operator.IMPLIES, thetaE,
        new SpecExp(Operator.GLOBALLY, new SpecExp(Operator.IMPLIES, HrhoE, rhoS)));
    Spec implJ = new SpecExp(Operator.IMPLIES,
        new SpecExp(Operator.AND, thetaE, new SpecExp(Operator.AND, new SpecExp(Operator.GLOBALLY, rhoE), envJ)), sysJ);

    Spec strongRealizability = new SpecExp(Operator.AND, new SpecExp(Operator.AND, implS, implJ), implI);

    // c.modelCheckStandardOutput(strongRealizability);
    c.modelCheck(strongRealizability);
  }

  /**
   * produces a conjunction of all justices as /\_{i} GF(J_i)
   * 
   * @param m
   * @return
   */
  private static Spec allJustice(PlayerModule m) {
    Spec allSysJ = new SpecExp(Operator.GLOBALLY, new SpecExp(Operator.FINALLY, new SpecBDD(m.justiceAt(0))));
    for (int i = 1; i < m.justiceNum(); i++) {
      Spec spec = new SpecExp(Operator.GLOBALLY, new SpecExp(Operator.FINALLY, new SpecBDD(m.justiceAt(i))));
      allSysJ = new SpecExp(Operator.AND, allSysJ, spec);
    }

    return allSysJ;
  }

  public static boolean checkRabinSpec(SymbolicController ctrl, GameModel m) {
    boolean res = false;

    PlayerModule sys = m.getSys();
    PlayerModule env = m.getEnv();

    LTLModelChecker c = null;
    try {
      PlayerModule mod = new PlayerModule();
      mod.setName("symbolicGraph");
      mod.resetInitial();
      mod.conjunctInitial(ctrl.initial().id());
      mod.resetTrans();
      mod.conjunctTrans(ctrl.trans().id());
      c = new LTLModelChecker(mod);
    } catch (Exception e) {
      e.printStackTrace();
      return false;
    }

    Spec sysJ = allJustice(sys);
    Spec envJ = allJustice(env);

    Spec rhoS = new SpecBDD(sys.trans());
    Spec rhoE = new SpecBDD(env.trans());

    Spec thetaS = new SpecBDD(sys.initial());
    Spec thetaE = new SpecBDD(env.initial());

    Spec implI = new SpecExp(Operator.IMPLIES, thetaE, thetaS);

    Spec HrhoE = null;
    // workaround of HISTORICALLY limitation in case of primes in rhoE
    if (Env.containPrimeVars(env.trans())) {
      HrhoE = new SpecExp(Operator.HISTORICALLY, new SpecExp(Operator.PRIME, new SpecExp(Operator.PREV, rhoE)));
    } else {
      HrhoE = new SpecExp(Operator.HISTORICALLY, rhoE);
    }
    Spec implS = new SpecExp(Operator.AND, thetaE,
        new SpecExp(Operator.GLOBALLY, new SpecExp(Operator.IMPLIES, HrhoE, rhoS)));
    Spec implJ = new SpecExp(Operator.IMPLIES,
        new SpecExp(Operator.AND, thetaE, new SpecExp(Operator.AND, new SpecExp(Operator.GLOBALLY, rhoE), envJ)), sysJ);

    Spec strongRealizability = new SpecExp(Operator.AND, new SpecExp(Operator.AND, implS, implJ), implI);

    Spec rabin = new SpecExp(Operator.NOT, strongRealizability);

    res = c.modelCheckWithNoCounterExample(rabin);

    return res;
  }
}
