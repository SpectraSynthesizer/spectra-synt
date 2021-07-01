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

package tau.smlab.syntech.checks;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.bddgenerator.BDDGenerator;
import tau.smlab.syntech.cores.domainagnostic.AbstractDdmin;
import tau.smlab.syntech.gameinput.model.Constraint;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.SpecTraceable;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gameinputtrans.translator.CounterTranslator;
import tau.smlab.syntech.gameinputtrans.translator.MonitorTranslator;
import tau.smlab.syntech.gameinputtrans.translator.PastLTLTranslator;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.games.controller.symbolic.SymbolicController;
import tau.smlab.syntech.jtlv.CoreUtil;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

/**
 * some non-trivial checks on the BDD level
 * 
 * @author ringert
 * 
 */
public class Checker {

  private PlayerModule sys;
  private PlayerModule env;
  private String[] monitorCheckMessages;
  private String[] counterCheckMessages;

  /**
   * Computes a subset of initial and safety system constraints that allow the environment to deadlock the system on its initial
   * move. <br>
   * 
   * @param m
   * @return null if no deadlock can occur
   */
  public List<BehaviorInfo> computeIniDeadlockCore(GameModel model, List<String> l) {
    env = model.getEnv();
    sys = model.getSys();

    BDD sysDead = sys.initial().id().impWith(env.controlStates(sys, Env.FALSE()));
    BDD envKillSys = env.initial().id().andWith(sysDead.forAll(sys.moduleUnprimeVars()));
    sysDead.free();

    if (envKillSys.isZero()) {
      return null;
    }
    envKillSys.free();

    return new AbstractDdmin<BehaviorInfo>() {

      @Override
      protected boolean check(List<BehaviorInfo> part) {
        BDD iniPart = Env.TRUE();
        part.stream().filter(p -> p.isInitial()).forEach(p -> iniPart.andWith(p.initial.id()));

        BDD transPart = Env.TRUE();
        part.stream().filter(p -> p.isSafety()).forEach(p -> transPart.andWith(p.safety.id()));

        PlayerModule partSys = new PlayerModule();
        partSys.conjunctTrans(transPart);
        // initial states where the environment can force system to deadlock
        BDD sysDead = iniPart.impWith(env.controlStates(partSys, Env.FALSE()));
        partSys.free();
        // initial environment assignment that can deadlock the system
        BDD envKillSys = env.initial().id().andWith(sysDead.forAll(sys.moduleUnprimeVars()));
        boolean win = !envKillSys.isZero();
        if (win) {
          BDD e = envKillSys.exist(sys.modulePrimeVars());
          BDD e1 = e.exist(sys.moduleUnprimeVars());
          e.free();
          if (e1.isOne()) {
            l.add("Any initial assignment.");
          } else {
            l.add(Env.toNiceSignleLineString(e1));
          }
          e1.free();
        }
        envKillSys.free();
        return win;
      }

    }.minimize(
        model.getSysBehaviorInfo().stream().filter(p -> p.isInitial() || p.isSafety()).collect(Collectors.toList()));
  }

  /**
   * Computes reachable states (most permissive environment and system) and checks if justices can be reached. <br>
   * This is a heuristics because it assumes environment and system to be only restricted by their safeties and not by
   * any strategy.
   * 
   * @param model
   * @return
   */
  public List<BehaviorInfo> computeUnreachableJustice(GameModel model) {

    env = model.getEnv();
    sys = model.getSys();

    List<BehaviorInfo> unreachableJustice = new ArrayList<>();

    // compute reachable states, which are not immediate deadlocks for the system
    BDD reach = Env.allSucc(env.initial().and(sys.initial()), env.trans().and(sys.trans()));
    BDD dead = env.controlStates(sys, Env.FALSE());
    reach.andWith(dead.not());
    dead.free();

    for (BehaviorInfo bi : model.getSysBehaviorInfo()) {
      if (bi.isJustice()) {
        BDD tmp = bi.justice.and(reach);
        if (tmp.isZero()) {
          unreachableJustice.add(bi);
        }
        tmp.free();
      }
    }

    for (BehaviorInfo bi : model.getEnvBehaviorInfo()) {
      if (bi.isJustice()) {
        BDD tmp = bi.justice.and(reach);
        if (tmp.isZero()) {
          unreachableJustice.add(bi);
        }
        tmp.free();
      }
    }

    reach.free();
    return unreachableJustice;
  }

  /**
   * computes a minimal unsatisfiable subset of constraints INI or SAFETY
   * 
   * @param model
   * @return
   */
  public List<BehaviorInfo> computeUnsatSafetyCore(GameModel model) {

    sys = model.getSys();

    DdminUNSAT cc = new DdminUNSAT();
    if (sys.initial().isZero()) {
      List<BehaviorInfo> inis = new ArrayList<>();
      for (BehaviorInfo bi : model.getSysBehaviorInfo()) {
        if (bi.isInitial()) {
          inis.add(bi);
        }
      }
      return cc.minimize(inis);
    } else if (sys.trans().isZero()) {
      List<BehaviorInfo> safeties = new ArrayList<>();
      for (BehaviorInfo bi : model.getSysBehaviorInfo()) {
        if (bi.isInitial() || bi.isSafety()) {
          safeties.add(bi);
        }
      }
      return cc.minimize(safeties);
    }

    env = model.getEnv();

    if (env.initial().isZero()) {
      List<BehaviorInfo> inis = new ArrayList<>();
      for (BehaviorInfo bi : model.getEnvBehaviorInfo()) {
        if (bi.isInitial()) {
          inis.add(bi);
        }
      }
      return cc.minimize(inis);
    } else if (env.trans().isZero()) {
      List<BehaviorInfo> safeties = new ArrayList<>();
      for (BehaviorInfo bi : model.getEnvBehaviorInfo()) {
        if (bi.isInitial() || bi.isSafety()) {
          safeties.add(bi);
        }
      }
      return cc.minimize(safeties);
    }

    return new ArrayList<>();
  }

  /**
   * checks constraints whether they are trivially TRUE or FALSE
   * 
   * @param model
   * @return
   */
  public List<BehaviorInfo> computeTrivialSpecs(GameModel model) {

    List<BehaviorInfo> infos = new ArrayList<>();
    infos.addAll(model.getSysBehaviorInfo());
    infos.addAll(model.getEnvBehaviorInfo());
    infos.addAll(model.getAuxBehaviorInfo());

    List<BehaviorInfo> trivial = new ArrayList<>();
    for (BehaviorInfo bi : infos) {
      if (bi.isInitial() && (bi.initial.isZero() || bi.initial.isOne())) {
        trivial.add(bi);
      } else if (bi.isSafety() && (bi.safety.isZero() || bi.safety.isOne())) {
        trivial.add(bi);
      } else if (bi.isJustice() && (bi.justice.isZero() || bi.justice.isOne())) {
        trivial.add(bi);
      }
    }
    return trivial;
  }

  /**
   * Checks whether there are illegal assumptions: initial assumptions with system variables or any primes, or safety
   * assumptions with any system primes.
   * 
   * @param model
   * @return
   */
  public List<BehaviorInfo> computeEnvBadPrimesSpecs(GameModel model) {
    List<BehaviorInfo> infos = new ArrayList<>();
    infos.addAll(computeEnvBadIniPrimesSpecs(model));
    infos.addAll(computeEnvBadIniSysSpec(model));
    infos.addAll(computeEnvBadSafetyPrimesSpecs(model));
    return infos;
  }

  /**
   * Checks whether there are illegal assumptions: initial assumptions with system variables.
   * 
   * @param model
   * @return
   */
  public List<BehaviorInfo> computeEnvBadIniSysSpec(GameModel model) {
    sys = model.getSys();
    env = model.getEnv();
    List<BehaviorInfo> infos = new ArrayList<>();
    BDDVarSet primes = Env.globalPrimeVars();

    // initial assumptions may not have system variables
    if (Env.hasVars(env.initial(), sys.moduleUnprimeVars())) {
      // check whether BehaviorInfo was created
      if (model.getEnvBehaviorInfo().isEmpty()) {
        infos.add(new BehaviorInfo(env.initial(), null, null, null, null, 0, false));
      } else {
        for (BehaviorInfo bi : model.getEnvBehaviorInfo()) {
          if (bi.isInitial() && Env.hasVars(bi.initial, sys.moduleUnprimeVars())) {
            infos.add(bi);
          }
        }
      }
    }

    primes.free();
    return infos;
  }

  /**
   * Checks whether there are illegal assumptions: initial assumptions with any primes.
   * 
   * @param model
   * @return
   */
  public List<BehaviorInfo> computeEnvBadIniPrimesSpecs(GameModel model) {
    sys = model.getSys();
    env = model.getEnv();
    List<BehaviorInfo> infos = new ArrayList<>();
    BDDVarSet primes = Env.globalPrimeVars();

    // initial assumptions may not have primed variables
    if (Env.hasVars(env.initial(), primes)) {
      // check whether BehaviorInfo was created
      if (model.getEnvBehaviorInfo().isEmpty()) {
        infos.add(new BehaviorInfo(env.initial(), null, null, null, null, 0, false));
      } else {
        for (BehaviorInfo bi : model.getEnvBehaviorInfo()) {
          if (bi.isInitial() && Env.hasVars(bi.initial, primes)) {
            infos.add(bi);
          }
        }
      }
    }

    primes.free();
    return infos;
  }

  /**
   * Checks whether there are illegal assumptions: safety assumptions with any system primes.
   * 
   * @param model
   * @return
   */
  public List<BehaviorInfo> computeEnvBadSafetyPrimesSpecs(GameModel model) {
    sys = model.getSys();
    env = model.getEnv();
    List<BehaviorInfo> infos = new ArrayList<>();

    // safety assumptions may not have primed system variables
    if (Env.hasVars(env.trans(), sys.modulePrimeVars())) {
      // check whether BehaviorInfo was created
      if (model.getEnvBehaviorInfo().isEmpty()) {
        infos.add(new BehaviorInfo(null, env.trans(), null, null, null, 0, false));
      } else {
        for (BehaviorInfo bi : model.getEnvBehaviorInfo()) {
          if (bi.isSafety() && Env.hasVars(bi.safety, sys.modulePrimeVars())) {
            infos.add(bi);
          }
        }
      }
    }
    return infos;
  }

  public List<BehaviorInfo> computeSysBadPrimesSpecs(GameModel model) {
    sys = model.getSys();
    env = model.getEnv();
    List<BehaviorInfo> infos = new ArrayList<>();
    BDDVarSet primes = Env.globalPrimeVars();

    if (Env.hasVars(sys.initial(), primes)) {
      // check whether BehaviorInfo was created
      if (model.getSysBehaviorInfo().isEmpty()) {
        infos.add(new BehaviorInfo(sys.initial(), null, null, null, null, 0, false));
      } else {
        for (BehaviorInfo bi : model.getSysBehaviorInfo()) {
          if (bi.isInitial() && Env.hasVars(bi.initial, primes)) {
            infos.add(bi);
          }
        }
      }
    }
    
    primes.free();
    return infos;
  }

  public Map<Integer, Set<BehaviorInfo>> computeTraceIdtoBIMap(GameModel gm) {
    Map<Integer, Set<BehaviorInfo>> traceIdToBehaviorInfo = new HashMap<>();
    Set<BehaviorInfo> biSet;
    for (BehaviorInfo bi : gm.getAuxBehaviorInfo()) {
      if (traceIdToBehaviorInfo.containsKey(bi.traceId)) {
        biSet = traceIdToBehaviorInfo.get(bi.traceId);
      } else {
        biSet = new HashSet<>();
        traceIdToBehaviorInfo.put(bi.traceId, biSet);
      }
      biSet.add(bi);
    }
    return traceIdToBehaviorInfo;
  }

  /**
   * Fills in for the specified monitor monName the following info:
   * (1) Behavior info of initial and safety constraints (in initialConstraints and safetyConstraints).
   * (2) Behavior info of PAST initial and safety constraints, i.e, which were created during the PAST translation,
   *     (in pastInitialConstraints and pastSafetyConstraints).
   * (3) A list of the PAST variable references (in monNameToVarRefs).
   */
  public void fillMonitorConstraints(GameModel gm,
      String monName,
      MonitorTranslator monTranslator,
      PastLTLTranslator pastTranslator,
      Map<Integer, Set<BehaviorInfo>> traceIdToBehaviorInfo,
      Map<String, Set<BehaviorInfo>> safetyConstraints,
      Map<String, Set<BehaviorInfo>> initialConstraints,
      Map<String, Set<BehaviorInfo>> pastSafetyConstraints,
      Map<String, Set<BehaviorInfo>> pastInitialConstraints,
      Map<String, Set<VariableReference>> monNameToVarRefs) {

    Set<BehaviorInfo> biSet;
    for (Integer traceId : monTranslator.getTraceIdsOfMonitor(monName)) {
      biSet = traceIdToBehaviorInfo.get(traceId);
      for (BehaviorInfo bi : biSet) {
        if (bi.isInitial()) {
          addConstraintToMonitor(initialConstraints, monName, bi);
        } else if (bi.isSafety()) {
          addConstraintToMonitor(safetyConstraints, monName, bi);
        }
      }
    }

    List<Constraint> monPastConstraints = new ArrayList<>();
    for (Constraint c : monTranslator.getMonitorConstraints(monName)) {
      monPastConstraints.addAll(
          getPastConstraints(monName, c.getSpec(), pastTranslator, monNameToVarRefs));
    }

    for (Constraint c : monPastConstraints) {
      biSet = traceIdToBehaviorInfo.get(c.getTraceId());
      for (BehaviorInfo bi : biSet) {
        if (bi.isInitial()) {
          if (!initialConstraints.get(monName).contains(bi)) {
            addConstraintToMonitor(pastInitialConstraints, monName, bi);
          }
        } else if (bi.isSafety()) {
          if (!safetyConstraints.get(monName).contains(bi)) {
            addConstraintToMonitor(pastSafetyConstraints, monName, bi);
          }
        }
      }
    }
  }

  private Set<Constraint> getPastConstraints(String monName, Set<Constraint> cons, PastLTLTranslator pastTranslator,
      Map<String, Set<VariableReference>> monNameToVarRefs) {
    Set<Constraint> all = new HashSet<>(cons);
    for (Constraint c : cons) {
      all.addAll(getPastConstraints(monName, c.getSpec(), pastTranslator, monNameToVarRefs));
    }
    return all;
  }

  private Set<Constraint> getPastConstraints(String monName, Spec spec,
      PastLTLTranslator pastTranslator,
      Map<String, Set<VariableReference>> monNameToVarRefs) {

    if (spec instanceof VariableReference) {
      VariableReference varRef = (VariableReference) spec;
      //we need to maintain a mapping of a monitor name to variables of a monitor
      if (pastTranslator.getConstraintsOfVarRef(varRef) != null) {
        Set<VariableReference> monVarRefs;
        if (monNameToVarRefs.containsKey(monName)) {
          monVarRefs = monNameToVarRefs.get(monName);
        } else {
          monVarRefs = new HashSet<>();
          monNameToVarRefs.put(monName, monVarRefs);
        }
        monVarRefs.add(varRef);
        return getPastConstraints(monName, new HashSet<Constraint>(pastTranslator.getConstraintsOfVarRef(varRef)),
            pastTranslator, monNameToVarRefs);
      }
    }

    if (spec instanceof SpecExp) {
      SpecExp se = (SpecExp) spec;
      Set<Constraint> childrenConstraints = new HashSet<>();
      for (int i = 0; i < se.getChildren().length; i++) {
        childrenConstraints.addAll(getPastConstraints(monName,
            se.getChildren()[i], pastTranslator, monNameToVarRefs));
      }
      return childrenConstraints;
    }

    return new HashSet<>();
  }

  /**
   * Returns the behavior info list of the specified monitor monName.
   * 
   * @param monName
   * @param safetyConstraints
   * @param initialConstraints
   * @return
   */
  private List<BehaviorInfo> getMonitorSpecList(String monName,
      Map<String, Set<BehaviorInfo>> safetyConstraints,
      Map<String, Set<BehaviorInfo>> initialConstraints) {

    List<BehaviorInfo> monitorSpecs = new ArrayList<>();

    if (safetyConstraints != null && safetyConstraints.containsKey(monName)) {
      monitorSpecs.addAll(safetyConstraints.get(monName));
    }
    if (initialConstraints != null && initialConstraints.containsKey(monName)) {
      monitorSpecs.addAll(initialConstraints.get(monName));
    }

    return monitorSpecs;

  }

  public List<BehaviorInfo> checkMonitorsForCompleteness(GameModel gm, MonitorTranslator monitorTranslator,
      PastLTLTranslator pastLTLTranslator) {

    List<BehaviorInfo> result;
    Map<Integer, Set<BehaviorInfo>> traceIdtoBI = computeTraceIdtoBIMap(gm); //Note that the same traceId may have multiple Behavior infos
    Map<String, Set<BehaviorInfo>> safetyConstraints = new HashMap<>();
    Map<String, Set<BehaviorInfo>> pastSafetyConstraints = new HashMap<>();
    Map<String, Set<BehaviorInfo>> pastInitialConstraints = new HashMap<>();
    Map<String, Set<BehaviorInfo>> initialConstraints = new HashMap<>();
    Map<String, Set<VariableReference>> monNameToVarRefs = new HashMap<>();

    for (String monName : monitorTranslator.getMonitorsNames()) {
      fillMonitorConstraints(gm, monName, monitorTranslator, pastLTLTranslator, traceIdtoBI,
          safetyConstraints, initialConstraints, pastSafetyConstraints, pastInitialConstraints, monNameToVarRefs);

      //check for completeness
      // build a symbolic controller from the monitor's definition

      SymbolicController symCtrl = new SymbolicController();
      symCtrl.initial().free();
      symCtrl.trans().free();
      symCtrl.setTrans(Env.TRUE());

      if (safetyConstraints.containsKey(monName)) {
        for (BehaviorInfo bi : safetyConstraints.get(monName)) {
          symCtrl.conjunctTransWith(bi.safety.id());
        }
      }

      if (pastSafetyConstraints.containsKey(monName)) {
        for (BehaviorInfo bi : pastSafetyConstraints.get(monName)) {
          symCtrl.conjunctTransWith(bi.safety.id());
        }
      }

      BDD initials = Env.TRUE();
      if (initialConstraints.containsKey(monName)) {
        for (BehaviorInfo bi : initialConstraints.get(monName)) {
          initials.andWith(bi.initial.id());
        }
      }
      if (pastInitialConstraints.containsKey(monName)) {
        for (BehaviorInfo bi : pastInitialConstraints.get(monName)) {
          initials.andWith(bi.initial.id());
        }
      }
      symCtrl.setInit(initials);

      result = performMonitorCompletenessCheck(gm, monName, monNameToVarRefs.get(monName), symCtrl, safetyConstraints,
          initialConstraints);
      if (!result.isEmpty()) {
        return result;
      }
    }
    // if we have reached here, the monitor has passed the check 
    return new ArrayList<>();
  }

  private void addConstraintToMonitor(Map<String, Set<BehaviorInfo>> constraints,
      String parentMonitor, BehaviorInfo bi) {
    Set<BehaviorInfo> constraintSet;
    if (constraints.containsKey(parentMonitor)) {
      constraintSet = constraints.get(parentMonitor);
    } else {
      constraintSet = new HashSet<>();
      constraints.put(parentMonitor, constraintSet);
    }
    constraintSet.add(bi);
  }

  private List<BehaviorInfo> performMonitorCompletenessCheck(GameModel m, String monitorName,
      Set<VariableReference> pastVarlist, SymbolicController ctrl,
      Map<String, Set<BehaviorInfo>> safetyConstraints,
      Map<String, Set<BehaviorInfo>> initialConstraints) {

    // handling domain information
    BDD doms = m.getSys().getDoms().and(m.getEnv().getDoms());
    BDD domsIni = doms.exist(Env.globalPrimeVars());
    ctrl.initial().andWith(domsIni.id());
    ctrl.conjunctTrans(doms);

    // 1) check that all initial assignments to all unprimed variables minus 
    // the monitor's (aux) variable(s) have a corresponding initial state in the monitor's controller
    ModuleBDDField monVar = Env.getVar(monitorName);

    //VarSets of unprimed and primed past aux variables. These may exists if there
    //are past expressions in the monitor definition.
    BDDVarSet monAuxSet = Env.getEmptySet(), primeMonAuxSet = Env.getEmptySet();
    if (pastVarlist != null) {
      ModuleBDDField monAuxVar;
      for (VariableReference varRef : pastVarlist) {
        monAuxVar = Env.getVar(varRef.getReferenceName());
        monAuxSet.unionWith(monAuxVar.getDomain().set());
        primeMonAuxSet.unionWith(monAuxVar.getOtherDomain().set());
      }
    }
    BDDVarSet unprimedMonSet = monVar.getDomain().set().unionWith(monAuxSet);
    BDDVarSet primedMonSet = monVar.getOtherDomain().set().unionWith(primeMonAuxSet);
    BDDVarSet unprimedGloalMinusMonSet = Env.globalUnprimeVars().minus(unprimedMonSet);
    BDDVarSet primedGloalMinusMonSet = Env.globalPrimeVars().minus(primedMonSet);

    BDD ctrlIni = ctrl.initial().exist(unprimedMonSet);
    BDD result = domsIni.imp(ctrlIni).forAll(unprimedGloalMinusMonSet);

    if (!result.isOne()) {
      result.free();
      unprimedGloalMinusMonSet.free();
      primedGloalMinusMonSet.free();
      primedMonSet.free();
      unprimedMonSet.free();
      String[] errorMsg = new String[2];
      errorMsg[0] = monitorName;
      BDDVarSet nonAuxVars = Env.union(m.getSys().getNonAuxFields()).union(Env.union(m.getEnv().getNonAuxFields()));
      errorMsg[1] = CoreUtil.satOne(domsIni.and(ctrlIni.not()), nonAuxVars).toStringWithDomains(Env.stringer);
      ctrlIni.free();
      this.setMonitorCheckMessages(errorMsg);
      nonAuxVars.free();
      return getMonitorSpecList(monitorName, null, initialConstraints);
    }
    result.free();

    // 2) check that for all reachable states in the monitor controller it is enabled
    // for all next assignments to all to all primed variables minus 
    // the monitor's (aux) variable 
    BDD reachable = Env.allSucc(ctrl.initial().id(), ctrl.trans());

    BDD ctrlTrans = ctrl.trans().exist(primedMonSet);
    result = reachable.and(doms).imp(ctrlTrans).forAll(Env.globalUnprimeVars().union(Env.globalPrimeVars()));
    if (!result.isOne()) {
      String[] errorMsg = new String[2];
      BDDVarSet nonAuxVars = Env.union(m.getSys().getNonAuxFields()).union(Env.union(m.getEnv().getNonAuxFields()));
      errorMsg[0] = monitorName;
      errorMsg[1] = CoreUtil.satOne(reachable.and(doms).and(ctrlTrans.not()), nonAuxVars)
          .toStringWithDomains(Env.stringer);
      this.setMonitorCheckMessages(errorMsg);
      ctrlTrans.free();
      nonAuxVars.free();
    }

    Map<String, Set<BehaviorInfo>> safetiesToMark = (result.isOne() ? null : safetyConstraints);
    result.free();
    reachable.free();
    unprimedGloalMinusMonSet.free();
    primedGloalMinusMonSet.free();
    primedMonSet.free();
    unprimedMonSet.free();

    return getMonitorSpecList(monitorName, safetiesToMark, null);
  }

  public void setMonitorCheckMessages(String[] messages) {
    this.monitorCheckMessages = messages;
  }

  public String[] getMonitorCheckMessages() {
    return this.monitorCheckMessages;
  }

  //////////////////////////
  // --- Counters checks ---
  //////////////////////////

  public List<BehaviorInfo> checkCountersConsistency(GameModel gm, CounterTranslator counterTranslator) {
    Map<Integer, Set<BehaviorInfo>> traceIdtoBI = computeTraceIdtoBIMap(gm);

    for (String counterName : counterTranslator.getCountersNames()) {
      List<SpecTraceable> inconstentPredicates = getInconstentCounterPredicates(gm, counterName, counterTranslator);
      if (inconstentPredicates.size() > 0) {
        List<BehaviorInfo> result = new ArrayList<BehaviorInfo>();
        for (SpecTraceable pred : inconstentPredicates) {
          Set<BehaviorInfo> behaviorInfos = traceIdtoBI.get(pred.getTraceId());
          result.addAll(behaviorInfos);
        }
        return result;
      }

    }

    return new ArrayList<BehaviorInfo>();
  }

  private List<SpecTraceable> getInconstentCounterPredicates(GameModel gm, String counterName,
      CounterTranslator counterTranslator) {
    List<SpecTraceable> predicates = counterTranslator.getCounterPredicates(counterName);

    PlayerModule env = gm.getEnv();
    PlayerModule sys = gm.getSys();
    BDD reachableStates = Env.allSucc(env.initial().and(sys.initial()), env.trans().and(sys.trans()));

    List<BDD> predicatesBdds = new ArrayList<BDD>();
    for (SpecTraceable pred : predicates) {
      BDD bdd = BDDGenerator.createBdd(pred.getContent(), pred.getTraceId());
      predicatesBdds.add(bdd);
    }

    try {
      for (int i = 0; i < predicates.size(); i++) {
        for (int j = i + 1; j < predicates.size(); j++) {
          SpecTraceable p1 = predicates.get(i);
          SpecTraceable p2 = predicates.get(j);

          BDD p1ReachableStates = predicatesBdds.get(i);
          BDD p2ReachableStates = predicatesBdds.get(j);

          BDD reachableAndP1 = p1ReachableStates.and(reachableStates).exist(Env.globalPrimeVars());
          BDD reachableAndP2 = p2ReachableStates.and(reachableStates).exist(Env.globalPrimeVars());
          BDD intersection = reachableAndP1.and(reachableAndP2);
          reachableAndP1.free();
          reachableAndP2.free();

          if (!intersection.isZero()) {
            BDDVarSet nonAuxVars = Env.union(sys.getNonAuxFields()).union(Env.union(env.getNonAuxFields()));

            String[] errorMsg = new String[2];
            errorMsg[0] = counterName;
            errorMsg[1] = CoreUtil.satOne(intersection, nonAuxVars).toStringWithDomains(Env.stringer);
            this.setCounterCheckMessages(errorMsg);

            List<SpecTraceable> intersectingPredicates = new ArrayList<SpecTraceable>();
            intersectingPredicates.add(p1);
            intersectingPredicates.add(p2);
            return intersectingPredicates;
          }
          intersection.free();
        }
      }

      return new ArrayList<SpecTraceable>();
    } finally {
      for (BDD bdd : predicatesBdds) {
        bdd.free();
      }
      reachableStates.free();
    }
  }

  public void setCounterCheckMessages(String[] messages) {
    this.counterCheckMessages = messages;
  }

  public String[] getCounterCheckMessages() {
    return this.counterCheckMessages;
  }
}
