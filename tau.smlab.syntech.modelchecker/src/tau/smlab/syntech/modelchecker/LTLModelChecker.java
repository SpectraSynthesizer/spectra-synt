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

package tau.smlab.syntech.modelchecker;

import java.util.HashMap;
import java.util.Set;
import java.util.Vector;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDException;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.jtlv.CoreUtil;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.ModuleVariableException;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;
import tau.smlab.syntech.gamemodel.ModuleException;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecBDD;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.Operator;

/**
 * <p>
 * A checker which knows how to check LTL properties for the given
 * ComposedPlayerModule
 * </p>
 * 
 */
public class LTLModelChecker {

  private PlayerModule design;

  public LTLModelChecker(PlayerModule design) throws ModelCheckException {
    if (design == null)
      throw new ModelCheckException("Cannot instatiate an LTL Model " + "Checker with a null module.");
    this.design = design;
  }

  /**
   * 
   * @param property
   * @return true if property is verified
   */
  public boolean modelCheckWithNoCounterExample(Spec property) {
    Spec negp = new SpecExp(Operator.NOT, property);
    LTLTesterBuilder builder;
    try {
      builder = new LTLTesterBuilder(negp, true);
      PlayerModule composed = design.compose(builder.getTester());
      boolean res = checkVerify(builder.getSpec2BDD(property).not(), composed);
      return res;
    } catch (ModelCheckException e) {
      e.printStackTrace();
    } catch (ModuleVariableException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return false;
  }

  private boolean checkVerify(BDD initial_condition, PlayerModule designWithTester) {
    // saving to the previous restriction state
    BDD initial = designWithTester.initial().id();

    designWithTester.conjunctInitial(initial_condition.id());
    FeasibilityChecker fsChecker = new FeasibilityChecker();
    BDD feas = fsChecker.check(designWithTester);
    // the initial_condition seems redundant
    if (!feas.and(designWithTester.initial().id()).and(initial_condition).isZero()) {
      // returning to the previous restriction state
      designWithTester.resetInitial();
      designWithTester.conjunctInitial(initial.id());
      return false;
    }
    // returning to the previous restriction state
    designWithTester.resetInitial();
    designWithTester.conjunctInitial(initial.id());
    return true;
  }

  /**
   * <p>
   * Given a specification \phi (as a formula in temporal logic) we want to decide
   * whether \phi is valid over finite state program P , i.e. whether all the
   * computations of the design satisfy \phi. This variant of implementation,
   * prints the results to the standard streams.
   * </p>
   * 
   * @param property The property to check.
   * @throws ModuleVariableException
   */
  public void modelCheckStandardOutput(Spec property) {
    System.out.println("model checking property: " + property);
    try {
      this.modelCheck(property);
    } catch (ModelCheckException mce) {
      System.err.println(mce.toString());
      return;
    } catch (ModuleVariableException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    // else - everything is OK.
    System.out.println("*** Property is VALID ***");
  }

  /**
   * <p>
   * Given a specification \phi (as a formula in temporal logic) we want to decide
   * whether \phi is valid over finite state program P , i.e. whether all the
   * computations of the design satisfy \phi.
   * </p>
   * 
   * @param property The property to check.
   * @throws ModelCheckException     When the method is initiated with other then
   *                                 LTL property.
   * @throws CounterExampleException When the property is not valid, a counter
   *                                 example is thrown.
   * @throws ModuleVariableException
   */
  public void modelCheck(Spec property) throws ModelCheckException, CounterExampleException, ModuleVariableException {
    Spec negp = new SpecExp(Operator.NOT, property);
    LTLTesterBuilder builder = new LTLTesterBuilder(negp, true);

    BDD tester_initials = builder.getSpec2BDD(property).not();

    BDDVarSet visibleVars = getRelevantVars(design);
    visibleVars.unionWith(getRelevantVars(builder.getTester())); // FIXME
    visibleVars.unionWith(tester_initials.support().minus(Env.union(builder.getTester().getAuxFields())));

    PlayerModule composed = design.compose(builder.getTester());
    try {
      verify(tester_initials, composed, visibleVars);
      // verify(tester_initials, this.design, null);
    } catch (CounterExampleException mce) {
      throw mce;
    }
  }

  /**
   * <p>
   * The main procedure for verifying.
   * </p>
   * 
   * @param initial_condition
   * @param designWithTester
   * @param relevantVars
   * @throws ModelCheckException
   */
  private static void verify(BDD initial_condition, PlayerModule designWithTester, BDDVarSet relevantVars)
      throws CounterExampleException {
    // saving to the previous restriction state
    BDD initial = designWithTester.initial().id();

    designWithTester.conjunctInitial(initial_condition.id());
    FeasibilityChecker fsChecker = new FeasibilityChecker();
    BDD feas = fsChecker.check(designWithTester);
    // the initial_condition seems redundant
    if (!feas.and(designWithTester.initial()).and(initial_condition).isZero()) {

      BDD[] example = extractWithness(feas, designWithTester, relevantVars, fsChecker);
      CounterExampleException cee = new CounterExampleException("\n*** Property is NOT VALID ***", example);

      // returning to the previous restriction state
      designWithTester.resetInitial();
      designWithTester.conjunctInitial(initial.id());
      throw cee;
    }

    // returning to the previous restriction state
    designWithTester.resetInitial();
    designWithTester.conjunctInitial(initial.id());
  }

//  private static BDDVarSet getRelevantVars(PlayerModule m) {
//    BDDVarSet vars = Env.getEmptySet();
//    if (m != null) {
//      vars.unionWith(m.initial().support());
//      vars.unionWith(m.trans().support());
//
//      // fairness variables are important to illustrate feasibility.
//      for (int i = 0; i < m.justiceNum(); i++) {
//        vars.unionWith(m.justiceAt(i).support());
//      }
//
//      for (ModuleBDDField f : m.getAuxFields()) {
//        BDDVarSet v = vars.minus(f.support());
//        vars.free();
//        vars = v;
//      }
//    }
//    vars.intersectWith(Env.globalUnprimeVars());
//    return vars;
//  }

  private static BDDVarSet getRelevantVars(PlayerModule m) {
    BDDVarSet vars = Env.getEmptySet();
    if (m != null) {
      vars = Env.getEmptySet();
      
      for (ModuleBDDField f : m.getNonAuxFields()) {
        if(!f.isPrime()) {
            vars.unionWith(f.support().id());
        }
      }
    }
    return vars;
  }
  
  /**
   * <p>
   * This is essentially algorithm "Witness", from the article: Yonit Ketsen, Amir
   * Pnueli, Li-on Raviv, Elad Shahar, "Model checking with strong fairness".<br>
   * The line numbers are the line numbers of that algorithm. Read the article for
   * further details.
   * </p>
   * 
   * @param feasible
   * @param designWithTester
   * @return
   */
  private static BDD[] extractWithness(BDD feasible, PlayerModule designWithTester, BDDVarSet relevantVars,
      FeasibilityChecker fsChecker) {
    BDD temp, fulfill;
    // saving the previous restriction state.
    BDD trans = designWithTester.trans().id();

    // Lines 1-2 are handled by the caller. ("verify")

    // Line 3
    designWithTester.conjunctTrans(feasible.and(Env.prime(feasible)));

    // Line 4
    BDD s = CoreUtil.satOne(feasible, designWithTester.moduleUnprimeVars());

    // BDD s = feasible.satOne();

    // Lines 5-6
    while (true) {
      temp = designWithTester.allSucc(s.id()).and(designWithTester.allPred(s.id()).not());
      if (!temp.isZero())
        s = CoreUtil.satOne(temp, designWithTester.moduleUnprimeVars());
      // s = temp.satOne();
      else
        break;
    }
    // Lines 5-6 : better version.
    // temp = tester.allSucc(s).and(tester.allPred(s).not());
    // while (!temp.isZero()){
    // s = temp.satOne(tester.moduleUnprimeVars(), false);
    // temp = tester.allSucc(s).and(tester.allPred(s).not());
    // }

    // Line 7: Compute MSCS containing s.
    BDD feas = designWithTester.allSucc(s.id());

    // Line 9
    // Find prefix - shortest path from initial state to subgraph feas.
    designWithTester.resetTrans();
    designWithTester.conjunctTrans(trans.id());
    Vector<BDD> prefix = new Vector<BDD>();
    BDD[] path = fsChecker.shortestPath(designWithTester, designWithTester.initial(), feas);
    for (int i = 0; i < path.length; i++)
      prefix.add(path[i]);

    // //// Calculate "_period".

    // Line 8: This has to come after line 9, because the way TS.tlv
    // implements restriction.
    designWithTester.conjunctTrans(feasible.and(Env.prime(feas)));

    // Line 10
    Vector<BDD> period = new Vector<BDD>();
    period.add(prefix.lastElement());

    // Since the last item of the prefix is the first item of
    // the period we don't need to print the last item of the prefix.
    temp = prefix.remove(prefix.size() - 1);

    // Lines 11-13
    for (int i = 0; i < designWithTester.justiceNum(); i++) {
      // Line 12, check if j[i] already satisfied
      fulfill = Env.FALSE();
      for (int j = 0; j < period.size(); j++) {
        fulfill = CoreUtil.satOne(period.elementAt(j).and(designWithTester.justiceAt(i)),
            designWithTester.moduleUnprimeVars());
        // fulfill =
        // period.elementAt(j).and(design.justiceAt(i)).satOne();
        if (!fulfill.isZero())
          break;
      }
      // Line 13
      if (fulfill.isZero()) {
        BDD from = period.lastElement();
        BDD to = feas.and(designWithTester.justiceAt(i));
        path = fsChecker.shortestPath(designWithTester, from, to);
        // eliminate the edge since from is already in period
        for (int j = 1; j < path.length; j++)
          period.add(path[j]);
      }
    }

    // Lines 14-16 - removed - we don't support compassion
    /*
     * for (int i = 0; i < designWithTester.compassionNum(); i++) { if
     * (!feas.and(designWithTester.pCompassionAt(i)).isZero()) { // check if C
     * requirement i is already satisfied fulfill = Env.FALSE(); for (int j = 0; j <
     * period.size(); j++) { fulfill = period.elementAt(j).and(
     * designWithTester.qCompassionAt(i)).satOne(
     * designWithTester.moduleUnprimeVars(), false); // fulfill = //
     * period.elementAt(j).and(design.qCompassionAt(i)).satOne(); if
     * (!fulfill.isZero()) break; }
     * 
     * if (fulfill.isZero()) { BDD from = period.lastElement(); BDD to =
     * feas.and(designWithTester.qCompassionAt(i)); path =
     * designWithTester.shortestPath(from, to); // eliminate the edge since from is
     * already in period for (int j = 1; j < path.length; j++) period.add(path[j]);
     * } } }
     */
    //
    // Close cycle
    //

    // A period of length 1 may be fair, but it might be the case that
    // period[1] is not a successor of itself. The routine path
    // will add nothing. To solve this
    // case we add another state to _period, now it will be OK since
    // period[1] and period[n] will not be equal.

    // Line 17, but modified
    if (!period.firstElement().and(period.lastElement()).isZero()) {
      // The first and last states are already equal, so we do not
      // need to extend them to complete a cycle, unless period is
      // a degenerate case of length = 1, which is not a successor of
      // self.
      if (period.size() == 1) {
        // Check if _period[1] is a successor of itself.
        if (period.firstElement().and(designWithTester.succ(period.firstElement())).isZero()) {
          // period[1] is not a successor of itself: Add state to
          // period.
          period
              .add(CoreUtil.satOne(designWithTester.succ(period.firstElement()), designWithTester.moduleUnprimeVars()));
          // period.add(design.succ(period.firstElement()).satOne());

          // Close cycle.
          BDD from = period.lastElement();
          BDD to = period.firstElement();
          path = fsChecker.shortestPath(designWithTester, from, to);
          // eliminate the edges since from and to are already in
          // period
          for (int i = 1; i < path.length - 1; i++)
            period.add(path[i]);
        }
      }
    } else {
      BDD from = period.lastElement();
      BDD to = period.firstElement();
      path = fsChecker.shortestPath(designWithTester, from, to);
      // eliminate the edges since from and to are already in period
      for (int i = 1; i < path.length - 1; i++)
        period.add(path[i]);
    }

    // Yaniv - the last one is for closing the cycle. He won't be printed.
    period.add(period.firstElement());

    // There is no need to have the last state of the period
    // in the counterexample since it already appears in _period[1]
    // if (period.size() > 1)
    // temp = period.remove(period.size() -1);

    // Copy prefix and period.
    prefix.addAll(period);
    BDD[] returned_path = new BDD[prefix.size()];
    prefix.toArray(returned_path);

    // Strip auxiliary variables introduced by tester.
    if (relevantVars != null) {
      BDDVarSet extraVars = Env.globalVarsMinus(relevantVars);
      // BDDVarSet extraVars = Env.globalVarsMinus(relevantVars);
      for (int i = 0; i < returned_path.length; i++) {
        returned_path[i] = CoreUtil.satOne(returned_path[i], relevantVars).exist(extraVars);
      }
    }

    // returning to the previous restriction state
    designWithTester.resetTrans();
    designWithTester.conjunctTrans(trans.id());

    return returned_path;
  }

  private static int tester_id = 0;
  private static int field_id = 0;

  public static class LTLTesterBuilder {
    private Spec root;
    private PlayerModule tester;
    private HashMap<SpecExp, ModuleBDDField> spec2field = new HashMap<SpecExp, ModuleBDDField>();

    public LTLTesterBuilder(Spec root_spec, boolean isWeak) throws ModelCheckException, ModuleVariableException {
      this.root = root_spec;
      if (root == null)
        throw new ModelCheckException("Cannot construct a tester for" + "specification: " + root);

      this.tester = new PlayerModule();
      this.tester.setName("LTLTester_" + (++tester_id));
      createAuxVariable(root);
      constructModule(root, isWeak);
    }

    public PlayerModule getTester() {
      return this.tester;
    }

    public BDD getSpec2BDD(Spec root) throws ModelCheckException {
      if (root instanceof SpecBDD)
        return ((SpecBDD) root).getVal();
      // else it is SpecExp (cannot be a SpecCTLRange)
      SpecExp se = (SpecExp) root;
      Spec[] child = se.getChildren();
      Operator op = se.getOperator();

      if (op == Operator.NOT)
        return getSpec2BDD(child[0]).not();
      if (op == Operator.AND)
        return getSpec2BDD(child[0]).and(getSpec2BDD(child[1]));
      if (op == Operator.OR)
        return getSpec2BDD(child[0]).or(getSpec2BDD(child[1]));
      if (op == Operator.XOR)
        return getSpec2BDD(child[0]).xor(getSpec2BDD(child[1]));
      if (op == Operator.IFF)
        return getSpec2BDD(child[0]).biimp(getSpec2BDD(child[1]));
      if (op == Operator.IMPLIES)
        return getSpec2BDD(child[0]).imp(getSpec2BDD(child[1]));
      if (op.isLTLOp()) {
        ModuleBDDField f = spec2field.get(root);
        if ((f != null) && (f.getDomain().size().intValue() == 2))
          return f.getDomain().ithVar(1);
      }
      // something is wrong
      throw new ModelCheckException("Failed to find corresponding bdd" + " to specification: " + root.toString());
    }

    private void createAuxVariable(Spec s) throws ModelCheckException, ModuleVariableException {
      if (!(s instanceof SpecExp))
        return;
      // else
      SpecExp se = (SpecExp) s;
      try {
        String name = "AUX[" + (++field_id) + "]";
        ModuleBDDField f = tester.addVar(name, true);
        spec2field.put(se, f);
      } catch (ModuleException e) {
        throw new ModelCheckException("Failed naming the extra " + "auxiliary fields");
      }

      Spec[] children = se.getChildren();
      for (int i = 0; i < children.length; i++) {
        createAuxVariable(children[i]);
      }
    }

    private void constructModule(Spec root, boolean isWeak) throws ModelCheckException {
      BDD p_c1, p_c2, p_aux;

      Set<SpecExp> specifications = spec2field.keySet();
      for (SpecExp spec : specifications) {
        // TODO: AVIV (migration)
        // 1. DONE - To check the correctness of unrealizability, take the property
        // forumla from the paper (including
        // "historically", which should be supported in Spectra), and pass it surrounded
        // by negation.
        // 2. DONE - Instead of re-building the initial and safeties spec, do as Jan did
        // in the old env validation -
        // pass it into a SpecBDD, which represents an "atomic" element of the spec.
        // We do not have SpecBDD in the new env - need to add it (it's in jtlv
        // package).
        // 3. The below switch case was for supporting LTL. We only need GR1, so no need
        // for UNTIL, RELEASES, SINCE
        // and TRIGGERED.
        // 4. DONE - The FINALLY operator doesn't exist in spectra - need to add it or
        // figure out what's its equivalent in spectra
        // 5. I will pass a PlayerModule, then create a ComposedPlayerModule which will
        // have in it the original
        // PlayerModule and the tester PlayerModule.
        // 5. implement feasible in a new class - "checker" - and it should get a
        // PlayerModule which is composed
        // 6. Add to PlayerModule "compose(PlayerModule m2)" which:
        // a) create a new PlayerModule which contains:
        // - conjuncts all initials of modules
        // - conjuncts all trans of modules
        // - concatenates the lists of justices - make sure the current module list is
        // first
        // - TODO - what about vars?
        // b) return this new PlayerModule
        // 7. in the feasible - perform the "restrictTrans / Ini" by first saving the
        // ini/trans, then do
        // conjunctIni/Trans and after, reset it with the saved ini/trans.
        //

        try {
          Operator op = spec.getOperator();
          Spec[] child = spec.getChildren();
          BDD aux = getSpec2BDD(spec);
          int noo = op.numOfOperands();
          BDD c1 = (noo > 0) ? getSpec2BDD(child[0]) : null;
          BDD c2 = (noo > 1) ? getSpec2BDD(child[1]) : null;
          switch (op) {
          case PRIME:
            p_c1 = Env.prime(c1);
            tester.conjunctTrans(aux.biimp(p_c1));
            break;
          case FINALLY:
            p_aux = Env.prime(aux);
            tester.conjunctTrans(aux.biimp(c1.or(p_aux)));
            tester.addJustice(c1.or(aux.not()));
            break;
          case GLOBALLY:
            p_aux = Env.prime(aux);
            tester.conjunctTrans(aux.biimp(c1.and(p_aux)));
            tester.addJustice(c1.not().or(aux));
            break;
          case PREV:
            p_aux = Env.prime(aux);
            tester.conjunctInitial(aux.not());
            tester.conjunctTrans(p_aux.biimp(c1));
            break;
          // no BEFORE
          case ONCE:
            p_c1 = Env.prime(c1);
            p_aux = Env.prime(aux);
            tester.conjunctInitial(aux.biimp(c1));
            tester.conjunctTrans(p_aux.biimp(aux.or(p_c1)));
            break;
          case HISTORICALLY:
            p_c1 = Env.prime(c1);
            p_aux = Env.prime(aux);
            tester.conjunctInitial(aux.biimp(c1));
            tester.conjunctTrans(p_aux.biimp(aux.and(p_c1)));
            break;
          case SINCE:
            p_c1 = Env.prime(c1);
            p_c2 = Env.prime(c2);
            p_aux = Env.prime(aux);
            tester.conjunctInitial(aux.biimp(c2));
            tester.conjunctTrans(p_aux.biimp(p_c2.or(p_c1.and(aux))));
            break;
          case TRIGGERED:
            p_c1 = Env.prime(c1);
            p_c2 = Env.prime(c2);
            p_aux = Env.prime(aux);
            tester.conjunctInitial(aux.biimp(c1.or(c2)));
            tester.conjunctTrans(p_aux.biimp(p_c2.or(p_c1.and(aux))));
            break;
          // NOT_PREV_NOT,
          default:
            break;
          }
        } catch (BDDException e) {
          throw new ModelCheckException("Failed to prime BDD " + "assertion for specification: " + spec.toString());
        }
      }
      if (!isWeak) {
        tester.conjunctInitial(getSpec2BDD(root).not());
      }
    }
  }
}
