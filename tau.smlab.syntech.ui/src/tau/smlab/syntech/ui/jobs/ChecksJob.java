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

package tau.smlab.syntech.ui.jobs;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import tau.smlab.syntech.checks.Checker;
import tau.smlab.syntech.gameinputtrans.translator.MonitorTranslator;
import tau.smlab.syntech.gameinputtrans.translator.CounterTranslator;
import tau.smlab.syntech.gameinputtrans.translator.PastLTLTranslator;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;

public class ChecksJob extends SyntechJob {
    
  @Override
  protected void doWork() {
    Checker c = new Checker();
    clearMarkers();

    List<BehaviorInfo> badPrimes = c.computeEnvBadIniSysSpec(model);
    if (!badPrimes.isEmpty()) {
      this.issuesKind = "Bad primes specs";
      this.numIssues = badPrimes.size();
      openView();
      printToConsole("An invalid initial assumption refers to system variables. See highlighting in editor.");
      createMarker(badPrimes, MarkerKind.CUSTOM_TEXT_ERROR);
      model.free();
      return;
    }
    
    badPrimes = c.computeEnvBadIniPrimesSpecs(model);
    if (!badPrimes.isEmpty()) {
      this.issuesKind = "Bad primes specs";
      this.numIssues = badPrimes.size();
      openView();
      printToConsole("An invalid initial assumption refers to next values of variables. See highlighting in editor.");
      createMarker(badPrimes, MarkerKind.CUSTOM_TEXT_ERROR);
      model.free();
      return;
    }
    
    badPrimes = c.computeSysBadPrimesSpecs(model);
    if (!badPrimes.isEmpty()) {
      this.issuesKind = "Bad primes specs";
      this.numIssues = badPrimes.size();
      openView();
      printToConsole("An invalid initial guarantee refers to next values of variables. See highlighting in editor.");
      createMarker(badPrimes, MarkerKind.CUSTOM_TEXT_ERROR);
      model.free();
      return;
    }
    
    badPrimes = c.computeEnvBadSafetyPrimesSpecs(model);
    if (!badPrimes.isEmpty()) {
      this.issuesKind = "Bad primes specs";
      this.numIssues = badPrimes.size();
      openView();
      printToConsole("An invalid safety assumption refers to next values of system variables. See highlighting in editor.");
      createMarker(badPrimes, MarkerKind.CUSTOM_TEXT_ERROR);
      model.free();
      return;
    }
    
    if (!this.checkCounters(c, model)) {
    	return;
    }

    List<BehaviorInfo> trivial = c.computeTrivialSpecs(model);
    if (!trivial.isEmpty()) {
      this.issuesKind = "Trivial specs";
      this.numIssues = trivial.size();

      openView();
      printToConsole("Found constraints that are trivially TRUE or FALSE. See highlighting in editor.");
      createMarker(trivial, MarkerKind.CUSTOM_TEXT_MARKER);
      model.free();
      return;
    }
    
    if (!this.checkMonitors(c, model)) {
      return;
    }

    List<BehaviorInfo> core = c.computeUnsatSafetyCore(model);
    if (!core.isEmpty()) {
      this.issuesKind = "Safety core";
      this.numIssues = core.size();

      openView();
      printToConsole("Found an unsatisfiable subset of guarantees. See highlighting in editor.");
      createMarker(core, MarkerKind.UNSAT_CORE);
      model.free();
      return;
    }

    List<String> examples = new ArrayList<>();
    List<BehaviorInfo> iniCore = c.computeIniDeadlockCore(model, examples);
    if (iniCore!=null) {
      this.issuesKind = "Safety core";
      this.numIssues = iniCore.size();

      openView();
      printToConsole("The environment can force the system to a deadlock from the initial state. It can do so by setting:");
      printToConsole(examples.get(examples.size()-1));
      printToConsole("See highlighting of guarantees in the editor.");
      createMarker(iniCore, MarkerKind.UNSAT_CORE);
      model.free();
      return;
    }


    List<BehaviorInfo> deadSpecs = c.computeUnreachableJustice(model);
    if (!deadSpecs.isEmpty()) {
      this.issuesKind = "Unreachable justice";
      this.numIssues = deadSpecs.size();

      openView();
      printToConsole("Detected unreachable or deadlock justice constraints. See highlighting in editor.");
      createMarker(deadSpecs, MarkerKind.CUSTOM_TEXT_MARKER);
      model.free();
      return;
    }

    printToConsole("Nothing strange to report.");
    model.free();
  }

  private void openView() {
    PlatformUI.getWorkbench().getDisplay().syncExec(new Runnable() {
      public void run() {
        try {
          PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().showView("specAnalysisMarker");
        } catch (PartInitException e) {
          System.err.println("View id not found");
        }
      }
    });

  }
  /**
   * Checks monitors for completeness and deadlocks, i.e., whether exists a contradiction in the definition of one of the monitors.
   * @param c
   * @param model
   * @return false if there is a problem with the monitors 
   */
  public boolean checkMonitors(Checker c, GameModel model) {

    List<BehaviorInfo> badMonitorSpecs = c.checkMonitorsForCompleteness(model, this.getTranslator(MonitorTranslator.class), 
        this.getTranslator(PastLTLTranslator.class));
    if(!badMonitorSpecs.isEmpty()) {
      String[] monitorCheckInfo = c.getMonitorCheckMessages();
      String badMonName = "";
      if(monitorCheckInfo.length > 0) {
        badMonName = ": " + monitorCheckInfo[0];
      }
      this.issuesKind = "Incomplete monitor" + badMonName;
      this.numIssues = badMonitorSpecs.size();

      openView();
      printToConsole("Detected an incomplete monitor" + badMonName + ". See highlighting in editor.");
      if(monitorCheckInfo.length > 1) {
        printToConsole("In the following reachable state the monitor does not have any successors:");
        printToConsole(monitorCheckInfo[1]);
      }
      createMarker(badMonitorSpecs, MarkerKind.MON_NOT_COMP);
      model.free();
      return false;
    }
    return true;
  }
  
  /**
   * Checks counters for consistency. i.e., whether 2 of its predicates (e.g., inc and reset)
   * can both return true in some reachable state of the game.
   * @param c
   * @param model
   * @return false if there are inconsistent counters. True otherwise.
   */
  public boolean checkCounters(Checker c, GameModel model) {
    List<BehaviorInfo> badCounterSpecs = c.checkCountersConsistency(model, this.getTranslator(CounterTranslator.class));
    if(!badCounterSpecs.isEmpty()) {
      String[] counterCheckInfo = c.getCounterCheckMessages();
      String badCounterName = "";
      if(counterCheckInfo.length > 0) {
        badCounterName = ": " + counterCheckInfo[0];
      }
      this.issuesKind = "Inconsistent counter" + badCounterName;
      this.numIssues = badCounterSpecs.size();

      openView();
      printToConsole("Detected an inconsistent counter" + badCounterName + ". See highlighting in editor.");
      if(counterCheckInfo.length > 1) {
        printToConsole("In the following reachable state the counter has multiple successors:");
        printToConsole(counterCheckInfo[1]);
      }
      createMarker(badCounterSpecs, MarkerKind.COUNTER_NOT_CONS);
      model.free();
      return false;
    }
    return true;
  }
}
