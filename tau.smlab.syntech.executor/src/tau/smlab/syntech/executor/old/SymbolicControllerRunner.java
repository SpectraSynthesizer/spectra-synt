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

package tau.smlab.syntech.executor.old;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import il.ac.tau.cs.smlab.model2lejos.library.Port;
import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.games.controller.symbolic.SymbolicController;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerReaderWriter;
import tau.smlab.syntech.jtlv.BDDPackage;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.logs.BDDLogWriter;

public class SymbolicControllerRunner {

  public static boolean OUTPUT = true;
  private static BDDLogWriter log;

  @SuppressWarnings("rawtypes")
  private Map<String, Port> inputPorts = new HashMap<>();
  @SuppressWarnings("rawtypes")
  private Map<String, Port> outputPorts = new HashMap<>();
  @SuppressWarnings("rawtypes")
  private Map<String, Class> outputPortValueTypes = new HashMap<>();

  private BDD currentState;
  private SymbolicController ctrl;
  private boolean initialState = true;

  public void init() {
    BDDPackage.setCurrPackage(BDDPackage.JTLV);

    try {
      ctrl = SymbolicControllerReaderWriter.readSymbolicController("out/");
    } catch (IOException e) {
      e.printStackTrace();
    }

    Env.enableReorder();

    currentState = ctrl.initial().id();
    initialState = true;

    prepareLog();
  }

  /**
   * <li>creates folder logs if it doesn't exist</li>
   * <li>copies the spec from the symbolic controller to the logs</li>
   * <li>prepares the log object to write a BDD log</li>
   * 
   */
  private void prepareLog() {
    File lp = new File("logs");
    if (!lp.exists()) {
      lp.mkdir();
    }
    String ts = "" + System.currentTimeMillis();
    String logName = lp.getPath() + "/" + ts + ".gol";
    try {
      log = new BDDLogWriter(logName, getVarSetFromIO());
      Files.copy(Paths.get("out", "spec.zip"), Paths.get(lp.getPath(), ts + "spec.zip"));
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  /**
   * compute the varset of variables used as input and output ports
   * 
   * @return
   */
  private BDDVarSet getVarSetFromIO() {
    BDDVarSet vars = Env.getEmptySet();
    for (String pName : inputPorts.keySet()) {
      vars.unionWith(Env.getVar(pName).support());
    }
    for (String pName : outputPorts.keySet()) {
      vars.unionWith(Env.getVar(pName).support());
    }
    return vars;
  }

  /**
   * register an input port with its name in the cmp file
   * 
   * @param name
   * @param p
   */
  public void addInputPort(String name, Port<?> p) {
    inputPorts.put(name, p);
  }

  /**
   * register an output port from the cmp file
   * 
   * @param name
   *          name as in cmp file
   * @param valueType
   *          the type of the port values, e.g., Boolean.class
   * @param p
   *          the port object
   */
  public void addOutputPort(String name, Class<?> valueType, Port<?> p) {
    outputPorts.put(name, p);
    outputPortValueTypes.put(name, valueType);
  }

  /**
   * do actural computation step by reading all inputs, determining next state, and writing all outputs on ports
   */
  public void compute() {
    BDD input = getInputBDD();

    boolean outputComputed = false;
    if (initialState) {
      currentState.andWith(input);
      if (!currentState.isZero()) {
        BDD one = currentState.satOne(Env.globalUnprimeVars());
        currentState.free();
        currentState = one;
        initialState = false;
        outputComputed = true;
        System.out.println("Initial state:");
        System.out.println(currentState.toStringWithDomains(Env.stringer));
        try {
          log.write(currentState);
        } catch (IOException e) {
        }
      } else if (OUTPUT) {
        System.out.println("No initial state matching current input:");
        System.out.println(input.toStringWithDomains(Env.stringer));
      }
    } else {
      BDD succs = ctrl.succ(currentState);
      if (OUTPUT && succs.isZero()) {
        // system forced environment into a deadlock
        System.out.println("No more successors at all.");
      } else {
        succs.andWith(input.id());
        if (!succs.isZero()) {
          currentState.free();
          currentState = succs.satOne(Env.globalUnprimeVars());
          succs.free();
          outputComputed = true;
          System.out.println("Current state:");
          System.out.println(currentState.toStringWithDomains(Env.stringer));
          try {
            log.write(currentState);
          } catch (IOException e) {
          }
        } else if (OUTPUT) {
          // environment violates a safety assumption  
          System.out.println("No more successors for current input:");
          System.out.println(input.toStringWithDomains(Env.stringer));
        }
        input.free();
      }
    }

    produceOutputOnPorts(outputComputed);
  }

  /**
   * if outputComputed produce messages in currentState on all output ports and produce null otherwise on all output
   * ports
   * 
   * @param outputComputed
   */
  @SuppressWarnings({ "unchecked", "rawtypes" })
  private void produceOutputOnPorts(boolean outputComputed) {
    if (outputComputed) {
      String state = currentState.toStringWithDomains(Env.stringer);
      String[] stateVals = state.replace("<", "").replace(">", "")
          .replace(" ", "").split(",");
      for (String name : outputPorts.keySet()) {
        Class valueType = outputPortValueTypes.get(name);
        Object value = null;
        if (valueType.equals(Boolean.class)) {
          value = new Boolean("true".equals(getVal(stateVals, name)));
        } else if (valueType.equals(Integer.class)) {
          value = Integer.parseInt(getVal(stateVals, name));
        } else {
          value = Enum.valueOf(outputPortValueTypes.get(name), getVal(stateVals, name));
        }
        outputPorts.get(name).setNextValue(value);
      }
    } else {
      for (String name : outputPorts.keySet()) {
        outputPorts.get(name).setNextValue(null);
      }
    }
  }

  @SuppressWarnings("unchecked")
  private BDD getInputBDD() {
    BDD input = Env.TRUE();

    for (String name : inputPorts.keySet()) {
      input.andWith(port2BDD(name, inputPorts.get(name)));
    }

    return input;
  }

  private String getVal(String[] stateVals, String name) {
    for (int i = 0; i < stateVals.length; i++) {
      if (stateVals[i].startsWith(name + ":")) {
        return stateVals[i].split(":")[1];
      }
    }
    return null;
  }

  /**
   * gets the BDD representing the variable <code>name</code> for the assignment <code>p.getCurrentValue()</code> types
   * that are supported are either Boolean or enumeration types
   * 
   * @param name
   * @param p
   * @return
   */
  private <T> BDD port2BDD(String name, Port<T> p) {
    if (p.getCurrentValue() != null) {
      String val = p.getCurrentValue().toString();
      if (p.getCurrentValue() instanceof Boolean) {
        if ((Boolean) p.getCurrentValue()) {
          val = "true";
        } else {
          val = "false";
        }
      }
      BDD input = Env.getBDDValue(name, val);
      if (input != null) {
        return input.id();
      } else {
        System.err.println("Unknown value " + val + " on port " + name + ".");
      }
    }
    System.err.println("No input on port " + name + ". Make sure all sensors are executed first!");
    return Env.FALSE();
  }

}
