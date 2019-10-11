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

package tau.smlab.syntech.executor;

import java.io.File;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDDomain;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.games.controller.symbolic.SymbolicController;
import tau.smlab.syntech.games.controller.symbolic.SymbolicControllerReaderWriter;
import tau.smlab.syntech.jtlv.BDDPackage;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.BDDPackage.BBDPackageVersion;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;
import tau.smlab.syntech.logs.BDDLogWriter;

/**
 * 	
 * A class for the execution of symbolic controllers
 *
 */
public class ControllerExecutor {

	private SymbolicController ctrl;
	private BDD currentState;
	private boolean initialState;
	private boolean envDeadlock;
	private Map<String, String> inputValues;
	private Set<String> sysVarNames;
	private Set<String> envVarNames;
	private String[] sysVarNamesArr;
	private String[] envVarNamesArr;

	/*#########################LOG#############*/
	private BDDLogWriter log;

	/**
	 * Instantiates a new symbolic controller executor. By default, uses the pure Java implementation and logs
	 * the execution into a file created in ./logs folder
	 */
	public ControllerExecutor() {
		this(true, true);
	}

	/**
	 * Instantiates a new symbolic controller executor. By default, uses the pure Java implementation.
	 * @param logExec whether to log the execution into a file created in ./logs folder
	 */
	public ControllerExecutor(boolean logExec) {
		this(logExec, true);
	}

	/**
	 * Instantiates a new symbolic controller executor.
	 * @param logExec whether to log the execution into a file created in ./logs folder
	 * @param pureJavaImpl whether to use a pure Java implementation. For better performance,
	 * use the native, non-Java implementation that requires the 'cudd3.dll' file in the root folder of the Eclipse project
	 */
	public ControllerExecutor(boolean logExec, boolean pureJavaImpl) {
		loadController(pureJavaImpl);
		initialState = true;
		inputValues = new HashMap<>();
		envDeadlock = false;
		log = null;
		if(logExec) {
			initLog();
		}
	}

	/**
	 * Checks whether the controller execution has reached a state which is a deadlock for the environment, i.e.,
	 * a state from which the environment has no choice for next inputs that satisfy the safety assumptions
	 * @return
	 */
	public boolean reachedEnvDeadlock() {
		return envDeadlock;
	}
	/**
	 * Returns the set of all output (system controlled) variables' names
	 * @return
	 */
	public Set<String> getSysVarNames() {
		return sysVarNames;
	}

	/**
	 * Returns the set of all input (environment controlled) variables' names
	 * @return
	 */
	public Set<String> getEnvVarNames() {
		return envVarNames;
	}

	/**
	 * Set the next value of the specified input variable. This value assignment is removed after the next state is picked by {@link #updateState(boolean)}
	 * @param varName the name of the input (environment controlled) variable
	 * @param value the next value
	 * @throws ControllerExecutorException if the variable name, varName, does not exist in the controller as an input variable or
	 * the value is not in the domain of the variable varName
	 */
	public void setInputValue(String varName, String value) throws ControllerExecutorException {
		BDD valBdd = Env.getBDDValue(varName, value);
		if(valBdd == null) {
			throw new ControllerExecutorException("Invalid variable name or value");
		}
		if(!envVarNames.contains(varName)) {
			throw new ControllerExecutorException("The variable name does not refer to an input variable");
		}
		inputValues.put(varName, value);
	}

	public void removeInputValue(String varName, String value) {
		inputValues.remove(varName);
	}
	/**
	 * Picks a valid next state according to the controller and the next inputs from the environment, specified
	 * via {@link #setInputValue(String, String)}.
	 * Note that the assignment to the next inputs may be partial, i.e., may not assign values to all input variables, and
	 * will be removed from the executor and, consequently, cannot be reused.
	 * If the current state is a deadlock for the environment, i.e., the environment
	 * has no choice for inputs that satisfy the assumptions, no next state is picked.
	 * 
	 * @param randomChoice whether the next state should be picked randomly
	 * @throws ControllerExecutorException if the current state is not an environment deadlock and the inputs set via {@link #setInputValue(String, String)}
	 * violate the safety assumptions of the environment
	 */
	public void updateState(boolean randomChoice) throws ControllerExecutorException {
		updateState(randomChoice, true);
	}

	/**
	 * Picks a valid next state according to the controller and possibly the next inputs from the environment, specified
	 * via {@link #setInputValue(String, String)}. If the current state is a deadlock for the environment, i.e., the environment
	 * has no choice for inputs that satisfy the assumptions, no next state is picked.
	 * 
	 * @param randomChoice whether the next state should be picked randomly
	 * @param withInputs whether the next state should have the assignment,
	 * which has been set via {@link #setInputValue(String, String)}, to the input variables. Note that
	 * this assignment may be partial, i.e., may not assign values to all input variables.
	 * If withInputs is true, this assignment is removed from the executor and, consequently, cannot be reused.
	 * 
	 * @throws ControllerExecutorException if the current state is not an environment deadlock and
	 * withInputs is true and the inputs set via {@link #setInputValue(String, String)}
	 * violate the safety assumptions of the environment
	 */
	public void updateState(boolean randomChoice, boolean withInputs) throws ControllerExecutorException {
		if(envDeadlock) {
			return;
		}
		BDD inputs = Env.TRUE();
		if(withInputs) {
			for(String varName : inputValues.keySet()) {
				inputs.andWith(Env.getBDDValue(varName, inputValues.get(varName)).id());
			}
			inputValues.clear();
		}
		if (initialState) {
			if(currentState.isZero()) {
				envDeadlock = true;
				return;
			}
			BDD curStateWithInputs = currentState.and(inputs), initialStateChoice;
			if(curStateWithInputs.isZero()) {
				throw new ControllerExecutorException("The specified initial input values do not satisfy the initial assumptions");
			}
			if(randomChoice) {
				initialStateChoice = randomSat(curStateWithInputs, Env.globalUnprimeVars()); 
			}
			else {
				initialStateChoice = curStateWithInputs.satOne(Env.globalUnprimeVars());		    		
			}
			curStateWithInputs.free();
			currentState.free();
			currentState = initialStateChoice;
			initialState = false;
		}
		else {
			BDD succs = ctrl.succ(currentState);
			if(succs.isZero()) {
				envDeadlock = true;
				return;
			}
			succs.andWith(inputs);
			if(succs.isZero()) {
				throw new ControllerExecutorException("The specified input values do not satisfy the safety assumptions");
			}
			if(randomChoice) {
				currentState = randomSat(succs, Env.globalUnprimeVars());		    	  
			}
			else {
				currentState = succs.satOne(Env.globalUnprimeVars());		    
			}
			succs.free();
		}
		if(log != null) {
			//try to log the current state
			try {
				log.write(currentState);
			} catch (IOException e) {
			}
		}
	}

	/**
	 * Returns a mapping of all system variables' names to their current values
	 * @return
	 */
	public Map<String, String> getCurOutputs() {
		Map<String, String> outputToVal = null;
		try {
			outputToVal = getModuleValues(sysVarNamesArr);
		}
		catch (ControllerExecutorException ce) {
		}
		return outputToVal;
	}

	/**
	 * Returns a mapping of all environment variables' names to their current values
	 * @return
	 */
	public Map<String, String> getCurInputs() {
		Map<String, String> inputToVal = null;
		try {
			inputToVal = getModuleValues(envVarNamesArr);
		}
		catch (ControllerExecutorException ce) {
		}
		return inputToVal;
	}

	/**
	 * Returns a mapping of all specified variables' names to their current values.
	 * Each variable name may refer to either an input or an output variable
	 * 
	 * @param varNames the variables' names that will be mapped to their current values
	 * @return
	 * @throws ControllerExecutorException if there is a variable name that does not exist in the controller
	 */
	public Map<String, String> getCurValues(String ... varNames) throws ControllerExecutorException {
		return getModuleValues(varNames);
	}

	/**
	 * Returns the current value of the specified variable name (either an input or an output)
	 *
	 * @param varName The variable name
	 * @return
	 * @throws ControllerExecutorException if the variable name does not exist in the controller
	 */
	public String getCurValue(String varName) throws ControllerExecutorException {
		if(initialState || currentState.isZero()) {
			return "";
		}

		ModuleBDDField field = Env.getVar(varName);
		if(field == null) {
			throw new ControllerExecutorException("The variable name " + varName + " does not exist in the controller");
		} 

		BDDDomain domain = field.getDomain();
		int[] domVarIdx = domain.vars();
		int[] varPolarity = new int[domVarIdx.length];
		BDD ithVar, valCheck, res = Env.TRUE();
		for(int i = 0 ; i < domVarIdx.length ; i++) {
			ithVar = currentState.getFactory().ithVar(domVarIdx[i]);
			valCheck = currentState.and(res).andWith(ithVar.id());
			if(!valCheck.isZero()) {
				res.andWith(ithVar);
				varPolarity[i] = 1;
			}
			else {
				res.andWith(currentState.getFactory().nithVar(domVarIdx[i]));
				varPolarity[i] = 0;
				ithVar.free();
			}
			valCheck.free();
		}
		res.free();
		BigInteger pos = BigInteger.ZERO;
		for(int i = domVarIdx.length - 1 ; i >= 0 ; i--) {
			pos = pos.shiftLeft(1);
			if(varPolarity[i] == 1) {
				pos = pos.setBit(0);
			}
		}

		return Env.stringer.elementName(domain.getIndex(), pos);
	}

	private Map<String, String> getModuleValues(String[] moduleVarNames) throws ControllerExecutorException {
		Map<String, String> varToVal = new HashMap<>();
		for(String varName : moduleVarNames) {
			varToVal.put(varName, getCurValue(varName));
		}
		return varToVal;
	}

	private void loadController(boolean pureJavaImpl) {
		if(pureJavaImpl) {
			BDDPackage.setCurrPackage(BDDPackage.JTLV);
		}
		else {
			//native BDD package
			BDDPackage.setCurrPackage(BDDPackage.CUDD, BBDPackageVersion.CUDD_3_0);			
		}

		String path = "out";

		try {
			sysVarNames = Collections.unmodifiableSet(SymbolicControllerReaderWriter.readSysVarNames(path));
			envVarNames = Collections.unmodifiableSet(SymbolicControllerReaderWriter.readEnvVarNames(path));
			ctrl = SymbolicControllerReaderWriter.readSymbolicController(path);
		} catch (IOException e) {
			e.printStackTrace();
		}

		Env.enableReorder();
		currentState = ctrl.initial().id();
		initialState = true;

		sysVarNamesArr = sysVarNames.toArray(new String[0]);
		envVarNamesArr = envVarNames.toArray(new String[0]);

	}

	/**
	 * Returns a BDD that represents a satisfying assignment to the given set of states. The assignment is random and refers to
	 * the specified Boolean variables.
	 * 
	 * @param states The set of states
	 * @param booleanVars
	 *          The Boolean variables the assignment should refer to
	 * @return A random assignment to the specified variables that satisfies the given set of states.
	 */
	private BDD randomSat(BDD states, BDDVarSet booleanVars) {

		if (states.isZero()) {
			return Env.FALSE();
		}

		//now we are sure we have a non trivially FALSE BDD

		BDD satRes = Env.TRUE(), satCheck;
		Random random = new Random();
		boolean randChoice;
		int[] varProfile = booleanVars.toBDD().varProfile();
		for (int i = 0; i < varProfile.length; i++) {
			if (varProfile[i] > 0) {
				randChoice = random.nextBoolean();
				if (randChoice) {
					satCheck = states.and(satRes).andWith(states.getFactory().ithVar(i));
				} else {
					satCheck = states.and(satRes).andWith(states.getFactory().nithVar(i));
				}
				if (!satCheck.isZero()) {
					satRes.andWith(randChoice ? states.getFactory().ithVar(i) : states.getFactory().nithVar(i));
				} else {
					satRes.andWith(randChoice ? states.getFactory().nithVar(i) : states.getFactory().ithVar(i));
				}
				satCheck.free();
			}
		}
		return satRes;
	}

	/**
	 * Creates folder ./logs if it doesn't exist. Prepares the log object to write a BDD log
	 */
	private void initLog() {
		File lp = new File("logs");
		if (!lp.exists()) {
			lp.mkdir();
		}
		String ts = "" + System.currentTimeMillis();
		String logName = lp.getPath() + "/" + ts + ".gol";
		try {
			log = new BDDLogWriter(logName, Env.globalUnprimeVars());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}


}
