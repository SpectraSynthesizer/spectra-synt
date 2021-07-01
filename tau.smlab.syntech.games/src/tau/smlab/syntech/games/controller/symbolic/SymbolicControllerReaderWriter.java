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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;

import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.games.util.SaveLoadWithDomains;
import tau.smlab.syntech.jtlv.Env;

public class SymbolicControllerReaderWriter {
	
	protected final static String CONTROLLER_INIT = "controller.init.bdd";
	protected final static String CONTROLLER_TRANS = "controller.trans.bdd";
	protected final static String VARS = "vars.doms";
	
	protected final static String SIZES = "sizes";
	protected final static String EXT_SIZES = "existential_sizes";
	protected final static String FIXPOINTS = "fixpoints.bdd";
	protected final static String TRANS = "trans.bdd";
	protected final static String JUSTICE = "justice.bdd";
	
	protected final static String FULFILL = "fulfill.bdd";
	protected final static String TOWARDS = "towards.bdd";
	protected final static String ENV_VIOLATION = "envviolation.bdd";

	/**
	 * loads a symbolic controller from a file path
	 * 
	 * Also loads and creates all BDD variables needed for it!
	 * 
	 * @param path
	 * @return
	 * @throws IOException
	 */
	public static SymbolicController readSymbolicController(ObjectInputStream ois, BufferedReader initReader,
			BufferedReader transReader) throws IOException {

		// TODO: not implemented yet
		return null;
	}

	/**
	 * stores a symbolic controller to a file path
	 * 
	 * @param ctrl
	 * @param model
	 * @param path  needs to be a folder name (folder will be created if not exists)
	 */
	public static void writeSymbolicController(SymbolicController ctrl, GameModel model, String path, boolean reorderBeforeSave) throws IOException {

		// write the actual symbolic controller BDDs and doms

		Env.disableReorder();

		File folder = new File(path);
		if (!folder.exists()) {
			folder.mkdir();
		}
		
		String prefix = path + File.separator;
		
		SaveLoadWithDomains.saveStructureAndDomains(prefix + VARS, model);

		Env.saveBDD(prefix + CONTROLLER_INIT, ctrl.initial(), reorderBeforeSave);
		Env.saveBDD(prefix + CONTROLLER_TRANS, ctrl.trans(), reorderBeforeSave);
	}
	
	/**
	 * stores a symbolic controller just in time info to a file path
	 * 
	 * @param jitInfo
	 * @param model
	 * @param path
	 * @param reorderBeforeSave
	 * @throws IOException
	 */
	public static void writeJitSymbolicController(SymbolicControllerJitInfo jitInfo, GameModel model, String path, boolean reorderBeforeSave) throws IOException {
		
		File folder = new File(path);
		if (!folder.exists()) {
			folder.mkdir();
		}
		
		String prefix = path + File.separator;
        
        FileWriter sizesWriter = new FileWriter(prefix + SIZES);
        sizesWriter.write(model.getSys().justiceNum() + System.lineSeparator() + model.getEnv().justiceNum() + System.lineSeparator());
        
        
        for (int j = 0; j < model.getSys().justiceNum(); j++) {
        	sizesWriter.write((jitInfo.ranks(j) - 1) + System.lineSeparator());
        }
        
        sizesWriter.close();
        
        SaveLoadWithDomains.saveStructureAndDomains(prefix + VARS, model);
        
        Env.saveBDD(prefix + File.separator + FIXPOINTS, jitInfo.fixpoints(), reorderBeforeSave);
        Env.saveBDD(prefix + File.separator + TRANS, jitInfo.safeties(), reorderBeforeSave);
        Env.saveBDD(prefix + File.separator + JUSTICE, jitInfo.justices(), reorderBeforeSave);
        
        if (model.getSys().hasExistReqs()) {
        	
        	SymbolicControllerExistentialJitInfo extJitInfo = (SymbolicControllerExistentialJitInfo) jitInfo;
        	
            sizesWriter = new FileWriter(prefix + EXT_SIZES);
            
            sizesWriter.write(model.getSys().existReqNum() + System.lineSeparator());
            
            for (int exj = 0; exj < model.getSys().existReqNum(); exj++) {
            	sizesWriter.write((extJitInfo.fulfillRanks(exj) - 1) + System.lineSeparator());
            }
            for (int exj = 0; exj < model.getSys().existReqNum(); exj++) {
            	sizesWriter.write((extJitInfo.towardsRanks(exj) - 1) + System.lineSeparator());
            }
            
            sizesWriter.write(extJitInfo.envViolationRank() + System.lineSeparator());
            
            sizesWriter.close();
            
            Env.saveBDD(prefix + File.separator + FULFILL, extJitInfo.fulfill(), reorderBeforeSave);
            Env.saveBDD(prefix + File.separator + TOWARDS, extJitInfo.towards(), reorderBeforeSave);
            Env.saveBDD(prefix + File.separator + ENV_VIOLATION, extJitInfo.envViolation(), reorderBeforeSave);
        }

	}
	
}
