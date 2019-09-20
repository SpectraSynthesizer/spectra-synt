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

import java.io.File;
import java.io.IOException;
import java.util.Set;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.games.util.SaveLoadWithDomains;
import tau.smlab.syntech.jtlv.Env;

public class SymbolicControllerReaderWriter {

  /**
   * stores a symbolic controller to a file path
   * 
   * @param ctrl
   * @param model
   * @param path
   *          needs to be a folder name (folder will be created if not exists)
   * @throws IOException
   */
  public static void writeSymbolicController(SymbolicController ctrl, GameModel model, String path)
      throws IOException {
    File f = new File(path);
    if (!f.exists()) {
      f.mkdir();
    }
    Env.disableReorder();
    SaveLoadWithDomains.saveStructureAndDomains(path + "/vars.doms", model);
    Env.saveBDD(path + "/controller.init.bdd", ctrl.initial());
    Env.saveBDD(path + "/controller.trans.bdd", ctrl.trans());
  }
  

  /**
   * writes only the BDDs of a symbolic controller to a file path
   * 
   * @param ctrl
   * @param path
   *          needs to be a folder name (folder will be created if not exists)
   * @throws IOException
   */
  public static void writeSymbolicController(SymbolicController ctrl, String path)
      throws IOException {
    File f = new File(path);
    if (!f.exists()) {
      f.mkdir();
    }
    Env.disableReorder();
    Env.saveBDD(path + "/controller.init.bdd", ctrl.initial());
    Env.saveBDD(path + "/controller.trans.bdd", ctrl.trans());
  }

  /**
   * loads a symbolic controller from a file path
   * 
   * Also loads and creates all BDD variables needed for it!
   * 
   * @param path
   * @return
   * @throws IOException
   */
  public static SymbolicController readSymbolicController(String path)
      throws IOException {

    SaveLoadWithDomains.loadStructureAndDomains(path + "/vars.doms");
    BDD init = Env.loadBDD(path + "/controller.init.bdd");
    init = init.exist(Env.globalPrimeVars());
    BDD trans = Env.loadBDD(path + "/controller.trans.bdd");

    SymbolicController ctrl = new SymbolicController();
    ctrl.setTrans(trans);
    ctrl.setInit(init);

    return ctrl;
  }

  /**
   * load names of system variables from directory of symbolic controller
   * 
   * @param path
   * @return
   * @throws IOException
   */
  public static Set<String> readSysVarNames(String path) throws IOException {
    return SaveLoadWithDomains.loadSysVarNames(path + "/vars.doms");
  }

  /**
   * load names of environment variables from directory of symbolic controller
   * 
   * @param path
   * @return
   * @throws IOException
   */
  public static Set<String> readEnvVarNames(String path) throws IOException {
    return SaveLoadWithDomains.loadEnvVarNames(path + "/vars.doms");
  }
}
