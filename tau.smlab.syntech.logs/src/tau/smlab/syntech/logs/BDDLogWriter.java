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

package tau.smlab.syntech.logs;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.jtlv.CoreUtil;
import tau.smlab.syntech.jtlv.Env;

public class BDDLogWriter {

  private BufferedWriter writer;
  private BDDVarSet vars;

  /**
   * by default logging all unprimed vars
   * 
   * @param fileName
   * @throws IOException
   */
  public BDDLogWriter(String fileName) throws IOException {
    this.writer = initWriter(fileName);
    this.vars = Env.globalUnprimeVars();
  }

  public BDDLogWriter(String fileName, BDDVarSet vars) throws IOException {
    this.writer = initWriter(fileName);
    this.vars = vars;
  }

  /**
   * create new buffered writer
   * 
   * @param fileName
   * @return
   * @throws IOException
   */
  private BufferedWriter initWriter(String fileName) throws IOException {
    return new BufferedWriter(new FileWriter(fileName));
  }

  /**
   * write the BDD to the log
   * 
   * if the BDD has more than one assignment, writes one of them, if the BDD has no assignments writes FALSE
   * 
   * @param entry
   * @throws IOException
   */
  public void write(BDD entry) throws IOException {
    if (entry.isZero()) {
      writer.write("FALSE");
    } else if (entry.satCount() > 1 && !vars.isEmpty()) {
      BDD one = CoreUtil.satOne(entry, vars);
      writer.write(one.toStringWithDomains(Env.stringer));
      one.free();
    } else if (vars.isEmpty() && !entry.isZero()) {
      writer.write("TRUE");
    } else {
      writer.write(entry.toStringWithDomains(Env.stringer));
    }
    writer.newLine();
    writer.flush();
  }

  /**
   * closes the file
   * 
   * @throws IOException
   */
  public void close() throws IOException {
    writer.close();
  }

}
