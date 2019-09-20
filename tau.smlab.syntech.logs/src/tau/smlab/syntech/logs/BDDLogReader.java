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

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Iterator;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;

/**
 * iterator over BDDs of a log
 *
 */
public class BDDLogReader implements Iterable<BDD> {

  private String fileName;

  public BDDLogReader(String fileName) {
    this.fileName = fileName;
  }

  /**
   * translate a log entry to a BDD
   * 
   * @param text
   * @return
   */
  private BDD toBDD(String text) {
    if ("FALSE".equals(text)) {
      return Env.FALSE();
    }
    if ("TRUE".equals(text)) {
      return Env.TRUE();
    }

    String[] stateVals = text.replace("<", "").replace(">", "").replace(" ", "").split(",");
    BDD res = Env.TRUE();
    // iterate over all assignments
    for (String asgmt : stateVals) {
      String[] asgm = asgmt.split(":");
      // check if variable exists
      if (Env.getVar(asgm[0]) != null) {
        BDD val = Env.getBDDValue(asgm[0], asgm[1]);
        // check if value exists
        if (val == null) {
          throw new RuntimeException("Value " + asgm[1] + " undefined for variable " + asgm[0]);
        } else {
          res.andWith(val.id());
        }
      } else {
        System.err.println("Warning:  Variable " + asgm[0] + " from log does not exist and is ignored!");
      }
    }
    return res;
  }

  @Override
  public Iterator<BDD> iterator() {
    try {
      return new Iterator<BDD>() {
        String line = null;
        boolean isNext = false;
        BufferedReader br = new BufferedReader(new FileReader(fileName));

        /**
         * check if we can read another line from the file
         */
        @Override
        public boolean hasNext() {
          if (!isNext) {
            try {
              line = br.readLine();
            } catch (IOException e) {
            }
            isNext = true;
          }
          return line != null;
        }

        /**
         * convert next line to BDD
         */
        @Override
        public BDD next() {
          if (!hasNext()) {
            return null;
          }
          isNext = false;
          return toBDD(line);
        }

        @Override
        public void remove() {
        }

      };
    } catch (IOException e) {
      e.printStackTrace();
    }
    return null;
  }
}
