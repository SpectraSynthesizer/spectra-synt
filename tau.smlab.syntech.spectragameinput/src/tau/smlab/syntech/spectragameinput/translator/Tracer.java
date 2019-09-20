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

package tau.smlab.syntech.spectragameinput.translator;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;

import tau.smlab.syntech.spectra.LTLAsm;
import tau.smlab.syntech.spectra.LTLGar;

/**
 * Tracer maintains a connection between EObjects (e.g., elements of a Spectra specification) and traceIds
 * (integers).<br>
 * Clients can look up this connection in both ways.
 *
 */
public class Tracer {

  public static int ID = 0;
  private Map<EObject, Integer> tracingMap = new HashMap<>();
  private static Map<Integer, EObject> trace = new HashMap<>();

  public Tracer() {
    tracingMap = new HashMap<>();
    trace = new HashMap<>();
  }

  /**
   * creates a new entry for the EObject with a fresh traceID
   * 
   * does not check whether the object already exists!
   * 
   * @param eobject
   * @return the ID of the added object
   */
  public int addTrace(EObject eobject) {
    trace.put(ID, eobject);
    tracingMap.put(eobject, ID);
    ID++;
    return ID-1;
  }

  /**
   * looks up the traceId of an EObject
   * 
   * @param eobject
   * @return ID of the traced eobject if exists. -1 otherwise.
   */
  public int getTrace(EObject eobject) {
    if (!tracingMap.containsKey(eobject)) {
      return -1;
    }
    return tracingMap.get(eobject);
  }

  /**
   * returns the EObject of this traceId (e.g., returns an element of the Spectra specification)
   * 
   * @param traceId
   * @return
   */
  public static EObject getTarget(int traceId) {
    return trace.get(traceId);
  }

  /**
   * translates the element with the given traceId to a string (e.g., prints a guarantee from the specification)
   * 
   * @param traceId
   * @return
   */
  public static String getNiceStringForId(int traceId) {
    if (traceId == -1) {
      return "TRUE";
    }
    EObject obj = getTarget(traceId);
    if (obj instanceof LTLGar) {
      LTLGar gar = (LTLGar) obj;
      if (gar.getName() != null) {
        return "guarantee " + gar.getName();
      }
    } else if (obj instanceof LTLAsm) {
      LTLAsm asm = (LTLAsm) obj;
      if (asm.getName() != null) {
        return "assumption " + asm.getName();
      }
    }
    return NodeModelUtils.getTokenText(NodeModelUtils.findActualNodeFor(obj));
  }
}
