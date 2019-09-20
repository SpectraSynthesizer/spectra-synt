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

public enum MarkerKind {
  WELL_SEP_CORE("Assumption which is part of the Well-Separated Core", "tau.smlab.syntech.ui.marker"), 
  UNSAT_CORE("Guarantee which is part of the unsatisfiable Core", "tau.smlab.syntech.ui.marker"),
  UNREAL_CORE("Guarantee which is part of the unrealizable Core", "tau.smlab.syntech.ui.marker"),
  UNSAT_MON_CORE("Monitor constraint which is part of an unsatisfiable core of its monitor", "tau.smlab.syntech.ui.marker"),
  MON_NOT_COMP("Constraint in a non complete monitor. Each monitor must be defined for all possible values the specification variables can get", "tau.smlab.syntech.ui.marker"),
  COUNTER_NOT_CONS("Constraint in a non consistent counter. Each counter must be uniquely defined for all possible values the specification variables can get", "tau.smlab.syntech.ui.marker"),
  CUSTOM_TEXT_MARKER("Info, see console...", "tau.smlab.syntech.ui.marker"),
  CUSTOM_TEXT_ERROR("Error, see console...", "tau.smlab.syntech.ui.error");

  private final String message;

  private final String id;
  
  private MarkerKind(String message, String id) {
    this.message = message;
    this.id = id;
  }

  public String getMessage() {
    return message;
  }

  // the one found in the plug-in definition
  public String getMarkerID() {
    return id;
  }

}