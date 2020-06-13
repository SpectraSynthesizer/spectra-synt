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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.emf.common.util.EList;

import tau.smlab.syntech.gameinput.model.Counter;
import tau.smlab.syntech.gameinput.model.Counter.OverFlowMethod;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecTraceable;
import tau.smlab.syntech.spectra.Subrange;
import tau.smlab.syntech.spectra.TemporalExpression;
import tau.smlab.syntech.spectragameinput.SpectraTranslationException;
import tau.smlab.syntech.typesystem.TypeSystemUtils;

public class CounterNameToCounterMapping {
  private Map<String, Counter> map;

  public CounterNameToCounterMapping() {
    map = new HashMap<>();
  }

  public List<Counter> getAllCounters() {
    return new ArrayList<>(map.values());
  }

  public Counter get(tau.smlab.syntech.spectra.Counter sCounter, EntitiesMapper entitiesMapper,
      Tracer tracer) throws SpectraTranslationException {
    String name = sCounter.getName();
    if (!map.containsKey(name)) {
      Counter counter = new Counter(tracer.addTrace(sCounter), sCounter.getName());
      map.put(name, counter);

      counter.setIniPred(getSpecTraceable(tracer, entitiesMapper, sCounter.getInitial()));
      counter.setIncPred(getSpecTraceable(tracer, entitiesMapper, sCounter.getIncPred()));
      counter.setResetPred(getSpecTraceable(tracer, entitiesMapper, sCounter.getResetPred()));
      counter.setDecPred(getSpecTraceable(tracer, entitiesMapper, sCounter.getDecPred()));

      Subrange range = sCounter.getRange();
      counter.setLower(TypeSystemUtils.sizeDefineToInt(range.getFrom()));
      counter.setUpper(TypeSystemUtils.sizeDefineToInt(range.getTo()));

      counter.setOverFlowMethod(getOverflowMethod(sCounter));
      counter.setUnderFlowMethod(getUnderflowMethod(sCounter));
    }
    return map.get(name);
  }

  private OverFlowMethod getOverflowMethod(tau.smlab.syntech.spectra.Counter sCounter) {
    if (sCounter.getOverflowMethod() == null || sCounter.getOverflowMethod().size() != 1) {
      return OverFlowMethod.FALSE;
    }

    switch (sCounter.getOverflowMethod().get(0)) {
    case FALSE:
      return OverFlowMethod.FALSE;
    case KEEP:
      return OverFlowMethod.KEEP;
    case MODULO:
      return OverFlowMethod.MODULO;
    default:
      break;
    }
    return null;
  }

  private OverFlowMethod getUnderflowMethod(tau.smlab.syntech.spectra.Counter sCounter) {
    if (sCounter.getUnderflowMethod() == null || sCounter.getUnderflowMethod().size() != 1) {
      return OverFlowMethod.FALSE;
    }

    switch (sCounter.getUnderflowMethod().get(0)) {
    case FALSE:
      return OverFlowMethod.FALSE;
    case KEEP:
      return OverFlowMethod.KEEP;
    case MODULO:
      return OverFlowMethod.MODULO;
    default:
      break;
    }
    return null;
  }

  /**
   * computes a SpecTraceable of the first element <code>eList[0]</code>
   * 
   * @param tracer
   * @param entitiesMapper
   * @param eList
   * @return
   * @throws SpectraTranslationException 
   */
  private SpecTraceable getSpecTraceable(Tracer tracer, EntitiesMapper entitiesMapper,
      EList<TemporalExpression> eList) throws SpectraTranslationException {
    if (eList == null || eList.size() != 1) {
      return null;
    }
    TemporalExpression exp = eList.get(0);
    Spec content;
    try {
      content = SpectraASTToSpecGenerator.getConstraintSpec(exp, entitiesMapper, tracer, null, null, null).getSpec();
    } catch (SpectraTranslationException e) {
      e.setTraceId(tracer.addTrace(exp));
      throw e;
    }

    return new SpecTraceable(content, tracer.addTrace(exp));
  }

}
