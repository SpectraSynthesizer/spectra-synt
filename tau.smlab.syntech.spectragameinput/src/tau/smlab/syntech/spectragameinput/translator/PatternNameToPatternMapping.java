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

import tau.smlab.syntech.gameinput.model.Pattern;
import tau.smlab.syntech.spectragameinput.SpectraTranslationException;

public class PatternNameToPatternMapping {
  private Map<String, Pattern> patternNameToPatternObjectMapping;

  public PatternNameToPatternMapping() {
    patternNameToPatternObjectMapping = new HashMap<>();
  }

  public List<Pattern> getAllPatterns() {
    return new ArrayList<>(patternNameToPatternObjectMapping.values());
  }
  
  /**
   * Computes GameInput Pattern on demand: If computed before, returns immediately. Otherwise computes the pattern, stores it and returns.
   * @param spectraPattern Spectra Pattern
   * @param entitiesMapper
   * @param tracer
   * @return GameInput pattern
   * @throws SpectraTranslationException 
   */
  public Pattern get(tau.smlab.syntech.spectra.Pattern spectraPattern, EntitiesMapper entitiesMapper, Tracer tracer) throws SpectraTranslationException {
    String patternName = spectraPattern.getName();
    if (patternNameToPatternObjectMapping.containsKey(patternName))
    {
      return patternNameToPatternObjectMapping.get(patternName);
    }
    else
    {
      // Compute the pattern
      tau.smlab.syntech.gameinput.model.Pattern giPattern = Spectra2GameInputTranslator.computePattern(entitiesMapper, tracer, spectraPattern);
      // Store it for future look ups
      patternNameToPatternObjectMapping.put(patternName, giPattern);
      return giPattern;
    }
  }
}
