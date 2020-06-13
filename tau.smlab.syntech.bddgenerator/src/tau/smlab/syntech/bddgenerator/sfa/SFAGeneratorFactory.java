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

package tau.smlab.syntech.bddgenerator.sfa;

import tau.smlab.syntech.bddgenerator.sfa.regexp.BricsRegExpSFAGenerator;
import tau.smlab.syntech.bddgenerator.sfa.regexp.RegExpSFAGenerator;
import tau.smlab.syntech.bddgenerator.sfa.regexp.SymRegExpSFAGenerator;
import tau.smlab.syntech.bddgenerator.sfa.trigger.SimpleTriggerSFAGenerator;
import tau.smlab.syntech.bddgenerator.sfa.trigger.TriggerSFAGenerator;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.gameinput.spec.SpecRegExp;

public class SFAGeneratorFactory {

	public enum RegExpSFAGeneratorType {
		SYMBOLIC, BRICS;
	}
	
	public enum TriggerSFAGeneratorType {
		SIMPLE;
	}

	public static RegExpSFAGenerator getGenerator(RegExpSFAGeneratorType generatorType, SpecRegExp regExp, int traceId) {
		RegExpSFAGenerator generator;
		switch(generatorType) {
		case SYMBOLIC:
			generator = new SymRegExpSFAGenerator(regExp, traceId);
			break;
		case BRICS:
			generator = new BricsRegExpSFAGenerator(regExp, traceId);
			break;
		default:
			throw new RegExpSFAGenerator.RegExpSFAGeneratorException(generatorType + " is an invalid type of RegExpSFAGenerator", traceId);
		}
		return generator;
	}
	
	
	public static TriggerSFAGenerator getGenerator(TriggerSFAGeneratorType generatorType, TriggerConstraint trigger, int traceId) {
		TriggerSFAGenerator generator;
		switch(generatorType) {
		case SIMPLE:
			generator = new SimpleTriggerSFAGenerator(trigger, traceId);
			break;
		default:
			throw new TriggerSFAGenerator.TriggerSFAGeneratorException(generatorType + " is an invalid type of TriggerSFAGenerator", traceId);
		}
		return generator;
	}
	
	public static TriggerSFAGenerator getGenerator(TriggerSFAGeneratorType generatorType, RegExpSFAGeneratorType regExpGeneratorType, TriggerConstraint trigger, int traceId) {
		TriggerSFAGenerator generator;
		switch(generatorType) {
		case SIMPLE:
			generator = new SimpleTriggerSFAGenerator(trigger, traceId, regExpGeneratorType);
			break;
		default:
			throw new TriggerSFAGenerator.TriggerSFAGeneratorException(generatorType + " is an invalid type of TriggerSFAGenerator", traceId);
		}
		return generator;
	}
}
