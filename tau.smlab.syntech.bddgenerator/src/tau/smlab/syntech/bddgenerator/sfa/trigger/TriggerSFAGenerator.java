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

package tau.smlab.syntech.bddgenerator.sfa.trigger;

import tau.smlab.syntech.bddgenerator.BDDTranslationException;
import tau.smlab.syntech.gameinput.model.TriggerConstraint;
import tau.smlab.syntech.sfa.SFA;


/**
 * 
 * Use a {@link TriggerSFAGenerator} to translate triggers (instances of {@link TriggerConstraint})
 * to equivalent {@link SFA}s.
 * 
 * @author Or Pistiner
 *
 */
public interface TriggerSFAGenerator {
	
	/**
	 *
	 * An exception thrown during translations of triggers (instances of {@link TriggerConstraint}) to {@link SFA}s.
	 *
	 */
	public static class TriggerSFAGeneratorException extends BDDTranslationException {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1780939868279839674L;

		public TriggerSFAGeneratorException() {
			super();
		}
		
		public TriggerSFAGeneratorException(String string, int traceId) {
			super(string, traceId);
		}

	}
	
	/**
	 * 
	 * Returns a deterministic and complete {@link SFA} whose language is that of the trigger associated with this generator.
	 * 
	 * @return
	 */
	public SFA generateTriggerSfa();
}
