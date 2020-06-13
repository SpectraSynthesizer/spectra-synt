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

package tau.smlab.syntech.gameinputtrans;

import java.util.List;

import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinputtrans.translator.DefaultTranslators;
import tau.smlab.syntech.gameinputtrans.translator.Translator;

/**
 * provides execution of inplace translations
 *
 */
public class TranslationProvider {

	/**
	 * translate the game input according to default translators
	 * 
	 * @param input
	 * @return
	 */
	public static void translate(GameInput input) {
		translate(input, DefaultTranslators.getDefaultTranslators());
	}

	/**
	 * translate game input based on supplied list of translators
	 * 
	 * @param input
	 * @param translators
	 * @return
	 */
	public static void translate(GameInput input, List<Translator> translators) {
		if (!isSomePlayerHasVariables(input)) {
			throw new TranslationException("None of the players has variables", -1);
		}
		for (Translator t : translators) {
			t.translate(input);
		}
	}

	private static boolean isSomePlayerHasVariables(GameInput input) {
		if (input.getAux().getVars() != null && input.getAux().getVars().size() > 0) {
			return true;
		}
		if (input.getSys().getVars() != null && input.getSys().getVars().size() > 0) {
			return true;
		}
		if (input.getEnv().getVars() != null && input.getEnv().getVars().size() > 0) {
			return true;
		}

		return false;
	}
}
