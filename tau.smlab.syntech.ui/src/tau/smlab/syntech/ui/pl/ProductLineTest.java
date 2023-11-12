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

package tau.smlab.syntech.ui.pl;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import net.sf.javabdd.BDDFactory;
import tau.smlab.syntech.bddgenerator.BDDGenerator;
import tau.smlab.syntech.bddgenerator.BDDGenerator.TraceInfo;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinputtrans.AuxVariableGenerator;
import tau.smlab.syntech.gameinputtrans.TranslationProvider;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule.TransFuncType;
import tau.smlab.syntech.games.gr1.GR1Game;
import tau.smlab.syntech.games.gr1.GR1GameExperiments;
import tau.smlab.syntech.games.gr1.GR1GameImplC;
import tau.smlab.syntech.jtlv.BDDPackage;
import tau.smlab.syntech.jtlv.BDDPackage.BBDPackageVersion;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.spectragameinput.SpectraInputProviderNoIDE;

class ProductLineTest {
	
	protected static SpectraInputProviderNoIDE ip;
	
	@BeforeEach
	protected void setup() {
		ip = new SpectraInputProviderNoIDE(true);
		BDDPackage.setCurrPackage(BDDPackage.CUDD, BBDPackageVersion.CUDD_3_0);
		Env.enableReorder();
		Env.TRUE().getFactory().autoReorder(BDDFactory.REORDER_SIFT);
		
		GR1GameExperiments.DETECT_FIX_POINT_EARLY = true;
		GR1GameExperiments.USE_FIXPOINT_RECYCLE = true;
		GR1GameExperiments.STOP_WHEN_INITIALS_LOST = true;
		GR1GameExperiments.SIMULTANEOUS_CONJUNCTION_ABSTRACTION = true;
	}
	
	@AfterEach
	public void tearDown() {
		Env.resetEnv();
	}
	
	@Test
	void realizabilityPL() {
		long total = 0;

		for (int i = 0; i < 10; i++) {
			long start = System.currentTimeMillis();
			variabilityAwareRealizability("pl/WashPL.spectra");
			long run = System.currentTimeMillis() - start;
			total += run;
		}
		
		System.out.println((double) total / 10000);
	}

	@Test
	void realizabilitySeparateSpecs() {
		
		long total = 0;

		for (int i = 0; i < 10; i++) {
			long start = System.currentTimeMillis();
			realizability("pl/wash.spectra");
			realizability("pl/wash_heat.spectra");
			realizability("pl/wash_dry.spectra");
			realizability("pl/wash_delay.spectra");
			realizability("pl/wash_dry_delay.spectra");
			realizability("pl/wash_dry_heat.spectra");
			long run = System.currentTimeMillis() - start;
			total += run;
		}
		
		System.out.println((double) total / 10000);
	}
	
	private boolean checkRealizability(GameInput gi) {
		GameModel gm = BDDGenerator.generateGameModel(gi, TraceInfo.NONE, false, TransFuncType.DECOMPOSED_FUNC);
		GR1Game gr1Game = new GR1GameImplC(gm);
		return gr1Game.checkRealizability();
	}

	private void realizability(String spec) {
		try {
			GameInput gi = ip.getGameInput(spec);
			TranslationProvider.translate(gi);
			checkRealizability(gi);
			AuxVariableGenerator.resetVar();
			Env.resetEnv();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	private void variabilityAwareRealizability(String spec) {
		try {
			GameInput gi = ip.getGameInput(spec);
			TranslationProvider.translate(gi);
			
			VariabilityAwareRealizability var = new VariabilityAwareRealizability(
					true, (input) -> checkRealizability(input), (str) -> System.out.println(str), gi);
			var.doWork();
			
			AuxVariableGenerator.resetVar();
			Env.resetEnv();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
