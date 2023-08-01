package tau.smlab.syntech.ui.pl;

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
	void realizabilityPLLattice() {
		long total = 0;

		for (int i = 0; i < 10; i++) {
			long start = System.currentTimeMillis();
			variabilityAwareRealizabilityLattice("pl/WashPL.spectra");
			long run = System.currentTimeMillis() - start;
			total += run;
		}
		
		System.out.println((double) total / 1000);
	}
	
	@Test
	void realizabilityPLEmbedded() {
		long total = 0;

		for (int i = 0; i < 10; i++) {
			long start = System.currentTimeMillis();
			variabilityAwareRealizabilityEmbedded("pl/WashPL.spectra");
			long run = System.currentTimeMillis() - start;
			total += run;
		}
		
		System.out.println((double) total / 1000);
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
		
		System.out.println((double) total / 1000);
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
	
	private void variabilityAwareRealizabilityLattice(String spec) {
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
	
	private void variabilityAwareRealizabilityEmbedded(String spec) {
		try {
			GameInput gi = ip.getGameInput(spec);
			TranslationProvider.translate(gi, true);
			checkRealizability(gi);
			AuxVariableGenerator.resetVar();
			Env.resetEnv();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
