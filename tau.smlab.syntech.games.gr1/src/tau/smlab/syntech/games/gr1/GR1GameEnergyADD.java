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

package tau.smlab.syntech.games.gr1;

import net.sf.javabdd.ADD;
import net.sf.javabdd.BDD;
import tau.smlab.syntech.bddgenerator.BDDGenerator;
import tau.smlab.syntech.gameinput.model.TypeDef;
import tau.smlab.syntech.gameinput.model.Variable;
import tau.smlab.syntech.gameinput.spec.Operator;
import tau.smlab.syntech.gameinput.spec.PrimitiveValue;
import tau.smlab.syntech.gameinput.spec.Spec;
import tau.smlab.syntech.gameinput.spec.SpecException;
import tau.smlab.syntech.gameinput.spec.SpecExp;
import tau.smlab.syntech.gameinput.spec.VariableReference;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.jtlv.lib.FixPoint;
import tau.smlab.syntech.games.gr1.BoundedEnergyLattice;

public class GR1GameEnergyADD extends GR1Game {

	public static boolean WINNING_STATES_WITHOUT_CREDITS = false;
	public static boolean WINNING_STATES_WITH_FLAT_CREDITS = false;

	protected BoundedEnergyLattice lattice;
	protected ADD sysMinWinCredits = null;
	protected double minWinInitCred;

	public GR1GameEnergyADD(GameModel model, int upperBound) {
		super(model);
		try {
			this.lattice = new BoundedEnergyLattice(model, upperBound);
		} catch (SpecException e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
		}
	}

	public void setSysMinWinCredits(ADD winEnergy) {
		this.sysMinWinCredits = winEnergy;
	}

	/**
	 * Returns the fixed-point Energy function that maps each state to the minimum
	 * initial credit, which wins for the system. It is the Energy-mu-calculus
	 * semantics of the formula that defines the set of winning states for the
	 * system.
	 * 
	 * @return
	 */
	public ADD getSysMinWinCredits() {
		return this.sysMinWinCredits;
	}

	public void setMinWinInitCred(double credit) {
		this.minWinInitCred = credit;
	}

	/**
	 * Returns the minimum initial credit for which the game wins for the system.
	 * 
	 * @return
	 */
	public double getMinWinInitCred() {
		return this.minWinInitCred;
	}

	public BDD getSysWinWithFlatCredits() {
		BDD flattenedWinCred = null;
		if (this.sysMinWinCredits != null) {
			if (WINNING_STATES_WITH_FLAT_CREDITS) {
				flattenedWinCred = this.mem.getWin();
			} else {
				flattenedWinCred = lattice.flattenEnergyValuesToEngVariable(this.sysMinWinCredits);
			}
		}
		return flattenedWinCred;
	}

	public void setSysWinningStatesWithFlatCredits() {
		if (!WINNING_STATES_WITH_FLAT_CREDITS) {
			BDD flatWinCredits = getSysWinWithFlatCredits();
			if (flatWinCredits != null) {
				this.mem.setWin(flatWinCredits);
			}
		}
	}

	@Override
	public boolean checkRealizability() {
		mem.x_mem = new ADD[sys.justiceNum()][env.justiceNum()][50];
		mem.y_mem = new ADD[sys.justiceNum()][50];
		mem.z_mem = new ADD[sys.justiceNum()];

		ADD x = null, y, z;
		FixPoint iterZ, iterY, iterX;
		int cy = 0;

		z = lattice.getTopEngFunc();

		for (iterZ = new FixPoint(false); iterZ.advance(z);) {

			for (int j = 0; j < sys.justiceNum(); j++) {
				cy = 0;
				y = lattice.getBottomEngFunc();
				for (iterY = new FixPoint(false); iterY.advance(y);) {
					ADD sysJustice = lattice.toEnergySemantics((ADD) sys.justiceAt(j));
					ADD cPreZ = lattice.sysECpre(z);
					ADD cPreY = lattice.sysECpre(y);
					ADD start = lattice.meetWith(sysJustice, cPreZ);
					lattice.joinWith(start, cPreY);

					y = lattice.getBottomEngFunc();
					for (int i = 0; i < env.justiceNum(); i++) {
						ADD negp = lattice.toEnergyNegatedSemantics((ADD) env.justiceAt(i));
						if (x != null)
							x.free();
						x = z.id();
						for (iterX = new FixPoint(false); iterX.advance(x);) {

							ADD sysCtrl = lattice.sysECpre(x);
							ADD sysCtrlAndNotJustice = lattice.meet(sysCtrl, negp);
							sysCtrl.free();
							x = lattice.join(sysCtrlAndNotJustice, start);
							sysCtrlAndNotJustice.free();
						}
						mem.x_mem[j][i][cy] = x.id();
						ADD oy = y;
						y = lattice.join(y, x);
						oy.free();
					}
					start.free();
					mem.y_mem[j][cy] = y.id();
					cy++;
					if (cy % 50 == 0) {
						mem.x_mem = mem.extend_size(mem.x_mem, cy);
						mem.y_mem = mem.extend_size(mem.y_mem, cy);
					}
				}
				z = y.id();
				mem.z_mem[j] = z.id();
			}
		}
		mem.x_mem = mem.extend_size(mem.x_mem, 0);
		mem.y_mem = mem.extend_size(mem.y_mem, 0);

		if (WINNING_STATES_WITHOUT_CREDITS) {
			mem.setWin(lattice.deriveSysWinningStates(z));
		} else if (WINNING_STATES_WITH_FLAT_CREDITS) {
			// this.flattenedWinCred = lattice.flattenEnergyValuesToEngVariable(z);
			mem.setWin(lattice.flattenEnergyValuesToEngVariable(z));
		}
		mem.setComplete(true);
		this.setSysMinWinCredits(z);
		return existsWinningInitCred(z);
	}

	/**
	 * Computes the minimum initial credit c_{0} for which the game wins for the
	 * system player.
	 * 
	 * @param winSysEng
	 * @return True if c_{0} is not equal to +INF.
	 */
	public boolean existsWinningInitCred(ADD winSysEng) {
		ADD negEngEnvIni = lattice.toEnergyNegatedSemantics((ADD) env.initial());
		ADD sysEngIni = lattice.toEnergySemantics((ADD) sys.initial());
		ADD sysWinEng = lattice.meet(sysEngIni, winSysEng);

		ADD sysWinEngAbsMin = sysWinEng.abstractMin(sys.moduleUnprimeVars());
		lattice.joinWith(sysWinEngAbsMin, negEngEnvIni);
		ADD result = sysWinEngAbsMin.abstractMax(env.moduleUnprimeVars());

		this.setMinWinInitCred(result.getConstantValue());

		sysWinEng.free();
		sysEngIni.free();
		sysWinEngAbsMin.free();
		result.free();

		return lattice.notBottomValue(this.getMinWinInitCred());
	}

	public void flattenGameMemoryTerminalsToVariables() {
		if (this.mem.x_mem == null || this.mem.y_mem == null || this.mem.z_mem == null) {
			return;
		}
		GR1Memory flatMem = new GR1Memory();
		flatMem.x_mem = new ADD[this.mem.x_mem.length][this.mem.x_mem[0].length][];
		flatMem.y_mem = new ADD[this.mem.y_mem.length][];
		flatMem.z_mem = new ADD[this.mem.z_mem.length];

		// flatten the x
		for (int j = 0; j < this.mem.x_mem.length; j++) {
			for (int i = 0; i < this.mem.x_mem[j].length; i++) {
				flatMem.x_mem[j][i] = new ADD[this.mem.x_mem[j][i].length];
				for (int k = 0; k < this.mem.x_mem[j][i].length; k++) {
					flatMem.x_mem[j][i][k] = lattice.flattenEnergyValuesToEngVariable((ADD) this.mem.x_mem[j][i][k]);
				}
			}
		}
		// flatten the y
		for (int j = 0; j < this.mem.y_mem.length; j++) {
			flatMem.y_mem[j] = new ADD[this.mem.y_mem[j].length];
			for (int i = 0; i < this.mem.y_mem[j].length; i++) {
				flatMem.y_mem[j][i] = lattice.flattenEnergyValuesToEngVariable((ADD) this.mem.y_mem[j][i]);
			}
		}

		// flatten the z
		for (int j = 0; j < this.mem.z_mem.length; j++) {
			flatMem.z_mem[j] = lattice.flattenEnergyValuesToEngVariable((ADD) this.mem.z_mem[j]);
		}

		this.mem.free();
		this.mem = flatMem;
	}

	public void updateSysTransWithEnergyConstraints() {
		int bound = this.lattice.getUpperBound();
		Variable energyVal = new Variable("energyVal", new TypeDef(0, bound));
		for (Integer w : model.getWeights().keySet()) {
			if (w > bound) {
				// optimization for weights above bound: all values of y' in [0, c] allowed
				continue;
			} else if (w < -bound) {
				// optimization for weights below negative bound: transition is losing
				sys.conjunctTrans(model.getWeights().get(w).not());
				// add trace info for this weight entry
			} else {
				// valPMinusW := energyVal' - w
				Spec valPMinusW = new SpecExp(Operator.SUBSTRACT, new VariableReference(energyVal, "energyVal'"),
						new PrimitiveValue(w));
				// spec := energyVal' - w <= energyVal
				Spec spec = new SpecExp(Operator.RIGHT_BIGGER_OR_EQUALS, valPMinusW, new VariableReference(energyVal));
				BDD energyValUpdate = BDDGenerator.createBdd(spec, 0);

				BDD transForWeight = model.getWeights().get(w).id().impWith(energyValUpdate);
				// add value update to transitions
				sys.conjunctTrans(transForWeight.id());
				// add trace info for this weight entry
			}
		}
	}
}