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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.GameModel;

/**
 * An implementation of the MeanPayoffGame to EnergyGame reduction from
 * Proposition 12 of:
 * 
 * Patricia Bouyer, Ulrich Fahrenberg, Kim Guldstrand Larsen, Nicolas Markey,
 * Jirí Srba: Infinite Runs in Weighted Timed Automata with Energy Constraints.
 * FORMATS 2008: 33-47
 * 
 * @author ringert
 * @author pistiner
 *
 */
public class MeanPayoffToEnergyReduction {

	/**
	 * reduce the game model (specifically its weight map) to that of an energy game
	 * where solving the EG gives a solution to the mean-payoff game with at least
	 * value mean payoff (approximates the fraction after the comma)
	 * 
	 * @param m
	 * @param value
	 * @return the bound as an integer (scaled according to scale)
	 */
	public static int reduce(GameModel m, double value, double bound) {
		Map<Integer, BDD> w = m.getWeights();
		Map<Integer, BDD> reducedW = new HashMap<>();

		int[] scaleNum = computeScaleAndNumExact(value);
		int scale = scaleNum[0];
		int num = scaleNum[1];

		// first scale the fraction away
		// then remove the MP value from all weights to get MP value 0 to obtain an EG
		for (Integer oldWeight : w.keySet()) {
			reducedW.put(oldWeight * scale - num, w.get(oldWeight));
		}

		w.clear();
		w.putAll(reducedW);

		return (int) Math.ceil(bound * scale);
	}

	/**
	 * Given a value determines integers {scale, num} s.t. num / scale is close to
	 * value
	 * 
	 * As an example, 1 gives {1, 1} and 3.1 gives {10, 31}
	 *
	 * 
	 * @param value
	 *            - A decimal either positive or negative
	 * @return {scale, num} s.t. num / scale is close to value
	 */
	public static int[] computeScaleAndNumExact(double value) {

		if (value == 0) {
			return new int[] { 1, 0 };
		}

		BigDecimal bValue = new BigDecimal(String.valueOf(value));

		BigInteger numerator = bValue.unscaledValue(); // == value * 10^{number of digits after decimal dot}
		BigInteger denominator = BigInteger.valueOf(10L).pow(bValue.scale()); // == 10^{number of digits after decimal
																				// dot}

		int gcd = numerator.gcd(denominator).intValue(); // compute GCD

		int num = numerator.intValue() / gcd;
		int scale = denominator.intValue() / gcd;

		return new int[] { scale, num };
	}

}
