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

import java.util.Vector;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.games.GameIncrementalMemory;
import tau.smlab.syntech.jtlv.Env;

public class GR1GameIncrementalMemory extends GameIncrementalMemory {

	public BDD[] prevZMem;
	public BDD[][][] prevXMem;
	protected BDD[] prevFirstZIterMem;

	public GR1GameIncrementalMemory() {
		startZ = Env.TRUE();
	}

	public void addToStartZ(BDD z, int idx) {
		// System.out.println(
		// "Add to startZ: startZ.isOne = " + startZ.isOne() + ", segMem.zMem[" + idx +
		// "].isOne = " + z.isOne());
		BDD tmp = startZ;
		startZ = tmp.and(z);
		tmp.free();
	}

	public void setPrevZMemory(BDD[] prevMem) {
		this.prevZMem = prevMem;
	}

	public void setPrevXMem(BDD[][][] xMem) {
		this.prevXMem = xMem;
	}

	public void setPrevFirstZIterMem(BDD[] prevFirstZIterMem) {
		this.prevFirstZIterMem = prevFirstZIterMem;
	}

	public void setPrevMem(Vector<BDD> zMem, Vector<Vector<Vector<BDD>>> xMem) {
	}

	public BDD[] getZMemoryCopy(BDD[] z_mem, int size1) {
		BDD[] z_mem_copy = new BDD[size1];
		for (int j = 0; j < z_mem.length; j++) {
			z_mem_copy[j] = z_mem[j].id();
		}

		return z_mem_copy;
	}

	public BDD[][][] getXMemoryCopy(BDD[][][] x_mem, int x_currSize, int size1, int size2) {
		BDD[][][] x_mem_copy = new BDD[size1][size2][];

		for (int j = 0; j < x_mem.length; j++) {
			for (int i = 0; i < x_mem[0].length; i++) {
				int cySize = x_mem[j][i].length;
				if (x_currSize > cySize)
					cySize = x_currSize;
				x_mem_copy[j][i] = new BDD[cySize];
				for (int k = 0; k < x_mem[j][i].length; k++) {
					x_mem_copy[j][i][k] = x_mem[j][i][k].id();
				}
			}
		}

		return x_mem_copy;
	}

}
