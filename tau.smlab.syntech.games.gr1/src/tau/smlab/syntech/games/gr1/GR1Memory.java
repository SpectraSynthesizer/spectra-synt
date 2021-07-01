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

import net.sf.javabdd.BDD;
import tau.smlab.syntech.games.GameMemory;
import tau.smlab.syntech.jtlv.Env;

public class GR1Memory extends GameMemory {
	
	protected BDD[][][] x_mem;
	protected BDD[][] y_mem;
	protected BDD[] z_mem;
	
	//Memory vectors used for GR(1)*
	
	//Memory to fulfill the k-th existential guarantee 
	protected BDD[][] fulfill_exist_gar_mem; 
	
	//Memory to reach a state that starts the fulfillment of the k-th existential guarantee 
	protected BDD[][] towards_exist_gar_mem; 
	
	//Memory of co-Buchi game of justice assumption violation (in case of non well-separation)
	protected BDD[][] envJusticeViolation_mem;

	@Override
	public void free() {
		super.free();
		Env.free(z_mem);
		z_mem = null;
		Env.free(y_mem);
		y_mem = null;
		Env.free(x_mem);
		x_mem = null;
		Env.free(fulfill_exist_gar_mem);
		fulfill_exist_gar_mem = null;
		Env.free(towards_exist_gar_mem);
		towards_exist_gar_mem = null;
		Env.free(envJusticeViolation_mem);
		envJusticeViolation_mem = null;
	}
	
	public BDD getFulfill(int exj, int f) {
		return fulfill_exist_gar_mem[exj][f];
	}
	
	public BDD getTowards(int exj, int t) {
		return towards_exist_gar_mem[exj][t];
	}
	
	public BDD getEnvViolation(int r, int i) {
		return envJusticeViolation_mem[i][r];
	}
	
	public BDD getX(int j, int r, int i) {
		return x_mem[j][i][r];
	}
	
	public int getRank(int j) {
		return y_mem[j].length;
	}
	
	public int getFulfillRank(int exj) {
		return fulfill_exist_gar_mem[exj].length;
	}
	
	public int getTowardsRank(int exj) {
		return towards_exist_gar_mem[exj].length;
	}
	
	public int getEnvViolationRank() {
		return envJusticeViolation_mem[0].length;
	}

	// extended_size<=0 will tight the arrays to be the exact sizes.
	public BDD[][][] extend_size(BDD[][][] in, int extended_size) {
		BDD[][][] res;
		if (extended_size > 0) {
			res = new BDD[in.length][in[0].length][in[0][0].length + extended_size];
			for (int i = 0; i < in.length; i++) {
				for (int j = 0; j < in[i].length; j++) {
					for (int k = 0; k < in[i][j].length; k++) {
						res[i][j][k] = in[i][j][k];
					}
				}
			}
		} else {
			res = new BDD[in.length][in[0].length][];
			for (int i = 0; i < in.length; i++) {
				for (int j = 0; j < in[i].length; j++) {
					int real_size = 0;
					for (int k = 0; k < in[i][j].length; k++) {
						if (in[i][j][k] != null)
							real_size++;
					}
					res[i][j] = new BDD[real_size];
					int new_add = 0;
					for (int k = 0; k < in[i][j].length; k++) {
						if (in[i][j][k] != null) {
							res[i][j][new_add] = in[i][j][k];
							new_add++;
						}
					}
				}
			}
		}
		return res;
	}

	// extended_size<=0 will tight the arrays to be the exact sizes.
	public BDD[][] extend_size(BDD[][] in, int extended_size) {
		BDD[][] res;
		if (extended_size > 0) {
			res = new BDD[in.length][in[0].length + extended_size];
			for (int i = 0; i < in.length; i++) {
				for (int j = 0; j < in[i].length; j++) {
					res[i][j] = in[i][j];
				}
			}
		} else {
			res = new BDD[in.length][];
			for (int i = 0; i < in.length; i++) {
				int real_size = 0;
				for (int j = 0; j < in[i].length; j++) {
					if (in[i][j] != null)
						real_size++;
				}
				res[i] = new BDD[real_size];
				int new_add = 0;
				for (int j = 0; j < in[i].length; j++) {
					if (in[i][j] != null) {
						res[i][new_add] = in[i][j];
						new_add++;
					}
				}
			}
		}
		return res;
	}

}
