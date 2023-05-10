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

package tau.smlab.syntech.games.controller.jits;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;

public class JitContext {
	
	private BDD envIni;
	private BDD envTrans;
	private BDD sysIni;
	private BDD sysTrans;
	
	private BDD[][] mY;
	private BDD[][][] mX;
	
	private BDD[][] mYunprimed;
	private BDD[][][] mXunprimed;
	
	// The transition BDD to be used during the execution
	private BDD trans;
	private BDD ini;

	public JitContext(BDD envIni, BDD envTrans, BDD sysIni, BDD sysTrans) {
		super();
		this.envIni = envIni;
		this.envTrans = envTrans;
		this.sysIni = sysIni;
		this.sysTrans = sysTrans;
	}
	
	public BDD getEnvIni() {
		return envIni;
	}
	public BDD getEnvTrans() {
		return envTrans;
	}
	public BDD getSysIni() {
		return sysIni;
	}
	public BDD getSysTrans() {
		return sysTrans;
	}

	public BDD getTrans() {
		return trans;
	}

	public void setTrans(BDD trans) {
		this.trans = trans;
	}
	
	public BDD getIni() {
		return ini;
	}

	public void setIni(BDD ini) {
		this.ini = ini;
	}
	
	public void free() {
		Env.free(mY);
		Env.free(mX);
		Env.free(mYunprimed);
		Env.free(mXunprimed);
		this.envIni.free();
		this.envTrans.free();
		this.sysIni.free();
		this.sysTrans.free();
		if (this.ini != null) {
			this.ini.free();	
		}
		if (this.trans != null) {
			this.trans.free();	
		}
	}
	
	public int rank(int j) {
		return mY[j].length;
	}
	
	public BDD Y(int j, int r) {
		if (r == -1) {
			return Env.FALSE();
		}
		return mY[j][r];
	}
	
	public BDD Yunprimed(int j, int r) {
		if (r == -1) {
			return Env.FALSE();
		}
		return mYunprimed[j][r];
	}
	
	public BDD X(int j, int i, int r) {
		return mX[j][i][r];
	}
	
	public BDD Xunprimed(int j, int i, int r) {
		return mXunprimed[j][i][r];
	}
	
	public void setMX(BDD[][][] mX) {
		this.mX = mX;
	}
	
	public void setMY(BDD[][] mY) {
		this.mY = mY;
	}
	
	public void setMXunprimed(BDD[][][] mX) {
		this.mXunprimed = mX;
	}
	
	public void setMYunprimed(BDD[][] mY) {
		this.mYunprimed = mY;
	}
}
