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

package tau.smlab.syntech.games.controller.enumerate;

import java.util.Vector;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.games.AbstractGamesException;
import tau.smlab.syntech.games.GamesStrategyException;

public class EnumStateImpl implements EnumStateI {
	private EnumStrategyI holder;
	private int id;
	private boolean ini;
	private AbstractRankInfo just;
	private BDD state;
	private Vector<EnumStateI> succ;
	
	private int numInEdges;
	private int numOutEdges;
	private int distFromIni;

	public EnumStateImpl(EnumStrategyI holder, int id, boolean ini, BDD state,
			AbstractRankInfo just) {
		this.holder = holder;
		this.id = id;
		this.ini = ini;
		this.state = state;
		this.just = just;
		this.succ = new Vector<EnumStateI>(10);
		this.numInEdges = 0;
		this.numOutEdges = 0;
		this.distFromIni = 0;
	}

	@Override
	public void addSuccessor(StateI add) throws AbstractGamesException {
		if (!(add instanceof EnumStateI))
			throw new GamesStrategyException("Cannot add successor from"
					+ " different type");
		succ.add((EnumStateI) add);
	}

	@Override
	public EnumStrategyI getHolder() {
		return this.holder;
	}

	@Override
	public int getStateId() {
		return this.id;
	}

	@Override
	public AbstractRankInfo get_rank_info() {
		return this.just;
	}

	@Override
	public Vector<EnumStateI> getSuccessors() {
	  return this.succ;
	}

	@Override
	public boolean equals(Object other) {
		if (!(other instanceof EnumStateImpl))
			return false;
		EnumStateImpl other_raw = (EnumStateImpl) other;

		return ((this.just == other_raw.just) & (this.ini == other_raw.ini) & (this.state
				.equals(other_raw.state)));
	}

	@Override
	public boolean isInitial() {
		return this.ini;
	}

	@Override
	public BDD getData() {
		return this.state;
	}
	
	@Override
	public int getNumOutEdges() {
		return this.numOutEdges;
	}
	
	@Override
	public int getNumInEdges() {
		return this.numInEdges;
	}

	@Override
    public int getDistFromIni() {
		return this.distFromIni;
    }
	
	@Override
	public void incNumOutEdges() {
		this.numOutEdges++;
	}

	@Override
	public void incNumInEdges() {
		this.numInEdges++;
	}
	
	@Override
	public boolean updateDistFromIni(int neighborDist) {
		if (isInitial()) {
			return false;
		}
		if (this.distFromIni == 0) {
			this.distFromIni = neighborDist + 1;
			return true;
		}
		if (this.distFromIni > neighborDist + 1) {
			this.distFromIni = neighborDist + 1;
			return true;
		}
		
		return false;
	}
}
