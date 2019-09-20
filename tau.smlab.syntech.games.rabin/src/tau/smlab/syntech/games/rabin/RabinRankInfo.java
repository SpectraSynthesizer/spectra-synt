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

package tau.smlab.syntech.games.rabin;

import tau.smlab.syntech.games.controller.enumerate.AbstractRankInfo;

public class RabinRankInfo extends AbstractRankInfo {

	private int count_rows;
	private int env_goal;
	private int count_env_goal;

	public RabinRankInfo(int count_rows, int env_goal, int count_env_goal) {
		this.count_rows = count_rows;
		this.env_goal = env_goal;
		this.count_env_goal = count_env_goal;
	}

	@Override
	public String rankInfoString() {
		return "Towards environment justice " + env_goal + " of " + count_env_goal + " avoiding system justice "
				+ count_rows + " mod numSysJus";
	}

	@Override
	public boolean equals(Object other) {
		if (!(other instanceof RabinRankInfo))
			return false;
		RabinRankInfo rri = (RabinRankInfo) other;
		return this.env_goal == rri.env_goal && this.count_rows == rri.count_rows
				&& this.count_env_goal == rri.count_env_goal;
	}

	@Override
	public int hashCode() {
		return ("RabinRankInfo" + rankInfoString()).hashCode();

	}

	public int get_row_count() {
		return this.count_rows;
	}

	public int get_env_goal() {
		return this.env_goal;
	}

	public int get_env_goal_count() {
		return this.count_env_goal;
	}

	@Override
	public int memVal() {
		return env_goal;
	}
}
