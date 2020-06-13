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

package tau.smlab.syntech.gamemodel.util;

import java.util.ArrayList;
import java.util.List;

import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;

/**
 * This class finds and builds game models according to traces of system behaviors
 * Auxiliary behaviors not attached to system or environment (like monitors, counters) count as system
 * 
 * @author shalom
 *
 */

public class SysTraceInfoBuilder extends TraceInfoBuilder {
	public SysTraceInfoBuilder(GameModel gm) {
		super(gm);
	}
	
	public GameModel build(List<Integer> taken) {
		// make sure that we ask only for listed behaviors. All non listed are taken
		assert(traceList.containsAll(taken));
		
		List<BehaviorInfo> gameSys = new ArrayList<BehaviorInfo>();
		
		for (BehaviorInfo bi : sys) {
			if (!traceList.contains(bi.traceId) || taken.contains(bi.traceId)) {
				gameSys.add(bi);
			}
		}
		
		List<BehaviorInfo> gameAux = gm.getAuxBehaviorInfo();
		gameAux.clear();
		for (BehaviorInfo bi : aux) {
			if (!traceList.contains(bi.traceId) || taken.contains(bi.traceId)) {
				gameAux.add(bi);
			}
		}		
		
		GameBuilderUtil.buildSys(gm, gameSys);
		return gm;
		
	}
	
	protected void createTraceList() {
		TraceIdentifier iden = new TraceIdentifier(gm);
		traceList = iden.getSysTraces();
	}
}
