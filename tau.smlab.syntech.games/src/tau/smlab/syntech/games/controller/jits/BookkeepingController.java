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

import java.util.List;
import java.util.Map;

import net.sf.javabdd.BDD;

/**
 * 
 * @author Ilia
 *
 */
public class BookkeepingController extends AbstractJitController {

	public BookkeepingController(JitController jitController) {
		super(jitController);
	}

	private boolean[] bookkeepingArray;
	private boolean master;

	@Override
	public void load(String folder, String name, Map<String, String[]> sysVars, Map<String, String[]> envVars) {
		jitController.load(folder, name, sysVars, envVars);
		bookkeepingArray = new boolean[jitController.getJusticeGar().size()];
	}

	@Override
	public BDD next(BDD currentState, BDD inputs) {
		
		int jx = jitController.getJitState().getJx();
		List<BDD> justiceGar = jitController.getJusticeGar();
		int n = justiceGar.size();
		
		// Update bookkeeping array
		for (int j = jx; j < n; j++) {
        	if (bookkeepingArray[j] == master) {
        		if (!justiceGar.get(j).and(currentState).isZero()) {
        			bookkeepingArray[j] = !master;
        		}
        	}
		}
		for (int j = 0; j < jx; j++) {
        	if (bookkeepingArray[j] == !master) {
        		if (!justiceGar.get(j).and(currentState).isZero()) {
        			bookkeepingArray[j] = master;
        		}
        	}
		}
		
		return jitController.next(currentState, inputs);
	}

	@Override
	public void init(BDD currentState) {
		jitController.init(currentState);
		jitController.getJitState().setGoalFinder(new NextJusticeGoalFinder() {
			
			@Override
			public int findNextJusticeGoal(int jx) {
				
				for (int j = jx + 1; j < jitController.getJusticeGar().size(); j++) {
		        	if (bookkeepingArray[j] == master) {
		        		return j;
		        	}
				}
				
				master = !master;
				for (int j = 0; j < jx; j++) {
		        	if (bookkeepingArray[j] == master) {
		            	return j;
		        	}
				}
				
				return jx;
			}
		});
	}

	@Override
	public void saveState() {
		this.controller.saveState();
	}

	@Override
	public void loadState() {
		this.controller.loadState();
	}

	@Override
	public BDD succ(BDD from) {
		return jitController.succ(from);
	}

	@Override
	public BDD pred(BDD to) {
		return jitController.pred(to);
	}
}
