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

package tau.smlab.syntech.games.gr1.unreal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.ModuleException;
import tau.smlab.syntech.games.GameIncrementalMemory;
import tau.smlab.syntech.games.gr1.GR1GameIncremental;
import tau.smlab.syntech.games.gr1.GR1GameIncrementalImplC;
import tau.smlab.syntech.games.rabin.RabinGameIncrementalImplC;
import tau.smlab.syntech.games.rabin.RabinMemory;
import tau.smlab.syntech.games.simple.SafetyGame;
import tau.smlab.syntech.jtlv.Env;

public class DdminUnrealizableCoreOpt extends DdminUnrealizableCore {

	private SegmentMem parentSegment;

	private boolean saveParent = false;
	private boolean saveSibling = false;
	private int ddMinIter = 0;
	private boolean useParentInfo = false;
	private boolean useSiblingInfo = false;
	List<BehaviorInfo> currMinIni;

	private Map<Segment, SegmentMem> segmentMemMap;
	private Segment currSegment;

	static public boolean MINIMIZE_INI_STATES = false;
	static public boolean CHECK_SAFETY_GAME_FIRST = false;

	public class Segment implements Comparable<Segment> {

		private int low;
		private int high;

		public Segment(int low, int high) {
			this.low = low;
			this.high = high;
		}

		@Override
		public String toString() {
			return "[" + low + "," + high + ")";
		}

		public int getLow() {
			return low;
		}

		public int getHigh() {
			return high;
		}

		@Override
		public boolean equals(Object o) {
			if (this == o)
				return true;
			if (!(o instanceof Segment))
				return false;
			Segment seg = (Segment) o;
			return ((low == seg.low) && (high == seg.high));
		}

		public boolean contains(Segment s) {
			return (low <= s.low) && (high >= s.high);
		}

		@Override
		public int compareTo(Segment s) {
			int res = this.low < s.low ? -1 : 0;
			res = this.low > s.low ? 1 : res;
			if (res != 0)
				return res;

			res = this.high < s.high ? -1 : 0;
			res = this.high > s.high ? 1 : res;
			return res;
		}
	}

	private class SegmentMem {
		List<BehaviorInfo> bahaviorInfo;
		private Map<Integer, Integer> justiceOrderMap;

		// GR1 memory
		private BDD[] zMem;
		private BDD[][][] xMem;
		private BDD[] firstZIterMem;

		// Rabin memory
		RabinMemory rabinMem;

		public SegmentMem(List<BehaviorInfo> bahaviorInfo) {
			this.bahaviorInfo = new ArrayList<BehaviorInfo>(bahaviorInfo);
			justiceOrderMap = new HashMap<>();
			this.zMem = null;
			this.xMem = null;
			this.rabinMem = null;
		}

		void setRabinMem(RabinMemory rabinMem) {
			this.rabinMem = rabinMem;
		}

		public void setZMem(BDD[] zMem) {
			this.zMem = zMem;
		}

		public void setXMem(BDD[][][] xMem) {
			this.xMem = xMem;
		}

		public void setFirstZIterMem(BDD[] firstZIterMem) {
			this.firstZIterMem = firstZIterMem;
		}

		public void free() {
			Env.free(zMem);
			zMem = null;
			Env.free(xMem);
			xMem = null;
			Env.free(firstZIterMem);
			firstZIterMem = null;
			if (rabinMem != null) {
				rabinMem.free();
				rabinMem = null;
			}
			justiceOrderMap.clear();
			bahaviorInfo.clear();
		}
	}

	public DdminUnrealizableCoreOpt(GameModel model) {
		super(model);

		// parentSet = new ArrayList<BehaviorInfo>();
		parentSegment = new SegmentMem(new ArrayList<BehaviorInfo>());
		segmentMemMap = new TreeMap<Segment, SegmentMem>();
	}

	private boolean hasSafetyCore() {
		SafetyGame sg = new SafetyGame(this.model);
		return !sg.checkRealizability();
	}

	private List<BehaviorInfo> GetSysSafetyBehaviorInfo() {
		List<BehaviorInfo> sysBehaviorInfo = model.getSysBehaviorInfo();
		List<BehaviorInfo> safetyInfo = new ArrayList<BehaviorInfo>();

		for (BehaviorInfo sysInfo : sysBehaviorInfo) {
			if (sysInfo.isSafety() || sysInfo.isInitial()) {
				safetyInfo.add(sysInfo);
			}
		}

		return safetyInfo;
	}

	@SuppressWarnings("unused")
	private List<BehaviorInfo> GetIniList() {
		List<BehaviorInfo> sysBehaviorInfo = model.getSysBehaviorInfo();
		List<BehaviorInfo> iniInfo = new ArrayList<BehaviorInfo>();

		for (BehaviorInfo sysInfo : sysBehaviorInfo) {
			if (sysInfo.isInitial()) {
				iniInfo.add(sysInfo);
			}
		}

		return iniInfo;
	}

	public List<BehaviorInfo> minimizeOpt(List<BehaviorInfo> elements) {

		System.out.println("---------------------START-------------------------------");

		List<BehaviorInfo> result;
		if (!CHECK_SAFETY_GAME_FIRST) {
			result = minimize(elements);
		} else {
			List<BehaviorInfo> sysSafety = GetSysSafetyBehaviorInfo();
			if (hasSafetyCore()) {
				System.out.println("There is a safety core");
				result = minimize(sysSafety);
			} else {
				System.out.println("There is NO a safety core");
				List<List<BehaviorInfo>> safetySet = new ArrayList<List<BehaviorInfo>>();
				safetySet.add(sysSafety);
				System.out.println("sysSafety size = " + sysSafety.size());
				result = minimize(elements, safetySet);
			}
		}

		parentSegment.free();
		freeSegmentMemMap();

		return result;
	}

	@Override
	protected List<BehaviorInfo> ddmin(List<BehaviorInfo> elements, int n, List<List<BehaviorInfo>> negSets) {

		System.out.println("---------ddmin start: n = " + n);
		ddMinIter++;

		int numElem = elements.size();
		int subSize = numElem / n;
		// sets need to be larger otherwise we miss some elements
		if (subSize * n < numElem) {
			subSize++;
		}

		if (numElem == 1 || n < 2) {
			if (numElem == 1 && check(new ArrayList<BehaviorInfo>())) {
				return new ArrayList<BehaviorInfo>();
			}
			return elements;
		}

		if (ddMinIter == 1) {
			useParentInfo = false;
			useSiblingInfo = false;
		}

		// First Case: If there exists a part that in unrealizable, recursively continue
		// with that part
		for (int i = 0; (i * subSize) < numElem; i++) {

			Segment seg = new Segment(i * subSize, Math.min((i + 1) * subSize, numElem));
			ArrayList<BehaviorInfo> part = new ArrayList<BehaviorInfo>(elements.subList(seg.low, seg.high));
			currSegment = seg;
			System.out.println("Checking: " + seg.low + ":" + seg.high + ", " + part.size() + " elements");

			saveParent = true;
			saveSibling = true;
			if (check(part, negSets)) {
				useParentInfo = true;
				useSiblingInfo = false;
				freeSegmentMemMap();
				removeRedundantIni(part);
				return ddmin(part, 2, negSets);
			}
		}

		// Second Case: check complements of parts
		if (n != 2) {
			useParentInfo = false;
			useSiblingInfo = true;
			for (int i = 0; (i * subSize) < numElem; i++) {
				int low = i * subSize;
				int high = Math.min((i + 1) * subSize, numElem);

				ArrayList<BehaviorInfo> part = new ArrayList<BehaviorInfo>(elements.subList(0, low));
				part.addAll(elements.subList(high, numElem));
				System.out.println(
						"Checking: 0:" + low + " and " + high + ":" + numElem + ", " + part.size() + " elements");
				saveParent = true;
				saveSibling = false;
				if (check(part, negSets)) {
					useParentInfo = true;
					useSiblingInfo = false;
					freeSegmentMemMap();
					removeRedundantIni(part);
					return ddmin(part, n - 1, negSets);
				}
			}
		}

		// Third Case: increase granularity and check for smaller subsets
		if (n < elements.size()) {
			useSiblingInfo = true;
			useParentInfo = false;
			return ddmin(elements, Math.min(numElem, 2 * n), negSets);
		}

		return elements;
	}

	@Override
	protected boolean check(List<BehaviorInfo> part) {

		System.out.println("DdminUnrealizableCoreOpt Check()");
		System.out.println("old sys.justiceNum() = " + sys.justiceNum());
		try {
			buildSys(part);
		} catch (ModuleException e) {
			throw new RuntimeException(e);
		}

		boolean unrealizable = false;
		GR1GameIncremental gr1Inc = new GR1GameIncrementalImplC(model);
		// new GR1GameIncremental(model);
		RabinGameIncrementalImplC rabinInc = new RabinGameIncrementalImplC(model);
		;
		if (gameType.equals(GameType.GR1_GAME)) {
			buildIncrementProfile(gr1Inc.GetGameIncrementalMemory(), part);
			unrealizable = !gr1Inc.checkRealizability();
		} else if (gameType.equals(GameType.RABIN_GAME)) {
			buildIncrementProfile(rabinInc.GetGameIncrementalMemory(), part);
			unrealizable = rabinInc.checkRealizability();
		}

		if (saveSibling && (!USE_NEGATIVE_SETS_LIST || unrealizable)) {
			System.out.println("saveSibling - saveSegment for next iters, segment " + currSegment);

			if (segmentMemMap.containsKey(currSegment)) {
				System.out.println("The segment " + currSegment + " is in the segmentMemMap already");
			}

			SegmentMem segMem = new SegmentMem(part);
			segMem.justiceOrderMap = new HashMap<Integer, Integer>(sys.getJusticeIndexToIDMap());

			if (gameType.equals(GameType.GR1_GAME)) {
				segMem.setXMem(gr1Inc.getXMemoryCopy());
				segMem.setZMem(gr1Inc.getZMemoryCopy());
				segMem.setFirstZIterMem(gr1Inc.getFirstZIterMem());
			} else if (gameType.equals(GameType.RABIN_GAME)) {
				segMem.setRabinMem(rabinInc.getMemoryCopy());
			}
			segmentMemMap.put(currSegment, segMem);
		}

		if (unrealizable) {
			if (MINIMIZE_INI_STATES && gameType.equals(GameType.GR1_GAME)) {
				MinimizeInitialStates minIniStates = new MinimizeInitialStates(model, gr1Inc.sysWinningStates());
				currMinIni = minIniStates.minimize(part);
			} else {
				currMinIni = new ArrayList<BehaviorInfo>();
			}

		}

		if (unrealizable && saveParent) {
			System.out.println("unrealizable - saving parent memory for next iter");
			parentSegment.bahaviorInfo = new ArrayList<BehaviorInfo>(part);

			if (gameType.equals(GameType.GR1_GAME)) {
				Env.free(parentSegment.zMem);
				parentSegment.zMem = null;
				parentSegment.zMem = gr1Inc.getZMemoryCopy();

				Env.free(parentSegment.firstZIterMem);
				parentSegment.firstZIterMem = null;
				parentSegment.firstZIterMem = gr1Inc.getFirstZIterMem();

				Env.free(parentSegment.xMem);
				parentSegment.xMem = null;
				parentSegment.xMem = gr1Inc.getXMemoryCopy();
			} else if (gameType.equals(GameType.RABIN_GAME)) {
				parentSegment.setRabinMem(rabinInc.getMemoryCopy());
			}

			parentSegment.justiceOrderMap = new HashMap<Integer, Integer>(sys.getJusticeIndexToIDMap());
			System.out.println("parent bahaviorInfo = " + parentSegment.bahaviorInfo);
			System.out.println("parentJusticeOrder = " + parentSegment.justiceOrderMap);
		}

		gr1Inc.free();
		rabinInc.free();

		return unrealizable;
	}

	private boolean removeRedundantIni(List<BehaviorInfo> part) {
		if (!MINIMIZE_INI_STATES) {
			return false;
		}

		System.out.println("removeRedundantIni(): currMinIni =  " + currMinIni);

		List<BehaviorInfo> removeInis = new ArrayList<BehaviorInfo>();
		for (BehaviorInfo gar : part) {
			if (gar.isInitial() && !currMinIni.contains(gar)) {
				removeInis.add(gar);
				System.out.println("remove ini: " + gar);
			}
		}
		return part.removeAll(removeInis);
	}

	private void freeSegmentMemMap() {
		for (Map.Entry<Segment, SegmentMem> entry : segmentMemMap.entrySet()) {
			entry.getValue().free();
		}

		segmentMemMap.clear();
	}

	private void buildIncrementProfile(GameIncrementalMemory incMem, List<BehaviorInfo> currPart) {

		System.out.println(
				"buildIncrementProfile: useParentInfo = " + useParentInfo + ", useSiblingInfo = " + useSiblingInfo);
		System.out.println("new sys.justiceNum() = " + sys.justiceNum());

		if (useSiblingInfo) {
			// BDD startZ = Env.TRUE();
			boolean foundSuperet = false;
			boolean foundSubset = false;
			Segment foundSeg = new Segment(0, 0);
			int foundSegSize = 0;
			for (Map.Entry<Segment, SegmentMem> entry : segmentMemMap.entrySet()) {
				SegmentMem segMem = entry.getValue();
				Segment seg = entry.getKey();
				if (currPart.containsAll(segMem.bahaviorInfo)) {
					System.out.println("currPart contains segment " + seg);
					if ((gameType.equals(GameType.GR1_GAME) && segMem.zMem == null)
							|| (gameType.equals(GameType.RABIN_GAME) && segMem.rabinMem == null)) {
						System.out.println("There is no zMem to reuse for segmant: " + seg);
						continue;
					}

					// System.out.println("Add to startZ: startZ.isOne = " + startZ.isOne() +
					// ", segMem.zMem["+lastIdx+"].isOne = " + segMem.zMem[lastIdx].isOne());
					// BDD tmp = startZ;
					// startZ = tmp.and(segMem.zMem[lastIdx]);
					// tmp.free();
					if (gameType.equals(GameType.GR1_GAME)) {
						int lastIdx = segMem.zMem.length - 1;
						incMem.addToStartZ(segMem.zMem[lastIdx], lastIdx);
					} else if (gameType.equals(GameType.RABIN_GAME)) {
						int lastIdx = segMem.rabinMem.sizeZ() - 1;
						incMem.addToStartZ(segMem.rabinMem.getZ(lastIdx), lastIdx);
					}

					// TODO: TMP
					if ((gameType.equals(GameType.GR1_GAME) && segMem.xMem == null)
							|| (gameType.equals(GameType.RABIN_GAME) && segMem.rabinMem == null)) {
						System.out.println("There is no xMem to reuse for segmant: " + seg);
						continue;
					}

					System.out.println("Can reuse memory from segmant: " + seg);
					// NOTE: take the largest contained segment.
					// can be optimized to take a segment that might benefit more to the diff
					if (segMem.bahaviorInfo.size() > foundSegSize) {
						foundSegSize = segMem.bahaviorInfo.size();
						foundSeg = seg;
						System.out.println("reuse candidate: " + seg);
					}
					foundSubset = true;

				} else if (segMem.bahaviorInfo.containsAll(currPart)) {
					// NOTE: sibling can act as parent when there are no negative sets in check()
					// and we increase granularity in ddmin
					System.out.println("currPart contained in segment " + seg);
					foundSuperet = true;
					foundSeg = seg;
					break;
				}
			}

			if (foundSuperet) {
				SegmentMem segMem = segmentMemMap.get(foundSeg);
				handleSupersetPrevMem(incMem, segMem, currPart);
			} else if (foundSubset) {
				// rg.setStartZ(startZ);
				SegmentMem segMem = segmentMemMap.get(foundSeg);
				handleSubsetPrevMem(incMem, segMem, currPart);
			} else {
				System.out.println("There is no memory to reuse");
			}
		} else if (useParentInfo) {
			if (!parentSegment.bahaviorInfo.containsAll(currPart)) {
				System.out.println("ERROR: parent set doesn't contain the current set, can't use parent info");
				return;
			}
			handleSupersetPrevMem(incMem, parentSegment, currPart);
		}

		boolean isAdded = incMem.NEW_INI_ADDED || incMem.NEW_SAFETY_ADDED || incMem.NEW_JUSTICE_ADDED;
		boolean isRemoved = incMem.PREV_INI_REMOVED || incMem.PREV_SAFETY_REMOVED || incMem.PREV_JUSTICE_REMOVED;

		if (isAdded && isRemoved) {
			System.out.println("ERROR: added and removed must be mutually exclusive!!!");
		}

		String resStr = incMem.NEW_INI_ADDED ? "\tNEW_INI_ADDED\n" : "";
		resStr += incMem.NEW_SAFETY_ADDED ? "\tNEW_SAFETY_ADDED\n" : "";
		resStr += incMem.NEW_JUSTICE_ADDED ? "\tNEW_JUSTICE_ADDED\n" : "";
		resStr += incMem.PREV_INI_REMOVED ? "\tPREV_INI_REMOVED\n" : "";
		resStr += incMem.PREV_SAFETY_REMOVED ? "\tPREV_SAFETY_REMOVED\n" : "";
		resStr += incMem.PREV_JUSTICE_REMOVED ? "\tPREV_JUSTICE_REMOVED\n" : "";
		System.out.print("buildIncrementProfile result: \n" + resStr);
	}

	private void handleSupersetPrevMem(GameIncrementalMemory gm, SegmentMem supersetSegMem, List<BehaviorInfo> subset) {
		System.out.println("handleSupersetPrevMem Start");
		gm.leastRemovedJusticeIdx = supersetSegMem.justiceOrderMap.size();
		System.out.println("supersetJusticeOrder = " + supersetSegMem.justiceOrderMap);
		for (BehaviorInfo elem : supersetSegMem.bahaviorInfo) {
			if (!subset.contains(elem)) {
				System.out.println("Diff set from parentSet: " + elem + " was removed");
				if (elem.isJustice()) {
					gm.PREV_JUSTICE_REMOVED = true;
					// rg.setPrevFirstZIterMem(supersetFirstZIterMem);
					int justiceIdx = supersetSegMem.justiceOrderMap.get(elem.traceId);
					System.out.println("justiceIdx = " + justiceIdx + ", rg.leastRemovedJusticeIdx = "
							+ gm.leastRemovedJusticeIdx);
					gm.leastRemovedJusticeIdx = gm.leastRemovedJusticeIdx > justiceIdx ? justiceIdx
							: gm.leastRemovedJusticeIdx;
					System.out.println("justiceIdx = " + justiceIdx + ", rg.leastRemovedJusticeIdx = "
							+ gm.leastRemovedJusticeIdx);
					System.out.println("parentJusticeOrder = " + supersetSegMem.justiceOrderMap);
				}
				if (elem.isSafety()) {
					gm.PREV_SAFETY_REMOVED = true;
				}

				if (elem.isInitial()) {
					gm.PREV_INI_REMOVED = true;
				}
			}
		}

		if (gm.leastRemovedJusticeIdx == supersetSegMem.justiceOrderMap.size()) {
			gm.PREV_JUSTICE_REMOVED = false;
			gm.leastRemovedJusticeIdx = 0;
		}

		System.out.println("gm.leastRemovedJusticeIdx = " + gm.leastRemovedJusticeIdx);

		if (gameType.equals(GameType.GR1_GAME)) {
			gm.setPrevZMemory(supersetSegMem.zMem);
			gm.setPrevXMem(supersetSegMem.xMem);
			gm.setPrevFirstZIterMem(supersetSegMem.firstZIterMem);
		} else if (gameType.equals(GameType.RABIN_GAME)) {
			gm.setPrevMem(supersetSegMem.rabinMem.getZMem(), supersetSegMem.rabinMem.getXMem());
		}

		System.out.println("handleSupersetPrevMem End");
	}

	private void handleSubsetPrevMem(GameIncrementalMemory gm, SegmentMem subsetSegMem, List<BehaviorInfo> superset) {
		System.out.println("handleSubsetPrevMem Start");
		ArrayList<BehaviorInfo> newJusticeList = new ArrayList<BehaviorInfo>();
		boolean setPrevFirstZItr = false;
		for (BehaviorInfo elem : superset) {
			if (!subsetSegMem.bahaviorInfo.contains(elem)) {
				System.out.println("Diff set from sibling set: " + elem + " was added");

				if (elem.isJustice()) {
					gm.NEW_JUSTICE_ADDED = true;
					newJusticeList.add(elem);
					setPrevFirstZItr = true;
				}

				if (elem.isSafety()) {
					gm.NEW_SAFETY_ADDED = true;
				}

				if (elem.isInitial()) {
					gm.NEW_INI_ADDED = true;
					setPrevFirstZItr = true;
				}
			}
		}

		if (gameType.equals(GameType.GR1_GAME)) {
			gm.setPrevZMemory(subsetSegMem.zMem);
			gm.setPrevXMem(subsetSegMem.xMem);
			if (setPrevFirstZItr)
				gm.setPrevFirstZIterMem(subsetSegMem.firstZIterMem);
		} else if (gameType.equals(GameType.RABIN_GAME)) {
			gm.setPrevMem(subsetSegMem.rabinMem.getZMem(), subsetSegMem.rabinMem.getXMem());
		}

		// NOTE: ensure the added justices are last in the order
		if (!newJusticeList.isEmpty()) {
			sys.resetJustice();
			ArrayList<BehaviorInfo> gars = new ArrayList<BehaviorInfo>();
			gars.addAll(aux);
			gars.addAll(superset);

			for (BehaviorInfo gar : gars) {
				if (gar.isJustice() && !newJusticeList.contains(gar)) {
					sys.addJustice(gar.justice.id(), gar.traceId);
				}
			}

			// NOTE: when justice added the rabin game needs the first index of the new
			// justice
			if (gameType.equals(GameType.RABIN_GAME)) {
				gm.leastRemovedJusticeIdx = sys.justiceNum();
			}

			for (BehaviorInfo just : newJusticeList) {
				sys.addJustice(just.justice.id(), just.traceId);
			}
		}
		System.out.println("handleSubsetPrevMem End");
	}
}
