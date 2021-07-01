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

package tau.smlab.syntech.gamemodel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.ModuleVariableException;
import tau.smlab.syntech.jtlv.VarKind;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

/**
 * module representing a player by
 * <li>variables as ModuleBDDField including their domains</li>
 * <li>BDD initial</li>
 * <li>BDD trans</li>
 * <li>List<BDD> justice</li>
 * 
 * The BDDs initial and trans are restricted according to the domains of the variables (while adding variables). The
 * composed domain restrictions are also kept in BDD doms.
 */
public class PlayerModule {

	public static boolean TEST_MODE = false;

	public enum TransFuncType {
		SINGLE_FUNC, DECOMPOSED_FUNC, PARTIAL_DECOMPOSED_FUNC
	}

	private class TransQuantPair {
		public TransQuantPair(BDD partTrans, BDDVarSet quantSet) {
			this.partTrans = partTrans;
			this.quantSet = quantSet;
		}


		public BDD partTrans;
		public BDDVarSet quantSet;
	}
	
	/**
	 * 
	 * A nested class that represents an existential requirement of this player module.
	 *
	 */
	public class ExistentialRequirement {
		List<BDD> existFinallyAssrts;
		SFAModuleConstraint regExpSfaConstraint;
		
		public ExistentialRequirement(List<BDD> existFinallyAssrts) {
			this.existFinallyAssrts = existFinallyAssrts;
		}
		
		public ExistentialRequirement(SFAModuleConstraint regExpSfaConstraint) {
			this.regExpSfaConstraint = regExpSfaConstraint;
		}
		
		public boolean hasRegExp() {
			return this.regExpSfaConstraint != null;
		}

		public List<BDD> getExistFinallyAssrts() {
			return existFinallyAssrts;
		}

		public SFAModuleConstraint getRegExpSfaConstraint() {
			return regExpSfaConstraint;
		}
		
		public int rank() {
			if(this.existFinallyAssrts != null) {
				return this.existFinallyAssrts.size();
			}
			return 0;
		}
	}

	private String name;
	private BDD initial = Env.TRUE();
	private BDD doms = Env.TRUE();
	private boolean simConjunctAbs;
	private TransFuncType transFunc;
	private BDD trans = Env.TRUE();
	private List<TransQuantPair> transQuantList = new ArrayList<>();
	private List<BDD> transList = new ArrayList<>();
	private List<BDD> justice = new ArrayList<>();
	private Map<Integer, Integer> justiceIDs = new HashMap<>();
	private List<ExistentialRequirement> existential = new ArrayList<>();
	private int existRegExpReqNum = 0;
	private Map<Integer, Integer> existentialIDs = new HashMap<>();

	/**
	 * was called all_couples
	 */
	private List<ModuleBDDField> allFields = new ArrayList<ModuleBDDField>();
	private List<ModuleBDDField> auxFields = new ArrayList<ModuleBDDField>();
	private List<ModuleBDDField> nonAuxFields = new ArrayList<ModuleBDDField>();

	private BDDVarSet cachedPrimeVars;
	private int cachedVarsLen;
	private BDDVarSet cachedUnprimeVars;

	/**
	 * returns set of all primed variables of the module
	 * 
	 * (do not consume)
	 * 
	 * @return
	 */
	public BDDVarSet modulePrimeVars() {
		if (cachedPrimeVars == null || this.getAllFields().size() != cachedVarsLen) {
			computeVarSets();
		}
		return cachedPrimeVars;
	}

	/**
	 * computes the cachedPrimeVars and cachedUnprimeVars
	 */
	private void computeVarSets() {
		cachedPrimeVars = Env.getEmptySet();
		cachedUnprimeVars = Env.getEmptySet();
		cachedVarsLen = this.getAllFields().size();

		for (ModuleBDDField c : this.getAllFields()) {
			if (c.isPrime()) {
				cachedPrimeVars = cachedPrimeVars.union(c.support());
				cachedUnprimeVars = cachedUnprimeVars.union(c.unprime().support());
			} else {
				cachedPrimeVars = cachedPrimeVars.union(c.prime().support());
				cachedUnprimeVars = cachedUnprimeVars.union(c.support());
			}
		}
	}

	public BDDVarSet moduleUnprimeVars() {
		if (cachedUnprimeVars == null || this.getAllFields().size() != cachedVarsLen) {
			computeVarSets();
		}
		return cachedUnprimeVars;
	}

	/**
	 * all fields including auxiliary fields (auxFields + nonAuxFields)
	 * 
	 * @see getAuxFields()
	 * @see getNonAuxFields()
	 * @return
	 */
	public List<ModuleBDDField> getAllFields() {
		return allFields;
	}

	/**
	 * remove the given field from the module's field lists (all/aux/nonAux)
	 * 
	 * @param f
	 */
	public void removeField(ModuleBDDField f) {
		allFields.remove(f);
		auxFields.remove(f);
		nonAuxFields.remove(f);
	}

	public void setSCA(boolean sca) {
		simConjunctAbs = sca;
	}
	/**
	 * returns original initial states
	 * 
	 * @return
	 */
	public BDD initial() {
		return initial;
	}

	/**
	 * returns original transition relation
	 * 
	 * @return
	 */
	public BDD trans() {
		return trans;
	}

	public int existReqNum() {
		return existential.size();
	}
	
	/**
	 * Returns the number of existential requirements that have regular expressions.
	 * 
	 * @return
	 */
	public int existRegExpReqNum() {
		return existRegExpReqNum;
	}
	
	/**
	 * Checks whether this module has any existential requirements.
	 * 
	 * @return
	 */
	public boolean hasExistReqs() {
		return !this.existential.isEmpty();
	}

	/**
	 * Returns the existential requirement at index <code>k</code>.
	 * 
	 * @param k
	 * @return
	 */
	public ExistentialRequirement existReqAt(int k) {
		try {
			return existential.get(k);
		} catch (IndexOutOfBoundsException e) {
			throw new IndexOutOfBoundsException("No existential requirement at index " + k);
		}
	}
	
	/**
	 * 
	 * Checks whether the existential requirement at index <code>k</code> contains a regular expression.
	 * 
	 * @param k
	 * @return
	 */
	public boolean existReqHasRegExp(int k) {
		try {
			return existential.get(k).hasRegExp();
		} catch (IndexOutOfBoundsException e) {
			throw new IndexOutOfBoundsException("No existential requirement at index " + k);
		} 
	}

	/**
	 * Returns the rank of the {@code k}-th existential requirement.
	 * The rank is nesting depth of finally (F) temporal operators.
	 * If the {@code k}-th existential requirement consists of a regular expression, its rank is 0.
	 * 
	 * @param k
	 * @return 
	 */
	public int existReqRank(int k) {
		try {
			return existential.get(k).rank();
		} catch (IndexOutOfBoundsException e) {
			throw new IndexOutOfBoundsException("No existential requirement at index " + k);
		}
	}

	/**
	 * Adds the specified existential requirement.
	 * 
	 * @param exReq the existential requirement
	 */
	public void addExistReq(ExistentialRequirement exReq) {
		this.existential.add(exReq);
		if(exReq.hasRegExp()) {
			existRegExpReqNum++;
		}
	}
	
	/**
	 * Adds a new existential requirement with the assertions in {@code existFinallyAssrts}.
	 * 
	 * @param existFinallyAssrts the assertions
	 */
	public void addExistReq(List<BDD> existFinallyAssrts) {
		this.addExistReq(new ExistentialRequirement(existFinallyAssrts));
	}
	
	/**
	 * Adds a new existential requirement that contains the specified regular expression
	 * given by the BDD encoding of its corresponding SFA.
	 * 
	 * @param regExpSfaConstraint the BDD encoding of the SFA that corresponds to the regular expression 
	 */
	public void addExistReq(SFAModuleConstraint regExpSfaConstraint) {
		this.addExistReq(new ExistentialRequirement(regExpSfaConstraint));
	}
	
	/**
	 * Adds a new existential requirement that contains the specified regular expression
	 * given by the BDD encoding of its corresponding SFA.
	 * 
	 * @param regExpSfaConstraint the BDD encoding of the SFA that corresponds to the regular expression
	 * @param traceId
	 */
	public void addExistReq(SFAModuleConstraint regExpSfaConstraint, int traceId) {
		this.addExistReq(new ExistentialRequirement(regExpSfaConstraint), traceId);
	}
	
	/**
	 * Adds a new existential requirement with the assertions in {@code existFinallyAssrts}.
	 * 
	 * @param existFinallyAssrts the assertions
	 * @param traceId
	 */
	public void addExistReq(List<BDD> existFinallyAssrts, int traceId) {
		this.addExistReq(new ExistentialRequirement(existFinallyAssrts), traceId);
	}
	
	/**
	 * Adds the specified existential requirement.
	 * 
	 * @param exReq the existential requirement
	 * @param traceId
	 */
	public void addExistReq(ExistentialRequirement exReq, int traceId) {
		this.addExistReq(exReq);
		this.existentialIDs.put(traceId, this.existential.size() - 1);
	}

	public Map<Integer, Integer> getTraceIdToExistReqIndexMap() {
		return this.existentialIDs;
	}

	public int justiceNum() {
		return justice.size();
	}

	/**
	 * Returns the original justice at index <code>i</code>
	 * 
	 * @param i
	 * @return
	 */
	public BDD justiceAt(int i) {
		try {
			return justice.get(i);
		} catch (Exception e) {
			throw new ArrayIndexOutOfBoundsException("No justice at index " + i);
		}
	}

	/**
	 * Adds the justice (does not free it!)
	 * 
	 * @param j
	 */
	public void addJustice(BDD j) {
		this.justice.add(j);
	}

	public void addJustice(BDD j, int id) {
		this.justice.add(j);
		this.justiceIDs.put(id, this.justice.size() - 1);
	}

	public Map<Integer, Integer> getJusticeIndexToIDMap() {
		return this.justiceIDs;
	}
	
	public BDD[] getJustices() {
		return this.justice.toArray(new BDD[this.justice.size()]);
	}

	/**
	 * conjunct transitions with <code>moreTrans</code> (consumed)
	 * 
	 * @param moreTrans
	 */
	public void conjunctTrans(BDD moreTrans) {
		trans.andWith(moreTrans);
	}

	/**
	 * conjunct transitions with <code>moreIni</code> (consumed)
	 * 
	 * @param moreIni
	 */
	public void conjunctInitial(BDD moreIni) {
		initial.andWith(moreIni);
	}

	/**
	 * Sets the type of the transition function that will be used in yield states.
	 * 
	 * @param type
	 *          the type of the transition function
	 * */
	public void setTransFuncType(TransFuncType type) {
		transFunc = type;
	}

	/**
	 * Gets the type of the transition function that will be used in yield states.
	 * */
	public TransFuncType getTransFuncType() {
		return transFunc;
	}

	/**
	 * Add a partial transition function to the transition functions list
	 * 
	 * @param trans
	 *          the partial transition function
	 * */
	public void addToTransList(BDD trans) {
		//    System.out.println("addToTransList: " + trans.support());
		BDDVarSet transSet = getIntersectSupportWithPrimed(trans);
		boolean isAdded = false;
		for (int i = 0; i < transList.size(); i++) {
			BDDVarSet set = getIntersectSupportWithPrimed(transList.get(i));
			if (transSet.equals(set)) {
				transList.get(i).andWith(trans);
				isAdded = true;
				break;
			}
		}
		if (!isAdded) {
			transList.add(trans);
		}    
	}

	public void simpleAddToTransList(BDD trans) {
		//    System.out.println("simpleAddToTransList: " + trans.support());
		transList.add(trans);
	}


	/**
	 * Create a single transition function from the partitioned transition function
	 * */
	public void createTransFromPartTrans() {
		if(trans != null && !trans.isFree()) {
			trans.free();
		}
		trans = Env.TRUE();
		for (TransQuantPair p : transQuantList) {
			conjunctTrans(p.partTrans);
		}
	}

	public List<TransQuantPair> getTransQuantList() {
		return transQuantList;
	}

	public BDD[] getPartTransArray() {
		if (transFunc == TransFuncType.SINGLE_FUNC) {
			return new BDD[0];
		}

		BDD[] partTransArr = new BDD[transQuantList.size()];
		for (int i = 0; i < transQuantList.size(); i++) {
			partTransArr[i] = transQuantList.get(i).partTrans;
		}
		return partTransArr;
	}

	public BDDVarSet[] getQuantSetArray() {
		if (transFunc == TransFuncType.SINGLE_FUNC) {
			return new BDDVarSet[0];
		}

		BDDVarSet[] quantSetArr = new BDDVarSet[transQuantList.size()];
		for (int i = 0; i < transQuantList.size(); i++) {
			quantSetArr[i] = transQuantList.get(i).quantSet;
		}
		return quantSetArr;
	}

	private BDDVarSet getIntersectSupportWithPrimed(BDD bdd) {    
		BDDVarSet set1 = bdd.support();
		BDDVarSet intr = getIntersectSupportWithPrimed(set1);
		set1.free();
		return intr;
	}

	private BDDVarSet getIntersectSupportWithPrimed(BDDVarSet set1) {
		BDDVarSet intr = Env.getEmptySet();
		BDDVarSet set2 = modulePrimeVars();
		int[] arr1 = set1.toArray();
		int[] arr2 = set2.toArray();

		for (int i = 0; i < arr1.length; i++) {
			for (int j = 0; j < arr2.length; j++) {
				if (arr1[i] == arr2[j]) {
					intr.unionWith(arr1[i]);
					break;
				}
			}
		}
		return intr;
	}

	@SuppressWarnings("unused")
	private BDDVarSet getMinus(BDDVarSet set1, BDDVarSet set2) {
		BDDVarSet minus = Env.getEmptySet();

		int[] arr1 = set1.toArray();
		int[] arr2 = set2.toArray();

		for (int i = 0; i < arr1.length; i++) {
			boolean isEqual = false;
			for (int j = 0; j < arr2.length; j++) {
				if (arr1[i] == arr2[j]) {
					isEqual = true;
					break;

				}
			}
			if (!isEqual) {
				minus.unionWith(arr1[i]);
			}
		}

		return minus;
	}

	// map from primed support set to the quantification set
	//  private Map<BDDVarSet, BDDVarSet> partTransQuantMap = new HashMap<>();
	private Map<Set<Integer>, BDDVarSet> partTransQuantMap = new HashMap<>();
	//  private List<BDDVarSet> orderTransList = new ArrayList<>();
	private List<Set<Integer>> orderTransList = new ArrayList<>();
	//private Map<BDD, Set<Integer>> primeSuppSet = new HashMap<>();

	public void addToPartTransList(BDD trans) {   
		BDDVarSet pSupp = getIntersectSupportWithPrimed(trans);
		//    partTransQuantMap.put(toIntSet(pSupp), Env.getEmptySet());
		Set<Integer> set = toIntSet(pSupp);
		if (!partTransQuantMap.containsKey(set)) {
			partTransQuantMap.put(set, Env.getEmptySet());
		}
		//    partTransQuantMap.put(pSupp, Env.getEmptySet());
	}

	/*
	 * Calculate the the order of the quantifications according to:
	 * 1. Largest set that can be quantified out
	 * 2. Largest (primed) support set
	 */
	public void partCalcTransQuantList() {
//		System.out.println("partTransQuantMap size =  " + partTransQuantMap.size());
//		int idx = 0;
//		for (Set<Integer> supp : partTransQuantMap.keySet()) {
//			System.out.println("partTransQuantMap support("+idx+") =  " + supp);
//			idx++;
//		}

		Set<Set<Integer>> keySets = new HashSet<Set<Integer>>();//partTransQuantMap.keySet());
		for (Set<Integer> key : partTransQuantMap.keySet()) {
			keySets.add(key.stream().collect(Collectors.toSet()));
		}

		while (!keySets.isEmpty()) {
			Set<Integer> maxSet = new HashSet<>();
			Set<Integer> keyMaxSet = new HashSet<>(); //Env.getEmptySet();
			for (Set<Integer> key : keySets) {
				Set<Integer> eset = key.stream().collect(Collectors.toSet());//toIntSet(key);
				for (Set<Integer> otherKey : keySets) {
					if (!key.equals(otherKey)) {
						Set<Integer> otherSet = otherKey;//toIntSet(otherKey);
						eset.removeAll(otherSet);
						if (eset.isEmpty()) {
							break;
						}
					}
				}
				if (maxSet.size() < eset.size()) {
					maxSet = eset;
					keyMaxSet = key.stream().collect(Collectors.toSet());//key;
				}
			}

			if (!maxSet.isEmpty()) {
//				System.out.println("found maximum E: " + maxSet + ", for key support set: " + keyMaxSet);
				partTransQuantMap.put(keyMaxSet, toVarSet(maxSet));
				orderTransList.add(keyMaxSet);
				keySets.remove(keyMaxSet);
				continue;
			}

//			System.out.println("no maximum E set - take the largest primed support set: ");
			Set<Integer> maxKey = new HashSet<>();
			for (Set<Integer> key : keySets) {
				if (key.size() > maxKey.size()) {
					maxKey = key;
				}
			}

			System.out.println("max primed support set: " + maxKey);
			if (!maxKey.isEmpty()) {
				orderTransList.add(maxKey);
				keySets.remove(maxKey);
			} else {
//				System.out.println("Can't decompose further, left trans: " + keySets.size());
				for (Set<Integer> k : keySets) {
					orderTransList.add(k);
				}
				break;
			}
		}
		keySets.clear();

//		System.out.println("orderTransList size =  " + orderTransList.size());
//		System.out.println("Order of quantification:");
//		for (int i = 0; i < orderTransList.size(); i++) {
//			System.out.println("support = " + orderTransList.get(i) + ", E_"+i+" = " + partTransQuantMap.get(orderTransList.get(i)));
//		}
	}

	/*
	 * Create the list of partitioned transitions and the sets to quantify,
	 * according to the calculations done in partCalcTransQuantList()
	 */
	public void createPartialTransQuantList() {
//		System.out.println("Curr transList size =  " + transList.size());
//		System.out.println("OrderTransList size =  " + orderTransList.size());

		//    for (int i = 0; i < orderTransList.size(); i++) {
		//      System.out.println("orderTransList.get("+i+").support() = " + orderTransList.get(i) 
		//      + ", E_"+i+" = " + partTransQuantMap.get(orderTransList.get(i)));      
		//    }
		//    
		//    for (int i = 0; i < transList.size(); i++) {
		//      System.out.println("transList.get("+i+").support() = " + transList.get(i).support() 
		//          + ", primed support = " +getIntersectSupportWithPrimed(transList.get(i).support()));      
		//    }

		Set<Integer> modulePrimeVars = toIntSet(modulePrimeVars());
		Set<Integer> leftoverSet = new HashSet<>(modulePrimeVars);

		Map<BDDVarSet, Integer> transSuppToIdx = new HashMap<>();
		Map<Set<Integer>, BDDVarSet> transPrimedSuppToSupp = new HashMap<>();
		for (int i = 0; i < transList.size(); i++) {
			BDDVarSet supp = transList.get(i).support();
			transSuppToIdx.put(supp, i);
			transPrimedSuppToSupp.put(toIntSet(getIntersectSupportWithPrimed(supp)), supp);
		}

		for (Set<Integer> psupp : orderTransList) {
			if (transList.isEmpty()) {
				break;
			}
			//check if elem in transPrimedSuppToSupp contained in psupp
			if (!transPrimedSuppToSupp.containsKey(psupp)) {
				BDDVarSet quantSet = partTransQuantMap.get(psupp);
				int last_idx = transQuantList.size() - 1;
				if (!transQuantList.isEmpty()) { // && transQuantList.get(last_idx).quantSet.isEmpty()) {
					transQuantList.get(last_idx).quantSet.unionWith(quantSet.id());
					leftoverSet.removeAll(toIntSet(quantSet));
				}
				continue;
			}

			BDDVarSet quantSet = partTransQuantMap.get(psupp);
			BDD partTrans = transList.get(transSuppToIdx.get(transPrimedSuppToSupp.get(psupp)));

			if (!quantSet.isEmpty()) {
				leftoverSet.removeAll(toIntSet(quantSet));

				int last_idx = transQuantList.size() - 1;
				if (!transQuantList.isEmpty() && transQuantList.get(last_idx).quantSet.isEmpty()) {
					transQuantList.get(last_idx).partTrans.andWith(partTrans);
					transQuantList.get(last_idx).quantSet = quantSet.id();
				} else {
					transQuantList.add(new TransQuantPair(partTrans, quantSet.id()));
				}
			} else {
				//        transQuantList.add(new TransQuantPair(partTrans.id(), quantSet.id()));
				if (transQuantList.isEmpty()) {
					transQuantList.add(new TransQuantPair(partTrans.id(), quantSet.id()));
				} else {
					transQuantList.get(transQuantList.size()-1).partTrans.andWith(partTrans);
				}
			}
		}

		int last_idx = transQuantList.size() - 1;
		if (last_idx > 0 && transQuantList.get(last_idx).quantSet.isEmpty()) {
			transQuantList.get(last_idx-1).partTrans.andWith(transQuantList.get(last_idx).partTrans);
			transQuantList.remove(last_idx);
		}

		if (!leftoverSet.isEmpty()) {
			System.out.println("leftoverSet =  " + leftoverSet);
			transQuantList.get(0).quantSet.unionWith(toVarSet(leftoverSet));
		}   

		// NOTE: group quantify sets with 1 variable to groups of 2 or 3 variables
		List<TransQuantPair> tmpList = new ArrayList<>();
		int setSize = 3;
		for (int j = 0; j < transQuantList.size(); j++) {
			if (j + 1 < transQuantList.size() && (transQuantList.get(j).quantSet.size() + transQuantList.get(j+1).quantSet.size() <= setSize)) {
				TransQuantPair p = new TransQuantPair(transQuantList.get(j).partTrans.andWith(transQuantList.get(j+1).partTrans), 
						transQuantList.get(j).quantSet.unionWith(transQuantList.get(j+1).quantSet));

				if (j + 2 < transQuantList.size() && (transQuantList.get(j+2).quantSet.size() + p.quantSet.size() <= setSize)) {
					p.partTrans.andWith(transQuantList.get(j+2).partTrans);
					p.quantSet.unionWith(transQuantList.get(j+2).quantSet);
					j++;
				}

				j++;
				tmpList.add(p);
			} else {
				tmpList.add(transQuantList.get(j));
			}

		}

		transQuantList.clear();
		transQuantList = tmpList;

		for (int j = 0; j < transQuantList.size(); j++) {
			BDDVarSet supp = transQuantList.get(j).partTrans.support();
//			System.out.println("transQuantList["+j+"].support = " + supp + ", E_"+j+" = " + transQuantList.get(j).quantSet);
			supp.free();
		}
	}

	/**
  public void partCalcTransQuantList() {
    System.out.println("partTransList size =  " + partTransList.size());

//    if (PlayerModule.TEST_MODE) {
//      System.out.println("trans support: " + trans.support());
//      BDD calcTrans = Env.TRUE();
//      for (int i = 0; i < transList.size(); i++) {
//        calcTrans.andWith(transList.get(i).id());
//      }
//      
//      if (!calcTrans.equals(trans)){
//        System.err.println("ERROR: The oroginal transition function doesn't equal the conjuction of the decomposed functions!" );
//      }
//      
//      System.out.println("calcTrans.support: " + calcTrans.support()); 
//      calcTrans.free();
//    }

    Map<BDD, Set<Integer>> primeSuppSet = new HashMap<>();
    List<BDD> currList = partTransList;
    Set<Integer> modulePrimeVars = toIntSet(modulePrimeVars());
//    for (BDD b : currList) {
//      Set<Integer> primedSup = toIntSuppSet(b);
//      primedSup.retainAll(modulePrimeVars);
//      primeSuppSet.put(b, primedSup);
//    }

    while (!currList.isEmpty()) {
      Set<Integer> maxSet = new HashSet<>();
      int maxIdx = -1;
      for (int j = 0; j < currList.size(); j++) {
//        Set<Integer> eset = new HashSet<Integer>(primeSuppSet.get(currList.get(j)));
//        Set<Integer> eset_orig = new HashSet<Integer>(primeSuppSet.get(currList.get(j)));  

        Set<Integer> eset = new HashSet<Integer>(primeSuppSet.get(currList.get(j)));
        for (int h = 0; h < currList.size(); h++) {
          if (h != j) {
            Set<Integer> otherSet = new HashSet<Integer>(primeSuppSet.get(currList.get(h)));
//            if (eset_orig.equals(otherSet)) { //TODO: NEW!!!
//              continue;
//            }
            eset.removeAll(otherSet);
            if (eset.isEmpty()) {
              break;
            }
          }
        }

        if (maxSet.size() < eset.size()) {
          maxSet = eset;
          maxIdx = j;
        }
      }

      if (maxIdx == -1) {
        System.out.println("no maximum - take the largest primed support set: ");
        Set<Integer> maxSupp = new HashSet<>();
        int maxSuppIdx = -1;
        for (int q = 0; q < currList.size(); q++) {
          Set<Integer> supp = primeSuppSet.get(currList.get(q));
          System.out.println("  " + supp);
          if (supp.size() > maxSupp.size()) {
            maxSupp = supp;
            maxSuppIdx = q;
          }
        }

        System.out.println("max support: " + maxSupp + ", maxSuppIdx: " + maxSuppIdx);
        if (maxSuppIdx > -1) {
          partTransQuantList.add(new TransQuantPair(currList.get(maxSuppIdx), Env.getEmptySet()));
          currList.remove(maxSuppIdx);
        } else {
          System.out.println("Can't decompose further, left trans: " + currList.size());
          for (BDD b : currList) {
            partTransQuantList.add(new TransQuantPair(b, Env.getEmptySet()));
          }
          break;
        }
      } else {
        System.out.println("found maximum E: " + maxSet);
        partTransQuantList.add(new TransQuantPair(currList.get(maxIdx), toVarSet(maxSet)));
        currList.remove(maxIdx);
      }
    }

//    for (BDD b : currList) {
//      b.free();
//    }
    currList.clear();

    for (int j = 0; j < partTransQuantList.size(); j++) {
      BDDVarSet supp = partTransQuantList.get(j).partTrans.support();
      System.out.println("partTransQuantList["+j+"].support = " + supp + ", E_"+j+" = " + partTransQuantList.get(j).quantSet);
      supp.free();
    }
  }
	 */

	/*
	 * Create the list of partitioned transitions and the sets to quantify,
	 * according to the calculations done in partCalcTransQuantList()
	 */
	/**
  public void createPartialTransQuantList() {
    System.out.println("Curr transList size =  " + transList.size());
    System.out.println("left partTransList size =  " + partTransList.size());

    Set<Integer> modulePrimeVars = toIntSet(modulePrimeVars());
    BDDVarSet leftoverSet = modulePrimeVars().id();

    for (TransQuantPair tq : partTransQuantList) {
      if (transList.isEmpty()) {
        break;
      }
      BDDVarSet supp = tq.partTrans.support();
      if (transList.remove(tq.partTrans)) {
        if (!tq.quantSet.isEmpty()) {
          leftoverSet = getMinus(leftoverSet, tq.quantSet);
          if (!transQuantList.isEmpty() && 
              (transQuantList.get(transQuantList.size()-1).quantSet.isEmpty() 
//                //TODO: NEW!!!
               || (transQuantList.get(transQuantList.size()-1).quantSet.equals(tq.quantSet) && 
                   transQuantList.get(transQuantList.size()-1).partTrans.support().equals(tq.partTrans.support()))
                  )) {
            transQuantList.get(transQuantList.size()-1).partTrans.andWith(tq.partTrans.id());
            transQuantList.get(transQuantList.size()-1).quantSet = tq.quantSet.id();
          } else {
            transQuantList.add(new TransQuantPair(tq.partTrans.id(), tq.quantSet.id()));
          }
        } else {

          if (transQuantList.isEmpty()) {
            transQuantList.add(new TransQuantPair(tq.partTrans.id(), tq.quantSet.id()));
          } else {
            transQuantList.get(transQuantList.size()-1).partTrans.andWith(tq.partTrans.id());
          }
        }
      }
    }

    for (BDD b : transList) {
      System.out.println("left in transList: " + b.support());
    }

    Set<Integer> e = new HashSet<>();
    BDD leftTrans = Env.TRUE();
    System.out.println("intersect the remaining trans: " + transList.size());
    for (BDD trans : transList) {
      BDDVarSet inter = getIntersectSupportWithPrimed(trans);
      System.out.println("  " + inter);
      inter.free();
      leftTrans.andWith(trans);
    }
    e = toIntSuppSet(leftTrans);
    e.retainAll(modulePrimeVars);
    System.out.println("leftoverSet: " + leftoverSet);
    leftoverSet = getMinus(leftoverSet, toVarSet(e));

    System.out.println("resulting E: " + e);
//    System.out.println("leftTrans: " + leftTrans.support());
//    System.out.println("leftoverSet: " + leftoverSet);

    if (!e.isEmpty() || transQuantList.isEmpty()) {
      transQuantList.add(new TransQuantPair(leftTrans, toVarSet(e)));
    } else {
      transQuantList.get(transQuantList.size()-1).partTrans.andWith(leftTrans);
    } 
    transList.clear();

    if (!leftoverSet.isEmpty()) {
      transQuantList.get(transQuantList.size()-1).quantSet.unionWith(leftoverSet);
    }



    // NOTE: group quantify sets with 1 variable to groups of 2 or 3 variables
    List<TransQuantPair> tmpList = new ArrayList<>();
    int setSize = 3;
    for (int j = 0; j < transQuantList.size(); j++) {
      if (j + 1 < transQuantList.size() && (transQuantList.get(j).quantSet.size() + transQuantList.get(j+1).quantSet.size() <= setSize)) {
        TransQuantPair p = new TransQuantPair(transQuantList.get(j).partTrans.andWith(transQuantList.get(j+1).partTrans), 
            transQuantList.get(j).quantSet.unionWith(transQuantList.get(j+1).quantSet));

        if (j + 2 < transQuantList.size() && (transQuantList.get(j+2).quantSet.size() + p.quantSet.size() <= setSize)) {
          p.partTrans.andWith(transQuantList.get(j+2).partTrans);
          p.quantSet.unionWith(transQuantList.get(j+2).quantSet);
          j++;
        }

        j++;
        tmpList.add(p);
      } else {
        tmpList.add(transQuantList.get(j));
      }

    }

//    System.err.println("7. " + (System.nanoTime() - startTimeNano));
    transQuantList.clear();
    transQuantList = tmpList;

    for (int j = 0; j < transQuantList.size(); j++) {
      BDDVarSet supp = transQuantList.get(j).partTrans.support();
      System.out.println("transQuantList["+j+"].support = " + supp + ", E_"+j+" = " + transQuantList.get(j).quantSet);
      supp.free();
    }
  }
	 */
	public void calcTransQuantList() {
//		long start = System.nanoTime();
		//    long startTimeNano = System.nanoTime();

//		System.out.println("modulePrimeVars: " + modulePrimeVars());
//		for (int i = 0; i < transList.size(); i++) {
//			System.out.println("transList.get("+i+").support() = " + transList.get(i).support());      
//		}


		if (transList.size() ==1 && transList.get(0).isOne()) {
//			System.out.println("Trans is TRUE");
			transQuantList.add(new TransQuantPair(transList.get(0), modulePrimeVars()));
			return;
		}
		//    System.err.println("1. " + (System.nanoTime() - startTimeNano));

		//    if (PlayerModule.TEST_MODE) {
		//      System.out.println("trans support: " + trans.support());
		//      BDD calcTrans = Env.TRUE();
		//      for (int i = 0; i < transList.size(); i++) {
		//        calcTrans.andWith(transList.get(i).id());
		//      }
		//      
		//      if (!calcTrans.equals(trans)){
		//        System.err.println("ERROR: The oroginal transition function doesn't equal the conjuction of the decomposed functions!" );
		//      }
		//      
		//      System.out.println("calcTrans.support: " + calcTrans.support()); 
		//      calcTrans.free();
		//    }

		//    System.err.println("2. " + (System.nanoTime() - startTimeNano));

		List<BDD> currList = transList;
		Set<Integer> modulePrimeVars = toIntSet(modulePrimeVars());
		Set<Integer> leftoverSet = new HashSet<>(modulePrimeVars);
		Map<BDD, Set<Integer>> primeSuppSet = new HashMap<>();
		for (BDD b : currList) {
			Set<Integer> primedSup = toIntSuppSet(b);
			primedSup.retainAll(modulePrimeVars); // currently leftoverSet is the primed vars
			primeSuppSet.put(b, primedSup);
		}

		//    System.err.println("3. " + (System.nanoTime() - startTimeNano));
		while (!currList.isEmpty()) {
			Set<Integer> maxSet = new HashSet<>();
			int maxIdx = -1;
			for (int j = 0; j < currList.size(); j++) {
				Set<Integer> eset = new HashSet<Integer>(primeSuppSet.get(currList.get(j)));        
				for (int h = 0; h < currList.size(); h++) {
					if (h != j) {
						Set<Integer> otherSet = new HashSet<Integer>(primeSuppSet.get(currList.get(h)));
						eset.removeAll(otherSet);
						if (eset.isEmpty()) {
							break;
						}
					}
				}
				if (maxSet.size() < eset.size()) {
					maxSet = eset;
					maxIdx = j;
				}
			}

			if (maxIdx > -1) {
				leftoverSet.removeAll(maxSet);
//				System.out.println("found maximum E: " + maxSet);
				transQuantList.add(new TransQuantPair(currList.get(maxIdx), toVarSet(maxSet)));
				currList.remove(maxIdx);
				continue;
			}

//			System.out.println("no maximum - take the largest support set: ");
			Set<Integer> maxSupp = new HashSet<>();
			int maxSuppIdx = -1;
			for (int q = 0; q < currList.size(); q++) {
				Set<Integer> supp = primeSuppSet.get(currList.get(q));
//				System.out.println("  " + supp);
				if (supp.size() > maxSupp.size()) {
					maxSupp = supp;
					maxSuppIdx = q;
				}
			}

//			System.out.println("max support: " + maxSupp + ", maxSuppIdx: " + maxSuppIdx);
			if (maxSuppIdx > -1) {
				transQuantList.add(new TransQuantPair(currList.get(maxSuppIdx), Env.getEmptySet()));
				currList.remove(maxSuppIdx);
			} else {
				Set<Integer> e = new HashSet<>();
				BDD leftTrans = Env.TRUE();
//				System.out.println("no maximum - intersect the remaining trans ");
				for (int q = 0; q < currList.size(); q++) {
//					Set<Integer> inter = primeSuppSet.get(currList.get(q));
//					System.out.println("  " + inter);
					leftTrans.andWith(currList.get(q));
				}
				e = toIntSuppSet(leftTrans);
				e.retainAll(modulePrimeVars);

//				System.out.println("resulting E: " + e);
				if (!e.isEmpty() || transQuantList.isEmpty()) {
					transQuantList.add(new TransQuantPair(leftTrans, toVarSet(e)));
				} else {
					transQuantList.get(transQuantList.size()-1).partTrans.andWith(leftTrans);
				} 
				leftoverSet.removeAll(e);
				currList.clear();
			}
		}

		//    System.err.println("4. " + (System.nanoTime() - startTimeNano));

		if (!leftoverSet.isEmpty()) {
			transQuantList.get(0).quantSet.unionWith(toVarSet(leftoverSet));
		}

		//    System.err.println("5. " + (System.nanoTime() - startTimeNano));

		// NOTE: handle the support sets with empty quantify sets
		List<TransQuantPair> tmpList = new ArrayList<TransQuantPair>(transQuantList);
		transQuantList.clear();
		Iterator<TransQuantPair> itr = tmpList.iterator();
		while (itr.hasNext()) {
			TransQuantPair p = itr.next();
			if (!p.quantSet.isEmpty()) {
				transQuantList.add(p);
			} else {
				boolean first = true;
				do {
					if (first) {
						transQuantList.add(p);
						first = false;
					} else {
						transQuantList.get(transQuantList.size()-1).partTrans.andWith(p.partTrans);
					}
					if (itr.hasNext()) {
						p = itr.next();
					} else {
						break;
					}
				} while (p.quantSet.isEmpty()); 
				transQuantList.get(transQuantList.size()-1).partTrans.andWith(p.partTrans);
				transQuantList.get(transQuantList.size()-1).quantSet = p.quantSet;
			}
		}

		//    System.err.println("6. " + (System.nanoTime() - startTimeNano));

		// NOTE: group quantify sets with 1 variable to groups of 2 or 3 variables
		tmpList = new ArrayList<>();
		int setSize = 3;
		for (int j = 0; j < transQuantList.size(); j++) {
			if (j + 1 < transQuantList.size() && (transQuantList.get(j).quantSet.size() + transQuantList.get(j+1).quantSet.size() <= setSize)) {
				TransQuantPair p = new TransQuantPair(transQuantList.get(j).partTrans.andWith(transQuantList.get(j+1).partTrans), 
						transQuantList.get(j).quantSet.unionWith(transQuantList.get(j+1).quantSet));

				if (j + 2 < transQuantList.size() && (transQuantList.get(j+2).quantSet.size() + p.quantSet.size() <= setSize)) {
					p.partTrans.andWith(transQuantList.get(j+2).partTrans);
					p.quantSet.unionWith(transQuantList.get(j+2).quantSet);
					j++;
				}

				j++;
				tmpList.add(p);
			} else {
				tmpList.add(transQuantList.get(j));
			}

		}

		//    System.err.println("7. " + (System.nanoTime() - startTimeNano));
		transQuantList.clear();
		transQuantList = tmpList;

		//    System.err.println("8. " + (System.nanoTime() - startTimeNano));

		//    BDDVarSet allSet = Env.getEmptySet();
		for (int j = 0; j < transQuantList.size(); j++) {
			BDDVarSet supp = transQuantList.get(j).partTrans.support();
//			System.out.println("transQuantList["+j+"].support = " + supp + ", E_"+j+" = " + transQuantList.get(j).quantSet);
//			System.out.println("nodes count = " +  transQuantList.get(j).partTrans.nodeCount());
			supp.free();
		}

//		long end = System.nanoTime() - start;
//		System.out.println("overall time of calcTransQuantList time = " + end);

		//    System.err.println("9. " + (System.nanoTime() - startTimeNano));

		//    if (!allSet.equals(modulePrimeVars())) {
		//      System.err.println("All the quant sets not equal to prime sets!!! ");
		//      System.err.println("  allSet = " + allSet);
		//      System.err.println("  cachedPrimeVars = " + modulePrimeVars());
		//    }  
		//    allSet.free();
	}

	/**
	 * create a BDDVarSet that contains the variables in e 
	 * @param e
	 * @return
	 */
	private BDDVarSet toVarSet(Set<Integer> e) {
		BDDVarSet res = Env.getEmptySet();
		for (int i : e) {
			res.unionWith(i);
		}
		return res;
	}

	/**
	 * create set that contains all integers of the BDDVarSet 
	 * @param varSet
	 * @return
	 */
	private Set<Integer> toIntSet(BDDVarSet varSet) {
		return Arrays.stream(varSet.toArray()).boxed().collect(Collectors.toSet());
	}

	private Set<Integer> toIntSuppSet(BDD b) {
		BDDVarSet supp = b.support();
		Set<Integer> res = toIntSet(supp); 
		supp.free();
		return res;
	}

	@SuppressWarnings("unused")
	private void calcTransQuantList2() { 
		computeVarSets();

		Iterator<BDD> transItr = transList.iterator();
		List<List<BDD>> tmpSets = new ArrayList<>();

		while (transItr.hasNext())
		{
			BDD bdd = transItr.next();
			BDDVarSet currVarSet = bdd.support().intersect(cachedUnprimeVars);
//			System.out.println("currVarSet = " + currVarSet);

			// TODO: add all the safeties to a list - conjunct just the ones with the same vars.
			// Then perform all the calculations of the E sets in some function inside the Player module. it can be called at the first yield. 
			boolean isDisjoint = true; 
			for (int k = 0; k < tmpSets.size(); k++) {
				for (int j = 0; j < tmpSets.get(k).size(); j++) {
					//System.out.println("k = " + k + ", j = " + j);
					//System.out.println("tmpSet.support() = " + tmpSets.get(k).get(j).support());
					BDDVarSet tmpVarSet = tmpSets.get(k).get(j).support().intersect(cachedUnprimeVars);
					BDDVarSet intersectSet = currVarSet.intersect(tmpVarSet);
					//System.out.println("tmpVarSet = " + tmpVarSet);
					//System.out.println("intersectSet = " + intersectSet);

					// TODO: handle disjoint sets separately?? 
					//          if (intersectSet.isEmpty()) {
					//            System.out.println("empty intersection, continue ");
					//            continue;
					//          }

					isDisjoint = false;
					boolean contains = intersectSet.equals(tmpVarSet);
					boolean contained = intersectSet.equals(currVarSet);
					if (contains && contained) {
//						System.out.println("equal support, add to this bdd");
						tmpSets.get(k).get(j).andWith(bdd.id());
						break;
					} else if (j == (tmpSets.get(k).size() - 1)) {
//						System.out.println("add to the end of the list ");
						tmpSets.get(k).add(bdd.id());
						break;
					} 
				}

				if (!isDisjoint) {
					break;
				}
			}

			if (isDisjoint && !bdd.support().isEmpty()) {
				List<BDD> l = new ArrayList<>();
				l.add(bdd.id());
				tmpSets.add(l);
//				System.out.println("found disjoint set, add list " + bdd.support());
			}

			if (bdd.support().isEmpty()) {
//				System.out.println("bdd with empty support: " + bdd);
			}
		}

//		for (int k = 0; k < tmpSets.size(); k++) {
//			System.out.println("set " + k);
//			for (int j = 0; j < tmpSets.get(k).size(); j++) { 
//				System.out.println("  " + tmpSets.get(k).get(j).support());   
//			}
//		}

//		System.out.println("trans support: " + trans.support());

		List<List<TransQuantPair>> finalTrans = new ArrayList<>();

		for (int k = 0; k < tmpSets.size(); k++) {
//			System.out.println("set " + k);

			List<TransQuantPair> partTransQuantList = new ArrayList<>();

			List<BDD> currList = tmpSets.get(k);
			while (!currList.isEmpty()) {
				BDDVarSet maxSet = Env.getEmptySet();
				int maxIdx = -1;
				for (int j = 0; j < currList.size(); j++) {           
					BDDVarSet set = currList.get(j).support().intersect(cachedUnprimeVars);
					BDDVarSet eset = set;

					for (int h = 0; h < currList.size(); h++) {
						if (h != j) {
							eset = eset.minus(currList.get(h).support().intersect(cachedUnprimeVars));  
						}
					}

					if (maxSet.size() < eset.size()) {
						maxSet = eset;
						maxIdx = j;
					}
				}

				if (maxIdx == -1) {
//					System.out.println("no maximum - intersect the remaining trans ");
					BDD leftTrans = Env.TRUE();
					for (int q = 0; q < currList.size(); q++) {
						leftTrans.andWith(currList.get(q));
					}
					BDDVarSet e = leftTrans.support().intersect(cachedUnprimeVars);
//					System.out.println("resulting E: " + e);
//					System.out.println("intersect with prime: " + leftTrans.support().intersect(cachedPrimeVars));
//					System.out.println("is TRUE: " + leftTrans.isOne());
//					System.out.println("is FALSE: " + leftTrans.isZero());
					if (!e.isEmpty()) {
						partTransQuantList.add(new TransQuantPair(leftTrans, e));
					}
					currList.clear();
				} else {
//					System.out.println("found maximum E: " + maxSet);
					partTransQuantList.add(new TransQuantPair(currList.get(maxIdx), maxSet));
					currList.remove(maxIdx);
				}
			}

			finalTrans.add(partTransQuantList);
		}

		// NOTE: keep the order of sets of every list, but order according the BDDVarSets between the lists. 
		//    while (!finalTrans.isEmpty()) {
		//      int maxSize = 0;
		//      int maxIdx = -1;
		//      for (int k = 0; k < finalTrans.size(); k++) {
		//        System.out.println("------------------------------------" + finalTrans.get(k).get(0).quantSet.size());
		//        if (maxSize < finalTrans.get(k).get(0).quantSet.size()) {
		//          maxSize = finalTrans.get(k).get(0).quantSet.size();
		//          maxIdx = k;
		//        }
		//      }
		//      transQuantList.add(finalTrans.get(maxIdx).get(0));
		//      finalTrans.get(maxIdx).remove(0);
		//      if (finalTrans.get(maxIdx).isEmpty()) {
		//        finalTrans.remove(maxIdx);
		//      }
		//    }

		for (int j = 0; j < transQuantList.size(); j++) {
			BDDVarSet supp = transQuantList.get(j).partTrans.support();
			System.out.println("transQuantList["+j+"].support = " + supp + ", E_"+j+" = " + transQuantList.get(j).quantSet);
			supp.free();
		}

	}
	/**
	 * <p>
	 * A state s in included in the returning result if responder module can force this module to reach a state in
	 * "to".<br>
	 * That is, regardless of how this module will move from the result, the responder module can choose an appropriate
	 * move into "to".
	 * </p>
	 * 
	 * @param responder
	 *          The module which moves (i.e. this is the responder).
	 * @param to
	 *          The states to reach.
	 * @return The states which can be controlled by this module.
	 */
	public BDD yieldStatesTransDecomposed(PlayerModule responder, BDD to) {    
		List<TransQuantPair> responder_transQuantList = responder.getTransQuantList();
		//    System.out.println("to.support: " + to.support());
		BDD res = Env.prime(to);

		for (int i = 0; i < responder_transQuantList.size(); i++) {
			//      BDD tmp = res.and(responder_transQuantList.get(i).partTrans);
			//      System.out.println("sys is trans: " + responder_transQuantList.get(i).equals(responder.trans()));
			//      res.free();
			//      res = tmp.exist(responder_transQuantList.get(i).quantSet);
			//      tmp.free();

			BDD tmp;
			if (simConjunctAbs) {
				tmp = res.relprod(responder_transQuantList.get(i).partTrans, responder_transQuantList.get(i).quantSet);
				res.free();
				res = tmp;
			} else {
				tmp = res.and(responder_transQuantList.get(i).partTrans);
				res.free();
				res = tmp.exist(responder_transQuantList.get(i).quantSet);
				tmp.free();
			}
		}

		if (PlayerModule.TEST_MODE) {
//			System.out.println("-------res.nodeCount = " + res.nodeCount());
			BDD tmpPrimedBdd = Env.prime(to);
			BDD tmpAndBdd = tmpPrimedBdd.and(responder.trans());
			BDD exy = tmpAndBdd.exist(responder.modulePrimeVars());

			if (!exy.equals(res)){
				System.err.println("exy not equals res" );
				System.err.println("  exy.support() = " + exy.support());
				System.err.println("  res.support() = " + res.support());
				assert(false);
			}

			exy.free();
			tmpAndBdd.free();
			tmpPrimedBdd.free();
		}

		for (int i = 0; i < this.transQuantList.size(); i++) {
			BDD tmp = this.transQuantList.get(i).partTrans.imp(res);
			res.free();
			res = tmp.forAll(this.transQuantList.get(i).quantSet);
			tmp.free();
		}

		if (PlayerModule.TEST_MODE) {
			BDDVarSet intrTest = getIntersectSupportWithPrimed(res);
			if (!intrTest.isEmpty()) {
				System.err.println("ERR: has prime vars!!!");
				System.err.println("  res.support() = " + res.support());
				System.err.println("  res.support().intersect(cachedPrimeVars) = " + intrTest);
			}
			intrTest.free();

			BDD res2 = yieldStatesOrig(responder, to);
			if (!res2.equals(res))
			{
				System.err.println("ERR: Not equal yield states! ");
				System.err.println("res.support() = " + res.support());
				System.err.println("res2.support() = " + res2.support());
				assert(false);
			}
			res2.free();  
		}
		return res;
	}

	public BDD yieldStatesOrig(PlayerModule responder, BDD to) {
		//    System.out.println("yieldStatesOrig");
		BDDVarSet responder_prime = responder.modulePrimeVars();
		BDDVarSet this_prime = this.modulePrimeVars();
		BDD tmpPrimedBdd = Env.prime(to);

		//    System.out.println("tmpPrimedBdd.nodeCount() = " + tmpPrimedBdd.nodeCount());
		//    System.out.println("responder.trans().nodeCount() = " + responder.trans().nodeCount());

		BDD exy;
		if (simConjunctAbs) {
			exy = tmpPrimedBdd.relprod(responder.trans(), responder_prime);
		} else {
			BDD tmpAndBdd = tmpPrimedBdd.and(responder.trans());
			//      System.out.println("after and: tmpAndBdd.nodeCount() = " + tmpAndBdd.nodeCount());
			//      System.out.println("to == responder.justiceAt(0): " +to.equals(responder.justiceAt(0)));
			//      System.out.println("tmpPrimedBdd == tmpAndBdd: " + tmpPrimedBdd.equals(tmpAndBdd));
			exy = tmpAndBdd.exist(responder_prime);
			tmpAndBdd.free();
		}

		//    System.out.println("after exist: exy = " + exy.toStringWithDomains());
		//    System.out.println("after exist: exy.nodeCount() = " + exy.nodeCount());
		//    System.out.println("after exist: exy.isOne() = " + exy.isOne());
		//    System.out.println("after exist: exy.isZero() = " + exy.isZero());
		BDD exyImp = this.trans().imp(exy);
		BDD res = exyImp.forAll(this_prime);

		//    System.out.println("res.nodeCount() = " + res.nodeCount());
		tmpPrimedBdd.free();
		exy.free();
		exyImp.free();
		return res;
	}

	public BDD yieldStates(PlayerModule responder, BDD to) {
		switch (transFunc) {
		case SINGLE_FUNC: 
			return yieldStatesOrig(responder, to);
		case DECOMPOSED_FUNC:
			return yieldStatesTransDecomposed(responder,to);
		case PARTIAL_DECOMPOSED_FUNC:
			return yieldStatesTransDecomposed(responder,to);
		default:
			System.err.println("Unknown type: transFunc = " + transFunc);
			break;
		}

		return Env.FALSE();  
	}

	public BDD yieldStatesTrans(PlayerModule responder, BDD trans) {
		BDDVarSet responder_prime = responder.modulePrimeVars();
		BDDVarSet this_prime = this.modulePrimeVars();
		BDD tmpAndBdd = trans.and(responder.trans());
		BDD exy = tmpAndBdd.exist(responder_prime);
		BDD exyImp = this.trans().imp(exy);
		BDD res = exyImp.forAll(this_prime);
		tmpAndBdd.free();
		exy.free();
		exyImp.free();
		return res;
	}
	/**
	 * <p>
	 * A state s is included in the returning result if the this module can force the responder to reach a state in
	 * "to".<br>
	 * </p>
	 * 
	 * @param responder
	 *          The module to check for.
	 * @param to
	 *          The states to reach.
	 * @return The states which can be controlled by this module.
	 */
	public BDD controlStates(PlayerModule responder, BDD to) {
		BDDVarSet responder_prime = responder.modulePrimeVars();
		BDDVarSet this_prime = this.modulePrimeVars();
		BDD tmpPrimeBdd = Env.prime(to);
		BDD tmpAndBdd = responder.trans().imp(tmpPrimeBdd);
		BDD exy = tmpAndBdd.forAll(responder_prime);
		BDD exyAnd = this.trans().and(exy);
		BDD res = exyAnd.exist(this_prime);
		tmpPrimeBdd.free();
		tmpAndBdd.free();
		exy.free();
		exyAnd.free();
		return res;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
	
	/**
	 * <p>
	 * Add a Boolean variable to this module.
	 * </p>
	 * 
	 * @param new_var the new variable name
	 * @param restrictIniTrans if true, then initials and transitions are restricted to the domain of the newly created variable
	 * @return The newly created field.
	 * @throws ModuleException
	 *           If an illegal manipulation to the module had been done. e.g. duplicate variable names.
	 * @throws ModuleVariableException
	 */
	public ModuleBDDField addVar(String new_var, boolean aux, boolean restrictIniTrans) throws ModuleException,
	ModuleVariableException {
		return addVar(new_var, VarKind.BOOLEAN, 2, null, Integer.MIN_VALUE, aux, restrictIniTrans);
	}
	
	
	/**
	 * <p>
	 * Add a Boolean variable to this module.
	 * </p>
	 * 
	 * @param new_var the new variable name
	 * @return The newly created field.
	 * @throws ModuleException
	 *           If an illegal manipulation to the module had been done. e.g. duplicate variable names.
	 * @throws ModuleVariableException
	 */
	public ModuleBDDField addVar(String new_var, boolean aux) throws ModuleException,
	ModuleVariableException {
		return addVar(new_var, VarKind.BOOLEAN, 2, null, Integer.MIN_VALUE, aux, true);
	}

	/**
	 * <p>
	 * Add a set of values variable to this module.
	 * </p>
	 * 
	 * @param new_var
	 *          The new variable name.
	 * @param val_names
	 *          The set of values that this variable can be assigned with.
	 * @param restrictIniTrans if true, then initials and transitions are restricted to the domain of the newly created variable
	 * @return The newly created field.
	 * @throws ModuleException
	 *           If an illegal manipulation to the module had been done. e.g. duplicate variable names.
	 * @throws ModuleVariableException
	 */
	public ModuleBDDField addVar(String new_var, String[] val_names, boolean aux, boolean restrictIniTrans)
			throws ModuleException, ModuleVariableException {
		return addVar(new_var, VarKind.ENUMERATION, val_names.length, val_names, Integer.MIN_VALUE, aux, restrictIniTrans);
	}
	
	/**
	 * <p>
	 * Add a set of values variable to this module.
	 * </p>
	 * 
	 * @param new_var
	 *          The new variable name.
	 * @param val_names
	 *          The set of values that this variable can be assigned with.
	 * @return The newly created field.
	 * @throws ModuleException
	 *           If an illegal manipulation to the module had been done. e.g. duplicate variable names.
	 * @throws ModuleVariableException
	 */
	public ModuleBDDField addVar(String new_var, String[] val_names, boolean aux)
			throws ModuleException, ModuleVariableException {
		return addVar(new_var, val_names, aux, true);
	}

	/**
	 * <p>
	 * Add a range variable to this module.
	 * </p>
	 * 
	 * @param new_var
	 *          The new variable name.
	 * @param range_start
	 *          The starting range of the variable.
	 * @param range_end
	 *          The ending range of the variable.
	 * @param restrictIniTrans if true, then initials and transitions are restricted to the domain of the newly created variable
	 * @return The newly created field.
	 * @throws ModuleException
	 *           If an illegal manipulation to the module had been done. e.g. duplicate variable names.
	 * @throws ModuleVariableException
	 */
	public ModuleBDDField addVar(String new_var, int range_start, int range_end, boolean aux, boolean restrictIniTrans)
			throws ModuleException, ModuleVariableException {
		return addVar(new_var, VarKind.RANGE, (range_end - range_start + 1), null, range_start, aux, restrictIniTrans);
	}
	
	/**
	 * <p>
	 * Add a range variable to this module.
	 * </p>
	 * 
	 * @param new_var
	 *          The new variable name.
	 * @param range_start
	 *          The starting range of the variable.
	 * @param range_end
	 *          The ending range of the variable.
	 * @return The newly created field.
	 * @throws ModuleException
	 *           If an illegal manipulation to the module had been done. e.g. duplicate variable names.
	 * @throws ModuleVariableException
	 */
	public ModuleBDDField addVar(String new_var, int range_start, int range_end, boolean aux)
			throws ModuleException, ModuleVariableException {
		return addVar(new_var, range_start, range_end, aux, true);
	}
	
	/**
	 * <p>
	 * Check whether a variable with the given name exists.
	 * </p>
	 * 
	 * @param addr
	 *          The variable full name relative to this module.
	 * @return true if the variable field exists.
	 */
	public boolean hasVar(String addr) {
		if (addr == null || addr.equals("")) {
			return false;
		}
		for (ModuleBDDField coup : this.getAllFields()) {
			if (coup.getName().equals(addr)) {
				return true;
			}
		}
		return false;
	}
	
//	private ModuleBDDField addVar(String new_var, VarKind kind, int values_size,
//			String[] val_names, int range_start, boolean aux) throws ModuleException, ModuleVariableException {
//		return addVar(new_var, kind, values_size, val_names, range_start, aux, true);
//	}

	private ModuleBDDField addVar(String new_var, VarKind kind, int values_size,
			String[] val_names, int range_start, boolean aux, boolean restrictIniTrans) throws ModuleException, ModuleVariableException {
		if (new_var == null || new_var.equals(""))
			throw new ModuleException("Couldn't declare a variable with no " + "name.");

		ModuleBDDField bdd_var = Env.getVar(new_var);
		if (bdd_var != null) {
			List<String> evs = Env.getValueNames(new_var);
			switch (kind) {
			case BOOLEAN:
				if (evs.size() != 2 || !evs.contains("true") || !evs.contains("false")) {
					throw new ModuleException("Variable " + new_var + " already declared with different domain.");
				}
				break;
			case RANGE:
				if (evs.size() != values_size || !evs.contains("" + range_start)
				|| !evs.contains("" + (range_start + values_size - 1))) {
					throw new ModuleException("Variable " + new_var + " already declared with different domain.");
				}
				break;
			case ENUMERATION:
				if (evs.size() != values_size || !evs.containsAll(Arrays.asList(val_names))) {
					throw new ModuleException("Variable " + new_var + " already declared with different domain.");
				}
				break;
			}
		} else {
			bdd_var = Env.newVar(new_var, values_size);

			// register value names
			switch (kind) {
			case BOOLEAN:
				Env.addBDDValueLookup(new_var, "false", bdd_var.getDomain().ithVar(0));
				Env.addBDDValueLookup(new_var, "true", bdd_var.getDomain().ithVar(1));
				Env.addBDDValueLookup(new_var + "'", "false", bdd_var.getOtherDomain().ithVar(0));
				Env.addBDDValueLookup(new_var + "'", "true", bdd_var.getOtherDomain().ithVar(1));
				break;
			case RANGE:
				Env.stringer.register_domain_module_values(bdd_var.getDomain(), range_start, values_size);
				Env.stringer.register_domain_module_values(bdd_var.getOtherDomain(), range_start, values_size);
				for (long i = range_start; i < range_start + values_size; i++) {
					Env.addBDDValueLookup(new_var, "" + i, bdd_var.getDomain().ithVar(i - range_start));
					Env.addBDDValueLookup(new_var + "'", "" + i, bdd_var.getOtherDomain().ithVar(i - range_start));
				}
				break;
			case ENUMERATION:
				if (values_size != val_names.length)
					throw new ModuleException("Internal error: values list do " + "not match the size");
				Env.stringer.register_domain_module_values(bdd_var.getDomain(), val_names);
				Env.stringer.register_domain_module_values(bdd_var.getOtherDomain(), val_names);
				for (long i = 0; i < val_names.length; i++) {
					Env.addBDDValueLookup(new_var, val_names[(int) i], bdd_var.getDomain().ithVar(i));
					Env.addBDDValueLookup(new_var + "'", val_names[(int) i], bdd_var.getOtherDomain().ithVar(i));
				}
				break;
			default:
				break;
			}
		}

		this.allFields.add(bdd_var);
		if (aux) {
			this.auxFields.add(bdd_var);
		} else {
			this.nonAuxFields.add(bdd_var);
		}
		// update doms with domains of variables
		this.doms.andWith(bdd_var.getDomain().domain());
		this.doms.andWith(bdd_var.getOtherDomain().domain());
		
		if(restrictIniTrans) {
			// restrict trans to domains
			conjunctTrans(doms.id());
			// restrict initials to domains
			conjunctInitial(bdd_var.getDomain().domain());
		}
		return bdd_var;
	}

	/**
	 * <p>
	 * Given a set of states, this procedure returns all states which can lead in a single module step to these states.
	 * </p>
	 * 
	 * @param to
	 *          The set of state to be reach.
	 * @return The set of states which can lead in a single module step to the given states.
	 */
	public BDD pred(BDD to) {
		return Env.pred(trans, to);
	}
	
	/**
	 * Given a set of states {@code to}, returns all states from which this and the responder module can reach {@code to} together in a single step.
	 * 
	 * @param responder
	 * @param to
	 * @return
	 */
	public BDD pred(PlayerModule responder, BDD to) {
		return pred(responder, to, null);
	}
	
	/**
	 * <p>Given a set of states {@code to}, returns all states from which this and the responder module can reach {@code to} together in a single step.
	 * However, as opposed to {@link #pred(PlayerModule, BDD)}, in addition to the transitions of this and the responder module, the single step also respects
	 * the transitions of the {@link SFAModuleConstraint} associated with the specified existential requirement at position {@code regExpSfaExReqIdx}.</p>
	 * 
	 * <p>Consequently, the returned set of states are of the form (s,q) where s is over the (unprimed) variables of both modules and q is over
	 * the variables that encode the states of the specified {@link SFAModuleConstraint}.</p>
	 * 
	 * @param responder
	 * @param to
	 * @param regExpSfaExReqIdx the index of the existential requirement, expected to have a regular expression
	 * @return
	 */
	public BDD pred(PlayerModule responder, BDD to, int regExpSfaExReqIdx) {
		ExistentialRequirement regExpExReq = this.existReqAt(regExpSfaExReqIdx);
		if(!regExpExReq.hasRegExp()) {
			throw new RuntimeException("The existential requirement at position " + regExpSfaExReqIdx + " does not have a regular expression (an SFA)");
		}
		return pred(responder, to, regExpExReq.getRegExpSfaConstraint());
	}
	
	/**
	 * <p>Given a set of states {@code to}, returns all states from which this and the responder module can reach {@code to} together in a single step.
	 * However, as opposed to {@link #pred(PlayerModule, BDD)}, in addition to the transitions of this and the responder module, the single step also respects
	 * the transitions of the specified {@link SFAModuleConstraint}, {@code exReqSfa}.</p>
	 * 
	 * <p>Consequently, the returned set of states are of the form (s,q) where s is over the (unprimed) variables of both modules and q is over
	 * the variables that encode the states {@code exReqSfa}.</p>
	 * 
	 * <p>However, if {@code exReqSfa} is {@code null}, it is ignored, and the result is as if {@link #pred(PlayerModule, BDD)} was invoked.</p>
	 * 
	 * @param responder
	 * @param to
	 * @param exReqSfa
	 * @return
	 */
	public BDD pred(PlayerModule responder, BDD to, SFAModuleConstraint exReqSfa) {
		switch (transFunc) {
		case SINGLE_FUNC: 
			return predSingleTrans(responder, to, exReqSfa);
		case DECOMPOSED_FUNC:
			return predTransDecomposed(responder,to, exReqSfa);
		case PARTIAL_DECOMPOSED_FUNC:
			return predTransDecomposed(responder,to, exReqSfa);
		default:
			System.err.println("Unknown type: transFunc = " + transFunc);
			break;
		}
		return Env.FALSE();  
	}
	
	private BDD predSingleTrans(PlayerModule responder, BDD to, SFAModuleConstraint exReqSfa) {
		BDD modulesTrans = (exReqSfa != null) ? (trans.and(responder.trans())).andWith(exReqSfa.getTrans().id()) : trans.and(responder.trans());
		BDD primedTo = Env.prime(to);		
		BDDVarSet modulesPrimeVars = this.modulePrimeVars().union(responder.modulePrimeVars());
		BDD result;
		if (simConjunctAbs) {
			result = primedTo.relprod(modulesTrans, modulesPrimeVars);
		}
		else {
			BDD transAndTo = modulesTrans.and(primedTo);
			result = transAndTo.exist(modulesPrimeVars);
			transAndTo.free();
		}


		if (PlayerModule.TEST_MODE) {
			BDD resSCA =  primedTo.relprod(modulesTrans, modulesPrimeVars);

			BDD transAndTo = modulesTrans.and(primedTo);
			BDD resAndEx = transAndTo.exist(modulesPrimeVars);
			transAndTo.free();

			if (!resSCA.equals(resAndEx)){
				System.err.println("resSCA not equals resAndEx" );
				assert(false);
			}
			resSCA.free();
			resAndEx.free();
		}

		modulesTrans.free();
		primedTo.free();		
		modulesPrimeVars.free();

		return result;
	}

	private BDD predTransDecomposed(PlayerModule responder, BDD to, SFAModuleConstraint exReqSfa) {
		List<TransQuantPair> responder_transQuantList = responder.getTransQuantList();
		BDD res = Env.prime(to);
		BDD tmp;
		if(exReqSfa != null) {
			if(simConjunctAbs) {
				tmp = res.relprod(exReqSfa.getTrans(), exReqSfa.getStatesVar().prime().support());
				res.free();
				res = tmp;
			}
			else {
				tmp = res.and(exReqSfa.getTrans());
				res.free();
				res = tmp.exist(exReqSfa.getStatesVar().prime().support());
				tmp.free();
			}
		}
		for (int i = 0; i < responder_transQuantList.size(); i++) {
			if (simConjunctAbs) {
				tmp = res.relprod(responder_transQuantList.get(i).partTrans, responder_transQuantList.get(i).quantSet);
				res.free();
				res = tmp;
			} else {
				tmp = res.and(responder_transQuantList.get(i).partTrans);
				res.free();
				res = tmp.exist(responder_transQuantList.get(i).quantSet);
				tmp.free();
			}
		}

		for (int i = 0; i < this.transQuantList.size(); i++) {
			if	(simConjunctAbs) {
				tmp = res.relprod(this.transQuantList.get(i).partTrans, this.transQuantList.get(i).quantSet);
				res.free();
				res = tmp;
			}
			else {
				tmp = res.and(this.transQuantList.get(i).partTrans);
				res.free();
				res = tmp.exist(this.transQuantList.get(i).quantSet);
				tmp.free();
			}
		}

		if (PlayerModule.TEST_MODE) {
			BDD singleRes = predSingleTrans(responder, to, exReqSfa);
			//				System.out.println("-------res.nodeCount = " + res.nodeCount());

			if (!singleRes.equals(res)){
				System.err.println("singleRes not equals res" );
				System.err.println("  singleRes.support() = " + singleRes.support());
				System.err.println("  res.support() = " + res.support());
				assert(false);
			}
			singleRes.free();
		}
		return res;
	}
	

	/**
	 * <p>
	 * Given a set of state, this procedure return all states which can lead in any number of module steps to these
	 * states.
	 * </p>
	 * 
	 * @param to
	 *          The set of state to be reach.
	 * @return The set of states which can lead in any number of module step to the given states.
	 */
	public BDD allPred(BDD to) {
		return Env.allPred(trans, to);
	}

	/**
	 * <p>
	 * This procedure return all states which the module can reach in a single step from given a set of state.
	 * </p>
	 * 
	 * @param from
	 *          The set of state to start from.
	 * @return The set of states which the module can reach in a single module step from the given states.
	 */
	public BDD succ(BDD from) {
		return Env.succ(from, trans);
	}

	/**
	 * <p>
	 * This procedure return all states which the module can reach in any number of steps from given a set of state.
	 * </p>
	 * 
	 * @param from
	 *          (consumed/freed) The set of state to start from.
	 * @return The set of states which the module can reach in any number of module steps from the given states.
	 */
	public BDD allSucc(BDD from) {
		return Env.allSucc(from, trans);
	}

	@Override
	public String toString() {
		StringBuffer buf = new StringBuffer("PlayerModule ");
		if (name != null) {
			buf.append(name);
		}
		buf.append("\n");
		buf.append("Variables\n");
		for (ModuleBDDField field : allFields) {
			String name = field.getName();
			buf.append("{");
			for (String val : Env.getValueNames(name)) {
				buf.append(val + ", ");
			}
			buf.replace(buf.length() - 2, buf.length(), "");
			buf.append("} ");
			buf.append(name + "\n");
		}
		buf.append("\n");
		buf.append("Initial\n");
		buf.append(Env.toNiceString(initial));
		buf.append("\n\n");
		buf.append("Safety\n");
		buf.append(Env.toNiceString(trans));
		buf.append("\n\n");
		buf.append("Justice\n");
		int i = 0;
		for (BDD j : justice) {
			buf.append(i++ + ": ");
			buf.append(Env.toNiceString(j));
		}
		buf.append("\n\n");
		buf.append("Existential\n");
		i = 0;
		for(ExistentialRequirement existReq : existential) {
			buf.append(i++ + ": ");
			if(existReq.hasRegExp()) {
				buf.append(existReq.getRegExpSfaConstraint().toString());
			}
			else {
				int j = 0;
				buf.append("The following ordered list of assertions:");
				for(BDD fAssrt : existReq.getExistFinallyAssrts()) {
					buf.append("\n");
					buf.append(j++ + ": ");
					buf.append(Env.toNiceString(fAssrt));
				}
			}
			buf.append("\n");
		}
		buf.append("\n\n");

		return buf.toString();
	}

	/**
	 * fields that are marked as auxiliary
	 * 
	 * @return
	 */
	public List<ModuleBDDField> getAuxFields() {
		return auxFields;
	}

	/**
	 * fields that are not marked as auxiliary
	 * 
	 * @return
	 */
	public List<ModuleBDDField> getNonAuxFields() {
		return nonAuxFields;
	}

	/**
	 * set initial to TRUE (restricted by variable domain constraint)
	 */
	public void resetInitial() {
		this.initial.free();
		this.initial = this.doms.exist(this.modulePrimeVars());
	}

	/**
	 * set trans to TRUE (restricted by variable domain constraint).
	 * Always sets the single trans to TRUE; if a decomposed trans is used, then it is also set to TRUE
	 */
	public void resetTrans() {    
		//an implementation which assumes that a single transition relation is always constructued
		this.trans.free();
		this.trans = this.doms.id();
		if (transFunc == TransFuncType.DECOMPOSED_FUNC ||
				transFunc == TransFuncType.PARTIAL_DECOMPOSED_FUNC) {
			this.transList.clear();
			this.transList.add(this.doms.id());
			this.transQuantList.clear();
		}
		//		if (transFunc == TransFuncType.SINGLE_FUNC) {
		//			this.trans.free();
		//			this.trans = this.doms.id();
		//		} else {
		//			if (TEST_MODE) {
		//				this.trans.free();
		//				this.trans = this.doms.id();
		//			}
		//			this.transList.clear();
		//			this.transList.add(this.doms.id());
		//			this.transQuantList.clear();
		//		}
	}

	/**
	 * set single trans to TRUE (restricted by variable domain constraint)
	 */
	public void resetSingleTrans() {
		this.trans.free();
		this.trans = this.doms.id();
	}

	/**
	 * frees and removes all justices
	 */
	public void resetJustice() {
		for (BDD b : justice) {
			b.free();
		}
		justice.clear();
		justiceIDs.clear();
	}

	/**
	 * calls resetInitial(); resetTrans(); resetJustice(); to free all BDDs and start from clean module with only domain
	 * restrictions
	 * 
	 */
	public void reset() {
		resetInitial();
		resetTrans();
		resetJustice();
	}

	public void free() {
		initial.free();
		trans.free();
		Env.free(justice);
		if (cachedPrimeVars != null) {
			cachedPrimeVars.free();
		}
		if (cachedUnprimeVars != null) {
			cachedUnprimeVars.free();
		}
	}

	public BDD getDoms() {
		return doms;
	}

	/**
	 * Used for returning a new module composed of current module and the given module.
	 * 
	 * Doesn't change current or given module.
	 * 
	 * @param other
	 * @return
	 */
	public PlayerModule compose(PlayerModule other) {
		PlayerModule composed = new PlayerModule();
		composed.setName(this.name + "_composed_" + other.getName());
		
		composed.doms = this.doms.and(other.doms);
		
		composed.allFields.addAll(this.allFields);
		composed.allFields.addAll(other.allFields);
		composed.auxFields.addAll(this.auxFields);
		composed.auxFields.addAll(other.auxFields);
    composed.nonAuxFields.addAll(this.nonAuxFields);
    composed.nonAuxFields.addAll(other.nonAuxFields);


		composed.conjunctInitial(this.initial().id());
		composed.conjunctInitial(other.initial().id());
		composed.conjunctTrans(this.trans().id());
		composed.conjunctTrans(other.trans().id());

		for (int i = 0; i < this.justiceNum(); i++) {
			composed.addJustice(this.justiceAt(i).id());
		}
		for (int i = 0; i < other.justiceNum(); i++) {
			composed.addJustice(other.justiceAt(i).id());
		}

		return composed;
	}

}
