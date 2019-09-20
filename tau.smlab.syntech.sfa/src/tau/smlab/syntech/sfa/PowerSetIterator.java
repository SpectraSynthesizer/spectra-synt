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

package tau.smlab.syntech.sfa;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.sfa.SFA.Pair;

/**
 * @author Maxim Finkel
 * @author Or Pistiner
 *
 */
public class PowerSetIterator {
	
	private PowerSetIterator() {};
	
	public static enum PowerSetIteratorType {
		DEFAULT, EFFICIENT;
	}

	/**
	 * Returns an iterator that iterates over the powerset of the given set of SFA transitions. Each element in
	 * the iteration is an (ordered) Pair object whose left component is a transitions' subset {@code t} and whose right component is a satisfiable BDD {@code b} that encodes
	 * the conjunction of all transitions' guards in {@code t}. Subsets {@code t} where {@code b} is unsatisfiable (i.e., where {@code b} is the FALSE BDD), do not occur in the iteration.
	 * 
	 * @param type
	 * @param transitionsSet
	 * @return
	 */
	public static <T extends SFAState> Iterator<Pair<Set<Map.Entry<T, BDD>>, BDD>>
	getIterator(PowerSetIteratorType type, Set<Map.Entry<T, BDD>> transitionsSet) {
		if(type.equals(PowerSetIteratorType.DEFAULT)) {
			return new DefaultPowerSetIterator<T>(transitionsSet);
		}
		if(type.equals(PowerSetIteratorType.EFFICIENT)) {
			return new EfficientPowerSetIterator<T>(transitionsSet);
		}
		return null;
	}

	/**
	 * A class for constructing a lazy powerset iterator over sets of SFA
	 * transitions. The iterator uses a binary counter to keep track of the subsets' enumeration.
	 * The iterator only generates transitions' subsets whose transition formulas'
	 * conjunction is satisfiable.
	 */
	private static class DefaultPowerSetIterator<T extends SFAState> implements Iterator<Pair<Set<Map.Entry<T, BDD>>, BDD>>{

		private List<Map.Entry<T, BDD>> transitionsSetList;

		private boolean[] nextSubsetBinaryList;
		private Set<Map.Entry<T, BDD>> nextSubset = new HashSet<>();
		private BDD nextSubsetBDD, nextNextSubsetBDD;

		private Set<Map.Entry<T, BDD>> nextNextSubset = new HashSet<>();

		DefaultPowerSetIterator(Set<Map.Entry<T, BDD>> transitionsSet) {
			this.transitionsSetList = new ArrayList<>(transitionsSet);
			this.nextSubsetBinaryList = new boolean[transitionsSet.size()];
			this.nextNextSubsetBDD = Env.TRUE();
		}

		@Override
		public boolean hasNext() {
			if (this.nextNextSubset == null) {
				return false;
			}
			return true;
		}

		@Override
		public Pair<Set<Map.Entry<T, BDD>>, BDD> next() {
			if (!this.hasNext()) {
				return null;
			}

			this.nextSubset.clear();
			this.nextSubset.addAll(this.nextNextSubset);
			this.nextSubsetBDD = this.nextNextSubsetBDD;

			do {
				for (int i = 0; i < this.nextSubsetBinaryList.length; ++i) {
					this.nextSubsetBinaryList[i] = !this.nextSubsetBinaryList[i];
					if (this.nextSubsetBinaryList[i]) {
						break;
					}
				}

				this.nextNextSubset.clear();
				if(this.nextNextSubsetBDD.isZero()) {
					this.nextNextSubsetBDD.free();
				}
				this.nextNextSubsetBDD = Env.TRUE();
				for (int i = 0; i < this.transitionsSetList.size(); ++i) {
					if (this.nextSubsetBinaryList[i]) {
						this.nextNextSubset.add(this.transitionsSetList.get(i));
						this.nextNextSubsetBDD.andWith(this.transitionsSetList.get(i).getValue().id());
						if(this.nextNextSubsetBDD.isZero()) {
							break;
						}
					}
				}
				if (this.nextNextSubset.isEmpty()) {
					this.nextNextSubset = null;
					this.nextNextSubsetBDD.free();
					break;
				}
			} while(this.nextNextSubsetBDD.isZero());

			return new Pair<Set<Map.Entry<T, BDD>>, BDD>(this.nextSubset, this.nextSubsetBDD);
		}
	}

	/**
	 * A class for constructing a lazy powerset iterator over sets of SFA
	 * transitions. The iterator only generates transitions' subsets whose transition formulas'
	 * conjunction is satisfiable. The iterator generates transitions' subsets with a non-decreasing cardinality. In addition, it applies pruning as an optimization. That is,
	 * it does not look at proper supersets of a set which has already been explored and the conjunction of its transitions' formulas is unsatisfiable.
	 *  
	 */
	private static class EfficientPowerSetIterator<T extends SFAState> implements Iterator<Pair<Set<Map.Entry<T, BDD>>, BDD>> {

		private List<Map.Entry<T, BDD>> transitionsSetList;
		private int nextTransitionsSetListIdx = 0;

		private Set<Map.Entry<T, BDD>> nextSubset = new HashSet<>(), nextNextSubset = new HashSet<>();

		private BDD nextSubsetBDD, nextNextSubsetBDD;

		private int currPrevSizeSubsetIdx = 0; //the current index of the subset (of size N) that we try to extend with an additional element
		private List<List<Integer>> prevSizeSubsetsIndices = new ArrayList<>(); //each list contains the transitions' indices in each subset of size N
		private List<List<Integer>> currSizeSubsetsIndices = new ArrayList<>(); //each list contains the transitions' indices in each subset of size N+1


		private List<BDD> prevSizeSubsetsBDDs = new ArrayList<>(); //each BDD is the conjunction of all transitions in the corresponding subset of size N
		private List<BDD> currSizeSubsetsBDDs = new ArrayList<>(); //each BDD is the conjunction of all transitions in the corresponding subset of size N+1

		EfficientPowerSetIterator(Set<Map.Entry<T, BDD>> transitionsSet) {
			this.transitionsSetList = new ArrayList<>(transitionsSet);

			this.prevSizeSubsetsIndices.add(new ArrayList<>());

			this.nextNextSubsetBDD = Env.TRUE();
			this.prevSizeSubsetsBDDs.add(Env.TRUE());

		}

		/*
		 * This method is basically a flat (iterative) version of a recursive method
		 * that generates lazily all SATISFIABLE transition subsets of growing size, and
		 * their formulas' BDD conjunction. Once there are no satisfiable subsets left,
		 * null is returned.
		 * 
		 */
		@Override
		public Pair<Set<Map.Entry<T, BDD>>, BDD> next() {
			if(!this.hasNext()) {
				return null;
			}

			this.nextSubset.clear();
			this.nextSubset.addAll(this.nextNextSubset);
			this.nextSubsetBDD = this.nextNextSubsetBDD;
			Pair<Set<Map.Entry<T, BDD>>, BDD> nextPair = new Pair<>(this.nextSubset, this.nextSubsetBDD);

			List<Integer> nextNextSubsetIndices, currPrevSizeSubsetIndices;


			while(true) {
				while (this.nextTransitionsSetListIdx < this.transitionsSetList.size()) {
					this.nextNextSubsetBDD = this.prevSizeSubsetsBDDs.get(this.currPrevSizeSubsetIdx)
							.and(this.transitionsSetList.get(this.nextTransitionsSetListIdx).getValue());

					if(this.nextNextSubsetBDD.isZero()) {
						this.nextNextSubsetBDD.free();
						this.nextTransitionsSetListIdx++;
					}
					else {
						//the BDD of the next next subset is not FALSE, so keep it to consider (in the future) proper supersets thereof
						this.currSizeSubsetsBDDs.add(this.nextNextSubsetBDD.id());

						//create the list of the transitions' indices which are in the next next transitions subset
						nextNextSubsetIndices = new ArrayList<>();
						nextNextSubsetIndices.addAll(this.prevSizeSubsetsIndices.get(this.currPrevSizeSubsetIdx));
						nextNextSubsetIndices.add(this.nextTransitionsSetListIdx);
						this.currSizeSubsetsIndices.add(nextNextSubsetIndices);

						//increment the index of the transition which should be added next to the transitions subset 
						this.nextTransitionsSetListIdx++;

						//compute the next next transitions subset (nextNextSubset) from its indices list (nextNextSubsetIndices)
						this.nextNextSubset.clear();
						for(int transIdx : nextNextSubsetIndices) {
							this.nextNextSubset.add(this.transitionsSetList.get(transIdx));
						}
						return nextPair;
					}
				}

				/*
				 * 
				 * At this point, we have created all possible (satisfiable) supersets (of size N+1) of the current transitions subset of size N
				 * (which in this.prevSizeSubsetsIndices[this.currPrevSizeSubsetIdx]). Thus, we proceed to this.currPrevSizeSubsetIdx+1.
				 * If that's impossible, i.e., (this.currPrevSizeSubsetIdx+1) == this.prevSizeSubsetsIndices.size(), we clear and insert to this.prevSizeSubsetsIndices the indices
				 * lists in this.currSizeSubsetsIndices.
				 * 
				 * */

				this.currPrevSizeSubsetIdx++;
				if(this.currPrevSizeSubsetIdx == this.prevSizeSubsetsIndices.size()) {
					Env.free(this.prevSizeSubsetsBDDs); //free all previously stored BDDs
					this.prevSizeSubsetsBDDs.clear(); //remove all previously stored BDD objects
					this.prevSizeSubsetsIndices.clear(); //remove all previously stored transitions subsets' indices lists 

					if(this.currSizeSubsetsIndices.isEmpty()) { //there are no transitions' indices lists whose supersets should be generated next -> we are done!
						this.setIsExhausted(); //mark that the iterator is exhausted
						break;
					}
					
					//we have that this.currSizeSubsetsIndices is not empty
					
					this.prevSizeSubsetsIndices.addAll(this.currSizeSubsetsIndices); //insert to this.prevSizeSubsetsIndices all the indices lists in this.currSizeSubsetsIndices
					this.prevSizeSubsetsBDDs.addAll(this.currSizeSubsetsBDDs); //insert to this.prevSizeSubsetsBDDs all the BDDs in this.currSizeSubsetsBDDs
					this.currPrevSizeSubsetIdx = 0; //reset the index of the subset that we try to extend with an additional element 

					//clear current subsets indices and BDDs
					this.currSizeSubsetsIndices.clear();
					this.currSizeSubsetsBDDs.clear();
				}

				//set this.nextTransitionsSetListIdx according to the current transitions' indices subset that we try to extend
				//the next value of this.nextTransitionsSetListIdx should be equal to [maximal (last) index element in this.prevSizeSubsetsIndices.get(this.currPrevSizeSubsetIdx)] + 1
				currPrevSizeSubsetIndices = this.prevSizeSubsetsIndices.get(this.currPrevSizeSubsetIdx);
				this.nextTransitionsSetListIdx = currPrevSizeSubsetIndices.get(currPrevSizeSubsetIndices.size()-1)+1;
			}
			return nextPair;
		}

		@Override
		public boolean hasNext() {
			if (this.nextNextSubset == null) {
				return false;
			}
			return true;
		}

		private void setIsExhausted() {
			this.nextNextSubset = null;
		}

	}





}
