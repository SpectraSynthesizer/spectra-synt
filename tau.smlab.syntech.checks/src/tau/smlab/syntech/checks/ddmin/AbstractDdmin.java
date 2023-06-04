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

package tau.smlab.syntech.checks.ddmin;

import java.util.ArrayList;
import java.util.List;

public abstract class AbstractDdmin<T> extends MonotonicCheckMinimizer<T> {

	static public boolean USE_NEGATIVE_SETS_LIST = true;
  /**
   * recursively minimize the set elements to a local minimum that still passes the check
   * 
   * requires monotonicity of check, i.e., supersets will give same result
   * 
   * @param elements
   * @param n
   * @return
   * @throws AbstractGamesException
   */
  @Override
  public List<T> minimize(List<T> elements) {
    return ddminDo(elements, 2, new ArrayList<List<T>>());
  }

  public List<T> minimize(List<T> elements, List<List<T>> negSets) {
    return ddminDo(elements, 2, negSets);
  }
  
  /**
   * recursively minimize the set elements to a local minimum that still passes the check
   * 
   * requires monotonicity of check, i.e., supersets will give same result
   * 
   * @param elements
   * @param n
   * @return
   * @throws AbstractGamesException
   */
  private List<T> ddminDo(List<T> elements, int n, List<List<T>> negSets) {
    List<T> min = ddmin(elements, n, negSets);
    // if input size equals output size check property to be sure
    if (min.size() == elements.size()) {
      if (!check(min)) {
        return new ArrayList<>();
      }
    }
    return min;
  }

  protected List<T> ddmin(List<T> elements, int n, List<List<T>> negSets) {

    int numElem = elements.size();
    int subSize = numElem / n;
    // sets need to be larger otherwise we miss some elements
    if (subSize * n < numElem) {
      subSize++;
    }

    if (numElem == 1 || n < 2) {
      if (numElem == 1 && check(new ArrayList<T>())) {
        return new ArrayList<T>();
      }
      return elements;
    }

    // First Case: If there exists a part that in unrealizable, recursively continue with that part
    for (int i = 0; (i * subSize) < numElem; i++) {

      ArrayList<T> part = new ArrayList<T>(elements.subList(i * subSize, Math.min((i + 1) * subSize, numElem)));
      System.out.println("Checking: " + i * subSize + ":" + Math.min((i + 1) * subSize, numElem) + ", " + part.size() + " elements");
      if (check(part, negSets)) {
        List<T> remainder = new ArrayList<>(elements);
        remainder.removeAll(part);
        dispose(remainder);
        return ddmin(part, 2, negSets);
      }
    }

    // Second Case: check complements of parts
    if (n != 2) {
      for (int i = 0; (i * subSize) < numElem; i++) {

        ArrayList<T> part = new ArrayList<T>(elements.subList(0, i * subSize));
        part.addAll(elements.subList(Math.min((i + 1) * subSize, numElem), numElem));
        System.out.println("Checking: 0:" + i * subSize + " and " + Math.min((i + 1) * subSize, numElem) + ":" + numElem 
                  + ", " + part.size() + " elements");
        if (check(part, negSets)) {
          List<T> remainder = new ArrayList<>(elements);
          remainder.removeAll(part);
          dispose(remainder);
          return ddmin(part, n - 1, negSets);
        }
      }
    }

    // Third Case: increase granularity and check for smaller subsets
    if (n < elements.size()) {
      return ddmin(elements, Math.min(numElem, 2 * n), negSets);
    }

    return elements;
  }

  /**
   * elements that will not be part of the core
   * @param elements
   */
  protected void dispose(List<T> elements) {
  }

  /**
   * check that tries to find a positively checked subset of part in checkedSets
   * 
   * @param part
   * @param posSets
   *          list of sets that satisfy criterion
   * @return true if part or any of its subsets satisfies the criterion
   */
  protected boolean check(List<T> part, List<List<T>> negSets) {

  	int setNum = 1; 
    for (List<T> supset : negSets) {
      if (supset.containsAll(part)) {
      	System.out.println("check: return false - part is in neg set num " + setNum);
        return false;
      }
      setNum++;
    }

    if (check(part)) {
      return true;
    }

    if (USE_NEGATIVE_SETS_LIST)
    	negSets.add(part);
    
    return false;
  }
}
