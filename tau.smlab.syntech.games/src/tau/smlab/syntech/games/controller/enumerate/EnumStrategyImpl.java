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

import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Vector;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.games.AbstractGamesException;
import tau.smlab.syntech.jtlv.Env;

public class EnumStrategyImpl implements EnumStrategyI {

  // ////////////////////////////////////////////////////////////////////////
  // the instance implementation of the result.

  private Vector<EnumStateI> aut;
  private Map<AbstractRankInfo, BDD> statesOfRank;
  private Map<AbstractRankInfo, Map<BDD, EnumStateI>> allStatesOfRank;

  private long wakeup_time = -1;
  // updating the construction time every time something is changed
  // (getState, when new state needs to be created, and addNextState).
  private long construction_time = -1;
  
  private int numTransitions;
  private int lenLongestShortestPath;
  private int maxNumOutEdgesPerState;
  private int maxNumInEdgesPerState;
  private boolean calcLenLongestShortestPath;

  public EnumStrategyImpl() {
	  this(false);
  }
  
  public EnumStrategyImpl(boolean calcStats) {
    aut = new Vector<EnumStateI>();
    statesOfRank = new HashMap<AbstractRankInfo, BDD>();
    allStatesOfRank = new HashMap<>();
    wakeup_time = System.currentTimeMillis();
    numTransitions = 0;
    maxNumOutEdgesPerState = 0;
    maxNumInEdgesPerState = 0;
    calcLenLongestShortestPath = calcStats;
  }

  /**
   * looks up a state or adds it to the automaton
   * 
   * if the state is not already in the automaton it is created as an initial state
   */
  @Override
  public EnumStateI getState(BDD state, AbstractRankInfo rank) {
    EnumStateI s = lookUpState(state, rank);
    if (s != null)
      return s;
    // else need to create state
    EnumStateI res = new EnumStateImpl(this, aut.size(), true, state, rank);
    aut.add(res);
    addState(res);
    construction_time = System.currentTimeMillis() - wakeup_time;
    return res;
  }

  @Override
  public EnumStateI getState(int id) throws ArrayIndexOutOfBoundsException {
    return aut.get(id);
  }

  // when doing together we could save an exhaustive search here.
  // returns the newly created state id or -1 if no state was created
  @Override
  public EnumStateI addSuccessorState(EnumStateI from, BDD state,
      AbstractRankInfo rank) throws AbstractGamesException {
	from.incNumOutEdges();  
	this.maxNumOutEdgesPerState = Math.max(from.getNumOutEdges(), this.maxNumOutEdgesPerState);  
	  
    EnumStateI succ = lookUpState(state, rank);
    if (succ != null) {
      succ.incNumInEdges();
      this.maxNumInEdgesPerState = Math.max(succ.getNumInEdges(), this.maxNumInEdgesPerState);
      if (this.calcLenLongestShortestPath) {
    	  boolean updated = succ.updateDistFromIni(from.getDistFromIni());
    	  if (updated) {
    		  // this calculation can be expensive, which is why we only perform it when the user
    		  // explicitly states he requires it via calcLenLongestSimplePath flag
    		  LinkedList<EnumStateI> workList = new LinkedList<EnumStateI>();
    		  workList.add(succ);
    		  while (!workList.isEmpty()) {
    			  EnumStateI currSucc = workList.pop();
    			  Vector<EnumStateI> succs = currSucc.getSuccessors();
    			  for (EnumStateI elem: succs) {
    				  updated = elem.updateDistFromIni(currSucc.getDistFromIni());
    				  if (updated) {
    					  workList.add(elem);
    				  }
    			  }
    		  }
    	  }
      }
      this.lenLongestShortestPath = Math.max(succ.getDistFromIni(), this.lenLongestShortestPath);
      from.addSuccessor(succ);
      construction_time = System.currentTimeMillis() - wakeup_time;
      numTransitions++;
      return null;
    }
    // else succ not existent
    EnumStateI to = new EnumStateImpl(this, aut.size(), false, state, rank);
    aut.add(to);
    addState(to);
    to.incNumInEdges();
    this.maxNumInEdgesPerState = Math.max(to.getNumInEdges(), this.maxNumInEdgesPerState);
    if (this.calcLenLongestShortestPath) {
    	to.updateDistFromIni(from.getDistFromIni());
    	this.lenLongestShortestPath = Math.max(to.getDistFromIni(), this.lenLongestShortestPath);
    }
    
    from.addSuccessor(to);
    construction_time = System.currentTimeMillis() - wakeup_time;
    numTransitions++;
    return to;
  }

  /**
   * lookup a state we already created
   * 
   * @param state
   * @param rank
   * @return null if state is not found
   */
  private EnumStateI lookUpState(BDD state, AbstractRankInfo rank) {
    Map<BDD, EnumStateI> m = allStatesOfRank.get(rank);
    if (m != null) {
      EnumStateI s = m.get(state);
      return s;
    }
    return null;
  }

  /**
   * adds a state to statesOfRank and allStatesOfRank
   * 
   * @param s
   */
  private void addState(EnumStateI s) {
    AbstractRankInfo rank = s.get_rank_info();
    BDD sor = statesOfRank.get(rank);
    if (sor == null) {
      sor = Env.FALSE();
      statesOfRank.put(rank, sor);
    }
    sor.orWith(s.getData().id());

    Map<BDD, EnumStateI> m = allStatesOfRank.get(rank);
    if (m == null) {
      m = new HashMap<>();
      allStatesOfRank.put(rank, m);
    }
    m.put(s.getData(), s);
  }

  /**
   * for states with ReactiveRankInfo retrive all of given rank
   * 
   * @param sys_just
   * @return
   */
  public BDD getStatesOfRank(AbstractRankInfo rank) {
    if (statesOfRank.containsKey(rank)) {
      return statesOfRank.get(rank);
    }
    return Env.FALSE();
  }

  @Override
  public int numOfStates() {
    return this.aut.size();
  }

  @Override
  public long getConstructionTime() {
    return this.construction_time;
  }

  @Override
  public Vector<? extends StateI> getInitialStates() {
    Vector<StateI> ret = new Vector<StateI>();
    for (StateI st : enumController())
      if (st.isInitial())
        ret.add(st);
    return ret;
  }

  @Override
  public Vector<? extends StateI> getNextState(StateI st) {
    if (st instanceof EnumStateI)
      return ((EnumStateI) st).getSuccessors();
    return new Vector<StateI>();
  }

  @Override
  public Vector<? extends EnumStateI> enumController() {
    return aut;
  }

  @Override
  public Vector<? extends EnumStateI> enumController(StateI st) {
    return aut;
  }
  
  @Override
  public int numOfTransitions() {
	  return this.numTransitions;
  }
  
  @Override
  public int lenLongestShortestPath() {
	  return this.lenLongestShortestPath;
  }
  
  @Override
  public int maxNumOutEdgesPerState() {
	  return this.maxNumOutEdgesPerState;
  }
  
  @Override
  public int maxNumInEdgesPerState() {
	  return this.maxNumInEdgesPerState;
  }

  @Override
  public boolean isCalcStats() {
    return this.calcLenLongestShortestPath;
  }


}
