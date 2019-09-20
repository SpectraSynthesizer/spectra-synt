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

public interface EnumStrategyI extends StrategyI {

  /**
   * If a new state needs to be created then it is an initial.
   * 
   * @param state
   * @param just
   * @return
   * @throws AbstractGamesException
   */
  public EnumStateI getState(BDD state, AbstractRankInfo just)
      throws AbstractGamesException;

  public EnumStateI getState(int id) throws ArrayIndexOutOfBoundsException;

  /**
   * Add a transition in the strategy from "from" to the new values. If these values don't have a state, then it is
   * created and returned, otherwise is an existing matching state was found returning null value.
   * 
   * @param from
   * @param state
   * @param just
   * @return
   * @throws AbstractGamesException
   */
  public EnumStateI addSuccessorState(EnumStateI from, BDD state,
      AbstractRankInfo just) throws AbstractGamesException;

  // refined API from StateI (for convenience)
  public Vector<? extends EnumStateI> enumController();

  public Vector<? extends EnumStateI> enumController(StateI st);

  // statistics
  public int numOfTransitions();

  public int lenLongestShortestPath();

  public int maxNumOutEdgesPerState();

  public int maxNumInEdgesPerState();

  public boolean isCalcStats();
}
