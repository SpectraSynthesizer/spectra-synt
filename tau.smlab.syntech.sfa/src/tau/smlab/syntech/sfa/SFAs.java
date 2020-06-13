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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.sfa.SFA.Pair;
import tau.smlab.syntech.sfa.SFA.SFAException;
import tau.smlab.syntech.sfa.SFAState.SFAStateException;

/**
 * 
 * A class that exclusively consists of static methods for creating SFAs of all sorts and states thereof.
 * 
 * @author Or Pistiner
 *
 */
public class SFAs {

	private SFAs() {};

	/**
	 * Returns a new instance either of a "simple" SFA or of an Epsilon-SFA,
	 * whose initial state attribute is initialized to null. 
	 * 
	 * @param enablesEpsTrans whether the new SFA instance should enable epsilon transitions.
	 * @return
	 */
	public static SFA newSfa(boolean enablesEpsTrans) {
		if(enablesEpsTrans) {
			return new EpsSFA();
		}
		return new SimpleSFA();
	}

	/**
	 * Returns a new instance either of a "simple" SFA or of an Epsilon-SFA,
	 * whose initial state is {@code ini}.
	 * 
	 * @param enablesEpsTrans whether the new SFA instance should enable epsilon transitions, i.e., whether
	 * it should be an Epsilon-SFA.
	 * @param ini the initial state of the new SFA.
	 * @throws SFAException if {@code (ini.enablesEpsTrans() != enablesEpsTrans)}
	 * @return
	 */
	public static SFA newSfa(boolean enablesEpsTrans, SFAState ini) {
		if(enablesEpsTrans) {
			return new EpsSFA(ini);
		}
		return new SimpleSFA(ini);
	}

	/**
	 * Returns a new instance of either a "simple" SFA state or an Epsilon-SFA state. 
	 * 
	 * @param enablesEpsTrans whether the new SFA state should enable outgoing epsilon transitions, i.e., whether it should be a state of an Epsilon-SFA.
	 * @param isAccepting whether the new SFA state should be marked as final.
	 * @return
	 */
	public static SFAState newSfaState(boolean enablesEpsTrans, boolean isAccepting) {
		if(enablesEpsTrans) {
			return new EpsSFAState(isAccepting);
		}
		return new SimpleSFAState(isAccepting);
	}

	/**
	 * Returns a new instance of a "simple" SFA state. This type of state does not enable outgoing epsilon transitions
	 * and it is used by "simple" SFAs.
	 * 
	 * @param isAccepting whether the new SFA state should be marked as final.
	 * @return
	 */
	public static SFAState newSimpleSfaState(boolean isAccepting) {
		return new SimpleSFAState(isAccepting);
	}

	/**
	 * Returns a new instance of an Epsilon-SFA state. This type of state enables outgoing epsilon transitions
	 * and it is used by Epsilon-SFAs.
	 * 
	 * @param isAccepting whether the new SFA state should be marked as final.
	 * @return
	 */
	public static SFAState newEpsSfaState(boolean isAccepting) {
		return new EpsSFAState(isAccepting);
	}

	/**
	 * Returns a new instance of a "simple" SFA. This type of SFA does not enable epsilon transitions.
	 * The initial state attribute is initialized to null.
	 * 
	 * @return
	 */
	public static SFA newSimpleSfa() {
		return new SimpleSFA();
	}

	/**
	 * Returns a new instance of a "simple" SFA. This type of SFA does not enable epsilon transitions.
	 * The initial state of the new SFA is set to {@code ini}.
	 * 
	 * @param ini the initial state of the new "simple" SFA.
	 * @throws SFAException if {@code ini.enablesEpsTrans()}
	 * @return
	 */
	public static SFA newSimpleSfa(SFAState ini) {
		return new SimpleSFA(ini);
	}

	/**
	 * Returns a new instance of an Epsilon-SFA. That is, an SFA that enables epsilon transitions.
	 * The initial state attribute is initialized to null.
	 * 
	 * @return
	 */
	public static SFA newEpsSfa() {
		return new EpsSFA();
	}

	/**
	 * Returns a new instance of an Epsilon-SFA. That is, an SFA that enables epsilon transitions.
	 * The initial state of the new SFA is set to {@code ini}.
	 * 
	 * @param ini the initial state of the new Epsilon-SFA.
	 * @throws SFAException if {@code !ini.enablesEpsTrans()}
	 * @return
	 */
	public static SFA newEpsSfa(SFAState ini) {
		return new EpsSFA(ini);
	}

	/**
	 * Returns a new instance of an Epsilon-SFA that has exactly One Final state.
	 * This special type of Epsilon-SFA is called OFSFA.
	 * The returned OFSFA has initial state and final state attributes initialized to null.
	 * 
	 * @return
	 */
	public static SFA newOFSfa() {
		return new OFEpsSFA();
	}

	/**
	 * Returns a new instance of an Epsilon-SFA that has exactly One Final state.
	 * This special type of Epsilon-SFA is called OFSFA.
	 * The returned OFSFA has an initial state set to {@code ini} and a final state attribute initialized to null.
	 * 
	 * @param ini the initial state of the new OFSFA.
	 * @return
	 */
	public static SFA newOFSfa(SFAState ini) {
		return new OFEpsSFA(ini);
	}

	/**
	 * Returns a new instance of an Epsilon-SFA that has exactly One Final state.
	 * This special type of Epsilon-SFA is called OFSFA.
	 * The returned OFSFA has an initial state set to {@code ini} and a final state set to {@code finalState}.
	 * 
	 * @param ini the initial state of the new OFSFA.
	 * @param finalState the unique final state of the new OFSFA.
	 * @return
	 */
	public static SFA newOFSfa(SFAState ini, SFAState finalState) {
		return new OFEpsSFA(ini, finalState);
	}

	/**
	 * Returns a "simple" SFA that accepts the empty language.
	 * This SFA has a single (initial) state that is not final and a self loop labeled TRUE.
	 * 
	 * @return
	 */
	public static SFA emptyLanguageSimpleSfa() {
		SFAState initialState = new SimpleSFAState(false);
		initialState.addTrans(Env.TRUE(), initialState);
		return new SimpleSFA(initialState);
	}

	/**
	 * Returns an Epsilon-SFA that accepts the empty language.
	 * This Epsilon-SFA has a single (initial) state that is not final and a self loop labeled TRUE.
	 * 
	 * @return
	 */
	public static SFA emptyLanguageEpsSfa() {
		SFAState initialState = new EpsSFAState(false);
		initialState.addTrans(Env.TRUE(), initialState);
		return new EpsSFA(initialState);
	}

	/**
	 * Returns a "simple" SFA that accepts language of TRUE*.
	 * This SFA has a single (initial) state that is final (accepting) and a self loop labeled TRUE.
	 * 
	 * @return
	 */
	public static SFA trueStarSimpleSfa() {
		SFAState initialState = new SimpleSFAState(true);
		initialState.addTrans(Env.TRUE(), initialState);
		return new SimpleSFA(initialState);
	}

	/**
	 * Using a product construction which follows Veanes et al. (2010), this method returns a new SFA whose language is the intersection of the languages
	 * of the two specified SFAs. If either of the specified SFAs enables epsilon transition, then 
	 * the returned product SFA is an Epsilon-SFA; otherwise, it is a "simple" SFA, which does not enable epsilon transitions.
	 * 
	 * @param sfaA either a "simple" SFA or an epsilon-SFA
	 * @param sfaB either a "simple" SFA or an epsilon-SFA
	 * @return
	 */
	public static SFA productSfa(SFA sfaA, SFA sfaB) {
		/*
		 * Check if either of the given SFAs enables epsilon transitions; if indeed that holds, then the
		 * product SFA that we are about to construct should have states that allow outgoing epsilon transitions.
		 * 
		 */
		boolean productIsEpsSfa = sfaA.enablesEpsTrans() || sfaB.enablesEpsTrans();
		/*
		 * Create a mapping between pairs of states from sfaA and sfaB, resp., to the new states in the
		 * new product SFA that we build.
		 * 
		 */
		Map<Pair<SFAState, SFAState>, SFAState> statesPairsToProductAutomatonStatesMapping = new HashMap<>();
		Pair<SFAState, SFAState> initialStatesPair = new Pair<>(sfaA.getIni(), sfaB.getIni());
		/*
		 * The initial state in the product SFA is a pair whose components are the initial states of sfaA and sfaB.
		 */
		SFAState productAutomatonInitialState = newSfaState(productIsEpsSfa, sfaA.getIni().isAccepting() && sfaB.getIni().isAccepting());
		statesPairsToProductAutomatonStatesMapping.put(initialStatesPair, productAutomatonInitialState);
		
		buildProductSfa(productIsEpsSfa, statesPairsToProductAutomatonStatesMapping, initialStatesPair);
		
		SFA productAutomaton = newSfa(productIsEpsSfa, productAutomatonInitialState);

		// remove states that cannot reach final states
		productAutomaton.removeDeadStates();

		return productAutomaton;
	}
	
	/*
	 * 
	 * 
	 * 
	 * One Final Epsilon-SFAs (OFSFAs) inductive construction operations.
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 */

	/**
	 * Returns an OFSFA that accepts the empty language. This OFSFA has an (initial) state that is not final and one unreachable final state.
	 * 
	 * @return
	 */
	public static SFA emptyLanguageOFSfa() {
		return newOFSfa(newEpsSfaState(false), newEpsSfaState(true));
	}

	/**
	 * Returns an OFSFA that accepts language of TRUE*, i.e., the language of all finite words over the alphabet.
	 * 
	 * @return
	 */
	public static SFA trueStarOFSfa() {
		SFAState initialState = newEpsSfaState(false);
		SFAState middleState = newEpsSfaState(false);
		initialState.addEpsTrans(middleState);
		middleState.addTrans(Env.TRUE(), middleState);
		SFAState finalState = newEpsSfaState(true);
		middleState.addEpsTrans(finalState);
		return newOFSfa(initialState, finalState);
	}

	/**
	 * Returns an OFSFA that accepts the language that solely consists of the empty word.
	 * 
	 * @return
	 */
	public static SFA emptyWordOFSfa() {
		SFAState initialState = newEpsSfaState(true);
		return newOFSfa(initialState, initialState);
	}

	/**
	 * Returns an OFSFA that accepts the language of all assignments (i.e., single characters) which satisfy the specified predicate.
	 * 
	 * @param predicate is consumed (freed) and cannot be used afterwards
	 * @return
	 */
	public static SFA predicateOFSfa(BDD predicate) {
		SFAState initialState = newEpsSfaState(false);
		SFAState finalState = newEpsSfaState(true);
		initialState.addTrans(predicate, finalState); //Note: if the specified predicate is unsatisfiable, we return an OFSFA that accepts the empty language
		return newOFSfa(initialState, finalState);
	}
	

	/**
	 * 
	 * Returns an OFSFA whose language is the union of the languages of the two specified predicate OFSFAs.
	 * Note that (1) the returned OFSFA is backed by the two given OFSFAs. That is, changes to either {@code oFSfaA} or {@code oFSfaB}
	 * are also reflected in the returned OFSFA, and vice-versa. (2) The two given OFSFAs are modified and cannot be used anymore.
	 * 
	 * @param predOFSfaA predicate OFSFA constructed using {@link #predicateOFSfa}
	 * @param predOFSfaB predicate OFSFA constructed using {@link #predicateOFSfa}
	 * @return
	 */
	public static SFA unionPredicateOFSfa(SFA predOFSfaA, SFA predOFSfaB) {
		if(predOFSfaA.isEmptyLanguage()) {
			return predOFSfaB;
		}
		if(predOFSfaB.isEmptyLanguage()) {
			return predOFSfaA;
		}
		
		predOFSfaA.getIni().addTrans(predOFSfaB.getIni().getSucc().get(predOFSfaB.getFinalState()), predOFSfaA.getFinalState());
	
		return predOFSfaA;
	}
	
	/**
	 * 
	 * Returns an OFSFA whose language is the intersection of the languages of the two specified predicate OFSFAs.
	 * Note that (1) the returned OFSFA is backed by the two given OFSFAs. That is, changes to either {@code oFSfaA} or {@code oFSfaB}
	 * are also reflected in the returned OFSFA, and vice-versa. (2) The two given OFSFAs are consumed (freed) and cannot be used anymore.
	 * 
	 * @param predOFSfaA predicate OFSFA constructed using {@link #predicateOFSfa}
	 * @param predOFSfaB predicate OFSFA constructed using {@link #predicateOFSfa}
	 * @return
	 */
	public static SFA productPredicateOFSfa(SFA predOFSfaA, SFA predOFSfaB) {
		if(predOFSfaA.isEmptyLanguage() || predOFSfaB.isEmptyLanguage()) {
			return emptyLanguageOFSfa();
		}
		
		BDD productGuard = predOFSfaA.getIni().getSucc().get(predOFSfaA.getFinalState())
				.and(predOFSfaB.getIni().getSucc().get(predOFSfaB.getFinalState()));
		
		predOFSfaA.free();
		predOFSfaB.free();
		
		return SFAs.predicateOFSfa(productGuard);
	}

	/**
	 * 
	 * Returns an OFSFA whose language is the union of the languages of the two specified OFSFAs.
	 * Note that (1) the returned OFSFA is backed by the two given OFSFAs. That is, changes to either {@code oFSfaA} or {@code oFSfaB}
	 * are also reflected in the returned OFSFA, and vice-versa. (2) The two given OFSFAs are modified.
	 * 
	 * @param oFSfaA
	 * @param oFSfaB
	 * @throws SFAException if either of the two given SFAs is not an OFSFA.
	 * @throws SFAException if the initial or final state attribute of either of the two given OFSFAs is set to null.
	 * @throws SFAStateException if either of the two given SFAs is not an OFSFA.
	 * @return
	 */
	public static SFA unionOFSfa(SFA oFSfaA, SFA oFSfaB) {
		assertOFSfaFieldsNotNull(oFSfaA, oFSfaB);

		SFAState initialState = newEpsSfaState(false);
		initialState.addEpsTrans(oFSfaA.getIni());
		initialState.addEpsTrans(oFSfaB.getIni());
		SFAState finalState = newEpsSfaState(true);
		oFSfaA.getFinalState().setAcceptance(false);
		oFSfaB.getFinalState().setAcceptance(false);
		oFSfaA.getFinalState().addEpsTrans(finalState);
		oFSfaB.getFinalState().addEpsTrans(finalState);
		return newOFSfa(initialState, finalState);
	}

	/**
	 * Returns an OFSFA whose language is the concatenation of the languages of the two specified OFSFAs,
	 * namely {@code L(oFSfaA) L(oFSfaB)}. Note that (1) the returned OFSFA is backed by the two given OFSFAs. That is, changes to either {@code oFSfaA} or {@code oFSfaB}
	 * are also reflected in the returned OFSFA, and vice-versa. (2) The two given OFSFAs, {@code oFSfaA} and {@code oFSfaB}, are modified.
	 * 
	 * 
	 * @param oFSfaA
	 * @param oFSfaB
	 * @throws SFAException if either of the two given SFAs is not an OFSFA.
	 * @throws SFAException if the initial or final state attribute of either of the two given OFSFAs is set to null.
	 * @return
	 */
	public static SFA concatOFSfa(SFA oFSfaA, SFA oFSfaB) {
		assertOFSfaFieldsNotNull(oFSfaA, oFSfaB);

		oFSfaA.getFinalState().setAcceptance(false);
		oFSfaA.getFinalState().addEpsTrans(oFSfaB.getIni());
		return newOFSfa(oFSfaA.getIni(), oFSfaB.getFinalState());
	}

	/**
	 * Returns an OFSFA whose language is the Kleene closure of the language of the specified OFSFA.
	 * Note that (1) the returned OFSFA is backed by {@code oFSfa}. That is, changes to {@code oFSfa}
	 * are also reflected in the returned OFSFA, and vice-versa. (2) {@code oFSfa} is modified.
	 * 
	 * @param oFSfa
	 * @throws SFAException if the given SFA is not an OFSFA.
	 * @throws SFAException if the initial or final state attribute of the given OFSFA is set to null.
	 * @throws SFAStateException if the given SFA is not an OFSFA.
	 * @return
	 */
	public static SFA kleeneClosureOFSfa(SFA oFSfa) {
		assertOFSfaFieldsNotNull(oFSfa);

		oFSfa.getFinalState().setAcceptance(false);
		oFSfa.getFinalState().addEpsTrans(oFSfa.getIni());
		SFAState initialState = newEpsSfaState(false);
		initialState.addEpsTrans(oFSfa.getIni());
		SFAState finalState = newEpsSfaState(true);
		initialState.addEpsTrans(finalState);
		oFSfa.getFinalState().addEpsTrans(finalState);
		return newOFSfa(initialState, finalState);
	}

	/**
	 * 
	 * Returns an OFSFA whose language is the Kleene closure of the language of the specified OFSFA, exclusive of the empty word.
	 * Note that (1) the returned OFSFA is backed by {@code oFSfa}. That is, changes to {@code oFSfa}
	 * are also reflected in the returned OFSFA, and vice-versa. (2) {@code oFSfa} is modified.
	 * 
	 * @param oFSfa
	 * @return
	 */
	public static SFA oneOrMoreOFSfa(SFA oFSfa) {
		assertOFSfaFieldsNotNull(oFSfa);

		oFSfa.getFinalState().setAcceptance(false);
		oFSfa.getFinalState().addEpsTrans(oFSfa.getIni());
		SFAState initialState = newEpsSfaState(false);
		initialState.addEpsTrans(oFSfa.getIni());
		SFAState finalState = newEpsSfaState(true);
		oFSfa.getFinalState().addEpsTrans(finalState);
		return newOFSfa(initialState, finalState);
	}

	/**
	 * Returns an OFSFA whose language consists of all words which have at least {@code n} iterations of words
	 * in the language of {@code oFSfa}. Note that (1) the returned OFSFA is backed by {@code oFSfa}. That is, changes to {@code oFSfa}
	 * are also reflected in the returned OFSFA, and vice-versa. (2) {@code oFSfa} is modified.
	 * 
	 * @param oFSfa
	 * @param n the least number of occurrences
	 * @throws SFAException if n is negative
	 * @return
	 */
	public static SFA nOrMoreOFSfa(SFA oFSfa, int n) {
		if(n < 0) {
			throw new SFAException("The specified number of occurrences must be"
					+ "non-negative. The invalid number " + n + " has been given.");
		}
		
		//n >= 0

		if(n == 0) {
			// 0 or more reduces to the (classical) Kleene closure
			return kleeneClosureOFSfa(oFSfa);
		}

		//n > 1

		//First, build an automaton that accepts all words that are n iterations of words in the language of oFSfa
		SFA nOrMore = concatNTimesOFSfa(oFSfa, n);
		
		//Second, concatenate the Kleene closure automaton of oFSfa so the resulting automaton
		//would accept all words that are AT LEAST n iterations of words in the language of oFSfa
		SFA zeroOrMore = kleeneClosureOFSfa(oFSfa);
		nOrMore.getFinalState().setAcceptance(false);
		nOrMore.getFinalState().addEpsTrans(zeroOrMore.getIni());
		nOrMore.setFinalState(zeroOrMore.getFinalState());

		return nOrMore;
	}

	/**
	 * Returns an OFSFA whose language consists of all words which are exactly {@code n} occurrences of words,
	 * each of which is in the language of {@code oFSfa}. The transitions' BDDs of {@code oFSfa} are consumed (freed).
	 * 
	 * @param oFSfa its transitions' BDDs are consumed
	 * @param n the exact number of occurrences
	 * @throws SFAException if n is negative
	 * @return
	 */
	public static SFA exactNOFSfa(SFA oFSfa, int n) {
		if(n < 0) {
			throw new SFAException("The specified number of occurrences must be"
					+ "non-negative. The invalid number " + n + " has been given.");
		}

		//n >= 0

		SFA exactNOFSfa;
		if(n == 0) {
			// 0 reduces to language of the empty word
			exactNOFSfa = emptyWordOFSfa();
		}
		else {
			//n > 0
			exactNOFSfa = concatNTimesOFSfa(oFSfa, n);
		}
		oFSfa.free();
		return exactNOFSfa;
	}

	/**
	 * 
	 * Returns an OFSFA whose language consists of all words which are at least {@code n} and at most {@code m} occurrences of words,
	 * each of which is in the language of {@code oFSfa}. The transitions' BDDs of {@code oFSfa} are consumed (freed).
	 * 
	 * @param oFSfa
	 * @param n the least number of repetitions
	 * @param m the maximal number of repetitions
	 * @return
	 */
	public static SFA nToMOFSfa(SFA oFSfa, int n, int m) {
		if(m < n || n < 0) {
			throw new SFAException("An invalid range of {" + n + "," + m +"} has been given.");
		}
		//m >= n >=0
		
		if(n == m) {
			//This case reduces to exactly n repetitions 
			return exactNOFSfa(oFSfa, n);
		}
		
		//m > n
		SFA nOccOFSfa;
		if(n == 0) {
			nOccOFSfa = emptyWordOFSfa();
		}
		else { //n > 0
			nOccOFSfa = concatNTimesOFSfa(oFSfa, n);			
		}
		
		int extraMaxOccNum = m-n; //This is the maximal number of repetitions allowed beyond n
		
		nOccOFSfa.getFinalState().setAcceptance(false);
		SFAState finalState = newEpsSfaState(true);
		nOccOFSfa.getFinalState().addEpsTrans(finalState);
		
		SFA atMostExtraMaxOcc = oFSfa.copy(), currOFSfaCopy;
		atMostExtraMaxOcc.getFinalState().setAcceptance(false);
		atMostExtraMaxOcc.getFinalState().addEpsTrans(finalState);
		for(int currCopyIdx = extraMaxOccNum-1; currCopyIdx > 0 ; currCopyIdx--) {
			currOFSfaCopy = oFSfa.copy();
			currOFSfaCopy.getFinalState().setAcceptance(false);
			currOFSfaCopy.getFinalState().addEpsTrans(finalState);
			currOFSfaCopy.getFinalState().addEpsTrans(atMostExtraMaxOcc.getIni());
			atMostExtraMaxOcc.setIni(currOFSfaCopy.getIni());
		}
		
		nOccOFSfa.getFinalState().addEpsTrans(atMostExtraMaxOcc.getIni());
		
		oFSfa.free();
		return newOFSfa(nOccOFSfa.getIni(), finalState);
	}


	/**
	 * 
	 * Returns an OFSFA whose language consists of all words in the language of the specified OFSFA together with the empty word.
	 * Note that (1) the returned OFSFA is backed by {@code oFSfa}. That is, changes to {@code oFSfa}
	 * are also reflected in the returned OFSFA, and vice-versa. (2) The given OFSFA, {@code oFSfa}, is modified.
	 * 
	 * @param oFSfa
	 * @return
	 */
	public static SFA zeroOrOneOFSfa(SFA oFSfa) {
		assertOFSfaFieldsNotNull(oFSfa);

		oFSfa.getFinalState().setAcceptance(false);
		SFAState initialState = newEpsSfaState(false);
		initialState.addEpsTrans(oFSfa.getIni());
		SFAState finalState = newEpsSfaState(true);
		initialState.addEpsTrans(finalState);
		oFSfa.getFinalState().addEpsTrans(finalState);
		return newOFSfa(initialState, finalState);
	}
	
	/**
	 * Returns an OFSFA whose language is the complement of the language of the specified OFSFA.
	 * 
	 * <p>NOTE: this methods has an exponential time complexity as it constructs a deterministic
	 * SFA equivalent to {@code oFSfa}.
	 * 
	 * 
	 * @param oFSfa the BDDs of its transitions' guards are consumed (freed)
	 * @return The complement of {@code oFSfa}
	 */
	public static SFA complementOFSfa(SFA oFSfa) {
		assertOFSfaFieldsNotNull(oFSfa);
		
		SFA compSimpleSfa = oFSfa.complement();
		
		SFAState compOFSfaFinalState = newEpsSfaState(true);
		SFA compOFSfa = newOFSfa(newEpsSfaState(false), compOFSfaFinalState);
		compOFSfa.getIni().addEpsTrans(compSimpleSfa.getIni());
		
		Set<? extends SFAState> compSimpleSfaFinalStates = compSimpleSfa.finalStates();
		
		for(SFAState formerFinalState : compSimpleSfaFinalStates) {
			formerFinalState.flipAcceptance();
			formerFinalState.addEpsTrans(compOFSfaFinalState);
		}
		
		//Free the transitions' BDDs of the input OFSFA
		oFSfa.free();
		
		return compOFSfa;
	}


	/**
	 * 
	 * Returns an OFSFA whose language is the intersection of the languages
	 * of the two specified OFSFAs.
	 * 
	 * @param oFSfaA the BDDs of its transitions' guards are consumed (freed)
	 * @param oFSfaB the BDDs of its transitions' guards are consumed (freed)
	 * @return
	 */
	public static SFA productOFSfa(SFA oFSfaA, SFA oFSfaB) {
		assertOFSfaFieldsNotNull(oFSfaA, oFSfaB);	
		
		/*
		 * Create a mapping between pairs of states from oFSfaA and oFSfaB, resp., to the new states in the
		 * new product OFSFA that we build.
		 * 
		 */
		Map<Pair<SFAState, SFAState>, SFAState> statesPairsToProductAutomatonStatesMapping = new HashMap<>();
		/*
		 * The initial (resp. the final) state in the product OFSFA is a pair whose components are the initial (resp. the final) states of oFSfaA and oFSfaB.
		 */
		
		Pair<SFAState, SFAState> initialStatesPair = new Pair<>(oFSfaA.getIni(), oFSfaB.getIni());
		SFAState productAutomatonInitialState = newEpsSfaState(oFSfaA.getIni().isAccepting() && oFSfaB.getIni().isAccepting());
		statesPairsToProductAutomatonStatesMapping.put(initialStatesPair, productAutomatonInitialState);
		
		SFAState productAutomatonFinalState;
		if(oFSfaA.getIni() == oFSfaA.getFinalState() && oFSfaB.getIni() == oFSfaB.getFinalState()) {
			productAutomatonFinalState = productAutomatonInitialState;
		}
		else {
			productAutomatonFinalState = newEpsSfaState(true);
			statesPairsToProductAutomatonStatesMapping.put(new Pair<>(oFSfaA.getFinalState(), oFSfaB.getFinalState()), productAutomatonFinalState);
		}
		
		buildProductSfa(true, statesPairsToProductAutomatonStatesMapping, initialStatesPair);
		
		//Free the transitions' BDDs of both input OFSFAs
		oFSfaA.free();
		oFSfaB.free();

		return newOFSfa(productAutomatonInitialState, productAutomatonFinalState);
	}
	
	
	/*
	 * 
	 * 
	 * 
	 * 
	 * 
	 * Private static methods
	 * 
	 * 
	 * 
	 * 
	 * 
	 * 
	 */
	private static void buildProductSfa(boolean productIsEpsSfa,
			Map<Pair<SFAState, SFAState>, SFAState> statesPairsToProductAutomatonStatesMapping,
			Pair<SFAState, SFAState> initialStatesPair) {
		
		/*
		 * We use a stack to do a DFS search on the given SFA. It will contain the state
		 * pairs of the product SFA that we create.
		 */
		
		List<Pair<SFAState, SFAState>> workStack = new Stack<>();
		workStack.add(0, initialStatesPair);
		BDD productTransGuard;
		Pair<SFAState, SFAState> currStatesPair, targetStatesPair;
		SFAState currentProductState, targetProductState;

		while (!workStack.isEmpty()) {
			currStatesPair = workStack.remove(0);

			//First, iterate over non-epsilon transitions
			Map<? extends SFAState, BDD> sfaASuccMap = currStatesPair.getLeft().getSucc();
			Map<? extends SFAState, BDD> sfaBSuccMap = currStatesPair.getRight().getSucc();
			for (SFAState sfaASuccState : sfaASuccMap.keySet()) {
				for (SFAState sfaBSuccState : sfaBSuccMap.keySet()) {

					productTransGuard = sfaASuccMap.get(sfaASuccState).and(sfaBSuccMap.get(sfaBSuccState));
					// if the intersection transition is satisfiable
					if (!productTransGuard.isZero()) {
						targetStatesPair = new Pair<>(sfaASuccState, sfaBSuccState);

						if (!statesPairsToProductAutomatonStatesMapping.containsKey(targetStatesPair)) {
							statesPairsToProductAutomatonStatesMapping.put(targetStatesPair,
									newSfaState(productIsEpsSfa, sfaASuccState.isAccepting() && sfaBSuccState.isAccepting()));
							workStack.add(0, targetStatesPair);
						}

						currentProductState = statesPairsToProductAutomatonStatesMapping.get(currStatesPair);
						targetProductState = statesPairsToProductAutomatonStatesMapping.get(targetStatesPair);
						currentProductState.addTrans(productTransGuard, targetProductState);
					}
					else {
						productTransGuard.free();
					}
				}
			}
			
			/*
			 * Second, iterate over outgoing epsilon transitions in currStatesPair, if such transitions exist.
			 * Each epsilon transition in sfaA (resp. sfaB) gives rise to an epsilon transition in the product automaton
			 * whose successor state (pair) consists of a left (resp. right)
			 * component that is the successor state of the corresponding epsilon transition in sfaA
			 * while the right (resp. left) component remains the same. Intuitively, this is as if we applied
			 * an elimination of epsilon moves in each of the input automatons (specifically, using the notion of "epsilon closure"),
			 * and then constructed the product automaton.
			 * 
			 */
			handleProductEpsTrans(currStatesPair, true, statesPairsToProductAutomatonStatesMapping, workStack);
			handleProductEpsTrans(currStatesPair, false, statesPairsToProductAutomatonStatesMapping, workStack);
		}
	}
	
	private static void handleProductEpsTrans(Pair<SFAState, SFAState> currStatesPair, boolean epsSrcStateIsLeft,
			Map<Pair<SFAState, SFAState>, SFAState> statesPairsToProductAutomatonStatesMapping, List<Pair<SFAState, SFAState>> workStack) {
		SFAState epsSrcState = epsSrcStateIsLeft ? currStatesPair.getLeft() : currStatesPair.getRight();
		SFAState otherSrcState = epsSrcStateIsLeft ? currStatesPair.getRight() : currStatesPair.getLeft();

		Pair<SFAState, SFAState> targetStatesPair;
		SFAState currProductState, targetProductState;

		if(epsSrcState.hasEpsSuccessors()) {
			for(SFAState epsSuccState : epsSrcState.getEpsSucc()) {
				targetStatesPair = epsSrcStateIsLeft ? new Pair<>(epsSuccState, otherSrcState) : new Pair<>(otherSrcState, epsSuccState);
				if (!statesPairsToProductAutomatonStatesMapping.containsKey(targetStatesPair)) {
					statesPairsToProductAutomatonStatesMapping.put(targetStatesPair,
							newEpsSfaState(epsSuccState.isAccepting() && otherSrcState.isAccepting()));
					workStack.add(0, targetStatesPair);
				}
				currProductState = statesPairsToProductAutomatonStatesMapping.get(currStatesPair);
				targetProductState = statesPairsToProductAutomatonStatesMapping.get(targetStatesPair);
				currProductState.addEpsTrans(targetProductState);
			}
		}
	}

	private static void assertOFSfaFieldsNotNull(SFA oFSfaA, SFA oFSfaB) {
		if(oFSfaA.getIni() == null || oFSfaB.getIni() == null) {
			throw new SFAException("The initial state attributes of the two given OFSFAs must not be null.");
		}
		if(oFSfaA.getFinalState() == null || oFSfaB.getFinalState() == null) {
			throw new SFAException("The final state attributes of the two given OFSFAs must not be null.");
		}
	}

	private static void assertOFSfaFieldsNotNull(SFA oFSfa) {
		if(oFSfa.getIni() == null) {
			throw new SFAException("The initial state attributes of the given OFSFA must not be null.");
		}
		if(oFSfa.getFinalState() == null) {
			throw new SFAException("The final state attributes of the given OFSFA must not be null.");
		}
	}
	
	/**
	 * Returns an OFSFA built by concatenating {@code oFSfa} n times.
	 * The input OFSFA {@code oFSfa} remains unchanged. The returned OFSFA does not depend
	 * on {@code oFSfa}.
	 * 
	 * @param oFSfa
	 * @param n an integer strictly greater than zero
	 * @return
	 */
	private static SFA concatNTimesOFSfa(SFA oFSfa, int n) {
		//This method assumes that n > 0.
		
		assertOFSfaFieldsNotNull(oFSfa);
		
		//First, create the n-th (last) copy of oFSfa that should accept; this copy
		//asserts the n-th repetition of a word in the language of oFSfa
		SFA exactNOFSfa = oFSfa.copy();

		//Second, create and concatenate the preceding (n-1) copies of oFSfa
		SFA currCopy;
		for(int copyIdx = n-1 ; copyIdx > 0 ; copyIdx--) {
			currCopy =  oFSfa.copy();
			currCopy.getFinalState().setAcceptance(false);
			currCopy.getFinalState().addEpsTrans(exactNOFSfa.getIni());
			exactNOFSfa.setIni(currCopy.getIni());
		}
		return exactNOFSfa;
	}

}