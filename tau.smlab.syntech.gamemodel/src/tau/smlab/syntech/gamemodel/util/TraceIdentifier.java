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

import org.eclipse.emf.ecore.EObject;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;

import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.spectragameinput.translator.Tracer;

/**
 * This class allows identification of model elements according to traces
 * 
 * Their respective module (parts of monitors and counters are built into the system but their module label is AUX)
 * Their type (pattrens and triggers are considered justices according to their non-aux behaviors)
 * Their kind (pattrens and triggers are considered COMPLEX because they are env or sys with supporting aux,
 * 				parts of monitors and counters are AUX, because they are listed as only aux behaviors)
 * 
 * @author shalom
 *
 */

public class TraceIdentifier {
	GameModel gm = null;
	protected List<BehaviorInfo> env = null;
	protected List<BehaviorInfo> sys = null;
	protected List<BehaviorInfo> aux = null;
	
	public enum Module {SYS, ENV, AUX};	
	public enum Type { INI, SAFE, JUST;
		
		public String toString() {
			switch (this) {
			case INI:
				return "<Initial>";
			case SAFE:
				return "<Safety>";
			case JUST:
				return "<Justice>";
			default:
				return "*error*";
			}	
		}
	}
	
	public enum Kind { COMPLEX, SIMPLE;
		
		public String toString() {
			switch (this) {
			case COMPLEX:
				return "<Complex>";
			case SIMPLE:
				return "<Simple GR(1)>";
			default:
				return "error";
			}
		}
	}
	
	public TraceIdentifier(GameModel gm) {
		this.gm = gm;
		env = new ArrayList<BehaviorInfo>(gm.getEnvBehaviorInfo());
		sys = new ArrayList<BehaviorInfo>(gm.getSysBehaviorInfo());
		aux = new ArrayList<BehaviorInfo>(gm.getAuxBehaviorInfo());
	}
	
	/**
	 * get all traces that build the system, including auxiliary behaviors
	 * 
	 * @return list of traces
	 */
	public List<Integer> getSysTraces() {
		List<Integer> envTraces = new ArrayList<Integer>();
		List<Integer> traceList = new ArrayList<Integer>();
		
		for (BehaviorInfo bi : env) {
			singularAdd(envTraces, bi.traceId);
		}
		
		for (BehaviorInfo bi : sys) {
			if (!envTraces.contains(bi.traceId)) {
				singularAdd(traceList, bi.traceId);
			}
		}
		
		for (BehaviorInfo bi : aux) {
			if (!envTraces.contains(bi.traceId)) {
				singularAdd(traceList, bi.traceId);
			}			
		}
		return traceList;
	}
	
	/**
	 * get all traces that build the environment
	 * @return list of traces
	 */
	public List<Integer> getEnvTraces() {
		List<Integer> traceList = new ArrayList<Integer>();
		for (BehaviorInfo bi : env) {
			singularAdd(traceList, bi.traceId);
		}
		return traceList;
	}	
	
	/**
	 * A trace is identified as ENV or SYS if at least one of its behaviors has it.
	 * 
	 * @param t
	 * @return
	 */
	public Module getModule(Integer t) {
		return in(env, t) ? Module.ENV : (in(sys, t) ? Module.SYS : Module.AUX);
	}
	
	/**
	 * This is the type in the sys or env module. 
	 * Note !! Ones in aux pick the first one they see (for example there could be a ONCE in a monitor safety)
	 * 
	 * @param t
	 * @return
	 */
	public Type getType(Integer t)	{
		List<BehaviorInfo> lookin = null;
		Type ret = null;
		
		switch (getModule(t)) {
		case SYS:
			lookin = sys;
			break;
		case ENV:
			lookin = env;
			break;
		case AUX:
			lookin = aux;
			break;
		}
		for (BehaviorInfo bi : lookin) {
			if (bi.traceId == t) {
				ret = bi.isInitial() ? Type.INI : (bi.isSafety() ? Type.SAFE : Type.JUST);
				break;
			}
		}
		return ret;
	}
	
	/**
	 * Traces both in sys or env but not in aux are simple GR(1) elements
	 * Traces both in sys or env yet also in aux are complex (patterns, triggers, containing ONCE)
	 * Traces only in aux are auxiliary behaviors (monitors, counters), and are simple if they have just one behavior
	 * 
	 * @param t
	 * @return
	 */
	public Kind getKind(Integer t) {
		boolean inSys = in(sys, t);
		boolean inEnv = in(env, t);
		int countAux = countin(aux, t);
		
		return ((inSys | inEnv) && countAux>0) || countAux>1 ? Kind.COMPLEX : Kind.SIMPLE;
	}
	
	/**
	 * How many of specified type ate in the list
	 * 
	 * @param lst
	 * @param t
	 * @return
	 */
	public int countType(List<Integer> lst, Type t) {
		int ret = 0;
		for (Integer trace : lst) {
			if (getType(trace)==t) {
				ret++;
			}
		}
		return ret;
	}
	
	/** get a list of lines according to traces
	 * 
	 * @param list
	 * @return
	 */
	public static String formatLines(List<Integer> list) {
		String formatted = "< ";
		for (Integer p : list) {
			EObject obj=Tracer.getTarget(p);
			formatted+= obj==null ? "untraced" : NodeModelUtils.getNode(Tracer.getTarget(p)).getStartLine() + " ";
		}
		return formatted + ">";
	}
	
	/**
	 * get a message about the line number
	 * 
	 * @param b
	 * @return
	 */
	public static String getLine(Integer b) {
		EObject obj=Tracer.getTarget(b);
		return obj==null ? "<line cannot be traced>" : "At line " + NodeModelUtils.getNode(Tracer.getTarget(b)).getStartLine();
	}
	
	/**
	 * Format a message about a marked element
	 * 
	 * @param b
	 * @return
	 */
	public String formatMarked(Integer b) {
		return TraceIdentifier.getLine(b) + (getModule(b)==TraceIdentifier.Module.AUX ? " is an auxiliary" : " is a")
				+ " behavior of kind " + getKind(b) +
				(getKind(b)==TraceIdentifier.Kind.SIMPLE ? " and type " + getType(b) : "");
	}
	
	/**
	 * get the correct traces according to start line numbers
	 * 
	 * @param traces
	 * @param lines
	 * @return
	 */
	public List<Integer> locateSysTraces(List<Integer> lines) {
		List<Integer> traces = getSysTraces();
		List<Integer> subTraces = new ArrayList<Integer>();
		for (Integer l : lines) {
			for (Integer t : traces) {
				if (Tracer.getTarget(t)!=null & l==NodeModelUtils.getNode(Tracer.getTarget(t)).getStartLine()) {
					singularAdd(subTraces, t);
					continue;
				}
			}
		}
		return subTraces;
	}

	/**
	 * add making sure the is the only instance in list
	 * 
	 * @param lst	the list
	 * @param x		the item
	 */
	private void singularAdd(List<Integer> lst, Integer x) {
		if (!lst.contains(x)) {
			lst.add(x);
		}
	}
	
	/**
	 * Is the trace in one of the behaviors?
	 * 
	 * @param lst
	 * @param who
	 * @return
	 */
	private boolean in(List<BehaviorInfo> lst, Integer who) {
		return countin(lst, who) > 0;
	}
	
	/**
	 * How many of the trace is in one of the behaviors?
	 * 
	 * @param lst
	 * @param who
	 * @return
	 */
	private int countin(List<BehaviorInfo> lst, Integer who) {
		int num = 0;
		for (BehaviorInfo bi : lst) {
			if (bi.traceId == who) {
				num++;
			}
		}
		return num;
	}
}
