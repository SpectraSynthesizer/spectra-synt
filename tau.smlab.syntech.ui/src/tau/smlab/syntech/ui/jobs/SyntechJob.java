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

package tau.smlab.syntech.ui.jobs;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.xtext.nodemodel.INode;
import org.eclipse.xtext.nodemodel.util.NodeModelUtils;

import net.sf.javabdd.BDD;
import net.sf.javabdd.BDDVarSet;
import tau.smlab.syntech.bddgenerator.BDDGenerator;
import tau.smlab.syntech.bddgenerator.BDDGenerator.TraceInfo;
import tau.smlab.syntech.bddgenerator.BDDTranslationException;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinputtrans.translator.Translator;
import tau.smlab.syntech.gamemodel.BehaviorInfo;
import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule.TransFuncType;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;
import tau.smlab.syntech.spectragameinput.translator.Tracer;
import tau.smlab.syntech.ui.preferences.PreferencePage;

public abstract class SyntechJob extends Job {

	protected IFile specFile;
	protected MessageConsole console;
	protected GameModel model;
	protected GameInput gi;
	protected TraceInfo trace = TraceInfo.NONE;
	protected static IFile previousFileWithMarkers;
	protected String consoleOutput = "";
	protected long computationTime;
	protected long bddTranslationTime;
	protected boolean isUserCancelledJob = false;
	protected boolean isRealizable;
	protected boolean isWellSeparated;
	protected int coreSize;
	protected String issuesKind;
	protected int numIssues;
	protected List<Translator> translators;
	private static List<Integer> traceIDListInCore = new ArrayList<Integer>();

	/**
	 * set info what elements of spec to trace (creates relevant BehaviorInfo).<br>
	 * Note: the more you trace the longer everything takes due to BDD creation.
	 * 
	 * @param trace
	 */
	public void setTrace(TraceInfo trace) {
		this.trace = trace;
	}

	public SyntechJob() {
		super("SYNTECH");
	}

	/**
	 * Implementation of default run method to start a thread that actually will do
	 * the work.
	 */
	@SuppressWarnings("deprecation")
	@Override
	protected IStatus run(IProgressMonitor monitor) {

		long jobTime = System.currentTimeMillis();

		Thread t = new Thread(() -> translateAnddoWork());
		t.start();
		while (t.isAlive()) {
			if (monitor.isCanceled()) {
				t.stop();
				printToConsole("User cancelled job.");
				Env.resetEnv();
				isUserCancelledJob = true;
				return Status.CANCEL_STATUS;
			}
			try {
				Thread.sleep(10);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		computationTime = System.currentTimeMillis() - jobTime;
		printToConsole("Computation time: " + computationTime + "ms");
		return Status.OK_STATUS;
	}
	
	/**
	 * determinize b
	 * 
	 * @param b
	 * @param vars
	 * @return
	 */
	protected BDD det(BDD b, BDDVarSet vars) {
		// set new varorder where vars are at the end
		Env.disableReorder();
		int[] oldOrder = b.getFactory().getVarOrder();
		List<Integer> newOrder = Arrays.stream(oldOrder).boxed().collect(Collectors.toList());
		List<Integer> varsL = Arrays.stream(vars.toArray()).boxed().collect(Collectors.toList());
		newOrder.removeAll(varsL);
		newOrder.addAll(varsL);
		b.getFactory().setVarOrder(newOrder.stream().mapToInt(i->i).toArray());

		BDD det = b.id();
		for (int var : vars.toArray()) {
			BDD tmp = det;
			det = det(tmp, var);
			tmp.free();
		}
		//b.getFactory().setVarOrder(oldOrder);
		return det;
	}
	
	/**
	 * determinizes b to a fresh BDD
	 * @param b
	 * @param var
	 * @return
	 */
	private BDD det(BDD b, int var) {

		if (b.isZero()) {
			return b.id();
		}

		if (b.isOne()) {
			return b.getFactory().nithVar(var).id();
		}

		// var appears so make decision one option
		// this can only work because of varorder
		if (b.var() == var) {
			BDD blow = b.low();
			if (!blow.isZero()) {
				// take low branch regardless of high branch        
				BDD res = b.getFactory().ithVar(var);
				res = res.ite(Env.FALSE(), blow);
				blow.free();
				return res;
			} else {
				// there is only one choice for b so it is fine
				return b.id();
			}
		}


		// var did not appear
		if (passedVar(b, var)) {
			BDD res = b.getFactory().ithVar(var);
			res = res.ite(Env.FALSE(), b);
			return res;
		} else {
			// determinize children
			BDD res = b.getFactory().ithVar(b.var());
			BDD bhi = b.high();
			BDD high = det(bhi, var);
			bhi.free();
			BDD blo = b.low();
			BDD low = det(blo, var);
			blo.free();
			res = res.ite(high, low);
			high.free();
			low.free();
			return res;
		}
	}
	
	/**
	 * did var only appear above b?
	 * 
	 * @param b
	 * @param var
	 * @return
	 */
	private boolean passedVar(BDD b, int var) {
		int lvlB = b.getFactory().var2Level(b.var());
		int lvlVar = b.getFactory().var2Level(var);
		return lvlB > lvlVar;
	}
	

	/**
	 * first translate GameInput into GameModel (this can already be expensive due
	 * to BDD creation)
	 */
	private void translateAnddoWork() {
		long start = System.currentTimeMillis();
		try {
			this.model = BDDGenerator.generateGameModel(gi, trace, PreferencePage.isGroupVarSelection(),
					PreferencePage.getTransFuncSelection(false));
			bddTranslationTime = System.currentTimeMillis() - start;
			printToConsole("BDD translation: " + bddTranslationTime + "ms (" + Env.getBDDPackageInfo() + ")");
			int sysNonAux = getVarNum(model.getSys().getNonAuxFields());
			int sysAux = getVarNum(model.getSys().getAuxFields());
			printToConsole("Statespace env: " + getVarNum(model.getEnv().getAllFields()) + ", sys: " + sysNonAux
					+ ", aux: " + sysAux);
			doWork();
		} catch (BDDTranslationException e) {
			if (e.getTraceId() >= 0) {
				createMarker(e.getTraceId(), e.getMessage(), MarkerKind.CUSTOM_TEXT_ERROR);
			}
			printToConsole(e.getMessage());
		} catch (Exception e) {
			printToConsole(e.getMessage());
			e.printStackTrace();
		}
	}

	/**
	 * compute number of variables of player
	 * 
	 * @param p
	 * @return
	 */
	private int getVarNum(List<ModuleBDDField> fs) {
		int varNum = 0;
		for (ModuleBDDField f : fs) {
			varNum += f.support().size();
		}
		return varNum;
	}

	/**
	 * do the actual work specific to the Job
	 */
	protected abstract void doWork();

	public void setSpecFile(IFile f) {
		specFile = f;
	}

	public void setConsole(MessageConsole console) {
		this.console = console;
	}

	/**
	 * print a string to the console of the plug-in
	 * 
	 * @param s
	 */
	public void printToConsole(String s) {
		consoleOutput += s + System.lineSeparator();
		MessageConsoleStream mcs = console.newMessageStream();
		mcs.println(s);
		try {
			mcs.flush();
			mcs.close();
		} catch (IOException e) {
		}
	}

	public void setGameInput(GameInput i) {
		this.gi = i;
	}

	/**
	 * deletes all markers of all SYNTECH MarkerKind(s)
	 */
	public void clearMarkers(IFile specIfile) {
		traceIDListInCore.clear();

		for (MarkerKind k : MarkerKind.values()) {
			try {
				specIfile.deleteMarkers(k.getMarkerID(), true, IResource.DEPTH_ZERO);
			} catch (CoreException e) {
			}
		}
	}

	public void clearMarkers() {
		clearMarkers(specFile);
	}

	/**
	 * creates markers in the current file for a list of BehaviorInfos
	 * 
	 * @param infos
	 * @param kind
	 */
	public void createMarker(List<BehaviorInfo> infos, MarkerKind kind) {
		if (infos != null && infos.size() > 0) {
			if (previousFileWithMarkers != null) {
				clearMarkers(previousFileWithMarkers);

			}
			previousFileWithMarkers = specFile;
		}
		for (BehaviorInfo info : infos) {
			createMarker(info.traceId, kind.getMessage(), kind);
		}
	}

	/**
	 * create a marker for an element with the given traceId
	 * 
	 * @param traceId
	 * @param message
	 * @param kind
	 */
	public void createMarker(int traceId, String message, MarkerKind kind) {
		EObject o = Tracer.getTarget(traceId);
		traceIDListInCore.add(traceId);
		if (o != null) {
			INode node = NodeModelUtils.getNode(o);
			try {
				IMarker marker = specFile.createMarker(kind.getMarkerID());
				marker.setAttribute(IMarker.MESSAGE, message);
				marker.setAttribute(IMarker.PRIORITY, IMarker.PRIORITY_NORMAL);
				marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_INFO);
				marker.setAttribute(IMarker.LOCATION, node.getStartLine());
				marker.setAttribute(IMarker.CHAR_START, node.getOffset());
				marker.setAttribute(IMarker.CHAR_END, node.getEndOffset());
			} catch (CoreException e) {
			}
		}
	}

	public String getConsoleOutput() {
		return this.consoleOutput;
	}

	public long getComputationTime() {
		return this.computationTime;
	}

	public long getBddTranslationTime() {
		return this.bddTranslationTime;
	}

	public boolean isUserCancelledJob() {
		return this.isUserCancelledJob;
	}

	public boolean isRealizable() {
		return this.isRealizable;
	}

	public boolean isWellSeparated() {
		return this.isWellSeparated;
	}

	public int getCoreSize() {
		return this.coreSize;
	}

	public String getIssuesKind() {
		return issuesKind;
	}

	public int getNumIssues() {
		return numIssues;
	}

	public static List<Integer> getTraceIDListInCore() {
		// find active ifile
		IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		IEditorPart activeEditor = page.getActiveEditor();
		IFile ifile;
		if (activeEditor != null) {
			IEditorInput input = activeEditor.getEditorInput();
			if (input != null && input instanceof FileEditorInput) {
				ifile = ((FileEditorInput) input).getFile();

				if (ifile == null || !("spectra".equals(ifile.getFileExtension()))
						|| (ifile != previousFileWithMarkers)) {
					// return empty list. (We don't want to clear traceIDListInCore in case the user
					// will open the previousFileWithMarkers again).
					return new ArrayList<Integer>();
				}
			}
		}

		return traceIDListInCore;
	}

	public void setTranslators(List<Translator> transList) {
		this.translators = transList;
	}

	@SuppressWarnings("unchecked")
	public <T extends Translator> T getTranslator(Class<T> translatorClass) {
		for (Translator t : translators) {
			if (t.getClass().equals(translatorClass)) {
				return (T) t;
			}
		}
		return null;
	}

	public boolean needsBound() {
		return false;
	}

	/**
	 * If decomposed transitions are used, frees and resets to TRUE (restricted by
	 * variable domain constraint) the single transition relations of both players
	 */
	public void resetSingleTrans() {
		TransFuncType transFuncType = PreferencePage.getTransFuncSelection(false);
		if (transFuncType != TransFuncType.SINGLE_FUNC) {
			this.model.resetSingleTransFunc();
		}
	}

}
