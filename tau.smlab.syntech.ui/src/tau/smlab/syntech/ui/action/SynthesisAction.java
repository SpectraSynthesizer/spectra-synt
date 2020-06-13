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

package tau.smlab.syntech.ui.action;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IConsoleView;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.part.FileEditorInput;

import net.sf.javabdd.BDDFactory;
import tau.smlab.syntech.bddgenerator.BDDGenerator.TraceInfo;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinputtrans.TranslationException;
import tau.smlab.syntech.gameinputtrans.TranslationProvider;
import tau.smlab.syntech.gameinputtrans.translator.DefaultTranslators;
import tau.smlab.syntech.gameinputtrans.translator.Translator;
import tau.smlab.syntech.jtlv.BDDPackage;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.spectragameinput.ErrorsInSpectraException;
import tau.smlab.syntech.spectragameinput.SpectraInputProvider;
import tau.smlab.syntech.spectragameinput.SpectraTranslationException;
import tau.smlab.syntech.ui.jobs.CheckRealizabilityJob;
import tau.smlab.syntech.ui.jobs.ChecksJob;
import tau.smlab.syntech.ui.jobs.CounterStrategyJob;
import tau.smlab.syntech.ui.jobs.MarkerKind;
import tau.smlab.syntech.ui.jobs.SyntechJob;
import tau.smlab.syntech.ui.jobs.SynthesizeSharedControllerJob;
import tau.smlab.syntech.ui.jobs.SynthesizeConcreteControllerJob;
import tau.smlab.syntech.ui.jobs.SynthesizeJitSymbolicControllerJob;
import tau.smlab.syntech.ui.jobs.SynthesizeSymbolicControllerJob;
import tau.smlab.syntech.ui.preferences.PreferencePage;

public class SynthesisAction implements IObjectActionDelegate, IEditorActionDelegate {

	private IFile specFile;
	private Shell shell;

	@Override
	public void run(IAction action) {
		if (specFile == null) {
			MessageDialog.openInformation(shell, "SYNTECH", "Please select one (and only one) Spectra file.");
		}

		if (!savePage()) {
			return;
		}
		
		SyntechJob job = null;
		closeOldProblemView();
		switch (action.getId()) {
		case "tau.smlab.syntech.syntSymbAction":
			job = new SynthesizeSymbolicControllerJob();
			break;
		case "tau.smlab.syntech.syntJitSymbAction":
			job = new SynthesizeJitSymbolicControllerJob();
			break;
		case "tau.smlab.syntech.syntSharedSymbAction":
			job = new SynthesizeSharedControllerJob();
			break;
		case "tau.smlab.syntech.syntCmpAction":
			job = new SynthesizeConcreteControllerJob();
			break;
		case "tau.smlab.syntech.checkRealAction":
			job = new CheckRealizabilityJob();
			break;
		case "tau.smlab.syntech.checksAction":
			job = new ChecksJob();
			job.setTrace(TraceInfo.ALL);
			break;
		case "tau.smlab.syntech.debugCmpAction":
			job = new CounterStrategyJob();
			break;
//		case "tau.smlab.syntech.coreUnrealizable":
//			job = new UnrealizableCoreJob();
//			job.setTrace(TraceInfo.SYS_AUX);
//			break;
//		case "tau.smlab.syntech.checkWellSeparation":
//			job = new CheckWellSeparationJob();
//			job.setTrace(TraceInfo.ALL);
//			break;
//		case "tau.smlab.syntech.coreWellSeparation":
//			job = new NonWellSeparationCoreJob();
//			job.setTrace(TraceInfo.ALL);
//			break;
//    case "tau.smlab.syntech.strategyWellSeparation":
//      job = new NonWellSeparationStrategyJob();
//      job.setTrace(TraceInfo.ALL);
//      break;
//		case "tau.smlab.syntech.couldAsmHelp":
//      job = new CheckCouldAsmHelpJob();
//      break;
		default:
			System.err.println("Unhandled action id: " + action.getId());
			break;
		}

		MessageConsole console = getConsole();
		console.clearConsole();
		showConsole();
		
		long setupTime = 0, parsingTime, simplificationTime;
		long start = System.currentTimeMillis();
		BDDPackage.setCurrPackage(PreferencePage.getBDDPackageSelection(), PreferencePage.getBDDPackageVersionSelection());
		if (PreferencePage.isReorderEnabled()) {
			Env.enableReorder();
			Env.TRUE().getFactory().autoReorder(BDDFactory.REORDER_SIFT);
		} else {
			Env.disableReorder();
		}

		job.setSpecFile(specFile);
		job.setConsole(console);
		job.setUser(true);
		job.clearMarkers();

		GameInput gi = null;
		try {
			gi = SpectraInputProvider.getGameInput(specFile.getFullPath().toString());
		} catch (ErrorsInSpectraException e) {
			job.printToConsole("Errors in input file:");
			job.printToConsole(e.getMessage());
			setupTime = System.currentTimeMillis() - start;
			job.printToConsole("Setup time: " + setupTime + "ms");
			return;
		} catch (SpectraTranslationException e) {
      job.printToConsole("Problems encountered in input file:");
      job.printToConsole(e.getMessage());
      setupTime = System.currentTimeMillis() - start;
      job.printToConsole("Setup time: " + setupTime + "ms");
      job.createMarker(e.getTraceId(), e.getMessage(), MarkerKind.CUSTOM_TEXT_ERROR);
      return;
    }
		parsingTime = System.currentTimeMillis() - start;
		job.printToConsole("Parsing: " + parsingTime + "ms");
		
		if (job.needsBound() && !gi.getWeightDefs().isEmpty()) {
		  InputDialog d = new InputDialog(shell, "Specify Energy Bound", "Please specify a suitable bound for the Energy Game:", "20", new IInputValidator() {        
        @Override
        public String isValid(String newText) {
          try {
            Integer.parseInt(newText);
          } catch(Exception e) {
            return "Unable to parse integer!";
          }
          return null;
        }
      });
		  if (d.open() == Window.OK) {
		    gi.setEnergyBound(Integer.parseInt(d.getValue()));
		  } else {		    
			gi.setEnergyBound(20);
		  }
		  job.printToConsole("Bound set to: " + gi.getEnergyBound());
		}
		
		start = System.currentTimeMillis();
		try {
			List<Translator> transList = DefaultTranslators.getDefaultTranslators();
			TranslationProvider.translate(gi, transList);
			job.setTranslators(transList);
		} catch (TranslationException e) {
			job.printToConsole(e.getMessage());
			if (e.getTraceId() >= 0) {
				job.createMarker(e.getTraceId(), e.getMessage(), MarkerKind.CUSTOM_TEXT_ERROR);
			}
			return;
		}
		simplificationTime = System.currentTimeMillis() - start;
		job.printToConsole("Simplification: " + simplificationTime + "ms");

		job.setGameInput(gi);

		job.schedule();
	}

	/**
	 * Closes problem view(s) (if it was opened) from previous actions.
	 */
	private void closeOldProblemView() {
		PlatformUI.getWorkbench().getDisplay().syncExec(new Runnable() {
			public void run() {
				String[] possibleViewsId = { "unrealizableCoreMarker", "wellSeparatedCoreMarker",
						"specAnalysisMarker" };
				for (int i = 0; i < possibleViewsId.length; i++) {
					// check if possibleViewsId[i] was opened
					IViewPart viewPart = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
							.findView(possibleViewsId[i]);
					if (viewPart != null) {
						// possibleViewsId[i] was opened. close it.
						PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().hideView(viewPart);
					}
				}
			}
		});
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		if (selection instanceof StructuredSelection && !selection.isEmpty()) {
			StructuredSelection s = (StructuredSelection) selection;
			if (s.size() > 1) {
				specFile = null;
			} else {
				specFile = (IFile) s.iterator().next();
			}
		}
	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
	}

	private MessageConsole getConsole() {
		String name = "SYNTECH Console";
		ConsolePlugin plugin = ConsolePlugin.getDefault();
		IConsoleManager conMan = plugin.getConsoleManager();
		IConsole[] existing = conMan.getConsoles();
		for (int i = 0; i < existing.length; i++)
			if (name.equals(existing[i].getName()))
				return (MessageConsole) existing[i];
		// no console found, so create a new one
		MessageConsole myConsole = new MessageConsole(name, null);
		conMan.addConsoles(new IConsole[] { myConsole });
		return myConsole;
	}

	/**
	 * If the page is unsaved, ask user if he wants to save it first
	 * @return false if the user has chosen to abort
	 */
	private boolean savePage() {
		IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

		// check if file is saved
		IEditorPart editorPart = page.getActiveEditor();
		if (editorPart != null && editorPart.isDirty()) {
			boolean isYes = MessageDialog.openQuestion(shell, "SYNTECH",
					"The file is not saved. Select 'Yes' to save and 'No' to abort.");
			if (isYes) {
				editorPart.doSave(null);
			} else {
				return false;
			}
		}
		return true;
	}

	private void showConsole() {
		try {
			IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			IConsoleView view = (IConsoleView) page.showView(IConsoleConstants.ID_CONSOLE_VIEW);
			view.display(getConsole());
		} catch (PartInitException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	@Override
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		if (targetEditor != null) {
			IEditorInput input = targetEditor.getEditorInput();
			if (input != null && input instanceof FileEditorInput) {
				IFile ifile = ((FileEditorInput) input).getFile();
				if (ifile != null) {
					if ("spectra".equals(ifile.getFileExtension())) {
						specFile = ifile;
					}
				}
			}
		}
	}

}
