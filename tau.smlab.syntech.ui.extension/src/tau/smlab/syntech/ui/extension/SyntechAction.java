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

package tau.smlab.syntech.ui.extension;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.part.FileEditorInput;

import tau.smlab.syntech.ui.console.ConsolePrinter;

public abstract class SyntechAction<T extends Enum<T> & ActionID> extends ContributionItem {

  protected IFile specFile;
  protected IWorkbench workbench;
  protected IWorkbenchWindow window;
  protected IWorkbenchPage activePage;
  protected Shell shell;
  protected MessageConsole console;
  protected ConsolePrinter consolePrinter;

  /**
   * name of the plug-in to identify it on the console
   * 
   * @return
   */
  public abstract String getPluginName();

  /**
   * basically the items in T.getValues()
   * 
   * @return
   */
  public abstract T[] getActionItems();

  /**
   * method that is executed on a click of the user
   * 
   * @param actionID
   * @param specFile
   */
  public abstract void run(T actionID, IFile specFile);

  @Override
  public void fill(Menu menu, int index) {
    // fill menu from enumeration values
    for (T aid : getActionItems()) {
      MenuItem menuItem = new MenuItem(menu, SWT.CHECK, index);
      menuItem.setText(aid.getMenuText());
      menuItem.addSelectionListener(new SelectionAdapter() {
        public void widgetSelected(SelectionEvent e) {
          //what to do when menu is subsequently selected.
          prepareAndRun(aid);
        }
      });

    }
  }

  /**
   * find the selected specFile and check if it requires saving <br>
   * then run run()
   * 
   * @param actionID
   */
  public void prepareAndRun(T actionID) {
    if (!setSpecFileAndEnvironmentFields()) {
      return;
    }

    if (!savePage()) {
      return;
    }

    console = getConsole();
    try {
      consolePrinter = new ConsolePrinter(getPluginName(), ConsolePrinter.CLEAR_CONSOLE);
    } catch (Exception e) {
      e.printStackTrace();
    }

    run(actionID, specFile);
  }

  /**
   * @return true if setting succeeded, false otherwise.
   */
  private boolean setSpecFileAndEnvironmentFields() {
    workbench = PlatformUI.getWorkbench();
    window = workbench.getActiveWorkbenchWindow();
    shell = window.getShell();
    activePage = window.getActivePage();
    List<IFile> selectedFiles = new ArrayList<>();

    if (window == null) {
      return false;
    }

    ISelectionService selectionService = window.getSelectionService();
    if (selectionService == null) {
      return false;
    }

    ISelection selection = selectionService.getSelection();
    if (selection == null) {
      return false;
    }

    // selected on text view (the code window)
    if (selection instanceof TextSelection) {
      IEditorPart editor = activePage.getActiveEditor();
      IFile original = ((FileEditorInput) editor.getEditorInput()).getFile();
      specFile = original;
    }

    // selected on explorer view
    else if (selection instanceof IStructuredSelection) {
      IStructuredSelection structSelection = (IStructuredSelection) selection;
      List<?> selected = structSelection.toList();
      for (Object object : selected) {
        if (object instanceof IFile) {
          IFile file = (IFile) object;
          selectedFiles.add(file);
        }
      }
      if (selectedFiles == null || selectedFiles.size() == 0 || selectedFiles.size() >= 2) {
        MessageDialog.openInformation(shell, getPluginName(), "Please select only one .spectra file.");
        return false;
      } else {
        specFile = selectedFiles.get(0);
      }
    }

    return true;
  }

  /**
   * If the page is unsaved, ask user if he wants to save it first
   * 
   * @return false if the user has chosen to abort
   */
  protected boolean savePage() {
    IWorkbenchPage page = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();

    // check if file is saved
    IEditorPart editorPart = page.getActiveEditor();
    if (editorPart != null && editorPart.isDirty()) {
      boolean isYes = MessageDialog.openQuestion(shell, getPluginName(),
          "The file is not saved. Select 'Yes' to save and 'No' to abort.");
      if (isYes) {
        editorPart.doSave(null);
      } else {
        return false;
      }
    }
    return true;
  }

  protected MessageConsole getConsole() {
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
}
