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

package tau.smlab.syntech.ui.console;

import java.io.IOException;
import java.io.PrintStream;

import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.console.ConsolePlugin;
import org.eclipse.ui.console.IConsole;
import org.eclipse.ui.console.IConsoleConstants;
import org.eclipse.ui.console.IConsoleManager;
import org.eclipse.ui.console.IConsoleView;
import org.eclipse.ui.console.MessageConsole;
import org.eclipse.ui.console.MessageConsoleStream;

public class ConsolePrinter {
	public final static int APPEND = 0;
	public final static int CLEAR_CONSOLE = 1;
	
	private String pluginName;
	private MessageConsole console;

	/**
	 * 
	 * @param activator should be an Activator object.
	 * @param mode ConsolePrinter.APPEND or ConsolePrinter.CLEAR_CONSOLE
	 * @throws SecurityException 
	 * @throws NoSuchFieldException 
	 * @throws IllegalAccessException 
	 * @throws IllegalArgumentException 
	 */
	public ConsolePrinter(String name, int mode) throws NoSuchFieldException, SecurityException, IllegalArgumentException, IllegalAccessException {
		this.pluginName = name;
		this.console = getConsole();
		if (mode == CLEAR_CONSOLE)
		{
			clearConsole();
		}
		else if (mode == APPEND)
		{
			println("");			
		}
		println(pluginName + ":");
		println("");
	}

	public void clearConsole() {
		console.clearConsole();
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

	public void print(String string) {
		print(string, false);
	}

	public void println(String string) {
		print(string, true);
	}

	private void print(String string, boolean isPrintln) {
		MessageConsoleStream mcs = console.newMessageStream();
		if (isPrintln) {
			mcs.println(string);
		} else {
			mcs.print(string);
		}
		try {
			mcs.flush();
			mcs.close();
		} catch (IOException e) {
		}
	}

	/**
	 * Call this function to open the console view
	 */
	public void showConsole(IWorkbenchPage page) {
		try {
			IConsoleView view = (IConsoleView) page.showView(IConsoleConstants.ID_CONSOLE_VIEW);
			view.display(getConsole());
		} catch (PartInitException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public PrintStream getPrintStream() {
		return new PrintStream(console.newOutputStream());
	}

}
