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

package tau.smlab.syntech.ui.extension.console;

import org.eclipse.core.resources.IFile;
import org.eclipse.debug.ui.console.FileLink;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.console.IPatternMatchListenerDelegate;
import org.eclipse.ui.console.PatternMatchEvent;
import org.eclipse.ui.console.TextConsole;
import org.eclipse.ui.part.FileEditorInput;

public class ExplanationConsolePatternMatchListener implements IPatternMatchListenerDelegate
{
	private TextConsole console;
	IWorkbenchPage activePage;
	
	public ExplanationConsolePatternMatchListener() {}

	@Override
	public void connect(TextConsole console)
	{
		this.console = console;
		 activePage = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
		
	}

	@Override
	public void disconnect()
	{
		console = null;
	}

	@Override
	public void matchFound(PatternMatchEvent event)
	{
		IEditorPart editor = activePage.getActiveEditor();
		if (editor != null) {
			IFile spec = ((FileEditorInput)editor.getEditorInput()).getFile();
			
			try {
				String file_reference_text = console.getDocument().get(event.getOffset(), event.getLength());
				// We add 2 because the match is "line: number" and we want to skip the ':' and the space
				int line_number_index = file_reference_text.lastIndexOf(":") + 2;
				int line_number = Integer.parseInt(file_reference_text.substring(line_number_index));
	
				FileLink f_l = new FileLink(spec, null, -1, -1, line_number);	
				
				console.addHyperlink(f_l, event.getOffset(), event.getLength());
			} catch (BadLocationException e) {
				e.printStackTrace();
			}
		}
	}
}