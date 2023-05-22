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

package tau.smlab.syntech.ui;

import java.util.concurrent.CountDownLatch;

import org.eclipse.emf.ecore.EObject;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.xtext.resource.EObjectAtOffsetHelper;

import tau.smlab.syntech.spectragameinput.SpectraInputProvider;

public class SelectionHandler {
	
	public static EObject getSelectedEObject() {
		EObject[] ans = new EObject[1];
		final CountDownLatch latch = new CountDownLatch(1);
		Display.getDefault().asyncExec(new Runnable() {
		    @Override
		    public void run() {
		    	final IEditorPart editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
				    	.getActiveEditor();
				if (!(editor instanceof ITextEditor)) {
					latch.countDown();
					return;
				}
				ITextEditor ite = (ITextEditor)editor;
				ISelection s = ite.getSelectionProvider().getSelection();
				
				if (!(s instanceof ITextSelection)) {
					latch.countDown();
					return;
				}
				ITextSelection t = (ITextSelection)s;
				ans[0] = new EObjectAtOffsetHelper().resolveElementAt(SpectraInputProvider.resource, t.getOffset());
				latch.countDown();
		    }
		});
		try {
			latch.await();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return ans[0];
	}
	
	
	public static void clearSelection() {
		Display.getDefault().asyncExec(new Runnable() {
		    @Override
		    public void run() {
		    	final IEditorPart editor = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage()
				    	.getActiveEditor();
				if (!(editor instanceof ITextEditor)) {
					return;
				}
				ITextEditor ite = (ITextEditor)editor;
				ISelectionProvider isp = ite.getSelectionProvider();
				isp.setSelection(StructuredSelection.EMPTY);
		    }
		});
	}
	

}
