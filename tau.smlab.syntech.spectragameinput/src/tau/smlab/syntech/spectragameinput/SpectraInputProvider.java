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

package tau.smlab.syntech.spectragameinput;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.resource.IResourceServiceProvider;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.ui.resource.IResourceSetProvider;
import org.eclipse.xtext.ui.resource.XtextResourceSetProvider;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;
import org.eclipse.xtext.validation.Issue;

import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.spectra.Model;
import tau.smlab.syntech.spectragameinput.translator.Spectra2GameInputTranslator;

/**
 * This class will invoke the Spectra parser and a translator to obtain the game input
 * 
 */
public class SpectraInputProvider {

	private static List<Issue> issues;

	/**
	 * Loads a spectra model from a file and returns its represenation in GameInput
	 * 
	 * The path inputFile must be relative to the workspace.
	 * 
	 * @param inputFile
	 *          relative path to the workspace
	 * @return
	 * @throws ErrorsInSpectraException
	 * @throws SpectraTranslationException 
	 */
	public static GameInput getGameInput(String inputFile) throws ErrorsInSpectraException, SpectraTranslationException {

		// x-text AST root is of type Model
		Model spectraModel = getSpectraModel(inputFile);

		if (!issues.isEmpty()) {
			StringBuffer buf = new StringBuffer();
			for (Issue i : issues) {
				buf.append(i.getMessage() + " on line " + i.getLineNumber());
				buf.append(System.lineSeparator());
			}
			throw new ErrorsInSpectraException(buf.toString());
		}

		// translate to game input
		GameInput gi = Spectra2GameInputTranslator.translate(spectraModel);

		return gi;
	}

	/**
	 * 
	 * Loads a spectra model from a file and returns its root node of type Model
	 * 
	 * The path inputFile must be relative to the workspace.
	 * 
	 * @param inputFile
	 *          relative path to the workspace
	 * @return
	 */
	public static Model getSpectraModel(String inputFile) {
		IWorkspace workspace = ResourcesPlugin.getWorkspace();
		IWorkspaceRoot root = workspace.getRoot();

		IFile file = root.getFile(new Path(inputFile));
		return getSpectraModel(file);
	}

	/**
	 * Loads a spectra model from a file and returns its representation as GameInput
	 * 
	 * The path inputFile can be relative to executed program or absolute in the file system
	 * 
	 * @param inputFile
	 *          path in the file system
	 * @return
	 * @throws ErrorsInSpectraException
	 * @throws SpectraTranslationException 
	 */
	public static GameInput getGameInput(IFile specFile, String inputFileDummy, byte[] content) throws ErrorsInSpectraException, SpectraTranslationException, IOException {
		URI uri = URI.createPlatformResourceURI(inputFileDummy, true);
		IProject project = specFile.getProject();

		XtextResourceSetProvider resourceSetProvider = (XtextResourceSetProvider) IResourceServiceProvider.Registry.INSTANCE
				.getResourceServiceProvider(uri)
				.get(IResourceSetProvider.class);
		XtextResourceSet resourceSet = (XtextResourceSet) resourceSetProvider.get(project);

		Resource resource = resourceSet.createResource(uri);
		resource.load(new ByteArrayInputStream(content), resourceSet.getLoadOptions());
		
		Model spectraModel = (Model) resource.getContents().get(0);
		
		IResourceValidator validator = ((XtextResource) resource).getResourceServiceProvider().getResourceValidator();
		issues = validator.validate(resource, CheckMode.ALL, CancelIndicator.NullImpl);
		if (!issues.isEmpty()) {
			issues = errorsOnly(issues);
		}
		
		if (!issues.isEmpty()) {
			StringBuffer buf = new StringBuffer();
			for (Issue i : issues) {
				buf.append(i.getMessage() + " on line " + i.getLineNumber());
				buf.append(System.lineSeparator());
			}
			throw new ErrorsInSpectraException(buf.toString());
		}

		// translate to game input
		GameInput gi = Spectra2GameInputTranslator.translate(spectraModel);

		return gi;
	}

	/**
	 * 
	 * Loads a spectra model from a file and returns its root node of type Model
	 * 
	 * The path inputFile must be relative to the workspace.
	 * @param file 
	 * @return
	 * 
	 */
	public static Model getSpectraModel(IFile file) {
		URI uri = URI.createPlatformResourceURI(file.getFullPath().toString(), true);
		IProject project = file.getProject();

		XtextResourceSetProvider rsp = (XtextResourceSetProvider) IResourceServiceProvider.Registry.INSTANCE
				.getResourceServiceProvider(uri)
				.get(IResourceSetProvider.class);
		XtextResourceSet rs = (XtextResourceSet) rsp.get(project);

		Resource r = rs.getResource(uri, true);
		Model m = (Model) r.getContents().get(0);

		IResourceValidator validator = ((XtextResource) r).getResourceServiceProvider().getResourceValidator();
		issues = validator.validate(r, CheckMode.ALL, CancelIndicator.NullImpl);
		if (!issues.isEmpty()) {
			issues = errorsOnly(issues);
		}

		return m;
	}

	/**
	 * returns a new list of Issues that contains all issues with Severity.ERROR
	 * 
	 * @param all
	 *          list of mixed issues
	 * @return list of errors
	 */
	protected static List<Issue> errorsOnly(List<Issue> all) {
		List<Issue> es = new ArrayList<>();
		for (Issue i : all) {
			if (Severity.ERROR.equals(i.getSeverity())) {
				es.add(i);
			}
		}
		return es;
	}

	public static List<Issue> getIssues()
	{
		return issues;
	}
}
