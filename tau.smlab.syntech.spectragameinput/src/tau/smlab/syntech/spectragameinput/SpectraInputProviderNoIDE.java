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
import java.io.File;
import java.io.IOException;
import java.util.List;

import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.IResourceValidator;
import org.eclipse.xtext.validation.Issue;

import com.google.inject.Injector;

import tau.smlab.syntech.SpectraStandaloneSetup;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.spectra.Model;
import tau.smlab.syntech.spectragameinput.translator.Spectra2GameInputTranslator;

/**
 * This class will invoke the Spectra parser and a translator to obtain the game input
 * 
 */
public class SpectraInputProviderNoIDE {

  private static Injector injector;
  private XtextResourceSet resourceSet;
  private boolean ignoreValidators = false;

  /**
   * allows to disable validation
   * 
   * @param ignoreValidators
   *          true to never run validation of Spectra models
   */
  public SpectraInputProviderNoIDE(boolean ignoreValidators) {
    this();
    this.ignoreValidators = ignoreValidators;
  }

  /**
   * provider that runs validation of Spectra models on every load
   */
  public SpectraInputProviderNoIDE() {
    // do this only once per application
    if (injector == null) {
      injector = new SpectraStandaloneSetup().createInjectorAndDoEMFRegistration();
    }
    // obtain a resourceset from the injector
    resourceSet = injector.getInstance(XtextResourceSet.class);
  }

  public void addResource(String resourceFile) {
    resourceSet.getResource(URI.createFileURI(resourceFile), false);
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
  public GameInput getGameInput(String inputFile) throws ErrorsInSpectraException, SpectraTranslationException {

	// load a resource by URI, in this case from the file system
	Resource resource = resourceSet.getResource(URI.createFileURI(new File(inputFile).getAbsolutePath()), true);
	    
    // x-text AST root is of type Model
    Model spectraModel = getSpectraModel(resource);

    // translate to game input
    GameInput gi = Spectra2GameInputTranslator.translate(spectraModel);

    return gi;
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
  public GameInput getGameInput(String inputFileDummy, byte[] content) throws ErrorsInSpectraException, SpectraTranslationException, IOException {

	Resource resource = resourceSet.createResource(URI.createPlatformResourceURI(inputFileDummy, true));
	resource.load(new ByteArrayInputStream(content), resourceSet.getLoadOptions());
	  
    // x-text AST root is of type Model
    Model spectraModel = getSpectraModel(resource);

    // translate to game input
    GameInput gi = Spectra2GameInputTranslator.translate(spectraModel);

    return gi;
  }
  
  public Model getSpectraModel(String filePath) {
	// load a resource by URI, in this case from the file system
	Resource resource = resourceSet.getResource(URI.createPlatformResourceURI(filePath, true), true);
	    
    // x-text AST root is of type Model
    Model spectraModel = getSpectraModel(resource);
    
    return spectraModel;
  }

  /**
   * * The path inputFile can be relative to executed program or absolute in the file system
   * 
   * @param inputFile
   *          path in the file system
   * @return
   */
  private Model getSpectraModel(Resource resource) {

    Model m = (Model) resource.getContents().get(0);

    if (!ignoreValidators) {
      IResourceValidator validator = ((XtextResource) resource).getResourceServiceProvider().getResourceValidator();
      List<Issue> issues = validator.validate(resource, CheckMode.ALL, CancelIndicator.NullImpl);
      if (!issues.isEmpty()) {
        issues = SpectraInputProvider.errorsOnly(issues);
      }
      if (!issues.isEmpty()) {
        StringBuffer buf = new StringBuffer();
        for (Issue i : issues) {
          buf.append(i.getMessage());
          buf.append(System.lineSeparator());
        }
        throw new ErrorsInSpectraException(buf.toString());
      }
    }
    return m;
  }

}
