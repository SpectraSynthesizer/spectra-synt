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

//package tau.smlab.syntech.ui.datapreparation;
//
//import java.io.InputStream;
//import java.util.ArrayList;
//import java.util.List;
//import java.util.Scanner;
//
//import org.eclipse.core.resources.IFile;
//import org.eclipse.core.runtime.CoreException;
//import org.eclipse.core.runtime.Path;
//
//import tau.smlab.syntech.games.controller.util.FilesUtils;
//
///**
// * 
// * Prepares the data before sending it to the database using performancemonitor
// * plug in.
// */
//public class DataPreparation {
//
//	/**
//	 * 
//	 * @return A list of Files contains the given main spectra file, and all of the
//	 *         dependencies (imported .spectra files). If no dependencies, the list
//	 *         would contain only the given spectra file.
//	 */
//	private static List<IFile> getSpectraFiles(IFile mainSpecfile) {
//		List<IFile> specsFilesList = new ArrayList<>();
//
//		// add main spec
//		specsFilesList.add(mainSpecfile);
//
//		// get imports list
//		List<String> importedSpecsPaths = getImportedSpecsPaths(mainSpecfile);
//
//		// add dependencies to list
//		for (String relativePath : importedSpecsPaths) {
//			IFile file = mainSpecfile.getParent().getFile(new Path(relativePath));
//			if (file.exists()) {
//				specsFilesList.add(file);
//			}
//		}
//
//		return specsFilesList;
//	}
//
//	public static InputStream getSpectraFilesAsZip(IFile mainSpecFile) throws Exception {
//		List<IFile> files = getSpectraFiles(mainSpecFile);
//		List<InputStream> inputStreamsList = FilesUtils.getISList(files);
//		List<String> fileNames = FilesUtils.getFileNames(files);
//		InputStream inputStreamZip = FilesUtils.compress(inputStreamsList, fileNames);
//		return inputStreamZip;
//	}
//
//	/**
//	 * scan mainSpec word by word until we reach out 'module'. (line by line is not
//	 * good enough because "import ".." module.." in one line is legal too)
//	 */
//	private static List<String> getImportedSpecsPaths(IFile mainSpecfile) {
//		List<String> importsList = new ArrayList<>();
//
//		Scanner lineByLineScanner = null;
//		Scanner wordByWordScanner = null;
//		String lastSeenPath = "";
//		try {
//			lineByLineScanner = new Scanner(mainSpecfile.getContents(true));
//		} catch (CoreException e) {
//			e.printStackTrace();
//		}
//		while (lineByLineScanner.hasNextLine()) {
//			wordByWordScanner = new Scanner(lineByLineScanner.nextLine());
//			while (wordByWordScanner.hasNext()) {
//				String word = wordByWordScanner.next();
//				if (word.equals("module")) {
//					wordByWordScanner.close();
//					lineByLineScanner.close();
//					if (!lastSeenPath.equals("")) {
//						importsList.add(lastSeenPath);
//					}
//					return importsList;
//				} else if (word.equals("import")) {
//					if (!lastSeenPath.equals("")) {
//						importsList.add(lastSeenPath);
//						lastSeenPath = ""; // 'import' seen -> reset string.
//					}
//				} else {
//					int firstQuotationMarkPos = word.indexOf('"');
//					int lastQuotationMarkPos = word.lastIndexOf('"');
//					if (word.length() > 2 && firstQuotationMarkPos != -1
//							&& firstQuotationMarkPos != lastQuotationMarkPos) {
//						String path = word.substring(firstQuotationMarkPos + 1, lastQuotationMarkPos);
//						lastSeenPath += path;
//					}
//				}
//			}
//		}
//		// no 'module' at all
//		wordByWordScanner.close();
//		lineByLineScanner.close();
//		return importsList;
//	}
//}
