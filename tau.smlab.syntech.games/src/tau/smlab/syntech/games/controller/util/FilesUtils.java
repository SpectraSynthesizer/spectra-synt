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

package tau.smlab.syntech.games.controller.util;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

public class FilesUtils {

	public static InputStream getIS(String string) {
		InputStream is = new ByteArrayInputStream(string.getBytes());
		return is;
	}

	/**
	 * List of input streams
	 */
	public static void compress(String zipName, List<InputStream> inputStreamsList, List<String> fileNames) throws IOException {
		byte data[] = new byte[2048];
		FileOutputStream fos = new FileOutputStream(zipName);
		ZipOutputStream zos = new ZipOutputStream(fos);
		for (int i = 0; i < inputStreamsList.size(); i++) {
			BufferedInputStream entryStream = new BufferedInputStream(inputStreamsList.get(i), 2048);
			ZipEntry entry = new ZipEntry(fileNames.get(i));
			zos.putNextEntry(entry);
			int count;
			while ((count = entryStream.read(data, 0, 2048)) != -1) {
				zos.write(data, 0, count);
			}
			entryStream.close();
			zos.closeEntry();
		}

		zos.close();
		fos.close();

		for (InputStream is : inputStreamsList) {
			is.close();
		}
	}

//	/**
//	 * obtain input streams from files
//	 * 
//	 * @param filesList
//	 * @return
//	 * @throws CoreException
//	 */
//	public static List<InputStream> getISList(List<IFile> filesList) throws CoreException {
//		List<InputStream> inputStreamsList = new ArrayList<>();
//
//		for (IFile file : filesList) {
//			inputStreamsList.add(file.getContents(true));
//		}
//		return inputStreamsList;
//	}

	/**
	 * Single input stream
	 */
	public static void compress(String zipName, InputStream inputStreamFile, String fileName) throws IOException {
		List<InputStream> inputsList = new ArrayList<>();
		inputsList.add(inputStreamFile);

		List<String> namesList = new ArrayList<>();
		namesList.add(fileName);

		compress(zipName, inputsList, namesList);
	}

//	/**
//	 * return list of file names relative to workspace
//	 * 
//	 * @param files
//	 * @return
//	 */
//	public static List<String> getFileNames(List<IFile> files) {
//		List<String> fileNames = new ArrayList<>();
//
//		for (IFile file : files) {
//			fileNames.add(getWSName(file));
//		}
//		return fileNames;
//	}
//
//	/**
//	 * get file name in workspace as a String (omits leading separators)
//	 * 
//	 * @param file
//	 * @return
//	 */
//	public static String getWSName(IFile file) {
//		String pathInWs = file.getFullPath().toString();
//		// check for leading separator (always "/" see toString() above)
//		if (pathInWs.startsWith("/")) {
//			pathInWs = pathInWs.substring(1);
//		}
//		return pathInWs;
//	}

}
