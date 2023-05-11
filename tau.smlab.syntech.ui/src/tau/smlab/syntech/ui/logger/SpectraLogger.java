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

package tau.smlab.syntech.ui.logger;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.time.Instant;
import java.util.HashMap;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IPath;

import tau.smlab.syntech.ui.preferences.PreferencePage;

public class SpectraLogger {
	public static void logOperationStart(IFile specFile, String actionID) {
		if (PreferencePage.isLoggerActive()) {
			long timeStamp = Instant.now().getEpochSecond();
			SpectraLogger.log(specFile, "TimeStamp-" + timeStamp + "-Start-" + actionID, timeStamp);
		}
	}

	public static void logOperationDone(IFile specFile, String actionID, long time, boolean status) {
		if (PreferencePage.isLoggerActive()) {
			long timeStamp = Instant.now().getEpochSecond();
			SpectraLogger.log(specFile,
					"TimeStamp-" + timeStamp + "-Done-" + actionID + "-Took-" + time + "ms-Status-" + status,
					timeStamp);
		}
	}

	public static void logBuffer(IFile specFile, String actionID, String buffer) {
		if (PreferencePage.isLoggerActive()) {
			long timeStamp = Instant.now().getEpochSecond();
			SpectraLogger.log(specFile, "TimeStamp-" + timeStamp + "-Operation-" + actionID + "-Buffer-" + buffer,
					timeStamp);
		}
	}

	public static void log(IFile specFile, String str, long timeStamp) {
		IPath log_dir_path = specFile.getLocation().removeLastSegments(1).append("/spectra-logs");
		File log_dir = log_dir_path.toFile();
		if (!log_dir.exists()) {
			log_dir.mkdir();
		}

		IPath spec_log_dir_path = log_dir_path.append("/" + specFile.getFullPath().removeFileExtension().lastSegment());
		File dir = spec_log_dir_path.toFile();
		if (!dir.exists()) {
			dir.mkdir();
		}

		FileHandler handler = null;
		try {
			handler = new FileHandler(dir.getAbsolutePath().concat("/" + timeStamp + ".spectraXML"), true);
			handler.setFormatter(new SpectraLoggerFormatter());
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		Logger logger = Logger.getLogger(specFile.getLocation().toString());
		logger.addHandler(handler);

		saveNewSpecificationCopy(specFile, timeStamp, spec_log_dir_path);
		logger.info(str);
		handler.close();
	}

	private static void saveNewSpecificationCopy(IFile specFile, long timeStamp, IPath spec_log_dir_path) {
		FileInputStream in = null;
		FileOutputStream out = null;
		try {
			File myObj = spec_log_dir_path.append(timeStamp + ".spectraArxived").toFile();

			myObj.createNewFile();
			FileWriter myWriter = null;
			in = new FileInputStream(specFile.getLocation().toFile());
			out = new FileOutputStream(myObj);

			int n;

			while ((n = in.read()) != -1) {
				out.write(n);
			}
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				if (in != null) {
					in.close();
				}
				if (out != null) {
					out.close();
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
}
