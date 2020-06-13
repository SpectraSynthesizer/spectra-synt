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

package tau.smlab.syntech.games.util;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Map;

import tau.smlab.syntech.gamemodel.GameModel;
import tau.smlab.syntech.gamemodel.PlayerModule;
import tau.smlab.syntech.jtlv.Env;
import tau.smlab.syntech.jtlv.env.module.ModuleBDDField;

public class SaveLoadWithDomains {
	
	public static final String VARS_FILE = "vars.doms";
	
	public static void saveStructureAndDomains(String file, GameModel m) throws IOException {
		FileOutputStream fos = new FileOutputStream(file);
		ObjectOutputStream oos = new ObjectOutputStream(fos);
		saveStructureAndDomains(oos, m);
		oos.close();
	}

	/**
	 * stores the variables of the players in the game in a file together with a
	 * list of their values
	 * 
	 * @param file
	 * @param m
	 * @throws IOException
	 */
	public static void saveStructureAndDomains(ObjectOutputStream oos, GameModel m) throws IOException {

		ArrayList<FieldWithName> fields = new ArrayList<FieldWithName>();
		for (ModuleBDDField f : m.getSys().getAllFields()) {
			fields.add(new FieldWithName(m.getSys().getName(), f));
		}
		for (ModuleBDDField f : m.getEnv().getAllFields()) {
			fields.add(new FieldWithName(m.getEnv().getName(), f));
		}

		Collections.sort(fields);
		ArrayList<String[]> fieldValues = new ArrayList<String[]>();

		for (FieldWithName f : fields) {
			String[] vals = new String[f.field.getDomain().size().intValue() + 2];
			vals[0] = f.moduleName;
			vals[1] = f.field.getName();
			for (int i = 2; i < vals.length; i++) {
				vals[i] = Env.stringer.elementName(f.field.getDomain(), new BigInteger("" + (i - 2)));
			}
			fieldValues.add(vals);
		}

		oos.writeObject(fieldValues);
	}
	
	public static void loadStructureAndDomains(String file, Map<String, String[]> sysVars, Map<String, String[]> envVars) throws IOException {
		
		ArrayList<String[]> fieldValues = loadFieldValues(file);
		loadStructureAndDomains(fieldValues, sysVars, envVars);
	}

	/**
	 * loads all variables and their domains from the given file
	 * 
	 * @param file
	 * @throws IOException
	 */
	public static void loadStructureAndDomains(ObjectInputStream ois, Map<String, String[]> sysVars, Map<String, String[]> envVars) throws IOException {

		ArrayList<String[]> fieldValues = loadFieldValues(ois);
		loadStructureAndDomains(fieldValues, sysVars, envVars);
	}
	
	private static void loadStructureAndDomains(ArrayList<String[]> fieldValues, Map<String, String[]> sysVars, Map<String, String[]> envVars) {
		PlayerModule mod = new PlayerModule();
		for (String[] module_field_and_vals : fieldValues) {
			String[] vals = new String[module_field_and_vals.length - 2];
			System.arraycopy(module_field_and_vals, 2, vals, 0, vals.length);
			try {
				mod.addVar(module_field_and_vals[1], vals, false);
			} catch (Exception e) {
				e.printStackTrace();
			}
			if (isSys(module_field_and_vals[0])) {
				sysVars.put(module_field_and_vals[1], vals);
			}
			if (isEnv(module_field_and_vals[0])) {
				envVars.put(module_field_and_vals[1], vals);
			}
		}
	}
	
	@SuppressWarnings("unchecked")
	private static ArrayList<String[]> loadFieldValues(String file) throws FileNotFoundException, IOException {
		FileInputStream fis = new FileInputStream(file);
		ObjectInputStream ois = new ObjectInputStream(fis);
		ArrayList<String[]> fieldValues = null;
		try {
			fieldValues = (ArrayList<String[]>) ois.readObject();
		} catch (ClassNotFoundException e1) {
			e1.printStackTrace();
		}
		ois.close();
		return fieldValues;
	}

	@SuppressWarnings("unchecked")
	private static ArrayList<String[]> loadFieldValues(ObjectInputStream ois) throws FileNotFoundException, IOException {
		ArrayList<String[]> fieldValues = null;
		try {
			fieldValues = (ArrayList<String[]>) ois.readObject();
		} catch (ClassNotFoundException e1) {
			e1.printStackTrace();
		}
		return fieldValues;
	}
//
//	/**
//	 * load names of system variables
//	 * 
//	 * @param file
//	 * @return
//	 * @throws IOException
//	 */
//	public static Set<String> loadSysVarNames(ObjectInputStream ois) throws IOException {
//		Set<String> sysVarNames = new HashSet<>();
//
//		ArrayList<String[]> fieldValues = loadFieldValues(ois);
//
//		for (String[] module_field_and_vals : fieldValues) {
//			if (isSys(module_field_and_vals[0])) {
//				sysVarNames.add(module_field_and_vals[1]);
//			}
//		}
//		return sysVarNames;
//	}
//
//	/**
//	 * load names of environment variables
//	 * 
//	 * @param file
//	 * @return
//	 * @throws IOException
//	 */
//	public static Set<String> loadEnvVarNames(ObjectInputStream ois) throws IOException {
//		Set<String> envVarNames = new HashSet<>();
//
//		ArrayList<String[]> fieldValues = loadFieldValues(ois);
//
//		for (String[] module_field_and_vals : fieldValues) {
//			if (isEnv(module_field_and_vals[0])) {
//				envVarNames.add(module_field_and_vals[1]);
//			}
//		}
//		return envVarNames;
//	}
//
	private static boolean isEnv(String string) {
		if (string.length() > 4) {
			string = string.substring(string.length() - 4);
			if (string.equals("_env")) {
				return true;
			}
		}
		return false;
	}

	private static boolean isSys(String string) {
		if (string.length() > 4) {
			string = string.substring(string.length() - 4);
			if (string.equals("_sys")) {
				return true;
			}
		}
		return false;
	}

}
