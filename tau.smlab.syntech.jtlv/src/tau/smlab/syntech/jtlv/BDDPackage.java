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

package tau.smlab.syntech.jtlv;

public enum BDDPackage {

	JTLV, CUDD, CUDD_ADD;

	public enum BBDPackageVersion {
		DEFAULT, CUDD_3_0, CUDD_3_0_PIPE;
	}

	private static BDDPackage curr = BDDPackage.JTLV;
	private static BBDPackageVersion ver = BBDPackageVersion.DEFAULT;

	public static BDDPackage getCurrPackage() {
		return curr;
	}

	public static BBDPackageVersion getCurrPackageVersion() {
		return ver;
	}

	public static String getCurrPackageSystemName(BDDPackage pack) {
		switch (pack) {
		case JTLV:
			return "jtlv";
		case CUDD:
			return "cudd";
		case CUDD_ADD:
			return "cuddadd";
		default:
			return "jtlv";
		}
	}

	public static String getCurrPackageVersionName(BBDPackageVersion ver) {
		switch (ver) {
		case DEFAULT:
			return "default";
		case CUDD_3_0:
			return "3.0";
		case CUDD_3_0_PIPE:
			return "3.0_PIPE";
		default:
			return "default";
		}
	}

	public static void setCurrPackage(BDDPackage pack, BBDPackageVersion packVer) {
		if (pack != null) {
			curr = pack;
		}
		try {
			// ////////////////////////////
			// setting BDD package
			String bddVerName;
			System.setProperty("bdd", getCurrPackageSystemName(pack));
			if (CUDD.equals(pack) || CUDD_ADD.equals(pack)) {
				bddVerName = getCurrPackageVersionName(packVer);
				ver = packVer;
			} else { // JTLV
				bddVerName = getCurrPackageVersionName(BBDPackageVersion.DEFAULT);
				ver = BBDPackageVersion.DEFAULT;
			}
			System.setProperty("bddver", bddVerName);
		} catch (Exception e) {
			System.err.println("failed loading the BDD library. Loading the" + " default JTLV BDD package");
			System.setProperty("bdd", "jtlv");
			curr = JTLV;
		}
		Env.resetEnv();
		// dummy BDD operation to invoke the factory
		Env.TRUE().and(Env.TRUE());

		CoreUtil.resetBDDPackage();
	}

	public static void setCurrPackage(BDDPackage pack) {
		setCurrPackage(pack, BBDPackageVersion.DEFAULT);
	}
};
