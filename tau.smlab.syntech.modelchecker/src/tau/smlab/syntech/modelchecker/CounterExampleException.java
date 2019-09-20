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

package tau.smlab.syntech.modelchecker;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.jtlv.Env;;

/**
 * An exception dedicated for counter examples.
 */
public class CounterExampleException extends ModelCheckException {
	private static final long serialVersionUID = 1L;
	private BDD[] path;

	public CounterExampleException(String desc, BDD[] path) {
		super(desc);
		this.path = path;
	}

	public BDD[] getPath() {
		return this.path;
	}

	public String toString() {
		String res = super.toString() + "\n";
		if ((path == null) || (path.length == 0))
			return res + "No Counter Example Exists.";
		res += " Counter Example\n";
		res += "=================\n";
		// the last is not printed. It is only to point to the cycle.
		BDD last = path[path.length - 1];
		int loop_index = -1;
		boolean loop_exists = false;
		for (int i = 0; i < path.length - 1; i++) {
			boolean loop_here = path[i].biimp(last).isOne();
			if ((loop_here) && (loop_index == -1)) {
				loop_index = i + 1;
				res += "[[" + (i + 1) + "]]";
				loop_exists = true;
			} else {
				res += "  " + (i + 1) + "  ";
			}
			res += " \t: " + Env.toNiceSignleLineString(path[i]) + "\n";
		}
		if (loop_exists)
			res += "Loop back to state " + loop_index;
		else
			res += "  " + path.length + "   \t: "
					+ Env.toNiceSignleLineString(path[path.length - 1]) + "\n";

		return res;
	}
}
