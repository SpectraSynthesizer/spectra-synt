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

package tau.smlab.syntech.games.rabin;

import java.util.Vector;

import net.sf.javabdd.BDD;
import tau.smlab.syntech.games.GameMemory;
import tau.smlab.syntech.jtlv.Env;

/**
 * winning staes of game memory are winning for the environment!
 *
 */
public class RabinMemory extends GameMemory {

	private Vector<BDD> zMem;
	private Vector<Vector<Vector<BDD>>> xMem;

	public RabinMemory() {
		zMem = new Vector<BDD>();
		xMem = new Vector<Vector<Vector<BDD>>>();
	}

	public RabinMemory(Vector<BDD> zMem, Vector<Vector<Vector<BDD>>> xMem) {
		this.zMem = zMem;
		this.xMem = xMem;
	}

	public int sizeZ() {
		return zMem.size();
	}

	public int sizeX(int cz, int k) {
		return xMem.get(cz).get(k).size();
	}

	public boolean addZ(BDD z) {
		return zMem.add(z);
	}

	public BDD getZ(int zi) {
		return zMem.get(zi);
	}

	public BDD getX(int layer, int justice, int xi) {
		return xMem.get(layer).get(justice).get(xi);
	}

	public void addXLayer(int width) {
		Vector<Vector<BDD>> layer = new Vector<Vector<BDD>>();
		for (int i = 0; i < width; i++)
			layer.add(new Vector<BDD>());
		xMem.add(layer);
	}

	/**
	 * deletes all vectors in the MOST RECENT x layer and frees their BDDs
	 */
	public void clearXLayer() {
		if (xMem != null && xMem.size() > 0) {
			Vector<Vector<BDD>> layer = xMem.lastElement();
			for (int i = 0; i < layer.size(); i++) {
				Env.free(layer.get(i));
				layer.get(i).clear();
			}
		}
	}

	public boolean addX(int justice, BDD x) {
		return xMem.lastElement().get(justice).add(x);
	}

	public int getZRank(BDD s) {
		for (int i = 0; i < zMem.size(); i++)
			if (!(zMem.get(i).and(s).isZero()))
				return i;
		throw new NullPointerException("the following state is not in the"
				+ " Z memory (is it winning for the environment?)\n" + Env.toNiceSignleLineString(s));
	}

	public int getXRank(int zRank, int k, BDD s) {
		// int zRank = getZRank(s);
		Vector<BDD> relevantMem = xMem.get(zRank).get(k);
		for (int i = 0; i < relevantMem.size(); i++)
			if (!(relevantMem.get(i).and(s).isZero()))
				return i;
		throw new NullPointerException("the following state is not in the" + " the X memory (something is wrong... )\n"
				+ Env.toNiceSignleLineString(s));
	}

	/**
	 * frees all BDDs in the x and z memory
	 */
	@Override
	public void free() {
		super.free();
		Env.free(xMem);
		Env.free(zMem);
	}

	public Vector<BDD> getZMem() {
		return zMem;
	}

	public Vector<Vector<Vector<BDD>>> getXMem() {
		return xMem;
	}
}
