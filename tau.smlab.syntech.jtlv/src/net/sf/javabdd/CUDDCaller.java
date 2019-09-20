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

package net.sf.javabdd;

import net.sf.javabdd.BDDFactory.ReorderMethod;

public interface CUDDCaller {
	
	public void initialize0Caller(int nodenum, int cachesize);
	public boolean isInitialized0Caller();
	public void done0Caller();
	public int varNum0Caller();
	public int setVarNum0Caller(int num, boolean ADD);
	public long ithVar0Caller(int var, boolean ADD);
	public int level2Var0Caller(int level);
	public int var2Level0Caller(int var);
	public void setVarOrder0Caller(int[] neworder);
	public int getAllocNum0Caller();
	public int getNodeNum0Caller();
	public int getCacheSize0Caller();
	public int var0Caller(long b);
	public long high0Caller(long b, boolean ADD);
	public long low0Caller(long b, boolean ADD);
	public long not0Caller(long b, boolean ADD);
	public long ite0Caller(long b, long c, long d, boolean ADD);
	public long relprod0Caller(long b, long c, long d);
	public long compose0Caller(long b, long c, int var, boolean ADD);
	public long exist0Caller(long b, long c, boolean ADD);
	public long forAll0Caller(long b, long c, boolean ADD);
	public long restrict0Caller(long b, long var);
	public long restrictWith0Caller(long b, long c, boolean deref_other);
	public long simplify0Caller(long b, long var, boolean ADD);
	public long support0Caller(long b, boolean is_add);
	public long apply0Caller(long b, long c, int opr, boolean ADD, boolean apply_with, boolean deref_other);
	public long satOne0Caller(long b, long c, boolean ADD);
	public int nodeCount0Caller(long b);
	public double pathCount0Caller(long b);
	public double satCount0Caller(long b);
	public void addRefCaller(long p);
	public void delRefCaller(long p, boolean ADD);
	public long veccompose0Caller(long b, long p, boolean ADD);
	public long replace0Caller(long b, long p, boolean ADD);
	public long allocCaller(boolean ADD);
	public void set0Caller(long p, int oldvar, int newvar, boolean ADD);
	public void set2Caller(long p, int oldvar, long newbdd);
	public void reset0Caller(long ptr, boolean ADD);
	public void free0Caller(long ptr);
	public boolean isZeroOneADD0Caller(long ptr);
	public long addConst0Caller(double constval);
	public boolean isAddConst0Caller(long ptr);
	public long addFindMax0Caller(long ptr);
	public long addFindMin0Caller(long ptr);
	public double retrieveConstValue0Caller(long ptr);
	public long addAdditiveNeg0Caller(long ptr);
	public long addApplyLog0Caller(long ptr);
	public void reorder0Caller(ReorderMethod method);
	public void autoReorder0Caller(ReorderMethod method, boolean setmax, int maxval);
	public ReorderMethod getreordermethod0Caller();
	public void autoReorder1Caller();
	public int reorderVerbose0Caller(int v);
	public void addVarBlock0Caller(int first, int last, boolean fixed);
	public void clearVarBlocks0Caller();
	public void printStat0Caller();
	public long toADD0Caller(long b);
	public long toBDD0Caller(long b);
	public long toBDDThresholdCaller(long b, double threshold);
	public long toBDDStrictThresholdCaller(long b, double threshold);
	public long toBDDIntervalCaller(long b, double lower, double upper);
	public long toBDDIthBitCaller(long b, int bit);
	public int[] varSupportIndex0Caller(long b);
	public void printSet0Caller(long b, int printMode);
	public long logicZero0Caller();
	public long arithmeticZero0Caller();
	public long arithmeticLogicOne0Caller();
	public long arithmeticPlusInfinity0Caller();
	public long arithmeticMinusInfinity0Caller();
	public long replaceWith0Caller(long b, long c, boolean ADD);
	public long addMinimizeVal0Caller(long ptr, double val);
	public long addAbstractMin0Caller(long b, long c);
	public long addAbstractMax0Caller(long b, long c);
	public int reorderTimes0Caller();
	public void printVarTree0Caller();
	public long addEngSubtract0Caller(long a, long b, double maxEnergy);
	public long convertToZeroOneADDByThres0Caller(long a, long opType, double thresValue);
	public void printDot0Caller(long a);
	public long arithmeticExist0Caller(long b, long c);
	public long determinizeController0Caller(long b, long c);
	public int getSize0Caller(long b);
	public long getBddZero();
	public long getAddZero();
	public long getManager();
	public long getOne();
  public boolean reorderEnabled0Caller();
}
