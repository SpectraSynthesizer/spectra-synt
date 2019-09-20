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

package tau.smlab.syntech.games;

import java.util.Vector;

import net.sf.javabdd.BDD;

public abstract class GameIncrementalMemory {
    
    public boolean NEW_INI_ADDED = false;
    public boolean NEW_SAFETY_ADDED = false;
    public boolean NEW_JUSTICE_ADDED = false;
    public boolean PREV_INI_REMOVED = false;
    public boolean PREV_SAFETY_REMOVED = false;
    public boolean PREV_JUSTICE_REMOVED = false;
    
    public int leastRemovedJusticeIdx = 0;
    
    public BDD startZ = null;
        
    abstract public void addToStartZ(BDD z, int idx);
        
    public int getIncTypeBitmap()
    {
      int typeBitmap = 0;
//      NOTE: the bitmap needs to correspond to the enum values in the C code.
//      typedef enum INC_TYPE {
//        INC_TYPE_NO_INC = 0,
//        INC_TYPE_NEW_INI_ADDED = 1,
//        INC_TYPE_NEW_SAFETY_ADDED =2,
//        INC_TYPE_NEW_JUSTICE_ADDED = 4,
//        INC_TYPE_PREV_INI_REMOVED = 8,
//        INC_TYPE_PREV_SAFETY_REMOVED = 16,
//        INC_TYPE_PREV_JUSTICE_REMOVED = 32
//      } inc_type;
      if (NEW_INI_ADDED) typeBitmap |= 1;
      if (NEW_SAFETY_ADDED) typeBitmap |= 2;
      if (NEW_JUSTICE_ADDED) typeBitmap |= 4;
      if (PREV_INI_REMOVED) typeBitmap |= 8;
      if (PREV_SAFETY_REMOVED) typeBitmap |= 16;
      if (PREV_JUSTICE_REMOVED) typeBitmap |= 32;
      
      return typeBitmap;
    }
    
    abstract public void setPrevFirstZIterMem(BDD[] prevFirstZIterMem);
    abstract public void setPrevZMemory(BDD[] prevMem);
    abstract public void setPrevXMem(BDD[][][] xMem);
    abstract public void setPrevMem(Vector<BDD> zMem, Vector<Vector<Vector<BDD>>> xMem);
    
}
