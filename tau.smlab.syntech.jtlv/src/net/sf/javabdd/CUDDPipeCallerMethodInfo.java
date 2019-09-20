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

import java.util.List;

import net.sf.javabdd.BDDFactory.ReorderMethod;

public class CUDDPipeCallerMethodInfo {

  public static final int MAX_METHOD_ARGS = 8;  /*maximum number of arguments that all library functions have*/
  
  public static class ReturnValue<T> {

    protected T value;

    public ReturnValue() {
      this.value = null;
    }

    public ReturnValue(T value) {
      this.value = value;
    }

    public void setValue(T value) {
      this.value = value;
    }

    public T getValue() {
      return this.value;
    }
  }

  public static class ReturnIntValue extends 
  ReturnValue <Integer> {

    public ReturnIntValue(int val) {
      this.value = val;
    }

  }

  public static class ReturnLongValue extends 
  ReturnValue <Long> {

    public ReturnLongValue(long val) {
      this.value = val;
    }
  }

  public static class ReturnBooleanValue extends 
  ReturnValue <Boolean> {

    public ReturnBooleanValue(boolean val) {
      this.value = val;
    }
  }

  public static class ReturnDoubleValue extends 
  ReturnValue <Double> {

    public ReturnDoubleValue(double val) {
      this.value = val;
    }
  }

  public static class ReturnReorderMethodValue extends 
  ReturnValue <ReorderMethod> {

    public ReturnReorderMethodValue(ReorderMethod val) {
      this.value = val;
    }
  }

  public static class ReturnIntListValue extends 
  ReturnValue <List<Integer>> {

    public ReturnIntListValue(List<Integer> val) {
      this.value = val;
    }

    public int[] toPrimitiveIntArray() {
      int size = this.value.size();
      Integer[] arr = new Integer[size];
      this.value.toArray(arr);
      int[] converted =  new int[size];
      for(int i = 0; i < size; i++) {
        converted[i] = arr[i];
      }
      return converted;
    }

  }

  public static class ReturnLongListValue extends 
  ReturnValue <List<Long>> {

    public ReturnLongListValue(List<Long> val) {
      this.value = val;
    }
  }

  public static class ReturnDoubleListValue extends 
  ReturnValue <List<Double>> {

    public ReturnDoubleListValue(List<Double> val) {
      this.value = val;
    }
  }


  
  public static enum ReturnType {
	    BOOLEAN(0,4), INTEGER(1,4), LONG(2,8),  DOUBLE(3,8), VOID(4,4), INT_LIST(5,4), LONG_LIST(6,8), DOUBLE_LIST(7,8), REORDER_METHOD(8,4);

	    private final int id;
	    private final int numBytes;
	    private ReturnType(int id, int numBytes) {
	        this.id = id;
	        this.numBytes = numBytes;
	    }

	    public int getId() {
	        return id;
	    }
	    public int getnumBytes() {
	        return numBytes;
	    }
	    
	    public static ReturnType resolveReturnType(Class<?> type) {
	      if(type.equals(Boolean.class) || type.equals(boolean.class)) {
	        return BOOLEAN;
	      }
	      if(type.equals(Integer.class) || type.equals(int.class)) {
          return INTEGER;
        }
	      if(type.equals(Long.class) || type.equals(long.class)) {
          return LONG;
        }
	      if(type.equals(Double.class) || type.equals(double.class)) {
          return DOUBLE;
        }
	      if(type.equals(Integer[].class) || type.equals(int[].class)) {
          return INT_LIST;
        }
	      if(type.equals(Long[].class) || type.equals(long[].class)) {
          return LONG_LIST;
        }
	      if(type.equals(Double[].class) || type.equals(double[].class)) {
          return DOUBLE_LIST;
        }
	      if(type.equals(ReorderMethod.class)) {
          return REORDER_METHOD;
        }
	      return VOID;
	    }
	    
	}


  public static enum MethodEntry {

    INITIALIZE0("initialize0", 1, 2, ReturnType.LONG_LIST),
    IS_INITIALIZED0("isInitialized0", 2, 0, ReturnType.BOOLEAN),
    DONE0("done0", 3, 0, ReturnType.VOID),
    VAR_NUM0("varNum0", 4, 0, ReturnType.INTEGER),
    GET_SIZE0("getSize0", 5, 1, ReturnType.INTEGER),
    SET_VAR_NUM0("setVarNum0", 6, 2, ReturnType.INTEGER),
    ITH_VAR0("ithVar0", 7, 2, ReturnType.LONG),
    LEVEL2_VAR0("level2Var0", 8, 1, ReturnType.INTEGER),
    VAR2_LEVEL0("var2Level0", 9, 1, ReturnType.INTEGER),
    SET_VAR_ORDER0("setVarOrder0", 10, 1, ReturnType.VOID),
    GET_ALLOC_NUM0("getAllocNum0", 11, 0, ReturnType.INTEGER),
    GET_NODE_NUM0("getNodeNum0", 12, 0, ReturnType.INTEGER),
    GET_CACHE_SIZE0("getCacheSize0", 13, 0, ReturnType.INTEGER),
    VAR0("var0", 14, 1, ReturnType.INTEGER),
    HIGH0("high0", 15, 2,  ReturnType.LONG),
    LOW0("low0", 16, 2, ReturnType.LONG),
    NOT0("not0", 17, 2, ReturnType.LONG),
    ITE0("ite0", 18, 4, ReturnType.LONG),
    RELPROD0("relprod0", 19, 3, ReturnType.LONG),
    COMPOSE0("compose0", 20, 4, ReturnType.LONG),
    EXIST0("exist0", 21, 3, ReturnType.LONG),
    FOR_ALL0("forAll0", 22, 3, ReturnType.LONG),
    SIMPLIFY0("simplify0", 23, 3, ReturnType.LONG),
    RESTRICT0("restrict0", 24, 2, ReturnType.LONG),
    RESTRICT_WITH0("restrictWith0", 25, 3, ReturnType.LONG),
    SUPPORT0("support0", 26, 2, ReturnType.LONG),
    APPLY0("apply0", 27, 6, ReturnType.LONG),
    SAT_ONE0("satOne0", 28, 3, ReturnType.LONG), //changed by Or: 3rd arg: 2 -> 3
    NODE_COUNT0("nodeCount0", 29, 1, ReturnType.INTEGER),
    PATH_COUNT0("pathCount0", 30, 1, ReturnType.DOUBLE),
    SAT_COUNT0("satCount0", 31, 1, ReturnType.DOUBLE),
    ADD_REF("addRef", 32, 1, ReturnType.VOID),
    DEL_REF("delRef", 33, 2, ReturnType.VOID),
    VECCOMPOSE0("veccompose0", 34, 3, ReturnType.LONG),
    REPLACE0("replace0", 35, 3, ReturnType.LONG),
    ALLOC("alloc", 36, 1, ReturnType.LONG),
    SET0("set0", 37, 4, ReturnType.VOID),
    SET2("set2", 38, 3, ReturnType.VOID),
    RESET0("reset0", 39, 2, ReturnType.VOID),
    FREE0("free0", 40, 2, ReturnType.VOID),
    IS_ZERO_ONE_ADD0("isZeroOneADD0", 41, 1, ReturnType.BOOLEAN),
    ADD_CONST0("addConst0", 42, 1, ReturnType.LONG),
    IS_ADD_CONST0("isAddConst0", 43, 1, ReturnType.BOOLEAN),
    ADD_FIND_MAX0("addFindMax0", 44, 1, ReturnType.LONG),
    ADD_FIND_MIN0("addFindMin0", 45, 1, ReturnType.LONG),
    RETRIEVE_CONST_VALUE0("retrieveConstValue0", 46, 1, ReturnType.DOUBLE),
    ADD_ADDITIVE_NEG0("addAdditiveNeg0", 47, 1, ReturnType.LONG),
    ADD_APPLY_LOG0("addApplyLog0", 48, 1, ReturnType.LONG),
    REORDER0("reorder0", 49, 1, ReturnType.VOID),
    AUTO_REORDER0("autoReorder0", 50, 3, ReturnType.VOID),
    GETREORDERMETHOD0("getreordermethod0", 51, 0, ReturnType.INTEGER),
    AUTO_REORDER1("autoReorder1", 52, 0, ReturnType.VOID),
    REORDER_VERBOSE0("reorderVerbose0", 53, 1, ReturnType.INTEGER),
    ADD_VAR_BLOCK0("addVarBlock0", 54, 3, ReturnType.VOID),
    CLEAR_VAR_BLOCKS0("clearVarBlocks0", 55, 0, ReturnType.VOID),
    PRINT_STAT0("printStat0", 56, 0, ReturnType.VOID),
    TO_ADD0("toADD0", 57, 1, ReturnType.LONG),
    TO_BDD0("toBDD0", 58, 1, ReturnType.LONG),
    TO_BDD_THRESHOLD("toBDDThreshold", 59, 2, ReturnType.LONG),
    TO_BDD_STRICT_THRESHOLD("toBDDStrictThreshold", 60, 2, ReturnType.LONG),
    TO_BDD_INTERVAL("toBDDInterval", 61, 3, ReturnType.LONG),
    TO_BDD_ITH_BIT("toBDDIthBit", 62, 2, ReturnType.LONG),
    VAR_SUPPORT_INDEX0("varSupportIndex0", 63, 1, ReturnType.INT_LIST),
    PRINT_SET0("printSet0", 64, 2, ReturnType.VOID),
    ARITHMETIC_ZERO0("arithmeticZero0", 65, 0, ReturnType.LONG),
    LOGIC_ZERO0("logicZero0", 66, 0, ReturnType.LONG),
    REPLACE_WITH0("replaceWith0", 67, 3, ReturnType.LONG),
    ADD_ABSTRACT_MIN0("addAbstractMin0", 68, 2, ReturnType.LONG),   
    ADD_ABSTRACT_MAX0("addAbstractMax0", 69, 2, ReturnType.LONG),
    ARITHMETIC_MINUS_INFINITY0("arithmeticMinusInfinity0", 70, 0, ReturnType.LONG), 
    ARITHMETIC_PLUS_INFINITY0("arithmeticPlusInfinity0", 71, 0, ReturnType.LONG),
    DETERMINIZE_CONTROLLER0("determinizeController0", 72, 2, ReturnType.LONG),
    REORDER_ENABLED0("reorderEnabled0", 73, 0, ReturnType.BOOLEAN), //Or added this method instead of negCycleCheck0!!!
    ARITHMETIC_LOGIC_ONE0("arithmeticLogicOne0", 74, 0, ReturnType.LONG), 
    ADD_MINIMIZE_VAL0("addMinimizeVal0", 75, 2, ReturnType.LONG),
    REORDER_TIMES0("reorderTimes0", 76, 0, ReturnType.INTEGER),  
    PRINT_VAR_TREE0("printVarTree0", 77, 0, ReturnType.VOID),
    ADD_ENG_SUBTRACT0("addEngSubtract0", 78, 3, ReturnType.LONG), 
    CONVERT_TO_ZERO_ONE_ADD_BY_THRES0("convertToZeroOneADDByThres0", 79, 3, ReturnType.LONG),       
    PRINT_DOT0("printDot0", 80, 1, ReturnType.VOID),
    ARITHMETIC_EXIST0("arithmeticExist0", 81, 2, ReturnType.LONG);

    private final String name; /*method name*/
    private final int methodIndex, argNum; /*method index, number of arguments (BOTH must be consistent with the native code!)*/
    private final ReturnType retType; /*return type (must be consistent with the native code!)*/

    private MethodEntry(String name, int methodIndex, int argNum, ReturnType retType) {
      this.name = name;
      this.methodIndex = methodIndex;
      this.argNum = argNum;
      this.retType = retType;
    }

    public String getName() {
      return this.name;
    }

    public int getIndex() {
      return this.methodIndex;
    }

    public int getReturnTypeId() {
      return this.retType.getId();
    }

    public ReturnType getReturnType() {
      return this.retType;
    }

    public int getArgNumber() {
      return this.argNum;
    }

    public static int getMaxNumberOfArgs() {
      int result = 0;
      for(MethodEntry method: MethodEntry.values()) {
        if(method.argNum > result) {
          result = method.argNum; 
        }
      }
      return result;
    }

  } 
  
  
  
  
  
  
  
  
  
}
