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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.lang.ProcessBuilder.Redirect;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import net.sf.javabdd.BDDFactory.ReorderMethod;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.MethodEntry;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.ReturnBooleanValue;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.ReturnDoubleListValue;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.ReturnDoubleValue;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.ReturnIntListValue;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.ReturnIntValue;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.ReturnLongListValue;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.ReturnLongValue;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.ReturnReorderMethodValue;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.ReturnType;
import net.sf.javabdd.CUDDPipeCallerMethodInfo.ReturnValue;

public class CUDDPipeCaller implements CUDDCaller {

  //private static final int ARR_TYPES_NUM = 2;    /*NOT USED!*/
  private boolean active;
	
  private static final String CUDD_PIPE_PREFIX = "\\\\.\\pipe\\CUDDPipe";

  private static final String CUDD_EXE = "CUDDPipe.exe";

  private static final int RETRY_TIMEOUT = 3000;
  
  protected String projectLib;
  protected Process process;
  protected OutputStream writePipe;
  protected RandomAccessFile readPipe;

  protected ByteBuffer bf;

  private long info;   // CUDD manager
  private long zero;      // constant CUDD BDD (logical) zero
  private long one;       // constant CUDD BDD and ADD one
  private long addZero; // constant CUDD ADD (arithmetic) zero

  public CUDDPipeCaller(){
    //this.numArrTypes = ARR_TYPES_NUM; //number of types of arrays that can be passed -> NOT USED!
    this.projectLib = CUDDPipeCaller.class.getProtectionDomain()
        .getCodeSource().getLocation().getPath(); 
    this.writePipe = null;
    this.bf = ByteBuffer.allocate(8); //initialize byte buffer
    this.bf.order(ByteOrder.LITTLE_ENDIAN); //set order of bytes to little endian: this is consistent with the native code!
    createSubProcAndPipe(); //create CUDD native sub-process
  }

  private byte[] fillValueInByteBuffer(Object value) {
    bf.clear();
    if(value instanceof Integer){
      bf.putInt(0,(int)value);
    }else if(value instanceof Long){
      bf.putLong(0,(long)value);
    }else if(value instanceof Double){  //cant convert double directly so converted as long
      bf.putDouble(0,(double)value);
    }else if(value instanceof ReorderMethod){
      ReorderMethod temp = (ReorderMethod)value;
      bf.putInt(0, temp.id);
    }
    return bf.array();
  }
 

  
  
  private void insertObject(ArrayList<Byte> list, Object arg,int size){
	  byte[] converted = fillValueInByteBuffer(arg);
	  for(int j=0;j<size;j++){
          list.add(converted[j]);
        }
  }
  
  
 
  private void loadParam(ArrayList<Byte> argList, ArrayList<Byte> typesList, ArrayList<Byte> lengthsList,Object arg){ //inserts a single function parameter and updates relevant data (array length, type id)
	
	if((arg instanceof Integer)||(arg instanceof ReorderMethod)){
		
		insertObject(argList,arg,ReturnType.INTEGER.getnumBytes());
		if(arg instanceof Integer){
			insertObject(typesList,ReturnType.INTEGER.getId(),ReturnType.INTEGER.getnumBytes());
		}else{
	        insertObject(typesList,ReturnType.REORDER_METHOD.getId(),ReturnType.INTEGER.getnumBytes());  
	    }    
		
	}else if((arg instanceof Long)||(arg instanceof Double)){
		
	    insertObject(argList,arg,ReturnType.LONG.getnumBytes());	
	    if((arg instanceof Long)){
	    	insertObject(typesList,ReturnType.LONG.getId(),ReturnType.INTEGER.getnumBytes()); 
	    }else{
	    	insertObject(typesList,ReturnType.DOUBLE.getId(),ReturnType.INTEGER.getnumBytes()); 
		}
	    
	}else if(arg instanceof Boolean){	
		
	    int b;
	    if((Boolean)arg==true){b=1;}else{b=0;}
	    insertObject(argList,b,ReturnType.BOOLEAN.getnumBytes());
	    insertObject(typesList,ReturnType.BOOLEAN.getId(),ReturnType.INTEGER.getnumBytes()); 
	
	}else if(arg.getClass() == int[].class){
		System.out.println("IN INT ARRAY HANDLE");
		int[] intArr=(int[])arg;
		System.out.printf("Array Length: %d\n",intArr.length);
	    insertObject(lengthsList,intArr.length,ReturnType.INTEGER.getnumBytes()); 
	    for(int j=0;j<intArr.length;j++){
	    		insertObject(argList,intArr[j],ReturnType.INT_LIST.getnumBytes()); 
		}				
		insertObject(typesList,ReturnType.INT_LIST.getId(),ReturnType.INTEGER.getnumBytes()); 
	
	}else if(arg.getClass() == long[].class){
	  
		System.out.println("IN LONG ARRAY");
		
		long[] longArr=(long[])arg;
	    insertObject(lengthsList,longArr.length,ReturnType.INTEGER.getnumBytes()); 
	    for(int j=0;j<longArr.length;j++){
	    	insertObject(argList,longArr[j],ReturnType.LONG_LIST.getnumBytes()); 
	    }
	    insertObject(typesList,ReturnType.LONG_LIST.getId(),ReturnType.INTEGER.getnumBytes()); 
	
	}else if(arg.getClass() == double[].class){
		
	 
		double[] doubleArr=(double[])arg;
	    insertObject(lengthsList,doubleArr.length,ReturnType.INTEGER.getnumBytes());
	    for(int j=0;j<doubleArr.length;j++){
	    	insertObject(argList,doubleArr[j],ReturnType.DOUBLE_LIST.getnumBytes()); 
	    }	
	    insertObject(typesList,ReturnType.DOUBLE_LIST.getId(),ReturnType.INTEGER.getnumBytes());
	}
  }
  
  
  
  private void piper(MethodEntry method, Object[] args) throws IOException {
    System.out.println("now piping function:");
    System.out.println(method.getName());
    int maxArgs = CUDDPipeCallerMethodInfo.MAX_METHOD_ARGS;
    ArrayList<Byte> constantData = new ArrayList<>(); //will carry func index and function parameter types
    ArrayList<Byte> funcParameters = new ArrayList<>();  //will carry function parameter bytes
    ArrayList<Byte> arrayLengths = new ArrayList<>(); //will carry lengths of all arrays passed as args by order of passing and according to type: int<long<double, if there are any
    insertObject(constantData, method.getIndex(), ReturnType.INTEGER.getnumBytes());
    if(args != null){ //load arguments
      if(args.length != method.getArgNumber()){
        throw new IOException("number of args doesnt match dictionary value for function");	
      }	
      for(int i = 0 ;i < method.getArgNumber() ; i++){  
        Object current = args[i];
        
        loadParam(funcParameters, constantData, arrayLengths,current);
        
      }
    }
     for(int j = 0; j<(maxArgs-method.getArgNumber());j++){   //empty slots in parameter array (constant size) are filled with -1
    	 	insertObject(constantData,-1,ReturnType.INTEGER.getnumBytes());
     }
     for(int j = 0;j<arrayLengths.size();j++){         //add array lengths if there are any array parameters
    	 	constantData.add(arrayLengths.get(j));
     }
     for(int j = 0;j<funcParameters.size();j++){       //finally, function add parameter bytes
    	 	constantData.add(funcParameters.get(j));
     }
     int finalSize = constantData.size();	 
     Byte[] dataBytes = constantData.toArray(new Byte[finalSize]);
     System.out.printf("finalSize = %d\n",finalSize);
     byte[] convertedBytes = new byte[finalSize];       //write can only accept byte    
     for(int j = 0;j<finalSize;j++){
    	 	convertedBytes[j] = dataBytes[j];
     }
      try {	
        System.out.println("#######################now writing to out pipe");
        /*for(int i=0;i<finalArr.length;i++){
				System.out.println(finalArr[i]);
			}*/
        writePipe.write(convertedBytes);
        writePipe.flush();
      }catch(IOException e) {
        e.printStackTrace();
      }
  }
  
  
  
  
  
  private void createSubProcAndPipe() throws LinkageError{
    ProcessBuilder pb = new ProcessBuilder(projectLib + CUDD_EXE );
    //pb.directory(new File(projectDir));

    pb.redirectOutput(Redirect.INHERIT); //stdout and stderr of the native subprocess will be the same as of the Java process
    pb.redirectError(Redirect.INHERIT);  // In this way, we will see all of the prints of the sub process

    //generate a pseudo random integer value that will be concatenated to the named pipe name
    int randomNum = ThreadLocalRandom.current().nextInt(Integer.MAX_VALUE);


    Process p=null;
    try {
      p = pb.start();   //create the native sub-process
      writePipe = p.getOutputStream();  //BufferedOutputStream get the sub process stdin
      writePipe.write(fillValueInByteBuffer(Integer.valueOf(randomNum)), 0, 4);
      writePipe.flush();
    }
    catch(IOException e) {
      e.printStackTrace();
      System.err.println("Error: Failed to create CUDD sub process or to write to the sub process stdout");
    }

    File pipe = new File(CUDD_PIPE_PREFIX + randomNum);   //open the named pipe which has just been created by the sub process	
    long startTime = System.currentTimeMillis(); //If pipe is not opened within 2 seconds system will timeout
    long currentTime = startTime;
    long timePassed = 0;
    boolean exceptionOccurred = true;
    while(exceptionOccurred && timePassed < RETRY_TIMEOUT){
      try {
        readPipe = new RandomAccessFile(pipe, "r"); //connect to the named pipe
        currentTime=System.currentTimeMillis();
        timePassed=currentTime-startTime;
      }
      catch(FileNotFoundException e){ continue;}
      exceptionOccurred = false;
    }

    if(exceptionOccurred) {
      System.err.println();
      throw new LinkageError("Error: Failed to open the named pipe: " + CUDD_PIPE_PREFIX + randomNum);
    }


    Runtime.getRuntime().addShutdownHook(new Thread(){
      public void run() {
        done0Caller();
      }});

    this.process=p;
  }

  private ReturnValue<? extends Object> extractRes(MethodEntry method, Process p){
    if(p == null){
      //return error
    }
    
    ReturnType retType = method.getReturnType();
    int arrLength;
    ReturnValue<? extends Object> res = null;   ///###/// handle error
    switch(retType){

    case BOOLEAN: {
      Boolean b=null;
      try {
    	int bInt = Integer.reverseBytes(readPipe.readInt());
        b = (bInt==1) ? true:false;
        System.out.println("$$$$$$$$$$$ANSWERBOOL$$$$$$$$$$$$$");
        System.out.println(bInt);
        res =  new ReturnBooleanValue(b);
      } catch (IOException e) {
        e.printStackTrace();
      }
      break;
    }
    case INTEGER: {
      try{
        res = new ReturnIntValue(Integer.reverseBytes(readPipe.readInt()));
      } catch (IOException e) {
        e.printStackTrace();
      }
      break;
    }
    case LONG: {
      try{
        res =  new ReturnLongValue(Long.reverseBytes(readPipe.readLong()));
        
      } catch (Exception e) {
        e.printStackTrace();
      }
      break;
    }
    case DOUBLE: {
      try{ 
        res = new ReturnDoubleValue(Double.longBitsToDouble(Long.reverseBytes(Double.doubleToLongBits(readPipe.readDouble()))));
      } catch (IOException e) {
        e.printStackTrace();
      }
      break;      
    }
    //case 4:
    //void
    case INT_LIST: {
   
      try{
        arrLength = Integer.reverseBytes(readPipe.readInt());
        List<Integer> intList = new ArrayList<>();
        for(int i = 0 ; i < arrLength ; i++){
          intList.add(Integer.reverseBytes(readPipe.readInt()));
        }
        res = new ReturnIntListValue(intList);
      } catch (IOException e) {
        e.printStackTrace();
      }
      break;
    }
    case LONG_LIST: {
      try{
        arrLength = Integer.reverseBytes(readPipe.readInt());
        System.out.println("$$$$$$$$$$$LENGTH$$$$$$$$$$$$$");
        System.out.println(arrLength);
        
        List<Long> longList = new ArrayList<>();
        for(int i=0 ; i < arrLength ; i++){
          longList.add(Long.reverseBytes(readPipe.readLong()));
        }
        res = new ReturnLongListValue(longList);
      } catch (IOException e) {
        e.printStackTrace();
      }
      break;
    }
    case DOUBLE_LIST: {
      try{
        arrLength = Integer.reverseBytes(readPipe.readInt()); 
        List<Double> DoubleList = new ArrayList<>();
        for(int i=0 ; i < arrLength ; i++){
          double rawDouble = Double.longBitsToDouble(Long.reverseBytes(Double.doubleToLongBits(readPipe.readDouble())));
          DoubleList.add(rawDouble);
        }
        res = new ReturnDoubleListValue(DoubleList);
      } catch (IOException e) {
        e.printStackTrace();
      }
      break;
    }
    case REORDER_METHOD: {
      try{
        int intV = Integer.reverseBytes(readPipe.readInt());
        ReorderMethod rMethod=null;
        switch(intV){
        case(0):
          rMethod=net.sf.javabdd.BDDFactory.REORDER_NONE;
        case(1):
          rMethod=net.sf.javabdd.BDDFactory.REORDER_WIN2;
        case(2):
          rMethod=net.sf.javabdd.BDDFactory.REORDER_WIN2ITE;
        case(3):
          rMethod=net.sf.javabdd.BDDFactory.REORDER_WIN3;
        case(4):
          rMethod=net.sf.javabdd.BDDFactory.REORDER_WIN3ITE;
        case(5):
          rMethod=net.sf.javabdd.BDDFactory.REORDER_SIFT;
        case(6):
          rMethod=net.sf.javabdd.BDDFactory.REORDER_SIFTITE;
        case(7):
          rMethod=net.sf.javabdd.BDDFactory.REORDER_RANDOM;
        }
        res = new ReturnReorderMethodValue(rMethod);
      } catch (IOException e) {
        e.printStackTrace();
      }
      break;
    }
    default:
      res = null;
    }		
    return res;
  }

  private ReturnValue<? extends Object> methodCaller(MethodEntry method, Object... args){

    //send arguments via pipe to subprocess
    try {
      if(args.length == 0) {
        piper(method, null);
      }
      else
      {
        piper(method, args);
      }
    }
    catch(IOException e){
      e.printStackTrace();
    }
    try{
      Thread.sleep(70);
    }catch(InterruptedException e){}
    return extractRes(method, process);
    //return result
  }

  
  @Override
  public void initialize0Caller(int nodenum, int cachesize){
    ReturnLongListValue retVal = (ReturnLongListValue) methodCaller(MethodEntry.INITIALIZE0, nodenum, cachesize);
    List<Long> retList = retVal.getValue();
    this.info = retList.get(0);
    this.zero = retList.get(1);
    this.one = retList.get(2);
    this.addZero = retList.get(3);
    active=true;
  }
  
  @Override
  public boolean isInitialized0Caller() {		
    ReturnBooleanValue result = (ReturnBooleanValue) methodCaller(MethodEntry.IS_INITIALIZED0);
    return result.getValue().booleanValue();
  }

  @Override
  public void done0Caller() {
	if(active==true){
    methodCaller(MethodEntry.DONE0);
    try{
        Thread.sleep(30);
      }catch(InterruptedException e){}
    active = false;
	}
  }

  @Override
  public int varNum0Caller() {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.VAR_NUM0);
    return result.getValue().intValue();
  }
  
  @Override
  public int setVarNum0Caller(int num, boolean ADD) {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.SET_VAR_NUM0, num, ADD);
    return result.getValue().intValue();
  }
  

  @Override
  public long ithVar0Caller(int var, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ITH_VAR0, var, ADD);
    return result.getValue().longValue();
  }

  @Override
  public int level2Var0Caller(int level) {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.LEVEL2_VAR0, level);
    return result.getValue().intValue();	
  }

  @Override
  public int var2Level0Caller(int var) {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.VAR2_LEVEL0, var);
    return result.getValue().intValue();	
  }

  @Override
  public void setVarOrder0Caller(int[] neworder) {
	for(int i=0;i<neworder.length;i++){
		System.out.printf("ARG %d: %d\n",i,neworder[i]);
	}
	  
	  
    methodCaller(MethodEntry.SET_VAR_ORDER0, neworder);
  }

  @Override
  public int getAllocNum0Caller() {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.GET_ALLOC_NUM0);
    return result.getValue().intValue();	
  }

  @Override
  public int getNodeNum0Caller() {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.GET_NODE_NUM0);
    return result.getValue().intValue();	
  }

  @Override
  public int getCacheSize0Caller() {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.GET_CACHE_SIZE0);
    return result.getValue().intValue();	
  }

  @Override
  public int var0Caller(long b) {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.VAR0, b);
    return result.getValue().intValue();	
  }

  @Override
  public long high0Caller(long b, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.HIGH0, b, ADD);
    return result.getValue().longValue();	
  }

  @Override
  public long low0Caller(long b, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.LOW0, b, ADD);
    return result.getValue().longValue();
  }

  @Override
  public long not0Caller(long b, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.NOT0, b, ADD);
    return result.getValue().longValue();
  }

  @Override
  public long ite0Caller(long b, long c, long d, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ITE0, b, c, d, ADD);
    return result.getValue().longValue();
  }

  @Override
  public long relprod0Caller(long b, long c, long d) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.RELPROD0, b, c, d);
    return result.getValue().longValue();
  }

  @Override
  public long compose0Caller(long b, long c, int var, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.COMPOSE0, b, c, var, ADD);
    return result.getValue().longValue();
  }

  @Override
  public long exist0Caller(long b, long c, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.EXIST0, b, c, ADD);
    return result.getValue().longValue();
  }

  @Override
  public long forAll0Caller(long b, long c, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.FOR_ALL0, b, c, ADD);
    return result.getValue().longValue();
  }

  @Override
  public long restrict0Caller(long b, long var) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.RESTRICT0, b, var);
    return result.getValue().longValue();
  }

  @Override
  public long restrictWith0Caller(long b, long c, boolean deref_other) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.RESTRICT_WITH0, b, c, deref_other);
    return result.getValue().longValue();
  }

  @Override
  public long simplify0Caller(long b, long var, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.SIMPLIFY0, b, var, ADD);
    return result.getValue().longValue();
  }

  @Override
  public long support0Caller(long b, boolean is_add) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.SUPPORT0, b, is_add);
    return result.getValue().longValue();
  }

  @Override
  public long apply0Caller(long b, long c, int opr, boolean ADD, boolean apply_with, boolean deref_other) {
	  ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.APPLY0, b, c,
        opr, ADD, apply_with, deref_other);
    return result.getValue().longValue();
  }

  @Override
  public long satOne0Caller(long b, long c, boolean ADD) { //changed by Or
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.SAT_ONE0, b, c, ADD);
    return result.getValue().longValue();
  }

  @Override
  public int nodeCount0Caller(long b) {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.NODE_COUNT0, b);
    return result.getValue().intValue();
  }

  @Override
  public double pathCount0Caller(long b) {
    ReturnDoubleValue result = (ReturnDoubleValue) methodCaller(MethodEntry.PATH_COUNT0, b);
    return result.getValue().doubleValue();
  }

  @Override
  public double satCount0Caller(long b) {
    ReturnDoubleValue result = (ReturnDoubleValue) methodCaller(MethodEntry.SAT_COUNT0, b);
    return result.getValue().doubleValue();
  }

  @Override
  public void addRefCaller(long p) {
    methodCaller(MethodEntry.ADD_REF, p);
  }

  @Override
  public void delRefCaller(long p, boolean ADD) {
    methodCaller(MethodEntry.DEL_REF, p, ADD);
  }

  @Override
  public long veccompose0Caller(long b, long p, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.VECCOMPOSE0, b, p, ADD);
    return result.getValue().longValue();
  }

  @Override
  public long replace0Caller(long b, long p, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.REPLACE0, b, p, ADD);
    return result.getValue().longValue();
  }

  @Override
  public long allocCaller(boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ALLOC, ADD);
    return result.getValue().longValue();
  }

  @Override
  public void set0Caller(long p, int oldvar, int newvar, boolean ADD) {	
    methodCaller(MethodEntry.SET0, p, oldvar, newvar, ADD);
  }

  @Override
  public void set2Caller(long p, int oldvar, long newbdd) {
    methodCaller(MethodEntry.SET2, p, oldvar, newbdd);
  }

  @Override
  public void reset0Caller(long ptr, boolean ADD) {
    methodCaller(MethodEntry.RESET0, ptr, ADD);
  }

  @Override
  public void free0Caller(long ptr) {
    methodCaller(MethodEntry.FREE0, ptr);
  }

  @Override
  public boolean isZeroOneADD0Caller(long ptr) {
    ReturnBooleanValue result = (ReturnBooleanValue) methodCaller(MethodEntry.IS_ZERO_ONE_ADD0, ptr);
    return result.getValue().booleanValue();
  }

  @Override
  public long addConst0Caller(double constval) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ADD_CONST0, constval);
    return result.getValue().longValue();
  }

  @Override
  public boolean isAddConst0Caller(long ptr) {
    ReturnBooleanValue result = (ReturnBooleanValue) methodCaller(MethodEntry.IS_ADD_CONST0, ptr);
    return result.getValue().booleanValue();
  }

  @Override
  public long addFindMax0Caller(long ptr) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ADD_FIND_MAX0, ptr);
    return result.getValue().longValue();
  }

  @Override
  public long addFindMin0Caller(long ptr) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ADD_FIND_MIN0, ptr);
    return result.getValue().longValue();
  }

  @Override
  public double retrieveConstValue0Caller(long ptr) {
    ReturnDoubleValue result = (ReturnDoubleValue) methodCaller(MethodEntry.RETRIEVE_CONST_VALUE0, ptr);
    return result.getValue().doubleValue();
  }

  @Override
  public long addAdditiveNeg0Caller(long ptr) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ADD_ADDITIVE_NEG0, ptr);
    return result.getValue().longValue();
  }

  @Override
  public long addApplyLog0Caller(long ptr) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ADD_APPLY_LOG0, ptr);
    return result.getValue().longValue();
  }

  @Override
  public void reorder0Caller(ReorderMethod method) {
    methodCaller(MethodEntry.REORDER0, method);
  }

  @Override
  public void autoReorder0Caller(ReorderMethod method, boolean setmax, int maxval) {
    methodCaller(MethodEntry.AUTO_REORDER0, method, setmax, maxval);
  }

  @Override
  public ReorderMethod getreordermethod0Caller() {
    ReturnReorderMethodValue result = (ReturnReorderMethodValue) methodCaller(MethodEntry.GETREORDERMETHOD0);
    return result.getValue();
  }

  @Override
  public void autoReorder1Caller() {
    methodCaller(MethodEntry.AUTO_REORDER1);

  }

  @Override
  public int reorderVerbose0Caller(int v) {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.REORDER_VERBOSE0, v);
    return result.getValue().intValue();
  }

  @Override
  public void addVarBlock0Caller(int first, int last, boolean fixed) {
    methodCaller(MethodEntry.ADD_VAR_BLOCK0, first, last, fixed);
  }

  @Override
  public void clearVarBlocks0Caller() {
    methodCaller(MethodEntry.CLEAR_VAR_BLOCKS0);
  }

  @Override
  public void printStat0Caller() {
    methodCaller(MethodEntry.PRINT_STAT0);
  }

  @Override
  public long toADD0Caller(long b) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.TO_ADD0, b);
    return result.getValue().longValue();
  }

  @Override
  public long toBDD0Caller(long b) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.TO_BDD0, b);
    return result.getValue().longValue();
  }

  @Override
  public long toBDDThresholdCaller(long b, double threshold) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.TO_BDD_THRESHOLD, b, threshold);
    return result.getValue().longValue();
  }

  @Override
  public long toBDDStrictThresholdCaller(long b, double threshold) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.TO_BDD_STRICT_THRESHOLD, b, threshold);
    return result.getValue().longValue();
  }

  @Override
  public long toBDDIntervalCaller(long b, double lower, double upper) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.TO_BDD_INTERVAL, b, lower, upper);
    return result.getValue().longValue();
  }

  @Override
  public long toBDDIthBitCaller(long b, int bit) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.TO_BDD_ITH_BIT, b, bit);
    return result.getValue().longValue();
  }

  @Override
  public int[] varSupportIndex0Caller(long b) {
    ReturnIntListValue result = (ReturnIntListValue) methodCaller(MethodEntry.VAR_SUPPORT_INDEX0, b);
    return result.toPrimitiveIntArray();
  }

  @Override
  public void printSet0Caller(long b, int printMode) {
    methodCaller(MethodEntry.PRINT_SET0, b, printMode);
  }

  @Override
  public long logicZero0Caller() {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.LOGIC_ZERO0);
    return result.getValue().longValue();
  }

  @Override
  public long arithmeticZero0Caller() {	
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ARITHMETIC_ZERO0);
    return result.getValue().longValue();
  }

  @Override
  public long arithmeticLogicOne0Caller() {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ARITHMETIC_LOGIC_ONE0);
    return result.getValue().longValue();
  }

  @Override
  public long arithmeticPlusInfinity0Caller() {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ARITHMETIC_PLUS_INFINITY0);
    return result.getValue().longValue();
  }

  @Override
  public long arithmeticMinusInfinity0Caller() {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ARITHMETIC_MINUS_INFINITY0);
    return result.getValue().longValue();
  }
  
  @Override
  public long replaceWith0Caller(long b, long c, boolean ADD) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.REPLACE_WITH0, b, c, ADD);
    return result.getValue().longValue();
  }

  @Override
  public long addMinimizeVal0Caller(long ptr, double val) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ADD_MINIMIZE_VAL0, ptr, val);
    return result.getValue().longValue();
  }

  @Override
  public long addAbstractMin0Caller(long b, long c) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ADD_ABSTRACT_MIN0, b, c);
    return result.getValue().longValue();
  }

  @Override
  public long addAbstractMax0Caller(long b, long c) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ADD_ABSTRACT_MAX0, b, c);
    return result.getValue().longValue();
  }

  @Override
  public int reorderTimes0Caller() {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.REORDER_TIMES0);
    return result.getValue().intValue();	
  }

  @Override
  public void printVarTree0Caller() {
    methodCaller(MethodEntry.PRINT_VAR_TREE0);
  }

  @Override
  public long addEngSubtract0Caller(long a, long b, double maxEnergy) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ADD_ENG_SUBTRACT0, a, b, maxEnergy);
    return result.getValue().longValue();
  }

  @Override
  public long convertToZeroOneADDByThres0Caller(long a, long opType, double thresValue) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.CONVERT_TO_ZERO_ONE_ADD_BY_THRES0,
        a, opType, thresValue);
    return result.getValue().longValue();
  }

  @Override
  public void printDot0Caller(long a) {
    methodCaller(MethodEntry.PRINT_DOT0, a);
  }

  @Override
  public long arithmeticExist0Caller(long b, long c) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.ARITHMETIC_EXIST0, b, c);
    return result.getValue().longValue();
  }

  @Override
  public long determinizeController0Caller(long b, long c) {
    ReturnLongValue result = (ReturnLongValue) methodCaller(MethodEntry.DETERMINIZE_CONTROLLER0, b, c);
    return result.getValue().longValue();
  }

  @Override
  public int getSize0Caller(long b) {
    ReturnIntValue result = (ReturnIntValue) methodCaller(MethodEntry.GET_SIZE0, b);
    return result.getValue().intValue();  
  }

  @Override
  public boolean reorderEnabled0Caller() { //a new method, add by Or
    ReturnBooleanValue result = (ReturnBooleanValue) methodCaller(MethodEntry.REORDER_ENABLED0);
    return result.getValue().booleanValue();  
  }

  @Override
  public long getBddZero() {
    return this.zero;
  }

  @Override
  public long getAddZero() {
    return this.addZero;
  }

  @Override
  public long getManager() {
    return this.info;
  }

  @Override
  public long getOne() {
    return this.one;
  }
}