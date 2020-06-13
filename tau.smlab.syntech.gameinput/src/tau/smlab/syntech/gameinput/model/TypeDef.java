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

package tau.smlab.syntech.gameinput.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import tau.smlab.syntech.gameinput.spec.PrimitiveValue;

public class TypeDef implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 6580802000461578725L;

	public List<Integer> getDimensions() {
		return dimensions;
	}

	public void setDimensions(List<Integer> dimensions) {
		this.dimensions = dimensions;
	}

	/**
	 * name is optional (in case of inline type definition)
	 */
	private String name;

	// either we have values or we have lower and upper bounds

	private List<String> values;
	private int lower, upper;
	private boolean isInteger;
	private boolean isBoolean;
	private List<Integer> dimensions;

	/**
	 * Copy constructor
	 */
	public TypeDef(TypeDef other) {
		this.values = other.getValues();
		this.lower = other.getLower();
		this.upper = other.getUpper();
		this.isInteger = other.isInteger();
		this.isBoolean = other.isBoolean();
		List<Integer> otherDimensions = other.getDimensions();
		if (otherDimensions == null) {
			this.dimensions = null;
		} else {
			this.dimensions = new ArrayList<>(otherDimensions);
		}

	}

	/**
	 * Constructor for a boolean type
	 */
	public TypeDef(List<Integer> dimensions) {
		this.values = null;
		this.isInteger = false;
		this.isBoolean = true;
		this.dimensions = dimensions;
	}

	/**
	 * Constructor for a boolean type
	 */
	public TypeDef() {
		this(new ArrayList<>());
	}

	/**
	 * Constructor for an integer type
	 * 
	 * @param lower lower int bound
	 * @param upper upper int bound
	 */
	public TypeDef(int lower, int upper, List<Integer> dimensions) {
		this.values = null;
		this.lower = lower;
		this.upper = upper;
		this.isInteger = true;
		this.isBoolean = false;
		this.dimensions = dimensions;
	}

	/**
	 * Constructor for an integer type
	 * 
	 * @param lower lower int bound
	 * @param upper upper int bound
	 */
	public TypeDef(int lower, int upper) {
		this(lower, upper, new ArrayList<>());
	}

	/**
	 * Constructor for a general type (neither boolean nor integer)
	 */
	public TypeDef(List<String> values, List<Integer> dimensions) {
		this.values = values;
		this.isInteger = false;
		this.isBoolean = false;
		this.dimensions = dimensions;
	}

	public String toString() {
		return "TypeDef:[name: " + name + " , values: " + values + " ,lowerBound: " + lower + " ,upperBound: " + upper
				+ " ,isInteger: " + isInteger + " ,isBoolean: " + isBoolean + " , dimensions: " + dimensions + "]";
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<String> getValues() {
		return values;
	}

	public void setValues(List<String> values) {
		this.values = values;
	}

	public int getLower() {
		return lower;
	}

	public void setLower(int lower) {
		this.lower = lower;
	}

	public int getUpper() {
		return upper;
	}

	public void setUpper(int upper) {
		this.upper = upper;
	}

	public boolean isInteger() {
		return isInteger;
	}

	public void setInteger(boolean isInteger) {
		this.isInteger = isInteger;
	}

	public boolean isBoolean() {
		return isBoolean;
	}

	public void setBoolean(boolean isBoolean) {
		this.isBoolean = isBoolean;
	}

	public boolean isArray() {
		return (dimensions != null && dimensions.size() > 0);
	}

	/**
	 * check whether types are comparable
	 * 
	 * IMPORTANT does not check actual array comparison, e.g., for boolean arrays
	 * <code>boolean[3] x</code> and <code>boolean[2][2] y</code> result would be
	 * true because the elements are comparable
	 * 
	 * @param other
	 * @return
	 */
	public boolean isComparable(TypeDef other) {
		// both are boolean
		if (isBoolean && other.isBoolean) {
			return true;
		}
		// only one is boolean
		if (isBoolean || other.isBoolean) {
			return false;
		}
		// both are integer
		if (isInteger && other.isInteger) {
			return true;
		}
		// only one is integer
		if (isInteger || other.isInteger) {
			return false;
		}

		// check if same values in same order
		return values.equals(other.values);
	}

	/**
	 * @param varType
	 * @return a list of all the primitive values that varType contains
	 */
	public List<PrimitiveValue> getPrimitivesList() {
		List<PrimitiveValue> lst = new ArrayList<PrimitiveValue>();
		if (this.isInteger()) {
			int from = this.getLower();
			int to = this.getUpper();
			for (int i = from; i <= to; i++) {
				lst.add(new PrimitiveValue(i));
			}
		} else if (this.isBoolean()) {
			lst.add(new PrimitiveValue("true"));
			lst.add(new PrimitiveValue("false"));
		} else {// a type with its own consts
			for (String str : this.getValues()) {
				lst.add(new PrimitiveValue(str));
			}
		}

		return lst;
	}

}
