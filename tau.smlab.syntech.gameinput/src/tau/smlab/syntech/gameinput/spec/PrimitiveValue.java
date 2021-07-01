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

package tau.smlab.syntech.gameinput.spec;

public class PrimitiveValue implements Spec {
	/**
	 * 
	 */
	private static final long serialVersionUID = 8011682341776162911L;
	
	private String value;

	public PrimitiveValue(int value)
	{
		this.value = Integer.toString(value);
	}

	public PrimitiveValue(String value)
	{
		this.value = value;
	}

	public String toString()
	{
		return "<PrimitiveValue: " + this.value +">";
	}

	@Override
	public boolean isPastLTLSpec() {
		return false;
	}

	@Override
	public boolean isPropSpec() {
		switch (value) {
		case "true":
			return true;
		case "false":
			return true;
		case "TRUE":
			return true;
		case "FALSE":
			return true;
		default:
			break;
		}

		return false;
	}

	@Override
	public boolean hasTemporalOperators() {
		return false;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	@Override
	public PrimitiveValue clone() throws CloneNotSupportedException {
		return new PrimitiveValue(this.value);
	}
	
	@Override
	public int hashCode() {
	  return toString().hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
	  if (obj instanceof PrimitiveValue) {
	    return this.value.equals(((PrimitiveValue) obj).getValue());
	  }
	  return false;
	}
}
