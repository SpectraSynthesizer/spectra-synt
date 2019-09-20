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

package tau.smlab.syntech.jtlv.env.module;

/**
 * <p>
 * A common interface from a module BDD entity. Mostly takes care of the entity
 * location (i.e. path and name..)
 * </p>
 * 
 * @version {@value edu.wis.jtlv.env.Env#version}
 * @author yaniv sa'ar.
 * 
 */
public abstract class ModuleEntity {

	/**
	 * <p>
	 * The field name.
	 * </p>
	 */
	protected String name;

	/**
	 * <p>
	 * Overrides the {@link java.lang.Object#equals(Object)}. Check whether
	 * this object is identical to the given object by comparing the field's
	 * path, and the field's name. This procedure does not continue to check
	 * whether the domains are identical. (This is done so that collections will
	 * identify that such a field exists, regardless of its domain)
	 * </p>
	 * 
	 * @param other
	 *            The other object to compare this filed to.
	 * 
	 * @return true if the given object is identical to this, false otherwise.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#comparable(ModuleBDDField)
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#strongEquals(Object)
	 */
	public boolean equals(Object other) {
		if (!(ModuleEntity.class.isInstance(other))) {
			return false;
		}
		if (this.getName() != ((ModuleEntity) other).getName()) {
			return false;
		}
		return true;
	}

	/**
	 * <p>
	 * Check whether this object is completely identical to the give object.
	 * This implementation is referred to the original
	 * {@link java.lang.Object#equals(Object)}.
	 * </p>
	 * 
	 * @param other
	 *            The other object to compare this filed to.
	 * @return true if the given object is identical to this, false otherwise.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#comparable(ModuleBDDField)
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#equals(Object)
	 */
	public boolean strongEquals(Object other) {
		return super.equals(other);
	}

	/**
	 * <p>
	 * Get the string representation of this field, without the path leading to
	 * it.
	 * </p>
	 * 
	 * @return A short string representing this field.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#toFullString()
	 */
	public String toString() {
		return this.name;
	}

	/**
	 * <p>
	 * Get the string representation of this field, with the path leading to it.
	 * </p>
	 * 
	 * @return A short string representing this field.
	 * 
	 * @see edu.wis.jtlv.env.module.ModuleBDDField#toString()
	 */
	public String toFullString() {
		return this.name;
	}

	/**
	 * <p>
	 * Get this field's name.
	 * </p>
	 * 
	 * @return This field's name.
	 */
	public String getName() {
		return this.name;
	}

	/**
	 * <p>
	 * In case this is an array name for field, returns the simple name without
	 * the braces '['']'.<br>
	 * If this is not an array name, then an empty string "", will be returned.
	 * </p>
	 * 
	 * @return The simple name for this field.
	 */
	public String getSimpleArrayName() {
		int last_RB = this.name.lastIndexOf(']');
		if ((last_RB + 1) != this.name.length())
			return "";

		int last_LB = last_RB - 1;
		int count = 1;
		while ((last_LB > 0) & (count > 0)) {
			if (this.name.charAt(last_LB) == ']')
				count++;
			if (this.name.charAt(last_LB) == '[')
				count--;
			last_LB--;
		}
		return this.name.substring(0, last_LB + 1);
	}

}