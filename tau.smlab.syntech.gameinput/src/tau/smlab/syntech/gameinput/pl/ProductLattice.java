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

package tau.smlab.syntech.gameinput.pl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class ProductLattice {
	
	Map<Product, List<Product>> bottomUp = new HashMap<>();
	Map<Product, List<Product>> topDown = new HashMap<>();
	
	public ProductLattice(List<Product> products) {
		
		for (Product p0 : products) {
			for (Product p1 : products) {
				
				if (p0.subsumes(p1) && !p0.equals(p1)) {
					
					if (products.stream().noneMatch(p -> !p.equals(p0) && !p.equals(p1) && p0.subsumes(p) && p.subsumes(p1))) {
						
						if (!bottomUp.containsKey(p0)) {
							bottomUp.put(p0, new ArrayList<>());
						}
						bottomUp.get(p0).add(p1);
						
						if (!topDown.containsKey(p1)) {
							topDown.put(p1, new ArrayList<>());
						}
						topDown.get(p1).add(p0);
					}
				}
				
			}
		}
	}
	
	public List<Product> getSubsumed(Product p) {
		return bottomUp.get(p);
	}
	
	public List<Product> getSubsuming(Product p) {
		return topDown.get(p);
	}
	
	public List<Product> getBottom() {
		return getRoots(bottomUp);
	}
	
	public List<Product> getTop() {
		return getRoots(topDown);
	}
	
	private List<Product> getRoots(Map<Product, List<Product>> lattice) {
		Set<Product> children = lattice.values().stream().flatMap(List<Product>::stream).collect(Collectors.toSet());
		return lattice.keySet().stream().filter(p -> !children.contains(p)).collect(Collectors.toList());
	}

}
