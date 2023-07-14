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

package tau.smlab.syntech.ui.pl;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.function.Consumer;
import java.util.function.Function;

import tau.smlab.syntech.bddgenerator.ProductLineBDDGenerator;
import tau.smlab.syntech.gameinput.model.GameInput;
import tau.smlab.syntech.gameinput.pl.Product;
import tau.smlab.syntech.gameinput.pl.ProductLattice;

public class VariabilityAwareRealizability {

	private boolean bottomUp;
	private GameInput gi;
	
	private Function<GameInput, Boolean> realizability;
	private Consumer<String> print;
	
	public VariabilityAwareRealizability(boolean bottomUp, Function<GameInput, Boolean> realizability, Consumer<String> print, GameInput gi) {
		super();
		this.bottomUp = bottomUp;
		this.realizability = realizability;
		this.print = print;
		this.gi = gi;
	}

	public void doWork() {
	
		List<Product> products = ProductLineBDDGenerator.createProducts(
				gi.getFeatures(), gi.getFeatureModel(), gi.getSys(), gi.getEnv());
		ProductLattice lattice = new ProductLattice(products);
		
		List<Product> roots;
		if (bottomUp) {
			roots = lattice.getBottom();
		} else {
			roots = lattice.getTop();
		}
		
		Queue<Product> queue = new LinkedList<>();
		
		queue.addAll(roots);
		
		while (!queue.isEmpty()) {
			
			
			Product toProcess = queue.poll();
			
			if (!Product.Status.UNKNOWN.equals(toProcess.getStatus())) {
				continue;
			}
			
			print.accept(String.format("Processing product %s", toProcess));
			
			gi.getSys().setConstraints(new ArrayList<>(toProcess.getGars()));
			gi.getEnv().setConstraints(new ArrayList<>(toProcess.getAsms()));
			
			
			// Original flow
			boolean isRealizable = realizability.apply(gi);
			
			// If realizable we can skip the rest of the lattice for this product
			
			Product.Status status = isRealizable ? Product.Status.REALIZABLE : Product.Status.UNREALIZABLE;
			print.accept(String.format("Flagging product %s as %s", toProcess, status));
			toProcess.setStatus(status);
			
			if (bottomUp) {
				
				if (Product.Status.REALIZABLE.equals(status)) {
					flagDescendants(lattice, toProcess, status);
				} else {
					if (lattice.getSubsumed(toProcess) != null) {
						queue.addAll(lattice.getSubsumed(toProcess));
					}
				}
				
			} else {
				
				if (Product.Status.UNREALIZABLE.equals(status)) {
					flagDescendants(lattice, toProcess, status);
				} else {
					if (lattice.getSubsuming(toProcess) != null) {
						queue.addAll(lattice.getSubsuming(toProcess));
					}
				}
				
			}
				
			
			
			
		}
	}
	
	private void flagDescendants(ProductLattice lattice, Product product, Product.Status status) {
		
		List<Product> descendants;
		if (bottomUp) {
			descendants = lattice.getSubsumed(product);
		} else {
			descendants = lattice.getSubsuming(product);
		}
		
		if (descendants == null) return;
		
		for (Product descendant : descendants) {
			if (Product.Status.UNKNOWN.equals(descendant.getStatus())) {
				print.accept(String.format("Flagging product %s as %s (infering from lattice)", descendant, status));
				descendant.setStatus(status);
				flagDescendants(lattice, descendant, status);
			}
		}
	}


}
