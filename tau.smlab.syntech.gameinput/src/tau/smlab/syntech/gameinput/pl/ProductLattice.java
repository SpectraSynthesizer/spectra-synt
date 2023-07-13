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
