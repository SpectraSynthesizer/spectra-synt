package tau.smlab.syntech.gameinput.pl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import tau.smlab.syntech.gameinput.model.Constraint;

public class Product {
	
	public static enum Status { REALIZABLE, UNREALIZABLE, UNKNOWN }

	private List<Feature> features = new ArrayList<>();
	private Set<Constraint> gars = new HashSet<>();
	private Set<Constraint> asms = new HashSet<>();
	
	private Status status = Status.UNKNOWN;
	
	public List<Feature> getFeatures() {
		return features;
	}
	public Set<Constraint> getGars() {
		return gars;
	}
	public Set<Constraint> getAsms() {
		return asms;
	}
	
	public boolean subsumes(Product other) {
		return gars.containsAll(other.gars) && other.asms.containsAll(asms);
	}
	
	public String toString() {
		return features.toString();
	}
	public Status getStatus() {
		return status;
	}
	public void setStatus(Status status) {
		this.status = status;
	}
}
