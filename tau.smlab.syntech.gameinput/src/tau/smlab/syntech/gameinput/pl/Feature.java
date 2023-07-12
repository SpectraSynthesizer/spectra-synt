package tau.smlab.syntech.gameinput.pl;

import java.io.Serializable;
import java.util.List;

import tau.smlab.syntech.gameinput.model.Constraint;

public class Feature implements Serializable {
	
	private static final long serialVersionUID = -5605621713229615919L;
	
	private String name;
	private List<Constraint> gars;
	private List<Constraint> asms;
	
	public Feature(String name, List<Constraint> gars, List<Constraint> asms, int traceId) {
		super();
		this.name = name;
		this.gars = gars;
		this.asms = asms;
		this.traceId = traceId;
	}
	private int traceId;
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public List<Constraint> getGars() {
		return gars;
	}
	public void setGars(List<Constraint> gars) {
		this.gars = gars;
	}
	public List<Constraint> getAsms() {
		return asms;
	}
	public void setAsms(List<Constraint> asms) {
		this.asms = asms;
	}
	public int getTraceId() {
		return traceId;
	}
	public void setTraceId(int traceId) {
		this.traceId = traceId;
	}

}
