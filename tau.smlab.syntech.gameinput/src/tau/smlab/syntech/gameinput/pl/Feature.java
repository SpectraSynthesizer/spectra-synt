package tau.smlab.syntech.gameinput.pl;

import java.io.Serializable;
import java.util.List;

public class Feature implements Serializable {
	
	private static final long serialVersionUID = -5605621713229615919L;
	
	private String name;
	private List<String> gars;
	private List<String> asms;
	
	public Feature(String name, List<String> gars, List<String> asms, int traceId) {
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
	public List<String> getGars() {
		return gars;
	}
	public void setGars(List<String> gars) {
		this.gars = gars;
	}
	public List<String> getAsms() {
		return asms;
	}
	public void setAsms(List<String> asms) {
		this.asms = asms;
	}
	public int getTraceId() {
		return traceId;
	}
	public void setTraceId(int traceId) {
		this.traceId = traceId;
	}

	public String toString() {
		return this.name;
	}
}
