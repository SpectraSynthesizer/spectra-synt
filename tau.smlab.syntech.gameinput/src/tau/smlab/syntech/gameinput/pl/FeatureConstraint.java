package tau.smlab.syntech.gameinput.pl;

import java.io.Serializable;
import tau.smlab.syntech.gameinput.spec.Operator;


public class FeatureConstraint implements Serializable {

	private static final long serialVersionUID = 7596374642072500344L;

	private Operator theOp;
	private FeatureConstraint[] elements;
	
	private Feature feature;
	
	private int traceId;

	public FeatureConstraint(Operator theOp, FeatureConstraint element, int traceId) {
		super();
		this.theOp = theOp;
		this.elements = new FeatureConstraint[] {element};
		this.traceId = traceId;
	}
	
	public FeatureConstraint(Operator theOp, FeatureConstraint element1, FeatureConstraint element2, int traceId) {
		super();
		this.theOp = theOp;
		this.elements = new FeatureConstraint[] {element1, element2};
		this.traceId = traceId;
	}
	
	public FeatureConstraint(Feature feature, int traceId) {
		super();
		this.feature = feature;
		this.traceId = traceId;
	}
	
	public Feature getFeature() {
		return feature;
	}

	public void setFeature(Feature feature) {
		this.feature = feature;
	}

	public Operator getTheOp() {
		return theOp;
	}
	public void setTheOp(Operator theOp) {
		this.theOp = theOp;
	}
	public FeatureConstraint[] getElements() {
		return elements;
	}
	public void setElements(FeatureConstraint[] elements) {
		this.elements = elements;
	}
	
	public int getTraceId() {
		return traceId;
	}

	public void setTraceId(int traceId) {
		this.traceId = traceId;
	}
}
