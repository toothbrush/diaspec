package fr.diaspec.webcam.generated;

public abstract class AbstractIP extends Publisher<String> implements Source<String> {

	protected abstract String getIPValue();


	@Override
	public String requireValue() {
		return getIPValue();
	}
	
	
}
