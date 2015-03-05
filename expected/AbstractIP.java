package fr.diaspec.webcam.generated;

public abstract class AbstractIP extends Publisher<String> implements Source {

	protected abstract String getIPValue();
	protected AbstractRunner runner;

	public void init(AbstractRunner runner) {
		this.runner = runner;
	}
}
