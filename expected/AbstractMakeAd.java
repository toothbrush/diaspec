package fr.diaspec.webcam.generated;

abstract public class AbstractMakeAd extends Publisher<String> implements Context {

	private AbstractRunner runner;

	final public void init(AbstractRunner runner) {
		Log.i("makead", "init has been called.");
		this.runner = runner;
	}

	protected abstract String whenRequiredMakeAd(IPProxy discover);
	// no trigger(..) function, since we're an on-require component

	final protected String requireValue() {
		IPProxy ipp = new IPProxy();
		ipp.setAccessible(true);
		String resp = whenRequiredMakeAd(ipp);
		ipp.setAccessible(false);
		return resp;
	}

	protected final class IPProxy  { // final: no subclassing
		private IPProxy() {
			// Exists only to defeat instantiation.
		}
	
		final private void setAccessible(boolean isAccessible) {
			this.isAccessible = isAccessible;
		}

		private boolean isAccessible = false;
		final public String getMakeAd() {
			if (isAccessible)
			{ return runner.getIP().requireValue(); }

			throw new RuntimeException("Access forbidden for IP source");
		}}
}
