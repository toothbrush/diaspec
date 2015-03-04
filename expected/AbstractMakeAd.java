package fr.diaspec.webcam.generated;

abstract public class AbstractMakeAd extends Publisher<String> implements Context {

	private AbstractRunner runner;

	void init(AbstractRunner runner) {
		this.runner = runner;
	}

	protected abstract String whenRequiredMakeAd(IPProxy discover);
	// no trigger(..) function, since we're an on-require component

	protected String requireValue() {
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
	
		private void setAccessible(boolean isAccessible) {
			this.isAccessible = isAccessible;
		}

		private boolean isAccessible = false;
		public String getMakeAd() {
			if (isAccessible)
			{ return runner.getIP().getIPValue(); }

			throw new RuntimeException("Access forbidden for IP source");
		}}
}
