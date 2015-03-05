package fr.diaspec.webcam.generated;

public abstract class AbstractDisplay implements Controller, Subscriber<Bitmap>  {

	protected abstract void whenProvidedComposeDisplay(Bitmap newValue, ScreenProxy fs);

	private AbstractRunner runner;

	final public void init(AbstractRunner runner) {
		this.runner = runner;
	}

	//public is the only modifier allowed in interface declarations,
	//which is where trigger() comes from
	@Override
	final public void trigger(Bitmap value) {
		ScreenProxy proxy = new ScreenProxy();
		proxy.setAccessible(true);
		whenProvidedComposeDisplay(value, proxy);
		proxy.setAccessible(false);
	}

	final protected class ScreenProxy {

		protected ScreenProxy() {}

		private boolean isAccessible = false;

		final private void setAccessible(boolean isAccessible) {
			this.isAccessible = isAccessible;
		}

		final public void doScreenAction(Bitmap newVisual) {
			if (isAccessible) {
				runner.getScreen().doScreenAction(newVisual);
			} else {
				throw new RuntimeException("Access forbidden for Screen action");
			}}}}
