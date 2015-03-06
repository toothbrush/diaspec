package fr.diaspec.webcam.generated;

abstract public class AbstractComposeDisplay extends Publisher<Bitmap> implements Context, Subscriber<Bitmap> {

    private AbstractRunner runner;

    final void init(AbstractRunner runner) {
        this.runner = runner;
    }

    protected abstract Maybe<Bitmap> onProcessPictureProvided(Bitmap modifiedPic,
    		MakeAdProxy discover);

    @Override
	final public void trigger(Bitmap value) {
        MakeAdProxy proxy = new MakeAdProxy();
        proxy.setAccessible(true);
        Maybe<Bitmap> v = onProcessPictureProvided(value, proxy);
        proxy.setAccessible(false);
        if (v instanceof Just)
            notify(((Just<Bitmap>) v).just_value);
    }

    protected final class MakeAdProxy {

        private MakeAdProxy() {}

        private boolean isAccessible = false;

        final private void setAccessible(boolean isAccessible) {
            this.isAccessible = isAccessible;
        }

        final public String makeAdValue() {
        	if (isAccessible) {
        		return runner.getMakeAd().requireValue();
        	}
        	throw new RuntimeException("Access forbidden for Advert source");
        }}}
