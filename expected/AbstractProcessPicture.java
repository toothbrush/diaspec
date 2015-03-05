package fr.diaspec.webcam.generated;

abstract public class AbstractProcessPicture 
  extends Publisher<Bitmap> 
  implements Subscriber<Bitmap>, Context {

	
	@SuppressWarnings("unused")
	private AbstractRunner runner;

	final public void init(AbstractRunner runner) {
		this.runner = runner;
	}

    protected abstract Bitmap onCameraProvided(Bitmap pictureProvided);

    @Override
    final public void trigger(Bitmap value) {
        
    	Bitmap v = onCameraProvided(value);
        notify(v);
    }
}
