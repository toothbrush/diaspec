package fr.diaspec.webcam.generated;

abstract public class AbstractProcessPicture 
  extends Publisher<Bitmap> 
  implements Subscriber<Bitmap>, Context {

	
	@SuppressWarnings("unused")
	private AbstractRunner runner;

    void init(AbstractRunner runner) {
        this.runner = runner;
    }

    protected abstract Bitmap onCameraProvided(Bitmap pictureProvided);

    @Override
	public void trigger(Bitmap value) {
        
    	Bitmap v = onCameraProvided(value);

        notify(v);
    }
}
