package fr.diaspec.webcam.generated;

public abstract class AbstractCamera extends Publisher<Bitmap> implements Source {
   
	// we immediately fire
	// the request for a picture, and hope that the device uses notify(Bitmap b) later.
	
	protected abstract Bitmap getCameraValue();
	protected AbstractRunner runner;

	public void init(AbstractRunner runner) {
		this.runner = runner;
	}
}
