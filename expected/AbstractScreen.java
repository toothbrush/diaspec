package fr.diaspec.webcam.generated;

public abstract class AbstractScreen implements Action<Bitmap> {
    
	@Override
	public void trigger(Bitmap value) {
		doScreenAction(value);
	}

	protected abstract void doScreenAction(Bitmap newVisual);
	   
}
