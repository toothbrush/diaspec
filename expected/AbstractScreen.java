package fr.diaspec.webcam.generated;

public abstract class AbstractScreen implements Action {
    
	protected abstract void doScreenAction(Bitmap newVisual);
	protected CommonRuncode runner;

	// note that resources are allowed to mess with init.
	// this is because e.g. sources may notify() of their
	// own accord, so it might be useful to be able to
	// start a thread.
	public void init(CommonRuncode runner) {
		this.runner = runner;
	}
   
}
