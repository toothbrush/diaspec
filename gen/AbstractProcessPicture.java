package fr.diaspec.webcam.generated;
public abstract class AbstractProcessPicture extends Publisher<Bitmap> implements Context, Subscriber<Bitmap>
{
  protected abstract Bitmap onCameraProvided (Bitmap newValue)
  ;
  public final void trigger (Bitmap value)
  {
    Bitmap v = onCameraProvided(value);
    notify(v);
  }
  private AbstractRunner runner;
  final protected void init (AbstractRunner runner)
  {
    this.runner = runner;
  }
}