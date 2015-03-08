package fr.diaspec.webcam.generated;
public abstract class AbstractCamera extends Publisher<Bitmap> implements Source<Bitmap>
{
  protected abstract Bitmap getCameraValue ()
  ;
  public Bitmap requireValue ()
  {
    return getCameraValue();
  }
}