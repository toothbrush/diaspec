package fr.diaspec.webcam.generated;
public abstract class AbstractScreen implements Action<Bitmap>
{
  protected abstract void doScreenAction (Bitmap value)
  ;
  public void trigger (Bitmap value)
  {
    doScreenAction(value);
  }
}