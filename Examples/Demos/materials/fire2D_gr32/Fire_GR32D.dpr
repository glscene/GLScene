{:
  A demo for using Alex Denissov's Graphics32 library (http://www.g32.org)
  to generate 2D texture for use with GLScene.

  By Nelson Chu

  Try lighting the white line near the bottom of the window with your mouse
  pointer and see the fire spreads. Press ESC to quit.

  To use Graphics32 with GLScene:

  1. Make sure GLS_Graphics32_SUPPORT is defined in GLSCene.inc. Recompile if
     needed.
  2. In your program, use code like:

       GLTexture.Image.GetBitmap32(0).assign(Bitmap32);
       GLTexture.Image.NotifyChange(self);

     to assign the Bitmap32 to your GLScene texture and notify GLScene.

  To get fast assignment, remember to make the dimensions of your Bitmap32 equal
  to a power of two, so that GLScene doesn't need to do conversion internally.

  In this sample program, a 256 x 256 Graphics32 TByteMap is used to generate a
  "fire" image. At each frame, the fire image is first "visualized" in a
  Graphics32 Bitmap32. Then, the TBitmap32 is copied to the texture of a Cube.
}
program Fire_GR32D;

uses
  Forms,
  fFire_GR32D in 'fFire_GR32D.pas' {FormFire2d_GR32};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormFire2d_GR32, FormFire2d_GR32);
  Application.Run;
end.
