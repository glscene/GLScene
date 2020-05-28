{: Render To Bitmap sample.

   This demo illustrates the two ways to obtain a 3D scene in a bitmap.<br>
   The first, which is also the fastest, is to use CreateSnapShot. It allows
   to obtain what you see on the screen, as it was rendered by the 3D acceleration
   device if any is available.<br>
   The second is to use RenderToBitmap. This makes use of Software OpenGL
   rendering and is significantly slower, but you can render to any size,
   including to bitmap much larger than the screen which are suited for use
   in printed documents for instance.

   Note that since RenderToBitmap uses software OpenGL, the output may be
   different from what you get with hardware acceleration, not only because
   the rasterizer is different, but also because the software implementation
   may not support the same feature set.
}

program tobitmap;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
