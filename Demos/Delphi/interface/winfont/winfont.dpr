{: A demo for TGLHUDText using the WindowsBitmapFont component.

   The WindowsBitmapFont can automatically generate a font texture based on
   one of the standard windows fonts. The texture dimensions are automatically
   computed to maximize the texture's fill ratio, up to a size of 512x512,
   with the usual ASCII character range being the default.<br>
   Should you happen to require larger fonts (that do not fit the max texture
   size), you can try to reduce the default range, or split it and request
   only the characters you will actually use.

   Clicking on the viewer will hide/show the teapot (when teapot is on, the
   framerate is much lower, f.i. on my GF3 / K7 1.2, the rating can easily
   reach 950FPS with teapot off)
}
program Winfont;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
