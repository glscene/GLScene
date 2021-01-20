{: A bare-bones sample for TGLHUDText.

   To use a TGLHUDText, you must first place a TGLBitmapFont component and specify
   a font bitmap and it character ranges (ie. which tile represents which
   character). The component allows for a wide variety of fixed-width font
   bitmaps, and you can reuse many of the old bitmap fonts sets that were
   written for Atari, Amiga etc.

   The TGLHUDText can then be placed in the hierarchy: just link it to the
   TGLBitmapFont, specify a text, alignment, layout, scale and position to
   whatever suits your need and that's all.

   Clicking on the viewer will hide/show the teapot (when teapot is on, the
   framerate is much lower, f.i. on my GF3 / K7 1.2, the rating can reach
   1050FPS with teapot off)
}
program Bmpfont;

uses
  Forms,
  BmpFontFm in 'BmpFontFm.pas' {FormBmpFont};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormBmpFont, FormBmpFont);
  Application.Run;
end.
