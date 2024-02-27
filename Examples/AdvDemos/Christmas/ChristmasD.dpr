(* GLScene Christmas "ScreenSaver".

   The scene is made up from a few meshes, some GLScene objects, several
   lens-flares and particle effects components, and a 2 text. BASS is used
   for the sound part (3D positioned fire loop, and mp3 playback).
   Wrapped gifts appear around Christmas every year.

   Models: from 3DCafe.com
   Textures: various origins, some from 3dtextures.fr.st, others made by Eric Grange
   Music: unknown origin, was in a "royalty free" download package
*)

program ChristmasD;

uses
  Vcl.Forms,
  GLS.SoundManager,
  fChrismasD in 'fChrismasD.pas';

{$E .scr}

{$R *.res}

begin
  // don't complain about missing sound support
  vVerboseGLSMErrors := False;
  Application.Initialize;
  Application.Title := 'GLScene Christmas';
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
