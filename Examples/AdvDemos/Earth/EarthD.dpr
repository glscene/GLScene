(*
  "Earth"
   The atmospheric effect is rendered in GLDirectOpenGL1Render, which essentially
   renders a disk, with color of the vertices computed via ray-tracing. Not that
   the tesselation of the disk has been hand-optimized so as to reduce CPU use
   while retaining quality.
   Stars support is built into the TGLSkyDome, but constellations are rendered
   via a TGLLines, which is filled in the LoadConstellationLines method.
*)
program EarthD;

uses
  Forms,
  fEarthD in 'fEarthD.pas' {FormEarth};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormEarth, FormEarth);
  Application.Run;
end.
