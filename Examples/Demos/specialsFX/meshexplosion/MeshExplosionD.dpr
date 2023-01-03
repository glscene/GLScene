(*
Explosion FX Demo

This project demonstrates the use of TGLBExplosionFx. Nothing out
of ordinary as one can see. Load the mesh, load the default settings,
click "on" to initiate the demo, "reset" to reset :)

The information of the mesh is cached on the cache variable, that is
restored every time the demo is reseted. The MaxSteps property defines
the max number of frames the explosion will be rendered. Speed is the
scalar speed each face is issued in the rendering

Original version by Matheus, matheus@tilt.net
*)

program MeshExplosionD;

uses
  Forms,
  fMeshExplosionD in 'fMeshExplosionD.pas' {FormMeshExplosion};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMeshExplosion, FormMeshExplosion);
  Application.Run;
end.
