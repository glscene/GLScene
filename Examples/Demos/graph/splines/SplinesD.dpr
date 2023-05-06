(*
   This is a quick demo for the TGLLines object and spline functionality.

   TGLLines can handle normal lines and cubic splines, each node can have a
   different color, and the line can be color-interpolated.

   Note that the camera in this sample is in <i>orthogonal</i> mode, this makes
   for a quick and easy way to work in 2D with OpenGL (try switching the camera
   to perpective mode if you don't see the point).
*)
program SplinesD;

uses
  Forms,
  fSplinesD in 'fSplinesD.pas' {FormSplines};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSplines, FormSplines);
  Application.Run;
end.
