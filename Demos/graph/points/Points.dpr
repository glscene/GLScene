(*
   Demo of the TGLPoints component.

   The component is specialized in rendering large numbers of points,
   with ability to adjust point style (from fast square point to smooth
   round points) and point parameters.
   The point parameters define how point size is adjusted with regard
   to eye-point distance (to make farther points smaller, see ARB_point_parameters
   for more details).
   The component is also suitable for particle systems, but offers less
   flexibility than the TGLParticleFX.
*)
program Points;

uses
  Forms,
  PointsFm in 'PointsFm.pas' {FormPoints};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPoints, FormPoints);
  Application.Run;
end.
