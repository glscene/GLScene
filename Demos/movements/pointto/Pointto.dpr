(* Demo/test case for the PointTo method of objects.

   The PointTo method allows to easily orient an object to point toward another
   object, whatever their relative positions in the scene hierarchy.
   In this sample, we have a green sphere turning in circle and riding a sin,
   while a blue arrow, turning in a smaller circle, is maintained pointed
   toward the sphere. The other items (lines...) are just here to help visualize
   the 3D nature of the thing.
*)
program Pointto;

uses
  Forms,
  PointtoFm in 'PointtoFm.pas' {FormPointto};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPointto, FormPointto);
  Application.Run;
end.
