//
(* Using Verlet Hair with ODE; Fur Balls *)
//
program OdeFurballD;

uses
  Forms,
  fOdeFurballD in 'fOdeFurballD.pas' {FormFurball};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFurball, FormFurball);
  Application.Run;
end.
