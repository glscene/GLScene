//
{: Using Verlet Hair with ODE; Fur Balls }
//
program OdeFurball;

uses
  Forms,
  OdeFurballFm in 'OdeFurballFm.pas' {FormFurball};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFurball, FormFurball);
  Application.Run;
end.
