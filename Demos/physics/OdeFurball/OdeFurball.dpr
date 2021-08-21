//
{: Using Verlet Hair with ODE; Fur Balls }
//
program OdeFurball;

uses
  Forms,
  fOdeFurball in 'fOdeFurball.pas' {FormFurball};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFurball, FormFurball);
  Application.Run;
end.
