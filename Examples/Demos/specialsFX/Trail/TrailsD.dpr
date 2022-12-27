{
 Demo showing GLTrail object
}
program TrailsD;

uses
  Forms,
  fTrailsD in 'fTrailsD.pas' {FormTrails};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTrails, FormTrails);
  Application.Run;
end.
