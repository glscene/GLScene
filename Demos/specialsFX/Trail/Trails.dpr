{
 Demo showing GLTrail object
}
program Trails;

uses
  Forms,
  TrailsFm in 'TrailsFm.pas' {FormTrails};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTrails, FormTrails);
  Application.Run;
end.
