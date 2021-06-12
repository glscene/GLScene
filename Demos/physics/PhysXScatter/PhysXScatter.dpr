program PhysXScatter;

uses
  Forms,
  PhysXScatterFm in 'PhysXScatterFm.pas' {FormPhysXScatter};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPhysXScatter, FormPhysXScatter);
  Application.Run;
end.
