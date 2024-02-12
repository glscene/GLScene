program PhysicsD;

uses
  Vcl.Forms,
  fPhysicsD in 'fPhysicsD.pas' {frmSandbox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSandbox, frmSandbox);
  Application.Run;
end.
