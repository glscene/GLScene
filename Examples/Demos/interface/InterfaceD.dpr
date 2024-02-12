program InterfaceD;

uses
  Vcl.Forms,
  fInterfaceD in 'fInterfaceD.pas' {frmSandbox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSandbox, frmSandbox);
  Application.Run;
end.
