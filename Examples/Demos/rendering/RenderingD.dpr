program RenderingD;

uses
  Vcl.Forms,
  fRenderingD in 'fRenderingD.pas' {frmSandbox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSandbox, frmSandbox);
  Application.Run;
end.
