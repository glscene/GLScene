program GLSLshadersD;

uses
  Vcl.Forms,
  fGLSLshadersD in 'fGLSLshadersD.pas' {frmSandbox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSandbox, frmSandbox);
  Application.Run;
end.
