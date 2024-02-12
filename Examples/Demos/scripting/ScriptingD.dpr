program ScriptingD;

uses
  Vcl.Forms,
  fScriptingD in 'fScriptingD.pas' {frmSandbox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSandbox, frmSandbox);
  Application.Run;
end.
