program Sandbox;

uses
  Vcl.Forms,
  SandboxFm in 'SandboxFm.pas' {FormSandox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSandox, FormSandox);
  Application.Run;
end.
