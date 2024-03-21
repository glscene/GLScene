program SoundD;

uses
  Vcl.Forms,
  fdSound in 'fdSound.pas' {frmSandbox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSandbox, frmSandbox);
  Application.Run;
end.
