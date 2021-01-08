program AllSandbox;

uses
  Vcl.Forms,
  fAllSandbox in 'fAllSandbox.pas' {FormAllSandox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormAllSandox, FormAllSandox);
  Application.Run;
end.
