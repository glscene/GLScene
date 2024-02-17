program pdInterface;

uses
  Vcl.Forms,
  fdInterface in 'fdInterface.pas' {frmSandbox};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSandbox, frmSandbox);
  Application.Run;
end.
