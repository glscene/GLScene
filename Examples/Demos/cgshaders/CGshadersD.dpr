program CGshadersD;

uses
  Vcl.Forms,
  fdCGshaders in 'fdCGshaders.pas' {frmCGshaders};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCGshaders, frmCGshaders);
  Application.Run;
end.
