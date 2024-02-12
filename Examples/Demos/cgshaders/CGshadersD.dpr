program CGshadersD;

uses
  Vcl.Forms,
  fCGshadersD in 'fCGshadersD.pas' {frmCGshaders};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCGshaders, frmCGshaders);
  Application.Run;
end.
