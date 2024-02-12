program SpritesD;

uses
  Vcl.Forms,
  fSpritesD in 'fSpritesD.pas' {frmSprites};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSprites, frmSprites);
  Application.Run;
end.
