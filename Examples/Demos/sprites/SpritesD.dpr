program SpritesD;

uses
  Vcl.Forms,
  fdSprites in 'fdSprites.pas' {FormSprites};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormSprites, FormSprites);
  Application.Run;
end.
