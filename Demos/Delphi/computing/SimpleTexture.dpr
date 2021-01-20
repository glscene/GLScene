program SimpleTexture;

uses
  Forms,
  SimpleTextureFm in 'SimpleTextureFm.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
