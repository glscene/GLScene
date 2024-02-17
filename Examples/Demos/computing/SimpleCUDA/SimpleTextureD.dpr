program SimpleTextureD;

uses
  Forms,
  fSimpleTexD in 'fSimpleTexD.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
