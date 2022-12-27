program SplitterD;

uses
  Forms,
  fSplitterD in 'fSplitterD.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
