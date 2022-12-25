program SkyClouds;

uses
  Forms,
  fSkyClouds in 'fSkyClouds.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
