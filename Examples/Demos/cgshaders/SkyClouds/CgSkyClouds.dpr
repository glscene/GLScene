(*
cg Sky Shaders, key:
d - day
n - night
c - clear weather
s - cloudy weather
*)
program CgSkyClouds;

uses
  Forms,
  fSkyClouds in 'fSkyClouds.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
