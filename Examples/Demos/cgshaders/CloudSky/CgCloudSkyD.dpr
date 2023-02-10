(*
cg Sky Shaders, key:
d - day
n - night
c - clear weather
s - cloudy weather
*)
program CgCloudSkyD;

uses
  Forms,
  fCloudSkyD in 'fCloudSkyD.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
