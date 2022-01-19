(*
  GLAtmosphere Demo.
  Note: object "Not_a_planet" is used to test atmosphere transparency issues.
*)
program AtmosphereD;

uses
  Forms,
  fAtmosphereD in 'fAtmosphereD.pas';

{$R *.res}
  
begin
  Application.Initialize;
  Application.CreateForm(TFormAtmosphere, FormAtmosphere);
  Application.Run;
end.
