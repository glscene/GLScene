(* Shaded terrain rendering demo *)
program ShadedTerrain;

uses
  Forms,
  ShadedTerrainFm in 'ShadedTerrainFm.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormShadedTerrain, FormShadedTerrain);
  Application.Run;
end.
