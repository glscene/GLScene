(* Shaded terrain rendering demo *)
program ShadedTerrainD;
uses
  Forms,
  fShadedTerrainD in 'fShadedTerrainD.pas' {FormShadedTerrain};

{$R *.RES}
begin
  Application.Initialize;
  Application.CreateForm(TFormShadedTerrain, FormShadedTerrain);
  Application.Run;
end.
