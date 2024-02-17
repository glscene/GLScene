(*
  Ode Terrain
  Getting a GLODEManager and GLTerrainRenderer talking.
  Use '1' -> '5' keys to drop objects onto Terrain.
*)
program OdeTerrainD;

uses
  Forms,
  fOdeTerrainD in 'fOdeTerrainD.pas' {FormOdeTerrain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormOdeTerrain, FormOdeTerrain);
  Application.Run;
end.
