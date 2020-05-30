{:  Ode Terrain
     Getting a GLODEManager and GLTerrainRenderer talking.
    Use '1' -> '5' keys to drop objects onto Terrain.
}
program OdeTerrain;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
