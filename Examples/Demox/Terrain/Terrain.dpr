program Terrain;

uses
  System.StartUpCopy,
  FMX.Forms,
  fdTerrain in 'fdTerrain.pas' {FormTerrain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTerrain, FormTerrain);
  Application.Run;
end.
