(* 
   Archipelago GLScene advdemo.

   This advdemo illustrates several GLScene components:
   - TerrainRenderer, used with a material library
   - TerrainRenderer's OnHeightDataPostRender, used to render sea surface
   - HeightTileFileHDS, used as primary elevation datasource
   - CustomHDS, used to attach texturing information to the elevation samples
   - DirectOpenGL, used to render the sailboat's wake

   Note that both custom OpenGL rendering sections are interrelated, the sea
   surface rendering code also setups the stencil buffer, which is used by
   the wake rendering code.
   Eric Grange
   
   Credits:
   - Terrain elevation map and textures : Mattias Fagerlund
   - Sailboat model and textures : Daniel Polli 
*)

program ArchipelagoD;

uses
  Forms,
  fArchipelagoD in 'fArchipelagoD.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Archipelago';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
