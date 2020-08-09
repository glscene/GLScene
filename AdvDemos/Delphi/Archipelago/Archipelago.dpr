{: Archipelago GLScene demo.

   This demo illustrates several GLScene components:
   - TerrainRenderer, used with a material library
   - TerrainRenderer's OnHeightDataPostRender, used to render sea surface
   - HeightTileFileHDS, used as primary elevation datasource
   - CustomHDS, used to attach texturing information to the elevation samples
   - DirectOpenGL, used to render the sailboat's wake

   Note that both custom OpenGL rendering sections are interrelated, the sea
   surface rendering code also setups the stencil buffer, which is used by
   the wake rendering code.

   Credits:
   - Terrain elevation map and textures : Mattias Fagerlund
     (http://www.cambrianlabs.com/Mattias/)
   - Sailboat model and textures : Daniel Polli / Daniel@dansteph.com
     (http://virtualsailor.dansteph.com)

   Eric Grange (http://glscene.org)
}
program Archipelago;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Archipelago';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
