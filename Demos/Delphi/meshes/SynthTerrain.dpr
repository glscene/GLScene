{: Synthetic terrain demo.

   This demo covers use of TGLCustomHDS to supply custome elevation info
   to the terrain engine as well as per-tile texturing. It also showcases
   use of simple 1D texturing as an alternative to per-vertex coloring.

   If you're after "nice" terrain rendering, check the "terrain" demo instead,
   this one is more about understanding "what goes inside" than obtaining a
   beautiful output - though obtaining beautiful output without understanding
   may not be that easy ;)

   All you need to understand to use TGLCustomHDS is what goes on in the
   OnStartPreparingData. You can have a quick look at the FormCreate, but there
   is nothing special: it setups initial camera position and prepares 3 1D
   textures for use in the terrain rendering.
   When implementing OnStartPreparingData, keep in mind that this event is a
   request from the terrain rendering engine, it asks for elevation and texture
   data for a new tile that comes in visibility range (or was marked directy
   and rhas new data, f.i. in the case of dynamic terrain). The engine requires
   its data in a specific format, the code revolving around DataType is there
   for that purpose If you are in an application specific context, this phase
   may be unnecessary (just prepare the data in the format you were asked).
   The TGLHeightData you receive is empty, meaning you've got to allocate it first
   (with the Allocate method), and then fill it using one of the various properties
   (see TGLHeightData).
   The material is specified by the MaterialName property (the material library
   being linked at the TGLTerrainRenderer level). Materials used can be dynamic
   between frames, but must remain coherent throughout a frame, and for as long
   as the TGLHeightData where you specified the material remains alive.
}
program SynthTerrain;

uses
  Forms,
  SynthTerrainFm in 'SynthTerrainFm.pas' {FormSynthTerrain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSynthTerrain, FormSynthTerrain);
  Application.Run;
end.
