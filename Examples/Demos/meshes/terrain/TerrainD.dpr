{: Basic terrain rendering demo.

   This demo showcases the TerrainRenderer, some of the SkyDome features
   and bits of 3D sound 'cause I got carried over ;)
   The terrain HeightData is provided by a TGLBitmapHDS (HDS stands for
   "Height Data Source"), and displayed by a TGLTerrainRenderer.

   The base terrain renderer uses a hybrid ROAM/brute-force approach to
   rendering terrain, by requesting height data tiles, then rendering them
   using either triangle strips (for those below "QualityDistance") or ROAM
   tessellation.
   Note that if the terrain is wrapping in this sample (to reduce the required
   datasets size), the engine is *not* aware of it and does not exploit this
   fact in any way: it considers just an infinite terrain.

   Controls:<ul>
   <li>Direction keys move the came nora (shift to speedup)
   <li>PageUp/PageDown move the camera up and down
   <li>Orient the camera freely by holding down the left button
   <li>Toggle wireframe mode with 'w'
   <li>Increase/decrease the viewing distance with '+'/'-'.
   <li>Increase/decrease CLOD precision with '*' and '/'.
   <li>Increase/decrease QualityDistance with '9' and '8'.
   <li>'n' turns on 'night' mode, 'd' turns back to 'day' mode.
   <li>Toggle star twinkle with 't' (night mode only)
   <li>'l' turns on/off the lens flares
   </ul>

   When increasing the range, or moving after having increased the range you
   may notice a one-time slowdown, this originates in the base height data
   being duplicated to create the illusion of an "infinite" terrain (at max
   range the visible area covers 1024x1024 height samples, and with tiles of
   size 16 or less, this is a lot of tiles to prepare).

   Misc. note: since the whole viewer is fully repainted at each frame,
   it was possible to set roNoColorBufferClear in the Viewer.Buffer.ContextOptions,
   which allows to gain a few more frames per second (try unsetting it).
}
program TerrainD;

uses
  Forms,
  fTerrainD in 'fTerrainD.pas' {FormTerrain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormTerrain, FormTerrain);
  Application.Run;
end.
