//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FileOCT;
(*
    Support-code to load OCT Files into TgxFreeForm-Components in GLScene.
    (OCT being the format output from FSRad, http://www.fluidstudios.com/fsrad.html).
*)
interface

{$I GXS.Scene.inc}

uses
  System.SysUtils,
  System.Classes,
  FMX.Graphics,

  GXS.VectorFileObjects,
  GXS.VectorGeometry,
  GXS.ApplicationFileIO,
  GXS.Texture,
  GXS.Material,
  GXS.Graphics,
  GXS.State,
  GXS.Utils,
  GXS.TextureFormat,

  Formatx.OCT;

type

  // The OCT vector file (FSRad output).
  TgxOCTgxVectorFile = class(TgxVectorFile)
  public
    class function Capabilities: TDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
  end;

var
  // Scaling factor, 1.0 = unchanged
  vFileOCTLightmapBrightness: single = 1;
  // Scaling factor, 1.0 = unchanged
  vFileOCTLightmapGammaCorrection: single = 1;
  // Flag to avoid loading materials (useful for IDE Extensions or scene editors)
  vFileOCTAllocateMaterials: boolean = True;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TgxOCTgxVectorFile ------------------
// ------------------

class function TgxOCTgxVectorFile.Capabilities: TDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TgxOCTgxVectorFile.LoadFromStream(aStream: TStream);
var
  i, y, n: integer;
  oct: TOCTFile;
  octFace: POCTFace;
  octLightmap: POCTLightmap;
  mo: TgxMeshObject;
  fg: TfgxVertexIndexList;
  lightmapLib: TgxMaterialLibrary;
  lightmapBmp: TBitmap;
  libMat: TgxLibMaterial;
begin
  oct := TOCTFile.Create(aStream);
  try
    mo := TgxMeshObject.CreateOwned(Owner.MeshObjects);
    mo.Mode := momFaceGroups;

    lightmapLib := Owner.LightmapLibrary;
    if (Assigned(lightmapLib)) and (vFileOCTAllocateMaterials) then
    begin
      // import lightmaps
      n := oct.Header.numLightmaps;
      lightmapBmp := TBitmap.Create;
      try
        // TODO : E2129 Cannot assign to a read-only property
        (*lightmapBmp.PixelFormat := glpf24bit;*)
        lightmapBmp.Width := 128;
        lightmapBmp.Height := 128;
        for i := 0 to n - 1 do
        begin
          octLightmap := @oct.Lightmaps[i];
          // Brightness correction
          if vFileOCTLightmapBrightness <> 1.0 then
            BrightenRGBArray(@octLightmap.map,
              lightmapBmp.Width * lightmapBmp.Height,
              vFileOCTLightmapBrightness);
          // Gamma correction
          if vFileOCTLightmapGammaCorrection <> 1.0 then
            GammaCorrectRGBArray(@octLightmap.map,
              lightmapBmp.Width * lightmapBmp.Height,
              vFileOCTLightmapGammaCorrection);
          // convert RAW RGB to BMP
          for y := 0 to 127 do
          // TODO : E2003 Undeclared identifier: 'ScanLine'
          (*
          Move(octLightmap.map[y * 128 * 3],
             lightmapBmp.ScanLine[127 - y]^, 128 * 3);
          *)
          // spawn lightmap
          libMat := lightmapLib.AddTextureMaterial(IntToStr(i), lightmapBmp);
          with libMat.Material.Texture do
          begin
            MinFilter := miLinear;
            TextureWrap := twNone;
            TextureFormat := tfRGB;
          end;
        end;
      finally
        lightmapBmp.Free;
      end;
    end;

    // import geometry
    n := oct.Header.numVerts;

    mo.Vertices.AdjustCapacityToAtLeast(n);
    mo.TexCoords.AdjustCapacityToAtLeast(n);
    mo.LightMapTexCoords.AdjustCapacityToAtLeast(n);
    for i := 0 to n - 1 do
      with oct.Vertices[i] do
      begin
        mo.Vertices.Add(pos.X, pos.Y, pos.Z);
        mo.TexCoords.Add(tv.s, tv.t);
        mo.LightMapTexCoords.Add(lv.s, lv.t);
      end;
    // import faces
    n := oct.Header.numFaces;
    for i := 0 to n - 1 do
    begin
      octFace := @oct.Faces[i];
      fg := TfgxVertexIndexList.CreateOwned(mo.FaceGroups);
      fg.Mode := fgmmTriangleFan;
      fg.VertexIndices.AddSerie(octFace.start, 1, octFace.num);
      if (Assigned(lightmapLib)) and (vFileOCTAllocateMaterials) then
        fg.LightMapIndex := octFace.lid;
    end;

  finally
    oct.Free;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterVectorFileFormat('oct', 'FSRad OCT files', TgxOCTgxVectorFile);

end.

