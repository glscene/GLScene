//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.FileQ3BSP;

(*
  Support-code to load Q3BSP Files into TGLFreeForm-Components in GLScene.
  Note that you must manually add this unit to one of your project's uses
  to enable support for OBJ & OBJF at run-time.
*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,

  GLS.VectorFileObjects,
  GLS.ApplicationFileIO,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.VectorLists,
  Formats.Q3BSP,
  GLS.MeshBSP,
  GLS.Texture,
  GLS.Graphics,
  GLS.State,
  GLS.Utils,
  GLS.Material,
  GLS.TextureFormat;

type

  //The Q3BSP vector file (Quake III BSP).
  TGLQ3BSPVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
     procedure LoadFromStream(aStream: TStream); override;
  end;

var
  // Q3 lightmaps are quite dark, we brighten them a lot by default
  vQ3BSPLightmapGammaCorrection: Single = 2.5;
  vQ3BSPLightmapBrightness: Single = 2; // scaling factor, 1.0 = unchanged
  vGLFileQ3BSPLoadMaterials: boolean = True; // Mrqzzz : Flag to avoid loading materials (useful for IDE Extentions like GlaredX)

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLSTLVectorFile ------------------
// ------------------

class function TGLQ3BSPVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead];
end;

procedure TGLQ3BSPVectorFile.LoadFromStream(aStream: TStream);

  function LocateTextureFile(const texName: string): string;
  begin
    if FileStreamExists(texName + '.bmp') then
      Result := texName + '.bmp'
    else if FileStreamExists(texName + '.jpg') then
      Result := texName + '.jpg'
    else if FileStreamExists(texName + '.tga') then
      Result := texName + '.tga'
    else
      Result := '';
  end;

  function GetOrAllocateMaterial(const matName: string): string;
  var
    matLib: TGLMaterialLibrary;
    libMat: TGLLibMaterial;
    texName: string;
  begin
    if GetOwner is TGLBaseMesh then
    begin
      // got a linked material library?
      matLib := TGLBaseMesh(GetOwner).MaterialLibrary;
      if Assigned(matLib) then
      begin
        Result := matName;
        libMat := matLib.Materials.GetLibMaterialByName(matName);
        if not Assigned(libMat) then
        begin
          if Pos('.', matName) < 1 then
          begin
            texName := LocateTextureFile(matName);
            if texName = '' then
              texName := LocateTextureFile(Copy(matName, LastDelimiter('\/', matName) + 1, MaxInt));
          end
          else
            texName := matName;
          with matLib.AddTextureMaterial(matName, texName) do
          begin
            Material.Texture.TextureMode := tmModulate;
          end;
        end;
      end
      else
        Result := '';
    end
    else
      Result := '';
  end;

var
  bsp: TQ3BSP;
  mo: TBSPMeshObject;
  fg, lastfg: TFGBSPNode;
  i, j, n, y: Integer;
  facePtr: PBSPFace;
  lightmapLib: TGLMaterialLibrary;
  lightmapBmp: TBitmap;
  libMat: TGLLibMaterial;
  bspLightMap: PBSPLightmap;
  plane: THmgPlane;
begin
  bsp := TQ3BSP.Create(aStream);
  try
    mo := TBSPMeshObject.CreateOwned(Owner.MeshObjects);

    // import all materials
    if vGLFileQ3BSPLoadMaterials then
    begin
      for i := 0 to High(bsp.Textures) do
      begin
        GetOrAllocateMaterial(Trim(string(StrPas(bsp.Textures[i].TextureName))));
      end;
    end;

    // import all lightmaps
    lightmapLib := Owner.LightmapLibrary;
    if Assigned(lightmapLib) and vGLFileQ3BSPLoadMaterials then
    begin
      // import lightmaps
      n := bsp.NumOfLightmaps;
      lightmapBmp := TBitmap.Create;
      try
        lightmapBmp.PixelFormat := pf24bit;
        lightmapBmp.Width := 128;
        lightmapBmp.Height := 128;
        for i := 0 to n - 1 do
        begin
          bspLightMap := @bsp.Lightmaps[i];
          // apply brightness correction if ant
          if vQ3BSPLightmapBrightness <> 1 then
            BrightenRGBArray(@bspLightMap.imageBits[0], 128 * 128,
              vQ3BSPLightmapBrightness);
          // apply gamma correction if any
          if vQ3BSPLightmapGammaCorrection <> 1 then
            GammaCorrectRGBArray(@bspLightMap.imageBits[0], 128 * 128,
              vQ3BSPLightmapGammaCorrection);
          // convert RAW RGB to BMP
          for y := 0 to 127 do
            BGR24ToRGB24(@bspLightMap.imageBits[y * 128 * 3],
            lightmapBmp.ScanLine[127 - y], 128);
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

    // import all geometry
    mo.Vertices.AdjustCapacityToAtLeast(bsp.NumOfVerts);
    mo.Normals.AdjustCapacityToAtLeast(bsp.NumOfVerts);
    mo.TexCoords.AdjustCapacityToAtLeast(bsp.NumOfVerts);
    for i := 0 to bsp.NumOfVerts - 1 do
    begin
      mo.Vertices.Add(bsp.Vertices[i].Position);
      mo.Normals.Add(bsp.Vertices[i].Normal);
      mo.TexCoords.Add(bsp.Vertices[i].TextureCoord);
      if Assigned(lightMapLib) and vGLFileQ3BSPLoadMaterials then
        mo.LightMapTexCoords.Add(bsp.Vertices[i].LightmapCoord)
    end;
    mo.TexCoords.Scale(AffineVectorMake(1, -1, 0));
    mo.TexCoords.Translate(YVector);
    mo.RenderSort := rsBackToFront;
    // Q3 BSP separates tree nodes from leafy nodes, we don't,
    // so we place nodes first, then all leafs afterwards
    for i := 0 to bsp.NumOfNodes - 1 do
    begin
      fg := TFGBSPNode.CreateOwned(mo.FaceGroups);
      plane := bsp.Planes[bsp.Nodes[i].plane];
      plane := VectorMake(plane.X, plane.Y, plane.Z, plane.W);
      fg.SplitPlane := plane;
      fg.PositiveSubNodeIndex := bsp.Nodes[i].Children[0];
      if fg.PositiveSubNodeIndex < 0 then
        fg.PositiveSubNodeIndex := bsp.NumOfNodes - fg.PositiveSubNodeIndex - 1;
      Assert(fg.PositiveSubNodeIndex < bsp.NumOfNodes + bsp.NumOfLeaves);
      Assert(fg.PositiveSubNodeIndex > 0);
      fg.NegativeSubNodeIndex := bsp.Nodes[i].Children[1];
      if fg.NegativeSubNodeIndex < 0 then
        fg.NegativeSubNodeIndex := bsp.NumOfNodes - fg.NegativeSubNodeIndex - 1;
      Assert(fg.NegativeSubNodeIndex < bsp.NumOfNodes + bsp.NumOfLeaves);
      Assert(fg.NegativeSubNodeIndex > 0);
    end;
    // import all leaves
    for i := 0 to bsp.NumOfLeaves - 1 do
      TFGBSPNode.CreateOwned(mo.FaceGroups);
    // import all faces into leaves & subnodes
    for i := 0 to bsp.NumOfLeaves - 1 do
    begin
      lastfg := nil;
      for j := 0 to bsp.Leaves[i].NumFaces - 1 do
      begin
        n := bsp.Leaves[i].FirstFace + j;
        if n >= bsp.NumOfFaces then
          Break; // corrupted BSP?
        facePtr := @bsp.Faces[n];
        if facePtr.FaceType = FACE_POLYGON then
        begin
          if lastfg = nil then
            fg := TFGBSPNode(mo.FaceGroups[i + bsp.NumOfNodes])
          else
          begin
            lastfg.PositiveSubNodeIndex := mo.FaceGroups.Count;
            fg := TFGBSPNode.CreateOwned(mo.FaceGroups);
          end;
          // check for BSP corruption
          if Cardinal(facePtr.textureID) <= Cardinal(bsp.NumOfTextures) then
            fg.MaterialName := Trim(string(StrPas(bsp.Textures[facePtr.textureID].TextureName)));
          if Assigned(lightmapLib) and vGLFileQ3BSPLoadMaterials then
            fg.LightMapIndex := facePtr.lightmapID;
          lastfg := fg;
          // Q3 Polygon Faces are actually fans, but winded the other way around!
          fg.Mode := fgmmTriangleFan;
          fg.VertexIndices.Add(facePtr.startVertIndex);
          fg.VertexIndices.AddSerie(facePtr.startVertIndex + facePtr.numOfVerts - 1,
            -1,
            facePtr.numOfVerts - 1);
          // there are also per-leaf mesh references... dunno what they
          // are for, did not encounter them so far... If you have a BSP
          // that has some, and if you know how to make use of them, shout!

          // Copy the cluster index, used for visibility determination
          fg.Cluster := bsp.Leaves[i].Cluster;
        end;
      end;
    end;

    // Copy the visibility data
    if bsp.VisData.numOfClusters > 0 then
      mo.ClusterVisibility.SetData(
        @bsp.VisData.bitSets[0], bsp.VisData.numOfClusters);
  finally
    bsp.Free;
  end;
  // Some BSP end up with empty nodes/leaves (information unused, incorrept BSP...)
  // This call takes care of cleaning up all the empty nodes
  mo.CleanupUnusedNodes;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterVectorFileFormat('q3bsp', 'Quake3 BSP files', TGLQ3BSPVectorFile);

  // registering this extension too might be a little abusive right now...
  RegisterVectorFileFormat('bsp', 'BSP files', TGLQ3BSPVectorFile);
end.

