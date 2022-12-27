//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.FileLWO;

(*  Support-code to load Lightwave LWO Files (v6.0+, partial support).*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  GLS.VectorFileObjects,
  GLS.VectorLists,
  Formats.LWO;

type
  TGLLWOVectorFile = class(TGLVectorFile)
  private
    FLWO: TLWObjectFile;
    FPnts: TLWPnts;
    procedure AddLayr(Layr: TLWLayr; LWO: TLWObjectFile);
    procedure AddSurf(Surf: TLWSurf; LWO: TLWObjectFile);
    procedure AddPnts(Pnts: TLWPnts; Mesh: TGLMeshObject);
    procedure AddPols(Pols: TLWPols; Mesh: TGLMeshObject);
    procedure AddVMap(VMap: TLWVMap; Mesh: TGLMeshObject);
  public
    procedure LoadFromStream(aStream: TStream); override;
  end;

//============================================
implementation
//============================================

uses
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.Material,
  GLS.VectorTypes;

type
  PVector3f = ^TVector3f;

function CalcTriNorm(v1, v2, v3: TVec12): TVector3f;
var
  e1, e2: TVector3f;
begin
  e1 := VectorSubtract(PVector3f(@v2)^, PVector3f(@v1)^);
  e2 := VectorSubtract(PVector3f(@v3)^, PVector3f(@v1)^);
  VectorCrossProduct(e1, e2, result);
  result := VectorNormalize(result);
end;

type
  TNormBuffer = record
    count, lasttag: TU2;
  end;
  TNormBufferDynArray = array of TNormBuffer;

//-----------------------------------------
// TGLLWOVectorFile
//-----------------------------------------

procedure TGLLWOVectorFile.AddLayr(Layr: TLWLayr; LWO: TLWObjectFile);
var
  Idx: Integer;
  Mesh: TGLMeshObject;
  Pnts: TLWPnts;
begin
  // Add mesh
  Mesh := TGLMeshObject.CreateOwned(Owner.MeshObjects);

  with Mesh do
  begin
    Name := Layr.Name;
    Mode := momFaceGroups;

    // pnts
    Idx := Layr.Items.FindChunk(@FindChunkById, @ID_PNTS);

    Pnts := TLWPnts(Layr.Items[Idx]);

    if Idx <> -1 then
      AddPnts(Pnts, Mesh);

    // vertex maps
    Idx := TLWPnts(Layr.Items[Idx]).Items.FindChunk(@FindChunkById, @ID_VMAP);

    while Idx <> -1 do
    begin
      AddVMap(TLWVMap(Pnts.Items[Idx]), Mesh);
      Idx := Pnts.Items.FindChunk(@FindChunkById, @ID_VMAP, Idx + 1);
    end;

    // Polygons
    Idx := Layr.Items.FindChunk(@FindChunkById, @ID_POLS);
    while Idx <> -1 do
    begin
      AddPols(TLWPols(Layr.Items[Idx]), Mesh);
      Idx := Layr.Items.FindChunk(@FindChunkById, @ID_POLS, Idx + 1);
    end;
    //    Normals.Normalize;
  end;
  FPnts := nil;
end;

procedure TGLLWOVectorFile.AddPnts(Pnts: TLWPnts; Mesh: TGLMeshObject);
var
  i: Integer;
begin

  FPnts := Pnts;
  with Mesh do
  begin
    Vertices.Capacity := Pnts.PntsCount;
    TexCoords.Capacity := Pnts.PntsCount;
    TexCoords.AddNulls(Pnts.PntsCount);

    for i := 0 to Pnts.PntsCount - 1 do
      Vertices.Add(PAffineVector(@Pnts.Pnts[i])^);
  end;
end;

procedure TGLLWOVectorFile.AddPols(Pols: TLWPols; Mesh: TGLMeshObject);
var
  Idx: Integer;
  i, j, k, PolyIdx, NormIdx: Integer;
  TagPolys: TU2DynArray;
  FaceGrp: TFGVertexNormalTexIndexList;
  VertPolys: TU2DynArray;
begin
  SetLength(VertPolys, 0);
  with Pols do
  begin
    // face type pols chunk
    if PolsType = POLS_TYPE_FACE then
    begin
      Idx := Items.FindChunk(@FindChunkById, @ID_PTAG);
      while Idx <> -1 do
      begin
        with TLWPTag(Items[Idx]) do
        begin
          if MapType = PTAG_TYPE_SURF then
          begin
            // for each tag
            for i := 0 to TagCount - 1 do
            begin
              // get polygons using this tag
              if GetPolsByTag(Tags[i], TagPolys) > 0 then
              begin
                // make the facegroup and set the material name
                FaceGrp := TFGVertexNormalTexIndexList.CreateOwned(Mesh.FaceGroups);
                FaceGrp.MaterialName := FLWO.SurfaceByTag[Tags[i]].Name;
                FaceGrp.Mode := fgmmTriangles;
                // for each polygon in the current surface Tags[i]
                for j := 0 to Length(TagPolys) - 1 do
                begin
                  PolyIdx := PolsByIndex[TagPolys[j]];
                  // triple 2,3 and 4 point ngons
                  case Indices[PolyIdx] of
                    2:
                      begin
                        // triangle line segment
                        NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[0])^);
                        FaceGrp.Add(Indices[PolyIdx + 1], NormIdx, Indices[PolyIdx + 1]);
                        NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[1])^);
                        FaceGrp.Add(Indices[PolyIdx + 2], NormIdx, Indices[PolyIdx + 1]);
                        NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[0])^);
                        FaceGrp.Add(Indices[PolyIdx + 1], NormIdx, Indices[PolyIdx + 1]);
                      end;
                    3: for k := 1 to 3 do
                      begin
                        NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[k - 1])^);
                        FaceGrp.Add(Indices[PolyIdx + k], NormIdx, Indices[PolyIdx + 1]);
                      end;
                    4:
                      begin
                        // triangle A
                        NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[0])^);
                        FaceGrp.Add(Indices[PolyIdx + 1], NormIdx, Indices[PolyIdx + 1]);
                        NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[1])^);
                        FaceGrp.Add(Indices[PolyIdx + 2], NormIdx, Indices[PolyIdx + 1]);
                        NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[2])^);
                        FaceGrp.Add(Indices[PolyIdx + 3], NormIdx, Indices[PolyIdx + 1]);
                        // triangle B
                        NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[0])^);
                        FaceGrp.Add(Indices[PolyIdx + 1], NormIdx, Indices[PolyIdx + 1]);
                        NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[2])^);
                        FaceGrp.Add(Indices[PolyIdx + 3], NormIdx, Indices[PolyIdx + 1]);
                        NormIdx := Mesh.Normals.Add(PVector3f(@PolsInfo[TagPolys[j]].vnorms[3])^);
                        FaceGrp.Add(Indices[PolyIdx + 4], NormIdx, Indices[PolyIdx + 1]);
                      end;
                  end;
                end;
                SetLength(TagPolys, 0);
              end;
            end;
          end
          else if MapType = PTAG_TYPE_PART then
          begin
            // Todo: PTag PART
          end
          else
            if MapType = PTAG_TYPE_SMGP then
            begin
              // Todo: PTag Smooth Group
            end;
          Idx := Items.FindChunk(@FindChunkById, @ID_PTAG, Idx + 1);
        end;
      end;
    end
    else
      // curv type pols chunk (catmull-rom splines)
      if PolsType = POLS_TYPE_CURV then
      begin
        // Todo: CURV Pols import
      end
      else
        // nurbs patch pols type chunk
        if PolsType = POLS_TYPE_PTCH then
        begin
          // Todo: NURBS Patch Pols import
        end
        else
          // // metaball pols type chunk
          if PolsType = POLS_TYPE_MBAL then
          begin
            // Todo: MetaBall type Pols import
          end
          else
            (* // bone pols type chunk *)
            if PolsType = POLS_TYPE_BONE then
            begin
              // Todo: Bone Pols import
            end;
    SetLength(TagPolys, 0);
  end;
end;

procedure TGLLWOVectorFile.AddSurf(Surf: TLWSurf; LWO: TLWObjectFile);
var
  matLib: TGLMaterialLibrary;
  libMat: TGLLibMaterial;
  tex2Mat: TGLLibMaterial;
  colr: TColr12;
  FloatParm, tran, refl: TF4;
  WordParm: TU2;
  StrParm: string;
  Idx: integer;
begin
  {DONE: implement surface inheritance}

  if GetOwner is TGLBaseMesh then
  begin
    matLib := TGLBaseMesh(GetOwner).MaterialLibrary;

    if Assigned(matLib) then
    begin

      libMat := matLib.Materials.GetLibMaterialByName(Surf.Name);

      if not Assigned(libMat) then
      begin

        libMat := matLib.Materials.Add;
        libMat.Name := Surf.Name;

        with libMat.Material.FrontProperties do
        begin

          tran := Surf.FloatParam[ID_TRAN];

          if tran <> 0 then
            libMat.Material.BlendingMode := bmTransparency;

          colr := Surf.Vec3Param[ID_COLR];

          //          Ambient.Color := VectorMake(colr[0],colr[1],colr[2],1);
          Ambient.Color := VectorMake(0, 0, 0, 1);

          (* Diffuse *)
          FloatParm := Surf.FloatParam[ID_DIFF];
          Diffuse.Color := VectorMake(colr[0] * FloatParm, colr[1] * FloatParm, colr[2] * FloatParm, tran);

          (* Luminosity -> Emission *)
          FloatParm := Surf.FloatParam[ID_LUMI];
          Emission.Color := VectorMake(colr[0] * FloatParm, colr[1] * FloatParm, colr[2] * FloatParm, 1);

          (* Specularity *)
          FloatParm := Surf.FloatParam[ID_SPEC];
          Specular.Color := VectorMake(colr[0] * FloatParm, colr[1] * FloatParm, colr[2] * FloatParm, 1);

          (* Glossiness -> Shininess *)
          FloatParm := Surf.FloatParam[ID_GLOS];
          Shininess := Round(Power(2, 7 * FloatParm));

          (* Polygon sidedness *)
          WordParm := Surf.WordParam[ID_SIDE];
          if (WordParm and SIDE_BACK) = SIDE_BACK then
            AssignTo(libMat.Material.BackProperties);

          (* Reflection settings *)
          refl := Surf.FloatParam[ID_REFL];
          if refl > 0 then
          begin
            // Check the reflection options
            WordParm := Surf.WordParam[ID_RFOP];
            if WordParm > RFOP_RAYTRACEANDBACKDROP then
            begin

              WordParm := Surf.VXParam[ID_RIMG];
              Idx := Surf.RootChunks.FindChunk(@FindClipByClipIndex, @WordParm);

              if Idx <> -1 then
              begin
                StrParm := string(PAnsiChar(TLWClip(Surf.RootChunks[Idx]).ParamAddr[ID_STIL]));
                StrParm := GetContentDir.FindContent(ToDosPath(StrParm));
                if FileExists(StrParm) then
                  try
                    if (not libMat.Material.Texture.Enabled) then
                      tex2Mat := libMat
                    else
                      tex2Mat := matLib.Materials.Add;

                    with tex2Mat do
                    begin

                      Material.Texture.Image.LoadFromFile(StrParm);
                      Material.Texture.Disabled := False;

                      with Material.Texture do
                      begin
                        MappingMode := tmmCubeMapReflection;
                        if refl < 100 then
                          TextureMode := tmBlend
                        else
                          TextureMode := tmDecal;
                      end;
                    end;
                    libMat.Texture2Name := 'REFL_' + ExtractFileName(StrParm);
                  except
                    on E: ETexture do
                    begin
                      if not Self.Owner.IgnoreMissingTextures then
                        raise;
                    end;
                  end;
              end;
            end;
          end;

        end;

      end;

    end;

  end;

end;

procedure TGLLWOVectorFile.AddVMap(VMap: TLWVMap; Mesh: TGLMeshObject);
var
  i: integer;
begin

  with VMap, Mesh do
  begin

    // texture coords
    if VMapType = VMAP_TYPE_TXUV then
    begin

      for i := 0 to ValueCount - 1 do
        TexCoords.Items[Value[i].vert] := AffineVectorMake(Value[i].values[0], Value[i].values[1], 0);

    end
    else

      {// vertex weight map} if VMapType = VMAP_TYPE_WGHT then
      begin
        {Todo: WeightMap import}

      end
      else

        {// vertex morph (relative)} if VMapType = VMAP_TYPE_MORF then
        begin
          {Todo: Morph target (relative) import}

        end
        else

          {// vertex morph (absolute)} if VMapType = VMAP_TYPE_SPOT then
          begin
            {Todo: Morph target (absolute) import}

          end;

  end;
end;

procedure TGLLWOVectorFile.LoadFromStream(aStream: TStream);
var
  Ind: Integer;
begin
  FLWO := TLWObjectFile.Create;

  with FLWO do
    try

      LoadFromStream(aStream);

      // Add Surfaces to material list
      Ind := Chunks.FindChunk(@FindChunkById, @ID_SURF, 0);

      while Ind <> -1 do
      begin

        AddSurf(TLWSurf(Chunks[Ind]), FLWO);

        Ind := Chunks.FindChunk(@FindChunkById, @ID_SURF, Ind + 1);

      end;

      // Lw layer
      Ind := Chunks.FindChunk(@FindChunkById, @ID_LAYR, 0);

      while Ind <> -1 do
      begin

        AddLayr(TLWLayr(Chunks[Ind]), FLWO);

        Ind := Chunks.FindChunk(@FindChunkById, @ID_LAYR, Ind + 1);

      end;

    finally

      FreeAndNil(FLWO);

    end;
end;

//------------------------------------------------------------
initialization
//------------------------------------------------------------

  RegisterVectorFileFormat('lwo', 'Lightwave3D object file (6.0 or above)', TGLLWOVectorFile);

finalization

end.

