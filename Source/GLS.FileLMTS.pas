//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.FileLMTS;

(* File loader for MTS *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  Vcl.Graphics,

  GLS.VectorTypes,
  GLS.VectorFileObjects,
  GLS.ApplicationFileIO,
  GLS.VectorLists,
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.PersistentClasses,
  GLS.Graphics,
  GLS.Material;

const
  C_LMTS_ID = $53544D4C;
  C_LMTS_VER = 4;
  C_LMTS_SUBS = $53425553;
  C_LMTS_TEXT = $54584554;
  C_LMTS_TRIS = $53495254;
  C_LMTS_TEXFNLEN = 255; // max texture filename length

type
  PLMTS_Header = ^TLMTS_Header;

  TLMTS_Header = record // packed
    ID: cardinal;
    Ver: cardinal;
    headerSize: cardinal;
    nTexts: word; // # of textures
    nSubsets: word;
    nTris: cardinal;
    subSize: word;
    vtxSize: word;
  end;

  PLMTS_TexData = ^TLMTS_TexData;

  TLMTS_TexData = record // packed
    fName: array [0 .. C_LMTS_TEXFNLEN] of AnsiChar;
    Flags: Word;
  end;

  PLMTS_Subset = ^TLMTS_Subset;

  TLMTS_Subset = record // packed
    Offset: LongInt;
    Count: LongInt;
    TextID1: Word;
    TextID2: Word;
  end;

  PLMTS_Vertex = ^TLMTS_Vertex;

  TLMTS_Vertex = record // packed
    x, y, z: Single;
    u1, v1, u2, v2: Single;
  end;

  PLMTS = ^TLMTS;

  TLMTS = record
    header: TLMTS_Header;
    usrData: pointer;
    usrSize: cardinal;
    texData: pointer;
    subsets: pointer;
    tris: pointer;
    ok: boolean;
  end;

  TMaterialInfo = record
    FShininess, BShininess: TGLShininess;
    FAmbient, FDiffuse, FEmission, FSpecular, BAmbient, BDiffuse, BEmission,
      BSpecular: TGLVector;
    ImageAlpha: TGLTextureImageAlpha;
    magFilter: TGLMagFilter;
    minFilter: TGLMinFilter;
    TextureMode: TGLTextureMode;
    TextureWrap: TGLTextureWrap;
    Blendingmode: TGLBlendingMode;
    FaceCulling: TGLFaceCulling;
    mathash: integer;
  end;

  TGLLMTSVectorFile = class(TGLVectorFile)
  public
    class function Capabilities: TGLDataFileCapabilities; override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
  end;

//---------------------------------------------------
implementation
//---------------------------------------------------

uses
  GLS.TextureFormat;

// ------------------
// ------------------ TGLLMTSVectorFile ------------------
// ------------------

class function TGLLMTSVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TGLLMTSVectorFile.LoadFromStream(aStream: TStream);
var
  MO: TMeshObject;
  FG: TFGVertexIndexList;
  LL: TGLMaterialLibrary;
  ML: TGLMaterialLibrary;
  LMTS: TLMTS;
  T: TLMTS_TexData;
  V: array [0 .. 2] of TLMTS_Vertex;
  S: TLMTS_Subset;
  _4cc: Cardinal;
  C: Integer;
  fName: string;
  vi: Tintegerlist;
  libmat: TGLLibmaterial;
  lmnames, matnames: TStringlist;
  MatInfoHeader: array [0 .. 3] of AnsiChar;
  MatInfoCount: Cardinal;
  Matinfo: array of TMaterialInfo;
  i, j: Integer;
begin
  owner.MeshObjects.Clear;

  MO := TMeshObject.CreateOwned(owner.MeshObjects);
  MO.Mode := momFaceGroups;

  vi := Tintegerlist.create;

  LL := owner.LightmapLibrary;
  ML := owner.MaterialLibrary;

  lmnames := TStringlist.create;
  matnames := TStringlist.create;
  MatInfoCount := 0;
  try
    // read header...
    aStream.Read(LMTS.header, SizeOf(TLMTS_Header));
    // verify...
    if (LMTS.header.ID <> C_LMTS_ID) or (LMTS.header.Ver <> C_LMTS_VER) or
      (LMTS.header.headerSize < SizeOf(TLMTS_Header)) or
      (LMTS.header.subSize < SizeOf(TLMTS_Subset)) or
      (LMTS.header.vtxSize < SizeOf(TLMTS_Vertex)) then
      raise Exception.create('Error in header');

    // read "user" data - actually skip this data...
    LMTS.usrSize := LMTS.header.headerSize - SizeOf(TLMTS_Header);
    if (LMTS.usrSize > 7) then
    begin
      aStream.Read(MatInfoHeader, 4);
      if MatInfoHeader = 'MATI' then
      begin
        aStream.Read(MatInfoCount, 4);
        if MatInfoCount > 0 then
        begin
          setlength(Matinfo, MatInfoCount);
          for i := 0 to MatInfoCount - 1 do
          begin
            aStream.Read(Matinfo[i], SizeOf(Matinfo[i]));
          end;
        end;
        if LMTS.usrSize > ((MatInfoCount * SizeOf(TMaterialInfo)) + 8) then
        begin
          LMTS.usrSize := LMTS.usrSize -
            ((MatInfoCount * SizeOf(TMaterialInfo)) + 8);
          aStream.Seek(LMTS.usrSize, soFromCurrent);
        end;
      end
      else
        aStream.Seek(LMTS.usrSize - 4, soFromCurrent);
    end
    else if (LMTS.usrSize > 0) then
      aStream.Seek(LMTS.usrSize, soFromCurrent);

    // read texture filenames data...
    aStream.Read(_4cc, SizeOf(_4cc));
    if (_4cc <> C_LMTS_TEXT) then
      raise Exception.create('Texture data not found');

    for C := 0 to LMTS.header.nTexts - 1 do
    begin
      aStream.Read(T, SizeOf(TLMTS_TexData));
      if T.Flags = 0 then
      begin
        if Assigned(ML) and (trim(String(T.fName)) <> '') then
        begin
          fName := String(T.fName);
          try
            if lastdelimiter('.', fName) <> length(fName) - 3 then
              fName := fName + '.tga'
            else
              fName := changefileext(fName, '.tga');
            if not fileexists(fName) then
            begin
              fName := changefileext(fName, '.jpg');
              if not fileexists(fName) then
              begin
                fName := changefileext(fName, '.png');
                if not fileexists(fName) then
                begin
                  fName := changefileext(fName, '.bmp');
                  if not fileexists(fName) then
                  begin
                    fName := changefileext(fName, '.dds');
                    if not fileexists(fName) then
                    begin
                      fName := String(T.fName);
                    end;
                    // fName:=fName+' (missing)';
                  end;

                end;
              end;
            end;
            libmat := ML.Materials.GetLibMaterialByName(fName);
            if not Assigned(libmat) then
            begin
              with ML.AddTextureMaterial(fName, fName) do
                Material.Texture.TextureMode := tmModulate;
            end;
            matnames.add(fName);
          except
            matnames.add(fName);
            { on E: ETexture do
              begin
              if not Owner.IgnoreMissingTextures then
              raise;
              end; }
          end;
        end
        else
          matnames.add(String(T.fName));
      end
      else
      begin
        if Assigned(LL) and (trim(String(T.fName)) <> '') then
        begin
          fName := String(T.fName);
          try
            if lastdelimiter('.', fName) <> length(fName) - 3 then
              fName := fName + '.tga'
            else
              fName := changefileext(fName, '.tga');
            if not fileexists(fName) then
            begin
              fName := changefileext(fName, '.jpg');
              if not fileexists(fName) then
              begin
                fName := changefileext(fName, '.png');
                if not fileexists(fName) then
                begin
                  fName := changefileext(fName, '.bmp');
                  if not fileexists(fName) then
                  begin
                    fName := changefileext(fName, '.dds');
                    if not fileexists(fName) then
                    begin
                      fName := String(T.fName);
                    end;
                    // fName:=fName+' (missing)';
                  end;
                end;

              end;
            end;
            libmat := LL.Materials.GetLibMaterialByName(fName);
            if not Assigned(libmat) then
            begin
              with LL.AddTextureMaterial(fName, fName).Material.Texture do
              begin
                minFilter := miLinear;
                TextureWrap := twNone;
                TextureFormat := tfRGB;
                TextureMode := tmModulate;
              end;
            end;
            lmnames.add(fName);
          except
            lmnames.add(fName);
            { on E: ETexture do
              begin
              if not Owner.IgnoreMissingTextures then
              raise;
              end; }
          end;
        end
        else
          lmnames.add(String(T.fName));
      end;
    end;

    if Assigned(owner.MaterialLibrary) then
      for i := 0 to MatInfoCount - 1 do
      begin
        libmat := nil;
        for j := 0 to owner.MaterialLibrary.Materials.Count - 1 do
          if owner.MaterialLibrary.Materials[j].NameHashKey = Matinfo[i].mathash
          then
          begin
            libmat := owner.MaterialLibrary.Materials[j];
            break;
          end;

        if Assigned(libmat) then
        begin
          with Matinfo[i] do
          begin
            libmat.Material.FrontProperties.Shininess := FShininess;
            libmat.Material.BackProperties.Shininess := BShininess;
            libmat.Material.FrontProperties.Ambient.Color := FAmbient;
            libmat.Material.FrontProperties.Diffuse.Color := FDiffuse;
            libmat.Material.FrontProperties.Emission.Color := FEmission;
            libmat.Material.FrontProperties.Specular.Color := FSpecular;

            libmat.Material.BackProperties.Ambient.Color := BAmbient;
            libmat.Material.BackProperties.Diffuse.Color := BDiffuse;
            libmat.Material.BackProperties.Emission.Color := BEmission;
            libmat.Material.BackProperties.Specular.Color := BSpecular;

            libmat.Material.Texture.ImageAlpha := ImageAlpha;
            libmat.Material.Texture.magFilter := magFilter;
            libmat.Material.Texture.minFilter := minFilter;
            libmat.Material.Texture.TextureMode := TextureMode;
            libmat.Material.Texture.TextureWrap := TextureWrap;
            libmat.Material.Blendingmode := Blendingmode;
            libmat.Material.FaceCulling := FaceCulling;
          end;
        end;
      end;

    // read subset data...
    aStream.Read(_4cc, SizeOf(_4cc));
    if (_4cc <> C_LMTS_SUBS) then
      raise Exception.create('Subset data not found');
    for C := LMTS.header.nSubsets - 1 downto 0 do
    begin
      aStream.Read(S, LMTS.header.subSize);
      FG := TFGVertexIndexList.CreateOwned(MO.FaceGroups);
      FG.Mode := fgmmTriangles;
      FG.vertexindices.AddSerie(S.Offset * 3, 1, S.Count * 3);
      vi.AddSerie(S.Offset * 3, 1, S.Count * 3);

      if Assigned(ML) and (S.TextID1 <> $FFFF) then
      begin
        if (S.TextID1 < matnames.Count) then
        begin
          libmat := ML.Materials.GetLibMaterialByName(matnames[S.TextID1]);
          if Assigned(libmat) then
            FG.MaterialName := libmat.Name;
        end;
      end;

      if Assigned(LL) and (S.TextID2 <> $FFFF) then
      begin
        if (S.TextID2 - matnames.Count < lmnames.Count) and
          (S.TextID2 - matnames.Count > -1) then
        begin
          libmat := LL.Materials.GetLibMaterialByName
            (lmnames[S.TextID2 - matnames.Count]);
          if Assigned(libmat) then
            FG.lightmapindex := libmat.Index;
        end;
      end;
    end;
    // read vertex data...
    aStream.Read(_4cc, SizeOf(_4cc));
    if (_4cc <> C_LMTS_TRIS) then
      raise Exception.create('Vertex data not found');
    for C := 0 to integer(LMTS.header.nTris) - 1 do
    begin
      aStream.Read(V[0], LMTS.header.vtxSize);
      aStream.Read(V[1], LMTS.header.vtxSize);
      aStream.Read(V[2], LMTS.header.vtxSize);

      MO.Vertices.add(-V[0].x, V[0].y, V[0].z);
      MO.TexCoords.add(V[0].u1, -V[0].v1);
      MO.LightmapTexCoords.add(V[0].u2, 1 - V[0].v2);

      MO.Vertices.add(-V[2].x, V[2].y, V[2].z);
      MO.TexCoords.add(V[2].u1, -V[2].v1);
      MO.LightmapTexCoords.add(V[2].u2, 1 - V[2].v2);

      MO.Vertices.add(-V[1].x, V[1].y, V[1].z);
      MO.TexCoords.add(V[1].u1, -V[1].v1);
      MO.LightmapTexCoords.add(V[1].u2, 1 - V[1].v2);
    end;
    MO.BuildNormals(vi, momtriangles);
    vi.free;
    matnames.free;
    lmnames.free;
    setlength(Matinfo, 0);
  except
    matnames.free;
    lmnames.free;
    MO.free;
  end;
end;

procedure TGLLMTSVectorFile.SaveToStream(aStream: TStream);
var
  MO: TMeshObject;
  FG: TFGVertexIndexList;
  i, j, k, l, lmstartindex, C, matindex: integer;
  h: TLMTS_Header;
  V: array [0 .. 2] of TLMTS_Vertex;
  texData: array of TLMTS_TexData;
  subsets: array of TLMTS_Subset;
  tris: array of TLMTS_Vertex;
  _4cc: cardinal;
  matname: AnsiString;
  ss: integer;
  Matinfo: array of TMaterialInfo;
  MatInfoCount: integer;
  libmat: TGLLibmaterial;
begin
  setlength(tris, 0);
  setlength(subsets, 0);
  setlength(texData, 0);
  C := 0;
  lmstartindex := maxint;
  for i := 0 to owner.MeshObjects.Count - 1 do
  begin
    MO := owner.MeshObjects[i];
    for j := 0 to MO.FaceGroups.Count - 1 do
    begin
      FG := TFGVertexIndexList(MO.FaceGroups[j]);

      matname := AnsiString(FG.MaterialName);

      // no duplicate textures please
      matindex := -1;
      for k := 0 to high(texData) do
        if texData[k].fName = matname then
        begin
          matindex := k;
          break;
        end;

      if matindex = -1 then // not a duplicate, so add it
      begin
        SetLength(texData, length(texData) + 1);
        with texData[high(texData)] do
        begin
          matindex := high(texData);

          StrPCopy(@fName, matname);
          Flags := 0;
        end;

        inc(C); // used to offest the lightmap index
      end;

      // set some of the facegroup (subsets) info here.
      setlength(subsets, length(subsets) + 1);
      with subsets[high(subsets)] do
      begin
        if (matname <> '') then
          TextID1 := matindex
        else
          TextID1 := $FFFF;
      end;

      if (FG.lightmapindex > -1) and (lmstartindex > FG.lightmapindex) then
        lmstartindex := FG.lightmapindex; // used to offest the lightmap index
    end;
  end;

  if lmstartindex = maxint then
    lmstartindex := 0; // cool, lightmaps start from the first index
  ss := 0;
  for i := 0 to owner.MeshObjects.Count - 1 do
  begin
    MO := owner.MeshObjects[i];
    for j := 0 to MO.FaceGroups.Count - 1 do
    begin
      FG := TFGVertexIndexList(MO.FaceGroups[j]);

      // subset already created earlier, just finish filling the data.
      // we needed the "c" and "lmstartindex" to be able to do this
      with subsets[ss] do
      begin
        Offset := length(tris) div 3;
        Count := FG.vertexindices.Count div 3;

        if (FG.lightmapindex > -1) and Assigned(owner.LightmapLibrary) then
          TextID2 := C + FG.lightmapindex - lmstartindex
        else
          TextID2 := $FFFF;
      end;

      // fill the vertex data
      k := 0;
      while k < FG.vertexindices.Count do
      begin
        for l := 0 to 2 do
        begin
          with V[l] do
          begin
            // vertex
            x := -MO.Vertices[FG.vertexindices[k + l]].X;
            y := MO.Vertices[FG.vertexindices[k + l]].Y;
            z := MO.Vertices[FG.vertexindices[k + l]].Z;

            // texcoords
            u1 := 0;
            v1 := 0;
            if FG is TFGVertexNormalTexIndexList then
            begin
              if MO.TexCoords.Count > TFGVertexNormalTexIndexList(FG)
                .texcoordIndices[k + l] then
              begin
                u1 := MO.TexCoords[TFGVertexNormalTexIndexList(FG)
                  .texcoordIndices[k + l]].X;
                v1 := -MO.TexCoords[TFGVertexNormalTexIndexList(FG)
                  .texcoordIndices[k + l]].Y;
              end;
            end
            else if FG is TFGIndexTexCoordList then
            begin
              u1 := TFGIndexTexCoordList(FG).TexCoords[k + l].X;
              v1 := -TFGIndexTexCoordList(FG).TexCoords[k + l].Y;
            end
            else if MO.TexCoords.Count > FG.vertexindices[k + l] then
            begin
              u1 := MO.TexCoords[FG.vertexindices[k + l]].X;
              v1 := -MO.TexCoords[FG.vertexindices[k + l]].Y;
            end;

            // lightmap texcoords
            u2 := 0;
            v2 := 0;
            if MO.LightmapTexCoords.Count > FG.vertexindices[k + l] then
            begin
              u2 := MO.LightmapTexCoords[FG.vertexindices[k + l]].X;
              v2 := 1 - MO.LightmapTexCoords[FG.vertexindices[k + l]].Y;
            end;
          end;
        end;
        setlength(tris, length(tris) + 3);

        tris[high(tris) - 2] := V[0];
        tris[high(tris) - 1] := V[2];
        tris[high(tris)] := V[1];

        inc(k, 3);
      end;
      inc(ss);
    end;
  end;

  setlength(Matinfo, 0);
  // store the material properties..
  if Assigned(owner.MaterialLibrary) then
  begin
    for i := 0 to high(texData) do
    begin
      libmat := owner.MaterialLibrary.Materials.GetLibMaterialByName
        (String(texData[i].fName));
      if Assigned(libmat) then
      begin
        setlength(Matinfo, length(Matinfo) + 1);
        with Matinfo[high(Matinfo)] do
        begin
          FShininess := libmat.Material.FrontProperties.Shininess;
          BShininess := libmat.Material.BackProperties.Shininess;
          FAmbient := libmat.Material.FrontProperties.Ambient.Color;
          FDiffuse := libmat.Material.FrontProperties.Diffuse.Color;
          FEmission := libmat.Material.FrontProperties.Emission.Color;
          FSpecular := libmat.Material.FrontProperties.Specular.Color;

          BAmbient := libmat.Material.BackProperties.Ambient.Color;
          BDiffuse := libmat.Material.BackProperties.Diffuse.Color;
          BEmission := libmat.Material.BackProperties.Emission.Color;
          BSpecular := libmat.Material.BackProperties.Specular.Color;

          ImageAlpha := libmat.Material.Texture.ImageAlpha;
          magFilter := libmat.Material.Texture.magFilter;
          minFilter := libmat.Material.Texture.minFilter;
          TextureMode := libmat.Material.Texture.TextureMode;
          TextureWrap := libmat.Material.Texture.TextureWrap;
          Blendingmode := libmat.Material.Blendingmode;
          FaceCulling := libmat.Material.FaceCulling;
          mathash := libmat.NameHashKey;
        end;
      end;
    end;
  end;

  // add the lightmap texture names to the texdata list
  C := length(texData);
  if Assigned(owner.LightmapLibrary) then
    for i := 0 to owner.MeshObjects.Count - 1 do
    begin
      MO := owner.MeshObjects[i];
      for j := 0 to MO.FaceGroups.Count - 1 do
      begin
        FG := TFGVertexIndexList(MO.FaceGroups[j]);
        if FG.lightmapindex > -1 then
        begin
          matname := AnsiString(owner.LightmapLibrary.Materials
            [FG.lightmapindex].Name);
          // no duplicate textures please
          matindex := -1;
          for k := C to high(texData) do
            if texData[k].fName = matname then
            begin
              matindex := k;
              break;
            end;
          if matindex = -1 then // not a duplicate, so add it
          begin
            SetLength(texData, length(texData) + 1);
            with texData[high(texData)] do
            begin
              StrPCopy(pansichar(@fName), matname);
              Flags := 1;
            end;
          end;
        end;
      end;
    end;

  // fill and write the file header
  with h do
  begin
    ID := C_LMTS_ID;
    Ver := C_LMTS_VER;
    headerSize := 24 + 8 + (length(Matinfo) * SizeOf(TMaterialInfo));
    nTexts := length(texData);
    nSubsets := length(subsets);
    nTris := length(tris) div 3;
    subSize := SizeOf(TLMTS_Subset);
    vtxSize := SizeOf(TLMTS_Vertex);
  end;
  aStream.Write(h, SizeOf(h));

  aStream.Write('MATI', 4);
  MatInfoCount := length(Matinfo);
  aStream.Write(MatInfoCount, SizeOf(MatInfoCount));
  // write the materials info
  for i := 0 to high(Matinfo) do
    aStream.Write(Matinfo[i], SizeOf(Matinfo[i]));

  // write the texture names
  _4cc := C_LMTS_TEXT;
  aStream.Write(_4cc, SizeOf(_4cc));
  for i := 0 to high(texData) do
    aStream.Write(texData[i], SizeOf(texData[i]));

  // fagegroups
  _4cc := C_LMTS_SUBS;
  aStream.Write(_4cc, SizeOf(_4cc));
  for i := 0 to high(subsets) do
    aStream.Write(subsets[i], SizeOf(subsets[i]));

  // vertex data
  _4cc := C_LMTS_TRIS;
  aStream.Write(_4cc, SizeOf(_4cc));
  for i := 0 to high(tris) do
    aStream.Write(tris[i], SizeOf(tris[i]));

  // free up used memory
  setlength(tris, 0);
  setlength(subsets, 0);
  setlength(texData, 0);
  setlength(Matinfo, 0);
end;

//-----------------------------------------------
initialization
//-----------------------------------------------

RegisterVectorFileFormat('lmts', 'Pulsar Studio LMTS File Format',
  TGLLMTSVectorFile);

end.
