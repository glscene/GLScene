//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.IntellectObjects;

(* Intellectual objects *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.Math,
  VCL.Consts,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.VectorTypesExt,
  GLS.VectorLists,
  GLS.PersistentClasses,
  GLS.Silhouette,
  GLS.Strings,
  GLS.Texture,
  GLS.Material,
  GLS.Mesh,
  GLS.Logger,
  GLS.Octree,
  GLS.GeometryBB,
  GLS.ApplicationFileIO,
  GLS.Context,
  GLS.Color,
  GLS.PipelineTransformation,
  GLS.Selection,
  GLS.RenderContextInfo,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.TextureFormat,
  GLS.VectorFileObjects;

type

  TGLMeshAutoCentering = (macCenterX, macCenterY, macCenterZ, macUseBarycenter, macRestorePosition);
  TGLMeshAutoCenterings = set of TGLMeshAutoCentering;
  TGLMeshObjectMode = (momTriangles, momTriangleStrip, momFaceGroups);





var
  vGLIntellectObjectsAllocateSences: Boolean = True;
  // Flag to avoid loading scences (useful for IDE Extentions or scene editors)
  vGLIntellectObjectsEnableScenceByDefault: Boolean = True;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  vVectorFileFormats: TGLVectorFileFormatsList;
  vNextRenderGroupID: Integer = 1;

const
  cAAFHeader: AnsiString = 'AAF';

function GetVectorFileFormats: TGLVectorFileFormatsList;
begin
  if not Assigned(vVectorFileFormats) then
    vVectorFileFormats := TGLVectorFileFormatsList.Create;
  Result := vVectorFileFormats;
end;

function VectorFileFormatsFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TGLVectorFile, Result, f);
end;

function VectorFileFormatsSaveFilter: string;
var
  f: string;
begin
  GetVectorFileFormats.BuildFilterStrings(TGLVectorFile, Result, f, False, True);
end;

procedure RegisterVectorFileFormat(const aExtension, aDescription: string; AClass: TGLVectorFileClass);
begin
  RegisterClass(AClass);
  GetVectorFileFormats.Add(aExtension, aDescription, 0, AClass);
end;

procedure UnregisterVectorFileClass(AClass: TGLVectorFileClass);
begin
  if Assigned(vVectorFileFormats) then
    vVectorFileFormats.Remove(AClass);
end;

function VectorFileFormatExtensionByIndex(Index: Integer): string;
begin
  Result := GetVectorFileFormats.FindExtByIndex(index);
end;

// ------------------
// ------------------ TGLVectorFileFormatsList ------------------
// ------------------

destructor TGLVectorFileFormatsList.Destroy;
begin
  Clean;
  inherited;
end;

procedure TGLVectorFileFormatsList.Add(const Ext, Desc: string; DescID: Integer; AClass: TGLVectorFileClass);
var
  newRec: TGLVectorFileFormat;
begin
  newRec := TGLVectorFileFormat.Create;
  with newRec do
  begin
    Extension := AnsiLowerCase(Ext);
    VectorFileClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(newRec);
end;

function TGLVectorFileFormatsList.FindExt(Ext: string): TGLVectorFileClass;
var
  i: Integer;
begin
  Ext := AnsiLowerCase(Ext);
  for i := Count - 1 downto 0 do
    with TGLVectorFileFormat(Items[i]) do
    begin
      if Extension = Ext then
      begin
        Result := VectorFileClass;
        Exit;
      end;
    end;
  Result := nil;
end;

function TGLVectorFileFormatsList.FindFromFileName(const filename: string): TGLVectorFileClass;
var
  Ext: string;
begin
  Ext := ExtractFileExt(filename);
  System.Delete(Ext, 1, 1);
  Result := FindExt(Ext);
  if not Assigned(Result) then
    raise EInvalidVectorFile.CreateFmt(strUnknownExtension, [Ext, 'GLFile' + UpperCase(Ext)]);
end;

procedure TGLVectorFileFormatsList.Remove(AClass: TGLVectorFileClass);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if TGLVectorFileFormat(Items[i]).VectorFileClass.InheritsFrom(AClass) then
      DeleteAndFree(i);
  end;
end;

procedure TGLVectorFileFormatsList.BuildFilterStrings(
   VectorFileClass: TGLVectorFileClass; out descriptions, filters: string;
  formatsThatCanBeOpened: Boolean = True; formatsThatCanBeSaved: Boolean = False);
var
  k, i: Integer;
  p: TGLVectorFileFormat;
begin
  descriptions := '';
  filters := '';
  k := 0;
  for i := 0 to Count - 1 do
  begin
    p := TGLVectorFileFormat(Items[i]);
    if p.VectorFileClass.InheritsFrom(vectorFileClass) and (p.Extension <> '')
      and ((formatsThatCanBeOpened and (dfcRead in
        p.VectorFileClass.Capabilities))
      or (formatsThatCanBeSaved and (dfcWrite in
        p.VectorFileClass.Capabilities))) then
    begin
      with p do
      begin
        if k <> 0 then
        begin
          descriptions := descriptions + '|';
          filters := filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(descriptions, '%s%s (*.%s)|*.%2:s', [descriptions, Description, Extension]);
        filters := filters + '*.' + Extension;
        Inc(k);
      end;
    end;
  end;
  if (k > 1) and (not formatsThatCanBeSaved) then
    FmtStr(descriptions, '%s (%s)|%1:s|%s', [sAllFilter, filters, descriptions]);
end;

function TGLVectorFileFormatsList.FindExtByIndex(Index: Integer;
  formatsThatCanBeOpened: Boolean = True;
  formatsThatCanBeSaved: Boolean = False): string;
var
  i: Integer;
  p: TGLVectorFileFormat;
begin
  Result := '';
  if index > 0 then
  begin
    for i := 0 to Count - 1 do
    begin
      p := TGLVectorFileFormat(Items[i]);
      if (formatsThatCanBeOpened and (dfcRead in p.VectorFileClass.Capabilities))
        or (formatsThatCanBeSaved and (dfcWrite in
          p.VectorFileClass.Capabilities)) then
      begin
        if index = 1 then
        begin
          Result := p.Extension;
          Break;
        end
        else
          Dec(index);
      end;
    end;
  end;
end;

// ------------------
// ------------------ TGLBaseMeshObject ------------------
// ------------------

constructor TGLBaseMeshObject.Create;
begin
  FVertices := TGLAffineVectorList.Create;
  FNormals := TGLAffineVectorList.Create;
  FVisible := True;
  inherited Create;
end;

destructor TGLBaseMeshObject.Destroy;
begin
  FNormals.Free;
  FVertices.Free;
  inherited;
end;

procedure TGLBaseMeshObject.Assign(Source: TPersistent);
begin
  if Source is TGLBaseMeshObject then
  begin
    FName := TGLBaseMeshObject(Source).Name;
    FVertices.Assign(TGLBaseMeshObject(Source).FVertices);
    FNormals.Assign(TGLBaseMeshObject(Source).FNormals);
  end
  else
    inherited; // Die!
end;

procedure TGLBaseMeshObject.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(1); // Archive Version 1, added FVisible
    WriteString(FName);
    FVertices.WriteToFiler(writer);
    FNormals.WriteToFiler(writer);
    WriteBoolean(FVisible);
  end;
end;

procedure TGLBaseMeshObject.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0 .. 1] then
    with reader do
    begin
      FName := ReadString;
      FVertices.ReadFromFiler(reader);
      FNormals.ReadFromFiler(reader);
      if archiveVersion >= 1 then
        FVisible := ReadBoolean
      else
        FVisible := True;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLBaseMeshObject.Clear;
begin
  FNormals.Clear;
  FVertices.Clear;
end;

procedure TGLBaseMeshObject.ContributeToBarycenter(var currentSum: TAffineVector; var nb: Integer);
begin
  AddVector(currentSum, FVertices.Sum);
  nb := nb + FVertices.Count;
end;

procedure TGLBaseMeshObject.Translate(const delta: TAffineVector);
begin
  FVertices.Translate(delta);
end;

procedure TGLBaseMeshObject.BuildNormals(vertexIndices: TGLIntegerList; Mode: TGLMeshObjectMode;
  normalIndices: TGLIntegerList = nil);
var
  i, base: Integer;
  n: TAffineVector;
  newNormals: TGLIntegerList;

  function TranslateNewNormal(vertexIndex: Integer; const delta: TAffineVector): Integer;
  var
    pv: PAffineVector;
  begin
    Result := newNormals[vertexIndex];
    if Result < base then
    begin
      result := Normals.Add(NullVector);
      newNormals[vertexIndex] := result;
    end;
    pv := @Normals.List[Result];
    AddVector(pv^, delta);
  end;

begin
  if not Assigned(normalIndices) then
  begin
    // build bijection
    Normals.Clear;
    Normals.Count := Vertices.Count;
    case Mode of
      momTriangles:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
            with Normals do
            begin
              with Vertices do
              begin
                CalcPlaneNormal(Items[vertexIndices[i + 0]],
                  Items[vertexIndices[i + 1]],
                  Items[vertexIndices[i + 2]], n);
              end;
              with Normals do
              begin
                TranslateItem(vertexIndices[i + 0], n);
                TranslateItem(vertexIndices[i + 1], n);
                TranslateItem(vertexIndices[i + 2], n);
              end;
              Inc(i, 3);
            end;
        end;
      momTriangleStrip:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
            with Normals do
            begin
              with Vertices do
              begin
                if (i and 1) = 0 then
                  CalcPlaneNormal(Items[vertexIndices[i + 0]],
                    Items[vertexIndices[i + 1]],
                    Items[vertexIndices[i + 2]], n)
                else
                  CalcPlaneNormal(Items[vertexIndices[i + 0]],
                    Items[vertexIndices[i + 2]],
                    Items[vertexIndices[i + 1]], n);
              end;
              with Normals do
              begin
                TranslateItem(vertexIndices[i + 0], n);
                TranslateItem(vertexIndices[i + 1], n);
                TranslateItem(vertexIndices[i + 2], n);
              end;
              Inc(i, 1);
            end;
        end;
    else
      Assert(False);
    end;
    Normals.Normalize;
  end
  else
  begin
    // add new normals
    base := Normals.Count;
    newNormals := TGLIntegerList.Create;
    newNormals.AddSerie(-1, 0, Vertices.Count);
    case Mode of
      momTriangles:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
          begin
            with Vertices do
            begin
              CalcPlaneNormal(Items[vertexIndices[i + 0]], Items[vertexIndices[i + 1]],
			    Items[vertexIndices[i + 2]], n);
            end;
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 0], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 1], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 2], n));
            Inc(i, 3);
          end;
        end;
      momTriangleStrip:
        begin
          i := 0;
          while i <= vertexIndices.Count - 3 do
          begin
            with Vertices do
            begin
              if (i and 1) = 0 then
                CalcPlaneNormal(Items[vertexIndices[i + 0]],
                  Items[vertexIndices[i + 1]],
                  Items[vertexIndices[i + 2]], n)
              else
                CalcPlaneNormal(Items[vertexIndices[i + 0]],
                  Items[vertexIndices[i + 2]],
                  Items[vertexIndices[i + 1]], n);
            end;
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 0], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 1], n));
            normalIndices.Add(TranslateNewNormal(vertexIndices[i + 2], n));
            Inc(i, 1);
          end;
        end;
    else
      Assert(False);
    end;
    for i := base to Normals.Count - 1 do
      NormalizeVector(Normals.List^[i]);
    newNormals.Free;
  end;
end;

procedure TGLBaseMeshObject.GenericOrderedBuildNormals(mode: TGLMeshObjectMode);
var
  i: Integer;
  n: TAffineVector;

begin
  Normals.Clear;
  Normals.Count := Vertices.Count;
  case mode of
    momTriangles:
      begin
        i := 0;
        while i <= Vertices.Count - 3 do
          with Normals do
          begin
            with Vertices do
            begin
              CalcPlaneNormal(Items[i], Items[i + 1], Items[i + 2], n);
            end;
            with Normals do
            begin
              TranslateItem(i, n);
              TranslateItem(i + 1, n);
              TranslateItem(i + 2, n);
            end;
            Inc(i, 3);
          end;
      end;
    momTriangleStrip:
      begin
        i := 0;
        while i <= Vertices.Count - 3 do
          with Normals do
          begin
            with Vertices do
            begin
              if (i and 1) = 0 then
                CalcPlaneNormal(Items[i], Items[i + 1], Items[i + 2], n)
              else
                CalcPlaneNormal(Items[i], Items[i + 2], Items[i + 1], n);
            end;
            with Normals do
            begin
              TranslateItem(i, n);
              TranslateItem(i + 1, n);
              TranslateItem(i + 2, n);
            end;
            Inc(i, 1);
          end;
      end
  else
    Assert(False);
  end;
  Normals.normalize;
end;

function TGLBaseMeshObject.ExtractTriangles(texCoords: TGLAffineVectorList = nil;
  normals: TGLAffineVectorList = nil): TGLAffineVectorList;
begin
  Result := TGLAffineVectorList.Create;
  if (Vertices.Count mod 3) = 0 then
  begin
    Result.Assign(Vertices);
    if Assigned(normals) then
      normals.Assign(Self.Normals);
  end;
end;

procedure TGLBaseMeshObject.SetVertices(const val: TGLAffineVectorList);
begin
  FVertices.Assign(val);
end;

procedure TGLBaseMeshObject.SetNormals(const val: TGLAffineVectorList);
begin
  FNormals.Assign(val);
end;

// ------------------
// ------------------ TGLSkeletonFrame ------------------
// ------------------

constructor TGLSkeletonFrame.CreateOwned(aOwner: TGLSkeletonFrameList);
begin
  FOwner := aOwner;
  aOwner.Add(Self);
  Create;
end;

constructor TGLSkeletonFrame.Create;
begin
  inherited Create;
  FPosition := TGLAffineVectorList.Create;
  FRotation := TGLAffineVectorList.Create;
  FQuaternion := TGLQuaternionList.Create;
  FTransformMode := sftRotation;
end;

destructor TGLSkeletonFrame.Destroy;
begin
  FlushLocalMatrixList;
  FRotation.Free;
  FPosition.Free;
  FQuaternion.Free;
  inherited Destroy;
end;

procedure TGLSkeletonFrame.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(1); // Archive Version 1
    WriteString(FName);
    FPosition.WriteToFiler(writer);
    FRotation.WriteToFiler(writer);
    FQuaternion.WriteToFiler(writer);
    WriteInteger(Integer(FTransformMode));
  end;
end;

procedure TGLSkeletonFrame.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if (archiveVersion = 0) or (archiveVersion = 1) then
    with reader do
    begin
      FName := ReadString;
      FPosition.ReadFromFiler(reader);
      FRotation.ReadFromFiler(reader);
      if (archiveVersion = 1) then
      begin
        FQuaternion.ReadFromFiler(reader);
        FTransformMode := TGLSkeletonFrameTransform(ReadInteger);
      end;
    end
  else
    RaiseFilerException(archiveVersion);
  FlushLocalMatrixList;
end;

procedure TGLSkeletonFrame.SetPosition(const val: TGLAffineVectorList);
begin
  FPosition.Assign(val);
end;

procedure TGLSkeletonFrame.SetRotation(const val: TGLAffineVectorList);
begin
  FRotation.Assign(val);
end;

procedure TGLSkeletonFrame.SetQuaternion(const val: TGLQuaternionList);
begin
  FQuaternion.Assign(val);
end;

function TGLSkeletonFrame.LocalMatrixList: PMatrixArray;
var
  i: Integer;
  s, c: Single;
  mat, rmat: TGLMatrix;
  quat: TQuaternion;
begin
  if not Assigned(FLocalMatrixList) then
  begin
    case FTransformMode of
      sftRotation:
        begin
          FLocalMatrixList := AllocMem(SizeOf(TGLMatrix) * Rotation.Count);
          for i := 0 to Rotation.Count - 1 do
          begin
            if Rotation[i].X <> 0 then
            begin
              SinCosine(Rotation[i].X, s, c);
              mat := CreateRotationMatrixX(s, c);
            end
            else
              mat := IdentityHmgMatrix;
            if Rotation[i].Y <> 0 then
            begin
              SinCosine(Rotation[i].Y, s, c);
              rmat := CreateRotationMatrixY(s, c);
              mat := MatrixMultiply(mat, rmat);
            end;
            if Rotation[i].Z <> 0 then
            begin
              SinCosine(Rotation[i].Z, s, c);
              rmat := CreateRotationMatrixZ(s, c);
              mat := MatrixMultiply(mat, rmat);
            end;
            mat.W.X := Position[i].X;
            mat.W.Y := Position[i].Y;
            mat.W.Z := Position[i].Z;
            FLocalMatrixList^[i] := mat;
          end;
        end;
      sftQuaternion:
        begin
          FLocalMatrixList := AllocMem(SizeOf(TGLMatrix) * Quaternion.Count);
          for i := 0 to Quaternion.Count - 1 do
          begin
            quat := Quaternion[i];
            mat := QuaternionToMatrix(quat);
            mat.W.X := Position[i].X;
            mat.W.Y := Position[i].Y;
            mat.W.Z := Position[i].Z;
            mat.W.W := 1;
            FLocalMatrixList^[i] := mat;
          end;
        end;
    end;
  end;
  Result := FLocalMatrixList;
end;

procedure TGLSkeletonFrame.FlushLocalMatrixList;
begin
  if Assigned(FLocalMatrixList) then
  begin
    FreeMem(FLocalMatrixList);
    FLocalMatrixList := nil;
  end;
end;

procedure TGLSkeletonFrame.ConvertQuaternionsToRotations(KeepQuaternions: Boolean = True);
var
  i: Integer;
  t: TTransformations;
  m: TGLMatrix;
begin
  Rotation.Clear;
  for i := 0 to Quaternion.Count - 1 do
  begin
    m := QuaternionToMatrix(Quaternion[i]);
    if MatrixDecompose(m, t) then
      Rotation.Add(t[ttRotateX], t[ttRotateY], t[ttRotateZ])
    else
      Rotation.Add(NullVector);
  end;
  if not KeepQuaternions then
    Quaternion.Clear;
end;

procedure TGLSkeletonFrame.ConvertRotationsToQuaternions(KeepRotations: Boolean = True);
var
  i: Integer;
  mat, rmat: TGLMatrix;
  s, c: Single;
begin
  Quaternion.Clear;
  for i := 0 to Rotation.Count - 1 do
  begin
    mat := IdentityHmgMatrix;
    SinCosine(Rotation[i].X, s, c);
    rmat := CreateRotationMatrixX(s, c);
    mat := MatrixMultiply(mat, rmat);
    SinCosine(Rotation[i].Y, s, c);
    rmat := CreateRotationMatrixY(s, c);
    mat := MatrixMultiply(mat, rmat);
    SinCosine(Rotation[i].Z, s, c);
    rmat := CreateRotationMatrixZ(s, c);
    mat := MatrixMultiply(mat, rmat);
    Quaternion.Add(QuaternionFromMatrix(mat));
  end;
  if not KeepRotations then
    Rotation.Clear;
end;

// ------------------
// ------------------ TGLSkeletonFrameList ------------------
// ------------------

constructor TGLSkeletonFrameList.CreateOwned(aOwner: TPersistent);
begin
  FOwner := AOwner;
  Create;
end;

destructor TGLSkeletonFrameList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TGLSkeletonFrameList.ReadFromFiler(reader: TGLVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TGLSkeletonFrameList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

function TGLSkeletonFrameList.GetSkeletonFrame(Index: Integer): TGLSkeletonFrame;
begin
  Result := TGLSkeletonFrame(List^[Index]);
end;

procedure TGLSkeletonFrameList.ConvertQuaternionsToRotations(KeepQuaternions: Boolean = True; SetTransformMode: Boolean = True);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].ConvertQuaternionsToRotations(KeepQuaternions);
    if SetTransformMode then
      Items[i].TransformMode := sftRotation;
  end;
end;

procedure TGLSkeletonFrameList.ConvertRotationsToQuaternions(KeepRotations: Boolean = True; SetTransformMode: Boolean = True);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].ConvertRotationsToQuaternions(KeepRotations);
    if SetTransformMode then
      Items[i].TransformMode := sftQuaternion;
  end;
end;

// ------------------
// ------------------ TGLSkeletonBoneList ------------------
// ------------------

constructor TGLSkeletonBoneList.CreateOwned(aOwner: TGLSkeleton);
begin
  FSkeleton := aOwner;
  Create;
end;

constructor TGLSkeletonBoneList.Create;
begin
  inherited;
  FGlobalMatrix := IdentityHmgMatrix;
end;

destructor TGLSkeletonBoneList.Destroy;
begin
  Clean;
  inherited;
end;

procedure TGLSkeletonBoneList.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing, yet
  end;
end;

procedure TGLSkeletonBoneList.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion, i: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing, yet
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TGLSkeletonBoneList.AfterObjectCreatedByReader(Sender: TObject);
begin
  with (Sender as TGLSkeletonBone) do
  begin
    FOwner := Self;
    FSkeleton := Self.Skeleton;
  end;
end;

function TGLSkeletonBoneList.GetSkeletonBone(Index: Integer): TGLSkeletonBone;
begin
  Result := TGLSkeletonBone(List^[Index]);
end;

function TGLSkeletonBoneList.BoneByID(anID: Integer): TGLSkeletonBone;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].BoneByID(anID);
    if Assigned(Result) then
      Break;
  end;
end;

function TGLSkeletonBoneList.BoneByName(const aName: string): TGLSkeletonBone;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    Result := Items[i].BoneByName(aName);
    if Assigned(Result) then
      Break;
  end;
end;

function TGLSkeletonBoneList.BoneCount: Integer;
var
  i: Integer;
begin
  Result := 1;
  for i := 0 to Count - 1 do
    Inc(Result, Items[i].BoneCount);
end;

procedure TGLSkeletonBoneList.PrepareGlobalMatrices;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].PrepareGlobalMatrices;
end;

// ------------------
// ------------------ TGLSkeletonRootBoneList ------------------
// ------------------

procedure TGLSkeletonRootBoneList.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing, yet
  end;
end;

procedure TGLSkeletonRootBoneList.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion, i: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing, yet
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TGLSkeletonRootBoneList.BuildList(var mrci: TGLRenderContextInfo);
var
  i: Integer;
begin
  // root node setups and restore OpenGL stuff
  mrci.GLStates.Disable(stColorMaterial);
  mrci.GLStates.Disable(stLighting);
  gl.Color3f(1, 1, 1);
  // render root-bones
  for i := 0 to Count - 1 do
    Items[i].BuildList(mrci);
end;

// ------------------
// ------------------ TGLSkeletonBone ------------------
// ------------------

constructor TGLSkeletonBone.CreateOwned(aOwner: TGLSkeletonBoneList);
begin
  FOwner := aOwner;
  aOwner.Add(Self);
  FSkeleton := aOwner.Skeleton;
  Create;
end;

constructor TGLSkeletonBone.Create;
begin
  FColor := $FFFFFFFF; // opaque white
  inherited;
end;

destructor TGLSkeletonBone.Destroy;
begin
  if Assigned(Owner) then
    Owner.Remove(Self);
  inherited Destroy;
end;

procedure TGLSkeletonBone.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteString(FName);
    WriteInteger(FBoneID);
    WriteInteger(Integer(FColor));
  end;
end;

procedure TGLSkeletonBone.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion, i: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FName := ReadString;
      FBoneID := ReadInteger;
      FColor := Cardinal(ReadInteger);
    end
  else
    RaiseFilerException(archiveVersion);
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TGLSkeletonBone.BuildList(var mrci: TGLRenderContextInfo);

  procedure IssueColor(Color: Cardinal);
  begin
    gl.Color4f(GetRValue(Color) / 255, GetGValue(Color) / 255, GetBValue(Color) / 255, ((Color shr 24) and 255) / 255);
  end;

var
  i: Integer;
begin
  // point for self
  mrci.GLStates.PointSize := 5;
  gl.Begin_(GL_POINTS);
  IssueColor(Color);
  gl.Vertex3fv(@GlobalMatrix.W.X);
  gl.End_;
  // parent-self bone line
  if Owner is TGLSkeletonBone then
  begin
    gl.Begin_(GL_LINES);
    gl.Vertex3fv(@TGLSkeletonBone(Owner).GlobalMatrix.W.X);
    gl.Vertex3fv(@GlobalMatrix.W.X);
    gl.End_;
  end;
  // render sub-bones
  for i := 0 to Count - 1 do
    Items[i].BuildList(mrci);
end;

function TGLSkeletonBone.GetSkeletonBone(Index: Integer): TGLSkeletonBone;
begin
  Result := TGLSkeletonBone(List^[Index]);
end;

procedure TGLSkeletonBone.SetColor(const val: Cardinal);
begin
  FColor := val;
end;

function TGLSkeletonBone.BoneByID(anID: Integer): TGLSkeletonBone;
begin
  if BoneID = anID then
    Result := Self
  else
    Result := inherited BoneByID(anID);
end;

function TGLSkeletonBone.BoneByName(const aName: string): TGLSkeletonBone;
begin
  if Name = aName then
    Result := Self
  else
    Result := inherited BoneByName(aName);
end;

procedure TGLSkeletonBone.Clean;
begin
  BoneID := 0;
  Name := '';
  inherited;
end;

procedure TGLSkeletonBone.PrepareGlobalMatrices;
begin
  if (Skeleton.FRagDollEnabled) then
    Exit; // ragdoll
  FGlobalMatrix :=
    MatrixMultiply(Skeleton.CurrentFrame.LocalMatrixList^[BoneID],
    TGLSkeletonBoneList(Owner).FGlobalMatrix);
  inherited;
end;

procedure TGLSkeletonBone.SetGlobalMatrix(const Matrix: TGLMatrix); // ragdoll
begin
  FGlobalMatrix := Matrix;
end;

procedure TGLSkeletonBone.SetGlobalMatrixForRagDoll(const RagDollMatrix: TGLMatrix);
// ragdoll
begin
  FGlobalMatrix := MatrixMultiply(RagDollMatrix,
    Skeleton.Owner.InvAbsoluteMatrix);
  inherited;
end;

// ------------------
// ------------------ TGLSkeletonCollider ------------------
// ------------------

constructor TGLSkeletonCollider.Create;
begin
  inherited;
  FLocalMatrix := IdentityHmgMatrix;
  FGlobalMatrix := IdentityHmgMatrix;
  FAutoUpdate := True;
end;

constructor TGLSkeletonCollider.CreateOwned(AOwner: TGLSkeletonColliderList);
begin
  Create;
  FOwner := AOwner;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

procedure TGLSkeletonCollider.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    if Assigned(FBone) then
      WriteInteger(FBone.BoneID)
    else
      WriteInteger(-1);
    Write(FLocalMatrix, SizeOf(TGLMatrix));
  end;
end;

procedure TGLSkeletonCollider.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FBoneID := ReadInteger;
      Read(FLocalMatrix, SizeOf(TGLMatrix));
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLSkeletonCollider.AlignCollider;
var
  mat: TGLMatrix;
begin
  if Assigned(FBone) then
  begin
    if Owner.Owner is TGLSkeleton then
      if TGLSkeleton(Owner.Owner).Owner is TGLBaseSceneObject then
        mat := MatrixMultiply(FBone.GlobalMatrix,
          TGLBaseSceneObject(TGLSkeleton(Owner.Owner).Owner).AbsoluteMatrix)
      else
        mat := FBone.GlobalMatrix;
    MatrixMultiply(FLocalMatrix, mat, FGlobalMatrix);
  end
  else
    FGlobalMatrix := FLocalMatrix;
end;

procedure TGLSkeletonCollider.SetBone(const val: TGLSkeletonBone);
begin
  if val <> FBone then
    FBone := val;
end;

procedure TGLSkeletonCollider.SetLocalMatrix(const val: TGLMatrix);
begin
  FLocalMatrix := val;
end;

// ------------------
// ------------------ TGLSkeletonColliderList ------------------
// ------------------

constructor TGLSkeletonColliderList.CreateOwned(aOwner: TPersistent);
begin
  Create;
  FOwner := aOwner;
end;

destructor TGLSkeletonColliderList.Destroy;
begin
  Clear;
  inherited;
end;

function TGLSkeletonColliderList.GetSkeletonCollider(Index: Integer): TGLSkeletonCollider;
begin
  Result := TGLSkeletonCollider(inherited Get(index));
end;

procedure TGLSkeletonColliderList.ReadFromFiler(reader: TGLVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
  begin
    Items[i].FOwner := Self;
    if (Owner is TGLSkeleton) and (Items[i].FBoneID <> -1) then
      Items[i].Bone := TGLSkeleton(Owner).BoneByID(Items[i].FBoneID);
  end;
end;

procedure TGLSkeletonColliderList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Items[i].FOwner := nil;
    Items[i].Free;
  end;
  inherited;
end;

procedure TGLSkeletonColliderList.AlignColliders;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].AutoUpdate then
      Items[i].AlignCollider;
end;

// ------------------
// ------------------ TGLSkeleton ------------------
// ------------------

constructor TGLSkeleton.CreateOwned(AOwner: TGLBaseMesh);
begin
  FOwner := aOwner;
  Create;
end;

constructor TGLSkeleton.Create;
begin
  inherited Create;
  FRootBones := TGLSkeletonRootBoneList.CreateOwned(Self);
  FFrames := TGLSkeletonFrameList.CreateOwned(Self);
  FColliders := TGLSkeletonColliderList.CreateOwned(Self);
end;

destructor TGLSkeleton.Destroy;
begin
  FlushBoneByIDCache;
  FCurrentFrame.Free;
  FFrames.Free;
  FRootBones.Free;
  FColliders.Free;
  inherited Destroy;
end;

procedure TGLSkeleton.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    if FColliders.Count > 0 then
      WriteInteger(1) // Archive Version 1 : with colliders
    else
      WriteInteger(0); // Archive Version 0
    FRootBones.WriteToFiler(writer);
    FFrames.WriteToFiler(writer);
    if FColliders.Count > 0 then
      FColliders.WriteToFiler(writer);
  end;
end;

procedure TGLSkeleton.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if (archiveVersion = 0) or (archiveVersion = 1) then
    with reader do
    begin
      FRootBones.ReadFromFiler(reader);
      FFrames.ReadFromFiler(reader);
      if (archiveVersion = 1) then
        FColliders.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLSkeleton.SetRootBones(const val: TGLSkeletonRootBoneList);
begin
  FRootBones.Assign(val);
end;

procedure TGLSkeleton.SetFrames(const val: TGLSkeletonFrameList);
begin
  FFrames.Assign(val);
end;

function TGLSkeleton.GetCurrentFrame: TGLSkeletonFrame;
begin
  if not Assigned(FCurrentFrame) then
    FCurrentFrame := TGLSkeletonFrame(FFrames.Items[0].CreateClone);
  Result := FCurrentFrame;
end;

procedure TGLSkeleton.SetCurrentFrame(val: TGLSkeletonFrame);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
  FCurrentFrame := TGLSkeletonFrame(val.CreateClone);
end;

procedure TGLSkeleton.SetColliders(const val: TGLSkeletonColliderList);
begin
  FColliders.Assign(val);
end;

procedure TGLSkeleton.FlushBoneByIDCache;
begin
  FBonesByIDCache.Free;
  FBonesByIDCache := nil;
end;

function TGLSkeleton.BoneByID(anID: Integer): TGLSkeletonBone;

  procedure CollectBones(Bone: TGLSkeletonBone);
  var
    i: Integer;
  begin
    if Bone.BoneID >= FBonesByIDCache.Count then
      FBonesByIDCache.Count := Bone.BoneID + 1;
    FBonesByIDCache[Bone.BoneID] := Bone;
    for i := 0 to Bone.Count - 1 do
      CollectBones(Bone[i]);
  end;

var
  i: Integer;
begin
  if not Assigned(FBonesByIDCache) then
  begin
    FBonesByIDCache := TList.Create;
    for i := 0 to RootBones.Count - 1 do
      CollectBones(RootBones[i]);
  end;
  Result := TGLSkeletonBone(FBonesByIDCache[anID])
end;

function TGLSkeleton.BoneByName(const aName: string): TGLSkeletonBone;
begin
  Result := RootBones.BoneByName(aName);
end;

function TGLSkeleton.BoneCount: Integer;
begin
  Result := RootBones.BoneCount;
end;

procedure TGLSkeleton.MorphTo(frameIndex: Integer);
begin
  CurrentFrame := Frames[frameIndex];
end;

procedure TGLSkeleton.MorphTo(frame: TGLSkeletonFrame);
begin
  CurrentFrame := frame;
end;

procedure TGLSkeleton.Lerp(frameIndex1, frameIndex2: Integer; lerpFactor: Single);
begin
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
  FCurrentFrame := TGLSkeletonFrame.Create;
  FCurrentFrame.TransformMode := Frames[frameIndex1].TransformMode;
  with FCurrentFrame do
  begin
    Position.Lerp(Frames[frameIndex1].Position,
      Frames[frameIndex2].Position, lerpFactor);
    case TransformMode of
      sftRotation: Rotation.AngleLerp(Frames[frameIndex1].Rotation,
          Frames[frameIndex2].Rotation, lerpFactor);
      sftQuaternion: Quaternion.Lerp(Frames[frameIndex1].Quaternion,
          Frames[frameIndex2].Quaternion, lerpFactor);
    end;
  end;
end;

procedure TGLSkeleton.BlendedLerps(const lerpInfos: array of TGLBlendedLerpInfo);
var
  i, n: Integer;
  blendPositions: TGLAffineVectorList;
  blendRotations: TGLAffineVectorList;
  blendQuaternions: TGLQuaternionList;
begin
  n := High(lerpInfos) - Low(lerpInfos) + 1;
  Assert(n >= 1);
  i := Low(lerpInfos);
  if n = 1 then
  begin
    // use fast lerp (no blend)
    with lerpInfos[i] do
      Lerp(frameIndex1, frameIndex2, lerpFactor);
  end
  else
  begin
    if Assigned(FCurrentFrame) then
      FCurrentFrame.Free;
    FCurrentFrame := TGLSkeletonFrame.Create;
    FCurrentFrame.TransformMode :=
      Frames[lerpInfos[i].frameIndex1].TransformMode;
    with FCurrentFrame do
    begin
      blendPositions := TGLAffineVectorList.Create;
      // lerp first item separately
      Position.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
        Frames[lerpInfos[i].frameIndex2].Position,
        lerpInfos[i].lerpFactor);
      if lerpInfos[i].weight <> 1 then
        Position.Scale(lerpInfos[i].weight);

      Inc(i);
      // combine the other items
      while i <= High(lerpInfos) do
      begin
        if not Assigned(lerpInfos[i].externalPositions) then
        begin
          blendPositions.Lerp(Frames[lerpInfos[i].frameIndex1].Position,
            Frames[lerpInfos[i].frameIndex2].Position,
            lerpInfos[i].lerpFactor);
          Position.AngleCombine(blendPositions, 1);
        end
        else
          Position.Combine(lerpInfos[i].externalPositions, 1);
        Inc(i);
      end;
      blendPositions.Free;

      i := Low(lerpInfos);
      case TransformMode of
        sftRotation:
          begin
            blendRotations := TGLAffineVectorList.Create;
            // lerp first item separately
            Rotation.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation,
              Frames[lerpInfos[i].frameIndex2].Rotation,
              lerpInfos[i].lerpFactor);
            Inc(i);
            // combine the other items
            while i <= High(lerpInfos) do
            begin
              if not Assigned(lerpInfos[i].externalRotations) then
              begin
                blendRotations.AngleLerp(Frames[lerpInfos[i].frameIndex1].Rotation,
                  Frames[lerpInfos[i].frameIndex2].Rotation,
                  lerpInfos[i].lerpFactor);
                Rotation.AngleCombine(blendRotations, 1);
              end
              else
                Rotation.AngleCombine(lerpInfos[i].externalRotations, 1);
              Inc(i);
            end;
            blendRotations.Free;
          end;

        sftQuaternion:
          begin
            blendQuaternions := TGLQuaternionList.Create;
            // Initial frame lerp
            Quaternion.Lerp(Frames[lerpInfos[i].frameIndex1].Quaternion,
              Frames[lerpInfos[i].frameIndex2].Quaternion,
              lerpInfos[i].lerpFactor);
            Inc(i);
            // Combine the lerped frames together
            while i <= High(lerpInfos) do
            begin
              if not Assigned(lerpInfos[i].externalQuaternions) then
              begin
                blendQuaternions.Lerp(Frames[lerpInfos[i].frameIndex1].Quaternion,
                  Frames[lerpInfos[i].frameIndex2].Quaternion,
                  lerpInfos[i].lerpFactor);
                Quaternion.Combine(blendQuaternions, 1);
              end
              else
                Quaternion.Combine(lerpInfos[i].externalQuaternions, 1);
              Inc(i);
            end;
            blendQuaternions.Free;
          end;
      end;
    end;
  end;
end;

procedure TGLSkeleton.MakeSkeletalTranslationStatic(startFrame, endFrame: Integer);
var
  delta: TAffineVector;
  i: Integer;
  f: Single;
begin
  if endFrame <= startFrame then
    Exit;
  delta := VectorSubtract(Frames[endFrame].Position[0],
    Frames[startFrame].Position[0]);
  f := -1 / (endFrame - startFrame);
  for i := startFrame to endFrame do
    Frames[i].Position[0] := VectorCombine(Frames[i].Position[0], delta,
      1, (i - startFrame) * f);
end;

procedure TGLSkeleton.MakeSkeletalRotationDelta(startFrame, endFrame: Integer);
var
  i, j: Integer;
  v: TAffineVector;
begin
  if endFrame <= startFrame then
    Exit;
  for i := startFrame to endFrame do
  begin
    for j := 0 to Frames[i].Position.Count - 1 do
    begin
      Frames[i].Position[j] := NullVector;
      v := VectorSubtract(Frames[i].Rotation[j],
        Frames[0].Rotation[j]);
      if VectorNorm(v) < 1e-6 then
        Frames[i].Rotation[j] := NullVector
      else
        Frames[i].Rotation[j] := v;
    end;
  end;
end;

procedure TGLSkeleton.MorphMesh(normalize: Boolean);
var
  i: Integer;
  mesh: TGLBaseMeshObject;
begin
  if Owner.MeshObjects.Count > 0 then
  begin
    RootBones.PrepareGlobalMatrices;
    if Colliders.Count > 0 then
      Colliders.AlignColliders;

    if FMorphInvisibleParts then
      for i := 0 to Owner.MeshObjects.Count - 1 do
      begin
        mesh := Owner.MeshObjects.Items[i];
        if (mesh is TGLSkeletonMeshObject) then
          TGLSkeletonMeshObject(mesh).ApplyCurrentSkeletonFrame(normalize);
      end
    else
      for i := 0 to Owner.MeshObjects.Count - 1 do
      begin
        mesh := Owner.MeshObjects.Items[i];
        if (mesh is TGLSkeletonMeshObject) and mesh.Visible then
          TGLSkeletonMeshObject(mesh).ApplyCurrentSkeletonFrame(normalize);
      end
  end;
end;

procedure TGLSkeleton.Synchronize(reference: TGLSkeleton);
begin
  CurrentFrame.Assign(reference.CurrentFrame);
  MorphMesh(True);
end;

procedure TGLSkeleton.Clear;
begin
  FlushBoneByIDCache;
  RootBones.Clean;
  Frames.Clear;
  FCurrentFrame.Free;
  FCurrentFrame := nil;
  FColliders.Clear;
end;

procedure TGLSkeleton.StartRagDoll; // ragdoll
var
  i: Integer;
  mesh: TGLBaseMeshObject;
begin
  if FRagDollEnabled then
    Exit
  else
    FRagDollEnabled := True;

  if Owner.MeshObjects.Count > 0 then
  begin
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      mesh := Owner.MeshObjects.Items[i];
      if mesh is TGLSkeletonMeshObject then
      begin
        TGLSkeletonMeshObject(mesh).BackupBoneMatrixInvertedMeshes;
        TGLSkeletonMeshObject(mesh).PrepareBoneMatrixInvertedMeshes;
      end;
    end;
  end;
end;

procedure TGLSkeleton.StopRagDoll; // ragdoll
var
  i: Integer;
  mesh: TGLBaseMeshObject;
begin
  FRagDollEnabled := False;
  if Owner.MeshObjects.Count > 0 then
  begin
    for i := 0 to Owner.MeshObjects.Count - 1 do
    begin
      mesh := Owner.MeshObjects.Items[i];
      if mesh is TGLSkeletonMeshObject then
        TGLSkeletonMeshObject(mesh).RestoreBoneMatrixInvertedMeshes;
    end;
  end;
end;

// ------------------
// ------------------ TGLMeshObject ------------------
// ------------------

constructor TGLMeshObject.CreateOwned(AOwner: TGLMeshObjectList);
begin
  FOwner := AOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

constructor TGLMeshObject.Create;
begin
  FMode := momTriangles;
  FTexCoords := TGLAffineVectorList.Create;
  FLightMapTexCoords := TGLAffineVectorList.Create;
  FColors := TGLVectorList.Create;
  FFaceGroups := TGLFaceGroups.CreateOwned(Self);
  FTexCoordsEx := TList.Create;
  FTangentsTexCoordIndex := 1;
  FBinormalsTexCoordIndex := 2;

  FUseVBO := vGLVectorFileObjectsEnableVBOByDefault;
  inherited;
end;

destructor TGLMeshObject.Destroy;
var
  i: Integer;
begin
  FVerticesVBO.Free;
  FNormalsVBO.Free;
  FColorsVBO.Free;
  for i := 0 to high(FTexCoordsVBO) do
    FTexCoordsVBO[i].Free;
  FLightmapTexCoordsVBO.Free;

  FFaceGroups.Free;
  FColors.Free;
  FTexCoords.Free;
  FLightMapTexCoords.Free;
  for i := 0 to FTexCoordsEx.Count - 1 do
    TGLVectorList(FTexCoordsEx[i]).Free;
  FTexCoordsEx.Free;
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

procedure TGLMeshObject.Assign(Source: TPersistent);
var
  I: Integer;
begin
  inherited Assign(Source);

  if Source is TGLMeshObject then
  begin
    FTexCoords.Assign(TGLMeshObject(Source).FTexCoords);
    FLightMapTexCoords.Assign(TGLMeshObject(Source).FLightMapTexCoords);
    FColors.Assign(TGLMeshObject(Source).FColors);
    FFaceGroups.Assign(TGLMeshObject(Source).FFaceGroups);
    FMode := TGLMeshObject(Source).FMode;
    FRenderingOptions := TGLMeshObject(Source).FRenderingOptions;
    FBinormalsTexCoordIndex := TGLMeshObject(Source).FBinormalsTexCoordIndex;
    FTangentsTexCoordIndex := TGLMeshObject(Source).FTangentsTexCoordIndex;

    // Clear FTexCoordsEx.
    for I := 0 to FTexCoordsEx.Count - 1 do
      TGLVectorList(FTexCoordsEx[I]).Free;

    FTexCoordsEx.Count := TGLMeshObject(Source).FTexCoordsEx.Count;

    // Fill FTexCoordsEx.
    for I := 0 to FTexCoordsEx.Count - 1 do
    begin
      FTexCoordsEx[I] := TGLVectorList.Create;
      TGLVectorList(FTexCoordsEx[I]).Assign(TGLMeshObject(Source).FTexCoordsEx[I]);
    end;
  end;
end;

procedure TGLMeshObject.WriteToFiler(writer: TGLVirtualWriter);
var
  i: Integer;
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(3); // Archive Version 3
    FTexCoords.WriteToFiler(writer);
    FLightMapTexCoords.WriteToFiler(writer);
    FColors.WriteToFiler(writer);
    FFaceGroups.WriteToFiler(writer);
    WriteInteger(Integer(FMode));
    WriteInteger(SizeOf(FRenderingOptions));
    Write(FRenderingOptions, SizeOf(FRenderingOptions));
    WriteInteger(FTexCoordsEx.Count);
    for i := 0 to FTexCoordsEx.Count - 1 do
      TexCoordsEx[i].WriteToFiler(writer);
    WriteInteger(BinormalsTexCoordIndex);
    WriteInteger(TangentsTexCoordIndex);
  end;
end;

procedure TGLMeshObject.ReadFromFiler(reader: TGLVirtualReader);
var
  i, Count, archiveVersion: Integer;
  lOldLightMapTexCoords: TGLTexPointList;
  tc: TTexPoint;
  size, ro: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0 .. 3] then
    with reader do
    begin
      FTexCoords.ReadFromFiler(reader);
      if archiveVersion = 0 then
      begin
        // FLightMapTexCoords did not exist back than.
        FLightMapTexCoords.Clear;
      end
      else if (archiveVersion = 1) or (archiveVersion = 2) then
      begin
        lOldLightMapTexCoords := TGLTexPointList.CreateFromFiler(reader);
        for i := 0 to lOldLightMapTexCoords.Count - 1 do
        begin
          tc:=lOldLightMapTexCoords[i];
          FLightMapTexCoords.Add(tc.S, tc.T);
        end;
        lOldLightMapTexCoords.Free;
      end
      else
      begin
        // Load FLightMapTexCoords the normal way.
        FLightMapTexCoords.ReadFromFiler(reader);
      end;

      FColors.ReadFromFiler(reader);
      FFaceGroups.ReadFromFiler(reader);
      FMode := TGLMeshObjectMode(ReadInteger);
      size := ReadInteger;
      ro := 0;
      Read(ro, size);
      FRenderingOptions := TGLMeshObjectRenderingOptions(Byte(ro));
      if archiveVersion >= 2 then
      begin
        Count := ReadInteger;
        for i := 0 to Count - 1 do
          TexCoordsEx[i].ReadFromFiler(reader);
        BinormalsTexCoordIndex := ReadInteger;
        TangentsTexCoordIndex := ReadInteger;
      end;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLMeshObject.Clear;
var
  i: Integer;
begin
  inherited;
  FFaceGroups.Clear;
  FColors.Clear;
  FTexCoords.Clear;
  FLightMapTexCoords.Clear;
  for i := 0 to FTexCoordsEx.Count - 1 do
    TexCoordsEx[i].Clear;
end;

function TGLMeshObject.ExtractTriangles(texCoords: TGLAffineVectorList = nil;
  Normals: TGLAffineVectorList = nil): TGLAffineVectorList;
begin
  case Mode of
    momTriangles:
      begin
        Result := inherited ExtractTriangles;
        if Assigned(texCoords) then
          texCoords.Assign(Self.TexCoords);
        if Assigned(normals) then
          normals.Assign(Self.Normals);
      end;
    momTriangleStrip:
      begin
        Result := TGLAffineVectorList.Create;
        ConvertStripToList(Vertices, Result);
        if Assigned(texCoords) then
          ConvertStripToList(Self.TexCoords, texCoords);
        if Assigned(normals) then
          ConvertStripToList(Self.Normals, normals);
      end;
    momFaceGroups:
      begin
        Result := TGLAffineVectorList.Create;
        FaceGroups.AddToTriangles(Result, texCoords, normals);
      end;
  else
    Result := nil;
    Assert(False);
  end;
end;

function TGLMeshObject.TriangleCount: Integer;
var
  i: Integer;
begin
  case Mode of
    momTriangles:
      Result := (Vertices.Count div 3);
    momTriangleStrip:
      begin
        Result := Vertices.Count - 2;
        if Result < 0 then
          Result := 0;
      end;
    momFaceGroups:
      begin
        Result := 0;
        for i := 0 to FaceGroups.Count - 1 do
          Result := Result + FaceGroups[i].TriangleCount;
      end;
  else
    Result := 0;
    Assert(False);
  end;
end;

procedure TGLMeshObject.PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
begin
  FaceGroups.PrepareMaterialLibraryCache(matLib);
end;

procedure TGLMeshObject.DropMaterialLibraryCache;
begin
  FaceGroups.DropMaterialLibraryCache;
end;

procedure TGLMeshObject.GetExtents(out min, max: TAffineVector);
begin
  if FVertices.Revision <> FExtentCacheRevision then
  begin
    FVertices.GetExtents(FExtentCache.min, FExtentCache.max);
    FExtentCacheRevision := FVertices.Revision;
  end;
  min := FExtentCache.min;
  max := FExtentCache.max;
end;

procedure TGLMeshObject.GetExtents(out aabb: TAABB);
begin
  if FVertices.Revision <> FExtentCacheRevision then
  begin
    FVertices.GetExtents(FExtentCache.min, FExtentCache.max);
    FExtentCacheRevision := FVertices.Revision;
  end;
  aabb := FExtentCache;
end;

function TGLMeshObject.GetBarycenter: TGLVector;
var
  dMin, dMax: TAffineVector;
begin
  GetExtents(dMin, dMax);

  Result.X := (dMin.X + dMax.X) / 2;
  Result.Y := (dMin.Y + dMax.Y) / 2;
  Result.Z := (dMin.Z + dMax.Z) / 2;
  Result.W := 0;
end;

procedure TGLMeshObject.Prepare;
var
  i: Integer;
begin
  ValidBuffers := [];
  for i := 0 to FaceGroups.Count - 1 do
    FaceGroups[i].Prepare;
end;

function TGLMeshObject.PointInObject(const aPoint: TAffineVector): Boolean;
var
  min, max: TAffineVector;
begin
  GetExtents(min, max);
  Result := (aPoint.X >= min.X) and
            (aPoint.Y >= min.Y) and
            (aPoint.Z >= min.Z) and
            (aPoint.X <= max.X) and
            (aPoint.Y <= max.Y) and
            (aPoint.Z <= max.Z);
end;

procedure TGLMeshObject.SetTexCoords(const val: TGLAffineVectorList);
begin
  FTexCoords.Assign(val);
end;

procedure TGLMeshObject.SetLightmapTexCoords(const val: TGLAffineVectorList);
begin
  FLightMapTexCoords.Assign(val);
end;

procedure TGLMeshObject.SetColors(const val: TGLVectorList);
begin
  FColors.Assign(val);
end;

procedure TGLMeshObject.SetTexCoordsEx(Index: Integer; const val: TGLVectorList);
begin
  TexCoordsEx[index].Assign(val);
end;

function TGLMeshObject.GetTexCoordsEx(Index: Integer): TGLVectorList;
var
  i: Integer;
begin
  if index > FTexCoordsEx.Count - 1 then
    for i := FTexCoordsEx.Count - 1 to index do
      FTexCoordsEx.Add(TGLVectorList.Create);
  Result := TGLVectorList(FTexCoordsEx[index]);
end;

procedure TGLMeshObject.SetBinormals(const val: TGLVectorList);
begin
  Binormals.Assign(val);
end;

function TGLMeshObject.GetBinormals: TGLVectorList;
begin
  Result := TexCoordsEx[BinormalsTexCoordIndex];
end;

procedure TGLMeshObject.SetBinormalsTexCoordIndex(const val: Integer);
begin
  Assert(val >= 0);
  if val <> FBinormalsTexCoordIndex then
  begin
    FBinormalsTexCoordIndex := val;
  end;
end;

procedure TGLMeshObject.SetTangents(const val: TGLVectorList);
begin
  Tangents.Assign(val);
end;

function TGLMeshObject.GetTangents: TGLVectorList;
begin
  Result := TexCoordsEx[TangentsTexCoordIndex];
end;

procedure TGLMeshObject.SetTangentsTexCoordIndex(const val: Integer);
begin
  Assert(val >= 0);
  if val <> FTangentsTexCoordIndex then
  begin
    FTangentsTexCoordIndex := val;
  end;
end;

procedure TGLMeshObject.GetTriangleData(tri: Integer; list: TGLAffineVectorList; var v0, v1, v2: TAffineVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
      begin
        v0 := list[3 * tri];
        v1 := list[3 * tri + 1];
        v2 := list[3 * tri + 2];
      end;
    momTriangleStrip:
      begin
        v0 := list[tri];
        v1 := list[tri + 1];
        v2 := list[tri + 2];
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TFGVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.Mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  v0 := list[fg.VertexIndices[3 * Count]];
                  v1 := list[fg.VertexIndices[3 * Count + 1]];
                  v2 := list[fg.VertexIndices[3 * Count + 2]];
                end;
              fgmmTriangleStrip:
                begin
                  v0 := list[fg.VertexIndices[Count]];
                  v1 := list[fg.VertexIndices[Count + 1]];
                  v2 := list[fg.VertexIndices[Count + 2]];
                end;
              fgmmTriangleFan:
                begin
                  v0 := list[fg.VertexIndices[0]];
                  v1 := list[fg.VertexIndices[Count + 1]];
                  v2 := list[fg.VertexIndices[Count + 2]];
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.VertexIndices[4 * (Count div 2) + 1]];
                    v2 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
                  end
                  else
                  begin
                    v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
                    v2 := list[fg.VertexIndices[4 * (Count div 2) + 3]];
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

procedure TGLMeshObject.GetTriangleData(tri: Integer; list: TGLVectorList; var v0, v1, v2: TGLVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
      begin
        v0 := list[3 * tri];
        v1 := list[3 * tri + 1];
        v2 := list[3 * tri + 2];
      end;
    momTriangleStrip:
      begin
        v0 := list[tri];
        v1 := list[tri + 1];
        v2 := list[tri + 2];
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TFGVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.Mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  v0 := list[fg.VertexIndices[3 * Count]];
                  v1 := list[fg.VertexIndices[3 * Count + 1]];
                  v2 := list[fg.VertexIndices[3 * Count + 2]];
                end;
              fgmmTriangleStrip:
                begin
                  v0 := list[fg.VertexIndices[Count]];
                  v1 := list[fg.VertexIndices[Count + 1]];
                  v2 := list[fg.VertexIndices[Count + 2]];
                end;
              fgmmTriangleFan:
                begin
                  v0 := list[fg.VertexIndices[0]];
                  v1 := list[fg.VertexIndices[Count + 1]];
                  v2 := list[fg.VertexIndices[Count + 2]];
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.VertexIndices[4 * (Count div 2) + 1]];
                    v2 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
                  end
                  else
                  begin
                    v0 := list[fg.VertexIndices[4 * (Count div 2)]];
                    v1 := list[fg.VertexIndices[4 * (Count div 2) + 2]];
                    v2 := list[fg.VertexIndices[4 * (Count div 2) + 3]];
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

procedure TGLMeshObject.SetTriangleData(tri: Integer; list: TGLAffineVectorList; const v0, v1, v2: TAffineVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
      begin
        list[3 * tri] := v0;
        list[3 * tri + 1] := v1;
        list[3 * tri + 2] := v2;
      end;
    momTriangleStrip:
      begin
        list[tri] := v0;
        list[tri + 1] := v1;
        list[tri + 2] := v2;
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TFGVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.Mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  list[fg.VertexIndices[3 * Count]] := v0;
                  list[fg.VertexIndices[3 * Count + 1]] := v1;
                  list[fg.VertexIndices[3 * Count + 2]] := v2;
                end;
              fgmmTriangleStrip:
                begin
                  list[fg.VertexIndices[Count]] := v0;
                  list[fg.VertexIndices[Count + 1]] := v1;
                  list[fg.VertexIndices[Count + 2]] := v2;
                end;
              fgmmTriangleFan:
                begin
                  list[fg.VertexIndices[0]] := v0;
                  list[fg.VertexIndices[Count + 1]] := v1;
                  list[fg.VertexIndices[Count + 2]] := v2;
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.VertexIndices[4 * (Count div 2) + 1]] := v1;
                    list[fg.VertexIndices[4 * (Count div 2) + 2]] := v2;
                  end
                  else
                  begin
                    list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.VertexIndices[4 * (Count div 2) + 2]] := v1;
                    list[fg.VertexIndices[4 * (Count div 2) + 3]] := v2;
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

procedure TGLMeshObject.SetTriangleData(tri: Integer; list: TGLVectorList; const v0, v1, v2: TGLVector);
var
  i, LastCount, Count: Integer;
  fg: TFGVertexIndexList;
begin
  case Mode of
    momTriangles:
      begin
        list[3 * tri] := v0;
        list[3 * tri + 1] := v1;
        list[3 * tri + 2] := v2;
      end;
    momTriangleStrip:
      begin
        list[tri] := v0;
        list[tri + 1] := v1;
        list[tri + 2] := v2;
      end;
    momFaceGroups:
      begin
        Count := 0;
        for i := 0 to FaceGroups.Count - 1 do
        begin
          LastCount := Count;
          fg := TFGVertexIndexList(FaceGroups[i]);
          Count := Count + fg.TriangleCount;
          if Count > tri then
          begin
            Count := tri - LastCount;
            case fg.Mode of
              fgmmTriangles, fgmmFlatTriangles:
                begin
                  list[fg.VertexIndices[3 * Count]] := v0;
                  list[fg.VertexIndices[3 * Count + 1]] := v1;
                  list[fg.VertexIndices[3 * Count + 2]] := v2;
                end;
              fgmmTriangleStrip:
                begin
                  list[fg.VertexIndices[Count]] := v0;
                  list[fg.VertexIndices[Count + 1]] := v1;
                  list[fg.VertexIndices[Count + 2]] := v2;
                end;
              fgmmTriangleFan:
                begin
                  list[fg.VertexIndices[0]] := v0;
                  list[fg.VertexIndices[Count + 1]] := v1;
                  list[fg.VertexIndices[Count + 2]] := v2;
                end;
              fgmmQuads:
                begin
                  if Count mod 2 = 0 then
                  begin
                    list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.VertexIndices[4 * (Count div 2) + 1]] := v1;
                    list[fg.VertexIndices[4 * (Count div 2) + 2]] := v2;
                  end
                  else
                  begin
                    list[fg.VertexIndices[4 * (Count div 2)]] := v0;
                    list[fg.VertexIndices[4 * (Count div 2) + 2]] := v1;
                    list[fg.VertexIndices[4 * (Count div 2) + 3]] := v2;
                  end;
                end;
            else
              Assert(False);
            end;
            Break;
          end;
        end;

      end;
  else
    Assert(False);
  end;
end;

procedure TGLMeshObject.SetUseVBO(const Value: Boolean);
var
  i: Integer;
begin
  if Value = FUseVBO then
    Exit;

  if FUseVBO then
  begin
    FreeAndNil(FVerticesVBO);
    FreeAndNil(FNormalsVBO);
    FreeAndNil(FColorsVBO);
    for i := 0 to high(FTexCoordsVBO) do
      FreeAndNil(FTexCoordsVBO[i]);
    FreeAndNil(FLightmapTexCoordsVBO);
  end;

  FValidBuffers := [];

  FUseVBO := Value;
end;

procedure TGLMeshObject.SetValidBuffers(Value: TGLVBOBuffers);
var
  I: Integer;
begin
  if FValidBuffers <> Value then
  begin
    FValidBuffers := Value;
    if Assigned(FVerticesVBO) then
      FVerticesVBO.NotifyChangesOfData;
    if Assigned(FNormalsVBO) then
      FNormalsVBO.NotifyChangesOfData;
    if Assigned(FColorsVBO) then
      FColorsVBO.NotifyChangesOfData;
    for I := 0 to high(FTexCoordsVBO) do
      if Assigned(FTexCoordsVBO[I]) then
        FTexCoordsVBO[I].NotifyChangesOfData;
    if Assigned(FLightmapTexCoordsVBO) then
      FLightmapTexCoordsVBO.NotifyChangesOfData;
  end;
end;

procedure TGLMeshObject.BuildTangentSpace(buildBinormals: Boolean = True; buildTangents: Boolean = True);
var
  i, j: Integer;
  v, n, t: array [0 .. 2] of TAffineVector;
  tangent, binormal: array [0 .. 2] of TGLVector;
  vt, tt: TAffineVector;
  interp, dot: Single;

  procedure SortVertexData(sortidx: Integer);
  begin
    if t[0].V[sortidx] < t[1].V[sortidx] then
    begin
      vt := v[0];
      tt := t[0];
      v[0] := v[1];
      t[0] := t[1];
      v[1] := vt;
      t[1] := tt;
    end;
    if t[0].V[sortidx] < t[2].V[sortidx] then
    begin
      vt := v[0];
      tt := t[0];
      v[0] := v[2];
      t[0] := t[2];
      v[2] := vt;
      t[2] := tt;
    end;
    if t[1].V[sortidx] < t[2].V[sortidx] then
    begin
      vt := v[1];
      tt := t[1];
      v[1] := v[2];
      t[1] := t[2];
      v[2] := vt;
      t[2] := tt;
    end;
  end;

begin
  Tangents.Clear;
  Binormals.Clear;
  if buildTangents then
    Tangents.Count := Vertices.Count;
  if buildBinormals then
    Binormals.Count := Vertices.Count;
  for i := 0 to TriangleCount - 1 do
  begin
    // Get triangle data
    GetTriangleData(i, Vertices, v[0], v[1], v[2]);
    GetTriangleData(i, Normals, n[0], n[1], n[2]);
    GetTriangleData(i, TexCoords, t[0], t[1], t[2]);

    for j := 0 to 2 do
    begin
      // Compute tangent
      if buildTangents then
      begin
        SortVertexData(1);

        if (t[2].Y - t[0].Y) = 0 then
          interp := 1
        else
          interp := (t[1].Y - t[0].Y) / (t[2].Y - t[0].Y);

        vt := VectorLerp(v[0], v[2], interp);
        interp := t[0].X + (t[2].X - t[0].X) * interp;
        vt := VectorSubtract(vt, v[1]);
        if t[1].X < interp then
          vt := VectorNegate(vt);
        dot := VectorDotProduct(vt, n[j]);
        vt.X := vt.X - n[j].X * dot;
        vt.Y := vt.Y - n[j].Y * dot;
        vt.Z := vt.Z - n[j].Z * dot;
        tangent[j] := VectorMake(VectorNormalize(vt), 0);
      end;

      // Compute Bi-Normal
      if buildBinormals then
      begin
        SortVertexData(0);

        if (t[2].X - t[0].X) = 0 then
          interp := 1
        else
          interp := (t[1].X - t[0].X) / (t[2].X - t[0].X);

        vt := VectorLerp(v[0], v[2], interp);
        interp := t[0].Y + (t[2].Y - t[0].Y) * interp;
        vt := VectorSubtract(vt, v[1]);
        if t[1].Y < interp then
          vt := VectorNegate(vt);
        dot := VectorDotProduct(vt, n[j]);
        vt.X := vt.X - n[j].X * dot;
        vt.Y := vt.Y - n[j].Y * dot;
        vt.Z := vt.Z - n[j].Z * dot;
        binormal[j] := VectorMake(VectorNormalize(vt), 0);
      end;
    end;

    if buildTangents then
      SetTriangleData(i, Tangents, tangent[0], tangent[1], tangent[2]);
    if buildBinormals then
      SetTriangleData(i, Binormals, binormal[0], binormal[1], binormal[2]);
  end;
end;

procedure TGLMeshObject.DeclareArraysToOpenGL(var mrci: TGLRenderContextInfo; evenIfAlreadyDeclared: Boolean = False);
var
  i: Integer;
  currentMapping: Cardinal;
  lists: array [0 .. 4] of pointer;
  tlists: array of pointer;
begin
  if evenIfAlreadyDeclared or (not FArraysDeclared) then
  begin
    FillChar(lists, SizeOf(lists), 0);
    SetLength(tlists, FTexCoordsEx.Count);

    // workaround for ATI bug, disable element VBO if
    // inside a display list
    FUseVBO := FUseVBO
      and GL.ARB_vertex_buffer_object
      and not mrci.GLStates.InsideList;

    if not FUseVBO then
    begin
      lists[0] := Vertices.List;
      lists[1] := Normals.List;
      lists[2] := Colors.List;
      lists[3] := TexCoords.List;
      lists[4] := LightMapTexCoords.List;

      for i := 0 to FTexCoordsEx.Count - 1 do
        tlists[i] := TexCoordsEx[i].List;
    end
    else
    begin
      BufferArrays;
    end;

    if not mrci.ignoreMaterials then
    begin
      if Normals.Count > 0 then
      begin
        if FUseVBO then
          FNormalsVBO.Bind;
        gl.EnableClientState(GL_NORMAL_ARRAY);
        gl.NormalPointer(GL_FLOAT, 0, lists[1]);
      end
      else
        gl.DisableClientState(GL_NORMAL_ARRAY);
      if (Colors.Count > 0) and (not mrci.ignoreMaterials) then
      begin
        if FUseVBO then
          FColorsVBO.Bind;
        gl.EnableClientState(GL_COLOR_ARRAY);
        gl.ColorPointer(4, GL_FLOAT, 0, lists[2]);
      end
      else
        gl.DisableClientState(GL_COLOR_ARRAY);
      if TexCoords.Count > 0 then
      begin
        if FUseVBO then
          FTexCoordsVBO[0].Bind;
        xgl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
        xgl.TexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), lists[3]);
      end
      else
        xgl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
      if gl.ARB_multitexture then
      begin
        if LightMapTexCoords.Count > 0 then
        begin
          if FUseVBO then
            FLightmapTexCoordsVBO.Bind;
          gl.ClientActiveTexture(GL_TEXTURE1);
          gl.TexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), lists[4]);
          gl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
          begin
            if FUseVBO then
              FTexCoordsVBO[i].Bind;
            gl.ClientActiveTexture(GL_TEXTURE0 + i);
            gl.TexCoordPointer(4, GL_FLOAT, SizeOf(TGLVector), tlists[i]);
            gl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
        end;
        gl.ClientActiveTexture(GL_TEXTURE0);
      end;
    end
    else
    begin
      gl.DisableClientState(GL_NORMAL_ARRAY);
      gl.DisableClientState(GL_COLOR_ARRAY);
      xgl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
    end;

    if Vertices.Count > 0 then
    begin
      if FUseVBO then
        FVerticesVBO.Bind;
      gl.EnableClientState(GL_VERTEX_ARRAY);
      gl.VertexPointer(3, GL_FLOAT, 0, lists[0]);
    end
    else
      gl.DisableClientState(GL_VERTEX_ARRAY);

    if gl.EXT_compiled_vertex_array and (LightMapTexCoords.Count = 0) and not FUseVBO then
      gl.LockArrays(0, Vertices.Count);

    FLastLightMapIndex := -1;
    FArraysDeclared := True;
    FLightMapArrayEnabled := False;
    if mrci.drawState <> dsPicking then
      FLastXOpenGLTexMapping := xgl.GetBitWiseMapping;
  end
  else
  begin
    if not mrci.ignoreMaterials and not (mrci.drawState = dsPicking) then
      if TexCoords.Count > 0 then
      begin
        currentMapping := xgl.GetBitWiseMapping;
        if FLastXOpenGLTexMapping <> currentMapping then
        begin
          xgl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
          xgl.TexCoordPointer(2, GL_FLOAT, SizeOf(TAffineVector), TexCoords.List);
          FLastXOpenGLTexMapping := currentMapping;
        end;
      end;
  end;
end;

procedure TGLMeshObject.DisableOpenGLArrays(var mrci: TGLRenderContextInfo);
var
  i: Integer;
begin
  if FArraysDeclared then
  begin
    DisableLightMapArray(mrci);
    if gl.EXT_compiled_vertex_array and (LightMapTexCoords.Count = 0) and not FUseVBO then
      gl.UnLockArrays;
    if Vertices.Count > 0 then
      gl.DisableClientState(GL_VERTEX_ARRAY);
    if not mrci.ignoreMaterials then
    begin
      if Normals.Count > 0 then
        gl.DisableClientState(GL_NORMAL_ARRAY);
      if (Colors.Count > 0) and (not mrci.ignoreMaterials) then
        gl.DisableClientState(GL_COLOR_ARRAY);
      if TexCoords.Count > 0 then
        xgl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
      if gl.ARB_multitexture then
      begin
        if LightMapTexCoords.Count > 0 then
        begin
          gl.ClientActiveTexture(GL_TEXTURE1);
          gl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
          begin
            gl.ClientActiveTexture(GL_TEXTURE0 + i);
            gl.DisableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
        end;
        gl.ClientActiveTexture(GL_TEXTURE0);
      end;
    end;

    if FUseVBO then
    begin
      if Vertices.Count > 0 then
        FVerticesVBO.UnBind;
      if Normals.Count > 0 then
        FNormalsVBO.UnBind;
      if Colors.Count > 0 then
        FColorsVBO.UnBind;
      if TexCoords.Count > 0 then
        FTexCoordsVBO[0].UnBind;
      if LightMapTexCoords.Count > 0 then
        FLightmapTexCoordsVBO.UnBind;
      if FTexCoordsEx.Count > 0 then
      begin
        for i := 0 to FTexCoordsEx.Count - 1 do
        begin
          if TexCoordsEx[i].Count > 0 then
            FTexCoordsVBO[i].UnBind;
        end;
      end;
    end;
    FArraysDeclared := False;
  end;
end;

procedure TGLMeshObject.EnableLightMapArray(var mrci: TGLRenderContextInfo);
begin
  if GL.ARB_multitexture and (not mrci.ignoreMaterials) then
  begin
    Assert(FArraysDeclared);
    if not FLightMapArrayEnabled then
    begin
      mrci.GLStates.ActiveTexture := 1;
      mrci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;
      mrci.GLStates.ActiveTexture := 0;
      FLightMapArrayEnabled := True;
    end;
  end;
end;

procedure TGLMeshObject.DisableLightMapArray(var mrci: TGLRenderContextInfo);
begin
  if GL.ARB_multitexture and FLightMapArrayEnabled then
  begin
    mrci.GLStates.ActiveTexture := 1;
    mrci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
    mrci.GLStates.ActiveTexture := 0;
    FLightMapArrayEnabled := False;
  end;
end;

procedure TGLMeshObject.PrepareBuildList(var mrci: TGLRenderContextInfo);
var
  i: Integer;
begin
  if (Mode = momFaceGroups) and Assigned(mrci.materialLibrary) then
  begin
    for i := 0 to FaceGroups.Count - 1 do
      with TGLFaceGroup(FaceGroups.List^[i]) do
      begin
        if MaterialCache <> nil then
          MaterialCache.PrepareBuildList;
      end;
  end;
end;

procedure TGLMeshObject.BufferArrays;
const
  BufferUsage = GL_DYNAMIC_DRAW;
var
  I: integer;
begin
  if Vertices.Count > 0 then
  begin
    if not Assigned(FVerticesVBO) then
      FVerticesVBO := TGLVBOArrayBufferHandle.Create;
    FVerticesVBO.AllocateHandle;

    if FVerticesVBO.IsDataNeedUpdate then
    begin
      FVerticesVBO.BindBufferData(Vertices.List, SizeOf(TAffineVector) * Vertices.Count, BufferUsage);
      FVerticesVBO.NotifyDataUpdated;
      FVerticesVBO.UnBind;
    end;
    Include(FValidBuffers, vbVertices);
  end;

  if Normals.Count > 0 then
  begin
    if not Assigned(FNormalsVBO) then
      FNormalsVBO := TGLVBOArrayBufferHandle.Create;
    FNormalsVBO.AllocateHandle;

    if FNormalsVBO.IsDataNeedUpdate then
    begin
      FNormalsVBO.BindBufferData(Normals.List, SizeOf(TAffineVector) * Normals.Count, BufferUsage);
      FNormalsVBO.NotifyDataUpdated;
      FNormalsVBO.UnBind;
    end;

    Include(FValidBuffers, vbNormals);
  end;

  if Colors.Count > 0 then
  begin
    if not Assigned(FColorsVBO) then
      FColorsVBO := TGLVBOArrayBufferHandle.Create;
    FColorsVBO.AllocateHandle;

    if FColorsVBO.IsDataNeedUpdate then
    begin
      FColorsVBO.BindBufferData(Colors.list, SizeOf(TGLVector) * Colors.Count, BufferUsage);
      FColorsVBO.NotifyDataUpdated;
      FColorsVBO.UnBind;
    end;

    Include(FValidBuffers, vbColors);
  end;

  if TexCoords.Count > 0 then
  begin
    if Length(FTexCoordsVBO) < 1 then
      SetLength(FTexCoordsVBO, 1);

    if not Assigned(FTexCoordsVBO[0]) then
      FTexCoordsVBO[0] := TGLVBOArrayBufferHandle.Create;
    FTexCoordsVBO[0].AllocateHandle;

    if FTexCoordsVBO[0].IsDataNeedUpdate then
    begin
      FTexCoordsVBO[0].BindBufferData(texCoords.list, SizeOf(TAffineVector) * texCoords.Count, BufferUsage);
      FTexCoordsVBO[0].NotifyDataUpdated;
      FTexCoordsVBO[0].UnBind;
    end;

    Include(FValidBuffers, vbTexCoords);
  end;

  if LightMapTexCoords.Count > 0 then
  begin
    if not Assigned(FLightmapTexCoordsVBO) then
      FLightmapTexCoordsVBO := TGLVBOArrayBufferHandle.Create;
    FLightmapTexCoordsVBO.AllocateHandle;

    FLightmapTexCoordsVBO.BindBufferData(LightMapTexCoords.list, SizeOf(TAffineVector) * LightMapTexCoords.Count, BufferUsage);
    FLightmapTexCoordsVBO.NotifyDataUpdated;
    FLightmapTexCoordsVBO.UnBind;

    Include(FValidBuffers, vbLightMapTexCoords);
  end;

  if FTexCoordsEx.Count > 0 then
  begin
    if Length(FTexCoordsVBO) < FTexCoordsEx.Count then
      SetLength(FTexCoordsVBO, FTexCoordsEx.Count);

    for I := 0 to FTexCoordsEx.Count - 1 do
    begin
      if TexCoordsEx[i].Count <= 0 then
        continue;

      if not Assigned(FTexCoordsVBO[i]) then
        FTexCoordsVBO[i] := TGLVBOArrayBufferHandle.Create;
      FTexCoordsVBO[i].AllocateHandle;

      if FTexCoordsVBO[i].IsDataNeedUpdate then
      begin
        FTexCoordsVBO[i].BindBufferData(TexCoordsEx[i].list, SizeOf(TGLVector) * TexCoordsEx[i].Count, BufferUsage);
        FTexCoordsVBO[i].NotifyDataUpdated;
        FTexCoordsVBO[i].UnBind;
      end;
    end;

    Include(FValidBuffers, vbTexCoordsEx);
  end;
  gl.CheckError;
end;

procedure TGLMeshObject.BuildList(var mrci: TGLRenderContextInfo);
var
  i, j, groupID, nbGroups: Integer;
  gotNormals, gotTexCoords, gotColor: Boolean;
  gotTexCoordsEx: array of Boolean;
  libMat: TGLLibMaterial;
  fg: TGLFaceGroup;
begin
  // Make sure no VBO is bound and states enabled
  FArraysDeclared := False;
  FLastXOpenGLTexMapping := 0;
  gotColor := (Vertices.Count = Colors.Count);
  if gotColor then
  begin
    mrci.GLStates.Enable(stColorMaterial);
    gl.ColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    mrci.GLStates.SetGLMaterialColors(cmFront, clrBlack, clrGray20, clrGray80, clrBlack, 0);
    mrci.GLStates.SetGLMaterialColors(cmBack, clrBlack, clrGray20, clrGray80, clrBlack, 0);
  end;
  case Mode of
    momTriangles, momTriangleStrip:
	if Vertices.Count > 0 then
      begin
        DeclareArraysToOpenGL(mrci);
        gotNormals := (Vertices.Count = Normals.Count);
        gotTexCoords := (Vertices.Count = TexCoords.Count);
        SetLength(gotTexCoordsEx, FTexCoordsEx.Count);
        for i := 0 to FTexCoordsEx.Count - 1 do
          gotTexCoordsEx[i] := (TexCoordsEx[i].Count > 0) and GL.ARB_multitexture;
        if Mode = momTriangles then
          gl.Begin_(GL_TRIANGLES)
        else
          gl.Begin_(GL_TRIANGLE_STRIP);
        for i := 0 to Vertices.Count - 1 do
        begin
          if gotNormals then
            gl.Normal3fv(@Normals.List[i]);
          if gotColor then
            gl.Color4fv(@Colors.List[i]);
          if FTexCoordsEx.Count > 0 then
          begin
            if gotTexCoordsEx[0] then
              gl.MultiTexCoord4fv(GL_TEXTURE0, @TexCoordsEx[0].List[i])
            else if gotTexCoords then
              xgl.TexCoord2fv(@TexCoords.List[i]);
            for j := 1 to FTexCoordsEx.Count - 1 do
              if gotTexCoordsEx[j] then
                gl.MultiTexCoord4fv(GL_TEXTURE0 + j, @TexCoordsEx[j].list[i]);
          end
          else
          begin
            if gotTexCoords then
              xgl.TexCoord2fv(@TexCoords.List[i]);
          end;
          gl.Vertex3fv(@Vertices.List[i]);
        end;
        gl.End_;
      end;
    momFaceGroups:
      begin
        if Assigned(mrci.materialLibrary) then
        begin
          if moroGroupByMaterial in RenderingOptions then
          begin
            // group-by-material rendering, reduces material switches,
            // but alters rendering order
            groupID := vNextRenderGroupID;
            Inc(vNextRenderGroupID);
            for i := 0 to FaceGroups.Count - 1 do
            begin
              if FaceGroups[i].FRenderGroupID <> groupID then
              begin
                libMat := FaceGroups[i].FMaterialCache;
                if Assigned(libMat) then
                  libMat.Apply(mrci);
                repeat
                  for j := i to FaceGroups.Count - 1 do
                    with FaceGroups[j] do
                    begin
                      if (FRenderGroupID <> groupID) and (FMaterialCache = libMat) then
                      begin
                        FRenderGroupID := groupID;
                        BuildList(mrci);
                      end;
                    end;
                until (not Assigned(libMat)) or (not libMat.UnApply(mrci));
              end;
            end;
          end
          else
          begin
            // canonical rendering (regroups only contiguous facegroups)
            i := 0;
            nbGroups := FaceGroups.Count;
            while i < nbGroups do
            begin
              libMat := FaceGroups[i].FMaterialCache;
              if Assigned(libMat) then
              begin
                libMat.Apply(mrci);
                repeat
                  j := i;
                  while j < nbGroups do
                  begin
                    fg := FaceGroups[j];
                    if fg.MaterialCache <> libMat then
                      Break;
                    fg.BuildList(mrci);
                    Inc(j);
                  end;
                until not libMat.UnApply(mrci);
                i := j;
              end
              else
              begin
                FaceGroups[i].BuildList(mrci);
                Inc(i);
              end;
            end;
          end;
          // restore faceculling
          if (stCullFace in mrci.GLStates.States) then
          begin
            if not mrci.bufferFaceCull then
              mrci.GLStates.Disable(stCullFace);
          end
          else
          begin
            if mrci.bufferFaceCull then
              mrci.GLStates.Enable(stCullFace);
          end;
        end
        else
          for i := 0 to FaceGroups.Count - 1 do
            FaceGroups[i].BuildList(mrci);
      end;
  else
    Assert(False);
  end;
  DisableOpenGLArrays(mrci);
end;

// ------------------
// ------------------ TGLMeshObjectList ------------------
// ------------------

constructor TGLMeshObjectList.CreateOwned(aOwner: TGLBaseMesh);
begin
  FOwner := AOwner;
  Create;
end;

destructor TGLMeshObjectList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TGLMeshObjectList.ReadFromFiler(reader: TGLVirtualReader);
var
  i: Integer;
  mesh: TGLMeshObject;
begin
  inherited;
  for i := 0 to Count - 1 do
  begin
    mesh := Items[i];
    mesh.FOwner := Self;
    if mesh is TGLSkeletonMeshObject then
      TGLSkeletonMeshObject(mesh).PrepareBoneMatrixInvertedMeshes;
  end;
end;

procedure TGLMeshObjectList.PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TGLMeshObject(List^[i]).PrepareMaterialLibraryCache(matLib);
end;

procedure TGLMeshObjectList.DropMaterialLibraryCache;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TGLMeshObject(List^[i]).DropMaterialLibraryCache;
end;

procedure TGLMeshObjectList.PrepareBuildList(var mrci: TGLRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if Visible then
        PrepareBuildList(mrci);
end;

procedure TGLMeshObjectList.BuildList(var mrci: TGLRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
      if Visible then
        BuildList(mrci);
end;

procedure TGLMeshObjectList.MorphTo(morphTargetIndex: Integer);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TGLMorphableMeshObject then
      TGLMorphableMeshObject(Items[i]).MorphTo(morphTargetIndex);
end;

procedure TGLMeshObjectList.Lerp(morphTargetIndex1, morphTargetIndex2: Integer; lerpFactor: Single);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i] is TGLMorphableMeshObject then
      TGLMorphableMeshObject(Items[i]).Lerp(morphTargetIndex1, morphTargetIndex2, lerpFactor);
end;

function TGLMeshObjectList.MorphTargetCount: Integer;
var
  i: Integer;
begin
  Result := MaxInt;
  for i := 0 to Count - 1 do
    if Items[i] is TGLMorphableMeshObject then
      with TGLMorphableMeshObject(Items[i]) do
        if Result > MorphTargets.Count then
          Result := MorphTargets.Count;
  if Result = MaxInt then
    Result := 0;
end;

procedure TGLMeshObjectList.Clear;
var
  i: Integer;
begin
  DropMaterialLibraryCache;
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

function TGLMeshObjectList.GetMeshObject(Index: Integer): TGLMeshObject;
begin
  Result := TGLMeshObject(List^[Index]);
end;

procedure TGLMeshObjectList.GetExtents(out min, max: TAffineVector);
var
  i, k: Integer;
  lMin, lMax: TAffineVector;
const
  cBigValue: Single = 1E30;
  cSmallValue: Single = -1E30;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to Count - 1 do
  begin
    GetMeshObject(i).GetExtents(lMin, lMax);
    for k := 0 to 2 do
    begin
      if lMin.V[k] < min.V[k] then
        min.V[k] := lMin.V[k];
      if lMax.V[k] > max.V[k] then
        max.V[k] := lMax.V[k];
    end;
  end;
end;

procedure TGLMeshObjectList.Translate(const delta: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    GetMeshObject(i).Translate(delta);
end;

function TGLMeshObjectList.ExtractTriangles(texCoords: TGLAffineVectorList = nil;
  normals: TGLAffineVectorList = nil): TGLAffineVectorList;
var
  i: Integer;
  obj: TGLMeshObject;
  objTris: TGLAffineVectorList;
  objTexCoords: TGLAffineVectorList;
  objNormals: TGLAffineVectorList;
begin
  Result := TGLAffineVectorList.Create;
  Result.AdjustCapacityToAtLeast(Self.TriangleCount * 3);
  if Assigned(texCoords) then
    objTexCoords := TGLAffineVectorList.Create
  else
    objTexCoords := nil;
  if Assigned(normals) then
    objNormals := TGLAffineVectorList.Create
  else
    objNormals := nil;
  try
    for i := 0 to Count - 1 do
    begin
      obj := GetMeshObject(i);
      if not obj.Visible then
        continue;
      objTris := obj.ExtractTriangles(objTexCoords, objNormals);
      try
        Result.Add(objTris);
        if Assigned(texCoords) then
        begin
          texCoords.Add(objTexCoords);
          objTexCoords.Count := 0;
        end;
        if Assigned(normals) then
        begin
          normals.Add(objNormals);
          objNormals.Count := 0;
        end;
      finally
        objTris.Free;
      end;
    end;
  finally
    objTexCoords.Free;
    objNormals.Free;
  end;
end;

function TGLMeshObjectList.TriangleCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Items[i].TriangleCount;
end;

function TGLMeshObjectList.Area: Single;
var
  i: Integer;
  Tri: TFaceRec;
  List: TGLAffineVectorList;

begin
  Result := 0;
  List := Self.ExtractTriangles;
  if List.Count > 0 then
  try
    i := 0;
    while i < List.Count do
    begin
      Tri.Normal := CalcPlaneNormal(List[i], List[i+1], List[i+2]);
      Tri.V1 := VectorTransform(List[i], TGLBaseSceneObject(Owner).AbsoluteMatrix);
      Tri.V2 := VectorTransform(List[i+1], TGLBaseSceneObject(Owner).AbsoluteMatrix);
      Tri.V3 := VectorTransform(List[i+2], TGLBaseSceneObject(Owner).AbsoluteMatrix);
      Inc(i, 3);
      Result := Result + TriangleArea(Tri.V1, Tri.V2, Tri.V3);
    end;
  finally
    List.Free();
  end;
end;

function TGLMeshObjectList.Volume: Single;
var
  i: Integer;
  Tri: TFaceRec;
  List: TGLAffineVectorList;

begin
  Result := 0;
  List := Self.ExtractTriangles;
  if List.Count > 0 then
  try
    i := 0;
    while i < List.Count do
    begin
      Tri.Normal := CalcPlaneNormal(List[i], List[i+1], List[i+2]);
      Tri.V1 := VectorTransform(List[i], TGLBaseSceneObject(Owner).AbsoluteMatrix);
      Tri.V2 := VectorTransform(List[i+1], TGLBaseSceneObject(Owner).AbsoluteMatrix);
      Tri.V3 := VectorTransform(List[i+2], TGLBaseSceneObject(Owner).AbsoluteMatrix);
      Inc(i, 3);
      Result := Result + VectorDotProduct(Tri.V1, VectorCrossProduct(Tri.V2, Tri.V3));
    end;
    Result := Result / 6;
  finally
    List.Free();
  end;
end;

procedure TGLMeshObjectList.Prepare;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Prepare;
end;

function TGLMeshObjectList.FindMeshByName(const MeshName: string): TGLMeshObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].Name = MeshName then
    begin
      Result := Items[i];
      Break;
    end;
end;

procedure TGLMeshObjectList.BuildTangentSpace(buildBinormals, buildTangents: Boolean);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetMeshObject(I).BuildTangentSpace(buildBinormals, buildTangents);
end;

function TGLMeshObjectList.GetUseVBO: Boolean;
var
  I: Integer;
begin
  Result := True;
  if Count <> 0 then
    for I := 0 to Count - 1 do
      Result := Result and GetMeshObject(I).FUseVBO;
end;

procedure TGLMeshObjectList.SetUseVBO(const Value: Boolean);
var
  I: Integer;
begin
  if Count <> 0 then
    for I := 0 to Count - 1 do
      GetMeshObject(I).SetUseVBO(Value);
end;

// ------------------
// ------------------ TGLMeshMorphTarget ------------------
// ------------------

constructor TGLMeshMorphTarget.CreateOwned(AOwner: TGLMeshMorphTargetList);
begin
  FOwner := AOwner;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

destructor TGLMeshMorphTarget.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

procedure TGLMeshMorphTarget.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    // nothing
  end;
end;

procedure TGLMeshMorphTarget.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      // nothing
    end
  else
    RaiseFilerException(archiveVersion);
end;

// ------------------
// ------------------ TGLMeshMorphTargetList ------------------
// ------------------

constructor TGLMeshMorphTargetList.CreateOwned(aOwner: TPersistent);
begin
  FOwner := AOwner;
  Create;
end;

destructor TGLMeshMorphTargetList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TGLMeshMorphTargetList.ReadFromFiler(reader: TGLVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TGLMeshMorphTargetList.Translate(const delta: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Translate(delta);
end;

procedure TGLMeshMorphTargetList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    with Items[i] do
    begin
      FOwner := nil;
      Free;
    end;
  inherited;
end;

function TGLMeshMorphTargetList.GeTGLMeshMorphTarget(Index: Integer): TGLMeshMorphTarget;
begin
  Result := TGLMeshMorphTarget(List^[Index]);
end;

// ------------------
// ------------------ TGLMorphableMeshObject ------------------
// ------------------

constructor TGLMorphableMeshObject.Create;
begin
  inherited;
  FMorphTargets := TGLMeshMorphTargetList.CreateOwned(Self);
end;

destructor TGLMorphableMeshObject.Destroy;
begin
  FMorphTargets.Free;
  inherited;
end;

procedure TGLMorphableMeshObject.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FMorphTargets.WriteToFiler(writer);
  end;
end;

procedure TGLMorphableMeshObject.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FMorphTargets.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLMorphableMeshObject.Clear;
begin
  inherited;
  FMorphTargets.Clear;
end;

procedure TGLMorphableMeshObject.Translate(const delta: TAffineVector);
begin
  inherited;
  MorphTargets.Translate(delta);
  ValidBuffers := ValidBuffers - [vbVertices];
end;

procedure TGLMorphableMeshObject.MorphTo(morphTargetIndex: Integer);
begin
  if (morphTargetIndex = 0) and (MorphTargets.Count = 0) then
    Exit;
  Assert(Cardinal(morphTargetIndex) < Cardinal(MorphTargets.Count));
  with MorphTargets[morphTargetIndex] do
  begin
    if Vertices.Count > 0 then
    begin
      Self.Vertices.Assign(Vertices);
      ValidBuffers := ValidBuffers - [vbVertices];
    end;
    if Normals.Count > 0 then
    begin
      Self.Normals.Assign(Normals);
      ValidBuffers := ValidBuffers - [vbNormals];
    end;
  end;
end;

procedure TGLMorphableMeshObject.Lerp(morphTargetIndex1, morphTargetIndex2: Integer; lerpFactor: Single);
var
  mt1, mt2: TGLMeshMorphTarget;
begin
  Assert((Cardinal(morphTargetIndex1) < Cardinal(MorphTargets.Count)) and
    (Cardinal(morphTargetIndex2) < Cardinal(MorphTargets.Count)));
  if lerpFactor = 0 then
    MorphTo(morphTargetIndex1)
  else if lerpFactor = 1 then
    MorphTo(morphTargetIndex2)
  else
  begin
    mt1 := MorphTargets[morphTargetIndex1];
    mt2 := MorphTargets[morphTargetIndex2];
    if mt1.Vertices.Count > 0 then
    begin
      Vertices.Lerp(mt1.Vertices, mt2.Vertices, lerpFactor);
      ValidBuffers := ValidBuffers - [vbVertices];
    end;
    if mt1.Normals.Count > 0 then
    begin
      Normals.Lerp(mt1.Normals, mt2.Normals, lerpFactor);
      Normals.Normalize;
      ValidBuffers := ValidBuffers - [vbNormals];
    end;
  end;
end;

// ------------------
// ------------------ TGLSkeletonMeshObject ------------------
// ------------------

constructor TGLSkeletonMeshObject.Create;
begin
  FBoneMatrixInvertedMeshes := TList.Create;
  FBackupInvertedMeshes := TList.Create; // ragdoll
  inherited Create;
end;

destructor TGLSkeletonMeshObject.Destroy;
begin
  Clear;
  FBoneMatrixInvertedMeshes.Free;
  FBackupInvertedMeshes.Free;
  inherited Destroy;
end;

procedure TGLSkeletonMeshObject.WriteToFiler(writer: TGLVirtualWriter);
var
  i: Integer;
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteInteger(FVerticeBoneWeightCount);
    WriteInteger(FBonesPerVertex);
    WriteInteger(FVerticeBoneWeightCapacity);
    for i := 0 to FVerticeBoneWeightCount - 1 do
      Write(FVerticesBonesWeights[i][0], FBonesPerVertex * SizeOf(TGLVertexBoneWeight));
  end;
end;

procedure TGLSkeletonMeshObject.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion, i: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FVerticeBoneWeightCount := ReadInteger;
      FBonesPerVertex := ReadInteger;
      FVerticeBoneWeightCapacity := ReadInteger;
      ResizeVerticesBonesWeights;
      for i := 0 to FVerticeBoneWeightCount - 1 do
        Read(FVerticesBonesWeights[i][0], FBonesPerVertex * SizeOf(TGLVertexBoneWeight));
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLSkeletonMeshObject.Clear;
var
  i: Integer;
begin
  inherited;
  FVerticeBoneWeightCount := 0;
  FBonesPerVertex := 0;
  ResizeVerticesBonesWeights;
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TGLBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
end;

procedure TGLSkeletonMeshObject.SetVerticeBoneWeightCount(const val: Integer);
begin
  if val <> FVerticeBoneWeightCount then
  begin
    FVerticeBoneWeightCount := val;
    if FVerticeBoneWeightCount > FVerticeBoneWeightCapacity then
      VerticeBoneWeightCapacity := FVerticeBoneWeightCount + 16;
    FLastVerticeBoneWeightCount := FVerticeBoneWeightCount;
  end;
end;

procedure TGLSkeletonMeshObject.SetVerticeBoneWeightCapacity(const val: Integer);
begin
  if val <> FVerticeBoneWeightCapacity then
  begin
    FVerticeBoneWeightCapacity := val;
    ResizeVerticesBonesWeights;
  end;
end;

procedure TGLSkeletonMeshObject.SetBonesPerVertex(const val: Integer);
begin
  if val <> FBonesPerVertex then
  begin
    FBonesPerVertex := val;
    ResizeVerticesBonesWeights;
  end;
end;

procedure TGLSkeletonMeshObject.ResizeVerticesBonesWeights;
var
  n, m, i, j: Integer;
  newArea: PGLVerticesBoneWeights;
begin
  n := BonesPerVertex * VerticeBoneWeightCapacity;
  if n = 0 then
  begin
    // release everything
    if Assigned(FVerticesBonesWeights) then
    begin
      FreeMem(FVerticesBonesWeights[0]);
      FreeMem(FVerticesBonesWeights);
      FVerticesBonesWeights := nil;
    end;
  end
  else
  begin
    // allocate new area
    GetMem(newArea, VerticeBoneWeightCapacity * SizeOf(PGLVertexBoneWeightArray));
    newArea[0] := AllocMem(n * SizeOf(TGLVertexBoneWeight));
    for i := 1 to VerticeBoneWeightCapacity - 1 do
      newArea[i] := PGLVertexBoneWeightArray(Cardinal(newArea[0]) +
	    Cardinal(i * SizeOf(TGLVertexBoneWeight) * BonesPerVertex));
    // transfer old data
    if FLastVerticeBoneWeightCount < VerticeBoneWeightCount then
      n := FLastVerticeBoneWeightCount
    else
      n := VerticeBoneWeightCount;
    if FLastBonesPerVertex < BonesPerVertex then
      m := FLastBonesPerVertex
    else
      m := BonesPerVertex;
    for i := 0 to n - 1 do
      for j := 0 to m - 1 do
        newArea[i][j] := VerticesBonesWeights[i][j];
    // release old area and switch to new
    if Assigned(FVerticesBonesWeights) then
    begin
      FreeMem(FVerticesBonesWeights[0]);
      FreeMem(FVerticesBonesWeights);
    end;
    FVerticesBonesWeights := newArea;
  end;
  FLastBonesPerVertex := FBonesPerVertex;
end;

procedure TGLSkeletonMeshObject.AddWeightedBone(aBoneID: Integer; aWeight: Single);
begin
  if BonesPerVertex < 1 then
    BonesPerVertex := 1;
  VerticeBoneWeightCount := VerticeBoneWeightCount + 1;
  with VerticesBonesWeights^[VerticeBoneWeightCount - 1]^[0] do
  begin
    BoneID := aBoneID;
    Weight := aWeight;
  end;
end;

procedure TGLSkeletonMeshObject.AddWeightedBones(const boneIDs: TGLVertexBoneWeightDynArray);
var
  i: Integer;
  n: Integer;
begin
  n := Length(boneIDs);
  if BonesPerVertex < n then
    BonesPerVertex := n;
  VerticeBoneWeightCount := VerticeBoneWeightCount + 1;
  for i := 0 to n - 1 do
  begin
    with VerticesBonesWeights^[VerticeBoneWeightCount - 1]^[i] do
    begin
      BoneID := boneIDs[i].BoneID;
      Weight := boneIDs[i].Weight;
    end;
  end;
end;

function TGLSkeletonMeshObject.FindOrAdd(BoneID: Integer; const vertex, normal: TAffineVector): Integer;
var
  i: Integer;
  dynArray: TGLVertexBoneWeightDynArray;
begin
  if BonesPerVertex > 1 then
  begin
    SetLength(dynArray, 1);
    dynArray[0].BoneID := boneID;
    dynArray[0].Weight := 1;
    Result := FindOrAdd(dynArray, vertex, normal);
    Exit;
  end;
  Result := -1;
  for i := 0 to Vertices.Count - 1 do
    if (VerticesBonesWeights^[i]^[0].BoneID = BoneID) and VectorEquals(Vertices.List^[i], vertex) and
	  VectorEquals(Normals.List^[i], normal) then
    begin
      Result := i;
      Break;
    end;
  if Result < 0 then
  begin
    AddWeightedBone(BoneID, 1);
    Vertices.Add(vertex);
    Result := Normals.Add(normal);
  end;
end;

function TGLSkeletonMeshObject.FindOrAdd(const boneIDs: TGLVertexBoneWeightDynArray; const vertex,
  normal: TAffineVector): Integer;
var
  i, j: Integer;
  bonesMatch: Boolean;
begin
  Result := -1;
  for i := 0 to Vertices.Count - 1 do
  begin
    bonesMatch := True;
    for j := 0 to High(boneIDs) do
    begin
      if (boneIDs[j].BoneID <> VerticesBonesWeights^[i]^[j].BoneID)
        or (boneIDs[j].Weight <> VerticesBonesWeights^[i]^[j].Weight) then
      begin
        bonesMatch := False;
        Break;
      end;
    end;
    if bonesMatch and VectorEquals(Vertices[i], vertex)
      and VectorEquals(Normals[i], normal) then
    begin
      Result := i;
      Break;
    end;
  end;
  if Result < 0 then
  begin
    AddWeightedBones(boneIDs);
    Vertices.Add(vertex);
    Result := Normals.Add(normal);
  end;
end;

procedure TGLSkeletonMeshObject.PrepareBoneMatrixInvertedMeshes;
var
  i, k, boneIndex: Integer;
  invMesh: TGLBaseMeshObject;
  invMat: TGLMatrix;
  Bone: TGLSkeletonBone;
  p: TGLVector;
begin
  // cleanup existing stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TGLBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
  // calculate
  for k := 0 to BonesPerVertex - 1 do
  begin
    invMesh := TGLBaseMeshObject.Create;
    FBoneMatrixInvertedMeshes.Add(invMesh);
    invMesh.Vertices := Vertices;
    invMesh.Normals := Normals;
    for i := 0 to Vertices.Count - 1 do
    begin
      boneIndex := VerticesBonesWeights^[i]^[k].BoneID;
      Bone := Owner.Owner.Skeleton.RootBones.BoneByID(boneIndex);
      // transform point
      MakePoint(p, Vertices[i]);
      invMat := Bone.GlobalMatrix;
      InvertMatrix(invMat);
      p := VectorTransform(p, invMat);
      invMesh.Vertices[i] := PAffineVector(@p)^;
      // transform normal
      SetVector(p, normals[i]);
      invMat := Bone.GlobalMatrix;
      invMat.W := NullHmgPoint;
      InvertMatrix(invMat);
      p := VectorTransform(p, invMat);
      invMesh.Normals[i] := PAffineVector(@p)^;
    end;
  end;
end;

procedure TGLSkeletonMeshObject.BackupBoneMatrixInvertedMeshes; // ragdoll
var
  i: Integer;
  bm: TGLBaseMeshObject;
begin
  // cleanup existing stuff
  for i := 0 to FBackupInvertedMeshes.Count - 1 do
    TGLBaseMeshObject(FBackupInvertedMeshes[i]).Free;
  FBackupInvertedMeshes.Clear;
  // copy current stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
  begin
    bm := TGLBaseMeshObject.Create;
    bm.Assign(TGLBaseMeshObject(FBoneMatrixInvertedMeshes[i]));
    FBackupInvertedMeshes.Add(bm);
    TGLBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  end;
  FBoneMatrixInvertedMeshes.Clear;
end;

procedure TGLSkeletonMeshObject.RestoreBoneMatrixInvertedMeshes; // ragdoll
var
  i: Integer;
  bm: TGLBaseMeshObject;
begin
  // cleanup existing stuff
  for i := 0 to FBoneMatrixInvertedMeshes.Count - 1 do
    TGLBaseMeshObject(FBoneMatrixInvertedMeshes[i]).Free;
  FBoneMatrixInvertedMeshes.Clear;
  // restore the backup
  for i := 0 to FBackupInvertedMeshes.Count - 1 do
  begin
    bm := TGLBaseMeshObject.Create;
    bm.Assign(TGLBaseMeshObject(FBackupInvertedMeshes[i]));
    FBoneMatrixInvertedMeshes.Add(bm);
    TGLBaseMeshObject(FBackupInvertedMeshes[i]).Free;
  end;
  FBackupInvertedMeshes.Clear;
end;

procedure TGLSkeletonMeshObject.ApplyCurrentSkeletonFrame(normalize: Boolean);
var
  i, j, BoneID: Integer;
  refVertices, refNormals: TGLAffineVectorList;
  n, nt: TGLVector;
  Bone: TGLSkeletonBone;
  Skeleton: TGLSkeleton;
  tempvert, tempnorm: TAffineVector;
begin
  with TGLBaseMeshObject(FBoneMatrixInvertedMeshes[0]) do
  begin
    refVertices := Vertices;
    refNormals := Normals;
  end;
  Skeleton := Owner.Owner.Skeleton;
  n.W := 0;
  if BonesPerVertex = 1 then
  begin
    // simple case, one bone per vertex
    for i := 0 to refVertices.Count - 1 do
    begin
      BoneID := VerticesBonesWeights^[i]^[0].BoneID;
      Bone := Skeleton.BoneByID(BoneID);
      Vertices.List^[i] := VectorTransform(refVertices.List^[i], Bone.GlobalMatrix);
      PAffineVector(@n)^ := refNormals.list^[i];
      nt := VectorTransform(n, Bone.GlobalMatrix);
      Normals.List^[i] := PAffineVector(@nt)^;
    end;
  end
  else
  begin
    // multiple bones per vertex
    for i := 0 to refVertices.Count - 1 do
    begin
      Vertices.List^[i] := NullVector;
      Normals.List^[i] := NullVector;
      for j := 0 to BonesPerVertex - 1 do
      begin
        with TGLBaseMeshObject(FBoneMatrixInvertedMeshes[j]) do
        begin
          refVertices := Vertices;
          refNormals := Normals;
        end;
        tempvert := NullVector;
        tempnorm := NullVector;
        if VerticesBonesWeights^[i]^[j].weight <> 0 then
        begin
          BoneID := VerticesBonesWeights^[i]^[j].BoneID;
          Bone := Skeleton.BoneByID(BoneID);
          CombineVector(tempvert, VectorTransform(refVertices.list^[i], Bone.GlobalMatrix),
            VerticesBonesWeights^[i]^[j].weight);
          PAffineVector(@n)^ := refNormals.list^[i];
          n := VectorTransform(n, Bone.GlobalMatrix);
          CombineVector(tempnorm, PAffineVector(@n)^, VerticesBonesWeights^[i]^[j].weight);
        end;
        AddVector(Vertices.list^[i], tempvert);
        AddVector(normals.list^[i], tempnorm);
      end;
    end;
  end;
  if normalize then
    normals.normalize;
end;

// ------------------
// ------------------ TGLFaceGroup ------------------
// ------------------

constructor TGLFaceGroup.CreateOwned(AOwner: TGLFaceGroups);
begin
  FOwner := AOwner;
  FLightMapIndex := -1;
  Create;
  if Assigned(FOwner) then
    FOwner.Add(Self);
end;

destructor TGLFaceGroup.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.Remove(Self);
  inherited;
end;

procedure TGLFaceGroup.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    if FLightMapIndex < 0 then
    begin
      WriteInteger(0); // Archive Version 0
      WriteString(FMaterialName);
    end
    else
    begin
      WriteInteger(1); // Archive Version 1, added FLightMapIndex
      WriteString(FMaterialName);
      WriteInteger(FLightMapIndex);
    end;
  end;
end;

procedure TGLFaceGroup.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion in [0 .. 1] then
    with reader do
    begin
      FMaterialName := ReadString;
      if archiveVersion >= 1 then
        FLightMapIndex := ReadInteger
      else
        FLightMapIndex := -1;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TGLFaceGroup.AttachLightmap(lightMap: TGLTexture; var mrci: TGLRenderContextInfo);
begin
  if GL.ARB_multitexture then
    with lightMap do
    begin
      Assert(Image.NativeTextureTarget = ttTexture2D);
      mrci.GLStates.TextureBinding[1, ttTexture2D] := Handle;
      gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      mrci.GLStates.ActiveTexture := 0;
    end;
end;

procedure TGLFaceGroup.AttachOrDetachLightmap(var mrci: TGLRenderContextInfo);
var
  libMat: TGLLibMaterial;
begin
  if GL.ARB_multitexture then
  begin
    if (not mrci.ignoreMaterials) and Assigned(mrci.LightmapLibrary) then
    begin
      if Owner.Owner.FLastLightMapIndex <> LightMapIndex then
      begin
        Owner.Owner.FLastLightMapIndex := LightMapIndex;
        if LightMapIndex >= 0 then
        begin
          // attach and activate lightmap
          Assert(LightMapIndex < TGLMaterialLibrary(mrci.LightmapLibrary).Materials.Count);
          libMat := TGLMaterialLibrary(mrci.LightmapLibrary).Materials[LightMapIndex];
          AttachLightmap(libMat.Material.Texture, mrci);
          Owner.Owner.EnableLightMapArray(mrci);
        end
        else
        begin
          // desactivate lightmap
          Owner.Owner.DisableLightMapArray(mrci);
        end;
      end;
    end;
  end;
end;

procedure TGLFaceGroup.PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
begin
  if (FMaterialName <> '') and (matLib <> nil) then
    FMaterialCache := matLib.Materials.GetLibMaterialByName(FMaterialName)
  else
    FMaterialCache := nil;
end;

procedure TGLFaceGroup.DropMaterialLibraryCache;
begin
  FMaterialCache := nil;
end;

procedure TGLFaceGroup.AddToTriangles(aList: TGLAffineVectorList; aTexCoords: TGLAffineVectorList = nil;
  aNormals: TGLAffineVectorList = nil);
begin
  // nothing
end;

procedure TGLFaceGroup.Reverse;
begin
  // nothing
end;

procedure TGLFaceGroup.Prepare;
begin
  // nothing
end;

// ------------------
// ------------------ TFGVertexIndexList ------------------
// ------------------

constructor TFGVertexIndexList.Create;
begin
  inherited;
  FVertexIndices := TGLIntegerList.Create;
  FMode := fgmmTriangles;
end;

destructor TFGVertexIndexList.Destroy;
begin
  FVertexIndices.Free;
  FIndexVBO.Free;
  inherited;
end;

procedure TFGVertexIndexList.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FVertexIndices.WriteToFiler(writer);
    WriteInteger(Integer(FMode));
  end;
end;

procedure TFGVertexIndexList.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FVertexIndices.ReadFromFiler(reader);
      FMode := TGLFaceGroupMeshMode(ReadInteger);
      InvalidateVBO;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TFGVertexIndexList.SetupVBO;
const
  BufferUsage = GL_STATIC_DRAW;
begin
  if not Assigned(FIndexVBO) then
    FIndexVBO := TGLVBOElementArrayHandle.Create;

  FIndexVBO.AllocateHandle;

  if FIndexVBO.IsDataNeedUpdate then
  begin
    FIndexVBO.BindBufferData(vertexIndices.list, SizeOf(Integer) * vertexIndices.Count, BufferUsage);
    FIndexVBO.NotifyDataUpdated;
  end;
end;

procedure TFGVertexIndexList.SetVertexIndices(const val: TGLIntegerList);
begin
  FVertexIndices.Assign(val);
  InvalidateVBO;
end;

procedure TFGVertexIndexList.BuildList(var mrci: TGLRenderContextInfo);
const
  cFaceGroupMeshModeToOpenGL: array [TGLFaceGroupMeshMode] of Integer = (GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLES,
    GL_TRIANGLE_FAN, GL_QUADS);
begin
  if VertexIndices.Count = 0 then
    Exit;
  Owner.Owner.DeclareArraysToOpenGL(mrci, False);
  AttachOrDetachLightmap(mrci);

  if Owner.Owner.UseVBO then
  begin
    SetupVBO;

    FIndexVBO.Bind;
    gl.DrawElements(cFaceGroupMeshModeToOpenGL[mode], vertexIndices.Count, GL_UNSIGNED_INT, nil);
    FIndexVBO.UnBind;
  end
  else
  begin
    gl.DrawElements(cFaceGroupMeshModeToOpenGL[mode], vertexIndices.Count, GL_UNSIGNED_INT, vertexIndices.list);
  end;
end;

procedure TFGVertexIndexList.AddToList(Source, destination: TGLAffineVectorList; indices: TGLIntegerList);
var
  i, n: Integer;
begin
  if not Assigned(destination) then
    Exit;
  if indices.Count < 3 then
    Exit;
  case Mode of
    fgmmTriangles, fgmmFlatTriangles:
      begin
        n := (indices.Count div 3) * 3;
        if Source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n);
          for i := 0 to n - 1 do
            destination.Add(Source[indices.list^[i]]);
        end
        else
          destination.AddNulls(destination.Count + n);
      end;
    fgmmTriangleStrip:
      begin
        if Source.Count > 0 then
          ConvertStripToList(Source, indices, destination)
        else
          destination.AddNulls(destination.Count + (indices.Count - 2) * 3);
      end;
    fgmmTriangleFan:
      begin
        n := (indices.Count - 2) * 3;
        if Source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n);
          for i := 2 to VertexIndices.Count - 1 do
          begin
            destination.Add(Source[indices.list^[0]], Source[indices.list^[i - 1]], Source[indices.list^[i]]);
          end;
        end
        else
          destination.AddNulls(destination.Count + n);
      end;
    fgmmQuads:
      begin
        n := indices.Count div 4;
        if Source.Count > 0 then
        begin
          destination.AdjustCapacityToAtLeast(destination.Count + n * 6);
          i := 0;
          while n > 0 do
          begin
            destination.Add(Source[indices.list^[i]], Source[indices.list^[i + 1]], Source[indices.list^[i + 2]]);
            destination.Add(Source[indices.list^[i]], Source[indices.list^[i + 2]], Source[indices.list^[i + 3]]);
            Inc(i, 4);
            Dec(n);
          end;
        end
        else
          destination.AddNulls(destination.Count + n * 6);
      end;
  else
    Assert(False);
  end;
end;

procedure TFGVertexIndexList.AddToTriangles(aList: TGLAffineVectorList; aTexCoords: TGLAffineVectorList = nil;
  aNormals: TGLAffineVectorList = nil);
var
  mo: TGLMeshObject;
begin
  mo := Owner.Owner;
  AddToList(mo.Vertices, aList, VertexIndices);
  AddToList(mo.TexCoords, aTexCoords, VertexIndices);
  AddToList(mo.Normals, aNormals, VertexIndices);
  InvalidateVBO;
end;

function TFGVertexIndexList.TriangleCount: Integer;
begin
  case Mode of
    fgmmTriangles, fgmmFlatTriangles:
      Result := VertexIndices.Count div 3;
    fgmmTriangleFan, fgmmTriangleStrip:
      begin
        Result := VertexIndices.Count - 2;
        if Result < 0 then
          Result := 0;
      end;
    fgmmQuads:
      result := VertexIndices.Count div 2;
  else
    Result := 0;
    Assert(False);
  end;
end;

procedure TFGVertexIndexList.Reverse;
begin
  VertexIndices.Reverse;
  InvalidateVBO;
end;

procedure TFGVertexIndexList.Add(idx: Integer);
begin
  FVertexIndices.Add(idx);
  InvalidateVBO;
end;

procedure TFGVertexIndexList.GetExtents(var min, max: TAffineVector);
var
  i, k: Integer;
  f: Single;
  ref: PFloatArray;
const
  cBigValue: Single = 1E50;
  cSmallValue: Single = -1E50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to VertexIndices.Count - 1 do
  begin
    ref := Owner.Owner.Vertices.ItemAddress[VertexIndices[i]];
    for k := 0 to 2 do
    begin
      f := ref^[k];
      if f < min.V[k] then
        min.V[k] := f;
      if f > max.V[k] then
        max.V[k] := f;
    end;
  end;
end;

procedure TFGVertexIndexList.ConvertToList;
var
  i: Integer;
  bufList: TGLIntegerList;
begin
  if VertexIndices.Count >= 3 then
  begin
    case Mode of
      fgmmTriangleStrip:
        begin
          bufList := TGLIntegerList.Create;
          try
            ConvertStripToList(VertexIndices, bufList);
            VertexIndices := bufList;
          finally
            bufList.Free;
          end;
          FMode := fgmmTriangles;
        end;
      fgmmTriangleFan:
        begin
          bufList := TGLIntegerList.Create;
          try
            for i := 0 to VertexIndices.Count - 3 do
              bufList.Add(vertexIndices[0], vertexIndices[i], vertexIndices[i + 1]);
            vertexIndices := bufList;
          finally
            bufList.Free;
          end;
          FMode := fgmmTriangles;
        end;
    end;
    InvalidateVBO;
  end;
end;

function TFGVertexIndexList.GetNormal: TAffineVector;
begin
  if VertexIndices.Count < 3 then
    Result := NullVector
  else
    with Owner.Owner.Vertices do
      CalcPlaneNormal(Items[VertexIndices[0]], Items[VertexIndices[1]],
        Items[VertexIndices[2]], Result);
end;

procedure TFGVertexIndexList.InvalidateVBO;
begin
  if Assigned(FIndexVBO) then
    FIndexVBO.NotifyChangesOfData;
end;

// ------------------
// ------------------ TFGVertexNormalTexIndexList ------------------
// ------------------

constructor TFGVertexNormalTexIndexList.Create;
begin
  inherited;
  FNormalIndices := TGLIntegerList.Create;
  FTexCoordIndices := TGLIntegerList.Create;
end;

destructor TFGVertexNormalTexIndexList.Destroy;
begin
  FTexCoordIndices.Free;
  FNormalIndices.Free;
  inherited;
end;

procedure TFGVertexNormalTexIndexList.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FNormalIndices.WriteToFiler(writer);
    FTexCoordIndices.WriteToFiler(writer);
  end;
end;

procedure TFGVertexNormalTexIndexList.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FNormalIndices.ReadFromFiler(reader);
      FTexCoordIndices.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TFGVertexNormalTexIndexList.SetNormalIndices(const val: TGLIntegerList);
begin
  FNormalIndices.Assign(val);
end;

procedure TFGVertexNormalTexIndexList.SetTexCoordIndices(const val: TGLIntegerList);
begin
  FTexCoordIndices.Assign(val);
end;

procedure TFGVertexNormalTexIndexList.BuildList(var mrci: TGLRenderContextInfo);
var
  i: Integer;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  texCoordPool: PAffineVectorArray;
  colorPool: PVectorArray;
  normalIdxList, texCoordIdxList, vertexIdxList: PIntegerVector;
begin
  Assert(((TexCoordIndices.Count = 0) or (VertexIndices.Count <= TexCoordIndices.Count))
    and ((NormalIndices.Count = 0) or (VertexIndices.Count <= NormalIndices.Count)));
  vertexPool := Owner.Owner.Vertices.List;
  normalPool := Owner.Owner.Normals.List;
  colorPool := Owner.Owner.Colors.List;
  texCoordPool := Owner.Owner.TexCoords.List;
  case Mode of
    fgmmTriangles, fgmmFlatTriangles: gl.Begin_(GL_TRIANGLES);
    fgmmTriangleStrip: gl.Begin_(GL_TRIANGLE_STRIP);
    fgmmTriangleFan: gl.Begin_(GL_TRIANGLE_FAN);
  else
    Assert(False);
  end;
  vertexIdxList := VertexIndices.List;
  if NormalIndices.Count > 0 then
    normalIdxList := NormalIndices.List
  else
    normalIdxList := vertexIdxList;
  if TexCoordIndices.Count > 0 then
    texCoordIdxList := TexCoordIndices.List
  else
    texCoordIdxList := vertexIdxList;

  for i := 0 to VertexIndices.Count - 1 do
  begin
    gl.Normal3fv(@normalPool[normalIdxList^[i]]);
    if Assigned(colorPool) then
      gl.Color4fv(@colorPool[vertexIdxList^[i]]);
    if Assigned(texCoordPool) then
      xgl.TexCoord2fv(@texCoordPool[texCoordIdxList^[i]]);
    gl.Vertex3fv(@vertexPool[vertexIdxList^[i]]);
  end;

  gl.End_;
end;

procedure TFGVertexNormalTexIndexList.AddToTriangles(aList: TGLAffineVectorList; aTexCoords: TGLAffineVectorList = nil;
  aNormals: TGLAffineVectorList = nil);
begin
  AddToList(Owner.Owner.Vertices, aList, VertexIndices);
  AddToList(Owner.Owner.TexCoords, aTexCoords, TexCoordIndices);
  AddToList(Owner.Owner.Normals, aNormals, NormalIndices);
end;

procedure TFGVertexNormalTexIndexList.Add(vertexIdx, normalIdx, texCoordIdx: Integer);
begin
  inherited Add(vertexIdx);
  FNormalIndices.Add(normalIdx);
  FTexCoordIndices.Add(texCoordIdx);
end;

// ------------------
// ------------------ TFGIndexTexCoordList ------------------
// ------------------

constructor TFGIndexTexCoordList.Create;
begin
  inherited;
  FTexCoords := TGLAffineVectorList.Create;
end;

destructor TFGIndexTexCoordList.Destroy;
begin
  FTexCoords.Free;
  inherited;
end;

procedure TFGIndexTexCoordList.WriteToFiler(writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FTexCoords.WriteToFiler(writer);
  end;
end;

procedure TFGIndexTexCoordList.ReadFromFiler(reader: TGLVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(reader);
  archiveVersion := reader.ReadInteger;
  if archiveVersion = 0 then
    with reader do
    begin
      FTexCoords.ReadFromFiler(reader);
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TFGIndexTexCoordList.SetTexCoords(const val: TGLAffineVectorList);
begin
  FTexCoords.Assign(val);
end;

procedure TFGIndexTexCoordList.BuildList(var mrci: TGLRenderContextInfo);
var
  i, k: Integer;
  texCoordPool: PAffineVectorArray;
  vertexPool: PAffineVectorArray;
  normalPool: PAffineVectorArray;
  indicesPool: PIntegerArray;
  colorPool: PVectorArray;
  gotColor: Boolean;

begin
  Assert(VertexIndices.Count = TexCoords.Count);
  texCoordPool := TexCoords.List;
  vertexPool := Owner.Owner.Vertices.List;
  indicesPool := @VertexIndices.List[0];
  colorPool := @Owner.Owner.Colors.List[0];
  gotColor := (Owner.Owner.Vertices.Count = Owner.Owner.Colors.Count);

  case Mode of
    fgmmTriangles: gl.Begin_(GL_TRIANGLES);
    fgmmFlatTriangles: gl.Begin_(GL_TRIANGLES);
    fgmmTriangleStrip: gl.Begin_(GL_TRIANGLE_STRIP);
    fgmmTriangleFan: gl.Begin_(GL_TRIANGLE_FAN);
    fgmmQuads: gl.Begin_(GL_QUADS);
  else
    Assert(False);
  end;
  if Owner.Owner.Normals.Count = Owner.Owner.Vertices.Count then
  begin
    normalPool := Owner.Owner.Normals.List;
    for i := 0 to VertexIndices.Count - 1 do
    begin
      xgl.TexCoord2fv(@texCoordPool[i]);
      k := indicesPool[i];
      if gotColor then
        gl.Color4fv(@colorPool[k]);
      gl.Normal3fv(@normalPool[k]);
      gl.Vertex3fv(@vertexPool[k]);
    end;
  end
  else
  begin
    for i := 0 to VertexIndices.Count - 1 do
    begin
      xgl.TexCoord2fv(@texCoordPool[i]);
      if gotColor then
        gl.Color4fv(@colorPool[indicesPool[i]]);
      gl.Vertex3fv(@vertexPool[indicesPool[i]]);
    end;
  end;
  gl.End_;
  gl.CheckError;
end;

procedure TFGIndexTexCoordList.AddToTriangles(aList: TGLAffineVectorList; aTexCoords: TGLAffineVectorList = nil;
  aNormals: TGLAffineVectorList = nil);
var
  i, n: Integer;
  texCoordList: TGLAffineVectorList;
begin
  AddToList(Owner.Owner.Vertices, aList, VertexIndices);
  AddToList(Owner.Owner.Normals, aNormals, VertexIndices);
  texCoordList := Self.TexCoords;
  case Mode of
    fgmmTriangles, fgmmFlatTriangles:
      begin
        if Assigned(aTexCoords) then
        begin
          n := (VertexIndices.Count div 3) * 3;
          aTexCoords.AdjustCapacityToAtLeast(aTexCoords.Count + n);
          for i := 0 to n - 1 do
            aTexCoords.Add(texCoordList[i]);
        end;
      end;
    fgmmTriangleStrip:
      begin
        if Assigned(aTexCoords) then
          ConvertStripToList(aTexCoords, texCoordList);
      end;
    fgmmTriangleFan:
      begin
        if Assigned(aTexCoords) then
        begin
          aTexCoords.AdjustCapacityToAtLeast(aTexCoords.Count + (VertexIndices.Count - 2) * 3);
          for i := 2 to VertexIndices.Count - 1 do
          begin
            aTexCoords.Add(texCoordList[0], texCoordList[i - 1], texCoordList[i]);
          end;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TFGIndexTexCoordList.Add(idx: Integer; const texCoord: TAffineVector);
begin
  TexCoords.Add(texCoord);
  inherited Add(idx);
end;

procedure TFGIndexTexCoordList.Add(idx: Integer; const s, t: Single);
begin
  TexCoords.Add(s, t, 0);
  inherited Add(idx);
end;

// ------------------
// ------------------ TGLFaceGroups ------------------
// ------------------

constructor TGLFaceGroups.CreateOwned(AOwner: TGLMeshObject);
begin
  FOwner := AOwner;
  Create;
end;

destructor TGLFaceGroups.Destroy;
begin
  Clear;
  inherited;
end;

procedure TGLFaceGroups.ReadFromFiler(reader: TGLVirtualReader);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Count - 1 do
    Items[i].FOwner := Self;
end;

procedure TGLFaceGroups.Clear;
var
  i: Integer;
  fg: TGLFaceGroup;
begin
  for i := 0 to Count - 1 do
  begin
    fg := GetFaceGroup(i);
    if Assigned(fg) then
    begin
      fg.FOwner := nil;
      fg.Free;
    end;
  end;
  inherited;
end;

function TGLFaceGroups.GetFaceGroup(Index: Integer): TGLFaceGroup;
begin
  Result := TGLFaceGroup(List^[Index]);
end;

procedure TGLFaceGroups.PrepareMaterialLibraryCache(matLib: TGLMaterialLibrary);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TGLFaceGroup(List^[i]).PrepareMaterialLibraryCache(matLib);
end;

procedure TGLFaceGroups.DropMaterialLibraryCache;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TGLFaceGroup(List^[i]).DropMaterialLibraryCache;
end;

procedure TGLFaceGroups.AddToTriangles(aList: TGLAffineVectorList; aTexCoords: TGLAffineVectorList = nil;
  aNormals: TGLAffineVectorList = nil);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].AddToTriangles(aList, aTexCoords, aNormals);
end;

function TGLFaceGroups.MaterialLibrary: TGLMaterialLibrary;
var
  mol: TGLMeshObjectList;
  bm: TGLBaseMesh;
begin
  if Assigned(Owner) then
  begin
    mol := Owner.Owner;
    if Assigned(mol) then
    begin
      bm := mol.Owner;
      if Assigned(bm) then
      begin
        Result := bm.MaterialLibrary;
        Exit;
      end;
    end;
  end;
  Result := nil;
end;

function CompareMaterials(item1, item2: TObject): Integer;

  function MaterialIsOpaque(fg: TGLFaceGroup): Boolean;
  var
    libMat: TGLLibMaterial;
  begin
    libMat := fg.MaterialCache;
    Result := (not Assigned(libMat)) or (not libMat.Material.Blended);
  end;

var
  fg1, fg2: TGLFaceGroup;
  opaque1, opaque2: Boolean;
begin
  fg1 := TGLFaceGroup(item1);
  opaque1 := MaterialIsOpaque(fg1);
  fg2 := TGLFaceGroup(item2);
  opaque2 := MaterialIsOpaque(fg2);
  if opaque1 = opaque2 then
  begin
    Result := CompareStr(fg1.MaterialName, fg2.MaterialName);
    if Result = 0 then
      Result := fg1.LightMapIndex - fg2.LightMapIndex;
  end
  else if opaque1 then
    Result := -1
  else
    Result := 1;
end;

procedure TGLFaceGroups.SortByMaterial;
begin
  PrepareMaterialLibraryCache(Owner.Owner.Owner.MaterialLibrary);
  Sort(@CompareMaterials);
end;

// ------------------
// ------------------ TGLVectorFile ------------------
// ------------------

constructor TGLVectorFile.Create(AOwner: TPersistent);
begin
  Assert(AOwner is TGLBaseMesh);
  inherited;
end;

function TGLVectorFile.Owner: TGLBaseMesh;
begin
  Result := TGLBaseMesh(GetOwner);
end;

procedure TGLVectorFile.SetNormalsOrientation(const val: TGLMeshNormalsOrientation);
begin
  FNormalsOrientation := val;
end;

// ------------------
// ------------------ TGLSMVectorFile ------------------
// ------------------

class function TGLSMVectorFile.Capabilities: TGLDataFileCapabilities;
begin
  Result := [dfcRead, dfcWrite];
end;

procedure TGLSMVectorFile.LoadFromStream(aStream: TStream);
begin
  Owner.MeshObjects.LoadFromStream(aStream);
end;

procedure TGLSMVectorFile.SaveToStream(aStream: TStream);
begin
  Owner.MeshObjects.SaveToStream(aStream);
end;

// ------------------
// ------------------ TGLBaseMesh ------------------
// ------------------

constructor TGLBaseMesh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if FMeshObjects = nil then
    FMeshObjects := TGLMeshObjectList.CreateOwned(Self);
  if FSkeleton = nil then
    FSkeleton := TGLSkeleton.CreateOwned(Self);
  FUseMeshMaterials := True;
  FAutoCentering := [];
  FAxisAlignedDimensionsCache.X := -1;
  FBaryCenterOffsetChanged := True;
  FAutoScaling := TGLCoordinates.CreateInitialized(Self, XYZWHmgVector, csPoint);
end;

destructor TGLBaseMesh.Destroy;
begin
  FConnectivity.Free;
  DropMaterialLibraryCache;
  FSkeleton.Free;
  FMeshObjects.Free;
  FAutoScaling.Free;
  inherited Destroy;
end;

procedure TGLBaseMesh.Assign(Source: TPersistent);
begin
  if Source is TGLBaseMesh then
  begin
    FSkeleton.Clear;
    FNormalsOrientation := TGLBaseMesh(Source).FNormalsOrientation;
    FMaterialLibrary := TGLBaseMesh(Source).FMaterialLibrary;
    FLightmapLibrary := TGLBaseMesh(Source).FLightmapLibrary;
    FAxisAlignedDimensionsCache :=  TGLBaseMesh(Source).FAxisAlignedDimensionsCache;
    FBaryCenterOffset := TGLBaseMesh(Source).FBaryCenterOffset;
    FUseMeshMaterials := TGLBaseMesh(Source).FUseMeshMaterials;
    FOverlaySkeleton := TGLBaseMesh(Source).FOverlaySkeleton;
    FIgnoreMissingTextures := TGLBaseMesh(Source).FIgnoreMissingTextures;
    FAutoCentering := TGLBaseMesh(Source).FAutoCentering;
    FAutoScaling.Assign(TGLBaseMesh(Source).FAutoScaling);
    FSkeleton.Assign(TGLBaseMesh(Source).FSkeleton);
    FSkeleton.RootBones.PrepareGlobalMatrices;
    FMeshObjects.Assign(TGLBaseMesh(Source).FMeshObjects);
  end;
  inherited Assign(Source);
end;

procedure TGLBaseMesh.LoadFromFile(const filename: string);
var
  fs: TFileStream;
begin
  FLastLoadedFilename := '';
  if fileName <> '' then
  begin
    try
      fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyWrite);
      LoadFromStream(fileName, fs);
      FLastLoadedFilename := filename;
    finally
      fs.Free;
    end;
  end;
end;

procedure TGLBaseMesh.LoadFromStream(const fileName: string; aStream: TStream);
var
  newVectorFile: TGLVectorFile;
  vectorFileClass: TGLVectorFileClass;
begin
  FLastLoadedFilename := '';
  if fileName <> '' then
  begin
    MeshObjects.Clear;
    Skeleton.Clear;
    vectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    try
      newVectorFile.ResourceName := filename;
      PrepareVectorFile(newVectorFile);
      if Assigned(Scene) then
        Scene.BeginUpdate;
      try
        newVectorFile.LoadFromStream(aStream);
        FLastLoadedFilename := filename;
      finally
        if Assigned(Scene) then
          Scene.EndUpdate;
      end;
    finally
      newVectorFile.Free;
    end;
    PerformAutoScaling;
    PerformAutoCentering;
    PrepareMesh;
  end;
end;

procedure TGLBaseMesh.SaveToFile(const filename: string);
var
  fs: TStream;
begin
  if fileName <> '' then
  begin
    try
      fs := TFileStream.Create(fileName, fmCreate);
      SaveToStream(fileName, fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TGLBaseMesh.SaveToStream(const fileName: string; aStream: TStream);
var
  newVectorFile: TGLVectorFile;
  vectorFileClass: TGLVectorFileClass;
begin
  if fileName <> '' then
  begin
    vectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    try
      newVectorFile.ResourceName := filename;
      PrepareVectorFile(newVectorFile);
      newVectorFile.SaveToStream(aStream);
    finally
      newVectorFile.Free;
    end;
  end;
end;

procedure TGLBaseMesh.AddDataFromFile(const filename: string);
var
  fs: TStream;
begin
  if fileName <> '' then
  begin
    fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyWrite);
    try
      AddDataFromStream(fileName, fs);
    finally
      fs.Free;
    end;
  end;
end;

procedure TGLBaseMesh.AddDataFromStream(const filename: string; aStream: TStream);
var
  newVectorFile: TGLVectorFile;
  VectorFileClass: TGLVectorFileClass;
begin
  if filename <> '' then
  begin
    VectorFileClass := GetVectorFileFormats.FindFromFileName(filename);
    newVectorFile := VectorFileClass.Create(Self);
    newVectorFile.ResourceName := filename;
    PrepareVectorFile(newVectorFile);
    try
      if Assigned(Scene) then
        Scene.BeginUpdate;
      newVectorFile.LoadFromStream(aStream);
      if Assigned(Scene) then
        Scene.EndUpdate;
    finally
      NewVectorFile.Free;
    end;
    PrepareMesh;
  end;
end;

procedure TGLBaseMesh.GetExtents(out min, max: TAffineVector);
var
  i, k: Integer;
  lMin, lMax: TAffineVector;
const
  cBigValue: Single = 1E50;
  cSmallValue: Single = -1E50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to MeshObjects.Count - 1 do
  begin
    TGLMeshObject(MeshObjects[i]).GetExtents(lMin, lMax);
    for k := 0 to 2 do
    begin
      if lMin.V[k] < min.V[k] then
        min.V[k] := lMin.V[k];
      if lMax.V[k] > max.V[k] then
        max.V[k] := lMax.V[k];
    end;
  end;
end;

function TGLBaseMesh.GetBarycenter: TAffineVector;
var
  i, nb: Integer;
begin
  Result := NullVector;
  nb := 0;
  for i := 0 to MeshObjects.Count - 1 do
    TGLMeshObject(MeshObjects[i]).ContributeToBarycenter(Result, nb);
  if nb > 0 then
    ScaleVector(Result, 1 / nb);
end;

function TGLBaseMesh.LastLoadedFilename: string;
begin
  Result := FLastLoadedFilename;
end;

procedure TGLBaseMesh.SetMaterialLibrary(const val: TGLMaterialLibrary);
begin
  if FMaterialLibrary <> val then
  begin
    if FMaterialLibraryCachesPrepared then
      DropMaterialLibraryCache;
    if Assigned(FMaterialLibrary) then
    begin
      DestroyHandle;
      FMaterialLibrary.RemoveFreeNotification(Self);
    end;
    FMaterialLibrary := val;
    if Assigned(FMaterialLibrary) then
      FMaterialLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TGLBaseMesh.SetLightmapLibrary(const val: TGLMaterialLibrary);
begin
  if FLightmapLibrary <> val then
  begin
    if Assigned(FLightmapLibrary) then
    begin
      DestroyHandle;
      FLightmapLibrary.RemoveFreeNotification(Self);
    end;
    FLightmapLibrary := val;
    if Assigned(FLightmapLibrary) then
      FLightmapLibrary.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TGLBaseMesh.SetNormalsOrientation(const val: TGLMeshNormalsOrientation);
begin
  if val <> FNormalsOrientation then
  begin
    FNormalsOrientation := val;
    StructureChanged;
  end;
end;

procedure TGLBaseMesh.SetOverlaySkeleton(const val: Boolean);
begin
  if FOverlaySkeleton <> val then
  begin
    FOverlaySkeleton := val;
    NotifyChange(Self);
  end;
end;

procedure TGLBaseMesh.SetAutoScaling(const Value: TGLCoordinates);
begin
  FAutoScaling.SetPoint(Value.DirectX, Value.DirectY, Value.DirectZ);
end;

procedure TGLBaseMesh.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FMaterialLibrary then
      MaterialLibrary := nil
    else if AComponent = FLightmapLibrary then
      LightmapLibrary := nil;
  end;
  inherited;
end;

function TGLBaseMesh.AxisAlignedDimensionsUnscaled: TGLVector;
var
  dMin, dMax: TAffineVector;
begin
  if FAxisAlignedDimensionsCache.X < 0 then
  begin
    MeshObjects.GetExtents(dMin, dMax);
    FAxisAlignedDimensionsCache.X := (dMax.X - dMin.X) / 2;
    FAxisAlignedDimensionsCache.Y := (dMax.Y - dMin.Y) / 2;
    FAxisAlignedDimensionsCache.Z := (dMax.Z - dMin.Z) / 2;
    FAxisAlignedDimensionsCache.W := 0;
  end;
  SetVector(Result, FAxisAlignedDimensionsCache);
end;

function TGLBaseMesh.BarycenterOffset: TGLVector;
var
  dMin, dMax: TAffineVector;
begin
  if FBaryCenterOffsetChanged then
  begin
    MeshObjects.GetExtents(dMin, dMax);

    FBaryCenterOffset.X := (dMin.X + dMax.X) / 2;
    FBaryCenterOffset.Y := (dMin.Y + dMax.Y) / 2;
    FBaryCenterOffset.Z := (dMin.Z + dMax.Z) / 2;
    FBaryCenterOffset.W := 0;
    FBaryCenterOffsetChanged := False;
  end;
  Result := FBaryCenterOffset;
end;

function TGLBaseMesh.BarycenterPosition: TGLVector;
begin
  Result := VectorAdd(Position.DirectVector, BarycenterOffset);
end;

function TGLBaseMesh.BarycenterAbsolutePosition: TGLVector;
begin
  Result := LocalToAbsolute(BarycenterPosition);
end;

procedure TGLBaseMesh.DestroyHandle;
begin
  if Assigned(FMaterialLibrary) then
    MaterialLibrary.DestroyHandles;
  if Assigned(FLightmapLibrary) then
    LightmapLibrary.DestroyHandles;
  inherited;
end;

procedure TGLBaseMesh.PrepareVectorFile(aFile: TGLVectorFile);
begin
  aFile.NormalsOrientation := NormalsOrientation;
end;

procedure TGLBaseMesh.PerformAutoCentering;
var
  delta, min, max: TAffineVector;
begin
  if macUseBarycenter in AutoCentering then
  begin
    delta := VectorNegate(GetBarycenter);
  end
  else
  begin
    GetExtents(min, max);
    if macCenterX in AutoCentering then
      delta.X := -0.5 * (min.X + max.X)
    else
      delta.X := 0;
    if macCenterY in AutoCentering then
      delta.Y := -0.5 * (min.Y + max.Y)
    else
      delta.Y := 0;
    if macCenterZ in AutoCentering then
      delta.Z := -0.5 * (min.Z + max.Z)
    else
      delta.Z := 0;
  end;
  MeshObjects.Translate(delta);

  if macRestorePosition in AutoCentering then
    Position.Translate(VectorNegate(delta));
end;

procedure TGLBaseMesh.PerformAutoScaling;
var
  i: Integer;
  vScal: TAffineFltVector;
begin
  if (FAutoScaling.DirectX <> 1) or (FAutoScaling.DirectY <> 1) or (FAutoScaling.DirectZ <> 1) then
  begin
    MakeVector(vScal, FAutoScaling.DirectX, FAutoScaling.DirectY, FAutoScaling.DirectZ);
    for i := 0 to MeshObjects.Count - 1 do
    begin
      MeshObjects[i].Vertices.Scale(vScal);
    end;
  end;
end;

procedure TGLBaseMesh.PrepareMesh;
begin
  StructureChanged;
end;

procedure TGLBaseMesh.PrepareMaterialLibraryCache;
begin
  if FMaterialLibraryCachesPrepared then
    DropMaterialLibraryCache;
  MeshObjects.PrepareMaterialLibraryCache(FMaterialLibrary);
  FMaterialLibraryCachesPrepared := True;
end;

procedure TGLBaseMesh.DropMaterialLibraryCache;
begin
  if FMaterialLibraryCachesPrepared then
  begin
    MeshObjects.DropMaterialLibraryCache;
    FMaterialLibraryCachesPrepared := False;
  end;
end;

procedure TGLBaseMesh.PrepareBuildList(var mrci: TGLRenderContextInfo);
begin
  MeshObjects.PrepareBuildList(mrci);
  if LightmapLibrary <> nil then
    LightmapLibrary.Materials.PrepareBuildList
end;

procedure TGLBaseMesh.SetUseMeshMaterials(const val: Boolean);
begin
  if val <> FUseMeshMaterials then
  begin
    FUseMeshMaterials := val;
    if FMaterialLibraryCachesPrepared and (not val) then
      DropMaterialLibraryCache;
    StructureChanged;
  end;
end;

procedure TGLBaseMesh.BuildList(var rci: TGLRenderContextInfo);
begin
  MeshObjects.BuildList(rci);
end;

procedure TGLBaseMesh.DoRender(var rci: TGLRenderContextInfo; renderSelf, renderChildren: Boolean);
begin
  if Assigned(LightmapLibrary) then
    xgl.ForbidSecondTextureUnit;
  if renderSelf then
  begin
    // set winding
    case FNormalsOrientation of
      mnoDefault:  ; // nothing
      mnoInvert:  rci.GLStates.InvertGLFrontFace;
    else
      Assert(False);
    end;
    if not rci.ignoreMaterials then
    begin
      if UseMeshMaterials and Assigned(MaterialLibrary) then
      begin
        rci.MaterialLibrary := MaterialLibrary;
        if not FMaterialLibraryCachesPrepared then
          PrepareMaterialLibraryCache;
      end
      else
        rci.MaterialLibrary := nil;
      if Assigned(LightmapLibrary) then
        rci.LightmapLibrary := LightmapLibrary
      else
        rci.LightmapLibrary := nil;
      if rci.amalgamating or not(ListHandleAllocated or (osDirectDraw in ObjectStyle)) then
        PrepareBuildList(rci);
      Material.Apply(rci);
      repeat
        if (osDirectDraw in ObjectStyle) or
		    rci.amalgamating or UseMeshMaterials then
          BuildList(rci)
        else
          rci.GLStates.CallList(GetHandle(rci));
      until not Material.UnApply(rci);
      rci.MaterialLibrary := nil;
    end
    else
    begin
      if (osDirectDraw in ObjectStyle) or rci.amalgamating then
        BuildList(rci)
      else
        rci.GLStates.CallList(GetHandle(rci));
    end;
    if FNormalsOrientation <> mnoDefault then
      rci.GLStates.InvertGLFrontFace;
  end;
  if Assigned(LightmapLibrary) then
    xgl.AllowSecondTextureUnit;
  if renderChildren and (Count > 0) then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TGLBaseMesh.StructureChanged;
begin
  FAxisAlignedDimensionsCache.X := -1;
  FBaryCenterOffsetChanged := True;
  DropMaterialLibraryCache;
  MeshObjects.Prepare;
  inherited;
end;

procedure TGLBaseMesh.StructureChangedNoPrepare;
begin
  inherited StructureChanged;
end;

function TGLBaseMesh.RayCastIntersect(const rayStart, rayVector: TGLVector; intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): Boolean;

var
  i,j: Integer;
  Obj: TGLMeshObject;
  Tris: TGLAffineVectorList;
  locRayStart, locRayVector, iPoint, iNormal: TGLVector;
  d, minD: Single;

begin
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  minD := -1;

  for j := 0 to MeshObjects.Count - 1 do
  begin
    Obj := MeshObjects.GetMeshObject(j);
    if not Obj.Visible then
      Continue;
    Tris := Obj.ExtractTriangles(NIL, NIL); //objTexCoords & objNormals
    try
      i := 0;
      while i < Tris.Count do
      begin
        if RayCastTriangleIntersect(locRayStart, locRayVector, Tris.List^[i],
          Tris.List^[i + 1], Tris.List^[i + 2], @iPoint, @iNormal) then
        begin
          d := VectorDistance2(locRayStart, iPoint);
          if (d < minD) or (minD < 0) then
          begin
            minD := d;
            if intersectPoint <> nil then
              intersectPoint^ := iPoint;
            if intersectNormal <> nil then
              intersectNormal^ := iNormal;
          end;
        end;
        Inc(i, 3);
      end;
    finally
      Tris.Free;
    end;
  end;

  Result := (minD >= 0);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

function TGLBaseMesh.GenerateSilhouette(const silhouetteParameters: TGLSilhouetteParameters): TGLSilhouette;
var
  mc: TGLBaseMeshConnectivity;
  sil: TGLSilhouette;
begin
  sil := nil;
  if Assigned(FConnectivity) then
  begin
    mc := TGLBaseMeshConnectivity(FConnectivity);
    mc.CreateSilhouette(silhouetteParameters, sil, True);
  end
  else
  begin
    mc := TGLBaseMeshConnectivity.CreateFromMesh(Self);
    try
      mc.CreateSilhouette(silhouetteParameters, sil, True);
    finally
      mc.Free;
    end;
  end;
  Result := sil;
end;

procedure TGLBaseMesh.BuildSilhouetteConnectivityData;
var
  i, j: Integer;
  mo: TGLMeshObject;
begin
  FreeAndNil(FConnectivity);
  // connectivity data works only on facegroups of TFGVertexIndexList class
  for i := 0 to MeshObjects.Count - 1 do
  begin
    mo := (MeshObjects[i] as TGLMeshObject);
    if mo.Mode <> momFaceGroups then
      Exit;
    for j := 0 to mo.FaceGroups.Count - 1 do
      if not mo.FaceGroups[j].InheritsFrom(TFGVertexIndexList) then
        Exit;
  end;
  FConnectivity := TGLBaseMeshConnectivity.CreateFromMesh(Self);
end;

// ------------------
// ------------------ TGLFreeForm ------------------
// ------------------

constructor TGLFreeForm.Create(aOwner: TComponent);
begin
  inherited;
  // ObjectStyle := [osDirectDraw];
  FUseMeshMaterials := True;
end;

destructor TGLFreeForm.Destroy;
begin
  FOctree.Free;
  inherited Destroy;
end;

procedure TGLFreeForm.BuildOctree(TreeDepth: Integer = 3);
var
  emin, emax: TAffineVector;
  tl: TGLAffineVectorList;
begin
  if not Assigned(FOctree) then // moved here from GetOctree
    FOctree := TGLOctree.Create;

  GetExtents(emin, emax);
  tl := MeshObjects.ExtractTriangles;
  try
    with Octree do
    begin
      DisposeTree;
      InitializeTree(emin, emax, tl, TreeDepth);
    end;
  finally
    tl.Free;
  end;
end;

function TGLFreeForm.OctreeRayCastIntersect(const rayStart, rayVector: TGLVector; intersectPoint: PGLVector = nil;
  intersectNormal: PGLVector = nil): Boolean;
var
  locRayStart, locRayVector: TGLVector;
begin
  Assert(Assigned(FOctree), strOctreeMustBePreparedBeforeUse);
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  Result := Octree.RayCastIntersect(locRayStart, locRayVector, intersectPoint, intersectNormal);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

function TGLFreeForm.OctreePointInMesh(const Point: TGLVector): Boolean;
const
  cPointRadiusStep = 10000;
var
  rayStart, rayVector, hitPoint, hitNormal: TGLVector;
  BRad: double;
  HitCount: Integer;
  hitDot: double;
begin
  Assert(Assigned(FOctree), strOctreeMustBePreparedBeforeUse);
  Result := False;

  // Makes calculations sligthly faster by ignoring cases that are guaranteed
  // to be outside the object
  if not PointInObject(Point) then
    Exit;

  BRad := BoundingSphereRadius;

  // This could be a fixed vector, but a fixed vector could have a systemic
  // bug on an non-closed mesh, making it fail constantly for one or several
  // faces.
  rayVector := VectorMake(2 * random - 1, 2 * random - 1, 2 * random - 1);
  rayStart := VectorAdd(VectorScale(rayVector, -BRad), Point);

  HitCount := 0;

  while OctreeRayCastIntersect(rayStart, rayVector, @hitPoint, @hitNormal) do
  begin
    // Are we past our taget?
    if VectorDotProduct(rayVector, VectorSubtract(Point, hitPoint)) < 0 then
    begin
      Result := HitCount > 0;
      Exit;
    end;

    hitDot := VectorDotProduct(hitNormal, rayVector);
    if hitDot < 0 then
      Inc(HitCount)
    else if hitDot > 0 then
      Dec(HitCount);

    // ditDot = 0 is a tricky special case where the ray is just grazing the
    // side of a face - this case means that it doesn't necessarily actually
    // enter the mesh - but it _could_ enter the mesh. If this situation occurs,
    // we should restart the run using a new rayVector - but this implementation
    // currently doesn't.

    // Restart the ray slightly beyond the point it hit the previous face. Note
    // that this step introduces a possible issue with faces that are very close
    rayStart := VectorAdd(hitPoint, VectorScale(rayVector, BRad / cPointRadiusStep));
  end;
end;

function TGLFreeForm.OctreeSphereSweepIntersect(const rayStart, rayVector: TGLVector; const velocity, radius: Single;
  intersectPoint: PGLVector = nil; intersectNormal: PGLVector = nil): Boolean;
var
  locRayStart, locRayVector: TGLVector;
begin
  Assert(Assigned(FOctree), strOctreeMustBePreparedBeforeUse);
  SetVector(locRayStart, AbsoluteToLocal(rayStart));
  SetVector(locRayVector, AbsoluteToLocal(rayVector));
  Result := Octree.SphereSweepIntersect(locRayStart, locRayVector, velocity, radius, intersectPoint, intersectNormal);
  if Result then
  begin
    if intersectPoint <> nil then
      SetVector(intersectPoint^, LocalToAbsolute(intersectPoint^));
    if intersectNormal <> nil then
    begin
      SetVector(intersectNormal^, LocalToAbsolute(intersectNormal^));
      if NormalsOrientation = mnoInvert then
        NegateVector(intersectNormal^);
    end;
  end;
end;

function TGLFreeForm.OctreeTriangleIntersect(const v1, v2, v3: TAffineVector): Boolean;
var
  t1, t2, t3: TAffineVector;
begin
  Assert(Assigned(FOctree), strOctreeMustBePreparedBeforeUse);
  SetVector(t1, AbsoluteToLocal(v1));
  SetVector(t2, AbsoluteToLocal(v2));
  SetVector(t3, AbsoluteToLocal(v3));

  Result := Octree.TriangleIntersect(t1, t2, t3);
end;

function TGLFreeForm.OctreeAABBIntersect(const AABB: TAABB; objMatrix, invObjMatrix: TGLMatrix;
  triangles: TGLAffineVectorList = nil): Boolean;
var
  m1to2, m2to1: TGLMatrix;
begin
  Assert(Assigned(FOctree), strOctreeMustBePreparedBeforeUse);

  // get matrixes needed
  // object to self
  MatrixMultiply(objMatrix, InvAbsoluteMatrix, m1to2);
  // self to object
  MatrixMultiply(AbsoluteMatrix, invObjMatrix, m2to1);

  Result := Octree.AABBIntersect(aabb, m1to2, m2to1, triangles);
end;

// ------------------
// ------------------ TGLActorAnimation ------------------
// ------------------

constructor TGLActorAnimation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TGLActorAnimation.Destroy;
begin
  with (Collection as TGLActorAnimations).FOwner do
    if FTargetSmoothAnimation = Self then
      FTargetSmoothAnimation := nil;
  inherited Destroy;
end;

procedure TGLActorAnimation.Assign(Source: TPersistent);
begin
  if Source is TGLActorAnimation then
  begin
    FName := TGLActorAnimation(Source).FName;
    FStartFrame := TGLActorAnimation(Source).FStartFrame;
    FEndFrame := TGLActorAnimation(Source).FEndFrame;
    FReference := TGLActorAnimation(Source).FReference;
  end
  else
    inherited;
end;

function TGLActorAnimation.GetDisplayName: string;
begin
  Result := Format('%d - %s [%d - %d]', [Index, Name, StartFrame, EndFrame]);
end;

function TGLActorAnimation.FrameCount: Integer;
begin
  case Reference of
    aarMorph: Result := TGLActorAnimations(Collection).FOwner.MeshObjects.MorphTargetCount;
    aarSkeleton: Result := TGLActorAnimations(Collection).FOwner.Skeleton.Frames.Count;
  else
    Result := 0;
    Assert(False);
  end;
end;

procedure TGLActorAnimation.SetStartFrame(const val: Integer);
var
  m: Integer;
begin
  if val < 0 then
    FStartFrame := 0
  else
  begin
    m := FrameCount;
    if val >= m then
      FStartFrame := m - 1
    else
      FStartFrame := val;
  end;
  if FStartFrame > FEndFrame then
    FEndFrame := FStartFrame;
end;

procedure TGLActorAnimation.SetEndFrame(const val: Integer);
var
  m: Integer;
begin
  if val < 0 then
    FEndFrame := 0
  else
  begin
    m := FrameCount;
    if val >= m then
      FEndFrame := m - 1
    else
      FEndFrame := val;
  end;
  if FStartFrame > FEndFrame then
    FStartFrame := FEndFrame;
end;

procedure TGLActorAnimation.SetReference(val: TGLActorAnimationReference);
begin
  if val <> FReference then
  begin
    FReference := val;
    StartFrame := StartFrame;
    EndFrame := EndFrame;
  end;
end;

procedure TGLActorAnimation.SetAsString(const val: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.CommaText := val;
    Assert(sl.Count >= 3);
    FName := sl[0];
    FStartFrame := StrToInt(sl[1]);
    FEndFrame := StrToInt(sl[2]);
    if sl.Count = 4 then
    begin
      if LowerCase(sl[3]) = 'morph' then
        Reference := aarMorph
      else if LowerCase(sl[3]) = 'skeleton' then
        Reference := aarSkeleton
      else
        Assert(False);
    end
    else
      Reference := aarMorph;
  finally
    sl.Free;
  end;
end;

function TGLActorAnimation.GetAsString: string;
const
  cAARToString: array [aarMorph .. aarSkeleton] of string = ('morph', 'skeleton');
begin
  Result := Format('"%s",%d,%d,%s', [FName, FStartFrame, FEndFrame, cAARToString[reference]]);
end;

function TGLActorAnimation.OwnerActor: TGLActor;
begin
  Result := ((Collection as TGLActorAnimations).GetOwner as TGLActor);
end;

procedure TGLActorAnimation.MakeSkeletalTranslationStatic;
begin
  OwnerActor.Skeleton.MakeSkeletalTranslationStatic(StartFrame, EndFrame);
end;

procedure TGLActorAnimation.MakeSkeletalRotationDelta;
begin
  OwnerActor.Skeleton.MakeSkeletalRotationDelta(StartFrame, EndFrame);
end;

// ------------------
// ------------------ TGLActorAnimations ------------------
// ------------------

constructor TGLActorAnimations.Create(AOwner: TGLActor);
begin
  FOwner := AOwner;
  inherited Create(TGLActorAnimation);
end;

function TGLActorAnimations.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGLActorAnimations.SetItems(Index: Integer; const val: TGLActorAnimation);
begin
  inherited Items[index] := val;
end;

function TGLActorAnimations.GetItems(Index: Integer): TGLActorAnimation;
begin
  Result := TGLActorAnimation(inherited Items[index]);
end;

function TGLActorAnimations.Last: TGLActorAnimation;
begin
  if Count > 0 then
    Result := TGLActorAnimation(inherited Items[Count - 1])
  else
    Result := nil;
end;

function TGLActorAnimations.Add: TGLActorAnimation;
begin
  Result := (inherited Add) as TGLActorAnimation;
end;

function TGLActorAnimations.FindItemID(ID: Integer): TGLActorAnimation;
begin
  Result := (inherited FindItemID(ID)) as TGLActorAnimation;
end;

function TGLActorAnimations.FindName(const aName: string): TGLActorAnimation;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if CompareText(Items[i].Name, aName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
end;

function TGLActorAnimations.FindFrame(aFrame: Integer; aReference: TGLActorAnimationReference): TGLActorAnimation;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    with Items[i] do
      if (StartFrame <= aFrame) and (EndFrame >= aFrame) and (Reference = aReference) then
      begin
        Result := Items[i];
        Break;
      end;
end;

procedure TGLActorAnimations.SetToStrings(aStrings: TStrings);

var
  i: Integer;
begin
  with aStrings do
  begin
    BeginUpdate;
    Clear;
    for i := 0 to Self.Count - 1 do
      Add(Self.Items[i].Name);
    EndUpdate;
  end;
end;

procedure TGLActorAnimations.SaveToStream(aStream: TStream);
var
  i: Integer;
begin
  WriteCRLFString(aStream, cAAFHeader);
  WriteCRLFString(aStream, IntToStr(Count));
  for i := 0 to Count - 1 do
    WriteCRLFString(aStream, Items[i].AsString);
end;

procedure TGLActorAnimations.LoadFromStream(aStream: TStream);
var
  i, n: Integer;
begin
  Clear;
  if ReadCRLFString(aStream) <> cAAFHeader then
    Assert(False);
  n := StrToInt(ReadCRLFString(aStream));
  for i := 0 to n - 1 do
    Add.AsString := ReadCRLFString(aStream);
end;

procedure TGLActorAnimations.SaveToFile(const fileName: string);
var
  fs: TStream;
begin
  fs := TFileStream.Create(fileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TGLActorAnimations.LoadFromFile(const fileName: string);
var
  fs: TStream;
begin
  try
    fs := TFileStream.Create(fileName, fmOpenRead + fmShareDenyWrite);
  finally
    fs.Free;
  end;
end;

// ------------------
// ------------------ TGLBaseAnimationControler ------------------
// ------------------

constructor TGLBaseAnimationControler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
end;

destructor TGLBaseAnimationControler.Destroy;
begin
  SetActor(nil);
  inherited Destroy;
end;

procedure TGLBaseAnimationControler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (AComponent = FActor) and (Operation = opRemove) then
    SetActor(nil);
  inherited;
end;

procedure TGLBaseAnimationControler.DoChange;
begin
  if Assigned(FActor) then
    FActor.NotifyChange(Self);
end;

procedure TGLBaseAnimationControler.SetEnabled(const val: Boolean);
begin
  if val <> FEnabled then
  begin
    FEnabled := val;
    if Assigned(FActor) then
      DoChange;
  end;
end;

procedure TGLBaseAnimationControler.SetActor(const val: TGLActor);
begin
  if FActor <> val then
  begin
    if Assigned(FActor) then
      FActor.UnRegisterControler(Self);
    FActor := val;
    if Assigned(FActor) then
    begin
      FActor.RegisterControler(Self);
      DoChange;
    end;
  end;
end;

function TGLBaseAnimationControler.Apply(var lerpInfo: TGLBlendedLerpInfo): Boolean;
begin
  // virtual
  Result := False;
end;

// ------------------
// ------------------ TGLAnimationControler ------------------
// ------------------

procedure TGLAnimationControler.DoChange;
begin
  if AnimationName <> '' then
    inherited;
end;

procedure TGLAnimationControler.SetAnimationName(const val: TGLActorAnimationName);
begin
  if FAnimationName <> val then
  begin
    FAnimationName := val;
    DoChange;
  end;
end;

procedure TGLAnimationControler.SetRatio(const val: Single);
begin
  if FRatio <> val then
  begin
    FRatio := ClampValue(val, 0, 1);
    DoChange;
  end;
end;

function TGLAnimationControler.Apply(var lerpInfo: TGLBlendedLerpInfo): Boolean;
var
  anim: TGLActorAnimation;
  baseDelta: Integer;
begin
  if not Enabled then
  begin
    Result := False;
    Exit;
  end;

  anim := Actor.Animations.FindName(AnimationName);
  Result := (anim <> nil);
  if not Result then
    Exit;

  with lerpInfo do
  begin
    if Ratio = 0 then
    begin
      frameIndex1 := anim.StartFrame;
      frameIndex2 := frameIndex1;
      lerpFactor := 0;
    end
    else if Ratio = 1 then
    begin
      frameIndex1 := anim.EndFrame;
      frameIndex2 := frameIndex1;
      lerpFactor := 0;
    end
    else
    begin
      baseDelta := anim.EndFrame - anim.StartFrame;
      lerpFactor := anim.StartFrame + baseDelta * Ratio;
      frameIndex1 := Trunc(lerpFactor);
      frameIndex2 := frameIndex1 + 1;
      lerpFactor := Frac(lerpFactor);
    end;
    weight := 1;
    externalRotations := nil;
    externalQuaternions := nil;
  end;
end;

// ------------------
// ------------------ TGLActor ------------------
// ------------------

constructor TGLActor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ObjectStyle := ObjectStyle + [osDirectDraw];
  FFrameInterpolation := afpLinear;
  FAnimationMode := aamNone;
  FInterval := 100; // 10 animation frames per second
  FAnimations := TGLActorAnimations.Create(Self);
  FControlers := nil; // created on request
  FOptions := cDefaultGLActorOptions;
end;

destructor TGLActor.Destroy;
begin
  inherited Destroy;
  FControlers.Free;
  FAnimations.Free;
end;

procedure TGLActor.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TGLActor then
  begin
    FAnimations.Assign(TGLActor(Source).FAnimations);
    FAnimationMode := TGLActor(Source).FAnimationMode;
    Synchronize(TGLActor(Source));
  end;
end;

procedure TGLActor.RegisterControler(aControler: TGLBaseAnimationControler);
begin
  if not Assigned(FControlers) then
    FControlers := TList.Create;
  FControlers.Add(aControler);
  FreeNotification(aControler);
end;

procedure TGLActor.UnRegisterControler(aControler: TGLBaseAnimationControler);
begin
  Assert(Assigned(FControlers));
  FControlers.Remove(aControler);
  RemoveFreeNotification(aControler);
  if FControlers.Count = 0 then
    FreeAndNil(FControlers);
end;

procedure TGLActor.SetCurrentFrame(val: Integer);
begin
  if val <> CurrentFrame then
  begin
    if val > FrameCount - 1 then
      FCurrentFrame := FrameCount - 1
    else if val < 0 then
      FCurrentFrame := 0
    else
      FCurrentFrame := val;
    FCurrentFrameDelta := 0;
    case AnimationMode of
      aamPlayOnce: if (CurrentFrame = EndFrame) and (FTargetSmoothAnimation =
        nil) then
          FAnimationMode := aamNone;
      aamBounceForward: if CurrentFrame = EndFrame then
          FAnimationMode := aamBounceBackward;
      aamBounceBackward: if CurrentFrame = StartFrame then
          FAnimationMode := aamBounceForward;
    end;
    StructureChanged;
    if Assigned(FOnFrameChanged) then
      FOnFrameChanged(Self);
  end;
end;

procedure TGLActor.SetCurrentFrameDirect(const Value: Integer);
begin
  FCurrentFrame := Value;
end;

procedure TGLActor.SetStartFrame(val: Integer);
begin
  if (val >= 0) and (val < FrameCount) and (val <> StartFrame) then
    FStartFrame := val;
  if EndFrame < StartFrame then
    FEndFrame := FStartFrame;
  if CurrentFrame < StartFrame then
    CurrentFrame := FStartFrame;
end;

procedure TGLActor.SetEndFrame(val: Integer);
begin
  if (val >= 0) and (val < FrameCount) and (val <> EndFrame) then
    FEndFrame := val;
  if CurrentFrame > EndFrame then
    CurrentFrame := FEndFrame;
end;

procedure TGLActor.SetReference(val: TGLActorAnimationReference);
begin
  if val <> Reference then
  begin
    FReference := val;
    StartFrame := StartFrame;
    EndFrame := EndFrame;
    CurrentFrame := CurrentFrame;
    StructureChanged;
  end;
end;

procedure TGLActor.SetAnimations(const val: TGLActorAnimations);
begin
  FAnimations.Assign(val);
end;

function TGLActor.StoreAnimations: Boolean;
begin
  Result := (FAnimations.Count > 0);
end;

procedure TGLActor.SetOptions(const val: TGLActorOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    StructureChanged;
  end;
end;

function TGLActor.NextFrameIndex: Integer;
begin
  case AnimationMode of
    aamLoop, aamBounceForward:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.StartFrame
        else
        begin
          Result := CurrentFrame + 1;
          if Result > EndFrame then
          begin
            Result := StartFrame + (Result - EndFrame - 1);
            if Result > EndFrame then
              Result := EndFrame;
          end;
        end;
      end;
    aamNone, aamPlayOnce:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.StartFrame
        else
        begin
          Result := CurrentFrame + 1;
          if Result > EndFrame then
            Result := EndFrame;
        end;
      end;
    aamBounceBackward, aamLoopBackward:
      begin
        if FTargetSmoothAnimation <> nil then
          Result := FTargetSmoothAnimation.StartFrame
        else
        begin
          Result := CurrentFrame - 1;
          if Result < StartFrame then
          begin
            Result := EndFrame - (StartFrame - Result - 1);
            if Result < StartFrame then
              Result := StartFrame;
          end;
        end;
      end;
    aamExternal: Result := CurrentFrame; // Do nothing
  else
    Result := CurrentFrame;
    Assert(False);
  end;
end;

procedure TGLActor.NextFrame(nbSteps: Integer = 1);
var
  n: Integer;
begin
  n := nbSteps;
  while n > 0 do
  begin
    CurrentFrame := NextFrameIndex;
    Dec(n);
    if Assigned(FOnEndFrameReached) and (CurrentFrame = EndFrame) then
      FOnEndFrameReached(Self);
    if Assigned(FOnStartFrameReached) and (CurrentFrame = StartFrame) then
      FOnStartFrameReached(Self);
  end;
end;

procedure TGLActor.PrevFrame(nbSteps: Integer = 1);
var
  Value: Integer;
begin
  Value := FCurrentFrame - nbSteps;
  if Value < FStartFrame then
  begin
    Value := FEndFrame - (FStartFrame - Value);
    if Value < FStartFrame then
      Value := FStartFrame;
  end;
  CurrentFrame := Value;
end;

procedure TGLActor.DoAnimate();
var
  i, k: Integer;
  nextFrameIdx: Integer;
  lerpInfos: array of TGLBlendedLerpInfo;
begin
  nextFrameIdx := NextFrameIndex;
  case Reference of
    aarMorph: if nextFrameIdx >= 0 then
      begin
        case FrameInterpolation of
          afpLinear:
            MeshObjects.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta)
        else
          MeshObjects.MorphTo(CurrentFrame);
        end;
      end;
    aarSkeleton: if Skeleton.Frames.Count > 0 then
      begin
        if Assigned(FControlers) and (AnimationMode <> aamExternal) then
        begin
          // Blended Skeletal Lerping
          SetLength(lerpInfos, FControlers.Count + 1);
          if nextFrameIdx >= 0 then
          begin
            case FrameInterpolation of
              afpLinear: with lerpInfos[0] do
                begin
                  frameIndex1 := CurrentFrame;
                  frameIndex2 := nextFrameIdx;
                  lerpFactor := CurrentFrameDelta;
                  weight := 1;
                end;
            else
              with lerpInfos[0] do
              begin
                frameIndex1 := CurrentFrame;
                frameIndex2 := CurrentFrame;
                lerpFactor := 0;
                weight := 1;
              end;
            end;
          end
          else
          begin
            with lerpInfos[0] do
            begin
              frameIndex1 := CurrentFrame;
              frameIndex2 := CurrentFrame;
              lerpFactor := 0;
              weight := 1;
            end;
          end;
          k := 1;
          for i := 0 to FControlers.Count - 1 do
            if TGLBaseAnimationControler(FControlers[i]).Apply(lerpInfos[k])
              then
              Inc(k);
          SetLength(lerpInfos, k);
          Skeleton.BlendedLerps(lerpInfos);
        end
        else if (nextFrameIdx >= 0) and (AnimationMode <> aamExternal) then
        begin
          // Single Skeletal Lerp
          case FrameInterpolation of
            afpLinear:
              Skeleton.Lerp(CurrentFrame, nextFrameIdx, CurrentFrameDelta);
          else
            Skeleton.SetCurrentFrame(Skeleton.Frames[CurrentFrame]);
          end;
        end;
        Skeleton.MorphMesh(aoSkeletonNormalizeNormals in Options);
      end;
    aarNone: ; // do nothing
  end;
end;

procedure TGLActor.BuildList(var rci: TGLRenderContextInfo);
begin
  DoAnimate;
  inherited;
  if OverlaySkeleton then
  begin
    rci.GLStates.Disable(stDepthTest);
    Skeleton.RootBones.BuildList(rci);
  end;
end;

procedure TGLActor.PrepareMesh;
begin
  FStartFrame := 0;
  FEndFrame := FrameCount - 1;
  FCurrentFrame := 0;
  if Assigned(FOnFrameChanged) then
    FOnFrameChanged(Self);
  inherited;
end;

procedure TGLActor.PrepareBuildList(var mrci: TGLRenderContextInfo);
begin
  // no preparation needed for actors, they don't use buildlists
end;

function TGLActor.FrameCount: Integer;
begin
  case Reference of
    aarMorph:
      Result := MeshObjects.MorphTargetCount;
    aarSkeleton:
      Result := Skeleton.Frames.Count;
    aarNone:
      Result := 0;
  else
    Result := 0;
    Assert(False);
  end;
end;

procedure TGLActor.DoProgress(const progressTime: TGLProgressTimes);
var
  fDelta: Single;
begin
  inherited;
  if (AnimationMode <> aamNone) and (Interval > 0) then
  begin
    if (StartFrame <> EndFrame) and (FrameCount > 1) then
    begin
      FCurrentFrameDelta := FCurrentFrameDelta + (progressTime.deltaTime * 1000) / FInterval;
      if FCurrentFrameDelta > 1 then
      begin
        if Assigned(FTargetSmoothAnimation) then
        begin
          SwitchToAnimation(FTargetSmoothAnimation);
          FTargetSmoothAnimation := nil;
        end;
        // we need to step on
        fDelta := Frac(FCurrentFrameDelta);
        NextFrame(Trunc(FCurrentFrameDelta));
        FCurrentFrameDelta := fDelta;
        StructureChanged;
      end
      else if FrameInterpolation <> afpNone then
        StructureChanged;
    end;
  end;
end;

procedure TGLActor.LoadFromStream(const FileName: string; aStream: TStream);
begin
  if FileName <> '' then
  begin
    Animations.Clear;
    inherited LoadFromStream(FileName, aStream);
  end;
end;

procedure TGLActor.SwitchToAnimation(const AnimationName: string; smooth: Boolean = False);
begin
  SwitchToAnimation(Animations.FindName(AnimationName), smooth);
end;

procedure TGLActor.SwitchToAnimation(animationIndex: Integer; smooth: Boolean = False);
begin
  if (animationIndex >= 0) and (animationIndex < Animations.Count) then
    SwitchToAnimation(Animations[animationIndex], smooth);
end;

procedure TGLActor.SwitchToAnimation(anAnimation: TGLActorAnimation; smooth: Boolean = False);
begin
  if Assigned(anAnimation) then
  begin
    if smooth then
    begin
      FTargetSmoothAnimation := anAnimation;
      FCurrentFrameDelta := 0;
    end
    else
    begin
      Reference := anAnimation.Reference;
      StartFrame := anAnimation.StartFrame;
      EndFrame := anAnimation.EndFrame;
      CurrentFrame := StartFrame;
    end;
  end;
end;

function TGLActor.CurrentAnimation: string;
var
  aa: TGLActorAnimation;
begin
  aa := Animations.FindFrame(CurrentFrame, Reference);
  if Assigned(aa) then
    Result := aa.Name
  else
    Result := '';
end;

procedure TGLActor.Synchronize(referenceActor: TGLActor);
begin
  if Assigned(referenceActor) then
  begin
    if referenceActor.StartFrame < FrameCount then
      FStartFrame := referenceActor.StartFrame;
    if referenceActor.EndFrame < FrameCount then
      FEndFrame := referenceActor.EndFrame;
    FReference := referenceActor.Reference;
    if referenceActor.CurrentFrame < FrameCount then
      FCurrentFrame := referenceActor.CurrentFrame;
    FCurrentFrameDelta := referenceActor.CurrentFrameDelta;
    FAnimationMode := referenceActor.AnimationMode;
    FFrameInterpolation := referenceActor.FrameInterpolation;
    if referenceActor.FTargetSmoothAnimation <> nil then
      FTargetSmoothAnimation := Animations.FindName(referenceActor.FTargetSmoothAnimation.Name)
    else
      FTargetSmoothAnimation := nil;
    if (Skeleton.Frames.Count > 0) and (referenceActor.Skeleton.Frames.Count > 0) then
      Skeleton.Synchronize(referenceActor.Skeleton);
  end;
end;

function TGLActor.isSwitchingAnimation: boolean;
begin
  result := FTargetSmoothAnimation <> nil;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterVectorFileFormat('glsm', 'GLScene Mesh', TGLSMVectorFile);

  RegisterClasses(
    [TGLFreeForm, TGLActor, TGLSkeleton, TGLSkeletonFrame, TGLSkeletonBone,
    TGLSkeletonMeshObject, TGLMeshObject, TGLSkeletonFrameList, TGLMeshMorphTarget,
    TGLMorphableMeshObject, TGLFaceGroup, TFGVertexIndexList,
    TFGVertexNormalTexIndexList, TGLAnimationControler,
    TFGIndexTexCoordList, TGLSkeletonCollider, TGLSkeletonColliderList]);

finalization

FreeAndNil(vVectorFileFormats);

end.

