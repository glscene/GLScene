//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLMeshOptimizer;

(* Mesh optimization unit *)

interface

uses
  System.Classes,
  System.Sysutils,

  GLVectorTypes,
  GLVectorGeometry,
  GLVectorFileObjects;

type

  TGLMeshOptimizerOption = (mooStandardize, mooVertexCache, mooSortByMaterials,
    mooMergeObjects);
  TGLMeshOptimizerOptions = set of TGLMeshOptimizerOption;

var
  vDefaultMeshOptimizerOptions: TGLMeshOptimizerOptions = [mooStandardize,
    mooVertexCache, mooSortByMaterials, mooMergeObjects];

(* Optimize Mesh (list, default options) *)
procedure OptimizeMesh(aList: TGLMeshObjectList; options: TGLMeshOptimizerOptions); overload;
procedure OptimizeMesh(aList: TGLMeshObjectList); overload;
// OptimizeMesh (object, with options)
procedure OptimizeMesh(aMeshObject: TMeshObject; options: TGLMeshOptimizerOptions); overload;
// OptimizeMesh (object, default options)
procedure OptimizeMesh(aMeshObject: TMeshObject); overload;
procedure FacesSmooth(aMeshObj: TMeshObject;
  aWeldDistance: Single = 0.0000001; aThreshold: Single = 35.0;
  InvertNormals: boolean = false);

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
  GLPersistentClasses, 
  GLVectorLists, 
  GLMeshUtils;

procedure OptimizeMesh(aList: TGLMeshObjectList);
begin
  OptimizeMesh(aList, vDefaultMeshOptimizerOptions);
end;

procedure OptimizeMesh(aList: TGLMeshObjectList;
  options: TGLMeshOptimizerOptions);
var
  i, k: Integer;
  mob, mo: TMeshObject;
  fg: TGLFaceGroup;
  fgvi: TFGVertexIndexList;
begin
  // optimize all mesh objects
  for i := 0 to aList.Count - 1 do
  begin
    OptimizeMesh(aList[i], options);
  end;
  if (mooStandardize in options) then
  begin
    // drop mesh objects that have become empty
    for i := aList.Count - 1 downto 0 do
    begin
      if (aList[i].Mode = momFaceGroups) and (aList[i].FaceGroups.Count = 0)
      then
        aList[i].Free;
    end;
  end;
  if (aList.Count > 0) and (mooMergeObjects in options) then
  begin
    mob := aList[0];
    Assert(mob.Mode = momFaceGroups);
    for i := 1 to aList.Count - 1 do
    begin
      mo := aList[i];
      Assert(mo.Mode = momFaceGroups);
      k := mob.Vertices.Count;
      mob.Vertices.Add(mo.Vertices);
      mob.Normals.Add(mo.Normals);
      mob.TexCoords.Add(mo.TexCoords);
      while mo.FaceGroups.Count > 0 do
      begin
        fg := mo.FaceGroups[0];
        fgvi := (fg as TFGVertexIndexList);
        fgvi.Owner := mob.FaceGroups;
        mob.FaceGroups.Add(fgvi);
        mo.FaceGroups.Delete(0);
        fgvi.VertexIndices.Offset(k);
      end;
    end;
    for i := aList.Count - 1 downto 1 do
      aList[i].Free;
  end;
end;

procedure OptimizeMesh(aMeshObject: TMeshObject);
begin
  OptimizeMesh(aMeshObject, vDefaultMeshOptimizerOptions);
end;

procedure OptimizeMesh(aMeshObject: TMeshObject;
  options: TGLMeshOptimizerOptions);
var
  i: Integer;
  fg: TGLFaceGroup;
  coords, TexCoords, Normals: TAffineVectorList;
  il: TIntegerList;
  materialName: String;
begin
  if (mooMergeObjects in options) then
  begin
    if aMeshObject.Mode = momFaceGroups then
    begin
      // remove empty facegroups
      for i := aMeshObject.FaceGroups.Count - 1 downto 0 do
      begin
        fg := aMeshObject.FaceGroups[i];
        if fg.TriangleCount = 0 then
          fg.Free;
      end;
    end;
  end;

  if (mooStandardize in options) then
  begin
    if (aMeshObject.Mode <> momFaceGroups) or (aMeshObject.FaceGroups.Count <= 1)
    then
    begin
      if aMeshObject.FaceGroups.Count = 1 then
        materialName := aMeshObject.FaceGroups[0].materialName;
      TexCoords := TAffineVectorList.Create;
      Normals := TAffineVectorList.Create;
      coords := aMeshObject.ExtractTriangles(TexCoords, Normals);
      try
        il := BuildVectorCountOptimizedIndices(coords, Normals, TexCoords);
        try
          aMeshObject.Clear;
          if il.Count > 0 then
          begin
            RemapReferences(Normals, il);
            RemapReferences(TexCoords, il);
            RemapAndCleanupReferences(coords, il);
            aMeshObject.Vertices := coords;
            aMeshObject.Normals := Normals;
            aMeshObject.TexCoords := TexCoords;
            fg := TFGVertexIndexList.CreateOwned(aMeshObject.FaceGroups);
            fg.materialName := materialName;
            TFGVertexIndexList(fg).VertexIndices := il;
          end;
        finally
          il.Free;
        end;
      finally
        coords.Free;
        Normals.Free;
        TexCoords.Free;
      end;
    end
    else
      Assert(false,
        'Standardization with multiple facegroups not supported... yet.');
  end;

  if (mooVertexCache in options) and (aMeshObject.Mode = momFaceGroups) then
  begin
    for i := 0 to aMeshObject.FaceGroups.Count - 1 do
    begin
      fg := aMeshObject.FaceGroups[i];
      if fg.ClassType = TFGVertexIndexList then
        with TFGVertexIndexList(fg) do
        begin
          if Mode in [fgmmTriangles, fgmmFlatTriangles] then
            IncreaseCoherency(VertexIndices, 12);
        end;
    end;
  end;

  if mooSortByMaterials in options then
    aMeshObject.FaceGroups.SortByMaterial;
end;

procedure FacesSmooth(aMeshObj: TMeshObject;
  aWeldDistance: Single = 0.0000001; aThreshold: Single = 35.0;
  InvertNormals: boolean = false);
Var
  i, J, k, L: Integer;
  WeldedVertex: TAffineVectorList;
  TmpIntegerList: TIntegerList;
  IndexMap: TStringList;
  n: TAffineVector;
  indicesMap: TIntegerList;
  Index: Integer;
  FaceList: TIntegerList;
  NormalList: TAffineVectorList;
  FaceNormalList: TAffineVectorList;
  FaceGroup: TGLFaceGroup;
  fg, FG1: TFGVertexIndexList;
  Threshold: Single;
  Angle: Single;
  ReferenceMap: TIntegerList;
  ID1, ID2: Integer;
  Index1, Index2, Index3: Integer;

  function FindReferenceIndex(aID: Integer): Integer;
  begin
    Result := ReferenceMap[aID];
  end;
  function iMin(a, b: Integer): Integer;
  begin
    if a < b then
      Result := a
    else
      Result := b;
  end;
  function iMax(a, b: Integer): Integer;
  begin
    if a > b then
      Result := a
    else
      Result := b;
  end;

begin
  Threshold := aThreshold * Pi / 180.0;
  // build the vectices reference map
  ReferenceMap := TIntegerList.Create;
  WeldedVertex := TAffineVectorList.Create;
  WeldedVertex.Assign(aMeshObj.Vertices);
  indicesMap := TIntegerList.Create;
  // first of all, weld the very closed vertices
  WeldVertices(WeldedVertex, indicesMap, aWeldDistance);
  // then, rebuild the map list
  IndexMap := TStringList.Create;
  for i := 0 to WeldedVertex.Count - 1 do
  begin
    ReferenceMap.Assign(indicesMap);
    TmpIntegerList := TIntegerList.Create;
    Index := ReferenceMap.IndexOf(i);
    while Index >= 0 do
    begin
      TmpIntegerList.Add(Index);
      ReferenceMap[Index] := -99999;
      Index := ReferenceMap.IndexOf(i);
    end;
    IndexMap.AddObject(IntToStr(i), TmpIntegerList);
  end;
  ReferenceMap.Assign(indicesMap);
  // never used these, free them all
  WeldedVertex.Free;
  indicesMap.Free;
  // creates a TexPoint list for save face infomation, where s=facegroup index, t=face index
  FaceList := TIntegerList.Create;
  NormalList := TAffineVectorList.Create;
  FaceNormalList := TAffineVectorList.Create;
  // NormalIndex := TIntegerList.Create;
  for i := 0 to aMeshObj.FaceGroups.Count - 1 do
  begin
    FaceGroup := aMeshObj.FaceGroups[i];
    TmpIntegerList := TFGVertexIndexList(FaceGroup).VertexIndices;
    for J := 0 to (TmpIntegerList.Count div 3) - 1 do
    begin
      FaceList.Add(i);
      FaceList.Add(J);
      CalcPlaneNormal(aMeshObj.Vertices[TmpIntegerList[J * 3 + 0]],
        aMeshObj.Vertices[TmpIntegerList[J * 3 + 1]],
        aMeshObj.Vertices[TmpIntegerList[J * 3 + 2]], n);
      // add three normals for one trangle
      FaceNormalList.Add(n);
      NormalList.Add(n);
      NormalList.Add(n);
      NormalList.Add(n);
    end;
  end;
  // do smooth
  for i := 0 to (FaceList.Count div 2) - 1 do
  begin
    Index := FaceList[i * 2 + 0];
    Index1 := FaceList[i * 2 + 1];
    fg := TFGVertexIndexList(aMeshObj.FaceGroups[Index]);
    for J := 0 to 2 do
    begin
      for k := 0 to (FaceList.Count div 2) - 1 do
      begin
        Index2 := FaceList[k * 2 + 0];
        Index3 := FaceList[k * 2 + 1];
        FG1 := TFGVertexIndexList(aMeshObj.FaceGroups[Index2]);
        if i <> k then
        begin
          for L := 0 to 2 do
          begin
            // two face contain the same vertex
            ID1 := FindReferenceIndex(fg.VertexIndices[Index1 * 3 + J]);
            ID2 := FindReferenceIndex(FG1.VertexIndices[Index3 * 3 + L]);
            if ID1 = ID2 then
            begin
              Angle := VectorDotProduct(FaceNormalList[i], FaceNormalList[k]);
              if Angle > Threshold then
                NormalList[i * 3 + J] := VectorAdd(NormalList[i * 3 + J],
                  FaceNormalList[k]);
            end;
          end;
        end;
      end;
      n := NormalList[i * 3 + J];
      NormalizeVector(n);
      NormalList[i * 3 + J] := n;
    end;
  end;
  for i := 0 to (FaceList.Count div 2) - 1 do
  begin
    Index := FaceList[i * 2 + 0];
    fg := TFGVertexIndexList(aMeshObj.FaceGroups[Index]);
    Index := FaceList[i * 2 + 1];
    aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 0)]] :=
      NormalList[(i * 3 + 0)];
    aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 1)]] :=
      NormalList[(i * 3 + 1)];
    aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 2)]] :=
      NormalList[(i * 3 + 2)];
    if InvertNormals then
    begin
      aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 0)]] :=
        VectorNegate(aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 0)]]);
      aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 1)]] :=
        VectorNegate(aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 1)]]);
      aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 2)]] :=
        VectorNegate(aMeshObj.Normals[fg.VertexIndices[(Index * 3 + 2)]]);
    end;
  end;
  FaceList.Free;
  NormalList.Free;
  FaceNormalList.Free;
  ReferenceMap.Free;
  for i := 0 to IndexMap.Count - 1 do
    IndexMap.Objects[i].Free;
  IndexMap.Free;
end;

end.
