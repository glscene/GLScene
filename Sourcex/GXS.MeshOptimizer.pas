//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.MeshOptimizer;

(* Mesh optimization *)

interface

uses
  System.Classes,
  System.SysUtils,

  GXS.VectorGeometry,
  GXS.VectorTypes,
  GXS.VectorFileObjects,
  GXS.PersistentClasses,
  GXS.VectorLists,
  GXS.MeshUtils;


type
   TMeshOptimizerOption = (mooStandardize, mooVertexCache, mooSortByMaterials, mooMergeObjects);
   TMeshOptimizerOptions = set of TMeshOptimizerOption;

var
   vDefaultMeshOptimizerOptions : TMeshOptimizerOptions = [mooStandardize, mooVertexCache, mooSortByMaterials, mooMergeObjects];

procedure OptimizeMesh(aList : TgxMeshObjectList; options : TMeshOptimizerOptions); overload;
procedure OptimizeMesh(aList : TgxMeshObjectList); overload;
procedure OptimizeMesh(aMeshObject : TgxMeshObject; options : TMeshOptimizerOptions); overload;
procedure OptimizeMesh(aMeshObject : TgxMeshObject); overload;
procedure FacesSmooth(aMeshObj: TgxMeshObject; aWeldDistance: Single=0.0000001; aThreshold: Single=35.0; InvertNormals:boolean=false);


// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure OptimizeMesh(aList : TgxMeshObjectList);
begin
   OptimizeMesh(aList, vDefaultMeshOptimizerOptions);
end;

procedure OptimizeMesh(aList : TgxMeshObjectList; options : TMeshOptimizerOptions);
var
   i, k : Integer;
   mob, mo : TgxMeshObject;
   fg : TgxFaceGroup;
   fgvi : TfgxVertexIndexList;
begin
   // optimize all mesh objects
   for i:=0 to aList.Count-1 do begin
      OptimizeMesh(aList[i], options);
   end;
   if (mooStandardize in options) then begin
      // drop mesh objects that have become empty
      for i:=aList.Count-1 downto 0 do begin
         if (aList[i].Mode=momFaceGroups) and (aList[i].FaceGroups.Count=0) then
            aList[i].Free;
      end;
   end;
   if (aList.Count>0) and (mooMergeObjects in options) then begin
      mob:=aList[0];
      Assert(mob.Mode=momFaceGroups);
      for i:=1 to aList.Count-1 do begin
         mo:=aList[i];
         Assert(mo.Mode=momFaceGroups);
         k:=mob.Vertices.Count;
         mob.Vertices.Add(mo.Vertices);
         mob.Normals.Add(mo.Normals);
         mob.TexCoords.Add(mo.TexCoords);
         while mo.FaceGroups.Count>0 do begin
            fg:=mo.FaceGroups[0];
            fgvi:=(fg as TfgxVertexIndexList);
            fgvi.Owner:=mob.FaceGroups;
            mob.FaceGroups.Add(fgvi);
            mo.FaceGroups.Delete(0);
            fgvi.VertexIndices.Offset(k);
         end;
      end;
      for i:=aList.Count-1 downto 1 do
         aList[i].Free;
   end;
end;

procedure OptimizeMesh(aMeshObject : TgxMeshObject);
begin
   OptimizeMesh(aMeshObject, vDefaultMeshOptimizerOptions);
end;

procedure OptimizeMesh(aMeshObject : TgxMeshObject; options : TMeshOptimizerOptions);
var
   i : Integer;
   fg : TgxFaceGroup;
   coords, texCoords, normals : TgxAffineVectorList;
   il : TgxIntegerList;
   materialName : String;
begin
   if (mooMergeObjects in options) then begin
      if aMeshObject.Mode=momFaceGroups then begin
         // remove empty facegroups
         for i:=aMeshObject.FaceGroups.Count-1 downto 0 do begin
            fg:=aMeshObject.FaceGroups[i];
            if fg.TriangleCount=0 then
               fg.Free;
         end;
      end;
   end;

   if (mooStandardize in options) then begin
      if (aMeshObject.Mode<>momFaceGroups) or (aMeshObject.FaceGroups.Count<=1) then begin
         if aMeshObject.FaceGroups.Count=1 then
            materialName:=aMeshObject.FaceGroups[0].MaterialName;
         texCoords:=TgxAffineVectorList.Create;
         normals:=TgxAffineVectorList.Create;
         coords:=aMeshObject.ExtractTriangles(texCoords, normals);
         try
            il:=BuildVectorCountOptimizedIndices(coords, normals, texCoords);
            try
               aMeshObject.Clear;
               if il.Count>0 then begin
                  RemapReferences(normals, il);
                  RemapReferences(texCoords, il);
                  RemapAndCleanupReferences(coords, il);
                  aMeshObject.Vertices:=coords;
                  aMeshObject.Normals:=normals;
                  aMeshObject.TexCoords:=texCoords;
                  fg:=TfgxVertexIndexList.CreateOwned(aMeshObject.FaceGroups);
                  fg.MaterialName:=materialName;
                  TfgxVertexIndexList(fg).VertexIndices:=il;
               end;
            finally
               il.Free;
            end;
         finally
            coords.Free;
            normals.Free;
            texCoords.Free;
         end;
      end else
        Assert(False, 'Standardization with multiple facegroups not supported');
   end;

   if (mooVertexCache in options) and (aMeshObject.Mode=momFaceGroups) then begin
       for i:=0 to aMeshObject.FaceGroups.Count-1 do begin
         fg:=aMeshObject.FaceGroups[i];
         if fg.ClassType=TfgxVertexIndexList then with TfgxVertexIndexList(fg) do begin
            if Mode in [fgmmTriangles, fgmmFlatTriangles] then
               IncreaseCoherency(VertexIndices, 12);
         end;
      end;
   end;

   if mooSortByMaterials in options then
      aMeshObject.FaceGroups.SortByMaterial;
end;



procedure FacesSmooth(aMeshObj: TgxMeshObject; aWeldDistance: Single=0.0000001; aThreshold: Single=35.0; InvertNormals:boolean=false);
Var
  I, J, K, L: integer;
  WeldedVertex: TgxAffineVectorList;
  TmpIntegerList: TgxIntegerList;
  IndexMap: TStringList;
  n: TAffineVector;
  indicesMap : TgxIntegerList;
  Index: Integer;
  FaceList: TgxIntegerList;
  NormalList: TgxAffineVectorList;
  FaceNormalList: TgxAffineVectorList;
  FaceGroup: TgxFaceGroup;
  FG, FG1: TfgxVertexIndexList;
  Threshold: Single;
  Angle: Single;
  ReferenceMap: TgxIntegerList;
  ID1, ID2: Integer;
  Index1, Index2, Index3: Integer;

  function FindReferenceIndex(aID: Integer): Integer;
  begin
    Result := ReferenceMap[aID];
  end;
  function iMin(a, b: Integer): Integer;
  begin
    if a<b then
      Result := a
    else
      Result := b;
  end;
  function iMax(a, b: Integer): Integer;
  begin
    if a>b then
      Result := a
    else
      Result := b;
  end;
begin
  Threshold := aThreshold * Pi/180.0;
  //build the vectices reference map
  ReferenceMap := TgxIntegerList.Create;
  WeldedVertex := TgxAffineVectorList.Create;
  WeldedVertex.Assign(aMeshObj.Vertices);
  indicesMap := TgxIntegerList.Create;
  //first of all, weld the very closed vertices
  WeldVertices(WeldedVertex, indicesMap, aWeldDistance);
  //then, rebuild the map list
  IndexMap := TStringList.Create;
  for I:=0 to WeldedVertex.Count-1 do
  begin
    ReferenceMap.Assign(indicesMap);
    TmpIntegerList := TgxIntegerList.Create;
    Index := ReferenceMap.IndexOf(I);
    while Index>=0 do
    begin
      TmpIntegerList.Add(Index);
      ReferenceMap[Index] := -99999;
      Index := ReferenceMap.IndexOf(I);
    end;
    IndexMap.AddObject(IntToStr(I), TmpIntegerList);
  end;
  ReferenceMap.Assign(indicesMap);
  //never used these, free them all
  WeldedVertex.free;
  indicesMap.free;
  //create a TexPoint list for save face infomation, where s=facegroup index, t=face index
  FaceList := TgxIntegerList.Create;
  NormalList := TgxAffineVectorList.Create;
  FaceNormalList := TgxAffineVectorList.Create;
  //NormalIndex := TgxIntegerList.Create;
  for I:=0 to aMeshObj.FaceGroups.Count-1 do
  begin
    FaceGroup := aMeshObj.FaceGroups[I];
    TmpIntegerList := TfgxVertexIndexList(FaceGroup).VertexIndices;
    for J:=0 to (TmpIntegerList.Count div 3)-1 do
    begin
      FaceList.Add(I);
      FaceList.Add(J);
      CalcPlaneNormal(aMeshObj.Vertices[TmpIntegerList[J * 3 + 0]],
        aMeshObj.Vertices[TmpIntegerList[J * 3 + 1]],
        aMeshObj.Vertices[TmpIntegerList[J * 3 + 2]],
        n);
      //add three normals for one trangle
      FaceNormalList.Add(n);
      NormalList.Add(n);
      NormalList.Add(n);
      NormalList.Add(n);
    end;
  end;
  //do smooth
  for I:=0 to (FaceList.Count div 2)-1 do
  begin
    Index := FaceList[I*2+0];
    Index1 := FaceList[I*2+1];
    FG := TfgxVertexIndexList(aMeshObj.FaceGroups[Index]);
    for J:=0 to 2 do
    begin
      for K:=0 to (FaceList.Count div 2)-1 do
      begin
        Index2 := FaceList[K*2+0];
        Index3 := FaceList[K*2+1];
        FG1 := TfgxVertexIndexList(aMeshObj.FaceGroups[Index2]);
        if I<>K then
        begin
          for L:=0 to 2 do
          begin
            //two face contain the same vertex
            ID1 := FindReferenceIndex(FG.VertexIndices[Index1*3+J]);
            ID2 := FindReferenceIndex(FG1.VertexIndices[Index3*3+L]);
            if ID1=ID2 then
            begin
              Angle := VectorDotProduct(FaceNormalList[I],FaceNormalList[K]);
              if angle>Threshold then
                NormalList[I*3+J] := VectorAdd(NormalList[I*3+J],FaceNormalList[K]);
            end;
          end;
        end;
      end;
      n := NormalList[I*3+J];
      NormalizeVector(n);
      NormalList[I*3+J] := n;
    end;
  end;
  for I:=0 to (FaceList.Count div 2)-1 do
  begin
    Index := FaceList[I*2+0];
    FG := TfgxVertexIndexList(aMeshObj.FaceGroups[Index]);
    Index := FaceList[I*2+1];
    aMeshObj.Normals[FG.VertexIndices[(Index*3+0)]] := NormalList[(I*3+0)];
    aMeshObj.Normals[FG.VertexIndices[(Index*3+1)]] := NormalList[(I*3+1)];
    aMeshObj.Normals[FG.VertexIndices[(Index*3+2)]] := NormalList[(I*3+2)];
    if InvertNormals then
    begin
         aMeshObj.Normals[FG.VertexIndices[(Index*3+0)]] := VectorNegate(aMeshObj.Normals[FG.VertexIndices[(Index*3+0)]]);
         aMeshObj.Normals[FG.VertexIndices[(Index*3+1)]] := VectorNegate(aMeshObj.Normals[FG.VertexIndices[(Index*3+1)]]);
         aMeshObj.Normals[FG.VertexIndices[(Index*3+2)]] := VectorNegate(aMeshObj.Normals[FG.VertexIndices[(Index*3+2)]]);
    end;
  end;
  FaceList.free;
  NormalList.free;
  FaceNormalList.free;
  ReferenceMap.free;
  for I:=0 to IndexMap.Count-1 do
    IndexMap.Objects[I].free;
  IndexMap.free;
end;


end.
