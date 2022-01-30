//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.MeshCSG;

(*
   Constructive Solid Geometry on mesh base.
   The CSG system uses BSP to optimize what triangles it considers.
   Its kept on a mesh basis to simplyfy things, it allways generates new BSP's,
   even if the meshes allready had BSP optimization.
*)

interface

{$I GLScene.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Math,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.VectorFileObjects,
  GLS.VectorGeometry,
  GLS.MeshBSP,
  GLS.VectorLists;

type
  TCSGOperation = (CSG_Union, CSG_Subtraction, CSG_Intersection);

procedure CSG_Operation(obj1, obj2: TGLMeshObject; Operation: TCSGOperation; Res: TGLMeshObject; const MaterialName1, MaterialName2: string);

//-----------------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------------

const
  cOwnTriangleEpsilon = 1e-5;

function IntersectPointToPointLinePlane(const point1, point2: TAffineVector; const plane: THmgPlane; intersectPoint: PAffineVector = nil): Boolean;
var
  a, b: Extended;
  t: Single;
  Direction: TAffineVector;
begin
  Result := False;
  VectorSubtract(Point2, Point1, Direction);
  a := VectorDotProduct(plane, Direction); // direction projected to plane normal
  if a <> 0 then
  begin // direction is parallel to plane
    b := PlaneEvaluatePoint(plane, point1); // distance to plane
    t := -b / a; // parameter of intersection
    Result := (t - EPSILON2 > 0) and (t + EPSILON2 < 1);
    if Result and (intersectPoint <> nil) then
    begin
      intersectPoint^ := VectorCombine(Point1, Direction, 1, t);

      if VectorEquals(intersectPoint^, point1) or VectorEquals(intersectPoint^, point2) then
        Result := False;
    end;
  end;
end;

type
  TCSGTri = array[0..2] of PAffineVector;

function MakeCSGTri(const V1, V2, V3: PAffineVector): TCSGTri;
begin
  Result[0] := v1;
  Result[1] := v2;
  Result[2] := v3;
end;

procedure CSG_Iterate_tri(const vec, nor: TCSGTri; BSP: TBSPMeshObject;
  Node: TFGBSPNode; ResMesh: TGLMeshObject; ResFG: TFGVertexNormalTexIndexList; keepinside, keepoutside, inverttriangle: Boolean);

var
  vertex_offset: Integer;

  function Iterate_further(const vec, nor: TCSGTri; Node: TFGBSPNode): Boolean;

    function MustSplit(d1, d2, d3: Single): Integer;
    var
      side: Integer;

      function Ok(Int: Single): Boolean;
      begin
        Result := False;
        if Int < 0 then
        begin
          if Side > 0 then
          begin
            Result := True;
            Exit;
          end
          else
            Side := -1;
        end
        else if Int > 0 then
        begin
          if Side < 0 then
          begin
            Result := True;
            Exit;
          end
          else
            Side := 1;
        end;
      end;

    begin
      Result := 0; // no split
      side := 0;
      Ok(D1); // can't go wrong yet...

      if Ok(D2) then
      begin
        Result := 1; // pure split.
        Exit;
      end;

      if Ok(D3) then
      begin
        Result := 1; // pure split.
        Exit;
      end;

      if side = 0 then
      begin
        Result := 2; // on plane.
        Exit;
      end;
    end;

  var
    d: array[0..2] of Single;
    i, i1: Integer;
    b1, b2, b3: Boolean;
    intersect_points: array[0..2] of TAffineVector;
    intersect_lines: array[0..2] of Integer;
    intersect_count: Integer;
    p0, p1: Integer;
    NextNode: TFGBSPNode;
    plane: THmgPlane;

  begin
    // This have no effect, however it removes a warning...
    Result := False;
    b1 := False;
    b2 := False;
    b3 := False;
    // This have no effect, however it removes a warning...

    // normally we use the Node.SplitPlane, however on the last branch this is a NullPlane, so we have to calculate it.
    if VectorEquals(Node.SplitPlane, NullHmgVector) then
      plane := PlaneMake(BSP.Vertices[Node.VertexIndices[0]], BSP.Vertices[Node.VertexIndices[1]], BSP.Vertices[Node.VertexIndices[2]])
    else
      plane := Node.SplitPlane;

    // check the three points in the triangle against the plane
    for i := 0 to 2 do
    begin
      d[i] := PlaneEvaluatePoint(plane, Vec[i]^);
      if Abs(d[i]) < cOwnTriangleEpsilon then
        d[i] := 0;
    end;

    // based on points placement determine action
    case MustSplit(d[0], d[1], d[2]) of
      0: // no split
        if (d[0] + d[1] + d[2] >= 0) then
        begin
          // check for sub node, and either iterate them or stop if none.
          if Node.PositiveSubNodeIndex = 0 then
            Result := keepoutside
          else
            Result := Iterate_further(Vec, nor, TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex]));
        end
        else
        begin
          // check for sub node, and either iterate them or stop if none.
          if Node.NegativeSubNodeIndex = 0 then
            Result := keepinside
          else
            Result := Iterate_further(Vec, nor, TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]));
        end;
      1: // must split.
        begin
          // determine if 2 or 3 triangles are needed for the split.
          intersect_count := 0;
          for i := 0 to 2 do
          begin
            i1 := (i + 1) mod 3;
            if IntersectPointToPointLinePlane(Vec[i]^, Vec[i1]^, plane, @intersect_points[intersect_count]) then
            begin
              intersect_lines[intersect_count] := i;
              Inc(intersect_count);
            end;
          end;
          // from here of its not thoroughly commented
          // the base concept is isolate the two or three triangles, findout which to keep.
          // If all is kept we can simply return true and the original triangle wont be split.
          // however if only some are needed, we have to return false and add them ourselves...
          case intersect_count of
            1:
              begin
                // simple split, two tri's
                i := intersect_lines[0];
                i1 := (i + 2) mod 3;

                // cannot be 0, as then intersect_lines[0] would have other value
                if (d[i] > 0) then
                  if Node.PositiveSubNodeIndex = 0 then
                  begin
                    NextNode := nil;
                    b1 := keepoutside;
                  end
                  else
                    NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
                else if Node.NegativeSubNodeIndex = 0 then
                begin
                  NextNode := nil;
                  b1 := keepinside;
                end
                else
                  NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

                if NextNode <> nil then
                  b1 := Iterate_further(MakeCSGTri(Vec[i], @intersect_points[0], Vec[i1]), MakeCSGTri(Nor[i], Nor[i {}], Nor[i1]), NextNode);

                i := (intersect_lines[0] + 1) mod 3;
                i1 := (i + 1) mod 3;
                // cannot be 0, as then intersect_lines[0] would have other value
                if (d[i] > 0) then
                  if Node.PositiveSubNodeIndex = 0 then
                  begin
                    NextNode := nil;
                    b2 := keepoutside;
                  end
                  else
                    NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
                else if Node.NegativeSubNodeIndex = 0 then
                begin
                  NextNode := nil;
                  b2 := keepinside;
                end
                else
                  NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

                if NextNode <> nil then
                  b2 := Iterate_further(MakeCSGTri(Vec[i], Vec[i1], @intersect_points[0]), MakeCSGTri(Nor[i], Nor[i1], Nor[i {}]), NextNode);

                Result := b1 and b2;
                if not Result then
                begin
                  if B1 then
                  begin
                    i := intersect_lines[0];
                    i1 := (i + 2) mod 3;
                    vertex_offset := ResMesh.Vertices.count;
                    ResMesh.Vertices.Add(Vec[i]^, intersect_points[0], Vec[i1]^);
                    ResMesh.TexCoords.Add(Vec[i]^, intersect_points[0], Vec[i1]^);

                    if inverttriangle then
                    begin
                      ResMesh.Normals.Add(VectorScale(Nor[i]^, -1), VectorScale(Nor[i {}]^, -1), VectorScale(Nor[i1]^, -1));
                      ResFG.VertexIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                      ResFG.NormalIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                      ResFG.TexCoordIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                    end
                    else
                    begin
                      ResMesh.Normals.Add(Nor[i]^, Nor[i {}]^, Nor[i1]^);
                      ResFG.VertexIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                      ResFG.NormalIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                      ResFG.TexCoordIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                    end;
                  end
                  else if B2 then
                  begin
                    i := (intersect_lines[0] + 1) mod 3;
                    i1 := (i + 1) mod 3;
                    vertex_offset := ResMesh.Vertices.count;
                    ResMesh.Vertices.Add(Vec[i]^, Vec[i1]^, intersect_points[0]);
                    ResMesh.TexCoords.Add(Vec[i]^, Vec[i1]^, intersect_points[0]);

                    if inverttriangle then
                    begin
                      ResMesh.Normals.Add(VectorScale(Nor[i]^, -1), VectorScale(Nor[i {}]^, -1), VectorScale(Nor[i1]^, -1));
                      ResFG.VertexIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                      ResFG.NormalIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                      ResFG.TexCoordIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                    end
                    else
                    begin
                      ResMesh.Normals.Add(Nor[i]^, Nor[i {}]^, Nor[i1]^);
                      ResFG.VertexIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                      ResFG.NormalIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                      ResFG.TexCoordIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                    end;
                  end;
                end;
              end;
            2:
              begin
                // complex split, three tri's
                if intersect_lines[0] + 1 = intersect_lines[1] then
                begin
                  p0 := 0;
                  p1 := 1;
                end
                else
                begin
                  p0 := 1;
                  p1 := 0;
                end;
                i := intersect_lines[p0];
                i1 := (i + 2) mod 3;

                // cannot be 0 as then there would be no intersection
                if (d[i] > 0) then
                  if Node.PositiveSubNodeIndex = 0 then
                  begin
                    NextNode := nil;
                    b1 := keepoutside;
                  end
                  else
                    NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
                else if Node.NegativeSubNodeIndex = 0 then
                begin
                  NextNode := nil;
                  b1 := keepinside;
                end
                else
                  NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

                if NextNode <> nil then
                  b1 := Iterate_further(MakeCSGTri(Vec[i], @intersect_points[p0], Vec[i1]), MakeCSGTri(Nor[i], Nor[i {}], Nor[i1]), NextNode);

                i1 := (i + 1) mod 3;
                // cannot be 0 as then there would be no intersection
                if (d[i1] > 0) then
                  if Node.PositiveSubNodeIndex = 0 then
                  begin
                    NextNode := nil;
                    b2 := keepoutside;
                  end
                  else
                    NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
                else if Node.NegativeSubNodeIndex = 0 then
                begin
                  NextNode := nil;
                  b2 := keepinside;
                end
                else
                  NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

                if NextNode <> nil then
                  b2 := Iterate_further(MakeCSGTri(@intersect_points[p0], Vec[i1], @intersect_points[p1]), MakeCSGTri(Nor[i1 {}], Nor[i1], Nor[i1 {}]), NextNode);

                i1 := (i + 2) mod 3;
                // cannot be 0 as then there would be no intersection
                if (d[i1] > 0) then
                  if Node.PositiveSubNodeIndex = 0 then
                  begin
                    NextNode := nil;
                    b3 := keepoutside;
                  end
                  else
                    NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
                else if Node.NegativeSubNodeIndex = 0 then
                begin
                  NextNode := nil;
                  b3 := keepinside;
                end
                else
                  NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

                if NextNode <> nil then
                  b3 := Iterate_further(MakeCSGTri(@intersect_points[p0], @intersect_points[p1], Vec[i1]), MakeCSGTri(Nor[i1 {}], Nor[i1 {}], Nor[i1]), NextNode);

                Result := b1 and b2 and b3;
                if not Result then
                begin
                  if B1 then
                  begin
                    i1 := (i + 2) mod 3;
                    vertex_offset := ResMesh.Vertices.count;
                    ResMesh.Vertices.Add(Vec[i]^, intersect_points[p0], Vec[i1]^);
                    ResMesh.TexCoords.Add(Vec[i]^, intersect_points[p0], Vec[i1]^);
                    if inverttriangle then
                    begin
                      ResMesh.Normals.Add(VectorScale(Nor[i]^, -1), VectorScale(Nor[i {}]^, -1), VectorScale(Nor[i1]^, -1));
                      ResFG.VertexIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                      ResFG.NormalIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                      ResFG.TexCoordIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                    end
                    else
                    begin
                      ResMesh.Normals.Add(Nor[i]^, Nor[i {}]^, Nor[i1]^);
                      ResFG.VertexIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                      ResFG.NormalIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                      ResFG.TexCoordIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                    end;
                  end;
                  if B2 then
                  begin
                    i1 := (i + 1) mod 3;
                    vertex_offset := ResMesh.Vertices.count;
                    ResMesh.Vertices.Add(intersect_points[p0], Vec[i1]^, intersect_points[p1]);
                    ResMesh.TexCoords.Add(intersect_points[p0], Vec[i1]^, intersect_points[p1]);

                    if inverttriangle then
                    begin
                      ResMesh.Normals.Add(VectorScale(Nor[i1 {}]^, -1), VectorScale(Nor[i1]^, -1), VectorScale(Nor[i1 {}]^, -1));
                      ResFG.VertexIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                      ResFG.NormalIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                      ResFG.TexCoordIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                    end
                    else
                    begin
                      ResMesh.Normals.Add(Nor[i1 {}]^, Nor[i1]^, Nor[i1 {}]^);
                      ResFG.VertexIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                      ResFG.NormalIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                      ResFG.TexCoordIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                    end;
                  end;
                  if B3 then
                  begin
                    i1 := (i + 2) mod 3;
                    vertex_offset := ResMesh.Vertices.count;
                    ResMesh.Vertices.Add(intersect_points[p0], intersect_points[p1], Vec[i1]^);
                    ResMesh.TexCoords.Add(intersect_points[p0], intersect_points[p1], Vec[i1]^);

                    if inverttriangle then
                    begin
                      ResMesh.Normals.Add(VectorScale(Nor[i1 {}]^, -1), VectorScale(Nor[i1 {}]^, -1), VectorScale(Nor[i1]^, -1));
                      ResFG.VertexIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                      ResFG.NormalIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                      ResFG.TexCoordIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
                    end
                    else
                    begin
                      ResMesh.Normals.Add(Nor[i1 {}]^, Nor[i1 {}]^, Nor[i1]^);
                      ResFG.VertexIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                      ResFG.NormalIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                      ResFG.TexCoordIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
                    end;
                  end;
                end;
              end;
          end;
        end;
      2: // on plane, no split but special logic
        begin
          // find out if they point the same direction.
          d[0] := PlaneEvaluatePoint(Plane, VectorAdd(Vec[0]^, Nor[0]^));
          // check for sub node, and either iterate them or stop if none.
          if d[0] >= 0 then
            if Node.PositiveSubNodeIndex = 0 then
            begin
              NextNode := nil;
              Result := keepoutside;
            end
            else
              NextNode := TFGBSPNode(BSP.FaceGroups[Node.PositiveSubNodeIndex])
          else if Node.NegativeSubNodeIndex = 0 then
          begin
            NextNode := nil;
            Result := keepinside;
          end
          else
            NextNode := TFGBSPNode(BSP.FaceGroups[Node.NegativeSubNodeIndex]);

          if NextNode <> nil then
            Result := Iterate_further(Vec, Nor, NextNode);
        end;
    end;
  end;

begin
  // check this triangle.
  if Iterate_Further(Vec, nor, Node) then
  begin
    // Keep this triangle, logic based on the (keepinside, keepoutside) booleans.
    vertex_offset := ResMesh.Vertices.count;
    ResMesh.Vertices.Add(Vec[0]^, Vec[1]^, Vec[2]^);
    ResMesh.TexCoords.Add(Vec[0]^, Vec[1]^, Vec[2]^);
    if inverttriangle then
    begin
      ResMesh.Normals.Add(VectorScale(Nor[0]^, -1), VectorScale(Nor[1]^, -1), VectorScale(Nor[2]^, -1));
      ResFG.VertexIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
      ResFG.NormalIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
      ResFG.TexCoordIndices.Add(vertex_offset + 2, vertex_offset + 1, vertex_offset);
    end
    else
    begin
      ResMesh.Normals.Add(Nor[0]^, Nor[1]^, Nor[2]^);
      ResFG.VertexIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
      ResFG.NormalIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
      ResFG.TexCoordIndices.Add(vertex_offset, vertex_offset + 1, vertex_offset + 2);
    end;
  end;
end;

procedure CSG_Operation(obj1, obj2: TGLMeshObject; Operation: TCSGOperation;
  Res: TGLMeshObject; const MaterialName1, MaterialName2: string);

var
  v1, t1, n1: TGLAffineVectorList;
  v2, t2, n2: TGLAffineVectorList;
  BSP1, BSP2: TBSPMeshObject;
  FG1, FG2: TFGBSPNode;
  i: Integer;
  FGR: TFGVertexNormalTexIndexList;

begin
  // prepare containers, fill the triangle list from the objects
  BSP1 := TBSPMeshObject.Create;
  BSP2 := TBSPMeshObject.Create;

  BSP1.Mode := momFaceGroups;
  BSP2.Mode := momFaceGroups;

  FG1 := TFGBSPNode.CreateOwned(BSP1.FaceGroups);
  FG2 := TFGBSPNode.CreateOwned(BSP2.FaceGroups);

  t1 := TGLAffineVectorList.create;
  n1 := TGLAffineVectorList.create;
  v1 := obj1.ExtractTriangles(t1, n1);

  v1.TransformAsPoints(obj1.Owner.Owner.Matrix^);

  BSP1.Mode := momTriangles;
  BSP1.Vertices := v1;
  BSP1.Normals := n1;
  BSP1.TexCoords := t1;
  FG1.VertexIndices.AddSerie(0, 1, BSP1.Vertices.Count);

  t2 := TGLAffineVectorList.create;
  n2 := TGLAffineVectorList.create;
  v2 := obj2.ExtractTriangles(t2, n2);
  v2.TransformAsPoints(obj2.Owner.Owner.Matrix^);

  BSP2.Mode := momTriangles;
  BSP2.Vertices := v2;
  BSP2.Normals := n2;
  BSP2.TexCoords := t2;

  FG2.VertexIndices.AddSerie(0, 1, BSP2.Vertices.Count);

  // Build BSPs
  FG1.PerformSplit(FG1.FindSplitPlane, 1);
  FG2.PerformSplit(FG2.FindSplitPlane, 1);

  // Start creating result.
  FGR := TFGVertexNormalTexIndexList.CreateOwned(Res.FaceGroups);
  FGR.MaterialName := MaterialName1;

  //  should be obj1.FaceGroups iteration for perfection and multiple materials!

  //  First iterate all triangles of object 1, one at a time,  down through the BSP tree of Object 2, the last booleans are the key to what actuelly happends.
  i := 0;
  while i < v1.Count - 2 do
  begin
    case Operation of
      CSG_Union:
        begin
          CSG_Iterate_tri(MakeCSGTri(@v1.List^[i], @v1.List^[i + 1], @v1.List^[i + 2]), MakeCSGTri(@n1.List^[i], @n1.List^[i + 1], @n1.List^[i + 2]), BSP2, FG2, Res, FGR, false, true, false);
        end;
      CSG_Subtraction:
        begin
          CSG_Iterate_tri(MakeCSGTri(@v1.List^[i], @v1.List^[i + 1], @v1.List^[i + 2]), MakeCSGTri(@n1.List^[i], @n1.List^[i + 1], @n1.List^[i + 2]), BSP2, FG2, Res, FGR, false, true, false);
        end;
      CSG_Intersection:
        begin
          CSG_Iterate_tri(MakeCSGTri(@v1.List^[i], @v1.List^[i + 1], @v1.List^[i + 2]), MakeCSGTri(@n1.List^[i], @n1.List^[i + 1], @n1.List^[i + 2]), BSP2, FG2, Res, FGR, true, false, false);
        end;
    end;
    inc(i, 3);
  end;

  //  Then iterate all triangles of object 2, one at a time, down through the BSP tree of Object 1, the last booleans are the key to what actuelly happends.
  FGR := TFGVertexNormalTexIndexList.CreateOwned(Res.FaceGroups);
  FGR.MaterialName := MaterialName2;
  i := 0;
  while i < v2.Count - 2 do
  begin
    case Operation of
      CSG_Union:
        begin
          CSG_Iterate_tri(MakeCSGTri(@v2.List^[i], @v2.List^[i + 1], @v2.List^[i + 2]), MakeCSGTri(@n2.List^[i], @n2.List^[i + 1], @n2.List^[i + 2]), BSP1, FG1, Res, FGR, false, true, false);
        end;
      CSG_Subtraction:
        begin
          CSG_Iterate_tri(MakeCSGTri(@v2.List^[i], @v2.List^[i + 1], @v2.List^[i + 2]), MakeCSGTri(@n2.List^[i], @n2.List^[i + 1], @n2.List^[i + 2]), BSP1, FG1, Res, FGR, true, false, true);
        end;
      CSG_Intersection:
        begin
          CSG_Iterate_tri(MakeCSGTri(@v2.List^[i], @v2.List^[i + 1], @v2.List^[i + 2]), MakeCSGTri(@n2.List^[i], @n2.List^[i + 1], @n2.List^[i + 2]), BSP1, FG1, Res, FGR, true, false, false);
        end;
    end;
    inc(i, 3);
  end;

  // clean up.
  v1.Free;
  n1.Free;
  t1.Free;

  v2.Free;
  n2.Free;
  t2.Free;

  BSP2.Free;
  BSP1.Free;

end;

end.

