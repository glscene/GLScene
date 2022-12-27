//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.MeshUtils;

(* General utilities for mesh manipulations *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Math,

  GLS.PersistentClasses,
  GLS.VectorLists,
  GLS.VectorGeometry,
  GLS.VectorTypes;

(* 
  Converts a triangle strips into a triangle list.
  Vertices are added to list, based on the content of strip. Both non-indexed
  and indexed variants are available, the output is *always* non indexed. 
*)
procedure ConvertStripToList(const strip: TGLAffineVectorList;
  list: TGLAffineVectorList); overload;
procedure ConvertStripToList(const strip: TGLIntegerList;
  list: TGLIntegerList); overload;
procedure ConvertStripToList(const strip: TGLAffineVectorList;
  const indices: TGLIntegerList; list: TGLAffineVectorList); overload;
function ConvertStripToList(const AindicesList: PLongWordArray; Count: LongWord;
  RestartIndex: LongWord): TGLLongWordList; overload;
function ConvertFansToList(const AindicesList: PLongWordArray; Count: LongWord;
  RestartIndex: LongWord): TGLLongWordList;
// Expands an indexed structure into a non-indexed structure.
procedure ConvertIndexedListToList(const data: TGLAffineVectorList;
  const indices: TGLIntegerList; list: TGLAffineVectorList);
(* 
  Builds a vector-count optimized indices list.
  The returned list (to be freed by caller) contains an "optimized" indices
  list in which duplicates coordinates in the original vertices list are used
  only once (the first available duplicate in the list is used).
  The vertices list is left untouched, to remap/cleanup, you may use the
  RemapAndCleanupReferences function. 
*)
function BuildVectorCountOptimizedIndices(const vertices: TGLAffineVectorList;
  const normals: TGLAffineVectorList = nil;
  const texCoords: TGLAffineVectorList = nil): TGLIntegerList;
(* 
  Alters a reference array and removes unused reference values.
  This functions scans the reference list and removes all values that aren't
  referred in the indices list, the indices list is *not* remapped. 
*)
procedure RemapReferences(reference: TGLAffineVectorList;
  const indices: TGLIntegerList); overload;
procedure RemapReferences(reference: TGLIntegerList;
  const indices: TGLIntegerList); overload;
(* 
  Alters a reference/indice pair and removes unused reference values.
  This functions scans the reference list and removes all values that aren't
  referred in the indices list, and the indices list is remapped so as to remain
  coherent. 
*)
procedure RemapAndCleanupReferences(reference: TGLAffineVectorList;
  indices: TGLIntegerList);
(* 
  Creates an indices map from a remap list.
  The remap list is what BuildVectorCountOptimizedIndices, a list of indices
  to distinct/unique items, the indices map contains the indices of these items
  after a remap and cleanup of the set referred by remapIndices... Clear?
  In short it takes the output of BuildVectorCountOptimizedIndices and can change
  it to something suitable for RemapTrianglesIndices.
  Any simpler documentation of this function welcome ;) 
*)
function RemapIndicesToIndicesMap(remapIndices: TGLIntegerList): TGLIntegerList;
(* 
  Remaps a list of triangles vertex indices and remove degenerate triangles.
  The indicesMap provides newVertexIndex:=indicesMap[oldVertexIndex] 
*)
procedure RemapTrianglesIndices(indices, indicesMap: TGLIntegerList);
(* 
  Remaps a list of indices.
  The indicesMap provides newVertexIndex:=indicesMap[oldVertexIndex] 
*)
procedure remapIndices(indices, indicesMap: TGLIntegerList);
(* 
  Attempts to unify triangle winding.
  Depending on topology, this may or may not be successful (some topologies
  can't be unified, f.i. those that have duplicate triangles, those that
  have edges shared by more than two triangles, those that have unconnected
  submeshes etc.) 
*)
procedure UnifyTrianglesWinding(indices: TGLIntegerList);
// Inverts the triangles winding (vertex order).
procedure InvertTrianglesWinding(indices: TGLIntegerList);
(* 
  Builds normals for a triangles list.
  Builds one normal per reference vertex (may be NullVector is reference isn't
  used), which is the averaged for normals of all adjacent triangles.
  Returned list must be freed by caller. 
*)
function BuildNormals(reference: TGLAffineVectorList; indices: TGLIntegerList)
  : TGLAffineVectorList;
(* 
  Builds a list of non-oriented (non duplicated) edges list.
  Each edge is represented by the two integers of its vertices,
  sorted in ascending order. If not nil then
  - triangleEdges is filled with the 3 indices of the 3 edges
  of the triangle, the edges ordering respecting the original triangle
  orientation;
  - edgesTriangles is filled with the indices of the first index
  of the triangle in triangleIndices that have this edge.
  A maximum of two triangles can be referred by this list,
  and its final size will be that of the Result (ie. non oriented edges list) 
*)
function BuildNonOrientedEdgesList(triangleIndices: TGLIntegerList;
  triangleEdges: TGLIntegerList = nil; edgesTriangles: TGLIntegerList = nil)
  : TGLIntegerList;
(* 
  Welds all vertices separated by a distance inferior to weldRadius.
  Any two vertices whose distance is inferior to weldRadius will be merged
  (ie. one of them will be removed, and the other replaced by the barycenter).
  The indicesMap is constructed to allow remapping of indices lists with the
  simple rule: newVertexIndex:=indicesMap[oldVertexIndex].
  The logic is protected from chain welding, and only vertices that were
  initially closer than weldRadius will be welded in the same resulting vertex.
  This procedure can be used for mesh simplification, but preferably at design-time
  for it is not optimized for speed. This is more a "fixing" utility for meshes
  exported from high-polycount CAD tools (to remove duplicate vertices,
  quantification errors, etc.) 
*)
procedure WeldVertices(vertices: TGLAffineVectorList; indicesMap: TGLIntegerList;
  weldRadius: Single);
(* 
  Attempts to create as few as possible triangle strips to cover the mesh.
  The indices parameters define a set of triangles as a set of indices to
  vertices in a vertex pool, free of duplicate vertices (or resulting
  stripification will be of lower quality).
  The function returns a list of TGLIntegerList, each of these lists hosting
  a triangle strip, returned objects must be freed by caller.
  If agglomerateLoneTriangles is True, the first of the lists actually contains
  the agglomerated list of the triangles that couldn't be stripified. 
*)
function StripifyMesh(indices: TGLIntegerList; maxVertexIndex: Integer;
  agglomerateLoneTriangles: Boolean = False): TGLPersistentObjectList;
(* 
  Increases indices coherency wrt vertex caches.
  The indices parameters is understood as vertex indices of a triangles set,
  the triangles are reordered to maximize coherency (vertex reuse) over the
  cacheSize latest indices. This allows higher rendering performance from
  hardware renderers that implement vertex cache (nVidia GeForce family f.i.),
  allowing reuse of T&amp;L performance (similar to stripification without
  the normals issues of strips).
  This procedure performs a coherency optimization via a greedy hill-climber
  algorithm (ie. not optimal but fast). 
*)
procedure IncreaseCoherency(indices: TGLIntegerList; cacheSize: Integer);

type
  TSubdivideEdgeEvent = procedure(const idxA, idxB, newIdx: Integer); register;
(* 
  Subdivides mesh triangles.
  Splits along edges, each triangle becomes four. The smoothFactor can be
  used to control subdivision smoothing, zero means no smoothing (tesselation
  only), while 1 means "sphere" subdivision (a low res sphere will be subdivided
  in a higher-res sphere), values outside of the [0..1] range are for, er,
  artistic purposes.
  The procedure is not intended for real-time use. 
*)
procedure SubdivideTriangles(smoothFactor: Single; vertices: TGLAffineVectorList;
  triangleIndices: TGLIntegerList; normals: TGLAffineVectorList = nil;
  onSubdivideEdge: TSubdivideEdgeEvent = nil);
// Create list of indices of triangles with adjacency from triangle list 
function MakeTriangleAdjacencyList(const AindicesList: PLongWordArray;
  Count: LongWord; const AVerticesList: PAffineVectorArray): TGLLongWordList;

var
  vImprovedFixingOpenTriangleEdge: Boolean = False;
  vEdgeInfoReserveSize: LongWord = 64;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

var
  v0to255reciproquals: array of Single;

function Get0to255reciproquals: PSingleArray;
var
  i: Integer;
begin
  if Length(v0to255reciproquals) <> 256 then
  begin
    SetLength(v0to255reciproquals, 256);
    for i := 1 to 255 do
      v0to255reciproquals[i] := 1 / i;
  end;
  Result := @v0to255reciproquals[0];
end;

procedure ConvertStripToList(const strip: TGLAffineVectorList;
  list: TGLAffineVectorList);
var
  i: Integer;
  stripList: PAffineVectorArray;
begin
  list.AdjustCapacityToAtLeast(list.Count + 3 * (strip.Count - 2));
  stripList := strip.list;
  for i := 0 to strip.Count - 3 do
  begin
    if (i and 1) = 0 then
      list.Add(stripList[i + 0], stripList[i + 1], stripList[i + 2])
    else
      list.Add(stripList[i + 2], stripList[i + 1], stripList[i + 0]);
  end;
end;

procedure ConvertStripToList(const strip: TGLIntegerList; list: TGLIntegerList);
var
  i: Integer;
  stripList: PIntegerArray;
begin
  list.AdjustCapacityToAtLeast(list.Count + 3 * (strip.Count - 2));
  stripList := strip.list;
  for i := 0 to strip.Count - 3 do
  begin
    if (i and 1) = 0 then
      list.Add(stripList[i + 0], stripList[i + 1], stripList[i + 2])
    else
      list.Add(stripList[i + 2], stripList[i + 1], stripList[i + 0]);
  end;
end;

procedure ConvertStripToList(const strip: TGLAffineVectorList;
  const indices: TGLIntegerList; list: TGLAffineVectorList);
var
  i: Integer;
  stripList: PAffineVectorArray;
begin
  list.AdjustCapacityToAtLeast(list.Count + 3 * (indices.Count - 2));
  stripList := strip.list;
  for i := 0 to indices.Count - 3 do
  begin
    if (i and 1) = 0 then
      list.Add(stripList[indices[i + 0]], stripList[indices[i + 1]],
        stripList[indices[i + 2]])
    else
      list.Add(stripList[indices[i + 2]], stripList[indices[i + 1]],
        stripList[indices[i + 0]])
  end;
end;

procedure ConvertIndexedListToList(const data: TGLAffineVectorList;
  const indices: TGLIntegerList; list: TGLAffineVectorList);
var
  i: Integer;
  indicesList: PIntegerArray;
  dataList, listList: PAffineVectorArray;
  oldResetMem: Boolean;
begin
  Assert(data <> list); // this is not allowed

  oldResetMem := list.SetCountResetsMemory;
  list.SetCountResetsMemory := False;

  list.Count := indices.Count;

  list.SetCountResetsMemory := oldResetMem;

  indicesList := indices.list;
  dataList := data.list;
  listList := list.list;

  for i := 0 to indices.Count - 1 do
    listList[i] := dataList[indicesList[i]];
end;

function BuildVectorCountOptimizedIndices(const vertices: TGLAffineVectorList;
  const normals: TGLAffineVectorList = nil;
  const texCoords: TGLAffineVectorList = nil): TGLIntegerList;
var
  i, j, k: Integer;
  found: Boolean;
  hashSize: Integer;
  hashTable: array of TGLIntegerList;
  list: TGLIntegerList;
  verticesList, normalsList, texCoordsList: PAffineVectorArray;
const
  cVerticesPerHashKey = 48;
  cInvVerticesPerHashKey = 1 / cVerticesPerHashKey;

  function HashKey(const v: TAffineVector; hashSize: Integer): Integer;
  begin
    Result := ((Integer(PIntegerArray(@v)[0]) xor Integer(PIntegerArray(@v)[1])
      xor Integer(PIntegerArray(@v)[2])) shr 16) and hashSize;
  end;

begin
  Result := TGLIntegerList.Create;
  Result.Capacity := vertices.Count;

  if Assigned(normals) then
  begin
    Assert(normals.Count >= vertices.Count);
    normalsList := normals.list
  end
  else
    normalsList := nil;
  if Assigned(texCoords) then
  begin
    Assert(texCoords.Count >= vertices.Count);
    texCoordsList := texCoords.list
  end
  else
    texCoordsList := nil;

  verticesList := vertices.list;

  // This method is very fast, at the price of memory requirement its
  // probable complexity is only O(n) (it's a kind of bucket-sort hellspawn)

  // Initialize data structures for a hash table
  // (each vertex will only be compared to vertices of similar hash value)
  hashSize := (1 shl MaxInteger(Integer(0),
    Integer(Trunc(Log2(vertices.Count * cInvVerticesPerHashKey))))) - 1;
  if hashSize < 7 then
    hashSize := 7;
  if hashSize > 65535 then
    hashSize := 65535;
  SetLength(hashTable, hashSize + 1);
  // allocate and fill our hashtable (will store "reference" vertex indices)
  for i := 0 to hashSize do
  begin
    hashTable[i] := TGLIntegerList.Create;
    hashTable[i].GrowthDelta := cVerticesPerHashKey div 2;
  end;
  // here we go for all vertices
  if Assigned(texCoordsList) or Assigned(normalsList) then
  begin
    for i := 0 to vertices.Count - 1 do
    begin
      list := hashTable[HashKey(verticesList[i], hashSize)];
      found := False;
      // Check each vertex against its hashkey siblings
      if list.Count > 0 then
      begin
        if Assigned(texCoordsList) then
        begin
          if Assigned(normalsList) then
          begin
            for j := 0 to list.Count - 1 do
            begin
              k := list.list[j];
              if VectorEquals(verticesList[k], verticesList[i]) and
                VectorEquals(normalsList[k], normalsList[i]) and
                VectorEquals(texCoordsList[k], texCoordsList[i]) then
              begin
                // vertex known, just store its index
                Result.Add(k);
                found := True;
                Break;
              end;
            end;
          end
          else
          begin
            for j := 0 to list.Count - 1 do
            begin
              k := list.list[j];
              if VectorEquals(verticesList[k], verticesList[i]) and
                VectorEquals(texCoordsList[k], texCoordsList[i]) then
              begin
                // vertex known, just store its index
                Result.Add(k);
                found := True;
                Break;
              end;
            end;
          end;
        end
        else
        begin
          for j := 0 to list.Count - 1 do
          begin
            k := list.list[j];
            if VectorEquals(verticesList[k], verticesList[i]) and
              VectorEquals(normalsList[k], normalsList[i]) then
            begin
              // vertex known, just store its index
              Result.Add(k);
              found := True;
              Break;
            end;
          end;
        end;
      end;
      if not found then
      begin
        // vertex unknown, store index and add to the hashTable's list
        list.Add(i);
        Result.Add(i);
      end;
    end;
  end
  else
  begin
    for i := 0 to vertices.Count - 1 do
    begin
      list := hashTable[HashKey(verticesList[i], hashSize)];
      found := False;
      // Check each vertex against its hashkey siblings
      for j := 0 to list.Count - 1 do
      begin
        k := list.list[j];
        if VectorEquals(verticesList[k], verticesList[i]) then
        begin
          // vertex known, just store its index
          Result.Add(k);
          found := True;
          Break;
        end;
      end;
      if not found then
      begin
        // vertex unknown, store index and add to the hashTable's list
        list.Add(i);
        Result.Add(i);
      end;
    end;
  end;
  // free hash data
  for i := 0 to hashSize do
    hashTable[i].Free;
  SetLength(hashTable, 0);
end;

// RemapReferences (vectors)
//
procedure RemapReferences(reference: TGLAffineVectorList;
  const indices: TGLIntegerList);
var
  i: Integer;
  tag: array of Byte;
  refListI, refListN: PAffineVector;
  indicesList: PIntegerArray;
begin
  Assert(reference.Count = indices.Count);
  SetLength(tag, reference.Count);
  indicesList := indices.list;
  // 1st step, tag all used references
  for i := 0 to indices.Count - 1 do
    tag[indicesList[i]] := 1;
  // 2nd step, build remap indices and cleanup references
  refListI := @reference.list[0];
  refListN := refListI;
  for i := 0 to High(tag) do
  begin
    if tag[i] <> 0 then
    begin
      if refListN <> refListI then
        refListN^ := refListI^;
      Inc(refListN);
    end;
    Inc(refListI);
  end;
  reference.Count := (Cardinal(refListN) - Cardinal(@reference.list[0]))
    div SizeOf(TAffineVector);
end;

procedure RemapReferences(reference: TGLIntegerList; const indices: TGLIntegerList);
var
  i, n: Integer;
  tag: array of Byte;
  refList: PIntegerArray;
  indicesList: PIntegerArray;
begin
  Assert(reference.Count = indices.Count);
  SetLength(tag, reference.Count);
  indicesList := indices.list;
  // 1st step, tag all used references
  for i := 0 to indices.Count - 1 do
    tag[indicesList[i]] := 1;
  // 2nd step, build remap indices and cleanup references
  n := 0;
  refList := reference.list;
  for i := 0 to High(tag) do
  begin
    if tag[i] <> 0 then
    begin
      if n <> i then
        refList[n] := refList[i];
      Inc(n);
    end;
  end;
  reference.Count := n;
end;

procedure RemapAndCleanupReferences(reference: TGLAffineVectorList;
  indices: TGLIntegerList);
var
  i, n: Integer;
  tag: array of Integer;
  refList: PAffineVectorArray;
  indicesList: PIntegerArray;
begin
  Assert(reference.Count = indices.Count);
  SetLength(tag, reference.Count);
  indicesList := indices.list;
  // 1st step, tag all used references
  for i := 0 to indices.Count - 1 do
    tag[indicesList[i]] := 1;
  // 2nd step, build remap indices and cleanup references
  n := 0;
  refList := reference.list;
  for i := 0 to High(tag) do
  begin
    if tag[i] <> 0 then
    begin
      tag[i] := n;
      if n <> i then
        refList[n] := refList[i];
      Inc(n);
    end;
  end;
  reference.Count := n;
  // 3rd step, remap indices
  for i := 0 to indices.Count - 1 do
    indicesList[i] := tag[indicesList[i]];
end;

function RemapIndicesToIndicesMap(remapIndices: TGLIntegerList): TGLIntegerList;
var
  i, n: Integer;
  tag: array of Integer;
  remapList, indicesMap: PIntegerArray;
begin
  SetLength(tag, remapIndices.Count);
  // 1st step, tag all used indices
  remapList := remapIndices.list;
  for i := 0 to remapIndices.Count - 1 do
    tag[remapList[i]] := 1;
  // 2nd step, build indices offset table
  n := 0;
  for i := 0 to remapIndices.Count - 1 do
  begin
    if tag[i] > 0 then
    begin
      tag[i] := n;
      Inc(n);
    end;
  end;
  // 3rd step, fillup indices map
  Result := TGLIntegerList.Create;
  Result.Count := remapIndices.Count;
  indicesMap := Result.list;
  for i := 0 to Result.Count - 1 do
    indicesMap[i] := tag[remapList[i]];
end;

procedure RemapTrianglesIndices(indices, indicesMap: TGLIntegerList);
var
  i, k, a, b, c, n: Integer;
begin
  Assert((indices.Count mod 3) = 0); // must be a multiple of 3
  n := indices.Count;
  i := 0;
  k := 0;
  while i < n do
  begin
    a := indicesMap[indices[i]];
    b := indicesMap[indices[i + 1]];
    c := indicesMap[indices[i + 2]];
    if (a <> b) and (b <> c) and (a <> c) then
    begin
      indices[k] := a;
      indices[k + 1] := b;
      indices[k + 2] := c;
      Inc(k, 3);
    end;
    Inc(i, 3);
  end;
  indices.Count := k;
end;

procedure remapIndices(indices, indicesMap: TGLIntegerList);
var
  i: Integer;
  map, ind: PIntegerArray;
begin
  ind := indices.list;
  map := indicesMap.list;
  for i := 0 to indices.Count - 1 do
    ind[i] := map[ind[i]];
end;

procedure UnifyTrianglesWinding(indices: TGLIntegerList);
var
  nbTris: Integer;
  mark: array of ByteBool; // marks triangles that have been processed
  triangleStack: TGLIntegerList; // marks triangles winded, that must be processed

  procedure TestRewind(a, b: Integer);
  var
    i, n: Integer;
    x, y, z: Integer;
  begin
    i := indices.Count - 3;
    n := nbTris - 1;
    while i > 0 do
    begin
      if not mark[n] then
      begin
        x := indices[i];
        y := indices[i + 1];
        z := indices[i + 2];
        if ((x = a) and (y = b)) or ((y = a) and (z = b)) or
          ((z = a) and (x = b)) then
        begin
          indices.Exchange(i, i + 2);
          mark[n] := True;
          triangleStack.Push(n);
        end
        else if ((x = b) and (y = a)) or ((y = b) and (z = a)) or
          ((z = b) and (x = a)) then
        begin
          mark[n] := True;
          triangleStack.Push(n);
        end;
      end;
      Dec(i, 3);
      Dec(n);
    end;
  end;

  procedure ProcessTriangleStack;
  var
    n, i: Integer;
  begin
    while triangleStack.Count > 0 do
    begin
      // get triangle, it is *assumed* properly winded
      n := triangleStack.Pop;
      i := n * 3;
      mark[n] := True;
      // rewind neighbours
      TestRewind(indices[i + 0], indices[i + 1]);
      TestRewind(indices[i + 1], indices[i + 2]);
      TestRewind(indices[i + 2], indices[i + 0]);
    end;
  end;

var
  n: Integer;
begin
  nbTris := indices.Count div 3;
  SetLength(mark, nbTris);
  // Build connectivity data
  triangleStack := TGLIntegerList.Create;
  try
    triangleStack.Capacity := nbTris div 4;
    // Pick a triangle, adjust normals of neighboring triangles, recurse
    for n := 0 to nbTris - 1 do
    begin
      if mark[n] then
        Continue;
      triangleStack.Push(n);
      ProcessTriangleStack;
    end;
  finally
    triangleStack.Free;
  end;
end;

procedure InvertTrianglesWinding(indices: TGLIntegerList);
var
  i: Integer;
begin
  Assert((indices.Count mod 3) = 0);
  i := indices.Count - 3;
  while i >= 0 do
  begin
    indices.Exchange(i, i + 2);
    Dec(i, 3);
  end;
end;

function BuildNormals(reference: TGLAffineVectorList; indices: TGLIntegerList)
  : TGLAffineVectorList;
var
  i, n, k: Integer;
  normalsCount: array of Byte;
  v: TAffineVector;
  refList, resultList: PAffineVectorArray;
  indicesList: PIntegerArray;
  reciproquals: PSingleArray;
begin
  Result := TGLAffineVectorList.Create;
  Result.Count := reference.Count;
  SetLength(normalsCount, reference.Count);
  refList := reference.list;
  indicesList := indices.list;
  resultList := Result.list;
  // 1st step, calculate triangle normals and sum
  i := 0;
  while i < indices.Count do
  begin
    v := CalcPlaneNormal(refList[indicesList[i]], refList[indicesList[i + 1]],
      refList[indicesList[i + 2]]);
    for n := i to i + 2 do
    begin
      k := indicesList[n];
      AddVector(resultList[k], v);
      Inc(normalsCount[k]);
    end;
    Inc(i, 3);
  end;
  // 2nd step, average normals
  reciproquals := Get0to255reciproquals;
  for i := 0 to reference.Count - 1 do
    ScaleVector(resultList[i], reciproquals[normalsCount[i]]);
end;

//----------------------------------------------------------
function BuildNonOrientedEdgesList(triangleIndices: TGLIntegerList;
  triangleEdges: TGLIntegerList = nil; edgesTriangles: TGLIntegerList = nil)
  : TGLIntegerList;
const
  cEdgesHashMax = 127; // must be a power of two minus 1
var
  edgesHash: array [0 .. cEdgesHashMax] of TGLIntegerList;
  curTri: Integer;
  edges: TGLIntegerList;

  function ProcessEdge(a, b: Integer): Integer;
  var
    i, n: Integer;
    HashKey: Integer;
    edgesList, iList: PIntegerArray;
    hashList: TGLIntegerList;
  begin
    if a >= b then
    begin
      i := a;
      a := b;
      b := i;
    end;
    HashKey := (a xor b) and cEdgesHashMax;
    hashList := edgesHash[HashKey];
    edgesList := edges.list;
    iList := hashList.list;
    for i := 0 to hashList.Count - 1 do
    begin
      n := iList[i];
      if (edgesList[n] = a) and (edgesList[n + 1] = b) then
      begin
        Result := n;
        Exit;
      end;
    end;
    Result := edges.Count;
    hashList.Add(Result);
    edges.Add(a, b);
  end;

  function ProcessEdge2(a, b: Integer): Integer;
  var
    n: Integer;
    HashKey: Integer;
    edgesList: PIntegerArray;
    iList, iListEnd: PInteger;
    hashList: TGLIntegerList;
  begin
    if a >= b then
    begin
      n := a;
      a := b;
      b := n;
    end;
    HashKey := (a xor (b shl 1)) and cEdgesHashMax;
    edgesList := edges.list;
    hashList := edgesHash[HashKey];
    iList := @hashList.list[0];
    iListEnd := @hashList.list[hashList.Count];
    while Cardinal(iList) < Cardinal(iListEnd) do
    begin
      n := iList^;
      if (edgesList[n] = a) and (edgesList[n + 1] = b) then
      begin
        edgesTriangles[n + 1] := curTri;
        Result := n;
        Exit;
      end;
      Inc(iList);
    end;
    Result := edges.Count;
    hashList.Add(Result);
    edges.Add(a, b);
    edgesTriangles.Add(curTri, -1);
  end;

var
  j, k: Integer;
  triIndicesList: PIntegerArray;
begin
  Result := TGLIntegerList.Create;
  Result.Capacity := 1024;
  Result.GrowthDelta := 1024;
  if Assigned(triangleEdges) then
    triangleEdges.Count := triangleIndices.Count;
  if Assigned(edgesTriangles) then
    edgesTriangles.Count := 0;
  // Creates Hash
  k := (triangleIndices.Count div (cEdgesHashMax + 1)) + 128;
  for j := 0 to High(edgesHash) do
  begin
    edgesHash[j] := TGLIntegerList.Create;
    edgesHash[j].Capacity := k;
  end;
  // collect all edges
  curTri := 0;
  triIndicesList := triangleIndices.list;
  edges := Result;
  if Assigned(triangleEdges) then
  begin
    if Assigned(edgesTriangles) then
    begin
      while curTri < triangleIndices.Count do
      begin
        triangleEdges[curTri] := ProcessEdge2(triIndicesList[curTri],
          triIndicesList[curTri + 1]);
        triangleEdges[curTri + 1] := ProcessEdge2(triIndicesList[curTri + 1],
          triIndicesList[curTri + 2]);
        triangleEdges[curTri + 2] := ProcessEdge2(triIndicesList[curTri + 2],
          triIndicesList[curTri]);
        Inc(curTri, 3);
      end;
    end
    else
    begin
      while curTri < triangleIndices.Count do
      begin
        triangleEdges[curTri] := ProcessEdge(triIndicesList[curTri],
          triIndicesList[curTri + 1]);
        triangleEdges[curTri + 1] := ProcessEdge(triIndicesList[curTri + 1],
          triIndicesList[curTri + 2]);
        triangleEdges[curTri + 2] := ProcessEdge(triIndicesList[curTri + 2],
          triIndicesList[curTri]);
        Inc(curTri, 3);
      end;
    end;
  end
  else
  begin
    if Assigned(edgesTriangles) then
    begin
      while curTri < triangleIndices.Count do
      begin
        ProcessEdge2(triIndicesList[curTri], triIndicesList[curTri + 1]);
        ProcessEdge2(triIndicesList[curTri + 1], triIndicesList[curTri + 2]);
        ProcessEdge2(triIndicesList[curTri + 2], triIndicesList[curTri]);
        Inc(curTri, 3);
      end;
    end
    else
    begin
      while curTri < triangleIndices.Count do
      begin
        ProcessEdge(triIndicesList[curTri], triIndicesList[curTri + 1]);
        ProcessEdge(triIndicesList[curTri + 1], triIndicesList[curTri + 2]);
        ProcessEdge(triIndicesList[curTri + 2], triIndicesList[curTri]);
        Inc(curTri, 3);
      end;
    end;
  end;
  // remove Hash
  for j := 0 to High(edgesHash) do
    edgesHash[j].Free;
end;

procedure IncreaseCoherency(indices: TGLIntegerList; cacheSize: Integer);
var
  i, n, maxVertex, bestCandidate, bestScore, candidateIdx,
    lastCandidate: Integer;
  trisOfVertex: array of TGLIntegerList;
  candidates: TGLIntegerList;
  indicesList: PIntegerArray;
begin
  // Alloc lookup structure
  maxVertex := indices.MaxInteger;
  SetLength(trisOfVertex, maxVertex + 1);
  for i := 0 to High(trisOfVertex) do
    trisOfVertex[i] := TGLIntegerList.Create;
  candidates := TGLIntegerList.Create;
  indicesList := PIntegerArray(indices.list);
  // Fillup lookup structure
  i := 0;
  while i < indices.Count do
  begin
    trisOfVertex[indicesList[i + 0]].Add(i);
    trisOfVertex[indicesList[i + 1]].Add(i);
    trisOfVertex[indicesList[i + 2]].Add(i);
    Inc(i, 3);
  end;
  // Optimize
  i := 0;
  while i < indices.Count do
  begin
    n := i - cacheSize;
    if n < 0 then
      n := 0;
    candidates.Count := 0;
    while n < i do
    begin
      candidates.Add(trisOfVertex[indicesList[n]]);
      Inc(n);
    end;
    bestCandidate := -1;
    if candidates.Count > 0 then
    begin
      candidateIdx := 0;
      bestScore := 0;
      candidates.Sort;
      lastCandidate := candidates.list[0];
      for n := 1 to candidates.Count - 1 do
      begin
        if candidates.list[n] <> lastCandidate then
        begin
          if n - candidateIdx > bestScore then
          begin
            bestScore := n - candidateIdx;
            bestCandidate := lastCandidate;
          end;
          lastCandidate := candidates.list[n];
          candidateIdx := n;
        end;
      end;
      if candidates.Count - candidateIdx > bestScore then
        bestCandidate := lastCandidate;
    end;
    if bestCandidate >= 0 then
    begin
      trisOfVertex[indicesList[i + 0]].Remove(i);
      trisOfVertex[indicesList[i + 1]].Remove(i);
      trisOfVertex[indicesList[i + 2]].Remove(i);
      trisOfVertex[indicesList[bestCandidate + 0]].Remove(bestCandidate);
      trisOfVertex[indicesList[bestCandidate + 1]].Remove(bestCandidate);
      trisOfVertex[indicesList[bestCandidate + 2]].Remove(bestCandidate);
      trisOfVertex[indicesList[i + 0]].Add(bestCandidate);
      trisOfVertex[indicesList[i + 1]].Add(bestCandidate);
      trisOfVertex[indicesList[i + 2]].Add(bestCandidate);
      indices.Exchange(bestCandidate + 0, i + 0);
      indices.Exchange(bestCandidate + 1, i + 1);
      indices.Exchange(bestCandidate + 2, i + 2);
    end
    else
    begin
      trisOfVertex[indicesList[i + 0]].Remove(i);
      trisOfVertex[indicesList[i + 1]].Remove(i);
      trisOfVertex[indicesList[i + 2]].Remove(i);
    end;
    Inc(i, 3);
  end;
  // Release lookup structure
  candidates.Free;
  for i := 0 to High(trisOfVertex) do
    trisOfVertex[i].Free;
end;

procedure WeldVertices(vertices: TGLAffineVectorList; indicesMap: TGLIntegerList;
  weldRadius: Single);
var
  i, j, n, k: Integer;
  pivot: PAffineVector;
  sum: TAffineVector;
  wr2: Single;
  mark: packed array of ByteBool;
begin
  indicesMap.Count := vertices.Count;
  SetLength(mark, vertices.Count);
  wr2 := Sqr(weldRadius);
  // mark duplicates, compute barycenters and indicesMap
  i := 0;
  k := 0;
  while i < vertices.Count do
  begin
    if not mark[i] then
    begin
      pivot := @vertices.list[i];
      indicesMap[i] := k;
      n := 0;
      j := vertices.Count - 1;
      while j > i do
      begin
        if not mark[j] then
        begin
          if VectorDistance2(pivot^, vertices.list[j]) <= wr2 then
          begin
            if n = 0 then
            begin
              sum := VectorAdd(pivot^, vertices.list[j]);
              n := 2;
            end
            else
            begin
              AddVector(sum, vertices.list[j]);
              Inc(n);
            end;
            indicesMap[j] := k;
            mark[j] := True;
          end;
        end;
        Dec(j);
      end;
      if n > 0 then
        vertices.list[i] := VectorScale(sum, 1 / n);
      Inc(k);
    end;
    Inc(i);
  end;
  // pack vertices list
  k := 0;
  for i := 0 to vertices.Count - 1 do
  begin
    if not mark[i] then
    begin
      vertices.list[k] := vertices.list[i];
      Inc(k);
    end;
  end;
  vertices.Count := k;
end;

function StripifyMesh(indices: TGLIntegerList; maxVertexIndex: Integer;
  agglomerateLoneTriangles: Boolean = False): TGLPersistentObjectList;
var
  accountedTriangles: array of ByteBool;
  vertexTris: array of TGLIntegerList;
  indicesList: PIntegerArray;
  indicesCount: Integer;
  currentStrip: TGLIntegerList;
  nextTriangle, nextVertex: Integer;

  function FindTriangleWithEdge(vertA, vertB: Integer): Boolean;
  var
    i, n: Integer;
    p: PIntegerArray;
    list: TGLIntegerList;
  begin
    Result := False;
    list := vertexTris[vertA];
    for n := 0 to list.Count - 1 do
    begin
      i := list.list[n];
      if not(accountedTriangles[i]) then
      begin
        p := @indicesList[i];
        if (p[0] = vertA) and (p[1] = vertB) then
        begin
          Result := True;
          nextVertex := p[2];
          nextTriangle := i;
          Break;
        end
        else if (p[1] = vertA) and (p[2] = vertB) then
        begin
          Result := True;
          nextVertex := p[0];
          nextTriangle := i;
          Break;
        end
        else if (p[2] = vertA) and (p[0] = vertB) then
        begin
          Result := True;
          nextVertex := p[1];
          nextTriangle := i;
          Break;
        end;
      end;
    end;

  end;

  procedure BuildStrip(vertA, vertB: Integer);
  var
    vertC: Integer;
  begin
    currentStrip.Add(vertA, vertB);
    repeat
      vertC := nextVertex;
      currentStrip.Add(vertC);
      accountedTriangles[nextTriangle] := True;
      if not FindTriangleWithEdge(vertB, vertC) then
        Break;
      currentStrip.Add(nextVertex);
      accountedTriangles[nextTriangle] := True;
      vertB := nextVertex;
      vertA := vertC;
    until not FindTriangleWithEdge(vertB, vertA);
  end;

var
  i, n, triangle: Integer;
  loneTriangles: TGLIntegerList;
begin
  Assert((indices.Count mod 3) = 0, 'indices count is not a multiple of 3!');
  Result := TGLPersistentObjectList.Create;
  // direct access and cache vars
  indicesList := indices.list;
  indicesCount := indices.Count;
  // Build adjacency lookup table (vertex based, not triangle based)
  SetLength(vertexTris, maxVertexIndex + 1);
  for i := 0 to High(vertexTris) do
    vertexTris[i] := TGLIntegerList.Create;
  n := 0;
  triangle := 0;
  for i := 0 to indicesCount - 1 do
  begin
    vertexTris[indicesList[i]].Add(triangle);
    if n = 2 then
    begin
      n := 0;
      Inc(triangle, 3);
    end
    else
      Inc(n);
  end;
  // Now, we use a greedy algo to build triangle strips
  SetLength(accountedTriangles, indicesCount); // yeah, waste of memory
  if agglomerateLoneTriangles then
  begin
    loneTriangles := TGLIntegerList.Create;
    Result.Add(loneTriangles);
  end
  else
    loneTriangles := nil;
  i := 0;
  while i < indicesCount do
  begin
    if not accountedTriangles[i] then
    begin
      accountedTriangles[i] := True;
      if FindTriangleWithEdge(indicesList[i + 1], indicesList[i]) then
      begin
        currentStrip := TGLIntegerList.Create;
        currentStrip.Add(indicesList[i + 2]);
        BuildStrip(indicesList[i], indicesList[i + 1]);
      end
      else if FindTriangleWithEdge(indicesList[i + 2], indicesList[i + 1]) then
      begin
        currentStrip := TGLIntegerList.Create;
        currentStrip.Add(indicesList[i]);
        BuildStrip(indicesList[i + 1], indicesList[i + 2]);
      end
      else if FindTriangleWithEdge(indicesList[i], indicesList[i + 2]) then
      begin
        currentStrip := TGLIntegerList.Create;
        currentStrip.Add(indicesList[i + 1]);
        BuildStrip(indicesList[i + 2], indicesList[i]);
      end
      else
      begin
        if agglomerateLoneTriangles then
          currentStrip := loneTriangles
        else
          currentStrip := TGLIntegerList.Create;
        currentStrip.Add(indicesList[i], indicesList[i + 1],
          indicesList[i + 2]);
      end;
      if currentStrip <> loneTriangles then
        Result.Add(currentStrip);
    end;
    Inc(i, 3);
  end;
  // cleanup
  for i := 0 to High(vertexTris) do
    vertexTris[i].Free;
end;

procedure SubdivideTriangles(smoothFactor: Single; vertices: TGLAffineVectorList;
  triangleIndices: TGLIntegerList; normals: TGLAffineVectorList = nil;
  onSubdivideEdge: TSubdivideEdgeEvent = nil);
var
  i, a, b, c, nv: Integer;
  edges: TGLIntegerList;
  triangleEdges: TGLIntegerList;
  p, n: TAffineVector;
  f: Single;
begin
  // build edges list
  triangleEdges := TGLIntegerList.Create;
  try
    edges := BuildNonOrientedEdgesList(triangleIndices, triangleEdges);
    try
      nv := vertices.Count;
      // split all edges, add corresponding vertex & normal
      i := 0;
      while i < edges.Count do
      begin
        a := edges[i];
        b := edges[i + 1];
        p := VectorLerp(vertices[a], vertices[b], 0.5);
        if Assigned(normals) then
        begin
          n := VectorNormalize(VectorLerp(normals[a], normals[b], 0.5));
          normals.Add(n);
          if smoothFactor <> 0 then
          begin
            f := 0.25 * smoothFactor * VectorDistance(vertices[a], vertices[b])
              * (1 - VectorDotProduct(normals[a], normals[b]));
            if VectorDotProduct(normals[a], VectorSubtract(vertices[b],
              vertices[a])) + VectorDotProduct(normals[b],
              VectorSubtract(vertices[a], vertices[b])) > 0 then
              f := -f;
            CombineVector(p, n, f);
          end;
        end;
        if Assigned(onSubdivideEdge) then
          onSubdivideEdge(a, b, vertices.Add(p))
        else
          vertices.Add(p);
        Inc(i, 2);
      end;
      // spawn new triangles geometry
      i := triangleIndices.Count - 3;
      while i >= 0 do
      begin
        a := nv + triangleEdges[i + 0] div 2;
        b := nv + triangleEdges[i + 1] div 2;
        c := nv + triangleEdges[i + 2] div 2;
        triangleIndices.Add(triangleIndices[i + 0], a, c);
        triangleIndices.Add(a, triangleIndices[i + 1], b);
        triangleIndices.Add(b, triangleIndices[i + 2], c);
        triangleIndices[i + 0] := a;
        triangleIndices[i + 1] := b;
        triangleIndices[i + 2] := c;
        Dec(i, 3);
      end;
    finally
      edges.Free;
    end;
  finally
    triangleEdges.Free;
  end;
end;

type

  TTriangleEdgeInfo = record
    adjacentTriangle: array [0 .. 2] of LongWord;
    // Bits 0:1 is edge number of adjacent triangle 0
    // Bits 2:3 is edge number of adjacent triangle 1
    // Bits 4:5 is edge number of adjacent triangle 2
    adjacentTriangleEdges: Byte;
    openEdgeMask: Byte;
  end;

  TTriangleEdgeInfoArray = array of TTriangleEdgeInfo;

  TTriangleBoundary = record
    vertexIndex: LongWord;
    triangle: LongWord;
    edge: LongWord;
    prev: LongWord;
    next: array [0 .. 2] of LongWord;
    active: LongWord;
    maxSqArea: Single;
  end;

  TTriangleBoundaryArray = array of TTriangleBoundary;

  TVector3dw = array [0 .. 2] of LongWord;
  PVector3dw = ^TVector3dw;

var
  indicesList: PLongWordArray; // Reference to indices list of usual triangles
  verticesList: PAffineVectorArray; // Reference to vertices list
  PrimitiveNum: LongWord; // Number of triangles
  edgeInfo: TTriangleEdgeInfoArray;
  boundaryList: TTriangleBoundaryArray;

function sameVertex(i0, i1: LongWord): Boolean;
begin
  Result := (verticesList[i0].x = verticesList[i1].x) and
    (verticesList[i0].y = verticesList[i1].y) and
    (verticesList[i0].z = verticesList[i1].z);
end;

procedure joinTriangles(tri1: Integer; edge1: Cardinal; tri2: Integer;
  edge2: Cardinal);
begin
  Assert((edge1 < 3) and (edge2 < 3), 'joinTriangles: Multiple edge detected.');

  edgeInfo[tri1].adjacentTriangle[edge1] := tri2;
  edgeInfo[tri1].adjacentTriangleEdges := edgeInfo[tri1]
    .adjacentTriangleEdges and not(3 shl (2 * edge1));
  edgeInfo[tri1].adjacentTriangleEdges := edgeInfo[tri1]
    .adjacentTriangleEdges or (edge2 shl (2 * edge1));

  edgeInfo[tri2].adjacentTriangle[edge2] := tri1;
  edgeInfo[tri2].adjacentTriangleEdges := edgeInfo[tri2]
    .adjacentTriangleEdges and not(3 shl (2 * edge2));
  edgeInfo[tri2].adjacentTriangleEdges := edgeInfo[tri2]
    .adjacentTriangleEdges or (edge1 shl (2 * edge2));
end;

procedure matchWithTriangleSharingEdge(triangle, edge, v0, v1,
  otherv: LongWord);
var
  i, j: Integer;
  doubleTri: Integer;
  otherEdge: Integer;
  vertexIndex: PVector3dw;
begin
  doubleTri := -1;
  otherEdge := 0;
  // Match shared edges based on vertex numbers (relatively fast).
  for i := triangle + 1 to PrimitiveNum - 1 do
  begin
    j := i * 3;
    vertexIndex := @indicesList[j];

    if vertexIndex[0] = v0 then
      if vertexIndex[2] = v1 then
        if edgeInfo[i].adjacentTriangle[2] = $FFFFFFFF then
          if vertexIndex[1] = otherv then
          begin
            if (doubleTri < 0) then
            begin
              doubleTri := i;
              otherEdge := 2;
            end;
          end
          else
          begin
            joinTriangles(i, 2, triangle, edge);
            Exit;
          end;

    if vertexIndex[1] = v0 then
      if vertexIndex[0] = v1 then
        if edgeInfo[i].adjacentTriangle[0] = $FFFFFFFF then
          if vertexIndex[2] = otherv then
          begin
            if doubleTri < 0 then
            begin
              doubleTri := i;
              otherEdge := 0;
            end;
          end
          else
          begin
            joinTriangles(i, 0, triangle, edge);
            Exit;
          end;

    if vertexIndex[2] = v0 then
      if vertexIndex[1] = v1 then
        if edgeInfo[i].adjacentTriangle[1] = $FFFFFFFF then
          if vertexIndex[0] = otherv then
          begin
            if doubleTri < 0 then
            begin
              doubleTri := i;
              otherEdge := 1;
            end;
          end
          else
          begin
            joinTriangles(i, 1, triangle, edge);
            Exit;
          end;
  end;

  // Match shared edges based on vertex XYZ values (slow check).
  for i := triangle + 1 to PrimitiveNum - 1 do
  begin
    j := i * 3;
    vertexIndex := @indicesList[j];

    if sameVertex(vertexIndex[0], v0) then
      if sameVertex(vertexIndex[2], v1) then
        if edgeInfo[i].adjacentTriangle[2] = $FFFFFFFF then
          if vertexIndex[0] = otherv then
          begin
            if doubleTri < 0 then
            begin
              doubleTri := i;
              otherEdge := 2;
            end;
          end
          else
          begin
            joinTriangles(i, 2, triangle, edge);
            Exit;
          end;

    if sameVertex(vertexIndex[1], v0) then
      if sameVertex(vertexIndex[0], v1) then
        if edgeInfo[i].adjacentTriangle[0] = $FFFFFFFF then
          if vertexIndex[0] = otherv then
          begin
            if doubleTri < 0 then
            begin
              doubleTri := i;
              otherEdge := 0;
            end;
          end
          else
          begin
            joinTriangles(i, 0, triangle, edge);
            Exit;
          end;

    if sameVertex(vertexIndex[2], v0) then
      if sameVertex(vertexIndex[1], v1) then
        if edgeInfo[i].adjacentTriangle[1] = $FFFFFFFF then
          if vertexIndex[0] = otherv then
          begin
            if doubleTri < 0 then
            begin
              doubleTri := i;
              otherEdge := 1;
            end;
          end
          else
          begin
            joinTriangles(i, 1, triangle, edge);
            Exit;
          end;
  end;

  // Only connect a triangle to a triangle with the exact
  // same three vertices as a last resort.
  if doubleTri >= 0 then
    joinTriangles(doubleTri, otherEdge, triangle, edge);
end;

function ComputeTriangleEdgeInfo: Boolean;
var
  i, j: Integer;
  vertexIndex: PVector3dw;
begin
  Result := True;
  try
    // Initialize edge information as if all triangles are fully disconnected.
    for i := 0 to PrimitiveNum - 1 do
    begin
      edgeInfo[i].adjacentTriangle[0] := $FFFFFFFF; // Vertex 0,1 edge
      edgeInfo[i].adjacentTriangle[1] := $FFFFFFFF; // Vertex 1,2 edge
      edgeInfo[i].adjacentTriangle[2] := $FFFFFFFF; // Vertex 2,0 edge
      edgeInfo[i].adjacentTriangleEdges := (3 shl 0) or (3 shl 2) or (3 shl 4);
      edgeInfo[i].openEdgeMask := 0;
    end;

    for i := 0 to PrimitiveNum - 1 do
    begin
      j := i * 3;
      vertexIndex := @indicesList[j];
      if edgeInfo[i].adjacentTriangle[0] = $FFFFFFFF then
        matchWithTriangleSharingEdge(i, 0, vertexIndex[0], vertexIndex[1],
          vertexIndex[2]);
      if edgeInfo[i].adjacentTriangle[1] = $FFFFFFFF then
        matchWithTriangleSharingEdge(i, 1, vertexIndex[1], vertexIndex[2],
          vertexIndex[0]);
      if edgeInfo[i].adjacentTriangle[2] = $FFFFFFFF then
        matchWithTriangleSharingEdge(i, 2, vertexIndex[2], vertexIndex[0],
          vertexIndex[1]);
    end;
  except
    Result := False;
  end;
end;

procedure findOpenBoundary(triangle, edge: LongWord;
  var boundaryVertices: LongWord);
var
  v0, v, nextEdge, otherTriangle, Count: LongWord;
  i: Byte;
  finded: Boolean;
begin
  Count := 0;
  if (edgeInfo[triangle].openEdgeMask and (1 shl edge)) <> 0 then
    Exit;
  Assert(edgeInfo[triangle].adjacentTriangle[edge] = $FFFFFFFF);

  edgeInfo[triangle].openEdgeMask := edgeInfo[triangle].openEdgeMask or
    (1 shl edge);

  v0 := indicesList[3 * triangle + edge];
  boundaryList[Count].vertexIndex := v0;
  boundaryList[Count].triangle := triangle;
  boundaryList[Count].edge := edge;
  Inc(Count);

  nextEdge := (edge + 1) mod 3;
  v := indicesList[3 * triangle + nextEdge];
  while not sameVertex(v, v0) do
  begin
    otherTriangle := edgeInfo[triangle].adjacentTriangle[nextEdge];
    while otherTriangle <> $FFFFFFFF do
    begin
      finded := False;
      for i := 0 to 2 do
        if edgeInfo[otherTriangle].adjacentTriangle[i] = triangle then
        begin
          Assert(sameVertex(indicesList[3 * otherTriangle + (i + 1) mod 3], v));
          triangle := otherTriangle;
          nextEdge := (i + 1) mod 3;
          finded := True;
          Break;
        end;
      Assert(finded);
      otherTriangle := edgeInfo[triangle].adjacentTriangle[nextEdge];
    end;

    // Mark this edge as processed to avoid reprocessing
    // the boundary multiple times.
    edgeInfo[triangle].openEdgeMask := edgeInfo[triangle].openEdgeMask or
      (1 shl nextEdge);

    boundaryList[Count].vertexIndex := v;
    boundaryList[Count].triangle := triangle;
    boundaryList[Count].edge := nextEdge;
    Inc(Count);

    nextEdge := (nextEdge + 1) mod 3;
    v := indicesList[3 * triangle + nextEdge];
  end;
  boundaryVertices := Count;
end;

function polygonArea(boundaryIndex: LongWord): Single;
var
  // Md2TriangleVertex *v;
  d01, d02, prod: TVector3f;
  v0, v1, v2: Integer;
begin
  // Get the vertices of the triangle along the boundary.
  v0 := boundaryList[boundaryIndex].vertexIndex;
  v1 := boundaryList[boundaryList[boundaryIndex].next[0]].vertexIndex;
  v2 := boundaryList[boundaryList[boundaryIndex].next[1]].vertexIndex;

  // Compute the area of the triangle
  d01 := VectorSubtract(verticesList[v0], verticesList[v1]);
  d02 := VectorSubtract(verticesList[v0], verticesList[v2]);
  prod := VectorCrossProduct(d01, d02);
  Result := VectorLength(prod);
end;

procedure fixOpenTriangle(boundaryIndex: LongWord);
var
  newTriIndex, b0, bp, b1, b2: LongWord;
begin
  b0 := boundaryIndex;
  bp := boundaryList[b0].prev;
  b1 := boundaryList[b0].next[0];
  b2 := boundaryList[b0].next[1];

  Assert(boundaryList[b1].next[0] = b2);
  Assert(boundaryList[bp].next[0] = b0);
  Assert(boundaryList[bp].next[1] = b1);

  // Initialize the new triangle.
  indicesList[PrimitiveNum * 3 + 0] := boundaryList[b2].vertexIndex;
  indicesList[PrimitiveNum * 3 + 1] := boundaryList[b1].vertexIndex;
  indicesList[PrimitiveNum * 3 + 2] := boundaryList[b0].vertexIndex;
  Inc(PrimitiveNum);

  // Mark edge 2 unconnected
  newTriIndex := indicesList[PrimitiveNum * 3 - 3];
  edgeInfo[newTriIndex].adjacentTriangle[2] := $FFFFFFFF;
  edgeInfo[newTriIndex].adjacentTriangleEdges := 3 shl 4;

  // Make sure edges we are joining are currently unconnected.
  Assert(edgeInfo[boundaryList[b1].triangle].adjacentTriangle
    [boundaryList[b1].edge] = $FFFFFFFF);
  Assert(edgeInfo[boundaryList[b0].triangle].adjacentTriangle
    [boundaryList[b0].edge] = $FFFFFFFF);

  // Join the triangles with the new triangle.
  joinTriangles(newTriIndex, 0, boundaryList[b1].triangle,
    boundaryList[b1].edge);
  joinTriangles(newTriIndex, 1, boundaryList[b0].triangle,
    boundaryList[b0].edge);

  // Update the boundary list based on the addition of the new triangle.
  boundaryList[b0].triangle := newTriIndex;
  boundaryList[b0].edge := 2;
  boundaryList[b0].next[0] := b2;
  boundaryList[b0].next[1] := boundaryList[b2].next[0];
  boundaryList[b0].maxSqArea := GLS.MeshUtils.polygonArea(b0);

  boundaryList[bp].next[1] := b2;
  boundaryList[b1].active := 0;
  boundaryList[b2].prev := b0;
end;

procedure fixOpenBoundary(Count: LongWord);
var
  b0, b1, b2: LongWord;
  i: Integer;
  maxMaxSqArea: Single;
  numActive: LongWord;
  minIndex: LongWord;
  min: Single;
begin
  if Count = 1 then
    (* Ugh, a degenerate triangle with two (or perhaps three)
      identical vertices tricking us into thinking that there
      is an open edge.  Hopefully these should be eliminated
      by an earlier "eliminate" pass, but such triangles are
      harmless. *)
    Exit;

  Assert(Count >= 3);

  if Count = 3 then
  begin
    (* Often a common case.  Save bookkeeping and close the triangle
      boundary immediately. *)
    b0 := 0;
    b1 := 1;
    b2 := 2;
  end
  else
  begin
    minIndex := 0;

    boundaryList[0].prev := Count - 1;
    boundaryList[0].next[0] := 1;
    boundaryList[0].next[1] := 2;
    boundaryList[0].active := 1;

    for i := 1 to Count - 3 do
    begin
      boundaryList[i].prev := i - 1;
      boundaryList[i].next[0] := i + 1;
      boundaryList[i].next[1] := i + 2;
      boundaryList[i].active := 1;
    end;
    i := Count - 3;

    boundaryList[i].prev := i - 1;
    boundaryList[i].next[0] := i + 1;
    boundaryList[i].next[1] := 0;
    boundaryList[i].active := 1;

    boundaryList[i + 1].prev := i;
    boundaryList[i + 1].next[0] := 0;
    boundaryList[i + 1].next[1] := 1;
    boundaryList[i + 1].active := 1;

    boundaryList[0].maxSqArea := GLS.MeshUtils.polygonArea(0);
    maxMaxSqArea := boundaryList[0].maxSqArea;

    for i := 1 to Count - 1 do
    begin
      boundaryList[i].maxSqArea := GLS.MeshUtils.polygonArea(i);
      if boundaryList[i].maxSqArea > maxMaxSqArea then
        maxMaxSqArea := boundaryList[i].maxSqArea;
    end;

    (* If triangles are formed from adjacent edges along the
      boundary, at least front-facing such triangle should
      be front-facing (ie, have a non-negative area). *)

    Assert(maxMaxSqArea >= 0.0);
    maxMaxSqArea := 2.0 * maxMaxSqArea;
    numActive := Count;

    while numActive > 3 do
    begin
      min := maxMaxSqArea;
      for i := 0 to Count - 1 do
        if boundaryList[i].active > 0 then
          if boundaryList[i].maxSqArea < min then
            if boundaryList[i].maxSqArea >= 0.0 then
            begin
              min := boundaryList[i].maxSqArea;
              minIndex := i;
            end;

      Assert(min < maxMaxSqArea);
      fixOpenTriangle(minIndex);

      (* Newly created triangle formed from adjacent edges
        along the boundary could be larger than the
        previous largest triangle. *)
      if (boundaryList[minIndex].maxSqArea > maxMaxSqArea) then
        maxMaxSqArea := 2.0 * boundaryList[minIndex].maxSqArea;

      Dec(numActive);
    end;

    for i := 0 to Count - 1 do
      if boundaryList[i].active > 0 then
      begin
        minIndex := i;
        Break;
      end;

    Assert(LongWord(i) < Count);

    b0 := minIndex;
    b1 := boundaryList[b0].next[0];
    b2 := boundaryList[b0].next[1];

    Assert(boundaryList[b0].prev = b2);
    Assert(boundaryList[b1].prev = b0);
    Assert(boundaryList[b1].next[0] = b2);
    Assert(boundaryList[b1].next[1] = b0);
    Assert(boundaryList[b2].prev = b1);
    Assert(boundaryList[b2].next[0] = b0);
    Assert(boundaryList[b2].next[1] = b1);
  end;

  // Place final "keystone" triangle to fill completely the open boundary

  if LongWord(Length(edgeInfo)) < (PrimitiveNum + 1) then
    SetLength(edgeInfo, Length(edgeInfo) + Integer(vEdgeInfoReserveSize));

  // Initialize the new triangle.
  indicesList[PrimitiveNum * 3 + 0] := boundaryList[b2].vertexIndex;
  indicesList[PrimitiveNum * 3 + 1] := boundaryList[b1].vertexIndex;
  indicesList[PrimitiveNum * 3 + 2] := boundaryList[b0].vertexIndex;
  // Join keystone triangle.
  joinTriangles(PrimitiveNum, 0, boundaryList[b1].triangle,
    boundaryList[b1].edge);
  joinTriangles(PrimitiveNum, 1, boundaryList[b0].triangle,
    boundaryList[b0].edge);
  joinTriangles(PrimitiveNum, 2, boundaryList[b2].triangle,
    boundaryList[b2].edge);
  Inc(PrimitiveNum);
end;

procedure findAndFixOpenTriangleGroups(triangle: LongWord);
var
  Count: LongWord;
begin
  if Length(boundaryList) < Integer(1 + 2 * PrimitiveNum) then
    SetLength(boundaryList, 1 + 2 * PrimitiveNum);

  if edgeInfo[triangle].adjacentTriangle[0] = $FFFFFFFF then
  begin
    findOpenBoundary(triangle, 0, Count);
    fixOpenBoundary(Count);
  end;
  if edgeInfo[triangle].adjacentTriangle[1] = $FFFFFFFF then
  begin
    findOpenBoundary(triangle, 1, Count);
    fixOpenBoundary(Count);
  end;
  if edgeInfo[triangle].adjacentTriangle[2] = $FFFFFFFF then
  begin
    findOpenBoundary(triangle, 2, Count);
    fixOpenBoundary(Count);
  end;
end;

procedure CloseOpenTriangleGroups;
var
  i: LongWord;
begin
  i := 0;
  while i < PrimitiveNum do
  begin
    if (edgeInfo[i].adjacentTriangle[0] = $FFFFFFFF) or
      (edgeInfo[i].adjacentTriangle[1] = $FFFFFFFF) or
      (edgeInfo[i].adjacentTriangle[2] = $FFFFFFFF) then
      findAndFixOpenTriangleGroups(i);
    Inc(i);
  end;
end;

procedure CheckForBogusAdjacency;

  function AdjacentEdge(x, n: Integer): Integer;
  begin
    Result := (x shr (2 * n)) and 3;
  end;

var
  i, j: Integer;
  adjacentTriangle, adjacentTriangleSharedEdge: LongWord;
begin
  for i := 0 to PrimitiveNum - 1 do
    for j := 0 to 2 do
    begin
      adjacentTriangleSharedEdge :=
        AdjacentEdge(edgeInfo[i].adjacentTriangleEdges, j);
      adjacentTriangle := edgeInfo[i].adjacentTriangle[j];
      if adjacentTriangle <> $FFFFFFFF then
      begin
        Assert(adjacentTriangleSharedEdge < 3);
        Assert(edgeInfo[adjacentTriangle].adjacentTriangle
          [adjacentTriangleSharedEdge] = LongWord(i));
        Assert(AdjacentEdge(edgeInfo[adjacentTriangle].adjacentTriangleEdges,
          adjacentTriangleSharedEdge) = j);
      end
      else
        Assert(adjacentTriangleSharedEdge = 3);
    end;
end;

procedure reconnectSharedEdges(isTri, wasTri: LongWord);
var
  tri, Count: LongWord;
  i, j: Byte;
begin
  for i := 0 to 3 do
  begin
    tri := edgeInfo[wasTri].adjacentTriangle[i];
    if tri <> $FFFFFFFF then
    begin
      Count := 0;
      for j := 0 to 3 do
      begin
        if edgeInfo[tri].adjacentTriangle[j] = wasTri then
        begin
          edgeInfo[tri].adjacentTriangle[j] := isTri;
          Inc(Count);
        end;
        if edgeInfo[tri].adjacentTriangle[j] = isTri then
          Inc(Count);
      end;
      Assert(Count > 0);
    end;
  end;
end;

procedure possiblyReconnectTriangle(tri, isTri, wasTri: LongWord);
var
  j: Byte;
begin
  for j := 0 to 3 do
    if edgeInfo[tri].adjacentTriangle[j] = wasTri then
      edgeInfo[tri].adjacentTriangle[j] := isTri;
end;

function eliminateAdjacentDegeneratePair(badTri, otherBadTri, goodTri: LongWord)
  : LongWord;
var
  otherGoodTri: LongWord;
  i: Integer;
  j: Byte;
begin
  Assert(badTri < PrimitiveNum);
  Assert(otherBadTri < PrimitiveNum);
  Assert(goodTri < PrimitiveNum);

  otherGoodTri := 0;
  { The other good triangle is the triangle adjacent to the other
    bad triangle but which is not the bad triangle. }
  for i := 0 to 3 do
    if edgeInfo[otherBadTri].adjacentTriangle[i] <> badTri then
    begin
      otherGoodTri := edgeInfo[otherBadTri].adjacentTriangle[i];
      Break;
    end;

  Assert(i < 3);

  (* Fix the good triangle so that both edges adjacent to the
    bad triangle are now adjacent to the other good triangle. *)
  for i := 0 to 3 do
    if edgeInfo[goodTri].adjacentTriangle[i] = badTri then
      edgeInfo[goodTri].adjacentTriangle[i] := otherGoodTri;

  (* Fix the other good triangle so that both edges adjacent to the
    other bad triangle are now adjacent to the good triangle. *)

  for i := 0 to 3 do
    if edgeInfo[otherGoodTri].adjacentTriangle[i] = otherBadTri then
      edgeInfo[otherGoodTri].adjacentTriangle[i] := goodTri;

  (* Decrement the object's triangle count by 2. Then copy
    non-degenerate triangles from the end of the triangle
    list to the slots once used by the eliminated triangles.
    Be sure to copy the edgeInfo data structure too.  Also
    if goodTri is one of the last two triangles, be careful
    to make sure it gets copied. *)

  Dec(PrimitiveNum, 2);

  if goodTri < PrimitiveNum then
  begin
    PVector3dw(@indicesList[3 * badTri])^ :=
      PVector3dw(@indicesList[3 * PrimitiveNum + 3])^;
    edgeInfo[badTri] := edgeInfo[PrimitiveNum + 1];
    PVector3dw(@indicesList[3 * otherBadTri])^ :=
      PVector3dw(@indicesList[3 * PrimitiveNum])^;
    edgeInfo[otherBadTri] := edgeInfo[PrimitiveNum];
    reconnectSharedEdges(badTri, PrimitiveNum + 1);
    reconnectSharedEdges(otherBadTri, PrimitiveNum);
    (* We are moving two triangles and they each might be
      connected to each other.  Possibly reconnect the
      edges appropriately if so. *)
    possiblyReconnectTriangle(badTri, otherBadTri, PrimitiveNum);
    possiblyReconnectTriangle(otherBadTri, badTri, PrimitiveNum + 1);
  end
  else
  begin
    if goodTri = PrimitiveNum + 1 then
      if badTri < PrimitiveNum then
      begin
        PVector3dw(@indicesList[3 * badTri])^ :=
          PVector3dw(@indicesList[3 * PrimitiveNum + 3])^;
        edgeInfo[badTri] := edgeInfo[PrimitiveNum + 1];
        PVector3dw(@indicesList[3 * otherBadTri])^ :=
          PVector3dw(@indicesList[3 * PrimitiveNum])^;
        edgeInfo[otherBadTri] := edgeInfo[PrimitiveNum];
        reconnectSharedEdges(badTri, PrimitiveNum + 1);
        possiblyReconnectTriangle(badTri, otherBadTri, PrimitiveNum);

        if otherBadTri < PrimitiveNum then
        begin
          reconnectSharedEdges(otherBadTri, PrimitiveNum);
          possiblyReconnectTriangle(otherBadTri, badTri, PrimitiveNum + 1);
        end;

        goodTri := badTri;
      end
      else
      begin
        Assert(otherBadTri < PrimitiveNum);
        PVector3dw(@indicesList[3 * otherBadTri])^ :=
          PVector3dw(@indicesList[3 * PrimitiveNum + 3])^;
        edgeInfo[otherBadTri] := edgeInfo[PrimitiveNum + 1];
        PVector3dw(@indicesList[3 * badTri])^ :=
          PVector3dw(@indicesList[3 * PrimitiveNum])^;
        edgeInfo[badTri] := edgeInfo[PrimitiveNum];
        reconnectSharedEdges(otherBadTri, PrimitiveNum + 1);
        possiblyReconnectTriangle(otherBadTri, badTri, PrimitiveNum);

        if badTri < PrimitiveNum then
        begin
          reconnectSharedEdges(badTri, PrimitiveNum);
          possiblyReconnectTriangle(badTri, otherBadTri, PrimitiveNum + 1);
        end;

        goodTri := otherBadTri;
      end
    else
    begin
      Assert(goodTri = PrimitiveNum);
      if badTri < PrimitiveNum then
      begin
        PVector3dw(@indicesList[3 * badTri])^ :=
          PVector3dw(@indicesList[3 * PrimitiveNum])^;
        edgeInfo[badTri] := edgeInfo[PrimitiveNum];
        PVector3dw(@indicesList[3 * otherBadTri])^ :=
          PVector3dw(@indicesList[3 * PrimitiveNum + 3])^;
        edgeInfo[otherBadTri] := edgeInfo[PrimitiveNum + 1];
        reconnectSharedEdges(badTri, PrimitiveNum);
        possiblyReconnectTriangle(badTri, otherBadTri, PrimitiveNum + 1);

        if otherBadTri < PrimitiveNum then
        begin
          reconnectSharedEdges(otherBadTri, PrimitiveNum + 1);
          possiblyReconnectTriangle(otherBadTri, badTri, PrimitiveNum);
        end;

        goodTri := badTri;
      end
      else
      begin
        Assert(otherBadTri < PrimitiveNum);
        PVector3dw(@indicesList[3 * otherBadTri])^ :=
          PVector3dw(@indicesList[3 * PrimitiveNum])^;
        edgeInfo[otherBadTri] := edgeInfo[PrimitiveNum];
        PVector3dw(@indicesList[3 * badTri])^ :=
          PVector3dw(@indicesList[3 * PrimitiveNum + 3])^;
        edgeInfo[badTri] := edgeInfo[PrimitiveNum + 1];
        reconnectSharedEdges(otherBadTri, PrimitiveNum);
        possiblyReconnectTriangle(otherBadTri, badTri, PrimitiveNum + 1);

        if badTri < PrimitiveNum then
        begin
          reconnectSharedEdges(badTri, PrimitiveNum + 1);
          possiblyReconnectTriangle(badTri, otherBadTri, PrimitiveNum);
        end;

        goodTri := otherBadTri;
      end;
    end;
  end;

  Assert(goodTri < PrimitiveNum);

  // Patch up the edge info for the two relocated triangles.
  for i := PrimitiveNum - 1 downto 0 do
    for j := 0 to 3 do
      Assert(edgeInfo[i].adjacentTriangle[j] < PrimitiveNum);

  // Two degenerate triangles eliminated.
  Result := 2;
end;

function findAndFixAdjacentDegeneratePair(tri: LongWord): Integer;
var
  t0, t1, t2: LongWord;
begin

  t0 := edgeInfo[tri].adjacentTriangle[0];
  t1 := edgeInfo[tri].adjacentTriangle[1];
  t2 := edgeInfo[tri].adjacentTriangle[2];

  // Trivially degnerate triangles should have already been eliminated.
  Assert(t0 <> tri);
  Assert(t1 <> tri);
  Assert(t2 <> tri);

  if (t0 = t1) and (t1 = t2) then
  begin
    if t0 <> $FFFFFFFF then
    begin
      Assert(edgeInfo[t0].adjacentTriangle[0] = tri);
      Assert(edgeInfo[t0].adjacentTriangle[1] = tri);
      Assert(edgeInfo[t0].adjacentTriangle[2] = tri);
    end;
    Result := 0;
    Exit;
  end;

  if t0 = t1 then
    if t0 <> $FFFFFFFF then
    begin
      Result := eliminateAdjacentDegeneratePair(tri, t0, t2);
      Exit;
    end;

  if t1 = t2 then
    if t1 <> $FFFFFFFF then
    begin
      Result := eliminateAdjacentDegeneratePair(tri, t1, t0);
      Exit;
    end;

  if t2 = t0 then
    if t1 <> $FFFFFFFF then
    begin
      Result := eliminateAdjacentDegeneratePair(tri, t2, t1);
      Exit;
    end;

  Result := 0;
end;

procedure EliminateAdjacentDegenerateTriangles;
var
  Count: Integer;
  loopCount: Integer;
  i: Integer;
begin
  (* Eliminating two degenerate triangle pairs may
    not be the end of the story if the two "good" triangles
    that get connected are also degenerate.  Loop to
    handle this unlikely event. *)
  Count := 0;
  repeat
    loopCount := Count;
    for i := 0 to PrimitiveNum - 1 do
      Count := Count + findAndFixAdjacentDegeneratePair(i);
  until Count > loopCount;
end;

function MakeTriangleAdjacencyList(const AindicesList: PLongWordArray;
  Count: LongWord; const AVerticesList: PAffineVectorArray): TGLLongWordList;

  function AdjacentEdge(x, n: Integer): Integer;
  begin
    Result := (x shr (2 * n)) and 3;
  end;

var
  i: Integer;
  j: Byte;
  n, ii, jj: LongWord;
  tri, adjtri: TVector3dw;
  NewIndices: TGLLongWordList;
begin
  Result := nil;
  Assert(Assigned(AindicesList));
  Assert(Assigned(AVerticesList));
  PrimitiveNum := Count div 3;
  Assert(PrimitiveNum > 0);
  indicesList := AindicesList;
  verticesList := AVerticesList;

  SetLength(edgeInfo, vEdgeInfoReserveSize + PrimitiveNum);

  if not ComputeTriangleEdgeInfo then
    Exit;
  CheckForBogusAdjacency;
  if vImprovedFixingOpenTriangleEdge then
  begin
    CloseOpenTriangleGroups;
    EliminateAdjacentDegenerateTriangles;
  end;

  NewIndices := TGLLongWordList.Create;
  NewIndices.SetCountResetsMemory := False;
  NewIndices.Capacity := 6 * PrimitiveNum;

  for i := 0 to PrimitiveNum - 1 do
  begin
    n := 3 * i;
    tri[0] := indicesList[n + 0];
    tri[1] := indicesList[n + 1];
    tri[2] := indicesList[n + 2];
    for j := 0 to 2 do
    begin
      NewIndices.Add(tri[j]);
      n := edgeInfo[i].adjacentTriangle[j];
      if n = $FFFFFFFF then
      begin
        jj := (j + 2) mod 3;
        NewIndices.Add(tri[jj]);
      end
      else
      begin
        n := 3 * n;
        adjtri[0] := indicesList[n + 0];
        adjtri[1] := indicesList[n + 1];
        adjtri[2] := indicesList[n + 2];
        ii := (AdjacentEdge(edgeInfo[i].adjacentTriangleEdges, j) + 2) mod 3;
        NewIndices.Add(adjtri[ii]);
      end;
    end;
  end;
  Result := NewIndices;
end;

function ConvertStripToList(const AindicesList: PLongWordArray; Count: LongWord;
  RestartIndex: LongWord): TGLLongWordList;
var
  i: Integer;
  Index, prevIndex1, prevIndex2, stripCount: LongWord;
  NewIndices: TGLLongWordList;
begin
  Result := nil;
  if not Assigned(AindicesList) or (Count < 3) then
    Exit;
  NewIndices := TGLLongWordList.Create;
  stripCount := 0;
  prevIndex1 := 0;
  prevIndex2 := 0;
  for i := 0 to Count - 1 do
  begin
    Index := AindicesList[i];
    if stripCount > 2 then
    begin
      // Check for restart index
      if Index = RestartIndex then
      begin
        stripCount := 0;
        Continue;
      end
      // Check for degenerate triangles
      else if Index = prevIndex1 then
      begin
        Continue;
      end
      else if prevIndex1 = prevIndex2 then
      begin
        stripCount := 0;
        Continue;
      end;
      if Boolean(stripCount and 1) then
      begin
        NewIndices.Add(prevIndex2);
        NewIndices.Add(prevIndex1);
      end
      else
      begin
        NewIndices.Add(prevIndex1);
        NewIndices.Add(prevIndex2);
      end;
    end
    else if stripCount = 2 then
    begin
      NewIndices.Add(prevIndex1);
      NewIndices.Items[NewIndices.Count - 2] := Index;
      prevIndex2 := prevIndex1;
      prevIndex1 := Index;
      Inc(stripCount);
      Continue;
    end;
    NewIndices.Add(Index);
    prevIndex2 := prevIndex1;
    prevIndex1 := Index;
    Inc(stripCount);
  end;

  Result := NewIndices;
end;

function ConvertFansToList(const AindicesList: PLongWordArray; Count: LongWord;
  RestartIndex: LongWord): TGLLongWordList;
var
  i: Integer;
  Index, centerIndex, prevIndex, fansCount: LongWord;
  NewIndices: TGLLongWordList;
  degenerate: Boolean;
begin
  Result := nil;
  if not Assigned(AindicesList) or (Count < 3) then
    Exit;
  NewIndices := TGLLongWordList.Create;
  fansCount := 0;
  prevIndex := 0;
  degenerate := False;
  centerIndex := AindicesList[0];
  for i := 0 to Count - 1 do
  begin
    Index := AindicesList[i];
    if fansCount > 2 then
    begin
      // Check for restart index
      if Index = RestartIndex then
      begin
        fansCount := 0;
        Continue;
      end
      // Check for degenerate triangles
      else if Index = prevIndex then
      begin
        degenerate := True;
        Continue;
      end
      else if degenerate then
      begin
        degenerate := False;
        fansCount := 0;
        Continue;
      end;
      NewIndices.Add(centerIndex);
      NewIndices.Add(prevIndex);
    end
    else if fansCount = 0 then
      centerIndex := Index;
    NewIndices.Add(Index);
    prevIndex := Index;
    Inc(fansCount);
  end;

  Result := NewIndices;
end;

end.
