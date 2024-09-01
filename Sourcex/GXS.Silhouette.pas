//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Silhouette;

(*
  Enhanced silhouette classes.
  Introduces more evolved/specific silhouette generation and management
  classes.
  CAUTION : both connectivity classes leak memory.
*)


interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.VectorLists;

type
  TgxSilhouetteStyle = (ssOmni, ssParallel);

  (* Silouhette generation parameters.
    SeenFrom and LightDirection are expected in local coordinates. *)
  TgxSilhouetteParameters = packed record
    SeenFrom, LightDirection: TAffineVector;
    Style: TgxSilhouetteStyle;
    CappingRequired: Boolean;
  end;

  (* Base class storing a volume silhouette.
    Made of a set of indexed vertices defining an outline, and another set
    of indexed vertices defining a capping volume. Coordinates system
    is the object's unscaled local coordinates system.
    This is the base class, you can use the TgxSilhouette subclass if you
    need some helper methods for generating the indexed sets. *)
  TgxSilhouette = class
  private
    FVertices: TgxVectorList;
    FIndices: TgxIntegerList;
    FCapIndices: TgxIntegerList;
    FParameters: TgxSilhouetteParameters;
  protected
    procedure SetIndices(const value: TgxIntegerList);
    procedure SetCapIndices(const value: TgxIntegerList);
    procedure SetVertices(const value: TgxVectorList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Parameters: TgxSilhouetteParameters read FParameters write FParameters;
    property Vertices: TgxVectorList read FVertices write SetVertices;
    property Indices: TgxIntegerList read FIndices write SetIndices;
    property CapIndices: TgxIntegerList read FCapIndices write SetCapIndices;
    procedure Flush;
    procedure Clear;
    procedure ExtrudeVerticesToInfinity(const origin: TAffineVector);
    (* Adds an edge (two vertices) to the silhouette.
      If TightButSlow is true, no vertices will be doubled in the
      silhouette list. This should only be used when creating re-usable
      silhouettes, because it's much slower. *)
    procedure AddEdgeToSilhouette(const v0, v1: TAffineVector; tightButSlow: Boolean);
    procedure AddIndexedEdgeToSilhouette(const Vi0, Vi1: integer);
    (* Adds a capping triangle to the silhouette.
      If TightButSlow is true, no vertices will be doubled in the
      silhouette list. This should only be used when creating re-usable
      silhouettes, because it's much slower. *)
    procedure AddCapToSilhouette(const v0, v1, v2: TAffineVector; tightButSlow: Boolean);
    procedure AddIndexedCapToSilhouette(const Vi0, Vi1, vi2: integer);
  end;

  TBaseConnectivity = class
  protected
    FPrecomputeFaceNormal: Boolean;
    function GetEdgeCount: integer; virtual;
    function GetFaceCount: integer; virtual;
  public
    property EdgeCount: integer read GetEdgeCount;
    property FaceCount: integer read GetFaceCount;
    property PrecomputeFaceNormal: Boolean read FPrecomputeFaceNormal;
    procedure CreateSilhouette(const ASilhouetteParameters: TgxSilhouetteParameters; var ASilhouette: TgxSilhouette;
      AddToSilhouette: Boolean); virtual;
    constructor Create(APrecomputeFaceNormal: Boolean); virtual;
  end;

  TConnectivity = class(TBaseConnectivity)
  protected
    (* All storage of faces and adges are cut up into tiny pieces for a reason,
      it'd be nicer with Structs or classes, but it's actually faster this way.
      The reason it's faster is because of less cache overwrites when we only
      access a tiny bit of a triangle (for instance), not all data. *)
    FEdgeVertices: TgxIntegerList;
    FEdgeFaces: TgxIntegerList;
    FFaceVisible: TByteList;
    FFaceVertexIndex: TgxIntegerList;
    FFaceNormal: TgxAffineVectorList;
    FVertexMemory: TgxIntegerList;
    FVertices: TgxAffineVectorList;
    function GetEdgeCount: integer; override;
    function GetFaceCount: integer; override;
    function ReuseOrFindVertexID(const SeenFrom: TAffineVector; ASilhouette: TgxSilhouette; index: integer): integer;
  public
    // Clears out all connectivity information.
    procedure Clear; virtual;
    procedure CreateSilhouette(const silhouetteParameters: TgxSilhouetteParameters; var ASilhouette: TgxSilhouette;
      AddToSilhouette: Boolean); override;
    function AddIndexedEdge(vertexIndex0, vertexIndex1: integer; FaceID: integer): integer;
    function AddIndexedFace(Vi0, Vi1, vi2: integer): integer;
    function AddFace(const vertex0, vertex1, vertex2: TAffineVector): integer;
    function AddQuad(const vertex0, vertex1, vertex2, vertex3: TAffineVector): integer;
    property EdgeCount: integer read GetEdgeCount;
    property FaceCount: integer read GetFaceCount;
    constructor Create(APrecomputeFaceNormal: Boolean); override;
    destructor Destroy; override;
  end;

// -------------------------------------------------------------
implementation
// -------------------------------------------------------------

// ------------------
// ------------------ TgxSilhouette ------------------
// ------------------

constructor TgxSilhouette.Create;
begin
  inherited;
  FVertices := TgxVectorList.Create;
  FIndices := TgxIntegerList.Create;
  FCapIndices := TgxIntegerList.Create;
end;

destructor TgxSilhouette.Destroy;
begin
  FCapIndices.Free;
  FIndices.Free;
  FVertices.Free;
  inherited;
end;

procedure TgxSilhouette.SetIndices(const value: TgxIntegerList);
begin
  FIndices.Assign(value);
end;

procedure TgxSilhouette.SetCapIndices(const value: TgxIntegerList);
begin
  FCapIndices.Assign(value);
end;

procedure TgxSilhouette.SetVertices(const value: TgxVectorList);
begin
  FVertices.Assign(value);
end;

procedure TgxSilhouette.Flush;
begin
  FVertices.Flush;
  FIndices.Flush;
  FCapIndices.Flush;
end;

procedure TgxSilhouette.Clear;
begin
  FVertices.Clear;
  FIndices.Clear;
  FCapIndices.Clear;
end;

procedure TgxSilhouette.ExtrudeVerticesToInfinity(const origin: TAffineVector);
var
  i, nv, ni, nc, k: integer;
  vList, vListN: PVectorArray;
  iList, iList2: PIntegerArray;
begin
  // extrude vertices
  nv := Vertices.Count;
  Vertices.Count := 2 * nv;
  vList := Vertices.List;
  vListN := @vList[nv];
  for i := 0 to nv - 1 do
  begin
    vListN^[i].W := 0;
    VectorSubtract(PAffineVector(@vList[i])^, origin, PAffineVector(@vListN[i])^);
  end;
  // change silhouette indices to quad indices
  ni := Indices.Count;
  Indices.Count := 2 * ni;
  iList := Indices.List;
  i := ni - 2;
  while i >= 0 do
  begin
    iList2 := @iList^[2 * i];
    iList2^[0] := iList^[i];
    iList2^[1] := iList^[i + 1];
    iList2^[2] := iList^[i + 1] + nv;
    iList2^[3] := iList^[i] + nv;
    Dec(i, 2);
  end;
  // add extruded triangles to capIndices
  nc := CapIndices.Count;
  CapIndices.Capacity := 2 * nc;
  iList := CapIndices.List;
  for i := nc - 1 downto 0 do
  begin
    k := iList^[i];
    CapIndices.Add(k);
    iList^[i] := k + nv;
  end;
end;

// ------------------
// ------------------ TgxSilhouette ------------------
// ------------------

procedure TgxSilhouette.AddEdgeToSilhouette(const v0, v1: TAffineVector; tightButSlow: Boolean);
begin
  if tightButSlow then
    Indices.Add(Vertices.FindOrAddPoint(v0), Vertices.FindOrAddPoint(v1))
  else
    Indices.Add(Vertices.Add(v0, 1), Vertices.Add(v1, 1));
end;

procedure TgxSilhouette.AddIndexedEdgeToSilhouette(const Vi0, Vi1: integer);

begin
  Indices.Add(Vi0, Vi1);
end;

procedure TgxSilhouette.AddCapToSilhouette(const v0, v1, v2: TAffineVector; tightButSlow: Boolean);
begin
  if tightButSlow then
    CapIndices.Add(Vertices.FindOrAddPoint(v0), Vertices.FindOrAddPoint(v1), Vertices.FindOrAddPoint(v2))
  else
    CapIndices.Add(Vertices.Add(v0, 1), Vertices.Add(v1, 1), Vertices.Add(v2, 1));
end;

procedure TgxSilhouette.AddIndexedCapToSilhouette(const Vi0, Vi1, vi2: integer);
begin
  CapIndices.Add(Vi0, Vi1, vi2);
end;

// ------------------
// ------------------ TBaseConnectivity ------------------
// ------------------

constructor TBaseConnectivity.Create(APrecomputeFaceNormal: Boolean);
begin
  FPrecomputeFaceNormal := APrecomputeFaceNormal;
end;

procedure TBaseConnectivity.CreateSilhouette(const ASilhouetteParameters: TgxSilhouetteParameters;
  var ASilhouette: TgxSilhouette; AddToSilhouette: Boolean);
begin
  // Purely virtual!
end;

// ------------------
// ------------------ TConnectivity ------------------
// ------------------

function TBaseConnectivity.GetEdgeCount: integer;
begin
  result := 0;
end;

function TBaseConnectivity.GetFaceCount: integer;
begin
  result := 0;
end;

{ TConnectivity }

constructor TConnectivity.Create(APrecomputeFaceNormal: Boolean);
begin
  FFaceVisible := TByteList.Create;
  FFaceVertexIndex := TgxIntegerList.Create;
  FFaceNormal := TgxAffineVectorList.Create;
  FEdgeVertices := TgxIntegerList.Create;
  FEdgeFaces := TgxIntegerList.Create;
  FPrecomputeFaceNormal := APrecomputeFaceNormal;
  FVertexMemory := TgxIntegerList.Create;
  FVertices := TgxAffineVectorList.Create;
end;

destructor TConnectivity.Destroy;
begin
  Clear;
  FFaceVisible.Free;
  FFaceVertexIndex.Free;
  FFaceNormal.Free;
  FEdgeVertices.Free;
  FEdgeFaces.Free;
  FVertexMemory.Free;
  if Assigned(FVertices) then
    FVertices.Free;
  inherited;
end;

procedure TConnectivity.Clear;
begin
  FEdgeVertices.Clear;
  FEdgeFaces.Clear;
  FFaceVisible.Clear;
  FFaceVertexIndex.Clear;
  FFaceNormal.Clear;
  FVertexMemory.Clear;
  if FVertices <> nil then
    FVertices.Clear;
end;

procedure TConnectivity.CreateSilhouette(const silhouetteParameters: TgxSilhouetteParameters; var ASilhouette: TgxSilhouette;
  AddToSilhouette: Boolean);
var
  i: integer;
  vis: PIntegerArray;
  tVi0, tVi1: integer;
  faceNormal: TAffineVector;
  face0ID, face1ID: integer;
  faceIsVisible: Boolean;
  verticesList: PAffineVectorArray;
begin
  if not Assigned(ASilhouette) then
    ASilhouette := TgxSilhouette.Create
  else if not AddToSilhouette then
    ASilhouette.Flush;
  // Clear the vertex memory
  FVertexMemory.Flush;
  // Update visibility information for all Faces
  vis := FFaceVertexIndex.List;
  for i := 0 to FaceCount - 1 do
  begin
    if FPrecomputeFaceNormal then
      faceIsVisible := (PointProject(silhouetteParameters.SeenFrom, FVertices.List^[vis^[0]], FFaceNormal.List^[i]) >= 0)
    else
    begin
      verticesList := FVertices.List;
      faceNormal := CalcPlaneNormal(verticesList^[vis^[0]], verticesList^[vis^[1]], verticesList^[vis^[2]]);
      faceIsVisible := (PointProject(silhouetteParameters.SeenFrom, FVertices.List^[vis^[0]], faceNormal) >= 0);
    end;

    FFaceVisible[i] := Byte(faceIsVisible);
    if (not faceIsVisible) and silhouetteParameters.CappingRequired then
      ASilhouette.CapIndices.Add(ReuseOrFindVertexID(silhouetteParameters.SeenFrom, ASilhouette, vis^[0]),
        ReuseOrFindVertexID(silhouetteParameters.SeenFrom, ASilhouette, vis^[1]),
        ReuseOrFindVertexID(silhouetteParameters.SeenFrom, ASilhouette, vis^[2]));
    vis := @vis[3];
  end;

  for i := 0 to EdgeCount - 1 do
  begin
    face0ID := FEdgeFaces[i * 2 + 0];
    face1ID := FEdgeFaces[i * 2 + 1];

    if (face1ID = -1) or (FFaceVisible.List^[face0ID] <> FFaceVisible.List^[face1ID]) then
    begin
      // Retrieve the two vertice values add add them to the Silhouette list
      vis := @FEdgeVertices.List[i * 2];

      // In this moment, we _know_ what vertex id the vertex had in the old
      // mesh. We can remember this information and re-use it for a speedup
      if FFaceVisible.List^[face0ID] = 0 then
      begin
        tVi0 := ReuseOrFindVertexID(silhouetteParameters.SeenFrom, ASilhouette, vis^[0]);
        tVi1 := ReuseOrFindVertexID(silhouetteParameters.SeenFrom, ASilhouette, vis^[1]);
        ASilhouette.Indices.Add(tVi0, tVi1);
      end
      else if face1ID > -1 then
      begin
        tVi0 := ReuseOrFindVertexID(silhouetteParameters.SeenFrom, ASilhouette, vis^[0]);
        tVi1 := ReuseOrFindVertexID(silhouetteParameters.SeenFrom, ASilhouette, vis^[1]);
        ASilhouette.Indices.Add(tVi1, tVi0);
      end;
    end;
  end;
end;

function TConnectivity.GetEdgeCount: integer;
begin
  result := FEdgeVertices.Count div 2;
end;

function TConnectivity.GetFaceCount: integer;
begin
  result := FFaceVisible.Count;
end;

function TConnectivity.ReuseOrFindVertexID(const SeenFrom: TAffineVector; ASilhouette: TgxSilhouette; index: integer): integer;
var
  pMemIndex: PInteger;
  memIndex, i: integer;
  oldCount: integer;
  List: PIntegerArray;
begin
  if index >= FVertexMemory.Count then
  begin
    oldCount := FVertexMemory.Count;
    FVertexMemory.Count := index + 1;

    List := FVertexMemory.List;
    for i := oldCount to FVertexMemory.Count - 1 do
      List^[i] := -1;
  end;

  pMemIndex := @FVertexMemory.List[index];

  if pMemIndex^ = -1 then
  begin
    // Add the "near" vertex
    memIndex := ASilhouette.Vertices.Add(FVertices.List^[index], 1);
    pMemIndex^ := memIndex;
    result := memIndex;
  end
  else
    result := pMemIndex^;
end;

function TConnectivity.AddIndexedEdge(vertexIndex0, vertexIndex1: integer; FaceID: integer): integer;
var
  i: integer;
  edgesVertices: PIntegerArray;
begin
  // Make sure that the edge doesn't already exists
  edgesVertices := FEdgeVertices.List;
  for i := 0 to EdgeCount - 1 do
  begin
    // Retrieve the two vertices in the edge
    if ((edgesVertices^[0] = vertexIndex0) and (edgesVertices^[1] = vertexIndex1)) or
      ((edgesVertices^[0] = vertexIndex1) and (edgesVertices^[1] = vertexIndex0)) then
    begin
      // Update the second Face of the edge and we're done (this _MAY_
      // overwrite a previous Face in a broken mesh)
      FEdgeFaces[i * 2 + 1] := FaceID;
      result := i * 2 + 1;
      Exit;
    end;
    edgesVertices := @edgesVertices[2];
  end;

  // No edge was found, create a new one
  FEdgeVertices.Add(vertexIndex0, vertexIndex1);
  FEdgeFaces.Add(FaceID, -1);

  result := EdgeCount - 1;
end;

function TConnectivity.AddIndexedFace(Vi0, Vi1, vi2: integer): integer;
var
  FaceID: integer;
begin
  FFaceVertexIndex.Add(Vi0, Vi1, vi2);
  if FPrecomputeFaceNormal then
    FFaceNormal.Add(CalcPlaneNormal(FVertices.List^[Vi0], FVertices.List^[Vi1], FVertices.List^[vi2]));
  FaceID := FFaceVisible.Add(0);
  AddIndexedEdge(Vi0, Vi1, FaceID);
  AddIndexedEdge(Vi1, vi2, FaceID);
  AddIndexedEdge(vi2, Vi0, FaceID);

  Result := FaceID;
end;

function TConnectivity.AddFace(const vertex0, vertex1, vertex2: TAffineVector): integer;
var
  Vi0, Vi1, vi2: integer;
begin
  Vi0 := FVertices.FindOrAdd(vertex0);
  Vi1 := FVertices.FindOrAdd(vertex1);
  vi2 := FVertices.FindOrAdd(vertex2);

  Result := AddIndexedFace(Vi0, Vi1, vi2);
end;

function TConnectivity.AddQuad(const vertex0, vertex1, vertex2, vertex3: TAffineVector): integer;
var
  Vi0, Vi1, vi2, Vi3: integer;
begin
  Vi0 := FVertices.FindOrAdd(vertex0);
  Vi1 := FVertices.FindOrAdd(vertex1);
  vi2 := FVertices.FindOrAdd(vertex2);
  Vi3 := FVertices.FindOrAdd(vertex3);
  // First face
  result := AddIndexedFace(Vi0, Vi1, vi2);
  // Second face
  AddIndexedFace(vi2, Vi3, Vi0);
end;

end.
