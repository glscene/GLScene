//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.VerletClothify;

(* Methods for turning a TGLBaseMesh into a Verlet cloth / jelly *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.VectorFileObjects,
  GLS.VerletTypes,
  GLS.VectorTypes,
  GLS.VectorLists,
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.RenderContextInfo,
  GLS.State,
  GLS.PersistentClasses,
  GLS.Context;

type
  (*
    Class that represents a face. This structure
    is not used for rendering, but for extracting info from meshes
  *)
  TGLFace = class
  public
    Vertices: array [0 .. 2] of integer;
    Normal: TAffineVector;
    MeshObject: TMeshObject;
    Active: boolean;
    procedure UpdateNormal;
    constructor Create(aMeshObject: TMeshObject);
  end;

  // List of faces
  TGLFaceList = class(TList)
  private
    function GetItems(i: integer): TGLFace;
    procedure SetItems(i: integer; const Value: TGLFace);
  public
    property Items[i: integer]: TGLFace read GetItems write SetItems; default;
  end;

  // Class that extracts faces from a GLBaseMesh
  TGLFaceExtractor = class
  private
    FFaceList: TGLFaceList;
    FGLBaseMesh: TGLBaseMesh;
    FNodeList: TGLVerletNodeList;
    FWeldDistance: single;
    FEdgeDoublesSkipped: integer;
    procedure SetWeldDistance(const Value: single);
  protected
    procedure ProcessMeshObject(const MeshObject: TMeshObject); virtual;
  public
    procedure ExtractFacesFromVertexIndexList(const FaceGroup
      : TFGVertexIndexList; const MeshObject: TMeshObject);
    property FaceList: TGLFaceList read FFaceList;
    procedure Clear; virtual;
    procedure ProcessMesh; virtual;
    property WeldDistance: single read FWeldDistance write SetWeldDistance;
    property EdgeDoublesSkipped: integer read FEdgeDoublesSkipped;
    property GLBaseMesh: TGLBaseMesh read FGLBaseMesh;
    property NodeList: TGLVerletNodeList read FNodeList;
    function AddFace(const Vi0, Vi1, Vi2: integer;
      const MeshObject: TMeshObject): TGLFace; virtual;
    constructor Create(const aGLBaseMesh: TGLBaseMesh); virtual;
    destructor Destroy; override;
  end;

  TGLEdgeDetector = class;

  TGLEdge = class
  private
    FSolid: boolean;
    FLength: single;
    FMeshObject: TMeshObject;
    FOwner: TGLEdgeDetector;
  public
    Vertices: array [0 .. 1] of integer;
    Faces: array [0 .. 1] of TGLFace;
    procedure Contract;
    property Owner: TGLEdgeDetector read FOwner;
    property MeshObject: TMeshObject read FMeshObject write FMeshObject;
    property Length: single read FLength write FLength;
    property Solid: boolean read FSolid write FSolid;
    procedure UpdateEdgeLength;
    constructor Create(const AOwner: TGLEdgeDetector; AVi0, AVi1: integer;
      AFace0, AFace1: TGLFace; aMeshObject: TMeshObject; ASolid: boolean);
  end;

  TGLEdgeList = class(TList)
  private
    function GetItems(i: integer): TGLEdge;
    procedure SetItems(i: integer; const Value: TGLEdge);
  public
    property Items[i: integer]: TGLEdge read GetItems write SetItems; default;
    procedure SortByLength;
    function InsertSorted(AEdge: TGLEdge): integer;
  end;

  TGLEdgeDetector = class(TGLFaceExtractor)
  private
    FEdgeList: TGLEdgeList;
    FCurrentNodeOffset: integer;
    FNodesAdded: boolean;
    procedure BuildOpposingEdges;
  protected
    FCalcEdgeLength: boolean;
  public
    property EdgeList: TGLEdgeList read FEdgeList;
    procedure Clear; override;
    procedure ProcessMesh; override;
    function AddEdge(const Vi0, Vi1: integer; const Face: TGLFace;
      const aMeshObject: TMeshObject): TGLEdge;
    function AddFace(const Vi0, Vi1, Vi2: integer;
      const MeshObject: TMeshObject): TGLFace; override;
    function AddNode(const VerletWorld: TGLVerletWorld;
      const MeshObject: TMeshObject; const VertexIndex: integer)
      : TGLVerletNode; virtual;
    procedure AddNodes(const VerletWorld: TGLVerletWorld);
    procedure AddEdgesAsSticks(const VerletWorld: TGLVerletWorld;
      const Slack: single);
    procedure AddEdgesAsSprings(const VerletWorld: TGLVerletWorld;
      const Strength, Damping, Slack: single);
    procedure AddEdgesAsSolidEdges(const VerletWorld: TGLVerletWorld);
    procedure AddOuterEdgesAsSolidEdges(const VerletWorld: TGLVerletWorld);
    procedure RenderEdges(var rci: TGLRenderContextInfo);
    property CurrentNodeOffset: integer read FCurrentNodeOffset;
    property NodesAdded: boolean read FNodesAdded;
    procedure ReplaceVertexIndex(const ViRemove, ViReplaceWith: integer);
    constructor Create(const aGLBaseMesh: TGLBaseMesh); override;
    destructor Destroy; override;
  end;

  TGLMeshObjectVerletNode = class(TGLVerletNode)
  private
    MeshObject: TMeshObject;
    VertexIndices: TIntegerList;
  public
    procedure AfterProgress; override;
    constructor CreateOwned(const AOwner: TGLVerletWorld); override;
    destructor Destroy; override;
  end;

// ----------------------------------------------------
implementation
// ----------------------------------------------------

// ------------------
// TGLFaceExtractor
// ------------------
procedure TGLFaceExtractor.Clear;
var
  i: integer;
begin
  for i := 0 to FaceList.Count - 1 do
    FaceList[i].Free;
  FaceList.Clear;
end;

constructor TGLFaceExtractor.Create(const aGLBaseMesh: TGLBaseMesh);
begin
  FFaceList := TGLFaceList.Create;
  FGLBaseMesh := aGLBaseMesh;
  FNodeList := TGLVerletNodeList.Create;
  FWeldDistance := 0.01;
end;

destructor TGLFaceExtractor.Destroy;
begin
  Clear;
  FreeAndNil(FNodeList);
  FreeAndNil(FFaceList);
  inherited;
end;

procedure TGLFaceExtractor.ExtractFacesFromVertexIndexList(const FaceGroup
  : TFGVertexIndexList; const MeshObject: TMeshObject);
var
  List: PIntegerArray;
  iFace, iVertex: integer;
begin
  case FaceGroup.Mode of
    fgmmTriangles, fgmmFlatTriangles:
      begin
        for iFace := 0 to FaceGroup.TriangleCount - 1 do
        begin
          List := @FaceGroup.VertexIndices.List[iFace * 3 + 0];
          AddFace(List^[0], List^[1], List^[2], MeshObject);
        end;
      end;
    fgmmTriangleStrip:
      begin
        for iFace := 0 to FaceGroup.VertexIndices.Count - 3 do
        begin
          List := @FaceGroup.VertexIndices.List[iFace];
          if (iFace and 1) = 0 then
            AddFace(List^[0], List^[1], List^[2], MeshObject)
          else
            AddFace(List^[2], List^[1], List^[0], MeshObject);
        end;
      end;
    fgmmTriangleFan:
      begin
        List := @FaceGroup.VertexIndices.List;

        for iVertex := 2 to FaceGroup.VertexIndices.Count - 1 do
          AddFace(List^[0], List^[iVertex - 1], List^[iVertex], MeshObject)
      end;
  else
    Assert(false, 'Not supported');
  end;
end;

procedure TGLFaceExtractor.ProcessMesh;
var
  iMeshObject: integer;
  MeshObject: TMeshObject;
begin
  for iMeshObject := 0 to FGLBaseMesh.MeshObjects.Count - 1 do
  begin
    MeshObject := FGLBaseMesh.MeshObjects[iMeshObject];
    ProcessMeshObject(MeshObject);
  end;
end;

procedure TGLFaceExtractor.ProcessMeshObject(const MeshObject: TMeshObject);
var
  iFaceGroup: integer;
begin
  if MeshObject.Mode = momFaceGroups then
  begin
    for iFaceGroup := 0 to MeshObject.FaceGroups.Count - 1 do
    begin
      if MeshObject.FaceGroups[iFaceGroup] is TFGVertexIndexList then
      begin
        ExtractFacesFromVertexIndexList(MeshObject.FaceGroups[iFaceGroup]
          as TFGVertexIndexList, MeshObject);
      end
      else
        Assert(false);
    end;
  end
  else
    Assert(false);
end;

function TGLFaceExtractor.AddFace(const Vi0, Vi1, Vi2: integer;
  const MeshObject: TMeshObject): TGLFace;
var
  Face: TGLFace;
begin
  Face := TGLFace.Create(MeshObject);
  FaceList.Add(Face);
  Face.Vertices[0] := Vi0;
  Face.Vertices[1] := Vi1;
  Face.Vertices[2] := Vi2;
  Result := Face;
end;

procedure TGLFaceExtractor.SetWeldDistance(const Value: single);
begin
  FWeldDistance := Value;
end;

// ------------------
// TGLFaceList
// ------------------
function TGLFaceList.GetItems(i: integer): TGLFace;
begin
  result := TGLFace(Get(i));
end;

procedure TGLFaceList.SetItems(i: integer; const Value: TGLFace);
begin
  Put(i, Value);
end;

// ------------------
// TGLEdgeList
// ------------------
function TGLEdgeList.GetItems(i: integer): TGLEdge;
begin
  result := TGLEdge(Get(i));
end;

function TGLEdgeList.InsertSorted(AEdge: TGLEdge): integer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if AEdge.Length < Items[i].Length then
    begin
      Insert(i, AEdge);
      Result := i;
      Exit;
    end;
  end;
  Result := Add(AEdge);
end;

procedure TGLEdgeList.SetItems(i: integer; const Value: TGLEdge);
begin
  Put(i, Value);
end;

function EdgeLength(Item1, Item2: pointer): integer;
begin
  if TGLEdge(Item1).Length < TGLEdge(Item2).Length then
    Result := -1
  else if TGLEdge(Item1).Length = TGLEdge(Item2).Length then
    Result := 0
  else
    Result := 1;
end;

procedure TGLEdgeList.SortByLength;
begin
  Sort(@EdgeLength);
end;

// --------------------------
// TGLMeshObjectVerletNode
// --------------------------
constructor TGLMeshObjectVerletNode.CreateOwned(const AOwner: TGLVerletWorld);
begin
  inherited;
  VertexIndices := TIntegerList.Create;
end;

destructor TGLMeshObjectVerletNode.Destroy;
begin
  VertexIndices.Free;
  inherited;
end;

procedure TGLMeshObjectVerletNode.AfterProgress;
var
  i: integer;
begin
  // Update the actual vertex
  for i := 0 to VertexIndices.Count - 1 do
    MeshObject.Vertices[VertexIndices[i]] :=
      MeshObject.Owner.Owner.AbsoluteToLocal(Location);
end;

// --------------------------
// TGLEdgeDetector
// --------------------------
procedure TGLEdgeDetector.Clear;
var
  i: integer;
begin
  inherited;
  for i := 0 to EdgeList.Count - 1 do
    EdgeList[i].Free;
  EdgeList.Clear;
  FCurrentNodeOffset := 0;
  FNodesAdded := false;
end;

constructor TGLEdgeDetector.Create(const aGLBaseMesh: TGLBaseMesh);
begin
  FEdgeList := TGLEdgeList.Create;
  FCurrentNodeOffset := 0;
  FNodesAdded := false;
  FCalcEdgeLength := false;
  inherited;
end;

destructor TGLEdgeDetector.Destroy;
begin
  inherited;
  FreeAndNil(FEdgeList);
end;

function TGLEdgeDetector.AddEdge(const Vi0, Vi1: integer; const Face: TGLFace;
  const aMeshObject: TMeshObject): TGLEdge;
var
  i: integer;
  Edge: TGLEdge;
begin
  // Find an indentical edge, if there is one
  for i := 0 to EdgeList.Count - 1 do
  begin
    Edge := EdgeList[i];
    if (Edge.Vertices[0] = Vi0) and (Edge.Vertices[1] = Vi1) or
      (Edge.Vertices[1] = Vi0) and (Edge.Vertices[0] = Vi1) then
    begin
      Edge.Faces[1] := Face;
      Result := Edge;
      Exit;
    end;
  end;
  // No edge was found, create a new one
  Edge := TGLEdge.Create(self, Vi0, Vi1, Face, nil, aMeshObject, true);
  EdgeList.Add(Edge);
  Result := Edge;
end;

function TGLEdgeDetector.AddFace(const Vi0, Vi1, Vi2: integer;
  const MeshObject: TMeshObject): TGLFace;
var
  Face: TGLFace;
begin
  Face := TGLFace.Create(MeshObject);
  FaceList.Add(Face);
  Face.Vertices[0] := Vi0;
  Face.Vertices[1] := Vi1;
  Face.Vertices[2] := Vi2;
  AddEdge(Vi0, Vi1, Face, MeshObject);
  AddEdge(Vi1, Vi2, Face, MeshObject);
  AddEdge(Vi2, Vi0, Face, MeshObject); // }
  Result := Face;
end;

procedure TGLEdgeDetector.AddNodes(const VerletWorld: TGLVerletWorld);
var
  i: integer;
  MO: TMeshObject;
begin
  FNodesAdded := true;
  FCurrentNodeOffset := FNodeList.Count;
  MO := FGLBaseMesh.MeshObjects[0];
  for i := 0 to MO.Vertices.Count - 1 do
    AddNode(VerletWorld, MO, i);
  // Assert(FNodeList.Count = MO.Vertices.Count, Format('%d <> %d',[FNodeList.Count, MO.Vertices.Count]));
end;

procedure TGLEdgeDetector.AddEdgesAsSprings(const VerletWorld: TGLVerletWorld;
  const Strength, Damping, Slack: single);
var
  i: integer;
  Edge: TGLEdge;
begin
  if not FNodesAdded then
    AddNodes(VerletWorld);
  for i := 0 to EdgeList.Count - 1 do
  begin
    // if not EdgeList[i].SameSame(FNodeList) then
    Edge := EdgeList[i];
    if FNodeList[FCurrentNodeOffset + Edge.Vertices[0]] <> FNodeList
      [FCurrentNodeOffset + Edge.Vertices[1]] then
    begin
      VerletWorld.CreateSpring(FNodeList[FCurrentNodeOffset + Edge.Vertices[0]],
        FNodeList[FCurrentNodeOffset + Edge.Vertices[1]], Strength,
        Damping, Slack);
    end;
  end;
end;

procedure TGLEdgeDetector.AddEdgesAsSticks(const VerletWorld: TGLVerletWorld;
  const Slack: single);
var
  i: integer;
  Edge: TGLEdge;
begin
  if not FNodesAdded then
    AddNodes(VerletWorld);
  for i := 0 to EdgeList.Count - 1 do
  begin
    // if not EdgeList[i].SameSame(FNodeList) then
    Edge := EdgeList[i];
    if FNodeList[FCurrentNodeOffset + Edge.Vertices[0]] <> FNodeList
      [FCurrentNodeOffset + Edge.Vertices[1]] then
    begin
      VerletWorld.CreateStick(FNodeList[FCurrentNodeOffset + Edge.Vertices[0]],
        FNodeList[FCurrentNodeOffset + Edge.Vertices[1]], Slack);
    end;
  end;
end;

procedure TGLEdgeDetector.AddEdgesAsSolidEdges(const VerletWorld: TGLVerletWorld);
var
  i: integer;
  Edge: TGLEdge;
begin
  if not FNodesAdded then
    AddNodes(VerletWorld);
  for i := 0 to EdgeList.Count - 1 do
  begin
    // if not EdgeList[i].SameSame(FNodeList) then
    Edge := EdgeList[i];
    if FNodeList[FCurrentNodeOffset + Edge.Vertices[0]] <> FNodeList
      [FCurrentNodeOffset + Edge.Vertices[1]] then
    begin
      if Edge.Solid then
        VerletWorld.AddSolidEdge(FNodeList[FCurrentNodeOffset + Edge.Vertices[0]
          ], FNodeList[FCurrentNodeOffset + Edge.Vertices[1]]);
    end;
  end;
end;

procedure TGLEdgeDetector.AddOuterEdgesAsSolidEdges(const VerletWorld
  : TGLVerletWorld);
var
  i: integer;
  Edge: TGLEdge;
begin
  if not FNodesAdded then
    AddNodes(VerletWorld);
  for i := 0 to EdgeList.Count - 1 do
  begin
    // if not EdgeList[i].SameSame(FNodeList) then
    Edge := EdgeList[i];
    if FNodeList[FCurrentNodeOffset + Edge.Vertices[0]] <> FNodeList
      [FCurrentNodeOffset + Edge.Vertices[1]] then
    begin
      if Edge.Solid and (Edge.Faces[1] = nil) then
        VerletWorld.AddSolidEdge(FNodeList[FCurrentNodeOffset + Edge.Vertices[0]
          ], FNodeList[FCurrentNodeOffset + Edge.Vertices[1]]);
    end;
  end;
end;

procedure TGLEdgeDetector.RenderEdges(var rci: TGLRenderContextInfo);
var
  i: integer;
  Edge: TGLEdge;
  Vertex0, Vertex1: TAffineVector;
begin
  if EdgeList.Count > 0 then
  begin
    rci.GLStates.Disable(stLighting);
    rci.GLStates.LineWidth := 3;
    gl.Color3f(1, 1, 1);
    gl.Begin_(GL_LINES);
    for i := 0 to EdgeList.Count - 1 do
    begin
      Edge := EdgeList[i];
      Vertex0 := Edge.MeshObject.Vertices[Edge.Vertices[0]];
      Vertex1 := Edge.MeshObject.Vertices[Edge.Vertices[1]];
      gl.Vertex3fv(PGLfloat(@Vertex0));
      gl.Vertex3fv(PGLfloat(@Vertex1));
    end;
    gl.End_;
  end;
  // }
end;

procedure TGLEdgeDetector.BuildOpposingEdges;
var
  iEdge, EdgeCount, Vi0, Vi1, iEdgeTest: integer;
  Face0, Face1: TGLFace;
  Edge, NewEdge, TestEdge: TGLEdge;
begin
  // For each edge that's connected by two triangles, create a new edge that
  // connects the two "extra" vertices.... makes sense?
  EdgeCount := EdgeList.Count;

  for iEdge := 0 to EdgeCount - 1 do
  begin
    Edge := EdgeList[iEdge];
    if Assigned(Edge.Faces[1]) then
    begin
      Face0 := Edge.Faces[0];
      Face1 := Edge.Faces[1];
      if (Face0.Vertices[0] <> Edge.Vertices[0]) and
        (Face0.Vertices[0] <> Edge.Vertices[1]) then
        Vi0 := Face0.Vertices[0]
      else if (Face0.Vertices[1] <> Edge.Vertices[0]) and
        (Face0.Vertices[1] <> Edge.Vertices[1]) then
        Vi0 := Face0.Vertices[1]
      else
        Vi0 := Face0.Vertices[2];

      if (Face1.Vertices[0] <> Edge.Vertices[0]) and
        (Face1.Vertices[0] <> Edge.Vertices[1]) then
        Vi1 := Face1.Vertices[0]
      else if (Face1.Vertices[1] <> Edge.Vertices[0]) and
        (Face1.Vertices[1] <> Edge.Vertices[1]) then
        Vi1 := Face1.Vertices[1]
      else
        Vi1 := Face1.Vertices[2];

      if (Vi0 = Vi1) or (Vi0 = Edge.Vertices[0]) or (Vi0 = Edge.Vertices[1]) or
        (Vi1 = Edge.Vertices[0]) or (Vi1 = Edge.Vertices[1]) then
        continue;
      // Find an indentical edge, if there is one
      for iEdgeTest := 0 to EdgeList.Count - 1 do
      begin
        TestEdge := EdgeList[iEdgeTest];

        if (TestEdge.Vertices[0] = Vi0) and (TestEdge.Vertices[1] = Vi1) or
          (TestEdge.Vertices[1] = Vi0) and (TestEdge.Vertices[0] = Vi1) then
        begin
          // Edge allready exists!
          inc(FEdgeDoublesSkipped);
          continue;
        end;
      end;
      NewEdge := TGLEdge.Create(self, Vi0, Vi1, nil, nil, Edge.MeshObject, false);
      EdgeList.Add(NewEdge);
      // *)
    end;
  end;
end;

function TGLEdgeDetector.AddNode(const VerletWorld: TGLVerletWorld;
  const MeshObject: TMeshObject; const VertexIndex: integer): TGLVerletNode;
var
  Location: TAffineVector;
  aNode: TGLMeshObjectVerletNode;
  i: integer;
begin
  // Is there an identical node?
  Location := MeshObject.Owner.Owner.LocalToAbsolute
    (MeshObject.Vertices[VertexIndex]);

  for i := FCurrentNodeOffset to FNodeList.Count - 1 do
  begin
    aNode := TGLMeshObjectVerletNode(FNodeList[i]);
    if VectorDistance2(Location, aNode.Location) <= FWeldDistance then
    begin
      FNodeList.Add(aNode);
      aNode.VertexIndices.Add(VertexIndex);
      result := aNode;
      exit;
    end;
  end; // *)
  aNode := TGLMeshObjectVerletNode.CreateOwned(VerletWorld);
  aNode.MeshObject := MeshObject;
  aNode.VertexIndices.Add(VertexIndex);
  aNode.Location := Location;
  aNode.OldLocation := Location;
  FNodeList.Add(aNode);
  result := aNode;
end;

procedure TGLEdgeDetector.ProcessMesh;
begin
  inherited;
  BuildOpposingEdges;
end;

procedure TGLEdgeDetector.ReplaceVertexIndex(const ViRemove,
  ViReplaceWith: integer);
var
  i: integer;
  Done: boolean;
  Edge: TGLEdge;
begin
  for i := 0 to FaceList.Count - 1 do
    with FaceList[i] do
    begin
      if Active then
      begin
        if Vertices[0] = ViRemove then
          Vertices[0] := ViReplaceWith;

        if Vertices[1] = ViRemove then
          Vertices[1] := ViReplaceWith;

        if Vertices[2] = ViRemove then
          Vertices[2] := ViReplaceWith;

        if (Vertices[0] = Vertices[1]) or (Vertices[1] = Vertices[2]) or
          (Vertices[2] = Vertices[0]) then
          Active := false;
      end;
    end;
  Done := false;
  while not Done do
  begin
    Done := true;
    for i := 0 to EdgeList.Count - 1 do
      with EdgeList[i] do
      begin
        if (Vertices[0] = ViRemove) or (Vertices[1] = ViRemove) then
        begin
          if Vertices[0] = ViRemove then
            Vertices[0] := ViReplaceWith;

          if Vertices[1] = ViRemove then
            Vertices[1] := ViReplaceWith;
          UpdateEdgeLength;
          Edge := EdgeList[i];
          EdgeList.Delete(i);
          if Edge.Length = -1 then
            Edge.Free
          else
            EdgeList.InsertSorted(Edge);
          Done := false;
          break; // }
        end;
      end;
  end;
end;

// --------------------------
// TGLFace
// --------------------------
constructor TGLFace.Create(aMeshObject: TMeshObject);
begin
  MeshObject := aMeshObject;
  Active := true;
end;

procedure TGLFace.UpdateNormal;
begin
  CalcPlaneNormal(MeshObject.Vertices[Vertices[0]],
    MeshObject.Vertices[Vertices[1]], MeshObject.Vertices[Vertices[2]], Normal);
end;

// ------------------
// TGLEdge
// ------------------
procedure TGLEdge.Contract;
begin
  // We're removing vertex 1 and replacing it with vertex 0
  FOwner.ReplaceVertexIndex(Vertices[1], Vertices[0]);
  // MeshObject.Vertices[Vertices[0]] := MeshObject.Vertices[Vertices[1]];
  Length := -1;
end;

constructor TGLEdge.Create(const AOwner: TGLEdgeDetector; AVi0, AVi1: integer;
  AFace0, AFace1: TGLFace; aMeshObject: TMeshObject; ASolid: boolean);
begin
  FOwner := AOwner;
  Vertices[0] := AVi0;
  Vertices[1] := AVi1;
  Faces[0] := AFace0;
  Faces[1] := AFace1;
  FMeshObject := aMeshObject;
  FSolid := true;
  UpdateEdgeLength;
end;

procedure TGLEdge.UpdateEdgeLength;
begin
  if FOwner.FCalcEdgeLength then
  begin
    if Vertices[0] = Vertices[1] then
      Length := -1
    else
      Length := VectorDistance(FOwner.GLBaseMesh.LocalToAbsolute
        (FMeshObject.Vertices[Vertices[0]]),
        FOwner.GLBaseMesh.LocalToAbsolute(FMeshObject.Vertices[Vertices[1]]));
  end;
end;

end.
