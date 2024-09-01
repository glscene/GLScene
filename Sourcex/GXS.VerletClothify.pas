//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.VerletClothify;

(* Methods for turning a TgxBaseMesh into a Verlet cloth / jelly *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,

  System.Classes,
  System.SysUtils,

  GXS.VectorTypes,
  GXS.VectorLists,
  GXS.VectorGeometry,
  GXS.VectorFileObjects,
  GXS.VerletTypes,
  GXS.Texture,
  GXS.RenderContextInfo,
  GXS.State,
  GXS.Context;


type
  (* Class that represents a face. This structure is not used for rendering, but
  for extracting info from meshes *)
  TFace = class
  public
    Vertices : array[0..2] of integer;
    Normal : TAffineVector;
    MeshObject : TgxMeshObject;
    Active : boolean;
    procedure UpdateNormal;
    constructor Create(aMeshObject : TgxMeshObject);
  end;

  { List of faces }
  TFaceList = class(TList)
  private
    function GetItems(i: integer): TFace;
    procedure SetItems(i: integer; const Value: TFace);
  public
    property Items[i : integer] : TFace read GetItems write SetItems; default;
  end;

  // Class that extracts faces from a GLBaseMesh
  TFaceExtractor = class
  private
    FFaceList : TFaceList;
    FGLBaseMesh : TgxBaseMesh;
    FNodeList : TgxVerletNodeList;
    FWeldDistance: single;
    FEdgeDoublesSkipped : integer;
    procedure SetWeldDistance(const Value: single);
  protected
    procedure ProcessMeshObject(const MeshObject : TgxMeshObject); virtual;
  public
    procedure ExtractFacesFromVertexIndexList(
      const FaceGroup : TfgxVertexIndexList; const MeshObject : TgxMeshObject);
    property FaceList : TFaceList read FFaceList;
    procedure Clear; virtual;
    procedure ProcessMesh; virtual;
    property WeldDistance : single read FWeldDistance write SetWeldDistance;
    property EdgeDoublesSkipped : integer read FEdgeDoublesSkipped;
    property GLBaseMesh : TgxBaseMesh read FGLBaseMesh;
    property NodeList : TgxVerletNodeList read FNodeList;
    function AddFace(const Vi0, Vi1, Vi2 : integer; const MeshObject : TgxMeshObject) : TFace; virtual;
    constructor Create(const aGLBaseMesh : TgxBaseMesh); virtual;
    destructor Destroy; override;
  end;

  // *** EDGE DETECTOR
  TEdgeDetector = class;
  TEdge = class
  private
    FSolid: boolean;
    FLength: single;
    FMeshObject: TgxMeshObject;
    FOwner: TEdgeDetector;
  public
    Vertices : array[0..1] of integer;
    Faces : array[0..1] of TFace;
    procedure Contract;
    property Owner : TEdgeDetector read FOwner;
    property MeshObject : TgxMeshObject read FMeshObject write FMeshObject;
    property Length : single read FLength write FLength;
    property Solid : boolean read FSolid write FSolid;
    procedure UpdateEdgeLength;
    constructor Create(const AOwner: TEdgeDetector; AVi0, AVi1 : integer;
      AFace0, AFace1 : TFace; AMeshObject : TgxMeshObject; ASolid : boolean);
  end;

  TEdgeList = class(TList)
  private
    function GetItems(i: integer): TEdge;
    procedure SetItems(i: integer; const Value: TEdge);
  public
    property Items[i : integer] : TEdge read GetItems write SetItems; default;
    procedure SortByLength;
    function InsertSorted(AEdge : TEdge) : integer;
  end;

  TEdgeDetector = class(TFaceExtractor)
  private
    FEdgeList : TEdgeList;
    FCurrentNodeOffset : integer;
    FNodesAdded : boolean;
    procedure BuildOpposingEdges;
  protected
    FCalcEdgeLength : boolean;
  public
    property EdgeList : TEdgeList read FEdgeList;
    procedure Clear; override;
    procedure ProcessMesh; override;
    function AddEdge(const Vi0, Vi1 : integer; const Face : TFace; const AMeshObject : TgxMeshObject) : TEdge;
    function AddFace(const Vi0, Vi1, Vi2 : integer; const MeshObject : TgxMeshObject) : TFace; override;
    function AddNode(const VerletWorld : TgxVerletWorld; const MeshObject : TgxMeshObject; const VertexIndex : integer) : TgxVerletNode; virtual;
    procedure AddNodes(const VerletWorld : TgxVerletWorld);
    procedure AddEdgesAsSticks(const VerletWorld : TgxVerletWorld; const Slack : single);
    procedure AddEdgesAsSprings(const VerletWorld : TgxVerletWorld; const Strength, Damping, Slack : single);
    procedure AddEdgesAsSolidEdges(const VerletWorld : TgxVerletWorld);
    procedure AddOuterEdgesAsSolidEdges(const VerletWorld : TgxVerletWorld);
    procedure RenderEdges(var rci : TgxRenderContextInfo);
    property CurrentNodeOffset : integer read FCurrentNodeOffset;
    property NodesAdded : boolean read FNodesAdded;
    procedure ReplaceVertexIndex(const ViRemove, ViReplaceWith : integer);
    constructor Create(const aGLBaseMesh : TgxBaseMesh); override;
    destructor Destroy; override;
  end;

  TgxMeshObjectVerletNode = class(TgxVerletNode)
  private
    MeshObject : TgxMeshObject;
    VertexIndices : TgxIntegerList;
  public
    procedure AfterProgress; override;

    constructor CreateOwned(const aOwner : TgxVerletWorld); override;
    destructor Destroy; override;
  end;

//---------------------------------------------------------------------------
implementation
//---------------------------------------------------------------------------


//----------------------------------------
// TFaceExtractor
//----------------------------------------

procedure TFaceExtractor.Clear;
var
  i : integer;
begin
  for i := 0 to FaceList.Count-1 do
    FaceList[i].Free;

  FaceList.Clear;
end;

constructor TFaceExtractor.Create(const aGLBaseMesh : TgxBaseMesh);
begin
  FFaceList := TFaceList.Create;
  FGLBaseMesh := aGLBaseMesh;
  FNodeList := TgxVerletNodeList.Create;
  FWeldDistance := 0.01;
end;

destructor TFaceExtractor.Destroy;
begin
  Clear;
  FreeAndNil(FNodeList);
  FreeAndNil(FFaceList);
  inherited;
end;

procedure TFaceExtractor.ExtractFacesFromVertexIndexList(
  const FaceGroup : TfgxVertexIndexList; const MeshObject : TgxMeshObject);
var
  List : PIntegerArray;
  iFace, iVertex  : integer;
begin
  case FaceGroup.Mode of

    fgmmTriangles, fgmmFlatTriangles :
    begin
      for iFace := 0 to FaceGroup.TriangleCount - 1 do
      begin
        List := @FaceGroup.VertexIndices.List[iFace * 3 + 0];
        AddFace(List^[0], List^[1], List^[2], MeshObject);
      end;
    end;

    fgmmTriangleStrip :
    begin
      for iFace:=0 to FaceGroup.VertexIndices.Count-3 do
      begin
        List := @FaceGroup.VertexIndices.List[iFace];
        if (iFace and 1)=0 then
           AddFace(List^[0], List^[1], List^[2], MeshObject)
        else
           AddFace(List^[2], List^[1], List^[0], MeshObject);
      end;
    end;

    fgmmTriangleFan :
    begin
      List := @FaceGroup.VertexIndices.List;

      for iVertex:=2 to FaceGroup.VertexIndices.Count-1 do
        AddFace(List^[0], List^[iVertex-1], List^[iVertex], MeshObject)
    end;
    else
      Assert(false,'Not supported');
  end;
end;

procedure TFaceExtractor.ProcessMesh;
var
  iMeshObject : integer;
  MeshObject : TgxMeshObject;
begin
  for iMeshObject := 0 to FGLBaseMesh.MeshObjects.Count - 1 do
  begin
    MeshObject := FGLBaseMesh.MeshObjects[iMeshObject];

    ProcessMeshObject(MeshObject);
  end;
end;

procedure TFaceExtractor.ProcessMeshObject(const MeshObject : TgxMeshObject);
var
 iFaceGroup : integer;
begin
  if MeshObject.Mode = momFaceGroups then
  begin
    for iFaceGroup := 0 to MeshObject.FaceGroups.Count - 1 do
    begin
      if MeshObject.FaceGroups[iFaceGroup] is TfgxVertexIndexList then
      begin
        ExtractFacesFromVertexIndexList(MeshObject.FaceGroups[iFaceGroup] as TfgxVertexIndexList, MeshObject);
      end else
        Assert(false);
    end;
  end else
    Assert(false);
end;

function TFaceExtractor.AddFace(const Vi0, Vi1, Vi2: integer; const MeshObject : TgxMeshObject) : TFace;
var
  Face : TFace;
begin
  Face := TFace.Create(MeshObject);
  FaceList.Add(Face);
  Face.Vertices[0] := Vi0;
  Face.Vertices[1] := Vi1;
  Face.Vertices[2] := Vi2;
  result := Face;
end;

procedure TFaceExtractor.SetWeldDistance(const Value: single);
begin
  FWeldDistance := Value;
end;

//----------------------------------------
// TFaceList
//----------------------------------------

function TFaceList.GetItems(i: integer): TFace;
begin
  result := TFace(Get(i));
end;

procedure TFaceList.SetItems(i: integer; const Value: TFace);
begin
  Put(i, Value);
end;

//----------------------------------------
// TEdgeList
//----------------------------------------

function TEdgeList.GetItems(i: integer): TEdge;
begin
  result := TEdge(Get(i));
end;

function TEdgeList.InsertSorted(AEdge: TEdge): integer;
var
  i : integer;
begin
  for i := 0 to Count-1 do
  begin
    if AEdge.Length<Items[i].Length then
    begin
      Insert(i, AEdge);
      result := i;
      exit;
    end;
  end;

  result := Add(AEdge);
end;

procedure TEdgeList.SetItems(i: integer; const Value: TEdge);
begin
  Put(i, Value);
end;

function EdgeLength(Item1, Item2 : pointer) : integer;
begin
  if TEdge(Item1).Length < TEdge(Item2).Length then
    result := -1

  else if TEdge(Item1).Length = TEdge(Item2).Length then
    result := 0

  else
    result := 1;
end;

procedure TEdgeList.SortByLength;
begin
  Sort(@EdgeLength);
end;

//----------------------------------------
// TgxMeshObjectVerletNode
//----------------------------------------

constructor TgxMeshObjectVerletNode.CreateOwned(const aOwner: TgxVerletWorld);
begin
  inherited;
  VertexIndices := TgxIntegerList.Create;
end;

destructor TgxMeshObjectVerletNode.Destroy;
begin
  VertexIndices.Free;
  inherited;
end;

procedure TgxMeshObjectVerletNode.AfterProgress;
var
  i : integer;
begin
  // Update the actual vertex
  for i := 0 to VertexIndices.Count-1 do
    MeshObject.Vertices[VertexIndices[i]] := MeshObject.Owner.Owner.AbsoluteToLocal(Location);
end;

//----------------------------------------
// TEdgeDetector
//----------------------------------------

procedure TEdgeDetector.Clear;
var
  i : integer;
begin
  inherited;

  for i := 0 to EdgeList.Count-1 do
    EdgeList[i].Free;

  EdgeList.Clear;

  FCurrentNodeOffset := 0;
  FNodesAdded := false;
end;

constructor TEdgeDetector.Create(const aGLBaseMesh: TgxBaseMesh);
begin
  FEdgeList := TEdgeList.Create;
  FCurrentNodeOffset := 0;
  FNodesAdded := false;
  FCalcEdgeLength := false;
  inherited;
end;

destructor TEdgeDetector.Destroy;
begin
  inherited;
  FreeAndNil(FEdgeList);
end;

function TEdgeDetector.AddEdge(const Vi0, Vi1: integer; const Face: TFace; const AMeshObject : TgxMeshObject): TEdge;
var
  i : integer;
  Edge : TEdge;
begin
  // Find an indentical edge, if there is one
  for i := 0 to EdgeList.Count - 1 do
  begin
    Edge := EdgeList[i];
    if (Edge.Vertices[0]=Vi0) and (Edge.Vertices[1]=Vi1) or
       (Edge.Vertices[1]=Vi0) and (Edge.Vertices[0]=Vi1) then
    begin
      Edge.Faces[1] := Face;
      result := Edge;
      exit;
    end;
  end;
  // No edge was found, create a new one
  Edge := TEdge.Create(self, Vi0, Vi1, Face, nil, AMeshObject, true);
  EdgeList.Add(Edge);
  result := Edge;
end;

function TEdgeDetector.AddFace(const Vi0, Vi1, Vi2: integer;
  const MeshObject: TgxMeshObject): TFace;
var
  Face : TFace;
begin
  Face := TFace.Create(MeshObject);
  FaceList.Add(Face);
  Face.Vertices[0] := Vi0;
  Face.Vertices[1] := Vi1;
  Face.Vertices[2] := Vi2;
  AddEdge(Vi0, Vi1, Face, MeshObject);
  AddEdge(Vi1, Vi2, Face, MeshObject);
  AddEdge(Vi2, Vi0, Face, MeshObject);//}
  result := Face;
end;

procedure TEdgeDetector.AddNodes(const VerletWorld : TgxVerletWorld);
var
  i : integer;
  MO : TgxMeshObject;
begin
  FNodesAdded := true;
  FCurrentNodeOffset := FNodeList.Count;
  MO := FGLBaseMesh.MeshObjects[0];
  for i := 0 to MO.Vertices.Count-1 do
    AddNode(VerletWorld, MO, i);
  // Assert(FNodeList.Count = MO.Vertices.Count, Format('%d <> %d',[FNodeList.Count, MO.Vertices.Count]));
end;

procedure TEdgeDetector.AddEdgesAsSprings(const VerletWorld : TgxVerletWorld;
  const Strength, Damping, Slack: single);
var
  i : integer;
  Edge : TEdge;
begin
  if not FNodesAdded then
    AddNodes(VerletWorld);

  for i := 0 to EdgeList.Count-1 do
  begin
    // if not EdgeList[i].SameSame(FNodeList) then
    Edge := EdgeList[i];
    if FNodeList[FCurrentNodeOffset+Edge.Vertices[0]] <> FNodeList[FCurrentNodeOffset+Edge.Vertices[1]] then
    begin
      VerletWorld.CreateSpring(
        FNodeList[FCurrentNodeOffset+Edge.Vertices[0]],
        FNodeList[FCurrentNodeOffset+Edge.Vertices[1]],
        Strength, Damping, Slack);
    end;
  end;
end;

procedure TEdgeDetector.AddEdgesAsSticks(const VerletWorld : TgxVerletWorld;
  const Slack : single);
var
  i : integer;
  Edge : TEdge;
begin
  if not FNodesAdded then
    AddNodes(VerletWorld);
  for i := 0 to EdgeList.Count-1 do
  begin
    // if not EdgeList[i].SameSame(FNodeList) then
    Edge := EdgeList[i];
    if FNodeList[FCurrentNodeOffset+Edge.Vertices[0]] <> FNodeList[FCurrentNodeOffset+Edge.Vertices[1]] then
    begin
      VerletWorld.CreateStick(
        FNodeList[FCurrentNodeOffset + Edge.Vertices[0]],
        FNodeList[FCurrentNodeOffset + Edge.Vertices[1]],
        Slack);
    end;
  end;
end;

procedure TEdgeDetector.AddEdgesAsSolidEdges(
  const VerletWorld: TgxVerletWorld);
var
  i : integer;
  Edge : TEdge;
begin
  if not FNodesAdded then
    AddNodes(VerletWorld);
  for i := 0 to EdgeList.Count-1 do
  begin
    // if not EdgeList[i].SameSame(FNodeList) then
    Edge := EdgeList[i];
    if FNodeList[FCurrentNodeOffset+Edge.Vertices[0]] <> FNodeList[FCurrentNodeOffset+Edge.Vertices[1]] then
    begin
      if Edge.Solid then
        VerletWorld.AddSolidEdge(
          FNodeList[FCurrentNodeOffset + Edge.Vertices[0]],
          FNodeList[FCurrentNodeOffset + Edge.Vertices[1]]);
    end;
  end;
end;

procedure TEdgeDetector.AddOuterEdgesAsSolidEdges(
  const VerletWorld: TgxVerletWorld);
var
  i : integer;
  Edge : TEdge;
begin
  if not FNodesAdded then
    AddNodes(VerletWorld);

  for i := 0 to EdgeList.Count-1 do
  begin
    // if not EdgeList[i].SameSame(FNodeList) then
    Edge := EdgeList[i];
    if FNodeList[FCurrentNodeOffset+Edge.Vertices[0]] <> FNodeList[FCurrentNodeOffset+Edge.Vertices[1]] then
    begin
      if Edge.Solid and (Edge.Faces[1]=nil) then
        VerletWorld.AddSolidEdge(
          FNodeList[FCurrentNodeOffset + Edge.Vertices[0]],
          FNodeList[FCurrentNodeOffset + Edge.Vertices[1]]);
    end;
  end;
end;

procedure TEdgeDetector.RenderEdges(var rci: TgxRenderContextInfo);
var
  i : integer;
  Edge : TEdge;
  Vertex0, Vertex1 : TAffineVector;
begin
  if EdgeList.Count>0 then
  begin
    rci.gxStates.Disable(stLighting);
    rci.gxStates.LineWidth := 3;
    glColor3f(1,1,1);

    glBegin(GL_LINES);
      for i := 0 to EdgeList.Count - 1 do
      begin
        Edge := EdgeList[i];

        Vertex0 := Edge.MeshObject.Vertices[Edge.Vertices[0]];
        Vertex1 := Edge.MeshObject.Vertices[Edge.Vertices[1]];

        glVertex3fv(PGLfloat(@Vertex0));
        glVertex3fv(PGLfloat(@Vertex1));
      end;
    glEnd;
  end;//}
end;

procedure TEdgeDetector.BuildOpposingEdges;
var
  iEdge, EdgeCount, vi0, vi1, iEdgeTest : integer;
  Face0, Face1 : TFace;
  Edge, NewEdge, TestEdge : TEdge;
begin
  // For each edge that's connected by two triangles, create a new edge that
  // connects the two "extra" vertices.... makes sense?
  EdgeCount := EdgeList.Count;

  for iEdge := 0 to EdgeCount-1 do
  begin
    Edge := EdgeList[iEdge];
    if Assigned(Edge.Faces[1]) then
    begin
      Face0 := Edge.Faces[0];
      Face1 := Edge.Faces[1];
      if (Face0.Vertices[0] <> Edge.Vertices[0]) and (Face0.Vertices[0] <> Edge.Vertices[1]) then
        vi0 := Face0.Vertices[0]
      else if (Face0.Vertices[1] <> Edge.Vertices[0]) and (Face0.Vertices[1] <> Edge.Vertices[1]) then
        vi0 := Face0.Vertices[1]
      else
        vi0 := Face0.Vertices[2];
      if (Face1.Vertices[0] <> Edge.Vertices[0]) and (Face1.Vertices[0] <> Edge.Vertices[1]) then
        vi1 := Face1.Vertices[0]
      else if (Face1.Vertices[1] <> Edge.Vertices[0]) and (Face1.Vertices[1] <> Edge.Vertices[1]) then
        vi1 := Face1.Vertices[1]
      else
        vi1 := Face1.Vertices[2];
      if (vi0=vi1) or
         (vi0=Edge.Vertices[0]) or
         (vi0=Edge.Vertices[1]) or
         (vi1=Edge.Vertices[0]) or
         (vi1=Edge.Vertices[1]) then
        continue;
      // Find an indentical edge, if there is one
      for iEdgeTest := 0 to EdgeList.Count - 1 do
      begin
        TestEdge := EdgeList[iEdgeTest];

        if (TestEdge.Vertices[0]=Vi0) and (TestEdge.Vertices[1]=Vi1) or
           (TestEdge.Vertices[1]=Vi0) and (TestEdge.Vertices[0]=Vi1) then
        begin
          // Edge allready exists!
          inc(FEdgeDoublesSkipped);
          continue;
        end;
      end;
      NewEdge := TEdge.Create(self, Vi0, Vi1, nil, nil, Edge.MeshObject, false);
      EdgeList.Add(NewEdge);//}
    end;
  end;
end;

function TEdgeDetector.AddNode(const VerletWorld : TgxVerletWorld; const MeshObject: TgxMeshObject;
  const VertexIndex: integer): TgxVerletNode;
var
  Location : TAffineVector;
  aNode : TgxMeshObjectVerletNode;
  i : integer;
begin
  // Is there an identical node?
  Location := MeshObject.Owner.Owner.LocalToAbsolute(MeshObject.Vertices[VertexIndex]);

  for i := FCurrentNodeOffset to FNodeList.Count-1 do
  begin
    aNode := TgxMeshObjectVerletNode(FNodeList[i]);

    if VectorDistance2(Location, aNode.Location)<=FWeldDistance then
    begin
      FNodeList.Add(aNode);
      aNode.VertexIndices.Add(VertexIndex);
      Result:=aNode;
      Exit;
    end;
  end;//}
  aNode := TgxMeshObjectVerletNode.CreateOwned(VerletWorld);
  aNode.MeshObject := MeshObject;
  aNode.VertexIndices.Add(VertexIndex);
  aNode.Location := Location;
  aNode.OldLocation := Location;
  FNodeList.Add(aNode);
  Result:=aNode;
end;

procedure TEdgeDetector.ProcessMesh;
begin
  inherited;
  BuildOpposingEdges;
end;

procedure TEdgeDetector.ReplaceVertexIndex(const ViRemove,
  ViReplaceWith: integer);
var
  i : integer;
  Done : boolean;
  Edge : TEdge;
begin
  for i := 0 to FaceList.Count-1 do
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
        if (Vertices[0]=Vertices[1]) or
         (Vertices[1]=Vertices[2]) or
         (Vertices[2]=Vertices[0]) then
          Active := false;
      end;
    end;
  Done := false;
  while not Done do
  begin
    Done := true;
    for i := 0 to EdgeList.Count-1 do
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
          if Edge.Length=-1 then
            Edge.Free
          else
            EdgeList.InsertSorted(Edge);
          Done := false;
          break;//}
        end;
      end;
  end;
end;

//----------------------------------------
// TFace
//----------------------------------------

constructor TFace.Create(aMeshObject: TgxMeshObject);
begin
  MeshObject := aMeshObject;
  Active := true;
end;

procedure TFace.UpdateNormal;
begin
  CalcPlaneNormal(
    MeshObject.Vertices[Vertices[0]],
    MeshObject.Vertices[Vertices[1]],
    MeshObject.Vertices[Vertices[2]], Normal);
end;

//----------------------------------------
// TEdge
//----------------------------------------

procedure TEdge.Contract;
begin
  // We're removing vertex 1 and replacing it with vertex 0
  FOwner.ReplaceVertexIndex(Vertices[1], Vertices[0]);
  //MeshObject.Vertices[Vertices[0]] := MeshObject.Vertices[Vertices[1]];
  Length := -1;
end;

constructor TEdge.Create(const AOwner: TEdgeDetector; AVi0, AVi1 : integer;
  AFace0, AFace1 : TFace; AMeshObject : TgxMeshObject; ASolid : boolean);
begin
  FOwner := AOwner;
  Vertices[0] := AVi0;
  Vertices[1] := AVi1;
  Faces[0] := AFace0;
  Faces[1] := AFace1;
  FMeshObject := AMeshObject;
  FSolid := true;
  UpdateEdgeLength;
end;

procedure TEdge.UpdateEdgeLength;
begin
  if FOwner.FCalcEdgeLength then
  begin
    if Vertices[0] = Vertices[1] then
      Length := -1
    else
      Length := VectorDistance(
        FOwner.GLBaseMesh.LocalToAbsolute(FMeshObject.Vertices[Vertices[0]]),
        FOwner.GLBaseMesh.LocalToAbsolute(FMeshObject.Vertices[Vertices[1]]));
  end;
end;

end.
