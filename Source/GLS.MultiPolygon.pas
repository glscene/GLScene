//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.MultiPolygon;

(* Object with support for complex polygons. 
   When the tesselator finds an intersection of edges it wants us to give him some storage
   for this new vertex, and he wants a pointer (see tessCombine). The pointers taken from
   TGLAffineVectorList become invalid after enlarging the capacity (makes a ReAllocMem), which
   can happen implicitly while adding. The TGLVectorPool keeps all pointers valid until the
   destruction itself.
   Reactivated the TGLVectorPool object. The GLS.VectorLists are not suitable for this job.
   If anyone feels responsible: it would be fine to have a method ImportFromFile (dxf?) in
   the TGLContour and TGLMultiPolygonBase objects...
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.OpenGLAdapter,
  GLS.Spline,
  GLS.XOpenGL,
  GLS.Context,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.PersistentClasses,
  GLS.Scene,
  GLS.Objects,
  GLS.GeomObjects,
  GLS.Nodes,
  GLS.BaseClasses,
  GLS.Coordinates,
  GLS.RenderContextInfo;

type
  TGLContourNodes = class(TGLNodes)
  public
    procedure NotifyChange; override;
  end;

  TGLContour = class(TCollectionItem)
  private
    FNodes: TGLContourNodes;
    FDivision: Integer;
    FSplineMode: TGLLineSplineMode;
    FDescription: string;
    procedure SetNodes(const Value: TGLContourNodes);
    procedure SetDivision(Value: Integer);
    procedure SetSplineMode(const Value: TGLLineSplineMode);
    procedure SetDescription(const Value: string);
  protected
    procedure CreateNodes;
    procedure NodesChanged(Sender: TObject);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Description: string read FDescription write SetDescription;
    // The nodes list
    property Nodes: TGLContourNodes read FNodes write SetNodes;
    (* Number of divisions for each segment in spline modes.
      Minimum 1 (disabled), ignored in lsmLines mode. *)
    property Division: Integer read FDivision write SetDivision default 10;
    (* Default spline drawing mode.
      This mode is used only for the curve, not for the rotation path. *)
    property SplineMode: TGLLineSplineMode read FSplineMode write SetSplineMode default lsmLines;
  end;

  TGLContourClass = class of TGLContour;

  TGLContours = class(TGLNotifyCollection)
  private
    function GetItems(index: Integer): TGLContour;
    procedure SetItems(index: Integer; const Value: TGLContour);
  protected
  public
    constructor Create(AOwner: TComponent); overload;
    function Add: TGLContour; inline;
    function FindItemID(ID: Integer): TGLContour; inline;
    property Items[index: Integer]: TGLContour read GetItems write SetItems; default;
    procedure GetExtents(var min, max: TAffineVector);
  end;

  TGLPolygonList = class(TGLPersistentObjectList)
  private
    FAktList: TGLAffineVectorList;
    function GetList(I: Integer): TGLAffineVectorList;
  public
    procedure Add;
    property AktList: TGLAffineVectorList read FAktList;
    property List[I: Integer]: TGLAffineVectorList read GetList;
  end;

  (* Multipolygon is defined with multiple contours.
     The contours have to be in the X-Y plane, otherwise they are projected
     to it (this is done automatically by the tesselator).
     The plane normal is pointing in +Z. All contours are automatically closed,
     so there is no need to specify the last node equal to the first one.
     Contours should be defined counterclockwise, the first contour (index = 0)
     is taken as is, all following are reversed. This means you can define the
     outer contour first and the holes and cutouts after that. If you give the
     following contours in clockwise order, the first contour is extended.
     TGLMultiPolygonBase will take the input contours and let the tesselator
     make an outline from it (this is done in RetreiveOutline). This outline is
     used for Rendering. Only when there are changes in the contours, the
     outline will be recalculated. The ouline in fact is a list of GLS.VectorLists. *)
  TGLMultiPolygonBase = class(TGLSceneObject)
  private
    FContours: TGLContours;
    FOutline: TGLPolygonList;
    FContoursNormal: TAffineVector;
    FAxisAlignedDimensionsCache: TGLVector;
    procedure SetContours(const Value: TGLContours);
    function GetPath(i: Integer): TGLContourNodes;
    procedure SetPath(i: Integer; const value: TGLContourNodes);
    function GetOutline: TGLPolygonList;
    procedure SetContoursNormal(const Value: TAffineVector);
  protected
    procedure RenderTesselatedPolygon(textured: Boolean;
      normal: PAffineVector; invertNormals: Boolean);
    procedure RetrieveOutline(List: TGLPolygonList);
    procedure ContourChanged(Sender: TObject); virtual;
    //property PNormal:PAffineVector read FPNormal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddNode(const i: Integer; const coords: TGLCoordinates); overload;
    procedure AddNode(const i: Integer; const X, Y, Z: TGLfloat); overload;
    procedure AddNode(const i: Integer; const value: TGLVector); overload;
    procedure AddNode(const i: Integer; const value: TAffineVector); overload;
    property Path[i: Integer]: TGLContourNodes read GetPath write SetPath;
    property Outline: TGLPolygonList read GetOutline;
    property ContoursNormal: TAffineVector read FContoursNormal write SetContoursNormal;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    procedure StructureChanged; override;
  published
    property Contours: TGLContours read FContours write SetContours;
  end;

  (* A polygon that can have holes and multiple contours.
     Use the Path property to access a contour or one of the AddNode methods
     to add a node to a contour (contours are allocated automatically). *)
  TGLMultiPolygon = class(TGLMultiPolygonBase)
  private
    FParts: TGLPolygonParts;
  protected
    procedure SetParts(const value: TGLPolygonParts);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
  published
    property Parts: TGLPolygonParts read FParts write SetParts default [ppTop, ppBottom];
  end;

  (* Page oriented pointer array, with persistent pointer target memory.
    In TGLVectorList a pointer to a vector will not be valid any more after
    a call to SetCapacity, which might be done implicitely during Add.
    The TGLVectorPool keeps memory in its original position during its
    whole lifetime. *)
  TGLVectorPool = class(TList)
  private
    FEntrySize: Integer; // size of each entry
    FPageSize: Integer; // number of entries per page
    FArrSize: Integer; // size of one page
    FUsedEntries: Integer; // used entries in actual page
    FAktArray: GLS.VectorGeometry.PByteArray; // pointer to actual page
    procedure CreatePage; // creates new page
  public
    constructor Create(APageSize, AEntrySize: Integer);
    destructor Destroy; override;
    { retrieve pointer to new entry. will create new page if needed }
    procedure GetNewVector(var P: Pointer);
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

//----------------------------------------
// TGLVectorPool
//----------------------------------------

constructor TGLVectorPool.Create(APageSize, AEntrySize: Integer);
begin
  inherited Create;
  Assert(APageSize > 0);
  Assert(AEntrySize > 0);
  FPageSize := APageSize;
  FEntrySize := AEntrySize;
  FArrSize := FPageSize * FEntrySize;
  CreatePage;
end;

procedure TGLVectorPool.CreatePage;
begin
  GetMem(FAktArray, FArrSize);
  Add(FAktArray);
  FUsedEntries := 0;
end;

destructor TGLVectorPool.Destroy;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    FreeMem(Items[i], FArrSize);
  inherited;
end;

procedure TGLVectorPool.GetNewVector(var P: Pointer);
begin
  if FUsedEntries >= FPageSize then
    CreatePage;
  Inc(FUsedEntries);
  P := @(FAktArray[(FUsedEntries - 1) * FEntrySize]);
end;

// ------------------
// ------------------ TGLPolygonList ------------------
// ------------------

procedure TGLPolygonList.Add;
begin
  FAktList := TGLAffineVectorList.Create;
  inherited Add(FAktList);
end;


function TGLPolygonList.GetList(i: Integer): TGLAffineVectorList;
begin
  Result := TGLAffineVectorList(Items[i]);
end;

// ------------------
// ------------------ TGLContour ------------------
// ------------------

constructor TGLContour.Create(Collection: TCollection);
begin
  inherited;
  CreateNodes;
  FDivision := 10;
  FSplineMode := lsmLines;
end;

procedure TGLContour.CreateNodes;
begin
  FNodes := TGLContourNodes.Create(Self);
end;

destructor TGLContour.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TGLContour.Assign(Source: TPersistent);
begin
  if Source is TGLContour then
  begin
    FNodes.Assign(TGLContour(Source).FNodes);
    FDivision := TGLContour(Source).FDivision;
    FSplineMode := TGLContour(Source).FSplineMode;
    FDescription := TGLContour(Source).FDescription;
  end
  else
    inherited;
end;

function TGLContour.GetDisplayName: string;
begin
  result := Description;
  if result = '' then
    result := Format('GLContour: %d nodes', [Nodes.Count]);
end;

procedure TGLContour.NodesChanged(Sender: TObject);
begin
  Changed(false);
end;

procedure TGLContour.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TGLContour.SetDivision(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FDivision then
  begin
    FDivision := value;
    Changed(false);
  end;
end;

procedure TGLContour.SetNodes(const Value: TGLContourNodes);
begin
  FNodes.Assign(Value);
  Changed(false);
end;

procedure TGLContour.SetSplineMode(const Value: TGLLineSplineMode);
begin
  if FSplineMode <> value then
  begin
    FSplineMode := value;
    Changed(false);
  end;
end;

//-----------------------------
// TGLContours
//-----------------------------

function TGLContours.Add: TGLContour;
begin
  Result := TGLContour(inherited Add);
end;

constructor TGLContours.Create(AOwner: TComponent);
begin
  Create(AOwner, TGLContour);
end;

function TGLContours.FindItemID(ID: Integer): TGLContour;
begin
  result := TGLContour(inherited FindItemId(Id));
end;

function TGLContours.GetItems(index: Integer): TGLContour;
begin
  result := TGLContour(inherited Items[index]);
end;

procedure TGLContours.SetItems(index: Integer; const Value: TGLContour);
begin
  inherited Items[index] := value;
end;


procedure TGLContours.GetExtents(var min, max: TAffineVector);
var
  i, k: Integer;
  lMin, lMax: TAffineVector;
const
  cBigValue: Single = 1e30;
  cSmallValue: Single = -1e30;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to Count - 1 do
  begin
    GetItems(i).Nodes.GetExtents(lMin, lMax);
    for k := 0 to 2 do
    begin
      if lMin.V[k] < min.V[k] then min.V[k] := lMin.V[k];
      if lMax.V[k] > max.V[k] then max.V[k] := lMax.V[k];
    end;
  end;
end;

//---------------------------------
// TGLMultiPolygonBase
//---------------------------------

constructor TGLMultiPolygonBase.Create(AOwner: TComponent);
begin
  inherited;
  FContours := TGLContours.Create(Self);
  FContours.OnNotifyChange := ContourChanged;
  FContoursNormal := AffineVectorMake(0, 0, 1);
  FAxisAlignedDimensionsCache.X := -1;
end;

destructor TGLMultiPolygonBase.Destroy;
begin
  if FOutline <> nil then
  begin
    FOutline.Clean;
    FreeAndNil(FOutline);
  end;  
  FContours.Free;
  inherited;
end;


procedure TGLMultiPolygonBase.Assign(Source: TPersistent);
begin
  if Source is TGLMultiPolygonBase then
  begin
    FContours.Assign(TGLMultiPolygonBase(Source).FContours);
  end;
  inherited;
end;


procedure TGLMultiPolygonBase.ContourChanged(Sender: TObject);
begin
  if Assigned(FOutline) then
  begin
    // force a RetrieveOutline with next Render
    FOutline.Clean;
    FreeAndNil(FOutline);
    StructureChanged;
  end;
end;


procedure TGLMultiPolygonBase.AddNode(const i: Integer; const value: TGLVector);
begin
  Path[i].AddNode(value);
end;


procedure TGLMultiPolygonBase.AddNode(const i: Integer; const x, y, z: TGLfloat);
begin
  Path[i].AddNode(x, y, z);
end;


procedure TGLMultiPolygonBase.AddNode(const i: Integer; const coords: TGLCoordinates);
begin
  Path[i].AddNode(coords);
end;


procedure TGLMultiPolygonBase.AddNode(const I: Integer; const value: TAffineVector);
begin
  Path[i].AddNode(value);
end;


procedure TGLMultiPolygonBase.SetContours(const Value: TGLContours);
begin
  FContours.Assign(Value);
end;


function TGLMultiPolygonBase.GetOutline: TGLPolygonList;
begin
  if not Assigned(FOutline) then
  begin
    FOutline := TGLPolygonList.Create;
    RetrieveOutline(FOutline);
  end;
  Result := FOutline;
end;


function TGLMultiPolygonBase.GetPath(i: Integer): TGLContourNodes;
begin
  Assert(i >= 0);
  while i >= Contours.Count do
    Contours.Add;
  Result := Contours[i].Nodes;
end;


procedure TGLMultiPolygonBase.SetPath(i: Integer; const value: TGLContourNodes);
begin
  Assert(i >= 0);
  while i >= Contours.Count do
    Contours.Add;
  Contours[i].Nodes.Assign(value);
end;

//
// Tessellation routines (OpenGL callbacks)
//

var
  vVertexPool: TGLVectorPool;

procedure tessError(errno: Cardinal);
{$IFDEF MSWINDOWS} stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  Assert(False, IntToStr(errno) + ' : ' + string(gluErrorString(errno)));
end;

procedure tessIssueVertex(vertexData: Pointer);
{$IFDEF MSWINDOWS} stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  xgl.TexCoord2fv(vertexData);
  gl.Vertex3fv(vertexData);
end;

procedure tessCombine(coords: PDoubleVector; vertex_data: Pointer;
  weight: PGLFloat; var outData: Pointer);
{$IFDEF MSWINDOWS} stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  vVertexPool.GetNewVector(outData);
  SetVector(PAffineVector(outData)^, coords^[0], coords^[1], coords^[2]);
end;

procedure tessBeginList(typ: Cardinal; polygonData: Pointer);
{$IFDEF MSWINDOWS} stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  TGLPolygonList(polygonData).Add;
end;

procedure tessIssueVertexList(vertexData: Pointer; polygonData: Pointer);
{$IFDEF MSWINDOWS} stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  TGLPolygonList(polygonData).AktList.Add(PAffineVector(vertexData)^);
end;

procedure TGLMultiPolygonBase.RetrieveOutline(List: TGLPolygonList);
var
  i, n: Integer;
  tess: PGLUTesselator;

  procedure TesselatePath(contour: TGLContour; inverted: Boolean);

    procedure IssueVertex(v: TAffineVector);
    var
      dblVector: TAffineDblVector;
      p: PAffineVector;
    begin
      vVertexPool.GetNewVector(Pointer(p));
      p^ := v;
      SetVector(dblVector, v);
      gluTessVertex(tess, dblVector, p);
    end;

  var
    i, n: Integer;
    spline: TCubicSpline;
    f: Single;
    splineDivisions: Integer;
    nodes: TGLContourNodes;
  begin
    gluTessBeginContour(tess);
    nodes := contour.Nodes;
    if contour.SplineMode = lsmLines then
      splineDivisions := 0
    else
      splineDivisions := contour.Division;
    if splineDivisions > 1 then
    begin
      spline := nodes.CreateNewCubicSpline;
      try
        f := 1 / splineDivisions;
        n := splineDivisions * (nodes.Count - 1);
        if inverted then
        begin
          for i := n downto 0 do
            IssueVertex(spline.SplineAffineVector(i * f))
        end
        else
        begin
          for i := 0 to n do
            IssueVertex(spline.SplineAffineVector(i * f));
        end;
      finally
        spline.Free;
      end;
    end
    else
    begin
      n := nodes.Count - 1;
      if inverted then
      begin
        for i := n downto 0 do
          IssueVertex(nodes[i].AsAffineVector)
      end
      else
      begin
        for i := 0 to n do
          IssueVertex(nodes[i].AsAffineVector);
      end;
    end;
    gluTessEndContour(tess);
  end;

begin
  List.Clear;
  if (Contours.Count > 0) and (Path[0].Count > 2) then
  begin
    // Vertex count
    n := 0;
    for i := 0 to Contours.Count - 1 do
      n := n + Path[i].Count;
    // Creates and initialize the GLU tesselator
    vVertexPool := TGLVectorPool.Create(n, SizeOf(TAffineVector));
    tess := gluNewTess;
    try
      // register callbacks
      gluTessCallback(tess, GLU_TESS_BEGIN_DATA, @tessBeginList);
      gluTessCallback(tess, GLU_TESS_END_DATA, nil);
      gluTessCallback(tess, GLU_TESS_VERTEX_DATA, @tessIssueVertexList);
      gluTessCallback(tess, GLU_TESS_ERROR, @tessError);
      gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);

      // issue normal
      gluTessNormal(tess, FContoursNormal.X, FContoursNormal.Y, FContoursNormal.Z);

      // set properties
      gluTessProperty(Tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_POSITIVE);
      gluTessProperty(Tess, GLU_TESS_BOUNDARY_ONLY, GL_TRUE);

      gluTessBeginPolygon(tess, List);
      // outside contour
      TesselatePath(Contours[0], False);
      // inside contours
      for n := 1 to Contours.Count - 1 do
        TesselatePath(Contours[n], True);
      gluTessEndPolygon(tess);
    finally
      gluDeleteTess(tess);
      vVertexPool.Free;
      vVertexPool := nil;
    end;
  end;
end;


procedure TGLMultiPolygonBase.RenderTesselatedPolygon(textured: Boolean;
  normal: PAffineVector;
  invertNormals: Boolean);
var
  tess: PGLUTesselator;

  procedure IssueVertex(v: TAffineVector);
  var
    dblVector: TAffineDblVector;
    p: PAffineVector;
  begin
    vVertexPool.GetNewVector(Pointer(p));
    p^ := v;
    SetVector(dblVector, v);
    gluTessVertex(tess, dblVector, p);
  end;

var
  i, n: Integer;
begin
  // call to Outline will call RetrieveOutline if necessary
  if (Outline.Count = 0) or (Outline.List[0].Count < 2) then
    Exit;
  // Vertex count
  n := 0;
  for i := 0 to Outline.Count - 1 do
    n := n + Outline.List[i].Count;
  // Creates and initialize a vertex pool and the GLU tesselator
  vVertexPool := TGLVectorPool.Create(n, Sizeof(TAffineVector));
  tess := gluNewTess;
  try
    gluTessCallback(tess, GLU_TESS_BEGIN, @gl.Begin_);
    if textured then
      gluTessCallback(tess, GLU_TESS_VERTEX, @tessIssueVertex)
    else
      gluTessCallback(tess, GLU_TESS_VERTEX, @gl.Vertex3fv);
    gluTessCallback(tess, GLU_TESS_END, @gl.End_);
    gluTessCallback(tess, GLU_TESS_ERROR, @tessError);
    gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);
    // Issue normal
    if Assigned(normal) then
    begin
      gl.Normal3fv(PGLFloat(normal));
      gluTessNormal(tess, normal^.X, normal^.Y, normal^.Z);
    end;
    gluTessProperty(Tess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_POSITIVE);
    // Issue polygon
    gluTessBeginPolygon(tess, nil);
    for n := 0 to Outline.Count - 1 do
    begin
      with Outline.List[n] do
      begin
        gluTessBeginContour(tess);
        if invertNormals then
          for i := Count - 1 downto 0 do
            IssueVertex(Items[i])
        else
          for i := 0 to Count - 1 do
            IssueVertex(Items[i]);
        gluTessEndContour(tess);
      end;
    end;
    gluTessEndPolygon(tess);
  finally
    gluDeleteTess(tess);
    vVertexPool.Free;
    vVertexPool := nil;
  end;
end;

// ------------------
// ------------------ TGLMultiPolygon ------------------
// ------------------


constructor TGLMultiPolygon.Create(AOwner: TComponent);
begin
  inherited;
  FParts := [ppTop, ppBottom];
end;


procedure TGLMultiPolygon.Assign(Source: TPersistent);
begin
  if Source is TGLMultiPolygon then
  begin
    FParts := TGLMultiPolygon(Source).FParts;
  end;
  inherited;
end;


procedure TGLMultiPolygon.BuildList(var rci: TGLRenderContextInfo);
var
  normal: TAffineVector;
begin
  if (Outline.Count < 1) then
    Exit;
  normal := ContoursNormal;
  // Render
  // tessellate top polygon
  if ppTop in FParts then
    RenderTesselatedPolygon(True, @normal, False);
  // tessellate bottom polygon
  if ppBottom in FParts then
  begin
    NegateVector(normal);
    RenderTesselatedPolygon(True, @normal, True)
  end;
end;


procedure TGLMultiPolygon.SetParts(const value: TGLPolygonParts);
begin
  if FParts <> value then
  begin
    FParts := value;
    StructureChanged;
  end;
end;


procedure TGLMultiPolygonBase.SetContoursNormal(const Value: TAffineVector);
begin
  FContoursNormal := Value;
end;


function TGLMultiPolygonBase.AxisAlignedDimensionsUnscaled: TGLVector;
var
  dMin, dMax: TAffineVector;
begin
  if FAxisAlignedDimensionsCache.X < 0 then
  begin
    Contours.GetExtents(dMin, dMax);
    FAxisAlignedDimensionsCache.X := MaxFloat(Abs(dMin.X), Abs(dMax.X));
    FAxisAlignedDimensionsCache.Y := MaxFloat(Abs(dMin.Y), Abs(dMax.Y));
    FAxisAlignedDimensionsCache.Z := MaxFloat(Abs(dMin.Z), Abs(dMax.Z));
  end;
  SetVector(Result, FAxisAlignedDimensionsCache);
end;


procedure TGLMultiPolygonBase.StructureChanged;
begin
  FAxisAlignedDimensionsCache.X := -1;
  inherited;
end;

// ------------------
// ------------------ TGLContourNodes ------------------
// ------------------


procedure TGLContourNodes.NotifyChange;
begin
  if (GetOwner <> nil) then
    (GetOwner as TGLContour).Changed(False);
end;

//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

  RegisterClass(TGLMultiPolygon);

end.

