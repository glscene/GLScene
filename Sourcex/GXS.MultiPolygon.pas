//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.MultiPolygon;

(* Object with support for complex polygons *)

(* TODO

  And I reactivated the TgxVectorPool object. The GXS.VectorLists are not suitable for this job.
  When the tesselator finds an intersection of edges it wants us to give him some storage
  for this new vertex, and he wants a pointer (see tessCombine). The pointers taken from
  TgxAffineVectorList become invalid after enlarging the capacity (makes a ReAllocMem), which
  can happen implicitly while adding. The TgxVectorPool keeps all pointers valid until the
  destruction itself.

  If anyone feels responsible: it would be fine to have a method ImportFromFile (dxf?) in
  the TgxContour and TgxMultiPolygonBase objects...
*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  GXS.XOpenGL,
  GXS.Spline,
  GXS.Context,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.VectorLists,
  GXS.PersistentClasses,
  GXS.Scene,
  GXS.Objects,
  GXS.GeomObjects,
  GXS.Nodes,
  GXS.BaseClasses,
  GXS.Coordinates,
  GXS.RenderContextInfo;

type
  TgxContourNodes = class(TgxNodes)
  public
    procedure NotifyChange; override;
  end;

  TgxContour = class(TCollectionItem)
  private
    FNodes: TgxContourNodes;
    FDivision: Integer;
    FSplineMode: TgxLineSplineMode;
    FDescription: string;
    procedure SetNodes(const Value: TgxContourNodes);
    procedure SetDivision(Value: Integer);
    procedure SetSplineMode(const Value: TgxLineSplineMode);
    procedure SetDescription(const Value: string);
  protected
    procedure CreateNodes; virtual;
    procedure NodesChanged(Sender: TObject);
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Description: string read FDescription write SetDescription;
    // The nodes list.
    property Nodes: TgxContourNodes read FNodes write SetNodes;
    (* Number of divisions for each segment in spline modes.
      Minimum 1 (disabled), ignored in lsmLines mode. *)
    property Division: Integer read FDivision write SetDivision default 10;
    (* Default spline drawing mode.
      This mode is used only for the curve, not for the rotation path. *)
    property SplineMode: TgxLineSplineMode read FSplineMode write SetSplineMode default lsmLines;
  end;

  TgxContourClass = class of TgxContour;

  TgxContours = class(TgxNotifyCollection)
  private
    function GetItems(index: Integer): TgxContour;
    procedure SetItems(index: Integer; const Value: TgxContour);
  protected
  public
    constructor Create(AOwner: TComponent); overload;
    function Add: TgxContour;
    function FindItemID(ID: Integer): TgxContour;
    property Items[index: Integer]: TgxContour read GetItems write SetItems; default;
    procedure GetExtents(var min, max: TAffineVector);
  end;

  TgxPolygonList = class(TgxPersistentObjectList)
  private
    FAktList: TgxAffineVectorList;
    function GetList(I: Integer): TgxAffineVectorList;
  public
    procedure Add;
    property AktList: TgxAffineVectorList read FAktList;
    property List[I: Integer]: TgxAffineVectorList read GetList;
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
     TgxMultiPolygonBase will take the input contours and let the tesselator
     make an outline from it (this is done in RetreiveOutline). This outline is
     used for Rendering. Only when there are changes in the contours, the
     outline will be recalculated. The ouline in fact is a list of GXS.VectorLists. *)
  TgxMultiPolygonBase = class(TgxSceneObject)
  private
    FContours: TgxContours;
    FOutline: TgxPolygonList;
    FContoursNormal: TAffineVector;
    FAxisAlignedDimensionsCache: TVector4f;
    procedure SetContours(const Value: TgxContours);
    function GetPath(i: Integer): TgxContourNodes;
    procedure SetPath(i: Integer; const value: TgxContourNodes);
    function GetOutline: TgxPolygonList;
    procedure SetContoursNormal(const Value: TAffineVector);
  protected
    procedure RenderTesselatedPolygon(textured: Boolean;
      normal: PAffineVector; invertNormals: Boolean);
    procedure RetrieveOutline(List: TgxPolygonList);
    procedure ContourChanged(Sender: TObject); virtual;
    //property PNormal:PAffineVector read FPNormal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AddNode(const i: Integer; const coords: TgxCoordinates); overload;
    procedure AddNode(const i: Integer; const X, Y, Z: Single); overload;
    procedure AddNode(const i: Integer; const value: TVector4f); overload;
    procedure AddNode(const i: Integer; const value: TAffineVector); overload;
    property Path[i: Integer]: TgxContourNodes read GetPath write SetPath;
    property Outline: TgxPolygonList read GetOutline;
    property ContoursNormal: TAffineVector read FContoursNormal write SetContoursNormal;
    function AxisAlignedDimensionsUnscaled: TVector4f; override;
    procedure StructureChanged; override;
  published
    property Contours: TgxContours read FContours write SetContours;
  end;

  (* A polygon that can have holes and multiple contours.
     Use the Path property to access a contour or one of the AddNode methods
     to add a node to a contour (contours are allocated automatically). *)
  TgxMultiPolygon = class(TgxMultiPolygonBase)
  private
    FParts: TgxPolygonParts;
  protected
    procedure SetParts(const value: TgxPolygonParts);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
  published
    property Parts: TgxPolygonParts read FParts write SetParts default [ppTop, ppBottom];
  end;

//============================================================
implementation
//============================================================

type
  (* page oriented pointer array, with persistent pointer target memory.
    In TgxVectorList a pointer to a vector will not be valid any more after
    a call to SetCapacity, which might be done implicitely during Add.
    The TgxVectorPool keeps memory in its original position during its
    whole lifetime. *)

  // removed Notify (only D5)
  // added Destroy (also working with D4)
  TgxVectorPool = class(TList)
  private
    FEntrySize: Integer; // size of each entry
    FPageSize: Integer; // number of entries per page
    FArrSize: Integer; // size of one page
    FUsedEntries: Integer; // used entries in actual page
    FAktArray: PByteArray; // pointer to actual page
    procedure CreatePage; // create new page
  public
    constructor Create(APageSize, AEntrySize: Integer);
    destructor Destroy; override;

    { retrieve pointer to new entry. will create new page if needed }
    procedure GetNewVector(var P: Pointer);
  end;

//-----------------------------------------------
// TgxVectorPool
//-----------------------------------------------

constructor TgxVectorPool.Create(APageSize, AEntrySize: Integer);
begin
  inherited Create;
  Assert(APageSize > 0);
  Assert(AEntrySize > 0);
  FPageSize := APageSize;
  FEntrySize := AEntrySize;
  FArrSize := FPageSize * FEntrySize;
  CreatePage;
end;

procedure TgxVectorPool.CreatePage;
begin
  GetMem(FAktArray, FArrSize);
  Add(FAktArray);
  FUsedEntries := 0;
end;

destructor TgxVectorPool.Destroy;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    FreeMem(Items[i], FArrSize);
  inherited;
end;

procedure TgxVectorPool.GetNewVector(var P: Pointer);
begin
  if FUsedEntries >= FPageSize then
    CreatePage;
  Inc(FUsedEntries);
  P := @(FAktArray[(FUsedEntries - 1) * FEntrySize]);
end;

// ------------------
// ------------------ TgxPolygonList ------------------
// ------------------

procedure TgxPolygonList.Add;
begin
  FAktList := TgxAffineVectorList.Create;
  inherited Add(FAktList);
end;

function TgxPolygonList.GetList(i: Integer): TgxAffineVectorList;
begin
  Result := TgxAffineVectorList(Items[i]);
end;

// ------------------
// ------------------ TgxContour ------------------
// ------------------

constructor TgxContour.Create(Collection: TCollection);
begin
  inherited;
  CreateNodes;
  FDivision := 10;
  FSplineMode := lsmLines;
end;

procedure TgxContour.CreateNodes;
begin
  FNodes := TgxContourNodes.Create(Self);
end;

destructor TgxContour.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TgxContour.Assign(Source: TPersistent);
begin
  if Source is TgxContour then
  begin
    FNodes.Assign(TgxContour(Source).FNodes);
    FDivision := TgxContour(Source).FDivision;
    FSplineMode := TgxContour(Source).FSplineMode;
    FDescription := TgxContour(Source).FDescription;
  end
  else
    inherited;
end;

function TgxContour.GetDisplayName: string;
begin
  result := Description;
  if result = '' then
    result := Format('GXContour: %d nodes', [Nodes.Count]);
end;

procedure TgxContour.NodesChanged(Sender: TObject);
begin
  Changed(false);
end;

procedure TgxContour.SetDescription(const Value: string);
begin
  FDescription := Value;
end;

procedure TgxContour.SetDivision(Value: Integer);
begin
  if Value < 1 then
    Value := 1;
  if Value <> FDivision then
  begin
    FDivision := value;
    Changed(false);
  end;
end;

procedure TgxContour.SetNodes(const Value: TgxContourNodes);
begin
  FNodes.Assign(Value);
  Changed(false);
end;

procedure TgxContour.SetSplineMode(const Value: TgxLineSplineMode);
begin
  if FSplineMode <> value then
  begin
    FSplineMode := value;
    Changed(false);
  end;
end;

//--------------------------------------------
// TgxContours
//--------------------------------------------

function TgxContours.Add: TgxContour;
begin
  Result := TgxContour(inherited Add);
end;

constructor TgxContours.Create(AOwner: TComponent);
begin
  Create(AOwner, TgxContour);
end;

function TgxContours.FindItemID(ID: Integer): TgxContour;
begin
  result := TgxContour(inherited FindItemId(Id));
end;

function TgxContours.GetItems(index: Integer): TgxContour;
begin
  result := TgxContour(inherited Items[index]);
end;

procedure TgxContours.SetItems(index: Integer; const Value: TgxContour);
begin
  inherited Items[index] := value;
end;

procedure TgxContours.GetExtents(var min, max: TAffineVector);
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
      if lMin.V[k] < min.V[k] then
        min.V[k] := lMin.V[k];
      if lMax.V[k] > max.V[k] then
        max.V[k] := lMax.V[k];
    end;
  end;
end;

//--------------------------------------------
// TgxMultiPolygonBase
//--------------------------------------------

constructor TgxMultiPolygonBase.Create(AOwner: TComponent);
begin
  inherited;
  FContours := TgxContours.Create(Self);
  FContours.OnNotifyChange := ContourChanged;
  FContoursNormal := AffineVectorMake(0, 0, 1);
  FAxisAlignedDimensionsCache.X := -1;
end;

destructor TgxMultiPolygonBase.Destroy;
begin
  if FOutline <> nil then
  begin
    FOutline.Clean;
    FreeAndNil(FOutline);
  end;  
  FContours.Free;
  inherited;
end;

procedure TgxMultiPolygonBase.Assign(Source: TPersistent);
begin
  if Source is TgxMultiPolygonBase then
  begin
    FContours.Assign(TgxMultiPolygonBase(Source).FContours);
  end;
  inherited;
end;

procedure TgxMultiPolygonBase.ContourChanged(Sender: TObject);
begin
  if Assigned(FOutline) then
  begin
    // force a RetrieveOutline with next Render
    FOutline.Clean;
    FreeAndNil(FOutline);
    StructureChanged;
  end;
end;

procedure TgxMultiPolygonBase.AddNode(const i: Integer; const value: TVector4f);
begin
  Path[i].AddNode(value);
end;

procedure TgxMultiPolygonBase.AddNode(const i: Integer; const x, y, z: Single);
begin
  Path[i].AddNode(x, y, z);
end;

procedure TgxMultiPolygonBase.AddNode(const i: Integer; const coords: TgxCoordinates);
begin
  Path[i].AddNode(coords);
end;

procedure TgxMultiPolygonBase.AddNode(const I: Integer; const value: TAffineVector);
begin
  Path[i].AddNode(value);
end;

procedure TgxMultiPolygonBase.SetContours(const Value: TgxContours);
begin
  FContours.Assign(Value);
end;

function TgxMultiPolygonBase.GetOutline: TgxPolygonList;
begin
  if not Assigned(FOutline) then
  begin
    FOutline := TgxPolygonList.Create;
    RetrieveOutline(FOutline);
  end;
  Result := FOutline;
end;

function TgxMultiPolygonBase.GetPath(i: Integer): TgxContourNodes;
begin
  Assert(i >= 0);
  while i >= Contours.Count do
    Contours.Add;
  Result := Contours[i].Nodes;
end;

procedure TgxMultiPolygonBase.SetPath(i: Integer; const value: TgxContourNodes);
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
  vVertexPool: TgxVectorPool;

procedure tessError(errno: GLEnum);
{$IFDEF Win32} stdcall;{$ELSE} cdecl;{$ENDIF}
begin
  Assert(False, IntToStr(errno) + ' : ' + string(gluErrorString(errno)));
end;

procedure tessIssueVertex(vertexData: Pointer);
{$IFDEF Win32} stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  glTexCoord2fv(vertexData);
  glVertex3fv(vertexData);
end;

procedure tessCombine(coords: PDoubleVector; vertex_data: Pointer;
  weight: PGLFloat; var outData: Pointer);
{$IFDEF Win32} stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  vVertexPool.GetNewVector(outData);
  SetVector(PAffineVector(outData)^, coords^[0], coords^[1], coords^[2]);
end;

procedure tessBeginList(typ: GLEnum; polygonData: Pointer);
{$IFDEF Win32} stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  TgxPolygonList(polygonData).Add;
end;

procedure tessIssueVertexList(vertexData: Pointer; polygonData: Pointer);
{$IFDEF Win32} stdcall;{$ELSE}cdecl;{$ENDIF}
begin
  TgxPolygonList(polygonData).AktList.Add(PAffineVector(vertexData)^);
end;

procedure TgxMultiPolygonBase.RetrieveOutline(List: TgxPolygonList);
var
  i, n: Integer;
  tess: GLUTesselator;

  procedure TesselatePath(contour: TgxContour; inverted: Boolean);

    procedure IssueVertex(v: TAffineVector);
    var
      dblVector: TAffineDblVector;
      p: PAffineVector;
    begin
      vVertexPool.GetNewVector(Pointer(p));
      p^ := v;
      SetVector(dblVector, v);
      gluTessVertex(tess, @dblVector, p);
    end;

  var
    i, n: Integer;
    spline: TCubicSpline;
    f: Single;
    splineDivisions: Integer;
    nodes: TgxContourNodes;
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
    // Create and initialize the GLU tesselator
    vVertexPool := TgxVectorPool.Create(n, SizeOf(TAffineVector));
    tess := gluNewTess;
    try
      // register callbacks
      gluTessCallback(tess, GLU_TESS_BEGIN, @tessBeginList);
      gluTessCallback(tess, GLU_TESS_END, nil);
      gluTessCallback(tess, GLU_TESS_VERTEX, @tessIssueVertexList);
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

procedure TgxMultiPolygonBase.RenderTesselatedPolygon(textured: Boolean;
  normal: PAffineVector;
  invertNormals: Boolean);
var
  tess: GLUTesselator;

  procedure IssueVertex(v: TAffineVector);
  var
    dblVector: TAffineDblVector;
    p: PAffineVector;
  begin
    vVertexPool.GetNewVector(Pointer(p));
    p^ := v;
    SetVector(dblVector, v);
    gluTessVertex(tess, @dblVector, p);
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
  // Create and initialize a vertex pool and the GLU tesselator
  vVertexPool := TgxVectorPool.Create(n, Sizeof(TAffineVector));
  tess := gluNewTess;
  try
    gluTessCallback(tess, GLU_TESS_BEGIN, @glBegin);
    if textured then
      gluTessCallback(tess, GLU_TESS_VERTEX, @tessIssueVertex)
    else
      gluTessCallback(tess, GLU_TESS_VERTEX, @glVertex3fv);
    gluTessCallback(tess, GLU_TESS_END, @glEnd);
    gluTessCallback(tess, GLU_TESS_ERROR, @tessError);
    gluTessCallback(tess, GLU_TESS_COMBINE, @tessCombine);
    // Issue normal
    if Assigned(normal) then
    begin
      glNormal3fv(PGLFloat(normal));
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
// ------------------ TgxMultiPolygon ------------------
// ------------------

constructor TgxMultiPolygon.Create(AOwner: TComponent);
begin
  inherited;
  FParts := [ppTop, ppBottom];
end;

procedure TgxMultiPolygon.Assign(Source: TPersistent);
begin
  if Source is TgxMultiPolygon then
  begin
    FParts := TgxMultiPolygon(Source).FParts;
  end;
  inherited;
end;

procedure TgxMultiPolygon.BuildList(var rci: TgxRenderContextInfo);
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

procedure TgxMultiPolygon.SetParts(const value: TgxPolygonParts);
begin
  if FParts <> value then
  begin
    FParts := value;
    StructureChanged;
  end;
end;

procedure TgxMultiPolygonBase.SetContoursNormal(const Value: TAffineVector);
begin
  FContoursNormal := Value;
end;

function TgxMultiPolygonBase.AxisAlignedDimensionsUnscaled: TVector4f;
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

procedure TgxMultiPolygonBase.StructureChanged;
begin
  FAxisAlignedDimensionsCache.X := -1;
  inherited;
end;

// ------------------
// ------------------ TgxContourNodes ------------------
// ------------------

procedure TgxContourNodes.NotifyChange;
begin
  if (GetOwner <> nil) then
    (GetOwner as TgxContour).Changed(False);
end;

//-------------------------------------------------------------
initialization
//-------------------------------------------------------------

  RegisterClass(TgxMultiPolygon);

end.

