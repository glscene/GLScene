//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.Mesh;

(*
  This unit is for simple meshes and legacy support,
  GLS.VectorFileObjects implements more efficient
  (though more complex) mesh tools.
*)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,

  GLS.OpenGLTokens,
  GLS.Strings,
  GLS.XOpenGL,
  GLS.Context,
  GLS.Scene,
  GLS.VectorGeometry,
  GLS.State,
  GLS.Color,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.VectorTypes;

type
  TGLMeshMode = (mmTriangleStrip, mmTriangleFan, mmTriangles, mmQuadStrip,
    mmQuads, mmPolygon);
  TGLVertexMode = (vmV, vmVN, vmVNC, vmVNCT, vmVNT, vmVT);

const
  cMeshModeToGLEnum: array[Low(TGLMeshMode)..High(TGLMeshMode)
    ] of Cardinal = (GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN, GL_TRIANGLES,
    GL_QUAD_STRIP, GL_QUADS, GL_POLYGON);
  cVertexModeToGLEnum: array[Low(TGLVertexMode)..High(TGLVertexMode)
    ] of Cardinal = (GL_V3F, GL_N3F_V3F, GL_C4F_N3F_V3F, GL_T2F_C4F_N3F_V3F,
    GL_T2F_N3F_V3F, GL_T2F_V3F);

type

  TGLVertexData = packed record
    textCoord: TTexPoint;
    color: TGLVector;
    normal: TAffineVector;
    coord: TVertex;
  end;

  PGLVertexData = ^TGLVertexData;
  TGLVertexDataArray = array[0..(MAXINT shr 6)] of TGLVertexData;
  PGLVertexDataArray = ^TGLVertexDataArray;

  (* Stores an interlaced vertex list for direct use in OpenGL.
    Locking (hardware passthrough) is supported, see "Locked" property for details. *)
  TGLVertexList = class(TGLUpdateAbleObject)
  private
    FValues: PGLVertexDataArray;
    FCount: Integer;
    FCapacity, FGrowth: Integer;
    FLockedOldValues: PGLVertexDataArray;
  protected
    procedure SetCapacity(const val: Integer);
    procedure SetGrowth(const val: Integer);
    procedure Grow;
    procedure SetVertices(index: Integer; const val: TGLVertexData);
    function GetVertices(index: Integer): TGLVertexData;
    procedure SetVertexCoord(index: Integer; const val: TAffineVector);
    function GetVertexCoord(index: Integer): TAffineVector;
    procedure SetVertexNormal(index: Integer; const val: TAffineVector);
    function GetVertexNormal(index: Integer): TAffineVector;
    procedure SetVertexTexCoord(index: Integer; const val: TTexPoint);
    function GetVertexTexCoord(index: Integer): TTexPoint;
    procedure SetVertexColor(index: Integer; const val: TVector4f);
    function GetVertexColor(index: Integer): TVector4f;
    function GetFirstEntry: PGLFloat;
    function GetFirstColor: PGLFloat;
    function GetFirstNormal: PGLFloat;
    function GetFirstVertex: PGLFloat;
    function GetFirstTexPoint: PGLFloat;
    function GetLocked: Boolean;
    procedure SetLocked(val: Boolean);
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CreateInterpolatedCoords(list2: TGLVertexList; lerpFactor: Single): TGLVertexList;
    //  Adds a vertex to the list, fastest method. 
    procedure AddVertex(const vertexData: TGLVertexData); overload;
    //  Adds a vertex to the list, fastest method for adding a triangle. 
    procedure AddVertex3(const vd1, vd2, vd3: TGLVertexData); overload;
    (*  Adds a vertex to the list.
      Use the NullVector, NullHmgVector or NullTexPoint constants for
      params you don't want to set. *)
    procedure AddVertex(const aVertex: TVertex; const aNormal: TAffineVector;
      const aColor: TGLColorVector; const aTexPoint: TTexPoint); overload;
    //  Adds a vertex to the list, no texturing version.  
    procedure AddVertex(const vertex: TVertex; const normal: TAffineVector;
      const color: TGLColorVector); overload;
    //  Adds a vertex to the list, no texturing, not color version.  
    procedure AddVertex(const vertex: TVertex; const normal: TAffineVector); overload;
    //  Duplicates the vertex of given index and adds it at the end of the list. 
    procedure DuplicateVertex(index: Integer);
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    property Vertices[index: Integer]: TGLVertexData read GetVertices write SetVertices; default;
    property VertexCoord[index: Integer]: TAffineVector read GetVertexCoord write SetVertexCoord;
    property VertexNormal[index: Integer]: TAffineVector read GetVertexNormal write SetVertexNormal;
    property VertexTexCoord[index: Integer]: TTexPoint read GetVertexTexCoord write SetVertexTexCoord;
    property VertexColor[index: Integer]: TVector4f read GetVertexColor write SetVertexColor;
    property Count: Integer read FCount;
    (*  Capacity of the list (nb of vertex).
      Use this to allocate memory quickly before calling AddVertex. *)
    property Capacity: Integer read FCapacity write SetCapacity;
    (*  Vertex capacity that will be added each time the list needs to grow.
      default value is 256 (chunks of approx 13 kb). *)
    property Growth: Integer read FGrowth write SetGrowth;
    //  Calculates the sum of all vertex coords 
    function SumVertexCoords: TAffineVector;
    //  Calculates the extents of the vertice coords. 
    procedure GetExtents(var min, max: TAffineVector);
    //  Normalizes all normals. 
    procedure NormalizeNormals;
    //  Translate all coords by given vector 
    procedure Translate(const v: TAffineVector);
    procedure DefineOpenGLArrays;
    property FirstColor: PGLFloat read GetFirstColor;
    property FirstEntry: PGLFloat read GetFirstEntry;
    property FirstNormal: PGLFloat read GetFirstNormal;
    property FirstVertex: PGLFloat read GetFirstVertex;
    property FirstTexPoint: PGLFloat read GetFirstTexPoint;
    (*  Locking state of the vertex list.
      You can "Lock" a list to increase rendering performance on some
      OpenGL implementations (NVidia's). A Locked list size shouldn't be
      changed and calculations should be avoided.
      Performance can only be gained from a lock for osDirectDraw object,
      ie. meshes that are updated for each frame (the default build list
      mode is faster on static meshes).
      Be aware that the "Locked" state enforcement is not very strict
      to avoid performance hits, and GLScene may not always notify you
      that you're doing things you shouldn't on a locked list! *)
    property Locked: Boolean read GetLocked write SetLocked;
    procedure EnterLockSection;
    procedure LeaveLockSection;
  end;

  (* Basic mesh object.
    Each mesh holds a set of vertices and a Mode value defines how they make
    up the mesh (triangles, strips...) *)
  TGLMesh = class(TGLSceneObject)
  private
    FVertices: TGLVertexList;
    FMode: TGLMeshMode;
    FVertexMode: TGLVertexMode;
    FAxisAlignedDimensionsCache: TGLVector;
  protected
    procedure SetMode(AValue: TGLMeshMode);
    procedure SetVertices(AValue: TGLVertexList);
    procedure SetVertexMode(AValue: TGLVertexMode);
    procedure VerticesChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure CalcNormals(Frontface: TGLFaceWinding);
    property Vertices: TGLVertexList read FVertices write SetVertices;
    function AxisAlignedDimensionsUnscaled: TGLVector; override;
    procedure StructureChanged; override;
    function Length: Single;
    function Area: Single;
    function Volume: Single;
  published
    property Mode: TGLMeshMode read FMode write SetMode;
    property VertexMode: TGLVertexMode read FVertexMode write SetVertexMode
      default vmVNCT;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ----------------- TGLVertexList ------------------------------------------------

constructor TGLVertexList.Create(AOwner: TPersistent);
begin
  inherited;
  FValues := nil;
  FCount := 0;
  FCapacity := 0;
  FGrowth := 256;
end;

destructor TGLVertexList.Destroy;
begin
  Locked := False;
  FreeMem(FValues);
  inherited;
end;

function TGLVertexList.CreateInterpolatedCoords(list2: TGLVertexList;
  lerpFactor: Single): TGLVertexList;
var
  i: Integer;
begin
  Assert(Count = list2.Count);
  Result := TGLVertexList.Create(nil);
  Result.Capacity := Count;
  Move(FValues[0], Result.FValues[0], Count * SizeOf(TGLVertexData));
  // interpolate vertices
  for i := 0 to Count - 1 do
    VectorLerp(FValues^[i].coord, list2.FValues^[i].coord, lerpFactor,
      Result.FValues^[i].coord);
end;

procedure TGLVertexList.SetCapacity(const val: Integer);
begin
  Assert(not Locked, 'Cannot change locked list capacity !');
  FCapacity := val;
  if FCapacity < FCount then
    FCapacity := FCount;
  ReallocMem(FValues, FCapacity * SizeOf(TGLVertexData));
end;

procedure TGLVertexList.SetGrowth(const val: Integer);
begin
  if val > 16 then
    FGrowth := val
  else
    FGrowth := 16;
end;

procedure TGLVertexList.Grow;
begin
  Assert(not Locked, 'Cannot add to a locked list !');
  FCapacity := FCapacity + FGrowth;
  ReallocMem(FValues, FCapacity * SizeOf(TGLVertexData));
end;

function TGLVertexList.GetFirstColor: PGLFloat;
begin
  Result := @(FValues^[0].color);
end;

function TGLVertexList.GetFirstEntry: PGLFloat;
begin
  Result := Pointer(FValues);
end;

function TGLVertexList.GetFirstNormal: PGLFloat;
begin
  Result := @(FValues^[0].normal);
end;

function TGLVertexList.GetFirstVertex: PGLFloat;
begin
  Result := @(FValues^[0].coord);
end;

function TGLVertexList.GetFirstTexPoint: PGLFloat;
begin
  Result := @(FValues^[0].textCoord);
end;

function TGLVertexList.GetLocked: Boolean;
begin
  Result := Assigned(FLockedOldValues);
end;

procedure TGLVertexList.SetLocked(val: Boolean);
var
  size: Integer;
begin

  if val <> Locked then
  begin
    // Only supported with NVidia's right now
    if GL.NV_vertex_array_range and (CurrentGLContext <> nil) then
    begin
      size := FCount * SizeOf(TGLVertexData);
      if val then
      begin
        // Lock
        FLockedOldValues := FValues;
{$IFDEF MSWINDOWS}
        FValues := gl.WAllocateMemoryNV(size, 0, 0, 0.5);
{$ENDIF}
{$IFDEF LINUX}
        FValues := gl.XAllocateMemoryNV(size, 0, 0, 0.5);
{$ENDIF}
        if FValues = nil then
        begin
          FValues := FLockedOldValues;
          FLockedOldValues := nil;
        end
        else
          Move(FLockedOldValues^, FValues^, size);
      end
      else
      begin
        // Unlock
{$IFDEF MSWINDOWS}
        gl.WFreeMemoryNV(FValues);
{$ENDIF}
{$IFDEF LINUX}
        gl.XFreeMemoryNV(FValues);
{$ENDIF}
        FValues := FLockedOldValues;
        FLockedOldValues := nil;
      end;
    end;
  end;
end;

procedure TGLVertexList.EnterLockSection;
begin
  if Locked then
  begin
    gl.VertexArrayRangeNV(FCount * SizeOf(TGLVertexData), FValues);
    gl.EnableClientState(GL_VERTEX_ARRAY_RANGE_NV);
  end;
end;

procedure TGLVertexList.LeaveLockSection;
begin
  if Locked then
  begin
    gl.DisableClientState(GL_VERTEX_ARRAY_RANGE_NV);
    gl.FlushVertexArrayRangeNV;
  end;
end;

procedure TGLVertexList.SetVertices(index: Integer; const val: TGLVertexData);
begin
  Assert(Cardinal(index) < Cardinal(Count));
  FValues^[index] := val;
  NotifyChange(Self);
end;

function TGLVertexList.GetVertices(index: Integer): TGLVertexData;
begin
  Assert(Cardinal(index) < Cardinal(Count));
  Result := FValues^[index];
end;

procedure TGLVertexList.SetVertexCoord(index: Integer; const val: TAffineVector);
begin
  FValues^[index].coord := val;
  NotifyChange(Self);
end;

function TGLVertexList.GetVertexCoord(index: Integer): TAffineVector;
begin
  Result := FValues^[index].coord;
end;

procedure TGLVertexList.SetVertexNormal(index: Integer; const val: TAffineVector);
begin
  FValues^[index].normal := val;
  NotifyChange(Self);
end;

function TGLVertexList.GetVertexNormal(index: Integer): TAffineVector;
begin
  Result := FValues^[index].normal;
end;

procedure TGLVertexList.SetVertexTexCoord(index: Integer; const val: TTexPoint);
begin
  FValues^[index].textCoord := val;
  NotifyChange(Self);
end;

function TGLVertexList.GetVertexTexCoord(index: Integer): TTexPoint;
begin
  Result := FValues^[index].textCoord;
end;

procedure TGLVertexList.SetVertexColor(index: Integer; const val: TVector4f);
begin
  FValues^[index].color := val;
  NotifyChange(Self);
end;

function TGLVertexList.GetVertexColor(index: Integer): TVector4f;
begin
  Result := FValues^[index].color;
end;

procedure TGLVertexList.AddVertex(const vertexData: TGLVertexData);
begin
  if FCount = FCapacity then
    Grow;
  FValues^[FCount] := vertexData;
  Inc(FCount);
  NotifyChange(Self);
end;

procedure TGLVertexList.AddVertex3(const vd1, vd2, vd3: TGLVertexData);
begin
  // extend memory space
  if FCount + 2 >= FCapacity then
    Grow;
  // calculate destination address for new vertex data
  FValues^[FCount] := vd1;
  FValues^[FCount + 1] := vd2;
  FValues^[FCount + 2] := vd3;
  Inc(FCount, 3);
  NotifyChange(Self);
end;

procedure TGLVertexList.AddVertex(const aVertex: TVertex;
  const aNormal: TAffineVector; const aColor: TGLColorVector;
  const aTexPoint: TTexPoint);
begin
  if FCount = FCapacity then
    Grow;
  // calculate destination address for new vertex data
  with FValues^[FCount] do
  begin
    textCoord := aTexPoint;
    color := aColor;
    normal := aNormal;
    coord := aVertex;
  end;
  Inc(FCount);
  NotifyChange(Self);
end;

procedure TGLVertexList.AddVertex(const vertex: TVertex;
  const normal: TAffineVector; const color: TGLColorVector);
begin
  AddVertex(vertex, normal, color, NullTexPoint);
end;

procedure TGLVertexList.AddVertex(const vertex: TVertex;
  const normal: TAffineVector);
begin
  AddVertex(vertex, normal, clrBlack, NullTexPoint);
end;

procedure TGLVertexList.DuplicateVertex(index: Integer);
begin
  Assert(Cardinal(index) < Cardinal(Count));
  if FCount = FCapacity then
    Grow;
  FValues[FCount] := FValues[index];
  Inc(FCount);
  NotifyChange(Self);
end;

procedure TGLVertexList.Clear;
begin
  Assert(not Locked, 'Cannot clear a locked list !');
  FreeMem(FValues);
  FCount := 0;
  FCapacity := 0;
  FValues := nil;
  NotifyChange(Self);
end;

function TGLVertexList.SumVertexCoords: TAffineVector;
var
  i: Integer;
begin
  Result := NullVector;
  for i := 0 to Count - 1 do
    AddVector(Result, FValues^[i].coord);
end;

procedure TGLVertexList.GetExtents(var min, max: TAffineVector);
var
  i, k: Integer;
  f: Single;
const
  cBigValue: Single = 1E50;
  cSmallValue: Single = -1E50;
begin
  SetVector(min, cBigValue, cBigValue, cBigValue);
  SetVector(max, cSmallValue, cSmallValue, cSmallValue);
  for i := 0 to Count - 1 do
  begin
    with FValues^[i] do
      for k := 0 to 2 do
      begin
        f := coord.V[k];
        if f < min.V[k] then
          min.V[k] := f;
        if f > max.V[k] then
          max.V[k] := f;
      end;
  end;
end;

procedure TGLVertexList.NormalizeNormals;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    NormalizeVector(FValues^[i].coord);
end;

procedure TGLVertexList.Translate(const v: TAffineVector);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    AddVector(FValues^[i].coord, v);
end;

procedure TGLVertexList.DefineOpenGLArrays;
begin
  gl.EnableClientState(GL_VERTEX_ARRAY);
  gl.VertexPointer(3, GL_FLOAT, SizeOf(TGLVertexData) - SizeOf(TAffineVector),
    FirstVertex);
  gl.EnableClientState(GL_NORMAL_ARRAY);
  gl.NormalPointer(GL_FLOAT, SizeOf(TGLVertexData) - SizeOf(TAffineVector),
    FirstNormal);
  xgl.EnableClientState(GL_TEXTURE_COORD_ARRAY);
  xgl.TexCoordPointer(2, GL_FLOAT, SizeOf(TGLVertexData) - SizeOf(TTexPoint),
    FirstTexPoint);
end;

procedure TGLVertexList.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLVertexList) then
  begin
    FCount := TGLVertexList(Source).FCount;
    FCapacity := FCount;
    ReallocMem(FValues, FCount * SizeOf(TGLVertexData));
    Move(TGLVertexList(Source).FValues^, FValues^, FCount * SizeOf(TGLVertexData));
  end
  else
    inherited Assign(Source);
end;

// ----------------- TGLMesh ------------------------------------------------------

constructor TGLMesh.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // ObjectStyle:=ObjectStyle+[osDirectDraw];
  FVertices := TGLVertexList.Create(Self);
  FVertices.AddVertex(XVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.AddVertex(YVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.AddVertex(ZVector, ZVector, NullHmgVector, NullTexPoint);
  FVertices.OnNotifyChange := VerticesChanged;
  FAxisAlignedDimensionsCache.X := -1;
  FVertexMode := vmVNCT;
  // should change this later to default to vmVN. But need to
end; // change GLMeshPropform so that it greys out unused vertex info

destructor TGLMesh.Destroy;
begin
  FVertices.Free;
  inherited Destroy;
end;

procedure TGLMesh.VerticesChanged(Sender: TObject);
begin
  StructureChanged;
end;

function TGLMesh.Length: Single;
begin
  Result := 0;
end;

function TGLMesh.Area: Single;
begin
  Result := 0;
end;

function TGLMesh.Volume: Single;
begin
  Result := 0;
end;

procedure TGLMesh.BuildList(var rci: TGLRenderContextInfo);
var
  VertexCount: Longint;
begin
  inherited;
  if osDirectDraw in ObjectStyle then
    FVertices.EnterLockSection;
  case FVertexMode of
    vmV:
      gl.InterleavedArrays(GL_V3F, SizeOf(TGLVertexData), FVertices.FirstVertex);
    vmVN:
      gl.InterleavedArrays(GL_N3F_V3F, SizeOf(TGLVertexData),
        FVertices.FirstNormal);
    vmVNC:
      gl.InterleavedArrays(GL_C4F_N3F_V3F, SizeOf(TGLVertexData),
        FVertices.FirstColor);
    vmVNT, vmVNCT:
      gl.InterleavedArrays(GL_T2F_C4F_N3F_V3F, 0, FVertices.FirstEntry);
    vmVT:
      gl.InterleavedArrays(GL_T2F_V3F, 0, FVertices.FirstEntry);
  else
    Assert(False, strInterleaveNotSupported);
  end;
  if FVertexMode in [vmVNC, vmVNCT] then
  begin
    rci.GLStates.Enable(stColorMaterial);
    gl.ColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
    rci.GLStates.SetGLMaterialColors(cmFront, clrBlack, clrGray20, clrGray80,
      clrBlack, 0);
    rci.GLStates.SetGLMaterialColors(cmBack, clrBlack, clrGray20, clrGray80,
      clrBlack, 0);
  end;
  VertexCount := FVertices.Count;
  case FMode of
    mmTriangleStrip:
      gl.DrawArrays(GL_TRIANGLE_STRIP, 0, VertexCount);
    mmTriangleFan:
      gl.DrawArrays(GL_TRIANGLE_FAN, 0, VertexCount);
    mmTriangles:
      gl.DrawArrays(GL_TRIANGLES, 0, VertexCount);
    mmQuadStrip:
      gl.DrawArrays(GL_QUAD_STRIP, 0, VertexCount);
    mmQuads:
      gl.DrawArrays(GL_QUADS, 0, VertexCount);
    mmPolygon:
      gl.DrawArrays(GL_POLYGON, 0, VertexCount);
  else
    Assert(False);
  end;
  if osDirectDraw in ObjectStyle then
    FVertices.LeaveLockSection;
end;

procedure TGLMesh.SetMode(AValue: TGLMeshMode);
begin
  if AValue <> FMode then
  begin
    FMode := AValue;
    StructureChanged;
  end;
end;

procedure TGLMesh.SetVertices(AValue: TGLVertexList);
begin
  if AValue <> FVertices then
  begin
    FVertices.Assign(AValue);
    StructureChanged;
  end;
end;

procedure TGLMesh.SetVertexMode(AValue: TGLVertexMode);
begin
  if AValue <> FVertexMode then
  begin
    FVertexMode := AValue;
    StructureChanged;
  end;
end;

procedure TGLMesh.CalcNormals(Frontface: TGLFaceWinding);
var
  vn: TAffineFltVector;
  i, j: Integer;
begin
  case FMode of
    mmTriangleStrip:
      with Vertices do
        for i := 0 to Count - 3 do
        begin
          if (Frontface = fwCounterClockWise) xor ((i and 1) = 0) then
            vn := CalcPlaneNormal(FValues^[i + 0].coord, FValues^[i + 1].coord,
              FValues^[i + 2].coord)
          else
            vn := CalcPlaneNormal(FValues^[i + 2].coord, FValues^[i + 1].coord,
              FValues^[i + 0].coord);
          FValues^[i].normal := vn;
        end;
    mmTriangles:
      with Vertices do
        for i := 0 to ((Count - 3) div 3) do
        begin
          j := i * 3;
          if Frontface = fwCounterClockWise then
            vn := CalcPlaneNormal(FValues^[j + 0].coord, FValues^[j + 1].coord,
              FValues^[j + 2].coord)
          else
            vn := CalcPlaneNormal(FValues^[j + 2].coord, FValues^[j + 1].coord,
              FValues^[j + 0].coord);
          FValues^[j + 0].normal := vn;
          FValues^[j + 1].normal := vn;
          FValues^[j + 2].normal := vn;
        end;
    mmQuads:
      with Vertices do
        for i := 0 to ((Count - 4) div 4) do
        begin
          j := i * 4;
          if Frontface = fwCounterClockWise then
            vn := CalcPlaneNormal(FValues^[j + 0].coord, FValues^[j + 1].coord,
              FValues^[j + 2].coord)
          else
            vn := CalcPlaneNormal(FValues^[j + 2].coord, FValues^[j + 1].coord,
              FValues^[j + 0].coord);
          FValues^[j + 0].normal := vn;
          FValues^[j + 1].normal := vn;
          FValues^[j + 2].normal := vn;
          FValues^[j + 3].normal := vn;
        end;
    mmPolygon:
      with Vertices do
        if Count > 2 then
        begin
          if Frontface = fwCounterClockWise then
            vn := CalcPlaneNormal(FValues^[0].coord, FValues^[1].coord,
              FValues^[2].coord)
          else
            vn := CalcPlaneNormal(FValues^[2].coord, FValues^[1].coord,
              FValues^[0].coord);
          for i := 0 to Count - 1 do
            FValues^[i].normal := vn;
        end;
  else
    Assert(False);
  end;
  StructureChanged;
end;

procedure TGLMesh.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLMesh) then
  begin
    FVertices.Assign(TGLMesh(Source).Vertices);
    FMode := TGLMesh(Source).FMode;
    FVertexMode := TGLMesh(Source).FVertexMode;
  end
  else
    inherited Assign(Source);
end;

function TGLMesh.AxisAlignedDimensionsUnscaled: TGLVector;
var
  dMin, dMax: TAffineVector;
begin
  if FAxisAlignedDimensionsCache.X < 0 then
  begin
    Vertices.GetExtents(dMin, dMax);
    FAxisAlignedDimensionsCache.X := MaxFloat(Abs(dMin.X), Abs(dMax.X));
    FAxisAlignedDimensionsCache.Y := MaxFloat(Abs(dMin.Y), Abs(dMax.Y));
    FAxisAlignedDimensionsCache.Z := MaxFloat(Abs(dMin.Z), Abs(dMax.Z));
  end;
  SetVector(Result, FAxisAlignedDimensionsCache);
end;

procedure TGLMesh.StructureChanged;
begin
  FAxisAlignedDimensionsCache.X := -1;
  inherited;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TGLMesh]);

end.


