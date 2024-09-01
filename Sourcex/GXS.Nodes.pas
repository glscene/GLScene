//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Nodes;

(* Nodes are used to describe lines, polygons + more *)

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  System.Math,

  GXS.VectorTypes,
  GXS.XOpenGL,
  GXS.VectorGeometry,
  GXS.Context,
  GXS.BaseClasses,
  GXS.Coordinates,
  GXS.Spline;

{$I GXS.Scene.inc}

type
  TgxNode = class(TCollectionItem)
  private
    FCoords: TVector4f;
    FTagObject: TObject;
    procedure SetAsVector(const Value: TVector4f);
    procedure SetAsAffineVector(const Value: TAffineVector);
    function GetAsAffineVector: TAffineVector;
    procedure SetCoordinate(AIndex: Integer; AValue: Single);
    function GetCoordinate(const Index: Integer): Single;
  protected
    function StoreCoordinate(AIndex: Integer): Boolean;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AsAddress: PGLFloat;
    (* The coordinates viewed as a vector.
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead. *)
    property AsVector: TVector4f read FCoords write SetAsVector;
    (* The coordinates viewed as an affine vector.
      Assigning a value to this property will trigger notification events,
      if you don't want so, use DirectVector instead.
      The W component is automatically adjustes depending on style. *)
    property AsAffineVector: TAffineVector read GetAsAffineVector write SetAsAffineVector;
    property W: Single index 3 read GetCoordinate write SetCoordinate stored StoreCoordinate;
    property TagObject: TObject read FTagObject write FTagObject;
  published
    property X: Single index 0 read GetCoordinate write SetCoordinate stored StoreCoordinate;
    property Y: Single index 1 read GetCoordinate write SetCoordinate stored StoreCoordinate;
    property Z: Single index 2 read GetCoordinate write SetCoordinate stored StoreCoordinate;
  end;

  TgxNodes = class(TOwnedCollection)
  protected
    procedure SetItems(Index: Integer; const Val: TgxNode);
    function GetItems(Index: Integer): TgxNode;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; AItemClass: TCollectionItemClass = nil);
    function CreateCopy(AOwner: TPersistent): TgxNodes;
    function Add: TgxNode;
    function FindItemID(ID: Integer): TgxNode;
    property Items[index: Integer]: TgxNode read GetItems write SetItems; default;
    function First: TgxNode;
    function Last: TgxNode;
    procedure NotifyChange; virtual;
    procedure EndUpdate; override;
    procedure AddNode(const Coords: TgxCustomCoordinates); overload;
    procedure AddNode(const X, Y, Z: Single); overload;
    procedure AddNode(const Value: TVector4f); overload;
    procedure AddNode(const Value: TAffineVector); overload;
    procedure AddXYArc(XRadius, YRadius: Single; StartAngle, StopAngle: Single; NbSegments: Integer;
      const Center: TAffineVector);
    // Calculates and returns the barycenter of the nodes
    function Barycenter: TAffineVector;
    (* Computes normal based on the 1st three nodes.
      Returns NullVector if there are less than 3 nodes. *)
    function Normal: TAffineVector;
    // Returns normalized vector Nodes[i+1]-Nodes[i]
    function Vector(I: Integer): TAffineVector;
    (* Calculates the extents of the nodes (min-max for all coordinates).
      The returned values are also the two corners of the axis-aligned
      bounding box. *)
    procedure GetExtents(var Min, Max: TAffineVector);
    // Translate all nodes
    procedure Translate(const Tv: TAffineVector);
    // Scale all node coordinates
    procedure Scale(const Fv: TAffineVector); overload;
    // Scale all node coordinates
    procedure Scale(F: Single); overload;
    // Rotate nodes around Y axis by the given angle (degrees)
    procedure RotateAroundX(Angle: Single);
    // Rotate nodes around Y axis by the given angle (degrees)
    procedure RotateAroundY(Angle: Single);
    // Rotate nodes around Y axis by the given angle (degrees)
    procedure RotateAroundZ(Angle: Single);
    procedure RenderTesselatedPolygon(ATextured: Boolean; ANormal: PAffineVector = nil; ASplineDivisions: Integer = 1;
      AInvertNormals: Boolean = False);
    function CreateNewCubicSpline: TCubicSpline;
  end;

  TgxNodesClass = class of TgxNodes;

//-----------------------------------------------------
implementation
//-----------------------------------------------------

// ------------------
// ------------------ TgxNode ------------------
// ------------------

constructor TgxNode.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  // nothing, yet
end;

destructor TgxNode.Destroy;
begin
  // nothing, yet
  inherited Destroy;
end;

procedure TgxNode.Assign(Source: TPersistent);
begin
  if Source is TgxNode then
  begin
    FCoords := TgxNode(Source).FCoords;
  end
  else
    inherited;
end;

function TgxNode.GetDisplayName: string;
begin
  Result := Format('%.4f; %.4f; %.4f', [X, Y, Z]);
end;

function TgxNode.AsAddress: PGLFloat;
begin
  Result := @FCoords;
end;

procedure TgxNode.SetAsVector(const Value: TVector4f);
begin
  FCoords := Value;
  (Collection as TgxNodes).NotifyChange;
end;

procedure TgxNode.SetAsAffineVector(const Value: TAffineVector);
begin
  SetVector(FCoords, Value);
  (Collection as TgxNodes).NotifyChange;
end;

function TgxNode.GetAsAffineVector: TAffineVector;
begin
  SetVector(Result, FCoords);
end;

function TgxNode.GetCoordinate(const Index: Integer): Single;
begin
  Result := FCoords.V[Index];
end;


procedure TgxNode.SetCoordinate(AIndex: Integer; AValue: Single);
begin
  FCoords.V[AIndex] := AValue;
  (Collection as TgxNodes).NotifyChange;
end;


function TgxNode.StoreCoordinate(AIndex: Integer): Boolean;
begin
  Result := (FCoords.V[AIndex] <> 0);
end;

// ------------------
// ------------------ TgxNodes ------------------
// ------------------

constructor TgxNodes.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass = nil);
begin
  if not Assigned(AItemClass) then
    inherited Create(AOwner, TgxNode)
  else
    inherited Create(AOwner, AItemClass);
end;

function TgxNodes.CreateCopy(AOwner: TPersistent): TgxNodes;
begin
  if Self <> nil then
  begin
    Result := TgxNodesClass(Self.ClassType).Create(AOwner);
    Result.Assign(Self);
  end
  else
    Result := nil;
end;

procedure TgxNodes.SetItems(Index: Integer; const Val: TgxNode);
begin
  inherited Items[index] := Val;
end;

function TgxNodes.GetItems(Index: Integer): TgxNode;
begin
  Result := TgxNode(inherited Items[index]);
end;

function TgxNodes.First: TgxNode;
begin
  if Count > 0 then
    Result := TgxNode(inherited Items[0])
  else
    Result := nil;
end;

function TgxNodes.Last: TgxNode;
var
  N: Integer;
begin
  N := Count - 1;
  if N >= 0 then
    Result := TgxNode(inherited Items[N])
  else
    Result := nil;
end;

procedure TgxNodes.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

function TgxNodes.Add: TgxNode;
begin
  Result := (inherited Add) as TgxNode;
end;

function TgxNodes.FindItemID(ID: Integer): TgxNode;
begin
  Result := (inherited FindItemID(ID)) as TgxNode;
end;

procedure TgxNodes.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and (GetOwner is TgxUpdateAbleComponent) then
    TgxUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

procedure TgxNodes.EndUpdate;
begin
  inherited EndUpdate;
  // Workaround for a bug in VCL's EndUpdate
  if UpdateCount = 0 then
    NotifyChange;
end;

procedure TgxNodes.AddNode(const Coords: TgxCustomCoordinates);
begin
  Add.AsVector := Coords.AsVector;
end;

procedure TgxNodes.AddNode(const X, Y, Z: Single);
begin
  Add.AsVector := PointMake(X, Y, Z);
end;

procedure TgxNodes.AddNode(const Value: TVector4f);
begin
  Add.AsVector := Value;
end;

procedure TgxNodes.AddNode(const Value: TAffineVector);
begin
  Add.AsAffineVector := Value;
end;

procedure TgxNodes.AddXYArc(XRadius, YRadius: Single; StartAngle, StopAngle: Single; NbSegments: Integer;
  const Center: TAffineVector);
var
  I: Integer;
  F: Single;
  S, C: Single;
begin
  BeginUpdate;
  try
    StartAngle := DegToRadian(StartAngle);
    StopAngle := DegToRadian(StopAngle);
    F := (StopAngle - StartAngle) / NbSegments;
    for I := 0 to NbSegments do
    begin
      SinCosine(I * F + StartAngle, S, C);
      SetVector(Add.FCoords, Center.X + XRadius * C, Center.Y + YRadius * S, Center.Z, 1);
    end;
  finally
    EndUpdate;
  end;
end;

function TgxNodes.Barycenter: TAffineVector;
var
  I: Integer;
begin
  Result := NullVector;
  if Count > 0 then
  begin
    for I := 0 to Count - 1 do
      AddVector(Result, PAffineVector(Items[I].AsAddress)^);
    ScaleVector(Result, 1.0 / Count);
  end;
end;

function TgxNodes.Normal: TAffineVector;
begin
  if Count >= 3 then
    CalcPlaneNormal(Items[0].FCoords, Items[1].FCoords, Items[2].FCoords, Result)
  else
    Result := NullVector;
end;

function TgxNodes.Vector(I: Integer): TAffineVector;

procedure CalcUsingPrev; forward;

  procedure CalcUsingNext;
  begin
    if I < Count - 1 then
      VectorSubtract(Items[I].AsVector, Items[I + 1].AsVector, Result)
    else
      CalcUsingPrev;
  end;

  procedure CalcUsingPrev;
  begin
    if I > 0 then
      VectorSubtract(Items[I - 1].AsVector, Items[I].AsVector, Result)
    else
      CalcUsingNext;
  end;

var
  J: Integer;
  Vecnull: Boolean;
begin
  Assert((I >= 0) and (I < Count));
  if I = 0 then
    if I = Count - 1 then
      SetVector(Result, NullVector)
    else
      VectorSubtract(Items[I + 1].AsVector, Items[I].AsVector, Result)
  else if I = Count - 1 then
    VectorSubtract(Items[I].AsVector, Items[I - 1].AsVector, Result)
  else
    VectorSubtract(Items[I + 1].AsVector, Items[I - 1].AsVector, Result);
  if VectorNorm(Result) < 1E-5 then
  begin
    // avoid returning null vector which generates display bugs in geometry
    J := 1;
    Vecnull := True;
    while (I + J < Count) and (Vecnull) do
    begin
      VectorSubtract(Items[I + J].AsVector, Items[I].AsVector, Result);
      if (VectorNorm(Result) > 1E-5) then
        Vecnull := False
      else
        Inc(J);
    end;
    J := 1;
    while (I - J > 0) and (Vecnull) do
    begin
      VectorSubtract(Items[I].AsVector, Items[I - J].AsVector, Result);
      if (VectorNorm(Result) > 1E-5) then
        Vecnull := False
      else
        Inc(J);
    end;
    if Vecnull then
      SetVector(Result, NullVector)
    else
      NormalizeVector(Result);
  end
  else
    NormalizeVector(Result);
end;

procedure TgxNodes.GetExtents(var Min, Max: TAffineVector);
var
  I, K: Integer;
  F: Single;
const
  CBigValue: Single = 1E50;
  CSmallValue: Single = -1E50;
begin
  SetVector(Min, CBigValue, CBigValue, CBigValue);
  SetVector(Max, CSmallValue, CSmallValue, CSmallValue);
  for I := 0 to Count - 1 do
  begin
    for K := 0 to 2 do
    begin
      F := PAffineVector(Items[I].AsAddress)^.V[K];
      if F < Min.V[K] then
        Min.V[K] := F;
      if F > Max.V[K] then
        Max.V[K] := F;
    end;
  end;
end;

procedure TgxNodes.Translate(const Tv: TAffineVector);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AddVector(PAffineVector(Items[I].AsAddress)^, Tv);
  NotifyChange;
end;

procedure TgxNodes.Scale(const Fv: TAffineVector);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ScaleVector(PAffineVector(Items[I].AsAddress)^, Fv);
  NotifyChange;
end;

procedure TgxNodes.Scale(F: Single);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    ScaleVector(PAffineVector(Items[I].AsAddress)^, F);
  NotifyChange;
end;

procedure TgxNodes.RotateAroundX(Angle: Single);
var
  I: Integer;
  C, S, V2: Single;
  V: PAffineVector;
begin
  SinCosine(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V2 := V^.Z;
    V^.Y := C * V^.Y + S * V2;
    V^.Z := C * V2 - S * V^.Y;
  end;
  NotifyChange;
end;

procedure TgxNodes.RotateAroundY(Angle: Single);
var
  I: Integer;
  C, S, V0: Single;
  V: PAffineVector;
begin
  SinCosine(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V0 := V^.X;
    V^.X := C * V0 + S * V^.Z;
    V^.Z := C * V^.Z - S * V0;
  end;
  NotifyChange;
end;

procedure TgxNodes.RotateAroundZ(Angle: Single);
var
  I: Integer;
  C, S, V1: Single;
  V: PAffineVector;
begin
  SinCosine(CPIDiv180 * Angle, S, C);
  for I := 0 to Count - 1 do
  begin
    V := PAffineVector(Items[I].AsAddress);
    V1 := V^.Y;
    V^.Y := C * V1 + S * V^.X;
    V^.X := C * V^.X - S * V1;
  end;
  NotifyChange;
end;

function TgxNodes.CreateNewCubicSpline: TCubicSpline;
var
  I: Integer;
  Xa, Ya, Za: PFloatArray;
begin
  GetMem(Xa, SizeOf(Single) * Count);
  GetMem(Ya, SizeOf(Single) * Count);
  GetMem(Za, SizeOf(Single) * Count);
  for I := 0 to Count - 1 do
    with Items[I] do
    begin
      Xa^[I] := X;
      Ya^[I] := Y;
      Za^[I] := Z;
    end;
  Result := TCubicSpline.Create(Xa, Ya, Za, nil, Count);
  FreeMem(Xa);
  FreeMem(Ya);
  FreeMem(Za);
end;

var
  NbExtraVertices: Integer;
  NewVertices: PAffineVectorArray;

function AllocNewVertex: PAffineVector;
begin
  Inc(NbExtraVertices);
  Result := @NewVertices[NbExtraVertices - 1];
end;

procedure TessError(Errno: GLEnum);
{$IFDEF Win32} stdcall; {$ENDIF}{$IFDEF UNIX} cdecl; {$ENDIF}
begin
  Assert(False, IntToStr(Errno) + ': ' + string(GluErrorString(Errno)));
end;

procedure TessIssueVertex(VertexData: Pointer);
{$IFDEF Win32} stdcall; {$ENDIF}{$IFDEF UNIX} cdecl; {$ENDIF}
begin
  glTexCoord2fv(VertexData);
  glVertex3fv(VertexData);
end;

procedure TessCombine(Coords: PDoubleVector; Vertex_data: Pointer; Weight: PGLFloat; var OutData: Pointer);
{$IFDEF Win32} stdcall; {$ENDIF}{$IFDEF UNIX} cdecl; {$ENDIF}
begin
  OutData := AllocNewVertex;
  SetVector(PAffineVector(OutData)^, Coords^[0], Coords^[1], Coords^[2]);
end;

procedure TgxNodes.RenderTesselatedPolygon(ATextured: Boolean; ANormal: PAffineVector = nil; ASplineDivisions: Integer = 1;
  AInvertNormals: Boolean = False);
var
  I: Integer;
  Tess: GLUTesselator;
  DblVector: TAffineDblVector;
  Spline: TCubicSpline;
  SplinePos: PAffineVector;
  F: Single;

begin
  if Count > 2 then
  begin
    // Create and initialize the GLU tesselator
    Tess := gluNewTess;
    gluTessCallback(Tess, GLU_TESS_BEGIN, @glBegin);
    if ATextured then
      gluTessCallback(Tess, GLU_TESS_VERTEX, @TessIssueVertex)
    else
      gluTessCallback(Tess, GLU_TESS_VERTEX, @glVertex3fv);
    gluTessCallback(Tess, GLU_TESS_END, @glEnd);
    gluTessCallback(Tess, GLU_TESS_ERROR, @TessError);
    gluTessCallback(Tess, GLU_TESS_COMBINE, @TessCombine);
    NbExtraVertices := 0;
    // Issue normal
    if Assigned(ANormal) then
    begin
      glNormal3fv(PGLFloat(ANormal));
      gluTessNormal(Tess, ANormal^.X, ANormal^.Y, ANormal^.Z);
    end;
    // Issue polygon
    gluTessBeginPolygon(Tess, nil);
    gluTessBeginContour(Tess);
    if ASplineDivisions <= 1 then
    begin
      // no spline, use direct coordinates
      GetMem(NewVertices, Count * SizeOf(TAffineVector));
      if AInvertNormals then
      begin
        for I := Count - 1 downto 0 do
        begin
          SetVector(DblVector, PAffineVector(Items[I].AsAddress)^);
          gluTessVertex(Tess, @DblVector, Items[I].AsAddress);
        end;
      end
      else
      begin
        for I := 0 to Count - 1 do
        begin
          SetVector(DblVector, PAffineVector(Items[I].AsAddress)^);
          gluTessVertex(Tess, @DblVector, Items[I].AsAddress);
        end;
      end;
    end
    else
    begin
      // cubic spline
      GetMem(NewVertices, 2 * ASplineDivisions * Count * SizeOf(TAffineVector));
      Spline := CreateNewCubicSpline;
      F := 1.0 / ASplineDivisions;
      if AInvertNormals then
      begin
        for I := ASplineDivisions * (Count - 1) downto 0 do
        begin
          SplinePos := AllocNewVertex;
          Spline.SplineAffineVector(I * F, SplinePos^);
          SetVector(DblVector, SplinePos^);
          gluTessVertex(Tess, @DblVector, SplinePos);
        end;
      end
      else
      begin
        for I := 0 to ASplineDivisions * (Count - 1) do
        begin
          SplinePos := AllocNewVertex;
          Spline.SplineAffineVector(I * F, SplinePos^);
          SetVector(DblVector, SplinePos^);
          gluTessVertex(Tess, @DblVector, SplinePos);
        end;
      end;
      Spline.Free;
    end;
    gluTessEndContour(Tess);
    gluTessEndPolygon(Tess);
    // release stuff
    if Assigned(NewVertices) then
      FreeMem(NewVertices);
    gluDeleteTess(Tess);
  end;
end;

end.
