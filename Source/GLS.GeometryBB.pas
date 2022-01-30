//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.GeometryBB;

(* Calculations and manipulations on Bounding Boxes *)

interface

{$I GLScene.inc}

uses
  System.SysUtils,

  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.VectorTypes;

type
  //  Structure for storing Bounding Boxes 
  PHmgBoundingBox = ^THmgBoundingBox;

  THmgBoundingBox = record
    BBox: array [0 .. 7] of TGLVector;
  end;

  //  Structure for storing Axis Aligned Bounding Boxes 
  TAABB = record
    Min, Max: TAffineVector;
  end;

  PAABB = ^TAABB;

  // Structure for storing BoundingSpheres. Similar to TAABB 
  TBSphere = record
    // Center of Bounding Sphere
    Center: TAffineVector;
    // Radius of Bounding Sphere
    Radius: Single;
  end;

  TGLClipRect = record
    Left, Top: Single;
    Right, Bottom: Single;
  end;

  (* Result type for space intersection tests, like AABBContainsAABB or
    BSphereContainsAABB *)
  TSpaceContains = (ScNoOverlap, ScContainsFully, ScContainsPartially);
  // Structure for storing the corners of an AABB, used with ExtractAABBCorners 
  TAABBCorners = array [0 .. 7] of TAffineVector;

  const
   NullBoundingBox: THmgBoundingBox =
   (BBox:((X: 0; Y: 0; Z: 0; W: 1),
          (X: 0; Y: 0; Z: 0; W: 1),
          (X: 0; Y: 0; Z: 0; W: 1),
          (X: 0; Y: 0; Z: 0; W: 1),
          (X: 0; Y: 0; Z: 0; W: 1),
          (X: 0; Y: 0; Z: 0; W: 1),
          (X: 0; Y: 0; Z: 0; W: 1),
          (X: 0; Y: 0; Z: 0; W: 1)));

// ------------------------------------------------------------------------------
// Bounding Box functions
// ------------------------------------------------------------------------------

function BoundingBoxesAreEqual(const ABoundingBox1, ABoundingBox2: THmgBoundingBox): Boolean; overload;
function BoundingBoxesAreEqual(const ABoundingBox1, ABoundingBox2: PHmgBoundingBox): Boolean; overload;

(*  Adds a BB into another BB. 
  The original BB (c1) is extended if necessary to contain c2. *)
function AddBB(var C1: THmgBoundingBox; const C2: THmgBoundingBox): THmgBoundingBox;
procedure AddAABB(var Aabb: TAABB; const Aabb1: TAABB);

procedure SetBB(var C: THmgBoundingBox; const V: TGLVector);
procedure SetAABB(var Bb: TAABB; const V: TGLVector); inline;

procedure BBTransform(var C: THmgBoundingBox; const M: TGLMatrix);
procedure AABBTransform(var Bb: TAABB; const M: TGLMatrix);
procedure AABBScale(var Bb: TAABB; const V: TAffineVector);

function BBMinX(const C: THmgBoundingBox): Single;
function BBMaxX(const C: THmgBoundingBox): Single;
function BBMinY(const C: THmgBoundingBox): Single;
function BBMaxY(const C: THmgBoundingBox): Single;
function BBMinZ(const C: THmgBoundingBox): Single;
function BBMaxZ(const C: THmgBoundingBox): Single;

// Resize the AABB if necessary to include p.
procedure AABBInclude(var Bb: TAABB; const P: TAffineVector);
// Make an AABB that is formed by sweeping a sphere (or AABB) from Start to Dest 
procedure AABBFromSweep(var SweepAABB: TAABB; const Start, Dest: TGLVector; const Radius: Single);
(* Returns the intersection AABB of two AABBs. 
  If the AABBs don't intersect, will return a degenerated AABB (plane, line or point). *)
function AABBIntersection(const Aabb1, Aabb2: TAABB): TAABB;
//  Extract AABB information from a BB. 
function BBToAABB(const ABB: THmgBoundingBox): TAABB;
// Converts an AABB to its canonical BB. 
function AABBToBB(const AnAABB: TAABB): THmgBoundingBox; overload;
// Transforms an AABB to a BB. 
function AABBToBB(const AnAABB: TAABB; const M: TGLMatrix): THmgBoundingBox; overload;
//  Adds delta to min and max of the AABB. 
procedure OffsetAABB(var Aabb: TAABB; const Delta: TAffineVector); overload;
procedure OffsetAABB(var Aabb: TAABB; const Delta: TGLVector); overload;
//  Adds delta to min and max of the BB.
procedure OffsetBB(var Bb: THmgBoundingBox; const Delta: TAffineVector); overload;
procedure OffsetBB(var Bb: THmgBoundingBox; const Delta: TGLVector); overload;
// The same as above but uses AddPoint() instead of AddVector().
procedure OffsetBBPoint(var Bb: THmgBoundingBox; const Delta: TGLVector); overload;
(* Determines if two AxisAlignedBoundingBoxes intersect.
  The matrices are the ones that convert one point to the other's AABB system *)
function IntersectAABBs(const Aabb1, Aabb2: TAABB; const M1To2, M2To1: TGLMatrix): Boolean; overload;
// Checks whether two Bounding boxes aligned with the world axes collide in the XY plane.
function IntersectAABBsAbsoluteXY(const Aabb1, Aabb2: TAABB): Boolean;
// Checks whether two Bounding boxes aligned with the world axes collide in the XZ plane.
function IntersectAABBsAbsoluteXZ(const Aabb1, Aabb2: TAABB): Boolean;
// Checks whether two Bounding boxes aligned with the world axes collide.
function IntersectAABBsAbsolute(const Aabb1, Aabb2: TAABB): Boolean;
(* Checks whether one Bounding box aligned with the world axes fits within
  another Bounding box *)
function AABBFitsInAABBAbsolute(const Aabb1, Aabb2: TAABB): Boolean;

// Checks if a point "p" is inside an AABB
function PointInAABB(const P: TAffineVector; const Aabb: TAABB): Boolean; overload;
function PointInAABB(const P: TGLVector; const Aabb: TAABB): Boolean; overload;

// Checks if a plane (given by the normal+d) intersects the AABB
function PlaneIntersectAABB(const Normal: TAffineVector; D: Single; const Aabb: TAABB): Boolean;
// Compute the intersection between a plane and the AABB
function PlaneAABBIntersection(const plane : THmgPlane; const AABB : TAABB) : TGLAffineVectorList;
(*
  Original source code by Tomas Akenine-Möller
  Based on the paper "Fast 3D Triangle-Box Overlap Testing"
  http://www.cs.lth.se/home/Tomas_Akenine_Moller/pubs/tribox.pdf
  http://jgt.akpeters.com/papers/AkenineMoller01/ (code)
  Use separating axis theorem to test overlap between triangle and box
  need to test for overlap in these directions:
  1) the (x,y,z)-directions (actually, since we use the AABB of the triangle
  we do not even need to test these)
  2) normal of the triangle
  3) crossproduct(edge from tri, {x,y,z}-directin)
  this gives 3x3=9 more tests
  Checks if a triangle (given by vertices v1, v2 and v3) intersects an AABB
*)
function TriangleIntersectAABB(const Aabb: TAABB; const V1, V2, V3: TAffineVector): Boolean;

// Extract the corners from an AABB 
procedure ExtractAABBCorners(const AABB: TAABB; var AABBCorners: TAABBCorners);

// Convert an AABB to a BSphere 
procedure AABBToBSphere(const AABB: TAABB; var BSphere: TBSphere);
// Convert a BSphere to an AABB 
procedure BSphereToAABB(const BSphere: TBSphere; var AABB: TAABB); overload;
function BSphereToAABB(const Center: TAffineVector; Radius: Single): TAABB; overload;
function BSphereToAABB(const Center: TGLVector; Radius: Single): TAABB; overload;

// Determines to which extent one AABB contains another AABB 
function AABBContainsAABB(const MainAABB, TestAABB: TAABB): TSpaceContains;
// Determines to which extent a BSphere contains an AABB 
function BSphereContainsAABB(const MainBSphere: TBSphere; const TestAABB: TAABB): TSpaceContains;
// Determines to which extent one BSphere contains another BSphere 
function BSphereContainsBSphere(const MainBSphere, TestBSphere: TBSphere): TSpaceContains;
// Determines to which extent an AABB contains a BSpher 
function AABBContainsBSphere(const MainAABB: TAABB; const TestBSphere: TBSphere): TSpaceContains;
// Determines to which extent a plane contains a BSphere 
function PlaneContainsBSphere(const Location, Normal: TAffineVector;
  const TestBSphere: TBSphere): TSpaceContains;
// Determines to which extent a frustum contains a BSphere 
function FrustumContainsBSphere(const Frustum: TFrustum;
  const TestBSphere: TBSphere): TSpaceContains;
// Determines to which extent a frustum contains an AABB 
function FrustumContainsAABB(const Frustum: TFrustum; const TestAABB: TAABB): TSpaceContains;
// Clips a position to an AABB 
function ClipToAABB(const V: TAffineVector; const AABB: TAABB): TAffineVector;
// Determines if one BSphere intersects another BSphere 
function BSphereIntersectsBSphere(const MainBSphere, TestBSphere: TBSphere): Boolean;

// Extend the clip rect to include given coordinate. 
procedure IncludeInClipRect(var ClipRect: TGLClipRect; X, Y: Single);
// Projects an AABB and determines the extent of its projection as a clip rect.
function AABBToClipRect(const Aabb: TAABB; const ModelViewProjection: TGLMatrix;
  ViewportSizeX, ViewportSizeY: Integer): TGLClipRect;

// Finds the intersection between a ray and an axis aligned bounding box. 
function RayCastAABBIntersect(const RayOrigin, RayDirection: TGLVector;
  const Aabb: TAABB; out TNear, TFar: Single): Boolean; overload;
function RayCastAABBIntersect(const RayOrigin, RayDirection: TGLVector;
  const Aabb: TAABB; IntersectPoint: PGLVector = nil): Boolean; overload;

type
  TPlanIndices = array [0 .. 3] of Integer;
  TPlanBB = array [0 .. 5] of TPlanIndices;
  TDirPlan = array [0 .. 5] of Integer;

const
  CBBFront: TPlanIndices = (0, 1, 2, 3);
  CBBBack: TPlanIndices = (4, 5, 6, 7);
  CBBLeft: TPlanIndices = (0, 4, 7, 3);
  CBBRight: TPlanIndices = (1, 5, 6, 2);
  CBBTop: TPlanIndices = (0, 1, 5, 4);
  CBBBottom: TPlanIndices = (2, 3, 7, 6);
  CBBPlans: TPlanBB = ((0, 1, 2, 3), (4, 5, 6, 7), (0, 4, 7, 3), (1, 5, 6, 2),
    (0, 1, 5, 4), (2, 3, 7, 6));
  CDirPlan: TDirPlan = (0, 0, 1, 1, 2, 2);

// --------------------------------------------------------------
implementation
// --------------------------------------------------------------
 
// ------------------------------------------------------------------------------
// ----------------- BB functions -------------------------------------------
// ------------------------------------------------------------------------------

procedure SetPlanBB(var BB: THmgBoundingBox; const NumPlan: Integer;
  const Valeur: Double);
var
  I: Integer;
begin
  for I := 0 to 3 do
  begin
    BB.BBox[CBBPlans[NumPlan][I]].V[CDirPlan[NumPlan]] := Valeur;
    BB.BBox[CBBPlans[NumPlan][I]].W := 1;
  end;
end;

function BoundingBoxesAreEqual(const ABoundingBox1, ABoundingBox2
  : THmgBoundingBox): Boolean;
begin
  Result := CompareMem(@ABoundingBox1, @ABoundingBox2, SizeOf(THmgBoundingBox));
end;

function BoundingBoxesAreEqual(const ABoundingBox1, ABoundingBox2
  : PHmgBoundingBox): Boolean;
begin
  Result := CompareMem(ABoundingBox1, ABoundingBox2, SizeOf(THmgBoundingBox));
end;

function AddBB(var C1: THmgBoundingBox; const C2: THmgBoundingBox): THmgBoundingBox;
var
  I, J: Integer;
begin
  for I := 0 to 7 do
  begin
    for J := 0 to 3 do
      if C1.BBox[CBBFront[J]].X < C2.BBox[I].X then
        SetPlanBB(C1, 0, C2.BBox[I].X);
    for J := 0 to 3 do
      if C1.BBox[CBBBack[J]].X > C2.BBox[I].X then
        SetPlanBB(C1, 1, C2.BBox[I].X);
    for J := 0 to 3 do
      if C1.BBox[CBBLeft[J]].Y < C2.BBox[I].Y then
        SetPlanBB(C1, 2, C2.BBox[I].Y);
    for J := 0 to 3 do
      if C1.BBox[CBBRight[J]].Y > C2.BBox[I].Y then
        SetPlanBB(C1, 3, C2.BBox[I].Y);
    for J := 0 to 3 do
      if C1.BBox[CBBTop[J]].Z < C2.BBox[I].Z then
        SetPlanBB(C1, 4, C2.BBox[I].Z);
    for J := 0 to 3 do
      if C1.BBox[CBBBottom[J]].Z > C2.BBox[I].Z then
        SetPlanBB(C1, 5, C2.BBox[I].Z);
  end;
  Result := C1;
end;

procedure AddAABB(var Aabb: TAABB; const Aabb1: TAABB);
begin
  if Aabb1.Min.X < Aabb.Min.X then
    Aabb.Min.X := Aabb1.Min.X;
  if Aabb1.Min.Y < Aabb.Min.Y then
    Aabb.Min.Y := Aabb1.Min.Y;
  if Aabb1.Min.Z < Aabb.Min.Z then
    Aabb.Min.Z := Aabb1.Min.Z;
  if Aabb1.Max.X > Aabb.Max.X then
    Aabb.Max.X := Aabb1.Max.X;
  if Aabb1.Max.Y > Aabb.Max.Y then
    Aabb.Max.Y := Aabb1.Max.Y;
  if Aabb1.Max.Z > Aabb.Max.Z then
    Aabb.Max.Z := Aabb1.Max.Z;
end;

procedure SetBB(var C: THmgBoundingBox; const V: TGLVector);
begin
  SetPlanBB(C, 0, V.X);
  SetPlanBB(C, 1, -V.X);
  SetPlanBB(C, 2, V.Y);
  SetPlanBB(C, 3, -V.Y);
  SetPlanBB(C, 4, V.Z);
  SetPlanBB(C, 5, -V.Z);
end;

procedure SetAABB(var Bb: TAABB; const V: TGLVector);
begin
  Bb.Max.X := Abs(V.X);
  Bb.Max.Y := Abs(V.Y);
  Bb.Max.Z := Abs(V.Z);
  Bb.Min.X := -Bb.Max.X;
  Bb.Min.Y := -Bb.Max.Y;
  Bb.Min.Z := -Bb.Max.Z;
end;

procedure BBTransform(var C: THmgBoundingBox; const M: TGLMatrix);
var
  I: Integer;
begin
  for I := 0 to 7 do
    C.BBox[I] := VectorTransform(C.BBox[I], M);
end;

procedure AABBTransform(var Bb: TAABB; const M: TGLMatrix);
var
  OldMin, OldMax: TAffineVector;
begin
  OldMin := Bb.Min;
  OldMax := Bb.Max;
  Bb.Min := VectorTransform(OldMin, M);
  Bb.Max := Bb.Min;
  AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMin.X,
    OldMin.Y, OldMax.Z), M));
  AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMin.X,
    OldMax.Y, OldMin.Z), M));
  AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMin.X,
    OldMax.Y, OldMax.Z), M));
  AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMax.X,
    OldMin.Y, OldMin.Z), M));
  AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMax.X,
    OldMin.Y, OldMax.Z), M));
  AABBInclude(Bb, VectorTransform(AffineVectorMake(OldMax.X,
    OldMax.Y, OldMin.Z), M));
  AABBInclude(Bb, VectorTransform(OldMax, M));
end;

procedure AABBScale(var Bb: TAABB; const V: TAffineVector);
begin
  ScaleVector(Bb.Min, V);
  ScaleVector(Bb.Max, V);
end;

function BBMinX(const C: THmgBoundingBox): Single;
var
  I: Integer;
begin
  Result := C.BBox[0].X;
  for I := 1 to 7 do
    Result := MinFloat(Result, C.BBox[I].X);
end;

function BBMaxX(const C: THmgBoundingBox): Single;
var
  I: Integer;
begin
  Result := C.BBox[0].X;
  for I := 1 to 7 do
    Result := MaxFloat(Result, C.BBox[I].X);
end;

function BBMinY(const C: THmgBoundingBox): Single;
var
  I: Integer;
begin
  Result := C.BBox[0].Y;
  for I := 1 to 7 do
    Result := MinFloat(Result, C.BBox[I].Y);
end;

function BBMaxY(const C: THmgBoundingBox): Single;
var
  I: Integer;
begin
  Result := C.BBox[0].Y;
  for I := 1 to 7 do
    Result := MaxFloat(Result, C.BBox[I].Y);
end;

function BBMinZ(const C: THmgBoundingBox): Single;
var
  I: Integer;
begin
  Result := C.BBox[0].Z;
  for I := 1 to 7 do
    Result := MinFloat(Result, C.BBox[I].Z);
end;

function BBMaxZ(const C: THmgBoundingBox): Single;
var
  I: Integer;
begin
  Result := C.BBox[0].Z;
  for I := 1 to 7 do
    Result := MaxFloat(Result, C.BBox[I].Z);
end;

procedure AABBInclude(var Bb: TAABB; const P: TAffineVector);
begin
  if P.X < Bb.Min.X then
    Bb.Min.X := P.X;
  if P.X > Bb.Max.X then
    Bb.Max.X := P.X;
  if P.Y < Bb.Min.Y then
    Bb.Min.Y := P.Y;
  if P.Y > Bb.Max.Y then
    Bb.Max.Y := P.Y;
  if P.Z < Bb.Min.Z then
    Bb.Min.Z := P.Z;
  if P.Z > Bb.Max.Z then
    Bb.Max.Z := P.Z;
end;

procedure AABBFromSweep(var SweepAABB: TAABB; const Start, Dest: TGLVector;
  const Radius: Single);
begin
  if Start.X < Dest.X then
  begin
    SweepAABB.Min.X := Start.X - Radius;
    SweepAABB.Max.X := Dest.X + Radius;
  end
  else
  begin
    SweepAABB.Min.X := Dest.X - Radius;
    SweepAABB.Max.X := Start.X + Radius;
  end;

  if Start.Y < Dest.Y then
  begin
    SweepAABB.Min.Y := Start.Y - Radius;
    SweepAABB.Max.Y := Dest.Y + Radius;
  end
  else
  begin
    SweepAABB.Min.Y := Dest.Y - Radius;
    SweepAABB.Max.Y := Start.Y + Radius;
  end;

  if Start.Z < Dest.Z then
  begin
    SweepAABB.Min.Z := Start.Z - Radius;
    SweepAABB.Max.Z := Dest.Z + Radius;
  end
  else
  begin
    SweepAABB.Min.Z := Dest.Z - Radius;
    SweepAABB.Max.Z := Start.Z + Radius;
  end;
end;

function AABBIntersection(const Aabb1, Aabb2: TAABB): TAABB;
var
  I: Integer;
begin
  for I := 0 to 2 do
  begin
    Result.Min.V[I] := MaxFloat(Aabb1.Min.V[I], Aabb2.Min.V[I]);
    Result.Max.V[I] := MinFloat(Aabb1.Max.V[I], Aabb2.Max.V[I]);
  end;
end;

function BBToAABB(const ABB: THmgBoundingBox): TAABB;
var
  I: Integer;
begin
  SetVector(Result.Min, ABB.BBox[0]);
  SetVector(Result.Max, ABB.BBox[0]);
  for I := 1 to 7 do
  begin
    if ABB.BBox[I].X < Result.Min.X then
      Result.Min.X := ABB.BBox[I].X;
    if ABB.BBox[I].X > Result.Max.X then
      Result.Max.X := ABB.BBox[I].X;
    if ABB.BBox[I].Y < Result.Min.Y then
      Result.Min.Y := ABB.BBox[I].Y;
    if ABB.BBox[I].Y > Result.Max.Y then
      Result.Max.Y := ABB.BBox[I].Y;
    if ABB.BBox[I].Z < Result.Min.Z then
      Result.Min.Z := ABB.BBox[I].Z;
    if ABB.BBox[I].Z > Result.Max.Z then
      Result.Max.Z := ABB.BBox[I].Z;
  end;
end;

function AABBToBB(const AnAABB: TAABB): THmgBoundingBox;
begin
  with AnAABB do
  begin
    SetPlanBB(Result, 0, Max.X);
    SetPlanBB(Result, 1, Min.X);
    SetPlanBB(Result, 2, Max.Y);
    SetPlanBB(Result, 3, Min.Y);
    SetPlanBB(Result, 4, Max.Z);
    SetPlanBB(Result, 5, Min.Z);
  end;
end;

function AABBToBB(const AnAABB: TAABB; const M: TGLMatrix): THmgBoundingBox;
begin
  Result := AABBToBB(AnAABB);
  BBTransform(Result, M);
end;

procedure OffsetAABB(var Aabb: TAABB; const Delta: TAffineVector);
begin
  AddVector(Aabb.Min, Delta);
  AddVector(Aabb.Max, Delta);
end;

procedure OffsetAABB(var Aabb: TAABB; const Delta: TGLVector);
begin
  AddVector(Aabb.Min, Delta);
  AddVector(Aabb.Max, Delta);
end;

procedure OffsetBB(var Bb: THmgBoundingBox; const Delta: TAffineVector);
var
  I: Integer;
  TempVector: TGLVector;
begin
  TempVector := VectorMake(Delta, 0);
  for I := 0 to 7 do
    AddVector(Bb.BBox[I], TempVector);
end;

procedure OffsetBB(var Bb: THmgBoundingBox; const Delta: TGLVector);
var
  I: Integer;
begin
  for I := 0 to 7 do
    AddVector(Bb.BBox[I], Delta);
end;

procedure OffsetBBPoint(var Bb: THmgBoundingBox; const Delta: TGLVector);
var
  I: Integer;
begin
  for I := 0 to 7 do
    AddPoint(Bb.BBox[I], Delta);
end;

function IntersectAABBs(const Aabb1, Aabb2: TAABB;
  const M1To2, M2To1: TGLMatrix): Boolean;
const
  CWires: array [0 .. 11, 0 .. 1] of Integer // Points of the wire
    = ((0, 1), (1, 2), (2, 3), (3, 0), (4, 5), (5, 6), (6, 7), (7, 4), (0, 4),
    (1, 5), (2, 6), (3, 7));
  CPlanes: array [0 .. 5, 0 .. 3] of Integer // points of the planes
    = ((1, 2, 6, 5), (2, 3, 7, 6), (0, 1, 2, 3), (0, 3, 7, 4), (0, 1, 5, 4),
    (5, 6, 7, 4));

  procedure MakeAABBPoints(const AABB: TAABB; var Pt: array of TVertex);
  begin
    with AABB do
    begin
      SetVector(Pt[0], Min.X, Min.Y, Min.Z);
      SetVector(Pt[1], Max.X, Min.Y, Min.Z);
      SetVector(Pt[2], Max.X, Max.Y, Min.Z);
      SetVector(Pt[3], Min.X, Max.Y, Min.Z);
      SetVector(Pt[4], Min.X, Min.Y, Max.Z);
      SetVector(Pt[5], Max.X, Min.Y, Max.Z);
      SetVector(Pt[6], Max.X, Max.Y, Max.Z);
      SetVector(Pt[7], Min.X, Max.Y, Max.Z);
    end;
  end;

  procedure MakePlanes(const Pt: array of TVertex;
    var Planes: array of THmgPlane);
  var
    I: Integer;
  begin
    for I := 0 to 5 do
      Planes[I] := PlaneMake(Pt[CPlanes[I, 0]], Pt[CPlanes[I, 1]],
        Pt[CPlanes[I, 2]]);
  end;

var
  Pt1, Pt2: array [0 .. 7] of TVertex;
  Pt: TVertex;
  Planes2: array [0 .. 5] of THmgPlane;
  I, T: Integer;
  V: TVertex;
  P: TGLVector;
begin
  Result := False;

  // Build Points
  MakeAABBPoints(AABB1, Pt1);
  MakeAABBPoints(AABB2, Pt2);
  for I := 0 to 7 do
  begin
    Pt := VectorTransform(Pt2[I], M2To1);
    // check for inclusion (points of Obj2 in Obj1)
    if IsInRange(Pt.X, AABB1.Min.X, AABB1.Max.X) and
      IsInRange(Pt.Y, AABB1.Min.Y, AABB1.Max.Y) and
      IsInRange(Pt.Z, AABB1.Min.Z, AABB1.Max.Z) then
    begin
      Result := True;
      Exit;
    end;
  end;

  for I := 0 to 7 do
  begin
    Pt1[I] := VectorTransform(Pt1[I], M1To2);
    // check for inclusion (points of Obj1 in Obj2)
    if IsInRange(Pt1[I].X, AABB2.Min.X, AABB2.Max.X) and
      IsInRange(Pt1[I].Y, AABB2.Min.Y, AABB2.Max.Y) and
      IsInRange(Pt1[I].Z, AABB2.Min.Z, AABB2.Max.Z) then
    begin
      Result := True;
      Exit;
    end;
  end;

  // Build Planes
  MakePlanes(Pt2, Planes2);

  // Wire test
  for I := 0 to 11 do
  begin
    for T := 0 to 5 do
    begin
      // Build Vector of Ray
      V := VectorSubtract(Pt1[CWires[I, 0]], Pt1[CWires[I, 1]]);
      if IntersectLinePlane(VectorMake(Pt1[CWires[I, 0]]), VectorMake(V),
        Planes2[T], @P) = 1 then
      begin
        // check point in Wire
        if IsInRange(P.X, Pt1[CWires[I, 0]].X,
          Pt1[CWires[I, 1]].X) and
          IsInRange(P.Y, Pt1[CWires[I, 0]].Y,
          Pt1[CWires[I, 1]].Y) and
          IsInRange(P.Z, Pt1[CWires[I, 0]].Z,
          Pt1[CWires[I, 1]].Z) then
        begin
          // check point in Plane
          if IsInRange(P.X, Pt2[CPlanes[T, 0]].X,
            Pt2[CPlanes[T, 2]].X) and
            IsInRange(P.Y, Pt2[CPlanes[T, 0]].Y,
            Pt2[CPlanes[T, 2]].Y) and
            IsInRange(P.Z, Pt2[CPlanes[T, 0]].Z,
            Pt2[CPlanes[T, 2]].Z) then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

function IntersectAABBsAbsoluteXY(const Aabb1, Aabb2: TAABB): Boolean;
begin
  Result := False;

  if (AABB2.Min.X > AABB1.Max.X) or
    (AABB2.Min.Y > AABB1.Max.Y) then
    Exit
  else if (AABB2.Max.X < AABB1.Min.X) or
    (AABB2.Max.Y < AABB1.Min.Y) then
    Exit
  else
    Result := True;

end;

function IntersectAABBsAbsoluteXZ(const Aabb1, Aabb2: TAABB): Boolean;
begin
  Result := ((AABB1.Min.X < AABB2.Max.X) and
    (AABB1.Min.Z < AABB2.Max.Z) and

    (AABB2.Min.X < AABB1.Max.X) and
    (AABB2.Min.Z < AABB1.Max.Z));
end;

function IntersectAABBsAbsolute(const Aabb1, Aabb2: TAABB): Boolean;
begin
  Result := not((AABB1.Min.X > AABB2.Max.X) or
    (AABB1.Min.Y > AABB2.Max.Y) or
    (AABB1.Min.Z > AABB2.Max.Z) or

    (AABB2.Min.X > AABB1.Max.X) or
    (AABB2.Min.Y > AABB1.Max.Y) or
    (AABB2.Min.Z > AABB1.Max.Z));
end;

function AABBFitsInAABBAbsolute(const Aabb1, Aabb2: TAABB): Boolean;
begin
  // AABB1 fits completely inside AABB2?
  // AABB1 min must be >= to AABB2 min
  // AABB1 max must be <= to AABB2 max

  Result := (AABB1.Min.X >= AABB2.Min.X) and
    (AABB1.Min.Y >= AABB2.Min.Y) and
    (AABB1.Min.Z >= AABB2.Min.Z) and

    (AABB1.Max.X <= AABB2.Max.X) and
    (AABB1.Max.Y <= AABB2.Max.Y) and
    (AABB1.Max.Z <= AABB2.Max.Z);
end;

function PointInAABB(const P: TAffineVector; const Aabb: TAABB): Boolean;
begin
  Result := (P.X <= Aabb.Max.X) and
    (P.X >= Aabb.Min.X) and (P.Y <= Aabb.Max.Y) and
    (P.Y >= Aabb.Min.Y) and (P.Z <= Aabb.Max.Z) and
    (P.Z >= Aabb.Min.Z);
end;

function PointInAABB(const P: TGLVector; const Aabb: TAABB): Boolean;
begin
  Result := (P.X <= Aabb.Max.X) and
    (P.X >= Aabb.Min.X) and (P.Y <= Aabb.Max.Y) and
    (P.Y >= Aabb.Min.Y) and (P.Z <= Aabb.Max.Z) and
    (P.Z >= Aabb.Min.Z);
end;

function PlaneIntersectAABB(const Normal: TAffineVector; D: Single; const Aabb: TAABB): Boolean;
var
  Vmax, Vmin: TAffineVector;
  I: Integer;
begin
  Result := False;
  for I := 0 to 2 do
    if Normal.V[I] > 0.0 then
    begin
      VMin.V[I] := Aabb.Min.V[I];
      VMax.V[I] := Aabb.Max.V[I];
    end
    else
    begin
      VMin.V[I] := Aabb.Max.V[I];
      VMax.V[I] := Aabb.Min.V[I];
    end;

  if VectorDotProduct(Normal, Vmin) + D > 0 then
    Exit;
  if VectorDotProduct(Normal, Vmax) + D >= 0 then
    Result := True;
end;

procedure FindMinMax(X0, X1, X2: Single; out Min, Max: Single);
begin
  Min := X0;
  Max := X0;
  if (X1 < Min) then
    Min := X1;
  if (X1 > Max) then
    Max := X1;
  if (X2 < Min) then
    Min := X2;
  if (X2 > Max) then
    Max := X2;
end;

function PlaneAABBIntersection(const plane : THmgPlane;const AABB : TAABB) : TGLAffineVectorList;
var
  i, j, annexe : Integer;
  index : array[0..2] of Integer;
  vec, temp : TVector3f;
  box : array [0..1] of TVector3f;
  V: array [0..7] of TVector3f;
  function EdgesStripPlaneIntersection(const pt0, pt1, pt4, pt7: TVector3f;
    const plane : THmgPlane; var inter : TVector3f): Boolean;
  begin
    Result := True;
    if not SegmentPlaneIntersection(pt0, pt1, plane, inter) then
      if not SegmentPlaneIntersection(pt1, pt4, plane, inter) then
        if not SegmentPlaneIntersection(pt4, pt7, plane, inter) then
          Result := False;
  end;
begin
  box[0] := AABB.min;
  box[1] := AABB.max;

  Result := TGLAffineVectorList.Create;

  // loop on vertices
  for i := 0 to 7 do
  begin
    for j := 0 to 2 do
    begin
      index[j] := (i div (1 shl j)) mod 2;
      vec.V[j] := box[index[j]].V[j];
    end;

    // try to find the right orientation to proceed intersection
    if (i = 0) then
    begin
      // prepare V 0 -> 7 array
      V[0] := vec;
      for j := 0 to 5 do
      begin
        temp := vec;
        temp.V[j mod 3] := box[(index[j mod 3] + 1) mod 2].V[j mod 3];
        if (j div 3) > 0 then
        begin
          temp.V[(j+1) mod 3] := box[(index[(j+1) mod 3] + 1) mod 2].V[(j+1) mod 3];
          if (j div 3) > 1 then
          begin
            temp.V[(j+2) mod 3] := box[(index[(j+2) mod 3] + 1) mod 2].V[(j+2) mod 3];
          end;
        end;
        V[j+1] := temp;
      end;
      for j := 0 to 2 do
        vec.V[j] := box[(index[j]+1) mod 2].V[j];
      V[7] := vec;
    end;
  end;

  //compute edge plane intersections
  for j := 0 to 2 do
  begin
    if j = 0 then annexe := 6 else annexe := j+3;

    // computes intersection with annexe edge
    if SegmentPlaneIntersection(V[j+1], V[annexe], plane, temp) then
      Result.Add(temp);
    // computes intersection with edge strip from main vertex V0 to opposite vertex V7
    if EdgesStripPlaneIntersection(V[0], V[j+1], V[j+4], V[7], plane, temp) then
      Result.Add(temp);
  end;
end;

function PlaneBoxOverlap(const Normal: TAffineVector; D: Single;
  const Maxbox: TAffineVector): Boolean;
var
  Q: Integer;
  Vmin, Vmax: TAffineVector;
begin
  Result := False;

  for Q := 0 to 2 do
  begin
    if (Normal.V[Q] > 0.0) then
    begin
      Vmin.V[Q] := -Maxbox.V[Q];
      Vmax.V[Q] := Maxbox.V[Q];
    end
    else
    begin
      Vmin.V[Q] := Maxbox.V[Q];
      Vmax.V[Q] := -Maxbox.V[Q];
    end;
  end;

  if (VectorDotProduct(Normal, Vmin) + D) > 0 then
    Exit;

  if (VectorDotProduct(Normal, Vmax) + D) >= 0 then
    Result := True;
end;

function TriangleIntersectAABB(const Aabb: TAABB;
  const V1, V2, V3: TAffineVector): Boolean;
var
  Boxcenter, Boxhalfsize: TAffineVector;
  Tv0, Tv1, Tv2: TAffineVector;
  Min, Max, D, P0, P1, P2, Rad, Fex, Fey, Fez: Single;
  Normal, E0, E1, E2: TAffineVector;
begin
  Result := False;

  Boxhalfsize := VectorSubtract(VectorScale(Aabb.Max, 0.5),
    VectorScale(Aabb.Min, 0.5));
  Boxcenter := VectorAdd(VectorScale(Aabb.Max, 0.5),
    VectorScale(Aabb.Min, 0.5));
  // move everything so that the boxcenter is in (0,0,0)
  VectorSubtract(V1, Boxcenter, Tv0);
  VectorSubtract(V2, Boxcenter, Tv1);
  VectorSubtract(V3, Boxcenter, Tv2);

  // compute triangle edges
  VectorSubtract(Tv1, Tv0, E0);
  VectorSubtract(Tv2, Tv1, E1);
  VectorSubtract(Tv0, Tv2, E2);

  // Bullet 3:
  // test the 9 tests first (this was faster)
  Fex := Abs(E0.X);
  Fey := Abs(E0.Y);
  Fez := Abs(E0.Z);

  // AXISTEST_X01(e0[Z], e0[Y], fez, fey);
  P0 := E0.Z * Tv0.Y - E0.Y * Tv0.Z;
  P2 := E0.Z * Tv2.Y - E0.Y * Tv2.Z;
  Min := MinFloat(P0, P2);
  Max := MaxFloat(P0, P2);
  Rad := Fez * Boxhalfsize.Y + Fey * Boxhalfsize.Z;
  if (Min > Rad) or (Max < -Rad) then
    Exit;

  // AXISTEST_Y02(e0[Z], e0[X], fez, fex);
  P0 := -E0.Z * Tv0.X + E0.X * Tv0.Z;
  P2 := -E0.Z * Tv2.X + E0.X * Tv2.Z;
  Min := MinFloat(P0, P2);
  Max := MaxFloat(P0, P2);
  Rad := Fez * Boxhalfsize.X + Fex * Boxhalfsize.Z;
  if (Min > Rad) or (Max < -Rad) then
    Exit;

  // AXISTEST_Z12(e0[Y], e0[X], fey, fex);
  P1 := E0.Y * Tv1.X - E0.X * Tv1.Y;
  P2 := E0.Y * Tv2.X - E0.X * Tv2.Y;
  Min := MinFloat(P1, P2);
  Max := MaxFloat(P1, P2);
  Rad := Fey * Boxhalfsize.X + Fex * Boxhalfsize.Y;
  if (Min > Rad) or (Max < -Rad) then
    Exit;

  Fex := Abs(E1.X);
  Fey := Abs(E1.Y);
  Fez := Abs(E1.Z);
  // AXISTEST_X01(e1[Z], e1[Y], fez, fey);
  P0 := E1.Z * Tv0.Y - E1.Y * Tv0.Z;
  P2 := E1.Z * Tv2.Y - E1.Y * Tv2.Z;
  Min := MinFloat(P0, P2);
  Max := MaxFloat(P0, P2);
  Rad := Fez * Boxhalfsize.Y + Fey * Boxhalfsize.Z;
  if (Min > Rad) or (Max < -Rad) then
    Exit;

  // AXISTEST_Y02(e1[Z], e1[X], fez, fex);
  P0 := -E1.Z * Tv0.X + E1.X * Tv0.Z;
  P2 := -E1.Z * Tv2.X + E1.X * Tv2.Z;
  Min := MinFloat(P0, P2);
  Max := MaxFloat(P0, P2);
  Rad := Fez * Boxhalfsize.X + Fex * Boxhalfsize.Z;
  if (Min > Rad) or (Max < -Rad) then
    Exit;

  // AXISTEST_Z0(e1[Y], e1[X], fey, fex);
  P0 := E1.Y * Tv0.X - E1.X * Tv0.Y;
  P1 := E1.Y * Tv1.X - E1.X * Tv1.Y;
  Min := MinFloat(P0, P1);
  Max := MaxFloat(P0, P1);
  Rad := Fey * Boxhalfsize.X + Fex * Boxhalfsize.Y;
  if (Min > Rad) or (Max < -Rad) then
    Exit;

  Fex := Abs(E2.X);
  Fey := Abs(E2.Y);
  Fez := Abs(E2.Z);
  // AXISTEST_X2(e2[Z], e2[Y], fez, fey);
  P0 := E2.Z * Tv0.Y - E2.Y * Tv0.Z;
  P1 := E2.Z * Tv1.Y - E2.Y * Tv1.Z;
  Min := MinFloat(P0, P1);
  Max := MaxFloat(P0, P1);
  Rad := Fez * Boxhalfsize.Y + Fey * Boxhalfsize.Z;
  if (Min > Rad) or (Max < -Rad) then
    Exit;

  // AXISTEST_Y1(e2[Z], e2[X], fez, fex);
  P0 := -E2.Z * Tv0.X + E2.X * Tv0.Z;
  P1 := -E2.Z * Tv1.X + E2.X * Tv1.Z;
  Min := MinFloat(P0, P1);
  Max := MaxFloat(P0, P1);
  Rad := Fez * Boxhalfsize.X + Fex * Boxhalfsize.Z;
  if (Min > Rad) or (Max < -Rad) then
    Exit;

  // AXISTEST_Z12(e2[Y], e2[X], fey, fex);
  P1 := E2.Y * Tv1.X - E2.X * Tv1.Y;
  P2 := E2.Y * Tv2.X - E2.X * Tv2.Y;
  Min := MinFloat(P1, P2);
  Max := MaxFloat(P1, P2);
  Rad := Fey * Boxhalfsize.X + Fex * Boxhalfsize.Y;
  if (Min > Rad) or (Max < -Rad) then
    Exit;

  // Bullet 1:
  // first test overlap in the {x,y,z}-directions
  // find min, max of the triangle each direction, and test for overlap in
  // that direction -- this is equivalent to testing a minimal AABB around
  // the triangle against the AABB

  // test in X-direction
  FindMinMax(Tv0.X, Tv1.X, Tv2.X, Min, Max);
  if (Min > Boxhalfsize.X) or (Max < -Boxhalfsize.X) then
    Exit;

  // test in Y-direction
  FindMinMax(Tv0.Y, Tv1.Y, Tv2.Y, Min, Max);
  if (Min > Boxhalfsize.Y) or (Max < -Boxhalfsize.Y) then
    Exit;

  // test in Z-direction
  FindMinMax(Tv0.Z, Tv1.Z, Tv2.Z, Min, Max);
  if (Min > Boxhalfsize.Z) or (Max < -Boxhalfsize.Z) then
    Exit;

  // Bullet 2:
  // test if the box intersects the plane of the triangle
  // compute plane equation of triangle: normal * x + d = 0
  VectorCrossProduct(E0, E1, Normal);
  D := -VectorDotProduct(Normal, Tv0); // plane eq: normal.x + d = 0
  if not PlaneBoxOverlap(Normal, D, Boxhalfsize) then
    Exit;

  // box and triangle overlaps
  Result := True;
end;

procedure ExtractAABBCorners(const AABB: TAABB; var AABBCorners: TAABBCorners);
begin
  MakeVector(AABBCorners[0], AABB.Min.X, AABB.Min.Y,
    AABB.Min.Z);
  MakeVector(AABBCorners[1], AABB.Min.X, AABB.Min.Y,
    AABB.Max.Z);
  MakeVector(AABBCorners[2], AABB.Min.X, AABB.Max.Y,
    AABB.Min.Z);
  MakeVector(AABBCorners[3], AABB.Min.X, AABB.Max.Y,
    AABB.Max.Z);

  MakeVector(AABBCorners[4], AABB.Max.X, AABB.Min.Y,
    AABB.Min.Z);
  MakeVector(AABBCorners[5], AABB.Max.X, AABB.Min.Y,
    AABB.Max.Z);
  MakeVector(AABBCorners[6], AABB.Max.X, AABB.Max.Y,
    AABB.Min.Z);
  MakeVector(AABBCorners[7], AABB.Max.X, AABB.Max.Y,
    AABB.Max.Z);
end;

procedure AABBToBSphere(const AABB: TAABB; var BSphere: TBSphere);
begin
  BSphere.Center := VectorScale(VectorAdd(AABB.Min, AABB.Max), 0.5);
  BSphere.Radius := VectorDistance(AABB.Min, AABB.Max) * 0.5;
end;

procedure BSphereToAABB(const BSphere: TBSphere; var AABB: TAABB);
begin
  AABB.Min := VectorSubtract(BSphere.Center, BSphere.Radius);
  AABB.Max := VectorAdd(BSphere.Center, BSphere.Radius);
end;

function BSphereToAABB(const Center: TAffineVector; Radius: Single): TAABB;
begin
  Result.Min := VectorSubtract(Center, Radius);
  Result.Max := VectorAdd(Center, Radius);
end;

function BSphereToAABB(const Center: TGLVector; Radius: Single): TAABB;
begin
  SetVector(Result.Min, VectorSubtract(Center, Radius));
  SetVector(Result.Max, VectorAdd(Center, Radius));
end;

function AABBContainsAABB(const MainAABB, TestAABB: TAABB): TSpaceContains;
begin
  // AABB1 fits completely inside AABB2?
  // AABB1 min must be >= to AABB2 min
  // AABB1 max must be <= to AABB2 max

  if ((MainAABB.Min.X < TestAABB.Max.X) and
    (MainAABB.Min.Y < TestAABB.Max.Y) and
    (MainAABB.Min.Z < TestAABB.Max.Z) and

    (TestAABB.Min.X < MainAABB.Max.X) and
    (TestAABB.Min.Y < MainAABB.Max.Y) and
    (TestAABB.Min.Z < MainAABB.Max.Z)) then
  begin
    if (TestAABB.Min.X >= MainAABB.Min.X) and
      (TestAABB.Min.Y >= MainAABB.Min.Y) and
      (TestAABB.Min.Z >= MainAABB.Min.Z) and

      (TestAABB.Max.X <= MainAABB.Max.X) and
      (TestAABB.Max.Y <= MainAABB.Max.Y) and
      (TestAABB.Max.Z <= MainAABB.Max.Z) then
      Result := ScContainsFully
    else
      Result := ScContainsPartially;
  end
  else
    Result := ScNoOverlap;
end;

function AABBContainsBSphere(const MainAABB: TAABB; const TestBSphere: TBSphere)
  : TSpaceContains;
var
  TestAABB: TAABB;
begin
  BSphereToAABB(TestBSphere, TestAABB);
  Result := AABBContainsAABB(MainAABB, TestAABB);
end;

function PlaneContainsBSphere(const Location, Normal: TAffineVector;
  const TestBSphere: TBSphere): TSpaceContains;
var
  Dist: Single;
begin
  Dist := PointPlaneDistance(TestBSphere.Center, Location, Normal);

  if Dist > TestBSphere.Radius then
    Result := ScNoOverlap
  else if Abs(Dist) <= TestBSphere.Radius then
    Result := ScContainsPartially
  else
    Result := ScContainsFully;
end;

function FrustumContainsBSphere(const Frustum: TFrustum;
  const TestBSphere: TBSphere): TSpaceContains;
var
  NegRadius: Single;
  HitCount: Integer;
  Distance: Single;
  I: Integer;
type
  TPlaneArray = array [0 .. 5] of THmgPlane;
begin
  NegRadius := -TestBSphere.Radius;

  HitCount := 0;
  // This would be fractionally faster to unroll, but oh so ugly!?
  for I := 0 to 5 do
  begin
    Distance := PlaneEvaluatePoint(TPlaneArray(Frustum)[I], TestBSphere.Center);
    if Distance < NegRadius then
    begin
      Result := ScNoOverlap;
      Exit;
    end
    else if Distance >= TestBSphere.Radius then
      Inc(HitCount);
  end; // }

  if HitCount = 6 then
    Result := ScContainsFully
  else
    Result := ScContainsPartially;
end;

// see http://www.flipcode.com/articles/article_frustumculling.shtml
function FrustumContainsAABB(const Frustum: TFrustum; const TestAABB: TAABB)
  : TSpaceContains;
type
  TPlaneArray = array [0 .. 5] of THmgPlane;
var
  IPlane, ICorner: Integer;
  PointIn: Boolean;
  AABBCorners: TAABBCorners;
  InCount: Integer;
  TotalIn: Integer;
begin
  ExtractAABBCorners(TestAABB, AABBCorners);

  TotalIn := 0;
  // test all 8 corners against the 6 sides
  // if all points are behind 1 specific plane, we are out
  // if we are in with all points, then we are fully in

  // For each plane
  for IPlane := Low(TPlaneArray) to High(TPlaneArray) do
  begin
    // We're about to test 8 corners
    InCount := 8;
    PointIn := True;

    // For each corner
    for ICorner := Low(AABBCorners) to High(AABBCorners) do
    begin
      if PlaneEvaluatePoint(TPlaneArray(Frustum)[IPlane], AABBCorners[ICorner]
        ) < 0 then
      begin
        PointIn := False;
        Dec(InCount);
      end;
    end;

    if InCount = 0 then
    begin
      Result := ScNoOverlap;
      Exit;
    end

    else if PointIn then
      Inc(TotalIn);
  end;

  if TotalIn = 6 then
    Result := ScContainsFully
  else
    Result := ScContainsPartially;
end;

function BSphereContainsAABB(const MainBSphere: TBSphere; const TestAABB: TAABB)
  : TSpaceContains;
var
  R2: Single;
  ClippedCenter: TAffineVector;

  AABBCorners: TAABBCorners;
  CornerHitCount: Integer;
begin
  R2 := Sqr(MainBSphere.Radius);

  ClippedCenter := ClipToAABB(MainBSphere.Center, TestAABB);

  if VectorDistance2(ClippedCenter, MainBSphere.Center) < R2 then
  begin
    ExtractAABBCorners(TestAABB, AABBCorners);

    CornerHitCount := 0;
    // BSphere fully contains aabb if all corners of aabb are within bsphere.
    if (VectorDistance2(MainBSphere.Center, AABBCorners[0]) < R2) then
      Inc(CornerHitCount);
    if (VectorDistance2(MainBSphere.Center, AABBCorners[1]) < R2) then
      Inc(CornerHitCount);
    if (VectorDistance2(MainBSphere.Center, AABBCorners[2]) < R2) then
      Inc(CornerHitCount);
    if (VectorDistance2(MainBSphere.Center, AABBCorners[3]) < R2) then
      Inc(CornerHitCount);
    if (VectorDistance2(MainBSphere.Center, AABBCorners[4]) < R2) then
      Inc(CornerHitCount);
    if (VectorDistance2(MainBSphere.Center, AABBCorners[5]) < R2) then
      Inc(CornerHitCount);
    if (VectorDistance2(MainBSphere.Center, AABBCorners[6]) < R2) then
      Inc(CornerHitCount);
    if (VectorDistance2(MainBSphere.Center, AABBCorners[7]) < R2) then
      Inc(CornerHitCount);

    if CornerHitCount = 7 then
      Result := ScContainsFully
    else
      Result := ScContainsPartially;
  end
  else
    Result := ScNoOverlap;
end;

function BSphereContainsBSphere(const MainBSphere, TestBSphere: TBSphere)
  : TSpaceContains;
var
  D2: Single;
begin
  D2 := VectorDistance2(MainBSphere.Center, TestBSphere.Center);

  if D2 < Sqr(MainBSphere.Radius + TestBSphere.Radius) then
  begin
    if D2 < Sqr(MainBSphere.Radius - TestBSphere.Radius) then
      Result := ScContainsFully
    else
      Result := ScContainsPartially;
  end
  else
    Result := ScNoOverlap;
end;

function BSphereIntersectsBSphere(const MainBSphere,
  TestBSphere: TBSphere): Boolean;
begin
  Result := VectorDistance2(MainBSphere.Center, TestBSphere.Center) <
    Sqr(MainBSphere.Radius + TestBSphere.Radius);
end;

function ClipToAABB(const V: TAffineVector; const AABB: TAABB): TAffineVector;
begin
  Result := V;

  if Result.X < AABB.Min.X then
    Result.X := AABB.Min.X;
  if Result.Y < AABB.Min.Y then
    Result.Y := AABB.Min.Y;
  if Result.Z < AABB.Min.Z then
    Result.Z := AABB.Min.Z;

  if Result.X > AABB.Max.X then
    Result.X := AABB.Max.X;
  if Result.Y > AABB.Max.Y then
    Result.Y := AABB.Max.Y;
  if Result.Z > AABB.Max.Z then
    Result.Z := AABB.Max.Z;
end;

procedure IncludeInClipRect(var ClipRect: TGLClipRect; X, Y: Single);
begin
  with ClipRect do
  begin
    if X < Left then
      Left := X;
    if X > Right then
      Right := X;
    if Y < Top then
      Top := Y;
    if Y > Bottom then
      Bottom := Y;
  end;
end;

function AABBToClipRect(const Aabb: TAABB; const ModelViewProjection: TGLMatrix;
  ViewportSizeX, ViewportSizeY: Integer): TGLClipRect;
var
  I: Integer;
  V, Vt: TGLVector;
  Minmax: array [0 .. 1] of PAffineVector;
begin
  Minmax[0] := @Aabb.Min;
  Minmax[1] := @Aabb.Max;
  V.W := 1;
  for I := 0 to 7 do
  begin
    V.X := Minmax[I and 1].X;
    V.Y := Minmax[(I shr 1) and 1].Y;
    V.Z := Minmax[(I shr 2) and 1].Z;

    // Project
    Vt := VectorTransform(V, ModelViewProjection);
    ScaleVector(Vt, 1 / Vt.W);

    // Convert to screen coordinates
    if I > 0 then
      IncludeInClipRect(Result, ViewportSizeX * (Vt.X + 1) * 0.5,
        ViewportSizeY * (Vt.Y + 1) * 0.5)
    else
    begin
      Result.Left := ViewportSizeX * (Vt.X + 1) * 0.5;
      Result.Top := ViewportSizeY * (Vt.Y + 1) * 0.5;
      Result.Right := Result.Left;
      Result.Bottom := Result.Top;
    end;
  end;
end;

function RayCastAABBIntersect(const RayOrigin, RayDirection: TGLVector;
  const Aabb: TAABB; out TNear, TFar: Single): Boolean; overload;
const
  Infinity = 1.0 / 0.0;
var
  P: Integer;
  InvDir: Double;
  T0, T1, Tmp: Single;
begin
  Result := False;

  TNear := -Infinity;
  TFar := Infinity;

  for P := 0 to 2 do
  begin
    if (RayDirection.V[P] = 0) then
    begin
      if ((RayOrigin.V[P] < Aabb.Min.V[P]) or
        (RayOrigin.V[P] > Aabb.Max.V[P])) then
        Exit;
    end
    else
    begin
      InvDir := 1 / RayDirection.V[P];
      T0 := (Aabb.Min.V[P] - RayOrigin.V[P]) * InvDir;
      T1 := (Aabb.Max.V[P] - RayOrigin.V[P]) * InvDir;

      if (T0 > T1) then
      begin
        Tmp := T0;
        T0 := T1;
        T1 := Tmp;
      end;

      if (T0 > TNear) then
        TNear := T0;
      if (T1 < TFar) then
        TFar := T1;

      if ((TNear > TFar) or (TFar < 0)) then
        Exit;
    end;
  end;

  Result := True;
end;

function RayCastAABBIntersect(const RayOrigin, RayDirection: TGLVector;
  const Aabb: TAABB; IntersectPoint: PGLVector = nil): Boolean; overload;
var
  TNear, TFar: Single;
begin
  Result := RayCastAABBIntersect(RayOrigin, RayDirection, Aabb, TNear, TFar);

  if Result and Assigned(IntersectPoint) then
  begin
    if TNear >= 0 then
      // origin outside the box
      IntersectPoint^ := VectorCombine(RayOrigin, RayDirection, 1, TNear)
    else
      // origin inside the box, near is "behind", use far
      IntersectPoint^ := VectorCombine(RayOrigin, RayDirection, 1, TFar);
  end;
end;

end.
