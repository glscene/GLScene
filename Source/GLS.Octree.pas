//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.Octree;

(*
  Octree management classes and structures.
  TODO: move the many public vars/fields to private/protected
*)


interface

{$I GLScene.inc}

uses
  System.Math,
  System.Classes,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.GeometryBB,
  GLS.Context;

type

  TProcInt = procedure(I: Integer) of object;
  TProcAffineAffineAffine = procedure(V1, V2, V3: TAffineFLTVector) of object;

  //  Stores information about an intersected triangle
  POctreeTriangleInfo = ^TOctreeTriangleInfo;
  TOctreeTriangleInfo = record
    Index: Integer;
    Vertex: array [0 .. 2] of TAffineVector;
  end;

  POctreeNode = ^TOctreeNode;
  TOctreeNode = record
    MinExtent: TAffineFLTVector;
    MaxExtent: TAffineFLTVector;
    // TextureHandle:TGLTextureHandle;
    ListHandle: TGLListHandle;
    WillDraw: Boolean; // temporary change
    // Duplicates possible?
    TriArray: array of Integer; // array of triangle references
    ChildArray: array [0 .. 7] of POctreeNode; // Octree's 8 children
  end;

  //  Manages an Octree containing references to triangles.
  TGLOctree = class(TObject)
  private
{$IFDEF DEBUG}
    Intersections: Integer;
    // for debugging  - number of triangles intersecting an AABB plane
{$ENDIF}
  protected
    // Find the exact centre of an AABB
    function GetMidPoint(Min, Max: Single): Single;
    // Check if a point lies within the AABB specified by min and max entents
    function PointInNode(const Min, Max, APoint: TAffineFLTVector): Boolean;
    // Check if a triangle (vertices v1, v2, v3) lies within the AABB specified by min and max entents
    function TriIntersectNode(const MinExtent, MaxExtent, V1, V2,
      V3: TAffineFLTVector): BOOLEAN;
    // Check if a sphere (at point C with radius) lies within the AABB specified by min and max entents
    function SphereInNode(const MinExtent, MaxExtent: TAffineVector;
      const C: TGLVector; Radius: Single): Boolean;
    procedure WalkTriToLeafx(Onode: POctreeNode;
      const V1, V2, V3: TAffineFLTVector);
    procedure WalkPointToLeafx(ONode: POctreeNode; const P: TAffineVector);
    procedure WalkSphereToLeafx(Onode: POctreeNode; const P: TGLVector;
      Radius: Single);
    procedure WalkRayToLeafx(Onode: POctreeNode; const P, V: TGLVector);
    function GetExtent(const Flags: array of Byte; ParentNode: POctreeNode)
      : TAffineFLTVector;
    //  Recursive routine to build nodes from parent to max depth level.
    procedure Refine(ParentNode: POctreeNode; Level: Integer);
    // Main "walking" routines.  Walks the item through the Octree down to a leaf node.
    procedure WalkPointToLeaf(ONode: POctreeNode; const P: TAffineVector);
    procedure WalkTriToLeaf(Onode: POctreeNode;
      const V1, V2, V3: TAffineVector);
    procedure WalkRayToLeaf(Onode: POctreeNode; const P, V: TGLVector);
    // Example of how to process each node in the tree
    procedure ConvertR4(ONode: POctreeNode; const Scale: TAffineFLTVector);
    procedure CreateTree(Depth: Integer);
    procedure CutMesh;
  public
    WorldMinExtent, WorldMaxExtent: TAffineFLTVector;
    RootNode: POctreeNode; // always points to root node
    MaxOlevel: Integer; // max depth level of TOctreeNode
    NodeCount: Integer;
    // number of nodes (ex: 8 for level 1, 72 for level 2 etc).
    TriCountMesh: Integer; // total number of triangles in the mesh
    TriCountOctree: Integer; // total number of triangles cut into the octree
    MeshCount: Integer; // number of meshes currently cut into the Octree
    ResultArray: array of POctreeNode;
    // holds the result nodes of various calls
    TriangleFiler: TAffineVectorList;
    procedure WalkSphereToLeaf(Onode: POctreeNode; const P: TGLVector;
      Radius: Single);
    (*  Initializes the tree from the triangle list.
      All triangles must be contained in the world extent to be properly
      taken into account. *)
    procedure InitializeTree(const AWorldMinExtent, AWorldMaxExtent
      : TAffineVector; const ATriangles: TAffineVectorList;
      const ATreeDepth: Integer);
    procedure DisposeTree;
    destructor Destroy; override;
    function RayCastIntersect(const RayStart, RayVector: TGLVector;
      IntersectPoint: PGLVector = nil; IntersectNormal: PGLVector = nil;
      TriangleInfo: POctreeTriangleInfo = nil): Boolean;
    function SphereSweepIntersect(const RayStart, RayVector: TGLVector;
      const Velocity, Radius: Single; IntersectPoint: PGLVector = nil;
      IntersectNormal: PGLVector = nil): Boolean;
    function TriangleIntersect(const V1, V2, V3: TAffineVector): Boolean;
    //  Returns all triangles in the AABB.
    function GetTrianglesFromNodesIntersectingAABB(const ObjAABB: TAABB)
      : TAffineVectorList;
    //  Returns all triangles in an arbitrarily placed cube
    function GetTrianglesFromNodesIntersectingCube(const ObjAABB: TAABB;
      const ObjToSelf, SelfToObj: TGLMatrix): TAffineVectorList;
    //  Checks if an AABB intersects a face on the octree
    function AABBIntersect(const AABB: TAABB; const M1to2, M2to1: TGLMatrix;
      Triangles: TAffineVectorList = nil): Boolean;
    // function SphereIntersect(position:TAffineVector; radius:single);
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ----------------------------------------------------------------------
// Name  : CheckPointInSphere()
// Input : point - point we wish to check for inclusion
// sO - Origin of sphere
// sR - radius of sphere
// Notes :
// Return: TRUE if point is in sphere, FALSE if not.
// -----------------------------------------------------------------------

function CheckPointInSphere(const Point, SO: TGLVector; const SR: Single)
  : Boolean;
begin
  // Allow small margin of error
  Result := (VectorDistance2(Point, SO) <= Sqr(SR));
end;

// ----------------------------------------------------------------------
// Name  : CheckPointInTriangle()
// Input : point - point we wish to check for inclusion
// a - first vertex in triangle
// b - second vertex in triangle
// c - third vertex in triangle
// Notes : Triangle should be defined in clockwise order a,b,c
// Return: TRUE if point is in triangle, FALSE if not.
// -----------------------------------------------------------------------
function CheckPointInTriangle(const Point, A, B, C: TAffineVector): Boolean;
var
  Total_angles: Single;
  V1, V2, V3: TAffineVector;
begin
  Total_angles := 0;

  // make the 3 vectors
  V1 := VectorSubtract(Point, A);
  V2 := VectorSubtract(Point, B);
  V3 := VectorSubtract(Point, C);

  NormalizeVector(V1);
  NormalizeVector(V2);
  NormalizeVector(V3);

  Total_angles := Total_angles + Arccos(VectorDotProduct(V1, V2));
  Total_angles := Total_angles + Arccos(VectorDotProduct(V2, V3));
  Total_angles := Total_angles + Arccos(VectorDotProduct(V3, V1));

  if (Abs(Total_angles - 2 * PI) <= 0.005) then
    Result := TRUE
  else
    Result := FALSE;
end;

// ----------------------------------------------------------------------
// Name  : ClosestPointOnLine()
// Input : a - first end of line segment
// b - second end of line segment
// p - point we wish to find closest point on line from
// Notes : Helper function for closestPointOnTriangle()
// Return: closest point on line segment
// -----------------------------------------------------------------------
function ClosestPointOnLine(const A, B, P: TAffineVector): TAffineVector;
var
  D, T: Double;
  C, V: TAffineFLTVector;
begin
  VectorSubtract(P, A, C);
  VectorSubtract(B, A, V);
  D := VectorLength(V);
  NormalizeVector(V);
  T := VectorDotProduct(V, C);

  // Check to see if t is beyond the extents of the line segment
  if (T < 0.0) then
    Result := A
  else if (T > D) then
    Result := B
  else
  begin
    V.X := V.X * T;
    V.Y := V.Y * T;
    V.Z := V.Z * T;
    Result := VectorAdd(A, V);
  end;
end;

// ----------------------------------------------------------------------
// Name  : ClosestPointOnTriangle()
// Input : a - first vertex in triangle
// b - second vertex in triangle
// c - third vertex in triangle
// p - point we wish to find closest point on triangle from
// Notes :
// Return: closest point on triangle
// -----------------------------------------------------------------------
(*
  function ClosestPointOnTriangle(const a, b, c, n, p: TAffineVector): TAffineVector;
  var
  dAB, dBC, dCA : Single;
  Rab, Rbc, Rca, intPoint : TAffineFLTVector;
  hit:boolean;
  begin
  //this would be faster if RayCastTriangleIntersect detected backwards hits
  hit:=RayCastTriangleIntersect(VectorMake(p),VectorMake(n),a,b,c,@intPoint) or
  RayCastTriangleIntersect(VectorMake(p),VectorMake(VectorNegate(n)),a,b,c,@intPoint);
  if (hit) then
  begin
  Result:=intPoint;
  end
  else
  begin
  Rab:=ClosestPointOnLine(a, b, p);
  Rbc:=ClosestPointOnLine(b, c, p);
  Rca:=ClosestPointOnLine(c, a, p);

  dAB:=VectorDistance2(p, Rab);
  dBC:=VectorDistance2(p, Rbc);
  dCA:=VectorDistance2(p, Rca);

  if dBC<dAB then
  if dCA<dBC then
  Result:=Rca
  else Result:=Rbc
  else if dCA<dAB then
  Result:=Rca
  else Result:=Rab;
  end;
  end;
*)

// ----------------------------------------------------------------------
// Name  : ClosestPointOnTriangleEdge()
// Input : a - first vertex in triangle
// b - second vertex in triangle
// c - third vertex in triangle
// p - point we wish to find closest point on triangle from
// Notes :
// Return: closest point on line triangle edge
// -----------------------------------------------------------------------

function ClosestPointOnTriangleEdge(const A, B, C, P: TAffineVector)
  : TAffineVector;
var
  DAB, DBC, DCA: Single;
  Rab, Rbc, Rca: TAffineFLTVector;
begin
  Rab := ClosestPointOnLine(A, B, P);
  Rbc := ClosestPointOnLine(B, C, P);
  Rca := ClosestPointOnLine(C, A, P);

  DAB := VectorDistance2(P, Rab);
  DBC := VectorDistance2(P, Rbc);
  DCA := VectorDistance2(P, Rca);

  if DBC < DAB then
    if DCA < DBC then
      Result := Rca
    else
      Result := Rbc
  else if DCA < DAB then
    Result := Rca
  else
    Result := Rab;
end;

function HitBoundingBox(const MinB, MaxB: TAffineFLTVector;
  const Origin, Dir: TGLVector; var Coord: TGLVector): BOOLEAN;
const
  NUMDIM = 2;
  RIGHT = 0;
  LEFT = 1;
  MIDDLE = 2;
var
  I, Whichplane: Integer;
  Inside: BOOLEAN;
  Quadrant: array [0 .. NUMDIM] of Byte;
  MaxT: array [0 .. NUMDIM] of Double;
  CandidatePlane: array [0 .. NUMDIM] of Double;

begin
  Inside := TRUE;

  // Find candidate planes; this loop can be avoided if
  // rays cast all from the eye(assume perpsective view)
  for I := 0 to NUMDIM do
  begin
    if (Origin.V[I] < MinB.V[I]) then
    begin
      Quadrant[I] := LEFT;
      CandidatePlane[I] := MinB.V[I];
      Inside := FALSE;
    end
    else if (Origin.V[I] > MaxB.V[I]) then
    begin
      Quadrant[I] := RIGHT;
      CandidatePlane[I] := MaxB.V[I];
      Inside := FALSE;
    end
    else
      Quadrant[I] := MIDDLE;
  end;

  // * Ray origin inside bounding box */
  if Inside then
  begin
    SetVector(Coord, Origin);
    Result := TRUE;
    Exit;
  end;

  // * Calculate T distances to candidate planes */
  for I := 0 to NUMDIM do
  begin
    if (Quadrant[I] <> MIDDLE) AND (Dir.V[I] <> 0) then
      MaxT[I] := (CandidatePlane[I] - Origin.V[I]) / Dir.V[I]
    else
      MaxT[I] := -1;
  end;

  // * Get largest of the maxT's for final choice of intersection */
  WhichPlane := 0;
  for I := 1 to NUMDIM do
    if (MaxT[WhichPlane] < MaxT[I]) then
      WhichPlane := I;

  // * Check final candidate actually inside box */
  if (MaxT[WhichPlane] < 0) then
  begin
    Result := FALSE;
    Exit;
  end;

  for I := 0 to NUMDIM do
  begin
    if WhichPlane <> I then
    begin
      Coord.V[I] := Origin.V[I] + MaxT[WhichPlane] * Dir.V[I];
      if (Coord.V[I] < MinB.V[I]) OR (Coord.V[I] > MaxB.V[I])
      then
      begin
        Result := FALSE;
        Exit;
      end;
    end
    else
      Coord.V[I] := CandidatePlane[I];
  end;
  Result := TRUE; // * ray hits box */
end;

const
  USE_EPSILON_TEST = TRUE;
  EPSILON = 0.000001;

function Coplanar_tri_tri(const N, V0, V1, V2, U0, U1,
  U2: TAffineFLTVEctor): Integer;
var
  A: TAffineFLTVector;
  I0, I1: Shortint;

  function EDGE_AGAINST_TRI_EDGES(const V0, V1, U0, U1,
    U2: TAffineFLTVector): Integer;
  var
    Ax, Ay, Bx, By, Cx, Cy, E, D, F: Single;

    // * this edge to edge test is based on Franlin Antonio's gem:
    // "Faster Line Segment Intersection", in Graphics Gems III,
    // pp. 199-202 */
    function EDGE_EDGE_TEST(const V0, U0, U1: TAffineFLTVector): Integer;
    begin
      Result := 0;
      Bx := U0.V[I0] - U1.V[I0];
      By := U0.V[I1] - U1.V[I1];
      Cx := V0.V[I0] - U0.V[I0];
      Cy := V0.V[I1] - U0.V[I1];
      F := Ay * Bx - Ax * By;
      D := By * Cx - Bx * Cy;
      if ((F > 0) and (D >= 0) and (D <= F)) or
        ((F < 0) and (D <= 0) and (D >= F)) then
      begin
        E := Ax * Cy - Ay * Cx;
        if (F > 0) then
        begin
          if (E >= 0) and (E <= F) then
            Result := 1
        end
        else if (E <= 0) and (E >= F) then
          Result := 1;
      end;
    end;

  begin
    Ax := V1.V[I0] - V0.V[I0];
    Ay := V1.V[I1] - V0.V[I1];
    // * test edge U0,U1 against V0,V1 */
    Result := EDGE_EDGE_TEST(V0, U0, U1);
    if Result = 1 then
      Exit;
    // * test edge U1,U2 against V0,V1 */
    Result := EDGE_EDGE_TEST(V0, U1, U2);
    if Result = 1 then
      Exit;
    // * test edge U2,U1 against V0,V1 */
    Result := EDGE_EDGE_TEST(V0, U2, U0);
  end;

  function POINT_IN_TRI(const V0, U0, U1, U2: TAffineFLTVector): Integer;
  var
    A, B, C, D0, D1, D2: Single;
  begin
    Result := 0;
    // * is T1 completly inside T2? */
    // * check if V0 is inside tri(U0,U1,U2) */
    A := U1.V[I1] - U0.V[I1];
    B := -(U1.V[I0] - U0.V[I0]);
    C := -A * U0.V[I0] - B * U0.V[I1];
    D0 := A * V0.V[I0] + B * V0.V[I1] + C;

    A := U2.V[I1] - U1.V[I1];
    B := -(U2.V[I0] - U1.V[I0]);
    C := -A * U1.V[I0] - B * U1.V[I1];
    D1 := A * V0.V[I0] + B * V0.V[I1] + C;

    A := U0.V[I1] - U2.V[I1];
    B := -(U0.V[I0] - U2.V[I0]);
    C := -A * U2.V[I0] - B * U2.V[I1];
    D2 := A * V0.V[I0] + B * V0.V[I1] + C;
    if (D0 * D1 > 0.0) then
      if (D0 * D2 > 0.0) then
        Result := 1;
  end;

/// Begin Main logic ///////////////////////////////
begin
  // * first project onto an axis-aligned plane, that maximizes the area */
  // * of the triangles, compute indices: i0,i1. */
  A.X := Abs(N.X);
  A.Y := Abs(N.Y);
  A.Z := Abs(N.Z);
  if (A.X > A.Y) then
  begin
    if (A.X > A.Z) then
    begin
      I0 := 1; // * A[0] is greatest */
      I1 := 2;
    end
    else
    begin
      I0 := 0; // * A[2] is greatest */
      I1 := 1;
    end
  end
  else
  begin // * A[0]<=A[1] */
    if (A.Z > A.Y) then
    begin
      I0 := 0; // * A[2] is greatest */
      I1 := 1;
    end
    else
    begin
      I0 := 0; // * A[1] is greatest */
      I1 := 2;
    end
  end;

  // * test all edges of triangle 1 against the edges of triangle 2 */
  Result := EDGE_AGAINST_TRI_EDGES(V0, V1, U0, U1, U2);
  if Result = 1 then
    Exit;
  Result := EDGE_AGAINST_TRI_EDGES(V1, V2, U0, U1, U2);
  if Result = 1 then
    Exit;
  Result := EDGE_AGAINST_TRI_EDGES(V2, V0, U0, U1, U2);
  if Result = 1 then
    Exit;

  // * finally, test if tri1 is totally contained in tri2 or vice versa */
  Result := POINT_IN_TRI(V0, U0, U1, U2);
  if Result = 1 then
    Exit;
  Result := POINT_IN_TRI(U0, V0, V1, V2);
end;

function Tri_tri_intersect(const V0, V1, V2, U0, U1,
  U2: TAFFineFLTVector): Integer;
var
  E1, E2: TAffineFLTVector;
  N1, N2: TAffineFLTVector;
  D1, D2: Single;
  Du0, Du1, Du2, Dv0, Dv1, Dv2: Single;
  D: TAffineFLTVector;
  Isect1: array [0 .. 1] of Single;
  Isect2: array [0 .. 1] of Single;
  Du0du1, Du0du2, Dv0dv1, Dv0dv2: Single;
  Index: Shortint;
  Vp0, Vp1, Vp2: Single;
  Up0, Up1, Up2: Single;
  B, C, Max: Single;

  procedure ISECT(VV0, VV1, VV2, D0, D1, D2: Single;
    out Isect0, Isect1: Single);
  begin
    Isect0 := VV0 + (VV1 - VV0) * D0 / (D0 - D1);
    Isect1 := VV0 + (VV2 - VV0) * D0 / (D0 - D2);
  end;

  function COMPUTE_INTERVALS(VV0, VV1, VV2, D0, D1, D2, D0D1, D0D2: Single;
    out Isect0, Isect1: Single): Integer;
  begin
    Result := 0;
    if (D0D1 > 0.0) then
      // * here we know that D0D2<=0.0 */
      // * that is D0, D1 are on the same side, D2 on the other or on the plane */ \
      ISECT(VV2, VV0, VV1, D2, D0, D1, Isect0, Isect1)
    else if (D0D2 > 0.0) then
      // * here we know that d0d1<=0.0 */
      ISECT(VV1, VV0, VV2, D1, D0, D2, Isect0, Isect1)
    else if (D1 * D2 > 0.0) or (D0 <> 0.0) then
      // * here we know that d0d1<=0.0 or that D0!=0.0 */
      ISECT(VV0, VV1, VV2, D0, D1, D2, Isect0, Isect1)
    else if (D1 <> 0.0) then
      ISECT(VV1, VV0, VV2, D1, D0, D2, Isect0, Isect1)
    else if (D2 <> 0.0) then
      ISECT(VV2, VV0, VV1, D2, D0, D1, Isect0, Isect1)
    else
      // * triangles are coplanar */
      Result := Coplanar_tri_tri(N1, V0, V1, V2, U0, U1, U2);
  end;

// * sort so that a<=b */
  procedure SORT(var A: Single; var B: Single);
  var
    C: Single;
  begin
    if (A > B) then
    begin
      C := A;
      A := B;
      B := C;
    end;
  end;

begin
  // * compute plane equation of triangle(V0,V1,V2) */
  E1 := VectorSubtract(V1, V0);
  E2 := VectorSubtract(V2, V0);
  N1 := VectorCrossProduct(E1, E2);
  D1 := -VectorDotProduct(N1, V0);
  // * plane equation 1: N1.X+d1=0 */

  // * put U0,U1,U2 into plane equation 1 to compute signed distances to the plane*/
  Du0 := VectorDotProduct(N1, U0) + D1;
  Du1 := VectorDotProduct(N1, U1) + D1;
  Du2 := VectorDotProduct(N1, U2) + D1;

  // * coplanarity robustness check */
  if USE_EPSILON_TEST = TRUE then
  begin
    if (Abs(Du0) < EPSILON) then
      Du0 := 0.0;
    if (Abs(Du1) < EPSILON) then
      Du1 := 0.0;
    if (Abs(Du2) < EPSILON) then
      Du2 := 0.0;
  end;
  Du0du1 := Du0 * Du1;
  Du0du2 := Du0 * Du2;

  if (Du0du1 > 0.0) and (Du0du2 > 0.0) then
  begin // * same sign on all of them + not equal 0 ? */
    Result := 0; // * no intersection occurs */
    Exit;
  end;

  // * compute plane of triangle (U0,U1,U2) */
  E1 := VectorSubtract(U1, U0);
  E2 := VectorSubtract(U2, U0);
  N2 := VectorCrossProduct(E1, E2);
  D2 := -VectorDotProduct(N2, U0);
  // * plane equation 2: N2.X+d2=0 */

  // * put V0,V1,V2 into plane equation 2 */
  Dv0 := VectorDotProduct(N2, V0) + D2;
  Dv1 := VectorDotProduct(N2, V1) + D2;
  Dv2 := VectorDotProduct(N2, V2) + D2;

  if USE_EPSILON_TEST = TRUE then
  begin
    if (Abs(Dv0) < EPSILON) then
      Dv0 := 0.0;
    if (Abs(Dv1) < EPSILON) then
      Dv1 := 0.0;
    if (Abs(Dv2) < EPSILON) then
      Dv2 := 0.0;
  end;

  Dv0dv1 := Dv0 * Dv1;
  Dv0dv2 := Dv0 * Dv2;

  if (Dv0dv1 > 0.0) and (Dv0dv2 > 0.0) then
  begin // * same sign on all of them + not equal 0 ? */
    Result := 0; // * no intersection occurs */
    Exit;
  end;

  // * compute direction of intersection line */
  D := VectorCrossProduct(N1, N2);

  // * compute and index to the largest component of D */
  Max := Abs(D.X);
  index := 0;
  B := Abs(D.Y);
  C := Abs(D.Z);
  if (B > Max) then
  begin
    Max := B;
    index := 1;
  end;
  if (C > Max) then
  begin
    // max:=c;   why?
    index := 2;
  end;
  // * this is the simplified projection onto L*/
  Vp0 := V0.V[index];
  Vp1 := V1.V[index];
  Vp2 := V2.V[index];

  Up0 := U0.V[index];
  Up1 := U1.V[index];
  Up2 := U2.V[index];

  // * compute interval for triangle 1 */
  COMPUTE_INTERVALS(Vp0, Vp1, Vp2, Dv0, Dv1, Dv2, Dv0dv1, Dv0dv2, Isect1[0],
    Isect1[1]);

  // * compute interval for triangle 2 */
  COMPUTE_INTERVALS(Up0, Up1, Up2, Du0, Du1, Du2, Du0du1, Du0du2, Isect2[0],
    Isect2[1]);

  SORT(Isect1[0], Isect1[1]);
  SORT(Isect2[0], Isect2[1]);

  if (Isect1[1] < Isect2[0]) or (Isect2[1] < Isect1[0]) then
    Result := 0
  else
    Result := 1;
end;

// ------------------
// ------------------ TGLOctree ------------------
// ------------------

const
  MIN = 0;
  MID = 1;
  MAX = 2;
  POINT = 0;
  TRIANGLE = 1;
  TOPFACE = 0;
  BOTTOMFACE = 1;
  LEFTFACE = 2;
  RIGHTFACE = 3;
  FRONTFACE = 4;
  BACKFACE = 5;
  TOPLEFT = 0;
  TOPRIGHT = 1;
  BOTTOMLEFT = 2;
  BOTTOMRIGHT = 3;

  // Theory on FlagMax and FlagMin:
  // When a node is subdivided, each of the 8 children assumes 1/8th ownership of its
  // parent's bounding box (defined by parent extents).  Calculating a child's min/max
  // extent only requires 3 values: the parent's min extent, the parent's max extent
  // and the midpoint of the parent's extents (since the cube is divided in half twice).
  // The following arrays assume that the children are numbered from 0 to 7, named Upper
  // and Lower (Upper = top 4 cubes on Y axis, Bottom = lower 4 cubes), Left and Right, and
  // Fore and Back (Fore facing furthest away from you the viewer).
  // Each node can use its corresponding element in the array to flag the operation needed
  // to find its new min/max extent.  Note that min, mid and max refer to an array of
  // 3 coordinates (x,y,z); each of which are flagged separately. Also note that these
  // flags are based on the Y vector being the up vector.
const
  FlagMax: array [0 .. 7] of array [0 .. 2] of Byte = ((MID, MAX, MAX),
    // Upper Fore Left
    (MAX, MAX, MAX), // Upper Fore Right
    (MID, MAX, MID), // Upper Back Left
    (MAX, MAX, MID), // Upper Back Right

    (MID, MID, MAX), // Lower Fore Left   (similar to above except height/2)
    (MAX, MID, MAX), // Lower Fore Right
    (MID, MID, MID), // Lower Back Left
    (MAX, MID, MID) // Lower Back Right
    );

  FlagMin: array [0 .. 7] of array [0 .. 2] of Byte = ((MIN, MID, MID),
    // Upper Fore Left
    (MID, MID, MID), // Upper Fore Right
    (MIN, MID, MIN), // Upper Back Left
    (MID, MID, MIN), // Upper Back Right

    (MIN, MIN, MID), // Lower Fore Left  (similar to above except height/2)
    (MID, MIN, MID), // Lower Fore Right
    (MIN, MIN, MIN), // Lower Back Left
    (MID, MIN, MIN) // Lower Back Right
    );

  // Design of the AABB faces, using similar method to above.. Note than normals are not
  // correct, but not needed for current tri-intersection test.
  // Could be removed if the tri-plane collision is replaced with a tri-box routine.
  FlagFaces: array [0 .. 23] of array [0 .. 2] of Byte = (
    // Top Face
    (MIN, MAX, MAX), // Upper left corner
    (MAX, MAX, MAX), // Upper right corner
    (MAX, MIN, MAX), // Bottom right corner
    (MIN, MIN, MAX),

    // Bottom Face
    (MIN, MAX, MIN), // Upper left corner
    (MAX, MAX, MIN), // Upper right corner
    (MAX, MIN, MIN), // Bottom right corner
    (MIN, MIN, MIN),

    // Back Face
    (MIN, MAX, MAX), // Upper left corner
    (MAX, MAX, MAX), // Upper right corner
    (MAX, MAX, MIN), // Bottom right corner
    (MIN, MAX, MIN),

    // Front Face
    (MIN, MIN, MAX), // Upper left corner
    (MAX, MIN, MAX), // Upper right corner
    (MAX, MIN, MIN), // Bottom right corner
    (MIN, MIN, MIN),

    // Left Face
    (MIN, MAX, MAX), // Upper left corner
    (MIN, MIN, MAX), // Upper right corner
    (MIN, MIN, MIN), // Bottom right corner
    (MIN, MAX, MIN),

    // Right Face
    (MAX, MIN, MAX), // Upper left corner
    (MAX, MAX, MAX), // Upper right corner
    (MAX, MAX, MIN), // Bottom right corner
    (MAX, MIN, MIN));

destructor TGLOctree.Destroy;
begin
  DisposeTree;
  inherited Destroy;
end;

procedure TGLOctree.DisposeTree;

  procedure WalkDispose(var Node: POctreeNode);
  var
    I: Integer;
  begin
    if Assigned(Node) then
    begin
      for I := 0 to 7 do
        WalkDispose(Node^.ChildArray[I]);
      Dispose(Node);
    end;
  end;

begin
  WalkDispose(RootNode);
  RootNode := nil;
  TriangleFiler.Free;
  TriangleFiler := nil;
end;

procedure TGLOctree.CreateTree(Depth: Integer);
begin
  MaxOlevel := Depth; // initialize max depth.
  Refine(Rootnode, 0);
end;

// "cuts" all the triangles in the mesh into the octree.
procedure TGLOctree.CutMesh;

  procedure AddTriangleToNodes(N: Integer);
  var
    X, K: Integer;
    P: POctreeNode;
  begin
    for X := 0 to High(ResultArray) do
    begin
      P := Resultarray[X]; // Pointer to a node.

      K := Length(P^.TriArray);
      SetLength(P^.TriArray, K + 1); // Increase array by 1.
      P^.TriArray[K] := N; // Store triangle # reference.

{$IFDEF DEBUG}
      Inc(Intersections);
{$ENDIF}
    end;
  end;

var
  N: Integer; // n = triangle # in mesh
begin
  TriCountMesh := TriangleFiler.Count div 3;
  N := 0;
  while N < TriangleFiler.Count do
  begin
    WalkTriToLeaf(RootNode, TriangleFiler.List^[N], TriangleFiler.List^[N + 1],
      TriangleFiler.List^[N + 2]);
    if ResultArray <> NIL then
    begin
      AddTriangleToNodes(N);
      Inc(TriCountOctree, 1);
    end;
    Inc(N, 3);
  end;
end;

function TGLOctree.GetMidPoint(Min, Max: Single): Single;
begin
  Result := Max / 2 + Min / 2;
  // This formula is non-quadrant specific; ie: good.
end;

function TGLOctree.GetExtent(const Flags: array of Byte; ParentNode: POctreeNode)
  : TAffineFLTVector;
var
  Emin, Emax: TAffineFLTVector;
  N: Integer;
begin
  Emin := ParentNode^.MinExtent; // Some easy to work with variables.
  Emax := ParentNode^.MaxExtent;

  for N := 0 to 2 do
  begin
    case Flags[N] of
      MIN:
        Result.V[N] := Emin.V[N];
      MID:
        Result.V[N] := GetMidPoint(Emin.V[N], Emax.V[N]);
      MAX:
        Result.V[N] := Emax.V[N];
    end;
  end;
end;

procedure TGLOctree.InitializeTree(const AWorldMinExtent, AWorldMaxExtent
  : TAffineVector; const ATriangles: TAffineVectorList;
  const ATreeDepth: Integer);
var
  N: Integer;
  Newnode: POctreeNode;
begin
  Self.WorldMinExtent := AWorldMinExtent;
  Self.WorldMaxExtent := AWorldMaxExtent;

  // set up the filer data for this mesh
  if TriangleFiler = nil then
    TriangleFiler := TAffineVectorList.Create;
  TriangleFiler.Assign(ATriangles);

  New(Newnode);
  Newnode^.MinExtent := AWorldMinExtent;
  Newnode^.MaxExtent := AWorldMaxExtent;
  Newnode^.TriArray := NIL;
  for N := 0 to 7 do
    Newnode^.ChildArray[N] := NIL;

  // Initialize work variables for new tree.
  Rootnode := Newnode; // rootnode always points to root.
  NodeCount := 0; // initialize node count

  CreateTree(ATreeDepth);
  CutMesh;
end;

procedure TGLOctree.Refine(ParentNode: POctreeNode; Level: Integer);
var
  N, X, Z: Integer;
  Pwork: array [0 .. 7] of POctreeNode;
  // Stores addresses of newly created children.
  Newnode: POctreeNode;
begin
  if Level < MaxOlevel then
  begin
    for N := 0 to 7 do
    begin // Creates 8 new children under this parent.
      Inc(NodeCount);
      New(Newnode);
      Pwork[N] := Newnode; // Creates work pointers for the next for loop.

      // Generate new extents based on parent's extents
      Newnode^.MinExtent := GetExtent(FlagMin[N], ParentNode);
      Newnode^.MaxExtent := GetExtent(FlagMax[N], ParentNode);

      Newnode^.TriArray := nil; // Initialize.

      for Z := 0 to 7 do
        Newnode^.ChildArray[Z] := nil; // initialize all child pointers to NIL

      ParentNode^.ChildArray[N] := Newnode;
      // initialize parent's child pointer to this node
    end;
    for X := 0 to 7 do // Now recursively Refine each child we just made
      Refine(Pwork[X], Level + 1);
  end; // end if
end;

procedure TGLOctree.ConvertR4(ONode: POctreeNode; const Scale: TAffineFLTVector);
var
  N: Smallint;
begin
  ScaleVector(Onode^.MinExtent, Scale);
  ScaleVector(Onode^.MaxExtent, Scale);
  if ONode^.ChildArray[0] <> NIL then
  begin // ie: if not a leaf then loop through children.
    for N := 0 to 7 do
    begin
      ConvertR4(Onode^.ChildArray[N], Scale);
    end;
  end
end;

function TGLOctree.PointInNode(const Min, Max, APoint: TAffineFLTVector): BOOLEAN;
begin
  Result := (APoint.X >= Min.X) and
    (APoint.Y >= Min.Y) and (APoint.Z >= Min.Z) and
    (APoint.X <= Max.X) and (APoint.Y <= Max.Y) and
    (APoint.Z <= Max.Z);
end;

procedure TGLOctree.WalkPointToLeaf(Onode: POctreeNode; const P: TAffineVector);
begin
  Finalize(Resultarray);
  WalkPointToLeafx(Onode, P);
end;

procedure TGLOctree.WalkPointToLeafx(Onode: POctreeNode; const P: TAffineVector);
var
  N: Integer;
begin
  if PointInNode(Onode^.MinExtent, Onode^.MaxExtent, P) then
  begin
    if Assigned(Onode^.ChildArray[0]) then
      for N := 0 to 7 do
        WalkPointToLeafx(Onode^.ChildArray[N], P)
    else
    begin
      SetLength(Resultarray, Length(Resultarray) + 1);
      Resultarray[High(Resultarray)] := Onode;
    end;
  end;
end;

function TGLOctree.SphereInNode(const MinExtent, MaxExtent: TAffineVector;
  const C: TGLVector; Radius: Single): Boolean;
// Sphere-AABB intersection by Miguel Gomez
var
  S, D: Single;
  I: Integer;
begin
  // find the square of the distance
  // from the sphere to the box
  D := 0;
  for I := 0 to 2 do
  begin
    if (C.V[I] < MinExtent.V[I]) then
    begin
      S := C.V[I] - MinExtent.V[I];
      D := D + S * S;
    end
    else if (C.V[I] > MaxExtent.V[I]) then
    begin
      S := C.V[I] - MaxExtent.V[I];
      D := D + S * S;
    end;
  end; // end for

  if D <= Radius * Radius then
    Result := TRUE
  else
    Result := FALSE;
end;

procedure TGLOctree.WalkSphereToLeaf(Onode: POctreeNode; const P: TGLVector;
  Radius: Single);
begin
  Finalize(Resultarray);
  WalkSphereToLeafx(Onode, P, Radius);
end;

procedure TGLOctree.WalkSphereToLeafx(Onode: POctreeNode; const P: TGLVector;
  Radius: Single);
var
  N: Integer;
begin
  if SphereInNode(Onode^.MinExtent, Onode^.MaxExtent, P, Radius) then
  begin
    if Assigned(Onode^.ChildArray[0]) then
      for N := 0 to 7 do
        WalkSphereToLeafx(Onode^.ChildArray[N], P, Radius)
    else
    begin
      SetLength(Resultarray, Length(Resultarray) + 1);
      Resultarray[High(Resultarray)] := Onode;
    end;
  end;
end;

// Cast a ray (point p, vector v) into the Octree (ie: ray-box intersect).
procedure TGLOctree.WalkRayToLeaf(Onode: POctreeNode; const P, V: TGLVector);
begin
  Finalize(Resultarray);

  WalkRayToLeafx(Onode, P, V);
end;

procedure TGLOctree.WalkRayToLeafx(Onode: POctreeNode; const P, V: TGLVector);
var
  N: Integer;
  Coord: TGLVector;
begin
  if HitBoundingBox(Onode^.MinExtent, Onode^.MaxExtent, P, V, Coord) then
  begin
    if Assigned(Onode^.ChildArray[0]) then
      for N := 0 to 7 do
        WalkRayToLeafx(Onode^.ChildArray[N], P, V)
    else
    begin
      SetLength(Resultarray, Length(Resultarray) + 1);
      Resultarray[High(Resultarray)] := Onode;
    end;
  end;
end;

// Check triangle intersect with any of the node's faces.
// Could be replaced with a tri-box check.
function TGLOctree.TriIntersectNode(const MinExtent, MaxExtent, V1, V2,
  V3: TAffineVector): Boolean;
var
  F0, F1, F2, F3: TAffineFLTVector;
  N, O, P: Integer;
  AFace: array [0 .. 3] of TAffineFLTVector; // A face's 4 corners.
begin
  for N := 0 to 5 do
  begin // Do all 6 faces.
    for O := 0 to 3 do
    begin // Do all 4 vertices for the face.
      for P := 0 to 2 do
      begin // Do x,y,z for each vertex.
        if FlagFaces[O + N * 4, P] = MIN then
          AFace[O].V[P] := MinExtent.V[P]
        else
          AFace[O].V[P] := MaxExtent.V[P];
      end; // end for o
    end; // end for p
    F0 := AFace[0];
    F1 := AFace[1];
    F2 := AFace[2];
    F3 := AFace[3];

    // Now check the two triangles in the face against the mesh triangle.
    if Tri_tri_intersect(V1, V2, V3, F0, F1, F2) = 1 then
      Result := TRUE
    else if Tri_tri_intersect(V1, V2, V3, F2, F3, F0) = 1 then
      Result := TRUE
    else
      Result := FALSE;
    if Result then
      Exit;

  end; // end for n
end;

procedure TGLOctree.WalkTriToLeaf(Onode: POctreeNode;
  const V1, V2, V3: TAffineFLTVector);
begin
  Finalize(Resultarray);
  WalkTriToLeafx(Onode, V1, V2, V3);
end;

procedure TGLOctree.WalkTriToLeafx(Onode: POctreeNode;
  const V1, V2, V3: TAffineFLTVector);
var
  M: Integer;
begin
  if TriIntersectNode(Onode^.MinExtent, Onode^.MaxExtent, V1, V2, V3) or
    PointInNode(Onode^.MinExtent, Onode^.MaxExtent, V1) or
    PointInNode(Onode^.MinExtent, Onode^.MaxExtent, V2) or
    PointInNode(Onode^.MinExtent, Onode^.MaxExtent, V3) then
  begin
    if Onode^.ChildArray[0] <> NIL then
      for M := 0 to 7 do
        WalkTriToLeafx(Onode^.ChildArray[M], V1, V2, V3)
    else
    begin
      SetLength(Resultarray, Length(Resultarray) + 1);
      Resultarray[High(Resultarray)] := Onode;
    end;
  end;
end;

function TGLOctree.RayCastIntersect(const RayStart, RayVector: TGLVector;
  IntersectPoint: PGLVector = nil; IntersectNormal: PGLVector = nil;
  TriangleInfo: POctreeTriangleInfo = nil): Boolean;
const
  CInitialMinD: Single = 1E40;
var
  I, T, K: Integer;
  D, MinD: Single;
  P: POctreeNode;
  IPoint, INormal: TGLVector;
begin
  WalkRayToLeaf(RootNode, RayStart, RayVector);

  if Resultarray = nil then
  begin
    Result := False;
    Exit;
  end;

  MinD := CInitialMinD;
  for I := 0 to High(Resultarray) do
  begin
    P := ResultArray[I];
    for T := 0 to High(P^.TriArray) do
    begin
      K := P^.Triarray[T];
      if RayCastTriangleIntersect(RayStart, RayVector, TriangleFiler.List^[K],
        TriangleFiler.List^[K + 1], TriangleFiler.List^[K + 2], @IPoint,
        @INormal) then
      begin
        D := VectorDistance2(RayStart, IPoint);
        if D < MinD then
        begin
          MinD := D;
          if IntersectPoint <> nil then
            IntersectPoint^ := IPoint;
          if IntersectNormal <> nil then
            IntersectNormal^ := INormal;
          if TriangleInfo <> nil then
          begin
            TriangleInfo^.Index := K;
            TriangleInfo^.Vertex[0] := TriangleFiler.List^[K];
            TriangleInfo^.Vertex[1] := TriangleFiler.List^[K + 1];
            TriangleInfo^.Vertex[2] := TriangleFiler.List^[K + 2];
          end;
        end;
      end;
    end;
  end;
  Result := (MinD <> CInitialMinD);
end;

// SphereIntersectAABB -- Walk a sphere through an Octree, given a velocity, and return the nearest polygon
// intersection point on that sphere, and its plane normal.
//
// **** This function is the result of a LOT of work and experimentation with both Paul Nettle's method
// (www.fluidstudios.com) and Telemachos' method (www.peroxide.dk) over a period of about 2 months. If
// you find ways to optimize the general structure, please let me know at rmch@cadvision.com. ****
//
// TO DO: R4 conversion (routine already exists for this) for ellipsoid space.
//
// Method for caclulating CD vs polygon: (Robert Hayes method)
// ...for each triangle:
// 1. Cast a ray from sphere origin to triangle's plane along plane's negative normal (set to length
// of sphere radius).  If no hit, skip this triangle.  Otherwise this is the default plane
// intersection point.
// 2. If the distance is =< the sphere radius, the plane is embedded in the sphere.  Go to step 6 with
// plane intersection point from above step.
// 3. If the distance > sphere radius, calculate the sphere intersection point to this plane by
// subtracting the plane's normal from the sphere's origin.
// 4. Cast a new ray from the sphere intersection point to the plane; this is the new plane
// intersection point.
// 5. Cast a ray from the sphere intersection point to the triangle.  If it is a direct hit, then
// this point is the polygon intersection point.
// 6. Else, find the point on the triangle that is closest to the plane intersection point; this becomes
// the polygon intersection point (ie: hit an edge or corner)
// 7. Cast a ray from the polygon intersection point along the negative velocity vector of the sphere
// (note: for accuracy reasons - SphereIntersect replaced with PointInSphere)
// 8. If there is no intersection, the sphere cannot possibly collide with this triangle
// 9. Else, save the distance from step 8 if, and only if, it is the shortest collision distance so far.
//
// Return the polygon intersection point and the closest triangle's normal if hit.
//
function TGLOctree.SphereSweepIntersect(const RayStart, RayVector: TGLVector;
  const Velocity, Radius: Single; IntersectPoint: PGLVector = nil;
  IntersectNormal: PGLVector = nil): Boolean;
const
  CInitialMinD2: Single = 1E40;
  CEpsilon: Single = 0.05;

var
  I, T, K: Integer;
  MinD2, Sd2, Radius2: Single;
  DistanceToTravel, DistanceToTravelMinusRadius2: Single;
  P: POctreeNode;
  PNormal: TAffineVector;
  PNormal4: TGLVector;
  NEGpNormal4: TGLVector;
  SIPoint, SIPoint2: TGLVector; // sphere intersection point
  PIPoint: TGLVector; // plane intersection point
  PolyIPoint: TGLVector; // polygon intersection point
  NEGVelocity: TGLVector; // sphere's forward velocity
  DirectHit: Boolean;

  P1, P2, P3: PAffineVector;

  // SphereSweepAABB:TAABB;

  // response identifiers (for future use)
  // destinationPoint, newdestinationPoint: TGLVector;
  // slidePlaneOrigin, slidePlaneNormal: TGLVector;
  // newvelocityVector: TGLVector;
  // v: single;
  // L: double;
begin
  // Note: Current implementation only accurate for FreeForm:Radius at 1:1 ratio.

  Result := False; // default: no collision

  // quit if no movement
  if (Velocity = 0) or (not(VectorNorm(RayVector) > 0)) then
    Exit;
  // How far ahead to check for collisions.
  DistanceToTravel := Velocity + Radius + CEpsilon;
  DistanceToTravelMinusRadius2 := Sqr(Velocity + CEpsilon);
  Radius2 := Sqr(Radius);

  // Determine all the octree nodes this sphere intersects with.
  // NOTE: would be more efficient to find the bounding box that includes the
  // startpoint and endpoint (+sphere diameter)... especially with a large sphere
  // and/or a large velocity.
  WalkSphereToLeaf(RootNode, RayStart, DistanceToTravel);

  // This method may be more effective if sphere sweeps from one point to another and stops.
  // NOTE: If it recursively slides of planes, then WalkSphereToLeaf would probably be better, as
  // it will determine all possible triangles that could intersect over the whole motion
  // AABBFromSweep(SphereSweepAABB,rayStart,destinationPoint,Radius+cEpsilon);
  // GetTrianglesFromNodesIntersectingAABB(SphereSweepAABB);

  if not Assigned(Resultarray) then
    Exit;

  // Negative velocity vector for use with ray-sphere check.
  VectorScale(RayVector, -Velocity / VectorLength(RayVector), NEGVelocity);

  MinD2 := CInitialMinD2;
  for I := 0 to High(Resultarray) do
  begin
    P := ResultArray[I];
    for T := 0 to High(P^.TriArray) do
    begin
      K := P^.Triarray[T];
      // These are the vertices of the triangle to check
      P1 := @Trianglefiler.List[K];
      P2 := @Trianglefiler.List[K + 1];
      P3 := @Trianglefiler.List[K + 2];

      // Creates the normal for this triangle
      PNormal := CalcPlaneNormal(P1^, P2^, P3^);

      // Ignore backfacing planes
      if VectorDotProduct(PNormal, PAffineVector(@RayVector)^) > 0.0 then
        Continue;

      // Set the normal to the radius of the sphere
      ScaleVector(PNormal, Radius);
      // Make some TVectors
      MakeVector(PNormal4, PNormal);
      NEGpNormal4 := VectorNegate(PNormal4);

      // Calculate the plane intersection point (sphere origin to plane)
      if RayCastPlaneIntersect(RayStart, NEGpNormal4, VectorMake(P1^), PNormal4,
        @PIPoint) then
      begin
        DirectHit := False;
        Sd2 := VectorDistance2(RayStart, PIPoint);

        // If sd <= radius, fall through to "not direct hit" code below with pIPoint
        // as the plane intersection point.  Sphere is embedded.
        // Otherwise...
        if Sd2 > Radius2 then
        begin
          // Calculate sphere intersection point (ie: point on sphere that hits plane)
          SetVector(SIPoint, VectorSubtract(RayStart, PNormal4));
          // Get new plane intersection point (in case not a direct hit)
          RayCastPlaneIntersect(SIPoint, RayVector, VectorMake(P1^), PNormal4,
            @PIPoint);
          // Does the velocity vector directly hit the triangle? If yes then that is the
          // polygon intersection point.
          if RayCastTriangleIntersect(SIPoint, RayVector, P1^, P2^, P3^,
            @PolyIPoint, @PNormal4) then
          begin
            Sd2 := VectorDistance2(SIPoint, PolyIPoint);
            DirectHit := True;
          end;
        end;

        // If not a direct hit then check if sphere "nicks" the triangle's edge or corner...
        // If it does then that is the polygon intersection point.
        if not DirectHit then
        begin
          if not CheckPointInTriangle(AffineVectorMake(PiPoint), P1^, P2^, P3^)
          then
            SetVector(PolyIPoint, ClosestPointOnTriangleEdge(P1^, P2^, P3^,
              PAffineVector(@PIPoint)^), 1)
          else
            PolyIPoint := PiPoint;

          // See if this point + negative velocity vector lies within the sphere.
          // (This implementation seems more accurate than RayCastSphereIntersect)
          { if not CheckPointInSphere(VectorAdd(polyIPoint, NEGVelocity), raystart, radius) then
            continue;
            // sd2:=0;  //stops the test too soon (noticable on triangle edges in flat planes)
            sd2:=sqr(VectorDistance(raystart, polyIPoint)-Radius);
          }
          // see if this point + negative velocity vector intersect the sphere.
          // (PointInSphere doesn't work for fast motion)
          if VectorDistance2(PolyIPoint, RayStart) > Radius2 then
          begin
            if RayCastSphereIntersect(PolyIPoint, VectorNormalize(NEGVelocity),
              RayStart, Radius, SIPoint, SIPoint2) = 0 then
              Continue;
            Sd2 := VectorDistance2(SIPoint, PolyIPoint);
          end
          else
            Sd2 := 0;
        end;

        // Allow sphere to get close to triangle (less epsilon which is built into distanceToTravel)
        if Sd2 <= DistanceToTravelMinusRadius2 then
        begin
          Result := True; // flag a collision
          if Sd2 < MinD2 then
          begin
            MinD2 := Sd2;
            if IntersectPoint <> nil then
              IntersectPoint^ := PolyIPoint;
            if IntersectNormal <> nil then
              IntersectNormal^ := PNormal4;
            if Sd2 = 0 then
              Exit;
          end;
        end;
      end;
    end; // end for t triangles
  end; // end for i nodes
end;

function TGLOctree.TriangleIntersect(const V1, V2, V3: TAffineVector): Boolean;
var
  I, T, K: Integer;
  P: POctreeNode;
  P1, P2, P3: PAffineVector;
begin
  Result := False; // default: no collision
  WalkTriToLeaf(RootNode, V1, V2, V3);
  if not Assigned(Resultarray) then
    Exit;

  for I := 0 to High(Resultarray) do
  begin
    P := ResultArray[I];
    for T := 0 to High(P^.TriArray) do
    begin
      K := P^.Triarray[T];
      // These are the vertices of the triangle to check
      P1 := @Trianglefiler.List[K];
      P2 := @Trianglefiler.List[K + 1];
      P3 := @Trianglefiler.List[K + 2];
      if Tri_tri_intersect(V1, V2, V3, P1^, P2^, P3^) <> 0 then
      begin
        Result := True;
        Exit;
      end;
    end; // end for t triangles
  end; // end for i nodes
end;

function TGLOctree.AABBIntersect(const AABB: TAABB; const M1to2, M2to1: TGLMatrix;
  Triangles: TAffineVectorList = nil): Boolean;
var
  TriList: TAffineVectorList;
  I: Integer;
begin
  // get triangles in nodes intersected by the aabb
  TriList := GetTrianglesFromNodesIntersectingCube(Aabb, M1to2, M2to1);

  Result := False;
  if Trilist.Count > 0 then
  begin
    Trilist.TransformAsPoints(M2to1);
    I := 0;
    // run all faces and check if they're inside the aabb
    // uncoment the * and comment the {//} to check only vertices
    { // } while I < TriList.Count - 1 do
    begin
      // *for i:= 0 to triList.count -1 do begin
      // *  v:=VectorMake(TriList.Items[i]);
      // *  if pointInAABB(AffinevectorMake(v), aabb) then
      { // } if TriangleIntersectAABB(Aabb, TriList[I], TriList[I + 1],
        Trilist[I + 2]) then
      begin
        Result := True;
        if not Assigned(Triangles) then
          Break
        else
          Triangles.Add(TriList[I], TriList[I + 1], Trilist[I + 2]);
      end;
      { // } I := I + 3;
    end;
  end;

  TriList.Free;
end;

function TGLOctree.GetTrianglesFromNodesIntersectingAABB(const ObjAABB: TAABB)
  : TAffineVectorList;
var
  AABB1: TAABB;

  procedure HandleNode(Onode: POctreeNode);
  var
    AABB2: TAABB;
    I: Integer;
  begin
    AABB2.Min := Onode^.MinExtent;
    AABB2.Max := Onode^.MaxExtent;

    if IntersectAABBsAbsolute(AABB1, AABB2) then
    begin
      if Assigned(Onode^.ChildArray[0]) then
      begin
        for I := 0 to 7 do
          HandleNode(Onode^.ChildArray[I])
      end
      else
      begin
        SetLength(ResultArray, Length(ResultArray) + 1);
        ResultArray[High(ResultArray)] := Onode;
      end;
    end;
  end;

var
  I, K: Integer;
  P: POctreeNode;
  TriangleIndices: TIntegerList;

begin
  // Calc AABBs
  AABB1 := ObjAABB;

  SetLength(ResultArray, 0);
  if Assigned(RootNode) then
    HandleNode(RootNode);

  Result := TAffineVectorList.Create;
  TriangleIndices := TIntegerList.Create;
  try
    // fill the triangles from all nodes in the resultarray to AL
    for I := 0 to High(ResultArray) do
    begin
      P := ResultArray[I];
      TriangleIndices.AddIntegers(P^.TriArray);
    end;
    TriangleIndices.SortAndRemoveDuplicates;
    Result.Capacity := TriangleIndices.Count * 3;
    for I := 0 to TriangleIndices.Count - 1 do
    begin
      K := TriangleIndices[I];
      Result.Add(TriangleFiler.List^[K], TriangleFiler.List^[K + 1],
        TriangleFiler.List^[K + 2]);
    end;
  finally
    TriangleIndices.Free;
  end;

end;

function TGLOctree.GetTrianglesFromNodesIntersectingCube(const ObjAABB: TAABB;
  const ObjToSelf, SelfToObj: TGLMatrix): TAffineVectorList;
var
  AABB1: TAABB;
  M1To2, M2To1: TGLMatrix;

  procedure HandleNode(Onode: POctreeNode);
  var
    AABB2: TAABB;
    I: Integer;
  begin
    AABB2.Min := Onode^.MinExtent;
    AABB2.Max := Onode^.MaxExtent;

    if IntersectAABBs(AABB1, AABB2, M1To2, M2To1) then
    begin
      if Assigned(Onode^.ChildArray[0]) then
      begin
        for I := 0 to 7 do
          HandleNode(Onode^.ChildArray[I])
      end
      else
      begin
        SetLength(ResultArray, Length(ResultArray) + 1);
        ResultArray[High(ResultArray)] := Onode;
      end;
    end;
  end;

var
  I, K: Integer;
  P: POctreeNode;
  TriangleIndices: TIntegerList;
begin
  // Calc AABBs
  AABB1 := ObjAABB;
  // Calc Conversion Matrixes
  M1To2 := ObjToSelf;
  M2To1 := SelfToObj;

  SetLength(ResultArray, 0);
  if Assigned(RootNode) then
    HandleNode(RootNode);

  Result := TAffineVectorList.Create;
  TriangleIndices := TIntegerList.Create;
  try
    // fill the triangles from all nodes in the resultarray to AL
    for I := 0 to High(ResultArray) do
    begin
      P := ResultArray[I];
      TriangleIndices.AddIntegers(P^.TriArray);
    end;
    TriangleIndices.SortAndRemoveDuplicates;
    Result.Capacity := TriangleIndices.Count * 3;
    for I := 0 to TriangleIndices.Count - 1 do
    begin
      K := TriangleIndices[I];
      Result.Add(TriangleFiler.List^[K], TriangleFiler.List^[K + 1],
        TriangleFiler.List^[K + 2]);
    end;
  finally
    TriangleIndices.Free;
  end;
end;

end.
