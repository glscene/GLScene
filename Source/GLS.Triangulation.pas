//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Triangulation;

(*
  Classes and methods for triangulation of scatter points.
*)

interface

uses
  System.Classes,
  System.Types,
  Vcl.Dialogs,

  GLS.VectorGeometry;

// Set these as applicable
const
  MaxVertices = 500000;
  MaxTriangles = 1000000;
  ExPtTolerance = 0.000001;

type
  TDProgressEvent = procedure(State: string; Pos, Max: Integer;
    AlwaysUpdate: Boolean = False) of object;

  // Points (Vertices)
type
  DVertex = record
    X: Single;
    Y: Single;
    Z: Single; // Added to save height of terrain.
    U: Single;
    V: Single;
    MatIndex: Integer;
  end;

  // Created Triangles, vv# are the vertex pointers
type
  DTriangle = record
    vv0: LongInt;
    vv1: LongInt;
    vv2: LongInt;
    PreCalc: Integer;
    Xc, Yc, R: Single;
  end;

type
  TDVertex = array of DVertex;
  TDTriangle = array of DTriangle;
  TDComplete = array of Boolean;
  TDEdges = array of array of LongInt;

type
(*
  TGLDelaunay2D is a class for Delaunay triangulation of arbitrary points
  Credit to Paul Bourke (http://paulbourke.net/) for the original Fortran 77 Program :))
  Conversion to Visual Basic by EluZioN (EluZioN@casesladder.com)
  Conversion from VB to Delphi6 by Dr Steve Evans (steve@lociuk.com)
  June 2002 Update by Dr Steve Evans: Heap memory allocation
  added to prevent stack overflow when MaxVertices and MaxTriangles are very large.
  Additional Updates in June 2002: Bug in InCircle function fixed. Radius r := Sqrt(rsqr).
  Check for duplicate points added when inserting new point.
  For speed, all points pre-sorted in x direction using quicksort algorithm and
  triangles flagged when no longer needed. The circumcircle centre and radius of
  the triangles are now stored to improve calculation time.
  You can use this code however you like providing the above credits remain in tact
*)
  TGLDelaunay2D = class
  private
    function InCircle(Xp, Yp, X1, Y1, X2, Y2, X3, Y3: Single; out Xc,Yc, R:
      Single; j: Integer): Boolean;
   (* Takes as input NVERT vertices in arrays Vertex()
      Returned is a list of NTRI triangular faces in the array
      Triangle(). These triangles are arranged in clockwise order.*)
    function Triangulate(nvert: Integer): Integer;
  public
    Vertex: TDVertex;
    Triangle: TDTriangle;
    HowMany: Integer;
    TPoints: Integer; //< Total number of points (vertices)
    OnProgress: TDProgressEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Mesh(sort: Boolean);
    procedure AddPoint(X, Y, Z, U, V: Single; MatIndex: Integer);
    procedure AddPointNoCheck(X, Y, Z, U, V: Single; MatIndex: Integer);
    procedure RemoveLastPoint;
    procedure QuickSort(var A: TDVertex; Low, High: Integer);
  end;

//------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------

constructor TGLDelaunay2D.Create;
begin
  // Initiate total points to 1, using base 0 causes problems in the functions
  inherited;
  TPoints := 1;
  HowMany := 0;
  SetLength(Vertex, MaxVertices);
  SetLength(Triangle, MaxTriangles);
  OnProgress := nil;
end;

destructor TGLDelaunay2D.Destroy;
begin
  SetLength(Vertex, 0);
  SetLength(Triangle, 0);
  inherited;
end;

function TGLDelaunay2D.InCircle(Xp, Yp, X1, Y1, X2, Y2, X3, Y3: Single;
  out Xc, Yc, R: Single; j: Integer): Boolean;
// Return TRUE if the point (xp,yp) lies inside the circumcircle
// made up by points (x1,y1) (x2,y2) (x3,y3)
// The circumcircle centre is returned in (xc,yc) and the radius r
// NOTE: A point on the edge is inside the circumcircle
var
  eps: Single;
  m1: Single;
  m2: Single;
  mx1: Single;
  mx2: Single;
  my1: Single;
  my2: Single;
  dx: Single;
  dy: Single;
  rsqr: Single;
  drsqr: Single;
begin
  eps := 0.000001;
  InCircle := False;
  // Check if xc,yc and r have already been calculated
  if Triangle[j].PreCalc = 1 then
  begin
    Xc := Triangle[j].Xc;
    Yc := Triangle[j].Yc;
    R := Triangle[j].R;
    rsqr := R * R;
    dx := Xp - Xc;
    dy := Yp - Yc;
    drsqr := dx * dx + dy * dy;
  end
  else
  begin
    if (Abs(Y1 - Y2) < eps) and (Abs(Y2 - Y3) < eps) then
    begin
      ShowMessage('INCIRCUM - F - Points are coincident !!');
      Exit;
    end;
    if Abs(Y2 - Y1) < eps then
    begin
      m2 := -(X3 - X2) / (Y3 - Y2);
      mx2 := (X2 + X3) / 2;
      my2 := (Y2 + Y3) / 2;
      Xc := (X2 + X1) / 2;
      Yc := m2 * (Xc - mx2) + my2;
    end
    else if Abs(Y3 - Y2) < eps then
    begin
      m1 := -(X2 - X1) / (Y2 - Y1);
      mx1 := (X1 + X2) / 2;
      my1 := (Y1 + Y2) / 2;
      Xc := (X3 + X2) / 2;
      Yc := m1 * (Xc - mx1) + my1;
    end
    else
    begin
      m1 := -(X2 - X1) / (Y2 - Y1);
      m2 := -(X3 - X2) / (Y3 - Y2);
      mx1 := (X1 + X2) / 2;
      mx2 := (X2 + X3) / 2;
      my1 := (Y1 + Y2) / 2;
      my2 := (Y2 + Y3) / 2;
      if (m1 - m2) <> 0 then // se
      begin
        Xc := (m1 * mx1 - m2 * mx2 + my2 - my1) / (m1 - m2);
        Yc := m1 * (Xc - mx1) + my1;
      end
      else
      begin
        Xc := (X1 + X2 + X3) / 3;
        Yc := (Y1 + Y2 + Y3) / 3;
      end;
    end;
    dx := X2 - Xc;
    dy := Y2 - Yc;
    rsqr := dx * dx + dy * dy;
    R := Sqrt(rsqr);
    dx := Xp - Xc;
    dy := Yp - Yc;
    drsqr := dx * dx + dy * dy;

    // store the xc,yc and r for later use
    Triangle[j].PreCalc := 1;
    Triangle[j].Xc := Xc;
    Triangle[j].Yc := Yc;
    Triangle[j].R := R;
  end;

  if drsqr <= rsqr then
    InCircle := True;
end;

function TGLDelaunay2D.Triangulate(nvert: Integer): Integer;
var
  Complete: TDComplete;
  Edges: TDEdges;
  Nedge: LongInt;
  // For Super Triangle
  xmin: Single;
  xmax: Single;
  ymin: Single;
  ymax: Single;
  xmid: Single;
  ymid: Single;
  dx: Single;
  dy: Single;
  dmax: Single;

  // General Variables
  i: Integer;
  j: Integer;
  k: Integer;
  ntri: Integer;
  Xc: Single;
  Yc: Single;
  R: Single;
  inc: Boolean;
begin
  // Allocate memory
  SetLength(Complete, MaxTriangles);
  SetLength(Edges, 2, MaxTriangles * 3);

  // Find the maximum and minimum vertex bounds.
  // This is to allow calculation of the bounding triangle
  xmin := Vertex[1].X;
  ymin := Vertex[1].Y;
  xmax := xmin;
  ymax := ymin;
  for i := 2 to nvert do
  begin
    if Vertex[i].X < xmin then
      xmin := Vertex[i].X;
    if Vertex[i].X > xmax then
      xmax := Vertex[i].X;
    if Vertex[i].Y < ymin then
      ymin := Vertex[i].Y;
    if Vertex[i].Y > ymax then
      ymax := Vertex[i].Y;
  end;

  dx := xmax - xmin;
  dy := ymax - ymin;
  if dx > dy then
    dmax := dx
  else
    dmax := dy;

  xmid := Trunc((xmax + xmin) / 2);
  ymid := Trunc((ymax + ymin) / 2);

  // Set up the supertriangle
  // This is a triangle which encompasses all the sample points.
  // The supertriangle coordinates are added to the end of the
  // vertex list. The supertriangle is the first triangle in
  // the triangle list.

  Vertex[nvert + 1].X := (xmid - 2 * dmax);
  Vertex[nvert + 1].Y := (ymid - dmax);
  Vertex[nvert + 2].X := xmid;
  Vertex[nvert + 2].Y := (ymid + 2 * dmax);
  Vertex[nvert + 3].X := (xmid + 2 * dmax);
  Vertex[nvert + 3].Y := (ymid - dmax);
  Triangle[1].vv0 := nvert + 1;
  Triangle[1].vv1 := nvert + 2;
  Triangle[1].vv2 := nvert + 3;
  Triangle[1].PreCalc := 0;

  Complete[1] := False;
  ntri := 1;

  // Include each point one at a time into the existing mesh
  for i := 1 to nvert do
  begin
    if Assigned(OnProgress) then
      OnProgress('Delaunay triangulation', i - 1, nvert);
    Nedge := 0;
    // Set up the edge buffer.
    // If the point (Vertex(i).x,Vertex(i).y) lies inside the circumcircle then the
    // three edges of that triangle are added to the edge buffer.
    j := 0;
    repeat
      j := j + 1;
      if Complete[j] <> True then
      begin
        inc := InCircle(Vertex[i].X, Vertex[i].Y, Vertex[Triangle[j].vv0].X,
          Vertex[Triangle[j].vv0].Y, Vertex[Triangle[j].vv1].X,
          Vertex[Triangle[j].vv1].Y, Vertex[Triangle[j].vv2].X,
          Vertex[Triangle[j].vv2].Y, Xc, Yc, R, j);
        // Include this if points are sorted by X
        if { usingsort and } ((Xc + R) < Vertex[i].X) then //
          Complete[j] := True //
        else //
          if inc then
          begin
            Edges[0, Nedge + 1] := Triangle[j].vv0;
            Edges[1, Nedge + 1] := Triangle[j].vv1;
            Edges[0, Nedge + 2] := Triangle[j].vv1;
            Edges[1, Nedge + 2] := Triangle[j].vv2;
            Edges[0, Nedge + 3] := Triangle[j].vv2;
            Edges[1, Nedge + 3] := Triangle[j].vv0;
            Nedge := Nedge + 3;
            Triangle[j].vv0 := Triangle[ntri].vv0;
            Triangle[j].vv1 := Triangle[ntri].vv1;
            Triangle[j].vv2 := Triangle[ntri].vv2;
            Triangle[j].PreCalc := Triangle[ntri].PreCalc;
            Triangle[j].Xc := Triangle[ntri].Xc;
            Triangle[j].Yc := Triangle[ntri].Yc;
            Triangle[j].R := Triangle[ntri].R;
            Triangle[ntri].PreCalc := 0;
            Complete[j] := Complete[ntri];
            j := j - 1;
            ntri := ntri - 1;
          end;
      end;
    until j >= ntri;
    // Tag multiple edges
    // Note: if all triangles are specified anticlockwise then all
    // interior edges are opposite pointing in direction.
    for j := 1 to Nedge - 1 do
    begin
      if not(Edges[0, j] = 0) and not(Edges[1, j] = 0) then
      begin
        for k := j + 1 to Nedge do
        begin
          if not(Edges[0, k] = 0) and not(Edges[1, k] = 0) then
          begin
            if Edges[0, j] = Edges[1, k] then
            begin
              if Edges[1, j] = Edges[0, k] then
              begin
                Edges[0, j] := 0;
                Edges[1, j] := 0;
                Edges[0, k] := 0;
                Edges[1, k] := 0;
              end;
            end;
          end;
        end;
      end;
    end;
    // Form new triangles for the current point
    // Skipping over any tagged edges.
    // All edges are arranged in clockwise order.
    for j := 1 to Nedge do
    begin
      if not(Edges[0, j] = 0) and not(Edges[1, j] = 0) then
      begin
        ntri := ntri + 1;
        Triangle[ntri].vv0 := Edges[0, j];
        Triangle[ntri].vv1 := Edges[1, j];
        Triangle[ntri].vv2 := i;
        Triangle[ntri].PreCalc := 0;
        Complete[ntri] := False;
      end;
    end;
  end;
  // Remove triangles with supertriangle vertices
  // These are triangles which have a vertex number greater than NVERT
  i := 0;
  repeat
    i := i + 1;
    if (Triangle[i].vv0 > nvert) or (Triangle[i].vv1 > nvert) or
      (Triangle[i].vv2 > nvert) then
    begin
      Triangle[i].vv0 := Triangle[ntri].vv0;
      Triangle[i].vv1 := Triangle[ntri].vv1;
      Triangle[i].vv2 := Triangle[ntri].vv2;
      i := i - 1;
      ntri := ntri - 1;
    end;
  until i >= ntri;
  Triangulate := ntri;

  // Free memory
  SetLength(Complete, 0);
  SetLength(Edges, 2, 0);
end;

procedure TGLDelaunay2D.Mesh(sort: Boolean);
begin
  if sort then
    QuickSort(Vertex, 1, tPoints - 1);
  (* usingsort:=sort; *)
  if tPoints > 3 then
    HowMany := Triangulate(tPoints - 1);
  // 'Returns number of triangles created.
end;

procedure TGLDelaunay2D.AddPoint(X, Y, Z, U, V: Single; MatIndex: Integer);
var
  i, AE: Integer;
begin
  // Check for duplicate points
  AE := 0;
  i := 1;
  while i < tPoints do
  begin
    if (Abs(X - Vertex[i].X) < ExPtTolerance) and
      (Abs(Y - Vertex[i].Y) < ExPtTolerance) then
      AE := 1;
    inc(i);
  end;
  if AE = 0 then
  begin
    // Set Vertex coordinates where you clicked the pic box
    Vertex[tPoints].X := X;
    Vertex[tPoints].Y := Y;
    Vertex[tPoints].Z := Z;
    Vertex[tPoints].U := U;
    Vertex[tPoints].V := V;
    Vertex[tPoints].MatIndex := MatIndex;
    // Increment the total number of points
    tPoints := tPoints + 1;
  end;
end;

procedure TGLDelaunay2D.AddPointNoCheck(X, Y, Z, U, V: Single; MatIndex: Integer);
begin
  Vertex[tPoints].X := X;
  Vertex[tPoints].Y := Y;
  Vertex[tPoints].Z := Z;
  Vertex[tPoints].U := U;
  Vertex[tPoints].V := V;
  Vertex[tPoints].MatIndex := MatIndex;
  tPoints := tPoints + 1;
end;

procedure TGLDelaunay2D.RemoveLastPoint;
begin
  tPoints := tPoints - 1;
end;

procedure TGLDelaunay2D.QuickSort(var A: TDVertex; Low, High: Integer);
// Sort all points by x
{sub}procedure DoQuickSort(var A: TDVertex; iLo, iHi: Integer);
var
  Lo, Hi: Integer;
  Mid: Single;
  T: DVertex;
begin
  Lo := iLo;
  Hi := iHi;
  Mid := A[(Lo + Hi) div 2].X;
  repeat
    while A[Lo].X < Mid do
      inc(Lo);
    while A[Hi].X > Mid do
      Dec(Hi);
    if Lo <= Hi then
    begin
      T := A[Lo];
      A[Lo] := A[Hi];
      A[Hi] := T;
      inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;
  if Hi > iLo then
    DoQuickSort(A, iLo, Hi);
  if Lo < iHi then
    DoQuickSort(A, Lo, iHi);
end;

begin
  DoQuickSort(A, Low, High);
end;

end.
