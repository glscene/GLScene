//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.CurvesAndSurfaces;

(* Bezier and B-Spline Curve and Surface Routines *)

interface

{$I GLScene.inc}

uses
  System.SysUtils,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorLists;

type
  TBSplineContinuity = (bscUniformNonPeriodic, bscUniformPeriodic);

function BezierCurvePoint(t: single; n: integer; cp: PAffineVectorArray): TAffineVector;
function BezierSurfacePoint(s, t: single; m, n: integer; cp: PAffineVectorArray): TAffineVector;
procedure GenerateBezierCurve(Steps: integer;  ControlPoints, Vertices: TAffineVectorList);
procedure GenerateBezierSurface(Steps, Width, Height: integer; ControlPoints, Vertices: TAffineVectorList);

function BSplinePoint(t: single; n, k: integer; knots: PSingleArray;
  cp: PAffineVectorArray): TAffineVector;
function BSplineSurfacePoint(s, t: single; m, n, k1, k2: integer;
  uknots, vknots: PSingleArray; cp: PAffineVectorArray): TAffineVector;
procedure GenerateBSpline(Steps, Order: integer; KnotVector: TSingleList;
  ControlPoints, Vertices: TAffineVectorList);
procedure GenerateBSplineSurface(Steps, UOrder, VOrder, Width, Height: integer;
  UKnotVector, VKnotVector: TSingleList;
  ControlPoints, Vertices: TAffineVectorList);
procedure GenerateKnotVector(KnotVector: TSingleList;
  NumberOfPoints, Order: integer; Continuity: TBSplineContinuity);

// --------------------------------------------------------------------------
implementation
// --------------------------------------------------------------------------

function Factorial(n: integer): single;
var
  i: integer;
begin
  if (n < 0) or (n > 32) then
    Exception.Create('Invalid factorial parameter: n = ' + IntToStr(n));

  Result := 1;
  for i := 2 to n do
    Result := Result * i;
end;

// ------------------------------------------------------------
// Bezier routines
// ------------------------------------------------------------

function BernsteinBasis(n, i: integer; t: single): single;
var
  ti, tni: single;
begin
  if (t = 0) and (i = 0) then
    ti := 1
  else
    ti := PowerInteger(t, i);
  if (n = i) and (t = 1) then
    tni := 1
  else
    tni := PowerInteger(1 - t, integer(n - i));
  Result := (Factorial(n) / (Factorial(i) * Factorial(n - i))) * ti * tni;
end;

function BezierCurvePoint(t: single; n: integer; cp: PAffineVectorArray)
  : TAffineVector;
var
  i: integer;
  b: single;
begin
  Result := NullVector;
  for i := 0 to n - 1 do
  begin
    b := BernsteinBasis(n - 1, i, t);
    Result.X := Result.X + cp[i].X * b;
    Result.Y := Result.Y + cp[i].Y * b;
    Result.Z := Result.Z + cp[i].Z * b;
  end;
end;

function BezierSurfacePoint(s, t: single; m, n: integer; cp: PAffineVectorArray)
  : TAffineVector;
var
  i, j: integer;
  b1, b2: single;
begin
  Result := NullVector;
  for j := 0 to n - 1 do
    for i := 0 to m - 1 do
    begin
      b1 := BernsteinBasis(m - 1, i, s);
      b2 := BernsteinBasis(n - 1, j, t);
      Result.X := Result.X + cp[j * m + i].X * b1 * b2;
      Result.Y := Result.Y + cp[j * m + i].Y * b1 * b2;
      Result.Z := Result.Z + cp[j * m + i].Z * b1 * b2;
    end;
end;

procedure GenerateBezierCurve(Steps: integer;
  ControlPoints, Vertices: TAffineVectorList);
var
  i: integer;
begin
  Vertices.Count := Steps;
  for i := 0 to Steps - 1 do
    Vertices[i] := BezierCurvePoint(i / (Steps - 1), ControlPoints.Count,
      ControlPoints.List);
end;

procedure GenerateBezierSurface(Steps, Width, Height: integer;
  ControlPoints, Vertices: TAffineVectorList);
var
  i, j: integer;
begin
  Vertices.Count := Steps * Steps;
  for j := 0 to Steps - 1 do
    for i := 0 to Steps - 1 do
      Vertices[i + j * Steps] := BezierSurfacePoint(i / (Steps - 1),
        j / (Steps - 1), Width, Height, ControlPoints.List);
end;

// ------------------------------------------------------------
// B-Spline routines
// ------------------------------------------------------------

function BSplineBasis(i, k, n: integer; u: single; knots: PSingleArray): single;
var
  v1, v2: single;
begin
  if (u < knots[i]) or (u > knots[i + k]) then
  begin
    Result := 0;
  end
  else if k = 1 then
  begin
    Result := 0;
    if (u >= knots[i]) and (u < knots[i + 1]) then
      Result := 1;
  end
  else if (i = n - 1) and (u = knots[i + k]) then
  begin
    Result := 1;
  end
  else
  begin
    v1 := (knots[i + k - 1] - knots[i]);
    v2 := (knots[i + k] - knots[i + 1]);
    if v1 <> 0 then
      v1 := (u - knots[i]) / v1 * BSplineBasis(i, k - 1, n, u, knots);
    if v2 <> 0 then
      v2 := (knots[i + k] - u) / v2 * BSplineBasis(i + 1, k - 1, n, u, knots);
    Result := v1 + v2;
  end;
end;

function BSplinePoint(t: single; n, k: integer; knots: PSingleArray;
  cp: PAffineVectorArray): TAffineVector;
var
  i: integer;
  b: array of single;
  det: single;
begin
  SetLength(b, n);
  for i := 0 to n - 1 do
    b[i] := BSplineBasis(i, k, n, t, knots);
  det := 0;
  for i := 0 to n - 1 do
    det := det + b[i];
  Result := NullVector;
  for i := 0 to n - 1 do
  begin
    if det <> 0 then
      b[i] := b[i] / det
    else
      b[i] := 0;
    Result.X := Result.X + cp[i].X * b[i];
    Result.Y := Result.Y + cp[i].Y * b[i];
    Result.Z := Result.Z + cp[i].Z * b[i];
  end;
  SetLength(b, 0);
end;

function BSplineSurfacePoint(s, t: single; m, n, k1, k2: integer;
  uknots, vknots: PSingleArray; cp: PAffineVectorArray): TAffineVector;
var
  i, j: integer;
  b1, b2: array of single;
  det1, det2: single;
begin
  SetLength(b1, m);
  SetLength(b2, n);
  det1 := 0;
  det2 := 0;
  for i := 0 to m - 1 do
    b1[i] := BSplineBasis(i, k1, m, s, uknots);
  for i := 0 to n - 1 do
    b2[i] := BSplineBasis(i, k2, n, t, vknots);
  for i := 0 to m - 1 do
    det1 := det1 + b1[i];
  for i := 0 to n - 1 do
    det2 := det2 + b2[i];
  Result := NullVector;
  for j := 0 to n - 1 do
  begin
    if det2 <> 0 then
      b2[j] := b2[j] / det2
    else
      b2[j] := 0;
    for i := 0 to m - 1 do
    begin
      if det1 <> 0 then
        b1[i] := b1[i] / det1
      else
        b1[i] := 0;
      Result.X := Result.X + cp[j * m + i].X * b1[i] * b2[j];
      Result.Y := Result.Y + cp[j * m + i].Y * b1[i] * b2[j];
      Result.Z := Result.Z + cp[j * m + i].Z * b1[i] * b2[j];
    end;
  end;
end;

procedure GenerateBSpline(Steps, Order: integer; KnotVector: TSingleList;
  ControlPoints, Vertices: TAffineVectorList);
var
  i: integer;
begin
  Vertices.Clear;
  Vertices.Count := Steps;
  for i := 0 to Steps - 1 do
    Vertices[i] := BSplinePoint(i / (Steps - 1), ControlPoints.Count, Order + 1,
      @KnotVector.List[0], ControlPoints.List);
end;

procedure GenerateBSplineSurface(Steps, UOrder, VOrder, Width, Height: integer;
  UKnotVector, VKnotVector: TSingleList; ControlPoints, Vertices: TAffineVectorList);
var
  i, j: integer;
begin
  Vertices.Clear;
  Vertices.Count := Steps * Steps;
  for j := 0 to Steps - 1 do
    for i := 0 to Steps - 1 do
      Vertices[i + j * Steps] := BSplineSurfacePoint(i / (Steps - 1),
        j / (Steps - 1), Width, Height, UOrder + 1, VOrder + 1,
        @UKnotVector.List[0], @VKnotVector.List[0], ControlPoints.List);
end;

procedure GenerateKnotVector(KnotVector: TSingleList;
  NumberOfPoints, Order: integer; Continuity: TBSplineContinuity);
var
  i, n, k: integer;
begin
  KnotVector.Clear;

  k := Order + 1;
  n := NumberOfPoints - 1;

  case Continuity of

    // Open curve
    bscUniformNonPeriodic:
      begin
        for i := 0 to n + k do
        begin
          if i < k then
            KnotVector.Add(0)
          else if i > n then
            KnotVector.Add(n - k + 2)
          else
            KnotVector.Add(i - k + 1);
        end;
      end;

    // Closed curve
    bscUniformPeriodic:
      begin
        for i := 0 to n + k do
        begin
          KnotVector.Add(i);
        end;
        KnotVector.Scale(1 / KnotVector.Sum);
      end;

  end;
end;

end.
