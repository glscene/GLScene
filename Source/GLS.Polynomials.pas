//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.Polynomials;

(*
  Utility functions for manipulationg and solving polynomials.

  Direct solving is supported for polynoms up to the 4th degree.

  Polynom solving code based on Jochen Schwarze (schwarze@isa.de) solver
  published in Graphics Gem (1990).

  Adapted to pascal by Eric Grange (egrange@glscene.org), if you find
  errors, they are probably mine. Note that contrary to the original code,
  the functions accept 'zero' values for any of the parameters.
  I also made some changes for certain limit cases that (seemingly) weren't
  properly handled, these are marked by comments in the code.
  Note: in progress - limited precision.
*)

interface

{$I GLScene.inc}

uses
  GLS.VectorGeometry;

type
  TDoubleArray = array of Double;

// Computes polynom's value for given x
function EvalPolynom(const poly: TDoubleArray; const x: Double): Double;
// Calculates the polynom's derivative
function DerivatedPolynom(const poly: TDoubleArray): TDoubleArray;
(* Finds a root between min and max with a precision of epsilon.
  The evaluation of min/max must be of opposit sign *)
function FindRoot(const poly: TDoubleArray; min, max, epsilon: Double): Double;
(* Finds the minimum positive coef in the array in aMin.
  Returns true if such an item was found. *)
function MinPositiveCoef(const coefs: TDoubleArray; var aMin: Double): Boolean;

// Calculates the cube root of its parameter.
function cbrt(const x: Double): Double;

(* Computes the real roots of a real polynomial of the 2nd degree.
  The polynomial is of the form:
  A(0) + A(1)*Z + A(2)*Z**2 *)
function SolveQuadric(const c: PDoubleArray): TDoubleArray;

(* Computes the real roots of a real polynomial of the 3rd degree.
  The polynomial is of the form:
  A(0) + A(1)*Z + A(2)*Z**2 + A(3)*Z**3 *)
function SolveCubic(const c: PDoubleArray): TDoubleArray;

(* Computes the real roots of a real polynomial of the 4th degree.
  The polynomial is of the form:
  A(0) + A(1)*Z + ... + A(4)*Z**4 *)
function SolveQuartic(const c: PDoubleArray): TDoubleArray;

// --------------------------------------------------------------
implementation
// --------------------------------------------------------------

const
  cEpsilon: Double = 1E-40;
  c1div3: Double = 0.3333333333333333333333333333333;
  cHalf: Double = 0.5;

function IsZero(var v: Double): Boolean; overload;
begin
  Result := (Abs(v) <= cEpsilon);
end;

function EvalPolynom(const poly: TDoubleArray; const x: Double): Double;
var
  i, n: Integer;
begin
  n := Length(poly);
  if n > 0 then
  begin
    Result := poly[n - 1];
    for i := n - 2 downto 0 do
      Result := Result * x + poly[i];
  end
  else
    Result := 0;
end;

function DerivatedPolynom(const poly: TDoubleArray): TDoubleArray;
var
  n, i: Integer;
begin
  n := Length(poly);
  if n > 1 then
  begin
    SetLength(Result, n - 1);
    for i := 1 to n - 1 do
      Result[i - 1] := poly[i] * i;
  end
  else
  begin
    SetLength(Result, 1);
    Result[0] := 0;
  end;
end;

function FindRoot(const poly: TDoubleArray; min, max, epsilon: Double): Double;
var
  evMin, evMax, mid, evMid: Double;
begin
  // handle degenerate cases first
  Assert(min < max);
  evMin := EvalPolynom(poly, min);
  if evMin = 0 then
  begin
    Result := min;
    Exit;
  end;
  evMax := EvalPolynom(poly, max);
  if evMax = 0 then
  begin
    Result := max;
    Exit;
  end;
  if evMax < 0 then
  begin
    Assert(evMin > 0);
    while Abs(max - min) > epsilon do
    begin
      mid := (max + min) * 0.5;
      evMid := EvalPolynom(poly, mid);
      if evMid > 0 then
        min := mid
      else
        max := mid;
    end;
  end
  else
  begin
    Assert(evMin < 0);
    while Abs(max - min) > epsilon do
    begin
      mid := (max + min) * 0.5;
      evMid := EvalPolynom(poly, mid);
      if evMid > 0 then
        max := mid
      else
        min := mid;
    end;
  end;
  Result := (max + min) * cHalf;
end;

function MinPositiveCoef(const coefs: TDoubleArray; var aMin: Double): Boolean;
var
  n, i, j: Integer;
begin
  n := Length(coefs);
  case n of
    0:
      Result := False;
    1:
      begin
        if coefs[0] >= 0 then
        begin
          aMin := coefs[0];
          Result := True;
        end
        else
          Result := False;
      end;
    2:
      begin
        if coefs[0] >= 0 then
        begin
          aMin := coefs[0];
          if (coefs[1] >= 0) and (coefs[1] < aMin) then
            aMin := coefs[1];
          Result := True;
        end
        else if coefs[1] >= 0 then
        begin
          aMin := coefs[1];
          Result := True;
        end
        else
          Result := False;
      end;
  else
    Result := False;
    // find a positive value, then find lowest positive
    for i := 0 to n - 1 do
    begin
      if coefs[i] >= 0 then
      begin
        aMin := coefs[i];
        for j := i + 1 to n - 1 do
        begin
          if (coefs[j] >= 0) and (coefs[j] < aMin) then
            aMin := coefs[j];
        end;
        Result := True;
        Break;
      end;
    end;
  end;
end;

function cbrt(const x: Double): Double;
begin
  if x > 0 then
    Result := PowerSingle(x, c1div3)
  else if x < 0 then
    Result := -PowerSingle(-x, c1div3)
  else
    Result := 0;
end;

function SolveQuadric(const c: PDoubleArray): TDoubleArray;
var
  p, q, D, sqrt_D: Double;
begin
  // normal form: x^2 + px + q = 0

  p := c^[1] / (2 * c^[2]);
  q := c^[0] / c^[2];

  D := Sqr(p) - q;

  if IsZero(D) then
  begin
    SetLength(Result, 1);
    Result[0] := -p;
  end
  else if D > 0 then
  begin
    sqrt_D := Sqrt(D);
    SetLength(Result, 2);
    Result[0] := sqrt_D - p;
    Result[1] := -sqrt_D - p;
  end
  else
  begin
    // if (D < 0)
    SetLength(Result, 0);
  end;
end;

function SolveCubic(const c: PDoubleArray): TDoubleArray;
var
  i: Integer;
  sub: Double;
  A, B, Cc: Double;
  sq_A, p, q: Double;
  cb_p, D: Double;
  u, v, phi, t, sqrt_D, invC3: Double;
begin
  if IsZero(c^[3]) then
  begin
    Result := SolveQuadric(c);
    Exit;
  end;
  // normal form: x^3 + Ax^2 + Bx + C = 0
  invC3 := 1 / c^[3];
  A := c^[2] * invC3;
  B := c^[1] * invC3;
  Cc := c^[0] * invC3;
  // substitute x = y - A/3 to eliminate quadric term:
  // x^3 +px + q = 0
  sq_A := Sqr(A);
  p := c1div3 * (B - c1div3 * sq_A);
  q := cHalf * (2.0 / 27 * A * sq_A - c1div3 * A * B + Cc);
  // use Cardano's formula
  cb_p := Sqr(p) * p;
  D := Sqr(q) + cb_p;
  if IsZero(D) then
  begin
    if IsZero(q) then
    begin // one triple solution
      SetLength(Result, 1);
      Result[0] := 0;
    end
    else
    begin // one single and one double solution
      u := cbrt(-q);
      SetLength(Result, 2);
      Result[0] := 2 * u;
      Result[1] := -u;
    end;
  end
  else if D < 0 then
  begin // Casus irreducibilis: three real solutions
    phi := c1div3 * ArcCosine(-q * RSqrt(-cb_p));
    t := 2 * Sqrt(-p);
    SetLength(Result, 3);
    Result[0] := t * Cos(phi);
    Result[1] := -t * Cos(phi + PI / 3);
    Result[2] := -t * Cos(phi - PI / 3);
  end
  else
  begin // one real solution
    sqrt_D := Sqrt(D);
    u := cbrt(sqrt_D - q);
    v := -cbrt(sqrt_D + q);
    SetLength(Result, 1);
    Result[0] := u + v;
  end;
  // resubstitute
  sub := c1div3 * A;
  for i := 0 to High(Result) do
    Result[i] := Result[i] - sub;
end;

function SolveQuartic(const c: PDoubleArray): TDoubleArray;
var
  coeffs: array [0 .. 3] of Double;
  z, u, v, sub: Double;
  A, B, Cc, D: Double;
  sq_A, p, q, r, invC4: Double;
  i, n, nt: Integer;
  temp: TDoubleArray;
begin
  if IsZero(c^[4]) then
  begin
    Result := SolveCubic(c);
    Exit;
  end;
  // normal form: x^4 + Ax^3 + Bx^2 + Cx + D = 0
  invC4 := 1 / c^[4];
  A := c^[3] * invC4;
  B := c^[2] * invC4;
  Cc := c^[1] * invC4;
  D := c^[0] * invC4;
  // substitute x = y - A/4 to eliminate cubic term:
  // x^4 + px^2 + qx + r = 0
  sq_A := Sqr(A);
  p := -3.0 / 8 * sq_A + B;
  q := 0.125 * sq_A * A - 0.5 * A * B + Cc;
  r := -3.0 / 256 * Sqr(sq_A) + 1.0 / 16 * sq_A * B - 0.25 * A * Cc + D;
  if IsZero(r) then
  begin
    // no absolute term: y(y^3 + py + q) = 0
    coeffs[0] := q;
    coeffs[1] := p;
    coeffs[2] := 0;
    coeffs[3] := 1;
    Result := SolveCubic(@coeffs[0]);
    n := Length(Result);
    SetLength(Result, n + 1);
    Result[n] := 0;
    SetLength(temp, 0);
  end
  else
  begin
    // solve the resolvent cubic ...
    coeffs[0] := 0.5 * r * p - 0.125 * Sqr(q);
    coeffs[1] := -r;
    coeffs[2] := -0.5 * p;
    coeffs[3] := 1;
    Result := SolveCubic(@coeffs[0]);
    // ... and take the one real solution ...
    Assert(Length(Result) > 0);
    z := Result[0];
    // ... to build two quadric equations
    u := Sqr(z) - r;
    v := 2 * z - p;
    if IsZero(u) then
      u := 0
    else if u > 0 then
      u := Sqrt(u)
    else
    begin
      SetLength(Result, 0);
      Exit;
    end;
    if IsZero(v) then
      v := 0
    else if v > 0 then
      v := Sqrt(v)
    else
    begin
      SetLength(Result, 0);
      Exit;
    end;
    coeffs[0] := z - u;
    if q < 0 then
      coeffs[1] := -v
    else
      coeffs[1] := v;
    coeffs[2] := 1;
    Result := SolveQuadric(@coeffs[0]);
    coeffs[0] := z + u;
    if q < 0 then
      coeffs[1] := v
    else
      coeffs[1] := -v;
    coeffs[2] := 1;
    temp := SolveQuadric(@coeffs[0]);
    nt := Length(temp);
    if nt > 0 then
    begin
      n := Length(Result);
      SetLength(Result, n + nt);
      for i := 0 to nt - 1 do
        Result[n + i] := temp[i];
    end;
    // resubstitute
    sub := 0.25 * A;
    for i := 0 to High(Result) do
      Result[i] := Result[i] - sub;
  end;
end;

end.
