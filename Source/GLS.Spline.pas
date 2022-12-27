//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Spline;

(* Cubic spline interpolation functions *)

interface

uses
  GLS.VectorTypes,
  GLS.VectorGeometry;

{$I GLScene.inc}

type

  TCubicSplineMatrix = array of array [0 .. 3] of Single;

  (* 3D cubic spline handler class.
    This class allows to describe and calculate values of a time-based,
    three-dimensionnal cubic spline.
    Cubic spline pass through all given points and tangent on point N is
    given by the (N-1) to (N+1) vector.
    Note : X, Y & Z are actually interpolated independently. *)
  TCubicSpline = class(TObject)
  private
    matX, matY, matZ, matW: TCubicSplineMatrix;
    FNb: Integer;
  public
    (* Creates the spline and declares interpolation points.
      Time references go from 0 (first point) to nb-1 (last point), the
      first and last reference matrices respectively are used when T is
      used beyond this range.
      Note : "nil" single arrays are accepted, in this case the axis is
      disabled and calculus will return 0 (zero) for this component. *)
    constructor Create(const X, Y, Z, W: PFloatArray; const nb: Integer);
    {$IFDEF CLR} unsafe; {$ENDIF}
    destructor Destroy; override;
    // Calculates X component at time t. 
    function SplineX(const t: Single): Single;
    // Calculates Y component at time t. 
    function SplineY(const t: Single): Single;
    // Calculates Z component at time t. 
    function SplineZ(const t: Single): Single;
    // Calculates W component at time t. 
    function SplineW(const t: Single): Single;
    // Calculates X and Y components at time t. 
    procedure SplineXY(const t: Single; out X, Y: Single);
    // Calculates X, Y and Z components at time t. 
    procedure SplineXYZ(const t: Single; out X, Y, Z: Single);
    // Calculates X, Y, Z and W components at time t. 
    procedure SplineXYZW(const t: Single; out X, Y, Z, W: Single);
    // Calculates affine vector at time t. 
    function SplineAffineVector(const t: Single): TAffineVector; overload;
    // Calculates affine vector at time t. 
    procedure SplineAffineVector(const t: Single;
      var vector: TAffineVector); overload;
    // Calculates vector at time t. 
    function SplineVector(const t: Single): TGLVector; overload;
    // Calculates vector at time t. 
    procedure SplineVector(const t: Single; var vector: TGLVector); overload;
    // Calculates X component slope at time t. 
    function SplineSlopeX(const t: Single): Single;
    // Calculates Y component slope at time t. 
    function SplineSlopeY(const t: Single): Single;
    // Calculates Z component slope at time t. 
    function SplineSlopeZ(const t: Single): Single;
    // Calculates W component slope at time t. 
    function SplineSlopeW(const t: Single): Single;
    // Calculates the spline slope at time t. 
    function SplineSlopeVector(const t: Single): TAffineVector; overload;
    (* Calculates the intersection of the spline with the YZ plane.
      Returns True if an intersection was found. *)
    function SplineIntersecYZ(X: Single; var Y, Z: Single): Boolean;
    (* Calculates the intersection of the spline with the XZ plane.
      Returns True if an intersection was found. *)
    function SplineIntersecXZ(Y: Single; var X, Z: Single): Boolean;
    (* Calculates the intersection of the spline with the XY plane.
      Returns True if an intersection was found. *)
    function SplineIntersecXY(Z: Single; var X, Y: Single): Boolean;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

procedure VECCholeskyTriDiagResol(const b: array of Single; const nb: Integer;
  var Result: array of Single);
var
  Y, LDiag, LssDiag: array of Single;
  i, k, Debut, Fin: Integer;
begin
  Debut := 0;
  Fin := nb - 1;
  Assert(Length(b) > 0);
  SetLength(LDiag, nb);
  SetLength(LssDiag, nb - 1);
  LDiag[Debut] := 1.4142135; // = sqrt(2)
  LssDiag[Debut] := 1.0 / 1.4142135;
  for k := Debut + 1 to Fin - 1 do
  begin
    LDiag[k] := Sqrt(4 - LssDiag[k - 1] * LssDiag[k - 1]);
    LssDiag[k] := 1.0 / LDiag[k];
  end;
  LDiag[Fin] := Sqrt(2 - LssDiag[Fin - 1] * LssDiag[Fin - 1]);
  SetLength(Y, nb);
  Y[Debut] := b[Debut] / LDiag[Debut];
  for i := Debut + 1 to Fin do
    Y[i] := (b[i] - Y[i - 1] * LssDiag[i - 1]) / LDiag[i];
  Assert(Length(Result) = nb);
  Result[Fin] := Y[Fin] / LDiag[Fin];
  for i := Fin - 1 downto Debut do
    Result[i] := (Y[i] - Result[i + 1] * LssDiag[i]) / LDiag[i];
end;

procedure MATInterpolationHermite(const ordonnees: PFloatArray;
  const nb: Integer; var Result: TCubicSplineMatrix); {$IFDEF CLR}unsafe;
{$ENDIF}
var
  a, b, c, d: Single;
  i, n: Integer;
  bb, deriv: array of Single;
begin
  Result := nil;
  if Assigned(ordonnees) and (nb > 0) then
  begin
    n := nb - 1;
    SetLength(bb, nb);
    bb[0] := 3 * (ordonnees[1] - ordonnees[0]);
    bb[n] := 3 * (ordonnees[n] - ordonnees[n - 1]);
    for i := 1 to n - 1 do
      bb[i] := 3 * (ordonnees[i + 1] - ordonnees[i - 1]);
    SetLength(deriv, nb);
    VECCholeskyTriDiagResol(bb, nb, deriv);
    SetLength(Result, n);
    for i := 0 to n - 1 do
    begin
      a := ordonnees[i];
      b := deriv[i];
      c := 3 * (ordonnees[i + 1] - ordonnees[i]) - 2 * deriv[i] - deriv[i + 1];
      d := -2 * (ordonnees[i + 1] - ordonnees[i]) + deriv[i] + deriv[i + 1];
      Result[i][3] := a + i * (i * (c - i * d) - b);
      Result[i][2] := b + i * (3 * i * d - 2 * c);
      Result[i][1] := c - 3 * i * d;
      Result[i][0] := d;
    end;
  end;
end;

function MATValeurSpline(const spline: TCubicSplineMatrix; const X: Single;
  const nb: Integer): Single;
var
  i: Integer;
begin
  if Length(spline) > 0 then
  begin
    if X <= 0 then
      i := 0
    else if X > nb - 1 then
      i := nb - 1
    else
      i := Integer(Trunc(X));
    { TODO : the following line looks like a bug... }
    if i = (nb - 1) then
      Dec(i);
    Result := ((spline[i][0] * X + spline[i][1]) * X + spline[i][2]) * X +
      spline[i][3];
  end
  else
    Result := 0;
end;

function MATValeurSplineSlope(const spline: TCubicSplineMatrix; const X: Single;
  const nb: Integer): Single;
var
  i: Integer;
begin
  if Length(spline) > 0 then
  begin
    if X <= 0 then
      i := 0
    else if X > nb - 1 then
      i := nb - 1
    else
      i := Integer(Trunc(X));
    { TODO : the following line looks like a bug... }
    if i = (nb - 1) then
      Dec(i);
    Result := (3 * spline[i][0] * X + 2 * spline[i][1]) * X + spline[i][2];
  end
  else
    Result := 0;
end;

// ------------------
// ------------------ TCubicSpline ------------------
// ------------------

constructor TCubicSpline.Create(const X, Y, Z, W: PFloatArray;
  const nb: Integer); {$IFDEF CLR}unsafe; {$ENDIF}
begin
  inherited Create;
  MATInterpolationHermite(X, nb, matX);
  MATInterpolationHermite(Y, nb, matY);
  MATInterpolationHermite(Z, nb, matZ);
  MATInterpolationHermite(W, nb, matW);
  FNb := nb;
end;

destructor TCubicSpline.Destroy;
begin
  inherited Destroy;
end;

function TCubicSpline.SplineX(const t: Single): Single;
begin
  Result := MATValeurSpline(matX, t, FNb);
end;

function TCubicSpline.SplineY(const t: Single): Single;
begin
  Result := MATValeurSpline(matY, t, FNb);
end;

function TCubicSpline.SplineZ(const t: Single): Single;
begin
  Result := MATValeurSpline(matZ, t, FNb);
end;

function TCubicSpline.SplineW(const t: Single): Single;
begin
  Result := MATValeurSpline(matW, t, FNb);
end;

procedure TCubicSpline.SplineXY(const t: Single; out X, Y: Single);
begin
  X := MATValeurSpline(matX, t, FNb);
  Y := MATValeurSpline(matY, t, FNb);
end;

procedure TCubicSpline.SplineXYZ(const t: Single; out X, Y, Z: Single);
begin
  X := MATValeurSpline(matX, t, FNb);
  Y := MATValeurSpline(matY, t, FNb);
  Z := MATValeurSpline(matZ, t, FNb);
end;

procedure TCubicSpline.SplineXYZW(const t: Single; out X, Y, Z, W: Single);
begin
  X := MATValeurSpline(matX, t, FNb);
  Y := MATValeurSpline(matY, t, FNb);
  Z := MATValeurSpline(matZ, t, FNb);
  W := MATValeurSpline(matW, t, FNb);
end;

function TCubicSpline.SplineAffineVector(const t: Single): TAffineVector;
begin
  Result.X := MATValeurSpline(matX, t, FNb);
  Result.Y := MATValeurSpline(matY, t, FNb);
  Result.Z := MATValeurSpline(matZ, t, FNb);
end;

procedure TCubicSpline.SplineAffineVector(const t: Single;
  var vector: TAffineVector);
begin
  vector.X := MATValeurSpline(matX, t, FNb);
  vector.Y := MATValeurSpline(matY, t, FNb);
  vector.Z := MATValeurSpline(matZ, t, FNb);
end;

function TCubicSpline.SplineVector(const t: Single): TGLVector;
begin
  Result.X := MATValeurSpline(matX, t, FNb);
  Result.Y := MATValeurSpline(matY, t, FNb);
  Result.Z := MATValeurSpline(matZ, t, FNb);
  Result.W := MATValeurSpline(matW, t, FNb);
end;

procedure TCubicSpline.SplineVector(const t: Single; var vector: TGLVector);
begin
  vector.X := MATValeurSpline(matX, t, FNb);
  vector.Y := MATValeurSpline(matY, t, FNb);
  vector.Z := MATValeurSpline(matZ, t, FNb);
  vector.W := MATValeurSpline(matW, t, FNb);
end;

function TCubicSpline.SplineSlopeX(const t: Single): Single;
begin
  Result := MATValeurSplineSlope(matX, t, FNb);
end;

function TCubicSpline.SplineSlopeY(const t: Single): Single;
begin
  Result := MATValeurSplineSlope(matY, t, FNb);
end;

function TCubicSpline.SplineSlopeZ(const t: Single): Single;
begin
  Result := MATValeurSplineSlope(matZ, t, FNb);
end;

function TCubicSpline.SplineSlopeW(const t: Single): Single;
begin
  Result := MATValeurSplineSlope(matW, t, FNb);
end;

function TCubicSpline.SplineSlopeVector(const t: Single): TAffineVector;
begin
  Result.X := MATValeurSplineSlope(matX, t, FNb);
  Result.Y := MATValeurSplineSlope(matY, t, FNb);
  Result.Z := MATValeurSplineSlope(matZ, t, FNb);
end;

function TCubicSpline.SplineIntersecYZ(X: Single; var Y, Z: Single): Boolean;
var
  Sup, Inf, Mid: Single;
  SSup, Sinf, Smid: Single;
begin
  Result := False;

  Sup := FNb;
  Inf := 0.0;

  SSup := SplineX(Sup);
  Sinf := SplineX(Inf);
  if SSup > Sinf then
  begin
    if (SSup < X) or (Sinf > X) then
      Exit;
    while Abs(SSup - Sinf) > 1E-4 do
    begin
      Mid := (Sup + Inf) * 0.5;
      Smid := SplineX(Mid);
      if X < Smid then
      begin
        SSup := Smid;
        Sup := Mid;
      end
      else
      begin
        Sinf := Smid;
        Inf := Mid;
      end;
    end;
    Y := SplineY((Sup + Inf) * 0.5);
    Z := SplineZ((Sup + Inf) * 0.5);
  end
  else
  begin
    if (Sinf < X) or (SSup > X) then
      Exit;
    while Abs(SSup - Sinf) > 1E-4 do
    begin
      Mid := (Sup + Inf) * 0.5;
      Smid := SplineX(Mid);
      if X < Smid then
      begin
        Sinf := Smid;
        Inf := Mid;
      end
      else
      begin
        SSup := Smid;
        Sup := Mid;
      end;
    end;
    Y := SplineY((Sup + Inf) * 0.5);
    Z := SplineZ((Sup + Inf) * 0.5);
  end;
  Result := True;
end;

function TCubicSpline.SplineIntersecXZ(Y: Single; var X, Z: Single): Boolean;
var
  Sup, Inf, Mid: Single;
  SSup, Sinf, Smid: Single;
begin
  Result := False;

  Sup := FNb;
  Inf := 0.0;

  SSup := SplineY(Sup);
  Sinf := SplineY(Inf);
  if SSup > Sinf then
  begin
    if (SSup < Y) or (Sinf > Y) then
      Exit;
    while Abs(SSup - Sinf) > 1E-4 do
    begin
      Mid := (Sup + Inf) * 0.5;
      Smid := SplineY(Mid);
      if Y < Smid then
      begin
        SSup := Smid;
        Sup := Mid;
      end
      else
      begin
        Sinf := Smid;
        Inf := Mid;
      end;
    end;
    X := SplineX((Sup + Inf) * 0.5);
    Z := SplineZ((Sup + Inf) * 0.5);
  end
  else
  begin
    if (Sinf < Y) or (SSup > Y) then
      Exit;
    while Abs(SSup - Sinf) > 1E-4 do
    begin
      Mid := (Sup + Inf) * 0.5;
      Smid := SplineY(Mid);
      if Y < Smid then
      begin
        Sinf := Smid;
        Inf := Mid;
      end
      else
      begin
        SSup := Smid;
        Sup := Mid;
      end;
    end;
    X := SplineX((Sup + Inf) * 0.5);
    Z := SplineZ((Sup + Inf) * 0.5);
  end;
  Result := True;
end;

function TCubicSpline.SplineIntersecXY(Z: Single; var X, Y: Single): Boolean;
var
  Sup, Inf, Mid: Single;
  SSup, Sinf, Smid: Single;
begin
  Result := False;

  Sup := FNb;
  Inf := 0.0;

  SSup := SplineZ(Sup);
  Sinf := SplineZ(Inf);
  if SSup > Sinf then
  begin
    if (SSup < Z) or (Sinf > Z) then
      Exit;
    while Abs(SSup - Sinf) > 1E-4 do
    begin
      Mid := (Sup + Inf) * 0.5;
      Smid := SplineZ(Mid);
      if Z < Smid then
      begin
        SSup := Smid;
        Sup := Mid;
      end
      else
      begin
        Sinf := Smid;
        Inf := Mid;
      end;
    end;
    X := SplineX((Sup + Inf) * 0.5);
    Y := SplineY((Sup + Inf) * 0.5);
  end
  else
  begin
    if (Sinf < Z) or (SSup > Z) then
      Exit;
    while Abs(SSup - Sinf) > 1E-4 do
    begin
      Mid := (Sup + Inf) * 0.5;
      Smid := SplineZ(Mid);
      if Z < Smid then
      begin
        Sinf := Smid;
        Inf := Mid;
      end
      else
      begin
        SSup := Smid;
        Sup := Mid;
      end;
    end;
    X := SplineX((Sup + Inf) * 0.5);
    Y := SplineY((Sup + Inf) * 0.5);
  end;
  Result := True;
end;

end.
