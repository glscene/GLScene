//
// This unit is part of the GLScene Engine, http://glscene.org
//
{
  Functions for generating perlin noise. 
}
unit GLPerlinBase;

interface

{$I GLScene.inc}

type
  T1DPerlinArray = array of Double;
  T2DPerlinArray = array of T1DPerlinArray;

  // Useless for final output! Usefull for after interpolation, as its FAST!
function Linear_Interpolate(const a, b, x: Double): Double;
// does a cubic interpolation
function Cubic_Interpolate(v0, v1, v2, v3, x: Double): Double;
// does a cosine interpolation
function Cosine_Interpolate(const a, b, x: Double): Double;
// just a random controlled by X
function Perlin_Random1(x: Integer): Double;
// just a random controlled by X,Y
function Perlin_Random2(Const x, Y: Integer): Double;
// generates a random strip
procedure Perlin_Random1DStrip(x, Width, Step: Integer; Amp: Double;
  Res: T1DPerlinArray);
// cubic interpolate 4 strips into one...
procedure Cubic_Interpolate_Strip(B1, B2, B3, B4, Res: T1DPerlinArray;
  Width: Integer);
// smooth interpolate 3 strips into one...
procedure Smooth_Interpolate_Strip(B1, B2, B3, Res: T1DPerlinArray;
  Width: Integer);

// a function returning some integer based on the root^exponant concept,
// result is crap and is only for "random" usage... eg perlin.
function ExponateCrap(root, exponant: Integer): Integer;

//-----------------------------------------------------------------
implementation
//-----------------------------------------------------------------

function ExponateCrap(root, exponant: Integer): Integer;
var
  D: Extended;
begin
  if root <= 0 then
    Result := 0
  else
  begin
    D := exp(ln(root) * exponant);
    If D >= 1E30 then // = Infinity then
      D := root * exponant;
    // if you got a better(faster) way of carving some integer value out of a double let me know!
    if D > maxInt then
      Result := maxInt
    else
      Result := Round(D);
  end;
end;

function Perlin_Random1(x: Integer): Double;
begin
  x := ExponateCrap((x shl 13) + (x shr 9), x);
  // mess up the number real good!

  // X        X          X       those three number can be played with, primes are incouraged!
  x := ((x * (x * x * 15731 + 789221) + 1376312589) And $7FFFFFFF);

  Result := 1.0 - x / 1073741824.0 // make it a [-1;1] affair!
end;

function Perlin_Random2(const x, Y: Integer): Double;
begin
  // it works! I guess any prime will do!
  Result := Perlin_Random1(x + Y * 57);
end;

procedure Perlin_Random1DStrip(x, Width, Step: Integer; Amp: Double;
  Res: T1DPerlinArray);
var
  Posi: PDouble;
  XC: Integer;
begin
  Posi := @Res[0];
  For XC := 0 to Width - 1 do
  begin
    Posi^ := Perlin_Random1(x) * Amp;
    inc(Posi);
    inc(x, Step);
  end;
end;

procedure Smooth_Interpolate_Strip(B1, B2, B3, Res: T1DPerlinArray;
  Width: Integer);
var
  Posi: PDouble;
  T1: PDouble;
  T2: PDouble;
  T3: PDouble;

  C1: PDouble;
  C2: PDouble;
  C3: PDouble;

  L1: PDouble;
  L2: PDouble;
  L3: PDouble;

  XC: Integer;
begin
  Posi := @Res[0];
  T1 := @B1[0];
  C1 := @B2[0];
  L1 := @B3[0];

  T2 := Pointer(Cardinal(T1) + SizeOf(Double));
  C2 := Pointer(Cardinal(C1) + SizeOf(Double));
  L2 := Pointer(Cardinal(L1) + SizeOf(Double));

  T3 := Pointer(Cardinal(T2) + SizeOf(Double));
  C3 := Pointer(Cardinal(C2) + SizeOf(Double));
  L3 := Pointer(Cardinal(L2) + SizeOf(Double));

  for XC := 0 to Width - 1 do
  begin
    Posi^ := (T1^ + T3^ + L1^ + L3^) / 16 + (T2^ + C1^ + C3^ + L2^) / 8
      + C2^ / 4;
    inc(Posi);

    T1 := T2;
    C1 := C2;
    L1 := L2;

    T2 := T3;
    C2 := C3;
    L2 := L3;

    inc(T3);
    inc(C3);
    inc(L3);
  end;
end;

procedure Cubic_Interpolate_Strip(B1, B2, B3, B4, Res: T1DPerlinArray;
  Width: Integer);
var
  Posi: PDouble;
  v1: PDouble;
  v2: PDouble;
  v3: PDouble;
  V4: PDouble;

  H1: PDouble;
  H2: PDouble;
  H3: PDouble;
  H4: PDouble;

  XC: Integer;
begin
  Posi := @Res[0];
  v1 := @B1[1];
  v2 := @B2[1];
  v3 := @B3[1];
  V4 := @B4[1];

  H1 := @B2[0];
  H2 := @B2[1];
  H3 := @B2[2];
  H4 := @B2[3];

  for XC := 0 to Width - 1 do
  begin
    Posi^ := Cubic_Interpolate(v1^, v2^, v3^, V4^, 0.5) / 2 +
      Cubic_Interpolate(H1^, H2^, H3^, H4^, 0.5) / 2;
    inc(Posi);

    H1 := H2;
    H2 := H3;
    H3 := H4;
    inc(H4);

    inc(v1);
    inc(v2);
    inc(v3);
    inc(V4);
  end;
end;

function Linear_Interpolate(const a, b, x: Double): Double;
begin
  Result := a * (1 - x) + b * x
end;

function Cosine_Interpolate(const a, b, x: Double): Double;
var
  ft: Double;
  f: Double;

begin
  ft := x * pi;
  f := (1 - cos(ft)) * 0.5;

  Result := a * (1 - f) + b * f;
end;

function Cubic_Interpolate(v0, v1, v2, v3, x: Double): Double;
var
  P, Q, R, S: Double;

begin
  { Result := Cosine_Interpolate(v1,v2,x);
    Exit;
    v0 := -0.5;
    v1 := 0;
    v2 := 0;
    v3 := -0.5; }
  P := (v3 - v2) - (v0 - v1);
  Q := (v0 - v1) - P;
  R := v2 - v0;
  S := v1;

  Result := (P * x * x * x + Q * x * x + R * x + S);
  // If (Abs(Result) > 1) then
  // Raise exception.create('Cubic_Interpolate result to high, '+FloatToStr(Result)+' values ['+FloatToStr(v0)+';'+FloatToStr(v1)+';'+FloatToStr(v2)+';'+FloatToStr(v3)+']');{}
end;

end.
