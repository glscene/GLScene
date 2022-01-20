//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.Perlin;

(*
  Classes and functions for generating perlin noise.

  The components and classes in the unit are a base to generate textures and heightmaps from,
  A Perlin Height Data Source have been included as an example.
  Use this combined with a terrain renderer for an infinite random landscape
*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  Vcl.Graphics,

  GLS.VectorGeometry,
  GLS.HeightData;

type

  T1DPerlinArray = array of Double;
  T2DPerlinArray = array of T1DPerlinArray;

  TGLPerlinInterpolation = (pi_none, pi_simple, pi_linear, pi_Smoothed,
    pi_Cosine, pi_cubic);

  TGLBasePerlinOctav = class
  private
    FAmplitude: Double;
    FScale: Double;
    FInterpolation: TGLPerlinInterpolation;
    FSmoothing: TGLPerlinInterpolation;
  public
    Procedure Generate; virtual; abstract;
    property Interpolation: TGLPerlinInterpolation read FInterpolation
      write FInterpolation;
    property Smoothing: TGLPerlinInterpolation read FSmoothing write FSmoothing;
    property Amplitude: Double read FAmplitude write FAmplitude;
    property Scale: Double read FScale write FScale;
  end;

  TGLPerlinOctav = class of TGLBasePerlinOctav;

  TGLBasePerlin = class(TComponent)
  private
    FPersistence: Double;
    FNumber_Of_Octaves: Integer;
    FOctaves: TList;
    FOctavClass: TGLPerlinOctav;
    FInterpolation: TGLPerlinInterpolation;
    FSmoothing: TGLPerlinInterpolation;
  protected
    function PerlinNoise_1D(x: Double): Double;
    function PerlinNoise_2D(x, y: Double): Double;
    function GetOctave(index: Integer): TGLBasePerlinOctav;
    Procedure SetPersistence(val: Double);
    Procedure Set_Number_Of_Octaves(val: Integer);
  public
    Constructor Create(AOwner: TComponent); override;
    Procedure Generate; virtual; abstract;
    Property Octaves[index: Integer]: TGLBasePerlinOctav read GetOctave;
  published
    property Smoothing: TGLPerlinInterpolation read FSmoothing write FSmoothing;
    property Interpolation: TGLPerlinInterpolation read FInterpolation
      write FInterpolation;
    property Persistence: Double read FPersistence write SetPersistence;
    property Number_Of_Octaves: Integer read FNumber_Of_Octaves
      write Set_Number_Of_Octaves;
  end;

  TGL1DPerlin = class(TGLBasePerlin)
    Function GetPerlinValue_1D(x: Double): Double;
  published
  end;

  TGL2DPerlinOctav = class(TGLBasePerlinOctav)
  public
    Data: T2DPerlinArray;
    Width, Height: Integer;
    XStart, YStart: Integer;
    XStep, YStep: Integer;
    YRate: Integer;
    Procedure Generate; override;
    Function GetDataSmoothed(x, y: Integer): Double;
    Function GetData(x, y: Integer): Double;

    Function GetCubic(x, y: Double): Double;
    Function GetCosine(x, y: Double): Double;
    Function GetPerling(x, y: Double): Double;
    Procedure Generate_CubicInterpolate;
    Procedure Generate_SmoothInterpolate;
    Procedure Generate_NonInterpolated;
  end;

  TGL2DPerlin = class(TGLBasePerlin)
  private
  public
    Width, Height: Integer;
    XStart, YStart: Integer;
    XStep, YStep: Integer;
    MaxValue, MinValue: Double;
    Constructor Create(AOwner: TComponent); override;
    Procedure Generate; override;
    Function GetPerlinValue_2D(x, y: Double): Double;
    Procedure MakeBitmap(Param: TBitmap);
    Procedure SetHeightData(heightData: TGLHeightData);
  end;

  TGLPerlinHDS = class(TGLHeightDataSource)
  private
    FInterpolation: TGLPerlinInterpolation;
    FSmoothing: TGLPerlinInterpolation;
    FPersistence: Double;
    FNumber_Of_Octaves: Integer;
    FLines: TStrings;
    FLinesChanged: Boolean;
    FXStart, FYStart: Integer;
  public
    MaxValue, MinValue: Double;
    Stall: Boolean;
    Constructor Create(AOwner: TComponent); override;
    procedure StartPreparingData(heightData: TGLHeightData); override;
    procedure WaitFor;
    property Lines: TStrings read FLines;
    property LinesChanged: Boolean read FLinesChanged write FLinesChanged;
  published
    property Interpolation: TGLPerlinInterpolation read FInterpolation
      write FInterpolation;
    property Smoothing: TGLPerlinInterpolation read FSmoothing write FSmoothing;
    property Persistence: Double read FPersistence write FPersistence;
    property Number_Of_Octaves: Integer read FNumber_Of_Octaves
      write FNumber_Of_Octaves;
    property MaxPoolSize;
    property XStart: Integer read FXStart write FXStart;
    property YStart: Integer read FYStart write FYStart;
  end;

  TGLPerlinHDSThread = class(TGLHeightDataThread)
    Perlin: TGL2DPerlin;
    PerlinSource: TGLPerlinHDS;
    Procedure OpdateOutSide;
    Procedure Execute; override;
  end;


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
procedure Perlin_Random1DStrip(x, Width, Step: Integer; Amp: Double; Res: T1DPerlinArray);
// cubic interpolate 4 strips into one...
procedure Cubic_Interpolate_Strip(B1, B2, B3, B4, Res: T1DPerlinArray; Width: Integer);
// smooth interpolate 3 strips into one...
procedure Smooth_Interpolate_Strip(B1, B2, B3, Res: T1DPerlinArray; Width: Integer);

(* The function returning some integer based on the root^exponant concept,
 result is crap and is only for "random" usage... eg perlin. *)
function ExponateCrap(root, exponant: Integer): Integer;

//----------------------------------------------------------------
implementation
//----------------------------------------------------------------

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
  (* Result := Cosine_Interpolate(v1,v2,x);
    Exit;
    v0 := -0.5;
    v1 := 0;
    v2 := 0;
    v3 := -0.5; *)
  P := (v3 - v2) - (v0 - v1);
  Q := (v0 - v1) - P;
  R := v2 - v0;
  S := v1;

  Result := (P * x * x * x + Q * x * x + R * x + S);
  // If (Abs(Result) > 1) then
  // Raise exception.create('Cubic_Interpolate result to high, '+FloatToStr(Result)+' values ['+FloatToStr(v0)+';'+FloatToStr(v1)+';'+FloatToStr(v2)+';'+FloatToStr(v3)+']');{}
end;

//-----------------------------------
// TGLBasePerlin
//-----------------------------------

function TGLBasePerlin.PerlinNoise_1D(x: Double): Double;
var
  int_x: Integer;
  frac_x: Double;

begin
  int_x := Round(Int(x));
  frac_x := x - int_x;
  case Interpolation of
    pi_none:
      Result := 0;
    pi_simple:
      Result := Perlin_Random1(int_x);
    pi_linear:
      Result := Linear_Interpolate(Perlin_Random1(int_x),
        Perlin_Random1(int_x + 1), frac_x);
    pi_cubic:
      Result := Cubic_Interpolate(Perlin_Random1(int_x - 1),
        Perlin_Random1(int_x), Perlin_Random1(int_x + 1),
        Perlin_Random1(int_x + 2), frac_x);
    pi_Cosine:
      Result := Cosine_Interpolate(Perlin_Random1(int_x),
        Perlin_Random1(int_x + 1), frac_x);
  else
    raise exception.Create
      ('PerlinNoise_1D, Interpolation not implemented!');
  end;
end;

function TGLBasePerlin.PerlinNoise_2D(x, y: Double): Double;
Var
  int_x, int_y: Integer;
  // frac_y,
  frac_x: Double;

begin
  int_x := Round(Int(x));
  int_y := Round(Int(y));
  frac_x := x - int_x;
  // frac_y := y-int_y;
  case Interpolation of
    pi_none:
      Result := 0;
    pi_simple:
      Result := Perlin_Random2(int_x, int_y);
    pi_linear:
      Result := Linear_Interpolate(Perlin_Random1(int_x),
        Perlin_Random1(int_x + 1), frac_x);
    pi_cubic:
      Result := Cubic_Interpolate(Perlin_Random1(int_x - 1),
        Perlin_Random1(int_x), Perlin_Random1(int_x + 1),
        Perlin_Random1(int_x + 2), frac_x);
    pi_Cosine:
      Result := Cosine_Interpolate(Perlin_Random1(int_x),
        Perlin_Random1(int_x + 1), frac_x);
  else
    raise exception.Create
      ('PerlinNoise_1D, Interpolation not implemented!');
  end;
end;

function TGLBasePerlin.GetOctave(index: Integer): TGLBasePerlinOctav;
begin
  Result := TGLBasePerlinOctav(FOctaves[index]);
end;

procedure TGLBasePerlin.Set_Number_Of_Octaves(val: Integer);
var
  XC: Integer;
  NewScale: Integer;
  Octav: TGLBasePerlinOctav;
begin
  If val <> FNumber_Of_Octaves then
  begin
    FNumber_Of_Octaves := val;
    For XC := FOctaves.Count to FNumber_Of_Octaves - 1 do
    begin
      Octav := FOctavClass.Create;
      If FPersistence = 0 then
        Octav.FAmplitude := 0
      else
        Octav.FAmplitude := exp(ln(FPersistence) * (XC + 1));
      Octav.FInterpolation := Interpolation;
      Octav.FSmoothing := Smoothing;
      FOctaves.Add(Octav);
    end;
    For XC := FOctaves.Count - 1 downto FNumber_Of_Octaves do
    begin
      Octav := Octaves[XC];
      FOctaves.Delete(XC);
      Octav.Free;
    end;

    NewScale := 1;
    For XC := FOctaves.Count - 1 downto 0 do
    begin
      Octaves[XC].Scale := NewScale;
      NewScale := NewScale shl 1;
    end;
  end;
end;

procedure TGLBasePerlin.SetPersistence(val: Double);
var
  XC: Integer;
begin
  If FPersistence <> val then
  begin
    FPersistence := val;
    For XC := FOctaves.Count to FNumber_Of_Octaves - 1 do
    begin
      Octaves[XC].FAmplitude := exp(ln(FPersistence) * XC);
    end;
  end;
end;

constructor TGLBasePerlin.Create(AOwner: TComponent);

begin
  inherited;
  FOctaves := TList.Create;
  FNumber_Of_Octaves := 0;
  FInterpolation := pi_Cosine;
  FSmoothing := pi_cubic;
end;

function TGL1DPerlin.GetPerlinValue_1D(x: Double): Double;
var
  total, p, frequency, Amplitude: Double;
  n, i: Integer;
begin
  total := 0;
  p := Persistence;
  n := Number_Of_Octaves - 1;

  For i := 0 to n do
  begin

    frequency := 2 * i;
    Amplitude := p * i;

    total := total + PerlinNoise_1D(x * frequency) * Amplitude;

  end;

  Result := total;
end;

procedure TGL2DPerlinOctav.Generate;

var
  YC: Integer;

begin
  SetLength(Data, Height + 3); // needed for smoothing
  For YC := 0 to Height + 2 do
    SetLength(Data[YC], Width + 3); // needed for smoothing
  case Smoothing of
    pi_cubic:
      begin
        Generate_CubicInterpolate;
      end;
    pi_Smoothed:
      begin
        Generate_SmoothInterpolate;
      end;
    pi_none:
      ;
    pi_simple:
      begin
        Generate_NonInterpolated;
      end;
  end;
end;

function TGL2DPerlinOctav.GetPerling(x, y: Double): Double;

begin
  Result := 0;
  case Interpolation of
    pi_cubic:
      begin
        Result := GetCubic(x, y);
      end;
    pi_Smoothed:
      begin
      end;
    pi_Cosine:
      begin
        Result := GetCosine(x, y);
      end;
  end;
end;

procedure TGL2DPerlinOctav.Generate_CubicInterpolate;

var
  B1, B2, B3, B4, T1: T1DPerlinArray;
  StripWidth: Integer;
  Offset: Integer;
  YC: Integer;

begin
  T1 := Nil;
  StripWidth := Width + 6;
  SetLength(B1, StripWidth);
  SetLength(B2, StripWidth);
  SetLength(B3, StripWidth);
  SetLength(B4, StripWidth);
  Offset := (XStart - 1) + (YStart - 1) * YStep * YRate;
  Perlin_Random1DStrip(Offset, StripWidth, XStep, FAmplitude, B1);
  inc(Offset, YRate * YStep);
  Perlin_Random1DStrip(Offset, StripWidth, XStep, FAmplitude, B2);
  inc(Offset, YRate * YStep);
  Perlin_Random1DStrip(Offset, StripWidth, XStep, FAmplitude, B3);
  inc(Offset, YRate * YStep);
  For YC := 0 to Height + 2 do
  begin
    Perlin_Random1DStrip(Offset, StripWidth, XStep, FAmplitude, B4);
    inc(Offset, YRate * YStep);
    Cubic_Interpolate_Strip(B1, B2, B3, B4, Data[YC], Width + 3);
    T1 := B1;
    B1 := B2;
    B2 := B3;
    B3 := B4;
    B4 := T1;
  end;
  SetLength(B1, 0);
  SetLength(B2, 0);
  SetLength(B3, 0);
  SetLength(B4, 0);
end;

procedure TGL2DPerlinOctav.Generate_SmoothInterpolate;
var
  B1, B2, B3, T1: T1DPerlinArray;
  StripWidth: Integer;
  Offset: Integer;
  YC: Integer;
begin
  T1 := Nil;
  StripWidth := Width + 5;
  SetLength(B1, StripWidth);
  SetLength(B2, StripWidth);
  SetLength(B3, StripWidth);
  Offset := (XStart - 1) + (YStart - 1) * YStep * YRate;
  Perlin_Random1DStrip(Offset, StripWidth, XStep, FAmplitude, B1);
  inc(Offset, YRate * YStep);
  Perlin_Random1DStrip(Offset, StripWidth, XStep, FAmplitude, B2);
  inc(Offset, YRate * YStep);
  For YC := 0 to Height + 2 do
  begin
    Perlin_Random1DStrip(Offset, StripWidth, XStep, FAmplitude, B3);
    inc(Offset, YRate * YStep);
    Smooth_Interpolate_Strip(B1, B2, B3, Data[YC], Width + 3);
    T1 := B1;
    B1 := B2;
    B2 := B3;
    B3 := T1;
  end;
  SetLength(B1, 0);
  SetLength(B2, 0);
  SetLength(B3, 0);
end;

procedure TGL2DPerlinOctav.Generate_NonInterpolated;

var
  Offset: Integer;
  YC: Integer;

begin
  Offset := XStart + YStart * YStep * YRate;
  For YC := 0 to Height + 2 do
  begin
    Perlin_Random1DStrip(Offset, Width + 3, XStep, FAmplitude, Data[YC]);
    inc(Offset, YRate * YStep);

  end;
end;

function TGL2DPerlinOctav.GetDataSmoothed(x, y: Integer): Double;

begin
  Result := (Data[y][x] + Data[y][x + 2] + Data[y + 2][x] + Data[y + 2][x + 2])
    / 16 + (Data[y + 1][x] + Data[y + 1][x + 2] + Data[y][x + 1] + Data[y + 2]
    [x + 1]) / 8 + Data[y + 1][x + 1] / 4; { }
end;

function TGL2DPerlinOctav.GetData(x, y: Integer): Double;

begin
  Result := Data[y][x];
end;

function TGL2DPerlinOctav.GetCubic(x, y: Double): Double;

Var
  X_Int: Integer;
  Y_Int: Integer;
  X_Frac, Y_Frac: Double;

begin
  X_Int := Round(Int(x));
  Y_Int := Round(Int(y));
  X_Frac := x - X_Int;
  Y_Frac := y - Y_Int;

  Result := (Cubic_Interpolate(GetData(X_Int, Y_Int + 1), GetData(X_Int + 1,
    Y_Int + 1), GetData(X_Int + 2, Y_Int + 1), GetData(X_Int + 3, Y_Int + 1),
    X_Frac) + Cubic_Interpolate(GetData(X_Int + 1, Y_Int), GetData(X_Int + 1,
    Y_Int + 1), GetData(X_Int + 1, Y_Int + 2), GetData(X_Int + 1, Y_Int + 3),
    Y_Frac)) / 2;
end;

function TGL2DPerlinOctav.GetCosine(x, y: Double): Double;

var
  X_Int: Integer;
  Y_Int: Integer;
  X_Frac, Y_Frac: Double;

begin
  X_Int := Round(Int(x));
  Y_Int := Round(Int(y));
  X_Frac := x - X_Int;
  Y_Frac := y - Y_Int;

  Result := Cosine_Interpolate(Cosine_Interpolate(GetData(X_Int, Y_Int),
    GetData(X_Int + 1, Y_Int), X_Frac),
    Cosine_Interpolate(GetData(X_Int, Y_Int + 1), GetData(X_Int + 1, Y_Int + 1),
    X_Frac), Y_Frac);
end;

constructor TGL2DPerlin.Create(AOwner: TComponent);

begin
  inherited;
  Width := 256;
  Height := 256;
  XStart := 0;
  YStart := 0;
  XStep := 1;
  YStep := 1;
  FOctavClass := TGL2DPerlinOctav;
end;

procedure TGL2DPerlin.Generate;

var
  i: Integer;

begin
  For i := 0 to Number_Of_Octaves - 1 do
    With TGL2DPerlinOctav(Octaves[i]) do
    begin
      Width := Round(Ceil(self.Width / Scale));
      Height := Round(Ceil(self.Height / Scale));
      XStart := Round(self.XStart / Scale);
      YStart := Round(self.YStart / Scale);
      XStep := self.XStep;
      YStep := self.YStep;
      YRate := 243 * 57 * 57;
      Generate;
    end;
end;

function TGL2DPerlin.GetPerlinValue_2D(x, y: Double): Double;
var
  total, frequency, Amplitude: Double;
  i: Integer;
begin
  total := 0;
  For i := 0 to Number_Of_Octaves - 1 do
  begin

    frequency := 2 * i;
    Amplitude := Persistence * i;

    total := total + PerlinNoise_2D(x * frequency, y * frequency) * Amplitude;

  end;

  Result := total;
end;

procedure TGL2DPerlin.MakeBitmap(Param: TBitmap);

Var
  XC, YC: Integer;
  Octaver: Integer;
  Posi: PByte;
  B: Integer;
  Value: Double;
  S: String;

begin

  MaxValue := -1;
  MinValue := 100;

  Param.Width := Width;
  Param.Height := Height;

  for YC := 0 to Height - 1 do
  begin
    Posi := Param.ScanLine[YC];
    For XC := 0 to Width - 1 do
    begin
      Value := 0;
      For Octaver := 0 to FNumber_Of_Octaves - 1 do
        With TGL2DPerlinOctav(Octaves[Octaver]) do
          Value := Value + GetPerling(XC / Scale, YC / Scale);

      Value := Value + 0.5;
      If MaxValue < Value then
        MaxValue := Value;

      If MinValue > Value then
        MinValue := Value;

      If Value > 1.0 then
      begin
        S := '';
        For Octaver := 0 to FNumber_Of_Octaves - 1 do
          With TGL2DPerlinOctav(Octaves[Octaver]) do
            S := S + FloatToStr(GetPerling(XC / Scale, YC / Scale)) + ' ,';
        Delete(S, Length(S) - 1, 2);
        // raise Exception.create('In Cubic_Interpolate_Strip a value greater than 1 occured! value = '+FloatToStr(Value)+' values=['+S+']');
      end;

      B := Round(Value * $FF) and $FF;
      Posi^ := B;
      inc(Posi);
    end;
  end;
end;

procedure TGL2DPerlin.SetHeightData(heightData: TGLHeightData);

var
  XC, YC: Integer;
  Octaver: Integer;
  Posi: PSmallInt;
  Value: Double;
  S: String;

begin

  MaxValue := -1;
  MinValue := 100;

  heightData.Allocate(hdtSmallInt);

  Posi := @heightData.SmallIntData^[0];
  For YC := 0 to Height - 1 do
  begin
    For XC := 0 to Width - 1 do
    begin
      Value := 0;
      For Octaver := 0 to FNumber_Of_Octaves - 1 do
        With TGL2DPerlinOctav(Octaves[Octaver]) do
          Value := Value + GetPerling(XC / Scale, YC / Scale);

      // value = [-0,5 .. 0,5]
      Posi^ := Round(Value * 256 * 100);
      // 100 instead of 128 to keep it well in range!

      If MaxValue < Value then
        MaxValue := Value;

      If MinValue > Value then
        MinValue := Value;

      If Value > 1.0 then
      begin
        S := '';
        For Octaver := 0 to FNumber_Of_Octaves - 1 do
          With TGL2DPerlinOctav(Octaves[Octaver]) do
            S := S + FloatToStr(GetPerling(XC / Scale, YC / Scale)) + ' ,';
        Delete(S, Length(S) - 1, 2);
        // raise Exception.create('In Cubic_Interpolate_Strip a value greater than 1 occured! value = '+FloatToStr(Value)+' values=['+S+']');
      end;

      inc(Posi);
    end;
  end;
end;

constructor TGLPerlinHDS.Create(AOwner: TComponent);

begin
  inherited;
  FLines := TStringList.Create;
  FInterpolation := pi_Cosine;
  FSmoothing := pi_cubic;
  FPersistence := 0.4;
  FNumber_Of_Octaves := 6;
  MaxValue := -MaxInt;
  MinValue := MaxInt;
  MaxThreads := 1;
end;

procedure TGLPerlinHDS.StartPreparingData(heightData: TGLHeightData);

var
  Perlin: TGL2DPerlin;
  Thread: TGLPerlinHDSThread;

begin
  If Stall then
    heightData.DataState := hdsNone
  else
    heightData.DataState := hdsPreparing;
  Perlin := TGL2DPerlin.Create(self);
  Perlin.Width := heightData.Size;
  Perlin.Height := heightData.Size;
  Perlin.XStart := heightData.XLeft + XStart;
  Perlin.YStart := heightData.YTop + YStart;

  Perlin.Interpolation := Interpolation;
  Perlin.Smoothing := Smoothing;
  Perlin.Persistence := Persistence;
  Perlin.Number_Of_Octaves := Number_Of_Octaves;

  If MaxThreads > 1 then
  begin
    Thread := TGLPerlinHDSThread.Create(True);
    Thread.FreeOnTerminate := True;
    heightData.Thread := Thread;
    Thread.FHeightData := HeightData;
    Thread.Perlin := Perlin;
    Thread.PerlinSource := self;
    Thread.Start;
  End
  else
  begin
    Perlin.Generate;
    Perlin.SetHeightData(heightData);
    heightData.DataState := hdsReady;

    If MaxValue < Perlin.MaxValue then
      MaxValue := Perlin.MaxValue;

    If MinValue < Perlin.MinValue then
      MinValue := Perlin.MinValue;
    Perlin.Free;
  end;

  Lines.Add('Prepared Perlin (' + IntToStr(Perlin.XStart) + ',' +
    IntToStr(Perlin.YStart) + ') size ' + IntToStr(Perlin.Width));
  LinesChanged := True;
end;

procedure TGLPerlinHDS.WaitFor;

var
  HDList: TList;
  HD: TGLHeightData;
  XC: Integer;
begin
  repeat
    HDList := Data.LockList;
    try
      HD := Nil;
      For XC := 0 to HDList.Count - 1 do
      begin
        HD := TGLHeightData(HDList[XC]);
        If HD.DataState <> hdsReady then
          Break;
      end;
      If Assigned(HD) then
        If HD.DataState = hdsReady then
          Break;
    finally
      Data.UnlockList;
    end;
    Sleep(10);
  until False;
end;

procedure TGLPerlinHDSThread.Execute;

begin
  Perlin.Generate;
  Perlin.SetHeightData(FHeightData);
  FHeightData.DataState := hdsReady;

  If PerlinSource.MaxValue < Perlin.MaxValue then
    PerlinSource.MaxValue := Perlin.MaxValue;

  If PerlinSource.MinValue < Perlin.MinValue then
    PerlinSource.MinValue := Perlin.MinValue;
  Perlin.Free;
end;

procedure TGLPerlinHDSThread.OpdateOutSide;
begin
end;

//-----------------------------------------------
initialization
//-----------------------------------------------

RegisterClasses([TGLPerlinHDS]);

end.
