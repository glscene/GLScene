unit GBE.Terrain;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math.Vectors,
  System.RTLConsts,

  FMX.Types,
  FMX.Types3D,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Graphics,
  Math,
  uGBEUtils3D;

type
  TGBETerrain = class(TMesh)
  private
    fAmplitude, fRoughness, fScalling: single;
    fOctaves, fSubdivX, fSubdivZ: integer;
    fSeed, fXOffset, fZOffset: integer;
    FUseRamp: boolean;
    function GetInterpolatedNoise(X, Z: single): single;
    function Interpolate(a, b, blend: single): single;
    function Noise(X, Z: integer): single;
    function SmoothNoise(X, Z: integer): single;
    procedure SetUseRamp(const Value: boolean);
    function GenerateHeight(X, Z: integer): single;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clean;
    procedure GenerateTerrain;
    function GetHeight(P: TPoint3d): single;
  published
    property Amplitude: single read fAmplitude write fAmplitude;
    property Roughness: single read fRoughness write fRoughness;
    property Octaves: integer read fOctaves write fOctaves;
    property XOffset: integer read fXOffset write fXOffset;
    property ZOffset: integer read fZOffset write fZOffset;
    property Seed: integer read fSeed write fSeed;
    property SubdivX: integer read fSubdivX write fSubdivX;
    property SubdivZ: integer read fSubdivZ write fSubdivZ;
    property UseRamp: boolean read FUseRamp write SetUseRamp;

  end;

procedure Register;

implementation //--------------------------------------------------------------

// TGBETerrain

function TGBETerrain.GenerateHeight(X, Z: integer): single;
begin
  var
  total := 0.0;
  var
  d := Math.Power(2, fOctaves - 1);

  for var i := 0 to fOctaves - 1 do
  begin
    var
    freq := Math.Power(2, i) / d;
    var
    amp := Math.Power(fRoughness, i) * fAmplitude;
    total := total + GetInterpolatedNoise((X + fXOffset) * freq,
      (Z + fZOffset) * freq) * amp;
  end;

  result := total;
end;

(*
  GetInterpolatedNoise return an interpolate value for the height
  of a position at x and z coordinates
*)
function TGBETerrain.GetInterpolatedNoise(X, Z: single): single;
begin
  var
    intX: integer := trunc(X);
  var
    intZ: integer := trunc(Z);
  var
  fracX := X - intX;
  var
  fracZ := Z - intZ;

  // use the near neighbours points v1, v2, v3, v4
  var
  v1 := SmoothNoise(intX, intZ);
  var
  v2 := SmoothNoise(intX + 1, intZ);
  var
  v3 := SmoothNoise(intX, intZ + 1);
  var
  v4 := SmoothNoise(intX + 1, intZ + 1);
  (*
    X is the point with x,z coordinates
    v1--------i1---v2
    |         .    |
    |         X    |
    |         .    |
    |         .    |
    |         .    |
    v3--------i2---v4
  *)
  var
  i1 := Interpolate(v1, v2, fracX);
  var
  i2 := Interpolate(v3, v4, fracX);
  // result interpolate i1 and i2
  result := Interpolate(i1, i2, fracZ);
end;

(*
  Cosine interpolation to be more natural
  return an interpolate value between 2 values a and b
*)
function TGBETerrain.Interpolate(a, b, blend: single): single;
begin
  var
  theta := blend * PI;
  var
  f := (1.0 - cos(theta)) * 0.5;
  result := a * (1.0 - f) + b * f;
end;

// smoothNoise use the noise function and the neighbours vertices from a specific vertex
function TGBETerrain.SmoothNoise(X, Z: integer): single;
begin
  var
  corners := (Noise(X - 1, Z - 1) + Noise(X + 1, Z - 1) + Noise(X - 1, Z + 1) +
    Noise(X + 1, Z + 1)) * 0.125;
  var
  sides := (Noise(X - 1, Z) + Noise(X + 1, Z) + Noise(X, Z - 1) + Noise(X,
    Z + 1)) * 0.25;
  var
  center := Noise(X, Z) * 0.5;
  result := corners + sides + center;
end;

// Nose function is a pure function to return a random number between -1 and 1
function TGBETerrain.Noise(X, Z: integer): single;
begin
  randSeed := X * 9158 + Z * 41765 + fSeed; // seed value for random
  result := random * 2.0 - 1.0;
  // random return a number between 0 and 1 and we want a number between -1 and 1
end;

procedure TGBETerrain.Clean;
begin
  Data.Clear;
end;

constructor TGBETerrain.Create(AOwner: TComponent);
begin
  inherited;
  fSeed := random(9999999);
  fXOffset := 0;
  fZOffset := 0;
  UseRamp := false;
  HitTest := false;
end;

destructor TGBETerrain.Destroy;
begin
  inherited;
end;

// generate procedural terrain
procedure TGBETerrain.GenerateTerrain;
begin
  var
  NP := 0;
  var
  NI := 0;
  var
  yMin := 0.0;
  var
  yMax := 0.0;
  var
    vertexArray: TArray<TPoint3d>;;

  fOctaves := Octaves;
  fAmplitude := Amplitude;
  fRoughness := Roughness;

  try
    Data.VertexBuffer.Length := Round(SubdivX * SubdivZ * 4);
    setLength(vertexArray, Data.VertexBuffer.Length);
    Data.IndexBuffer.Length := Round(SubdivX * SubdivZ * 6);

    // Initialize vertexArray and compute Y for each vertex
    var
    v := 0.0;
    while v < SubdivZ do
    begin
      var
      u := 0.0;
      while u < SubdivX do
      begin
        vertexArray[NP + 0].X := u;
        vertexArray[NP + 0].Z := v;
        vertexArray[NP + 0].Y :=
          GenerateHeight(trunc(vertexArray[NP + 0].X + fXOffset),
          trunc(vertexArray[NP + 0].Z + fZOffset));
        if vertexArray[NP + 0].Y < yMin then
          yMin := vertexArray[NP + 0].Y;
        if vertexArray[NP + 0].Y > yMax then
          yMax := vertexArray[NP + 0].Y;

        vertexArray[NP + 1].X := u + 1;
        vertexArray[NP + 1].Z := v;
        vertexArray[NP + 1].Y :=
          GenerateHeight(trunc(vertexArray[NP + 1].X + fXOffset),
          trunc(vertexArray[NP + 1].Z + fZOffset));
        if vertexArray[NP + 1].Y < yMin then
          yMin := vertexArray[NP + 1].Y;
        if vertexArray[NP + 1].Y > yMax then
          yMax := vertexArray[NP + 1].Y;

        vertexArray[NP + 2].X := u + 1;
        vertexArray[NP + 2].Z := v + 1;
        vertexArray[NP + 2].Y :=
          GenerateHeight(trunc(vertexArray[NP + 2].X + fXOffset),
          trunc(vertexArray[NP + 2].Z + fZOffset));
        if vertexArray[NP + 2].Y < yMin then
          yMin := vertexArray[NP + 2].Y;
        if vertexArray[NP + 2].Y > yMax then
          yMax := vertexArray[NP + 2].Y;

        vertexArray[NP + 3].X := u;
        vertexArray[NP + 3].Z := v + 1;
        vertexArray[NP + 3].Y :=
          GenerateHeight(trunc(vertexArray[NP + 3].X + fXOffset),
          trunc(vertexArray[NP + 3].Z + fZOffset));
        if vertexArray[NP + 3].Y < yMin then
          yMin := vertexArray[NP + 3].Y;
        if vertexArray[NP + 3].Y > yMax then
          yMax := vertexArray[NP + 3].Y;

        NP := NP + 4;
        u := u + 1;
      end;
      v := v + 1;
    end;

    if yMax - yMin > 0 then
      fScalling := self.Height / (yMax - yMin)
    else
      fScalling := 1;

    var
    heightToColor := 255 / (abs(yMin) + abs(yMax)) / 255;

    var
    i := 0;
    NP := 0;
    while i < Length(vertexArray) - 3 do
    begin
      with Data do
      begin
        with VertexBuffer do
        begin
          Vertices[NP + 0] := vertexArray[i + 0];
          Vertices[NP + 1] := vertexArray[i + 1];
          Vertices[NP + 2] := vertexArray[i + 2];
          Vertices[NP + 3] := vertexArray[i + 3];

          if UseRamp then
          begin
            TexCoord0[NP + 0] := PointF((vertexArray[i + 0].Y + abs(yMin)) *
              heightToColor, 0);
            TexCoord0[NP + 1] := PointF((vertexArray[i + 1].Y + abs(yMin)) *
              heightToColor, 0);
            TexCoord0[NP + 2] := PointF((vertexArray[i + 2].Y + abs(yMin)) *
              heightToColor, 0);
            TexCoord0[NP + 3] := PointF((vertexArray[i + 3].Y + abs(yMin)) *
              heightToColor, 0);
          end
          else
          begin
            TexCoord0[NP + 0] := PointF((vertexArray[i + 0].X) / SubdivX,
              (vertexArray[i + 0].Z) / SubdivZ);
            TexCoord0[NP + 1] := PointF((vertexArray[i + 1].X) / SubdivX,
              (vertexArray[i + 1].Z) / SubdivZ);
            TexCoord0[NP + 2] := PointF((vertexArray[i + 2].X) / SubdivX,
              (vertexArray[i + 2].Z) / SubdivZ);
            TexCoord0[NP + 3] := PointF((vertexArray[i + 3].X) / SubdivX,
              (vertexArray[i + 3].Z) / SubdivZ);
          end;
        end;

        IndexBuffer[NI + 0] := NP + 0;
        IndexBuffer[NI + 1] := NP + 1;
        IndexBuffer[NI + 2] := NP + 3;
        IndexBuffer[NI + 3] := NP + 3;
        IndexBuffer[NI + 4] := NP + 1;
        IndexBuffer[NI + 5] := NP + 2;
      end;

      NP := NP + 4;
      NI := NI + 6;
      inc(i, 4);
    end;

    Data.CalcTangentBinormals;

  finally
  end;
end;

procedure TGBETerrain.SetUseRamp(const Value: boolean);
begin
  if Value <> FUseRamp then
    FUseRamp := Value;
end;

function TGBETerrain.GetHeight(P: TPoint3d): single;
begin
  result := CalculateHeight(self, P, fScalling, fSubdivX, fSubdivZ);
end;

//---------------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBETerrain]);
end;

end.
