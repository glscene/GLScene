//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.ProcTextures;

(* Procedural textures *)

interface

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  
  GLS.Texture,
  GLS.Graphics,
  GLS.TextureFormat,
  GLS.VectorGeometry;

const
  GRADIENT_TABLE_SIZE = 256;
  DAMP_SHIFT = 20;

type
  TGLProcTextureNoise = class(TGLTextureImage)
  private
    FNoiseMap: TGLBitmap32;
    FWidth, FHeight: Integer;
    FMinCut: Byte;
    FNoiseSharpness: Single;
    FNoiseAnimate: Single;
    FSeamless: Boolean;
    FNoiseRandSeed: Longint;
  protected
    FGradients: array[0..GRADIENT_TABLE_SIZE * 3 - 1] of Single;
    PERM: array[0..GRADIENT_TABLE_SIZE - 1] of Byte;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetDepth: Integer; override;
    function GetTextureTarget: TGLTextureTarget; override;
    function Noise(x, y: Single): Single;
    procedure SetMinCut(const val: Byte);
    procedure SetSeamless(const val: Boolean);
    procedure SetWidth(const val: Integer);
    procedure SetHeight(const val: Integer);
    procedure SetNoiseSharpness(const val: Single);
    procedure SetNoiseRandSeed(const val: Longint);
    procedure UpdateNoise;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    procedure Assign(Source: TPersistent); override;
    function GetBitmap32: TGLBitmap32; override;
    procedure ReleaseBitmap32; override;
    procedure SaveToFile(const fileName: string); override;
    procedure LoadFromFile(const fileName: string); override;
    procedure NoiseAnimate(speed: Single);
    procedure SetPermFromData(const inPERM: array of Byte);
    procedure SetPermToDefault;
  published
    property Width: Integer read GetWidth write SetWidth default 128;
    property Height: Integer read GetHeight write SetHeight default 128;
    property Depth: Integer read GetDepth;
    property MinCut: Byte read FMinCut write SetMinCut;
    property NoiseSharpness: Single read FNoiseSharpness write
      SetNoiseSharpness;
    property Seamless: Boolean read FSeamless write SetSeamless;
    property NoiseRandSeed: Longint read FNoiseRandSeed write SetNoiseRandSeed;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

constructor TGLProcTextureNoise.Create(AOwner: TPersistent);
{ PERM array Borrowed from Darwyn Peachey.
  The gradient table is indexed with an XYZ triplet, which is first turned
  into a single random index using a lookup in PERM array. The PERM array simply
  contains all numbers in [0..255] in random order. }
//Can now be set to a different set of Random arrangement
var
  i: Integer;
  seedBackup: Longint;
  z, r, theta: Single;
begin
  inherited;
  FWidth := 128;
  FHeight := 128;
  FMinCut := 0;
  FNoiseSharpness := 0.99;
  FSeamless := False;
  seedBackup := RandSeed;
  Randomize;
  FNoiseRandSeed := Random(2147483647); //Random(10000);
  RandSeed := FNoiseRandSeed;
  SetPermToDefault;
  // Generate random gradient vectors.
  for i := 0 to GRADIENT_TABLE_SIZE - 1 do
  begin
    z := 1 - 2 * Random;
    r := sqrt(1 - z * z);
    theta := 2 * PI * Random;
    FGradients[i * 3] := r * cos(theta);
    FGradients[i * 3 + 1] := r * sin(theta);
    FGradients[i * 3 + 2] := z;
  end;
  RandSeed := seedBackup;
end;

destructor TGLProcTextureNoise.Destroy;
begin
  ReleaseBitmap32;
  inherited;
end;

procedure TGLProcTextureNoise.UpdateNoise;
var
  X, Y, C: Integer;
  Line: PGLPixel32Array;
  nf: Single;
  n: Byte;

  function NoiseSeamless(Scale: Single): Single;
  begin
    Result := (Noise(x / Scale, y / Scale) * (Width - x) * (Height - y)
      + Noise((x - width) / Scale, y / Scale) * x * (Height - y)
      + Noise((x - width) / Scale, (y - Height) / Scale) * x * y
      + Noise(x / Scale, (y - Height) / Scale) * (Width - x) * y)
      / (Width * Height);
  end;

begin
  // Update the noise texture.
  for y := 0 to FNoiseMap.Height - 1 do
  begin
    Line := FNoiseMap.ScanLine[y];
    for x := 0 to FNoiseMap.Width - 1 do
    begin

      case FSeamless of
        // Take 4 octaves of noise and add them weighted for seamless.
        // uses much Ghz
        True:
          begin
            nf := NoiseSeamless(16)
              + NoiseSeamless(8) / 2
              + NoiseSeamless(4) / 4
              + NoiseSeamless(2) / 8;
          end;
        // Take 4  octaves of noise and add them.
        False:
          begin
            nf := Noise(x / 16, y / 16)
              + Noise(x / 8, y / 8) / 2
              + Noise(x / 4, y / 4) / 4
              + Noise(x / 2, y / 2) / 8;
          end;
      end;

      // Range between 0 and 255
      n := Round(255 * (nf + 1) / 2);
      if MinCut > 0 then
      begin
        // Min Cut
        C := n - FMinCut;
        if C < 0 then
          n := 0
        else
          // Noise Sharpness
          n := 255 - Round(PowerInteger(FNoiseSharpness, C) * 255);
      end;
      //if n < 13 then n:=13;
      // Write the result to the texture image.
      Line^[x].r := n;
      Line^[x].g := n;
      Line^[x].b := n;
      Line^[x].a := n;
    end;
  end;
end;

function TGLProcTextureNoise.GetBitmap32: TGLBitmap32;
begin
  if not Assigned(FNoiseMap) then
  begin
    FNoiseMap := TGLBitmap32.Create;
    FNoiseMap.Width := FWidth;
    FNoiseMap.Height := FHeight;
    FNoiseMap.Blank := false;
    UpdateNoise;
  end;
  Result := FNoiseMap;
end;

 
class function TGLProcTextureNoise.FriendlyName: string;
begin
  Result := 'Procedural Noise';
end;


class function TGLProcTextureNoise.FriendlyDescription: string;
begin
  Result := 'Procedural Noise (Animated)';
end;

procedure TGLProcTextureNoise.SetSeamless(const val: Boolean);
begin
  if val <> FSeamless then
  begin
    FSeamless := val;
    Invalidate;
  end;
end;

procedure TGLProcTextureNoise.LoadFromFile(const fileName: string);
begin
  Assert(False, 'TGLProcTextureNoise.LoadFromFile not implemented');
end;

procedure TGLProcTextureNoise.ReleaseBitmap32;
begin
  if Assigned(FNoiseMap) then
  begin
    FNoiseMap.Free;
    FNoiseMap := nil;
  end;
end;

procedure TGLProcTextureNoise.SaveToFile(const fileName: string);
begin
  {Nothing here}
end;

function TGLProcTextureNoise.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TGLProcTextureNoise.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TGLProcTextureNoise.GetDepth: Integer;
begin
  Result := 1;
end;


function TGLProcTextureNoise.GetTextureTarget: TGLTextureTarget;
begin
  Result := ttTexture2D;
end;

procedure TGLProcTextureNoise.SetHeight(const val: Integer);
begin
  if val <> FHeight then
  begin
    FHeight := val;
    if FHeight < 1 then
      FHeight := 1;
    Invalidate;
  end;
end;

procedure TGLProcTextureNoise.SetWidth(const val: Integer);
begin
  if val <> FWidth then
  begin
    FWidth := val;
    if FWidth < 1 then
      FWidth := 1;
    Invalidate;
  end;
end;

procedure TGLProcTextureNoise.SetMinCut(const val: Byte);
begin
  if val <> FMinCut then
  begin
    FMinCut := val;
    Invalidate;
  end;
end;

procedure TGLProcTextureNoise.SetNoiseSharpness(const val: Single);
begin
  if val <> FNoiseSharpness then
  begin
    FNoiseSharpness := val;
    if FNoiseSharpness > 1 then
      FNoiseSharpness := 1;
    Invalidate;
  end;
end;

procedure TGLProcTextureNoise.SetNoiseRandSeed(const val: Longint);
var
  i: Integer;
  seedBackup: Longint;
  z, r, theta: Single;
begin
  if val <> FNoiseRandSeed then
  begin
    seedBackup := RandSeed;
    FNoiseRandSeed := val;
    //Dunno, might be ok to be negative
    if FNoiseRandSeed < 1 then
      FNoiseRandSeed := 1;
    RandSeed := FNoiseRandSeed;
    //didnt change so added/copied FGradients here... to get Seed to work
    // Generate random gradient vectors.
    for i := 0 to GRADIENT_TABLE_SIZE - 1 do
    begin
      z := 1 - 2 * Random;
      r := sqrt(1 - z * z);
      theta := 2 * PI * Random;
      FGradients[i * 3] := r * cos(theta);
      FGradients[i * 3 + 1] := r * sin(theta);
      FGradients[i * 3 + 2] := z;
    end;
    RandSeed := seedBackup;
    Invalidate;
  end;
end;

procedure TGLProcTextureNoise.Assign(Source: TPersistent);
begin
  if Assigned(Source) and (Source is TGLProcTextureNoise) then
  begin
    FWidth := TGLProcTextureNoise(Source).FWidth;
    FHeight := TGLProcTextureNoise(Source).FHeight;
    FMinCut := TGLProcTextureNoise(Source).FMinCut;
    FNoiseSharpness := TGLProcTextureNoise(Source).FNoiseSharpness;
    FNoiseRandSeed := TGLProcTextureNoise(Source).FNoiseRandSeed;
    Invalidate;
  end
  else
    inherited;
end;

procedure TGLProcTextureNoise.NoiseAnimate(speed: Single);
begin
  FNoiseAnimate := FNoiseAnimate + speed;
  Invalidate;
end;

function TGLProcTextureNoise.Noise(x, y: Single): Single;
var
  ix, iy, iz: Integer;
  fx0, fx1, fy0, fy1, fz0, fz1: Single;
  wx, wy, wz: Single;
  vx0, vx1, vy0, vy1, vz0, vz1: Single;

  function Smooth(x: Single): Single;
  begin
    { Smoothing curve. This is used to calculate interpolants so that the noise
      doesn't look blocky when the frequency is low. }
    Result := x * x * (3 - 2 * x);
  end;

  function Permutate(x: Integer): Integer;
  const
    MASK = GRADIENT_TABLE_SIZE - 1;
  begin
    // Do a lookup in the permutation table.
    Result := PERM[x and MASK];
  end;

  function Index(ix, iy, iz: Integer): Integer;
  begin
    // Turn an XYZ triplet into a single gradient table index.
    Result := Permutate(ix + Permutate(iy + Permutate(iz)));
  end;

  function Lattice(lx, ly, lz: Integer; fx, fy, fz: Single): Single;
  var
    g: Integer;
  begin
    // Look up a random gradient at [ix,iy,iz] and dot it with the [fx,fy,fz] vector.
    g := Index(lx, ly, lz) * 3;
    Result := FGradients[g] * fx + FGradients[g + 1] * fy + FGradients[g + 2] *
      fz;
  end;

  function Lerp(t, x0, x1: Single): Single;
  begin
    // Simple linear interpolation.
    Result := x0 + t * (x1 - x0);
  end;

begin
  { The main noise function. Looks up the pseudorandom gradients at the nearest
    lattice points, dots them with the input vector, and interpolates the
    results to produce a single output value in [0, 1] range. }

  ix := Floor(x);
  fx0 := x - ix;
  fx1 := fx0 - 1;
  wx := Smooth(fx0);
  iy := Floor(y);
  fy0 := y - iy;
  fy1 := fy0 - 1;
  wy := Smooth(fy0);

  iz := Floor(FNoiseAnimate);
  fz0 := FNoiseAnimate - iz;
  fz1 := fz0 - 1;
  wz := Smooth(fz0);

  vx0 := Lattice(ix, iy, iz, fx0, fy0, fz0);
  vx1 := Lattice(ix + 1, iy, iz, fx1, fy0, fz0);
  vy0 := Lerp(wx, vx0, vx1);

  vx0 := Lattice(ix, iy + 1, iz, fx0, fy1, fz0);
  vx1 := Lattice(ix + 1, iy + 1, iz, fx1, fy1, fz0);
  vy1 := Lerp(wx, vx0, vx1);

  vz0 := Lerp(wy, vy0, vy1);

  vx0 := Lattice(ix, iy, iz + 1, fx0, fy0, fz1);
  vx1 := Lattice(ix + 1, iy, iz + 1, fx1, fy0, fz1);
  vy0 := Lerp(wx, vx0, vx1);

  vx0 := Lattice(ix, iy + 1, iz + 1, fx0, fy1, fz1);
  vx1 := Lattice(ix + 1, iy + 1, iz + 1, fx1, fy1, fz1);
  vy1 := Lerp(wx, vx0, vx1);

  vz1 := Lerp(wy, vy0, vy1);

  Result := Lerp(wz, vz0, vz1);
end;

procedure TGLProcTextureNoise.SetPermFromData(const inPERM: array of Byte);
var
  I: Integer;
begin
  for I := 0 to 255 do
    PERM[I] := inPERM[I];
  Invalidate;
end;

procedure TGLProcTextureNoise.SetPermToDefault;
begin
  //225,155,210,108,175,199,221,144,203,116, 70,213, 69,158, 33,252,
  PERM[0] := 225;
  PERM[1] := 155;
  PERM[2] := 210;
  PERM[3] := 108;
  PERM[4] := 175;
  PERM[5] := 199;
  PERM[6] := 221;
  PERM[7] := 144;
  PERM[8] := 203;
  PERM[9] := 116;
  PERM[10] := 70;
  PERM[11] := 213;
  PERM[12] := 69;
  PERM[13] := 158;
  PERM[14] := 33;
  PERM[15] := 252;
  //5, 82,173,133,222,139,174, 27,  9, 71, 90,246, 75,130, 91,191,
  PERM[16] := 5;
  PERM[17] := 82;
  PERM[18] := 173;
  PERM[19] := 133;
  PERM[20] := 222;
  PERM[21] := 139;
  PERM[22] := 174;
  PERM[23] := 27;
  PERM[24] := 9;
  PERM[25] := 71;
  PERM[26] := 90;
  PERM[27] := 246;
  PERM[28] := 75;
  PERM[29] := 130;
  PERM[30] := 91;
  PERM[31] := 191;
  //169,138,  2,151,194,235, 81,  7, 25,113,228,159,205,253,134,142,
  PERM[32] := 169;
  PERM[33] := 138;
  PERM[34] := 2;
  PERM[35] := 151;
  PERM[36] := 194;
  PERM[37] := 235;
  PERM[38] := 81;
  PERM[39] := 7;
  PERM[40] := 25;
  PERM[41] := 113;
  PERM[42] := 228;
  PERM[43] := 159;
  PERM[44] := 205;
  PERM[45] := 253;
  PERM[46] := 134;
  PERM[47] := 142;
  //248, 65,224,217, 22,121,229, 63, 89,103, 96,104,156, 17,201,129,
  PERM[48] := 248;
  PERM[49] := 65;
  PERM[50] := 224;
  PERM[51] := 217;
  PERM[52] := 22;
  PERM[53] := 121;
  PERM[54] := 229;
  PERM[55] := 63;
  PERM[56] := 89;
  PERM[57] := 103;
  PERM[58] := 96;
  PERM[59] := 104;
  PERM[60] := 156;
  PERM[61] := 17;
  PERM[62] := 201;
  PERM[63] := 129;
  //36,  8,165,110,237,117,231, 56,132,211,152, 20,181,111,239,218,
  PERM[64] := 36;
  PERM[65] := 8;
  PERM[66] := 165;
  PERM[67] := 110;
  PERM[68] := 237;
  PERM[69] := 117;
  PERM[70] := 231;
  PERM[71] := 56;
  PERM[72] := 132;
  PERM[73] := 211;
  PERM[74] := 152;
  PERM[75] := 20;
  PERM[76] := 181;
  PERM[77] := 111;
  PERM[78] := 239;
  PERM[79] := 218;
  // 170,163, 51,172,157, 47, 80,212,176,250, 87, 49, 99,242,136,189,
  PERM[80] := 170;
  PERM[81] := 163;
  PERM[82] := 51;
  PERM[83] := 172;
  PERM[84] := 157;
  PERM[85] := 47;
  PERM[86] := 80;
  PERM[86] := 212;
  PERM[88] := 176;
  PERM[89] := 250;
  PERM[90] := 87;
  PERM[91] := 49;
  PERM[92] := 99;
  PERM[93] := 242;
  PERM[94] := 136;
  PERM[95] := 189;
  //162,115, 44, 43,124, 94,150, 16,141,247, 32, 10,198,223,255, 72,
  PERM[96] := 162;
  PERM[97] := 115;
  PERM[98] := 44;
  PERM[99] := 43;
  PERM[100] := 124;
  PERM[101] := 94;
  PERM[102] := 150;
  PERM[103] := 16;
  PERM[104] := 141;
  PERM[105] := 247;
  PERM[106] := 32;
  PERM[107] := 10;
  PERM[108] := 198;
  PERM[109] := 223;
  PERM[110] := 255;
  PERM[111] := 72;
  //53,131, 84, 57,220,197, 58, 50,208, 11,241, 28,  3,192, 62,202,
  PERM[112] := 53;
  PERM[113] := 131;
  PERM[114] := 84;
  PERM[115] := 57;
  PERM[116] := 220;
  PERM[117] := 197;
  PERM[118] := 58;
  PERM[119] := 50;
  PERM[120] := 208;
  PERM[121] := 11;
  PERM[122] := 241;
  PERM[123] := 28;
  PERM[124] := 3;
  PERM[125] := 192;
  PERM[126] := 62;
  PERM[127] := 202;
  //18,215,153, 24, 76, 41, 15,179, 39, 46, 55,  6,128,167, 23,188,
  PERM[128] := 18;
  PERM[129] := 215;
  PERM[130] := 153;
  PERM[131] := 24;
  PERM[132] := 76;
  PERM[133] := 41;
  PERM[134] := 15;
  PERM[135] := 179;
  PERM[136] := 39;
  PERM[137] := 46;
  PERM[138] := 55;
  PERM[139] := 6;
  PERM[140] := 128;
  PERM[141] := 167;
  PERM[142] := 23;
  PERM[143] := 188;
  // 106, 34,187,140,164, 73,112,182,244,195,227, 13, 35, 77,196,185,
  PERM[144] := 106;
  PERM[145] := 34;
  PERM[146] := 187;
  PERM[147] := 140;
  PERM[148] := 164;
  PERM[149] := 73;
  PERM[150] := 112;
  PERM[151] := 182;
  PERM[152] := 244;
  PERM[153] := 195;
  PERM[154] := 227;
  PERM[155] := 13;
  PERM[156] := 35;
  PERM[157] := 77;
  PERM[158] := 196;
  PERM[159] := 185;
  //26,200,226,119, 31,123,168,125,249, 68,183,230,177,135,160,180,
  PERM[160] := 26;
  PERM[161] := 200;
  PERM[162] := 226;
  PERM[163] := 119;
  PERM[164] := 31;
  PERM[165] := 123;
  PERM[166] := 168;
  PERM[167] := 125;
  PERM[168] := 249;
  PERM[169] := 68;
  PERM[170] := 183;
  PERM[171] := 230;
  PERM[172] := 177;
  PERM[173] := 135;
  PERM[174] := 160;
  PERM[175] := 180;
  // 12,  1,243,148,102,166, 38,238,251, 37,240,126, 64, 74,161, 40,
  PERM[176] := 12;
  PERM[177] := 1;
  PERM[178] := 243;
  PERM[179] := 148;
  PERM[180] := 102;
  PERM[181] := 166;
  PERM[182] := 38;
  PERM[183] := 238;
  PERM[184] := 251;
  PERM[185] := 37;
  PERM[186] := 240;
  PERM[187] := 126;
  PERM[188] := 64;
  PERM[189] := 74;
  PERM[190] := 161;
  PERM[191] := 40;
  // 184,149,171,178,101, 66, 29, 59,146, 61,254,107, 42, 86,154,  4,
  PERM[192] := 184;
  PERM[193] := 149;
  PERM[194] := 171;
  PERM[195] := 178;
  PERM[196] := 101;
  PERM[197] := 66;
  PERM[198] := 29;
  PERM[199] := 59;
  PERM[200] := 146;
  PERM[201] := 61;
  PERM[202] := 254;
  PERM[203] := 107;
  PERM[204] := 42;
  PERM[205] := 86;
  PERM[206] := 154;
  PERM[207] := 4;
  //  236,232,120, 21,233,209, 45, 98,193,114, 78, 19,206, 14,118,127,
  PERM[208] := 236;
  PERM[209] := 232;
  PERM[210] := 120;
  PERM[211] := 21;
  PERM[212] := 233;
  PERM[213] := 209;
  PERM[214] := 45;
  PERM[215] := 98;
  PERM[216] := 193;
  PERM[217] := 114;
  PERM[218] := 78;
  PERM[219] := 19;
  PERM[220] := 206;
  PERM[221] := 14;
  PERM[222] := 118;
  PERM[223] := 127;
  // 48, 79,147, 85, 30,207,219, 54, 88,234,190,122, 95, 67,143,109,
  PERM[224] := 48;
  PERM[225] := 79;
  PERM[226] := 147;
  PERM[227] := 85;
  PERM[228] := 30;
  PERM[229] := 207;
  PERM[230] := 219;
  PERM[231] := 54;
  PERM[232] := 88;
  PERM[233] := 234;
  PERM[234] := 190;
  PERM[235] := 122;
  PERM[236] := 95;
  PERM[237] := 67;
  PERM[238] := 143;
  PERM[239] := 109;
  // 137,214,145, 93, 92,100,245,  0,216,186, 60, 83,105, 97,204, 52
  PERM[240] := 137;
  PERM[241] := 214;
  PERM[242] := 145;
  PERM[243] := 93;
  PERM[244] := 92;
  PERM[245] := 100;
  PERM[246] := 245;
  PERM[247] := 0;
  PERM[248] := 216;
  PERM[249] := 186;
  PERM[250] := 60;
  PERM[251] := 83;
  PERM[252] := 105;
  PERM[253] := 97;
  PERM[254] := 204;
  PERM[255] := 52;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterGLTextureImageClass(TGLProcTextureNoise);

finalization

end.

