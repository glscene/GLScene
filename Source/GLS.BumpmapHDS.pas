//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.BumpmapHDS;

(*
  Implements a HDS that automatically generates an elevation bumpmap.
  The object-space elevation bumpmap can be used for dynamic terrain lighting.
  A bumpmap texture is generated for each terrain tile, and placed into a TGLMaterialLibrary.
*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.UITypes,
  Vcl.Graphics,

  GLS.OpenGLTokens,
  GLS.Coordinates,
  GLS.HeightData,
  GLS.Graphics,
  GLS.Color,
  GLS.VectorTypes,
  GLS.VectorLists,
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.Material,
  GLS.Utils;

type

  TGLNormalMapSpace = (nmsObject, nmsTangent);

  TGLBumpmapHDS = class;

  TNewTilePreparedEvent = procedure(Sender: TGLBumpmapHDS;
    heightData: TGLHeightData; normalMapMaterial: TGLLibMaterial) of object;

  (*  An Height Data Source that generates elevation bumpmaps automatically.
    The HDS must be connected to another HDS, which will provide the elevation
    data, and to a MaterialLibrary where bumpmaps will be placed. *)
  TGLBumpmapHDS = class(TGLHeightDataSourceFilter)
  private
    // FElevationHDS : TGLHeightDataSource;
    FBumpmapLibrary: TGLMaterialLibrary;
    FOnNewTilePrepared: TNewTilePreparedEvent;
    FBumpScale: Single;
    FSubSampling: Integer;
    FMaxTextures: Integer;
    Uno: TCriticalSection;
  protected
    procedure SetBumpmapLibrary(const val: TGLMaterialLibrary);
    procedure SetBumpScale(const val: Single);
    function StoreBumpScale: Boolean;
    procedure SetSubSampling(const val: Integer);
    procedure Trim(MaxTextureCount: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Release(aHeightData: TGLHeightData); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure GenerateNormalMap(heightData: TGLHeightData; normalMap: TGLImage;
      scale: Single);
	(* This will repeatedly delete the oldest unused texture from the TGLMaterialLibrary,
      until the texture count drops to MaxTextureCount.
      DONT use this if you used TGLHeightData.MaterialName to link your terrain textures.
      Either use with TGLHeightData.LibMaterial, or manually delete unused Normal-Map textures.*)
    procedure TrimTextureCache(MaxTextureCount: Integer);
    // procedure  TileTextureCoordinates(heightData : TGLHeightData; TextureScale:TTexPoint; TextureOffset:TTexPoint);
    procedure PreparingData(heightData: TGLHeightData); override;
  published
    property BumpmapLibrary: TGLMaterialLibrary read FBumpmapLibrary
      write SetBumpmapLibrary;
    property OnNewTilePrepared: TNewTilePreparedEvent read FOnNewTilePrepared
      write FOnNewTilePrepared;
    property BumpScale: Single read FBumpScale write SetBumpScale
      stored StoreBumpScale;
    (* Specifies the amount of subsampling for the bump texture.
      This value must be a power of 2, and is used to divide the height
      tile resolution to determine the bump texture resolution (f.i.
      a tile size of 128 with a subsampling of 4 will result in textures
      of a resolution of 32x32. SubSampling won't allow texture resolution
      to get below 16x16 (minimal bumpmap resolution). *)
    property SubSampling: Integer read FSubSampling write SetSubSampling
      default 1;
    property MaxPoolSize;
    (* If MaxTextures>0 then the Bumpmap library is trimmed down to size whenever
      the texture count is larger than MaxTextures. The oldest, unused texture is trimmed first.
      However, if you used TGLHeightData.MaterialName, instead of TGLHeightData.LibMaterial,
      then the TGLHeightData component does not register the texture as being used.
      So, if you use TGLHeightData.MaterialName then make sure MaxTextures=0.
      If MaxTextures=0 or if treads(GLS.AsyncHDS) are used, then the texture cache
      is NOT trimmed automatically.
      You will have to manually trim the cache from the main thread, by
      calling 'TrimTextureCache'. (GLS.AsyncHDS.OnIdle is a good place.) *)
    property MaxTextures: Integer read FMaxTextures write FMaxTextures;
    property OnSourceDataFetched;
  end;

//------------------- Bumpmapping routines -----------------------

(*  Some useful methods for setting up bump maps. *)
procedure CalcObjectSpaceLightVectors(Light : TAffineVector;
                                      Vertices: TGLAffineVectorList;
                                      Colors: TGLVectorList);
procedure SetupTangentSpace(Vertices, Normals, TexCoords,
                            Tangents, BiNormals : TGLAffineVectorList);
procedure CalcTangentSpaceLightVectors(Light : TAffineVector;
                                       Vertices, Normals,
                                       Tangents, BiNormals : TGLAffineVectorList;
                                       Colors: TGLVectorList);
function CreateObjectSpaceNormalMap(Width, Height : Integer;
                                    HiNormals,HiTexCoords : TGLAffineVectorList) : TBitmap;
function CreateTangentSpaceNormalMap(Width, Height : Integer;
                                     HiNormals, HiTexCoords,
                                     LoNormals, LoTexCoords,
                                     Tangents, BiNormals : TGLAffineVectorList) : TBitmap;


// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TGLBumpmapHDS ------------------
// ------------------

const
  cDefaultBumpScale = 0.01;

   
constructor TGLBumpmapHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBumpScale := cDefaultBumpScale;
  FSubSampling := 1;
  Uno := TCriticalSection.Create;
end;

 
destructor TGLBumpmapHDS.Destroy;
begin
  BumpmapLibrary := nil;
  Uno.Free;
  inherited Destroy;
end;

procedure TGLBumpmapHDS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FBumpmapLibrary then
      BumpmapLibrary := nil;
  end;
  inherited;
end;

procedure TGLBumpmapHDS.Release(aHeightData: TGLHeightData);
var
  libMat: TGLLibMaterial;
begin
  libMat := aHeightData.LibMaterial;
  aHeightData.MaterialName := '';
  if (FMaxTextures > 0) and (assigned(libMat)) and (libMat.IsUsed = false) then
    libMat.Free;
  inherited;
end;

procedure TGLBumpmapHDS.TrimTextureCache(MaxTextureCount: Integer);
// Thread-safe Version
begin
  if assigned(self) then
  begin
    Uno.Acquire;
    Trim(MaxTextureCount);
    Uno.Release;
  end;
end;

procedure TGLBumpmapHDS.Trim(MaxTextureCount: Integer); // internal use only
var
  matLib: TGLMaterialLibrary;
  libMat: TGLLibMaterial;
  i: Integer;
  cnt: Integer;
begin
  matLib := FBumpmapLibrary;
  if matLib <> nil then
  begin
    cnt := matLib.Materials.Count;
    i := 0;
    while (i < cnt) and (cnt >= MaxTextureCount) do
    begin
      libMat := matLib.Materials[i];
      if libMat.IsUsed then
        i := i + 1
      else
        libMat.Free;
      cnt := matLib.Materials.Count;
    end;
  end;
end;

procedure TGLBumpmapHDS.PreparingData(heightData: TGLHeightData);
var
  TmpHD: TGLHeightData;
  libMat: TGLLibMaterial;
  bmp32: TGLImage;
  MatName: string;
begin
  if not assigned(FBumpmapLibrary) then
    exit;
  // --Generate Normal Map for tile--
  heightData.TextureCoordinatesMode := tcmLocal;
  heightData.TextureCoordinatesOffset := NullTexPoint;
  heightData.TextureCoordinatesScale := XYTexPoint;
  MatName := 'BumpHDS_x' + IntToStr(heightData.XLeft) + 'y' +
    IntToStr(heightData.YTop) + '.';
  // name contains xy coordinates of the current tile
  Uno.Acquire;
  libMat := FBumpmapLibrary.Materials.GetLibMaterialByName(MatName);
  // Check if Tile Texture already exists
  if libMat = nil then
  begin
    if (FMaxTextures > 0) then
    begin
      if heightData.Thread = nil { //Dont trim the cache from a sub-thread; }
      then
        TrimTextureCache(FMaxTextures)
        // Trim unused textures from the material library
    end;
    // Generate new NormalMap texture for this tile
    libMat := FBumpmapLibrary.Materials.Add;
    libMat.Name := MatName;
    // Transfer tile texture coordinates to generated texture
    libMat.TextureScale.X := heightData.TextureCoordinatesScale.S;
    libMat.TextureScale.Y := heightData.TextureCoordinatesScale.T;
    libMat.TextureOffset.X := heightData.TextureCoordinatesOffset.S;
    libMat.TextureOffset.Y := heightData.TextureCoordinatesOffset.T;
    // ------------------------------------------------------
    // --Set up new Normalmap texture for the current tile--
    libMat.Material.MaterialOptions := [moNoLighting];
    with libMat.Material.Texture do
    begin
      ImageClassName := TGLBlankImage.ClassName;
      Enabled := True;
      MinFilter := miNearestMipmapNearest;
      MagFilter := maLinear; // MagFilter:=maNearest;
      TextureMode := tmReplace;
      TextureWrap := twNone;
      TextureFormat := tfRGB16;
      // TextureFormat:=tfRGBA16;
      bmp32 := (Image as TGLBlankImage).GetBitmap32;
      TmpHD := HeightDataSource.GetData(heightData.XLeft - 1,
        heightData.YTop - 1, heightData.Size + 1, heightData.DataType);
      GenerateNormalMap(TmpHD, bmp32, FBumpScale);
      TmpHD.Release;
    end;
    // ----------------------------------------------------
  end;
  // HD.MaterialName:=LibMat.Name;
  heightData.LibMaterial := libMat; // attach texture to current tile
  if assigned(FOnNewTilePrepared) then
    FOnNewTilePrepared(self, heightData, libMat);
  Uno.Release;
end;

procedure TGLBumpmapHDS.GenerateNormalMap(heightData: TGLHeightData;
  normalMap: TGLImage; scale: Single);
var
  MapSize: Integer;
  HD: TGLHeightData;
  X, Y: Integer;
  scaleVec: TAffineVector;
  vec: TAffineVector;
  nmRow: PGLPixel32Array;
  px, py: Integer;
begin
  HD := heightData;
  MapSize := (HD.Size - 1);
  MapSize := MapSize div SubSampling;
  normalMap.Height := MapSize;
  normalMap.Width := MapSize;
  normalMap.Blank := false;
  SetVector(scaleVec, 1, 1, FBumpScale);
  for Y := 0 to MapSize - 1 do
  begin
    nmRow := normalMap.ScanLine[MapSize - 1 - Y];
    for X := 0 to MapSize - 1 do
    begin
      px := X * SubSampling;
      py := Y * SubSampling;
      vec := HD.NormalAtNode(px, py, scaleVec);
      nmRow[X].r := round(128 + 127 * vec.X); // nmRow[x].r:=0;         //Red
      nmRow[X].g := round(128 + 127 * vec.Y);
      // nmRow[x].g:=0;         //Green
      nmRow[X].b := round(128 + 127 * vec.Z);
      // nmRow[x].b:=0;         //Blue
      nmRow[X].a := 255;
    end;
  end;
end;

procedure TGLBumpmapHDS.SetBumpmapLibrary(const val: TGLMaterialLibrary);
begin
  if val <> FBumpmapLibrary then
  begin
    if assigned(FBumpmapLibrary) then
      FBumpmapLibrary.RemoveFreeNotification(self);
    FBumpmapLibrary := val;
    if assigned(FBumpmapLibrary) then
      FBumpmapLibrary.FreeNotification(self);
    MarkDirty;
  end;
end;

procedure TGLBumpmapHDS.SetBumpScale(const val: Single);
begin
  if FBumpScale <> val then
  begin
    FBumpScale := val;
    MarkDirty;
  end;
end;

function TGLBumpmapHDS.StoreBumpScale: Boolean;
begin
  Result := (FBumpScale <> cDefaultBumpScale);
end;

procedure TGLBumpmapHDS.SetSubSampling(const val: Integer);
begin
  if val <> FSubSampling then
  begin
    FSubSampling := RoundDownToPowerOf2(val);
    if FSubSampling < 1 then
      FSubSampling := 1;
    MarkDirty;
  end;
end;

//----------------- BumpMapping routines ---------------------

procedure CalcObjectSpaceLightVectors(Light: TAffineVector; Vertices: TGLAffineVectorList; Colors: TGLVectorList);
var
  i: Integer;
  vec: TAffineVector;
begin
  Colors.Count := Vertices.Count;
  for i := 0 to Vertices.Count - 1 do
  begin
    vec := VectorNormalize(VectorSubtract(Light, Vertices[i]));
    Colors[i] := VectorMake(VectorAdd(VectorScale(vec, 0.5), 0.5), 1);
  end;
end;

procedure SetupTangentSpace(Vertices, Normals, TexCoords, Tangents, BiNormals: TGLAffineVectorList);
var
  i, j: Integer;
  v, n, t: TAffineMatrix;
  vt, tt: TAffineVector;
  interp, dot: Single;

  procedure SortVertexData(sortidx: Integer);
  begin
    if t.v[0].V[sortidx] < t.v[1].V[sortidx] then
    begin
      vt := v.v[0];
      tt := t.v[0];
      v.v[0] := v.v[1];
      t.v[0] := t.v[1];
      v.v[1] := vt;
      t.v[1] := tt;
    end;
    if t.v[0].V[sortidx] < t.v[2].V[sortidx] then
    begin
      vt := v.v[0];
      tt := t.v[0];
      v.v[0] := v.v[2];
      t.v[0] := t.v[2];
      v.v[2] := vt;
      t.v[2] := tt;
    end;
    if t.v[1].V[sortidx] < t.v[2].V[sortidx] then
    begin
      vt := v.v[1];
      tt := t.v[1];
      v.v[1] := v.v[2];
      t.v[1] := t.v[2];
      v.v[2] := vt;
      t.v[2] := tt;
    end;
  end;

begin
  for i := 0 to (Vertices.Count div 3) - 1 do
  begin
    // Get triangle data
    for j := 0 to 2 do
    begin
      v.v[j] := Vertices[3 * i + j];
      n.v[j] := Normals[3 * i + j];
      t.v[j] := TexCoords[3 * i + j];
    end;

    for j := 0 to 2 do
    begin
      // Compute tangent
      SortVertexData(1);

      if (t.v[2].Y - t.v[0].Y) = 0 then
        interp := 1
      else
        interp := (t.v[1].Y - t.v[0].Y) / (t.v[2].Y - t.v[0].Y);

      vt := VectorLerp(v.v[0], v.v[2], interp);
      interp := t.v[0].X + (t.v[2].X - t.v[0].X) * interp;
      vt := VectorSubtract(vt, v.v[1]);
      if t.v[1].X < interp then
        vt := VectorNegate(vt);
      dot := VectorDotProduct(vt, n.v[j]);
      vt.X := vt.X - n.v[j].X * dot;
      vt.Y := vt.Y - n.v[j].Y * dot;
      vt.Z := vt.Z - n.v[j].Z * dot;
      Tangents.Add(VectorNormalize(vt));

      // Compute Bi-Normal
      SortVertexData(0);

      if (t.v[2].X - t.v[0].X) = 0 then
        interp := 1
      else
        interp := (t.v[1].X - t.v[0].X) / (t.v[2].X - t.v[0].X);

      vt := VectorLerp(v.v[0], v.v[2], interp);
      interp := t.v[0].Y + (t.v[2].Y - t.v[0].Y) * interp;
      vt := VectorSubtract(vt, v.v[1]);
      if t.v[1].Y < interp then
        vt := VectorNegate(vt);
      dot := VectorDotProduct(vt, n.v[j]);
      vt.X := vt.X - n.v[j].X * dot;
      vt.Y := vt.Y - n.v[j].Y * dot;
      vt.Z := vt.Z - n.v[j].Z * dot;
      BiNormals.Add(VectorNormalize(vt));
    end;
  end;
end;

procedure CalcTangentSpaceLightVectors(Light: TAffineVector;
  Vertices, Normals, Tangents, BiNormals: TGLAffineVectorList;
  Colors: TGLVectorList);
var
  i: Integer;
  mat: TAffineMatrix;
  vec: TAffineVector;
begin
  Colors.Count := Vertices.Count;
  for i := 0 to Vertices.Count - 1 do
  begin
    mat.v[0] := Tangents[i];
    mat.v[1] := BiNormals[i];
    mat.v[2] := Normals[i];
    TransposeMatrix(mat);
    vec := VectorNormalize(VectorTransform(VectorSubtract(Light, Vertices[i]), mat));
    vec.X := -vec.X;
    Colors[i] := VectorMake(VectorAdd(VectorScale(vec, 0.5), 0.5), 1);
  end;
end;

// ------------------------------------------------------------------------
// Local functions used for creating normal maps
// ------------------------------------------------------------------------

function ConvertNormalToColor(normal: TAffineVector): TColor;

var
  r, g, b: Byte;
begin
  r := Round(255 * (normal.X * 0.5 + 0.5));
  g := Round(255 * (normal.Y * 0.5 + 0.5));
  b := Round(255 * (normal.Z * 0.5 + 0.5));
  Result := RGB2Color(r, g, b);
end;

procedure GetBlendCoeffs(X, Y, x1, y1, x2, y2, x3, y3: Integer; var f1, f2, f3: Single);
var
  m1, m2, d1, d2, px, py: Single;
begin
  if (x1 = X) and (x2 = x3) then
    f1 := 0
  else
  begin
    if x1 = X then
    begin
      m2 := (y3 - y2) / (x3 - x2);
      d2 := y2 - m2 * x2;
      px := X;
      py := m2 * px + d2;
    end
    else if x2 = x3 then
    begin
      m1 := (y1 - Y) / (x1 - X);
      d1 := y1 - m1 * x1;
      px := x2;
      py := m1 * px + d1;
    end
    else
    begin
      m1 := (y1 - Y) / (x1 - X);
      d1 := y1 - m1 * x1;
      m2 := (y3 - y2) / (x3 - x2);
      d2 := y2 - m2 * x2;
      px := (d1 - d2) / (m2 - m1);
      py := m2 * px + d2;
    end;
    f1 := sqrt((X - x1) * (X - x1) + (Y - y1) * (Y - y1)) /
	sqrt((px - x1) * (px - x1) + (py - y1) * (py - y1));
  end;

  if (x2 = X) and (x1 = x3) then
    f2 := 0
  else
  begin
    if x2 = X then
    begin
      m2 := (y3 - y1) / (x3 - x1);
      d2 := y1 - m2 * x1;
      px := X;
      py := m2 * px + d2;
    end
    else if x3 = x1 then
    begin
      m1 := (y2 - Y) / (x2 - X);
      d1 := y2 - m1 * x2;
      px := x1;
      py := m1 * px + d1;
    end
    else
    begin
      m1 := (y2 - Y) / (x2 - X);
      d1 := y2 - m1 * x2;
      m2 := (y3 - y1) / (x3 - x1);
      d2 := y1 - m2 * x1;
      px := (d1 - d2) / (m2 - m1);
      py := m2 * px + d2;
    end;
    f2 := sqrt((X - x2) * (X - x2) + (Y - y2) * (Y - y2)) /
	sqrt((px - x2) * (px - x2) + (py - y2) * (py - y2));
  end;

  if (x3 = X) and (x1 = x2) then
    f3 := 0
  else
  begin
    if X = x3 then
    begin
      m2 := (y2 - y1) / (x2 - x1);
      d2 := y1 - m2 * x1;
      px := X;
      py := m2 * px + d2;
    end
    else if x2 = x1 then
    begin
      m1 := (y3 - Y) / (x3 - X);
      d1 := y3 - m1 * x3;
      px := x1;
      py := m1 * px + d1;
    end
    else
    begin
      m1 := (y3 - Y) / (x3 - X);
      d1 := y3 - m1 * x3;
      m2 := (y2 - y1) / (x2 - x1);
      d2 := y1 - m2 * x1;
      px := (d1 - d2) / (m2 - m1);
      py := m2 * px + d2;
    end;
    f3 := sqrt((X - x3) * (X - x3) + (Y - y3) * (Y - y3)) /
	sqrt((px - x3) * (px - x3) + (py - y3) * (py - y3));
  end;

end;

function BlendNormals(X, Y, x1, y1, x2, y2, x3, y3: Integer;
  n1, n2, n3: TAffineVector): TAffineVector;
var
  f1, f2, f3: Single;
begin
  GetBlendCoeffs(X, Y, x1, y1, x2, y2, x3, y3, f1, f2, f3);
  Result := VectorScale(n1, 1 - f1);
  AddVector(Result, VectorScale(n2, 1 - f2));
  AddVector(Result, VectorScale(n3, 1 - f3));
end;

procedure CalcObjectSpaceNormalMap(Width, Height: Integer;
  NormalMap, Normals, TexCoords: TGLAffineVectorList);
var
  i, X, Y, xs, xe, x1, y1, x2, y2, x3, y3: Integer;
  n, n1, n2, n3: TAffineVector;
begin
  for i := 0 to (TexCoords.Count div 3) - 1 do
  begin
    x1 := Round(TexCoords[3 * i].X * (Width - 1));
    y1 := Round((1 - TexCoords[3 * i].Y) * (Height - 1));
    x2 := Round(TexCoords[3 * i + 1].X * (Width - 1));
    y2 := Round((1 - TexCoords[3 * i + 1].Y) * (Height - 1));
    x3 := Round(TexCoords[3 * i + 2].X * (Width - 1));
    y3 := Round((1 - TexCoords[3 * i + 2].Y) * (Height - 1));
    n1 := Normals[3 * i];
    n2 := Normals[3 * i + 1];
    n3 := Normals[3 * i + 2];

    if y2 < y1 then
    begin
      X := x1;
      Y := y1;
      n := n1;
      x1 := x2;
      y1 := y2;
      n1 := n2;
      x2 := X;
      y2 := Y;
      n2 := n;
    end;
    if y3 < y1 then
    begin
      X := x1;
      Y := y1;
      n := n1;
      x1 := x3;
      y1 := y3;
      n1 := n3;
      x3 := X;
      y3 := Y;
      n3 := n;
    end;
    if y3 < y2 then
    begin
      X := x2;
      Y := y2;
      n := n2;
      x2 := x3;
      y2 := y3;
      n2 := n3;
      x3 := X;
      y3 := Y;
      n3 := n;
    end;

    if y1 < y2 then
      for Y := y1 to y2 do
      begin
        xs := Round(x1 + (x2 - x1) * ((Y - y1) / (y2 - y1)));
        xe := Round(x1 + (x3 - x1) * ((Y - y1) / (y3 - y1)));
        if xe < xs then
        begin
          X := xs;
          xs := xe;
          xe := X;
        end;
        for X := xs to xe do
          NormalMap[X + Y * Width] := BlendNormals(X, Y, x1, y1, x2, y2, x3, y3, n1, n2, n3);
      end;
    if y2 < y3 then
      for Y := y2 to y3 do
      begin
        xs := Round(x2 + (x3 - x2) * ((Y - y2) / (y3 - y2)));
        xe := Round(x1 + (x3 - x1) * ((Y - y1) / (y3 - y1)));
        if xe < xs then
        begin
          X := xs;
          xs := xe;
          xe := X;
        end;
        for X := xs to xe do
          NormalMap[X + Y * Width] := BlendNormals(X, Y, x1, y1, x2, y2, x3, y3, n1, n2, n3);
      end;
  end;
end;

function CreateObjectSpaceNormalMap(Width, Height: Integer;
  HiNormals, HiTexCoords: TGLAffineVectorList): TBitmap;
var
  i: Integer;
  NormalMap: TGLAffineVectorList;
begin
  NormalMap := TGLAffineVectorList.Create;
  NormalMap.AddNulls(Width * Height);
  CalcObjectSpaceNormalMap(Width, Height, NormalMap, HiNormals, HiTexCoords);

  // Creates the bitmap
  Result := TBitmap.Create;
  Result.Width := Width;
  Result.Height := Height;
  Result.PixelFormat := pf24bit;

  // Paint bitmap with normal map normals (X,Y,Z) -> (R,G,B)
  for i := 0 to NormalMap.Count - 1 do
    Result.Canvas.Pixels[i mod Width, i div Height] := ConvertNormalToColor(NormalMap[i]);

  NormalMap.Free;
end;

function CreateTangentSpaceNormalMap(Width, Height: Integer; HiNormals, HiTexCoords, LoNormals, LoTexCoords, Tangents,
  BiNormals: TGLAffineVectorList): TBitmap;

  function NormalToTangentSpace(normal: TAffineVector; X, Y, x1, y1, x2, y2, x3, y3: Integer; m1, m2, m3: TAffineMatrix)
    : TAffineVector;
  var
    n1, n2, n3: TAffineVector;
  begin
    n1 := VectorTransform(normal, m1);
    n2 := VectorTransform(normal, m2);
    n3 := VectorTransform(normal, m3);
    Result := BlendNormals(X, Y, x1, y1, x2, y2, x3, y3, n1, n2, n3);
    NormalizeVector(Result);
  end;

var
  i, X, Y, xs, xe, x1, y1, x2, y2, x3, y3: Integer;
  NormalMap: TGLAffineVectorList;
  n: TAffineVector;
  m, m1, m2, m3: TAffineMatrix;
begin
  NormalMap := TGLAffineVectorList.Create;
  NormalMap.AddNulls(Width * Height);

  CalcObjectSpaceNormalMap(Width, Height, NormalMap, HiNormals, HiTexCoords);

  // Transform the object space normals into tangent space
  for i := 0 to (LoTexCoords.Count div 3) - 1 do
  begin
    x1 := Round(LoTexCoords[3 * i].X * (Width - 1));
    y1 := Round((1 - LoTexCoords[3 * i].Y) * (Height - 1));
    x2 := Round(LoTexCoords[3 * i + 1].X * (Width - 1));
    y2 := Round((1 - LoTexCoords[3 * i + 1].Y) * (Height - 1));
    x3 := Round(LoTexCoords[3 * i + 2].X * (Width - 1));
    y3 := Round((1 - LoTexCoords[3 * i + 2].Y) * (Height - 1));

    m1.X := Tangents[3 * i];
    m1.Y := BiNormals[3 * i];
    m1.Z := LoNormals[3 * i];
    m2.X := Tangents[3 * i + 1];
    m2.Y := BiNormals[3 * i + 1];
    m2.Z := LoNormals[3 * i + 1];
    m3.X := Tangents[3 * i + 2];
    m3.Y := BiNormals[3 * i + 2];
    m3.Z := LoNormals[3 * i + 2];
    TransposeMatrix(m1);
    TransposeMatrix(m2);
    TransposeMatrix(m3);
    InvertMatrix(m1);
    InvertMatrix(m2);
    InvertMatrix(m3);
    if y2 < y1 then
    begin
      X := x1;
      Y := y1;
      m := m1;
      x1 := x2;
      y1 := y2;
      m1 := m2;
      x2 := X;
      y2 := Y;
      m2 := m;
    end;
    if y3 < y1 then
    begin
      X := x1;
      Y := y1;
      m := m1;
      x1 := x3;
      y1 := y3;
      m1 := m3;
      x3 := X;
      y3 := Y;
      m3 := m;
    end;
    if y3 < y2 then
    begin
      X := x2;
      Y := y2;
      m := m2;
      x2 := x3;
      y2 := y3;
      m2 := m3;
      x3 := X;
      y3 := Y;
      m3 := m;
    end;

    if y1 < y2 then
      for Y := y1 to y2 do
      begin
        xs := Round(x1 + (x2 - x1) * ((Y - y1) / (y2 - y1)));
        xe := Round(x1 + (x3 - x1) * ((Y - y1) / (y3 - y1)));
        if xe < xs then
        begin
          X := xs;
          xs := xe;
          xe := X;
        end;
        for X := xs to xe - 1 do
        begin
          n := NormalToTangentSpace(NormalMap[X + Y * Width], X, Y, x1, y1, x2, y2, x3, y3, m1, m2, m3);
          NormalizeVector(n);
          n.X := -n.X;
          NormalMap[X + Y * Width] := n;
        end;
      end;
    if y2 < y3 then
      for Y := y2 + 1 to y3 do
      begin
        xs := Round(x2 + (x3 - x2) * ((Y - y2) / (y3 - y2)));
        xe := Round(x1 + (x3 - x1) * ((Y - y1) / (y3 - y1)));
        if xe < xs then
        begin
          X := xs;
          xs := xe;
          xe := X;
        end;
        for X := xs to xe - 1 do
        begin
          n := NormalToTangentSpace(NormalMap[X + Y * Width], X, Y, x1, y1, x2, y2, x3, y3, m1, m2, m3);
          NormalizeVector(n);
          n.X := -n.X;
          NormalMap[X + Y * Width] := n;
        end;
      end;
  end;

  // Creates the bitmap
  Result := TBitmap.Create;
  Result.Width := Width;
  Result.Height := Height;
  Result.PixelFormat := pf24bit;

  // Paint bitmap with normal map normals (X,Y,Z) -> (R,G,B)
  for i := 0 to NormalMap.Count - 1 do
    Result.Canvas.Pixels[i mod Width, i div Height] := ConvertNormalToColor(NormalMap[i]);

  NormalMap.Free;
end;


// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterClass(TGLBumpmapHDS);

end.
