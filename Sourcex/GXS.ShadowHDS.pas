//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.ShadowHDS;

(*
  Implements an HDS that automatically generates a terrain lightmap texture

  Issues:1:Ambient and Diffuse light properties can not be set to 0, to avoid what
  seems to be a Delphi bug: If a property of type 'Single' is set to 0,
  Delphi seems to skip the property's set method at startup, and just
  uses the default value instead. (Does anyone know a better workaround?)
  2:Subsampling is not currently supported.
  3:If the light vector's y component is not 0 then the shadow edges may be
  a bit jagged, due to the crude Bresenham line algorythm that was used.
  You can hide this by increasing SoftRange though.
  5:At some light angles, rounding errors cause various artifacts:
  (Black tile edges / slight mis-alignments /etc.)
  6:Applying materials ocasionally causes AV's

  PS. The RayCastShadowHeight function returns the height of the shadow at a point
  on the terrain. This, and the LightVector may come in handy for implementing shadow volumes?
*)

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  System.Math,

  GXS.VectorTypes,
  GXS.VectorLists,
  GXS.HeightData,
  GXS.Graphics,
  GXS.VectorGeometry,
  GXS.Texture,
  GXS.Coordinates,
  GXS.Material;

type
  TgxShadowHDS = class;
  TNewTilePreparedEvent = procedure(Sender: TgxShadowHDS;
    HeightData: TgxHeightData; ShadowMapMaterial: TgxLibMaterial) of object;
  TThreadBmp32 = procedure(Sender: TgxShadowHDS; HeightData: TgxHeightData;
    bmp32: TgxBitmap32) of object;

  (* An Height Data Source that generates terrain shadow maps automatically.
    The HDS must be connected to another HDS, which will provide the elevation
    data, and to a MaterialLibrary where shadowmaps will be placed. *)
  TgxShadowHDS = class(TgxHeightDataSourceFilter)
  private
    FTileSize: integer;
    FShadowmapLibrary: TgxMaterialLibrary;
    FLightVector: TgxCoordinates;
    FScale: TgxCoordinates;
    FScaleVec: TVector3f;
    FOnNewTilePrepared: TNewTilePreparedEvent;
    FOnThreadBmp32: TThreadBmp32;
    // FSubSampling : Integer;
    FMaxTextures: integer;
    Step: TVector3f;
    FScanDistance: integer;
    FSoftRange: cardinal;
    FDiffuse: single;
    FAmbient: single;
    OwnerHDS: TgxHeightDataSource; // The owner of the tile
  protected
    procedure SetShadowmapLibrary(const val: TgxMaterialLibrary);
    procedure SetScale(AValue: TgxCoordinates);
    procedure SetLightVector(AValue: TgxCoordinates);
    procedure SetSoftRange(AValue: cardinal);
    procedure SetDiffuse(AValue: single);
    procedure SetAmbient(AValue: single);
    // procedure SetSubSampling(const val : Integer);
    procedure Trim(MaxTextureCount: integer);
    function FindUnusedMaterial: TgxLibMaterial;
    function CalcStep: TAffineVector;
    function CalcScale: TAffineVector;
    (* Get the number of steps, before the current tile's edge is reached,
      in the direction of the step vector; *)
    function WrapDist(Lx, Ly: single): integer;
    // Converts local tile coordinates to world coordinages. Even if the coordinates are off the tile.
    procedure LocalToWorld(Lx, Ly: single; HD: TgxHeightData; var Wx: single;
      var Wy: single);
    // Takes World coordinates and returns the correct tile, and converted local coordinates
    procedure WorldToLocal(Wx, Wy: single; var HD: TgxHeightData;
      var Lx: single; var Ly: single);
  public
    // When true, only a blank ShadowMap is generated (FAST), but OnThreadBmp32 is still called in a subthread.
    SkipGenerate: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // procedure   Release(aHeightData : TgxHeightData); override;
    (* This will repeatedly delete the oldest unused texture from the TGLMaterialLibrary,
      until the texture count drops to MaxTextureCount.
      DONT use this if you used TGLHeightData.MaterialName to link your terrain textures.
      Either use with TGLHeightData.LibMaterial, or manually delete unused LightMap textures. *)
    procedure TrimTextureCache(MaxTextureCount: integer = 0);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    // Prepare a blank texture for this tile's lightmap, from the main thread
    procedure BeforePreparingData(HeightData: TgxHeightData); override;
    // Calculate the lightmap from the HD thread, using the attached blank texture
    procedure PreparingData(HeightData: TgxHeightData); override;
    procedure AfterPreparingData(HeightData: TgxHeightData); override;
    procedure GenerateShadowMap(HeightData: TgxHeightData;
      ShadowMap: TgxBitmap32; scale: single);
    (* This traces a ray from a point on the terrain surface, back to the Lightsource,
      while testing for any intersections with the terrain.
      It returns the height of the shadow. There is no shadow if the shadow height is equal to terrain height.
      This is slow, but only needs to be done for pixels along the tile edge, facing the light. *)
    function RayCastShadowHeight(HD: TgxHeightData; localX, localY: single)
      : single; overload;
    procedure RayCastLine(HeightData: TgxHeightData; Lx, Ly: single;
      ShadowMap: TgxBitmap32);
    (* Calculate the pixel brightness, using Direct Diffuse light and Ambient light.
      DirectLight  = 1 if in direct sunlight (no shadows)
      0 if in shadow. (Use "SoftRange" for soft shadow edges i.e. 1>Directlight>0 )
      AmbientLight = Relative to Angle between surface Normal and sky (Directly up)
      ie. Vertical walls are darker because they see less sky.
      DiffuseLight = Relative to Angle between surface Normal, and Sun vector. *)
    function Shade(HeightData: TgxHeightData; x, y: integer;
      ShadowHeight, TerrainHeight: single): byte;
  published
    property ShadowmapLibrary: TgxMaterialLibrary read FShadowmapLibrary
      write SetShadowmapLibrary;
    property OnThreadBmp32: TThreadBmp32 read FOnThreadBmp32
      write FOnThreadBmp32; // WARNING: This runs in a subthread
    property OnNewTilePrepared: TNewTilePreparedEvent read FOnNewTilePrepared
      write FOnNewTilePrepared;
    property LightVector: TgxCoordinates read FLightVector write SetLightVector;
    property scale: TgxCoordinates read FScale write FScale;
    property ScanDistance: integer read FScanDistance write FScanDistance;
    property SoftRange: cardinal read FSoftRange write SetSoftRange;
    // Shadow height above sufrace for max diffuse light
    property Diffuse: single read FDiffuse write SetDiffuse;
    property Ambient: single read FAmbient write SetAmbient;
    property MaxTextures: integer read FMaxTextures write FMaxTextures;
    property OnSourceDataFetched;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

constructor TgxShadowHDS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLightVector := TgxCoordinates.CreateInitialized(Self, VectorMake(1, 0, -1));
  FLightVector.Style := csVector; // csPoint;
  FScale := TgxCoordinates.CreateInitialized(Self, VectorMake(1, 1, 1));
  FScale.Style := csVector; // csPoint;
  FScanDistance := 64;
  FAmbient := 0.25;
  FDiffuse := 0.75;
  FSoftRange := 1;
  // FSubSampling:=1;
  OwnerHDS := Self;
  // Until told otherwise, assume that ShadowHDS IS the tile owner.
  SkipGenerate := false;
  // Set to true in "OnSourceDataFetched" to skip shadow generation.
end;

destructor TgxShadowHDS.Destroy;
begin
  Self.Active := false;
  FreeAndNil(FLightVector);
  FreeAndNil(FScale);
  ShadowmapLibrary := nil;
  inherited Destroy;
end;

procedure TgxShadowHDS.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FShadowmapLibrary then
      ShadowmapLibrary := nil;
  end;
  inherited;
end;

(*
  procedure TgxShadowHDS.Release(aHeightData : TgxHeightData);
  var libMat : TgxLibMaterial;
  begin
  HeightDataSource.Data.LockList;
  libMat:=aHeightData.LibMaterial;
  aHeightData.MaterialName:='';
  if (FMaxTextures>0)and(assigned(LibMat))and(libMat.IsUsed=false)
  then LibMat.free;
  inherited;
  HeightDataSource.Data.UnlockList;
  end;
*)

procedure TgxShadowHDS.TrimTextureCache(MaxTextureCount: integer);
// Thread-safe Version
// Thread-safe Version
begin
  If (not assigned(Self)) or (not assigned(OwnerHDS)) then
    exit;
  with OwnerHDS.Data.LockList do
    try
      Trim(MaxTextureCount);
    finally
      OwnerHDS.Data.UnlockList;
    end;
end;

procedure TgxShadowHDS.Trim(MaxTextureCount: integer); // internal use only
var
  matLib: TgxMaterialLibrary;
  libMat: TgxLibMaterial;
  i: integer;
  cnt: integer;
begin
  matLib := FShadowmapLibrary;
  if matLib <> nil then
  begin
    // ---------------------------------
    // --Trim unused textures, until MaxTextureCount is reached--
    cnt := matLib.Materials.Count;
    i := 0;
    while (i < cnt) and (cnt >= MaxTextureCount) do
    begin
      libMat := matLib.Materials[i];
      if libMat.IsUsed then
        inc(i)
      else
      begin
        libMat.Free;
        dec(cnt); // cnt:=matlib.Materials.Count;
      end;
    end;
    // ----------------------------------------------------------
  end;
end;

function TgxShadowHDS.FindUnusedMaterial: TgxLibMaterial;
var
  matLib: TgxMaterialLibrary;
  i: integer;
  cnt: integer;
begin
  result := nil;
  matLib := FShadowmapLibrary;
  if matLib <> nil then
  begin
    cnt := matLib.Materials.Count;
    i := 0;
    while (i < cnt) and (matLib.Materials[i].IsUsed) do
      inc(i);
    if (i < cnt) then
      result := matLib.Materials[i];
  end;
end;

procedure TgxShadowHDS.SetLightVector(AValue: TgxCoordinates);
begin
  With OwnerHDS.Data.LockList do
    try
      FLightVector.Assign(AValue);
      CalcStep;
      // MarkDirty;
    finally
      OwnerHDS.Data.UnlockList;
    end;
end;

function TgxShadowHDS.CalcStep: TAffineVector;
var
  L: single;
  v: TAffineVector;
begin
  MakeVector(v, FLightVector.x / FScale.x, FLightVector.y / FScale.y,
    256 * FLightVector.Z / FScale.Z);
  L := MaxFloat(abs(v.x), abs(v.y));
  Step := VectorScale(v, 1 / L);
  Step.x := trunc(Step.x * 16384) / 16384;
  // round down the fraction now, to prevent rounding errors later
  Step.y := trunc(Step.y * 16384) / 16384;
  // round down the fraction now, to prevent rounding errors later

  if ((FLightVector.x = 0) and (FLightVector.y = 0)) then
  begin
    Step.x := 1;
    Step.y := 0;
    Step.Z := -maxint;
  end;

  result := Step;
end;

function TgxShadowHDS.CalcScale: TAffineVector;
begin
  FScaleVec.x := FScale.x * 256;
  FScaleVec.y := FScale.y * 256;
  FScaleVec.Z := FScale.Z;
  result := FScaleVec;
end;

procedure TgxShadowHDS.BeforePreparingData(HeightData: TgxHeightData);
var
  HD: TgxHeightData;
  libMat: TgxLibMaterial;
  MatName: string;
begin
  if not assigned(FShadowmapLibrary) then
    exit;
  HD := HeightData;
  OwnerHDS := HD.Owner;
  with OwnerHDS.Data.LockList do
    try
      Trim(FMaxTextures);
      MatName := 'ShadowHDS_x' + IntToStr(HD.XLeft) + 'y' + IntToStr(HD.YTop) +
        '.'; // name contains xy coordinates of the current tile
      libMat := FShadowmapLibrary.Materials.Add;
      // ---------Recycle Textures---------
      // libMat:=self.FindUnusedMaterial;                  //look for an unused texture, to recycle
      // if libMat=nil
      // then libMat:=FShadowmapLibrary.Materials.Add    //if no free textures were found, get a new one
      // else libMat.Material.Texture.Enabled:=false;    //recycle the unused texture
      // ----------------------------------
      libMat.Name := MatName;
      // HD.MaterialName:=LibMat.Name;
      HD.LibMaterial := libMat; // attach texture to current tile
    finally
      OwnerHDS.Data.UnlockList;
    end;
end;

procedure TgxShadowHDS.PreparingData(HeightData: TgxHeightData);
var
  HD: TgxHeightData;
  libMat: TgxLibMaterial;
  bmp32: TgxBitmap32;
begin
  HD := HeightData;
  libMat := HD.LibMaterial;
  Assert(assigned(HD));
  Assert(assigned(libMat));
  Assert(libMat.Material.Texture.Disabled);

  // With heightData.Owner.Data.LockList do try //lock out other threads
  // Transfer tile texture coordinates to generated texture
  libMat.TextureScale.x := HD.TextureCoordinatesScale.S;
  libMat.TextureScale.y := HD.TextureCoordinatesScale.T;
  libMat.TextureOffset.x := HD.TextureCoordinatesOffset.S;
  libMat.TextureOffset.y := HD.TextureCoordinatesOffset.T;
  // ------------------------------------------------------
  // --Set up new Lightmap texture for the current tile--
  libMat.Material.MaterialOptions := [moNoLighting];
  with libMat.Material.Texture do
  begin
    ImageClassName := TgxBlankImage.ClassName;
    MinFilter := miNearestMipmapNearest;
    // MinFilter:=miLinearMipmapLinear;
    // MagFilter:=maNearest;
    MagFilter := maLinear;

    TextureMode := tmReplace;
    TextureWrap := twNone;
    // TextureFormat:=tfLuminance;
    TextureFormat := tfRGB16;
    // TextureFormat:=tfRGBA;
    bmp32 := (Image as TgxBlankImage).GetBitmap32;
    if not SkipGenerate then
      GenerateShadowMap(HD, bmp32, 1);
    if assigned(FOnThreadBmp32) then
      FOnThreadBmp32(Self, HeightData, bmp32);

    // Enabled:=True;
    with HD.Owner.Data.LockList do
      try
        Enabled := True;
      finally
        HD.Owner.Data.UnlockList;
      end;
  end;
  // finally HD.Owner.Data.UnlockList; end;
  // ----------------------------------------------------
end;

procedure TgxShadowHDS.AfterPreparingData(HeightData: TgxHeightData);
begin
  if assigned(FOnNewTilePrepared) then
    FOnNewTilePrepared(Self, HeightData, HeightData.LibMaterial);
end;

(*
  procedure TgxShadowHDS.PreparingData(heightData : TgxHeightData);
  var HD    : TgxHeightData;
  libMat: TgxLibMaterial;
  bmp32 : TgxBitmap32;
  MatName:string;
  Hold:TgxUpdateAbleObject;
  lst:TList;
  begin

  if not assigned (FShadowmapLibrary) then exit;
  //--Generate Shadow Map for tile--
  lst:=HeightDataSource.Data.LockList;   //lock out other threads
  //Uno.Acquire;
  HD:=HeightData;
  MatName:='ShadowHDS_x'+IntToStr(HD.XLeft)+'y'+IntToStr(HD.YTop)+'.'; //name contains xy coordinates of the current tile
  Hold:=TgxUpdateAbleObject.Create(self);

  LibMat:=FShadowmapLibrary.Materials.GetLibMaterialByName(MatName);   //Check if Tile Texture already exists
  //if assigned(libmat) then LibMat.Name:='Dirty';

  //LibMat:=nil;
  if LibMat=nil then begin
  if (FMaxTextures>0)and(HD.Thread=nil)  //Dont trim the cache from a sub-thread;
  then TrimTextureCache(FMaxTextures); //Trim unused textures from the material library
  //Generate new ShadowMap texture for this tile
  libMat:=FShadowmapLibrary.Materials.Add;
  libMat.RegisterUser(Hold);  //hold onto the texture, so another thread doesnt delete it

  //Transfer tile texture coordinates to generated texture
  libMat.TextureScale.X :=HD.TextureCoordinatesScale.S;
  libMat.TextureScale.Y :=HD.TextureCoordinatesScale.T;
  libMat.TextureOffset.X:=HD.TextureCoordinatesOffset.S;
  libMat.TextureOffset.Y:=HD.TextureCoordinatesOffset.T;
  //------------------------------------------------------
  //--Set up new Lightmap texture for the current tile--
  libMat.Material.MaterialOptions:=[moNoLighting];
  with libMat.Material.Texture do begin
  ImageClassName:=TgxBlankImage.ClassName;
  Enabled:=True;
  MinFilter:=miNearestMipmapNearest;
  //MagFilter:=maNearest;
  MagFilter:=maLinear;
  TextureMode:=tmReplace;
  //TextureWrap:=twBoth;
  TextureWrap:=twNone;
  //TextureFormat:=tfRGB16;
  //TextureFormat:=tfRGBA16;
  TextureFormat:=tfLuminanceAlpha;
  bmp32:=(Image as TgxBlankImage).GetBitmap32(GL_TEXTURE_2D);
  GenerateShadowMap(HD , bmp32, 1);
  end;
  libMat.Name:=MatName;
  //----------------------------------------------------
  end;
  //HD.MaterialName:=LibMat.Name;
  HD.LibMaterial:=LibMat;  //attach texture to current tile
  libMat.UnregisterUser(Hold);
  Hold.Free;
  //Uno.Release;
  HeightDataSource.Data.UnlockList;
  if Assigned(FOnNewTilePrepared) then FOnNewTilePrepared(Self,HD,libMat);
  end;
*)

procedure TgxShadowHDS.GenerateShadowMap(HeightData: TgxHeightData;
  ShadowMap: TgxBitmap32; scale: single);
var
  HD: TgxHeightData;
  x, y: integer; // in local space
  sx, sy: single;
begin
  HD := HeightData;
  FTileSize := (HD.Size - 1);
  ShadowMap.Height := FTileSize;
  ShadowMap.Width := FTileSize;
  CalcStep;
  CalcScale;
  sx := Step.x;
  sy := Step.y;
  if abs(sx) > abs(sy) then
  begin
    y := 0;
    if sx < 0 then
      x := FTileSize - 1 // right to left
    else
      x := 0; // left to right
    while (y < FTileSize) do
    begin
      RayCastLine(HD, x, y, ShadowMap); // cast a shadow line across the tile
      inc(y);
    end;
  end
  else
  begin
    x := 0;
    if sy < 0 then
      y := FTileSize - 1 // top to bottom
    else
      y := 0; // bottom to top
    while (x < FTileSize) do
    begin
      RayCastLine(HD, x, y, ShadowMap); // cast a shadow line across the tile
      inc(x);
    end;
  end;
end;

function TgxShadowHDS.RayCastShadowHeight(HD: TgxHeightData;
  localX, localY: single): single;
var
  tmpHD: TgxHeightData;
  Wx, Wy: single;
  Lx, Ly: single;
  h: single;
  ctr: integer;
  rh: single;
  dif: single;
  ShadowDif: single;
  startH: single;
  jump: integer;
begin
  Lx := ClampValue(localX, 0, FTileSize);
  Ly := ClampValue(localY, 0, FTileSize);
  startH := HD.InterpolatedHeight(Lx, Ly);
  tmpHD := HD;
  ctr := 0;
  ShadowDif := 0;
  rh := startH;
  jump := 1;
  while (ctr < FScanDistance) and (tmpHD.DataState <> hdsNone) do
  begin
    Lx := Lx - Step.x * jump;
    Ly := Ly - Step.y * jump;
    rh := rh - Step.Z * jump;
    // --jump to new tile--
    if (Lx < 0) or (Lx >= FTileSize) or (Ly < 0) or (Ly >= FTileSize) then
    begin
      LocalToWorld(Lx, Ly, tmpHD, Wx, Wy);
      // if our local coordinates are off the tile,
      WorldToLocal(Wx, Wy, tmpHD, Lx, Ly);
      // get the new tile, and local coordinates
    end
    else
    begin
      h := tmpHD.InterpolatedHeight(Lx, Ly);
      dif := h - rh;
      ShadowDif := MaxFloat(dif, ShadowDif);
      if ShadowDif > (-Step.Z) + FSoftRange
      // if ray is more than 1 steps above the surface
      then
        jump := 2 // then take 2 steps at a time
      else
        jump := 1;
      inc(ctr);
    end;
  end;
  result := startH + ShadowDif; // actual height of shadow
end;

procedure TgxShadowHDS.LocalToWorld(Lx, Ly: single; HD: TgxHeightData;
  var Wx: single; var Wy: single);
var
  HDS: TgxHeightDataSource;
begin
  HDS := Self.HeightDataSource;
  Wx := Lx + HD.XLeft;
  Wy := HDS.Height - HD.YTop - Ly;

  // wrap terrain                               //no longer needed?
  // if wx>=HDS.Width then wx:=wx-HDS.Width;
  // if wx<0 then wx:=wx+HDS.Width;
  // if wy>=HDS.Height then wy:=wy-HDS.Height;
  // if wy<0 then wy:=wy+HDS.Height;
end;

procedure TgxShadowHDS.WorldToLocal(Wx, Wy: single; var HD: TgxHeightData;
  var Lx: single; var Ly: single);
var
  HDS: TgxHeightDataSource;
  XLeft, YTop: integer;
  Size: integer;
begin
  // wrap terrain                               //no longer needed?
  // HDS:=self.HeightDataSource;
  // if wx>=HDS.Width then wx:=wx-HDS.Width;
  // if wx<0 then wx:=wx+HDS.Width;
  // if wy>=HDS.Height then wy:=wy-HDS.Height;
  // if wy<0 then wy:=wy+HDS.Height;

  HDS := Self.HeightDataSource;
  Size := FTileSize;
  XLeft := floor(Wx / Size) * Size;
  Lx := Wx - XLeft;
  YTop := floor((HDS.Height - Wy) / Size) * Size;
  Ly := (HDS.Height - YTop - Wy);
  HD := HDS.GetData(XLeft, YTop, Size + 1, hdtSmallInt);
end;
// ----------------------------------------------------------

procedure TgxShadowHDS.RayCastLine(HeightData: TgxHeightData; Lx, Ly: single;
  ShadowMap: TgxBitmap32);
var
  sh, h: single;
  HD: TgxHeightData;
  Size: integer;
  nmRow: PgxPixel32Array;
  ctr: integer;
  px, py: integer;
  lum: byte;
  wrapDst: integer;
  // pink:boolean;
  // PinkMax:integer;
  cx, cy: single;
  procedure LineStep; // draw the pixel, and increase counters
  begin
    cx := ClampValue(Lx, 0, Size - 1);
    cy := ClampValue(Ly, 0, Size - 1);
    px := trunc(cx);
    py := trunc(cy);
    h := HD.InterpolatedHeight(cx, cy);
    sh := MaxFloat(sh, h);
    lum := Shade(HD, px, py, sh, h);
    nmRow := ShadowMap.ScanLine[Size - 1 - py];
    nmRow[px].r := lum;
    nmRow[px].g := lum;
    nmRow[px].b := lum;
    nmRow[px].a := 255;
    // pinkMax:=MinInteger(Integer(lum+8),255);
    // if pink=true then nmRow[px].r:=pinkMax;
    Lx := Lx + Step.x;
    Ly := Ly + Step.y;
    sh := sh + Step.Z;
    inc(ctr);
  end;

begin
  HD := HeightData;
  sh := RayCastShadowHeight(HD, Lx, Ly);
  Size := FTileSize;
  ctr := 0;
  wrapDst := WrapDist(Lx, Ly);
  // pink:=false;
  if wrapDst < Size then
  begin // check if this line will wrap before its end
    while ctr <= wrapDst do
      LineStep; // take one exta step, to prevent gaps due to rounding errors
    Lx := Lx - Step.x; //
    Ly := Ly - Step.y; // step back, to compensate for the extra step
    ctr := ctr - 1; //
    if abs(Step.x) > abs(Step.y) then
    begin // East or West
      if Step.y < 0 then
        Ly := Ly + Size; // ESE or WSW
      if Step.y > 0 then
        Ly := Ly - Size; // ENE or WNW
    end
    else
    begin // North or South
      if Step.x < 0 then
        Lx := Lx + Size; // NNW or SSW
      if Step.x > 0 then
        Lx := Lx - Size; // NNE or SSE
    end;
    cx := ClampValue(Lx, 0, Size - 1);
    cy := ClampValue(Ly, 0, Size - 1);
    sh := RayCastShadowHeight(HD, cx, cy);
    sh := sh + Step.Z * 0.4;
    // pink:=true;
  end;
  while ctr < Size do
    LineStep; // No wrapping
end;

// ----------------------------------------------------------

function TgxShadowHDS.WrapDist(Lx, Ly: single): integer;
var
  x, y: single;
  Size: integer;
  sx, sy: single;
begin
  sx := Step.x;
  sy := Step.y;
  Size := FTileSize;
  x := Size;
  y := Size;
  if abs(sx) > abs(sy) then
  begin
    if sy > 0 then
      y := (Size - Ly) / sy;
    if sy < 0 then
      y := -Ly / sy;
  end
  else
  begin
    if sx > 0 then
      x := (Size - Lx) / sx;
    if sx < 0 then
      x := -Lx / sx;
  end;
  result := Ceil(minFloat(x, y));
end;

function TgxShadowHDS.Shade(HeightData: TgxHeightData; x, y: integer;
  ShadowHeight, TerrainHeight: single): byte;
var
  HD: TgxHeightData;
  nv: TAffineVector;
  dot: single;
  sunVec: TAffineVector;
  directLight: single;
  // Range:0-1  (0 if in shadow) (<1 and >0 if near shadow edge)
  diffuseLight: single;
  ambientLight: single;
  Light: single;
begin
  HD := HeightData;
  nv := HD.NormalAtNode(x, y, FScaleVec);
  // --Ambient Light from blue sky (directly up)--
  ambientLight := nv.Z;
  // --Shadows/Direct light/Soft shadow edges--
  directLight := ClampValue(1 - (ShadowHeight - TerrainHeight) /
    SoftRange, 0, 1);
  // --Diffuse light, when not in shadow--
  if directLight = 0 then
    diffuseLight := 0 // no direct light (shadow)
  else
  begin // diffused light ~ cos of normalVec and lightVec
    MakeVector(sunVec, LightVector.x, LightVector.y, -LightVector.Z);
    NormalizeVector(sunVec);
    dot := VectorDotProduct(nv, sunVec);
    // cos of the angle between the normal and light
    diffuseLight := MaxFloat(dot, 0);
  end;
  // -------------------------------------
  Light := (FDiffuse * diffuseLight * directLight) + (FAmbient * ambientLight);
  result := round(ClampValue(Light, 0, 1) * 255);
end;

procedure TgxShadowHDS.SetShadowmapLibrary(const val: TgxMaterialLibrary);
begin
  if val <> FShadowmapLibrary then
  begin
    if assigned(FShadowmapLibrary) then
      FShadowmapLibrary.RemoveFreeNotification(Self);
    FShadowmapLibrary := val;
    if assigned(FShadowmapLibrary) then
      FShadowmapLibrary.FreeNotification(Self);
    MarkDirty;
  end;
end;

procedure TgxShadowHDS.SetScale(AValue: TgxCoordinates);
begin
  with OwnerHDS.Data.LockList do
    try
      FScale.Assign(AValue);
      // CalcScale;
      // MarkDirty;
    finally
      OwnerHDS.Data.UnlockList;
    end;
end;

procedure TgxShadowHDS.SetSoftRange(AValue: cardinal);
begin
  with OwnerHDS.Data.LockList do
    try
      FSoftRange := MaxInteger(AValue, 1);
      // MarkDirty;
    finally
      OwnerHDS.Data.UnlockList;
    end;
end;

procedure TgxShadowHDS.SetDiffuse(AValue: single);
begin
  with OwnerHDS.Data.LockList do
    try
      FDiffuse := ClampValue(AValue, 0.001, 1);
      // MarkDirty;
    finally
      OwnerHDS.Data.UnlockList;
    end;
end;

procedure TgxShadowHDS.SetAmbient(AValue: single);
begin
  with OwnerHDS.Data.LockList do
    try
      FAmbient := ClampValue(AValue, 0.001, 1);
      // MarkDirty;
    finally
      OwnerHDS.Data.UnlockList;
    end;
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterClass(TgxShadowHDS);

end.
