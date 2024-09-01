//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Skydome;

(* Skydome object *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Math,
  FMX.Graphics,

  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.Scene,
  GXS.Context,
  GXS.State,
  GXS.Graphics,
  GXS.Color,
  GXS.Material,
  GXS.RenderContextInfo;

type
  TgxStarRecord = packed record
    RA: Word; // x100 builtin factor, degrees
    DEC: SmallInt; // x100 builtin factor, degrees
    BVColorIndex: Byte; // x100 builtin factor
    VMagnitude: Byte; // x10 builtin factor
  end;
  PgxStarRecord = ^TgxStarRecord;

// ------------------------- SkyBox class -------------------------

TgxSkyBoxStyle = (sbsFull, sbsTopHalf, sbsBottomHalf, sbTopTwoThirds, sbsTopHalfClamped);

  TgxSkyBox = class(TgxCameraInvariantObject, IgxMaterialLibrarySupported)
  private
    FMatNameTop: string;
    FMatNameRight: string;
    FMatNameFront: string;
    FMatNameLeft: string;
    FMatNameBack: string;
    FMatNameBottom: string;
    FMatNameClouds: string;
    FMaterialLibrary: TgxMaterialLibrary;
    FCloudsPlaneOffset: Single;
    FCloudsPlaneSize: Single;
    FStyle: TgxSkyBoxStyle;
    //implementing IgxMaterialLibrarySupported
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
  protected
    procedure SetMaterialLibrary(const Value: TgxMaterialLibrary);
    procedure SetMatNameBack(const Value: string);
    procedure SetMatNameBottom(const Value: string);
    procedure SetMatNameFront(const Value: string);
    procedure SetMatNameLeft(const Value: string);
    procedure SetMatNameRight(const Value: string);
    procedure SetMatNameTop(const Value: string);
    procedure SetMatNameClouds(const Value: string);
    procedure SetCloudsPlaneOffset(const Value: single);
    procedure SetCloudsPlaneSize(const Value: single);
    procedure SetStyle(const value: TgxSkyBoxStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TgxRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
    procedure BuildList(var ARci: TgxRenderContextInfo); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property MaterialLibrary: TgxMaterialLibrary read FMaterialLibrary write SetMaterialLibrary;
    property MatNameTop: TgxLibMaterialName read FMatNameTop write SetMatNameTop;
    property MatNameBottom: TgxLibMaterialName read FMatNameBottom write SetMatNameBottom;
    property MatNameLeft: TgxLibMaterialName read FMatNameLeft write SetMatNameLeft;
    property MatNameRight: TgxLibMaterialName read FMatNameRight write SetMatNameRight;
    property MatNameFront: TgxLibMaterialName read FMatNameFront write SetMatNameFront;
    property MatNameBack: TgxLibMaterialName read FMatNameBack write SetMatNameBack;
    property MatNameClouds: TgxLibMaterialName read FMatNameClouds write SetMatNameClouds;
    property CloudsPlaneOffset: Single read FCloudsPlaneOffset write SetCloudsPlaneOffset;
    property CloudsPlaneSize: Single read FCloudsPlaneSize write SetCloudsPlaneSize;
    property Style: TgxSkyBoxStyle read FStyle write FStyle default sbsFull;
  end;

//--------------------- SkyDome classes -----------------------------

  TgxSkyDomeBand = class(TCollectionItem)
  private
    FStartAngle: Single;
    FStopAngle: Single;
    FStartColor: TgxColor;
    FStopColor: TgxColor;
    FSlices: Integer;
    FStacks: Integer;
  protected
    function GetDisplayName: string; override;
    procedure SetStartAngle(const val: Single);
    procedure SetStartColor(const val: TgxColor);
    procedure SetStopAngle(const val: Single);
    procedure SetStopColor(const val: TgxColor);
    procedure SetSlices(const val: Integer);
    procedure SetStacks(const val: Integer);
    procedure OnColorChange(sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo);
  published
    property StartAngle: Single read FStartAngle write SetStartAngle;
    property StartColor: TgxColor read FStartColor write SetStartColor;
    property StopAngle: Single read FStopAngle write SetStopAngle;
    property StopColor: TgxColor read FStopColor write SetStopColor;
    property Slices: Integer read FSlices write SetSlices default 12;
    property Stacks: Integer read FStacks write SetStacks default 1;
  end;

  TgxSkyDomeBands = class(TCollection)
  protected
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TgxSkyDomeBand);
    function GetItems(index: Integer): TgxSkyDomeBand;
  public
    constructor Create(AOwner: TComponent);
    function Add: TgxSkyDomeBand;
    function FindItemID(ID: Integer): TgxSkyDomeBand;
    property Items[index: Integer]: TgxSkyDomeBand read GetItems write SetItems; default;
    procedure NotifyChange;
    procedure BuildList(var rci: TgxRenderContextInfo);
  end;

  TgxSkyDomeStar = class(TCollectionItem)
  private
    FRA, FDec: Single;
    FMagnitude: Single;
    FColor: TColor;
    FCacheCoord: TAffineVector; // cached cartesian coordinates
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    // Right Ascension, in degrees.
    property RA: Single read FRA write FRA;
    // Declination, in degrees.
    property DEC: Single read FDec write FDec;
    // Absolute magnitude.
    property Magnitude: Single read FMagnitude write FMagnitude;
    // Color of the star.
    property Color: TColor read FColor write FColor;
  end;

  TgxSkyDomeStars = class(TCollection)
  protected
    owner: TComponent;
    function GetOwner: TPersistent; override;
    procedure SetItems(index: Integer; const val: TgxSkyDomeStar);
    function GetItems(index: Integer): TgxSkyDomeStar;
    procedure PrecomputeCartesianCoordinates;
  public
    constructor Create(AOwner: TComponent);
    function Add: TgxSkyDomeStar;
    function FindItemID(ID: Integer): TgxSkyDomeStar;
    property Items[index: Integer]: TgxSkyDomeStar read GetItems write SetItems; default;
    procedure BuildList(var rci: TgxRenderContextInfo; twinkle: Boolean);
    (* Adds nb random stars of the given color.
      Stars are homogenously scattered on the complete sphere, not only the band defined or visible dome. *)
    procedure AddRandomStars(const nb: Integer; const Color: TColor; const limitToTopDome: Boolean = False); overload;
    procedure AddRandomStars(const nb: Integer; const ColorMin, ColorMax: TVector3b;
      const Magnitude_min, Magnitude_max: Single;
      const limitToTopDome: Boolean = False); overload;
    (* Load a 'stars' file, which is made of TGLStarRecord.
       Not that '.stars' files should already be sorted by magnitude and color. *)
    procedure LoadStarsFile(const starsFileName: string);
  end;

  TgxSkyDomeOption = (sdoEquatorialGrid, sdoEclipticGrid, sdoGalacticGrid, sdoSupergalacticGrid, sdoTwinkle);
  TgxSkyDomeOptions = set of TgxSkyDomeOption;

  (* Renders a sky dome always centered on the camera.
    If you use this object make sure it is rendered *first*, as it ignores
    depth buffering and overwrites everything. All children of a skydome
    are rendered in the skydome's coordinate system.
    The skydome is described by "bands", each "band" is an horizontal cut
    of a sphere, and you can have as many bands as you wish. *)
  TgxSkyDome = class(TgxCameraInvariantObject)
  private
    FOptions: TgxSkyDomeOptions;
    FBands: TgxSkyDomeBands;
    FStars: TgxSkyDomeStars;
  protected
    procedure SetBands(const val: TgxSkyDomeBands);
    procedure SetStars(const val: TgxSkyDomeStars);
    procedure SetOptions(const val: TgxSkyDomeOptions);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
  published
    property Bands: TgxSkyDomeBands read FBands write SetBands;
    property Stars: TgxSkyDomeStars read FStars write SetStars;
    property Options: TgxSkyDomeOptions read FOptions write SetOptions default [];
  end;

  TEarthSkydomeOption = (esoFadeStarsWithSun, esoRotateOnTwelveHours, esoDepthTest);
  TEarthSkydomeOptions = set of TEarthSkydomeOption;

  (* Render a skydome like what can be seen on earth.
    Color is based on sun position and turbidity, to "mimic" atmospheric
    Rayleigh and Mie scatterings. The colors can be adjusted to render exoplanet atmospheres too.
    The default slices/stacks values make for an average quality rendering,
    for a very clean rendering, use 64/64 (more is overkill in most cases).
    The complexity is quite high though, making a T&L 3D board a necessity
    for using TgxEarthSkyDome. *)
  TgxEarthSkyDome = class(TgxSkyDome)
  private
    FSunElevation: Single;
    FTurbidity: Single;
    FCurSunColor, FCurSkyColor, FCurHazeColor: TgxColorVector;
    FCurHazeTurbid, FCurSunSkyTurbid: Single;
    FSunZenithColor: TgxColor;
    FSunDawnColor: TgxColor;
    FHazeColor: TgxColor;
    FSkyColor: TgxColor;
    FNightColor: TgxColor;
    FDeepColor: TgxColor;
    FSlices, FStacks: Integer;
    FExtendedOptions: TEarthSkydomeOptions;
    FMorning: Boolean;
  protected
    procedure Loaded; override;
    procedure SetSunElevation(const val: Single);
    procedure SetTurbidity(const val: Single);
    procedure SetSunZenithColor(const val: TgxColor);
    procedure SetSunDawnColor(const val: TgxColor);
    procedure SetHazeColor(const val: TgxColor);
    procedure SetSkyColor(const val: TgxColor);
    procedure SetNightColor(const val: TgxColor);
    procedure SetDeepColor(const val: TgxColor);
    procedure SetSlices(const val: Integer);
    procedure SetStacks(const val: Integer);
    procedure OnColorChanged(sender: TObject);
    procedure PreCalculate;
    procedure RenderDome;
    function CalculateColor(const theta, cosGamma: Single): TgxColorVector;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure SetSunAtTime(HH, MM: Single);
  published
    // Elevation of the sun, measured in degrees
    property SunElevation: Single read FSunElevation write SetSunElevation;
    // Expresses the purity of air. Value range is from 1 (pure atmosphere) to 120 (very nebulous)
    property Turbidity: Single read FTurbidity write SetTurbidity;
    property SunZenithColor: TgxColor read FSunZenithColor write SetSunZenithColor;
    property SunDawnColor: TgxColor read FSunDawnColor write SetSunDawnColor;
    property HazeColor: TgxColor read FHazeColor write SetHazeColor;
    property SkyColor: TgxColor read FSkyColor write SetSkyColor;
    property NightColor: TgxColor read FNightColor write SetNightColor;
    property DeepColor: TgxColor read FDeepColor write SetDeepColor;
    property ExtendedOptions: TEarthSkydomeOptions read FExtendedOptions write FExtendedOptions;
    property Slices: Integer read FSlices write SetSlices default 24;
    property Stacks: Integer read FStacks write SetStacks default 48;
  end;

  // Computes position on the unit sphere of a star record (Z=up).
function StarRecordPositionZUp(const starRecord: TgxStarRecord): TAffineVector;
// Computes position on the unit sphere of a star record (Y=up).
function StarRecordPositionYUp(const starRecord: TgxStarRecord): TAffineVector;
// Computes star color from BV index (RGB) and magnitude (alpha).
function StarRecordColor(const starRecord: TgxStarRecord; bias: Single): TVector4f;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------


// ------------------
// ------------------ TgxSkyBox ------------------
// ------------------

constructor TgxSkyBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CamInvarianceMode := cimPosition;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FCloudsPlaneOffset := 0.2;
    // this should be set far enough to avoid near plane clipping
  FCloudsPlaneSize := 32;
    // the bigger, the more this extends the clouds cap to the horizon
end;

destructor TgxSkyBox.Destroy;
begin
  inherited;
end;

function TgxSkyBox.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxSkyBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FMaterialLibrary) then
    MaterialLibrary := nil;
  inherited;
end;

procedure TgxSkyBox.DoRender(var ARci: TgxRenderContextInfo; ARenderSelf,
  ARenderChildren: Boolean);
begin
  Arci.gxStates.DepthWriteMask := False;
  Arci.ignoreDepthRequests := true;
  inherited;
  Arci.ignoreDepthRequests := False;
end;

procedure TgxSkyBox.BuildList(var ARci: TgxRenderContextInfo);
var
  f, cps, cof1: Single;
  oldStates: TgxStates;
  libMat: TgxLibMaterial;
begin
  if FMaterialLibrary = nil then
    Exit;

  with ARci.gxStates do
  begin
    oldStates := States;
    Disable(stDepthTest);
    Disable(stLighting);
    Disable(stFog);
  end;

  glPushMatrix;
  f := ARci.rcci.farClippingDistance * 0.5;
  glScalef(f, f, f);

  try
    case Style of
      sbsFull: ;
      sbsTopHalf, sbsTopHalfClamped:
        begin
          glTranslatef(0, 0.5, 0);
          glScalef(1, 0.5, 1);
        end;
      sbsBottomHalf:
        begin
          glTranslatef(0, -0.5, 0);
          glScalef(1, 0.5, 1);
        end;
      sbTopTwoThirds:
        begin
          glTranslatef(0, 1 / 3, 0);
          glScalef(1, 2 / 3, 1);
        end;
    end;

    // FRONT
    libMat := MaterialLibrary.LibMaterialByName(FMatNameFront);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(-1, 1, -1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(-1, -1, -1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(1, -1, -1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(1, 1, -1);
        if Style = sbsTopHalfClamped then
        begin
          glTexCoord2f(0.002, 0.002);
          glVertex3f(-1, -1, -1);
          glTexCoord2f(0.002, 0.002);
          glVertex3f(-1, -3, -1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(1, -3, -1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(1, -1, -1);
        end;
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // BACK
    libMat := MaterialLibrary.LibMaterialByName(FMatNameBack);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(1, 1, 1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(1, -1, 1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(-1, -1, 1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(-1, 1, 1);
        if Style = sbsTopHalfClamped then
        begin
          glTexCoord2f(0.002, 0.002);
          glVertex3f(1, -1, 1);
          glTexCoord2f(0.002, 0.002);
          glVertex3f(1, -3, 1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(-1, -3, 1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(-1, -1, 1);
        end;
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // TOP
    libMat := MaterialLibrary.LibMaterialByName(FMatNameTop);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(-1, 1, 1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(-1, 1, -1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(1, 1, -1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(1, 1, 1);
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // BOTTOM
    libMat := MaterialLibrary.LibMaterialByName(FMatNameBottom);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(-1, -1, -1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(-1, -1, 1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(1, -1, 1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(1, -1, -1);
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // LEFT
    libMat := MaterialLibrary.LibMaterialByName(FMatNameLeft);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(-1, 1, 1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(-1, -1, 1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(-1, -1, -1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(-1, 1, -1);
        if Style = sbsTopHalfClamped then
        begin
          glTexCoord2f(0.002, 0.002);
          glVertex3f(-1, -1, 1);
          glTexCoord2f(0.002, 0.002);
          glVertex3f(-1, -3, 1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(-1, -3, -1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(-1, -1, -1);
        end;
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // RIGHT
    libMat := MaterialLibrary.LibMaterialByName(FMatNameRight);
    if libMat <> nil then
    begin
      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0.002, 0.998);
        glVertex3f(1, 1, -1);
        glTexCoord2f(0.002, 0.002);
        glVertex3f(1, -1, -1);
        glTexCoord2f(0.998, 0.002);
        glVertex3f(1, -1, 1);
        glTexCoord2f(0.998, 0.998);
        glVertex3f(1, 1, 1);
        if Style = sbsTopHalfClamped then
        begin
          glTexCoord2f(0.002, 0.002);
          glVertex3f(1, -1, -1);
          glTexCoord2f(0.002, 0.002);
          glVertex3f(1, -3, -1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(1, -3, 1);
          glTexCoord2f(0.998, 0.002);
          glVertex3f(1, -1, 1);
        end;
        glEnd;
      until not libMat.UnApply(ARci);
    end;
    // CLOUDS CAP PLANE
    libMat := MaterialLibrary.LibMaterialByName(FMatNameClouds);
    if libMat <> nil then
    begin
      // pre-calculate possible values to speed up
      cps := FCloudsPlaneSize * 0.5;
      cof1 := FCloudsPlaneOffset;

      libMat.Apply(ARci);
      repeat
        glBegin(GL_QUADS);
        glTexCoord2f(0, 1);
        glVertex3f(-cps, cof1, cps);
        glTexCoord2f(0, 0);
        glVertex3f(-cps, cof1, -cps);
        glTexCoord2f(1, 0);
        glVertex3f(cps, cof1, -cps);
        glTexCoord2f(1, 1);
        glVertex3f(cps, cof1, cps);
        glEnd;
      until not libMat.UnApply(ARci);
    end;

    glPopMatrix;
    if stLighting in oldStates then
      ARci.gxStates.Enable(stLighting);
    if stFog in oldStates then
      ARci.gxStates.Enable(stFog);
    if stDepthTest in oldStates then
      ARci.gxStates.Enable(stDepthTest);

  finally
  end;
end;

procedure TgxSkyBox.SetCloudsPlaneOffset(const Value: single);
begin
  FCloudsPlaneOffset := Value;
  StructureChanged;
end;

procedure TgxSkyBox.SetCloudsPlaneSize(const Value: single);
begin
  FCloudsPlaneSize := Value;
  StructureChanged;
end;

procedure TgxSkyBox.SetStyle(const value: TgxSkyBoxStyle);
begin
  FStyle := value;
  StructureChanged;
end;

procedure TgxSkyBox.SetMaterialLibrary(const value: TgxMaterialLibrary);
begin
  FMaterialLibrary := value;
  StructureChanged;
end;

procedure TgxSkyBox.SetMatNameBack(const Value: string);
begin
  FMatNameBack := Value;
  StructureChanged;
end;

procedure TgxSkyBox.SetMatNameBottom(const Value: string);
begin
  FMatNameBottom := Value;
  StructureChanged;
end;

procedure TgxSkyBox.SetMatNameClouds(const Value: string);
begin
  FMatNameClouds := Value;
  StructureChanged;
end;

procedure TgxSkyBox.SetMatNameFront(const Value: string);
begin
  FMatNameFront := Value;
  StructureChanged;
end;

procedure TgxSkyBox.SetMatNameLeft(const Value: string);
begin
  FMatNameLeft := Value;
  StructureChanged;
end;

procedure TgxSkyBox.SetMatNameRight(const Value: string);
begin
  FMatNameRight := Value;
  StructureChanged;
end;

procedure TgxSkyBox.SetMatNameTop(const Value: string);
begin
  FMatNameTop := Value;
  StructureChanged;
end;

//--------------------- SkyDome Region ------------------------------

function StarRecordPositionYUp(const starRecord: TgxStarRecord): TAffineVector;
var
  f: Single;
begin
  SinCosine(starRecord.DEC * (0.01 * PI / 180), Result.Y, f);
  SinCosine(starRecord.RA * (0.01 * PI / 180), f, Result.X, Result.Z);
end;

function StarRecordPositionZUp(const starRecord: TgxStarRecord): TAffineVector;
var
  f: Single;
begin
  SinCosine(starRecord.DEC * (0.01 * PI / 180), Result.Z, f);
  SinCosine(starRecord.RA * (0.01 * PI / 180), f, Result.X, Result.Y);
end;

function StarRecordColor(const starRecord: TgxStarRecord; bias: Single): TVector4f;
const
  // very *rough* approximation
  cBVm035: TVector4f = (X: 0.7; Y: 0.8; Z: 1.0; W: 1);
  cBV015: TVector4f = (X: 1.0; Y: 1.0; Z: 1.0; W: 1);
  cBV060: TVector4f = (X: 1.0; Y: 1.0; Z: 0.7; W: 1);
  cBV135: TVector4f = (X: 1.0; Y: 0.8; Z: 0.7; W: 1);
var
  bvIndex100: Integer;
begin
  bvIndex100 := starRecord.BVColorIndex - 50;
  // compute RGB color for B&V index
  if bvIndex100 < -035 then
    Result := cBVm035
  else if bvIndex100 < 015 then
    VectorLerp(cBVm035, cBV015, (bvIndex100 + 035) * (1 / (015 + 035)), Result)
  else if bvIndex100 < 060 then
    VectorLerp(cBV015, cBV060, (bvIndex100 - 015) * (1 / (060 - 015)), Result)
  else if bvIndex100 < 135 then
    VectorLerp(cBV060, cBV135, (bvIndex100 - 060) * (1 / (135 - 060)), Result)
  else
    Result := cBV135;
  // compute transparency for VMag
  // the actual factor is 2.512, and not used here
  Result.W := PowerSingle(1.2, -(starRecord.VMagnitude * 0.1 - bias));
end;

// ------------------
// ------------------ TgxSkyDomeBand ------------------
// ------------------

constructor TgxSkyDomeBand.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FStartColor := TgxColor.Create(Self);
  FStartColor.Initialize(clrBlue);
  FStartColor.OnNotifyChange := OnColorChange;
  FStopColor := TgxColor.Create(Self);
  FStopColor.Initialize(clrBlue);
  FStopColor.OnNotifyChange := OnColorChange;
  FSlices := 12;
  FStacks := 1;
end;

destructor TgxSkyDomeBand.Destroy;
begin
  FStartColor.Free;
  FStopColor.Free;
  inherited Destroy;
end;

procedure TgxSkyDomeBand.Assign(Source: TPersistent);
begin
  if Source is TgxSkyDomeBand then
  begin
    FStartAngle := TgxSkyDomeBand(Source).FStartAngle;
    FStopAngle := TgxSkyDomeBand(Source).FStopAngle;
    FStartColor.Assign(TgxSkyDomeBand(Source).FStartColor);
    FStopColor.Assign(TgxSkyDomeBand(Source).FStopColor);
    FSlices := TgxSkyDomeBand(Source).FSlices;
    FStacks := TgxSkyDomeBand(Source).FStacks;
  end;
  inherited Destroy;
end;

function TgxSkyDomeBand.GetDisplayName: string;
begin
  Result := Format('%d: %.1f° - %.1f°', [Index, StartAngle, StopAngle]);
end;

procedure TgxSkyDomeBand.SetStartAngle(const val: Single);
begin
  FStartAngle := ClampValue(val, -90, 90);
  if FStartAngle > FStopAngle then
    FStopAngle := FStartAngle;
  TgxSkyDomeBands(Collection).NotifyChange;
end;

procedure TgxSkyDomeBand.SetStartColor(const val: TgxColor);
begin
  FStartColor.Assign(val);
end;

procedure TgxSkyDomeBand.SetStopAngle(const val: Single);
begin
  FStopAngle := ClampValue(val, -90, 90);
  if FStopAngle < FStartAngle then
    FStartAngle := FStopAngle;
  TgxSkyDomeBands(Collection).NotifyChange;
end;

procedure TgxSkyDomeBand.SetStopColor(const val: TgxColor);
begin
  FStopColor.Assign(val);
end;

procedure TgxSkyDomeBand.SetSlices(const val: Integer);
begin
  if val < 3 then
    FSlices := 3
  else
    FSlices := val;
  TgxSkyDomeBands(Collection).NotifyChange;
end;

procedure TgxSkyDomeBand.SetStacks(const val: Integer);
begin
  if val < 1 then
    FStacks := 1
  else
    FStacks := val;
  TgxSkyDomeBands(Collection).NotifyChange;
end;

procedure TgxSkyDomeBand.OnColorChange(sender: TObject);
begin
  TgxSkyDomeBands(Collection).NotifyChange;
end;

procedure TgxSkyDomeBand.BuildList(var rci: TgxRenderContextInfo);

// coordinates system note: X is forward, Y is left and Z is up
// always rendered as sphere of radius 1

  procedure RenderBand(start, stop: Single;
    const colStart, colStop: TgxColorVector);
  var
    i: Integer;
    f, r, r2: Single;
    vertex1, vertex2: TVector4f;
  begin
    vertex1.W := 1;
    if start = -90 then
    begin
      // triangle fan with south pole
      glBegin(GL_TRIANGLE_FAN);
      glColor4fv(@colStart);
      glVertex3f(0, 0, -1);
      f := 2 * PI / Slices;
      SinCosine(DegToRadian(stop), vertex1.Z, r);
      glColor4fv(@colStop);
      for i := 0 to Slices do
      begin
        SinCosine(i * f, r, vertex1.Y, vertex1.X);
        glVertex4fv(@vertex1);
      end;
      glEnd;
    end
    else if stop = 90 then
    begin
      // triangle fan with north pole
      glBegin(GL_TRIANGLE_FAN);
      glColor4fv(@colStop);
      glVertex3fv(@ZHmgPoint);
      f := 2 * PI / Slices;
      SinCosine(DegToRadian(start), vertex1.Z, r);
      glColor4fv(@colStart);
      for i := Slices downto 0 do
      begin
        SinCosine(i * f, r, vertex1.Y, vertex1.X);
        glVertex4fv(@vertex1);
      end;
      glEnd;
    end
    else
    begin
      vertex2.W := 1;
      // triangle strip
      glBegin(GL_TRIANGLE_STRIP);
      f := 2 * PI / Slices;
      SinCosine(DegToRadian(start), vertex1.Z, r);
      SinCosine(DegToRadian(stop), vertex2.Z, r2);
      for i := 0 to Slices do
      begin
        SinCosine(i * f, r, vertex1.Y, vertex1.X);
        glColor4fv(@colStart);
        glVertex4fv(@vertex1);
        SinCosine(i * f, r2, vertex2.Y, vertex2.X);
        glColor4fv(@colStop);
        glVertex4fv(@vertex2);
      end;
      glEnd;
    end;
  end;

var
  n: Integer;
  t, t2: Single;
begin
  if StartAngle = StopAngle then
    Exit;
  for n := 0 to Stacks - 1 do
  begin
    t := n / Stacks;
    t2 := (n + 1) / Stacks;
    RenderBand(Lerp(StartAngle, StopAngle, t), Lerp(StartAngle, StopAngle, t2),
      VectorLerp(StartColor.Color, StopColor.Color, t),
      VectorLerp(StartColor.Color, StopColor.Color, t2));
  end;
end;

// ------------------
// ------------------ TgxSkyDomeBands ------------------
// ------------------

constructor TgxSkyDomeBands.Create(AOwner: TComponent);
begin
  owner := AOwner;
  inherited Create(TgxSkyDomeBand);
end;

function TgxSkyDomeBands.GetOwner: TPersistent;
begin
  Result := owner;
end;

procedure TgxSkyDomeBands.SetItems(index: Integer; const val: TgxSkyDomeBand);
begin
  inherited Items[index] := val;
end;

function TgxSkyDomeBands.GetItems(index: Integer): TgxSkyDomeBand;
begin
  Result := TgxSkyDomeBand(inherited Items[index]);
end;

function TgxSkyDomeBands.Add: TgxSkyDomeBand;
begin
  Result := (inherited Add) as TgxSkyDomeBand;
end;

function TgxSkyDomeBands.FindItemID(ID: Integer): TgxSkyDomeBand;
begin
  Result := (inherited FindItemID(ID)) as TgxSkyDomeBand;
end;

procedure TgxSkyDomeBands.NotifyChange;
begin
  if Assigned(owner) and (owner is TgxBaseSceneObject) then
    TgxBaseSceneObject(owner).StructureChanged;
end;

procedure TgxSkyDomeBands.BuildList(var rci: TgxRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].BuildList(rci);
end;

// ------------------
// ------------------ TgxSkyDomeStar ------------------
// ------------------

constructor TgxSkyDomeStar.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TgxSkyDomeStar.Destroy;
begin
  inherited Destroy;
end;

procedure TgxSkyDomeStar.Assign(Source: TPersistent);
begin
  if Source is TgxSkyDomeStar then
  begin
    FRA := TgxSkyDomeStar(Source).FRA;
    FDec := TgxSkyDomeStar(Source).FDec;
    FMagnitude := TgxSkyDomeStar(Source).FMagnitude;
    FColor := TgxSkyDomeStar(Source).FColor;
    SetVector(FCacheCoord, TgxSkyDomeStar(Source).FCacheCoord);
  end;
  inherited Destroy;
end;

function TgxSkyDomeStar.GetDisplayName: string;
begin
  Result := Format('RA: %5.1f / Dec: %5.1f', [RA, DEC]);
end;

// ------------------
// ------------------ TgxSkyDomeStars ------------------
// ------------------

constructor TgxSkyDomeStars.Create(AOwner: TComponent);
begin
  owner := AOwner;
  inherited Create(TgxSkyDomeStar);
end;

function TgxSkyDomeStars.GetOwner: TPersistent;
begin
  Result := owner;
end;

procedure TgxSkyDomeStars.SetItems(index: Integer; const val: TgxSkyDomeStar);
begin
  inherited Items[index] := val;
end;

function TgxSkyDomeStars.GetItems(index: Integer): TgxSkyDomeStar;
begin
  Result := TgxSkyDomeStar(inherited Items[index]);
end;

function TgxSkyDomeStars.Add: TgxSkyDomeStar;
begin
  Result := (inherited Add) as TgxSkyDomeStar;
end;

function TgxSkyDomeStars.FindItemID(ID: Integer): TgxSkyDomeStar;
begin
  Result := (inherited FindItemID(ID)) as TgxSkyDomeStar;
end;

procedure TgxSkyDomeStars.PrecomputeCartesianCoordinates;
var
  i: Integer;
  star: TgxSkyDomeStar;
  raC, raS, decC, decS: Single;
begin
  // to be enhanced...
  for i := 0 to Count - 1 do
  begin
    star := Items[i];
    SinCosine(star.DEC * cPIdiv180, decS, decC);
    SinCosine(star.RA * cPIdiv180, decC, raS, raC);
    star.FCacheCoord.X := raC;
    star.FCacheCoord.Y := raS;
    star.FCacheCoord.Z := decS;
  end;
end;

procedure TgxSkyDomeStars.BuildList(var rci: TgxRenderContextInfo;
  twinkle: Boolean);
var
  i, n: Integer;
  star: TgxSkyDomeStar;
  lastColor: TColor;
  lastPointSize10, pointSize10: Integer;
  Color, twinkleColor: TgxColorVector;

  procedure DoTwinkle;
  begin
    if (n and 63) = 0 then
    begin
      twinkleColor := VectorScale(Color, Random * 0.6 + 0.4);
      glColor3fv(@twinkleColor.X);
      n := 0;
    end
    else
      Inc(n);
  end;

begin
  if Count = 0 then
    Exit;
  PrecomputeCartesianCoordinates;
  lastColor := -1;
  n := 0;
  lastPointSize10 := -1;

  rci.gxStates.Enable(stPointSmooth);
  rci.gxStates.Enable(stAlphaTest);
  rci.gxStates.SetAlphaFunction(cfNotEqual, 0.0);
  rci.gxStates.Enable(stBlend);
  rci.gxStates.SetBlendFunc(bfSrcAlpha, bfOne);

  glBegin(GL_POINTS);
  for i := 0 to Count - 1 do
  begin
    star := Items[i];
    pointSize10 := Round((4.5 - star.Magnitude) * 10);
    if pointSize10 <> lastPointSize10 then
    begin
      if pointSize10 > 15 then
      begin
        glEnd;
        lastPointSize10 := pointSize10;
        rci.gxStates.PointSize := pointSize10 * 0.1;
        glBegin(GL_POINTS);
      end
      else if lastPointSize10 <> 15 then
      begin
        glEnd;
        lastPointSize10 := 15;
        rci.gxStates.PointSize := 1.5;
        glBegin(GL_POINTS);
      end;
    end;
    if lastColor <> star.FColor then
    begin
      Color := ConvertWinColor(star.FColor);
      if twinkle then
      begin
        n := 0;
        DoTwinkle;
      end
      else
        glColor3fv(@Color.X);
      lastColor := star.FColor;
    end
    else if twinkle then
      DoTwinkle;
    glVertex3fv(@star.FCacheCoord.X);
  end;
  glEnd;

  // restore default AlphaFunc
  rci.gxStates.SetAlphaFunction(cfGreater, 0);
end;

procedure TgxSkyDomeStars.AddRandomStars(const nb: Integer; const Color: TColor;
  const limitToTopDome: Boolean = False);
var
  i: Integer;
  coord: TAffineVector;
  star: TgxSkyDomeStar;
begin
  for i := 1 to nb do
  begin
    star := Add;
    // pick a point in the half-cube
    if limitToTopDome then
      coord.Z := Random
    else
      coord.Z := Random * 2 - 1;
    // calculate RA and Dec
    star.DEC := ArcSin(coord.Z) * c180divPI;
    star.RA := Random * 360 - 180;
    // pick a color
    star.Color := Color;
    // pick a magnitude
    star.Magnitude := 3;
  end;
end;

//------------------------------------------------------------
procedure TgxSkyDomeStars.AddRandomStars(const nb: Integer;
  const ColorMin, ColorMax: TVector3b;
  const Magnitude_min, Magnitude_max: Single;
  const limitToTopDome: Boolean = False);

  function RandomTT(Min, Max: Byte): Byte;
  begin
    Result := Min + Random(Max - Min);
  end;

var
  i: Integer;
  coord: TAffineVector;
  star: TgxSkyDomeStar;

begin
  for i := 1 to nb do
  begin
    star := Add;
    // pick a point in the half-cube
    if limitToTopDome then
      coord.Z := Random
    else
      coord.Z := Random * 2 - 1;
    // calculate RA and Dec
    star.DEC := ArcSin(coord.Z) * c180divPI;
    star.RA := Random * 360 - 180;
    // pick a color
    star.Color := RGB2Color(RandomTT(ColorMin.X, ColorMax.X),
      RandomTT(ColorMin.Y, ColorMax.Y), RandomTT(ColorMin.Z, ColorMax.Z));
    // pick a magnitude
    star.Magnitude := Magnitude_min + Random * (Magnitude_max - Magnitude_min);
  end;
end;

procedure TgxSkyDomeStars.LoadStarsFile(const starsFileName: string);
var
  fs: TFileStream;
  sr: TgxStarRecord;
  colorVector: TgxColorVector;
begin
  fs := TFileStream.Create(starsFileName, fmOpenRead + fmShareDenyWrite);
  try
    while fs.Position < fs.Size do
    begin
      fs.Read(sr, SizeOf(sr));
      with Add do
      begin
        RA := sr.RA * 0.01;
        DEC := sr.DEC * 0.01;
        colorVector := StarRecordColor(sr, 3);
        Magnitude := sr.VMagnitude * 0.1;
        if sr.VMagnitude > 35 then
          Color := ConvertColorVector(colorVector, colorVector.W)
        else
          Color := ConvertColorVector(colorVector);
      end;
    end;
  finally
    fs.Free;
  end;
end;

// ------------------
// ------------------ TgxSkyDome ------------------
// ------------------

constructor TgxSkyDome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CamInvarianceMode := cimPosition;
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FBands := TgxSkyDomeBands.Create(Self);
  with FBands.Add do
  begin
    StartAngle := 0;
    StartColor.Color := clrWhite;
    StopAngle := 15;
    StopColor.Color := clrBlue;
  end;
  with FBands.Add do
  begin
    StartAngle := 15;
    StartColor.Color := clrBlue;
    StopAngle := 90;
    Stacks := 4;
    StopColor.Color := clrNavy;
  end;
  FStars := TgxSkyDomeStars.Create(Self);
end;

destructor TgxSkyDome.Destroy;
begin
  FStars.Free;
  FBands.Free;
  inherited Destroy;
end;

procedure TgxSkyDome.Assign(Source: TPersistent);
begin
  if Source is TgxSkyDome then
  begin
    FBands.Assign(TgxSkyDome(Source).FBands);
    FStars.Assign(TgxSkyDome(Source).FStars);
  end;
  inherited;
end;

procedure TgxSkyDome.SetBands(const val: TgxSkyDomeBands);
begin
  FBands.Assign(val);
  StructureChanged;
end;

procedure TgxSkyDome.SetStars(const val: TgxSkyDomeStars);
begin
  FStars.Assign(val);
  StructureChanged;
end;

procedure TgxSkyDome.SetOptions(const val: TgxSkyDomeOptions);
begin
  if val <> FOptions then
  begin
    FOptions := val;
    if sdoTwinkle in FOptions then
      ObjectStyle := ObjectStyle + [osDirectDraw]
    else
    begin
      ObjectStyle := ObjectStyle - [osDirectDraw];
      DestroyHandle;
    end;
    StructureChanged;
  end;
end;

procedure TgxSkyDome.BuildList(var rci: TgxRenderContextInfo);
var
  f: Single;
begin
  // setup states
  rci.gxStates.Disable(stLighting); // 8
  rci.gxStates.Disable(stDepthTest);
  rci.gxStates.Disable(stFog);
  rci.gxStates.Disable(stCullFace);
  rci.gxStates.Disable(stBlend); // 2
  rci.gxStates.DepthWriteMask := False;
  rci.gxStates.PolygonMode := pmFill;

  f := rci.rcci.farClippingDistance * 0.90;
  glScalef(f, f, f);

  Bands.BuildList(rci);
  Stars.BuildList(rci, (sdoTwinkle in FOptions));
end;

// ------------------
// ------------------ TgxEarthSkyDome ------------------
// ------------------

constructor TgxEarthSkyDome.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMorning := true;
  Bands.Clear;
  FSunElevation := 75;
  FTurbidity := 15;
  FSunZenithColor := TgxColor.CreateInitialized(Self, clrWhite, OnColorChanged);
  FSunDawnColor := TgxColor.CreateInitialized(Self, Vectormake(1, 0.5, 0, 0),
    OnColorChanged);
  FHazeColor := TgxColor.CreateInitialized(Self, Vectormake(0.9, 0.95, 1, 0),
    OnColorChanged);
  FSkyColor := TgxColor.CreateInitialized(Self, Vectormake(0.45, 0.6, 0.9, 0),
    OnColorChanged);
  FNightColor := TgxColor.CreateInitialized(Self, clrTransparent,
    OnColorChanged);
  FDeepColor := TgxColor.CreateInitialized(Self, Vectormake(0, 0.2, 0.4, 0));
  FStacks := 24;
  FSlices := 48;
  PreCalculate;
end;

destructor TgxEarthSkyDome.Destroy;
begin
  FSunZenithColor.Free;
  FSunDawnColor.Free;
  FHazeColor.Free;
  FSkyColor.Free;
  FNightColor.Free;
  FDeepColor.Free;
  inherited Destroy;
end;

procedure TgxEarthSkyDome.Assign(Source: TPersistent);
begin
  if Source is TgxSkyDome then
  begin
    FSunElevation := TgxEarthSkyDome(Source).SunElevation;
    FTurbidity := TgxEarthSkyDome(Source).Turbidity;
    FSunZenithColor.Assign(TgxEarthSkyDome(Source).FSunZenithColor);
    FSunDawnColor.Assign(TgxEarthSkyDome(Source).FSunDawnColor);
    FHazeColor.Assign(TgxEarthSkyDome(Source).FHazeColor);
    FSkyColor.Assign(TgxEarthSkyDome(Source).FSkyColor);
    FNightColor.Assign(TgxEarthSkyDome(Source).FNightColor);
    FSlices := TgxEarthSkyDome(Source).FSlices;
    FStacks := TgxEarthSkyDome(Source).FStacks;
    PreCalculate;
  end;
  inherited;
end;

procedure TgxEarthSkyDome.Loaded;
begin
  inherited;
  PreCalculate;
end;

procedure TgxEarthSkyDome.SetSunElevation(const val: Single);
var
  newVal: Single;
begin
  newVal := ClampValue(val, -90, 90);
  if FSunElevation <> newVal then
  begin
    FSunElevation := newVal;
    PreCalculate;
  end;
end;

procedure TgxEarthSkyDome.SetTurbidity(const val: Single);
begin
  FTurbidity := ClampValue(val, 1, 120);
  PreCalculate;
end;

procedure TgxEarthSkyDome.SetSunZenithColor(const val: TgxColor);
begin
  FSunZenithColor.Assign(val);
  PreCalculate;
end;

procedure TgxEarthSkyDome.SetSunDawnColor(const val: TgxColor);
begin
  FSunDawnColor.Assign(val);
  PreCalculate;
end;

procedure TgxEarthSkyDome.SetHazeColor(const val: TgxColor);
begin
  FHazeColor.Assign(val);
  PreCalculate;
end;

procedure TgxEarthSkyDome.SetSkyColor(const val: TgxColor);
begin
  FSkyColor.Assign(val);
  PreCalculate;
end;

procedure TgxEarthSkyDome.SetNightColor(const val: TgxColor);
begin
  FNightColor.Assign(val);
  PreCalculate;
end;

procedure TgxEarthSkyDome.SetDeepColor(const val: TgxColor);
begin
  FDeepColor.Assign(val);
  PreCalculate;
end;

procedure TgxEarthSkyDome.SetSlices(const val: Integer);
begin
  if val > 6 then
    FSlices := val
  else
    FSlices := 6;
  StructureChanged;
end;

procedure TgxEarthSkyDome.SetStacks(const val: Integer);
begin
  if val > 1 then
    FStacks := val
  else
    FStacks := 1;
  StructureChanged;
end;

procedure TgxEarthSkyDome.BuildList(var rci: TgxRenderContextInfo);
var
  f: Single;
begin
  // setup states
  with rci.gxStates do
  begin
    CurrentProgram := 0;
    Disable(stLighting);
    if esoDepthTest in FExtendedOptions then
    begin
      Enable(stDepthTest);
      DepthFunc := cfLEqual;
    end
    else
      Disable(stDepthTest);
    Disable(stFog);
    Disable(stCullFace);
    Disable(stBlend);
    Disable(stAlphaTest);
    DepthWriteMask := False;
    PolygonMode := pmFill;
  end;

  f := rci.rcci.farClippingDistance * 0.95;
  glScalef(f, f, f);

  RenderDome;
  Bands.BuildList(rci);
  Stars.BuildList(rci, (sdoTwinkle in FOptions));

  // restore
  rci.gxStates.DepthWriteMask := true;
end;

procedure TgxEarthSkyDome.OnColorChanged(sender: TObject);
begin
  PreCalculate;
end;

procedure TgxEarthSkyDome.SetSunAtTime(HH, MM: Single);
const
  cHourToElevation1: array [0 .. 23] of Single = (-45, -67.5, -90, -57.5, -45,
    -22.5, 0, 11.25, 22.5, 33.7, 45, 56.25, 67.5, 78.75, 90, 78.75, 67.5, 56.25,
    45, 33.7, 22.5, 11.25, 0, -22.5);
  cHourToElevation2: array [0 .. 23] of Single = (-0.375, -0.375, 0.375, 0.375,
    0.375, 0.375, 0.1875, 0.1875, 0.1875, 0.1875, 0.1875, 0.1875, 0.1875,
    0.1875, -0.1875, -0.1875, -0.1875, -0.1875, -0.1875, -0.1875, -0.1875,
    -0.1875, -0.375, -0.375);
var
  ts: Single;
  fts: Single;
  i: Integer;
  Color: TColor;
begin
  HH := Round(HH);
  if HH < 0 then
    HH := 0;
  if HH > 23 then
    HH := 23;
  if MM < 0 then
    MM := 0;
  if MM >= 60 then
  begin
    MM := 0;
    HH := HH + 1;
    if HH > 23 then
      HH := 0;
  end;
  FSunElevation := cHourToElevation1[Round(HH)] + cHourToElevation2
    [Round(HH)] * MM;

  ts := DegToRadian(90 - FSunElevation);
  // Mix base colors
  fts := exp(-6 * (PI / 2 - ts));
  VectorLerp(SunZenithColor.Color, SunDawnColor.Color, fts, FCurSunColor);
  fts := IntPower(1 - cos(ts - 0.5), 2);
  VectorLerp(HazeColor.Color, NightColor.Color, fts, FCurHazeColor);
  VectorLerp(SkyColor.Color, NightColor.Color, fts, FCurSkyColor);
  // Precalculate Turbidity factors
  FCurHazeTurbid := -sqrt(121 - Turbidity) * 2;
  FCurSunSkyTurbid := -(121 - Turbidity);

  // fade stars if required
  if SunElevation > -40 then
    ts := PowerInteger(1 - (SunElevation + 40) / 90, 11)
  else
    ts := 1;
  Color := RGB2Color(Round(ts * 255), Round(ts * 255), Round(ts * 255));
  if esoFadeStarsWithSun in ExtendedOptions then
    for i := 0 to Stars.Count - 1 do
      Stars[i].Color := Color;

  if esoRotateOnTwelveHours in ExtendedOptions then // spining around blue orb
  begin
    if (HH >= 14) and (FMorning = true) then
    begin
      roll(180);
      for i := 0 to Stars.Count - 1 do
        Stars[i].RA := Stars[i].RA + 180;
      FMorning := False;
    end;

    if (HH >= 2) and (HH < 14) and (FMorning = False) then
    begin
      roll(180);
      for i := 0 to Stars.Count - 1 do
        Stars[i].RA := Stars[i].RA + 180;
      FMorning := true;
    end;
  end;
  StructureChanged;
end;

procedure TgxEarthSkyDome.PreCalculate;
var
  ts: Single;
  fts: Single;
  i: Integer;
  Color: TColor;
begin
  ts := DegToRadian(90 - SunElevation);
  // Precompose base colors
  fts := exp(-6 * (PI / 2 - ts));
  VectorLerp(SunZenithColor.Color, SunDawnColor.Color, fts, FCurSunColor);
  fts := PowerInteger(1 - cos(ts - 0.5), 2);
  VectorLerp(HazeColor.Color, NightColor.Color, fts, FCurHazeColor);
  VectorLerp(SkyColor.Color, NightColor.Color, fts, FCurSkyColor);
  // Precalculate Turbidity factors
  FCurHazeTurbid := -sqrt(121 - Turbidity) * 2;
  FCurSunSkyTurbid := -(121 - Turbidity);

  // fade stars if required
  if SunElevation > -40 then
    ts := PowerInteger(1 - (SunElevation + 40) / 90, 11)
  else
    ts := 1;
  Color := RGB2Color(Round(ts * 255), Round(ts * 255), Round(ts * 255));
  if esoFadeStarsWithSun in ExtendedOptions then
    for i := 0 to Stars.Count - 1 do
      Stars[i].Color := Color;

  if esoRotateOnTwelveHours in ExtendedOptions then
  begin
    if SunElevation = 90 then
    begin
      roll(180);
      for i := 0 to Stars.Count - 1 do
        Stars[i].RA := Stars[i].RA + 180;
    end
    else if SunElevation = -90 then
    begin
      roll(180);
      for i := 0 to Stars.Count - 1 do
        Stars[i].RA := Stars[i].RA + 180;
    end;
  end;

  StructureChanged;
end;

function TgxEarthSkyDome.CalculateColor(const theta, cosGamma: Single)
  : TgxColorVector;
var
  t: Single;
begin
  t := PI / 2 - theta;
  // mix to get haze/sky
  VectorLerp(FCurSkyColor, FCurHazeColor, ClampValue(exp(FCurHazeTurbid * t), 0,
    1), Result);
  // then mix sky with sun
  VectorLerp(Result, FCurSunColor,
    ClampValue(exp(FCurSunSkyTurbid * cosGamma * (1 + t)) * 1.1, 0, 1), Result);
end;

procedure TgxEarthSkyDome.RenderDome;
var
  ts: Single;
  steps: Integer;
  sunPos: TAffineVector;
  sinTable, cosTable: PFloatArray;

  // coordinates system note: X is forward, Y is left and Z is up
  // always rendered as sphere of radius 1

  function CalculateCosGamma(const p: TVector4f): Single;
  begin
    Result := 1 - VectorAngleCosine(PAffineVector(@p)^, sunPos);
  end;

  procedure RenderDeepBand(stop: Single);
  var
    i: Integer;
    r, thetaStart: Single;
    vertex1: TVector4f;
    Color: TgxColorVector;
  begin
    r := 0;
    vertex1.W := 1;
    // triangle fan with south pole
    glBegin(GL_TRIANGLE_FAN);
    Color := CalculateColor(0, CalculateCosGamma(ZHmgPoint));
    glColor4fv(DeepColor.AsAddress);
    glVertex3f(0, 0, -1);
    SinCosine(DegToRadian(stop), vertex1.Z, r);
    thetaStart := DegToRadian(90 - stop);
    for i := 0 to steps - 1 do
    begin
      vertex1.X := r * cosTable[i];
      vertex1.Y := r * sinTable[i];
      Color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
      glColor4fv(@Color);
      glVertex4fv(@vertex1);
    end;
    glEnd;
  end;

  procedure RenderBand(start, stop: Single);
  var
    i: Integer;
    r, r2, thetaStart, thetaStop: Single;
    vertex1, vertex2: TVector4f;
    Color: TgxColorVector;
  begin
    vertex1.W := 1;
    if stop = 90 then
    begin
      // triangle fan with north pole
      glBegin(GL_TRIANGLE_FAN);
      Color := CalculateColor(0, CalculateCosGamma(ZHmgPoint));
      glColor4fv(@Color);
      glVertex4fv(@ZHmgPoint);
      SinCosine(DegToRadian(start), vertex1.Z, r);
      thetaStart := DegToRadian(90 - start);
      for i := 0 to steps - 1 do
      begin
        vertex1.X := r * cosTable[i];
        vertex1.Y := r * sinTable[i];
        Color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
        glColor4fv(@Color);
        glVertex4fv(@vertex1);
      end;
      glEnd;
    end
    else
    begin
      vertex2.W := 1;
      // triangle strip
      glBegin(GL_TRIANGLE_STRIP);
      SinCosine(DegToRadian(start), vertex1.Z, r);
      SinCosine(DegToRadian(stop), vertex2.Z, r2);
      thetaStart := DegToRadian(90 - start);
      thetaStop := DegToRadian(90 - stop);
      for i := 0 to steps - 1 do
      begin
        vertex1.X := r * cosTable[i];
        vertex1.Y := r * sinTable[i];
        Color := CalculateColor(thetaStart, CalculateCosGamma(vertex1));
        glColor4fv(@Color);
        glVertex4fv(@vertex1);
        vertex2.X := r2 * cosTable[i];
        vertex2.Y := r2 * sinTable[i];
        Color := CalculateColor(thetaStop, CalculateCosGamma(vertex2));
        glColor4fv(@Color);
        glVertex4fv(@vertex2);
      end;
      glEnd;
    end;
  end;

var
  n, i, sdiv2: Integer;
  t, t2, p, fs: Single;
begin
  ts := DegToRadian(90 - SunElevation);
  SetVector(sunPos, sin(ts), 0, cos(ts));
  // prepare sin/cos LUT, with a higher sampling around 0Ѝ
  n := Slices div 2;
  steps := 2 * n + 1;
  GetMem(sinTable, steps * SizeOf(Single));
  GetMem(cosTable, steps * SizeOf(Single));
  for i := 1 to n do
  begin
    p := (1 - sqrt(cos((i / n) * cPIdiv2))) * PI;
    SinCosine(p, sinTable[n + i], cosTable[n + i]);
    sinTable[n - i] := -sinTable[n + i];
    cosTable[n - i] := cosTable[n + i];
  end;
  // these are defined by hand for precision issue: the dome must wrap exactly
  sinTable[n] := 0;
  cosTable[n] := 1;
  sinTable[0] := 0;
  cosTable[0] := -1;
  sinTable[steps - 1] := 0;
  cosTable[steps - 1] := -1;
  fs := SunElevation / 90;
  // start render
  t := 0;
  sdiv2 := Stacks div 2;
  for n := 0 to Stacks - 1 do
  begin
    if fs > 0 then
    begin
      if n < sdiv2 then
        t2 := fs - fs * Sqr((sdiv2 - n) / sdiv2)
      else
        t2 := fs + Sqr((n - sdiv2) / (sdiv2 - 1)) * (1 - fs);
    end
    else
      t2 := (n + 1) / Stacks;
    RenderBand(Lerp(1, 90, t), Lerp(1, 90, t2));
    t := t2;
  end;
  RenderDeepBand(1);
  FreeMem(sinTable);
  FreeMem(cosTable);
end;

// -------------------------------------------------------------
initialization

// -------------------------------------------------------------

RegisterClasses([TgxSkyDome, TgxEarthSkyDome]);

end.
