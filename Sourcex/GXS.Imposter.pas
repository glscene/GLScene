//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Imposter;

(* Imposter building and rendering implementation *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Types,
  System.Classes,
  System.SysUtils,

  GXS.Scene,
  GXS.Context,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.PersistentClasses,
  GXS.Graphics,
  GXS.Color,
  GXS.RenderContextInfo,
  GXS.Coordinates,
  GXS.BaseClasses,
  GXS.State,
  GXS.PipelineTransformation,
  GXS.TextureFormat,
  GXS.Utils;

type
  (* Imposter rendering options.
     Following options are supported:
      impoBlended : the imposters are transparently blended during renders,
     this will smooth their edges but requires them to be rendered sorted
     from back to front
      impoAlphaTest : alpha test is used to eliminate transparent pixels,
     the alpha treshold is adjusted by the AlphaTreshold property
      impoNearestFiltering : use nearest texture filtering (the alternative
     is linear filtering)
      impoPerspectiveCorrection : activates a special imposter rendering
     projection suitable for distorting the sprites when seen from a level
     angle of view with a wide focal camera (think trees/grass when walking
     in a forest), if not active, the imposter sprites are camera-facing  *)
  TImposterOption = (impoBlended, impoAlphaTest, impoNearestFiltering,
    impoPerspectiveCorrection);
  TImposterOptions = set of TImposterOption;

const
  cDefaultImposterOptions = [impoBlended, impoAlphaTest];

type
  TgxImposterBuilder = class;

  (* Base class for imposters manipulation and handling.
     Rendering imposters is performed by three methods, BeginRender must
     be invoked first, then Render for each of the impostr
     This class assumes a single impostor per texture.

     Note: Remeber to enable Destination Alpha on your viewer.*)
  TImposter = class(TObject)
  private

    FRequestCount: Integer;
    FBuilder: TgxImposterBuilder;
    FTexture: TgxTextureHandle;
    FImpostoredObject: TgxBaseSceneObject;
    FAspectRatio: Single;
    FModulated: Boolean;
  protected
    FVx, FVy: TVector4f;
    FStaticOffset: TVector4f;
    FQuad: array[0..3] of TVector4f;
    FStaticScale: Single;
    procedure PrepareTexture(var rci: TgxRenderContextInfo); virtual;
    procedure RenderQuad(const texExtents, objPos: TVector4f; size: Single);
  public
    constructor Create(aBuilder: TgxImposterBuilder); virtual;
    destructor Destroy; override;
    procedure BeginRender(var rci: TgxRenderContextInfo); virtual;
    procedure Render(var rci: TgxRenderContextInfo;
      const objPos, localCameraPos: TVector4f;
      size: Single); virtual;
    procedure EndRender(var rci: TgxRenderContextInfo); virtual;
    procedure RenderOnce(var rci: TgxRenderContextInfo;
      const objPos, localCameraPos: TVector4f;
      size: Single);
    property AspectRatio: Single read FAspectRatio write FAspectRatio;
    property Builder: TgxImposterBuilder read FBuilder;
    property Texture: TgxTextureHandle read FTexture;
    property ImpostoredObject: TgxBaseSceneObject read FImpostoredObject write
      FImpostoredObject;
    property Modulated: Boolean read FModulated write FModulated;
  end;

   // Imposter loading events
   TLoadingImposterEvent = function (Sender : TObject; impostoredObject :
     TgxBaseSceneObject; destImposter : TImposter) : TgxBitmap32 of object;
   {$NODEFINE TLoadingImposterEvent}
   //Used CPPB procedure instead of Delphi function
   //TLoadingImposterEvent = procedure (Sender : TObject; impostoredObject : TgxBaseSceneObject; destImposter : TImposter; var result : TgxBitmap32) of object;
   {$HPPEMIT 'typedef Glgraphics::TgxBitmap32* __fastcall (__closure *TLoadingImposterEvent)(System::TObject* Sender, Glscene::TgxBaseSceneObject* impostoredObject, TImposter* destImposter);'}

   TImposterLoadedEvent = procedure (Sender : TObject; impostoredObject :
         TgxBaseSceneObject;
         destImposter : TImposter) of object;

  TImposterReference = (irCenter, irTop, irBottom);

  // Abstract ImposterBuilder class.
  TgxImposterBuilder = class(TgxUpdateAbleComponent)
  private
    FBackColor: TgxColor;
    FBuildOffset: TgxCoordinates;
    FImposterRegister: TgxPersistentObjectList;
    FRenderPoint: TgxRenderPoint;
    FImposterOptions: TImposterOptions;
    FAlphaTreshold: Single;
    FImposterReference: TImposterReference;
    FOnLoadingImposter: TLoadingImposterEvent;
    FOnImposterLoaded: TImposterLoadedEvent;
  protected
    procedure SetRenderPoint(AValue: TgxRenderPoint);
    procedure RenderPointFreed(Sender: TObject);
    procedure SetBackColor(AValue: TgxColor);
    procedure SetBuildOffset(AValue: TgxCoordinates);
    procedure SetImposterReference(AValue: TImposterReference);
    procedure InitializeImpostorTexture(const TextureSize: TPoint);
    property ImposterRegister: TgxPersistentObjectList read FImposterRegister;
    procedure UnregisterImposter(imposter: TImposter);
    function CreateNewImposter: TImposter; virtual;
    procedure PrepareImposters(Sender: TObject; var rci: TgxRenderContextInfo);
      virtual;
    procedure DoPrepareImposter(var rci: TgxRenderContextInfo;
      impostoredObject: TgxBaseSceneObject;
      destImposter: TImposter); virtual; abstract;
    procedure DoUserSpecifiedImposter(
      var rci: TgxRenderContextInfo;
      destImposter: TImposter;
      bmp32: TgxBitmap32); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure NotifyChange(Sender: TObject); override;
    (* Returns a valid imposter for the specified object.
       Imposter must have been requested first, and the builder given
       an opportunity to prepare it before it can be available. *)
    function ImposterFor(impostoredObject: TgxBaseSceneObject): TImposter;
    // Request an imposter to be prepared for the specified object
    procedure RequestImposterFor(impostoredObject: TgxBaseSceneObject);
    // Tells the imposter for the specified object is no longer needed
    procedure UnRequestImposterFor(impostoredObject: TgxBaseSceneObject);
  published
    (* Specifies the render point at which the impostor texture(s) can be prepared.
       For best result, the render point should happen in viewer that has
       a destination alpha (otherwise, impostors will be opaque). *)
    property RenderPoint: TgxRenderPoint read FRenderPoint write SetRenderPoint;
    (* Background color for impostor rendering.
       Typically, you'll want to leave the alpha channel to zero, and pick
       as RGB as color that matches the impostor'ed objects edge colors most.*)
    property BackColor: TgxColor read FBackColor write SetBackColor;
    (* Offset applied to the impostor'ed object during imposter construction.
       Can be used to manually tune the centering of objects. *)
    property BuildOffset: TgxCoordinates read FBuildOffset write SetBuildOffset;
    // Imposter rendering options
    property ImposterOptions: TImposterOptions read FImposterOptions write
      FImposterOptions default cDefaultImposterOptions;
    (* Determines how the imposter are handled.
       This is the reference point for imposters, impostor'ed objects that
       are centered should use irCenter, those whose bottom is the origin
       should use irBottom, etc. *)
    property ImposterReference: TImposterReference read FImposterReference write
      SetImposterReference default irCenter;
    // Alpha testing teshold.
    property AlphaTreshold: Single read FAlphaTreshold write FAlphaTreshold;
    (* Event fired before preparing/loading an imposter.
       If an already prepared version of the importer is available, place
       it in the TgxBitmap32 the event shall return (the bitmap will be
       freed by the imposter builder). If a bitmap is specified, it will
       be used in place of what automatic generation could have generated. *)
    property OnLoadingImposter: TLoadingImposterEvent read FOnLoadingImposter
      write FOnLoadingImposter;
    (* Event fired after preparing/loading an imposter.
       This events gives an opportunity to save the imposter after it has
       been loaded or prepared. *)
    property OnImposterLoaded: TImposterLoadedEvent read FOnImposterLoaded write
      FOnImposterLoaded;
  end;

  // Describes a set of orientation in a corona fashion
  TgxStaticImposterBuilderCorona = class(TCollectionItem)
  private
    FSamples: Integer;
    FElevation: Single;
    FSampleBaseIndex: Integer;
  protected
    function GetDisplayName: string; override;
    procedure SetSamples(AValue: Integer);
    procedure SetElevation(AValue: Single);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Samples: Integer read FSamples write SetSamples default 8;
    property Elevation: Single read FElevation write SetElevation;
  end;

  TCoronaTangentLookup = record
    minTan, maxTan: Single;
    corona: TgxStaticImposterBuilderCorona;
  end;

  TgxStaticImposterBuilderCoronas = class(TOwnedCollection)
  private
    FCoronaTangentLookup: array of TCoronaTangentLookup;
  protected
    procedure SetItems(AIndex: Integer; const AValue:
      TgxStaticImposterBuilderCorona);
    function GetItems(AIndex: Integer): TgxStaticImposterBuilderCorona;
    procedure Update(Item: TCollectionItem); override;
    procedure PrepareSampleBaseIndices;
    procedure PrepareCoronaTangentLookup;
    function CoronaForElevationTangent(aTangent: Single):
      TgxStaticImposterBuilderCorona;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TgxStaticImposterBuilderCorona; overload;
    function Add(const elevation: Single; samples: Integer):
      TgxStaticImposterBuilderCorona; overload;
    property Items[AIndex: Integer]: TgxStaticImposterBuilderCorona read GetItems
    write SetItems; default;
    function SampleCount: Integer;
    procedure NotifyChange; virtual;
    procedure EndUpdate; override;
  end;

  // Imposter class whose texture contains several views from different angles
  TStaticImposter = class(TImposter)
  public
    procedure Render(var rci: TgxRenderContextInfo;
      const objPos, localCameraPos: TVector4f;
      size: Single); override;
  end;

  TSIBLigthing = (siblNoLighting, siblStaticLighting, siblLocalLighting);

  // Builds imposters whose texture is a catalog of prerendered views
  TgxStaticImposterBuilder = class(TgxImposterBuilder)
  private
    FCoronas: TgxStaticImposterBuilderCoronas;
    FSampleSize: Integer;
    FTextureSize: TPoint;
    FSamplesPerAxis: TPoint;
    FInvSamplesPerAxis: TVector2f;
    FSamplingRatioBias, FInvSamplingRatioBias: Single;
    FLighting: TSIBLigthing;
    FSamplesAlphaScale: Single;
  protected
    procedure SetCoronas(AValue: TgxStaticImposterBuilderCoronas);
    procedure SetSampleSize(AValue: Integer);
    procedure SetSamplingRatioBias(AValue: Single);
    function StoreSamplingRatioBias: Boolean;
    procedure SetLighting(AValue: TSIBLigthing);
    procedure SetSamplesAlphaScale(AValue: Single);
    function StoreSamplesAlphaScale: Boolean;
    function GetTextureSizeInfo: string;
    procedure SetTextureSizeInfo(const texSize: string);
    // Computes the optimal texture size that would be able to hold all samples
    function ComputeOptimalTextureSize: TPoint;
    function CreateNewImposter: TImposter; override;
    procedure DoPrepareImposter(var rci: TgxRenderContextInfo;
      impostoredObject: TgxBaseSceneObject;
      destImposter: TImposter); override;
    procedure DoUserSpecifiedImposter(
      var rci: TgxRenderContextInfo;
      destImposter: TImposter;
      bmp32: TgxBitmap32); override;
    procedure ComputeStaticParams(destImposter: TImposter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Render imposter texture.
       Buffer and object must be compatible, RC must have been activated. *)
    procedure Render(var rci: TgxRenderContextInfo;
      impostoredObject: TgxBaseSceneObject;
      destImposter: TImposter);
    (* Ratio (0..1) of the texture that will be used by samples.
       If this value is below 1, you're wasting texture space and may
       as well increase the number of samples. *)
    function TextureFillRatio: Single;
    // Meaningful only after imposter texture has been prepared
    property TextureSize: TPoint read FTextureSize;
    property SamplesPerAxis: TPoint read FSamplesPerAxis;
  published
    // Description of the samples looking orientations
    property Coronas: TgxStaticImposterBuilderCoronas read FCoronas write
      SetCoronas;
    // Size of the imposter samples (square)
    property SampleSize: Integer read FSampleSize write SetSampleSize default  32;
    (* Size ratio applied to the impostor'ed objects during sampling.
       Values greater than one can be used to "fill" the samples more
       by scaling up the object. This is especially useful when the impostor'ed
       object doesn't fill its bounding sphere, and/or if the outer details
       are not relevant for impostoring. *)
    property SamplingRatioBias: Single read FSamplingRatioBias write
      SetSamplingRatioBias stored StoreSamplingRatioBias;
    (* Scale factor apply to the sample alpha channel.
       Main use is to saturate the samples alpha channel, and make fully
       opaque what would have been partially transparent, while leaving
       fully transparent what was fully transparent. *)
    property SamplesAlphaScale: Single read FSamplesAlphaScale write
      SetSamplesAlphaScale stored StoreSamplesAlphaScale;
    // Lighting mode to apply during samples construction
    property Lighting: TSIBLigthing read FLighting write FLighting default
      siblStaticLighting;
    (* Dummy property that returns the size of the imposter texture.
       This property is essentially here as a helper at design time,
       to give you the requirements your coronas and samplesize parameters
       imply. *)
    property TextureSizeInfo: string read GetTextureSizeInfo write
      SetTextureSizeInfo stored False;
  end;

  TgxDynamicImposterBuilder = class(TgxImposterBuilder)
  private
    FMinTexSize, FMaxTexSize: Integer;
    FMinDistance, FTolerance: Single;
    FUseMatrixError: Boolean;
  protected
    procedure SetMinDistance(const AValue: Single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (*  procedure DoRender(var rci : TgxRenderContextInfo;
                                renderSelf, renderChildren : Boolean); override; *)
  published
    property MinTexSize: Integer read FMinTexSize write FMinTexSize;
    property MaxTexSize: Integer read FMaxTexSize write FMaxTexSize;
    property MinDistance: Single read FMinDistance write SetMinDistance;
    property Tolerance: Single read FTolerance write FTolerance;
    property UseMatrixError: Boolean read FUseMatrixError write FUseMatrixError;
  end;

  TgxImposter = class(TgxImmaterialSceneObject)
  private
    FBuilder: TgxImposterBuilder;
    FImpostoredObject: TgxBaseSceneObject;
  protected
    procedure SetBuilder(const AValue: TgxImposterBuilder);
    procedure SetImpostoredObject(const AValue: TgxBaseSceneObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation);  override;
    procedure DoRender(var ARci: TgxRenderContextInfo;
      ARenderSelf, ARenderChildren: Boolean); override;
  published
    property Builder: TgxImposterBuilder read FBuilder write SetBuilder;
    property ImpostoredObject: TgxBaseSceneObject read FImpostoredObject write
      SetImpostoredObject;
  end;

//-------------------------------------------------------------
implementation
//-------------------------------------------------------------

const
  cReferenceToPos: array[Low(TImposterReference)..High(TImposterReference)] of Single = (0, -1, 1);

  // ----------
  // ---------- TImposter ----------
  // ----------

constructor TImposter.Create(aBuilder: TgxImposterBuilder);
begin
  inherited Create;
  FBuilder := aBuilder;
  FTexture := TgxTextureHandle.Create;
  aBuilder.FImposterRegister.Add(Self);
  FAspectRatio := 1;
end;

destructor TImposter.Destroy;
begin
  if Assigned(FBuilder) then
    FBuilder.UnregisterImposter(Self);
  FTexture.Free;
  inherited;
end;

procedure TImposter.PrepareTexture(var rci: TgxRenderContextInfo);
var
  i: Integer;
begin
  if FTexture.Handle <> 0 then
    Exit;

  FTexture.AllocateHandle;
  FTexture.Target := ttTexture2D;
  rci.gxStates.TextureBinding[0, ttTexture2D] := FTexture.Handle;

{
  if GL_EXT_texture_edge_clamp then     // GL_TEXTURE_BORDER
    i := GL_CLAMP_TO_EDGE
  else
    i := GL_CLAMP;
}
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
end;

procedure TImposter.BeginRender(var rci: TgxRenderContextInfo);
var
  mat: TMatrix4f;
  filter: GLEnum;
  fx, fy, yOffset, cosAlpha, dynScale: Single;
begin
  with rci.gxStates do
  begin
    Disable(stLighting);
    Disable(stCullFace);
    ActiveTextureEnabled[ttTexture2D] := True;

    if impoAlphaTest in Builder.ImposterOptions then
    begin
      Enable(stAlphaTest);
      SetAlphaFunction(cfGEqual, Builder.AlphaTreshold);
    end
    else
      Disable(stAlphaTest);

    if impoBlended in Builder.ImposterOptions then
    begin
      Enable(stBlend);
      SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);
    end
    else
      Disable(stBlend);

    TextureBinding[0, ttTexture2D] := Texture.Handle;

    if impoNearestFiltering in Builder.ImposterOptions then
      filter := GL_NEAREST
    else
      filter := GL_LINEAR;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, filter);
    if FModulated then
    begin
      glColor4fv(@XYZWHmgVector);
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    end
    else
      glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

    mat := rci.PipelineTransformation.ModelViewMatrix^;
    FVx.X := mat.X.X;
    FVx.Y := mat.Y.X;
    FVx.Z := mat.Z.X;
    NormalizeVector(FVx);

    FVy.X := mat.X.Y;
    FVy.Y := mat.Y.Y;
    FVy.Z := mat.Z.Y;
    NormalizeVector(FVy);
    if impoPerspectiveCorrection in Builder.ImposterOptions then
    begin
      cosAlpha := VectorDotProduct(FVy, YHmgVector);
      FVy := VectorLerp(FVy, YHmgVector, Abs(cosAlpha));
      NormalizeVector(FVy);
      dynScale := ClampValue(1 / cosAlpha, 1, 1.414) * FStaticScale;
    end
    else
      dynScale := FStaticScale;

    fx := Sqrt(FAspectRatio);
    fy := 1 / fx;
    yOffset := cReferenceToPos[Builder.ImposterReference] * dynScale * fy;
    fx := fx * dynScale;
    fy := fy * dynScale;

    FQuad[0] := VectorSubtract(VectorCombine(FVx, FVy, fx, fy + yOffset),
      FStaticOffset);
    FQuad[1] := VectorSubtract(VectorCombine(FVx, FVy, -fx, fy + yOffset),
      FStaticOffset);
    FQuad[2] := VectorSubtract(VectorCombine(FVx, FVy, -fx, -fy + yOffset),
      FStaticOffset);
    FQuad[3] := VectorSubtract(VectorCombine(FVx, FVy, fx, -fy + yOffset),
      FStaticOffset);

    glBegin(GL_QUADS);
  end;
end;

procedure TImposter.Render(var rci: TgxRenderContextInfo;
  const objPos, localCameraPos: TVector4f;
  size: Single);
const
  cQuadTexExtents: TVector4f = (X:0; Y:0; Z:1; W:1);
begin
  RenderQuad(cQuadTexExtents, objPos, size);
end;

procedure TImposter.RenderQuad(const texExtents, objPos: TVector4f; size: Single);
var
  pos: TVector4f;
begin
  VectorCombine(objPos, FQuad[0], size, pos);
  glTexCoord2f(texExtents.Z, texExtents.W);
  glVertex3fv(@pos);
  VectorCombine(objPos, FQuad[1], size, pos);
  glTexCoord2f(texExtents.X, texExtents.W);
  glVertex3fv(@pos);
  VectorCombine(objPos, FQuad[2], size, pos);
  glTexCoord2f(texExtents.X, texExtents.Y);
  glVertex3fv(@pos);
  VectorCombine(objPos, FQuad[3], size, pos);
  glTexCoord2f(texExtents.Z, texExtents.Y);
  glVertex3fv(@pos);
end;

procedure TImposter.EndRender(var rci: TgxRenderContextInfo);
begin
  glEnd;
  rci.gxStates.ActiveTextureEnabled[ttTexture2D] := False;
end;

procedure TImposter.RenderOnce(var rci: TgxRenderContextInfo;
  const objPos, localCameraPos: TVector4f;
  size: Single);
begin
  BeginRender(rci);
  Render(rci, objPos, localCameraPos, size);
  EndRender(rci);
end;

// ----------
// ---------- TgxImposterBuilder ----------
// ----------

constructor TgxImposterBuilder.Create(AOwner: TComponent);
begin
  inherited;
  FImposterRegister := TgxPersistentObjectList.Create;
  FBackColor := TgxColor.CreateInitialized(Self, clrTransparent);
  FBuildOffset := TgxCoordinates.CreateInitialized(Self, NullHmgPoint, CsPoint);
  FImposterOptions := cDefaultImposterOptions;
  FAlphaTreshold := 0.5;
end;

destructor TgxImposterBuilder.Destroy;
var
  i: Integer;
begin
  FBuildOffset.Free;
  FBackColor.Free;
  for i := 0 to FImposterRegister.Count - 1 do
    TImposter(FImposterRegister[i]).FBuilder := nil;
  FImposterRegister.CleanFree;
  inherited;
end;

procedure TgxImposterBuilder.Notification(AComponent: TComponent; Operation:
  TOperation);
var
  i: Integer;
  imposter: TImposter;
begin
  if Operation = opRemove then
  begin
    if AComponent = FRenderPoint then
      FRenderPoint := nil;
    for i := FImposterRegister.Count - 1 downto 0 do
    begin
      imposter := TImposter(FImposterRegister[i]);
      if imposter.ImpostoredObject = AComponent then
      begin
        imposter.Free;
        Break;
      end;
    end;
  end;
  inherited;
end;

function TgxImposterBuilder.CreateNewImposter: TImposter;
begin
  Result := TImposter.Create(Self);
end;

procedure TgxImposterBuilder.PrepareImposters(Sender: TObject; var rci:
  TgxRenderContextInfo);
var
  i: Integer;
  imp: TImposter;
  bmp32: TgxBitmap32;
begin
  for i := 0 to ImposterRegister.Count - 1 do
  begin
    imp := TImposter(ImposterRegister[i]);
    if (imp.ImpostoredObject <> nil) and (imp.Texture.Handle = 0) then
    begin
      if Assigned(FOnLoadingImposter) then
        bmp32:=FOnLoadingImposter(Self, imp.ImpostoredObject, imp)
      else
        bmp32 := nil;
		      if not Assigned(bmp32) then
       DoPrepareImposter(rci, imp.ImpostoredObject, imp)
      else
      begin
        DoUserSpecifiedImposter(rci, imp, bmp32);
        bmp32.Free;
      end;
      if Assigned(FOnImposterLoaded) then
        FOnImposterLoaded(Self, imp.ImpostoredObject, imp);
    end;
  end;
end;

procedure TgxImposterBuilder.DoUserSpecifiedImposter(
  var rci: TgxRenderContextInfo;
  destImposter: TImposter;
  bmp32: TgxBitmap32);
var
  size: Integer;
begin
  destImposter.PrepareTexture(rci);
  bmp32.RegisterAsOpenRXTexture(
    destImposter.FTexture, False, GL_RGBA8, size, size, size);
end;

procedure TgxImposterBuilder.NotifyChange(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to FImposterRegister.Count - 1 do
    TImposter(FImposterRegister[i]).Texture.DestroyHandle;
  inherited;
end;

function TgxImposterBuilder.ImposterFor(impostoredObject: TgxBaseSceneObject):
  TImposter;
var
  i: Integer;
begin
  for i := 0 to FImposterRegister.Count - 1 do
  begin
    Result := TImposter(FImposterRegister[i]);
    if Result.ImpostoredObject = impostoredObject then
      Exit;
  end;
  Result := nil;
end;

procedure TgxImposterBuilder.RequestImposterFor(impostoredObject:
  TgxBaseSceneObject);
var
  imposter: TImposter;
begin
  if impostoredObject = nil then
    Exit;
  imposter := ImposterFor(impostoredObject);
  if imposter = nil then
  begin
    imposter := CreateNewImposter;
    imposter.ImpostoredObject := impostoredObject;
  end;
  Inc(imposter.FRequestCount);
end;

procedure TgxImposterBuilder.UnRequestImposterFor(impostoredObject:
  TgxBaseSceneObject);
var
  imposter: TImposter;
begin
  if impostoredObject = nil then
    Exit;
  imposter := ImposterFor(impostoredObject);
  if imposter <> nil then
  begin
    Dec(imposter.FRequestCount);
    if imposter.FRequestCount = 0 then
      imposter.Free;
  end;
end;

procedure TgxImposterBuilder.SetRenderPoint(AValue: TgxRenderPoint);
begin
  if AValue <> FRenderPoint then
  begin
    if Assigned(FRenderPoint) then
    begin
      FRenderPoint.RemoveFreeNotification(Self);
      FRenderPoint.UnRegisterCallBack(PrepareImposters);
    end;
    FRenderPoint := AValue;
    if Assigned(FRenderPoint) then
    begin
      FRenderPoint.FreeNotification(Self);
      FRenderPoint.RegisterCallBack(PrepareImposters, RenderPointFreed);
    end;
  end;
end;

procedure TgxImposterBuilder.RenderPointFreed(Sender: TObject);
begin
  FRenderPoint := nil;
end;

procedure TgxImposterBuilder.SetBackColor(AValue: TgxColor);
begin
  FBackColor.Assign(AValue);
end;

procedure TgxImposterBuilder.SetBuildOffset(AValue: TgxCoordinates);
begin
  FBuildOffset.Assign(AValue);
end;

procedure TgxImposterBuilder.SetImposterReference(AValue: TImposterReference);
begin
  if FImposterReference <> AValue then
  begin
    FImposterReference := AValue;
    NotifyChange(Self);
  end;
end;

procedure TgxImposterBuilder.InitializeImpostorTexture(const textureSize:
  TPoint);
begin
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, textureSize.X, textureSize.Y, 0,
      GL_RGBA, GL_UNSIGNED_BYTE, nil);
end;

procedure TgxImposterBuilder.UnregisterImposter(imposter: TImposter);
begin
  if imposter.Builder = Self then
  begin
    FImposterRegister.Remove(imposter);
    imposter.FBuilder := nil;
  end;
end;

// ----------
// ---------- TgxStaticImposterBuilderCorona ----------
// ----------

constructor TgxStaticImposterBuilderCorona.Create(ACollection: TCollection);
begin
  inherited;
  FSamples := 8;
end;

destructor TgxStaticImposterBuilderCorona.Destroy;
begin
  inherited;
end;

procedure TgxStaticImposterBuilderCorona.Assign(Source: TPersistent);
begin
  if Source is TgxStaticImposterBuilderCorona then
  begin
    FSamples := TgxStaticImposterBuilderCorona(Source).FSamples;
    FElevation := TgxStaticImposterBuilderCorona(Source).FElevation;
  end;
  inherited;
end;

function TgxStaticImposterBuilderCorona.GetDisplayName: string;
begin
  Result := Format('%.1f° / %d samples', [Elevation, Samples]);
end;

procedure TgxStaticImposterBuilderCorona.SetSamples(AValue: Integer);
begin
  if AValue <> FSamples then
  begin
    FSamples := AValue;
    if FSamples < 1 then
      FSamples := 1;
    (Collection as TgxStaticImposterBuilderCoronas).NotifyChange;
  end;
end;

procedure TgxStaticImposterBuilderCorona.SetElevation(AValue: Single);
begin
  if AValue <> FElevation then
  begin
    FElevation := ClampValue(AValue, -89, 89);
    (Collection as TgxStaticImposterBuilderCoronas).NotifyChange;
  end;
end;

// ----------
// ---------- TgxStaticImposterBuilderCoronas ----------
// ----------

constructor TgxStaticImposterBuilderCoronas.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TgxStaticImposterBuilderCorona);
end;

function TgxStaticImposterBuilderCoronas.Add: TgxStaticImposterBuilderCorona;
begin
  Result := (inherited Add) as TgxStaticImposterBuilderCorona;
end;

function TgxStaticImposterBuilderCoronas.Add(const elevation: Single;
  samples: Integer): TgxStaticImposterBuilderCorona;
begin
  Result := (inherited Add) as TgxStaticImposterBuilderCorona;
  Result.Elevation := elevation;
  Result.Samples := samples;
end;

procedure TgxStaticImposterBuilderCoronas.SetItems(AIndex: Integer; const
  AValue: TgxStaticImposterBuilderCorona);
begin
  inherited Items[AIndex] := AValue;
end;

function TgxStaticImposterBuilderCoronas.GetItems(AIndex: Integer):
  TgxStaticImposterBuilderCorona;
begin
  Result := TgxStaticImposterBuilderCorona(inherited Items[AIndex]);
end;

procedure TgxStaticImposterBuilderCoronas.Update(Item: TCollectionItem);
begin
  inherited;
  NotifyChange;
end;

procedure TgxStaticImposterBuilderCoronas.NotifyChange;
begin
  if (UpdateCount = 0) and (GetOwner <> nil) and (GetOwner is
    TgxUpdateAbleComponent) then
    TgxUpdateAbleComponent(GetOwner).NotifyChange(Self);
end;

procedure TgxStaticImposterBuilderCoronas.EndUpdate;
begin
  inherited;
  NotifyChange;
end;

function TgxStaticImposterBuilderCoronas.SampleCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    Result := Result + Items[i].Samples;
end;

procedure TgxStaticImposterBuilderCoronas.PrepareSampleBaseIndices;
var
  p, i: Integer;
begin
  p := 0;
  for i := 0 to Count - 1 do
  begin
    Items[i].FSampleBaseIndex := p;
    Inc(p, Items[i].Samples);
  end;
end;

procedure TgxStaticImposterBuilderCoronas.PrepareCoronaTangentLookup;
var
  i, j: Integer;
  corona: TgxStaticImposterBuilderCorona;
  boundary: Single;
begin
  SetLength(FCoronaTangentLookup, Count);
  // place them in the array and sort by ascending elevation
  for i := 0 to Count - 1 do
    FCoronaTangentLookup[i].corona := Items[i];
  for i := 0 to Count - 2 do
    for j := i + 1 to Count - 1 do
      if FCoronaTangentLookup[j].corona.Elevation <
        FCoronaTangentLookup[i].corona.Elevation then
      begin
        corona := FCoronaTangentLookup[j].corona;
        FCoronaTangentLookup[j].corona := FCoronaTangentLookup[i].corona;
        FCoronaTangentLookup[i].corona := corona;
      end;
  // adjust min max then intermediate boundaries
  FCoronaTangentLookup[0].minTan := -1e30;
  FCoronaTangentLookup[Count - 1].minTan := 1e30;
  for i := 0 to Count - 2 do
  begin
    boundary := Tangent((0.5 * cPIdiv180) * (FCoronaTangentLookup[i].corona.Elevation
      + FCoronaTangentLookup[i + 1].corona.Elevation));
    FCoronaTangentLookup[i].maxTan := boundary;
    FCoronaTangentLookup[i + 1].minTan := boundary;
  end;
end;

function TgxStaticImposterBuilderCoronas.CoronaForElevationTangent(aTangent:
  Single): TgxStaticImposterBuilderCorona;
var
  i, n: Integer;
begin
  n := High(FCoronaTangentLookup);
  if (n = 0) or (aTangent <= FCoronaTangentLookup[0].maxTan) then
    Result := FCoronaTangentLookup[0].corona
  else if aTangent > FCoronaTangentLookup[n].minTan then
    Result := FCoronaTangentLookup[n].corona
  else
  begin
    Result := FCoronaTangentLookup[1].corona;
    for i := 2 to n - 2 do
    begin
      if aTangent <= FCoronaTangentLookup[i].minTan then
        Break;
      Result := FCoronaTangentLookup[i].corona;
    end;
  end;
end;

// ----------
// ---------- TStaticImposter ----------
// ----------

procedure TStaticImposter.Render(var rci: TgxRenderContextInfo;
  const objPos, localCameraPos: TVector4f;
  size: Single);
var
  azimuthAngle: Single;
  i: Integer;
  x, y: Word;
  bestCorona: TgxStaticImposterBuilderCorona;
  texExtents: TVector4f;
  tdx, tdy: Single;
  siBuilder: TgxStaticImposterBuilder;
begin // inherited; exit;
  siBuilder := TgxStaticImposterBuilder(Builder);

  // determine closest corona
  bestCorona := siBuilder.Coronas.CoronaForElevationTangent(
    localCameraPos.Y / VectorLength(localCameraPos.X, localCameraPos.Z));

  // determine closest sample in corona
  azimuthAngle := FastArcTangent2(localCameraPos.Z, localCameraPos.X) + cPI;
  i := Round(azimuthAngle * bestCorona.Samples * cInv2PI);
  if i < 0 then
    i := 0
  else if i >= bestCorona.Samples then
    i := bestCorona.Samples - 1;
  i := bestCorona.FSampleBaseIndex + i;

  tdx := siBuilder.FInvSamplesPerAxis.X;
  tdy := siBuilder.FInvSamplesPerAxis.Y;
  DivMod(i, siBuilder.SamplesPerAxis.X, y, x);
  texExtents.X := tdx * x;
  texExtents.Y := tdy * y;
  texExtents.Z := texExtents.X + tdx;
  texExtents.W := texExtents.Y + tdy;

  // then render it
  RenderQuad(texExtents, objPos, Size);
end;

// ----------
// ---------- TgxStaticImposterBuilder ----------
// ----------

constructor TgxStaticImposterBuilder.Create(AOwner: TComponent);
begin
  inherited;
  FCoronas := TgxStaticImposterBuilderCoronas.Create(Self);
  FCoronas.Add;
  FSampleSize := 16;
  FSamplingRatioBias := 1;
  FInvSamplingRatioBias := 1;
  FLighting := siblStaticLighting;
  FSamplesAlphaScale := 1;
end;

destructor TgxStaticImposterBuilder.Destroy;
begin
  FCoronas.Free;
  inherited;
end;

function TgxStaticImposterBuilder.CreateNewImposter: TImposter;
begin
  Result := TStaticImposter.Create(Self);
end;

procedure TgxStaticImposterBuilder.SetCoronas(AValue:
  TgxStaticImposterBuilderCoronas);
begin
  FCoronas.Assign(AValue);
  NotifyChange(Self);
end;

procedure TgxStaticImposterBuilder.SetSampleSize(AValue: Integer);
begin
  AValue := RoundUpToPowerOf2(AValue);
  if AValue < 8 then
    AValue := 8;
  if AValue > 1024 then
    AValue := 1024;
  if AValue <> FSampleSize then
  begin
    FSampleSize := AValue;
    NotifyChange(Self);
  end;
end;

procedure TgxStaticImposterBuilder.SetSamplingRatioBias(AValue: Single);
begin
  AValue := ClampValue(AValue, 0.1, 10);
  if AValue <> FSamplingRatioBias then
  begin
    FSamplingRatioBias := AValue;
    FInvSamplingRatioBias := 1 / AValue;
    NotifyChange(Self);
  end;
end;

function TgxStaticImposterBuilder.StoreSamplingRatioBias: Boolean;
begin
  Result := (FSamplingRatioBias <> 1);
end;

procedure TgxStaticImposterBuilder.SetLighting(AValue: TSIBLigthing);
begin
  if AValue <> FLighting then
  begin
    FLighting := AValue;
    NotifyChange(Self);
  end;
end;

procedure TgxStaticImposterBuilder.SetSamplesAlphaScale(AValue: Single);
begin
  if FSamplesAlphaScale <> AValue then
  begin
    FSamplesAlphaScale := AValue;
    NotifyChange(Self);
  end;
end;

function TgxStaticImposterBuilder.StoreSamplesAlphaScale: Boolean;
begin
  Result := (FSamplesAlphaScale <> 1);
end;

function TgxStaticImposterBuilder.GetTextureSizeInfo: string;
var
  t: TPoint;
  fill: Integer;
begin
  t := ComputeOptimalTextureSize;
  Result := Format('%d x %d', [t.X, t.Y]);
  fill := Coronas.SampleCount * SampleSize * SampleSize;
  if fill < t.X * t.Y then
    Result := Result + Format(' (%.1f%%)', [(100 * fill) / (t.X * t.Y)]);
end;

procedure TgxStaticImposterBuilder.SetTextureSizeInfo(const texSize: string);
begin
  // do nothing, this is a dummy property!
end;

procedure TgxStaticImposterBuilder.DoPrepareImposter(var rci:
  TgxRenderContextInfo;
  impostoredObject: TgxBaseSceneObject; destImposter: TImposter);
begin
  Render(rci, impostoredObject, destImposter);
end;

procedure TgxStaticImposterBuilder.DoUserSpecifiedImposter(
  var rci: TgxRenderContextInfo;
  destImposter:
  TImposter;
  bmp32: TgxBitmap32);
begin
  inherited;
  FTextureSize.X := bmp32.Width;
  FTextureSize.Y := bmp32.Height;
  ComputeStaticParams(destImposter);
end;

procedure TgxStaticImposterBuilder.ComputeStaticParams(destImposter: TImposter);
var
  radius: Single;
begin
  Coronas.PrepareCoronaTangentLookup;
  Coronas.PrepareSampleBaseIndices;

  FSamplesPerAxis.X := FTextureSize.X div SampleSize;
  FSamplesPerAxis.Y := FTextureSize.Y div SampleSize;
  FInvSamplesPerAxis.X := 1 / FSamplesPerAxis.X;
  FInvSamplesPerAxis.Y := 1 / FSamplesPerAxis.Y;
  Assert(FSamplesPerAxis.X * FSamplesPerAxis.Y >= Coronas.SampleCount,
    'User specified bitmap and imposter parameters don''t match');

  radius := destImposter.ImpostoredObject.BoundingSphereRadius /
    SamplingRatioBias;

  if ImposterReference = irCenter then
    destImposter.FStaticScale := radius
  else
    destImposter.FStaticScale := radius * 0.5;
  destImposter.FStaticOffset := FBuildOffset.DirectVector;
end;

procedure TgxStaticImposterBuilder.Render(var rci: TgxRenderContextInfo;
  impostoredObject: TgxBaseSceneObject; destImposter: TImposter);
var
  i, coronaIdx, curSample: Integer;
  radius: Single;
  cameraDirection, cameraOffset: TVector4f;
  xDest, xSrc, yDest, ySrc: Integer;
  corona: TgxStaticImposterBuilderCorona;
  fx, fy, yOffset: Single;
  LM: TMatrix4f;
begin
  FTextureSize := ComputeOptimalTextureSize;
  if (FTextureSize.X <= 0) and (FTextureSize.Y <= 0) then
  begin
    SampleSize := SampleSize shr 1;
    Assert(False,
      'Too many samples, can''t fit in a texture! Reduce SampleSize.');
  end;

  ComputeStaticParams(destImposter);

  radius := impostoredObject.BoundingSphereRadius / SamplingRatioBias;
  if ImposterReference <> irCenter then
    radius := radius * 0.5;

  Assert((rci.gxStates.ViewPort.Z >= SampleSize) and (rci.gxStates.ViewPort.W >= SampleSize),
    'ViewPort too small to render imposter samples!');

  // Setup the buffer in a suitable fashion for our needs
  with FBackColor do
    rci.gxStates.ColorClearValue := Color;
  if Lighting = siblNoLighting then
    rci.gxStates.Disable(stLighting);

  rci.PipelineTransformation.Push;
  fx := radius * rci.gxStates.ViewPort.Z / SampleSize;
  fy := radius * rci.gxStates.ViewPort.W / SampleSize;
  yOffset := cReferenceToPos[ImposterReference] * radius;
  rci.PipelineTransformation.SetProjectionMatrix(
    CreateOrthoMatrix(-fx, fx, yOffset - fy, yOffset + fy, radius * 0.5, radius * 5));
  xSrc := (rci.gxStates.ViewPort.Z - SampleSize) div 2;
  ySrc := (rci.gxStates.ViewPort.W - SampleSize) div 2;

  // setup imposter texture
  if destImposter.Texture.Handle = 0 then
  begin
    {$IFDEF USE_OPENGL_DEBUG}
      if GL_GREMEDY_string_marker then
        glStringMarkerGREMEDY(22, 'Imposter texture setup');
    {$ENDIF}
    destImposter.PrepareTexture(rci);
    InitializeImpostorTexture(FTextureSize);
  end;

  glPixelTransferf(GL_ALPHA_SCALE, FSamplesAlphaScale);

  // Now render each sample
  curSample := 0;
  for coronaIdx := 0 to Coronas.Count - 1 do
  begin
    corona := Coronas[coronaIdx];
    cameraDirection := XHmgVector;
    RotateVector(cameraDirection, ZHmgPoint, corona.Elevation * cPIdiv180);
    for i := 0 to corona.Samples - 1 do
    begin
      cameraOffset := cameraDirection;
      RotateVector(cameraOffset, YHmgVector, (c2PI * i) / corona.Samples);
      ScaleVector(cameraOffset, -radius * 2);
      rci.gxStates.DepthWriteMask := True;
      glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);

      LM := CreateLookAtMatrix(cameraOffset, NullHmgVector, YHmgVector);
      if Lighting = siblStaticLighting then
        (rci.scene as TgxScene).SetupLights(rci.gxStates.MaxLights);
      rci.PipelineTransformation.SetViewMatrix(MatrixMultiply(
        CreateTranslationMatrix(FBuildOffset.AsVector), LM));
      impostoredObject.Render(rci);
      //CheckOpenGLError;

      xDest := (curSample mod FSamplesPerAxis.X) * SampleSize;
      yDest := (curSample div FSamplesPerAxis.X) * SampleSize;

      rci.gxStates.TextureBinding[0, ttTexture2D] :=
        destImposter.Texture.Handle;
      glCopyTexSubImage2D(GL_TEXTURE_2D, 0, xDest, yDest, xSrc, ySrc,
        SampleSize, SampleSize);

      Inc(curSample);
    end;
  end;

  // Restore buffer stuff
  glPixelTransferf(GL_ALPHA_SCALE, 1);
  rci.PipelineTransformation.Pop;

  glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
  if Lighting = siblStaticLighting then
    (rci.scene as TgxScene).SetupLights(rci.gxStates.MaxLights);
end;

function TgxStaticImposterBuilder.ComputeOptimalTextureSize: TPoint;
var
  nbSamples, maxSamples, maxTexSize, baseSize: Integer;
  texDim, bestTexDim: TPoint;
  requiredSurface, currentSurface, bestSurface: Integer;
begin
  nbSamples := Coronas.SampleCount;
  if CurrentContext = nil then
    maxTexSize := 16 * 1024
  else
    glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxTexSize);
  maxSamples := Sqr(maxTexSize div SampleSize);
  if nbSamples < maxSamples then
  begin
    Result.X := -1;
    Result.Y := -1;
  end;
  requiredSurface := nbSamples * SampleSize * SampleSize;
  baseSize := RoundUpToPowerOf2(SampleSize);

  // determine the texture size with the best fill ratio
  bestSurface := MaxInt;
  texDim.X := baseSize;
  while texDim.X <= maxTexSize do
  begin
    texDim.Y := baseSize;
    while texDim.Y <= maxTexSize do
    begin
      currentSurface := texDim.X * texDim.Y;
      if currentSurface >= requiredSurface then
      begin
        if currentSurface < bestSurface then
        begin
          bestTexDim := texDim;
          bestSurface := currentSurface;
        end
        else if (currentSurface = bestSurface)
          and (MaxInteger(texDim.X, texDim.Y) < MaxInteger(bestTexDim.X,
          bestTexDim.Y)) then
        begin
          bestTexDim := texDim;
          bestSurface := currentSurface;
        end
        else
          Break;
      end;
      texDim.Y := texDim.Y * 2;
    end;
    texDim.X := texDim.X * 2;
  end;
  Assert(bestSurface <> MaxInt);

  Result := bestTexDim;
end;

function TgxStaticImposterBuilder.TextureFillRatio: Single;
var
  texDim: TPoint;
begin
  texDim := ComputeOptimalTextureSize;
  Result := (Coronas.SampleCount * SampleSize * SampleSize) / (texDim.X *
    texDim.Y);
end;

// ----------
// ---------- TgxDynamicImposterBuilder ----------
// ----------

constructor TgxDynamicImposterBuilder.Create(AOwner: TComponent);
begin
  inherited;
  FTolerance := 0.1;
  FUseMatrixError := True;
  FMinTexSize := 16;
  FMaxTexSize := 64;
end;

destructor TgxDynamicImposterBuilder.Destroy;
begin
  inherited;
end;

{
procedure TgxDynamicImposterBuilder.DoRender(var rci : TgxRenderContextInfo;
  renderSelf, renderChildren : Boolean);
var
  i, size, Left, Top, Width, Height : Integer;
  imposter : TgxImposter;
  mat, projection, modelview : TMatrix4f;
  BackColor, pos, temp : TVector4f;
  rad : Single;
  AABB : TAABB;
begin
  if (csDesigning in ComponentState) or not FEnabled then exit;

  // Store the current clear color
  glGetFloatv(GL_COLOR_CLEAR_VALUE, @BackColor[0]);

  // Get the projection matrix
  if UseMatrixError then
    glGetFloatv(GL_PROJECTION_MATRIX, @projection);

  // Render and save each imposter as required
  for i:=0 to FImposterRegister.Count-1 do begin
    imposter:=TgxImposter(FImposterRegister[i]);
    if (imposter.Count = 0) or not imposter.Visible then Continue;
    imposter.FDrawImposter:=True;

    if VectorDistance(imposter.AbsolutePosition, rci.cameraPosition)<FMinDistance then begin
      imposter.FDrawImposter:=False;
      Continue;
    end;

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glMultMatrixf(@imposter.AbsoluteMatrixAsAddress[0]);
    glGetFloatv(GL_MODELVIEW_MATRIX, @modelview);

    // Get imposters dimensions
    AABB:=imposter.AxisAlignedBoundingBox;
    rad:=MaxFloat(AABB.max[0],AABB.max[1],AABB.max[2]);
    pos:=imposter.AbsolutePosition;
    temp:=Scene.CurrentBuffer.Camera.AbsoluteEyeSpaceVector(0,1,0);
    temp:=VectorAdd(pos, VectorScale(temp,rad));
    pos:=Scene.CurrentBuffer.WorldToScreen(pos);
    temp:=Scene.CurrentBuffer.WorldToScreen(temp);
    size:=RoundUpToPowerOf2(Round(2*VectorDistance(pos,temp)));
    if size<FMinTexSize then size:=FMinTexSize;
    if size>FMaxTexSize then begin
      imposter.FDrawImposter:=False;
      glPopMatrix;
      Continue;
    end;
    temp:=pos;
    temp[0]:=temp[0]+size;
    temp:=Scene.CurrentBuffer.ScreenToWorld(temp);
    Imposter.FSize:=VectorDistance(imposter.AbsolutePosition,temp);
    imposter.FTexSize:=size;
    pos[0]:=pos[0]-size/2;
    pos[1]:=pos[1]-size/2;

    // Calculate error
    if UseMatrixError then begin
      mat:=MatrixMultiply(modelview, projection);
      if (imposter.CalcError(mat)>FTolerance) or (imposter.FInvalidated) then
        imposter.FOldMatrix:=mat
      else begin
        glPopMatrix;
        Continue;
      end;
    end;

    // Clear to transparent black
    glClearColor(0,0,0,0);

    // Determine size by color (for debug purposes)
    (*case size of
      16 : glClearColor(0,0,1,0.1);
      32 : glClearColor(0,1,0,0.1);
      64 : glClearColor(1,0,0,0.1);
      128 : glClearColor(1,1,0,0.1);
      256 : glClearColor(1,0,1,0.1);
    end;// *)

    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

    // Render the imposter's children
    imposter.RenderChildren(0, imposter.Count-1, rci);
    glPopMatrix;

    // Select the imposters texture (will create the handle if null)
    glBindTexture(GL_TEXTURE_2D,imposter.TextureHandle);

    // Check for resize or invalidation
    if (imposter.FTexSize <> imposter.FLastTexSize)
    or (imposter.FInvalidated) then begin
      glTexImage2d(GL_TEXTURE_2D, 0, GL_RGBA, size, size, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
      imposter.FLastTexSize:=imposter.FTexSize;
      imposter.FInvalidated:=False;
      imposter.NotifyChange(self);
    end;

    // Get the region to be copied from the frame buffer
    Left:=Floor(pos[0]); Top:=Floor(pos[1]);
    Width:=Size; Height:=Size;
    // ... Perhaps some region clamping here?

    // Copy the frame buffer pixels to the imposter texture
    glCopyTexSubImage2d(GL_TEXTURE_2D, 0, 0, 0,
                        Left, Top, Width, Height);
  end;

  // Reset the clear color and clear color, depth and stencil buffers
  glClearColor(BackColor[0],BackColor[1],BackColor[2],BackColor[3]);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
end;
}

procedure TgxDynamicImposterBuilder.SetMinDistance(const AValue: Single);
begin
  if AValue <> FMinDistance then
  begin
    FMinDistance := AValue;
    NotifyChange(Self);
  end;
end;

// ----------
// ---------- TgxImposter ----------
// ----------

constructor TgxImposter.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := ObjectStyle + [osDirectDraw];
end;

destructor TgxImposter.Destroy;
begin
  Builder := nil;
  ImpostoredObject := nil;
  inherited;
end;

procedure TgxImposter.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = Builder then
      Builder := nil;
    if AComponent = ImpostoredObject then
      ImpostoredObject := nil;
  end;
  inherited;
end;

procedure TgxImposter.DoRender(var ARci: TgxRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
var
  camPos: TVector4f;
  imposter: TImposter;
begin
  if ARenderSelf and Assigned(Builder) and Assigned(ImpostoredObject) then
  begin
    imposter := Builder.ImposterFor(ImpostoredObject);
    if Assigned(imposter) and (imposter.Texture.Handle <> 0) then
    begin
      camPos := AbsoluteToLocal(ARci.cameraPosition);
      imposter.BeginRender(ARci);
      imposter.Render(ARci, NullHmgPoint, camPos, Scale.MaxXYZ);
      imposter.EndRender(ARci);
    end;
  end;
  if ARenderChildren then
    Self.RenderChildren(0, Count - 1, ARci);
end;

procedure TgxImposter.SetBuilder(const AValue: TgxImposterBuilder);
begin
  if AValue <> FBuilder then
  begin
    if Assigned(FBuilder) then
    begin
      FBuilder.RemoveFreeNotification(Self);
      FBuilder.UnRequestImposterFor(ImpostoredObject);
    end;
    FBuilder := AValue;
    if Assigned(FBuilder) then
    begin
      FBuilder.FreeNotification(Self);
      FBuilder.RequestImposterFor(ImpostoredObject);
    end;
  end;
end;

procedure TgxImposter.SetImpostoredObject(const AValue: TgxBaseSceneObject);
begin
  if AValue <> FImpostoredObject then
  begin
    if Assigned(Builder) then
      FBuilder.UnRequestImposterFor(ImpostoredObject);
    FImpostoredObject := AValue;
    if Assigned(Builder) then
      FBuilder.RequestImposterFor(ImpostoredObject);
  end;
end;

{
function TgxImposter.AxisAlignedDimensionsUnscaled : TVector4f;
begin
   Result:=NullHMGVector;
end;

function TgxImposter.CalcError(NewMatrix : TMatrix4f) : Single;
var
   i : Integer;
   mat : TMatrix4f;
   err : Single;
begin
   err:=0;
   mat:=NewMatrix;
   InvertMatrix(mat);
   mat:=MatrixMultiply(FOldMatrix, mat);
   for i:=0 to 3 do mat[i][i]:=mat[i][i]-1;
   for i:=0 to 15 do err:=err+Abs(mat[i div 4][i mod 4]);
   Result:=err;
end;

function TgxImposter.GetTextureHandle: Cardinal;
begin
  if FTextureHandle = 0 then
    glGenTextures(1, @FTextureHandle);
  Result:=FTextureHandle;
end;

procedure TgxImposter.Invalidate;
begin
  FInvalidated:=True;
end;
}
initialization

  //  RegisterClasses([TgxDynamicImposterBuilder, TgxImposter]);
  RegisterClasses([TgxImposter]);

end.


