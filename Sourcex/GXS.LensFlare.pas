//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.LensFlare;

(* Lens flare object *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.Classes,
  System.SysUtils,
  System.Math,

  GXS.PersistentClasses,
  GXS.BaseClasses,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.Scene,
  GXS.Objects,
  GXS.PipelineTransformation,
  GXS.Context,
  GXS.Color,
  GXS.RenderContextInfo,
  GXS.State,
  GXS.Utils,
  GXS.TextureFormat;

type

  TFlareElement = (feGlow, feRing, feStreaks, feRays, feSecondaries);
  TFlareElements = set of TFlareElement;

  { The actual gradients between two colors are, of course, calculated by OpenGL.
    The start and end colors of a gradient are stored to represent the color of
    lens flare elements. }
  TgxFlareGradient = class(TgxUpdateAbleObject)
  private
    FFromColor: TgxColor;
    FToColor: TgxColor;
  protected
    procedure SetFromColor(const val: TgxColor);
    procedure SetToColor(const val: TgxColor);
  public
    constructor Create(AOwner: TPersistent); override;
    constructor CreateInitialized(AOwner: TPersistent; const fromColor, toColor: TgxColorVector);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property fromColor: TgxColor read FFromColor write SetFromColor;
    property toColor: TgxColor read FToColor write SetToColor;
  end;

const
  cDefaultFlareElements = [feGlow, feRing, feStreaks, feRays, feSecondaries];

type

  TgxLensFlare = class(TgxBaseSceneObject)
  private
    FSize: Integer;
    FDeltaTime: Single;
    FCurrSize: Single;
    FSeed: Integer;
    FSqueeze: Single;
    FNumStreaks: Integer;
    FStreakWidth, FStreakAngle: Single;
    FNumSecs: Integer;
    FResolution: Integer;
    FAutoZTest: Boolean;
    FElements: TFlareElements;
    FSin20Res, FCos20Res: array of Single;
    FSinRes, FCosRes: array of Single;
    FTexRays: TgxTextureHandle;
    FFlareIsNotOccluded: Boolean;
    FOcclusionQuery: TgxOcclusionQueryHandle;
    FGlowGradient: TgxFlareGradient;
    FRingGradient: TgxFlareGradient;
    FStreaksGradient: TgxFlareGradient;
    FRaysGradient: TgxFlareGradient;
    FSecondariesGradient: TgxFlareGradient;
    FDynamic: Boolean;
    FPreRenderPoint: TgxRenderPoint;
  protected
    procedure SetGlowGradient(const val: TgxFlareGradient);
    procedure SetRingGradient(const val: TgxFlareGradient);
    procedure SetStreaksGradient(const val: TgxFlareGradient);
    procedure SetRaysGradient(const val: TgxFlareGradient);
    procedure SetSecondariesGradient(const val: TgxFlareGradient);
    procedure SetSize(aValue: Integer);
    procedure SetSeed(aValue: Integer);
    procedure SetSqueeze(aValue: Single);
    function StoreSqueeze: Boolean;
    procedure SetNumStreaks(aValue: Integer);
    procedure SetStreakWidth(aValue: Single);
    function StoreStreakWidth: Boolean;
    procedure SetStreakAngle(aValue: Single);
    procedure SetNumSecs(aValue: Integer);
    procedure SetResolution(aValue: Integer);
    procedure SetAutoZTest(aValue: Boolean);
    procedure SetElements(aValue: TFlareElements);
    procedure SetDynamic(aValue: Boolean);
    procedure SetPreRenderPoint(const val: TgxRenderPoint);
    procedure PreRenderEvent(Sender: TObject; var rci: TgxRenderContextInfo);
    procedure PreRenderPointFreed(Sender: TObject);
    // These are quite unusual in that they don't use an RCI, since
    // PreRender is done before proper rendering starts, but we do know
    // which RC is being used, so we can use this state cache
    procedure SetupRenderingOptions(StateCache: TgxStateCache);
    procedure RenderRays(StateCache: TgxStateCache; const size: Single);
    procedure RenderStreaks(StateCache: TgxStateCache);
    procedure RenderRing;
    procedure RenderSecondaries(const posVector: TAffineVector);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BuildList(var rci: TgxRenderContextInfo); override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    { Prepares pre-rendered texture to speed up actual rendering.
      Will use the currently active context as scratch space, and will
      automatically do nothing if things have already been prepared,
      thus you can invoke it systematically in a Viewer.BeforeRender
      event f.i. }
    procedure PreRender(activeBuffer: TgxSceneBuffer);
    { Access to the Flare's current size.
      Flares decay or grow back over several frames, depending on their
      occlusion status, and this property allows to track or manually
      alter this instantaneous size. }
    property FlareInstantaneousSize: Single read FCurrSize write FCurrSize;
  published
    property GlowGradient: TgxFlareGradient read FGlowGradient write SetGlowGradient;
    property RingGradient: TgxFlareGradient read FRingGradient;
    property StreaksGradient: TgxFlareGradient read FStreaksGradient;
    property RaysGradient: TgxFlareGradient read FRaysGradient;
    property SecondariesGradient: TgxFlareGradient read FSecondariesGradient;
    // MaxRadius of the flare.
    property size: Integer read FSize write SetSize default 50;
    // Random seed
    property Seed: Integer read FSeed write SetSeed;
    // To create elliptic flares.
    property Squeeze: Single read FSqueeze write SetSqueeze stored StoreSqueeze;
    // Number of streaks.
    property NumStreaks: Integer read FNumStreaks write SetNumStreaks default 4;
    // Width of the streaks.
    property StreakWidth: Single read FStreakWidth write SetStreakWidth stored StoreStreakWidth;
    // Angle of the streaks (in degrees)
    property StreakAngle: Single read FStreakAngle write SetStreakAngle;
    // Number of secondary flares.
    property NumSecs: Integer read FNumSecs write SetNumSecs default 8;
    // Number of segments used when rendering circles.
    property Resolution: Integer read FResolution write SetResolution default 64;
    { Automatically computes FlareIsNotOccluded depending on ZBuffer test.
      Not that the automated test may use test result from the previous
      frame into the next (to avoid a rendering stall). }
    property AutoZTest: Boolean read FAutoZTest write SetAutoZTest default True;
    { Is the LensFlare not occluded?.
      If false the flare will fade away, if true, it will fade in and stay.
      This value is automatically updated if AutoZTest is set. }
    property FlareIsNotOccluded: Boolean read FFlareIsNotOccluded write FFlareIsNotOccluded;
    // Which elements should be rendered?
    property Elements: TFlareElements read FElements write SetElements default cDefaultFlareElements;
    { Is the flare size adjusted dynamically?
      If true, the flare size will be grown and reduced over a few frames
      when it switches between occluded and non-occluded states. This
      requires animation to be active, but results in a smoother appearance.
      When false, flare will either be at full size or hidden.
      The flare is always considered non-dynamic at design-time. }
    property Dynamic: Boolean read FDynamic write FDynamic default True;

    { PreRender point for pre-rendered flare textures.
      See PreRender method for more details. }
    property PreRenderPoint: TgxRenderPoint read FPreRenderPoint write SetPreRenderPoint;
    property ObjectsSorting;
    property Position;
    property Visible;
    property OnProgress;
    property Behaviours;
    property Effects;
  end;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

// ------------------
// ------------------ TgxFlareGradient ------------------
// ------------------

constructor TgxFlareGradient.Create(AOwner: TPersistent);
begin
  inherited;
  FFromColor := TgxColor.Create(Self);
  FToColor := TgxColor.Create(Self);
end;

constructor TgxFlareGradient.CreateInitialized(AOwner: TPersistent; const fromColor, toColor: TgxColorVector);
begin
  Create(AOwner);
  FFromColor.Initialize(fromColor);
  FToColor.Initialize(toColor);
end;

destructor TgxFlareGradient.Destroy;
begin
  FToColor.Free;
  FFromColor.Free;
  inherited;
end;

procedure TgxFlareGradient.Assign(Source: TPersistent);
begin
  if Source is TgxFlareGradient then
  begin
    fromColor := TgxFlareGradient(Source).fromColor;
    toColor := TgxFlareGradient(Source).toColor;
  end;
  inherited;
end;

procedure TgxFlareGradient.SetFromColor(const val: TgxColor);
begin
  FFromColor.Assign(val);
end;

procedure TgxFlareGradient.SetToColor(const val: TgxColor);
begin
  FToColor.Assign(val);
end;

// ------------------
// ------------------ TgxLensFlare ------------------
// ------------------

constructor TgxLensFlare.Create(AOwner: TComponent);
begin
  inherited;
  // Set default parameters:
  ObjectStyle := ObjectStyle + [osDirectDraw, osNoVisibilityCulling];
  FSize := 50;
  FSeed := 1465;
  FSqueeze := 1;
  FNumStreaks := 4;
  FStreakWidth := 2;
  FNumSecs := 8;
  FAutoZTest := True;
  FlareIsNotOccluded := True;
  FDynamic := True;
  SetResolution(64);
  // Render all elements by default.
  FElements := [feGlow, feRing, feStreaks, feRays, feSecondaries];
  // Setup default gradients:
  FGlowGradient := TgxFlareGradient.CreateInitialized(Self, VectorMake(1, 1, 0.8, 0.3), VectorMake(1, 0.2, 0, 0));
  FRingGradient := TgxFlareGradient.CreateInitialized(Self, VectorMake(0.5, 0.2, 0, 0.1), VectorMake(0.5, 0.4, 0, 0.1));
  FStreaksGradient := TgxFlareGradient.CreateInitialized(Self, VectorMake(1, 1, 1, 0.2), VectorMake(0.2, 0, 1, 0));
  FRaysGradient := TgxFlareGradient.CreateInitialized(Self, VectorMake(1, 0.8, 0.5, 0.05), VectorMake(0.5, 0.2, 0, 0));
  FSecondariesGradient := TgxFlareGradient.CreateInitialized(Self, VectorMake(0, 0.2, 1, 0), VectorMake(0, 0.8, 0.2, 0.15));

  FTexRays := TgxTextureHandle.Create;
end;

destructor TgxLensFlare.Destroy;
begin
  PreRenderPoint := nil;
  FGlowGradient.Free;
  FRingGradient.Free;
  FStreaksGradient.Free;
  FRaysGradient.Free;
  FSecondariesGradient.Free;
  FOcclusionQuery.Free;
  FTexRays.Free;
  inherited;
end;

procedure TgxLensFlare.Notification(AComponent: TComponent; Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FPreRenderPoint) then
    PreRenderPoint := nil;
  inherited;
end;

procedure TgxLensFlare.SetupRenderingOptions(StateCache: TgxStateCache);
begin
  with StateCache do
  begin
    Disable(stLighting);
    Disable(stDepthTest);
    Disable(stFog);
    Disable(stColorMaterial);
    Disable(stCullFace);
    DepthWriteMask := False;
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOne);
    Disable(stAlphaTest);
    PolygonMode := pmFill;
  end;
end;

procedure TgxLensFlare.RenderRays(StateCache: TgxStateCache; const size: Single);
var
  i: Integer;
  rnd: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL_GREMEDY_string_marker then
    glStringMarkerGREMEDY(14, 'LensFlare.Rays');
{$ENDIF}
  with StateCache do
  begin
    LineWidth := 1;
    Disable(stLineSmooth);
    Disable(stLineStipple);
  end;

  glBegin(GL_LINES);
  for i := 0 to Resolution * 20 - 1 do
  begin
    if (i and 1) <> 0 then
      rnd := 1.5 * Random * size
    else
      rnd := Random * size;
    glColor4fv(RaysGradient.fromColor.AsAddress);
    glVertex2f(0, 0);
    glColor4fv(RaysGradient.toColor.AsAddress);
    glVertex2f(rnd * FCos20Res[i], rnd * FSin20Res[i] * Squeeze);
  end;
  glEnd;
end;

procedure TgxLensFlare.RenderStreaks(StateCache: TgxStateCache);
var
  i: Integer;
  a, f, s, c: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL_GREMEDY_string_marker then
    glStringMarkerGREMEDY(17, 'LensFlare.Streaks');
{$ENDIF}
  StateCache.Enable(stLineSmooth);
  StateCache.LineWidth := StreakWidth;
  a := c2PI / NumStreaks;
  f := 1.5 * FCurrSize;
  glBegin(GL_LINES);
  for i := 0 to NumStreaks - 1 do
  begin
    SinCosine(StreakAngle * cPIdiv180 + a * i, f, s, c);
    glColor4fv(StreaksGradient.fromColor.AsAddress);
    glVertex3fv(@NullVector);
    glColor4fv(StreaksGradient.toColor.AsAddress);
    glVertex2f(c, Squeeze * s);
  end;
  glEnd;
  StateCache.Disable(stLineSmooth);
end;

procedure TgxLensFlare.RenderRing;
var
  i: Integer;
  rW, s0, c0, s, c: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL_GREMEDY_string_marker then
    glStringMarkerGREMEDY(14, 'LensFlare.Ring');
{$ENDIF}
  rW := FCurrSize * (1 / 15); // Ring width
  glBegin(GL_QUADS);
  s0 := 0;
  c0 := 0.6;
  for i := 0 to Resolution - 1 do
  begin
    s := s0;
    c := c0;
    s0 := FSinRes[i] * 0.6 * Squeeze;
    c0 := FCosRes[i] * 0.6;

    glColor4fv(GlowGradient.toColor.AsAddress);
    glVertex2f((FCurrSize - rW) * c, (FCurrSize - rW) * s);
    glColor4fv(RingGradient.fromColor.AsAddress);
    glVertex2f(FCurrSize * c, Squeeze * FCurrSize * s);

    glVertex2f(FCurrSize * c0, FCurrSize * s0);
    glColor4fv(GlowGradient.toColor.AsAddress);
    glVertex2f((FCurrSize - rW) * c0, (FCurrSize - rW) * s0);

    glColor4fv(RingGradient.fromColor.AsAddress);
    glVertex2f(FCurrSize * c, FCurrSize * s);
    glVertex2f(FCurrSize * c0, FCurrSize * s0);

    glColor4fv(GlowGradient.toColor.AsAddress);
    glVertex2f((FCurrSize + rW) * c0, (FCurrSize + rW) * s0);
    glVertex2f((FCurrSize + rW) * c, (FCurrSize + rW) * s);
  end;
  glEnd;
end;

procedure TgxLensFlare.RenderSecondaries(const posVector: TAffineVector);
var
  i, j: Integer;
  rnd: Single;
  v: TAffineVector;
  grad: TgxFlareGradient;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL_GREMEDY_string_marker then
    glStringMarkerGREMEDY(21, 'LensFlare.Secondaries');
{$ENDIF}
  // Other secondaries (plain gradiented circles, like the glow):
  for j := 1 to NumSecs do
  begin
    rnd := 2 * Random - 1;
    // If rnd < 0 then the secondary glow will end up on the other side
    // of the origin. In this case, we can push it really far away from
    // the flare. If  the secondary is on the flare's side, we pull it
    // slightly towards the origin to avoid it winding up in the middle
    // of the flare.
    if rnd < 0 then
      v := VectorScale(posVector, rnd)
    else
      v := VectorScale(posVector, 0.8 * rnd);
    if j mod 3 = 0 then
      grad := GlowGradient
    else
      grad := SecondariesGradient;
    rnd := (Random + 0.1) * FCurrSize * 0.25;

    glBegin(GL_TRIANGLE_FAN);
    glColor4fv(grad.fromColor.AsAddress);
    glVertex2f(v.X, v.Y);
    glColor4fv(grad.toColor.AsAddress);
    for i := 0 to Resolution - 1 do
      glVertex2f(FCosRes[i] * rnd + v.X, FSinRes[i] * rnd + v.Y);
    glEnd;
  end;
end;

procedure TgxLensFlare.BuildList(var rci: TgxRenderContextInfo);
var
  i: Integer;
  depth, dist: Single;
  posVector, v, rv: TAffineVector;
  screenPos: TAffineVector;
  flareInViewPort, dynamicSize: Boolean;
  oldSeed: LongInt;
  projMatrix: TMatrix4f;
  CurrentBuffer: TgxSceneBuffer;
begin
  if (rci.drawState = dsPicking) then
  begin
    if Count <> 0 then
      Self.RenderChildren(0, Count - 1, rci);
    Exit;
  end;
  CurrentBuffer := TgxSceneBuffer(rci.buffer);

  SetVector(v, AbsolutePosition);
  // are we looking towards the flare?
  rv := VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
  if VectorDotProduct(rci.cameraDirection, rv) > 0 then
  begin
    // find out where it is on the screen.
    screenPos := CurrentBuffer.WorldToScreen(v);
    flareInViewPort := (screenPos.X < rci.viewPortSize.cx) and (screenPos.X >= 0) and (screenPos.Y < rci.viewPortSize.cy) and
      (screenPos.Y >= 0);
  end
  else
    flareInViewPort := False;

  dynamicSize := FDynamic and not(csDesigning in ComponentState);
  if dynamicSize then
  begin
    // make the glow appear/disappear progressively
    if flareInViewPort and FlareIsNotOccluded then
    begin
      FCurrSize := FCurrSize + FDeltaTime * 10 * size;
      if FCurrSize > size then
        FCurrSize := size;
    end
    else
    begin
      FCurrSize := FCurrSize - FDeltaTime * 10 * size;
      if FCurrSize < 0 then
        FCurrSize := 0;
    end;
  end
  else
  begin
    if flareInViewPort and FlareIsNotOccluded then
      FCurrSize := size
    else
      FCurrSize := 0;
  end;

  // Prepare matrices
  glPushMatrix;
  glLoadMatrixf(@CurrentBuffer.BaseProjectionMatrix);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  projMatrix := IdentityHmgMatrix;
  projMatrix.X.X := 2 / rci.viewPortSize.cx;
  projMatrix.Y.Y := 2 / rci.viewPortSize.cy;
  glLoadMatrixf(@projMatrix);

  MakeVector(posVector, screenPos.X - rci.viewPortSize.cx * 0.5, screenPos.Y - rci.viewPortSize.cy * 0.5, 0);

  if AutoZTest then
  begin
    if dynamicSize and (TgxOcclusionQueryHandle.IsSupported = True) then  //GL_OCCLUSION_TEST_HP
    begin
      // hardware-based occlusion test is possible
      FlareIsNotOccluded := True;

      rci.gxStates.SetColorMask([]);
      rci.gxStates.Disable(stAlphaTest);
      rci.gxStates.DepthWriteMask := False;
      rci.gxStates.Enable(stDepthTest);
      rci.gxStates.DepthFunc := cfLEqual;

      if TgxOcclusionQueryHandle.IsSupported > False then
      begin
        // preferred method, doesn't stall rendering too badly
        if not Assigned(FOcclusionQuery) then
          FOcclusionQuery := TgxOcclusionQueryHandle.Create;
        FOcclusionQuery.AllocateHandle;
        if FOcclusionQuery.IsDataNeedUpdate then
          FOcclusionQuery.NotifyDataUpdated
        else
          FlareIsNotOccluded := (FOcclusionQuery.PixelCount <> 0);
        FOcclusionQuery.BeginQuery;
      end
      else
      begin
        // occlusion_test, stalls rendering a bit
        glEnable(GL_OCCLUSION_TEST_HP);
      end;

      glBegin(GL_QUADS);
      glVertex3f(posVector.X + 2, posVector.Y, 1);
      glVertex3f(posVector.X, posVector.Y + 2, 1);
      glVertex3f(posVector.X - 2, posVector.Y, 1);
      glVertex3f(posVector.X, posVector.Y - 2, 1);
      glEnd;

      if TgxOcclusionQueryHandle.IsSupported > False then
        FOcclusionQuery.EndQuery
      else
      begin
        glDisable(GL_OCCLUSION_TEST_HP);
        glGetBooleanv(GL_OCCLUSION_TEST_RESULT_HP, @FFlareIsNotOccluded)
      end;

      rci.gxStates.DepthFunc := cfLEqual;
      rci.gxStates.SetColorMask(cAllColorComponents);
    end
    else
    begin
      // Compares the distance to the lensflare, to the z-buffer depth.
      // This prevents the flare from being occluded by objects BEHIND the light.
      depth := CurrentBuffer.PixelToDistance(Round(screenPos.X), Round(rci.viewPortSize.cy - screenPos.Y));
      dist := VectorDistance(rci.cameraPosition, Self.AbsolutePosition);
      FlareIsNotOccluded := ((dist - depth) < 1);
    end;
  end;

  if FCurrSize >= 0 then
  begin

    // Random seed must be backed up, could be used for other purposes
    // (otherwise we essentially reset the random generator at each frame)
    oldSeed := RandSeed;
    RandSeed := Seed;

    SetupRenderingOptions(rci.gxStates);

    if [feGlow, feStreaks, feRays, feRing] * Elements <> [] then
    begin
      glTranslatef(posVector.X, posVector.Y, posVector.Z);

      // Glow (a circle with transparent edges):
      if feGlow in Elements then
      begin
        glBegin(GL_TRIANGLE_FAN);
        glColor4fv(GlowGradient.fromColor.AsAddress);
        glVertex2f(0, 0);
        glColor4fv(GlowGradient.toColor.AsAddress);
        for i := 0 to Resolution - 1 do
          glVertex2f(FCurrSize * FCosRes[i], Squeeze * FCurrSize * FSinRes[i]);
        glEnd;
      end;

      if feStreaks in Elements then
        RenderStreaks(rci.gxStates);

      // Rays (random-length lines from the origin):
      if feRays in Elements then
      begin
        if FTexRays.Handle <> 0 then
        begin
{$IFDEF USE_OPENGL_DEBUG}
          if GL_GREMEDY_string_marker then
            glStringMarkerGREMEDY(19, 'LensFlare.RaysQuad');
{$ENDIF}
          rci.gxStates.TextureBinding[0, ttTexture2D] := FTexRays.Handle;
          rci.gxStates.ActiveTextureEnabled[ttTexture2D] := True;
          glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

          glBegin(GL_QUADS);
          glTexCoord2f(0, 0);
          glVertex2f(-FCurrSize, -FCurrSize);
          glTexCoord2f(1, 0);
          glVertex2f(FCurrSize, -FCurrSize);
          glTexCoord2f(1, 1);
          glVertex2f(FCurrSize, FCurrSize);
          glTexCoord2f(0, 1);
          glVertex2f(-FCurrSize, FCurrSize);
          glEnd;

          rci.gxStates.ActiveTextureEnabled[ttTexture2D] := False;
        end
        else
          RenderRays(rci.gxStates, FCurrSize);
      end;

      if feRing in Elements then
        RenderRing;

      glLoadMatrixf(@projMatrix);
    end;

    if feSecondaries in Elements then
      RenderSecondaries(posVector);

    RandSeed := oldSeed;
  end;

  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TgxLensFlare.DoProgress(const progressTime: TgxProgressTimes);
begin
  inherited;
  FDeltaTime := progressTime.deltaTime;
end;

procedure TgxLensFlare.PreRender(activeBuffer: TgxSceneBuffer);
var
  texSize, maxSize: Integer;
  StateCache: TgxStateCache;
begin
  if FTexRays.Handle <> 0 then
    Exit;
  with activeBuffer.RenderingContext do
  begin
    StateCache := gxStates;
    PipelineTransformation.Push;
    PipelineTransformation.SetProjectionMatrix(CreateOrthoMatrix(0, activeBuffer.Width, 0, activeBuffer.Height, -1, 1));
    PipelineTransformation.SetViewMatrix(IdentityHmgMatrix);
  end;
  SetupRenderingOptions(StateCache);

  texSize := RoundUpToPowerOf2(size);
  if texSize < size * 1.5 then
    texSize := texSize * 2;
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
  if texSize > maxSize then
    texSize := maxSize;

  StateCache.Disable(stBlend);
  glColor4f(0, 0, 0, 0);
  glBegin(GL_QUADS);
  glVertex2f(0, 0);
  glVertex2f(texSize + 4, 0);
  glVertex2f(texSize + 4, texSize + 4);
  glVertex2f(0, texSize + 4);
  glEnd;
  StateCache.Enable(stBlend);

  glTranslatef(texSize * 0.5 + 2, texSize * 0.5 + 2, 0);
  RenderRays(StateCache, texSize * 0.5);

  FTexRays.AllocateHandle;
  StateCache.TextureBinding[0, ttTexture2D] := FTexRays.Handle;
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glCopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 2, 2, texSize, texSize, 0);

  activeBuffer.RenderingContext.PipelineTransformation.Pop;

///  CheckOpenGLError;
end;

procedure TgxLensFlare.SetGlowGradient(const val: TgxFlareGradient);
begin
  FGlowGradient.Assign(val);
  StructureChanged;
end;

procedure TgxLensFlare.SetRingGradient(const val: TgxFlareGradient);
begin
  FRingGradient.Assign(val);
  StructureChanged;
end;

procedure TgxLensFlare.SetStreaksGradient(const val: TgxFlareGradient);
begin
  FStreaksGradient.Assign(val);
  StructureChanged;
end;

procedure TgxLensFlare.SetRaysGradient(const val: TgxFlareGradient);
begin
  FRaysGradient.Assign(val);
  StructureChanged;
end;

procedure TgxLensFlare.SetSecondariesGradient(const val: TgxFlareGradient);
begin
  FSecondariesGradient.Assign(val);
  StructureChanged;
end;

procedure TgxLensFlare.SetSize(aValue: Integer);
begin
  FSize := aValue;
  StructureChanged;
end;

procedure TgxLensFlare.SetSeed(aValue: Integer);
begin
  FSeed := aValue;
  StructureChanged;
end;

procedure TgxLensFlare.SetSqueeze(aValue: Single);
begin
  FSqueeze := aValue;
  StructureChanged;
end;

function TgxLensFlare.StoreSqueeze: Boolean;
begin
  Result := (FSqueeze <> 1);
end;

procedure TgxLensFlare.SetNumStreaks(aValue: Integer);
begin
  FNumStreaks := aValue;
  StructureChanged;
end;

procedure TgxLensFlare.SetStreakWidth(aValue: Single);
begin
  FStreakWidth := aValue;
  StructureChanged;
end;

function TgxLensFlare.StoreStreakWidth: Boolean;
begin
  Result := (FStreakWidth <> 2);
end;

procedure TgxLensFlare.SetStreakAngle(aValue: Single);
begin
  FStreakAngle := aValue;
  StructureChanged;
end;

procedure TgxLensFlare.SetNumSecs(aValue: Integer);
begin
  FNumSecs := aValue;
  StructureChanged;
end;

procedure TgxLensFlare.SetResolution(aValue: Integer);
begin
  if FResolution <> aValue then
  begin
    FResolution := aValue;
    StructureChanged;
    SetLength(FSin20Res, 20 * FResolution);
    SetLength(FCos20Res, 20 * FResolution);
    PrepareSinCosCache(FSin20Res, FCos20Res, 0, 360);
    SetLength(FSinRes, FResolution);
    SetLength(FCosRes, FResolution);
    PrepareSinCosCache(FSinRes, FCosRes, 0, 360);
  end;
end;

procedure TgxLensFlare.SetAutoZTest(aValue: Boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;

procedure TgxLensFlare.SetElements(aValue: TFlareElements);
begin
  if FElements <> aValue then
  begin
    FElements := aValue;
    StructureChanged;
  end;
end;

procedure TgxLensFlare.SetDynamic(aValue: Boolean);
begin
  if aValue <> FDynamic then
  begin
    FDynamic := aValue;
    NotifyChange(Self);
  end;
end;

procedure TgxLensFlare.SetPreRenderPoint(const val: TgxRenderPoint);
begin
  if val <> FPreRenderPoint then
  begin
    if Assigned(FPreRenderPoint) then
      FPreRenderPoint.UnRegisterCallBack(Self.PreRenderEvent);
    FPreRenderPoint := val;
    if Assigned(FPreRenderPoint) then
      FPreRenderPoint.RegisterCallBack(Self.PreRenderEvent, Self.PreRenderPointFreed);
  end;
end;

procedure TgxLensFlare.PreRenderEvent(Sender: TObject; var rci: TgxRenderContextInfo);
begin
  PreRender(rci.buffer as TgxSceneBuffer);
end;

procedure TgxLensFlare.PreRenderPointFreed(Sender: TObject);
begin
  FPreRenderPoint := nil;
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterClasses([TgxLensFlare]);

end.
