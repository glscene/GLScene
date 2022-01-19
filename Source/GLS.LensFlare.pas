//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.LensFlare;

(* Lens flare object. *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  System.Math,

  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.PipelineTransformation,
  GLS.VectorGeometry,
  GLS.Objects,
  GLS.Context,
  GLS.Color,
  GLS.BaseClasses,
  GLS.RenderContextInfo,
  GLS.State,
  GLS.VectorTypes,
  GLS.Utils,
  GLS.TextureFormat;

type

  TGLFlareElement = (feGlow, feRing, feStreaks, feRays, feSecondaries);
  TGLFlareElements = set of TGLFlareElement;

  (* The actual gradients between two colors are, of course, calculated by OpenGL.
     The start and end colors of a gradient are stored to represent the color of
     lens flare elements. *)
  TGLFlareGradient = class(TGLUpdateAbleObject)
  private
    FFromColor: TGLColor;
    FToColor: TGLColor;
  protected
    procedure SetFromColor(const val: TGLColor);
    procedure SetToColor(const val: TGLColor);
  public
    constructor Create(AOwner: TPersistent); override;
    constructor CreateInitialized(AOwner: TPersistent;
      const fromColor, toColor: TGLColorVector);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property FromColor: TGLColor read FFromColor write SetFromColor;
    property ToColor: TGLColor read FToColor write SetToColor;
  end;

const
  cDefaultFlareElements = [feGlow, feRing, feStreaks, feRays, feSecondaries];

type

  TGLLensFlare = class(TGLBaseSceneObject)
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
    FElements: TGLFlareElements;
    FSin20Res, FCos20Res: array of Single;
    FSinRes, FCosRes: array of Single;
    FTexRays: TGLTextureHandle;
    FFlareIsNotOccluded: Boolean;
    FOcclusionQuery: TGLOcclusionQueryHandle;
    FGlowGradient: TGLFlareGradient;
    FRingGradient: TGLFlareGradient;
    FStreaksGradient: TGLFlareGradient;
    FRaysGradient: TGLFlareGradient;
    FSecondariesGradient: TGLFlareGradient;
    FDynamic: Boolean;
    FPreRenderPoint: TGLRenderPoint;
  protected
    procedure SetGlowGradient(const val: TGLFlareGradient);
    procedure SetRingGradient(const val: TGLFlareGradient);
    procedure SetStreaksGradient(const val: TGLFlareGradient);
    procedure SetRaysGradient(const val: TGLFlareGradient);
    procedure SetSecondariesGradient(const val: TGLFlareGradient);
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
    procedure SetElements(aValue: TGLFlareElements);
    procedure SetDynamic(aValue: Boolean);
    procedure SetPreRenderPoint(const val: TGLRenderPoint);
    procedure PreRenderEvent(Sender: TObject; var rci: TGLRenderContextInfo);
    procedure PreRenderPointFreed(Sender: TObject);
    (* These are quite unusual in that they don't use an RCI, since
     PreRender is done before proper rendering starts, but we do know
     which RC is being used, so we can use this state cache *)
    procedure SetupRenderingOptions(StateCache: TGLStateCache);
    procedure RenderRays(StateCache: TGLStateCache; const size: Single);
    procedure RenderStreaks(StateCache: TGLStateCache);
    procedure RenderRing;
    procedure RenderSecondaries(const posVector: TAffineVector);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BuildList(var rci: TGLRenderContextInfo); override;
    procedure DoProgress(const progressTime: TGLProgressTimes); override;
    (* Prepares pre-rendered texture to speed up actual rendering.
       Will use the currently active context as scratch space, and will
       automatically do nothing if things have already been prepared,
       thus you can invoke it systematically in a Viewer.BeforeRender
       event f.i. *)
    procedure PreRender(activeBuffer: TGLSceneBuffer);
    (* Access to the Flare's current size.
       Flares decay or grow back over several frames, depending on their
       occlusion status, and this property allows to track or manually
       alter this instantaneous size. *)
    property FlareInstantaneousSize: Single read FCurrSize write FCurrSize;
  published
    property GlowGradient: TGLFlareGradient read FGlowGradient write SetGlowGradient;
    property RingGradient: TGLFlareGradient read FRingGradient;
    property StreaksGradient: TGLFlareGradient read FStreaksGradient;
    property RaysGradient: TGLFlareGradient read FRaysGradient;
    property SecondariesGradient: TGLFlareGradient read FSecondariesGradient;
    // MaxRadius of the flare.
    property Size: Integer read FSize write SetSize default 50;
    // Random seed
    property Seed: Integer read FSeed write SetSeed;
    // To create elliptic flares.
    property Squeeze: Single read FSqueeze write SetSqueeze stored StoreSqueeze;
    // Number of streaks.
    property NumStreaks: Integer read FNumStreaks write SetNumStreaks default 4;
    // Width of the streaks.
    property StreakWidth: Single read FStreakWidth write SetStreakWidth stored
      StoreStreakWidth;
    // Angle of the streaks (in degrees)
    property StreakAngle: Single read FStreakAngle write SetStreakAngle;
    // Number of secondary flares.
    property NumSecs: Integer read FNumSecs write SetNumSecs default 8;
    // Number of segments used when rendering circles.
    property Resolution: Integer read FResolution write SetResolution default 64;
    (* Automatically computes FlareIsNotOccluded depending on ZBuffer test.
       Not that the automated test may use test result from the previous
       frame into the next (to avoid a rendering stall). *)
    property AutoZTest: Boolean read FAutoZTest write SetAutoZTest default True;
    (* Is the LensFlare not occluded?.
       If false the flare will fade away, if true, it will fade in and stay.
       This value is automatically updated if AutoZTest is set. *)
    property FlareIsNotOccluded: Boolean read FFlareIsNotOccluded write
      FFlareIsNotOccluded;
    // Which elements should be rendered?
    property Elements: TGLFlareElements read FElements write SetElements default
      cDefaultFlareElements;
    (* Is the flare size adjusted dynamically?
       If true, the flare size will be grown and reduced over a few frames
       when it switches between occluded and non-occluded states. This
       requires animation to be active, but results in a smoother appearance.
       When false, flare will either be at full size or hidden.
       The flare is always considered non-dynamic at design-time. *)
    property Dynamic: Boolean read FDynamic write FDynamic default True;
    (* PreRender point for pre-rendered flare textures.
       See PreRender method for more details. *)
    property PreRenderPoint: TGLRenderPoint read FPreRenderPoint write
      SetPreRenderPoint;
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
// ------------------ TGLFlareGradient ------------------
// ------------------

constructor TGLFlareGradient.Create(AOwner: TPersistent);
begin
  inherited;
  FFromColor := TGLColor.Create(Self);
  FToColor := TGLColor.Create(Self);
end;

constructor TGLFlareGradient.CreateInitialized(AOwner: TPersistent;
  const fromColor, toColor: TGLColorVector);
begin
  Create(AOwner);
  FFromColor.Initialize(fromColor);
  FToColor.Initialize(toColor);
end;

destructor TGLFlareGradient.Destroy;
begin
  FToColor.Free;
  FFromColor.Free;
  inherited;
end;

procedure TGLFlareGradient.Assign(Source: TPersistent);
begin
  if Source is TGLFlareGradient then
  begin
    FromColor := TGLFlareGradient(Source).FromColor;
    ToColor := TGLFlareGradient(Source).ToColor;
  end;
  inherited;
end;

procedure TGLFlareGradient.SetFromColor(const val: TGLColor);
begin
  FFromColor.Assign(val);
end;

procedure TGLFlareGradient.SetToColor(const val: TGLColor);
begin
  FToColor.Assign(val);
end;

// ------------------
// ------------------ TGLLensFlare ------------------
// ------------------

constructor TGLLensFlare.Create(AOwner: TComponent);
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
  FGlowGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(1, 1, 0.8, 0.3), VectorMake(1, 0.2, 0, 0));
  FRingGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(0.5, 0.2, 0, 0.1), VectorMake(0.5, 0.4, 0, 0.1));
  FStreaksGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(1, 1, 1, 0.2), VectorMake(0.2, 0, 1, 0));
  FRaysGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(1, 0.8, 0.5, 0.05), VectorMake(0.5, 0.2, 0, 0));
  FSecondariesGradient := TGLFlareGradient.CreateInitialized(Self,
    VectorMake(0, 0.2, 1, 0), VectorMake(0, 0.8, 0.2, 0.15));
  FTexRays := TGLTextureHandle.Create;
end;

destructor TGLLensFlare.Destroy;
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

procedure TGLLensFlare.Notification(AComponent: TComponent; Operation:
  TOperation);
begin
  if (Operation = opRemove) and (AComponent = FPreRenderPoint) then
    PreRenderPoint := nil;
  inherited;
end;

procedure TGLLensFlare.SetupRenderingOptions(StateCache: TGLStateCache);
begin
  StateCache.Disable(stLighting);
  StateCache.Disable(stDepthTest);
  StateCache.Disable(stFog);
  StateCache.Disable(stColorMaterial);
  StateCache.Disable(stCullFace);
  StateCache.DepthWriteMask := False;
  StateCache.Enable(stBlend);
  StateCache.SetBlendFunc(bfSrcAlpha, bfOne);
  StateCache.Disable(stAlphaTest);
  StateCache.PolygonMode := pmFill;
end;

procedure TGLLensFlare.RenderRays(StateCache: TGLStateCache; const size:
  Single);
var
  i: Integer;
  rnd: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if gl.GREMEDY_string_marker then
    gl.StringMarkerGREMEDY(14, 'LensFlare.Rays');
{$ENDIF}
  StateCache.LineWidth := 1;
  StateCache.Disable(stLineSmooth);
  StateCache.Disable(stLineStipple);

  gl.Begin_(GL_LINES);
  for i := 0 to Resolution * 20 - 1 do
  begin
    if (i and 1) <> 0 then
      rnd := 1.5 * Random * size
    else
      rnd := Random * size;
    gl.Color4fv(RaysGradient.FromColor.AsAddress);
    gl.Vertex2f(0, 0);
    gl.Color4fv(RaysGradient.ToColor.AsAddress);
    gl.Vertex2f(rnd * FCos20Res[i], rnd * FSin20Res[i] * Squeeze);
  end;
  gl.End_;
end;

procedure TGLLensFlare.RenderStreaks(StateCache: TGLStateCache);
var
  i: Integer;
  a, f, s, c: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if gl.GREMEDY_string_marker then
    gl.StringMarkerGREMEDY(17, 'LensFlare.Streaks');
{$ENDIF}
  StateCache.Enable(stLineSmooth);
  StateCache.LineWidth := StreakWidth;
  a := c2PI / NumStreaks;
  f := 1.5 * FCurrSize;
  gl.Begin_(GL_LINES);
  for i := 0 to NumStreaks - 1 do
  begin
    SinCosine(StreakAngle * cPIdiv180 + a * i, f, s, c);
    gl.Color4fv(StreaksGradient.FromColor.AsAddress);
    gl.Vertex3fv(@NullVector);
    gl.Color4fv(StreaksGradient.ToColor.AsAddress);
    gl.Vertex2f(c, Squeeze * s);
  end;
  gl.End_;
  StateCache.Disable(stLineSmooth);
end;

procedure TGLLensFlare.RenderRing;
var
  i: Integer;
  rW, s0, c0, s, c: Single;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(14, 'LensFlare.Ring');
{$ENDIF}
  rW := FCurrSize * (1 / 15); // Ring width
  gl.Begin_(GL_QUADS);
  s0 := 0;
  c0 := 0.6;
  for i := 0 to Resolution - 1 do
  begin
    s := s0;
    c := c0;
    s0 := FSinRes[i] * 0.6 * Squeeze;
    c0 := FCosRes[i] * 0.6;
    gl.Color4fv(GlowGradient.ToColor.AsAddress);
    gl.Vertex2f((FCurrSize - rW) * c, (FCurrSize - rW) * s);
    gl.Color4fv(RingGradient.FromColor.AsAddress);
    gl.Vertex2f(FCurrSize * c, Squeeze * FCurrSize * s);
    gl.Vertex2f(FCurrSize * c0, FCurrSize * s0);
    gl.Color4fv(GlowGradient.ToColor.AsAddress);
    gl.Vertex2f((FCurrSize - rW) * c0, (FCurrSize - rW) * s0);
    gl.Color4fv(RingGradient.FromColor.AsAddress);
    gl.Vertex2f(FCurrSize * c, FCurrSize * s);
    gl.Vertex2f(FCurrSize * c0, FCurrSize * s0);
    gl.Color4fv(GlowGradient.ToColor.AsAddress);
    gl.Vertex2f((FCurrSize + rW) * c0, (FCurrSize + rW) * s0);
    gl.Vertex2f((FCurrSize + rW) * c, (FCurrSize + rW) * s);
  end;
  gl.End_;
end;

procedure TGLLensFlare.RenderSecondaries(const posVector: TAffineVector);
var
  i, j: Integer;
  rnd: Single;
  v: TAffineVector;
  grad: TGLFlareGradient;
begin
{$IFDEF USE_OPENGL_DEBUG}
  if GL.GREMEDY_string_marker then
    GL.StringMarkerGREMEDY(21, 'LensFlare.Secondaries');
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
    gl.Begin_(GL_TRIANGLE_FAN);
    gl.Color4fv(grad.FromColor.AsAddress);
    gl.Vertex2f(v.X, v.Y);
    gl.Color4fv(grad.ToColor.AsAddress);
    for i := 0 to Resolution - 1 do
      gl.Vertex2f(FCosRes[i] * rnd + v.X, FSinRes[i] * rnd + v.Y);
    gl.End_;
  end;
end;

procedure TGLLensFlare.BuildList(var rci: TGLRenderContextInfo);
var
  i: Integer;
  depth, dist: Single;
  posVector, v, rv: TAffineVector;
  screenPos: TAffineVector;
  flareInViewPort, dynamicSize: Boolean;
  oldSeed: LongInt;
  projMatrix: TGLMatrix;
  CurrentBuffer: TGLSceneBuffer;
begin
  if (rci.drawState = dsPicking) then
  begin
    if Count <> 0 then
      Self.RenderChildren(0, Count - 1, rci);
    Exit;
  end;
  CurrentBuffer := TGLSceneBuffer(rci.buffer);
  SetVector(v, AbsolutePosition);
  // are we looking towards the flare?
  rv := VectorSubtract(v, PAffineVector(@rci.cameraPosition)^);
  if VectorDotProduct(rci.cameraDirection, rv) > 0 then
  begin
    // find out where it is on the screen.
    screenPos := CurrentBuffer.WorldToScreen(v);
    flareInViewPort := (screenPos.X < rci.viewPortSize.cx)
    and (screenPos.X >= 0)
    and (screenPos.Y < rci.viewPortSize.cy)
    and (screenPos.Y >= 0);
  end
  else
    flareInViewPort := False;
  dynamicSize := FDynamic and not (csDesigning in ComponentState);
  if dynamicSize then
  begin
    // make the glow appear/disappear progressively
    if flareInViewPort and FlareIsNotOccluded then
    begin
      FCurrSize := FCurrSize + FDeltaTime * 10 * Size;
      if FCurrSize > Size then
        FCurrSize := Size;
    end
    else
    begin
      FCurrSize := FCurrSize - FDeltaTime * 10 * Size;
      if FCurrSize < 0 then
        FCurrSize := 0;
    end;
  end
  else
  begin
    if flareInViewPort and FlareIsNotOccluded then
      FCurrSize := Size
    else
      FCurrSize := 0;
  end;
  // Prepare matrices
  gl.PushMatrix;
  gl.LoadMatrixf(@CurrentBuffer.BaseProjectionMatrix);

  gl.MatrixMode(GL_PROJECTION);
  gl.PushMatrix;
  projMatrix := IdentityHmgMatrix;
  projMatrix.V[0].X := 2 / rci.viewPortSize.cx;
  projMatrix.V[1].Y := 2 / rci.viewPortSize.cy;
  gl.LoadMatrixf(@projMatrix);

  MakeVector(posVector,
    screenPos.X - rci.viewPortSize.cx * 0.5,
    screenPos.Y - rci.viewPortSize.cy * 0.5,
    0);
  if AutoZTest then
  begin
    if (dynamicSize and (GL.HP_occlusion_test or
      TGLOcclusionQueryHandle.IsSupported)) then
    begin
      // hardware-based occlusion test is possible
      FlareIsNotOccluded := True;
      rci.GLStates.SetColorMask([]);
      rci.GLStates.Disable(stAlphaTest);
      rci.GLStates.DepthWriteMask := False;
      rci.GLStates.Enable(stDepthTest);
      rci.GLStates.DepthFunc := cfLEqual;
      if TGLOcclusionQueryHandle.IsSupported then
      begin
        // preferred method, doesn't stall rendering too badly
        if not Assigned(FOcclusionQuery) then
          FOcclusionQuery := TGLOcclusionQueryHandle.Create;
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
        gl.Enable(GL_OCCLUSION_TEST_HP);
      end;
      gl.Begin_(GL_QUADS);
      gl.Vertex3f(posVector.X + 2, posVector.Y, 1);
      gl.Vertex3f(posVector.X, posVector.Y + 2, 1);
      gl.Vertex3f(posVector.X - 2, posVector.Y, 1);
      gl.Vertex3f(posVector.X, posVector.Y - 2, 1);
      gl.End_;
      if TGLOcclusionQueryHandle.IsSupported then
        FOcclusionQuery.EndQuery
      else
      begin
        gl.Disable(GL_OCCLUSION_TEST_HP);
        gl.GetBooleanv(GL_OCCLUSION_TEST_RESULT_HP, @FFlareIsNotOccluded)
      end;
      rci.GLStates.DepthFunc := cfLEqual;
      rci.GLStates.SetColorMask(cAllColorComponents);
    end
    else
    begin
      //Compares the distance to the lensflare, to the z-buffer depth.
      //This prevents the flare from being occluded by objects BEHIND the light.
      (*
      depth := CurrentBuffer.PixelToDistance(Round(ScreenPos.X),
        Round(rci.viewPortSize.cy - ScreenPos.Y));
      dist := VectorDistance(rci.cameraPosition, self.AbsolutePosition);
      FlareIsNotOccluded := ((dist - depth) < 1);
      *)
    end;
  end;
  if FCurrSize >= 0 then
  begin
    // Random seed must be backed up, could be used for other purposes
    // (otherwise we essentially reset the random generator at each frame)
    oldSeed := RandSeed;
    RandSeed := Seed;
    SetupRenderingOptions(rci.GLStates);
    if [feGlow, feStreaks, feRays, feRing] * Elements <> [] then
    begin
      gl.Translatef(posVector.X, posVector.Y, posVector.Z);
      // Glow (a circle with transparent edges):
      if feGlow in Elements then
      begin
        gl.Begin_(GL_TRIANGLE_FAN);
        gl.Color4fv(GlowGradient.FromColor.AsAddress);
        gl.Vertex2f(0, 0);
        gl.Color4fv(GlowGradient.ToColor.AsAddress);
        for i := 0 to Resolution - 1 do
          gl.Vertex2f(FCurrSize * FCosRes[i],
            Squeeze * FCurrSize * FSinRes[i]);
        gl.End_;
      end;

      if feStreaks in Elements then
        RenderStreaks(rci.GLStates);

      // Rays (random-length lines from the origin):
      if feRays in Elements then
      begin
        if FTexRays.Handle <> 0 then
        begin
        {$IFDEF USE_OPENGL_DEBUG}
          if GL.GREMEDY_string_marker then
            GL.StringMarkerGREMEDY(19, 'LensFlare.RaysQuad');
        {$ENDIF}
          rci.GLStates.TextureBinding[0, ttTexture2D] := FTexRays.Handle;
          rci.GLStates.ActiveTextureEnabled[ttTexture2D] := True;
          gl.TexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

          gl.Begin_(GL_QUADS);
          gl.TexCoord2f(0, 0);
          gl.Vertex2f(-FCurrSize, -FCurrSize);
          gl.TexCoord2f(1, 0);
          gl.Vertex2f(FCurrSize, -FCurrSize);
          gl.TexCoord2f(1, 1);
          gl.Vertex2f(FCurrSize, FCurrSize);
          gl.TexCoord2f(0, 1);
          gl.Vertex2f(-FCurrSize, FCurrSize);
          gl.End_;

          rci.GLStates.ActiveTextureEnabled[ttTexture2D] := False;
        end
        else
          RenderRays(rci.GLStates, FCurrSize);
      end;

      if feRing in Elements then
        RenderRing;

      gl.LoadMatrixf(@projMatrix);
    end;

    if feSecondaries in Elements then
      RenderSecondaries(posVector);

    RandSeed := oldSeed;
  end;

  gl.PopMatrix;
  gl.MatrixMode(GL_MODELVIEW);
  gl.PopMatrix;

  if Count > 0 then
    Self.RenderChildren(0, Count - 1, rci);
end;

procedure TGLLensFlare.DoProgress(const progressTime: TGLProgressTimes);
begin
  inherited;
  FDeltaTime := progressTime.deltaTime;
end;

procedure TGLLensFlare.PreRender(activeBuffer: TGLSceneBuffer);
var
  i, texSize, maxSize: Integer;
  stateCache: TGLStateCache;
begin
  if FTexRays.Handle <> 0 then
    Exit;
  with activeBuffer.RenderingContext do
  begin
    stateCache := GLStates;
    PipelineTransformation.Push;
    PipelineTransformation.SetProjectionMatrix(CreateOrthoMatrix(0, activeBuffer.Width, 0, activeBuffer.Height, -1, 1));
    PipelineTransformation.SetViewMatrix(IdentityHmgMatrix);
  end;
  SetupRenderingOptions(stateCache);

  texSize := RoundUpToPowerOf2(Size);
  if texSize < Size * 1.5 then
    texSize := texSize * 2;
  gl.GetIntegerv(GL_MAX_TEXTURE_SIZE, @maxSize);
  if texSize > maxSize then
    texSize := maxSize;

  stateCache.Disable(stBlend);
  gl.Color4f(0, 0, 0, 0);
  gl.Begin_(GL_QUADS);
  gl.Vertex2f(0, 0);
  gl.Vertex2f(texSize + 4, 0);
  gl.Vertex2f(texSize + 4, texSize + 4);
  gl.Vertex2f(0, texSize + 4);
  gl.End_;
  stateCache.Enable(stBlend);

  gl.Translatef(texSize * 0.5 + 2, texSize * 0.5 + 2, 0);
  RenderRays(stateCache, texSize * 0.5);

  FTexRays.AllocateHandle;
  stateCache.TextureBinding[0, ttTexture2D] := FTexRays.Handle;
  if gl.EXT_texture_edge_clamp then
    i := GL_CLAMP_TO_EDGE
  else
    i := GL_CLAMP;
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, i);
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, i);
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  gl.TexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  gl.CopyTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 2, 2, texSize, texSize, 0);

  activeBuffer.RenderingContext.PipelineTransformation.Pop;

  gl.CheckError;
end;

procedure TGLLensFlare.SetGlowGradient(const val: TGLFlareGradient);
begin
  FGlowGradient.Assign(val);
  StructureChanged;
end;

procedure TGLLensFlare.SetRingGradient(const val: TGLFlareGradient);
begin
  FRingGradient.Assign(val);
  StructureChanged;
end;

procedure TGLLensFlare.SetStreaksGradient(const val: TGLFlareGradient);
begin
  FStreaksGradient.Assign(val);
  StructureChanged;
end;

procedure TGLLensFlare.SetRaysGradient(const val: TGLFlareGradient);
begin
  FRaysGradient.Assign(val);
  StructureChanged;
end;

procedure TGLLensFlare.SetSecondariesGradient(const val: TGLFlareGradient);
begin
  FSecondariesGradient.Assign(val);
  StructureChanged;
end;

procedure TGLLensFlare.SetSize(aValue: Integer);
begin
  FSize := aValue;
  StructureChanged;
end;

procedure TGLLensFlare.SetSeed(aValue: Integer);
begin
  FSeed := aValue;
  StructureChanged;
end;

procedure TGLLensFlare.SetSqueeze(aValue: Single);
begin
  FSqueeze := aValue;
  StructureChanged;
end;

function TGLLensFlare.StoreSqueeze: Boolean;
begin
  Result := (FSqueeze <> 1);
end;

procedure TGLLensFlare.SetNumStreaks(aValue: Integer);
begin
  FNumStreaks := aValue;
  StructureChanged;
end;

procedure TGLLensFlare.SetStreakWidth(aValue: Single);
begin
  FStreakWidth := aValue;
  StructureChanged;
end;

function TGLLensFlare.StoreStreakWidth: Boolean;
begin
  Result := (FStreakWidth <> 2);
end;

procedure TGLLensFlare.SetStreakAngle(aValue: Single);
begin
  FStreakAngle := aValue;
  StructureChanged;
end;

procedure TGLLensFlare.SetNumSecs(aValue: Integer);
begin
  FNumSecs := aValue;
  StructureChanged;
end;

procedure TGLLensFlare.SetResolution(aValue: Integer);
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

procedure TGLLensFlare.SetAutoZTest(aValue: Boolean);
begin
  if FAutoZTest <> aValue then
  begin
    FAutoZTest := aValue;
    StructureChanged;
  end;
end;

procedure TGLLensFlare.SetElements(aValue: TGLFlareElements);
begin
  if FElements <> aValue then
  begin
    FElements := aValue;
    StructureChanged;
  end;
end;

procedure TGLLensFlare.SetDynamic(aValue: Boolean);
begin
  if aValue <> FDynamic then
  begin
    FDynamic := aValue;
    NotifyChange(Self);
  end;
end;

procedure TGLLensFlare.SetPreRenderPoint(const val: TGLRenderPoint);
begin
  if val <> FPreRenderPoint then
  begin
    if Assigned(FPreRenderPoint) then
      FPreRenderPoint.UnRegisterCallBack(Self.PreRenderEvent);
    FPreRenderPoint := val;
    if Assigned(FPreRenderPoint) then
      FPreRenderPoint.RegisterCallBack(Self.PreRenderEvent,
        Self.PreRenderPointFreed);
  end;
end;

procedure TGLLensFlare.PreRenderEvent(Sender: TObject; var rci:
  TGLRenderContextInfo);
begin
  PreRender(rci.buffer as TGLSceneBuffer);
end;

procedure TGLLensFlare.PreRenderPointFreed(Sender: TObject);
begin
  FPreRenderPoint := nil;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterClasses([TGLLensFlare]);

end.
