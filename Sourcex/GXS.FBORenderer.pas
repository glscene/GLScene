//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FBORenderer;

(* Implements FBO support *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.Classes,
  System.SysUtils,
  FMX.Dialogs,

  GXS.PersistentClasses,
  GXS.VectorGeometry,
  GXS.Scene,
  GXS.Texture,
  GXS.Context,
  GXS.FBO,
  GXS.Color,
  GXS.Material,
  GXS.RenderContextInfo,
  GXS.State,
  GXS.PipelineTransformation,
  GXS.TextureFormat,
  GXS.VectorTypes,
  GXS.MultisampleImage;

type
  TgxEnabledRenderBuffer = (erbDepth, erbStencil);
  TgxEnabledRenderBuffers = set of TgxEnabledRenderBuffer;

  TgxFBOTargetVisibility = (tvDefault, tvFBOOnly);

  TgxFBOClearOption = (coColorBufferClear, coDepthBufferClear, coStencilBufferClear, coUseBufferBackground);
  TgxFBOClearOptions = set of TgxFBOClearOption;

  TgxTextureArray = array of TgxTexture;

  TSetTextureTargetsEvent = procedure(Sender: TObject; var colorTexs: TgxTextureArray) of object;

  TgxFBORenderer = class(TgxBaseSceneObject, IgxMaterialLibrarySupported)
  private
    FFbo: TgxFrameBuffer;
    FDepthRBO: TgxDepthRBO;
    FStencilRBO: TgxStencilRBO;
    FColorAttachment: Integer;
    FRendering: Boolean;
    FHasColor: Boolean;
    FHasDepth: Boolean;
    FHasStencil: Boolean;
    FMaterialLibrary: TgxMaterialLibrary;
    FColorTextureName: TgxLibMaterialName;
    FDepthTextureName: TgxLibMaterialName;
    FWidth: Integer;
    FHeight: Integer;
    FForceTextureDimensions: Boolean;
    FStencilPrecision: TgxStencilPrecision;
    FRootObject: TgxBaseSceneObject;
    FRootVisible: Boolean;
    FCamera: TgxCamera;
    FEnabledRenderBuffers: TgxEnabledRenderBuffers;
    FTargetVisibility: TgxFBOTargetVisibility;
    FBeforeRender: TDirectRenderEvent;
    FPostInitialize: TNotifyEvent;
    FAfterRender: TDirectRenderEvent;
    FPreInitialize: TNotifyEvent;
    FBackgroundColor: TgxColor;
    FClearOptions: TgxFBOClearOptions;
    FAspect: Single;
    FSceneScaleFactor: Single;
    FUseLibraryAsMultiTarget: Boolean;
    FPostGenerateMipmap: Boolean;
    FMaxSize: Integer;
    FMaxAttachment: Integer;
    FStoreCamera: array [0 .. 2] of TVector4f;
    FOnSetTextureTargets: TSetTextureTargetsEvent;
    // implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TgxAbstractMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary);
    procedure SetDepthTextureName(const Value: TgxLibMaterialName);
    procedure SetColorTextureName(const Value: TgxLibMaterialName);
    procedure SetForceTextureDimentions(const Value: Boolean);
    procedure SetHeight(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetLayer(const Value: Integer);
    function GetLayer: Integer;
    procedure SetLevel(const Value: Integer);
    function GetLevel: Integer;
    procedure SetStencilPrecision(const Value: TgxStencilPrecision);
    procedure SetRootObject(const Value: TgxBaseSceneObject);
    function GetViewport: TRectangle;
    procedure SetCamera(const Value: TgxCamera);
    procedure SetEnabledRenderBuffers(const Value: TgxEnabledRenderBuffers);
    procedure SetTargetVisibility(const Value: TgxFBOTargetVisibility);
    procedure SetBackgroundColor(const Value: TgxColor);
    function StoreSceneScaleFactor: Boolean;
    function StoreAspect: Boolean;
    procedure SetUseLibraryAsMultiTarget(Value: Boolean);
    procedure SetPostGenerateMipmap(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Initialize;
    procedure ForceDimensions(Texture: TgxTexture);
    procedure RenderToFBO(var ARci: TgxRenderContextInfo);
    procedure ApplyCamera(var ARci: TgxRenderContextInfo);
    procedure UnApplyCamera(var ARci: TgxRenderContextInfo);
    procedure DoBeforeRender(var ARci: TgxRenderContextInfo);
    procedure DoAfterRender(var ARci: TgxRenderContextInfo);
    procedure DoPreInitialize;
    procedure DoPostInitialize;
    property HasColor: Boolean read FHasColor;
    property HasDepth: Boolean read FHasDepth;
    property HasStencil: Boolean read FHasStencil;
    property Viewport: TRectangle read GetViewport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TgxRenderContextInfo; ARenderSelf: Boolean; ARenderChildren: Boolean); override;
    (* Layer (also cube map face) is activated only on
      the volume textures, texture array and cube map.
      You can select the layer during the drawing to. *)
    property Layer: Integer read GetLayer write SetLayer;
    // Mipmap Level where will be rendering
    property Level: Integer read GetLevel write SetLevel;
  published
    property Active: Boolean read GetVisible write SetVisible default True;
    property PickableTarget: Boolean read GetPickable write SetPickable default False;
    { force texture dimensions when initializing
      only works with TgxBlankImage and GLfloatDataImage, otherwise does nothing }
    property ForceTextureDimensions: Boolean read FForceTextureDimensions write SetForceTextureDimentions default True;
    property Width: Integer read FWidth write SetWidth default 256;
    property Height: Integer read FHeight write SetHeight default 256;
    property Aspect: Single read FAspect write FAspect stored StoreAspect;
    property ColorTextureName: TgxLibMaterialName read FColorTextureName write SetColorTextureName;
    property DepthTextureName: TgxLibMaterialName read FDepthTextureName write SetDepthTextureName;
    property MaterialLibrary: TgxAbstractMaterialLibrary read GetMaterialLibrary write SetMaterialLibrary;
    property BackgroundColor: TgxColor read FBackgroundColor write SetBackgroundColor;
    property ClearOptions: TgxFBOClearOptions read FClearOptions write FClearOptions;
    { camera used for rendering to the FBO
      if not assigned, use the active view's camera }
    property Camera: TgxCamera read FCamera write SetCamera;
    { adjust the scene scale of the camera so that the rendering
      becomes independent of the width of the fbo renderer
      0 = disabled }
    property SceneScaleFactor: Single read FSceneScaleFactor write FSceneScaleFactor stored StoreSceneScaleFactor;
    { root object used when rendering to the FBO
      if not assigned, uses itself as root and renders the child objects to the FBO }
    property RootObject: TgxBaseSceneObject read FRootObject write SetRootObject;
    { determines if target is rendered to FBO only or rendered normally
      in FBO only mode, if RootObject is assigned, the RootObject's Visible flag is modified
      in default mode, if RootObject is not assigned, children are rendered normally after being
      rendered to the FBO }
    property TargetVisibility: TgxFBOTargetVisibility read FTargetVisibility write SetTargetVisibility default tvDefault;
    { Enables the use of a render buffer if a texture is not assigned }
    property EnabledRenderBuffers: TgxEnabledRenderBuffers read FEnabledRenderBuffers write SetEnabledRenderBuffers;
    { use stencil buffer }
    property StencilPrecision: TgxStencilPrecision read FStencilPrecision write SetStencilPrecision default spDefault;
    { called before rendering to the FBO }
    property BeforeRender: TDirectRenderEvent read FBeforeRender write FBeforeRender;
    { called after the rendering to the FBO }
    property AfterRender: TDirectRenderEvent read FAfterRender write FAfterRender;
    { Called before the FBO is initialized
      the FBO is bound before calling this event }
    property PreInitialize: TNotifyEvent read FPreInitialize write FPreInitialize;
    { Called after the FBO is initialized, but before any rendering
      the FBO is bound before calling this event }
    property PostInitialize: TNotifyEvent read FPostInitialize write FPostInitialize;
    property UseLibraryAsMultiTarget: Boolean read FUseLibraryAsMultiTarget write SetUseLibraryAsMultiTarget default False;
    { Control mipmap generation after rendering
      texture must have MinFilter with mipmaping }
    property PostGenerateMipmap: Boolean read FPostGenerateMipmap write SetPostGenerateMipmap default True;
    { Allows multiTargeting to different texture sources instead of all coming
      from one single MatLib with UseLibraryAsMultiTarget. OnSetTextureTargets
      overrides the other method of setting target textures via the MaterialLibrary,
      ColorTextureName and DepthTextureName propertes }
    property OnSetTextureTargets: TSetTextureTargetsEvent read FOnSetTextureTargets write FOnSetTextureTargets;
  end;

//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// TgxFBORenderer
//------------------------------------------------------------------------------

procedure TgxFBORenderer.ApplyCamera(var ARci: TgxRenderContextInfo);
var
  sc: Single;
begin
  with ARci.PipelineTransformation do
  begin
    Push;
    if Assigned(Camera) then
    begin
      FStoreCamera[0] := ARci.cameraPosition;
      FStoreCamera[1] := ARci.cameraDirection;
      FStoreCamera[2] := ARci.cameraUp;
      IdentityAll;
      sc := FCamera.SceneScale;
      if FSceneScaleFactor > 0 then
        FCamera.SceneScale := Width / FSceneScaleFactor;
      FCamera.ApplyPerspective(Viewport, Width, Height, 96);
      // 96 is default dpi
      FCamera.SceneScale := sc;

      SetViewMatrix(CreateScaleMatrix(Vector3fMake(1.0 / FAspect, 1.0, 1.0)));
      FCamera.Apply;
    end
    else
    begin
      SetViewMatrix(MatrixMultiply(ViewMatrix^, CreateScaleMatrix(Vector3fMake(1.0 / FAspect, 1.0, 1.0))));
    end;
  end;
end;

procedure TgxFBORenderer.UnApplyCamera(var ARci: TgxRenderContextInfo);
begin
  ARci.cameraPosition := FStoreCamera[0];
  ARci.cameraDirection := FStoreCamera[1];
  ARci.cameraUp := FStoreCamera[2];
  ARci.PipelineTransformation.Pop;
end;

constructor TgxFBORenderer.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := [osDirectDraw, osNoVisibilityCulling];
  FFbo := TgxFrameBuffer.Create;
  FBackgroundColor := TgxColor.Create(Self);
  FUseLibraryAsMultiTarget := False;
  FForceTextureDimensions := True;
  FWidth := 256;
  FHeight := 256;
  FEnabledRenderBuffers := [erbDepth];
  FClearOptions := [coColorBufferClear, coDepthBufferClear, coStencilBufferClear, coUseBufferBackground];
  PickableTarget := False;
  FAspect := 1.0;
  FSceneScaleFactor := 0.0;
  FPostGenerateMipmap := True;
  StructureChanged;
end;

destructor TgxFBORenderer.Destroy;
begin
  FFbo.Free;
  FDepthRBO.Free;
  FStencilRBO.Free;
  FBackgroundColor.Free;
  inherited;
end;

procedure TgxFBORenderer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FRootObject) and (Operation = opRemove) then
    FRootObject := nil;
end;

procedure TgxFBORenderer.DoAfterRender(var ARci: TgxRenderContextInfo);
begin
  if Assigned(FAfterRender) then
    FAfterRender(Self, ARci);
end;

procedure TgxFBORenderer.DoBeforeRender(var ARci: TgxRenderContextInfo);
begin
  if Assigned(FBeforeRender) then
    FBeforeRender(Self, ARci);
end;

procedure TgxFBORenderer.DoPostInitialize;
begin
  if Assigned(FPostInitialize) then
    FPostInitialize(Self);
end;

procedure TgxFBORenderer.DoPreInitialize;
begin
  if Assigned(FPreInitialize) then
    FPreInitialize(Self);
end;

procedure TgxFBORenderer.DoRender(var ARci: TgxRenderContextInfo; ARenderSelf, ARenderChildren: Boolean);
begin
  if not(csDesigning in ComponentState) then
    RenderToFBO(ARci);

  if (not Assigned(FRootObject)) and (TargetVisibility = tvDefault) and ARenderChildren then
    RenderChildren(0, Count - 1, ARci);
end;

procedure TgxFBORenderer.ForceDimensions(Texture: TgxTexture);
var
  bi: TgxBlankImage;
  mi: TgxMultisampleImage;
begin
  if Texture.Image is TgxBlankImage then
  begin
    bi := TgxBlankImage(Texture.Image);
    bi.Width := Width;
    bi.Height := Height;
  end
  else if Texture.Image is TgxMultisampleImage then
  begin
    mi := TgxMultisampleImage(Texture.Image);
    mi.Width := Width;
    mi.Height := Height;
  end;
end;

function TgxFBORenderer.GetViewport: TRectangle;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Width := Width;
  Result.Height := Height;
end;

procedure TgxFBORenderer.Initialize;

  procedure AddOneMultiTarget(colorTex: TgxTexture);
  begin
    if ForceTextureDimensions then
      ForceDimensions(colorTex);
    if FColorAttachment >= FMaxAttachment then
    begin
      ShowMessage('Number of color attachments out of GL_MAX_COLOR_ATTACHMENTS');
      Visible := False;
      Abort;
    end;
    FFbo.AttachTexture(FColorAttachment, colorTex);
    Inc(FColorAttachment);
  end;

const
  cDrawBuffers: array [0 .. 15] of GLenum = (GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1, GL_COLOR_ATTACHMENT2,
    GL_COLOR_ATTACHMENT3, GL_COLOR_ATTACHMENT4, GL_COLOR_ATTACHMENT5, GL_COLOR_ATTACHMENT6, GL_COLOR_ATTACHMENT7,
    GL_COLOR_ATTACHMENT8, GL_COLOR_ATTACHMENT9, GL_COLOR_ATTACHMENT10, GL_COLOR_ATTACHMENT11, GL_COLOR_ATTACHMENT12,
    GL_COLOR_ATTACHMENT13, GL_COLOR_ATTACHMENT14, GL_COLOR_ATTACHMENT15);
var
  colorTex: TgxTexture;
  depthTex: TgxTexture;
  I: Integer;
  MulTexture: TgxTextureArray;
begin
  for I := 0 to MaxColorAttachments - 1 do
    FFbo.DetachTexture(I);

  if FMaxSize = 0 then
    glGetIntegerv(GL_MAX_RENDERBUFFER_SIZE, @FMaxSize);
  if Width > FMaxSize then
  begin
    FWidth := FMaxSize;
    ShowMessage(Format('%s.Width out of GL_MAX_RENDERBUFFER_SIZE', [Name]));
  end;
  if Height > FMaxSize then
  begin
    FHeight := FMaxSize;
    ShowMessage(Format('%s.Height out of GL_MAX_RENDERBUFFER_SIZE', [Name]));
  end;

  FFbo.Width := Width;
  FFbo.Height := Height;

  FFbo.Bind;
  DoPreInitialize;
  FFbo.Unbind;

  if Assigned(FMaterialLibrary) then
  begin
    colorTex := FMaterialLibrary.TextureByName(ColorTextureName);
    depthTex := FMaterialLibrary.TextureByName(DepthTextureName);
  end
  else
  begin
    colorTex := nil;
    depthTex := nil;
  end;

  FHasColor := False;
  FHasDepth := False;
  FHasStencil := False;
  FColorAttachment := 0;

  if FUseLibraryAsMultiTarget or Assigned(FOnSetTextureTargets) then
  begin
    if FMaxAttachment = 0 then
      glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, @FMaxAttachment);

    if Assigned(FOnSetTextureTargets) then
    begin
      FOnSetTextureTargets(Self, MulTexture);
      for I := 0 to High(MulTexture) do
      begin
        colorTex := MulTexture[I];
        // Skip depth texture
        if colorTex = depthTex then
          Continue;
        AddOneMultiTarget(colorTex);
      end;
    end
    else
      // Multicolor attachments
      for I := 0 to FMaterialLibrary.Materials.Count - 1 do
      begin
        colorTex := FMaterialLibrary.Materials[I].Material.Texture;
        // Skip depth texture
        if colorTex = depthTex then
          Continue;
        AddOneMultiTarget(colorTex);
      end;
    FHasColor := FColorAttachment > 0;
  end
  else
  begin
    // One color attachment
    if Assigned(colorTex) then
    begin
      if ForceTextureDimensions then
        ForceDimensions(colorTex);
      FFbo.AttachTexture(0, colorTex);
      Inc(FColorAttachment);
      FHasColor := True;
    end;
  end;

  if Assigned(depthTex) then
  begin
    if ForceTextureDimensions then
      ForceDimensions(depthTex);
    FFbo.AttachDepthTexture(depthTex);
    FDepthRBO.Free;
    FDepthRBO := nil;
    FHasDepth := True;
    FHasStencil := depthTex.TextureFormatEx = tfDEPTH24_STENCIL8;
  end
  else if erbDepth in EnabledRenderBuffers then
  begin
    if not Assigned(FDepthRBO) then
      FDepthRBO := TgxDepthRBO.Create;

    FDepthRBO.Width := Width;
    FDepthRBO.Height := Height;

    FFbo.AttachDepthBuffer(FDepthRBO);
    FHasDepth := True;
  end
  else
  begin
    FFbo.DetachDepthBuffer;
    if Assigned(FDepthRBO) then
    begin
      FDepthRBO.Free;
      FDepthRBO := nil;
    end;
  end;

  if erbStencil in EnabledRenderBuffers then
  begin
    if not Assigned(FStencilRBO) then
      FStencilRBO := TgxStencilRBO.Create;

    FStencilRBO.StencilPrecision := FStencilPrecision;
    FStencilRBO.Width := Width;
    FStencilRBO.Height := Height;

    FFbo.AttachStencilBuffer(FStencilRBO);
    FHasStencil := True;
  end
  else
  begin
    if not FHasStencil then
      FFbo.DetachStencilBuffer;
    if Assigned(FStencilRBO) then
    begin
      FStencilRBO.Free;
      FStencilRBO := nil;
    end;
  end;
  FFbo.Bind;

  if FColorAttachment = 0 then
  begin
    glDrawBuffer(GL_NONE);
    glReadBuffer(GL_NONE);
  end
  else
    glDrawBuffers(FColorAttachment, @cDrawBuffers);

  DoPostInitialize;
  FFbo.Unbind;

  /// CheckOpenGLError;
  ClearStructureChanged;
end;

procedure TgxFBORenderer.RenderToFBO(var ARci: TgxRenderContextInfo);

  function GetClearBits: cardinal;
  begin
    Result := 0;
    if HasColor and (coColorBufferClear in FClearOptions) then
      Result := Result or GL_COLOR_BUFFER_BIT;
    if HasDepth and (coDepthBufferClear in FClearOptions) then
      Result := Result or GL_DEPTH_BUFFER_BIT;
    if HasStencil and (coStencilBufferClear in FClearOptions) then
      Result := Result or GL_STENCIL_BUFFER_BIT;
  end;

type
  TgxStoredStates = record
    ColorClearValue: TgxColorVector;
    ColorWriteMask: TgxColorMask;
    Tests: TgxStates;
  end;

  function StoreStates: TgxStoredStates;
  begin
    Result.ColorClearValue := ARci.gxStates.ColorClearValue;
    Result.ColorWriteMask := ARci.gxStates.ColorWriteMask[0];
    Result.Tests := [stDepthTest, stStencilTest] * ARci.gxStates.States;
  end;

  procedure RestoreStates(const aStates: TgxStoredStates);
  begin
    ARci.gxStates.ColorClearValue := aStates.ColorClearValue;
    ARci.gxStates.SetColorMask(aStates.ColorWriteMask);
    if stDepthTest in aStates.Tests then
      ARci.gxStates.Enable(stDepthTest)
    else
      ARci.gxStates.Disable(stDepthTest);

    if stStencilTest in aStates.Tests then
      ARci.gxStates.Enable(stStencilTest)
    else
      ARci.gxStates.Disable(stStencilTest);
  end;

var
  backColor: TgxColorVector;
  buffer: TgxSceneBuffer;
  savedStates: TgxStoredStates;
  w, h: Integer;
  s: string;
begin
  if (ARci.drawState = dsPicking) and not PickableTarget then
    exit;

  if TgxFramebufferHandle.IsSupported = True then
  begin
    ShowMessage('Framebuffer not supported - deactivated');
    Active := False;
    exit;
  end;

  // prevent recursion
  if FRendering then
    exit;

  FRendering := True;
  if (ocStructure in Changes) or Assigned(FOnSetTextureTargets) then
  begin
    Initialize;
    if not Active then
      exit;
  end;

  ApplyCamera(ARci);

  try
    savedStates := StoreStates;

    FFbo.Bind;
    if FFbo.GetStringStatus(s) <> fsComplete then
    begin
      ShowMessage(Format('Framebuffer error: %s. Deactivated', [s]));
      Active := False;
      exit;
    end;

    DoBeforeRender(ARci);
    if Assigned(Camera) then
      Camera.Scene.SetupLights(ARci.gxStates.MaxLights);

    w := Width;
    h := Height;
    if FFbo.Level > 0 then
    begin
      w := w shr FFbo.Level;
      h := h shr FFbo.Level;
      if w = 0 then
        w := 1;
      if h = 0 then
        h := 1;
    end;
    ARci.gxStates.Viewport := Vector4iMake(0, 0, w, h);
    buffer := ARci.buffer as TgxSceneBuffer;

    if HasColor then
      ARci.gxStates.SetColorMask(cAllColorComponents)
    else
      ARci.gxStates.SetColorMask([]);

    ARci.gxStates.DepthWriteMask := HasDepth;

    if HasStencil then
      ARci.gxStates.Enable(stStencilTest)
    else
      ARci.gxStates.Disable(stStencilTest);

    if coUseBufferBackground in FClearOptions then
    begin
      backColor := ConvertWinColor(buffer.BackgroundColor);
      backColor.w := buffer.BackgroundAlpha;
      ARci.gxStates.ColorClearValue := backColor;
    end
    else
    begin
      ARci.gxStates.ColorClearValue := FBackgroundColor.Color;
    end;

    glClear(GetClearBits);

    FFbo.PreRender;
    // render to fbo
    if Assigned(RootObject) then
    begin
      // if object should only be rendered to the fbo
      // ensure it's visible before rendering to fbo
      if TargetVisibility = tvFBOOnly then
        RootObject.Visible := True;
      RootObject.Render(ARci);
      // then make it invisible afterwards
      if TargetVisibility = tvFBOOnly then
        RootObject.Visible := False;
    end
    else if (Count > 0) then
      RenderChildren(0, Count - 1, ARci);
    FFbo.PostRender(FPostGenerateMipmap);

    RestoreStates(savedStates);
    ARci.gxStates.Viewport := Vector4iMake(0, 0, ARci.viewPortSize.cx, ARci.viewPortSize.cy);
  finally
    FFbo.Unbind;
    FRendering := False;
    DoAfterRender(ARci);
    UnApplyCamera(ARci);
    if Assigned(Camera) then
      Camera.Scene.SetupLights(ARci.gxStates.MaxLights);
  end;
end;

procedure TgxFBORenderer.SetBackgroundColor(const Value: TgxColor);
begin
  FBackgroundColor.Assign(Value);
end;

procedure TgxFBORenderer.SetCamera(const Value: TgxCamera);
begin
  if FCamera <> Value then
  begin
    FCamera := Value;
    StructureChanged;
  end;
end;

procedure TgxFBORenderer.SetColorTextureName(const Value: TgxLibMaterialName);
begin
  if FColorTextureName <> Value then
  begin
    FColorTextureName := Value;
    StructureChanged;
  end;
end;

procedure TgxFBORenderer.SetDepthTextureName(const Value: TgxLibMaterialName);
begin
  if FDepthTextureName <> Value then
  begin
    FDepthTextureName := Value;
    StructureChanged;
  end;
end;

procedure TgxFBORenderer.SetEnabledRenderBuffers(const Value: TgxEnabledRenderBuffers);
begin
  if FEnabledRenderBuffers <> Value then
  begin
    FEnabledRenderBuffers := Value;
    StructureChanged;
  end;
end;

procedure TgxFBORenderer.SetForceTextureDimentions(const Value: Boolean);
begin
  if FForceTextureDimensions <> Value then
  begin
    FForceTextureDimensions := Value;
    StructureChanged;
  end;
end;

function TgxFBORenderer.GetMaterialLibrary: TgxAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TgxFBORenderer.SetMaterialLibrary(const Value: TgxAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if Value is TgxMaterialLibrary then
    begin
      FMaterialLibrary := TgxMaterialLibrary(Value);
      StructureChanged;
    end;
  end;
end;

procedure TgxFBORenderer.SetUseLibraryAsMultiTarget(Value: Boolean);
begin
  if FUseLibraryAsMultiTarget <> Value then
  begin
    FUseLibraryAsMultiTarget := Value;
    StructureChanged;
  end;
end;

procedure TgxFBORenderer.SetPostGenerateMipmap(const Value: Boolean);
begin
  if FPostGenerateMipmap <> Value then
    FPostGenerateMipmap := Value;
end;

procedure TgxFBORenderer.SetRootObject(const Value: TgxBaseSceneObject);
begin
  if FRootObject <> Value then
  begin
    if Assigned(FRootObject) then
      FRootObject.RemoveFreeNotification(Self);
    FRootObject := Value;
    if Assigned(FRootObject) then
      FRootObject.FreeNotification(Self);
    StructureChanged;
  end;
end;

procedure TgxFBORenderer.SetStencilPrecision(const Value: TgxStencilPrecision);
begin
  if FStencilPrecision <> Value then
  begin
    FStencilPrecision := Value;
    StructureChanged;
  end;
end;

procedure TgxFBORenderer.SetTargetVisibility(const Value: TgxFBOTargetVisibility);
begin
  if FTargetVisibility <> Value then
  begin
    if Assigned(RootObject) then
    begin
      if (TargetVisibility = tvFBOOnly) then
      begin
        // we went from fbo only, restore root's old visibility
        RootObject.Visible := FRootVisible;
      end
      else
      begin
        // we're going to fbo only, save root visibility for later
        FRootVisible := RootObject.Visible;
      end;
    end;

    FTargetVisibility := Value;
    StructureChanged;
  end;
end;

function TgxFBORenderer.StoreSceneScaleFactor: Boolean;
begin
  Result := (FSceneScaleFactor <> 0.0);
end;

function TgxFBORenderer.StoreAspect: Boolean;
begin
  Result := (FAspect <> 1.0);
end;

procedure TgxFBORenderer.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    StructureChanged;
  end;
end;

procedure TgxFBORenderer.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    StructureChanged;
  end;
end;

procedure TgxFBORenderer.SetLayer(const Value: Integer);
begin
  if Value <> FFbo.Layer then
  begin
    if FRendering or (ocStructure in Changes) then
      FFbo.Layer := Value
    else
    begin
      FFbo.Bind;
      FFbo.Layer := Value;
      FFbo.Unbind;
    end;
  end;
end;

function TgxFBORenderer.GetLayer: Integer;
begin
  Result := FFbo.Layer;
end;

procedure TgxFBORenderer.SetLevel(const Value: Integer);
var
  w, h: Integer;
begin
  if Value <> FFbo.Level then
  begin
    if FRendering or (ocStructure in Changes) then
    begin
      FFbo.Level := Value;
      w := Width;
      h := Height;
      if FFbo.Level > 0 then
      begin
        w := w shr FFbo.Level;
        h := h shr FFbo.Level;
        if w = 0 then
          w := 1;
        if h = 0 then
          h := 1;
        CurrentContext.gxStates.Viewport := Vector4iMake(0, 0, w, h);
      end;
    end
    else
    begin
      FFbo.Bind;
      FFbo.Level := Value;
      FFbo.Unbind;
    end;
  end;
end;

function TgxFBORenderer.GetLevel: Integer;
begin
  Result := FFbo.Level;
end;

//-------------------------------------------------------------------
initialization
//-------------------------------------------------------------------

RegisterClasses([TgxFBORenderer]);

end.
