//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLFBORenderer;

(* Implements FBO support *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  
  OpenGLTokens,
  Scene.VectorGeometry,
  Scene.PersistentClasses,
  GLPipelineTransformation,
  GLScene,
  GLTexture,
  GLContext,
  GLColor,
  GLMaterial,
  GLRenderContextInfo,
  GLState,
  GLTextureFormat,
  Scene.VectorTypes,
  GLMultisampleImage,
  GLFBO,
  Scene.Logger;

type
  TGLEnabledRenderBuffer = (erbDepth, erbStencil);
  TGLEnabledRenderBuffers = set of TGLEnabledRenderBuffer;

  TGLFBOTargetVisibility = (tvDefault, tvFBOOnly);

  TGLFBOClearOption = (coColorBufferClear, coDepthBufferClear,
    coStencilBufferClear, coUseBufferBackground);
  TGLFBOClearOptions = set of TGLFBOClearOption;

  TGLTextureArray = array of TGLTexture;

  TSetTextureTargetsEvent = procedure(Sender : TObject;
    var colorTexs : TGLTextureArray) of object;

  TGLFBORenderer = class(TGLBaseSceneObject, IGLMaterialLibrarySupported)
  private
    FFbo: TGLFrameBuffer;
    FDepthRBO: TGLDepthRBO;
    FStencilRBO: TGLStencilRBO;
    FColorAttachment: Integer;
    FRendering: Boolean;
    FHasColor: Boolean;
    FHasDepth: Boolean;
    FHasStencil: Boolean;
    FMaterialLibrary: TGLMaterialLibrary;
    FColorTextureName: TGLLibMaterialName;
    FDepthTextureName: TGLLibMaterialName;
    FWidth: Integer;
    FHeight: Integer;
    FForceTextureDimensions: Boolean;
    FStencilPrecision: TGLStencilPrecision;
    FRootObject: TGLBaseSceneObject;
    FRootVisible: Boolean;
    FCamera: TGLCamera;
    FEnabledRenderBuffers: TGLEnabledRenderBuffers;
    FTargetVisibility: TGLFBOTargetVisibility;
    FBeforeRender: TGLDirectRenderEvent;
    FPostInitialize: TNotifyEvent;
    FAfterRender: TGLDirectRenderEvent;
    FPreInitialize: TNotifyEvent;
    FBackgroundColor: TGLColor;
    FClearOptions: TGLFBOClearOptions;
    FAspect: Single;
    FSceneScaleFactor: Single;
    FUseLibraryAsMultiTarget: Boolean;
    FPostGenerateMipmap: Boolean;
    FMaxSize: Integer;
    FMaxAttachment: Integer;
    FStoreCamera: array[0..2] of TVector;
    FOnSetTextureTargets: TSetTextureTargetsEvent;
    // implementing IGLMaterialLibrarySupported
    function GetMaterialLibrary: TGLAbstractMaterialLibrary;
    procedure SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
    procedure SetDepthTextureName(const Value: TGLLibMaterialName);
    procedure SetColorTextureName(const Value: TGLLibMaterialName);
    procedure SetForceTextureDimentions(const Value: Boolean);
    procedure SetHeight(Value: Integer);
    procedure SetWidth(Value: Integer);
    procedure SetLayer(const Value: Integer);
    function GetLayer: Integer;
    procedure SetLevel(const Value: Integer);
    function GetLevel: Integer;
    procedure SetStencilPrecision(const Value: TGLStencilPrecision);
    procedure SetRootObject(const Value: TGLBaseSceneObject);
    function GetViewport: TRectangle;
    procedure SetCamera(const Value: TGLCamera);
    procedure SetEnabledRenderBuffers(const Value: TGLEnabledRenderBuffers);
    procedure SetTargetVisibility(const Value: TGLFBOTargetVisibility);
    procedure SetBackgroundColor(const Value: TGLColor);
    function StoreSceneScaleFactor: Boolean;
    function StoreAspect: Boolean;
    procedure SetUseLibraryAsMultiTarget(Value: Boolean);
    procedure SetPostGenerateMipmap(const Value: Boolean);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Initialize;
    procedure ForceDimensions(Texture: TGLTexture);
    procedure RenderToFBO(var ARci: TGLRenderContextInfo);
    procedure ApplyCamera(var ARci: TGLRenderContextInfo);
    procedure UnApplyCamera(var ARci: TGLRenderContextInfo);
    procedure DoBeforeRender(var ARci: TGLRenderContextInfo);
    procedure DoAfterRender(var ARci: TGLRenderContextInfo);
    procedure DoPreInitialize;
    procedure DoPostInitialize;
    property HasColor: Boolean read FHasColor;
    property HasDepth: Boolean read FHasDepth;
    property HasStencil: Boolean read FHasStencil;
    property Viewport: TRectangle read GetViewport;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoRender(var ARci: TGLRenderContextInfo; ARenderSelf: Boolean;
      ARenderChildren: Boolean); override;
    (*  Layer (also cube map face) is activated only on
      the volume textures, texture array and cube map.
      You can select the layer during the drawing to. *)
    property Layer: Integer read GetLayer write SetLayer;
    //  Mipmap Level where will be rendering
    property Level: Integer read GetLevel write SetLevel;
  published
    property Active: Boolean read GetVisible write SetVisible default True;
    property PickableTarget: Boolean read GetPickable write SetPickable default False;
    (* Force texture dimensions when initializing
      only works with TGLBlankImage and TGLFloatDataImage, otherwise does nothing *)
    property ForceTextureDimensions: Boolean read FForceTextureDimensions
      write SetForceTextureDimentions default True;
    property Width: Integer read FWidth write SetWidth default 256;
    property Height: Integer read FHeight write SetHeight default 256;
    property Aspect: Single read FAspect write FAspect stored StoreAspect;
    property ColorTextureName: TGLLibMaterialName read FColorTextureName
      write SetColorTextureName;
    property DepthTextureName: TGLLibMaterialName read FDepthTextureName
      write SetDepthTextureName;
    property MaterialLibrary: TGLAbstractMaterialLibrary read GetMaterialLibrary
      write SetMaterialLibrary;
    property BackgroundColor: TGLColor read FBackgroundColor write SetBackgroundColor;
    property ClearOptions: TGLFBOClearOptions read FClearOptions
      write FClearOptions;
    (* Camera used for rendering to the FBO
      if not assigned, use the active view's camera *)
    property Camera: TGLCamera read FCamera write SetCamera;
    (* Adjust the scene scale of the camera so that the rendering
      becomes independent of the width of the fbo renderer
      0 = disabled *)
    property SceneScaleFactor: Single read FSceneScaleFactor
      write FSceneScaleFactor stored StoreSceneScaleFactor;
    (* Root object used when rendering to the FBO
      if not assigned, uses itself as root and renders the child objects to the FBO *)
    property RootObject: TGLBaseSceneObject read FRootObject
      write SetRootObject;
    (* Determines if target is rendered to FBO only or rendered normally
      in FBO only mode, if RootObject is assigned, the RootObject's Visible flag is modified
      in default mode, if RootObject is not assigned, children are rendered normally after being
      rendered to the FBO *)
    property TargetVisibility: TGLFBOTargetVisibility read FTargetVisibility
      write SetTargetVisibility default tvDefault;
    //  Enables the use of a render buffer if a texture is not assigned
    property EnabledRenderBuffers: TGLEnabledRenderBuffers
      read FEnabledRenderBuffers write SetEnabledRenderBuffers;
    //  use stencil buffer
    property StencilPrecision: TGLStencilPrecision read FStencilPrecision
      write SetStencilPrecision default spDefault;
    //  called before rendering to the FBO
    property BeforeRender: TGLDirectRenderEvent read FBeforeRender write FBeforeRender;
    //  called after the rendering to the FBO
    property AfterRender: TGLDirectRenderEvent read FAfterRender write FAfterRender;
    (*  Called before the FBO is initialized
      the FBO is bound before calling this event *)
    property PreInitialize: TNotifyEvent read FPreInitialize write FPreInitialize;
    (*  Called after the FBO is initialized, but before any rendering
      the FBO is bound before calling this event *)
    property PostInitialize: TNotifyEvent read FPostInitialize write FPostInitialize;
    property UseLibraryAsMultiTarget: Boolean read FUseLibraryAsMultiTarget
      write SetUseLibraryAsMultiTarget default False;
    (*  Control mipmap generation after rendering
      texture must have MinFilter with mipmaping *)
    property PostGenerateMipmap: Boolean read FPostGenerateMipmap
      write SetPostGenerateMipmap default True;
    (* Allows multiTargeting to different texture sources instead of all coming
      from one single MatLib with UseLibraryAsMultiTarget. OnSetTextureTargets
      overrides the other method of setting target textures via the MaterialLibrary,
      ColorTextureName and DepthTextureName propertes *)
    property OnSetTextureTargets: TSetTextureTargetsEvent read FOnSetTextureTargets
      write FOnSetTextureTargets;
  end;

//======================================================================
implementation
//======================================================================

//
// TGLFBORenderer
//
procedure TGLFBORenderer.ApplyCamera(var ARci: TGLRenderContextInfo);
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
      SetViewMatrix(MatrixMultiply(ViewMatrix^,
        CreateScaleMatrix(Vector3fMake(1.0 / FAspect, 1.0, 1.0))));
    end;
  end;
end;

procedure TGLFBORenderer.UnApplyCamera(var ARci: TGLRenderContextInfo);
begin
  ARci.cameraPosition := FStoreCamera[0];
  ARci.cameraDirection := FStoreCamera[1];
  ARci.cameraUp := FStoreCamera[2];
  ARci.PipelineTransformation.Pop;
end;

constructor TGLFBORenderer.Create(AOwner: TComponent);
begin
  inherited;
  ObjectStyle := [osDirectDraw, osNoVisibilityCulling];
  FFbo := TGLFrameBuffer.Create;
  FBackgroundColor := TGLColor.Create(Self);
  FUseLibraryAsMultiTarget := False;
  FForceTextureDimensions := True;
  FWidth := 256;
  FHeight := 256;
  FEnabledRenderBuffers := [erbDepth];
  FClearOptions := [coColorBufferClear, coDepthBufferClear,
    coStencilBufferClear, coUseBufferBackground];
  PickableTarget := False;
  FAspect := 1.0;
  FSceneScaleFactor := 0.0;
  FPostGenerateMipmap := True;
  StructureChanged;
end;

destructor TGLFBORenderer.Destroy;
begin
  FFbo.Free;
  FDepthRBO.Free;
  FStencilRBO.Free;
  FBackgroundColor.Free;
  inherited;
end;

procedure TGLFBORenderer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FRootObject) and (Operation = opRemove) then
    FRootObject := nil;
end;

procedure TGLFBORenderer.DoAfterRender(var ARci: TGLRenderContextInfo);
begin
  if Assigned(FAfterRender) then
    FAfterRender(Self, ARci);
end;

procedure TGLFBORenderer.DoBeforeRender(var ARci: TGLRenderContextInfo);
begin
  if Assigned(FBeforeRender) then
    FBeforeRender(Self, ARci);
end;

procedure TGLFBORenderer.DoPostInitialize;
begin
  if Assigned(FPostInitialize) then
    FPostInitialize(Self);
end;

procedure TGLFBORenderer.DoPreInitialize;
begin
  if Assigned(FPreInitialize) then
    FPreInitialize(Self);
end;

procedure TGLFBORenderer.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: Boolean);
begin
  if not (csDesigning in ComponentState) then
    RenderToFBO(ARci);

  if (not Assigned(FRootObject)) and (TargetVisibility = tvDefault) and ARenderChildren 
  then
    RenderChildren(0, Count - 1, ARci);
end;

procedure TGLFBORenderer.ForceDimensions(Texture: TGLTexture);
var
  bi: TGLBlankImage;
  mi: TGLMultisampleImage;
begin
  if Texture.Image is TGLBlankImage then
  begin
    bi := TGLBlankImage(Texture.Image);
    bi.Width := Width;
    bi.Height := Height;
  end
  else if Texture.Image is TGLMultisampleImage then
  begin
    mi := TGLMultisampleImage(Texture.Image);
    mi.Width := Width;
    mi.Height := Height;
  end;
end;

function TGLFBORenderer.GetViewport: TRectangle;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Width := Width;
  Result.Height := Height;
end;

procedure TGLFBORenderer.Initialize;

  procedure AddOneMultiTarget(colorTex: TGLTexture);
  begin
    if ForceTextureDimensions then
      ForceDimensions(colorTex);
    if FColorAttachment >= FMaxAttachment then
    begin
      GLSLogger.LogError
        ('Number of color attachments out of GL_MAX_COLOR_ATTACHMENTS');
      Visible := False;
      Abort;
    end;
    FFbo.AttachTexture(FColorAttachment, colorTex);
    Inc(FColorAttachment);
  end;

const
  cDrawBuffers: array [0 .. 15] of Cardinal = (GL_COLOR_ATTACHMENT0,
    GL_COLOR_ATTACHMENT1, GL_COLOR_ATTACHMENT2,
    GL_COLOR_ATTACHMENT3, GL_COLOR_ATTACHMENT4,
    GL_COLOR_ATTACHMENT5, GL_COLOR_ATTACHMENT6,
    GL_COLOR_ATTACHMENT7, GL_COLOR_ATTACHMENT8,
    GL_COLOR_ATTACHMENT9, GL_COLOR_ATTACHMENT10,
    GL_COLOR_ATTACHMENT11, GL_COLOR_ATTACHMENT12,
    GL_COLOR_ATTACHMENT13, GL_COLOR_ATTACHMENT14,
    GL_COLOR_ATTACHMENT15);
var
  colorTex: TGLTexture;
  depthTex: TGLTexture;
  I: Integer;
  MulTexture : TGLTextureArray;
begin
  for I := 0 to MaxColorAttachments - 1 do
    FFbo.DetachTexture(I);

  if FMaxSize = 0 then
    gl.GetIntegerv(GL_MAX_RENDERBUFFER_SIZE, @FMaxSize);
  if Width > FMaxSize then
  begin
    FWidth := FMaxSize;
    GLSLogger.LogWarningFmt('%s.Width out of GL_MAX_RENDERBUFFER_SIZE', [Name]);
  end;
  if Height > FMaxSize then
  begin
    FHeight := FMaxSize;
    GLSLogger.LogWarningFmt('%s.Height out of GL_MAX_RENDERBUFFER_SIZE', [Name]);
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
    if not(gl.ARB_draw_buffers or gl.ATI_draw_buffers) then
    begin
      GLSLogger.LogError('Hardware do not support MRT');
      Active := False;
      exit;
    end;
    if FMaxAttachment = 0 then
      gl.GetIntegerv(GL_MAX_COLOR_ATTACHMENTS, @FMaxAttachment);

    if Assigned(FOnSetTextureTargets) then
    begin
      FOnSetTextureTargets(Self, MulTexture);
      for I := 0 to High(MulTexture) do
      begin
        colorTex := MulTexture[i];
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
      FDepthRBO := TGLDepthRBO.Create;

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
      FStencilRBO := TGLStencilRBO.Create;

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
    gl.DrawBuffer(GL_NONE);
    gl.ReadBuffer(GL_NONE);
  end
  else
    gl.DrawBuffers(FColorAttachment, @cDrawBuffers);

  DoPostInitialize;
  FFbo.Unbind;

  gl.CheckError;
  ClearStructureChanged;
end;

procedure TGLFBORenderer.RenderToFBO(var ARci: TGLRenderContextInfo);

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
  TGLStoredStates = record
    ColorClearValue: TColorVector;
    ColorWriteMask: TGLColorMask;
    Tests: TGLStates;
  end;

  function StoreStates: TGLStoredStates;
  begin
    Result.ColorClearValue := ARci.GLStates.ColorClearValue;
    Result.ColorWriteMask := ARci.GLStates.ColorWriteMask[0];
    Result.Tests := [stDepthTest, stStencilTest] * ARci.GLStates.States;
  end;

  procedure RestoreStates(const aStates: TGLStoredStates);
  begin
    ARci.GLStates.ColorClearValue := aStates.ColorClearValue;
    ARci.GLStates.SetColorMask(aStates.ColorWriteMask);
    if stDepthTest in aStates.Tests then
      ARci.GLStates.Enable(stDepthTest)
    else
      ARci.GLStates.Disable(stDepthTest);

    if stStencilTest in aStates.Tests then
      ARci.GLStates.Enable(stStencilTest)
    else
      ARci.GLStates.Disable(stStencilTest);
  end;

var
  backColor: TColorVector;
  buffer: TGLSceneBuffer;
  savedStates: TGLStoredStates;
  w, h: Integer;
  s: string;
begin
  if (ARci.drawState = dsPicking) and not PickableTarget then
    Exit;

  if not TGLFramebufferHandle.IsSupported then
  begin
    GLSLogger.LogError('Framebuffer not supported - deactivated');
    Active := False;
    Exit;
  end;

  // prevent recursion
  if FRendering then
    Exit;

  FRendering := True;
  if (ocStructure in Changes) or Assigned(FOnSetTextureTargets) then
  begin
    Initialize;
    if not Active then
      Exit;
  end;

  ApplyCamera(ARci);

  try
    savedStates := StoreStates;

    FFbo.Bind;
    if FFbo.GetStringStatus(s) <> fsComplete then
    begin
      GLSLogger.LogErrorFmt('Framebuffer error: %s. Deactivated', [s]);
      Active := False;
      Exit;
    end;

    DoBeforeRender(ARci);
    if Assigned(Camera) then
      Camera.Scene.SetupLights(ARci.GLStates.MaxLights);

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
    ARci.GLStates.Viewport := Vector4iMake(0, 0, w, h);
    buffer := ARci.buffer as TGLSceneBuffer;

    if HasColor then
      ARci.GLStates.SetColorMask(cAllColorComponents)
    else
      ARci.GLStates.SetColorMask([]);

    ARci.GLStates.DepthWriteMask := HasDepth;

    if HasStencil then
      ARci.GLStates.Enable(stStencilTest)
    else
      ARci.GLStates.Disable(stStencilTest);

    if coUseBufferBackground in FClearOptions then
    begin
      backColor := ConvertWinColor(buffer.BackgroundColor);
      backColor.W := buffer.BackgroundAlpha;
      ARci.GLStates.ColorClearValue := backColor;
    end
    else
    begin
      ARci.GLStates.ColorClearValue := FBackgroundColor.Color;
    end;

    gl.Clear(GetClearBits);

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
    ARci.GLStates.Viewport := Vector4iMake(0, 0, ARci.viewPortSize.cx,
      ARci.viewPortSize.cy);
  finally
    FFbo.Unbind;
    FRendering := False;
    DoAfterRender(ARci);
    UnApplyCamera(ARci);
    if Assigned(Camera) then
      Camera.Scene.SetupLights(ARci.GLStates.MaxLights);
  end;
end;

procedure TGLFBORenderer.SetBackgroundColor(const Value: TGLColor);
begin
  FBackgroundColor.Assign(Value);
end;

procedure TGLFBORenderer.SetCamera(const Value: TGLCamera);
begin
  if FCamera <> Value then
  begin
    FCamera := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetColorTextureName(const Value: TGLLibMaterialName);
begin
  if FColorTextureName <> Value then
  begin
    FColorTextureName := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetDepthTextureName(const Value: TGLLibMaterialName);
begin
  if FDepthTextureName <> Value then
  begin
    FDepthTextureName := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetEnabledRenderBuffers(const Value
  : TGLEnabledRenderBuffers);
begin
  if FEnabledRenderBuffers <> Value then
  begin
    FEnabledRenderBuffers := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetForceTextureDimentions(const Value: Boolean);
begin
  if FForceTextureDimensions <> Value then
  begin
    FForceTextureDimensions := Value;
    StructureChanged;
  end;
end;


function TGLFBORenderer.GetMaterialLibrary: TGLAbstractMaterialLibrary;
begin
  Result := FMaterialLibrary;
end;

procedure TGLFBORenderer.SetMaterialLibrary(const Value: TGLAbstractMaterialLibrary);
begin
  if FMaterialLibrary <> Value then
  begin
    if Value is TGLMaterialLibrary then
    begin
      FMaterialLibrary := TGLMaterialLibrary(Value);
      StructureChanged;
    end;
  end;
end;


procedure TGLFBORenderer.SetUseLibraryAsMultiTarget(Value: Boolean);
begin
  if FUseLibraryAsMultiTarget <> Value then
  begin
    FUseLibraryAsMultiTarget := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetPostGenerateMipmap(const Value: Boolean);
begin
  if FPostGenerateMipmap <> Value then
    FPostGenerateMipmap := Value;
end;

procedure TGLFBORenderer.SetRootObject(const Value: TGLBaseSceneObject);
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

procedure TGLFBORenderer.SetStencilPrecision(const Value: TGLStencilPrecision);
begin
  if FStencilPrecision <> Value then
  begin
    FStencilPrecision := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetTargetVisibility(const Value
  : TGLFBOTargetVisibility);
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

function TGLFBORenderer.StoreSceneScaleFactor: Boolean;
begin
  Result := (FSceneScaleFactor <> 0.0);
end;

function TGLFBORenderer.StoreAspect: Boolean;
begin
  Result := (FAspect <> 1.0);
end;

procedure TGLFBORenderer.SetWidth(Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetHeight(Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    StructureChanged;
  end;
end;

procedure TGLFBORenderer.SetLayer(const Value: Integer);
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

function TGLFBORenderer.GetLayer: Integer;
begin
  Result := FFbo.Layer;
end;

procedure TGLFBORenderer.SetLevel(const Value: Integer);
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
        CurrentGLContext.GLStates.Viewport := Vector4iMake(0, 0, w, h);
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

function TGLFBORenderer.GetLevel: Integer;
begin
  Result := FFbo.Level;
end;

//-------------------------------------------------
initialization
//-------------------------------------------------

RegisterClasses([TGLFBORenderer]);

end.
