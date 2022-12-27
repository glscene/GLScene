//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.FBORenderer;

(* Implements FBO support *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.Classes,
  System.SysUtils,
  
  GLS.OpenGLTokens,
  GLS.VectorGeometry,
  GLS.PersistentClasses,
  GLS.PipelineTransformation,
  GLS.Scene,
  GLS.Texture,
  GLS.Context,
  GLS.Color,
  GLS.Material,
  GLS.RenderContextInfo,
  GLS.State,
  GLS.TextureFormat,
  GLS.VectorTypes,
  GLS.MultiSampleImage,
  GLS.Logger;

const
  MaxColorAttachments = 32;

type
  TGLRenderbuffer = class
  private
    FRenderbufferHandle: TGLRenderbufferHandle;
    FWidth: Integer;
    FHeight: Integer;
    FStorageValid: Boolean;
    function GetHandle: Cardinal;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    function GetInternalFormat: cardinal; virtual; abstract;
    procedure InvalidateStorage;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Bind;
    procedure Unbind;
    (* Handle to the OpenGL render buffer object.
      If the handle hasn't already been allocated, it will be allocated
      by this call (ie. do not use if no OpenGL context is active!) *)
    property Handle: Cardinal read GetHandle;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  TGLDepthRBO = class(TGLRenderbuffer)
  private
    FDepthPrecision: TGLDepthPrecision;
    procedure SetDepthPrecision(const Value: TGLDepthPrecision);
  protected
    function GetInternalFormat: cardinal; override;
  public
    constructor Create;
    property DepthPrecision: TGLDepthPrecision read FDepthPrecision write
      SetDepthPrecision;
  end;

  TGLStencilPrecision = (spDefault, sp1bit, sp4bits, sp8bits, sp16bits);

  TGLStencilRBO = class(TGLRenderbuffer)
  private
    FStencilPrecision: TGLStencilPrecision;
    procedure SetStencilPrecision(const Value: TGLStencilPrecision);
  protected
    function GetInternalFormat: cardinal; override;
  public
    constructor Create;
    property StencilPrecision: TGLStencilPrecision read FStencilPrecision write
      SetStencilPrecision;
  end;

  TGLFrameBuffer = class
  private
    FFrameBufferHandle: TGLFramebufferHandle;
    FTarget: Cardinal;
    FWidth: Integer;
    FHeight: Integer;
    FLayer: Integer;
    FLevel: Integer;
    FTextureMipmap: cardinal;
    FAttachedTexture: array[0..MaxColorAttachments - 1] of TGLTexture;
    FDepthTexture: TGLTexture;
    FDRBO: TGLDepthRBO;
    FSRBO: TGLStencilRBO;
    function GetStatus: TGLFramebufferStatus;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetLayer(const Value: Integer);
    procedure SetLevel(const Value: Integer);
  protected
    procedure AttachTexture(
      const attachment: Cardinal;
      const textarget: Cardinal;
      const texture: Cardinal;
      const level: TGLint;
      const layer: TGLint); overload;
    procedure ReattachTextures;
  public
    constructor Create;
    destructor Destroy; override;
    // attaches a depth rbo to the fbo
    // the depth buffer must have the same dimentions as the fbo
    procedure AttachDepthBuffer(DepthBuffer: TGLDepthRBO); overload;
    // detaches depth attachment from the fbo
    procedure DetachDepthBuffer;
    // attaches a stencil rbo to the fbo
    // the stencil buffer must have the same dimentions as the fbo
    procedure AttachStencilBuffer(StencilBuffer: TGLStencilRBO); overload;
    // detaches stencil attachment from the fbo
    procedure DetachStencilBuffer;
    // attaches a depth texture to the fbo
    // the depth texture must have the same dimentions as the fbo
    procedure AttachDepthTexture(Texture: TGLTexture); overload;
    procedure DetachDepthTexture;
    procedure AttachTexture(n: Cardinal; Texture: TGLTexture); overload;
    procedure DetachTexture(n: Cardinal);
    function GetStringStatus(out clarification: string): TGLFramebufferStatus;
    property Status: TGLFramebufferStatus read GetStatus;
    procedure Bind;
    procedure Unbind;
    procedure PreRender;
    procedure Render(var rci: TGLRenderContextInfo; baseObject: TGLBaseSceneObject);
    procedure PostRender(const PostGenerateMipmap: Boolean);
    property Handle: TGLFramebufferHandle read FFrameBufferHandle;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Layer: Integer read FLayer write SetLayer;
    property Level: Integer read FLevel write SetLevel;
  end;

//----------------------- GLS.FBORenderer classes --------------------------

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
    FStoreCamera: array[0..2] of TGLVector;
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


//---------------------------------------
//---------- TGLRenderbuffer
//---------------------------------------

constructor TGLRenderbuffer.Create;
begin
  inherited Create;
  FRenderbufferHandle := TGLRenderbufferHandle.Create;
  FWidth := 256;
  FHeight := 256;
end;

destructor TGLRenderbuffer.Destroy;
begin
  FRenderbufferHandle.DestroyHandle;
  FRenderbufferHandle.Free;
  inherited Destroy;
end;

function TGLRenderbuffer.GetHandle: Cardinal;
begin
  if FRenderbufferHandle.Handle = 0 then
    FRenderbufferHandle.AllocateHandle;
  Result := FRenderbufferHandle.Handle;
end;

procedure TGLRenderbuffer.InvalidateStorage;
begin
  FStorageValid := False;
end;

procedure TGLRenderbuffer.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    InvalidateStorage;
  end;
end;

procedure TGLRenderbuffer.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    InvalidateStorage;
  end;
end;

procedure TGLRenderbuffer.Bind;
var
  internalFormat: cardinal;
begin
  FRenderbufferHandle.AllocateHandle;
  FRenderbufferHandle.Bind;
  if not FStorageValid then
  begin
    internalFormat := GetInternalFormat;
    FRenderbufferHandle.SetStorage(internalFormat, FWidth, FHeight);
  end;
end;

procedure TGLRenderbuffer.Unbind;
begin
  FRenderbufferHandle.UnBind;
end;

//---------------------------------
//----------- TGLDepthRBO
//---------------------------------
constructor TGLDepthRBO.Create;
begin
  inherited Create;
  FDepthPrecision := dpDefault;
end;

function TGLDepthRBO.GetInternalFormat: cardinal;
begin
  case DepthPrecision of
    dp24bits: Result := GL_DEPTH_COMPONENT24;
    dp16bits: Result := GL_DEPTH_COMPONENT16;
    dp32bits: Result := GL_DEPTH_COMPONENT32;
  else
    // dpDefault
    Result := GL_DEPTH_COMPONENT24_ARB;
  end;
end;

procedure TGLDepthRBO.SetDepthPrecision(const Value: TGLDepthPrecision);
begin
  if FDepthPrecision <> Value then
  begin
    FDepthPrecision := Value;
    InvalidateStorage;
  end;
end;

//-----------------------------------------
//------------- TGLStencilRBO
//-----------------------------------------
constructor TGLStencilRBO.Create;
begin
  inherited Create;
  FStencilPrecision := spDefault;
end;

function TGLStencilRBO.GetInternalFormat: cardinal;
begin
  case StencilPrecision of
    spDefault: Result := GL_STENCIL_INDEX;
    sp1bit: Result := GL_STENCIL_INDEX1_EXT;
    sp4bits: Result := GL_STENCIL_INDEX4_EXT;
    sp8bits: Result := GL_STENCIL_INDEX8_EXT;
    sp16bits: Result := GL_STENCIL_INDEX16_EXT;
  else
    // spDefault
    Result := GL_STENCIL_INDEX;
  end;
end;

procedure TGLStencilRBO.SetStencilPrecision(const Value: TGLStencilPrecision);
begin
  if FStencilPrecision <> Value then
  begin
    FStencilPrecision := Value;
    InvalidateStorage;
  end;
end;

//-----------------------------------------
//--------------- TGLFrameBuffer
//-----------------------------------------
constructor TGLFrameBuffer.Create;
begin
  inherited;
  FFrameBufferHandle := TGLFrameBufferHandle.Create;
  FWidth := 256;
  FHeight := 256;
  FLayer := 0;
  FLevel := 0;
  FTextureMipmap := 0;
  FTarget := GL_FRAMEBUFFER;
end;

destructor TGLFrameBuffer.Destroy;
begin
  FFrameBufferHandle.DestroyHandle;
  FFrameBufferHandle.Free;
  inherited Destroy;
end;

procedure TGLFrameBuffer.AttachTexture(n: Cardinal; Texture: TGLTexture);
var
  textarget: TGLTextureTarget;
begin
  Assert(n < MaxColorAttachments);
  Texture.Handle;
  FAttachedTexture[n] := Texture;
  textarget := Texture.Image.NativeTextureTarget;
  // Store mipmaping requires
  if not ((Texture.MinFilter in [miNearest, miLinear])
    or (textarget = ttTextureRect)) then
    FTextureMipmap := FTextureMipmap or (1 shl n);

  if Texture.Image is TGLMultiSampleImage then
    FTextureMipmap := 0;

  AttachTexture(
    GL_COLOR_ATTACHMENT0_EXT + n,
    DecodeTextureTarget(textarget),
    Texture.Handle,
    FLevel, FLayer);
end;

procedure TGLFrameBuffer.AttachDepthBuffer(DepthBuffer: TGLDepthRBO);

  procedure AttachDepthRB;
  begin
    // forces initialization
    DepthBuffer.Bind;
    DepthBuffer.Unbind;
    gl.FramebufferRenderbuffer(FTarget, GL_DEPTH_ATTACHMENT_EXT,
      GL_RENDERBUFFER_EXT, DepthBuffer.Handle);
  end;
var
  dp: TGLDepthPrecision;
begin
  if Assigned(FDRBO) then
    DetachDepthBuffer;
  FDRBO := DepthBuffer;
  Bind;
  AttachDepthRB;
  // if default format didn't work, try something else
  // crude, but might work
  if (Status = fsUnsupported) and (DepthBuffer.DepthPrecision = dpDefault) then
  begin
    // try the other formats
    // best quality first
    for dp := high(dp) downto low(dp) do
    begin
      if dp = dpDefault then
        Continue;
      DepthBuffer.DepthPrecision := dp;
      AttachDepthRB;
      if not (Status = fsUnsupported) then
        Break;
    end;
  end;
  Status;
  Unbind;
end;

procedure TGLFrameBuffer.AttachDepthTexture(Texture: TGLTexture);
begin
  FDepthTexture := Texture;

  if FDepthTexture.Image is TGLMultisampleImage then
  begin
    if not IsDepthFormat(FDepthTexture.TextureFormatEx) then
    begin
      // Force texture properties to depth compatibility
      FDepthTexture.TextureFormatEx := tfDEPTH_COMPONENT24;
      TGLMultisampleImage(FDepthTexture.Image).Width := Width;
      TGLMultisampleImage(FDepthTexture.Image).Height := Height;
    end;
    FTextureMipmap := 0;
  end
  else
  begin
    if not IsDepthFormat(FDepthTexture.TextureFormatEx) then
    begin
      // Force texture properties to depth compatibility
      FDepthTexture.ImageClassName := TGLBlankImage.ClassName;
      FDepthTexture.TextureFormatEx := tfDEPTH_COMPONENT24;
      TGLBlankImage(FDepthTexture.Image).Width := Width;
      TGLBlankImage(FDepthTexture.Image).Height := Height;
    end;
    if FDepthTexture.TextureFormatEx = tfDEPTH24_STENCIL8 then
    begin
      TGLBlankImage(FDepthTexture.Image).GetBitmap32.SetColorFormatDataType(GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8);
      TGLBlankImage(FDepthTexture.Image).ColorFormat := GL_DEPTH_STENCIL;
    end
    else
    begin
      TGLBlankImage(FDepthTexture.Image).GetBitmap32.SetColorFormatDataType(GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE);
      TGLBlankImage(FDepthTexture.Image).ColorFormat := GL_DEPTH_COMPONENT;
    end;
    // Depth texture mipmaping
    if not ((FDepthTexture.MinFilter in [miNearest, miLinear])) then
      FTextureMipmap := FTextureMipmap or Cardinal(1 shl MaxColorAttachments);
  end;
  AttachTexture(
    GL_DEPTH_ATTACHMENT,
    DecodeTextureTarget(FDepthTexture.Image.NativeTextureTarget),
    FDepthTexture.Handle,
    FLevel,
    FLayer);
  if FDepthTexture.TextureFormatEx = tfDEPTH24_STENCIL8 then
    AttachTexture(
      GL_STENCIL_ATTACHMENT,
      DecodeTextureTarget(FDepthTexture.Image.NativeTextureTarget),
      FDepthTexture.Handle,
      FLevel,
      FLayer);
end;

procedure TGLFrameBuffer.DetachDepthTexture;
begin
  if Assigned(FDepthTexture) then
  begin
    FTextureMipmap := FTextureMipmap and (not (1 shl MaxColorAttachments));
    AttachTexture(
      GL_DEPTH_ATTACHMENT,
      DecodeTextureTarget(FDepthTexture.Image.NativeTextureTarget),
      0, 0, 0);
    FDepthTexture := nil;
  end;
end;

procedure TGLFrameBuffer.AttachStencilBuffer(StencilBuffer: TGLStencilRBO);

  procedure AttachStencilRB;
  begin
    // forces initialization
    StencilBuffer.Bind;
    StencilBuffer.Unbind;
    gl.FramebufferRenderbuffer(FTarget, GL_STENCIL_ATTACHMENT,
      GL_RENDERBUFFER_EXT, StencilBuffer.Handle);
  end;

var
  sp: TGLStencilPrecision;
begin
  if Assigned(FSRBO) then
    DetachStencilBuffer;
  FSRBO := StencilBuffer;

  Bind;
  AttachStencilRB;

  // if default format didn't work, try something else
  // crude, but might work
  if (Status = fsUnsupported)
    and (StencilBuffer.StencilPrecision = spDefault) then
  begin
    // try the other formats
    // best quality first
    for sp := high(sp) downto low(sp) do
    begin
      if sp = spDefault then
        Continue;
      StencilBuffer.StencilPrecision := sp;
      AttachStencilRB;
      if not (Status = fsUnsupported) then
        Break;
    end;
  end;
  Status;
  Unbind;
end;

procedure TGLFrameBuffer.AttachTexture(
  const attachment: Cardinal;
  const textarget: Cardinal;
  const texture: Cardinal;
  const level: TGLint;
  const layer: TGLint);
var
  storeDFB: Cardinal;
  RC: TGLContext;
begin
  RC := SafeCurrentGLContext;
  storeDFB := RC.GLStates.DrawFrameBuffer;
  if storeDFB <> FFrameBufferHandle.Handle then
    Bind;
  with FFrameBufferHandle do
    case textarget of
      GL_TEXTURE_1D:
        Attach1DTexture(FTarget, attachment, textarget, texture, level);
      GL_TEXTURE_2D:
        Attach2DTexture(FTarget, attachment, textarget, texture, level);
      GL_TEXTURE_RECTANGLE: // Rectangle texture can't be leveled
        Attach2DTexture(FTarget, attachment, textarget, texture, 0);
      GL_TEXTURE_3D:
        Attach3DTexture(FTarget, attachment, textarget, texture, level, layer);
      GL_TEXTURE_CUBE_MAP:
        Attach2DTexture(FTarget, attachment, GL_TEXTURE_CUBE_MAP_POSITIVE_X + layer, texture, level);
      GL_TEXTURE_CUBE_MAP_POSITIVE_X,
        GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
        GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
        GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
        GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
        GL_TEXTURE_CUBE_MAP_NEGATIVE_Z:
          Attach2DTexture(FTarget, attachment, textarget, texture, level);
      GL_TEXTURE_CUBE_MAP_ARRAY,
        GL_TEXTURE_1D_ARRAY,
        GL_TEXTURE_2D_ARRAY:
        AttachLayer(FTarget, attachment, texture, level, layer);
      GL_TEXTURE_2D_MULTISAMPLE: // Multisample texture can't be leveled
        Attach2DTexture(FTarget, attachment, textarget, texture, 0);
      GL_TEXTURE_2D_MULTISAMPLE_ARRAY:
        AttachLayer(FTarget, attachment, texture, 0, layer);
    end;
  if storeDFB <> FFrameBufferHandle.Handle then
    RC.GLStates.SetFrameBuffer(storeDFB);
end;

procedure TGLFrameBuffer.Bind;
begin
  if Handle.IsDataNeedUpdate then
    ReattachTextures
  else
    Handle.Bind;
end;

procedure TGLFrameBuffer.Unbind;
begin
  FFrameBufferHandle.UnBind;
end;

procedure TGLFrameBuffer.DetachTexture(n: Cardinal);
begin
  // textarget ignored when binding 0
  if Assigned(FAttachedTexture[n]) then
  begin
    Bind;
    AttachTexture(
      GL_COLOR_ATTACHMENT0 + n,
      GL_TEXTURE_2D, // target does not matter
      0, 0, 0);

    FTextureMipmap := FTextureMipmap and (not (1 shl n));
    FAttachedTexture[n] := nil;
    Unbind;
  end;
end;

procedure TGLFrameBuffer.DetachDepthBuffer;
begin
  Bind;
  gl.FramebufferRenderbuffer(FTarget, GL_DEPTH_ATTACHMENT,
    GL_RENDERBUFFER, 0);
  Unbind;
  FDRBO := nil;
end;

procedure TGLFrameBuffer.DetachStencilBuffer;
begin
  Bind;
  gl.FramebufferRenderbuffer(FTarget, GL_STENCIL_ATTACHMENT,
    GL_RENDERBUFFER, 0);
  Unbind;
  FSRBO := nil;
end;

function TGLFrameBuffer.GetStatus: TGLFramebufferStatus;
var
  status: cardinal;
begin
  status := gl.CheckFramebufferStatus(FTarget);

  case status of
    GL_FRAMEBUFFER_COMPLETE_EXT: Result := fsComplete;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT: Result := fsIncompleteAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT: Result :=
      fsIncompleteMissingAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT: Result :=
      fsIncompleteDuplicateAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT: Result := fsIncompleteDimensions;
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT: Result := fsIncompleteFormats;
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT: Result := fsIncompleteDrawBuffer;
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT: Result := fsIncompleteReadBuffer;
    GL_FRAMEBUFFER_UNSUPPORTED_EXT: Result := fsUnsupported;
    GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE: Result := fsIncompleteMultisample;
  else
    Result := fsStatusError;
  end;
end;

function TGLFrameBuffer.GetStringStatus(out clarification: string):
  TGLFramebufferStatus;
const
  cFBOStatus: array[TGLFramebufferStatus] of string = (
    'Complete',
    'Incomplete attachment',
    'Incomplete missing attachment',
    'Incomplete duplicate attachment',
    'Incomplete dimensions',
    'Incomplete formats',
    'Incomplete draw buffer',
    'Incomplete read buffer',
    'Unsupported',
    'Incomplite multisample',
    'Status Error');
begin
  Result := GetStatus;
  clarification := cFBOStatus[Result];
end;

procedure TGLFrameBuffer.PostRender(const PostGenerateMipmap: Boolean);
var
  n: Integer;
  textarget: TGLTextureTarget;
begin
  if (FTextureMipmap > 0) and PostGenerateMipmap then
  begin
    for n := 0 to MaxColorAttachments - 1 do
      if Assigned(FAttachedTexture[n]) then
      begin
        if FTextureMipmap and (1 shl n) = 0 then
          Continue;
        textarget := FAttachedTexture[n].Image.NativeTextureTarget;
        with FFrameBufferHandle.RenderingContext.GLStates do
          TextureBinding[ActiveTexture, textarget] :=
            FAttachedTexture[n].Handle;
        gl.GenerateMipmap(DecodeTextureTarget(textarget));
      end;
  end;
end;

procedure TGLFrameBuffer.PreRender;
begin

end;

procedure TGLFrameBuffer.Render(var rci: TGLRenderContextInfo; baseObject:
  TGLBaseSceneObject);
var
  backColor: TGLColorVector;
  buffer: TGLSceneBuffer;
begin
  Bind;
  Assert(Status = fsComplete, 'Framebuffer not complete');

  buffer := TGLSceneBuffer(rci.buffer);

  backColor := ConvertWinColor(buffer.BackgroundColor);
  gl.ClearColor(backColor.X, backColor.Y, backColor.Z,
    buffer.BackgroundAlpha);
  rci.GLStates.SetColorMask(cAllColorComponents);
  rci.GLStates.DepthWriteMask := True;
  gl.Clear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  baseObject.Render(rci);
  Unbind;
end;

procedure TGLFrameBuffer.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
  end;
end;

procedure TGLFrameBuffer.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
  end;
end;

procedure TGLFrameBuffer.ReattachTextures;
var
  n: Integer;
  bEmpty: Boolean;
  s: String;
begin
  Handle.AllocateHandle;
  Handle.Bind;
  // Reattach layered textures
  bEmpty := True;

  for n := 0 to MaxColorAttachments - 1 do
    if Assigned(FAttachedTexture[n]) then
    begin
      AttachTexture(
        GL_COLOR_ATTACHMENT0_EXT + n,
        DecodeTextureTarget(FAttachedTexture[n].Image.NativeTextureTarget),
        FAttachedTexture[n].Handle,
        FLevel,
        FLayer);
      bEmpty := False;
    end;

  if Assigned(FDepthTexture) then
  begin
    AttachTexture(
      GL_DEPTH_ATTACHMENT,
      DecodeTextureTarget(FDepthTexture.Image.NativeTextureTarget),
      FDepthTexture.Handle,
      FLevel,
      FLayer);
    bEmpty := False;
  end;

  if Assigned(FDRBO) then
  begin
    FDRBO.Bind;
    FDRBO.Unbind;
    gl.FramebufferRenderbuffer(FTarget, GL_DEPTH_ATTACHMENT_EXT,
      GL_RENDERBUFFER_EXT, FDRBO.Handle);
    bEmpty := False;
  end;

  if Assigned(FSRBO) then
  begin
    FSRBO.Bind;
    FSRBO.Unbind;
    gl.FramebufferRenderbuffer(FTarget, GL_STENCIL_ATTACHMENT,
      GL_RENDERBUFFER_EXT, FSRBO.Handle);
    bEmpty := False;
  end;
  if not bEmpty and (GetStringStatus(s) <> fsComplete) then
    GLSLogger.LogErrorFmt('Framebuffer error: %s. Deactivated', [s]);
  Handle.NotifyDataUpdated;
end;

procedure TGLFrameBuffer.SetLayer(const Value: Integer);
var
  RC: TGLContext;
begin
  if FLayer <> Value then
  begin
    FLayer := Value;
    RC := CurrentGLContext;
    if Assigned(RC) then
    begin
      if RC.GLStates.DrawFrameBuffer = FFrameBufferHandle.Handle then
        ReattachTextures;
    end;
  end;
end;

procedure TGLFrameBuffer.SetLevel(const Value: Integer);
var
  RC: TGLContext;
begin
  if FLevel <> Value then
  begin
    FLevel := Value;
    RC := CurrentGLContext;
    if Assigned(RC) then
    begin
      if RC.GLStates.DrawFrameBuffer = FFrameBufferHandle.Handle then
        ReattachTextures;
    end;
  end;
end;


//
// ------------------- TGLFBORenderer --------------------------
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
    ColorClearValue: TGLColorVector;
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
  backColor: TGLColorVector;
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
