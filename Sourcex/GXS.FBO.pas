//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FBO;

(* Implements FBO support *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,

  System.SysUtils,
  FMX.Dialogs,

  GXS.VectorTypes,
  GXS.Scene,
  GXS.Context,
  GXS.State,
  GXS.Texture,
  GXS.Color,
  GXS.RenderContextInfo,
  GXS.MultisampleImage,
  GXS.Graphics,
  GXS.TextureFormat;


const
  MaxColorAttachments = 32;

type
  TgxRenderbuffer = class
  private
    FRenderbufferHandle: TgxRenderbufferHandle;
    FWidth: Integer;
    FHeight: Integer;
    FStorageValid: Boolean;
    function GetHandle: GLuint;
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
    property Handle: GLuint read GetHandle;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  TgxDepthRBO = class(TgxRenderbuffer)
  private
    FDepthPrecision: TgxDepthPrecision;
    procedure SetDepthPrecision(const Value: TgxDepthPrecision);
  protected
    function GetInternalFormat: cardinal; override;
  public
    constructor Create;
    property DepthPrecision: TgxDepthPrecision read FDepthPrecision write
      SetDepthPrecision;
  end;

  TgxStencilPrecision = (spDefault, sp1bit, sp4bits, sp8bits, sp16bits);

  TgxStencilRBO = class(TgxRenderbuffer)
  private
    FStencilPrecision: TgxStencilPrecision;
    procedure SetStencilPrecision(const Value: TgxStencilPrecision);
  protected
    function GetInternalFormat: cardinal; override;
  public
    constructor Create;
    property StencilPrecision: TgxStencilPrecision read FStencilPrecision write
      SetStencilPrecision;
  end;

  TgxFrameBuffer = class
  private
    FFrameBufferHandle: TgxFramebufferHandle;
    FTarget: GLEnum;
    FWidth: Integer;
    FHeight: Integer;
    FLayer: Integer;
    FLevel: Integer;
    FTextureMipmap: cardinal;
    FAttachedTexture: array[0..MaxColorAttachments - 1] of TgxTexture;
    FDepthTexture: TgxTexture;
    FDRBO: TgxDepthRBO;
    FSRBO: TgxStencilRBO;
    function GetStatus: TgxFramebufferStatus;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetLayer(const Value: Integer);
    procedure SetLevel(const Value: Integer);
  protected
    procedure AttachTexture(
      const attachment: GLEnum;
      const textarget: GLEnum;
      const texture: GLuint;
      const level: GLint;
      const layer: GLint); overload;
    procedure ReattachTextures;
  public
    constructor Create;
    destructor Destroy; override;
    // attaches a depth rbo to the fbo
    // the depth buffer must have the same dimentions as the fbo
    procedure AttachDepthBuffer(DepthBuffer: TgxDepthRBO); overload;
    // detaches depth attachment from the fbo
    procedure DetachDepthBuffer;
    // attaches a stencil rbo to the fbo
    // the stencil buffer must have the same dimentions as the fbo
    procedure AttachStencilBuffer(StencilBuffer: TgxStencilRBO); overload;
    // detaches stencil attachment from the fbo
    procedure DetachStencilBuffer;
    // attaches a depth texture to the fbo
    // the depth texture must have the same dimentions as the fbo
    procedure AttachDepthTexture(Texture: TgxTexture); overload;
    procedure DetachDepthTexture;
    procedure AttachTexture(n: Cardinal; Texture: TgxTexture); overload;
    procedure DetachTexture(n: Cardinal);
    function GetStringStatus(out clarification: string): TgxFramebufferStatus;
    property Status: TgxFramebufferStatus read GetStatus;
    procedure Bind;
    procedure Unbind;
    procedure PreRender;
    procedure Render(var rci: TgxRenderContextInfo; baseObject:
      TgxBaseSceneObject);
    procedure PostRender(const PostGenerateMipmap: Boolean);
    property Handle: TgxFramebufferHandle read FFrameBufferHandle;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Layer: Integer read FLayer write SetLayer;
    property Level: Integer read FLevel write SetLevel;
  end;

//------------------------------------------
implementation
//------------------------------------------

//------------------------------------------
// TgxRenderbuffer
//------------------------------------------

constructor TgxRenderbuffer.Create;
begin
  inherited Create;
  FRenderbufferHandle := TgxRenderbufferHandle.Create;
  FWidth := 256;
  FHeight := 256;
end;

destructor TgxRenderbuffer.Destroy;
begin
  FRenderbufferHandle.DestroyHandle;
  FRenderbufferHandle.Free;
  inherited Destroy;
end;

function TgxRenderbuffer.GetHandle: GLuint;
begin
  if FRenderbufferHandle.Handle = 0 then
    FRenderbufferHandle.AllocateHandle;
  Result := FRenderbufferHandle.Handle;
end;

procedure TgxRenderbuffer.InvalidateStorage;
begin
  FStorageValid := False;
end;

procedure TgxRenderbuffer.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    InvalidateStorage;
  end;
end;

procedure TgxRenderbuffer.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    InvalidateStorage;
  end;
end;

procedure TgxRenderbuffer.Bind;
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

procedure TgxRenderbuffer.Unbind;
begin
  FRenderbufferHandle.UnBind;
end;

{ TgxDepthRBO }

constructor TgxDepthRBO.Create;
begin
  inherited Create;
  FDepthPrecision := dpDefault;
end;

function TgxDepthRBO.GetInternalFormat: cardinal;
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

procedure TgxDepthRBO.SetDepthPrecision(const Value: TgxDepthPrecision);
begin
  if FDepthPrecision <> Value then
  begin
    FDepthPrecision := Value;
    InvalidateStorage;
  end;
end;

{ TgxStencilRBO }

constructor TgxStencilRBO.Create;
begin
  inherited Create;
  FStencilPrecision := spDefault;
end;

function TgxStencilRBO.GetInternalFormat: cardinal;
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

procedure TgxStencilRBO.SetStencilPrecision(const Value: TgxStencilPrecision);
begin
  if FStencilPrecision <> Value then
  begin
    FStencilPrecision := Value;
    InvalidateStorage;
  end;
end;

{ TgxFrameBuffer }

constructor TgxFrameBuffer.Create;
begin
  inherited;
  FFrameBufferHandle := TgxFrameBufferHandle.Create;
  FWidth := 256;
  FHeight := 256;
  FLayer := 0;
  FLevel := 0;
  FTextureMipmap := 0;
  FTarget := GL_FRAMEBUFFER;
end;

destructor TgxFrameBuffer.Destroy;
begin
  FFrameBufferHandle.DestroyHandle;
  FFrameBufferHandle.Free;
  inherited Destroy;
end;

procedure TgxFrameBuffer.AttachTexture(n: Cardinal; Texture: TgxTexture);
var
  textarget: TgxTextureTarget;
begin
  Assert(n < MaxColorAttachments);
  Texture.Handle;
  FAttachedTexture[n] := Texture;
  textarget := Texture.Image.NativeTextureTarget;
  // Store mipmaping requires
  if not ((Texture.MinFilter in [miNearest, miLinear])
    or (textarget = ttTextureRect)) then
    FTextureMipmap := FTextureMipmap or (1 shl n);

  if Texture.Image is TgxMultiSampleImage then
    FTextureMipmap := 0;

  AttachTexture(
    GL_COLOR_ATTACHMENT0_EXT + n,
    DecodeTextureTarget(textarget),
    Texture.Handle,
    FLevel, FLayer);
end;

procedure TgxFrameBuffer.AttachDepthBuffer(DepthBuffer: TgxDepthRBO);

  procedure AttachDepthRB;
  begin
    // forces initialization
    DepthBuffer.Bind;
    DepthBuffer.Unbind;
    glFramebufferRenderbuffer(FTarget, GL_DEPTH_ATTACHMENT_EXT,
      GL_RENDERBUFFER_EXT, DepthBuffer.Handle);
  end;

var
  dp: TgxDepthPrecision;
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

procedure TgxFrameBuffer.AttachDepthTexture(Texture: TgxTexture);
begin
  FDepthTexture := Texture;

  if FDepthTexture.Image is TgxMultisampleImage then
  begin
    if not IsDepthFormat(FDepthTexture.TextureFormatEx) then
    begin
      // Force texture properties to depth compatibility
      FDepthTexture.TextureFormatEx := tfDEPTH_COMPONENT24;
      TgxMultisampleImage(FDepthTexture.Image).Width := Width;
      TgxMultisampleImage(FDepthTexture.Image).Height := Height;
    end;
    FTextureMipmap := 0;
  end
  else
  begin
    if not IsDepthFormat(FDepthTexture.TextureFormatEx) then
    begin
      // Force texture properties to depth compatibility
      FDepthTexture.ImageClassName := TgxBlankImage.ClassName;
      FDepthTexture.TextureFormatEx := tfDEPTH_COMPONENT24;
      TgxBlankImage(FDepthTexture.Image).Width := Width;
      TgxBlankImage(FDepthTexture.Image).Height := Height;
    end;
    if FDepthTexture.TextureFormatEx = tfDEPTH24_STENCIL8 then
    begin
      TgxBlankImage(FDepthTexture.Image).GetBitmap32.SetColorFormatDataType(GL_DEPTH_STENCIL, GL_UNSIGNED_INT_24_8);
      TgxBlankImage(FDepthTexture.Image).ColorFormat := GL_DEPTH_STENCIL;
    end
    else
    begin
      TgxBlankImage(FDepthTexture.Image).GetBitmap32.SetColorFormatDataType(GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE);
      TgxBlankImage(FDepthTexture.Image).ColorFormat := GL_DEPTH_COMPONENT;
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

procedure TgxFrameBuffer.DetachDepthTexture;
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

procedure TgxFrameBuffer.AttachStencilBuffer(StencilBuffer: TgxStencilRBO);

  procedure AttachStencilRB;
  begin
    // forces initialization
    StencilBuffer.Bind;
    StencilBuffer.Unbind;
    glFramebufferRenderbuffer(FTarget, GL_STENCIL_ATTACHMENT,
      GL_RENDERBUFFER_EXT, StencilBuffer.Handle);
  end;

var
  sp: TgxStencilPrecision;
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

procedure TgxFrameBuffer.AttachTexture(
  const attachment: GLEnum;
  const textarget: GLEnum;
  const texture: GLuint;
  const level: GLint;
  const layer: GLint);
var
  storeDFB: GLuint;
  RC: TgxContext;
begin
  RC := SafeCurrentContext;
  storeDFB := RC.gxStates.DrawFrameBuffer;
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
    RC.gxStates.SetFrameBuffer(storeDFB);
end;

procedure TgxFrameBuffer.Bind;
begin
  if Handle.IsDataNeedUpdate then
    ReattachTextures
  else
    Handle.Bind;
end;

procedure TgxFrameBuffer.Unbind;
begin
  FFrameBufferHandle.UnBind;
end;

procedure TgxFrameBuffer.DetachTexture(n: Cardinal);
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

procedure TgxFrameBuffer.DetachDepthBuffer;
begin
  Bind;
  glFramebufferRenderbuffer(FTarget, GL_DEPTH_ATTACHMENT,
    GL_RENDERBUFFER, 0);
  Unbind;
  FDRBO := nil;
end;

procedure TgxFrameBuffer.DetachStencilBuffer;
begin
  Bind;
  glFramebufferRenderbuffer(FTarget, GL_STENCIL_ATTACHMENT,
    GL_RENDERBUFFER, 0);
  Unbind;
  FSRBO := nil;
end;

function TgxFrameBuffer.GetStatus: TgxFramebufferStatus;
var
  status: cardinal;
begin
  status := glCheckFramebufferStatus(FTarget);

  case status of
    GL_FRAMEBUFFER_COMPLETE_EXT: Result := fsComplete;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT: Result := fsIncompleteAttachment;
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT: Result :=
      fsIncompleteMissingAttachment;
//    GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT: Result := fsIncompleteDuplicateAttachment;
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

function TgxFrameBuffer.GetStringStatus(out clarification: string):
  TgxFramebufferStatus;
const
  cFBOStatus: array[TgxFramebufferStatus] of string = (
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

procedure TgxFrameBuffer.PostRender(const PostGenerateMipmap: Boolean);
var
  n: Integer;
  textarget: TgxTextureTarget;
begin
  if (FTextureMipmap > 0) and PostGenerateMipmap then
  begin
    for n := 0 to MaxColorAttachments - 1 do
      if Assigned(FAttachedTexture[n]) then
      begin
        if FTextureMipmap and (1 shl n) = 0 then
          Continue;
        textarget := FAttachedTexture[n].Image.NativeTextureTarget;
        with FFrameBufferHandle.RenderingContext.gxStates do
          TextureBinding[ActiveTexture, textarget] :=
            FAttachedTexture[n].Handle;
        glGenerateMipmap(DecodeTextureTarget(textarget));
      end;
  end;
end;

procedure TgxFrameBuffer.PreRender;
begin

end;

procedure TgxFrameBuffer.Render(var rci: TgxRenderContextInfo; baseObject:
  TgxBaseSceneObject);
var
  backColor: TgxColorVector;
  buffer: TgxSceneBuffer;
begin
  Bind;
  Assert(Status = fsComplete, 'Framebuffer not complete');

  buffer := TgxSceneBuffer(rci.buffer);

  backColor := ConvertWinColor(buffer.BackgroundColor);
  glClearColor(backColor.X, backColor.Y, backColor.Z,
    buffer.BackgroundAlpha);
  rci.gxStates.SetColorMask(cAllColorComponents);
  rci.gxStates.DepthWriteMask := True;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  baseObject.Render(rci);
  Unbind;
end;

procedure TgxFrameBuffer.SetHeight(const Value: Integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
  end;
end;

procedure TgxFrameBuffer.SetWidth(const Value: Integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
  end;
end;

procedure TgxFrameBuffer.ReattachTextures;
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
    glFramebufferRenderbuffer(FTarget, GL_DEPTH_ATTACHMENT_EXT,
      GL_RENDERBUFFER_EXT, FDRBO.Handle);
    bEmpty := False;
  end;

  if Assigned(FSRBO) then
  begin
    FSRBO.Bind;
    FSRBO.Unbind;
    glFramebufferRenderbuffer(FTarget, GL_STENCIL_ATTACHMENT,
      GL_RENDERBUFFER_EXT, FSRBO.Handle);
    bEmpty := False;
  end;

  if not bEmpty and (GetStringStatus(s) <> fsComplete) then
    ShowMessage(Format('Framebuffer error: %s. Deactivated', [s]));

  Handle.NotifyDataUpdated;
end;

procedure TgxFrameBuffer.SetLayer(const Value: Integer);
var
  RC: TgxContext;
begin
  if FLayer <> Value then
  begin
    FLayer := Value;
    RC := CurrentContext;
    if Assigned(RC) then
    begin
      if RC.gxStates.DrawFrameBuffer = FFrameBufferHandle.Handle then
        ReattachTextures;
    end;
  end;
end;

procedure TgxFrameBuffer.SetLevel(const Value: Integer);
var
  RC: TgxContext;
begin
  if FLevel <> Value then
  begin
    FLevel := Value;
    RC := CurrentContext;
    if Assigned(RC) then
    begin
      if RC.gxStates.DrawFrameBuffer = FFrameBufferHandle.Handle then
        ReattachTextures;
    end;
  end;
end;

end.
