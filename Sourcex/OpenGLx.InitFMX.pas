//
// The graphics engine GXScene https://github.com/glscene
//
unit OpenGXS.InitFMX;

(* OpenGL for Initialization in FMX *)

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  FMX.Forms;

type
  TgxOpenGL = class
  private
    CommonCustomForm: TCommonCustomForm;
    _WND: HWND;
    _DC: HDC;
  protected
    _PFD: TPixelFormatDescriptor;
    _PFI: Integer;
    _RC: HGLRC;
    procedure SetPFD(const PFD_: TPixelFormatDescriptor);
    procedure SetPFI(const PFI_: Integer);
    procedure CreateWindow;
    procedure DestroyWindow;
    procedure ValidatePFD(const PFD_: TPixelFormatDescriptor);
    procedure ValidatePFI(const PFI_: Integer);
    procedure CreateDC;
    procedure DestroyDC;
    procedure CreateRC;
    procedure DestroyRC;
  public
    constructor Create;
    destructor Destroy; override;
    property PFD: TPixelFormatDescriptor read _PFD write SetPFD;
    property PFI: Integer read _PFI write SetPFI;
    property RC: HGLRC read _RC;
    class function DefaultPFD: TPixelFormatDescriptor;
    procedure BeginGL;
    procedure EndGL;
    procedure InitOpenGL;
    procedure ApplyPixelFormat(const DC_: HDC);
  end;

//-----------------------------------------------------------------------
  TgxShader = class
  private
  protected
    _ID: GLuint;
  public
    constructor Create(const Kind_: GLenum);
    destructor Destroy; override;
    property ID: GLuint read _ID;
    procedure SetSource(const Source_: String);
  end;

//-----------------------------------------------------------------------

  TgxShaderV = class(TgxShader)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TgxShaderG = class(TgxShader)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TgxShaderF = class(TgxShader)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TgxProgram = class
  private
  protected
    _ID: GLuint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach(const Shader_: TgxShader);
    procedure Detach(const Shader_: TgxShader);
    procedure Link;
    procedure Use;
  end;

//-----------------------------------------------------------------------

  TgxBuffer<_TYPE_: record > = class
  public type
    _PValue_ = ^_TYPE_;
  private
  protected
    _ID: GLuint;
    _Kind: GLenum;
    _Count: Integer;
    _Head: _PValue_;
    procedure SetCount(const Count_: Integer);
  public
    constructor Create(const Kind_: GLenum);
    destructor Destroy; override;
    property ID: GLuint read _ID;
    property Count: Integer read _Count write SetCount;
    procedure Bind;
    procedure Unbind;
    procedure Map;
    procedure Unmap;
  end;

//-----------------------------------------------------------------------

  TgxBufferV<_TYPE_: record > = class(TgxBuffer<_TYPE_>)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TgxBufferI<_TYPE_: record > = class(TgxBuffer<_TYPE_>)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TgxBufferU<_TYPE_: record > = class(TgxBuffer<_TYPE_>)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

//-----------------------------------------------------------------------

  TgxArray = class
  private
  protected
    _ID: GLuint;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: GLuint read _ID;
    procedure BeginBind;
    procedure EndBind;
  end;

var
  GXOpenGL: TgxOpenGL;

//-----------------------------------------------------------------------
implementation
//-----------------------------------------------------------------------

uses
  System.SysUtils,
  FMX.Platform.Win;

procedure TgxOpenGL.SetPFD(const PFD_: TPixelFormatDescriptor);
begin
  DestroyRC;
  DestroyDC;
  CreateDC;
  ValidatePFD(PFD_);
  CreateRC;
end;

procedure TgxOpenGL.SetPFI(const PFI_: Integer);
begin
  DestroyRC;
  DestroyDC;
  CreateDC;
  ValidatePFI(PFI_);
  CreateRC;
end;

// ------------------------------------------------------------------------------
procedure TgxOpenGL.CreateWindow;
begin
  CommonCustomForm := TCommonCustomForm.Create(nil);
  _WND := WindowHandleToPlatform(CommonCustomForm.Handle).Wnd;
end;

procedure TgxOpenGL.DestroyWindow;
begin
  CommonCustomForm.Free;
end;

// ------------------------------------------------------------------------------

procedure TgxOpenGL.ValidatePFD(const PFD_: TPixelFormatDescriptor);
var
  I: Integer;
begin
  _PFD := PFD_;
  I := ChoosePixelFormat(_DC, @_PFD);
  Assert(I > 0, 'Not found the PixelFormat with a close setting!');
  ValidatePFI(I);
end;

procedure TgxOpenGL.ValidatePFI(const PFI_: Integer);
begin
  _PFI := PFI_;
  Assert(DescribePixelFormat(_DC, _PFI, SizeOf(TPixelFormatDescriptor), _PFD),
    'Not found the PixelFormat of the index!');
end;

// ------------------------------------------------------------------------------

procedure TgxOpenGL.CreateDC;
begin
  _DC := GetDC(_WND);
end;

procedure TgxOpenGL.DestroyDC;
begin
  ReleaseDC(0, _DC);
end;

// ------------------------------------------------------------------------------

procedure TgxOpenGL.CreateRC;
begin
  ApplyPixelFormat(_DC);

  _RC := wglCreateContext(_DC);
end;

procedure TgxOpenGL.DestroyRC;
begin
  wglDeleteContext(_RC);
end;

constructor TgxOpenGL.Create;
begin
  inherited;
  CreateWindow;
  CreateDC;
  ValidatePFD(DefaultPFD);
  CreateRC;
  InitOpenGL;
end;

destructor TgxOpenGL.Destroy;
begin
  DestroyRC;
  DestroyDC;
  DestroyWindow;
  inherited;
end;

// ------------------------------------------------------------------------------

class function TgxOpenGL.DefaultPFD: TPixelFormatDescriptor;
begin
  with Result do
  begin
    nSize := SizeOf(TPixelFormatDescriptor);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 24;
    cRedBits := 0;
    cRedShift := 0;
    cGreenBits := 0;
    cGreenShift := 0;
    cBlueBits := 0;
    cBlueShift := 0;
    cAlphaBits := 0;
    cAlphaShift := 0;
    cAccumBits := 0;
    cAccumRedBits := 0;
    cAccumGreenBits := 0;
    cAccumBlueBits := 0;
    cAccumAlphaBits := 0;
    cDepthBits := 32;
    cStencilBits := 0;
    cAuxBuffers := 0;
    iLayerType := PFD_MAIN_PLANE;
    bReserved := 0;
    dwLayerMask := 0;
    dwVisibleMask := 0;
    dwDamageMask := 0;
  end;
end;

// ------------------------------------------------------------------------------

procedure TgxOpenGL.BeginGL;
begin
  wglMakeCurrent(_DC, _RC);
end;

procedure TgxOpenGL.EndGL;
begin
  wglMakeCurrent(_DC, 0);
end;

// ------------------------------------------------------------------------------

procedure TgxOpenGL.InitOpenGL;
begin
  BeginGL;
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
  EndGL;
end;

// ------------------------------------------------------------------------------

procedure TgxOpenGL.ApplyPixelFormat(const DC_: HDC);
begin
  Assert(SetPixelFormat(DC_, _PFI, @_PFD), 'SetPixelFormat() is failed!');
end;

// ------------------------------------------------------------------------------

constructor TgxShader.Create(const Kind_: GLenum);
begin
  inherited Create;
  _ID := glCreateShader(Kind_);
end;

destructor TgxShader.Destroy;
begin
  glDeleteShader(_ID);
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TgxShader.SetSource(const Source_: String);
var
  P: PAnsiChar;
  N: GLint;
  E: GLint;
  Cs: array of PGLchar;
  CsN: GLsizei;
begin
  P := PAnsiChar(AnsiString(Source_));
  N := Length(Source_);
  glShaderSource(_ID, 1, @P, @N);
  glCompileShader(_ID);
  glGetShaderiv(_ID, GL_COMPILE_STATUS, @E);
  if E = GL_FALSE then
  begin
    glGetShaderiv(_ID, GL_INFO_LOG_LENGTH, @N);
    SetLength(Cs, N);
    glGetShaderInfoLog(_ID, N, @CsN, @Cs[0]);
    Assert(False, AnsiString(Cs));
  end;
end;

// ------------------------------------------------------------------------------

constructor TgxShaderV.Create;
begin
  inherited Create(GL_VERTEX_SHADER);
end;

destructor TgxShaderV.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TgxShaderG.Create;
begin
  inherited Create(GL_GEOMETRY_SHADER);
end;

destructor TgxShaderG.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TgxShaderF.Create;
begin
  inherited Create(GL_FRAGMENT_SHADER);
end;

destructor TgxShaderF.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TgxProgram.Create;
begin
  inherited;
  _ID := glCreateProgram;
end;

destructor TgxProgram.Destroy;
begin
  glDeleteProgram(_ID);
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TgxProgram.Attach(const Shader_: TgxShader);
begin
  glAttachShader(_ID, Shader_.ID);
end;

procedure TgxProgram.Detach(const Shader_: TgxShader);
begin
  glDetachShader(_ID, Shader_.ID);
end;

// ------------------------------------------------------------------------------

procedure TgxProgram.Link;
begin
  glLinkProgram(_ID);
end;

// ------------------------------------------------------------------------------

procedure TgxProgram.Use;
begin
  glUseProgram(_ID);
end;

// ------------------------------------------------------------------------------

procedure TgxBuffer<_TYPE_>.SetCount(const Count_: Integer);
begin
  _Count := Count_;
  Bind;
  glBufferData(_Kind, SizeOf(_TYPE_) * _Count, nil, GL_DYNAMIC_DRAW);
  Unbind;
end;

// ------------------------------------------------------------------------------

constructor TgxBuffer<_TYPE_>.Create(const Kind_: GLenum);
begin
  inherited Create;
  glGenBuffers(1, @_ID);
  _Kind := Kind_;
  Count := 0;
end;

// ------------------------------------------------------------------------------

destructor TgxBuffer<_TYPE_>.Destroy;
begin
  glDeleteBuffers(1, @_ID);
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TgxBuffer<_TYPE_>.Bind;
begin
  glBindBuffer(_Kind, _ID);
end;

procedure TgxBuffer<_TYPE_>.Unbind;
begin
  glBindBuffer(_Kind, 0);
end;

// ------------------------------------------------------------------------------

procedure TgxBuffer<_TYPE_>.Map;
begin
  Bind;
  _Head := glMapBuffer(_Kind, GL_READ_WRITE);
end;

procedure TgxBuffer<_TYPE_>.Unmap;
begin
  glUnmapBuffer(_Kind);
  Unbind;
end;

constructor TgxBufferV<_TYPE_>.Create;
begin
  inherited Create(GL_ARRAY_BUFFER);
end;

destructor TgxBufferV<_TYPE_>.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TgxBufferI<_TYPE_>.Create;
begin
  inherited Create(GL_ELEMENT_ARRAY_BUFFER);
end;

destructor TgxBufferI<_TYPE_>.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TgxBufferU<_TYPE_>.Create;
begin
  inherited Create(GL_UNIFORM_BUFFER);
end;

destructor TgxBufferU<_TYPE_>.Destroy;
begin
  inherited;
end;

// ------------------------------------------------------------------------------

constructor TgxArray.Create;
begin
  inherited Create;
  glGenVertexArrays(1, @_ID);
end;

destructor TgxArray.Destroy;
begin
  glDeleteVertexArrays(1, @_ID);
  inherited;
end;

// ------------------------------------------------------------------------------

procedure TgxArray.BeginBind;
begin
  glBindVertexArray(_ID);
end;

procedure TgxArray.EndBind;
begin
  glBindVertexArray(0);
end;

//==========================================================================
initialization
//==========================================================================

GXOpenGL := TgxOpenGL.Create;
GXOpenGL.BeginGL;
InitOpenGLext;

finalization

GXOpenGL.EndGL;
GXOpenGL.Free;

end.
