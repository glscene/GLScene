//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit OpenGL.InitVCL;
(*
  OpenGL for Vcl adapted from github.com/LUXOPHIA
*)

interface

uses
  Winapi.OpenGL,
  Winapi.OpenGLext,
  Winapi.Windows,
  System.SysUtils,

  GLS.VectorTypes,
  Vcl.Forms;

type
  TGLOpenGL = class
  private
    CustomForm: TCustomForm;
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

  TGLShader = class
  private
  protected
    _ID: TGLuint;
  public
    constructor Create(const Kind_: TGLuint);
    destructor Destroy; override;
    property ID: TGLuint read _ID;
    procedure SetSource(const Source_: String);
  end;

  TGLShaderV = class(TGLShader)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TGLShaderG = class(TGLShader)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TGLShaderF = class(TGLShader)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TGLProgram = class
  private
  protected
    _ID: TGLuint;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach(const Shader_: TGLShader);
    procedure Detach(const Shader_: TGLShader);
    procedure Link;
    procedure Use;
  end;

  TGLBuffer<_TYPE_: record > = class
  public type
    _PValue_ = ^_TYPE_;
  private
  protected
    _ID: TGLuint;
    _Kind: TGLuint;
    _Count: Integer;
    _Head: _PValue_;
    procedure SetCount(const Count_: Integer);
  public
    constructor Create(const Kind_: TGLuint);
    destructor Destroy; override;
    property ID: TGLuint read _ID;
    property Count: Integer read _Count write SetCount;
    procedure Bind;
    procedure Unbind;
    procedure Map;
    procedure Unmap;
  end;

  TGLBufferV<_TYPE_: record > = class(TGLBuffer<_TYPE_>)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TGLBufferI<_TYPE_: record > = class(TGLBuffer<_TYPE_>)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TGLBufferU<_TYPE_: record > = class(TGLBuffer<_TYPE_>)
  private
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TGLArray = class
  private
  protected
    _ID: TGLuint;
  public
    constructor Create;
    destructor Destroy; override;
    property ID: TGLuint read _ID;
    procedure BeginBind;
    procedure EndBind;
  end;

var
  GLOpenGL: TGLOpenGL;

//=====================================================================
implementation
//=====================================================================

procedure TGLOpenGL.SetPFD(const PFD_: TPixelFormatDescriptor);
begin
  DestroyRC;
  DestroyDC;
  CreateDC;
  ValidatePFD(PFD_);
  CreateRC;
end;

procedure TGLOpenGL.SetPFI(const PFI_: Integer);
begin
  DestroyRC;
  DestroyDC;
  CreateDC;
  ValidatePFI(PFI_);
  CreateRC;
end;

procedure TGLOpenGL.CreateWindow;
begin
  CustomForm := TCustomForm.CreateNew(nil);
  _WND := CustomForm.Handle;
end;

procedure TGLOpenGL.DestroyWindow;
begin
  CustomForm.Free;
end;

// ------------------------------------------------------------------------------

procedure TGLOpenGL.ValidatePFD(const PFD_: TPixelFormatDescriptor);
var
  I: Integer;
begin
  _PFD := PFD_;
  I := ChoosePixelFormat(_DC, @_PFD);
  Assert(I > 0, 'Not found the PixelFormat with a close setting!');
  ValidatePFI(I);
end;

procedure TGLOpenGL.ValidatePFI(const PFI_: Integer);
begin
  _PFI := PFI_;
  Assert(DescribePixelFormat(_DC, _PFI, SizeOf(TPixelFormatDescriptor), _PFD),
    'Not found the PixelFormat of the index!');
end;

// ------------------------------------------------------------------------------

procedure TGLOpenGL.CreateDC;
begin
  _DC := GetDC(_WND);
end;

procedure TGLOpenGL.DestroyDC;
begin
  ReleaseDC(0, _DC);
end;

// ------------------------------------------------------------------------------

procedure TGLOpenGL.CreateRC;
begin
  ApplyPixelFormat(_DC);
  _RC := wglCreateContext(_DC);
end;

procedure TGLOpenGL.DestroyRC;
begin
  wglDeleteContext(_RC);
end;

constructor TGLOpenGL.Create;
begin
  inherited;
  CreateWindow;
  CreateDC;
  ValidatePFD(DefaultPFD);
  CreateRC;
  InitOpenGL;
end;

destructor TGLOpenGL.Destroy;
begin
  DestroyRC;
  DestroyDC;
  DestroyWindow;
  inherited;
end;

class function TGLOpenGL.DefaultPFD: TPixelFormatDescriptor;
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

procedure TGLOpenGL.BeginGL;
begin
  wglMakeCurrent(_DC, _RC);
end;

procedure TGLOpenGL.EndGL;
begin
  wglMakeCurrent(_DC, 0);
end;

// ------------------------------------------------------------------------------

procedure TGLOpenGL.InitOpenGL;
begin
  BeginGL;
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_CULL_FACE);
  EndGL;
end;

// ------------------------------------------------------------------------------

procedure TGLOpenGL.ApplyPixelFormat(const DC_: HDC);
begin
  Assert(SetPixelFormat(DC_, _PFI, @_PFD), 'SetPixelFormat() is failed!');
end;

constructor TGLShader.Create(const Kind_: TGLuint);
begin
  inherited Create;
  _ID := glCreateShader(Kind_);
end;

destructor TGLShader.Destroy;
begin
  glDeleteShader(_ID);
  inherited;
end;

procedure TGLShader.SetSource(const Source_: String);
var
  P: PAnsiChar;
  N: TGLint;
  E: TGLint;
  Cs: array of PGLchar;
  CsN: TGLsizei;
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

constructor TGLShaderV.Create;
begin
  inherited Create(GL_VERTEX_SHADER);
end;

destructor TGLShaderV.Destroy;
begin
  inherited;
end;

constructor TGLShaderG.Create;
begin
  inherited Create(GL_GEOMETRY_SHADER);
end;

destructor TGLShaderG.Destroy;
begin
  inherited;
end;

constructor TGLShaderF.Create;
begin
  inherited Create(GL_FRAGMENT_SHADER);
end;

destructor TGLShaderF.Destroy;
begin
  inherited;
end;

constructor TGLProgram.Create;
begin
  inherited;
  _ID := glCreateProgram;
end;

destructor TGLProgram.Destroy;
begin
  glDeleteProgram(_ID);
  inherited;
end;

procedure TGLProgram.Attach(const Shader_: TGLShader);
begin
  glAttachShader(_ID, Shader_.ID);
end;

procedure TGLProgram.Detach(const Shader_: TGLShader);
begin
  glDetachShader(_ID, Shader_.ID);
end;

// ------------------------------------------------------------------------------

procedure TGLProgram.Link;
begin
  glLinkProgram(_ID);
end;

// ------------------------------------------------------------------------------

procedure TGLProgram.Use;
begin
  glUseProgram(_ID);
end;

procedure TGLBuffer<_TYPE_>.SetCount(const Count_: Integer);
begin
  _Count := Count_;
  Bind;
  glBufferData(_Kind, SizeOf(_TYPE_) * _Count, nil, GL_DYNAMIC_DRAW);
  Unbind;
end;

constructor TGLBuffer<_TYPE_>.Create(const Kind_: TGLuint);
begin
  inherited Create;
  glGenBuffers(1, @_ID);
  _Kind := Kind_;
  Count := 0;
end;

destructor TGLBuffer<_TYPE_>.Destroy;
begin
  glDeleteBuffers(1, @_ID);
  inherited;
end;

procedure TGLBuffer<_TYPE_>.Bind;
begin
  glBindBuffer(_Kind, _ID);
end;

procedure TGLBuffer<_TYPE_>.Unbind;
begin
  glBindBuffer(_Kind, 0);
end;

// ------------------------------------------------------------------------------

procedure TGLBuffer<_TYPE_>.Map;
begin
  Bind;
  _Head := glMapBuffer(_Kind, GL_READ_WRITE);
end;

procedure TGLBuffer<_TYPE_>.Unmap;
begin
  glUnmapBuffer(_Kind);
  Unbind;
end;

constructor TGLBufferV<_TYPE_>.Create;
begin
  inherited Create(GL_ARRAY_BUFFER);
end;

destructor TGLBufferV<_TYPE_>.Destroy;
begin
  inherited;
end;

constructor TGLBufferI<_TYPE_>.Create;
begin
  inherited Create(GL_ELEMENT_ARRAY_BUFFER);
end;

destructor TGLBufferI<_TYPE_>.Destroy;
begin
  inherited;
end;

constructor TGLBufferU<_TYPE_>.Create;
begin
  inherited Create(GL_UNIFORM_BUFFER);
end;

destructor TGLBufferU<_TYPE_>.Destroy;
begin
  inherited;
end;

constructor TGLArray.Create;
begin
  inherited Create;
  glGenVertexArrays(1, @_ID);
end;

destructor TGLArray.Destroy;
begin
  glDeleteVertexArrays(1, @_ID);
  inherited;
end;

procedure TGLArray.BeginBind;
begin
  glBindVertexArray(_ID);
end;

procedure TGLArray.EndBind;
begin
  glBindVertexArray(0);
end;

// ====================================================================
initialization
// ====================================================================

GLOpenGL := TGLOpenGL.Create;
GLOpenGL.BeginGL;
InitOpenGLext;

finalization

GLOpenGL.EndGL;
GLOpenGL.Free;

end.
