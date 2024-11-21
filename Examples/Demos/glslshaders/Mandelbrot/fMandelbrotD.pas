unit fMandelbrotD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  Winapi.OpenGLext,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Imaging.Jpeg,

  Stage.Keyboard,
  Stage.Utils,

  GLS.SceneViewer,
  GLS.Texture,
  GLS.Cadencer,
  GLS.Scene,
  GLS.Context,
  GLS.XCollection,
  GLS.FileTGA,
  GLS.HUDObjects,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.Material,
  GLS.Coordinates,

  GLS.RenderContextInfo,
  GLS.BaseClasses,
  GLSL.CustomShader,
  GLSL.Shader;

type
  TForm1 = class(TForm)
    Scene: TGLScene;
    Timer1: TTimer;
    Viewer: TGLSceneViewer;
    GLCadencer: TGLCadencer;
    Mandelbrot: TGLDirectOpenGL;
    GLMatLib: TGLMaterialLibrary;
    GLCamera: TGLCamera;
    OpenDialog1: TOpenDialog;
    GLHUDText: TGLHUDText;
    GLWindowsBitmapFont: TGLWindowsBitmapFont;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencerProgress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MandelbrotRender(Sender: TObject; var rci: TGLRenderContextInfo);
  public
    PathToAsset: TFileName;
    MandelbrotProgram: TGLProgramHandle;
  end;

const
  HELP_TEXT = '+: Zoom in'#13#10 + '-: Zoom out'#13#10 +
    'Arrow keys: Move around'#13#10 + 'F3: Load colormap';

var
  Form1: TForm1;
  PositionX, PositionY, Scale: Single;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
//  SetCurrentDir(ExtractFilePath(ParamStr(0)));
  PathToAsset := GetCurrentAssetPath();
  SetCurrentDir(PathToAsset  + '\texture');
  GLMatLib.TexturePaths := PathToAsset;
  GLMatLib.Materials[0].Material.Texture.Image.LoadFromFile('hot_metal.bmp');
  PositionX := -0.5;
  PositionY := 0.0;
  Scale := 1.0;

  GLHUDText.Text := HELP_TEXT;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := Format('Mandelbrot %.1f FPS', [Viewer.FramesPerSecond]);

  Viewer.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencerProgress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  deltax, deltay: Single;
  pt: TPoint;
begin
  if IsKeyDown(VK_F3) then
    if OpenDialog1.Execute then
      GLMatLib.Materials[0].Material.Texture.Image.LoadFromFile
        (OpenDialog1.FileName);

  if IsKeyDown('+') or IsKeyDown(VK_ADD) then
    Scale := Scale * 1.0 / (1.0 + deltaTime * 0.5);

  if IsKeyDown('-') or IsKeyDown(VK_SUBTRACT) then
    Scale := Scale * (1.0 + deltaTime * 0.5);

  if IsKeyDown(VK_DOWN) or IsKeyDown(VK_NUMPAD8) then
    PositionY := PositionY + deltaTime * Scale * 0.5;

  if IsKeyDown(VK_UP) or IsKeyDown(VK_NUMPAD2) then
    PositionY := PositionY - deltaTime * Scale * 0.5;

  if IsKeyDown(VK_LEFT) or IsKeyDown(VK_NUMPAD6) then
    PositionX := PositionX + deltaTime * Scale * 0.5;

  if IsKeyDown(VK_RIGHT) or IsKeyDown(VK_NUMPAD4) then
    PositionX := PositionX - deltaTime * Scale * 0.5;

  Viewer.Invalidate;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  DistDelta: Single;
begin
end;

procedure TForm1.MandelbrotRender(Sender: TObject;
  var rci: TGLRenderContextInfo);
begin
  // shader init
  if not Assigned(MandelbrotProgram) then
  begin
    SetCurrentDir(PathToAsset  + '\shader');

    MandelbrotProgram := TGLProgramHandle.CreateAndAllocate;
      MandelbrotProgram.AddShader(TGLFragmentShaderHandle,
      String(LoadAnsiStringFromFile('Mandelbrot.frag')), True);
    MandelbrotProgram.AddShader(TGLVertexShaderHandle,
      String(LoadAnsiStringFromFile('Mandelbrot.vert')), True);

    if not MandelbrotProgram.LinkProgram then
      raise Exception.Create(MandelbrotProgram.InfoLog);

    if not MandelbrotProgram.ValidateProgram then
      raise Exception.Create(MandelbrotProgram.InfoLog);
  end;

  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glLoadIdentity;
  glMatrixMode(GL_PROJECTION);
  glPushMatrix;
  glLoadIdentity;

  MandelbrotProgram.UseProgramObject;

  MandelbrotProgram.Uniform1f['positionX'] := PositionX;
  MandelbrotProgram.Uniform1f['positionY'] := PositionY;
  MandelbrotProgram.Uniform1f['scale'] := Scale;

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, GLMatLib.Materials[0].Material.Texture.Handle);
  MandelbrotProgram.Uniform1i['colorMap'] := 0;

  // drawing rectangle over screen
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_LIGHTING);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0);
  glVertex2f(-1.0, -1.0);
  glTexCoord2f(1.0, 0.0);
  glVertex2f(1.0, -1.0);

  glTexCoord2f(1.0, 1.0);
  glVertex2f(1.0, 1.0);
  glTexCoord2f(0.0, 1.0);
  glVertex2f(-1.0, 1.0);
  glEnd;

  MandelbrotProgram.EndUseProgramObject;

  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix;
  glPopAttrib;

  /// -CheckOpenGLError;
end;

end.
