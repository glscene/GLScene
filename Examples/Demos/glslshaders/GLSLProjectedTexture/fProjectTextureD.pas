unit fProjectTextureD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.Cadencer,
  GLS.VectorFileObjects,
  GLS.ShadowVolume,
  GLS.GeomObjects,
  GLS.Utils,
  GLS.FileLMTS,
  GLS.Context,
  GLS.VectorGeometry,
  GLS.PipelineTransformation,
  GLSL.ProjectedTextures,

  GLS.Graphics,
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.FileTGA;

type

  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    GLLightSource2: TGLLightSource;
    GLArrowLine1: TGLArrowLine;
    Timer1: TTimer;
    GLDummyCube3: TGLDummyCube;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLSLTextureEmitter1: TGLSLTextureEmitter;
    GLSLProjectedTextures1: TGLSLProjectedTextures;
    GLFreeForm1: TGLFreeForm;
    GLCube1: TGLCube;
    GLSLTextureEmitter2: TGLSLTextureEmitter;
    procedure GLCamera1CustomPerspective(const viewport: TRectangle; Width, Height, DPI: Integer; var viewPortRadius: Single);
    procedure GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    mx, my: Integer;
    sdir: Integer;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  I : Integer;
begin
  Randomize;
  sdir := -10;
  GLCamera1.CameraStyle := cscustom;

  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\texture');

  GLSLProjectedTextures1.Material.Texture.Image.LoadFromFile('flare1.bmp');
  GLSLProjectedTextures1.Material.Texture.Disabled := False;
  GLSLProjectedTextures1.Material.Texture.TextureWrap := twNone;
  GLSLProjectedTextures1.Material.Texture.MinFilter := miLinear;
  GLSLProjectedTextures1.Material.Texture.MagFilter := maLinear;
  GLSLProjectedTextures1.UseLightmaps := True;
  GLCube1.Material.Texture.Image.LoadFromFile('ashwood.jpg');
  GLCube1.Material.Texture.Disabled := False;

  GLFreeForm1.LoadFromFile('groundtest.lmts'); // persistent image
  GLFreeForm1.ObjectStyle := [osDirectDraw];

  for I := 0 to GLMaterialLibrary1.Materials.Count - 1 do
    GLMaterialLibrary1.Materials.Items[I].Material.MaterialOptions := [moNoLighting];

end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
var
  I: Integer;
begin
  for I := 1 to glslProjectedTextures1.Emitters.Count - 1 do
    glslProjectedTextures1.Emitters[I].Emitter.turn(DeltaTime * (I + 1) * 10);
  GLSceneViewer1.Invalidate;
  GLArrowLine1.Position.Y := GLArrowLine1.Position.Y + sdir * DeltaTime;
  if GLArrowLine1.Position.Y > 20 then
  begin
    GLArrowLine1.Position.Y := 20;
    sdir := -10;
  end;
  if GLArrowLine1.Position.Y < 10 then
  begin
    GLArrowLine1.Position.Y := 10;
    sdir := 10;
  end;
end;

procedure TForm1.GLCamera1CustomPerspective(const viewport: TRectangle; Width, Height, DPI: Integer; var viewPortRadius: Single);
begin
  CurrentGLContext.PipelineTransformation.ProjectionMatrix^ :=
    CreatePerspectiveMatrix(GLCamera1.GetFieldOfView(Width)/2, Width / Height,
                            GLCamera1.NearPlaneBias, GLCamera1.DepthOfView);
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Caption := 'GLSL Projected Texture ' +GLSceneViewer1.FramesPerSecondText();
end;

end.

