unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Imaging.Jpeg,
  
  GLScene,
  Scene.VectorTypes,
  Scene.VectorGeometry,
  GLCadencer,
  GLSceneViewer,
  GLVectorFileObjects,
  GLAsyncTimer,
  GLS.ShaderCel,
  GLGeomObjects,
  GLTexture,
  GLObjects,
  GLCrossPlatform,
  GLMaterial,
  GLCoordinates,
  GLBaseClasses,
  GLFileMD2,
  GLKeyboard,
  GLS.Uti, GLS.ShaderCells;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    GLActor1: TGLActor;
    AsyncTimer1: TGLAsyncTimer;
    GLTexturedCelShader: TGLCelShader;
    GLColoredCelShader: TGLCelShader;
    GLTorus1: TGLTorus;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure AsyncTimer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
  private
     
  public
     
    mx, my, lx, ly: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  r: Single;
begin
  SetGLSceneMediaDir();
  GLActor1.LoadFromFile('waste.md2');
  r := GLActor1.BoundingSphereRadius;
  GLActor1.Scale.SetVector(2.5 / r, 2.5 / r, 2.5 / r);
  GLActor1.AnimationMode := aamLoop;
  GLMaterialLibrary1.Materials[0].Material.Texture.Image.LoadFromFile
    ('wastecell.jpg');
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  lx := X;
  ly := Y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TForm1.AsyncTimer1Timer(Sender: TObject);
begin
  Form1.Caption := Format('GLScene Cel Shading - %.2f FPS',
    [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  if IsKeyDown(VK_LBUTTON) then
  begin
    GLCamera1.MoveAroundTarget(ly - my, lx - mx);
    lx := mx;
    ly := my;
  end;

  GLTorus1.TurnAngle := 15 * Sin(newTime * 5);
  GLTorus1.PitchAngle := 15 * Cos(newTime * 5);
end;

end.
