unit fWhirlD;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.OpenGL,
  System.SysUtils,
  System.Math,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,

  GLS.BaseClasses,
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.VectorGeometry,
  GLS.SceneViewer,
  GLS.Particles,
  GLS.Cadencer,
  GLS.Objects,
  GLS.Coordinates,
  GLS.Behaviours;

type
  TFormWhirlD = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    Panel1: TPanel;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    GLScene1: TGLScene;
    GLParticles1: TGLParticles;
    DummyCube1: TGLDummyCube;
    Sprite1: TGLSprite;
    GLCamera1: TGLCamera;
    procedure Timer1Timer(Sender: TObject);
    procedure GLDummyCube1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLParticles1ActivateParticle(Sender: TObject;
      Particle: TGLBaseSceneObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    mx, my: Integer;
  public

  end;

var
  FormWhirlD: TFormWhirlD;

implementation

{$R *.dfm}

procedure TFormWhirlD.GLParticles1ActivateParticle(Sender: TObject;
  Particle: TGLBaseSceneObject);
var
  r, alpha, cr, sr: Single;
begin
  alpha := Random * 2 * PI;
  r := 2 * Random;
  SinCosine(alpha, r * r, sr, cr);
  Particle.Children[0].Position.SetPoint(sr, 3 * r - 3, cr);
  GetOrCreateInertia(Particle).TurnSpeed := Random(30);
  TGLCustomSceneObject(Particle).TagFloat := GLCadencer1.CurrentTime;
end;

procedure TFormWhirlD.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormWhirlD.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [] then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TFormWhirlD.GLDummyCube1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  with TGLCustomSceneObject(Sender) do
  begin
    if newTime - TagFloat > 3 then
      GLParticles1.KillParticle(TGLCustomSceneObject(Sender));
  end;
end;

procedure TFormWhirlD.Timer1Timer(Sender: TObject);
begin
  Panel1.Caption := Format('%d particles, %.1f FPS',
    [GLParticles1.Count, GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormWhirlD.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLParticles1.CreateParticle;
end;

end.
