unit fBoomD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  
  Stage.VectorTypes,
  GLS.Scene,
  GLS.Objects,
  GLS.Cadencer,
  GLS.FireFX,
  GLS.Behaviours,
  Stage.VectorGeometry,
  GLS.SceneViewer,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormBoom = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    FireFX: TGLFireFXManager;
    GLCadencer1: TGLCadencer;
    Sphere1: TGLSphere;
    Timer1: TTimer;
    Button1: TButton;
    SmokeFX: TGLFireFXManager;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
  public
    mx, my: Integer;
  end;

var
  FormBoom: TFormBoom;

implementation

{$R *.DFM}

procedure TFormBoom.Button1Click(Sender: TObject);
begin
  // A button click triggers the small animation sequence
  // first, we enabled the cadencer, to get Progression events
  GLCadencer1.Enabled := True;
  // then we set (or reset) the sphere position and initial speed
  Sphere1.Position.AsVector := NullHmgPoint;
  GetOrCreateInertia(Sphere1).TranslationSpeed.SetVector(Random, 9, Random);
  // the tagfloat is used as timer (before explosion)
  Sphere1.TagFloat := 3.5;
  // Here we reinitialize the FireFX (starts enabled) and SmokeFX (disabled at first)
  FireFX.ParticleSize := 0.5;
  FireFX.Disabled := False;
  FireFX.FireInit;
  SmokeFX.Disabled := True;
  SmokeFX.FireInit;
end;

procedure TFormBoom.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  // have we exploded yet?
  if Sphere1.TagFloat > 0 then
  begin
    // no, so decrease time before explosion
    Sphere1.TagFloat := Sphere1.TagFloat - deltaTime;
    // explosion time?
    if Sphere1.TagFloat < 0 then
    begin
      // yep! make particles bigger,
      FireFX.ParticleSize := 2;
      // fire the explosion
      if Random() > 0.5 then
        FireFX.IsotropicExplosion(8, 10, 5)
      else
        FireFX.RingExplosion(8, 10, 5, XVector, ZVector);
      // stop the fire trail
      FireFX.Disabled := True;
      // and start the smoke trail
      SmokeFX.Disabled := False;
    end;
  end;
  // A gravity-like acceleration is applied to the sphere
  GetOrCreateInertia(Sphere1).ApplyTranslationAcceleration(deltaTime,
    VectorMake(0, -2, 0));
  // restart effect when sphere fell too low
  if Sphere1.Position.Y < -2 then
    Button1Click(Self);
end;

procedure TFormBoom.Timer1Timer(Sender: TObject);
begin
  // standard issue framerate & particle count update
  Caption := 'Boom - ' + Format('%.1f FPS - %d Particles',
    [GLSceneViewer1.FramesPerSecond, FireFX.ParticleCount +
    SmokeFX.ParticleCount]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormBoom.FormResize(Sender: TObject);
begin
  // take care of zooming if window is resize
  GLCamera1.FocalLength := Width * 0.1;
end;

procedure TFormBoom.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormBoom.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
  begin
    // move around target
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
    GLCadencer1.Progress;
  end;
end;

end.
