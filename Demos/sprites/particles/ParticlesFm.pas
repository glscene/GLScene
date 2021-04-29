unit ParticlesFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Controls,

  
  GLS.Scene,
  GLS.Objects,
  GLS.Particles,
  GLS.Behaviours,
  GLS.VectorGeometry,
  GLS.PersistentClasses,
  GLS.Cadencer,
  GLS.VectorTypes,
  GLS.SceneViewer,
 
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Utils;

type
  TFormParticles = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLParticles1: TGLParticles;
    Sprite1: TGLSprite;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    procedure GLParticles1ActivateParticle(Sender: TObject;
      particle: TGLBaseSceneObject);
    procedure Sprite1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  public
  end;

var
  FormParticles: TFormParticles;

implementation

{$R *.DFM}

procedure TFormParticles.FormCreate(Sender: TObject);
var
  MediaPath: String;
begin
  SetGLSceneMediaDir;
  MediaPath := GetCurrentDir + '\';
  Sprite1.Material.Texture.Image.LoadFromFile(MediaPath + 'Flare1.bmp');
  // if we don't do this, our random won't look like random
  Randomize;
end;

procedure TFormParticles.GLParticles1ActivateParticle(Sender: TObject;
  particle: TGLBaseSceneObject);
begin
  // this event is called when a particle is activated,
  // ie. just before it will be rendered we pick a random color
  TGLSprite(Particle).Material.FrontProperties.Emission.Color := PointMake(Random, Random, Random);
  // our halo starts transparent
  TGLSprite(Particle).Material.FrontProperties.Diffuse.Alpha := 0;
  // this is our "birth time"
  TGLSprite(Particle).TagFloat := GLCadencer1.CurrentTime;
end;

procedure TFormParticles.Sprite1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  life: Double;
begin
  with TGLSprite(Sender) do
  begin
    // calculate for how long we've been living
    life := (newTime - TagFloat);
    if life > 10 then
      // old particle to kill
      GLParticles1.KillParticle(TGLSprite(Sender))
    else if life < 1 then
      // baby particles become brighter in their 1st second of life...
      Material.FrontProperties.Diffuse.Alpha := life
    else // ...and slowly disappear in the darkness
      Material.FrontProperties.Diffuse.Alpha := (9 - life) / 9;
  end;
end;

procedure TFormParticles.Timer1Timer(Sender: TObject);
begin
  // every timer, we create a particle at a random position
  with TGLSprite(GLParticles1.CreateParticle).Position do
  begin
    X := 3 * (Random - 0.5);
    Y := 3 * (Random - 0.5);
    Z := 3 * (Random - 0.5);
  end;
  // infos for the user
  Caption := 'Particles - ' + Format('%d particles, %.1f FPS',
    [GLParticles1.Count - 1, GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormParticles.FormResize(Sender: TObject);
begin
  // change focal so the view will shrink and not just get clipped
  GLCamera1.FocalLength := 50 * Width / 280;
end;

end.
