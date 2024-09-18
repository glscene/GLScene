unit fSmokingD;

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

  
  GLS.Cadencer, 
  GLS.ParticleFX, 
  GLS.PerlinPFX, 
  GLS.Scene, 
  GLS.Objects,
  GLS.SceneViewer, 
  GLS.Coordinates, 
  GLS.SimpleNavigation, 
  GLS.BaseClasses;

type
  TFormSmoking = class(TForm)
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCamera: TGLCamera;
    DCFire1: TGLDummyCube;
    ParticleFXRenderer: TGLParticleFXRenderer;
    SmokePFX: TGLPerlinPFXManager;
    FlamePFX: TGLCustomSpritePFXManager;
    GLCadencer: TGLCadencer;
    DCTarget: TGLDummyCube;
    Timer: TTimer;
    Panel1: TPanel;
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure TimerTimer(Sender: TObject);
  private
     
  public
     
  end;

var
  FormSmoking: TFormSmoking;

implementation

{$R *.dfm}

procedure TFormSmoking.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   SmokePFX.Rotation:=newTime;
   GLSceneViewer.Invalidate;
end;

procedure TFormSmoking.TimerTimer(Sender: TObject);
begin
   Panel1.Caption := GLSceneViewer.FramesPerSecondText
            +Format(' - %d Particles - %.3f ms Sort',
                    [SmokePFX.ParticleCount+FlamePFX.ParticleCount,
                     ParticleFXRenderer.LastSortTime]);
   GLSceneViewer.ResetPerformanceMonitor;
end;

end.
