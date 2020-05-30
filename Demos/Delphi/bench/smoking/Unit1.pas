unit Unit1;

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

  
  GLCadencer, GLParticleFX, GLPerlinPFX, GLScene, GLObjects,
  GLWin32Viewer, GLCrossPlatform, GLCoordinates, GLSimpleNavigation, GLBaseClasses;

type
  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   SmokePFX.Rotation:=newTime;
   GLSceneViewer.Invalidate;
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
   Panel1.Caption := GLSceneViewer.FramesPerSecondText
            +Format(' - %d Particles - %.3f ms Sort',
                    [SmokePFX.ParticleCount+FlamePFX.ParticleCount,
                     ParticleFXRenderer.LastSortTime]);
   GLSceneViewer.ResetPerformanceMonitor;
end;

end.
