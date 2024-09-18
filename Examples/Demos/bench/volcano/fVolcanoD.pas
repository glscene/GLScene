unit fVolcanoD;

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

  
  GLS.Scene,
  GLS.Objects,
  GLS.ParticleFX,
  GLS.Cadencer,
  GLS.Behaviours,
  GLS.SceneViewer,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormVolcano = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    DCVolcano: TGLDummyCube;
    PFXVolcano: TGLPolygonPFXManager;
    GLCadencer1: TGLCadencer;
    PFXRenderer: TGLParticleFXRenderer;
    Timer1: TTimer;
    Sphere1: TGLSphere;
    GLLightSource1: TGLLightSource;
    PFXBlue: TGLPolygonPFXManager;
    DCCamera: TGLDummyCube;
    RadioGroup1: TRadioGroup;
    Panel1: TPanel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Timer1Timer(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  end;

var
  FormVolcano: TFormVolcano;

implementation

{$R *.dfm}

procedure TFormVolcano.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   GLSceneViewer1.Invalidate;
end;

procedure TFormVolcano.Timer1Timer(Sender: TObject);
begin
   Panel1.Caption:=Format('%.1f FPS - %3d Particles - Depth Sort: %.2f msec',
                   [GLSceneViewer1.FramesPerSecond,
                    PFXVolcano.Particles.ItemCount+PFXBlue.Particles.ItemCount,
                    PFXRenderer.LastSortTime]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormVolcano.RadioGroup1Click(Sender: TObject);
var
   source : TGLSourcePFXEffect;
begin
   source:=GetOrCreateSourcePFX(DCVolcano);
   case RadioGroup1.ItemIndex of
      0 : source.ParticleInterval:=0.1;
      1 : source.ParticleInterval:=0.05;
      2 : source.ParticleInterval:=0.02;
      3 : source.ParticleInterval:=0.01;
      4 : source.ParticleInterval:=0.005;
      5 : source.ParticleInterval:=0.001;
   end;
end;

procedure TFormVolcano.FormCreate(Sender: TObject);
begin
   RadioGroup1Click(Self);
end;

end.
 