unit SpiralFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Buttons,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  
  GLS.Scene,
  GLS.Objects,
  GLS.VectorTypes,
  GLS.ParticleFX,
  GLS.Cadencer,
  GLS.SceneViewer,
  GLS.Behaviours,
  GLS.VectorGeometry,
 
  GLS.FullScreenViewer,
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormSpiral = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    DCBase: TGLDummyCube;
    DCSrc: TGLDummyCube;
    PFXSpiral: TGLPolygonPFXManager;
    GLCadencer: TGLCadencer;
    PFXRenderer: TGLParticleFXRenderer;
    GLCamera: TGLCamera;
    Timer: TTimer;
    PFXRing: TGLPolygonPFXManager;
    GLFullScreenViewer: TGLFullScreenViewer;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    procedure TimerTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure GLSceneViewerDblClick(Sender: TObject);
    procedure GLFullScreenViewerDblClick(Sender: TObject);
    procedure GLFullScreenViewerKeyPress(Sender: TObject; var Key: Char);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
     
  public
     
  end;

var
  FormSpiral: TFormSpiral;

implementation

{$R *.dfm}

procedure TFormSpiral.TimerTimer(Sender: TObject);
begin
   // Display FPS and particle count in the form's caption
   Caption:=Format('%.1f FPS - %d Particles',
                   [GLSceneViewer.FramesPerSecond,
                    PFXRing.Particles.ItemCount+PFXSpiral.Particles.ItemCount]);
   GLSceneViewer.ResetPerformanceMonitor;

   // Alternatively trigger a "sphere" or "ring" explosion
   //
   Timer.Tag:=Timer.Tag+1;
   if (Timer.Tag and 1)<>0 then begin
      // "Sphere" explosion
      with GetOrCreateSourcePFX(DCBase) do begin
         VelocityDispersion:=1.5;
         Burst(GLCadencer.CurrentTime, 200);
         VelocityDispersion:=0;
      end;
   end else begin
      // Ring explosion
      GetOrCreateSourcePFX(DCBase).RingExplosion(GLCadencer.CurrentTime, 1, 1.2, 150);
   end;
end;

procedure TFormSpiral.FormResize(Sender: TObject);
begin
   // Rescale when window is resized
   GLCamera.SceneScale:=GLSceneViewer.Height/350;
end;

procedure TFormSpiral.GLSceneViewerDblClick(Sender: TObject);
begin
   // Switch to full-screen, but using the same screen resolution
   // If you uncomment the line below, it will switch to 800x600x32
   GLFullScreenViewer.UseCurrentResolution;
   GLFullScreenViewer.Active:=True;
   // Hide the windows viewer so it is no longer updated
   GLSceneViewer.Visible:=False;
   // Apply proper scale
   GLCamera.SceneScale:=GLFullScreenViewer.Height/350;
end;

procedure TFormSpiral.GLFullScreenViewerDblClick(Sender: TObject);
begin
   // Make the windows viewer visible again
   GLSceneViewer.Visible:=True;
   // Deactivate full-screen mode
   GLFullScreenViewer.Active:=False;
   // And apply back the adequate scale for the SceneViewer
   FormResize(Self);
end;

procedure TFormSpiral.GLFullScreenViewerKeyPress(Sender: TObject;
  var Key: Char);
begin
   // Hitting 'ESC' has same effect as a double-click
   // (the FullScreenViewer has several Form-like events)
   if Key=#27 then begin
      GLFullScreenViewerDblClick(Self);
      Key:=#0;
   end;
end;

procedure TFormSpiral.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   // Mouse moved in the Viewer (windowed or fullscreen mode)
   if ssRight in Shift then
      GLCamera.Position.Y:=(GLSceneViewer.Height div 2-Y)*0.1/GLCamera.SceneScale;
   // Ensures we don't flood the event system with mouse moves (high priority events)
   // and then prevent the cadencer from progressing (low priority events)
   GLCadencer.Progress;
end;

end.
