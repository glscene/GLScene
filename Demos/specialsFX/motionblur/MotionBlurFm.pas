unit MotionBlurFm;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.Controls,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.Context,
  GLS.SceneViewer,
  GLS.Cadencer,
  GLS.Objects,
  GLS.Texture,
  GLS.HUDObjects,
  GLS.GeomObjects,
  GLS.Utils,
 
  GLS.Coordinates,
  GLS.BaseClasses, GLS.VectorFileObjects;

type
  TFormMotionBlur = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    Camera: TGLCamera;
    GLCadencer1: TGLCadencer;
    Light: TGLLightSource;
    Cube: TGLCube;
    HUD: TGLHUDSprite;
    Torus: TGLTorus;
    Timer1: TTimer;
    Dodecahedron: TGLDodecahedron;
    DummyCube: TGLDummyCube;
    Panel1: TPanel;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewerPostRender(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure GLSceneViewerMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
     
  public
     
    Frames : Integer;
    mx, my : Integer;
  end;

var
  FormMotionBlur: TFormMotionBlur;

implementation

{$R *.dfm}

procedure TFormMotionBlur.FormCreate(Sender: TObject);
begin
   Frames:=5;
   HUD.Material.FrontProperties.Diffuse.Alpha:=1-1/Frames;
end;

procedure TFormMotionBlur.GLSceneViewerPostRender(Sender: TObject);
begin
   // render is done, we transfer it to our hud plane so it can be used
   // in the next frame
   GLSceneViewer.Buffer.CopyToTexture(HUD.Material.Texture);
end;

procedure TFormMotionBlur.FormResize(Sender: TObject);
var
   w, h : Integer;
begin
   // Here we resize our texture and plane to follow window dimension changes
   // Note that we have to stick to power of two texture dimensions if we don't
   // want performance to drop dramatically, this implies we can waste 3/4
   // of our texture memory... (f.i. a 513x513 window will require and use
   // a 1024x1024 texture)
   w:=RoundUpToPowerOf2(GLSceneViewer.Width);
   h:=RoundUpToPowerOf2(GLSceneViewer.Height);
   HUD.Material.Texture.DestroyHandles;
   with ((HUD.Material.Texture.Image) as TGLBlankImage) do begin
      Width:=w;
      Height:=h;
   end;
   HUD.Position.X:=w*0.5;
   HUD.Position.Y:=GLSceneViewer.Height-h*0.5;
   HUD.Width:=w;
   HUD.Height:=h;
end;

procedure TFormMotionBlur.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // make things move
   Cube.TurnAngle:=newTime*90;
   DummyCube.PitchAngle:=newTime*60;
   Dodecahedron.RollAngle:=newTime*15;
end;

procedure TFormMotionBlur.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   // turn on/off VSync, this has an obvious impact on framerate,
   // which in turns impacts the motion blur look
   if (Key=Ord('S')) or (Key=Ord('V')) then
      if GLSceneViewer.VSync=vsmNoSync then
         GLSceneViewer.VSync:=vsmSync
      else GLSceneViewer.VSync:=vsmNoSync;

   // change the number of motion blur frames, and adjust
   // the transparency of the plane accordingly
   if Key=VK_UP then Inc(Frames);
   if (Key=VK_DOWN) and (Frames>0) then Dec(Frames);
   if Frames=0 then
      HUD.Visible:=False
   else begin
      HUD.Visible:=True;
      HUD.Material.FrontProperties.Diffuse.Alpha:=1-1/(1+Frames);
   end;
end;

// standard issue camera movement

procedure TFormMotionBlur.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TFormMotionBlur.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift=[ssLeft] then
      Camera.MoveAroundTarget(my-y, mx-x);
   mx:=x; my:=y;
end;

procedure TFormMotionBlur.Timer1Timer(Sender: TObject);
const
   cVSync : array [vsmSync..vsmNoSync] of String = ('VSync ON', 'VSync OFF');
begin
   Panel1.Caption:=Format('Motion Blur on %d frames | %s | %f FPS',
                   [frames, cVSync[GLSceneViewer.VSync], GLSceneViewer.FramesPerSecond]);
   GLSceneViewer.ResetPerformanceMonitor;
end;


end.
