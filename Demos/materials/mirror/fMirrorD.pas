unit fMirrorD;

interface

uses
  System.SysUtils,
  System.Classes, 
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms,
  Vcl.ExtCtrls, 
  Vcl.StdCtrls,
  
  
  GLS.BaseClasses, 
  GLS.Scene, 
  GLS.Objects, 
  GLS.Extrusion, 
  GLS.Mirror,
  GLS.Cadencer, 
  GLS.SceneViewer, 
  GLS.GeomObjects,
  GLS.Coordinates, 
 
  GLS.MultiPolygon;

type
  TFormMirror = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
    ReflectingObjects: TGLDummyCube;
    Cylinder: TGLTorus;
    Teapot1: TGLTeapot;
    Cylinder1: TGLCylinder;
    GLMirror: TGLMirror;
    Cadre: TGLExtrusionSolid;
    Timer1: TTimer;
    GLCadencer1: TGLCadencer;
    Panel1: TPanel;
    LabelFPS: TLabel;
    CBOpaque: TCheckBox;
    CBStencil: TCheckBox;
    Cylinder2: TGLCylinder;
    CBClearZ: TCheckBox;
    CylinderThroughMirror: TGLCylinder;
    CBPlaneClip: TCheckBox;
    DCNonReflectingStuff: TGLDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure CBOpaqueClick(Sender: TObject);
    procedure CBStencilClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CBClearZClick(Sender: TObject);
    procedure CBPlaneClipClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
     
  public
     
    mx, my: Integer;
  end;

var
  FormMirror: TFormMirror;

implementation

{$R *.DFM}

procedure TFormMirror.FormCreate(Sender: TObject);
begin
  CBClearZClick(Self);
  CBOpaqueClick(Self);
end;

//
// Those events simply add/remove one of the mirror options
// when the related checkbox is clicked
//

procedure TFormMirror.CBOpaqueClick(Sender: TObject);
begin
  if CBOpaque.Checked then
    GLMirror.MirrorOptions := GLMirror.MirrorOptions + [moOpaque]
  else
    GLMirror.MirrorOptions := GLMirror.MirrorOptions - [moOpaque];
end;

procedure TFormMirror.CBStencilClick(Sender: TObject);
begin
  if CBStencil.Checked then
    GLMirror.MirrorOptions := GLMirror.MirrorOptions + [moUseStencil]
  else
    GLMirror.MirrorOptions := GLMirror.MirrorOptions - [moUseStencil];
end;

procedure TFormMirror.CBClearZClick(Sender: TObject);
begin
  if CBClearZ.Checked then
    GLMirror.MirrorOptions := GLMirror.MirrorOptions + [moClearZBuffer]
  else
    GLMirror.MirrorOptions := GLMirror.MirrorOptions - [moClearZBuffer];
end;

procedure TFormMirror.CBPlaneClipClick(Sender: TObject);
begin
  if CBPlaneClip.Checked then
    GLMirror.MirrorOptions := GLMirror.MirrorOptions + [moMirrorPlaneClip]
  else
    GLMirror.MirrorOptions := GLMirror.MirrorOptions - [moMirrorPlaneClip];
end;

//
// Standard-issue move around target code
//

procedure TFormMirror.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormMirror.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

//
// Standard issue resize/zoom, timer and viewr invalidation (to force redraws)
//
procedure TFormMirror.FormResize(Sender: TObject);
begin
  GLCamera1.SceneScale := GLSceneViewer1.Width / 380;
end;

procedure TFormMirror.Timer1Timer(Sender: TObject);
begin
  LabelFPS.Caption := Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
  GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormMirror.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;
end;

end.
