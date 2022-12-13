unit fShadowPlaneD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.Jpeg,

  GLS.ShadowPlane,
  GLS.Scene,
  GLS.SceneViewer,
  GLS.Objects,
  GLS.Cadencer,
  GLS.VectorGeometry,
  GLS.Texture,
  GLS.GeomObjects,
 
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Utils;

type
  TFormShadowPlane = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    DCShadowing: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Cube1: TGLCube;
    Sphere1: TGLSphere;
    GLCamera1: TGLCamera;
    GLCadencer1: TGLCadencer;
    DCLight: TGLDummyCube;
    Sphere2: TGLSphere;
    Torus1: TGLTorus;
    DCCameraTarget: TGLDummyCube;
    GLShadowPlane1: TGLShadowPlane;
    Timer1: TTimer;
    Panel1: TPanel;
    CBShadows: TCheckBox;
    CBStencil: TCheckBox;
    GLShadowPlane2: TGLShadowPlane;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLShadowPlane3: TGLShadowPlane;
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CBShadowsClick(Sender: TObject);
    procedure CBStencilClick(Sender: TObject);
  private
     
  public
     
  end;

var
  FormShadowPlane: TFormShadowPlane;

implementation

{$R *.dfm}

procedure TFormShadowPlane.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\texture');

  GLMaterialLibrary.TexturePaths := GetCurrentDir;
  GLMaterialLibrary.Materials[0].Material.Texture.Image.LoadFromFile('BeigeMarble.jpg');
end;

procedure TFormShadowPlane.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   DCLight.PitchAngle:=Sin(newTime)*60;
   DCShadowing.TurnAngle:=newTime*10;
end;

procedure TFormShadowPlane.CBShadowsClick(Sender: TObject);
begin
   if CBShadows.Checked then
      GLShadowPlane1.ShadowedLight:=GLLightSource1
   else GLShadowPlane1.ShadowedLight:=nil;
   GLShadowPlane2.ShadowedLight:=GLShadowPlane1.ShadowedLight;
   GLShadowPlane3.ShadowedLight:=GLShadowPlane1.ShadowedLight;
end;

procedure TFormShadowPlane.CBStencilClick(Sender: TObject);
begin
   if CBStencil.Checked then
      GLShadowPlane1.ShadowOptions:=[spoUseStencil, spoScissor]
   else GLShadowPlane1.ShadowOptions:=[spoScissor];
   GLShadowPlane2.ShadowOptions:=GLShadowPlane1.ShadowOptions;
   GLShadowPlane3.ShadowOptions:=GLShadowPlane1.ShadowOptions;
end;

procedure TFormShadowPlane.Timer1Timer(Sender: TObject);
begin
   Caption := 'Shadow Plane - ' + Format('%.1f FPS',
     [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

end.
