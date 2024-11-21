unit fDynCubemapD;

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

  GLS.Objects,
  GLS.Scene,
  Stage.VectorTypes,
  GLS.SceneViewer,
  GLS.SkyDome,
  GLS.Cadencer,
  GLS.GeomObjects,
  GLS.Context,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormDynCubeMap = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLMemoryViewer1: TGLMemoryViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Torus1: TGLTorus;
    CubeMapCamera: TGLCamera;
    Sphere1: TGLSphere;
    Cylinder1: TGLCylinder;
    Teapot1: TGLTeapot;
    SkyDome1: TGLSkyDome;
    Cube1: TGLCube;
    GLCadencer1: TGLCadencer;
    Timer1: TTimer;
    Panel1: TPanel;
    CBDynamic: TCheckBox;
    LabelFPS: TLabel;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure GLSceneViewer1BeforeRender(Sender: TObject);
  private
    procedure GenerateCubeMap;
  public
    mx, my : Integer;
    CubmapSupported : Boolean;
    cubeMapWarnDone : Boolean;
  end;

var
  FormDynCubeMap: TFormDynCubeMap;

//===================================
implementation
//===================================

{$R *.dfm}

procedure TFormDynCubeMap.GenerateCubeMap;
begin
   // teapot in the scene hierarchy)
   with Teapot1 do begin
      // hide the teapot while rendering the cube map
      Visible:=False;
      // render cube map to the teapot's texture
      GLMemoryViewer1.RenderCubeMapTextures(Material.Texture);
      // teapot visible again
      Material.Texture.Disabled:=False;
      Visible:=True;
   end;
end;

procedure TFormDynCubeMap.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   if CBDynamic.Checked then begin
      // make things move
      Teapot1.Position.Y:=2*Sin(newTime);
      Torus1.RollAngle:=newTime*15;
      // generate the cube map
      GenerateCubeMap;
   end;
   GLSceneViewer1.Invalidate;
end;

// Standard issue mouse movement

procedure TFormDynCubeMap.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x;
   my:=y;
end;

procedure TFormDynCubeMap.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x;
      my:=y;
   end;
end;

procedure TFormDynCubeMap.Timer1Timer(Sender: TObject);
begin
   LabelFPS.Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TFormDynCubeMap.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  CubmapSupported := GL.ARB_texture_cube_map;
  GLSceneViewer1.BeforeRender := nil;
end;

end.
