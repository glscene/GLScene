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
  Vcl.StdCtrls,

  GLObjects,
  GLScene,
  GLVectorTypes,
  GLWin32Viewer,
  GLSkydome,
  GLCadencer,
  GLTeapot,
  GLGeomObjects,
  GLContext,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses;

type
  TForm1 = class(TForm)
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
  Form1: TForm1;

//===================================
implementation
//===================================

{$R *.dfm}

procedure TForm1.GenerateCubeMap;
begin
   // Don't do anything if cube maps aren't supported
   if not CubmapSupported then begin
      if not cubeMapWarnDone then
         ShowMessage('Your graphics hardware does not support cube maps...');
      cubeMapWarnDone:=True;
      Exit;
   end;
   // Here we generate the new cube map, from CubeMapCamera (a child of the
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

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
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

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x;
   my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if Shift<>[] then begin
      GLCamera1.MoveAroundTarget(my-y, mx-x);
      mx:=x;
      my:=y;
   end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
   LabelFPS.Caption:=Format('%.1f FPS', [GLSceneViewer1.FramesPerSecond]);
   GLSceneViewer1.ResetPerformanceMonitor;
end;

procedure TForm1.GLSceneViewer1BeforeRender(Sender: TObject);
begin
  CubmapSupported := GL.ARB_texture_cube_map;
  GLSceneViewer1.BeforeRender := nil;
end;

end.
