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
  Vcl.Imaging.Jpeg,
  
  GLScene, GLObjects, GLWin32Viewer, GLTexture,
  GLGeomObjects, GLCrossPlatform, GLMaterial, GLCoordinates, GLBaseClasses,
  GLUtils;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera: TGLCamera;
    DCCamera: TGLDummyCube;
    PLGround: TGLPlane;
    GLLightSource1: TGLLightSource;
    GLMaterialLibrary: TGLMaterialLibrary;
    GLCube1: TGLCube;
    DCPositionInvariant: TGLDummyCube;
    GLCylinder1: TGLCylinder;
    GLArrowLine1: TGLArrowLine;
    DCOrientationInvariant: TGLDummyCube;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormActivate(Sender: TObject);
  private
     
  public
     
    mx, my : Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormActivate(Sender: TObject);
begin
  SetGLSceneMediaDir();
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   mx:=x; my:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
      GLCamera.MoveAroundTarget(my-y, mx-x);
   if ssRight in Shift then
      GLCamera.MoveTargetInEyeSpace((y-my)*0.05, (mx-x)*0.05, 0);
   mx:=x; my:=y;
end;

end.
