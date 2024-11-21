unit fInvarianceD;

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

  GLS.Scene,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.Texture,
  GLS.GeomObjects,
  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  Stage.Utils;

type
  TFormInvariance = class(TForm)
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
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormActivate(Sender: TObject);
  private
  public
    mx, my: Integer;
  end;

var
  FormInvariance: TFormInvariance;

implementation

{$R *.dfm}

procedure TFormInvariance.FormActivate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\texture');
  GLMaterialLibrary.TexturePaths := GetCurrentDir();
end;

procedure TFormInvariance.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormInvariance.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera.MoveAroundTarget(my - Y, mx - X);
  if ssRight in Shift then
    GLCamera.MoveTargetInEyeSpace((Y - my) * 0.05, (mx - X) * 0.05, 0);
  mx := X;
  my := Y;
end;

end.
