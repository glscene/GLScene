unit fMeshHitD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,

  GLS.Scene,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.SceneViewer,
  Stage.VectorTypes,
  Stage.VectorGeometry,
  GLS.GeomObjects,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.File3DS,
  Stage.Utils;

type
  TFormMeshHit = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    FreeForm1: TGLFreeForm;
    Sphere1: TGLSphere;
    ArrowLine1: TGLArrowLine;
    GLSceneViewer2: TGLSceneViewer;
    GLCamera2: TGLCamera;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer2MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
  end;

var
  FormMeshHit: TFormMeshHit;

implementation

{$R *.dfm}

procedure TFormMeshHit.FormCreate(Sender: TObject);
begin
  // Load mushroom mesh
  var Path: TFileName := GetCurrentAssetPath();
 	SetCurrentDir(Path + '\model');
  FreeForm1.LoadFromFile('mushroom.3ds');
end;

// Perform the raycasting for the perspective camera & viewer

procedure TFormMeshHit.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  rayStart, rayVector, iPoint, iNormal: TGLVector;
begin
  // retrieve raycasting data:
  // rayStart is obtained for camera and screen position
  // rayVector is the camera direction (i.e direction to target since our camera is targeted)
  // (note that (0, 0) is lower left for the Screen function, whereas Delphi
  // uses top-left as origin, hence the Y inversion)
  SetVector(rayStart, GLSceneViewer1.Buffer.OrthoScreenToWorld(X,
    GLSceneViewer1.Height - Y));
  SetVector(rayVector, GLCamera1.AbsoluteVectorToTarget);
  NormalizeVector(rayVector);
  // Here we require RayCast intersection
  if FreeForm1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then
  begin
    // got one, move the sphere there and orient it appropriately
    Sphere1.Position.AsVector := iPoint;
    Sphere1.Direction.AsVector := VectorNormalize(iNormal);
    // make it visible
    Sphere1.Visible := True;
  end
  else
  begin
    // hide it if we did not hit
    Sphere1.Visible := False;
  end;
end;

procedure TFormMeshHit.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  // when mouse moves, recompute intersection
  if Shift <> [] then
    GLSceneViewer1MouseDown(Sender, TMouseButton(mbLeft), Shift, X, Y);
end;

// Perform the raycasting for the perspective camera & viewer

procedure TFormMeshHit.GLSceneViewer2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  rayStart, rayVector, iPoint, iNormal: TGLVector;
begin
  // retrieve raycasting data:
  // rayStart is the eye (camera) position
  // rayVector is computed from screen position
  // (note that (0, 0) is lower left for the Screen function, whereas Delphi
  // uses top-left as origin, hence the Y inversion)
  SetVector(rayStart, GLCamera2.AbsolutePosition);
  SetVector(rayVector, GLSceneViewer2.Buffer.ScreenToVector(AffineVectorMake(X,
    GLSceneViewer2.Height - Y, 0)));
  NormalizeVector(rayVector);
  // Here we request RayCast intersection
  if FreeForm1.RayCastIntersect(rayStart, rayVector, @iPoint, @iNormal) then
  begin
    // got one, move the sphere there and orient it appropriately
    Sphere1.Position.AsVector := iPoint;
    Sphere1.Direction.AsVector := VectorNormalize(iNormal);
    // make it visible
    Sphere1.Visible := True;
  end
  else
  begin
    // hide it if we did not hit
    Sphere1.Visible := False;
  end;
end;

procedure TFormMeshHit.GLSceneViewer2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift <> [] then
    GLSceneViewer2MouseDown(Sender, TMouseButton(mbLeft), Shift, X, Y);
end;

end.
