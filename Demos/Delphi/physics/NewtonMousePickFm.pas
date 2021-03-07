unit NewtonMousePickFm;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.Objects,
  Physics.NGDManager,
  GLS.Coordinates,
  GLS.Cadencer,
  GLS.SceneViewer,
 
  GLS.BaseClasses,
  GLS.VectorGeometry;

type
  TFormNewtonMousePick = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    Floor: TGLCube;
    GLCube1: TGLCube;
    GLNGDManager1: TGLNGDManager;
    GLSphere1: TGLSphere;
    GLCube2: TGLCube;
    GLCube3: TGLCube;
    GLCube4: TGLCube;
    GLLines1: TGLLines;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
     
    point3d, FPaneNormal: TGLVector;
  public
     
    pickjoint: TGLNGDJoint;
  end;

var
  FormNewtonMousePick: TFormNewtonMousePick;

implementation

{$R *.dfm}

procedure TFormNewtonMousePick.FormCreate(Sender: TObject);
begin
  pickjoint := TGLNGDJoint(GLNGDManager1.NewtonJoint.Items[0]);
end;

procedure TFormNewtonMousePick.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLNGDManager1.Step(deltaTime);
end;

procedure TFormNewtonMousePick.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  PickedSceneObject: TGLBaseSceneObject;
begin
  if Button = TMouseButton(mbLeft) then
  begin

    PickedSceneObject := GLSceneViewer1.Buffer.GetPickedObject(X, Y);
    if Assigned(PickedSceneObject) and Assigned
      (GetNGDDynamic(PickedSceneObject)) then
      pickjoint.ParentObject := PickedSceneObject
    else
      exit;

    point3d := VectorMake(GLSceneViewer1.Buffer.PixelRayToWorld(X, Y));
    // Attach the body
    pickjoint.KinematicControllerPick(point3d, paAttach);

    if Assigned(GLSceneViewer1.Camera.TargetObject) then
      FPaneNormal := GLSceneViewer1.Camera.AbsoluteVectorToTarget
    else
      FPaneNormal := GLSceneViewer1.Camera.AbsoluteDirection;
  end;
end;

procedure TFormNewtonMousePick.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  point2d, GotoPoint3d: TGLVector;
begin

  if ssLeft in Shift then
  begin
    // Get the screenPoint with opengl correction [Height - Y] for the next function
    point2d := VectorMake(X, GLSceneViewer1.Height - Y, 0, 0);

    // Get the intersect point between the plane [parallel to camera] and mouse position
    if GLSceneViewer1.Buffer.ScreenVectorIntersectWithPlane(point2d, point3d,
      FPaneNormal, GotoPoint3d) then
      // Move the body to the new position
      pickjoint.KinematicControllerPick(GotoPoint3d, paMove);

  end
  else
    pickjoint.KinematicControllerPick(GotoPoint3d, paDetach);

end;

procedure TFormNewtonMousePick.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Detach the body
  if Button = TMouseButton(mbLeft) then
    pickjoint.KinematicControllerPick(NullHmgVector, paDetach);
end;

end.
