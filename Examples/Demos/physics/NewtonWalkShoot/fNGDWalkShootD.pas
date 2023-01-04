unit fNGDWalkShootD;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  GLS.Scene,
  GLS.VectorTypes,
  GLS.VectorLists,
  GLS.PersistentClasses,
  GLS.Objects,
  GLS.Coordinates,
  GLS.Cadencer,
  Physics.NGDManager,
  GLS.SimpleNavigation,
  GLS.SceneViewer,
 
  GLS.BaseClasses,
  GLS.VectorFileObjects,
  GLS.GeomObjects,
  GLS.HUDObjects,
  GLS.File3DS,
  GLS.VectorGeometry,
  GLS.Material,
  GLS.Keyboard,
  GLS.Utils;

type
  TFormNewtonWalkShoot = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLNGDManager1: TGLNGDManager;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    World: TGLDummyCube;
    GLLines1: TGLLines;
    Beer: TGLFreeForm;
    Mushroom: TGLFreeForm;
    Chair: TGLFreeForm;
    Teapot: TGLFreeForm;
    Map: TGLFreeForm;
    GLHUDCross: TGLHUDSprite;
    Body: TGLTorus;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure ScaleMesh(freeform: TGLFreeForm; val: Single);
    procedure MoveCam(const deltaTime, newTime: Double);
    procedure MoveGrab;
  public
    grabJoint: TGLNGDJoint;
  end;

var
  FormNewtonWalkShoot: TFormNewtonWalkShoot;

implementation

{$R *.dfm}

procedure TFormNewtonWalkShoot.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\model');

  // Set Mesh and scale their vertice position
  // because matrix scale is not supported in newton
  Map.LoadFromFile('ngdmap1.3ds');
  Map.Direction.SetVector(0, 1, 0);
  Map.Translate(0, -3, 0);

  Beer.LoadFromFile('beer.3ds');
  ScaleMesh(Beer, 0.25);
  Beer.Translate(-5, 10, 10);

  Mushroom.LoadFromFile('mushroom.3ds');
  ScaleMesh(Mushroom, 0.1);
  Mushroom.Direction.SetVector(0, 1, 0);
  Mushroom.Translate(0, 0, -10);
  Mushroom.Material.FaceCulling := fcNoCull;
  Mushroom.Material.FrontProperties.Emission.SetColor(1, 0.5, 0.5, 0.5);

  Chair.LoadFromFile('ngdchair.3ds');
  Chair.Direction.SetVector(0, 1, 0);
  Chair.Translate(0, 0, -5);

  Teapot.LoadFromFile('teapot.3ds');
  ScaleMesh(Teapot, 0.05);
  Teapot.Direction.SetVector(0, 1, 0);
  Teapot.Translate(0, 0, 10);

  // Create Physic behavior
  GetOrCreateNGDStatic(Map).NGDNewtonCollisions := nc_Tree;
  GetNGDStatic(Map).Manager := GLNGDManager1;

  // nc_Convex use ConvexCollisionTolerance at creation wich is the resolution
  // for collision shape. Bigger the value, lower the collision match,
  // but give higher performance (use mdShowGeometry to see the difference at runtime)
  GetOrCreateNGDDynamic(Chair).NGDNewtonCollisions := nc_Convex;
  GetNGDDynamic(Chair).Manager := GLNGDManager1;

  GetOrCreateNGDDynamic(Beer).NGDNewtonCollisions := nc_Convex;
  GetNGDDynamic(Beer).Manager := GLNGDManager1;

  GetOrCreateNGDDynamic(Teapot).NGDNewtonCollisions := nc_Convex;
  GetNGDDynamic(Teapot).Manager := GLNGDManager1;

  GetOrCreateNGDDynamic(Mushroom).NGDNewtonCollisions := nc_Convex;
  GetNGDDynamic(Mushroom).Manager := GLNGDManager1;

  // Move camera in target
  GLCamera1.Parent := GLCamera1.TargetObject;

  // Create the PickJoint
  grabJoint := TGLNGDJoint.Create(GLNGDManager1.NewtonJoint);
  grabJoint.JointType := nj_KinematicController;
  grabJoint.KinematicControllerOptions.PickModeLinear := True;
  grabJoint.KinematicControllerOptions.LinearFriction := 100;

  // Set some parameters
  GetNGDDynamic(Body).AutoSleep := False;
  GLHUDCross.Height := 2;
  GLHUDCross.Width := 2;
  GLCamera1.NearPlaneBias := 0.1;
  GLCamera1.DepthOfView := 1E15;
end;

procedure TFormNewtonWalkShoot.FormKeyPress(Sender: TObject; var Key: Char);
var
  newFreeform: TGLFreeForm;
  I, rand: Integer;
begin
  if Key = 'x' then
  begin
    newFreeform := TGLFreeForm.CreateAsChild(GLDummyCube1);
    newFreeform.LoadFromFile('HighPolyObject.3ds');
    ScaleMesh(newFreeform, 0.05);

    // Keep only one mesh in this newFreeform
    for I := 0 to newFreeform.MeshObjects.Count - 2 do
    begin
      rand := Random(newFreeform.MeshObjects.Count);
      newFreeform.MeshObjects.Delete(rand);
    end;

    GetOrCreateNGDDynamic(newFreeform).NGDNewtonCollisions := nc_Convex;
    GetNGDDynamic(newFreeform).Manager := GLNGDManager1;
  end;
  if Key = 'c' then
    GetNGDDynamic(Body).NewtonBodyMatrix := IdentityHmgMatrix;
end;

procedure TFormNewtonWalkShoot.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLNGDManager1.Step(deltaTime);
  MoveCam(deltaTime, newTime);
  MoveGrab;

  // Set a point on screen to see where you shoot or grab
  GLHUDCross.Position.X := GLSceneViewer1.Width div 2;
  GLHUDCross.Position.Y := GLSceneViewer1.Height div 2;
end;

procedure TFormNewtonWalkShoot.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Ball: TGLSphere;
  delta: TGLVector;

  PickedSceneObject: TGLBaseSceneObject;
  point3d: TGLVector;
  MyX, MyY: Integer;
begin
  // Shoot a Bullet
  if Button = TMouseButton(mbLeft) then
  begin
    Ball := TGLSphere.CreateAsChild(GLDummyCube1);
    Ball.Radius := 0.1;
    Ball.AbsolutePosition := GLCamera1.TargetObject.AbsolutePosition;
    delta := VectorScale(GLCamera1.AbsoluteVectorToTarget, 2);
    Ball.Translate(delta.X, delta.Y, delta.Z);
    GetOrCreateNGDDynamic(Ball);
    GetNGDDynamic(Ball).Manager := GLNGDManager1;
    GetNGDDynamic(Ball).Density := 100;
    GetNGDDynamic(Ball).LinearDamping := 0.5;
    // Add impulse in the camera direction
    GetNGDDynamic(Ball).AddImpulse
      (VectorScale(GLCamera1.AbsoluteVectorToTarget, 100),
      Ball.AbsolutePosition);
  end;
  // Start Grab
  if Button = TMouseButton(mbMiddle) then
  begin
    MyX := GLSceneViewer1.Width div 2;
    MyY := GLSceneViewer1.Height div 2;
    PickedSceneObject := GLSceneViewer1.Buffer.GetPickedObject(MyX, MyY);

    if Assigned(PickedSceneObject) and Assigned
      (GetNGDDynamic(PickedSceneObject)) then
      grabJoint.ParentObject := PickedSceneObject
    else
      exit;
    point3d := PickedSceneObject.AbsolutePosition;
    // Attach the body
    grabJoint.KinematicControllerPick(point3d, paAttach);
  end;
end;

procedure TFormNewtonWalkShoot.GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Detach the body
  if Button = TMouseButton(mbMiddle) then
    grabJoint.KinematicControllerPick(NullHmgVector, paDetach);
end;

procedure TFormNewtonWalkShoot.ScaleMesh(freeform: TGLFreeForm; val: Single);
var
  I, J: Integer;
begin
  for J := 0 to freeform.MeshObjects.Count - 1 do
    for I := 0 to freeform.MeshObjects[J].Vertices.Count - 1 do
    begin
      freeform.MeshObjects[J].Vertices[I] := VectorScale
        (freeform.MeshObjects[J].Vertices[I], val);
    end;
end;

procedure TFormNewtonWalkShoot.MoveCam(const deltaTime, newTime: Double);
var
  f: TGLVector;
  fup, fdn, flf, frg: TGLVector;
  Bup, Bdn, Blf, Brg: Boolean;
  NGDDyn: TGLNGDDynamic;
begin
  Bup := IsKeyDown('w') or IsKeyDown('z') or IsKeyDown(VK_UP);
  Bdn := IsKeyDown('s') or IsKeyDown(VK_DOWN);
  Blf := IsKeyDown('d') or IsKeyDown(VK_LEFT);
  Brg := IsKeyDown('a') or IsKeyDown('q') or IsKeyDown(VK_RIGHT);

  if Bup then
    fup := GLCamera1.AbsoluteVectorToTarget
  else
    fup := VectorMake(0, 0, 0, 0);

  if Bdn then
    fdn := VectorNegate(GLCamera1.AbsoluteVectorToTarget)
  else
    fdn := VectorMake(0, 0, 0, 0);

  if Blf then
    frg := GLCamera1.AbsoluteRightVectorToTarget
  else
    frg := VectorMake(0, 0, 0, 0);

  if Brg then
    flf := VectorNegate(GLCamera1.AbsoluteRightVectorToTarget)
  else
    flf := VectorMake(0, 0, 0, 0);

  NGDDyn := GetNGDDynamic(GLCamera1.TargetObject);

  if Bup or Bdn or Blf or Brg then
  begin
    // Add every vector
    f := VectorAdd(fup, fdn);
    f := VectorAdd(f, frg);
    f := VectorAdd(f, flf);
    f.Y := 0; // Do not allow the body to go up or down
    NormalizeVector(f);
    // Move the body
    if NGDDyn.Force.VectorLength < 5 then //before - AppliedVelocity
      NGDDyn.AddImpulse(f, GLCamera1.TargetObject.AbsolutePosition);
  end
  else
  begin
    // Slow down the body if the user stop pushing keys
    if NGDDyn.Force.VectorLength > 3 then //before - AppliedVelocity
      NGDDyn.AddImpulse(VectorScale(NGDDyn.AppliedForce.AsVector, 0.5), f); //before - SetVector
  end;

end;

procedure TFormNewtonWalkShoot.MoveGrab;
var
  point3d: TGLVector;
  delta: TGLVector;
begin
  // Move the object in 3 unit front of GLCamera1.TargetObject
  if IsKeyDown(VK_MBUTTON) then
  begin
    point3d := GLCamera1.TargetObject.AbsolutePosition;
    delta := GLCamera1.AbsoluteVectorToTarget;
    while VectorLength(delta) < 3 do
      delta := VectorScale(delta, 2);
    point3d := VectorAdd(point3d, delta);
    grabJoint.KinematicControllerPick(point3d, paMove);
  end;
end;

end.
