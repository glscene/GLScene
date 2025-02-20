//
// The graphics engine GLXEngine. The unit of GXScene for Delphi
//
unit GXS.ODERagdoll;

(* Ragdoll extended using Open Dynamics Engine (ODE) *)

interface

uses
  Stage.VectorTypes,
  Stage.VectorGeometry,
  GXS.Ragdoll,
  GXS.Scene,
  GXS.Objects,
  GXS.Texture,
  GXS.VectorFileObjects,

  ODE.Import,
  GXS.ODEUtils;

const
  cMaxContacts = 4;

type

  TgxODERagdoll = class;
  TgxODERagdollBone = class;

  TgxODERagdollCube = class(TgxCube)
  public
    Bone: TgxODERagdollBone; // Useful in Oncollision Event
    Ragdoll: TgxODERagdoll; // Useful in Oncollision Event
  end;

  TgxODERagdollWorld = class
  private
    FSpace: PdxSpace;
    FWorld: PdxWorld;
    FContactGroup: TdJointGroupID;
    FRagdoll: TgxODERagdoll;
    isWorldCreated: Boolean; // NEW1
  public
    constructor Create;
    // Create the world from any existing ODE world
    constructor CreateFrom(World: PdxWorld; Space: PdxSpace;
      ContactGroup: TdJointGroupID);
    destructor Destroy; override;
    procedure WorldUpdate;
    property World: PdxWorld read FWorld;
    property Space: PdxSpace read FSpace;
    property ContactGroup: TdJointGroupID read FContactGroup;
    property Ragdoll: TgxODERagdoll read FRagdoll;
  end;

  TgxODERagdollDummyJoint = class(TgxRagdolJoint)
  end;

  TgxODERagdollHingeJoint = class(TgxRagdolJoint)
  private
    FParamHiStop: Single;
    FParamLoStop: Single;
    FAxis: TAffineVector;
  public
    constructor Create(Axis: TAffineVector; ParamLoStop: Single;
      ParamHiStop: Single);
    property Axis: TAffineVector read FAxis;
    property ParamLoStop: Single read FParamLoStop write FParamLoStop;
    property ParamHiStop: Single read FParamHiStop write FParamHiStop;
  end;

  TgxODERagdollUniversalJoint = class(TgxODERagdollHingeJoint)
  private
    FParamHiStop2: Single;
    FParamLoStop2: Single;
    FAxis2: TAffineVector;
  public
    constructor Create(Axis: TAffineVector; ParamLoStop: Single;
      ParamHiStop: Single; Axis2: TAffineVector; ParamLoStop2: Single;
      ParamHiStop2: Single);
    property Axis2: TAffineVector read FAxis2;
    property ParamLoStop2: Single read FParamLoStop2 write FParamLoStop2;
    property ParamHiStop2: Single read FParamHiStop2 write FParamHiStop2;
  end;

  TgxODERagdollBone = class(TgxRagdolBone)
  private
    FOwner: TgxODERagdollBone;
    FRagdoll: TgxODERagdoll;
    FBody: PdxBody;
    FGeom: PdxGeom;
    FJointId: TdJointID;
    procedure AlignBodyToMatrix(Mat: TMatrix4f);
  protected
    procedure Start; override;
    procedure Align; override;
    procedure Update; override;
    procedure Stop; override;
  public
    constructor CreateOwned(aOwner: TgxODERagdollBone);
    constructor Create(Ragdoll: TgxODERagdoll);
    property Body: PdxBody read FBody;
    property Geom: PdxGeom read FGeom;
  end;

  TgxODERagdoll = class(TgxRagdoll)
  private
    FODEWorld: TgxODERagdollWorld;
    FGLXceneRoot: TgxBaseSceneObject;
    FShowBoundingBoxes: Boolean;
  public
    constructor Create(aOwner: TgxBaseMesh);
    property ODEWorld: TgxODERagdollWorld read FODEWorld write FODEWorld;
    property GLXceneRoot: TgxBaseSceneObject read FGLXceneRoot
      write FGLXceneRoot;
    property ShowBoundingBoxes: Boolean read FShowBoundingBoxes
      write FShowBoundingBoxes;
  end;

var
  vODERagdoll_cDensity: Single;
  vODERagdoll_cMass: Single;

// ------------------------------------------------------------------------
implementation
// ------------------------------------------------------------------------

procedure ODERagdollCallback(data: pointer; o1, o2: PdxGeom); cdecl;
var
  i, n: integer;
  b1, b2: PdxBody;
  c: TdJointID;
  contact: Array [0 .. cMaxContacts - 1] of TdContact;
begin
  b1 := dGeomGetBody(o1);
  b2 := dGeomGetBody(o2);
  if (assigned(b1) and assigned(b2) and (dAreConnected(b1, b2) <> 0)) then
    exit;
  n := dCollide(o1, o2, cMaxContacts, contact[0].Geom, sizeof(TdContact));
  if (n > 0) then
  begin
    for i := 0 to n - 1 do
    begin
      contact[i].surface.mode := ord(dContactBounce) or ord(dContactSoftCFM) or
        ord(dContactSlip1) or ord(dContactSlip2);
      contact[i].surface.mu := 10E9;
      contact[i].surface.mu2 := 0;
      contact[i].surface.soft_cfm := 0.001;
      contact[i].surface.bounce := 0.15;
      contact[i].surface.bounce_vel := 0.2;
      contact[i].surface.slip1 := 0.1;
      contact[i].surface.slip2 := 0.1;
      c := dJointCreateContact(TgxODERagdollWorld(data).World,
        TgxODERagdollWorld(data).ContactGroup, @contact[i]);
      dJointAttach(c, dGeomGetBody(contact[i].Geom.g1),
        dGeomGetBody(contact[i].Geom.g2));
    end;
  end;
end;

// ------------------------------------------
// TgxODERagdollWorld
// ------------------------------------------

constructor TgxODERagdollWorld.Create;
begin
  // Create default physics
  FWorld := dWorldCreate();
  dWorldSetQuickStepNumIterations(FWorld, 8);
  FSpace := dHashSpaceCreate(nil);
  FContactGroup := dJointGroupCreate(0);
  dWorldSetGravity(FWorld, 0, 0, -0.81);
  dWorldSetCFM(FWorld, 1E-5);
  isWorldCreated := True; // NEW1
end;

constructor TgxODERagdollWorld.CreateFrom(World: PdxWorld; Space: PdxSpace;
  ContactGroup: TdJointGroupID);
begin
  FWorld := World;
  FSpace := Space;
  FContactGroup := ContactGroup;
  isWorldCreated := False; // NEW1
end;

destructor TgxODERagdollWorld.Destroy;
begin
  if isWorldCreated then
  begin
    dJointGroupDestroy(FContactGroup);
    dSpaceDestroy(FSpace);
    dWorldDestroy(FWorld);
  end;
  inherited;
end;

procedure TgxODERagdollWorld.WorldUpdate;
const
  cDeltaTime = 1 / 50;
begin
  // Update the physic
  dSpaceCollide(FSpace, Self, ODERagdollCallback);
  dWorldQuickStep(FWorld, cDeltaTime);
  // remove all contact joints
  dJointGroupEmpty(FContactGroup);
end;

//------------------------------------
// TgxODERagdollHingeJoint
//------------------------------------

constructor TgxODERagdollHingeJoint.Create(Axis: TAffineVector;
  ParamLoStop, ParamHiStop: Single);
begin
  inherited Create;
  FAxis := Axis;
  FParamLoStop := ParamLoStop;
  FParamHiStop := ParamHiStop;
end;

//------------------------------------
// TgxODERagdollUniversalJoint
//------------------------------------

constructor TgxODERagdollUniversalJoint.Create(Axis: TAffineVector;
  ParamLoStop, ParamHiStop: Single; Axis2: TAffineVector;
  ParamLoStop2, ParamHiStop2: Single);
begin
  inherited Create(Axis, ParamLoStop, ParamHiStop);
  FAxis2 := Axis2;
  FParamLoStop := ParamLoStop;
  FParamHiStop := ParamHiStop;
  FParamLoStop2 := ParamLoStop2;
  FParamHiStop2 := ParamHiStop2;
end;

//------------------------------------
// TgxODERagdollBone
//------------------------------------

constructor TgxODERagdollBone.Create(Ragdoll: TgxODERagdoll);
begin
  inherited Create(Ragdoll);
  FRagdoll := Ragdoll;
end;

constructor TgxODERagdollBone.CreateOwned(aOwner: TgxODERagdollBone);
begin
  inherited CreateOwned(aOwner);
  FOwner := aOwner;
  FRagdoll := aOwner.FRagdoll;
end;

procedure TgxODERagdollBone.AlignBodyToMatrix(Mat: TMatrix4f); // By Stuart Gooding
var
  R: TdMatrix3;
begin
  if not assigned(FBody) then
    exit;
  R[0] := Mat.X.X;
  R[1] := Mat.Y.X;
  R[2] := Mat.Z.X;
  R[3] := 0;
  R[4] := Mat.X.Y;
  R[5] := Mat.Y.Y;
  R[6] := Mat.Z.Y;
  R[7] := 0;
  R[8] := Mat.X.Z;
  R[9] := Mat.Y.Z;
  R[10] := Mat.Z.Z;
  R[11] := 0;
  dBodySetRotation(FBody, R);
  dBodySetPosition(FBody, Mat.W.X, Mat.W.Y, Mat.W.Z);
end;

procedure TgxODERagdollBone.Start;
var
  mass: TdMass;
  boneSize, vAxis, vAxis2: TAffineVector;
  n: integer;

  function RotateAxis(Axis: TAffineVector): TAffineVector;
  var
    absMat: TMatrix4f;
  begin
    absMat := ReferenceMatrix;
    absMat.W := NullHmgVector;
    Result := VectorNormalize(VectorTransform(Axis, absMat));
  end;

begin
  FBody := dBodyCreate(FRagdoll.ODEWorld.World);

  boneSize.X := Size.X * VectorLength(BoneMatrix.X);
  boneSize.Y := Size.Y * VectorLength(BoneMatrix.Y);
  boneSize.Z := Size.Z * VectorLength(BoneMatrix.Z);

  // prevent ODE 0.9 "bNormalizationResult failed" error:
  for n := 0 to 2 do
    if (boneSize.V[n] = 0) then
      boneSize.V[n] := 0.000001;
  dMassSetBox(mass, vODERagdoll_cDensity, boneSize.X, boneSize.Y, boneSize.Z);
  dMassAdjust(mass, vODERagdoll_cMass);
  dBodySetMass(FBody, @mass);
  AlignBodyToMatrix(ReferenceMatrix);
  FGeom := dCreateBox(FRagdoll.ODEWorld.Space, boneSize.X, boneSize.Y, boneSize.Z);
  FGeom.data := FRagdoll.GLXceneRoot.AddNewChild(TgxODERagdollCube);
  if (Joint is TgxODERagdollDummyJoint) then
    dGeomSetBody(FGeom, FOwner.Body)
  else
    dGeomSetBody(FGeom, FBody);
  if (Owner <> nil) then
  begin
    if (Joint is TgxODERagdollHingeJoint) then
      with (Joint as TgxODERagdollHingeJoint) do
      begin
        vAxis := RotateAxis(Axis);
        FJointId := dJointCreateHinge(FRagdoll.ODEWorld.World, nil);
        dJointAttach(FJointId, TgxODERagdollBone(Owner).Body, FBody);
        dJointSetHingeAnchor(FJointId, Anchor.X, Anchor.Y, Anchor.Z);
        dJointSetHingeAxis(FJointId, vAxis.X, vAxis.Y, vAxis.Z);
        dJointSetHingeParam(FJointId, dParamLoStop, ParamLoStop);
        dJointSetHingeParam(FJointId, dParamHiStop, ParamHiStop);
      end;
    if (Joint is TgxODERagdollUniversalJoint) then
      with (Joint as TgxODERagdollUniversalJoint) do
      begin
        vAxis := RotateAxis(Axis);
        vAxis2 := RotateAxis(Axis2);
        FJointId := dJointCreateUniversal(FRagdoll.ODEWorld.World, nil);
        dJointAttach(FJointId, TgxODERagdollBone(Owner).Body, FBody);
        dJointSetUniversalAnchor(FJointId, Anchor.X, Anchor.Y, Anchor.Z);
        dJointSetUniversalAxis1(FJointId, vAxis.X, vAxis.Y, vAxis.Z);
        dJointSetUniversalAxis2(FJointId, vAxis2.X, vAxis2.Y, vAxis2.Z);
        dJointSetUniversalParam(FJointId, dParamLoStop, ParamLoStop);
        dJointSetUniversalParam(FJointId, dParamHiStop, ParamHiStop);
        dJointSetUniversalParam(FJointId, dParamLoStop2, ParamLoStop2);
        dJointSetUniversalParam(FJointId, dParamHiStop2, ParamHiStop2);
      end;
  end;

  with TgxODERagdollCube(FGeom.data) do
  begin
    Visible := FRagdoll.ShowBoundingBoxes;
    Material.FrontProperties.Diffuse.SetColor(1, 0, 0, 0.4);
    CubeWidth := boneSize.X;
    CubeHeight := boneSize.Y;
    CubeDepth := boneSize.Z;
    Bone := Self;
    Ragdoll := Self.FRagdoll;
  end;
end;

procedure TgxODERagdollBone.Stop;
var
  o: TgxBaseSceneObject;
begin
  inherited;
  dBodyDestroy(FBody);
  if assigned(FGeom.data) then
  begin
    o := TgxBaseSceneObject(FGeom.data);
    FRagdoll.GLXceneRoot.Remove(o, False);
    o.free;
  end;
  if FJointId <> nil then
    dJointDestroy(FJointId);
  dGeomDestroy(FGeom);
end;

procedure TgxODERagdollBone.Update;
begin
  PositionSceneObject(TgxBaseSceneObject(PdxGeom(FGeom.data)), FGeom);
  Ragdoll.Owner.Skeleton.BoneByID(BoneID).SetGlobalMatrixForRagDoll
    (TgxBaseSceneObject(PdxGeom(FGeom.data)).AbsoluteMatrix);
end;

procedure TgxODERagdollBone.Align;
begin
  inherited;
  AlignBodyToMatrix(BoneMatrix);
end;

//------------------------------------
// TgxODERagdoll
//------------------------------------

constructor TgxODERagdoll.Create(aOwner: TgxBaseMesh);
begin
  inherited Create(aOwner);
  FShowBoundingBoxes := False;
end;

//------------------------------------
initialization
//------------------------------------

vODERagdoll_cDensity := 20;
vODERagdoll_cMass := 1;

end.
