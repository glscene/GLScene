//
// The graphics rendering engine GLScene http://glscene.org
//
unit Physics.ODERagdoll;

(* The extension of TGLRagdoll by using Open Dynamics Engine (ODE) *)

interface

uses
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Ragdoll,
  GLS.Scene,
  GLS.Objects,
  GLS.Texture,
  GLS.VectorFileObjects,

  Physics.ODEImport,
  Physics.ODEUtils;

const
  cMaxContacts = 4;

type
  TGLODERagdoll = class;
  TGLODERagdollBone = class;

  TGLODERagdollCube = class(TGLCube)
  public
    Bone: TGLODERagdollBone; // Useful in Oncollision Event
    Ragdoll: TGLODERagdoll; // Useful in Oncollision Event
  end;

  TGLODERagdollWorld = class
  private
    FSpace: PdxSpace;
    FWorld: PdxWorld;
    FContactGroup: TdJointGroupID;
    FRagdoll: TGLODERagdoll;
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
    property Ragdoll: TGLODERagdoll read FRagdoll;
  end;

  TGLODERagdollDummyJoint = class(TGLRagdolJoint)
  end;

  TGLODERagdollHingeJoint = class(TGLRagdolJoint)
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

  TGLODERagdollUniversalJoint = class(TGLODERagdollHingeJoint)
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

  TGLODERagdollBone = class(TGLRagdolBone)
  private
    FOwner: TGLODERagdollBone;
    FRagdoll: TGLODERagdoll;
    FBody: PdxBody;
    FGeom: PdxGeom;
    FJointId: TdJointID;
    procedure AlignBodyToMatrix(Mat: TGLMatrix);
  protected
    procedure Start; override;
    procedure Align; override;
    procedure Update; override;
    procedure Stop; override;
  public
    constructor CreateOwned(aOwner: TGLODERagdollBone);
    constructor Create(Ragdoll: TGLODERagdoll);
    property Body: PdxBody read FBody;
    property Geom: PdxGeom read FGeom;
  end;

  TGLODERagdoll = class(TGLRagdoll)
  private
    FODEWorld: TGLODERagdollWorld;
    FGLSceneRoot: TGLBaseSceneObject;
    FShowBoundingBoxes: Boolean;
    FEnabled: Boolean;
  public
    constructor Create(aOwner: TGLBaseMesh);
    property ODEWorld: TGLODERagdollWorld read FODEWorld write FODEWorld;
    property GLSceneRoot: TGLBaseSceneObject read FGLSceneRoot
      write FGLSceneRoot;
    property ShowBoundingBoxes: Boolean read FShowBoundingBoxes
      write FShowBoundingBoxes;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

var
  vODERagdoll_cDensity: Single;
  vODERagdoll_cMass: Single;

// ----------------------------------------
implementation

//-------------------------------------
// TGLODERagdollWorld
// ----------------------------------------

constructor TGLODERagdollWorld.Create;
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

constructor TGLODERagdollWorld.CreateFrom(World: PdxWorld; Space: PdxSpace;
  ContactGroup: TdJointGroupID);
begin
  FWorld := World;
  FSpace := Space;
  FContactGroup := ContactGroup;
  isWorldCreated := False; // NEW1
end;

destructor TGLODERagdollWorld.Destroy;
begin
  if isWorldCreated then
  begin
    dJointGroupDestroy(FContactGroup);
    dSpaceDestroy(FSpace);
    dWorldDestroy(FWorld);
  end;
  inherited;
end;

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

      c := dJointCreateContact(TGLODERagdollWorld(data).World,
        TGLODERagdollWorld(data).ContactGroup, @contact[i]);
      dJointAttach(c, dGeomGetBody(contact[i].Geom.g1),
        dGeomGetBody(contact[i].Geom.g2));
    end;
  end;

end;

procedure TGLODERagdollWorld.WorldUpdate;
const
  cDeltaTime = 1 / 50;
begin
  // Update the physic
  dSpaceCollide(FSpace, Self, ODERagdollCallback);
  dWorldQuickStep(FWorld, cDeltaTime);
  // remove all contact joints
  dJointGroupEmpty(FContactGroup);
end;

//
// TGLODERagdollHingeJoint
//
constructor TGLODERagdollHingeJoint.Create(Axis: TAffineVector;
  ParamLoStop, ParamHiStop: Single);
begin
  inherited Create;
  FAxis := Axis;
  FParamLoStop := ParamLoStop;
  FParamHiStop := ParamHiStop;
end;

//
// TGLODERagdollUniversalJoint
//
constructor TGLODERagdollUniversalJoint.Create(Axis: TAffineVector;
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

//
// TGLODERagdollBone
//
constructor TGLODERagdollBone.Create(Ragdoll: TGLODERagdoll);
begin
  inherited Create(Ragdoll);
  FRagdoll := Ragdoll;
end;

constructor TGLODERagdollBone.CreateOwned(aOwner: TGLODERagdollBone);
begin
  inherited CreateOwned(aOwner);
  FOwner := aOwner;
  FRagdoll := aOwner.FRagdoll;
end;

procedure TGLODERagdollBone.AlignBodyToMatrix(Mat: TGLMatrix);
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

procedure TGLODERagdollBone.Start;
var
  mass: TdMass;
  boneSize, vAxis, vAxis2: TAffineVector;
  n: integer;

  function RotateAxis(Axis: TAffineVector): TAffineVector;
  var
    absMat: TGLMatrix;
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

  FGeom := dCreateBox(FRagdoll.ODEWorld.Space, boneSize.X, boneSize.Y,
    boneSize.Z);
  FGeom.data := FRagdoll.GLSceneRoot.AddNewChild(TGLODERagdollCube);
  if (Joint is TGLODERagdollDummyJoint) then
    dGeomSetBody(FGeom, FOwner.Body)
  else
    dGeomSetBody(FGeom, FBody);
  if (Owner <> nil) then
  begin
    if (Joint is TGLODERagdollHingeJoint) then
      with (Joint as TGLODERagdollHingeJoint) do
      begin
        vAxis := RotateAxis(Axis);
        FJointId := dJointCreateHinge(FRagdoll.ODEWorld.World, nil);
        dJointAttach(FJointId, TGLODERagdollBone(Owner).Body, FBody);
        dJointSetHingeAnchor(FJointId, Anchor.X, Anchor.Y, Anchor.Z);
        dJointSetHingeAxis(FJointId, vAxis.X, vAxis.Y, vAxis.Z);
        dJointSetHingeParam(FJointId, dParamLoStop, ParamLoStop);
        dJointSetHingeParam(FJointId, dParamHiStop, ParamHiStop);
      end;
    if (Joint is TGLODERagdollUniversalJoint) then
      with (Joint as TGLODERagdollUniversalJoint) do
      begin
        vAxis := RotateAxis(Axis);
        vAxis2 := RotateAxis(Axis2);
        FJointId := dJointCreateUniversal(FRagdoll.ODEWorld.World, nil);
        dJointAttach(FJointId, TGLODERagdollBone(Owner).Body, FBody);
        dJointSetUniversalAnchor(FJointId, Anchor.X, Anchor.Y, Anchor.Z);
        dJointSetUniversalAxis1(FJointId, vAxis.X, vAxis.Y, vAxis.Z);
        dJointSetUniversalAxis2(FJointId, vAxis2.X, vAxis2.Y, vAxis2.Z);
        dJointSetUniversalParam(FJointId, dParamLoStop, ParamLoStop);
        dJointSetUniversalParam(FJointId, dParamHiStop, ParamHiStop);
        dJointSetUniversalParam(FJointId, dParamLoStop2, ParamLoStop2);
        dJointSetUniversalParam(FJointId, dParamHiStop2, ParamHiStop2);
      end;
  end;
  with TGLODERagdollCube(FGeom.data) do
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

procedure TGLODERagdollBone.Stop;
var
  o: TGLBaseSceneObject;
begin
  inherited;
  dBodyDestroy(FBody);
  if assigned(FGeom.data) then
  begin
    o := TGLBaseSceneObject(FGeom.data);
    FRagdoll.GLSceneRoot.Remove(o, False);
    o.free;
  end;
  if FJointId <> nil then
    dJointDestroy(FJointId);
  dGeomDestroy(FGeom);
end;

procedure TGLODERagdollBone.Update;
begin
  PositionSceneObject(TGLBaseSceneObject(PdxGeom(FGeom.data)), FGeom);
  Ragdoll.Owner.Skeleton.BoneByID(BoneID).SetGlobalMatrixForRagDoll
    (TGLBaseSceneObject(PdxGeom(FGeom.data)).AbsoluteMatrix);
end;

procedure TGLODERagdollBone.Align;
begin
  inherited;
  AlignBodyToMatrix(BoneMatrix);
end;

//
// TGLODERagdoll
//
constructor TGLODERagdoll.Create(aOwner: TGLBaseMesh);
begin
  inherited Create(aOwner);
  FShowBoundingBoxes := False;
end;

// -------------------------------------------------
initialization

// -------------------------------------------------

vODERagdoll_cDensity := 20;
vODERagdoll_cMass := 1;

end.
