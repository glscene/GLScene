//
// The graphics rendering engine GLScene http://glscene.org
//
unit Physics.NGDRagdoll;

(* The Ragdoll extension by using Newton Game Dynamics Engine (ODE) *)

interface

uses
  System.Classes,
  System.SysUtils,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.VectorFileObjects,
  Physics.NGDImport;

type
  TNewtonRagdoll = class
  private
    FERP, FSlideLimit, FAngleLimit: single;
    FEnabled: boolean;
    newtonworld: PNewtonWorld;
    procedure SetSlideLimit(value: single);
    procedure SetAngleLimit(value: single);
    procedure SetERP(value: single);
    procedure SetEnabled(value: boolean);
    procedure Clean;
  public
    Actor: TGLActor;
    Bodies: TList;
    Joints: array of PNewtonJoint;
    Norm_matrices: array of TGLMatrix;
    Envelopes: array of record kind: byte;
    Mat: TGLMatrix;
    Pt: TVector3f;
    W, D, H, Mass: single;
  end;

property Enabled: boolean read FEnabled write SetEnabled;
property SlideLimit: single read FSlideLimit write SetSlideLimit;
property AngleLimit: single read FAngleLimit write SetAngleLimit;
property ERP: single read FERP write SetERP;

constructor Create(model: TGLActor; world: PNewtonWorld;
  min_env_size: single = 0.8; slide_limit: single = 0.5; erp_: single = 0.8;
  angle_limit: single = 15; full: boolean = true);
procedure Conform;
destructor Destroy; override;
procedure LoadFromFile(filename: string);
procedure SaveToFile(filename: string);
function TranslatePos(n: integer; add: boolean): TGLVector;
end;

function GetBoneParent(actor: TGLActor; bone: integer): integer;

// --------------------------------------------
implementation
// --------------------------------------------

function TNewtonRagdoll.TranslatePos;
begin
  with envelopes[n] do
    if add then
      Result := VectorAdd(Mat.w, VectorAdd(VectorScale(Mat.X, pt.X),
        VectorAdd(VectorScale(Mat.Y, pt.Y), VectorScale(Mat.Z, pt.Z))))
    else
      Result := VectorSubtract(Mat.w, VectorAdd(VectorScale(Mat.X, pt.X),
        VectorAdd(VectorScale(Mat.Y, pt.Y), VectorScale(Mat.Z, pt.Z))));
end;

function GetBoneParent;
var
  i, j: integer;
begin
  Result := 0;
  for i := 0 to actor.Skeleton.BoneCount - 2 do
    if actor.Skeleton.BoneByID(i) <> nil then
      for j := 0 to actor.Skeleton.BoneByID(i).Count - 1 do
        if actor.Skeleton.BoneByID(i).Items[j].BoneID = bone then
        begin
          Result := i;
          Exit;
        end;
end;

procedure NewtonApplyForceAndTorqueCallback(const body: PNewtonBody;
  timestep: single; threadIndex: integer); cdecl;
var
  m: single;
  i: TVector3f;
  F: TVector3f;
begin
  NewtonBodyGetMassMatrix(body, @m, @i.X, @i.Y, @i.Z);
  F := AffineVectorMake(0, -9.81 * m, 0);
  NewtonBodyAddForce(body, @F.X);
end;

function NewtonJointCallBack(const Universal: PNewtonJoint;
  desc: PNewtonHingeSliderUpdateDesc): cardinal; cdecl;
var
  angle: single;
begin
  // if Abs(desc.m_accel)>100 then
  // desc.m_accel:=desc.m_accel/2;
  // desc.m_accel:=NewtonUniversalCalculateStopAlpha1(Universal,desc,0);
  (*
    angle:=NewtonUniversalGetJointAngle0(Universal);
    if angle<-2.6 then desc.m_accel:=NewtonUniversalCalculateStopAlpha0(Universal,desc,-2.6);
    if angle>0 then desc.m_accel:=NewtonUniversalCalculateStopAlpha0(Universal,desc,0);
    angle:=NewtonUniversalGetJointAngle1(Universal);
    if angle<-2.6 then desc.m_accel:=NewtonUniversalCalculateStopAlpha1(Universal,desc,-2.6);
    if angle>0 then desc.m_accel:=NewtonUniversalCalculateStopAlpha1(Universal,desc,0);
  *)
  Result := 0;
end;

constructor TNewtonRagdoll.Create;
var
  i, j: integer;
  p1, p2: TVector4f;
  d: single;
  Collision: PNewtonCollision;
  CollisionBox, CollisionCylinder, CollisionSphere: PNewtonCollision;
  Matrix: TGLMatrix;
  CollisionOffsetMatrix: TGLMatrix; // For cone capsule and cylinder
begin
  collisionOffsetMatrix := IdentityHmgMatrix;
  d := 0;
  if full then
  begin
    inherited Create;
    actor := model;
    newtonworld := world;
  end;
  FEnabled := false;
  bodies := TList.Create;
  SetLength(envelopes, actor.Skeleton.BoneCount - 1);
  SetLength(norm_matrices, actor.Skeleton.BoneCount - 1);
  SetLength(joints, actor.Skeleton.BoneCount - 1);
  for i := 0 to actor.Skeleton.BoneCount - 2 do
  begin
    p1 := actor.Skeleton.BoneByID(i).GlobalMatrix.W;
    if actor.Skeleton.BoneByID(i).BoneCount > 1 then
      p2 := actor.Skeleton.BoneByID(i).Items[0].GlobalMatrix.W
    else
      p2 := p1;
    p1 := VectorTransform(p1, actor.AbsoluteMatrix);
    p2 := VectorTransform(p2, actor.AbsoluteMatrix);

    with envelopes[i] do
    begin
      if full then
      begin
        kind := 1;
        mass := 1;
        d := 2 * min_env_size;
        h := 2 * min_env_size;
        w := 0.8 * VectorLength(VectorSubtract(p2, p1));
        if w < 1 then
        begin
          w := min_env_size;
          d := min_env_size;
          h := min_env_size;
        end;
        pt := AffineVectorMake(w * 0.5 / 0.8, 0, 0);
        // p2:=m[0];  m[0]:=m[1];  m[1]:=p2;
      end;
      Mat := MatrixMultiply(Actor.Skeleton.BoneByID(i).GlobalMatrix,
        Actor.AbsoluteMatrix);
      Mat.w := TranslatePos(i, true);
      case kind of
        0, 1:
          begin
            Collision := NewtonCreateBox(NewtonWorld, w, h, d, 0, @CollisionOffsetMatrix);
            Bodies.Add(NewtonCreateBody(World, Collision, @CollisionOffsetMatrix));
            NewtonBodySetMassMatrix(bodies[bodies.Count - 1], mass, w, h, d);
          end;
        2:
          begin
            Bodies.Add(NewtonCreateBody(World, NewtonCreateCylinder(newtonworld,
              w, h, 0, @CollisionOffsetMatrix), @CollisionOffsetMatrix));
            NewtonBodySetMassMatrix(bodies[bodies.Count - 1], mass, 2, w, h);
          end;
        3:
          begin
            Bodies.Add(NewtonCreateBody(world, NewtonCreateSphere(newtonworld,
              w, w, w, 0, @CollisionOffsetMatrix), @CollisionOffsetMatrix));
            NewtonBodySetMassMatrix(bodies[bodies.Count - 1], mass, w, w, w);
          end;
      end;
      NewtonBodySetLinearDamping(bodies[i], 0);
      NewtonBodySetAngularDamping(bodies[i], @d);

      NewtonBodySetMatrix(bodies[i], @Mat);
      NewtonBodySetForceAndTorqueCallBack(bodies[i],
        NewtonApplyForceAndTorqueCallback);
    end;
  end;

  FERP := erp_;
  FSlideLimit := slide_limit;
  FAngleLimit := angle_limit;
  for i := 0 to actor.Skeleton.BoneCount - 2 do
  begin
    j := GetBoneParent(actor, i);
    if i = j then
      Continue;
    p1 := TranslatePos(i, false);
    with envelopes[i] do
      joints[i] := NewtonConstraintCreateHinge(newtonworld, @p1, @Mat.Y,
        bodies[i], bodies[j]);
    NewtonJointSetCollisionState(joints[i], 1);
    NewtonHingeSetUserCallback(joints[i], NewtonJointCallBack);
  end;
end;

procedure TNewtonRagdoll.Conform;
var
  i: integer;
begin
  if Enabled = false then
    Exit;
  for i := 0 to Length(envelopes) - 1 do
    with envelopes[i] do
    begin
      NewtonBodyGetMatrix(Bodies[i], @Mat);
      Mat.w := TranslatePos(i, false);
      Actor.Skeleton.BoneByID(i).SetGlobalMatrixForRagDoll(Mat);
    end;
  actor.Skeleton.MorphMesh(true);
end;

procedure TNewtonRagdoll.Clean;
var
  i: integer;
begin
  for i := 0 to Length(joints) - 1 do
    if joints[i] <> nil then
      NewtonDestroyJoint(newtonworld, joints[i]);
  SetLength(joints, 0);
  for i := 0 to bodies.Count - 1 do
    NewtonDestroyBody(newtonworld, bodies[i]);
  bodies.Clear;
  FreeAndNil(bodies);
end;

destructor TNewtonRagdoll.Destroy;
begin
  Clean;
  SetLength(envelopes, 0);
  SetLength(norm_matrices, 0);
  inherited Destroy;
end;

procedure TNewtonRagdoll.SetEnabled;
var
  i: integer;
  a: TGLMatrix;
  v: TVector3f;
begin
  if FEnabled = value then
    Exit;
  FEnabled := value;
  if value = true then
  begin
    actor.Skeleton.StartRagdoll;
    for i := 0 to Length(envelopes) - 1 do
      norm_matrices[i] := envelopes[i].Mat;
    Exit;
  end;

  actor.Skeleton.StopRagdoll;
  for i := 0 to Length(envelopes) - 1 do
  begin
    v := NullVector;
    NewtonBodySetVelocity(bodies[i], @v);
    NewtonBodySetOmega(bodies[i], @v);
    NewtonBodySetForce(bodies[i], @v);
    NewtonBodySetTorque(bodies[i], @v);

    envelopes[i].Mat := norm_matrices[i];
    a := envelopes[i].Mat;
    NewtonBodySetMatrix(bodies[i], @a);
    NewtonBodySetCollision(bodies[i], nil);
    actor.Skeleton.BoneByID(i).SetGlobalMatrixForRagDoll(a);
  end;
  actor.Skeleton.MorphMesh(true);
end;

procedure TNewtonRagdoll.SetSlideLimit;
begin
  FSlideLimit := value;
end;

procedure TNewtonRagdoll.SetAngleLimit;
begin
  FAngleLimit := value;
end;

procedure TNewtonRagdoll.SetERP;
var
  i: integer;
begin
  FERP := value;
end;

procedure TNewtonRagdoll.LoadFromFile;
var
  i: integer;
  s, a, e: single;
  F: TFileStream;
begin
  F := TFileStream.Create(filename, fmOpenRead);
  F.Read(i, SizeOf(i));
  SetLength(envelopes, i);
  F.Read(s, SizeOf(s));
  F.Read(e, SizeOf(e));
  F.Read(a, SizeOf(a));
  Clean;
  for i := 0 to Length(envelopes) - 1 do
  begin
    F.Read(envelopes[i].kind, SizeOf(envelopes[i].kind));
    F.Read(envelopes[i].w, SizeOf(envelopes[i].w));
    F.Read(envelopes[i].h, SizeOf(envelopes[i].h));
    F.Read(envelopes[i].d, SizeOf(envelopes[i].d));
    F.Read(envelopes[i].mass, SizeOf(envelopes[i].mass));
    F.Read(envelopes[i].pt, SizeOf(envelopes[i].pt));
  end;
  /// Create(actor, newtonworld, 0.8, s, e, a, false);
  F.Free;
end;

procedure TNewtonRagdoll.SaveToFile;
var
  i: integer;
  F: TFileStream;
begin
  if FileExists(filename) then
    F := TFileStream.Create(filename, fmOpenWrite)
  else
    F := TFileStream.Create(filename, fmCreate);
  i := Length(envelopes);
  F.Write(i, SizeOf(i));
  F.Write(FSlideLimit, SizeOf(FSlideLimit));
  F.Write(FERP, SizeOf(FERP));
  F.Write(FAngleLimit, SizeOf(FAngleLimit));
  for i := 0 to Length(envelopes) - 1 do
  begin
    F.Write(envelopes[i].kind, SizeOf(envelopes[i].kind));
    F.Write(envelopes[i].w, SizeOf(envelopes[i].w));
    F.Write(envelopes[i].h, SizeOf(envelopes[i].h));
    F.Write(envelopes[i].d, SizeOf(envelopes[i].d));
    F.Write(envelopes[i].mass, SizeOf(envelopes[i].mass));
    F.Write(envelopes[i].pt, SizeOf(envelopes[i].pt));
  end;
  F.Free;
end;

end.
