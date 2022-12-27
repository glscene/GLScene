//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit NGD.Ragdoll;

(* The Ragdoll extension using Newton Game Dynamics Engine *)

interface

uses
  System.Classes,
  System.SysUtils,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.VectorFileObjects,
  NGD.Import;

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

constructor Create(model: TGLActor; world: PNewtonWorld; min_env_size: single = 0.8;
  slide_limit: single = 0.5; erp_: single = 0.8; angle_limit: single = 15; full: boolean = true);
procedure Conform;
destructor Destroy; override;
procedure LoadFromFile(filename: string);
procedure SaveToFile(filename: string);
function TranslatePos(n: integer; add: boolean): TGLVector;
end;

function GetBoneParent(Actor: TGLActor; bone: integer): integer;

// --------------------------------------------
implementation
// --------------------------------------------

function TNewtonRagdoll.TranslatePos;
begin
  with Envelopes[n] do
    if add then
      Result := VectorAdd(Mat.W, VectorAdd(VectorScale(Mat.X, Pt.X),
        VectorAdd(VectorScale(Mat.Y, Pt.Y), VectorScale(Mat.Z, Pt.Z))))
    else
      Result := VectorSubtract(Mat.W, VectorAdd(VectorScale(Mat.X, Pt.X),
        VectorAdd(VectorScale(Mat.Y, Pt.Y), VectorScale(Mat.Z, Pt.Z))));
end;

function GetBoneParent;
var
  i, j: integer;
begin
  Result := 0;
  for i := 0 to Actor.Skeleton.BoneCount - 2 do
    if Actor.Skeleton.BoneByID(i) <> nil then
      for j := 0 to Actor.Skeleton.BoneByID(i).Count - 1 do
        if Actor.Skeleton.BoneByID(i).Items[j].BoneID = bone then
        begin
          Result := i;
          Exit;
        end;
end;

procedure NewtonApplyForceAndTorqueCallback(const body: PNewtonBody; timestep: single;
  threadIndex: integer); cdecl;
var
  m: single;
  i: TVector3f;
  F: TVector3f;
begin
  NewtonBodyGetMassMatrix(body, @m, @i.X, @i.Y, @i.Z);
  F := AffineVectorMake(0, -9.81 * m, 0);
  NewtonBodyAddForce(body, @F.X);
end;

function NewtonJointCallBack(const Universal: PNewtonJoint; desc: PNewtonHingeSliderUpdateDesc)
  : cardinal; cdecl;
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
  D: single;
  Collision: PNewtonCollision;
  CollisionBox, CollisionCylinder, CollisionSphere: PNewtonCollision;
  Matrix: TGLMatrix;
  CollisionOffsetMatrix: TGLMatrix; // For cone capsule and cylinder
begin
  CollisionOffsetMatrix := IdentityHmgMatrix;
  D := 0;
  if full then
  begin
    inherited Create;
    Actor := model;
    newtonworld := world;
  end;
  FEnabled := false;
  Bodies := TList.Create;
  SetLength(Envelopes, Actor.Skeleton.BoneCount - 1);
  SetLength(Norm_matrices, Actor.Skeleton.BoneCount - 1);
  SetLength(Joints, Actor.Skeleton.BoneCount - 1);
  for i := 0 to Actor.Skeleton.BoneCount - 2 do
  begin
    p1 := Actor.Skeleton.BoneByID(i).GlobalMatrix.W;
    if Actor.Skeleton.BoneByID(i).BoneCount > 1 then
      p2 := Actor.Skeleton.BoneByID(i).Items[0].GlobalMatrix.W
    else
      p2 := p1;
    p1 := VectorTransform(p1, Actor.AbsoluteMatrix);
    p2 := VectorTransform(p2, Actor.AbsoluteMatrix);

    with Envelopes[i] do
    begin
      if full then
      begin
        kind := 1;
        Mass := 1;
        D := 2 * min_env_size;
        H := 2 * min_env_size;
        W := 0.8 * VectorLength(VectorSubtract(p2, p1));
        if W < 1 then
        begin
          W := min_env_size;
          D := min_env_size;
          H := min_env_size;
        end;
        Pt := AffineVectorMake(W * 0.5 / 0.8, 0, 0);
        // p2:=m[0];  m[0]:=m[1];  m[1]:=p2;
      end;
      Mat := MatrixMultiply(Actor.Skeleton.BoneByID(i).GlobalMatrix, Actor.AbsoluteMatrix);
      Mat.W := TranslatePos(i, true);
      case kind of
        0, 1:
          begin
            Collision := NewtonCreateBox(newtonworld, W, H, D, 0, @CollisionOffsetMatrix);
            Bodies.add(NewtonCreateBody(world, Collision, @CollisionOffsetMatrix));
            NewtonBodySetMassMatrix(Bodies[Bodies.Count - 1], Mass, W, H, D);
          end;
        2:
          begin
            Bodies.add(NewtonCreateBody(world, NewtonCreateCylinder(newtonworld, W, H, 0,
              @CollisionOffsetMatrix), @CollisionOffsetMatrix));
            NewtonBodySetMassMatrix(Bodies[Bodies.Count - 1], Mass, 2, W, H);
          end;
        3:
          begin
            Bodies.add(NewtonCreateBody(world, NewtonCreateSphere(newtonworld, W, W, W, 0,
              @CollisionOffsetMatrix), @CollisionOffsetMatrix));
            NewtonBodySetMassMatrix(Bodies[Bodies.Count - 1], Mass, W, W, W);
          end;
      end;
      NewtonBodySetLinearDamping(Bodies[i], 0);
      NewtonBodySetAngularDamping(Bodies[i], @D);

      NewtonBodySetMatrix(Bodies[i], @Mat);
      NewtonBodySetForceAndTorqueCallBack(Bodies[i], NewtonApplyForceAndTorqueCallback);
    end;
  end;

  FERP := erp_;
  FSlideLimit := slide_limit;
  FAngleLimit := angle_limit;
  for i := 0 to Actor.Skeleton.BoneCount - 2 do
  begin
    j := GetBoneParent(Actor, i);
    if i = j then
      Continue;
    p1 := TranslatePos(i, false);
    with Envelopes[i] do
      Joints[i] := NewtonConstraintCreateHinge(newtonworld, @p1, @Mat.Y, Bodies[i], Bodies[j]);
    NewtonJointSetCollisionState(Joints[i], 1);
    NewtonHingeSetUserCallback(Joints[i], NewtonJointCallBack);
  end;
end;

procedure TNewtonRagdoll.Conform;
var
  i: integer;
begin
  if Enabled = false then
    Exit;
  for i := 0 to Length(Envelopes) - 1 do
    with Envelopes[i] do
    begin
      NewtonBodyGetMatrix(Bodies[i], @Mat);
      Mat.W := TranslatePos(i, false);
      Actor.Skeleton.BoneByID(i).SetGlobalMatrixForRagDoll(Mat);
    end;
  Actor.Skeleton.MorphMesh(true);
end;

procedure TNewtonRagdoll.Clean;
var
  i: integer;
begin
  for i := 0 to Length(Joints) - 1 do
    if Joints[i] <> nil then
      NewtonDestroyJoint(newtonworld, Joints[i]);
  SetLength(Joints, 0);
  for i := 0 to Bodies.Count - 1 do
    NewtonDestroyBody(newtonworld, Bodies[i]);
  Bodies.Clear;
  FreeAndNil(Bodies);
end;

destructor TNewtonRagdoll.Destroy;
begin
  Clean;
  SetLength(Envelopes, 0);
  SetLength(Norm_matrices, 0);
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
    Actor.Skeleton.StartRagdoll;
    for i := 0 to Length(Envelopes) - 1 do
      Norm_matrices[i] := Envelopes[i].Mat;
    Exit;
  end;

  Actor.Skeleton.StopRagdoll;
  for i := 0 to Length(Envelopes) - 1 do
  begin
    v := NullVector;
    NewtonBodySetVelocity(Bodies[i], @v);
    NewtonBodySetOmega(Bodies[i], @v);
    NewtonBodySetForce(Bodies[i], @v);
    NewtonBodySetTorque(Bodies[i], @v);

    Envelopes[i].Mat := Norm_matrices[i];
    a := Envelopes[i].Mat;
    NewtonBodySetMatrix(Bodies[i], @a);
    NewtonBodySetCollision(Bodies[i], nil);
    Actor.Skeleton.BoneByID(i).SetGlobalMatrixForRagDoll(a);
  end;
  Actor.Skeleton.MorphMesh(true);
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
  SetLength(Envelopes, i);
  F.Read(s, SizeOf(s));
  F.Read(e, SizeOf(e));
  F.Read(a, SizeOf(a));
  Clean;
  for i := 0 to Length(Envelopes) - 1 do
  begin
    F.Read(Envelopes[i].kind, SizeOf(Envelopes[i].kind));
    F.Read(Envelopes[i].W, SizeOf(Envelopes[i].W));
    F.Read(Envelopes[i].H, SizeOf(Envelopes[i].H));
    F.Read(Envelopes[i].D, SizeOf(Envelopes[i].D));
    F.Read(Envelopes[i].Mass, SizeOf(Envelopes[i].Mass));
    F.Read(Envelopes[i].Pt, SizeOf(Envelopes[i].Pt));
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
  i := Length(Envelopes);
  F.Write(i, SizeOf(i));
  F.Write(FSlideLimit, SizeOf(FSlideLimit));
  F.Write(FERP, SizeOf(FERP));
  F.Write(FAngleLimit, SizeOf(FAngleLimit));
  for i := 0 to Length(Envelopes) - 1 do
  begin
    F.Write(Envelopes[i].kind, SizeOf(Envelopes[i].kind));
    F.Write(Envelopes[i].W, SizeOf(Envelopes[i].W));
    F.Write(Envelopes[i].H, SizeOf(Envelopes[i].H));
    F.Write(Envelopes[i].D, SizeOf(Envelopes[i].D));
    F.Write(Envelopes[i].Mass, SizeOf(Envelopes[i].Mass));
    F.Write(Envelopes[i].Pt, SizeOf(Envelopes[i].Pt));
  end;
  F.Free;
end;

end.
