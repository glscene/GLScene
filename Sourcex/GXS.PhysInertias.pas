//
// The graphics engine GXScene
//
unit GXS.PhysInertias;

interface

uses
  System.SysUtils,
  System.Classes,

  GXS.XCollection,
  Stage.VectorGeometry,
  Stage.VectorTypes,
  Stage.Strings,

  GXS.Coordinates,
  GXS.BaseClasses,
  GXS.PersistentClasses,
  GXS.PhysManager,
  GXS.Scene,
  GXS.Behaviours;

type
  TgxParticleInertia = class(TgxBaseInertia)
  // modified from TgxBInertia
  private
    FMass: Single;
    FTranslationSpeed: TgxCoordinates;
    FTranslationDamping: TgxDamping;
  protected
    function CalcLinearPositionDot(): TAffineVector;
    function CalcLinearMomentumDot(): TAffineVector;
    procedure SetTranslationSpeed(const val: TgxCoordinates);
    procedure SetTranslationDamping(const val: TgxDamping);
  public
    fForce: TAffineVector;
    LinearPosition: TAffineVector;
    LinearMomentum: TAffineVector;
    procedure StateToArray(var StateArray: TStateArray; StatePos: Integer); override;
    procedure ArrayToState( { var } StateArray: TStateArray; StatePos: Integer); override;
    procedure CalcStateDot(var StateArray: TStateArray; StatePos: Integer); override;
    procedure RemoveForces(); override;
    procedure CalculateForceFieldForce(ForceFieldEmitter
      : TgxBaseForceFieldEmitter); override;
    procedure CalcAuxiliary(); override;
    procedure SetUpStartingState(); override;
    function CalculateKE(): Real; override;
    function CalculatePE(): Real; override;
    procedure SetForce(x, y, z: Real); virtual;
    procedure ApplyForce(x, y, z: Real); overload; virtual;
    procedure ApplyForce(Force: TAffineVector); overload; virtual;
    procedure ApplyForce(pos, Force: TAffineVector); overload; virtual;
    procedure ApplyLocalForce(pos, Force: TAffineVector); virtual;
    procedure ApplyImpulse(j, x, y, z: Real); overload; virtual;
    procedure ApplyImpulse(j: Single; normal: TAffineVector); overload; virtual;
    procedure ApplyDamping(damping: TgxDamping); virtual;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function UniqueItem: Boolean; override;
    // Inverts the translation vector
    procedure MirrorTranslation;
    (* Bounce speed as if hitting a surface.
      restitution is the coefficient of restituted energy (1=no energy loss,
      0=no bounce). The normal is NOT assumed to be normalized. *)
    procedure SurfaceBounce(const surfaceNormal: TGLVector; restitution: Single);
  published
    property Mass: Single read FMass write FMass;
    property TranslationSpeed: TgxCoordinates read FTranslationSpeed
      write SetTranslationSpeed;

    (* Enable/Disable damping (damping has a high cpu-cycle cost).
      Damping is enabled by default. *)
    // property DampingEnabled : Boolean read FDampingEnabled write FDampingEnabled;
    (* Damping applied to translation speed.<br>
      Note that it is not "exactly" applied, ie. if damping would stop
      your object after 0.5 time unit, and your progression steps are
      of 1 time unit, there will be an integration error of 0.5 time unit. *)
    property TranslationDamping: TgxDamping read FTranslationDamping
      write SetTranslationDamping;
  end;

  TgxRigidBodyInertia = class;

  (* Stores Inertia Tensor for TgxRigidBodyInertia model *)
  TgxInertiaTensor = class(TgxUpdateAbleObject)
  private
    fm11, fm12, fm13, fm21, fm22, fm23, fm31, fm32, fm33: Single;
  public
    constructor Create(aOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);
  published
    property m11: Single read fm11 write fm11;
    property m12: Single read fm12 write fm12;
    property m13: Single read fm13 write fm13;
    property m21: Single read fm21 write fm21;
    property m22: Single read fm22 write fm22;
    property m23: Single read fm23 write fm23;
    property m31: Single read fm31 write fm31;
    property m32: Single read fm32 write fm32;
    property m33: Single read fm33 write fm33;
  end;

  (* A more complex model than TgxBInertia for Inertia *)
  TgxRigidBodyInertia = class(TgxParticleInertia)
  private
    fDensity: Real;
    fBodyInertiaTensor: TAffineMAtrix;
    fBodyInverseInertiaTensor: TAffineMAtrix;
    fInertiaTensor: TgxInertiaTensor;
    InverseInertiaTensor: TAffineMAtrix;
    // LinearVelocity:TAffineVector;
    fRotationSpeed: TgxCoordinates;
    /// AngularVelocity:TAffineVector;      //rotation about axis, magnitude=speed
    // damping properties
    FRotationDamping: TgxDamping;
  protected
    // torques
    fTorque: TAffineVector;
    procedure SetLinearDamping(const val: TgxDamping);
    procedure SetAngularDamping(const val: TgxDamping);
  public
    AngularOrientation: TQuaternion; // As Quat will help improve accuracy
    R: TMatrix3f; // corresponds to AngularOrientation
    AngularMomentum: TAffineVector;
    procedure StateToArray(var StateArray: TStateArray;
      StatePos: Integer); override;
    procedure ArrayToState( (* var *) StateArray: TStateArray;
      StatePos: Integer); override;
    procedure CalcStateDot(var StateArray: TStateArray;
      StatePos: Integer); override;
    procedure ApplyImpulse(j, xpos, ypos, zpos, x, y, z: Real); overload;
    procedure ApplyImpulse(j: Single; position, normal: TAffineVector);
      overload;
    procedure ApplyDamping(damping: TgxDamping); override;
    // function CalcLinearPositionDot():TAffineVector;
    // function CalcLinearMomentumDot():TAffineVector;
    function CalcAngularOrientationDot(): TQuaternion;
    function CalcAngularVelocityDot(): TAffineVector;
    function CalculateKE(): Real; override;
    function CalculatePE(): Real; override;
    procedure CalcAuxiliary(); override;
    procedure SetUpStartingState(); override;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function UniqueItem: Boolean; override;
    // function Star(Vector:TAffineVector):TGLMatrix;
    function QuaternionToString(Quat: TQuaternion): String;
    procedure RemoveForces(); override;
    procedure SetTorque(x, y, z: Real);
    procedure ApplyTorque(x, y, z: Real);
    procedure ApplyForce(pos, Force: TAffineVector); override;
    procedure ApplyLocalForce(pos, Force: TVector3f); override;
    procedure ApplyLocalImpulse(xpos, ypos, zpos, x, y, z: Real);
    procedure SetInertiaTensor(newVal: TgxInertiaTensor);
    procedure SetRotationSpeed(const val: TgxCoordinates);
    procedure SetRotationDamping(const val: TgxDamping);
  published
    property Density: Real read fDensity write fDensity;
    property InertiaTensor: TgxInertiaTensor read fInertiaTensor
      write SetInertiaTensor;
    property RotationSpeed: TgxCoordinates read fRotationSpeed
      write SetRotationSpeed;
    property RotationDamping: TgxDamping read FRotationDamping
      write SetRotationDamping;
  end;

(* Returns or creates the TgxParticleInertia within the given behaviours.
   This helper function is convenient way to access a TgxParticleInertia. *)
function GetOrCreateParticleInertia(behaviours: TgxBehaviours): TgxParticleInertia; overload;
(* Returns or creates the TgxParticleInertia within the given object's behaviours.
  This helper function is convenient way to access a TgxParticleInertia. *)
function GetOrCreateParticleInertia(obj: TgxBaseSceneObject): TgxParticleInertia; overload;
(* Returns or creates the TgxRigidBodyInertia within the given behaviours.
  This helper function is convenient way to access a TgxRigidBodyInertia. *)
function GetOrCreateRigidBodyInertia(behaviours: TgxBehaviours): TgxRigidBodyInertia; overload;
(* Returns or creates the TgxRigidBodyInertia within the given object's behaviours.
  This helper function is convenient way to access a TgxRigidBodyInertia. *)
function GetOrCreateRigidBodyInertia(obj: TgxBaseSceneObject): TgxRigidBodyInertia; overload;

const
  DebugMode = false;

implementation //-----------------------------------------------------------

// ------------------
// ------------------ TgxParticleInertia ------------------
// ------------------

constructor TgxParticleInertia.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FMass := 1;
  StateSize := 6;
  FTranslationSpeed := TgxCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  LinearPosition := OwnerBaseSceneObject.position.AsAffineVector;
  LinearMomentum := FTranslationSpeed.AsAffineVector;
  FTranslationDamping := TgxDamping.Create(Self);
end;

destructor TgxParticleInertia.Destroy;
begin
  FTranslationDamping.Free;
  FTranslationSpeed.Free;
  inherited Destroy;
end;

procedure TgxParticleInertia.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    FMass := TgxParticleInertia(Source).FMass;
    FTranslationSpeed.Assign(TgxParticleInertia(Source).FTranslationSpeed);
    LinearPosition := TgxParticleInertia(Source).LinearPosition;
    LinearMomentum := TgxParticleInertia(Source).LinearMomentum;
    // FDampingEnabled:=TGLInertia(Source).DampingEnabled;
    FTranslationDamping.Assign(TgxParticleInertia(Source).TranslationDamping);
    // FRotationDamping.Assign(TgxBInertia(Source).RotationDamping);
  end;
  inherited Assign(Source);
end;

procedure TgxParticleInertia.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FMass);
    Write(LinearPosition, SizeOf(LinearPosition));
    Write(LinearMomentum, SizeOf(LinearMomentum));
    Write(fForce, SizeOf(fForce));
    FTranslationSpeed.WriteToFiler(writer);
    FTranslationDamping.WriteToFiler(writer);
  end;
end;

procedure TgxParticleInertia.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FMass := ReadFloat;
    Read(LinearPosition, SizeOf(LinearPosition));
    Read(LinearMomentum, SizeOf(LinearMomentum));
    Read(fForce, SizeOf(fForce));
    FTranslationSpeed.ReadFromFiler(reader);
    FTranslationDamping.ReadFromFiler(reader);
  end;
  // Loaded;
  SetUpStartingState();
end;

procedure TgxParticleInertia.SetTranslationSpeed(const val: TgxCoordinates);
begin
  FTranslationSpeed.Assign(val);
  LinearMomentum := VectorScale(FTranslationSpeed.AsAffineVector, FMass);
end;

procedure TgxParticleInertia.SetUpStartingState();
begin
  LinearPosition := OwnerBaseSceneObject.position.AsAffineVector;
  LinearMomentum := VectorScale(TranslationSpeed.AsAffineVector, Mass);
end;

procedure TgxParticleInertia.CalcAuxiliary( { RBody:TgxRigidBody } );
begin
  TranslationSpeed.AsAffineVector := VectorScale(LinearMomentum, 1 / Mass);
  // OwnerBaseSceneObject.Matrix:=QuaternionToMatrix(AngularOrientation);
  OwnerBaseSceneObject.position.AsAffineVector := LinearPosition; // position
  // OwnerBaseSceneObject.position.x:=LinearPosition[0];//position
  // OwnerBaseSceneObject.position.y:=LinearPosition[1];
  // OwnerBaseSceneObject.position.z:=LinearPosition[2];
end;

procedure TgxParticleInertia.RemoveForces();
begin
  fForce := nullVector;
end;

procedure TgxParticleInertia.CalculateForceFieldForce(ForceFieldEmitter
  : TgxBaseForceFieldEmitter);
begin
  ForceFieldEmitter.CalculateForceField(Self.OwnerBaseSceneObject);
end;

function TgxParticleInertia.CalculateKE(): Real;
begin
  Result := 1 / (2 * Mass) * VectorNorm(LinearMomentum);
end;

function TgxParticleInertia.CalculatePE(): Real;
begin
  // need to find potentials due to fields acting on body
  // may be easier to do via ForceFieldEmitters?
  Result := 0;
end;

procedure TgxParticleInertia.SetForce(x, y, z: Real);
begin
  fForce.x := x;
  fForce.y := y;
  fForce.z := z;
end;

procedure TgxParticleInertia.ApplyForce(x, y, z: Real);
begin
  fForce.X := fForce.X + x;
  fForce.Y := fForce.Y + y;
  fForce.Z := fForce.Z + z;
end;

procedure TgxParticleInertia.ApplyForce(Force: TAffineVector);
begin
  fForce := VectorAdd(fForce, Force);
end;

procedure TgxParticleInertia.ApplyForce(pos, Force: TAffineVector);
// var
// abspos:TAffineVector;
begin
  fForce := VectorAdd(fForce, Force);
  // abspos:=VectorTransform(pos,R);
  // fTorque:=VectorAdd(fTorque,VectorCrossProduct(abspos,force));
  // fForce:=VectorAdd(fForce,force);
end;

procedure TgxParticleInertia.ApplyLocalForce(pos, Force: TAffineVector);
// var
// abspos:TAffineVector;
// absForce:TAffineVector;
begin
  // abspos:=VectorTransform(pos,R);
  // absForce:=VectorTransform(Force,R);
  // fTorque:=VectorAdd(fTorque,VectorCrossProduct(abspos,absforce));
  fForce := VectorAdd(fForce, Force);
end;

procedure TgxParticleInertia.ApplyImpulse(j, x, y, z: Real);
begin
  // V2 = V1 + (j/M)n
  // V2.M = V1.M +j.n
  LinearMomentum.X := LinearMomentum.X + j * x;
  LinearMomentum.Y := LinearMomentum.Y + j * y;
  LinearMomentum.Z := LinearMomentum.Z + j * z;
end;

procedure TgxParticleInertia.ApplyImpulse(j: Single; normal: TAffineVector);
begin
  CombineVector(LinearMomentum, normal, j);
end;

procedure TgxParticleInertia.ApplyDamping(damping: TgxDamping);
var
  velocity: TAffineVector;
  v: Real;
  dampingForce: TAffineVector;
begin
  velocity := VectorScale(LinearMomentum, 1 / Mass); // v = p/m
  // apply force in opposite direction to velocity
  v := VectorLength(velocity);
  // F = -Normalised(V)*( Constant + (Linear)*(V) + (Quadtratic)*(V)*(V) )
  dampingForce := VectorScale(VectorNormalize(velocity),
    -(damping.Constant + damping.Linear * v + damping.Quadratic * v * v));
  (*
    dampingForce:=VectorScale(VectorNormalize(velocity),
    -(Damping.Constant+Damping.Linear*v+Damping.Quadratic*v*v));
  *)
  ApplyForce(dampingForce);
end;

procedure TgxParticleInertia.SetTranslationDamping(const val: TgxDamping);
begin
  FTranslationDamping.Assign(val);
end;

class function TgxParticleInertia.FriendlyName: String;
begin
  Result := 'Particle Inertia';
end;

class function TgxParticleInertia.FriendlyDescription: String;
begin
  Result := 'A simple translation inertia';
end;

class function TgxParticleInertia.UniqueItem: Boolean;
begin
  Result := True;
end;

function TgxParticleInertia.CalcLinearPositionDot(): TAffineVector;
begin
  Result := VectorScale(LinearMomentum, 1 / FMass);
  // Result:=FTranslationSpeed.AsAffineVector;
end;

function TgxParticleInertia.CalcLinearMomentumDot(): TAffineVector;
begin
  Result := fForce;
end;

procedure TgxParticleInertia.StateToArray(var StateArray: TStateArray;
  StatePos: Integer);
begin
  // SetLength(Result,StateSize);
  StateArray[StatePos] := LinearPosition.X; // position
  StateArray[StatePos + 1] := LinearPosition.Y;
  StateArray[StatePos + 2] := LinearPosition.Z;
  StateArray[StatePos + 3] := LinearMomentum.X; // momentum
  StateArray[StatePos + 4] := LinearMomentum.Y;
  StateArray[StatePos + 5] := LinearMomentum.Z;
end;

procedure TgxParticleInertia.ArrayToState(StateArray: TStateArray;
  StatePos: Integer);
begin
  LinearPosition.X := StateArray[StatePos];
  LinearPosition.Y := StateArray[StatePos + 1];
  LinearPosition.Z := StateArray[StatePos + 2];
  LinearMomentum.X := StateArray[StatePos + 3];
  LinearMomentum.Y := StateArray[StatePos + 4];
  LinearMomentum.Z := StateArray[StatePos + 5];
  // TODO change?
  (* OwnerBaseSceneObject.position.x:=StateArray[StatePos];//position
    OwnerBaseSceneObject.position.y:=StateArray[StatePos+1];
    OwnerBaseSceneObject.position.z:=StateArray[StatePos+2];
    FTranslationSpeed.X:=StateArray[StatePos+3]/fMass;//velocity
    FTranslationSpeed.Y:=StateArray[StatePos+4]/fMass;
    FTranslationSpeed.Z:=StateArray[StatePos+5]/fMass;
  *)
end;

procedure TgxParticleInertia.CalcStateDot(var StateArray: TStateArray;
  StatePos: Integer);
var
  LinPos, LinMom: TAffineVector;
begin
  LinPos := CalcLinearPositionDot();
  LinMom := CalcLinearMomentumDot();
  StateArray[StatePos] := LinPos.X;
  StateArray[StatePos + 1] := LinPos.Y;
  StateArray[StatePos + 2] := LinPos.Z;
  StateArray[StatePos + 3] := LinMom.X;
  StateArray[StatePos + 4] := LinMom.Y;
  StateArray[StatePos + 5] := LinMom.Z;
end;

procedure TgxParticleInertia.MirrorTranslation;
begin
  FTranslationSpeed.Invert;
end;

procedure TgxParticleInertia.SurfaceBounce(const surfaceNormal: TGLVector;
  restitution: Single);
var
  f: Single;
begin
  // does the current speed vector comply?
  f := VectorDotProduct(FTranslationSpeed.AsVector, surfaceNormal);
  if f < 0 then
  begin
    // remove the non-complying part of the speed vector
    FTranslationSpeed.AddScaledVector(-f / VectorNorm(surfaceNormal) *
      (1 + restitution), surfaceNormal);
  end;
end;

function GetOrCreateParticleInertia(behaviours: TgxBehaviours)
  : TgxParticleInertia;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TgxParticleInertia);
  if i >= 0 then
    Result := TgxParticleInertia(behaviours[i])
  else
    Result := TgxParticleInertia.Create(behaviours);
end;

function GetOrCreateParticleInertia(obj: TgxBaseSceneObject)
  : TgxParticleInertia;
begin
  Result := GetOrCreateParticleInertia(obj.behaviours);
end;


// -----------------------------------------------------------------------
// ------------ TgxInertiaTensor
// -----------------------------------------------------------------------

constructor TgxInertiaTensor.Create(aOwner: TPersistent);
begin
  inherited Create(aOwner);
  fm11 := 1;
  fm22 := 1;
  fm33 := 1;
end;

destructor TgxInertiaTensor.Destroy;
begin
  inherited Destroy;
end;

procedure TgxInertiaTensor.Assign(Source: TPersistent);
begin
  inherited;
  fm11 := TgxInertiaTensor(Source).fm11;
  fm12 := TgxInertiaTensor(Source).fm12;
  fm13 := TgxInertiaTensor(Source).fm13;
  fm21 := TgxInertiaTensor(Source).fm21;
  fm22 := TgxInertiaTensor(Source).fm22;
  fm23 := TgxInertiaTensor(Source).fm23;
  fm31 := TgxInertiaTensor(Source).fm31;
  fm32 := TgxInertiaTensor(Source).fm32;
  fm33 := TgxInertiaTensor(Source).fm33;
end;

procedure TgxInertiaTensor.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(fm11);
    WriteFloat(fm12);
    WriteFloat(fm13);
    WriteFloat(fm21);
    WriteFloat(fm22);
    WriteFloat(fm23);
    WriteFloat(fm31);
    WriteFloat(fm32);
    WriteFloat(fm33);
  end;
end;

procedure TgxInertiaTensor.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger(); // Archive Version 0
    fm11 := ReadFloat();
    fm12 := ReadFloat();
    fm13 := ReadFloat();
    fm21 := ReadFloat();
    fm22 := ReadFloat();
    fm23 := ReadFloat();
    fm31 := ReadFloat();
    fm32 := ReadFloat();
    fm33 := ReadFloat();
  end;
end;

//--------------------------
// TgxRigidBodyInertia
//--------------------------
procedure TgxRigidBodyInertia.SetInertiaTensor(newVal: TgxInertiaTensor);
begin
  fInertiaTensor := newVal;
end;

procedure TgxRigidBodyInertia.SetRotationSpeed(const val: TgxCoordinates);
begin
  AngularMomentum := VectorTransform(val.AsAffineVector, fBodyInertiaTensor);
  fRotationSpeed.Assign(val);
end;

procedure TgxRigidBodyInertia.SetRotationDamping(const val: TgxDamping);
begin
  FRotationDamping.Assign(val);
end;

procedure TgxRigidBodyInertia.ApplyImpulse(j, xpos, ypos, zpos, x, y, z: Real);
begin
  // V2 = V1 + (j/M)n
  // V2.M = V1.M +j.n
  LinearMomentum.X := LinearMomentum.X + j * x;
  LinearMomentum.Y := LinearMomentum.Y + j * y;
  LinearMomentum.Z := LinearMomentum.Z + j * z;

  AngularMomentum.X := AngularMomentum.X + j * x * xpos;
  AngularMomentum.Y := AngularMomentum.Y + j * y * ypos;
  AngularMomentum.Z := AngularMomentum.Z + j * z * zpos;
end;

procedure TgxRigidBodyInertia.ApplyImpulse(j: Single;
  position, normal: TAffineVector);
begin
  CombineVector(LinearMomentum, normal, j);
  CombineVector(AngularMomentum, VectorCrossProduct(position, normal), j); // ?
end;

procedure TgxRigidBodyInertia.ApplyDamping(damping: TgxDamping);
var
  velocity, angularvelocity: TAffineVector;
  v, angularv: Real;
  dampingForce: TAffineVector;
  angulardampingForce: TAffineVector;
begin
  velocity := VectorScale(LinearMomentum, 1 / Mass); // v = p/m
  // apply force in opposite direction to velocity
  v := VectorLength(velocity);
  // F = -Normalised(V)*( Constant + (Linear)*(V) + (Quadtratic)*(V)*(V) )
  dampingForce := VectorScale(VectorNormalize(velocity),
    -(damping.Constant + damping.Linear * v + damping.Quadratic * v * v));
  // ScaleVector(AngularMomentum,0.999);
  // ScaleVector(AngularVelocity,Damping.Constant);
  // dampingForce:=VectorScale(VectorNormalize(velocity),-(Damping.Constant+Damping.Linear*v+Damping.Quadratic*v*v));
  ApplyForce(dampingForce);

  angularvelocity := RotationSpeed.AsAffineVector; // v = p/m
  // apply force in opposite direction to velocity
  angularv := VectorLength(angularvelocity);
  // F = -Normalised(V)*( Constant + (Linear)*(V) + (Quadtratic)*(V)*(V) )
  angulardampingForce := VectorScale(VectorNormalize(angularvelocity),
    -(RotationDamping.Constant + RotationDamping.Linear * v +
    RotationDamping.Quadratic * v * v));
  // ScaleVector(AngularMomentum,0.999);
  // ScaleVector(AngularVelocity,Damping.Constant);
  // dampingForce:=VectorScale(VectorNormalize(velocity),-(Damping.Constant+Damping.Linear*v+Damping.Quadratic*v*v));
  ApplyTorque(angulardampingForce.X, angulardampingForce.Y, angulardampingForce.Z);

end;

procedure TgxRigidBodyInertia.CalcStateDot(var StateArray: TStateArray;
  StatePos: Integer);
var
  LinPos, LinMom, AngMom: TAffineVector;
  AngPos: TQuaternion;
begin
  LinPos := CalcLinearPositionDot();
  LinMom := CalcLinearMomentumDot();
  AngPos := CalcAngularOrientationDot();
  AngMom := CalcAngularVelocityDot();
  // SetLength(Result,StateSize);
  StateArray[StatePos] := LinPos.X;
  StateArray[StatePos + 1] := LinPos.Y;
  StateArray[StatePos + 2] := LinPos.Z;
  StateArray[StatePos + 3] := LinMom.X;
  StateArray[StatePos + 4] := LinMom.Y;
  StateArray[StatePos + 5] := LinMom.Z;
  StateArray[StatePos + 6] := AngPos.imagPart.X;
  StateArray[StatePos + 7] := AngPos.imagPart.Y;
  StateArray[StatePos + 8] := AngPos.imagPart.Z;
  StateArray[StatePos + 9] := AngPos.RealPart;
  StateArray[StatePos + 10] := AngMom.X;
  StateArray[StatePos + 11] := AngMom.Y;
  StateArray[StatePos + 12] := AngMom.Z;
end;

function TgxRigidBodyInertia.CalculateKE(): Real;
begin
  // Result:= "Linear KE" + "Angular KE"
  // only linear part so far
  Result := 1 / (2 * Mass) * VectorNorm(LinearMomentum);
end;

function TgxRigidBodyInertia.CalculatePE(): Real;
begin
  // need to find potentials due to fields acting on body
  // may be easier to do via forcefieldemitters?
  Result := 0;
end;

function TgxRigidBodyInertia.CalcAngularOrientationDot(): TQuaternion;
var
  q1: TQuaternion;
begin
  q1.imagPart := VectorScale(RotationSpeed.AsAffineVector, 1 / 2); // v1;
  q1.RealPart := 0;
  Result := QuaternionMultiply(q1, AngularOrientation);
end;

function TgxRigidBodyInertia.CalcAngularVelocityDot(): TAffineVector;
begin
  Result := fTorque;
end;

function TgxRigidBodyInertia.QuaternionToString(Quat: TQuaternion): String;
begin
  Result := '<Quaternion><imagPart>' + FloatToSTr(Quat.imagPart.X) + ',' +
    FloatToSTr(Quat.imagPart.Y) + ',' + FloatToSTr(Quat.imagPart.Z) +
    '</imagPart><realPart>' + FloatToSTr(Quat.RealPart) +
    '</realPart><Quaternion>';
end;

(*
 function TgxRigidBodyInertia.Star(Vector:TAffineVector):TGLMatrix;
  begin
  Result.X.X:=0;             Result[0][1]:=-Vector[2];  Result[0][2]:=Vector[1];  Result[0][3]:=0;
  Result[1][0]:=Vector[2];   Result[1][1]:=0;           Result[1][2]:=-Vector[0]; Result[1][3]:=0;
  Result[2][0]:=Vector[1];   Result[2][1]:=Vector[0];   Result[2][2]:=0;          Result[2][3]:=0;
  Result[3][0]:=0;           Result[3][1]:=0;            Result[3][2]:=0;          Result[3][3]:=1;
  end;
*)

procedure TgxRigidBodyInertia.SetTorque(x, y, z: Real);
begin
  fTorque.X := x;
  fTorque.Y := y;
  fTorque.Z := z;
end;

procedure TgxRigidBodyInertia.ApplyTorque(x, y, z: Real);
begin
  fTorque.X := fTorque.X + x;
  fTorque.Y := fTorque.Y + y;
  fTorque.Z := fTorque.Z + z;
end;

(*
procedure TgxRigidBodyInertia.ApplyImpulse(x,y,z:Real);
  begin
  //
  end;
*)

procedure TgxRigidBodyInertia.RemoveForces();
begin
  fForce := nullVector;
  fTorque := nullVector;
end;

procedure TgxRigidBodyInertia.ApplyForce(pos, Force: TVector3f);
var
  abspos: TAffineVector;
begin
  abspos := VectorTransform(pos, R);
  fTorque := VectorAdd(fTorque, VectorCrossProduct(abspos, Force));
  fForce := VectorAdd(fForce, Force);
end;

procedure TgxRigidBodyInertia.ApplyLocalForce(pos, Force: TVector3f);
var
  abspos: TAffineVector;
  absForce: TAffineVector;
begin
  abspos := VectorTransform(pos, R);
  absForce := VectorTransform(Force, R);
  fTorque := VectorAdd(fTorque, VectorCrossProduct(abspos, absForce));
  fForce := VectorAdd(fForce, absForce);
end;

procedure TgxRigidBodyInertia.ApplyLocalImpulse(xpos, ypos, zpos, x, y,
  z: Real);
begin
  //
end;

procedure TgxRigidBodyInertia.SetUpStartingState();
begin
  //
  inherited SetUpStartingState();
  fBodyInertiaTensor.X.X := InertiaTensor.fm11;
  fBodyInertiaTensor.X.Y := InertiaTensor.fm12;
  fBodyInertiaTensor.X.Z := InertiaTensor.fm13;
  fBodyInertiaTensor.Y.X := InertiaTensor.fm21;
  fBodyInertiaTensor.Y.Y  := InertiaTensor.fm22;
  fBodyInertiaTensor.Y.Z  := InertiaTensor.fm23;
  fBodyInertiaTensor.Z.X := InertiaTensor.fm31;
  fBodyInertiaTensor.Z.Y := InertiaTensor.fm32;
  fBodyInertiaTensor.Z.Z  := InertiaTensor.fm33;

  fBodyInverseInertiaTensor := fBodyInertiaTensor;

  InvertMatrix(fBodyInverseInertiaTensor);
  // Write
  (*
    Messagedlg('setting BodyIit: '+ Format('%f,%f,%f,%f,%f,%f,%f,%f,%f',
    [fBodyInverseInertiaTensor[0][0],fBodyInverseInertiaTensor[0][1],fBodyInverseInertiaTensor[0][2],
     fBodyInverseInertiaTensor[1][0],fBodyInverseInertiaTensor[1][1],fBodyInverseInertiaTensor[1][2],
     fBodyInverseInertiaTensor[2][0],fBodyInverseInertiaTensor[2][1],
     fBodyInverseInertiaTensor[2][2]]),mtinformation,[mbok],0);
  *)
  AngularOrientation := IdentityQuaternion;
  AngularMomentum := VectorTransform(RotationSpeed.AsAffineVector,
    fBodyInertiaTensor);
end;

procedure TgxRigidBodyInertia.CalcAuxiliary();
var
  IRt: TAffineMAtrix;
  Rt: TAffineMAtrix;
  Scale: TAffineVector;
  RMatrix: TGLMatrix;
begin
  // TODO: sort this out
  fBodyInverseInertiaTensor := IdentityMatrix;
  // compute auxiliary variables
  R := QuaternionToAffineMatrix(AngularOrientation);
  Rt := R;
  TransposeMatrix(Rt);

  IRt := MatrixMultiply(fBodyInverseInertiaTensor, Rt);
  InverseInertiaTensor := MatrixMultiply(R, IRt);

  RotationSpeed.AsAffineVector := VectorTransform(AngularMomentum,
    InverseInertiaTensor);
  TranslationSpeed.AsAffineVector := VectorScale(LinearMomentum, 1 / Mass);

  Scale := OwnerBaseSceneObject.Scale.AsAffineVector;
  OwnerBaseSceneObject.BeginUpdate;

  SetMatrix(RMatrix, R);
  OwnerBaseSceneObject.SetMatrix(RMatrix);
  // OwnerBaseSceneObject.Matrix:=QuaternionToMatrix(AngularOrientation);
  OwnerBaseSceneObject.Scale.AsAffineVector := Scale;

  OwnerBaseSceneObject.position.x := LinearPosition.X; // position
  OwnerBaseSceneObject.position.y := LinearPosition.Y;
  OwnerBaseSceneObject.position.z := LinearPosition.Z;
  OwnerBaseSceneObject.EndUpdate;
end;

procedure TgxRigidBodyInertia.StateToArray(var StateArray: TStateArray;
  StatePos: Integer);
begin
  // with State do
  begin
    // copy Linear Position
    StateArray[StatePos] := LinearPosition.X;
    StateArray[StatePos + 1] := LinearPosition.Y;
    StateArray[StatePos + 2] := LinearPosition.Z;
    // copy Linear Momentum
    StateArray[StatePos + 3] := LinearMomentum.X;
    StateArray[StatePos + 4] := LinearMomentum.Y;
    StateArray[StatePos + 5] := LinearMomentum.Z;
    // copy Angular Orientation
    StateArray[StatePos + 6] := AngularOrientation.imagPart.X;
    StateArray[StatePos + 7] := AngularOrientation.imagPart.Y;
    StateArray[StatePos + 8] := AngularOrientation.imagPart.Z;
    StateArray[StatePos + 9] := AngularOrientation.RealPart;
    // copy Angular Momentum
    StateArray[StatePos + 10] := AngularMomentum.X;
    StateArray[StatePos + 11] := AngularMomentum.Y;
    StateArray[StatePos + 12] := AngularMomentum.Z;
  end;
end;

procedure TgxRigidBodyInertia.ArrayToState( { var } StateArray: TStateArray;
  StatePos: Integer);
begin
  // restore Linear Position
  LinearPosition.X := StateArray[StatePos];
  LinearPosition.Y := StateArray[StatePos + 1];
  LinearPosition.Z := StateArray[StatePos + 2];
  // restore Linear Momentum
  LinearMomentum.X := StateArray[StatePos + 3];
  LinearMomentum.Y := StateArray[StatePos + 4];
  LinearMomentum.Z := StateArray[StatePos + 5];
  // restore Angular Orientation
  AngularOrientation.imagPart.X := StateArray[StatePos + 6];
  AngularOrientation.imagPart.Y := StateArray[StatePos + 7];
  AngularOrientation.imagPart.Z := StateArray[StatePos + 8];
  AngularOrientation.RealPart := StateArray[StatePos + 9];
  // restore Angular Momentum
  AngularMomentum.X := StateArray[StatePos + 10];
  AngularMomentum.Y := StateArray[StatePos + 11];
  AngularMomentum.Z := StateArray[StatePos + 12];
end;

procedure TgxRigidBodyInertia.SetLinearDamping(const val: TgxDamping);
begin
  // FLinearDamping.Assign(val);
end;


procedure TgxRigidBodyInertia.SetAngularDamping(const val: TgxDamping);
begin
  // FAngularDamping.Assign(val);
end;


constructor TgxRigidBodyInertia.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  Mass := 1;
  fDensity := 1;
  StateSize := 13;

  fInertiaTensor := TgxInertiaTensor.Create(Self);
  fRotationSpeed := TgxCoordinates.CreateInitialized(Self, VectorMake(0, 0, 0));

  // LinearPosition:=OwnerBaseSceneObject.Position.AsAffineVector;
  AngularOrientation := IdentityQuaternion; // fromAngleAxis(0,XVector);

  fTorque := nullVector;
  fForce := nullVector;

  // DampingEnabled:=False;
  // FTranslationDamping:=TgxDamping.Create(Self);
  FRotationDamping := TgxDamping.Create(Self);
  // RotationDamping:=TgxDamping.Create(Self);

  R := IdentityMatrix;
  InverseInertiaTensor := IdentityMatrix;

  // CalcAuxiliary();
  // SetDESolver(ssEuler);
end;

//---------------------------------------------------------------------------

destructor TgxRigidBodyInertia.Destroy;
begin
  // FLinearDamping.Free;
  // FAngularDamping.Free;
  fInertiaTensor.Free();
  fRotationSpeed.Free();
  FRotationDamping.Free;

  inherited Destroy;
end;

procedure TgxRigidBodyInertia.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    // FRigidBody.Assign(TgxRigidBodyInertia(Source));

    Mass := TgxRigidBodyInertia(Source).Mass;
    fDensity := TgxRigidBodyInertia(Source).fDensity;
    fBodyInertiaTensor := TgxRigidBodyInertia(Source).fBodyInertiaTensor;
    fBodyInverseInertiaTensor := TgxRigidBodyInertia(Source)
      .fBodyInverseInertiaTensor;

    InertiaTensor.Assign(TgxRigidBodyInertia(Source).InertiaTensor);

    LinearPosition := TgxRigidBodyInertia(Source).LinearPosition;
    AngularOrientation := TgxRigidBodyInertia(Source).AngularOrientation;

    LinearMomentum := TgxRigidBodyInertia(Source).LinearMomentum;
    AngularMomentum := TgxRigidBodyInertia(Source).AngularMomentum;

    // TranslationSpeed.AsAffineVector:=TgxRigidBodyInertia(Source).TranslationSpeed.AsAffineVector;
    RotationSpeed.Assign(TgxRigidBodyInertia(Source).RotationSpeed);

    // fForce:=TgxRigidBodyInertia(Source).fForce;
    fTorque := TgxRigidBodyInertia(Source).fTorque;

    // fInverseInertiaTensor:=TgxRigidBodyInertia(Source).fInverseInertiaTensor;

    // RigidBody.fTorque:=TgxRigidBodyInertia(Source).fTorque;
    // RigidBody.fForce:=TgxRigidBodyInertia(Source).fForce;

    FRotationDamping.Assign(TgxRigidBodyInertia(Source).FRotationDamping);

    // DampingEnabled:=TgxRigidBodyInertia(Source).DampingEnabled;
    // FTranslationDamping.Assign(TgxRigidBodyInertia(Source).LinearDamping);
    // FRotationDamping.Assign(TgxRigidBodyInertia(Source).AngularDamping);

  end;
  inherited Assign(Source);
end;

class function TgxRigidBodyInertia.FriendlyName: String;
begin
  Result := 'Rigid Body Inertia';
end;

class function TgxRigidBodyInertia.FriendlyDescription: String;
begin
  Result := 'An inertia model for rigid bodies';
end;

class function TgxRigidBodyInertia.UniqueItem: Boolean;
begin
  Result := True;
end;

// **************************************************************************
// *****************      DoProgress     ************************************
// **************************************************************************

(*
procedure TgxRigidBodyInertia.DoProgress(const progressTime : TProgressTimes);
  var
  TempScale:TaffineVector;
  UndampedLinearMomentum,DampedLinearMomentum:Real;
  UnDampedAngularMomentum,DampedAngularMomentum:Real;
  i:integer;
  begin
  // Write('Calculating next state...');

  with OwnerBaseSceneObject do
  with progressTime do
  begin

  if (DampingEnabled=true) then
  begin
  UndampedLinearMomentum:=VectorLength(LinearMomentum);
  DampedLinearMomentum:=TranslationDamping.Calculate(UndampedLinearMomentum,deltaTime);
  {   if Stage.VectorGeometry.vSIMD=1 then
  //  RigidBody.LinearMomentum:=VectorScale(VectorNormalize(RigidBody.LinearMomentum),DampedLinearMomentum)
  else
  }        begin
  if Length(LinearMomentum)<>0 then
  LinearMomentum:=VectorScale(VectorNormalize(LinearMomentum),DampedLinearMomentum)
  else
  LinearMomentum:=NullVector; //line not required
  end;

  UndampedAngularMomentum:=VectorLength(AngularMomentum);
  DampedAngularMomentum:=RotationDamping.Calculate(UndampedAngularMomentum,deltaTime);
  AngularMomentum:=VectorScale(VectorNormalize(AngularMomentum),DampedAngularMomentum);

  //     ApplyForce(VectorScale(RigidBody.LinearVelocity,-0.5));    //Either apply resistive force & torque
  //     ApplyTorque(VectorLength(RigidBody.AngularVelocity));      //or use TgxDamping
  end;

  //      Euler(RigidBody,deltaTime);
  //      RungeKutta4(DeltaTime);
  //DESolver(RigidBody,DeltaTime);

  //update OwnerBaseSceneObject
  TempScale:=Scale.AsAffineVector;
  Matrix:=QuaternionToMatrix(AngularOrientation);

  position.AsAffineVector:=LinearPosition;
  Scale.AsAffineVector:=TempScale;

  //calc auxiliary variables for next iteration
  CalcAuxiliary();
  end;
  end;
*)

procedure TgxRigidBodyInertia.WriteToFiler(writer: TWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    // WriteInteger(0); // Archive Version 0
    // FRigidBody.WriteToFiler(writer);
    // WriteFloat(fMass);
    WriteFloat(fDensity);
    Write(fBodyInertiaTensor, SizeOf(fBodyInertiaTensor));
    Write(fBodyInverseInertiaTensor, SizeOf(fBodyInverseInertiaTensor));

    fInertiaTensor.WriteToFiler(writer);

    Write(AngularOrientation, SizeOf(AngularOrientation));
    Write(AngularMomentum, SizeOf(AngularMomentum));

    // Write(LinearVelocity,SizeOf(LinearVelocity));
    RotationSpeed.WriteToFiler(writer);
    // Write(AngularVelocity,SizeOf(AngularVelocity));

    Write(fTorque, SizeOf(fTorque));
    // Write(fForce,SizeOf(fForce));

    FRotationDamping.WriteToFiler(writer);

    // WriteInteger(Integer(FDESolverType));
    // WriteBoolean(FDampingEnabled);
    // FLinearDamping.WriteToFiler(writer);
    // FAngularDamping.WriteToFiler(writer);
  end;
end;

procedure TgxRigidBodyInertia.ReadFromFiler(reader: TReader);
begin
  inherited ReadFromFiler(reader);
  with reader do
  begin
    // ReadInteger; // ignore archiveVersion
    // FRigidBody.ReadFromFiler(Reader);
    // fMass:=ReadFloat;
    fDensity := ReadFloat;
    Read(fBodyInertiaTensor, SizeOf(fBodyInertiaTensor));
    Read(fBodyInverseInertiaTensor, SizeOf(fBodyInverseInertiaTensor));

    InertiaTensor.ReadFromFiler(reader);

    Read(AngularOrientation, SizeOf(AngularOrientation));
    Read(AngularMomentum, SizeOf(AngularMomentum));

    // Read(LinearVelocity,SizeOf(LinearVelocity));
    RotationSpeed.ReadFromFiler(reader);
    // Read(AngularVelocity,SizeOf(AngularVelocity));

    Read(fTorque, SizeOf(fTorque));
    // Read(fForce, SizeOf(fForce));

    FRotationDamping.ReadFromFiler(reader);
    // SetDESolver(TDESolverType(ReadInteger));
    // FDampingEnabled:=ReadBoolean;
    // FLinearDamping.ReadFromFiler(reader);
    // FAngularDamping.ReadFromFiler(reader);
  end;
  // SetDESolver(fDESolverType);
  // CalcAuxiliary();
  SetUpStartingState();
end;

function GetOrCreateRigidBodyInertia(behaviours: TgxBehaviours)
  : TgxRigidBodyInertia;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TgxRigidBodyInertia);
  if i >= 0 then
    Result := TgxRigidBodyInertia(behaviours[i])
  else
    Result := TgxRigidBodyInertia.Create(behaviours);
end;

function GetOrCreateRigidBodyInertia(obj: TgxBaseSceneObject)
  : TgxRigidBodyInertia;
begin
  Result := GetOrCreateRigidBodyInertia(obj.behaviours);
end;

initialization // -----------------------------------------------------------

// class registrations
RegisterXCollectionItemClass(TgxParticleInertia);
RegisterXCollectionItemClass(TgxRigidBodyInertia);

end.
