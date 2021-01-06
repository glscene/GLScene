//
// The graphics rendering engine GLScene http://glscene.org
//

unit Physics.SPIFields;

interface

uses
  System.Classes,
  GLS.VectorGeometry,
  GLS.XCollection,
  GLS.Scene,
  GLS.Coordinates,
  GLS.Behaviours,
  (* GLS.RigidBodyInertia *)
  Physics.SPIInertias,
  Physics.SPIManager;

type

  TGLUniformGravityEmitter = class(TGLBaseForceFieldEmitter)
  private
    fGravity: TGLCoordinates;
  protected
    procedure SetGravity(const val: TGLCoordinates);
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function UniqueItem: Boolean; override;
    function CalculateForceField(Body: TGLBaseSceneObject)
      : TAffineVector; override;
  published
    property Gravity: TGLCoordinates read fGravity write SetGravity;
  end;

  TGLRadialGravityEmitter = class(TGLBaseForceFieldEmitter)
  private
    fMass: Real;
    fMassOverG: Real;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function UniqueItem: Boolean; override;
    function CalculateForceField(Body: TGLBaseSceneObject)
      : TAffineVector; override;
  published
    property Mass: Real read fMass write fMass;
  end;

  TGLDampingFieldEmitter = class(TGLBaseForceFieldEmitter)
  private
    fDamping: TGLDamping;
  protected
    procedure SetDamping(const val: TGLDamping);
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function UniqueItem: Boolean; override;
    function CalculateForceField(Body: TGLBaseSceneObject)
      : TAffineVector; override;
  published
    property Damping: TGLDamping read fDamping write SetDamping;
  end;

const
  GravitationalConstant = 6.6726E-11;

// ==================================================================
implementation
// ==================================================================

// -------------------------------------
// ---- TGLUniformGravityEmitter
// -------------------------------------
constructor TGLUniformGravityEmitter.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  fGravity := TGLCoordinates.CreateInitialized(Self, nullHmgVector, csVector);
end;

destructor TGLUniformGravityEmitter.Destroy;
begin
  fGravity.Free;
  inherited Destroy;
end;

procedure TGLUniformGravityEmitter.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    fGravity := TGLUniformGravityEmitter(Source).fGravity;
  end;
end;

class function TGLUniformGravityEmitter.FriendlyName: String;
begin
  Result := 'Uniform Gravity';
end;

class function TGLUniformGravityEmitter.FriendlyDescription: String;
begin
  Result := 'Uniform Gravity, appropriate near surface of planet';
end;

class function TGLUniformGravityEmitter.UniqueItem: Boolean;
begin
  Result := false;
end;

procedure TGLUniformGravityEmitter.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    fGravity.WriteToFiler(writer);
  end;
end;

procedure TGLUniformGravityEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    fGravity.ReadFromFiler(reader);
  end;
end;

procedure TGLUniformGravityEmitter.SetGravity(const val: TGLCoordinates);
begin
  fGravity.Assign(val);
end;

// CalculateForceField  (TODO: ParticleInertia -> BaseInertia, add BaseInertia.ApplyAcceleration)
function TGLUniformGravityEmitter.CalculateForceField(Body: TGLBaseSceneObject)
  : TAffineVector;
var
  inertia1: TGLParticleInertia;
begin
  inertia1 := TGLParticleInertia
    (Body.Behaviours.GetByClass(TGLParticleInertia));
  if Assigned(inertia1) then
  begin
    Result := VectorScale(fGravity.AsAffineVector, inertia1.Mass);
    inertia1.ApplyForce(Result);
  end
  else
    Result := nullVector;
end;

// ------------------------------------------------------------------------------
// ------------------------------Radial Gravity Emitter -------------------------
// ------------------------------------------------------------------------------

constructor TGLRadialGravityEmitter.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
end;

destructor TGLRadialGravityEmitter.Destroy;
begin
  inherited Destroy;
end;

procedure TGLRadialGravityEmitter.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    fMass := TGLRadialGravityEmitter(Source).fMass;
  end;
end;

class function TGLRadialGravityEmitter.FriendlyName: String;
begin
  Result := 'Radial Gravity';
end;

class function TGLRadialGravityEmitter.FriendlyDescription: String;
begin
  Result := 'Radial Gravity, can be applied anywhere (use for planets)';
end;

class function TGLRadialGravityEmitter.UniqueItem: Boolean;
begin
  Result := false;
end;

procedure TGLRadialGravityEmitter.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteFloat(fMass);
  end;
end;

procedure TGLRadialGravityEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    fMass := ReadFloat();;
  end;
end;

// CalculateForceField (TODO: ParticleInertia -> BaseInertia if possible)
function TGLRadialGravityEmitter.CalculateForceField(Body: TGLBaseSceneObject)
  : TAffineVector;
var
  inertia1: TGLParticleInertia;
  R: TAffineVector;
  L: Real;
begin
  inertia1 := TGLParticleInertia
    (Body.Behaviours.GetByClass(TGLParticleInertia));
  if Assigned(inertia1) then
  begin
    R := VectorSubtract(Body.Position.AsAffineVector,
      Self.OwnerBaseSceneObject.Position.AsAffineVector);
    L := VectorLength(R);
    Result := VectorScale(R, -GravitationalConstant * (fMass / L));
    inertia1.ApplyForce(Result);
  end
  else
    Result := nullVector;
end;

// -----------------------------------------------------------------------------
// ------------------------------Damping Field Emitter -------------------------
// -----------------------------------------------------------------------------

constructor TGLDampingFieldEmitter.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  fDamping := TGLDamping.Create(Self);
end;

destructor TGLDampingFieldEmitter.Destroy;
begin
  fDamping.Free;
  inherited Destroy;
end;

procedure TGLDampingFieldEmitter.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    fDamping := TGLDampingFieldEmitter(Source).fDamping;
  end;
end;

class function TGLDampingFieldEmitter.FriendlyName: String;
begin
  Result := 'Damping Field';
end;

class function TGLDampingFieldEmitter.FriendlyDescription: String;
begin
  Result := 'Damping Field, to approximate air/fluid resistance';
end;

class function TGLDampingFieldEmitter.UniqueItem: Boolean;
begin
  Result := false;
end;

procedure TGLDampingFieldEmitter.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    fDamping.WriteToFiler(writer);
  end;
end;

procedure TGLDampingFieldEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    fDamping.ReadFromFiler(reader);
  end;
end;

procedure TGLDampingFieldEmitter.SetDamping(const val: TGLDamping);
begin
  fDamping.Assign(val);
end;

// CalculateForceField (TODO: ParticleInertia -> BaseInertia, BaseInertia.ApplyDamping?)
function TGLDampingFieldEmitter.CalculateForceField(Body: TGLBaseSceneObject)
  : TAffineVector;
var
  inertia1: TGLParticleInertia;
  // velocity:TAffineVector;
  // v:Real;
begin
  inertia1 := TGLParticleInertia
    (Body.Behaviours.GetByClass(TGLParticleInertia));
  if Assigned(inertia1) then
    inertia1.ApplyDamping(Damping);

  { Inertia1:=TGLParticleInertia(Body.Behaviours.GetByClass(TGLParticleInertia));
    if Assigned(inertia1) then
    begin
    velocity:=VectorScale(inertia1.LinearMomentum, 1/Inertia1.Mass); // v = p/m
    //apply force in opposite direction to velocity
    v:=VectorLength(velocity);
    //  F = -Normalised(V)*( Constant + (Linear)*(V) + (Quadtratic)*(V)*(V) )
    Result:=VectorScale(VectorNormalize(velocity),-(fDamping.Constant+fDamping.Linear*v+fDamping.Quadratic*v*v));
    inertia1.ApplyForce(Result);
    end
    else
    Result:=nullvector;
  }
end;

// -------------------------------------------------------------------------
initialization

// -------------------------------------------------------------------------

RegisterXCollectionItemClass(TGLUniformGravityEmitter);
RegisterXCollectionItemClass(TGLRadialGravityEmitter);
RegisterXCollectionItemClass(TGLDampingFieldEmitter);

end.
