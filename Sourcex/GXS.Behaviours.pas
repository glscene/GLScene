//
//
// The graphics engine GXScene https://github.com/glscene
//
//

unit GXS.Behaviours;

(* Standard TgxBehaviour subclasses *)

interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.XCollection,
  GXS.VectorTypes,
  GXS.Scene,
  GXS.VectorGeometry,
  GXS.BaseClasses,
  GXS.Coordinates;

type
  (* Holds parameters for TgxScene basic damping model.
    Damping is modeled by calculating a force from the speed, this force
    can then be transformed to an acceleration is you know the object's mass.
    Formulas :
    damping = constant + linear * Speed + quadratic * Speed^2
    accel = damping / Mass
    That's just basic physics :). A note on the components :
    constant : use it for solid friction (will stop abruptly an object after
      decreasing its speed.
    linear : linear friction damping.
    quadratic : expresses viscosity. *)
  TgxDamping = class(TgxUpdateAbleObject)
  private
    FConstant: single;
    FLinear: single;
    FQuadratic: single;
  public
    constructor Create(aOwner: TPersistent); override;
    destructor Destroy; override;
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);
    procedure Assign(Source: TPersistent); override;
    (* Calculates attenuated speed over deltaTime.
       Integration step is 0.01 sec, and the following formula is applied
       at each step: constant+linear*speed+quadratic*speed^2 *)
    function Calculate(speed, deltaTime: double): double;
    // Returns a "[constant; linear; quadractic]" string
    function AsString(const damping: TgxDamping): string;
    // Sets all damping parameters in a single call.
    procedure SetDamping(const constant: single = 0; const linear: single = 0;
      const quadratic: single = 0);
  published
    property Constant: single read FConstant write FConstant;
    property Linear: single read FLinear write FLinear;
    property Quadratic: single read FQuadratic write FQuadratic;
  end;

  (* Simple translation and rotation Inertia behaviour.
    Stores translation and rotation speeds, to which you can apply
    accelerations.
    Note that the rotation model is not physical, so feel free to contribute
    a "realworld" inertia class with realistic, axis-free, rotation inertia
    if this approximation does not suits your needs :). *)
  TgxBInertia = class(TgxBehaviour)
  private
    FMass: single;
    FTranslationSpeed: TgxCoordinates;
    FTurnSpeed, FRollSpeed, FPitchSpeed: single;
    FTranslationDamping, FRotationDamping: TgxDamping;
    FDampingEnabled: boolean;
  protected
    procedure SetTranslationSpeed(const val: TgxCoordinates);
    procedure SetTranslationDamping(const val: TgxDamping);
    procedure SetRotationDamping(const val: TgxDamping);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    // Adds time-proportionned acceleration to the speed.
    procedure ApplyTranslationAcceleration(const deltaTime: double;
      const accel: TVector4f);
    // Applies a timed force to the inertia. If Mass is null, nothing is done.
    procedure ApplyForce(const deltaTime: double; const force: TVector4f);
    (* Applies a timed torque to the inertia (yuck!).
      This gets a "yuck!" because it is as false as the rest of the rotation  model. *)
    procedure ApplyTorque(const deltaTime: double;
      const turnTorque, rollTorque, pitchTorque: single);
    // Inverts the translation vector.
    procedure MirrorTranslation;
    (* Bounce speed as if hitting a surface.
      restitution is the coefficient of restituted energy (1=no energy loss,
      0=no bounce). The normal is NOT assumed to be normalized. *)
    procedure SurfaceBounce(const surfaceNormal: TVector4f; restitution: single);
  published
    property Mass: single read FMass write FMass;
    property TranslationSpeed: TgxCoordinates
      read FTranslationSpeed write SetTranslationSpeed;
    property TurnSpeed: single read FTurnSpeed write FTurnSpeed;
    property RollSpeed: single read FRollSpeed write FRollSpeed;
    property PitchSpeed: single read FPitchSpeed write FPitchSpeed;
    (* Enable/Disable damping (damping has a high cpu-cycle cost).
      Damping is enabled by default. *)
    property DampingEnabled: boolean read FDampingEnabled write FDampingEnabled;
    (* Damping applied to translation speed.
      Note that it is not "exactly" applied, ie. if damping would stop
      your object after 0.5 time unit, and your progression steps are
      of 1 time unit, there will be an integration error of 0.5 time unit. *)
    property TranslationDamping: TgxDamping read FTranslationDamping write SetTranslationDamping;
     (* Damping applied to rotation speed (yuck!).
        Well, this one is not "exact", like TranslationDamping, and neither
        it is "physical" since I'm reusing the mass and... and... well don't
        show this to your science teacher 8).
        Anyway that's easier to use than the realworld formulas, calculated
        faster, and properly used can give a good illusion of reality. *)
    property RotationDamping: TgxDamping read FRotationDamping write SetRotationDamping;
  end;

  // Applies a constant acceleration to a TgxBInertia.
  TgxBAcceleration = class(TgxBehaviour)
  private
    FAcceleration: TgxCoordinates;
  protected
    procedure SetAcceleration(const val: TgxCoordinates);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
  published
    property Acceleration: TgxCoordinates read FAcceleration write FAcceleration;
  end;

(* Returns or creates the TgxBInertia within the given behaviours.
  This helper function is convenient way to access a TgxBInertia. *)
function GetInertia(const AGLXceneObject: TgxBaseSceneObject): TgxBInertia;
function GetOrCreateInertia(behaviours: TgxBehaviours): TgxBInertia; overload;
function GetOrCreateInertia(obj: TgxBaseSceneObject): TgxBInertia; overload;

(* Returns or creates the TgxBAcceleration within the given behaviours.
  This helper function is convenient way to access a TgxBAcceleration. *)
function GetOrCreateAcceleration(behaviours: TgxBehaviours): TgxBAcceleration;
  overload;
function GetOrCreateAcceleration(obj: TgxBaseSceneObject): TgxBAcceleration; overload;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

function GetInertia(const AGLXceneObject: TgxBaseSceneObject): TgxBInertia;
var
  i: integer;
begin
  i := AGLXceneObject.behaviours.IndexOfClass(TgxBInertia);
  if i >= 0 then
    Result := TgxBInertia(AGLXceneObject.behaviours[i])
  else
    Result := nil;
end;

function GetOrCreateInertia(behaviours: TgxBehaviours): TgxBInertia;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TgxBInertia);
  if i >= 0 then
    Result := TgxBInertia(behaviours[i])
  else
    Result := TgxBInertia.Create(behaviours);
end;

function GetOrCreateInertia(obj: TgxBaseSceneObject): TgxBInertia;
begin
  Result := GetOrCreateInertia(obj.Behaviours);
end;

function GetOrCreateAcceleration(behaviours: TgxBehaviours): TgxBAcceleration;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TgxBAcceleration);
  if i >= 0 then
    Result := TgxBAcceleration(behaviours[i])
  else
    Result := TgxBAcceleration.Create(behaviours);
end;

function GetOrCreateAcceleration(obj: TgxBaseSceneObject): TgxBAcceleration;
begin
  Result := GetOrCreateAcceleration(obj.Behaviours);
end;

// ------------------
// ------------------ TgxDamping ------------------
// ------------------

constructor TgxDamping.Create(aOwner: TPersistent);
begin
  inherited Create(AOwner);
end;

destructor TgxDamping.Destroy;
begin
  inherited Destroy;
end;

procedure TgxDamping.Assign(Source: TPersistent);
begin
  if Source is TgxDamping then
  begin
    FConstant := TgxDamping(Source).Constant;
    FLinear := TgxDamping(Source).Linear;
    FQuadratic := TgxDamping(Source).Quadratic;
  end
  else
    inherited Assign(Source);
end;

procedure TgxDamping.WriteToFiler(writer: TWriter);
var
  writeStuff: boolean;
begin
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    writeStuff := (FConstant <> 0) or (FLinear <> 0) or (FQuadratic <> 0);
    WriteBoolean(writeStuff);
    if writeStuff then
    begin
      WriteFloat(FConstant);
      WriteFloat(FLinear);
      WriteFloat(FQuadratic);
    end;
  end;
end;

procedure TgxDamping.ReadFromFiler(reader: TReader);
begin
  with reader do
  begin
    ReadInteger; // ignore Archive Version
    if ReadBoolean then
    begin
      FConstant := ReadFloat;
      FLinear := ReadFloat;
      FQuadratic := ReadFloat;
    end
    else
    begin
      FConstant := 0;
      FLinear := 0;
      FQuadratic := 0;
    end;
  end;
end;

function TgxDamping.Calculate(speed, deltaTime: double): double;
var
  dt: double;
begin
  while deltaTime > 0 do
  begin
    if deltaTime > 0.01 then
    begin
      dt := 0.01;
      deltaTime := deltaTime - 0.01;
    end
    else
    begin
      dt := deltaTime;
      deltaTime := 0;
    end;
    speed := speed - dt * ((FQuadratic * speed + FLinear) * speed + FConstant);
  end;
  Result := speed;
end;

function TgxDamping.AsString(const damping: TgxDamping): string;
begin
  Result := Format('[%f; %f; %f]', [Constant, Linear, Quadratic]);
end;

procedure TgxDamping.SetDamping(const constant: single = 0;
  const linear: single = 0; const quadratic: single = 0);
begin
  FConstant := constant;
  FLinear := linear;
  FQuadratic := quadratic;
end;

// ------------------
// ------------------ TgxBInertia ------------------
// ------------------

constructor TgxBInertia.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FTranslationSpeed := TgxCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  FMass := 1;
  FDampingEnabled := True;
  FTranslationDamping := TgxDamping.Create(Self);
  FRotationDamping := TgxDamping.Create(Self);
end;

destructor TgxBInertia.Destroy;
begin
  FRotationDamping.Free;
  FTranslationDamping.Free;
  FTranslationSpeed.Free;
  inherited Destroy;
end;

procedure TgxBInertia.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    FMass := TgxBInertia(Source).Mass;
    FTranslationSpeed.Assign(TgxBInertia(Source).FTranslationSpeed);
    FTurnSpeed := TgxBInertia(Source).TurnSpeed;
    FRollSpeed := TgxBInertia(Source).RollSpeed;
    FPitchSpeed := TgxBInertia(Source).PitchSpeed;
    FDampingEnabled := TgxBInertia(Source).DampingEnabled;
    FTranslationDamping.Assign(TgxBInertia(Source).TranslationDamping);
    FRotationDamping.Assign(TgxBInertia(Source).RotationDamping);
  end;
  inherited Assign(Source);
end;

procedure TgxBInertia.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteFloat(FMass);
    FTranslationSpeed.WriteToFiler(writer);
    WriteFloat(FTurnSpeed);
    WriteFloat(FRollSpeed);
    WriteFloat(FPitchSpeed);
    WriteBoolean(FDampingEnabled);
    FTranslationDamping.WriteToFiler(writer);
    FRotationDamping.WriteToFiler(writer);
  end;
end;

procedure TgxBInertia.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FMass := ReadFloat;
    FTranslationSpeed.ReadFromFiler(reader);
    FTurnSpeed := ReadFloat;
    FRollSpeed := ReadFloat;
    FPitchSpeed := ReadFloat;
    FDampingEnabled := ReadBoolean;
    FTranslationDamping.ReadFromFiler(reader);
    FRotationDamping.ReadFromFiler(reader);
  end;
end;

procedure TgxBInertia.SetTranslationSpeed(const val: TgxCoordinates);
begin
  FTranslationSpeed.Assign(val);
end;

procedure TgxBInertia.SetTranslationDamping(const val: TgxDamping);
begin
  FTranslationDamping.Assign(val);
end;

procedure TgxBInertia.SetRotationDamping(const val: TgxDamping);
begin
  FRotationDamping.Assign(val);
end;

class function TgxBInertia.FriendlyName: string;
begin
  Result := 'Simple Inertia';
end;

class function TgxBInertia.FriendlyDescription: string;
begin
  Result := 'A simple translation and rotation inertia';
end;

class function TgxBInertia.UniqueItem: boolean;
begin
  Result := True;
end;

procedure TgxBInertia.DoProgress(const progressTime: TgxProgressTimes);
var
  trnVector: TVector4f;
  speed, newSpeed: double;

  procedure ApplyRotationDamping(var rotationSpeed: single);
  begin
    if rotationSpeed > 0 then
    begin
      rotationSpeed := RotationDamping.Calculate(rotationSpeed, progressTime.deltaTime);
      if rotationSpeed <= 0 then
        rotationSpeed := 0;
    end
    else
    begin
      rotationSpeed := -RotationDamping.Calculate(-rotationSpeed, progressTime.deltaTime);
      if rotationSpeed >= 0 then
        rotationSpeed := 0;
    end;
  end;

begin
  // Apply damping to speed
  if DampingEnabled then
  begin
    // Translation damping
    speed := TranslationSpeed.VectorLength;
    if speed > 0 then
    begin
      newSpeed := TranslationDamping.Calculate(speed, progressTime.deltaTime);
      if newSpeed <= 0 then
      begin
        trnVector := NullHmgVector;
        TranslationSpeed.AsVector := trnVector;
      end
      else
      begin
        TranslationSpeed.Scale(newSpeed / Speed);
        SetVector(trnVector, TranslationSpeed.AsVector);
      end;
    end
    else
      SetVector(trnVector, NullHmgVector);
    // Rotation damping (yuck!)
    ApplyRotationDamping(FTurnSpeed);
    ApplyRotationDamping(FRollSpeed);
    ApplyRotationDamping(FPitchSpeed);
  end
  else
    SetVector(trnVector, TranslationSpeed.AsVector);
  // Apply speed to object
  with OwnerBaseSceneObject do
    with progressTime do
    begin
      Position.AddScaledVector(deltaTime, trnVector);
      TurnAngle := TurnAngle + TurnSpeed * deltaTime;
      RollAngle := RollAngle + RollSpeed * deltaTime;
      PitchAngle := PitchAngle + PitchSpeed * deltaTime;
    end;
end;

procedure TgxBInertia.ApplyTranslationAcceleration(const deltaTime: double;
  const accel: TVector4f);
begin
  FTranslationSpeed.AsVector := VectorCombine(FTranslationSpeed.AsVector,
    accel, 1, deltaTime);
end;

procedure TgxBInertia.ApplyForce(const deltaTime: double; const force: TVector4f);
begin
  if Mass <> 0 then
    FTranslationSpeed.AsVector :=
      VectorCombine(FTranslationSpeed.AsVector, force, 1, deltaTime / Mass);
end;

procedure TgxBInertia.ApplyTorque(const deltaTime: double;
  const turnTorque, rollTorque, pitchTorque: single);
var
  factor: double;
begin
  if Mass <> 0 then
  begin
    factor := deltaTime / Mass;
    FTurnSpeed := FTurnSpeed + turnTorque * factor;
    FRollSpeed := FRollSpeed + rollTorque * factor;
    FPitchSpeed := FPitchSpeed + pitchTorque * factor;
  end;
end;

procedure TgxBInertia.MirrorTranslation;
begin
  FTranslationSpeed.Invert;
end;

procedure TgxBInertia.SurfaceBounce(const surfaceNormal: TVector4f; restitution: single);
var
  f: single;
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

// ------------------
// ------------------ TgxBAcceleration ------------------
// ------------------

constructor TgxBAcceleration.Create(aOwner: TXCollection);
begin
  inherited;
  if aOwner <> nil then
    if not (csReading in TComponent(aOwner.Owner).ComponentState) then
      GetOrCreateInertia(TgxBehaviours(aOwner));
  FAcceleration := TgxCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
end;

destructor TgxBAcceleration.Destroy;
begin
  inherited;
  FAcceleration.Free;
end;

procedure TgxBAcceleration.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    FAcceleration.Assign(TgxBAcceleration(Source).FAcceleration);
  end;
  inherited Assign(Source);
end;

procedure TgxBAcceleration.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FAcceleration.WriteToFiler(writer);
  end;
end;

procedure TgxBAcceleration.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FAcceleration.ReadFromFiler(reader);
  end;
end;

procedure TgxBAcceleration.SetAcceleration(const val: TgxCoordinates);
begin
  FAcceleration.Assign(val);
end;

class function TgxBAcceleration.FriendlyName: string;
begin
  Result := 'Simple Acceleration';
end;

class function TgxBAcceleration.FriendlyDescription: string;
begin
  Result := 'A simple and constant acceleration';
end;

class function TgxBAcceleration.UniqueItem: boolean;
begin
  Result := False;
end;

procedure TgxBAcceleration.DoProgress(const progressTime: TgxProgressTimes);
var
  i: integer;
  Inertia: TgxBInertia;
begin
  i := Owner.IndexOfClass(TgxBInertia);
  if i >= 0 then
  begin
    Inertia := TgxBInertia(Owner[i]);
    Inertia.ApplyTranslationAcceleration(progressTime.deltaTime,
      FAcceleration.DirectVector);
  end
  else
  begin
    TgxBInertia.Create(Owner);
    //on next progress event this exception won't be raised, because TgxBInertia will be created again
    raise Exception.Create(ClassName + ' requires ' + TgxBInertia.ClassName +
      '! (' + TgxBInertia.ClassName + ' was added to the Behaviours again)');
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  RegisterXCollectionItemClass(TgxBInertia);
  RegisterXCollectionItemClass(TgxBAcceleration);

finalization

  UnregisterXCollectionItemClass(TgxBInertia);
  UnregisterXCollectionItemClass(TgxBAcceleration);

end.

