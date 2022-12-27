//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Behaviours;

(* Standard TGLBehaviour subclasses *)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.VectorTypes,
  GLS.Scene,
  GLS.VectorGeometry,
  GLS.XCollection,
  GLS.BaseClasses,
  GLS.Coordinates;

type
  (*
    Holds parameters for a basic damping model by calculating a force
    from the speed, this force can then be transformed to an acceleration
    is you know the object's mass.
    Formulas:
    damping = constant + linear * Speed + quadratic * Speed^2
    accel = damping / Mass
    That's just basic physics. A note on the components:
    constant: use it for solid friction (will stop abruptly an object after decreasing its speed.
    linear: linear friction damping.
    quadratic: expresses viscosity
  *)
  TGLDamping = class(TGLUpdateAbleObject)
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
    (*Calculates attenuated speed over deltaTime.
      integration step is 0.01 sec, and the following formula is applied
      at each step: constant+linear*speed+quadratic*speed^2 *)
    function Calculate(speed, deltaTime: Double): Double;
    // Returns a "[constant; linear; quadratic]" String
    function AsString(const damping: TGLDamping): String;
    // Sets all damping parameters in a single call.
    procedure SetDamping(const constant: Single = 0;
	  const linear: Single = 0;
      const quadratic: Single = 0);
  published
    property Constant: Single read FConstant write FConstant;
    property Linear: Single read FLinear write FLinear;
    property Quadratic: Single read FQuadratic write FQuadratic;
  end;

  (* Simple translation and rotation Inertia behaviour.
    Stores translation and rotation speeds, to which you can apply
    accelerations.
    Note that the rotation model is not physical, so feel free to contribute
    a "realworld" inertia class with realistic, axis-free, rotation inertia
    if this approximation does not suits your needs :). *)
  TGLBInertia = class(TGLBehaviour)
  private
    FMass: single;
    FTranslationSpeed: TGLCoordinates;
    FTurnSpeed, FRollSpeed, FPitchSpeed: single;
    FTranslationDamping, FRotationDamping: TGLDamping;
    FDampingEnabled: boolean;
  protected
    procedure SetTranslationSpeed(const val: TGLCoordinates);
    procedure SetTranslationDamping(const val: TGLDamping);
    procedure SetRotationDamping(const val: TGLDamping);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;
    procedure DoProgress(const progressTime: TGLProgressTimes); override;
    // Adds time-proportionned acceleration to the speed. 
    procedure ApplyTranslationAcceleration(const deltaTime: double;
      const accel: TGLVector);
    // Applies a timed force to the inertia. If Mass is null, nothing is done. 
    procedure ApplyForce(const deltaTime: Double; const Force: TGLVector);
    (*Applies a timed torque to the inertia (yuck!).
      This gets a "yuck!" because it is as false as the rest of the rotation  model. *)
    procedure ApplyTorque(const deltaTime: double;
      const turnTorque, rollTorque, pitchTorque: single);
    // Inverts the translation vector.  
    procedure MirrorTranslation;
    (* Bounce speed as if hitting a surface. 
       restitution is the coefficient of restituted energy (1=no energy loss, 0=no bounce). 
	   The normal is NOT assumed to be normalized. *)
    procedure SurfaceBounce(const surfaceNormal: TGLVector; restitution: single);
  published
    property Mass: single read FMass write FMass;
    property TranslationSpeed: TGLCoordinates read FTranslationSpeed write SetTranslationSpeed;
    property TurnSpeed: single read FTurnSpeed write FTurnSpeed;
    property RollSpeed: single read FRollSpeed write FRollSpeed;
    property PitchSpeed: single read FPitchSpeed write FPitchSpeed;

    // Enable/Disable damping (damping has a high cpu-cycle cost). Damping is enabled by default. 
    property DampingEnabled: boolean read FDampingEnabled write FDampingEnabled;
    (* Damping applied to translation speed.
       Note that it is not "exactly" applied, ie. if damping would stop
       your object after 0.5 time unit, and your progression steps are
       of 1 time unit, there will be an integration error of 0.5 time unit. *)
    property TranslationDamping: TGLDamping read FTranslationDamping write SetTranslationDamping;
    (* Damping applied to rotation speed (yuck!).
       Well, this one is not "exact", like TranslationDamping, and neither
       it is "physical" since I'm reusing the mass and... and... well don't
       show this to your science teacher 8).
       Anyway that's easier to use than the realworld formulas, calculated
       faster, and properly used can give a good illusion of reality. *)
    property RotationDamping: TGLDamping read FRotationDamping write SetRotationDamping;
  end;

  // Applies a constant acceleration to a TGLBInertia.  
  TGLBAcceleration = class(TGLBehaviour)
  private
    FAcceleration: TGLCoordinates;
  protected
    procedure SetAcceleration(const val: TGLCoordinates);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;
    procedure DoProgress(const progressTime: TGLProgressTimes); override;
  published
    property Acceleration: TGLCoordinates read FAcceleration write FAcceleration;
  end;

(* Returns or creates the TGLBInertia within the given behaviours. 
   This helper function is convenient way to access a TGLBInertia. *)
function GetInertia(const AGLSceneObject: TGLBaseSceneObject): TGLBInertia;
function GetOrCreateInertia(behaviours: TGLBehaviours): TGLBInertia; overload;
function GetOrCreateInertia(obj: TGLBaseSceneObject): TGLBInertia; overload;

(* Returns or creates the TGLBAcceleration within the given behaviours. 
   This helper function is convenient way to access a TGLBAcceleration. *)
function GetOrCreateAcceleration(behaviours: TGLBehaviours): TGLBAcceleration; overload;
function GetOrCreateAcceleration(obj: TGLBaseSceneObject): TGLBAcceleration; overload;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

function GetInertia(const AGLSceneObject: TGLBaseSceneObject): TGLBInertia;
var
  i: integer;
begin
  i := AGLSceneObject.behaviours.IndexOfClass(TGLBInertia);
  if i >= 0 then
    Result := TGLBInertia(AGLSceneObject.behaviours[i])
  else
    Result := nil;
end;

function GetOrCreateInertia(behaviours: TGLBehaviours): TGLBInertia;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLBInertia);
  if i >= 0 then
    Result := TGLBInertia(behaviours[i])
  else
    Result := TGLBInertia.Create(behaviours);
end;

function GetOrCreateInertia(obj: TGLBaseSceneObject): TGLBInertia;
begin
  Result := GetOrCreateInertia(obj.Behaviours);
end;

function GetOrCreateAcceleration(behaviours: TGLBehaviours): TGLBAcceleration;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLBAcceleration);
  if i >= 0 then
    Result := TGLBAcceleration(behaviours[i])
  else
    Result := TGLBAcceleration.Create(behaviours);
end;

function GetOrCreateAcceleration(obj: TGLBaseSceneObject): TGLBAcceleration;
begin
  Result := GetOrCreateAcceleration(obj.Behaviours);
end;

// ------------------
// ------------------ TGLDamping ------------------
// ------------------

constructor TGLDamping.Create(aOwner: TPersistent);
begin
  inherited Create(AOwner);
end;

destructor TGLDamping.Destroy;
begin
  inherited Destroy;
end;

procedure TGLDamping.Assign(Source: TPersistent);
begin
  if Source is TGLDamping then
  begin
    FConstant := TGLDamping(Source).Constant;
    FLinear := TGLDamping(Source).Linear;
    FQuadratic := TGLDamping(Source).Quadratic;
  end
  else
    inherited Assign(Source);
end;

procedure TGLDamping.WriteToFiler(writer: TWriter);
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

procedure TGLDamping.ReadFromFiler(reader: TReader);
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

function TGLDamping.Calculate(speed, deltaTime: double): double;
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

function TGLDamping.AsString(const damping: TGLDamping): string;
begin
  Result := Format('[%f; %f; %f]', [Constant, Linear, Quadratic]);
end;

procedure TGLDamping.SetDamping(const constant: single = 0;
  const linear: single = 0; const quadratic: single = 0);
begin
  FConstant := constant;
  FLinear := linear;
  FQuadratic := quadratic;
end;

// ------------------
// ------------------ TGLBInertia ------------------
// ------------------

constructor TGLBInertia.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FTranslationSpeed := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  FMass := 1;
  FDampingEnabled := True;
  FTranslationDamping := TGLDamping.Create(Self);
  FRotationDamping := TGLDamping.Create(Self);
end;

destructor TGLBInertia.Destroy;
begin
  FRotationDamping.Free;
  FTranslationDamping.Free;
  FTranslationSpeed.Free;
  inherited Destroy;
end;

procedure TGLBInertia.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    FMass := TGLBInertia(Source).Mass;
    FTranslationSpeed.Assign(TGLBInertia(Source).FTranslationSpeed);
    FTurnSpeed := TGLBInertia(Source).TurnSpeed;
    FRollSpeed := TGLBInertia(Source).RollSpeed;
    FPitchSpeed := TGLBInertia(Source).PitchSpeed;
    FDampingEnabled := TGLBInertia(Source).DampingEnabled;
    FTranslationDamping.Assign(TGLBInertia(Source).TranslationDamping);
    FRotationDamping.Assign(TGLBInertia(Source).RotationDamping);
  end;
  inherited Assign(Source);
end;

procedure TGLBInertia.WriteToFiler(writer: TWriter);
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

procedure TGLBInertia.ReadFromFiler(reader: TReader);
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

procedure TGLBInertia.SetTranslationSpeed(const val: TGLCoordinates);
begin
  FTranslationSpeed.Assign(val);
end;

procedure TGLBInertia.SetTranslationDamping(const val: TGLDamping);
begin
  FTranslationDamping.Assign(val);
end;

procedure TGLBInertia.SetRotationDamping(const val: TGLDamping);
begin
  FRotationDamping.Assign(val);
end;

class function TGLBInertia.FriendlyName: string;
begin
  Result := 'Simple Inertia';
end;

class function TGLBInertia.FriendlyDescription: string;
begin
  Result := 'A simple translation and rotation inertia';
end;

class function TGLBInertia.UniqueItem: boolean;
begin
  Result := True;
end;

procedure TGLBInertia.DoProgress(const progressTime: TGLProgressTimes);
var
  trnVector: TGLVector;
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

procedure TGLBInertia.ApplyTranslationAcceleration(const deltaTime: double;
  const accel: TGLVector);
begin
  FTranslationSpeed.AsVector := VectorCombine(FTranslationSpeed.AsVector,
    accel, 1, deltaTime);
end;

procedure TGLBInertia.ApplyForce(const deltaTime: double; const force: TGLVector);
begin
  if Mass <> 0 then
    FTranslationSpeed.AsVector :=
      VectorCombine(FTranslationSpeed.AsVector, force, 1, deltaTime / Mass);
end;

procedure TGLBInertia.ApplyTorque(const deltaTime: double;
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

procedure TGLBInertia.MirrorTranslation;
begin
  FTranslationSpeed.Invert;
end;

procedure TGLBInertia.SurfaceBounce(const surfaceNormal: TGLVector; restitution: single);
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
// ------------------ TGLBAcceleration ------------------
// ------------------

constructor TGLBAcceleration.Create(aOwner: TXCollection);
begin
  inherited;
  if aOwner <> nil then
    if not (csReading in TComponent(aOwner.Owner).ComponentState) then
      GetOrCreateInertia(TGLBehaviours(aOwner));
  FAcceleration := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
end;

destructor TGLBAcceleration.Destroy;
begin
  inherited;
  FAcceleration.Free;
end;

procedure TGLBAcceleration.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    FAcceleration.Assign(TGLBAcceleration(Source).FAcceleration);
  end;
  inherited Assign(Source);
end;

procedure TGLBAcceleration.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    FAcceleration.WriteToFiler(writer);
  end;
end;

procedure TGLBAcceleration.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FAcceleration.ReadFromFiler(reader);
  end;
end;

procedure TGLBAcceleration.SetAcceleration(const val: TGLCoordinates);
begin
  FAcceleration.Assign(val);
end;

 

class function TGLBAcceleration.FriendlyName: string;
begin
  Result := 'Simple Acceleration';
end;

class function TGLBAcceleration.FriendlyDescription: string;
begin
  Result := 'A simple and constant acceleration';
end;

class function TGLBAcceleration.UniqueItem: boolean;
begin
  Result := False;
end;

procedure TGLBAcceleration.DoProgress(const progressTime: TGLProgressTimes);
var
  i: integer;
  Inertia: TGLBInertia;
begin
  i := Owner.IndexOfClass(TGLBInertia);
  if i >= 0 then
  begin
    Inertia := TGLBInertia(Owner[i]);
    Inertia.ApplyTranslationAcceleration(progressTime.deltaTime,
      FAcceleration.DirectVector);
  end
  else
  begin
    TGLBInertia.Create(Owner);
    //on next progress event this exception won't be raised, because TGLBInertia will be created again
    raise Exception.Create(ClassName + ' requires ' + TGLBInertia.ClassName +
      '! (' + TGLBInertia.ClassName + ' was added to the Behaviours again)');
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

// class registrations
  RegisterXCollectionItemClass(TGLBInertia);
  RegisterXCollectionItemClass(TGLBAcceleration);

finalization

  UnregisterXCollectionItemClass(TGLBInertia);
  UnregisterXCollectionItemClass(TGLBAcceleration);

end.

