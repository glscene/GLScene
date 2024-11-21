//
// The graphics engine GXScene
//
unit GXS.PhysForces;

interface

uses
  System.Classes,

  Stage.VectorTypes,
  GXS.XCollection,
  Stage.VectorGeometry,
  GXS.Coordinates,
  Stage.Strings,
  
  GXS.Scene,
  GXS.Behaviours;

type
  TgxForce = class;
  TgxForceType = (ftHookes, ftGravitation, ftCustom);

  TOnCustomForce = procedure() of object;

  TgxForce = class(TXCollectionItem)
  private
    fObject1: TgxBaseSceneObject;
    fObject2: TgxBaseSceneObject;
    fposition1: TgxCoordinates;
    fposition2: TgxCoordinates;
    object1Name: String;
    object2Name: String;
    // fOnCustomForce: TOnCustomForce;
  protected
    procedure Loaded; override;
    procedure SetName(const val: String); override;
    (* Returns the TgxBaseSceneObject on which the behaviour should be applied.
      Does NOT check for nil owners *)
    // function OwnerBaseSceneObject : TgxBaseSceneObject;
  public
    (* constructor Create(Collection: TCollection);override; *)
    // Override this function to write subclass data.
    procedure WriteToFiler(writer: TWriter); override;
    // Override this function to read subclass data.
    procedure ReadFromFiler(reader: TReader); override;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function UniqueItem: Boolean; override;
    procedure SetObject1(const val: TgxBaseSceneObject);
    procedure SetObject2(const val: TgxBaseSceneObject);
    procedure SetPosition1(const val: TgxCoordinates);
    procedure SetPosition2(const val: TgxCoordinates);
    function CalculateForce(): TAffineVector; virtual;
  published
    property Object1: TgxBaseSceneObject read fObject1 write SetObject1;
    property Object2: TgxBaseSceneObject read fObject2 write SetObject2;
    property Position1: TgxCoordinates read fposition1 write SetPosition1;
    property Position2: TgxCoordinates read fposition2 write SetPosition2;
    // property OnCustomForce:TOnCustomForce read fOnCustomForce write fOnCustomForce;
  end;

  TgxHookesSpring = class(TgxForce)
  private
    fNaturalLength: Real;
    fElasticity: Real;
    fLength: Real;
    fExtension: Real;
    fDamping: TgxDamping;
  public
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function UniqueItem: Boolean; override;
    procedure SetDamping(const val: TgxDamping);
    function CalculateForce(): TAffineVector; override;
  published
    property NaturalLength: Real read fNaturalLength write fNaturalLength;
    property Elasticity: Real read fElasticity write fElasticity;
    property Damping: TgxDamping read fDamping write SetDamping;
    // property Name;
  end;

  TgxHookesString = class(TgxHookesSpring)
  protected
    // procedure WriteToFiler(writer : TWriter); override;
    // procedure ReadFromFiler(reader : TReader); override;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function UniqueItem: Boolean; override;
    function CalculateForce(): TAffineVector; override;
  end;

implementation // -------------------------------------------------------------

uses
  GXS.PhysInertias,
  GXS.PhysManager;

constructor TgxForce.Create(aOwner: TXCollection);
begin
  inherited; // Create(aOwner)
  fposition1 := TgxCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  fposition2 := TgxCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  // fObject1:=TgxBaseSceneObject.Create(Self);
  // fObject2:=TgxBaseSceneObject.Create(Self);
end;

destructor TgxForce.Destroy;
begin
  fposition1.Free();
  fposition2.Free();
  // SetObject1(nil);
  // SetObject2(nil);
  // fObject1.Free();
  // fObject2.Free();

  inherited Destroy;
end;

procedure TgxForce.Assign(Source: TPersistent);
begin
  // inherited Assign(Source);
  fposition1.Assign(TgxForce(Source).fposition1);
  fposition2.Assign(TgxForce(Source).fposition2);
  Object1 := TgxForce(Source).Object1;
  Object2 := TgxForce(Source).Object2;
  inherited Assign(Source);
end;

procedure TgxForce.SetObject1(const val: TgxBaseSceneObject);
begin
  if val.Behaviours.IndexOfClass(TgxBaseInertia) >=0 then
    fObject1 := val
  else
    Write('Object1 does not have an inertia behaviour');
end;

procedure TgxForce.SetObject2(const val: TgxBaseSceneObject);
begin
  if val.Behaviours.IndexOfClass(TgxBaseInertia) >=0 then
    fObject2 := val
  else
   Write('Object2 does not have an inertia behaviour');
end;

procedure TgxForce.SetPosition1(const val: TgxCoordinates);
begin
  fposition1.Assign(val); // DB101
end;

procedure TgxForce.SetPosition2(const val: TgxCoordinates);
begin
  fposition2.Assign(val);
end;

procedure TgxForce.Loaded;
var
  PhysMan: TgxPhysManager;
begin
  inherited Loaded;
  // not nice at all!!!
  // assumes owner is TgxForces belonging to TGLPhysicsManager
  PhysMan := TgxPhysManager(Self.Owner.Owner);
  if (object1Name <> '') then
  begin
    // PhysMan:=TGLPhysicsManager(Self.Owner.Owner);
    fObject1 := PhysMan.FindObjectByName(object1Name);
    // fObject1:=TgxBaseSceneObject(FindComponent(Object1Name));
    // Object1Name:='';
  end;

  if object2Name <> '' then
  begin
    fObject2 := PhysMan.FindObjectByName(object2Name);
    // Object2Name:='';
  end;
end;

class function TgxForce.FriendlyName: String;
begin
  Result := 'Force';
end;

class function TgxForce.FriendlyDescription: String;
begin
  Result := 'Physics Force';
end;

class function TgxForce.UniqueItem: Boolean;
begin
  Result := false;
end;

procedure TgxForce.WriteToFiler(writer: TWriter);
begin
  inherited WriteToFiler(writer);
  // Write('Writing to filer'+GetNamePath);
  with writer do
  begin
    fposition1.WriteToFiler(writer);
    fposition2.WriteToFiler(writer);
    if Assigned(fObject1) then
      WriteString(fObject1.GetNamePath)
    else
      WriteString('');
    if Assigned(fObject2) then
      WriteString(fObject2.GetNamePath)
    else
      WriteString('');
    // WriteString(Object2Name);
  end;
end;

procedure TgxForce.ReadFromFiler(reader: TReader);
begin
  // Read('Reading from filer'+GetNamePath);
  inherited ReadFromFiler(reader);
  with reader do
  begin
    fposition1.ReadFromFiler(reader);
    fposition2.ReadFromFiler(reader);
    object1Name := ReadString;
    fObject1 := nil;
    object2Name := ReadString;
    fObject2 := nil;
  end;
  // Loaded;
end;

procedure TgxForce.SetName(const val: String);
begin
  inherited SetName(val);
  (*
  if Assigned(vGLBehaviourNameChangeEvent)
  then
   vGLBehaviourNameChangeEvent(Self);
  *)
end;

function TgxForce.CalculateForce(): TAffineVector;
begin
  //
end;

constructor TgxHookesSpring.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  fNaturalLength := 1;
  fElasticity := 1;
  fDamping := TgxDamping.Create(Self);
end;

destructor TgxHookesSpring.Destroy;
begin
  fDamping.Free;
  inherited Destroy;
end;

procedure TgxHookesSpring.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteFloat(fNaturalLength); // :Real;
    WriteFloat(fElasticity); // :Real;
    WriteFloat(fLength); // :Real;
    WriteFloat(fExtension); // :Real;
    fDamping.WriteToFiler(writer);
  end;
end;

procedure TgxHookesSpring.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    fNaturalLength := ReadFloat(); // :Real;
    fElasticity := ReadFloat(); // :Real;
    fLength := ReadFloat(); // :Real;
    fExtension := ReadFloat(); // :Real;
    fDamping.ReadFromFiler(reader);
  end;
end;

procedure TgxHookesSpring.SetDamping(const val: TgxDamping);
begin
  fDamping.Assign(val);
end;

function TgxHookesSpring.CalculateForce(): TAffineVector;
var
  rvector, vvector: TAffineVector;
  Inertia1, Inertia2: TgxParticleInertia;
begin
  if (fObject1 = nil) or (fObject2 = nil) then
    Exit;
  Inertia2 := TgxParticleInertia
    (Object2.Behaviours.GetByClass(TgxParticleInertia));
  Inertia1 := TgxParticleInertia
    (Object1.Behaviours.GetByClass(TgxParticleInertia));

  // rvector:=VectorSubtract({VectorAdd(Object2.Position.asAffineVector,}VectorTransform(Position2.AsAffineVector,Object2.Matrix{)}),
  // {VectorAdd(Object1.Position.asAffineVector,}VectorTransform(Position1.AsAffineVector,Object1.Matrix){)});
  rvector := VectorSubtract(Object2.LocalToAbsolute(Position2.AsAffineVector),
    Object1.LocalToAbsolute(Position1.AsAffineVector));
  (*
    rvector:=VectorSubtract(VectorAdd(Object2.Position.asAffineVector,VectorTransform(Position2.AsAffineVector,Object2.Matrix)),
    VectorAdd(Object1.Position.asAffineVector,VectorTransform(Position1.AsAffineVector,Object1.Matrix)));
  *)
  fLength := VectorLength(rvector);
  NormalizeVector(rvector);
  fExtension := fLength - fNaturalLength;

  // fDamping.Calculate();

  Result := VectorScale(rvector, fElasticity * fExtension / fNaturalLength);
  if Assigned(Inertia2) then
    Inertia2.ApplyForce(Position2.AsAffineVector, VectorNegate(Result));
  if Assigned(Inertia1) then
    Inertia1.ApplyForce(Position1.AsAffineVector, Result);
  // TGLInertia(Object1.Behaviours.GetByClass(TGLInertia)).ApplyForce(Position1.AsAffineVector,Result);
end;

class function TgxHookesSpring.FriendlyName: String;
begin
  Result := 'Hookes Spring';
end;

class function TgxHookesSpring.FriendlyDescription: String;
begin
  Result := 'A spring obeying Hookes Law';
end;

class function TgxHookesSpring.UniqueItem: Boolean;
begin
  Result := false;
end;

constructor TgxHookesString.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
end;

destructor TgxHookesString.Destroy;
begin
  inherited Destroy;
end;

class function TgxHookesString.FriendlyName: String;
begin
  Result := 'Hookes String';
end;

class function TgxHookesString.FriendlyDescription: String;
begin
  Result := 'A string (that can go slack) obeying Hookes Law';
end;

class function TgxHookesString.UniqueItem: Boolean;
begin
  Result := false;
end;

function TgxHookesString.CalculateForce(): TAffineVector;
var
  rvector: TAffineVector;
  Inertia1, Inertia2: TgxParticleInertia;
begin
  if (Object1 = nil) or (Object2 = nil) then
    Exit;
  rvector := VectorSubtract(Object2.LocalToAbsolute(Position2.AsAffineVector),
    Object1.LocalToAbsolute(Position1.AsAffineVector));
  // VectorAdd(Object2.Position.asAffineVector,VectorTransform(Object2.Position2.AsAffineVector,Object2.Matrix)),
  // VectorAdd(Object1.Position.asAffineVector,VectorTransform(Position1.AsAffineVector,Object1.Matrix)));
  fLength := VectorLength(rvector);
  if (fLength < fNaturalLength) then
    Result := NullVector
  else
  begin
    NormalizeVector(rvector);
    fExtension := fLength - fNaturalLength;
    Result := VectorScale(rvector, fElasticity * fExtension / fNaturalLength);
    // TGLInertia(Object2.Behaviours.GetByClass(TGLInertia)).ApplyForce(Position2.AsAffineVector,VectorNegate(Result));
    // TGLInertia(Object1.Behaviours.GetByClass(TGLInertia)).ApplyForce(Position1.AsAffineVector,Result);
    Inertia2 := TgxParticleInertia
      (Object2.Behaviours.GetByClass(TgxParticleInertia));
    Inertia1 := TgxParticleInertia
      (Object1.Behaviours.GetByClass(TgxParticleInertia));
    if Assigned(Inertia2) then
      Inertia2.ApplyForce(Position2.AsAffineVector, VectorNegate(Result));
    if Assigned(Inertia1) then
      Inertia1.ApplyForce(Position1.AsAffineVector, Result);
  end;
  // Result:= inherited CalculateForce();
  // if (fLength < fNaturalLength) then Result:=NullVector;
end;

initialization // -------------------------------------------------------------

RegisterXCollectionItemClass(TgxHookesSpring);
RegisterXCollectionItemClass(TgxHookesString);

end.
