//
// The graphics rendering engine GLScene http://glscene.org
//
unit Physics.SPIForces;

interface

uses
  System.Classes,
  Vcl.Dialogs,

  GLS.VectorTypes,
  GLS.XCollection,
  GLS.Scene,
  GLS.VectorGeometry,
  GLS.Behaviours,
  GLS.Coordinates,
  GLS.Strings;

type
  TGLForce = class;
  TGLForceType = (ftHookes, ftGravitation, ftCustom);

  TOnCustomForce = procedure() of object;

  TGLForce = class(TXCollectionItem)
  private
    fObject1: TGLBaseSceneObject;
    fObject2: TGLBaseSceneObject;
    fposition1: TGLCoordinates;
    fposition2: TGLCoordinates;
    object1Name: String;
    object2Name: String;
    // fOnCustomForce: TOnCustomForce;
  protected
    procedure Loaded; override;
    procedure SetName(const val: String); override;
    (* Returns the TGLBaseSceneObject on which the behaviour should be applied.
      Does NOT check for nil owners *)
    // function OwnerBaseSceneObject : TGLBaseSceneObject;
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
    procedure SetObject1(const val: TGLBaseSceneObject);
    procedure SetObject2(const val: TGLBaseSceneObject);
    procedure SetPosition1(const val: TGLCoordinates);
    procedure SetPosition2(const val: TGLCoordinates);
    function CalculateForce(): TAffineVector; virtual;
  published
    property Object1: TGLBaseSceneObject read fObject1 write SetObject1;
    property Object2: TGLBaseSceneObject read fObject2 write SetObject2;
    property Position1: TGLCoordinates read fposition1 write SetPosition1;
    property Position2: TGLCoordinates read fposition2 write SetPosition2;
    // property OnCustomForce:TOnCustomForce read fOnCustomForce write fOnCustomForce;
  end;

  TGLHookesSpring = class(TGLForce)
  private
    fNaturalLength: Real;
    fElasticity: Real;
    fLength: Real;
    fExtension: Real;
    fDamping: TGLDamping;
  public
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function UniqueItem: Boolean; override;
    procedure SetDamping(const val: TGLDamping);
    function CalculateForce(): TAffineVector; override;
  published
    property NaturalLength: Real read fNaturalLength write fNaturalLength;
    property Elasticity: Real read fElasticity write fElasticity;
    property Damping: TGLDamping read fDamping write SetDamping;
    // property Name;
  end;

  TGLHookesString = class(TGLHookesSpring)
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

// --------------------------------------------------------------
implementation
// --------------------------------------------------------------

uses
  Physics.SPIInertias,
  Physics.SPIManager;

constructor TGLForce.Create(aOwner: TXCollection);
begin
  inherited; // Create(aOwner)
  fposition1 := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  fposition2 := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  // fObject1:=TGLBaseSceneObject.Create(Self);
  // fObject2:=TGLBaseSceneObject.Create(Self);
end;

destructor TGLForce.Destroy;
begin
  fposition1.Free();
  fposition2.Free();
  // SetObject1(nil);
  // SetObject2(nil);
  // fObject1.Free();
  // fObject2.Free();

  inherited Destroy;
end;

procedure TGLForce.Assign(Source: TPersistent);
begin
  // inherited Assign(Source);
  fposition1.Assign(TGLForce(Source).fposition1);
  fposition2.Assign(TGLForce(Source).fposition2);
  Object1 := TGLForce(Source).Object1;
  Object2 := TGLForce(Source).Object2;
  inherited Assign(Source);
end;

procedure TGLForce.SetObject1(const val: TGLBaseSceneObject);
begin
  // if val.Behaviours.IndexOfClass(TGLBaseInertia) >=0 then
  fObject1 := val
  // else
  // messagedlg('Object1 does not have an inertia behaviour',mtWarning,[mbOk],0);
end;

procedure TGLForce.SetObject2(const val: TGLBaseSceneObject);
begin
  // if val.Behaviours.IndexOfClass(TGLBaseInertia) >=0 then
  fObject2 := val
  // else
  // messagedlg('Object2 does not have an inertia behaviour',mtWarning,[mbOk],0);
end;

procedure TGLForce.SetPosition1(const val: TGLCoordinates);
begin
  fposition1.Assign(val); // DB101
end;

procedure TGLForce.SetPosition2(const val: TGLCoordinates);
begin
  fposition2.Assign(val);
end;

procedure TGLForce.Loaded;
var
  PhysMan: TGLSPIManager;
begin
  inherited Loaded;
  // not nice, not nice at all!!!!!!
  // assumes owner is TGLForces belonging to TGLPhysicsManager
  PhysMan := TGLSPIManager(Self.Owner.Owner);
  if (object1Name <> '') then
  begin
    // PhysMan:=TGLPhysicsManager(Self.Owner.Owner);
    fObject1 := PhysMan.FindObjectByName(object1Name);
    // fObject1:=TGLBaseSceneObject(FindComponent(Object1Name));
    // Object1Name:='';
  end;

  if object2Name <> '' then
  begin
    fObject2 := PhysMan.FindObjectByName(object2Name);
    // Object2Name:='';
  end;
end;

class function TGLForce.FriendlyName: String;
begin
  Result := 'Force';
end;

class function TGLForce.FriendlyDescription: String;
begin
  Result := 'Physics Force';
end;

class function TGLForce.UniqueItem: Boolean;
begin
  Result := false;
end;

procedure TGLForce.WriteToFiler(writer: TWriter);
begin
  inherited WriteToFiler(writer);
  // messagedlg('Writing to filer'+GetNamePath,mtInformation,[mbOk],0);
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

procedure TGLForce.ReadFromFiler(reader: TReader);
begin
  // messagedlg('Reading from filer'+GetNamePath,mtInformation,[mbOk],0);
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

procedure TGLForce.SetName(const val: String);
begin
  inherited SetName(val);
  // if Assigned(vGLBehaviourNameChangeEvent) then
  // vGLBehaviourNameChangeEvent(Self);
end;

function TGLForce.CalculateForce(): TAffineVector;
begin
  //
end;

constructor TGLHookesSpring.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  fNaturalLength := 1;
  fElasticity := 1;
  fDamping := TGLDamping.Create(Self);
end;

destructor TGLHookesSpring.Destroy;
begin
  fDamping.Free;
  inherited Destroy;
end;

procedure TGLHookesSpring.WriteToFiler(writer: TWriter);
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

procedure TGLHookesSpring.ReadFromFiler(reader: TReader);
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

procedure TGLHookesSpring.SetDamping(const val: TGLDamping);
begin
  fDamping.Assign(val);
end;

function TGLHookesSpring.CalculateForce(): TAffineVector;
var
  rvector, vvector: TAffineVector;
  Inertia1, Inertia2: TGLParticleInertia;
begin
  if (fObject1 = nil) or (fObject2 = nil) then
    Exit;
  Inertia2 := TGLParticleInertia
    (Object2.Behaviours.GetByClass(TGLParticleInertia));
  Inertia1 := TGLParticleInertia
    (Object1.Behaviours.GetByClass(TGLParticleInertia));

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

class function TGLHookesSpring.FriendlyName: String;
begin
  Result := 'Hookes Spring';
end;

class function TGLHookesSpring.FriendlyDescription: String;
begin
  Result := 'A spring obeying Hookes Law';
end;

class function TGLHookesSpring.UniqueItem: Boolean;
begin
  Result := false;
end;

constructor TGLHookesString.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
end;

destructor TGLHookesString.Destroy;
begin
  inherited Destroy;
end;

class function TGLHookesString.FriendlyName: String;
begin
  Result := 'Hookes String';
end;

class function TGLHookesString.FriendlyDescription: String;
begin
  Result := 'A string (that can go slack) obeying Hookes Law';
end;

class function TGLHookesString.UniqueItem: Boolean;
begin
  Result := false;
end;

function TGLHookesString.CalculateForce(): TAffineVector;
var
  rvector: TAffineVector;
  Inertia1, Inertia2: TGLParticleInertia;
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
    Inertia2 := TGLParticleInertia
      (Object2.Behaviours.GetByClass(TGLParticleInertia));
    Inertia1 := TGLParticleInertia
      (Object1.Behaviours.GetByClass(TGLParticleInertia));
    if Assigned(Inertia2) then
      Inertia2.ApplyForce(Position2.AsAffineVector, VectorNegate(Result));
    if Assigned(Inertia1) then
      Inertia1.ApplyForce(Position1.AsAffineVector, Result);
  end;
  // Result:= inherited CalculateForce();
  // if (fLength < fNaturalLength) then Result:=NullVector;
end;

// =================================================================
initialization
// =================================================================

RegisterXCollectionItemClass(TGLHookesSpring);
RegisterXCollectionItemClass(TGLHookesString);

end.
