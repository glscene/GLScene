//
// The graphics engine GXScene
//
unit GXS.PhysManager;

(* The Manager for Scene Physics Interactions (Phys) *)

interface

uses
  System.Classes,
  System.SysUtils,

  GXS.XCollection,
  Stage.VectorGeometry,
  GXS.Scene,
  GXS.PhysForces,
  GXS.Behaviours;

type

  // only ssEuler is usable at the moment
  TDESolverType = (ssEuler, ssRungeKutta4, ssVerlet);
  // TDESolver = procedure((*RigidBody:TgxRigidBody;*)DeltaTime:Real) of object;
  TStateArray = array of Real;
  TgxPhysManager = class;

  (*
    ***Euler***, EulerImproved, EulerModified, MidPoint
    RungeKutta2, ***RungeKutta4***, RungKutta4Adaptive
    State Variables:  Position,  Velocity

    Verlet
    State Variables:  Position, Old Position
  *)

  // need to have state array(s) seperate from inertias to allow for implicit & explicit methods
  TDESolver = class(TObject)
  public
    StateSize: Integer;
    StateArray: TStateArray;
    Owner: TgxPhysManager;
    function StateToArray(): TStateArray; virtual;
    procedure ArrayToState(StateArray: TStateArray); virtual;
    procedure Solve(DeltaTime: Real); virtual; abstract;
    constructor Create(aOwner: TgxPhysManager); // override; //abstract;
    destructor Destroy; override;
    // procedure Assign(Source: TPersistent); override;
  end;

  // explicit   e.g. Euler, Mid-point, Runge-Kutta integration
  TDESolverExplicit = class(TDESolver)
  public
    StateArrayDot: TStateArray; // Velocity stored
    function CalcStateDot(): TStateArray; virtual;
  end;

  TDESolverEuler = class(TDESolverExplicit)
  public
    procedure Solve(DeltaTime: Real); override;
  end;

  TDESolverRungeKutta4 = class(TDESolverExplicit)
  public
    procedure Solve(DeltaTime: Real); override;
  end;

  // implicit   e.g. Verlet Integration
  TDESolverImplicit = class(TDESolver)
  public
    LastStateArray: TStateArray; // Last state stored
  end;

  TDESolverVerlet = class(TDESolverImplicit)
  public
  end;

  TgxForces = class;
  TgxBaseForceFieldEmitter = class;
  // TgxPhysManager = class;

  (* purpose of TgxBaseInertia is to allow for inertias that may be constrained
   to 1 or 2 dimensions
   Shouldn't be used directly, instead use TgxParticleInertia (for a 3D particle)
   TgxRigidBodyInertia (for a 3D rigid-body) or define a new sub-class
   e.g.  Tgx1DParticleInertia, this will allow for faster speed *)
  TgxBaseInertia = class(TgxBehaviour)
  private
    FDampingEnabled: Boolean;
    FManager: TgxPhysManager;
    FManagerName: String; // NOT persistent, temporarily used for persistence
  protected
    procedure Loaded; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    StateSize: Integer; // don't re-declare this in sub-classes
    // just initialise it in constructor
    procedure StateToArray(var StateArray: TStateArray;
      StatePos: Integer); virtual;
    procedure ArrayToState( { var } StateArray: TStateArray;
      StatePos: Integer); virtual;
    procedure CalcStateDot(var StateArray: TStateArray;
      StatePos: Integer); virtual;
    procedure RemoveForces(); virtual;
    procedure CalculateForceFieldForce(ForceFieldEmitter
      : TgxBaseForceFieldEmitter); virtual;
    procedure CalcAuxiliary(); virtual;
    procedure SetUpStartingState(); virtual;
    function CalculateKE(): Real; virtual;
    function CalculatePE(): Real; virtual;
    constructor Create(aOwner: TXCollection); override; // abstract;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetManager(const val: TgxPhysManager);
  published
    property DampingEnabled: Boolean read FDampingEnabled write FDampingEnabled;
    property Manager: TgxPhysManager read FManager write SetManager;
  end;

  (* A base for different types of force-field behaviours *)
  TgxBaseForceFieldEmitter = class(TgxBehaviour)
  private
    FManager: TgxPhysManager;
    FManagerName: String; // NOT persistent, temporarily used for persistence
  protected
    procedure Loaded; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    constructor Create(aOwner: TXCollection); override; // abstract;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetManager(const val: TgxPhysManager);
    function CalculateForceField(Body: TgxBaseSceneObject): TAffineVector; virtual;
  published
    property Manager: TgxPhysManager read FManager write SetManager;
  end;

  (* The Simple Physics Interaction (SPI) manager can only deal with objects from one scene
    More than one physics manager can be assigned to a scene *)
  TgxPhysManager = class(TComponent)
    // StateSize:Integer;
  protected
    fInertias: TList; // list of all inertias with manager = self
    fForceFieldEmitters: TList; // list of all forcefield emitters
    fForces: TgxForces; // Collection of forces acting on/between objects
    fDESolverType: TDESolverType;
    DESolver: TDESolver;
    fScene: TgxScene;
  protected
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteForces(stream: TStream);
    procedure ReadForces(stream: TStream);
    procedure SetForces(const val: TgxForces);
    function GetForces: TgxForces;
    procedure SetInertias(const val: TList);
    procedure SetForceFieldEmitters(const val: TList);
    procedure SetScene(const val: TgxScene);
  public
    procedure RegisterInertia(aInertia: TgxBaseInertia);
    procedure DeRegisterInertia(aInertia: TgxBaseInertia);
    procedure DeRegisterAllInertias;
    procedure RegisterForceFieldEmitter(aForceField: TgxBaseForceFieldEmitter);
    procedure DeRegisterForceFieldEmitter(aForceField: TgxBaseForceFieldEmitter);
    procedure DeRegisterAllForceFieldEmitters;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CalculateNextState(DeltaTime: Real);
    function CalculateKE(): Real;
    function CalculatePE(): Real;
    procedure SetDESolver(SolverType: TDESolverType);
    function FindObjectByName(Name: String): TgxBaseSceneObject;
    function FindForceFieldEmitterByName(Name: String): TgxBaseSceneObject;
    property Inertias: TList read fInertias write SetInertias; // stored False;
    property ForceFieldEmitters: TList read fForceFieldEmitters
      write SetForceFieldEmitters; // stored False;
  published
    property Forces: TgxForces read GetForces write SetForces; // stored False;
    property Solver: TDESolverType read fDESolverType write SetDESolver;
    property Scene: TgxScene read fScene write SetScene;
  end;

  TgxForces = class(TXCollection)
  protected
    function GetForce(index: Integer): TgxForce;
  public
    constructor Create(aOwner: TPersistent); override;
    // destructor Destroy;override;
    class function ItemsClass: TXCollectionItemClass; override;
    property Force[index: Integer]: TgxForce read GetForce; default;
    function CanAdd(aClass: TXCollectionItemClass): Boolean; override;
  end;

implementation // ------------------------------------------------------------

procedure TgxPhysManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  (* if Operation=opRemove then
    begin
    if AComponent=FScene then FScene:=nil;
    end;
  *)
end;

procedure TgxPhysManager.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('ForcesData', ReadForces, WriteForces,
    (Assigned(fForces) and (fForces.Count > 0)));
end;

procedure TgxPhysManager.Loaded;
begin
  inherited Loaded;
  if Assigned(fForces) then
    fForces.Loaded;
end;

function TgxPhysManager.FindObjectByName(Name: String): TgxBaseSceneObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fInertias.Count - 1 do
  begin
    if (TgxBaseInertia(fInertias.Items[i]).OwnerBaseSceneObject.GetNamePath =
      Name) then
    begin
      Result := TgxBaseInertia(fInertias.Items[i]).OwnerBaseSceneObject;
    end
    else if Owner.FindComponent(Name) <> nil then
    begin
      Result := TgxBaseSceneObject(Owner.FindComponent(Name));
    end;
  end;
end;

function TgxPhysManager.FindForceFieldEmitterByName(Name: String)
  : TgxBaseSceneObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fForceFieldEmitters.Count - 1 do
  begin
    if (TgxBaseForceFieldEmitter(fForceFieldEmitters.Items[i])
      .OwnerBaseSceneObject.GetNamePath = Name) then
    begin
      Result := TgxBaseForceFieldEmitter(fForceFieldEmitters.Items[i])
        .OwnerBaseSceneObject;
    end;
  end;
end;

procedure TgxPhysManager.WriteForces(stream: TStream);
var
  writer: TWriter;
begin
  // Writing forces
  writer := TWriter.Create(stream, 16384);
  try
    Forces.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TgxPhysManager.ReadForces(stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  try
    Forces.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TgxPhysManager.SetForces(const val: TgxForces);
begin
  Forces.Assign(val);
end;

procedure TgxPhysManager.SetInertias(const val: TList);
begin
  fInertias.Assign(val);
end;

procedure TgxPhysManager.SetForceFieldEmitters(const val: TList);
begin
  fForceFieldEmitters.Assign(val);
end;

procedure TgxPhysManager.SetScene(const val: TgxScene);
begin
  // fScene:=val;
  if fScene <> val then
  begin
    if Assigned(fScene) then
      fScene.RemoveFreeNotification(Self);
    fScene := val;
    if Assigned(fScene) then
      fScene.FreeNotification(Self);
  end;
end;

function TgxPhysManager.GetForces: TgxForces;
begin
  if not Assigned(fForces) then
    fForces := TgxForces.Create(Self);
  Result := fForces;
end;

// Not accurate yet, because Forces should be re-calculated for each KVector.
// Since forces will depend on distances between objects, then this will require
// a central physics manager, that calculates KVector for all objects, then calculate forces
// between objects for this new estimated state.
//

function TDESolver.StateToArray(): TStateArray;
var
  i { ,j } : Integer;
  currentpos: Integer;
  // state:TStateArray;
begin
  currentpos := 0;
  for i := 0 to Owner.fInertias.Count - 1 do
  begin
    TgxBaseInertia(Owner.fInertias.Items[i]).StateToArray(StateArray,
      currentpos);
    currentpos := currentpos + TgxBaseInertia(Owner.fInertias.Items[i])
      .StateSize;
  end;
  Result := StateArray;
end;

procedure TDESolver.ArrayToState(StateArray: TStateArray);
var
  i: Integer;
  currentpos: Integer;
begin
  currentpos := 0;
  for i := 0 to Owner.fInertias.Count - 1 do
  begin
    TgxBaseInertia(Owner.fInertias.Items[i]).ArrayToState(StateArray,
      currentpos);
    currentpos := currentpos + TgxBaseInertia(Owner.fInertias.Items[i])
      .StateSize;
  end;
end;

constructor TDESolver.Create(aOwner: TgxPhysManager);
begin
  Self.Owner := aOwner;
end;

destructor TDESolver.Destroy;
begin
  //
end;

function TDESolverExplicit.CalcStateDot(): TStateArray;
var
  i { ,j } : Integer;
  currentpos: Integer;
  state: TStateArray;
begin
  //
  SetLength(state, StateSize);
  for i := 0 to StateSize - 1 do
    state[i] := StateArray[i];
  // state:=StateArray;
  currentpos := 0;
  for i := 0 to Owner.fInertias.Count - 1 do
  begin
    TgxBaseInertia(Owner.fInertias.Items[i]).CalcStateDot(state, currentpos);
    currentpos := currentpos + TgxBaseInertia(Owner.fInertias.Items[i])
      .StateSize;
  end;
  Result := state;
end;

procedure TDESolverRungeKutta4.Solve(DeltaTime: Real);
var
  // X,X0:TStateArray;
  Kvectors: array [0 .. 3] of TStateArray;
  n: Integer;
  StateArray0: TStateArray;
  tempStateArray: TStateArray;
  // tempState:TgxBInertia;
begin
  // tempState:=TgxBInertia.Create(nil);
  // tempState.Assign(Self);
  tempStateArray := StateToArray();
  StateArray0 := tempStateArray;

  for n := 0 to 3 do
    SetLength(Kvectors[n], Length(StateArray0));

  Kvectors[0] := CalcStateDot();
  for n := 0 to StateSize - 1 do
    tempStateArray[n] := tempStateArray[n] + DeltaTime / 2 * Kvectors[0][n];
  ArrayToState(tempStateArray);

  Kvectors[1] := CalcStateDot();
  for n := 0 to StateSize - 1 do
    tempStateArray[n] := tempStateArray[n] + DeltaTime / 2 * Kvectors[1][n];
  ArrayToState(tempStateArray);

  Kvectors[2] := CalcStateDot();
  for n := 0 to StateSize - 1 do
    tempStateArray[n] := tempStateArray[n] + DeltaTime / 2 * Kvectors[2][n];
  ArrayToState(tempStateArray);

  Kvectors[3] := CalcStateDot();

  for n := 0 to StateSize - 1 do
  begin
    tempStateArray[n] := StateArray0[n] + DeltaTime / 6 *
      (Kvectors[0][n] + 2 * Kvectors[1][n] + 2 * Kvectors[2][n] +
      Kvectors[3][n]);
  end;

  ArrayToState(tempStateArray);

  // NormalizeQuaternion(AngularOrientation);
  // tempState.Free();
end;

procedure TDESolverEuler.Solve(DeltaTime: Real);
var
  i, j: Integer;
  tempState, tempStateDot: TStateArray;
  // force1:TAffineVector;
  Inertia1: TgxBaseInertia;
  tempForce: TAffineVector;
  // UnDampedMomentum,DampedMomentum:Real;
begin
{$IFDEF DEBUG}
  Write('Euler integration');
{$ENDIF}
  for i := 0 to Owner.fInertias.Count - 1 do
  begin
    Inertia1 := TgxBaseInertia(Owner.fInertias.Items[i]);
    // TgxRigidBodyInertia(FObjects.Items[i]).SetTorque(0,0,0);
    for j := 0 to Owner.fForceFieldEmitters.Count - 1 do
    begin
      Inertia1.CalculateForceFieldForce
        (TgxBaseForceFieldEmitter(Owner.fForceFieldEmitters.Items[j]));
      // Inertia1.ApplyForce(TgxForceFieldEmitter(FForceFieldEmitters.Items[j]).CalculateForceField(Inertia1.OwnerBaseSceneObject));
    end;
  end;

  for i := 0 to Owner.Forces.Count - 1 do
  begin
    { force1:= } Owner.Forces.Force[i].CalculateForce();
  end;

  tempState := StateToArray();
  tempStateDot := CalcStateDot();
  for i := 0 to StateSize - 1 do
    tempState[i] := tempState[i] + DeltaTime * tempStateDot[i];

  ArrayToState(tempState);

  for i := 0 to Owner.fInertias.Count - 1 do
  begin
    // TGLInertia(FObjects.Items[i]).SetForce(0,0,0);
    Inertia1 := TgxBaseInertia(Owner.fInertias.Items[i]);
    if Inertia1.DampingEnabled = true then
    begin
      // UnDampedMomentum:=VectorLength(Inertia1.TranslationSpeed.AsAffineVector);
      // DampedMomentum:= Inertia1.TranslationDamping.Calculate(UnDampedMomentum,deltaTime);
      // if  UnDampedMomentum<>0 then
      begin
        // ScaleVector(Inertia1.TranslationSpeed.AsAffineVector,DampedMomentum/UnDampedMomentum);
        // ScaleVector(Inertia1.LinearMomentum,DampedMomentum/UnDampedMomentum);
      end;
      // Inertia1.TranslationDamping.Calculate(VectorLength(Inertia1.LinearMomentum),deltaTime);
    end;
    Inertia1.CalcAuxiliary();
    Inertia1.RemoveForces();
  end;
  // NormalizeQuaternion(AngularOrientation);
end;

constructor TgxPhysManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fInertias := TList.Create();
  fForceFieldEmitters := TList.Create();
  fForces := TgxForces.Create(Self);
  SetDESolver(ssEuler);
  ///RegisterManager(Self);
end;

destructor TgxPhysManager.Destroy;
begin
  // fScene:=nil;
  DeRegisterAllInertias();
  DeRegisterAllForceFieldEmitters();
///  DeRegisterManager(Self);
  fInertias.Free();
  fForceFieldEmitters.Free();
  fForces.Free();
  inherited Destroy;
end;

procedure TgxPhysManager.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TgxPhysManager.SetDESolver(SolverType: TDESolverType);
var
  tempSolver: TDESolver;
begin
  if Assigned(DESolver) then
  begin
    if (fDESolverType <> SolverType) then
      case SolverType of
        ssRungeKutta4:
          begin
            // DESolver:=RungeKutta4;
          end;
        ssEuler:
          begin
            // DESolver:=Euler;
          end;
      end;
  end
  else
  begin
    // if (fDESolverType<>SolverType) then
    case SolverType of
      ssRungeKutta4:
        begin
          DESolver := TDESolverRungeKutta4.Create(Self);
        end;
      ssEuler:
        begin
          DESolver := TDESolverEuler.Create(Self);
        end;
    end;
    fDESolverType := SolverType;
  end;
end;

procedure TgxPhysManager.RegisterInertia(aInertia: TgxBaseInertia);
begin
  if Assigned(aInertia) then
    if fInertias.IndexOf(aInertia) < 0 then
    begin
      fInertias.Add(aInertia);
      aInertia.FManager := Self;
      DESolver.StateSize := DESolver.StateSize + aInertia.StateSize;
      SetLength(DESolver.StateArray, DESolver.StateSize);
    end;
end;

procedure TgxPhysManager.DeRegisterInertia(aInertia: TgxBaseInertia);
begin
  if Assigned(aInertia) then
  begin
    aInertia.FManager := nil;
    fInertias.Remove(aInertia);
    DESolver.StateSize := DESolver.StateSize - aInertia.StateSize;
    SetLength(DESolver.StateArray, DESolver.StateSize);
  end;

end;

procedure TgxPhysManager.DeRegisterAllInertias;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to fInertias.Count - 1 do
    TgxBaseInertia(fInertias[i]).FManager := nil;
  fInertias.Clear;
  DESolver.StateSize := 0;
  // SetLEngth(StateArray,0);
end;

procedure TgxPhysManager.RegisterForceFieldEmitter
  (aForceField: TgxBaseForceFieldEmitter);
begin
  if Assigned(aForceField) then
    if fForceFieldEmitters.IndexOf(aForceField) < 0 then
    begin
      fForceFieldEmitters.Add(aForceField);
      aForceField.FManager := Self;
    end;
end;

procedure TgxPhysManager.DeRegisterForceFieldEmitter
  (aForceField: TgxBaseForceFieldEmitter);
begin
  if Assigned(aForceField) then
  begin
    aForceField.FManager := nil;
    fForceFieldEmitters.Remove(aForceField);
  end;
end;

procedure TgxPhysManager.DeRegisterAllForceFieldEmitters;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to fForceFieldEmitters.Count - 1 do
    TgxBaseForceFieldEmitter(fForceFieldEmitters[i]).FManager := nil;
  fForceFieldEmitters.Clear;
end;

function TgxPhysManager.CalculateKE(): Real;
var
  Total: Real;
  i: Integer;
begin
  Total := 0;
  for i := 0 to fInertias.Count - 1 do
  begin
    // calculate fInertias[i] KE
    Total := Total + TgxBaseInertia(fInertias.Items[i]).CalculateKE();
  end;
  Result := Total;
end;

function TgxPhysManager.CalculatePE(): Real;
var
  Total: Real;
  i: Integer;
begin
  Total := 0;
  for i := 0 to fInertias.Count - 1 do
  begin
    // calculate fobject[i] PE
    Total := Total + TgxBaseInertia(fInertias.Items[i]).CalculatePE();
  end;
  Result := Total;
end;

procedure TgxPhysManager.CalculateNextState(DeltaTime: Real);
begin
  if Assigned(DESolver) then
    DESolver.Solve(DeltaTime);
end;

constructor TgxForces.Create(aOwner: TPersistent);
begin
  // Assert(aOwner is TgxBaseSceneObject);
  inherited Create(aOwner);
end;

{ destructor TgxForces.Destroy;
  begin
  inherited Destroy;
  end;
}

class function TgxForces.ItemsClass: TXCollectionItemClass;
begin
  Result := TgxForce;
end;

function TgxForces.GetForce(index: Integer): TgxForce;
begin
  Result := TgxForce(Items[index]);
end;

function TgxForces.CanAdd(aClass: TXCollectionItemClass): Boolean;
begin
  Result := { (not aClass.InheritsFrom(TGLEffect)) and }
    (inherited CanAdd(aClass));
end;

// -----------------------------------------------------------------------------

procedure TgxBaseInertia.SetManager(const val: TgxPhysManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterInertia(Self);
    if Assigned(val) then
      val.RegisterInertia(Self);
    // Write(val.GetNamePath);
  end;
end;

procedure TgxBaseInertia.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
///?    mng := FindManager(TgxPhysManager, FManagerName);
    if Assigned(mng) then
      Manager := TgxPhysManager(mng);
    FManagerName := '';
  end;
end;

procedure TgxBaseInertia.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    WriteBoolean(FDampingEnabled);
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
  end;
end;

procedure TgxBaseInertia.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FDampingEnabled := ReadBoolean;
    FManagerName := ReadString;
    Manager := nil;
  end;
  // Loaded;     //DB100
end;

constructor TgxBaseInertia.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FDampingEnabled := true;
end;

destructor TgxBaseInertia.Destroy;
begin
  SetManager(nil);
  inherited Destroy;
end;

procedure TgxBaseInertia.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    StateSize := TgxBaseInertia(Source).StateSize;
    FDampingEnabled := TgxBaseInertia(Source).DampingEnabled;
    Manager := TgxBaseInertia(Source).Manager;
  end;
  inherited Assign(Source);
end;

procedure TgxBaseInertia.StateToArray(var StateArray: TStateArray;
  StatePos: Integer);
begin
end;

procedure TgxBaseInertia.ArrayToState( { var } StateArray: TStateArray;
  StatePos: Integer);
begin
end;

procedure TgxBaseInertia.CalcStateDot(var StateArray: TStateArray;
  StatePos: Integer);
begin
end;

procedure TgxBaseInertia.RemoveForces();
begin
end;

procedure TgxBaseInertia.CalculateForceFieldForce(ForceFieldEmitter
  : TgxBaseForceFieldEmitter);
begin
end;

function TgxBaseInertia.CalculateKE(): Real;
begin
  Result := 0;
end;

function TgxBaseInertia.CalculatePE(): Real;
begin
  Result := 0;
end;

procedure TgxBaseInertia.CalcAuxiliary();
begin
end;

procedure TgxBaseInertia.SetUpStartingState();
begin
end;

// -----------------------------------------------------------------------------

procedure TgxBaseForceFieldEmitter.SetManager(const val: TgxPhysManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterForceFieldEmitter(Self);
    if Assigned(val) then
      val.RegisterForceFieldEmitter(Self);
  end;
end;

procedure TgxBaseForceFieldEmitter.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
///?    mng := FindManager(TgxPhysManager, FManagerName);
    if Assigned(mng) then
      Manager := TgxPhysManager(mng);
    FManagerName := '';
  end;
end;

procedure TgxBaseForceFieldEmitter.WriteToFiler(writer: TWriter);
begin
  inherited; // Dan Bartlett
  with writer do
  begin
    WriteInteger(0); // Archive Version 0
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
  end;
end;

procedure TgxBaseForceFieldEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    ReadInteger; // ignore archiveVersion
    FManagerName := ReadString;
    Manager := nil;
  end;
  // Loaded;  //DB100
end;

constructor TgxBaseForceFieldEmitter.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
end;

destructor TgxBaseForceFieldEmitter.Destroy;
begin
  SetManager(nil);
  inherited Destroy;
end;

procedure TgxBaseForceFieldEmitter.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    Manager := TgxBaseForceFieldEmitter(Source).Manager;
  end;
  inherited Assign(Source);
end;

// CalculateForceField
function TgxBaseForceFieldEmitter.CalculateForceField(Body: TgxBaseSceneObject)
  : TAffineVector;
begin
  Result := nullVector;
end;

initialization // ------------------------------------------------------------

// RegisterClasses([TgxForces]);
// RegisterClasses([TgxPhysManager, TgxBaseInertia, TgxBaseForceFieldEmitter]);
// RegisterXCollectionItemClass(TgxBaseInertia);
// RegisterXCollectionItemClass(TgxBaseForceFieldEmitter);
// RegisterXCollectionItemClass(TGLPhysicsForce);

// ------------------------------------------------------------------

end.
