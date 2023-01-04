//
// The graphics platform GLScene https://github.com/glscene
//
unit Physics.GLxManager;

(* The GLxManager for GLScene Physics *)

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Dialogs,

  GLS.VectorTypesExt,
  GLS.VectorGeometry,
  GLS.Scene,
  GLS.Coordinates,
  GLS.XCollection,

  GLS.Behaviours;

type
  TGLxForce = class;
  TGLxForceType = (ftHookes, ftGravitation, ftCustom);

  TGLxOnCustomForce = procedure() of object;

  TGLxForces = class;
  TGLxBaseForceFieldEmitter = class;

  // only ssEuler is usable at the moment
  TGLxSolverType = (ssEuler, ssRungeKutta4, ssVerlet);
  // TGLxSolver = procedure((*RigidBody:TGLRigidBody;*)DeltaTime:Real) of object;
  TGLxManager = class;

  (*
    ***Euler***, EulerImproved, EulerModified, MidPoint
    RungeKutta2, ***RungeKutta4***, RungKutta4Adaptive
    State Variables:  Position,  Velocity

    Verlet
    State Variables:  Position, Old Position
  *)

  // need to have state array(s) seperate from inertias to allow for implicit & explicit methods
  TGLxSolver = class(TObject)
  public
    StateSize: Integer;
    StateArray: TDoubleArray;
    Owner: TGLxManager;
    function StateToArray(): TDoubleArray; virtual;
    procedure ArrayToState(StateArray: TDoubleArray); virtual;
    procedure Solve(DeltaTime: Real); virtual; abstract;
    constructor Create(aOwner: TGLxManager); // override; //abstract;
    destructor Destroy; override;
    // procedure Assign(Source: TPersistent); override;
  end;

  // explicit   e.g. Euler, Mid-point, Runge-Kutta integration
  TGLxSolverExplicit = class(TGLxSolver)
  public
    StateArrayDot: TDoubleArray; // Velocity stored
    function CalcStateDot(): TDoubleArray; virtual;
  end;

  TGLxSolverEuler = class(TGLxSolverExplicit)
  public
    procedure Solve(DeltaTime: Real); override;
  end;

  TGLxSolverRungeKutta4 = class(TGLxSolverExplicit)
  public
    procedure Solve(DeltaTime: Real); override;
  end;

  // implicit   e.g. Verlet Integration
  TGLxSolverImplicit = class(TGLxSolver)
  public
    LastStateArray: TDoubleArray; // Last state stored
  end;

  TGLxSolverVerlet = class(TGLxSolverImplicit)
  public
  end;

  (* purpose of TGLxBaseInertia is to allow for inertias that may be constrained
    to 1 or 2 dimensions
    Shouldn't be used directly, instead use TGLParticleInertia (for a 3D particle)
    TGLRigidBodyInertia (for a 3D rigid-body) or define a new sub-class
    e.g.  TGL1DParticleInertia, this will allow for faster speed *)
  TGLxBaseInertia = class(TGLBehaviour)
  private
    FDampingEnabled: Boolean;
    FManager: TGLxManager;
    FManagerName: String; // NOT persistent, temporarily used for persistence
  protected
    procedure Loaded; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    StateSize: Integer; // don't re-declare this in sub-classes
    // just initialise it in constructor
    procedure StateToArray(var StateArray: TDoubleArray; StatePos: Integer); virtual;
    procedure ArrayToState( { var } StateArray: TDoubleArray; StatePos: Integer); virtual;
    procedure CalcStateDot(var StateArray: TDoubleArray; StatePos: Integer); virtual;
    procedure RemoveForces(); virtual;
    procedure CalculateForceFieldForce(ForceFieldEmitter: TGLxBaseForceFieldEmitter); virtual;
    procedure CalcAuxiliary(); virtual;
    procedure SetUpStartingState(); virtual;
    function CalculateKE(): Real; virtual;
    function CalculatePE(): Real; virtual;
    constructor Create(aOwner: TXCollection); override; // abstract;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetManager(const val: TGLxManager);
  published
    property DampingEnabled: Boolean read FDampingEnabled write FDampingEnabled;
    property Manager: TGLxManager read FManager write SetManager;
  end;

  (* A base for different types of force-field behaviours *)
  TGLxBaseForceFieldEmitter = class(TGLBehaviour)
  private
    FManager: TGLxManager;
    FManagerName: String; // NOT persistent, temporarily used for persistence
  protected
    procedure Loaded; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    constructor Create(aOwner: TXCollection); override; // abstract;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetManager(const val: TGLxManager);
    function CalculateForceField(Body: TGLBaseSceneObject): TAffineVector; virtual;
  published
    property Manager: TGLxManager read FManager write SetManager;
  end;

  TGLxUniformGravityEmitter = class(TGLxBaseForceFieldEmitter)
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

  TGLxRadialGravityEmitter = class(TGLxBaseForceFieldEmitter)
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

  TGLxDampingFieldEmitter = class(TGLxBaseForceFieldEmitter)
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

  (* The Simple Physics Interaction (SPI) manager can only deal with objects from one scene
    More than one physics manager can be assigned to a scene *)
  TGLxManager = class(TComponent)
    // StateSize:Integer;
  protected
    fInertias: TList; // list of all inertias with manager = self
    fForceFieldEmitters: TList; // list of all forcefield emitters
    fForces: TGLxForces; // Collection of forces acting on/between objects
    fDESolverType: TGLxSolverType;
    DESolver: TGLxSolver;
    fScene: TGLScene;
  protected
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteForces(stream: TStream);
    procedure ReadForces(stream: TStream);
    procedure SetForces(const val: TGLxForces);
    function GetForces: TGLxForces;
    procedure SetInertias(const val: TList);
    procedure SetForceFieldEmitters(const val: TList);
    procedure SetScene(const val: TGLScene);
  public
    procedure RegisterInertia(aInertia: TGLxBaseInertia);
    procedure DeRegisterInertia(aInertia: TGLxBaseInertia);
    procedure DeRegisterAllInertias;
    procedure RegisterForceFieldEmitter(aForceField: TGLxBaseForceFieldEmitter);
    procedure DeRegisterForceFieldEmitter(aForceField: TGLxBaseForceFieldEmitter);
    procedure DeRegisterAllForceFieldEmitters;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure CalculateNextState(DeltaTime: Real);
    function CalculateKE(): Real;
    function CalculatePE(): Real;
    procedure SetDESolver(SolverType: TGLxSolverType);
    function FindObjectByName(Name: String): TGLBaseSceneObject;
    function FindForceFieldEmitterByName(Name: String): TGLBaseSceneObject;
    property Inertias: TList read fInertias write SetInertias; // stored False;
    property ForceFieldEmitters: TList read fForceFieldEmitters write SetForceFieldEmitters;
    // stored False;
  published
    property Forces: TGLxForces read GetForces write SetForces; // stored False;
    property Solver: TGLxSolverType read fDESolverType write SetDESolver;
    property Scene: TGLScene read fScene write SetScene;
  end;

   TGLxForce = class(TXCollectionItem)
  private
    fObject1: TGLBaseSceneObject;
    fObject2: TGLBaseSceneObject;
    fposition1: TGLCoordinates;
    fposition2: TGLCoordinates;
    object1Name: String;
    object2Name: String;
    // fOnCustomForce: TGLxOnCustomForce;
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
    // property OnCustomForce:TGLxOnCustomForce read fOnCustomForce write fOnCustomForce;
  end;

  TGLxHookSpring = class(TGLxForce)
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

  TGLxHookStrings = class(TGLxHookSpring)
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



  TGLxForces = class(TXCollection)
  protected
    function GetForce(index: Integer): TGLxForce;
  public
    constructor Create(aOwner: TPersistent); override;
    // destructor Destroy;override;
    class function ItemsClass: TXCollectionItemClass; override;
    property Force[index: Integer]: TGLxForce read GetForce; default;
    function CanAdd(aClass: TXCollectionItemClass): Boolean; override;
  end;


const
  GravitationalConstant = 6.6726E-11;

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

uses
  Physics.GLxInertias;


// ------------------------------------------------------------------
// ------------------------TGLxSolver--------------------------------
// ------------------------------------------------------------------

// Not accurate yet, because Forces should be re-calculated for each KVector.
// Since forces will depend on distances between objects, then this will require
// a central physics manager, that calculates KVector for all objects, then calculate forces
// between objects for this new estimated state.
function TGLxSolver.StateToArray(): TDoubleArray;
var
  i { ,j } : Integer;
  currentpos: Integer;
begin
  currentpos := 0;
  for i := 0 to Owner.fInertias.Count - 1 do
  begin
    TGLxBaseInertia(Owner.fInertias.Items[i]).StateToArray(StateArray, currentpos);
    currentpos := currentpos + TGLxBaseInertia(Owner.fInertias.Items[i]).StateSize;
  end;
  Result := StateArray;
end;

procedure TGLxSolver.ArrayToState(StateArray: TDoubleArray);
var
  i: Integer;
  currentpos: Integer;
begin
  currentpos := 0;
  for i := 0 to Owner.fInertias.Count - 1 do
  begin
    TGLxBaseInertia(Owner.fInertias.Items[i]).ArrayToState(StateArray, currentpos);
    currentpos := currentpos + TGLxBaseInertia(Owner.fInertias.Items[i]).StateSize;
  end;
end;

constructor TGLxSolver.Create(aOwner: TGLxManager);
begin
  Self.Owner := aOwner;
end;

destructor TGLxSolver.Destroy;
begin
  //
end;


// -------------------------------------------------------------------
// ------------------------TGLxManager--------------------------------
// -------------------------------------------------------------------

constructor TGLxManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fInertias := TList.Create();
  fForceFieldEmitters := TList.Create();
  fForces := TGLxForces.Create(Self);
  SetDESolver(ssEuler);
  /// RegisterManager(Self);
end;

procedure TGLxManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  (*
  if Operation=opRemove then
  begin
    if AComponent=FScene then FScene:=nil;
  end;
  *)
end;

procedure TGLxManager.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('ForcesData', ReadForces, WriteForces,
    (Assigned(fForces) and (fForces.Count > 0)));
end;

procedure TGLxManager.Loaded;
begin
  inherited Loaded;
  if Assigned(fForces) then
    fForces.Loaded;
end;

function TGLxManager.FindObjectByName(Name: String): TGLBaseSceneObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fInertias.Count - 1 do
  begin
    if (TGLxBaseInertia(fInertias.Items[i]).OwnerBaseSceneObject.GetNamePath = Name) then
    begin
      Result := TGLxBaseInertia(fInertias.Items[i]).OwnerBaseSceneObject;
    end
    else if Owner.FindComponent(Name) <> nil then
    begin
      Result := TGLBaseSceneObject(Owner.FindComponent(Name));
    end;
  end;
end;

function TGLxManager.FindForceFieldEmitterByName(Name: String): TGLBaseSceneObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to fForceFieldEmitters.Count - 1 do
  begin
    if (TGLxBaseForceFieldEmitter(fForceFieldEmitters.Items[i])
      .OwnerBaseSceneObject.GetNamePath = Name) then
    begin
      Result := TGLxBaseForceFieldEmitter(fForceFieldEmitters.Items[i]).OwnerBaseSceneObject;
    end;
  end;
end;

procedure TGLxManager.WriteForces(stream: TStream);
var
  writer: TWriter;
begin
  // messagedlg('Writing forces',mtInformation,[mbOk],0);
  writer := TWriter.Create(stream, 16384);
  try
    Forces.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TGLxManager.ReadForces(stream: TStream);
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

procedure TGLxManager.SetForces(const val: TGLxForces);
begin
  Forces.Assign(val);
end;

procedure TGLxManager.SetInertias(const val: TList);
begin
  fInertias.Assign(val);
end;

procedure TGLxManager.SetForceFieldEmitters(const val: TList);
begin
  fForceFieldEmitters.Assign(val);
end;

procedure TGLxManager.SetScene(const val: TGLScene);
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

function TGLxManager.GetForces: TGLxForces;
begin
  if not Assigned(fForces) then
    fForces := TGLxForces.Create(Self);
  Result := fForces;
end;

destructor TGLxManager.Destroy;
begin
  // fScene:=nil;
  DeRegisterAllInertias();
  DeRegisterAllForceFieldEmitters();
  /// DeRegisterManager(Self);
  fInertias.Free();
  fForceFieldEmitters.Free();
  fForces.Free();
  inherited Destroy;
end;

procedure TGLxManager.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

procedure TGLxManager.SetDESolver(SolverType: TGLxSolverType);
var
  tempSolver: TGLxSolver;
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
          DESolver := TGLxSolverRungeKutta4.Create(Self);
        end;
      ssEuler:
        begin
          DESolver := TGLxSolverEuler.Create(Self);
        end;
    end;
    fDESolverType := SolverType;
  end;
end;

procedure TGLxManager.RegisterInertia(aInertia: TGLxBaseInertia);
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

procedure TGLxManager.DeRegisterInertia(aInertia: TGLxBaseInertia);
begin
  if Assigned(aInertia) then
  begin
    aInertia.FManager := nil;
    fInertias.Remove(aInertia);
    DESolver.StateSize := DESolver.StateSize - aInertia.StateSize;
    SetLength(DESolver.StateArray, DESolver.StateSize);
  end;
end;

procedure TGLxManager.DeRegisterAllInertias;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to fInertias.Count - 1 do
    TGLxBaseInertia(fInertias[i]).FManager := nil;
  fInertias.Clear;
  DESolver.StateSize := 0;
  // SetLEngth(StateArray,0);
end;

procedure TGLxManager.RegisterForceFieldEmitter(aForceField: TGLxBaseForceFieldEmitter);
begin
  if Assigned(aForceField) then
    if fForceFieldEmitters.IndexOf(aForceField) < 0 then
    begin
      fForceFieldEmitters.Add(aForceField);
      aForceField.FManager := Self;
    end;
end;

procedure TGLxManager.DeRegisterForceFieldEmitter(aForceField: TGLxBaseForceFieldEmitter);
begin
  if Assigned(aForceField) then
  begin
    aForceField.FManager := nil;
    fForceFieldEmitters.Remove(aForceField);
  end;
end;

procedure TGLxManager.DeRegisterAllForceFieldEmitters;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to fForceFieldEmitters.Count - 1 do
    TGLxBaseForceFieldEmitter(fForceFieldEmitters[i]).FManager := nil;
  fForceFieldEmitters.Clear;
end;

function TGLxManager.CalculateKE(): Real;
var
  Total: Real;
  i: Integer;
begin
  Total := 0;
  for i := 0 to fInertias.Count - 1 do
  begin
    // calculate fInertias[i] KE
    Total := Total + TGLxBaseInertia(fInertias.Items[i]).CalculateKE();
  end;
  Result := Total;
end;

function TGLxManager.CalculatePE(): Real;
var
  Total: Real;
  i: Integer;
begin
  Total := 0;
  for i := 0 to fInertias.Count - 1 do
  begin
    // calculate fobject[i] PE
    Total := Total + TGLxBaseInertia(fInertias.Items[i]).CalculatePE();
  end;
  Result := Total;
end;

procedure TGLxManager.CalculateNextState(DeltaTime: Real);
begin
  if Assigned(DESolver) then
    DESolver.Solve(DeltaTime);
end;


function TGLxSolverExplicit.CalcStateDot(): TDoubleArray;
var
  i { ,j } : Integer;
  currentpos: Integer;
  state: TDoubleArray;
begin
  //
  SetLength(state, StateSize);
  for i := 0 to StateSize - 1 do
    state[i] := StateArray[i];
  // state:=StateArray;
  currentpos := 0;
  for i := 0 to Owner.fInertias.Count - 1 do
  begin
    TGLxBaseInertia(Owner.fInertias.Items[i]).CalcStateDot(state, currentpos);
    currentpos := currentpos + TGLxBaseInertia(Owner.fInertias.Items[i]).StateSize;
  end;
  Result := state;
end;

procedure TGLxSolverRungeKutta4.Solve(DeltaTime: Real);
var
  // X,X0:TDoubleArray;
  Kvectors: array [0 .. 3] of TDoubleArray;
  n: Integer;
  StateArray0: TDoubleArray;
  tempStateArray: TDoubleArray;
  // tempState:TGLBInertia;
begin
  // tempState:=TGLBInertia.Create(nil);
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
      (Kvectors[0][n] + 2 * Kvectors[1][n] + 2 * Kvectors[2][n] + Kvectors[3][n]);
  end;
  ArrayToState(tempStateArray);
  // NormalizeQuaternion(AngularOrientation);
  // tempState.Free();
end;

procedure TGLxSolverEuler.Solve(DeltaTime: Real);
var
  i, j: Integer;
  tempState, tempStateDot: TDoubleArray;
  // force1:TAffineVector;
  Inertia1: TGLxBaseInertia;
  tempForce: TAffineVector;
  // UnDampedMomentum,DampedMomentum:Real;
begin
{$IFDEF DEBUG}
  messagedlg('Euler integration', mtinformation, [mbok], 0);
{$ENDIF}
  for i := 0 to Owner.fInertias.Count - 1 do
  begin
    Inertia1 := TGLxBaseInertia(Owner.fInertias.Items[i]);
    // TGLRigidBodyInertia(FObjects.Items[i]).SetTorque(0,0,0);
    for j := 0 to Owner.fForceFieldEmitters.Count - 1 do
    begin
      Inertia1.CalculateForceFieldForce
        (TGLxBaseForceFieldEmitter(Owner.fForceFieldEmitters.Items[j]));
      // Inertia1.ApplyForce(TGLForceFieldEmitter(FForceFieldEmitters.Items[j]).CalculateForceField(Inertia1.OwnerBaseSceneObject));
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
    Inertia1 := TGLxBaseInertia(Owner.fInertias.Items[i]);
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


// -------------------------------------------------------------------
// ------------------------TGLxForces---------------------------------
// -------------------------------------------------------------------
constructor TGLxForces.Create(aOwner: TPersistent);
begin
  // Assert(aOwner is TGLBaseSceneObject);
  inherited Create(aOwner);
end;

class function TGLxForces.ItemsClass: TXCollectionItemClass;
begin
  Result := TGLxForce;
end;

function TGLxForces.GetForce(index: Integer): TGLxForce;
begin
  Result := TGLxForce(Items[index]);
end;

function TGLxForces.CanAdd(aClass: TXCollectionItemClass): Boolean;
begin
  Result := { (not aClass.InheritsFrom(TGLEffect)) and }
    (inherited CanAdd(aClass));
end;

// -------------------------------------------------------------------
// ------------------------TGLxBaseInertia----------------------------
// -------------------------------------------------------------------
procedure TGLxBaseInertia.SetManager(const val: TGLxManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterInertia(Self);
    if Assigned(val) then
      val.RegisterInertia(Self);
    // messagedlg(val.GetNamePath,mtinformation,[mbok],0);
  end;
end;

procedure TGLxBaseInertia.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    /// ?    mng := FindManager(TGLxManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLxManager(mng);
    FManagerName := '';
  end;
end;

procedure TGLxBaseInertia.WriteToFiler(writer: TWriter);
begin
  inherited; // Dan Bartlett
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

procedure TGLxBaseInertia.ReadFromFiler(reader: TReader);
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

constructor TGLxBaseInertia.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FDampingEnabled := true;
end;

destructor TGLxBaseInertia.Destroy;
begin
  SetManager(nil);
  inherited Destroy;
end;

procedure TGLxBaseInertia.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    StateSize := TGLxBaseInertia(Source).StateSize;
    FDampingEnabled := TGLxBaseInertia(Source).DampingEnabled;
    Manager := TGLxBaseInertia(Source).Manager;
  end;
  inherited Assign(Source);
end;

procedure TGLxBaseInertia.StateToArray(var StateArray: TDoubleArray; StatePos: Integer);
begin
end;

procedure TGLxBaseInertia.ArrayToState( { var } StateArray: TDoubleArray; StatePos: Integer);
begin
end;

procedure TGLxBaseInertia.CalcStateDot(var StateArray: TDoubleArray; StatePos: Integer);
begin
end;

procedure TGLxBaseInertia.RemoveForces();
begin
end;

procedure TGLxBaseInertia.CalculateForceFieldForce(ForceFieldEmitter: TGLxBaseForceFieldEmitter);
begin
end;

function TGLxBaseInertia.CalculateKE(): Real;
begin
  Result := 0;
end;

function TGLxBaseInertia.CalculatePE(): Real;
begin
  Result := 0;
end;

procedure TGLxBaseInertia.CalcAuxiliary();
begin
end;

procedure TGLxBaseInertia.SetUpStartingState();
begin
end;

// -------------------------------------------------------------------
// ------------------------TGLxBaseForceFieldEmitter------------------
// -------------------------------------------------------------------

procedure TGLxBaseForceFieldEmitter.SetManager(const val: TGLxManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterForceFieldEmitter(Self);
    if Assigned(val) then
      val.RegisterForceFieldEmitter(Self);
  end;
end;

procedure TGLxBaseForceFieldEmitter.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    /// ?    mng := FindManager(TGLxManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLxManager(mng);
    FManagerName := '';
  end;
end;

procedure TGLxBaseForceFieldEmitter.WriteToFiler(writer: TWriter);
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

procedure TGLxBaseForceFieldEmitter.ReadFromFiler(reader: TReader);
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

constructor TGLxBaseForceFieldEmitter.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
end;

destructor TGLxBaseForceFieldEmitter.Destroy;
begin
  SetManager(nil);
  inherited Destroy;
end;

procedure TGLxBaseForceFieldEmitter.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    Manager := TGLxBaseForceFieldEmitter(Source).Manager;
  end;
  inherited Assign(Source);
end;

// CalculateForceField
function TGLxBaseForceFieldEmitter.CalculateForceField(Body: TGLBaseSceneObject): TAffineVector;
begin
  Result := nullVector;
end;

// -------------------------------------
// ---- TGLxUniformGravityEmitter
// -------------------------------------
constructor TGLxUniformGravityEmitter.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  fGravity := TGLCoordinates.CreateInitialized(Self, nullHmgVector, csVector);
end;

destructor TGLxUniformGravityEmitter.Destroy;
begin
  fGravity.Free;
  inherited Destroy;
end;

procedure TGLxUniformGravityEmitter.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    fGravity := TGLxUniformGravityEmitter(Source).fGravity;
  end;
end;

class function TGLxUniformGravityEmitter.FriendlyName: String;
begin
  Result := 'Uniform Gravity';
end;

class function TGLxUniformGravityEmitter.FriendlyDescription: String;
begin
  Result := 'Uniform Gravity, appropriate near surface of planet';
end;

class function TGLxUniformGravityEmitter.UniqueItem: Boolean;
begin
  Result := false;
end;

procedure TGLxUniformGravityEmitter.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    fGravity.WriteToFiler(writer);
  end;
end;

procedure TGLxUniformGravityEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    fGravity.ReadFromFiler(reader);
  end;
end;

procedure TGLxUniformGravityEmitter.SetGravity(const val: TGLCoordinates);
begin
  fGravity.Assign(val);
end;

// CalculateForceField  (TODO: ParticleInertia -> BaseInertia, add BaseInertia.ApplyAcceleration)
function TGLxUniformGravityEmitter.CalculateForceField(Body: TGLBaseSceneObject)
  : TAffineVector;
var
  inertia1: TGLxParticleInertia;
begin
  inertia1 := TGLxParticleInertia
    (Body.Behaviours.GetByClass(TGLxParticleInertia));
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

constructor TGLxRadialGravityEmitter.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
end;

destructor TGLxRadialGravityEmitter.Destroy;
begin
  inherited Destroy;
end;

procedure TGLxRadialGravityEmitter.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    fMass := TGLxRadialGravityEmitter(Source).fMass;
  end;
end;

class function TGLxRadialGravityEmitter.FriendlyName: String;
begin
  Result := 'Radial Gravity';
end;

class function TGLxRadialGravityEmitter.FriendlyDescription: String;
begin
  Result := 'Radial Gravity, can be applied anywhere (use for planets)';
end;

class function TGLxRadialGravityEmitter.UniqueItem: Boolean;
begin
  Result := false;
end;

procedure TGLxRadialGravityEmitter.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteFloat(fMass);
  end;
end;

procedure TGLxRadialGravityEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    fMass := ReadFloat();;
  end;
end;

// CalculateForceField (TODO: ParticleInertia -> BaseInertia if possible)
function TGLxRadialGravityEmitter.CalculateForceField(Body: TGLBaseSceneObject)
  : TAffineVector;
var
  inertia1: TGLxParticleInertia;
  R: TAffineVector;
  L: Real;
begin
  inertia1 := TGLxParticleInertia
    (Body.Behaviours.GetByClass(TGLxParticleInertia));
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
constructor TGLxDampingFieldEmitter.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  fDamping := TGLDamping.Create(Self);
end;

destructor TGLxDampingFieldEmitter.Destroy;
begin
  fDamping.Free;
  inherited Destroy;
end;

procedure TGLxDampingFieldEmitter.Assign(Source: TPersistent);
begin
  if Source.ClassType = Self.ClassType then
  begin
    fDamping := TGLxDampingFieldEmitter(Source).fDamping;
  end;
end;

class function TGLxDampingFieldEmitter.FriendlyName: String;
begin
  Result := 'Damping Field';
end;

class function TGLxDampingFieldEmitter.FriendlyDescription: String;
begin
  Result := 'Damping Field, to approximate air/fluid resistance';
end;

class function TGLxDampingFieldEmitter.UniqueItem: Boolean;
begin
  Result := false;
end;

procedure TGLxDampingFieldEmitter.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    fDamping.WriteToFiler(writer);
  end;
end;

procedure TGLxDampingFieldEmitter.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    fDamping.ReadFromFiler(reader);
  end;
end;

procedure TGLxDampingFieldEmitter.SetDamping(const val: TGLDamping);
begin
  fDamping.Assign(val);
end;

// CalculateForceField (TODO: ParticleInertia -> BaseInertia, BaseInertia.ApplyDamping?)
function TGLxDampingFieldEmitter.CalculateForceField(Body: TGLBaseSceneObject): TAffineVector;
var
  inertia1: TGLxParticleInertia;
  // velocity:TAffineVector;
  // v:Real;
begin
  inertia1 := TGLxParticleInertia
    (Body.Behaviours.GetByClass(TGLxParticleInertia));
  if Assigned(inertia1) then
    inertia1.ApplyDamping(Damping);

  (*
    Inertia1 := TGLxParticleInertia(Body.Behaviours.GetByClass(TGLxParticleInertia));
    if Assigned(inertia1) then
    begin
    velocity := VectorScale(inertia1.LinearMomentum, 1/Inertia1.Mass); // v = p/m
    //apply force in opposite direction to velocity
    v := VectorLength(velocity);
    //  F = -Normalised(V)*( Constant + (Linear)*(V) + (Quadtratic)*(V)*(V) )
    Result := VectorScale(VectorNormalize(velocity),
              -(fDamping.Constant + fDamping.Linear*v + fDamping.Quadratic*v*v));
    inertia1.ApplyForce(Result);
    end
    else
    Result:=nullvector;
  *)
end;

// -------------------------------------------------------------------
// ------------------------------  TGLxForce -------------------------
// -------------------------------------------------------------------

constructor TGLxForce.Create(aOwner: TXCollection);
begin
  inherited; // Create(aOwner)
  fposition1 := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  fposition2 := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
  // fObject1:=TGLBaseSceneObject.Create(Self);
  // fObject2:=TGLBaseSceneObject.Create(Self);
end;

destructor TGLxForce.Destroy;
begin
  fposition1.Free();
  fposition2.Free();
  // SetObject1(nil);
  // SetObject2(nil);
  // fObject1.Free();
  // fObject2.Free();

  inherited Destroy;
end;

procedure TGLxForce.Assign(Source: TPersistent);
begin
  // inherited Assign(Source);
  fposition1.Assign(TGLxForce(Source).fposition1);
  fposition2.Assign(TGLxForce(Source).fposition2);
  Object1 := TGLxForce(Source).Object1;
  Object2 := TGLxForce(Source).Object2;
  inherited Assign(Source);
end;

procedure TGLxForce.SetObject1(const val: TGLBaseSceneObject);
begin
  // if val.Behaviours.IndexOfClass(TGLBaseInertia) >=0 then
  fObject1 := val
  // else
  // Messagedlg('Object1 does not have an inertia behaviour',mtWarning,[mbOk],0);
end;

procedure TGLxForce.SetObject2(const val: TGLBaseSceneObject);
begin
  // if val.Behaviours.IndexOfClass(TGLBaseInertia) >=0 then
  fObject2 := val
  // else
  // messagedlg('Object2 does not have an inertia behaviour',mtWarning,[mbOk],0);
end;

procedure TGLxForce.SetPosition1(const val: TGLCoordinates);
begin
  fposition1.Assign(val); // DB101
end;

procedure TGLxForce.SetPosition2(const val: TGLCoordinates);
begin
  fposition2.Assign(val);
end;

procedure TGLxForce.Loaded;
var
  PhysMan: TGLxManager;
begin
  inherited Loaded;
  // not nice at all! ?
  // assumes owner is TGLxForces belonging to TGLxPhysicsManager
  PhysMan := TGLxManager(Self.Owner.Owner);
  if (object1Name <> '') then
  begin
    // PhysMan:=TGLPhysicsManager(Self.Owner.Owner);
    fObject1 := PhysMan.FindObjectByName(object1Name);
    // fObject1 := TGLBaseSceneObject(FindComponent(Object1Name));
    // Object1Name:='';
  end;

  if object2Name <> '' then
  begin
    fObject2 := PhysMan.FindObjectByName(object2Name);
    // Object2Name:='';
  end;
end;

class function TGLxForce.FriendlyName: String;
begin
  Result := 'Force';
end;

class function TGLxForce.FriendlyDescription: String;
begin
  Result := 'Physics Force';
end;

class function TGLxForce.UniqueItem: Boolean;
begin
  Result := false;
end;

procedure TGLxForce.WriteToFiler(writer: TWriter);
begin
  inherited WriteToFiler(writer);
  // MessageDlg('Writing to filer'+ GetNamePath,mtInformation,[mbOk],0);
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

procedure TGLxForce.ReadFromFiler(reader: TReader);
begin
  // MessageDlg('Reading from filer' + GetNamePath,mtInformation,[mbOk],0);
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

procedure TGLxForce.SetName(const val: String);
begin
  inherited SetName(val);
  // if Assigned(vGLBehaviourNameChangeEvent) then
  // vGLBehaviourNameChangeEvent(Self);
end;

function TGLxForce.CalculateForce(): TAffineVector;
begin
  //
end;

constructor TGLxHookSpring.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  fNaturalLength := 1;
  fElasticity := 1;
  fDamping := TGLDamping.Create(Self);
end;

destructor TGLxHookSpring.Destroy;
begin
  fDamping.Free;
  inherited Destroy;
end;

procedure TGLxHookSpring.WriteToFiler(writer: TWriter);
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

procedure TGLxHookSpring.ReadFromFiler(reader: TReader);
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

procedure TGLxHookSpring.SetDamping(const val: TGLDamping);
begin
  fDamping.Assign(val);
end;

function TGLxHookSpring.CalculateForce(): TAffineVector;
var
  rvector, vvector: TAffineVector;
  Inertia1, Inertia2: TGLxParticleInertia;
begin
  if (fObject1 = nil) or (fObject2 = nil) then
    Exit;
  Inertia2 := TGLxParticleInertia
    (Object2.Behaviours.GetByClass(TGLxParticleInertia));
  Inertia1 := TGLxParticleInertia
    (Object1.Behaviours.GetByClass(TGLxParticleInertia));

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

class function TGLxHookSpring.FriendlyName: String;
begin
  Result := 'Hookes Spring';
end;

class function TGLxHookSpring.FriendlyDescription: String;
begin
  Result := 'A spring obeying Hookes Law';
end;

class function TGLxHookSpring.UniqueItem: Boolean;
begin
  Result := false;
end;

constructor TGLxHookStrings.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
end;

destructor TGLxHookStrings.Destroy;
begin
  inherited Destroy;
end;

class function TGLxHookStrings.FriendlyName: String;
begin
  Result := 'Hookes String';
end;

class function TGLxHookStrings.FriendlyDescription: String;
begin
  Result := 'A string (that can go slack) obeying Hookes Law';
end;

class function TGLxHookStrings.UniqueItem: Boolean;
begin
  Result := false;
end;

function TGLxHookStrings.CalculateForce(): TAffineVector;
var
  rvector: TAffineVector;
  Inertia1, Inertia2: TGLxParticleInertia;
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
    Inertia2 := TGLxParticleInertia
      (Object2.Behaviours.GetByClass(TGLxParticleInertia));
    Inertia1 := TGLxParticleInertia
      (Object1.Behaviours.GetByClass(TGLxParticleInertia));
    if Assigned(Inertia2) then
      Inertia2.ApplyForce(Position2.AsAffineVector, VectorNegate(Result));
    if Assigned(Inertia1) then
      Inertia1.ApplyForce(Position1.AsAffineVector, Result);
  end;
  // Result:= inherited CalculateForce();
  // if (fLength < fNaturalLength) then Result:=NullVector;
end;



// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterXCollectionItemClass(TGLxHookSpring);
RegisterXCollectionItemClass(TGLxHookStrings);

RegisterXCollectionItemClass(TGLxUniformGravityEmitter);
RegisterXCollectionItemClass(TGLxRadialGravityEmitter);
RegisterXCollectionItemClass(TGLxDampingFieldEmitter);


RegisterClasses([TGLxForces]);
RegisterClasses([TGLxManager, TGLxBaseInertia, TGLxBaseForceFieldEmitter]);
RegisterXCollectionItemClass(TGLxBaseInertia);
RegisterXCollectionItemClass(TGLxBaseForceFieldEmitter);

end.
