//
// The graphics rendering engine GLScene http://glscene.org
//
unit Physics.NGDManager;
(*
  The Scene Manager based on Newton Game Dynamics Engine (http://newtondynamics.com)
  Notes:
  This code is still being developed so any part of it may change at anytime.
*)
interface

{ .$I GLScene.inc }

uses
  System.Classes, // TComponent TList TWriter TReader TPersistent
  System.SysUtils, // System utilities
  System.Math, // Samevalue isZero to compare single
  System.Types,

  Physics.NGDImport,
///  Imports.NGD_Joints,

  /// Import.Newton,    // new version

  GLS.VectorTypes,
  GLS.VectorGeometry, // PGLVector TGLVector TGLMatrix PGLMatrix NullHmgVector...
  GLS.VectorLists, // TAffineVectorList for Tree
  GLS.XCollection, // TXCollection file function
  GLS.GeometryBB, // For show debug
  GLS.BaseClasses,
  GLS.PersistentClasses,
  GLS.Scene,
  GLS.Manager,
  GLS.Coordinates,
  GLS.Objects,
  GLS.GeomObjects,
  GLS.VectorFileObjects, // cube cone freeform...
  GLS.Color;

type
  NGDFloat = Physics.NGDImport.dFloat;
  PNGDFloat = ^NGDFloat;

  TGLNGDHeightField = record
    heightArray: array of Word;
    width: Integer;
    depth: Integer;
    gridDiagonals: Boolean;
    widthDepthScale: Single;
    heightScale: Single;
  end;

  TGLNGDBehaviour = class;
  TGLNGDManager = class;
  TGLNGDSurfaceItem = class;
  TGLNGDJoint = class;

  TGLNGDSolverModels = (smExact = 0, smLinear1, smLinear2, smLinear3, smLinear4,
    smLinear5, smLinear6, smLinear7, smLinear8, smLinear9);

  TGLNGDFrictionModels = (fmExact = 0, fmAdaptive);
  TGLNGDPickedActions = (paAttach = 0, paMove, paDetach);

  TGLNGDManagerDebug = (mdShowGeometry, mdShowAABB, mdShowCenterOfMass,
    mdShowContact, mdShowJoint, mdShowForce, mdShowAppliedForce,
    mdShowAppliedVelocity);
  TGLNGDManagerDebugs = set of TGLNGDManagerDebug;

  TGLNGDCollisions = (nc_Primitive = 0, nc_Convex, nc_BBox, nc_BSphere, nc_Tree,
    nc_Mesh, nc_Null, nc_HeightField, nc_NGDFile);

  TGLNGDJoints = (nj_BallAndSocket, nj_Hinge, nj_Slider, nj_Corkscrew,
    nj_Universal, nj_CustomBallAndSocket, nj_CustomHinge, nj_CustomSlider,
    nj_UpVector, nj_KinematicController);

  TGLNGDBehaviourList = class(TList)
  protected
    function GetBehav(index: Integer): TGLNGDBehaviour;
    procedure PutBehav(index: Integer; Item: TGLNGDBehaviour);
  public
    property ItemsBehav[index: Integer]: TGLNGDBehaviour read GetBehav
      write PutBehav; default;
  end;

  // Events for Newton Callback
  TCollisionIteratorEvent = procedure(const userData: Pointer;
    vertexCount: Integer; const cfaceArray: PNGDFloat; faceId: Integer)
    of object;

  TApplyForceAndTorqueEvent = procedure(const cbody: PNewtonBody;
    timestep: NGDFloat; threadIndex: Integer) of object;

  TSetTransformEvent = procedure(const cbody: PNewtonBody;
    const cmatrix: PNGDFloat; threadIndex: Integer) of object;

  TSerializeEvent = procedure(serializeHandle: Pointer; const cbuffer: Pointer;
    size: Cardinal) of object;

  TDeSerializeEvent = procedure(serializeHandle: Pointer; buffer: Pointer;
    size: Cardinal) of object;

  TAABBOverlapEvent = function(const cmaterial: PNewtonMaterial;
    const cbody0: PNewtonBody; const cbody1: PNewtonBody; threadIndex: Integer)
    : Boolean of object;

  TContactProcessEvent = procedure(const ccontact: PNewtonJoint;
    timestep: NGDFloat; threadIndex: Integer) of object;

  TGLNGDDebugOption = class(TPersistent)
  strict private
    FManager: TGLNGDManager;
    FGeomColorDyn: TGLColor; // Green
    FGeomColorStat: TGLColor; // Red
    FAABBColor: TGLColor; // Yellow
    FAABBColorSleep: TGLColor; // Orange
    FCenterOfMassColor: TGLColor; // Purple dot
    FContactColor: TGLColor; // White
    FJointAxisColor: TGLColor; // Blue
    FJointPivotColor: TGLColor; // Aquamarine
    FForceColor: TGLColor; // Black
    FAppliedForceColor: TGLColor; // Silver
    FAppliedVelocityColor: TGLColor; // Lime
    FCustomColor: TGLColor; // Aqua
    FDotAxisSize: Single; // 1
    FNGDManagerDebugs: TGLNGDManagerDebugs; // Default All false
    procedure SetNGDManagerDebugs(const Value: TGLNGDManagerDebugs);
    procedure SetDotAxisSize(const Value: Single);
    function StoredDotAxis: Boolean;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property GeomColorDyn: TGLColor read FGeomColorDyn write FGeomColorDyn;
    property GeomColorStat: TGLColor read FGeomColorStat write FGeomColorStat;
    property AABBColor: TGLColor read FAABBColor write FAABBColor;
    property AABBColorSleep: TGLColor read FAABBColorSleep
      write FAABBColorSleep;
    property CenterOfMassColor: TGLColor read FCenterOfMassColor
      write FCenterOfMassColor;
    property ContactColor: TGLColor read FContactColor write FContactColor;
    property JointAxisColor: TGLColor read FJointAxisColor
      write FJointAxisColor;
    property JointPivotColor: TGLColor read FJointPivotColor
      write FJointPivotColor;
    property ForceColor: TGLColor read FForceColor write FForceColor;
    property AppliedForceColor: TGLColor read FAppliedForceColor
      write FAppliedForceColor;
    property AppliedVelocityColor: TGLColor read FAppliedVelocityColor
      write FAppliedVelocityColor;
    property CustomColor: TGLColor read FCustomColor write FCustomColor;
    property NGDManagerDebugs: TGLNGDManagerDebugs read FNGDManagerDebugs
      write SetNGDManagerDebugs default [];
    property DotAxisSize: Single read FDotAxisSize write SetDotAxisSize
      stored StoredDotAxis;
  end;

  TGLNGDManager = class(TComponent)
  strict private
    FVisible: Boolean; // Show Debug at design time
    FVisibleAtRunTime: Boolean; // Show Debug at run time
    FDllVersion: Integer;
    FSolverModel: TGLNGDSolverModels; // Default=Exact
    FFrictionModel: TGLNGDFrictionModels; // Default=Exact
    FMinimumFrameRate: Integer; // Default=60
    FWorldSizeMin: TGLCoordinates; // Default=-100, -100, -100
    FWorldSizeMax: TGLCoordinates; // Default=100, 100, 100
    FThreadCount: Integer; // Default=1
    FGravity: TGLCoordinates; // Default=(0,-9.81,0)
    FNewtonSurfaceItem: TCollection;
    FNewtonSurfacePair: TOwnedCollection;
    FNewtonJointGroup: TOwnedCollection;
    FNGDDebugOption: TGLNGDDebugOption;
    FGLLines: TGLLines;
  private
    FNewtonWorld: PNewtonWorld;
    FNGDBehaviours: TGLNGDBehaviourList;
    FCurrentColor: TGLColor;
  protected
    procedure Loaded; override;
    procedure SetVisible(const Value: Boolean);
    procedure SetVisibleAtRunTime(const Value: Boolean);
    procedure SetSolverModel(const Value: TGLNGDSolverModels);
    procedure SetFrictionModel(const Value: TGLNGDFrictionModels);
    procedure SetMinimumFrameRate(const Value: Integer);
    procedure SetThreadCount(const Value: Integer);
    procedure SetGLLines(const Value: TGLLines);
    function GetBodyCount: Integer;
    function GetConstraintCount: Integer;
    procedure AddNode(const coords: TGLCustomCoordinates); overload;
    procedure AddNode(const X, Y, Z: Single); overload;
    procedure AddNode(const Value: TGLVector); overload;
    procedure AddNode(const Value: TAffineVector); overload;
    procedure RebuildAllMaterial;
    procedure RebuildAllJoint(Sender: TObject);
    // Events
    procedure NotifyWorldSizeChange(Sender: TObject);
    procedure NotifyChange(Sender: TObject); // Debug view
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Step(deltatime: Single);
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime
      write SetVisibleAtRunTime default False;
    property SolverModel: TGLNGDSolverModels read FSolverModel
      write SetSolverModel default smExact;
    property FrictionModel: TGLNGDFrictionModels read FFrictionModel
      write SetFrictionModel default fmExact;
    property MinimumFrameRate: Integer read FMinimumFrameRate
      write SetMinimumFrameRate default 60;
    property ThreadCount: Integer read FThreadCount write SetThreadCount
      default 1;
    property DllVersion: Integer read FDllVersion;
    property NewtonBodyCount: Integer read GetBodyCount;
    property NewtonConstraintCount: Integer read GetConstraintCount;
    property Gravity: TGLCoordinates read FGravity write FGravity;
    property WorldSizeMin: TGLCoordinates read FWorldSizeMin
      write FWorldSizeMin;
    property WorldSizeMax: TGLCoordinates read FWorldSizeMax
      write FWorldSizeMax;
    property NewtonSurfaceItem: TCollection read FNewtonSurfaceItem
      write FNewtonSurfaceItem;
    property NewtonSurfacePair: TOwnedCollection read FNewtonSurfacePair
      write FNewtonSurfacePair;
    property DebugOption: TGLNGDDebugOption read FNGDDebugOption
      write FNGDDebugOption;
    property Line: TGLLines read FGLLines write SetGLLines;
    property NewtonJoint: TOwnedCollection read FNewtonJointGroup
      write FNewtonJointGroup;
  end;

  // Basis structures for Behaviour style implementations.
  TGLNGDBehaviour = class(TGLBehaviour)
  private
    FManager: TGLNGDManager;
    FManagerName: string;
    FInitialized: Boolean;
    FNewtonBody: PNewtonBody;
    FCollision: PNewtonCollision;
    FNewtonBodyMatrix: TGLMatrix; // Position and Orientation
    FContinuousCollisionMode: Boolean; // Default=False
    FNGDCollisions: TGLNGDCollisions;
    FCollisionIteratorEvent: TCollisionIteratorEvent;
    FOwnerBaseSceneObject: TGLBaseSceneObject;
    // FNullCollisionMass: Single; // Default=0
    FTreeCollisionOptimize: Boolean; // Default=True
    FConvexCollisionTolerance: Single; // Default=0.01 1%
    FFileCollision: string;
    FNGDSurfaceItem: TGLNGDSurfaceItem;
    FHeightFieldOptions: TGLNGDHeightField;
  protected
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure SetManager(Value: TGLNGDManager);
    procedure SetNewtonBodyMatrix(const Value: TGLMatrix);
    procedure SetContinuousCollisionMode(const Value: Boolean);
    function GetNewtonBodyMatrix: TGLMatrix;
    function GetNewtonBodyAABB: TAABB;
    procedure UpdCollision; virtual;
    procedure Render; virtual;
    procedure SetNGDNewtonCollisions(const Value: TGLNGDCollisions);
    procedure SetNGDSurfaceItem(const Value: TGLNGDSurfaceItem);
    procedure SetHeightFieldOptions(const Value: TGLNGDHeightField);
    function GetPrimitiveCollision(): PNewtonCollision;
    function GetConvexCollision(): PNewtonCollision;
    function GetBBoxCollision(): PNewtonCollision;
    function GetBSphereCollision(): PNewtonCollision;
    function GetTreeCollision(): PNewtonCollision;
    function GetMeshCollision(): PNewtonCollision;
    function GetNullCollision(): PNewtonCollision;
    function GetHeightFieldCollision(): PNewtonCollision;
    function GetNGDFileCollision(): PNewtonCollision;
    function StoredTolerance: Boolean;
    // Event
    procedure OnCollisionIteratorEvent(const userData: Pointer;
      vertexCount: Integer; const cfaceArray: PNGDFloat; faceId: Integer);
    // CallBack
    class procedure NewtonCollisionIterator(const userData: Pointer;
      vertexCount: Integer; const faceArray: PNGDFloat; faceId: Integer);
      static; cdecl;
    class procedure NewtonSerialize(serializeHandle: Pointer;
      const buffer: Pointer; size: Cardinal); static; cdecl;
    class procedure NewtonDeserialize(serializeHandle: Pointer; buffer: Pointer;
      size: Cardinal); static; cdecl;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Reinitialize;
    property Initialized: Boolean read FInitialized;
    class function UniqueItem: Boolean; override;
    property NewtonBodyMatrix: TGLMatrix read GetNewtonBodyMatrix
      write SetNewtonBodyMatrix;
    property NewtonBodyAABB: TAABB read GetNewtonBodyAABB;
    procedure Serialize(filename: string);
    procedure DeSerialize(filename: string);
    property HeightFieldOptions: TGLNGDHeightField read FHeightFieldOptions
      write SetHeightFieldOptions;
  published
    property Manager: TGLNGDManager read FManager write SetManager;
    property ContinuousCollisionMode: Boolean read FContinuousCollisionMode
      write SetContinuousCollisionMode default False;
    property NGDNewtonCollisions: TGLNGDCollisions read FNGDCollisions
      write SetNGDNewtonCollisions default nc_Primitive;
    property TreeCollisionOptimize: Boolean read FTreeCollisionOptimize
      write FTreeCollisionOptimize default True;
    property ConvexCollisionTolerance: Single read FConvexCollisionTolerance
      write FConvexCollisionTolerance stored StoredTolerance;
    property FileCollision: string read FFileCollision write FFileCollision;
    property NGDSurfaceItem: TGLNGDSurfaceItem read FNGDSurfaceItem
      write SetNGDSurfaceItem;
  end;

  TGLNGDDynamic = class(TGLNGDBehaviour)
  strict private
    FAABBmin: TGLCoordinates;
    FAABBmax: TGLCoordinates;
    FForce: TGLCoordinates;
    FTorque: TGLCoordinates;
    FCenterOfMass: TGLCoordinates;
    FAutoSleep: Boolean; // Default=True
    FLinearDamping: Single; // default=0.1
    FAngularDamping: TGLCoordinates; // Default=0.1
    FDensity: Single; // Default=1
    FUseGravity: Boolean; // Default=True
    FNullCollisionVolume: Single; // Default=0
    FApplyForceAndTorqueEvent: TApplyForceAndTorqueEvent;
    FSetTransformEvent: TSetTransformEvent;
    FCustomForceAndTorqueEvent: TApplyForceAndTorqueEvent;
    // Read Only
    FVolume: Single;
    FMass: Single;
    FAppliedForce: TGLCoordinates;
    FAppliedTorque: TGLCoordinates;
    FAppliedOmega: TGLCoordinates;
    FAppliedVelocity: TGLCoordinates;
    function StoredDensity: Boolean;
    function StoredLinearDamping: Boolean;
    function StoredNullCollisionVolume: Boolean;
  protected
    procedure SetAutoSleep(const Value: Boolean);
    procedure SetLinearDamping(const Value: Single);
    procedure SetDensity(const Value: Single); virtual;
    procedure Initialize; override;
    procedure Finalize; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure Render; override;
    // Events
    procedure NotifyCenterOfMassChange(Sender: TObject);
    procedure NotifyAngularDampingChange(Sender: TObject);
    procedure OnApplyForceAndTorqueEvent(const cbody: PNewtonBody;
      timestep: NGDFloat; threadIndex: Integer);
    procedure OnSetTransformEvent(const cbody: PNewtonBody;
      const cmatrix: PNGDFloat; threadIndex: Integer);
    // Callback
    class procedure NewtonApplyForceAndTorque(const body: PNewtonBody;
      timestep: NGDFloat; threadIndex: Integer); static; cdecl;
    class procedure NewtonSetTransform(const body: PNewtonBody;
      const matrix: PNGDFloat; threadIndex: Integer); static; cdecl;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure AddImpulse(const veloc, pointposit: TGLVector);
    function GetOmega: TGLVector;
    procedure SetOmega(const Omega: TGLVector);
    function GetVelocity: TGLVector;
    procedure SetVelocity(const Velocity: TGLVector);
    class function FriendlyName: string; override;
    property CustomForceAndTorqueEvent: TApplyForceAndTorqueEvent
      read FCustomForceAndTorqueEvent write FCustomForceAndTorqueEvent;
    property Velocity: TGLVector read GetVelocity write SetVelocity;
    property Omega: TGLVector read GetOmega write SetOmega;
  published
    property Force: TGLCoordinates read FForce write FForce;
    property Torque: TGLCoordinates read FTorque write FTorque;
    property CenterOfMass: TGLCoordinates read FCenterOfMass
      write FCenterOfMass;
    property AutoSleep: Boolean read FAutoSleep write SetAutoSleep default True;
    property LinearDamping: Single read FLinearDamping write SetLinearDamping
      stored StoredLinearDamping;
    property AngularDamping: TGLCoordinates read FAngularDamping
      write FAngularDamping;
    property Density: Single read FDensity write SetDensity
      stored StoredDensity;
    property UseGravity: Boolean read FUseGravity write FUseGravity
      default True;
    property NullCollisionVolume: Single read FNullCollisionVolume
      write FNullCollisionVolume stored StoredNullCollisionVolume;
    // Read Only
    property AppliedOmega: TGLCoordinates read FAppliedOmega;
    property AppliedVelocity: TGLCoordinates read FAppliedVelocity;
    property AppliedForce: TGLCoordinates read FAppliedForce;
    property AppliedTorque: TGLCoordinates read FAppliedTorque;
    property Volume: Single read FVolume;
    property Mass: Single read FMass;
  end;

  TGLNGDStatic = class(TGLNGDBehaviour)
  protected
    procedure Render; override;
  public
    class function FriendlyName: string; override;
  published
  end;

  TGLNGDSurfaceItem = class(TCollectionItem)
  private
    FDisplayName: string;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  published
    property DisplayName;
    property ID;
  end;

  TGLNGDSurfacePair = class(TCollectionItem)
  strict private
    FManager: TGLNGDManager;
    FNGDSurfaceItem1: TGLNGDSurfaceItem;
    FNGDSurfaceItem2: TGLNGDSurfaceItem;
    FAABBOverlapEvent: TAABBOverlapEvent;
    FContactProcessEvent: TContactProcessEvent;
    FSoftness: Single; // 0.1
    FElasticity: Single; // 0.4
    FCollidable: Boolean; // true
    FStaticFriction: Single; // 0.9
    FKineticFriction: Single; // 0.5
    FContinuousCollisionMode: Boolean; // False
    FThickness: Boolean; // False
    procedure SetCollidable(const Value: Boolean);
    procedure SetElasticity(const Value: Single);
    procedure SetKineticFriction(const Value: Single);
    procedure SetSoftness(const Value: Single);
    procedure SetStaticFriction(const Value: Single);
    procedure SetContinuousCollisionMode(const Value: Boolean);
    procedure SetThickness(const Value: Boolean);
    function StoredElasticity: Boolean;
    function StoredKineticFriction: Boolean;
    function StoredSoftness: Boolean;
    function StoredStaticFriction: Boolean;
  private
    // Callback
    class function NewtonAABBOverlap(const material: PNewtonMaterial;
      const body0: PNewtonBody; const body1: PNewtonBody; threadIndex: Integer)
      : Integer; static; cdecl;
    class procedure NewtonContactsProcess(const contact: PNewtonJoint;
      timestep: NGDFloat; threadIndex: Integer); static; cdecl;
    // Event
    function OnNewtonAABBOverlapEvent(const cmaterial: PNewtonMaterial;
      const cbody0: PNewtonBody; const cbody1: PNewtonBody;
      threadIndex: Integer): Boolean;
    procedure OnNewtonContactsProcessEvent(const ccontact: PNewtonJoint;
      timestep: NGDFloat; threadIndex: Integer);
  public
    constructor Create(Collection: TCollection); override;
    procedure SetMaterialItems(const item1, item2: TGLNGDSurfaceItem);
    property NGDSurfaceItem1: TGLNGDSurfaceItem read FNGDSurfaceItem1;
    property NGDSurfaceItem2: TGLNGDSurfaceItem read FNGDSurfaceItem2;
  published
    property Softness: Single read FSoftness write SetSoftness
      stored StoredSoftness;
    property Elasticity: Single read FElasticity write SetElasticity
      stored StoredElasticity;
    property Collidable: Boolean read FCollidable write SetCollidable
      default True;
    property StaticFriction: Single read FStaticFriction write SetStaticFriction
      stored StoredStaticFriction;
    property KineticFriction: Single read FKineticFriction
      write SetKineticFriction stored StoredKineticFriction;
    property ContinuousCollisionMode: Boolean read FContinuousCollisionMode
      write SetContinuousCollisionMode default False;
    property Thickness: Boolean read FThickness write SetThickness
      default False;
    property ContactProcessEvent: TContactProcessEvent read FContactProcessEvent
      write FContactProcessEvent;
    property AABBOverlapEvent: TAABBOverlapEvent read FAABBOverlapEvent
      write FAABBOverlapEvent;
  end;

  TGLNGDJointPivot = class(TPersistent)
  private
    FManager: TGLNGDManager;
    FPivotPoint: TGLCoordinates;
    FOuter: TGLNGDJoint;
  public
    constructor Create(AOwner: TComponent; aOuter: TGLNGDJoint); virtual;
    destructor Destroy; override;
  published
    property PivotPoint: TGLCoordinates read FPivotPoint write FPivotPoint;
  end;

  TGLNGDJointPin = class(TGLNGDJointPivot)
  private
    FPinDirection: TGLCoordinates;
  public
    constructor Create(AOwner: TComponent; aOuter: TGLNGDJoint); override;
    destructor Destroy; override;
  published
    property PinDirection: TGLCoordinates read FPinDirection
      write FPinDirection;
  end;

  TGLNGDJointPin2 = class(TGLNGDJointPin)
  private
    FPinDirection2: TGLCoordinates;
  public
    constructor Create(AOwner: TComponent; aOuter: TGLNGDJoint); override;
    destructor Destroy; override;
  published
    property PinDirection2: TGLCoordinates read FPinDirection2
      write FPinDirection2;
  end;

  TGLNGDJointBallAndSocket = class(TGLNGDJointPivot)
  private
    FConeAngle: Single; // 90
    FMinTwistAngle: Single; // -90
    FMaxTwistAngle: Single; // 90
    procedure SetConeAngle(const Value: Single);
    procedure SetMaxTwistAngle(const Value: Single);
    procedure SetMinTwistAngle(const Value: Single);
    function StoredMaxTwistAngle: Boolean;
    function StoredMinTwistAngle: Boolean;
    function StoredConeAngle: Boolean;
  public
    constructor Create(AOwner: TComponent; aOuter: TGLNGDJoint); override;
  published
    property ConeAngle: Single read FConeAngle write SetConeAngle
      stored StoredConeAngle;
    property MinTwistAngle: Single read FMinTwistAngle write SetMinTwistAngle
      stored StoredMinTwistAngle;
    property MaxTwistAngle: Single read FMaxTwistAngle write SetMaxTwistAngle
      stored StoredMaxTwistAngle;
  end;

  TGLNGDJointHinge = class(TGLNGDJointPin)
  private
    FMinAngle: Single; // -90
    FMaxAngle: Single; // 90
    procedure SetMaxAngle(const Value: Single);
    procedure SetMinAngle(const Value: Single);
    function StoredMaxAngle: Boolean;
    function StoredMinAngle: Boolean;
  public
    constructor Create(AOwner: TComponent; aOuter: TGLNGDJoint); override;
  published
    property MinAngle: Single read FMinAngle write SetMinAngle
      stored StoredMinAngle;
    property MaxAngle: Single read FMaxAngle write SetMaxAngle
      stored StoredMaxAngle;
  end;

  TGLNGDJointSlider = class(TGLNGDJointPin)
  private
    FMinDistance: Single; // -10
    FMaxDistance: Single; // 10
    procedure SetMaxDistance(const Value: Single);
    procedure SetMinDistance(const Value: Single);
    function StoredMaxDistance: Boolean;
    function StoredMinDistance: Boolean;
  public
    constructor Create(AOwner: TComponent; aOuter: TGLNGDJoint); override;
  published
    property MinDistance: Single read FMinDistance write SetMinDistance
      stored StoredMinDistance;
    property MaxDistance: Single read FMaxDistance write SetMaxDistance
      stored StoredMaxDistance;
  end;

  TGLNGDJointKinematicController = class(TPersistent)
  private
    FPickModeLinear: Boolean; // False
    FLinearFriction: Single; // 750
    FAngularFriction: Single; // 250
    function StoredAngularFriction: Boolean;
    function StoredLinearFriction: Boolean;
  public
    constructor Create();
  published
    property PickModeLinear: Boolean read FPickModeLinear write FPickModeLinear
      default False;
    property LinearFriction: Single read FLinearFriction write FLinearFriction
      stored StoredLinearFriction;
    property AngularFriction: Single read FAngularFriction
      write FAngularFriction stored StoredAngularFriction;
  end;

  TGLNGDJoint = class(TCollectionItem)
  private
    // Global
    FManager: TGLNGDManager;
    FParentObject: TGLBaseSceneObject;
    FJointType: TGLNGDJoints;
    FStiffness: Single; // 0.9
    // With Two object
    // Every joint except nj_UpVector and nj_KinematicController
    FChildObject: TGLBaseSceneObject;
    FCollisionState: Boolean; // False
    // With classic joint
    // nj_BallAndSocket, nj_Hinge, nj_Slider, nj_Corkscrew
    // nj_Universal, nj_UpVector
    FNewtonJoint: PNewtonJoint;
    // With CustomJoint
    // nj_CustomBallAndSocket, nj_CustomHinge, nj_CustomSlider
    // nj_KinematicController
    FNewtonUserJoint: Pointer;
    // nj_UpVector
    FUPVectorDirection: TGLCoordinates;
    FBallAndSocketOptions: TGLNGDJointPivot;
    FHingeOptions: TGLNGDJointPin;
    FSliderOptions: TGLNGDJointPin;
    FCorkscrewOptions: TGLNGDJointPin;
    FUniversalOptions: TGLNGDJointPin2;
    FCustomBallAndSocketOptions: TGLNGDJointBallAndSocket;
    FCustomHingeOptions: TGLNGDJointHinge;
    FCustomSliderOptions: TGLNGDJointSlider;
    FKinematicOptions: TGLNGDJointKinematicController;
    procedure SetJointType(const Value: TGLNGDJoints);
    procedure SetChildObject(const Value: TGLBaseSceneObject);
    procedure SetCollisionState(const Value: Boolean);
    procedure SetParentObject(const Value: TGLBaseSceneObject);
    procedure SetStiffness(const Value: Single);
    procedure Render;
    function StoredStiffness: Boolean;
    procedure DestroyNewtonData;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure KinematicControllerPick(pickpoint: TGLVector;
      PickedActions: TGLNGDPickedActions);
  published
    property BallAndSocketOptions: TGLNGDJointPivot read FBallAndSocketOptions
      write FBallAndSocketOptions;
    property HingeOptions: TGLNGDJointPin read FHingeOptions
      write FHingeOptions;
    property SliderOptions: TGLNGDJointPin read FSliderOptions
      write FSliderOptions;
    property CorkscrewOptions: TGLNGDJointPin read FCorkscrewOptions
      write FCorkscrewOptions;
    property UniversalOptions: TGLNGDJointPin2 read FUniversalOptions
      write FUniversalOptions;
    property CustomBallAndSocketOptions: TGLNGDJointBallAndSocket
      read FCustomBallAndSocketOptions write FCustomBallAndSocketOptions;
    property CustomHingeOptions: TGLNGDJointHinge read FCustomHingeOptions
      write FCustomHingeOptions;
    property CustomSliderOptions: TGLNGDJointSlider read FCustomSliderOptions
      write FCustomSliderOptions;
    property KinematicControllerOptions: TGLNGDJointKinematicController
      read FKinematicOptions write FKinematicOptions;
    property JointType: TGLNGDJoints read FJointType write SetJointType;
    property ParentObject: TGLBaseSceneObject read FParentObject
      write SetParentObject;
    property ChildObject: TGLBaseSceneObject read FChildObject
      write SetChildObject;
    property CollisionState: Boolean read FCollisionState
      write SetCollisionState default False;
    property Stiffness: Single read FStiffness write SetStiffness
      stored StoredStiffness;
    property UPVectorDirection: TGLCoordinates read FUPVectorDirection
      write FUPVectorDirection;
  end;

function GetNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
function GetOrCreateNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
function GetNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;
function GetOrCreateNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;
function GetBodyFromGLSceneObject(Obj: TGLBaseSceneObject): PNewtonBody;

// ----------------------------------------------------------------------
implementation
// ----------------------------------------------------------------------

const
  epsilon = 0.0000001; // 1E-07

function GetNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
begin
  Result := TGLNGDStatic(Obj.Behaviours.GetByClass(TGLNGDStatic));
end;

function GetOrCreateNGDStatic(Obj: TGLBaseSceneObject): TGLNGDStatic;
begin
  Result := TGLNGDStatic(Obj.GetOrCreateBehaviour(TGLNGDStatic));
end;

function GetNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;
begin
  Result := TGLNGDDynamic(Obj.Behaviours.GetByClass(TGLNGDDynamic));
end;

function GetOrCreateNGDDynamic(Obj: TGLBaseSceneObject): TGLNGDDynamic;
begin
  Result := TGLNGDDynamic(Obj.GetOrCreateBehaviour(TGLNGDDynamic));
end;

function GetBodyFromGLSceneObject(Obj: TGLBaseSceneObject): PNewtonBody;
var
  Behaviour: TGLNGDBehaviour;
begin
  Behaviour := TGLNGDBehaviour(Obj.Behaviours.GetByClass(TGLNGDBehaviour));
  Assert(Behaviour <> nil,
    'NGD Behaviour (static or dynamic) is missing for this object');
  Result := Behaviour.FNewtonBody;
end;

// -----------------------
// TGLNGDDebugOption
// -----------------------
constructor TGLNGDDebugOption.Create(AOwner: TComponent);
begin
  FManager := AOwner as TGLNGDManager;
  with FManager do
  begin
    FGeomColorDyn := TGLColor.CreateInitialized(self, clrGreen, NotifyChange);
    FGeomColorStat := TGLColor.CreateInitialized(self, clrRed, NotifyChange);
    FAABBColor := TGLColor.CreateInitialized(self, clrYellow, NotifyChange);
    FAABBColorSleep := TGLColor.CreateInitialized(self, clrOrange,
      NotifyChange);
    FCenterOfMassColor := TGLColor.CreateInitialized(self, clrPurple,
      NotifyChange);
    FContactColor := TGLColor.CreateInitialized(self, clrWhite, NotifyChange);
    FJointAxisColor := TGLColor.CreateInitialized(self, clrBlue, NotifyChange);
    FJointPivotColor := TGLColor.CreateInitialized(self, clrAquamarine,
      NotifyChange);

    FForceColor := TGLColor.CreateInitialized(self, clrBlack, NotifyChange);
    FAppliedForceColor := TGLColor.CreateInitialized(self, clrSilver,
      NotifyChange);
    FAppliedVelocityColor := TGLColor.CreateInitialized(self, clrLime,
      NotifyChange);

    FCustomColor := TGLColor.CreateInitialized(self, clrAqua, NotifyChange);
  end;
  FDotAxisSize := 1;
  FNGDManagerDebugs := [];

  FManager := AOwner as TGLNGDManager;
end;

destructor TGLNGDDebugOption.Destroy;
begin
  FGeomColorDyn.Free;
  FGeomColorStat.Free;
  FAABBColor.Free;
  FAABBColorSleep.Free;
  FCenterOfMassColor.Free;
  FContactColor.Free;
  FJointAxisColor.Free;
  FJointPivotColor.Free;
  FForceColor.Free;
  FAppliedForceColor.Free;
  FAppliedVelocityColor.Free;
  FCustomColor.Free;
  inherited;
end;

procedure TGLNGDDebugOption.SetDotAxisSize(const Value: Single);
begin
  FDotAxisSize := Value;
  FManager.NotifyChange(self);
end;

procedure TGLNGDDebugOption.SetNGDManagerDebugs(const Value
  : TGLNGDManagerDebugs);
begin
  FNGDManagerDebugs := Value;
  FManager.NotifyChange(self);
end;

function TGLNGDDebugOption.StoredDotAxis: Boolean;
begin
  Result := not SameValue(FDotAxisSize, 1, epsilon);
end;

// ------------------------
// TGLNGDManager
// ------------------------
procedure TGLNGDManager.AddNode(const Value: TGLVector);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(Value);

    with (FGLLines.Nodes.Last as TGLLinesNode) do
      Color := FCurrentColor;
  end;
end;

procedure TGLNGDManager.AddNode(const coords: TGLCustomCoordinates);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(coords);
    (FGLLines.Nodes.Last as TGLLinesNode).Color := FCurrentColor;
  end;
end;

procedure TGLNGDManager.AddNode(const X, Y, Z: Single);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(X, Y, Z);
    (FGLLines.Nodes.Last as TGLLinesNode).Color := FCurrentColor;
  end;
end;

procedure TGLNGDManager.AddNode(const Value: TAffineVector);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(Value);
    (FGLLines.Nodes.Last as TGLLinesNode).Color := FCurrentColor;
  end;
end;

constructor TGLNGDManager.Create(AOwner: TComponent);
var
  minworld, maxworld: TGLVector;
begin
  inherited;
  FNGDBehaviours := TGLNGDBehaviourList.Create;
  FVisible := True;
  FVisibleAtRunTime := False;
  FSolverModel := smExact;
  FFrictionModel := fmExact;
  FMinimumFrameRate := 60;
  FWorldSizeMin := TGLCoordinates.CreateInitialized(self,
    VectorMake(-100, -100, -100, 0), csPoint);
  FWorldSizeMax := TGLCoordinates.CreateInitialized(self,
    VectorMake(100, 100, 100, 0), csPoint);

  // Using Events because we need to call API Function when
  // theses TGLCoordinates change.
  FWorldSizeMin.OnNotifyChange := NotifyWorldSizeChange;
  FWorldSizeMax.OnNotifyChange := NotifyWorldSizeChange;

  FThreadCount := 1;
  FGravity := TGLCoordinates3.CreateInitialized(self,
    VectorMake(0, -9.81, 0, 0), csVector);

  FNewtonWorld := NewtonCreate(nil, nil);
  FDllVersion := NewtonWorldGetVersion(FNewtonWorld);

  // This is to prevent body out the world at startTime
  minworld := VectorMake(-1E50, -1E50, -1E50);
  maxworld := VectorMake(1E50, 1E50, 1E50);
  NewtonSetWorldSize(FNewtonWorld, @minworld, @maxworld);

  NewtonWorldSetUserData(FNewtonWorld, self);

  FNewtonSurfaceItem := TCollection.Create(TGLNGDSurfaceItem);
  FNewtonSurfacePair := TOwnedCollection.Create(self, TGLNGDSurfacePair);
  FNewtonJointGroup := TOwnedCollection.Create(self, TGLNGDJoint);
  FNGDDebugOption := TGLNGDDebugOption.Create(self);
  RegisterManager(self);
end;

destructor TGLNGDManager.Destroy;
begin
  // for joint before body.
  FreeAndNil(FNewtonJointGroup);

  // Unregister everything
  while FNGDBehaviours.Count > 0 do
    FNGDBehaviours[0].Manager := nil;

  // Clean up everything
  FreeAndNil(FNGDBehaviours);
  FreeAndNil(FWorldSizeMin);
  FreeAndNil(FWorldSizeMax);
  FreeAndNil(FGravity);
  FreeAndNil(FNewtonSurfaceItem);
  FreeAndNil(FNewtonSurfacePair);
  FreeAndNil(FNGDDebugOption);

  NewtonDestroyAllBodies(FNewtonWorld);
  NewtonMaterialDestroyAllGroupID(FNewtonWorld);
  NewtonDestroy(FNewtonWorld);
  FNewtonWorld := nil;
  DeregisterManager(self);
  inherited;
end;

procedure TGLNGDManager.Loaded;
begin
  inherited;
  NotifyWorldSizeChange(self);
  RebuildAllJoint(self);
end;

function TGLNGDManager.GetBodyCount: Integer;
begin
  if (csDesigning in ComponentState) then
    Result := FNGDBehaviours.Count
  else
    Result := NewtonWorldGetBodyCount(FNewtonWorld);
end;

function TGLNGDManager.GetConstraintCount: Integer;
begin
  if (csDesigning in ComponentState) then
    Result := FNewtonJointGroup.Count
  else
    // Constraint is the number of joint
    Result := NewtonWorldGetConstraintCount(FNewtonWorld);
end;

procedure TGLNGDManager.NotifyChange(Sender: TObject);
var
  I: Integer;
begin
  // This event is raise
  // when debugOptions properties are edited,
  // when a behavior is initialized/finalize,
  // when joints are rebuilded, (runtime only)
  // when visible and visibleAtRuntime are edited (designTime only),
  // in manager.step, and in SetGLLines.

  // Here the manager call render method for bodies and joints in its lists

  if not Assigned(FGLLines) then
    exit;
  FGLLines.Nodes.Clear;

  if not Visible then
    exit;
  if not(csDesigning in ComponentState) then
    if not VisibleAtRunTime then
      exit;

  for I := 0 to FNGDBehaviours.Count - 1 do
    FNGDBehaviours[I].Render;

  if mdShowJoint in FNGDDebugOption.NGDManagerDebugs then
    for I := 0 to NewtonJoint.Count - 1 do //
      (NewtonJoint.Items[I] as TGLNGDJoint).Render;

end;

procedure TGLNGDManager.SetFrictionModel(const Value: TGLNGDFrictionModels);
begin
  FFrictionModel := Value;
  if not(csDesigning in ComponentState) then
    NewtonSetFrictionModel(FNewtonWorld, Ord(FFrictionModel));
end;

procedure TGLNGDManager.SetGLLines(const Value: TGLLines);
begin
  if Assigned(FGLLines) then
    FGLLines.Nodes.Clear;

  FGLLines := Value;

  if Assigned(FGLLines) then
  begin
    FGLLines.SplineMode := lsmSegments;
    FGLLines.NodesAspect := lnaInvisible;
    FGLLines.Options := [loUseNodeColorForLines];
    FGLLines.Pickable := False;
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.SetMinimumFrameRate(const Value: Integer);
begin
  if (Value >= 60) and (Value <= 1000) then
    FMinimumFrameRate := Value;
  if not(csDesigning in ComponentState) then
    NewtonSetMinimumFrameRate(FNewtonWorld, FMinimumFrameRate);
end;

procedure TGLNGDManager.SetSolverModel(const Value: TGLNGDSolverModels);
begin
  FSolverModel := Value;
  if not(csDesigning in ComponentState) then
    NewtonSetSolverModel(FNewtonWorld, Ord(FSolverModel));
end;

procedure TGLNGDManager.SetThreadCount(const Value: Integer);
begin
  if Value > 0 then
    FThreadCount := Value;
  NewtonSetThreadsCount(FNewtonWorld, FThreadCount);
  FThreadCount := NewtonGetThreadsCount(FNewtonWorld);
end;

procedure TGLNGDManager.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  if (csDesigning in ComponentState) then
    NotifyChange(self);
end;

procedure TGLNGDManager.SetVisibleAtRunTime(const Value: Boolean);
begin
  FVisibleAtRunTime := Value;
  if (csDesigning in ComponentState) then
    NotifyChange(self);
end;

procedure TGLNGDManager.NotifyWorldSizeChange(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    NewtonSetWorldSize(FNewtonWorld, @FWorldSizeMin.AsVector,
      @FWorldSizeMax.AsVector);
end;

procedure TGLNGDManager.RebuildAllJoint(Sender: TObject);

  procedure BuildBallAndSocket(Joint: TGLNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateBall(FNewtonWorld,
          @(FBallAndSocketOptions.FPivotPoint.AsVector),
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildHinge(Joint: TGLNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateHinge(FNewtonWorld,
          @(FHingeOptions.FPivotPoint.AsVector),
          @(FHingeOptions.FPinDirection.AsVector),
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildSlider(Joint: TGLNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateSlider(FNewtonWorld,
          @(FSliderOptions.FPivotPoint.AsVector),
          @(FSliderOptions.FPinDirection.AsVector),
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildCorkscrew(Joint: TGLNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateCorkscrew(FNewtonWorld,
          @(FCorkscrewOptions.FPivotPoint.AsVector),
          @(FCorkscrewOptions.FPinDirection.AsVector),
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildUniversal(Joint: TGLNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateUniversal(FNewtonWorld,
          @(FUniversalOptions.FPivotPoint.AsVector),
          @(FUniversalOptions.FPinDirection.AsVector),
          @(FUniversalOptions.FPinDirection2.AsVector),
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildCustomBallAndSocket(Joint: TGLNGDJoint);
  var
    pinAndPivot: TGLMatrix;
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        pinAndPivot := IdentityHmgMatrix;
        pinAndPivot.W := FCustomBallAndSocketOptions.FPivotPoint.AsVector;
(* from dJointLibrary.dll
        FNewtonUserJoint := CreateCustomBallAndSocket(@pinAndPivot,
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        BallAndSocketSetConeAngle(FNewtonUserJoint,
          DegToRadian(FCustomBallAndSocketOptions.FConeAngle));
        BallAndSocketSetTwistAngle(FNewtonUserJoint,
          DegToRadian(FCustomBallAndSocketOptions.FMinTwistAngle),
          DegToRadian(FCustomBallAndSocketOptions.FMaxTwistAngle));
        CustomSetBodiesCollisionState(FNewtonUserJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(CustomGetNewtonJoint(FNewtonUserJoint),
          FStiffness);
*)
      end;
  end;

  procedure BuildCustomHinge(Joint: TGLNGDJoint);
  var
    pinAndPivot: TGLMatrix;
    bso: TGLBaseSceneObject;
  begin
    (* Newton wait from FPinAndPivotMatrix a structure like that:
      First row: the pin direction
      Second and third rows are set to create an orthogonal matrix
      Fourth: The pivot position
      The GLBaseSceneObjects direction is the third row,
      because the first row is the right vector (second row is up vector). *)
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        bso := TGLBaseSceneObject.Create(FManager);
        bso.AbsolutePosition := FCustomHingeOptions.FPivotPoint.AsVector;
        bso.AbsoluteDirection := FCustomHingeOptions.FPinDirection.AsVector;
        pinAndPivot := bso.AbsoluteMatrix;
        pinAndPivot.X := bso.AbsoluteMatrix.Z;
        pinAndPivot.Z := bso.AbsoluteMatrix.X;
        bso.Free;

(* from dJointLibrary.dll
        FNewtonUserJoint := CreateCustomHinge(@pinAndPivot,
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        HingeEnableLimits(FNewtonUserJoint, 1);
        HingeSetLimits(FNewtonUserJoint,
          DegToRadian(FCustomHingeOptions.FMinAngle),
          DegToRadian(FCustomHingeOptions.FMaxAngle));
        CustomSetBodiesCollisionState(FNewtonUserJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(CustomGetNewtonJoint(FNewtonUserJoint),
          FStiffness);
        CustomSetUserData(FNewtonUserJoint, CustomHingeOptions);
*)
      end;
  end;

  procedure BuildCustomSlider(Joint: TGLNGDJoint);
  var
    pinAndPivot: TGLMatrix;
    bso: TGLBaseSceneObject;

  begin
    (*
      Newton wait from FPinAndPivotMatrix a structure like that:
      First row: the pin direction
      Second and third rows are set to create an orthogonal matrix
      Fourth: The pivot position

      In GLS.Scene, the GLBaseSceneObjects direction is the third row,
      because the first row is the right vector (second row is up vector).
    *)
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin

        bso := TGLBaseSceneObject.Create(FManager);
        bso.AbsolutePosition := FCustomSliderOptions.FPivotPoint.AsVector;
        bso.AbsoluteDirection := FCustomSliderOptions.FPinDirection.AsVector;
        pinAndPivot := bso.AbsoluteMatrix;
        pinAndPivot.X := bso.AbsoluteMatrix.Z;
        pinAndPivot.Z := bso.AbsoluteMatrix.X;
        bso.Free;

(* from dJointLibrary.dll
        FNewtonUserJoint := CreateCustomSlider(@pinAndPivot,
          GetBodyFromGLSceneObject(FChildObject),
          GetBodyFromGLSceneObject(FParentObject));
        SliderEnableLimits(FNewtonUserJoint, 1);
        SliderSetLimits(FNewtonUserJoint, FCustomSliderOptions.FMinDistance,
          FCustomSliderOptions.FMaxDistance);
        NewtonJointSetStiffness(CustomGetNewtonJoint(FNewtonUserJoint), 0);

        CustomSetBodiesCollisionState(FNewtonUserJoint, Ord(FCollisionState));
        CustomSetUserData(FNewtonUserJoint, CustomSliderOptions);
*)
      end;
  end;

  procedure BuildUpVector(Joint: TGLNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateUpVector(FNewtonWorld,
          @FUPVectorDirection.AsVector,
          GetBodyFromGLSceneObject(FParentObject));
      end;
  end;

  procedure BuildKinematicController(Joint: TGLNGDJoint);
  begin
    // do nothing
  end;

  procedure BuildOneJoint(Joint: TGLNGDJoint);
  begin
    case Joint.FJointType of
      nj_BallAndSocket:
        begin
          Joint.DestroyNewtonData;
          BuildBallAndSocket(Joint);
        end;

      nj_Hinge:
        begin
          Joint.DestroyNewtonData;
          BuildHinge(Joint);
        end;

      nj_Slider:
        begin
          Joint.DestroyNewtonData;
          BuildSlider(Joint);
        end;

      nj_Corkscrew:
        begin
          Joint.DestroyNewtonData;
          BuildCorkscrew(Joint);
        end;

      nj_Universal:
        begin
          Joint.DestroyNewtonData;
          BuildUniversal(Joint);
        end;

      nj_CustomBallAndSocket:
        begin
          Joint.DestroyNewtonData;
          BuildCustomBallAndSocket(Joint);
        end;

      nj_CustomHinge:
        begin
          Joint.DestroyNewtonData;
          BuildCustomHinge(Joint);
        end;

      nj_CustomSlider:
        begin
          Joint.DestroyNewtonData;
          BuildCustomSlider(Joint);
        end;

      nj_UpVector:
        begin
          Joint.DestroyNewtonData;
          BuildUpVector(Joint);
        end;

      nj_KinematicController:
        begin
          // DestroyJoint(Joint);
          // BuildKinematicController(Joint);
        end;
    end;
  end;

var
  I: Integer;
begin
  if not(csDesigning in ComponentState) and not(csLoading in ComponentState)
  then
  begin
    if Sender is TGLNGDManager then
      for I := 0 to NewtonJoint.Count - 1 do
        BuildOneJoint(NewtonJoint.Items[I] as TGLNGDJoint);
    if (Sender is TGLNGDJoint) then
      BuildOneJoint((Sender as TGLNGDJoint));
    if Sender is TGLCoordinates then
      BuildOneJoint(((Sender as TGLCoordinates).Owner as TGLNGDJoint));
    NotifyChange(self);
  end;
end;

procedure TGLNGDManager.RebuildAllMaterial;

  procedure BuildMaterialPair;
  var
    I, ID0, ID1: Integer;
  begin
    for I := 0 to FNewtonSurfacePair.Count - 1 do
      with (FNewtonSurfacePair.Items[I] as TGLNGDSurfacePair) do
      begin
        if Assigned(NGDSurfaceItem1) and Assigned(NGDSurfaceItem2) then
        begin
          ID0 := NGDSurfaceItem1.ID;
          ID1 := NGDSurfaceItem2.ID;
          NewtonMaterialSetContinuousCollisionMode(FNewtonWorld, ID0, ID1,
            Ord(ContinuousCollisionMode));
          if Thickness then
            NewtonMaterialSetSurfaceThickness(FNewtonWorld, ID0, ID1, 1);
          NewtonMaterialSetDefaultSoftness(FNewtonWorld, ID0, ID1, Softness);
          NewtonMaterialSetDefaultElasticity(FNewtonWorld, ID0, ID1,
            Elasticity);
          NewtonMaterialSetDefaultCollidable(FNewtonWorld, ID0, ID1,
            Ord(Collidable));
          NewtonMaterialSetDefaultFriction(FNewtonWorld, ID0, ID1,
            StaticFriction, KineticFriction);

          NewtonMaterialSetCollisionCallback(FNewtonWorld, ID0, ID1,
            FNewtonSurfacePair.Items[I], @TGLNGDSurfacePair.NewtonAABBOverlap,
            @TGLNGDSurfacePair.NewtonContactsProcess);
        end;
      end;
  end;

var
  I: Integer;
  maxID: Integer;
begin
  maxID := 0;
  if not(csDesigning in ComponentState) then
  begin
    // for newton materials
    NewtonMaterialDestroyAllGroupID(FNewtonWorld);
    // Creates materialID
    for I := 0 to FNewtonSurfaceItem.Count - 1 do
      maxID := MaxInteger((FNewtonSurfaceItem.Items[I] as TGLNGDSurfaceItem)
        .ID, maxID);
    for I := 0 to maxID - 1 do
      NewtonMaterialCreateGroupID(FNewtonWorld);
    // assign matID to bodies
    for I := 0 to FNGDBehaviours.Count - 1 do
      with FNGDBehaviours[I] do
        if Assigned(FNGDSurfaceItem) then
          NewtonBodySetMaterialGroupID(FNewtonBody, FNGDSurfaceItem.ID)
        else
          NewtonBodySetMaterialGroupID(FNewtonBody, 0);
    // Set values to newton material pair :callback userdata friction...
    BuildMaterialPair;
  end;
end;

procedure TGLNGDManager.Step(deltatime: Single);
begin
  if not(csDesigning in ComponentState) then
    NewtonUpdate(FNewtonWorld, deltatime);

  NotifyChange(self);
end;

// ---------------------------
// TGLNGDBehaviour
// ---------------------------

constructor TGLNGDBehaviour.Create(AOwner: TXCollection);
begin
  inherited;
  FInitialized := False;
  FOwnerBaseSceneObject := OwnerBaseSceneObject;

  FContinuousCollisionMode := False;
  FNewtonBody := nil;
  FCollision := nil;

  FNGDCollisions := nc_Primitive;

  FCollisionIteratorEvent := OnCollisionIteratorEvent;

  FTreeCollisionOptimize := True;
  FConvexCollisionTolerance := 0.01;
  FFileCollision := '';
  name := 'NGD Static';
end;

destructor TGLNGDBehaviour.Destroy;
begin
  if Assigned(FManager) then
    Manager := nil; // This will call finalize
  inherited;
end;

procedure TGLNGDBehaviour.Finalize;
var
  I: Integer;
begin
  FInitialized := False;

  if Assigned(FManager) then
  begin

    if Assigned(FManager.NewtonJoint) then
      for I := FManager.NewtonJoint.Count - 1 downto 0 do
      begin
        if ((FManager.NewtonJoint.Items[I] as TGLNGDJoint)
          .ParentObject = FOwnerBaseSceneObject) or
          ((FManager.NewtonJoint.Items[I] as TGLNGDJoint)
          .ChildObject = FOwnerBaseSceneObject) then
        begin
          FManager.NewtonJoint.Items[I].Free;
        end;
      end;

    NewtonDestroyBody(FManager.FNewtonWorld, FNewtonBody);
    FNewtonBody := nil;
    FCollision := nil;
  end;
end;

function TGLNGDBehaviour.GetBBoxCollision: PNewtonCollision;
var
  vc: array [0 .. 7] of TGLVector;
  I: Integer;
begin
  for I := 0 to 8 - 1 do
    vc[I] := AABBToBB(FOwnerBaseSceneObject.AxisAlignedBoundingBoxEx).BBox[I];
  Result := NewtonCreateConvexHull(FManager.FNewtonWorld, 8, @vc[0],
    SizeOf(TGLVector), 0.01, 0, nil);
end;

function TGLNGDBehaviour.GetBSphereCollision: PNewtonCollision;
var
  boundingSphere: TBSphere;
  collisionOffsetMatrix: TGLMatrix;
begin
  AABBToBSphere(FOwnerBaseSceneObject.AxisAlignedBoundingBoxEx, boundingSphere);

  collisionOffsetMatrix := IdentityHmgMatrix;
  collisionOffsetMatrix.W := VectorMake(boundingSphere.Center, 1);
  Result := NewtonCreateSphere(FManager.FNewtonWorld, boundingSphere.Radius,
    boundingSphere.Radius, boundingSphere.Radius, 0, @collisionOffsetMatrix);
end;

function TGLNGDBehaviour.GetConvexCollision: PNewtonCollision;
var
  I, J: Integer;
  vertexArray: array of TVertex;
begin
  if FOwnerBaseSceneObject is TGLBaseMesh then
  begin
    with (FOwnerBaseSceneObject as TGLBaseMesh) do
    begin
      for I := 0 to MeshObjects.Count - 1 do
        for J := 0 to MeshObjects[I].Vertices.Count - 1 do
        begin
          SetLength(vertexArray, Length(vertexArray) + 1);
          vertexArray[Length(vertexArray) - 1] := MeshObjects[I].Vertices[J];
        end;
      if Length(vertexArray) > 0 then
        Result := NewtonCreateConvexHull(FManager.FNewtonWorld,
          Length(vertexArray), @vertexArray[0], SizeOf(TVertex),
          FConvexCollisionTolerance, 0, nil)
      else
        Result := GetNullCollision;
    end;
  end
  else
    Result := GetNullCollision;
end;

function TGLNGDBehaviour.GetHeightFieldCollision: PNewtonCollision;
var
  I: Integer;
  attributeMap: array of ShortInt;
begin
  SetLength(attributeMap, Length(FHeightFieldOptions.heightArray));
  for I := 0 to Length(FHeightFieldOptions.heightArray) - 1 do
    attributeMap[I] := 0;
  Result := NewtonCreateHeightFieldCollision(FManager.FNewtonWorld,
    FHeightFieldOptions.width, FHeightFieldOptions.depth,
    Ord(FHeightFieldOptions.gridDiagonals),
    PUnsigned_short(FHeightFieldOptions.heightArray), P2Char(attributeMap),
    FHeightFieldOptions.widthDepthScale, FHeightFieldOptions.heightScale, 0);
end;

function TGLNGDBehaviour.GetMeshCollision: PNewtonCollision;
var
  collisionArray: array of PNewtonCollision;
  I, J: Integer;
  vertexArray: array of TVertex;
begin
  if FOwnerBaseSceneObject is TGLBaseMesh then
  begin
    with (FOwnerBaseSceneObject as TGLBaseMesh) do
    begin
      // Iterate trough mesh of GLobject
      for I := 0 to MeshObjects.Count - 1 do
      begin
        // Iterate trough vertices of mesh
        for J := 0 to MeshObjects[I].Vertices.Count - 1 do
        begin
          SetLength(vertexArray, Length(vertexArray) + 1);
          vertexArray[Length(vertexArray) - 1] := MeshObjects[I].Vertices[J];
        end;
        if Length(vertexArray) > 3 then
        begin
          SetLength(collisionArray, Length(collisionArray) + 1);
          collisionArray[Length(collisionArray) - 1] :=
            NewtonCreateConvexHull(FManager.FNewtonWorld, Length(vertexArray),
            @vertexArray[0], SizeOf(TVertex),
            FConvexCollisionTolerance, 0, nil);
          // Remove last collision if the newton function was not successful
          if collisionArray[Length(collisionArray) - 1] = nil then
            SetLength(collisionArray, Length(collisionArray) - 1);
        end;
        SetLength(vertexArray, 0);
      end;
      if Length(collisionArray) > 0 then
        Result := NewtonCreateCompoundCollision(FManager.FNewtonWorld,
          Length(collisionArray),
          TCollisionPrimitiveArray(@collisionArray[0]), 0)
      else
        Result := GetNullCollision;
    end;
  end
  else
    Result := GetNullCollision;
end;

function TGLNGDBehaviour.GetNewtonBodyMatrix: TGLMatrix;
begin
  if Assigned(FManager) then
    NewtonBodyGetmatrix(FNewtonBody, @FNewtonBodyMatrix);
  Result := FNewtonBodyMatrix;
end;

function TGLNGDBehaviour.GetNewtonBodyAABB: TAABB;
begin
  if Assigned(FManager) then
    NewtonBodyGetAABB(FNewtonBody, @(Result.min), @(Result.max));
end;

function TGLNGDBehaviour.GetNGDFileCollision: PNewtonCollision;
var
  MyFile: TFileStream;
begin
  if FileExists(FFileCollision) then
  begin
    MyFile := TFileStream.Create(FFileCollision, fmOpenRead);
    Result := NewtonCreateCollisionFromSerialization(FManager.FNewtonWorld,
      @TGLNGDBehaviour.NewtonDeserialize, Pointer(MyFile));
    MyFile.Free;
  end
  else
    Result := NewtonCreateNull(FManager.FNewtonWorld);
end;

function TGLNGDBehaviour.GetNullCollision: PNewtonCollision;
begin
  Result := NewtonCreateNull(FManager.FNewtonWorld);
end;

function TGLNGDBehaviour.GetPrimitiveCollision: PNewtonCollision;
var
  collisionOffsetMatrix: TGLMatrix; // For cone capsule and cylinder
begin
  collisionOffsetMatrix := IdentityHmgMatrix;

  if (FOwnerBaseSceneObject is TGLCube) then
  begin
    with (FOwnerBaseSceneObject as TGLCube) do
      Result := NewtonCreateBox(FManager.FNewtonWorld, CubeWidth, CubeHeight,
        CubeDepth, 0, @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TGLSphere) then
  begin
    with (FOwnerBaseSceneObject as TGLSphere) do
      Result := NewtonCreateSphere(FManager.FNewtonWorld, Radius, Radius,
        Radius, 0, @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TGLCone) then
  begin
    collisionOffsetMatrix := MatrixMultiply(collisionOffsetMatrix,
      CreateRotationMatrixZ(Pi / 2.0));
    with (FOwnerBaseSceneObject as TGLCone) do
      Result := NewtonCreateCone(FManager.FNewtonWorld, BottomRadius, Height, 0,
        @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TGLCapsule) then
  begin
    collisionOffsetMatrix := MatrixMultiply(collisionOffsetMatrix,
      CreateRotationMatrixY(Pi / 2.0));
    with (FOwnerBaseSceneObject as TGLCapsule) do
      // Use Cylinder shape for buoyancy
      Result := NewtonCreateCapsule(FManager.FNewtonWorld, Radius,
        Height + 2 * Radius, 0, @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TGLCylinder) then
  begin
    collisionOffsetMatrix := MatrixMultiply(collisionOffsetMatrix,
      CreateRotationMatrixZ(Pi / 2.0));
    with (FOwnerBaseSceneObject as TGLCylinder) do
      Result := NewtonCreateCylinder(FManager.FNewtonWorld, BottomRadius,
        Height, 0, @collisionOffsetMatrix);
  end
  else
    Result := GetNullCollision;
end;

function TGLNGDBehaviour.GetTreeCollision: PNewtonCollision;
var
  meshIndex, triangleIndex: Integer;
  triangleList: TAffineVectorList;
  v: array [0 .. 2] of TAffineVector;
begin
  if FOwnerBaseSceneObject is TGLBaseMesh then
  begin
    with (FOwnerBaseSceneObject as TGLBaseMesh) do
    begin
      Result := NewtonCreateTreeCollision(FManager.FNewtonWorld, 0);
      NewtonTreeCollisionBeginBuild(Result);

      for meshIndex := 0 to MeshObjects.Count - 1 do
      begin
        triangleList := MeshObjects[meshIndex].ExtractTriangles;
        for triangleIndex := 0 to triangleList.Count - 1 do
        begin
          if triangleIndex mod 3 = 0 then
          begin
            v[0] := triangleList.Items[triangleIndex];
            // ScaleVector(v[0], FOwnerBaseSceneObject.Scale.X);
            v[1] := triangleList.Items[triangleIndex + 1];
            // ScaleVector(v[1], FOwnerBaseSceneObject.Scale.Y);
            v[2] := triangleList.Items[triangleIndex + 2];
            // ScaleVector(v[2], FOwnerBaseSceneObject.Scale.Z);
            NewtonTreeCollisionAddFace(Result, 3, @(v),
              SizeOf(TAffineVector), 1);
          end;
        end;
        triangleList.Free;
      end;
      NewtonTreeCollisionEndBuild(Result, Ord(FTreeCollisionOptimize));
    end;
  end
  else
    Result := GetNullCollision;
end;

procedure TGLNGDBehaviour.Initialize;
begin
  FInitialized := True;
  if Assigned(FManager) then
  begin
    // Creates NewtonBody with null collision
    FCollision := NewtonCreateNull(FManager.FNewtonWorld);
    FNewtonBodyMatrix := FOwnerBaseSceneObject.AbsoluteMatrix;
    FNewtonBody := NewtonCreateBody(FManager.FNewtonWorld, FCollision,
      @FNewtonBodyMatrix);
    // Release NewtonCollision
    NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);
    // Set Link between glscene and newton
    NewtonBodySetUserdata(FNewtonBody, self);
    // Set position and orientation
    SetNewtonBodyMatrix(FOwnerBaseSceneObject.AbsoluteMatrix);
    // Set Collision
    UpdCollision;
  end;
end;

procedure TGLNGDBehaviour.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TGLNGDManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLNGDManager(mng);
    FManagerName := '';
  end;

  if Assigned(FManager) then
  begin
    SetContinuousCollisionMode(FContinuousCollisionMode);
  end;
end;

class procedure TGLNGDBehaviour.NewtonCollisionIterator(const userData: Pointer;
  vertexCount: Integer; const faceArray: PNGDFloat; faceId: Integer)cdecl;
begin
  TGLNGDBehaviour(userData).FCollisionIteratorEvent(userData, vertexCount,
    faceArray, faceId);
end;

// Serializes are called by NGDBehaviour to save and load collision in file
// It's better to save/load big collisions [over 50000 polygones] to reduce
// loading time
class procedure TGLNGDBehaviour.NewtonDeserialize(serializeHandle,
  buffer: Pointer; size: Cardinal)cdecl;
begin
  TFileStream(serializeHandle).read(buffer^, size);
end;

class procedure TGLNGDBehaviour.NewtonSerialize(serializeHandle: Pointer;
  const buffer: Pointer; size: Cardinal)cdecl;
begin
  TFileStream(serializeHandle).write(buffer^, size);
end;

procedure TGLNGDBehaviour.OnCollisionIteratorEvent(const userData: Pointer;
  vertexCount: Integer; const cfaceArray: PNGDFloat; faceId: Integer);
var
  I: Integer;
  v0, v1: array [0 .. 2] of Single;
  vA: array of Single;
begin
  // This algorithme draw Collision Shape for Debuggin.
  // Taken to Sascha Willems in SDLNewton-Demo at
  // http://www.saschawillems.de/?page_id=82
  // Leave if there is no or to much vertex
  if (vertexCount = 0) then
    exit;
  SetLength(vA, vertexCount * 3);
  Move(cfaceArray^, vA[0], vertexCount * 3 * SizeOf(Single));
  v0[0] := vA[(vertexCount - 1) * 3];
  v0[1] := vA[(vertexCount - 1) * 3 + 1];
  v0[2] := vA[(vertexCount - 1) * 3 + 2];
  for I := 0 to vertexCount - 1 do
  begin
    v1[0] := vA[I * 3];
    v1[1] := vA[I * 3 + 1];
    v1[2] := vA[I * 3 + 2];
    FManager.AddNode(v0[0], v0[1], v0[2]);
    FManager.AddNode(v1[0], v1[1], v1[2]);
    v0 := v1;
  end;
end;

procedure TGLNGDBehaviour.Reinitialize;
begin
  if Initialized then
  begin
    // Set Appropriate NewtonCollision
    UpdCollision();
    // Set position and orientation
    SetNewtonBodyMatrix(FOwnerBaseSceneObject.AbsoluteMatrix);
  end;
  Loaded;
end;

procedure TGLNGDBehaviour.Render;
var
  M: TGLMatrix;
begin
  // Rebuild collision in design time
  if (csDesigning in FOwnerBaseSceneObject.ComponentState) then
    Reinitialize;

  if self is TGLNGDDynamic then
    FManager.FCurrentColor := FManager.DebugOption.GeomColorDyn
  else
    FManager.FCurrentColor := FManager.DebugOption.GeomColorStat;
  M := FOwnerBaseSceneObject.AbsoluteMatrix;
  if mdShowGeometry in FManager.DebugOption.NGDManagerDebugs then
    NewtonCollisionForEachPolygonDo(FCollision, @M,
      @TGLNGDBehaviour.NewtonCollisionIterator, self);
end;

// In this procedure, we assign collision to body
// [Because when initialised, the collision for body is type NULL]
procedure TGLNGDBehaviour.UpdCollision;
var
  collisionInfoRecord: TNewtonCollisionInfoRecord;
begin
  case FNGDCollisions of
    nc_Primitive:
      FCollision := GetPrimitiveCollision;
    nc_Convex:
      FCollision := GetConvexCollision;
    nc_BBox:
      FCollision := GetBBoxCollision;
    nc_BSphere:
      FCollision := GetBSphereCollision;
    nc_Tree:
      FCollision := GetTreeCollision;
    nc_Mesh:
      FCollision := GetMeshCollision;
    nc_Null:
      FCollision := GetNullCollision;
    nc_HeightField:
      FCollision := GetHeightFieldCollision;
    nc_NGDFile:
      FCollision := GetNGDFileCollision;
  end;

  if Assigned(FCollision) then
  begin
    NewtonBodySetCollision(FNewtonBody, FCollision);

    // The API Ask for releasing Collision to avoid memory leak
    NewtonCollisionGetInfo(FCollision, @collisionInfoRecord);
    if collisionInfoRecord.m_collisionType > 2 then
      NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);
  end;

end;

procedure TGLNGDBehaviour.SetContinuousCollisionMode(const Value: Boolean);
begin
  // for continue collision to be active the continue collision mode must on
  // the material pair of the colliding bodies as well as on at
  // least one of the two colliding bodies.
  // see NewtonBodySetContinuousCollisionMode
  // see NewtonMaterialSetContinuousCollisionMode
  FContinuousCollisionMode := Value;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
      NewtonBodySetContinuousCollisionMode(FNewtonBody, Ord(Value));
end;

procedure TGLNGDBehaviour.SetHeightFieldOptions(const Value: TGLNGDHeightField);
begin
  FHeightFieldOptions := Value;
  Reinitialize;
end;

procedure TGLNGDBehaviour.SetManager(Value: TGLNGDManager);
begin
  if FManager <> Value then
  begin
    if Assigned(FManager) then
    begin
      if Initialized then
        Finalize;
      FManager.FNGDBehaviours.Remove(self);
      // FManager.NotifyChange(self);
    end;
    FManager := Value;
    if Assigned(FManager) then
    begin
      Initialize;
      FManager.FNGDBehaviours.Add(self);
      FManager.NotifyChange(self);
    end;
  end;
end;

procedure TGLNGDBehaviour.SetNewtonBodyMatrix(const Value: TGLMatrix);
begin
  FNewtonBodyMatrix := Value;
  if Assigned(FManager) then
    NewtonBodySetmatrix(FNewtonBody, @FNewtonBodyMatrix);
end;

procedure TGLNGDBehaviour.SetNGDNewtonCollisions(const Value: TGLNGDCollisions);
begin
  FNGDCollisions := Value;
  if Assigned(FManager) then
    UpdCollision;
end;

procedure TGLNGDBehaviour.SetNGDSurfaceItem(const Value: TGLNGDSurfaceItem);
begin
  FNGDSurfaceItem := Value;
  FManager.RebuildAllMaterial;
end;

function TGLNGDBehaviour.StoredTolerance: Boolean;
begin
  Result := not SameValue(FConvexCollisionTolerance, 0.01, epsilon);
end;

class function TGLNGDBehaviour.UniqueItem: Boolean;
begin
  Result := True;
end;

procedure TGLNGDBehaviour.ReadFromFiler(reader: TReader);
var
  version: Integer;
begin
  inherited;
  with reader do
  begin
    version := ReadInteger; // read data version
    Assert(version <= 1); // Archive version

    FManagerName := ReadString;
    FContinuousCollisionMode := ReadBoolean;
    read(FNGDCollisions, SizeOf(TGLNGDCollisions));
    FTreeCollisionOptimize := ReadBoolean;
    if version <= 0 then
      FConvexCollisionTolerance := ReadSingle
    else
      FConvexCollisionTolerance := ReadFloat;
    FFileCollision := ReadString;
  end;
end;

procedure TGLNGDBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(1); // Archive version
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
    WriteBoolean(FContinuousCollisionMode);
    write(FNGDCollisions, SizeOf(TGLNGDCollisions));
    WriteBoolean(FTreeCollisionOptimize);
    WriteFloat(FConvexCollisionTolerance);
    WriteString(FFileCollision);
  end;
end;

procedure TGLNGDBehaviour.Serialize(filename: string);
var
  MyFile: TFileStream;
begin
  MyFile := TFileStream.Create(filename, fmCreate or fmOpenReadWrite);

  NewtonCollisionSerialize(FManager.FNewtonWorld, FCollision,
    @TGLNGDBehaviour.NewtonSerialize, Pointer(MyFile));

  MyFile.Free;
end;

procedure TGLNGDBehaviour.DeSerialize(filename: string);
var
  MyFile: TFileStream;
  collisionInfoRecord: TNewtonCollisionInfoRecord;
begin
  MyFile := TFileStream.Create(filename, fmOpenRead);
  FCollision := NewtonCreateCollisionFromSerialization(FManager.FNewtonWorld,
    @TGLNGDBehaviour.NewtonDeserialize, Pointer(MyFile));
  // SetCollision;
  NewtonBodySetCollision(FNewtonBody, FCollision);
  // Release collision
  NewtonCollisionGetInfo(FCollision, @collisionInfoRecord);
  if collisionInfoRecord.m_collisionType > 2 then
    NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);
  MyFile.Free;
end;

//-------------------------
// TGLNGDDynamic
//-------------------------

procedure TGLNGDDynamic.AddImpulse(const veloc, pointposit: TGLVector);
begin
  if Assigned(FNewtonBody) then
    NewtonBodyAddImpulse(FNewtonBody, @veloc, @pointposit);
end;

constructor TGLNGDDynamic.Create(AOwner: TXCollection);
begin
  inherited;
  FAutoSleep := True;
  FLinearDamping := 0.1;
  FAngularDamping := TGLCoordinates.CreateInitialized(self,
    VectorMake(0.1, 0.1, 0.1, 0), csPoint);
  FAngularDamping.OnNotifyChange := NotifyAngularDampingChange;
  FDensity := 1;
  FVolume := 1;
  FForce := TGLCoordinates.CreateInitialized(self, NullHmgVector, csVector);
  FTorque := TGLCoordinates.CreateInitialized(self, NullHmgVector, csVector);
  FCenterOfMass := TGLCoordinates.CreateInitialized(self,
    NullHmgVector, csPoint);
  FCenterOfMass.OnNotifyChange := NotifyCenterOfMassChange;
  FAABBmin := TGLCoordinates.CreateInitialized(self, NullHmgVector, csPoint);
  FAABBmax := TGLCoordinates.CreateInitialized(self, NullHmgVector, csPoint);
  FAppliedOmega := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedVelocity := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedForce := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedTorque := TGLCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FUseGravity := True;
  FNullCollisionVolume := 0;

  FApplyForceAndTorqueEvent := OnApplyForceAndTorqueEvent;
  FSetTransformEvent := OnSetTransformEvent;
  name := 'NGD Dynamic'
end;

destructor TGLNGDDynamic.Destroy;
begin
  // Clean up everything
  FAngularDamping.Free;
  FForce.Free;
  FTorque.Free;
  FCenterOfMass.Free;
  FAABBmin.Free;
  FAABBmax.Free;
  FAppliedForce.Free;
  FAppliedTorque.Free;
  FAppliedVelocity.Free;
  FAppliedOmega.Free;
  inherited;
end;

procedure TGLNGDDynamic.Finalize;
begin
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
    begin
      // Removing Callback
      NewtonBodySetForceAndTorqueCallback(FNewtonBody, nil);
      NewtonBodySetTransformCallback(FNewtonBody, nil);
    end;
  inherited;
end;

class function TGLNGDDynamic.FriendlyName: string;
begin
  Result := 'NGD Dynamic';
end;

procedure TGLNGDDynamic.Initialize;
begin
  inherited;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
    begin
      // Set Density, Mass and inertie matrix
      SetDensity(FDensity);

      // Set Callback
      NewtonBodySetForceAndTorqueCallback(FNewtonBody,
        @TGLNGDDynamic.NewtonApplyForceAndTorque);
      NewtonBodySetTransformCallback(FNewtonBody,
        @TGLNGDDynamic.NewtonSetTransform);
    end;
end;

procedure TGLNGDDynamic.Render;

  procedure DrawAABB(min, max: TGLCoordinates3);
  begin
    (*
      //    H________G
      //   /.       /|
      //  / .      / |
      // D__._____C  |
      // |  .     |  |
      // | E.-----|--F
      // | .      | /
      // |.       |/
      // A________B
    *)
    // Back
    FManager.AddNode(min.X, min.Y, min.Z); // E
    FManager.AddNode(max.X, min.Y, min.Z); // F
    FManager.AddNode(max.X, min.Y, min.Z); // F
    FManager.AddNode(max.X, max.Y, min.Z); // G
    FManager.AddNode(max.X, max.Y, min.Z); // G
    FManager.AddNode(min.X, max.Y, min.Z); // H
    FManager.AddNode(min.X, max.Y, min.Z); // H
    FManager.AddNode(min.X, min.Y, min.Z); // E
    // Front
    FManager.AddNode(min.X, min.Y, max.Z); // A
    FManager.AddNode(max.X, min.Y, max.Z); // B
    FManager.AddNode(max.X, min.Y, max.Z); // B
    FManager.AddNode(max.X, max.Y, max.Z); // C
    FManager.AddNode(max.X, max.Y, max.Z); // C
    FManager.AddNode(min.X, max.Y, max.Z); // D
    FManager.AddNode(min.X, max.Y, max.Z); // D
    FManager.AddNode(min.X, min.Y, max.Z); // A
    // Edges
    FManager.AddNode(min.X, min.Y, max.Z); // A
    FManager.AddNode(min.X, min.Y, min.Z); // E
    FManager.AddNode(max.X, min.Y, max.Z); // B
    FManager.AddNode(max.X, min.Y, min.Z); // F
    FManager.AddNode(max.X, max.Y, max.Z); // C
    FManager.AddNode(max.X, max.Y, min.Z); // G
    FManager.AddNode(min.X, max.Y, max.Z); // D
    FManager.AddNode(min.X, max.Y, min.Z); // H
  end;

  procedure DrawContact;
  var
    cnt: PNewtonJoint;
    thisContact: PNewtonJoint;
    material: PNewtonMaterial;
    pos, nor: TGLVector;
  begin
    FManager.FCurrentColor := FManager.DebugOption.ContactColor;
    cnt := NewtonBodyGetFirstContactJoint(FNewtonBody);
    while cnt <> nil do
    begin
      thisContact := NewtonContactJointGetFirstContact(cnt);
      while thisContact <> nil do
      begin
        material := NewtonContactGetMaterial(thisContact);
        NewtonMaterialGetContactPositionAndNormal(material, FNewtonBody,
          @pos, @nor);
        FManager.AddNode(pos);
        nor := VectorAdd(pos, nor);
        FManager.AddNode(nor);

        thisContact := NewtonContactJointGetNextContact(cnt, thisContact);
      end;
      cnt := NewtonBodyGetNextContactJoint(FNewtonBody, cnt);
    end;
  end;

  function GetAbsCom(): TGLVector;
  var
    M: TGLMatrix;
  begin
    NewtonBodyGetCentreOfMass(FNewtonBody, @Result);
    M := IdentityHmgMatrix;
    M.W := Result;
    M.W.W := 1;
    M := MatrixMultiply(M, FOwnerBaseSceneObject.AbsoluteMatrix);
    Result := M.W;
  end;

  procedure DrawForce;
  var
    pos: TGLVector;
    nor: TGLVector;
  begin
    pos := GetAbsCom;

    if mdShowForce in FManager.DebugOption.NGDManagerDebugs then
    begin
      FManager.FCurrentColor := FManager.DebugOption.ForceColor;
      nor := VectorAdd(pos, FForce.AsVector);
      FManager.AddNode(pos);
      FManager.AddNode(nor);
    end;

    if mdShowAppliedForce in FManager.DebugOption.NGDManagerDebugs then
    begin
      FManager.FCurrentColor := FManager.DebugOption.AppliedForceColor;
      nor := VectorAdd(pos, FAppliedForce.AsVector);
      FManager.AddNode(pos);
      FManager.AddNode(nor);
    end;

    if mdShowAppliedVelocity in FManager.DebugOption.NGDManagerDebugs then
    begin
      FManager.FCurrentColor := FManager.DebugOption.AppliedVelocityColor;
      nor := VectorAdd(pos, FAppliedVelocity.AsVector);
      FManager.AddNode(pos);
      FManager.AddNode(nor);
    end;
  end;

  procedure DrawCoM;
  var
    com: TGLVector;
    size: Single;
  begin
    FManager.FCurrentColor := FManager.DebugOption.CenterOfMassColor;
    size := FManager.DebugOption.DotAxisSize;
    com := GetAbsCom;
    FManager.AddNode(VectorAdd(com, VectorMake(0, 0, size)));
    FManager.AddNode(VectorAdd(com, VectorMake(0, 0, -size)));
    FManager.AddNode(VectorAdd(com, VectorMake(0, size, 0)));
    FManager.AddNode(VectorAdd(com, VectorMake(0, -size, 0)));
    FManager.AddNode(VectorAdd(com, VectorMake(size, 0, 0)));
    FManager.AddNode(VectorAdd(com, VectorMake(-size, 0, 0)));
  end;

begin
  inherited;
  // Move/Rotate NewtonObject if matrix are not equal in design time.
  if (csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if not MatrixEquals(NewtonBodyMatrix, FOwnerBaseSceneObject.AbsoluteMatrix)
    then
      SetNewtonBodyMatrix(FOwnerBaseSceneObject.AbsoluteMatrix);
  NewtonBodyGetAABB(FNewtonBody, @(FAABBmin.AsVector), @(FAABBmax.AsVector));
  if NewtonBodyGetSleepState(FNewtonBody) = 1 then
    FManager.FCurrentColor := FManager.DebugOption.AABBColorSleep
  else
    FManager.FCurrentColor := FManager.DebugOption.AABBColor;
  if mdShowAABB in FManager.DebugOption.NGDManagerDebugs then
    DrawAABB(FAABBmin, FAABBmax);
  if mdShowContact in FManager.DebugOption.NGDManagerDebugs then
    DrawContact;
  DrawForce; // Draw Force, AppliedForce and AppliedVelocity
  if mdShowCenterOfMass in FManager.DebugOption.NGDManagerDebugs then
    DrawCoM;
end;

procedure TGLNGDDynamic.SetAutoSleep(const Value: Boolean);
begin
  FAutoSleep := Value;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
      NewtonBodySetAutoSleep(FNewtonBody, Ord(FAutoSleep));
end;

procedure TGLNGDDynamic.SetDensity(const Value: Single);
var
  inertia: TGLVector;
  origin: TGLVector;
begin
  if Assigned(FManager) then
    if Value >= 0 then
    begin
      FDensity := Value;

      FVolume := NewtonConvexCollisionCalculateVolume(FCollision);
      NewtonConvexCollisionCalculateInertialMatrix(FCollision,
        @inertia, @origin);

      if IsZero(FVolume, epsilon) then
      begin
        FVolume := FNullCollisionVolume;
        inertia := VectorMake(FNullCollisionVolume, FNullCollisionVolume,
          FNullCollisionVolume, 0);
      end;
      FMass := FVolume * FDensity;
      if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
        NewtonBodySetMassMatrix(FNewtonBody, FMass, FMass * inertia.X,
          FMass * inertia.Y, FMass * inertia.Z);
      FCenterOfMass.AsVector := origin;
    end;
end;

procedure TGLNGDDynamic.SetLinearDamping(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FLinearDamping := Value;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
      NewtonBodySetLinearDamping(FNewtonBody, FLinearDamping);
end;

function TGLNGDDynamic.GetOmega: TGLVector;
begin
  NewtonBodyGetOmega(FNewtonBody, @Result);
end;

procedure TGLNGDDynamic.SetOmega(const Omega: TGLVector);
begin
  NewtonBodySetOmega(FNewtonBody, @Omega);
end;

function TGLNGDDynamic.GetVelocity: TGLVector;
begin
  NewtonBodyGetVelocity(FNewtonBody, @Result);
end;

procedure TGLNGDDynamic.SetVelocity(const Velocity: TGLVector);
begin
  NewtonBodySetVelocity(FNewtonBody, @Velocity);
end;

function TGLNGDDynamic.StoredDensity: Boolean;
begin
  Result := not SameValue(FDensity, 1, epsilon);
end;

function TGLNGDDynamic.StoredLinearDamping: Boolean;
begin
  Result := not SameValue(FLinearDamping, 0.1, epsilon);
end;

function TGLNGDDynamic.StoredNullCollisionVolume: Boolean;
begin
  Result := not SameValue(FNullCollisionVolume, 0, epsilon);
end;

procedure TGLNGDDynamic.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(1); // Archive version
    WriteBoolean(FAutoSleep);
    WriteFloat(FLinearDamping);
    WriteFloat(FDensity);
    WriteBoolean(FUseGravity);
    WriteFloat(FNullCollisionVolume);
  end;
  FForce.WriteToFiler(writer);
  FTorque.WriteToFiler(writer);
  FCenterOfMass.WriteToFiler(writer);
  FAngularDamping.WriteToFiler(writer);
end;

procedure TGLNGDDynamic.ReadFromFiler(reader: TReader);
var
  version: Integer;
begin
  inherited;
  with reader do
  begin
    version := ReadInteger; // read data version
    Assert(version <= 1); // Archive version
    FAutoSleep := ReadBoolean;
    if version <= 0 then
      FLinearDamping := ReadSingle
    else
      FLinearDamping := ReadFloat;
    if version <= 0 then
      FDensity := ReadSingle
    else
      FDensity := ReadFloat;
    // if Version >= 1 then
    FUseGravity := ReadBoolean;
    if version <= 0 then
      FNullCollisionVolume := ReadSingle
    else
      FNullCollisionVolume := ReadFloat;
  end;
  FForce.ReadFromFiler(reader);
  FTorque.ReadFromFiler(reader);
  FCenterOfMass.ReadFromFiler(reader);
  FAngularDamping.ReadFromFiler(reader);
end;

procedure TGLNGDDynamic.Loaded;
begin
  inherited;
  if Assigned(FManager) then
  begin
    SetAutoSleep(FAutoSleep);
    SetLinearDamping(FLinearDamping);
    SetDensity(FDensity);
    NotifyCenterOfMassChange(self);
    NotifyAngularDampingChange(self);
  end;
end;

class procedure TGLNGDDynamic.NewtonApplyForceAndTorque(const body: PNewtonBody;
  timestep: NGDFloat; threadIndex: Integer); cdecl;
begin
  TGLNGDDynamic(NewtonBodyGetUserData(body)).FApplyForceAndTorqueEvent(body,
    timestep, threadIndex);
end;

class procedure TGLNGDDynamic.NewtonSetTransform(const body: PNewtonBody;
  const matrix: PNGDFloat; threadIndex: Integer); cdecl;
begin
  TGLNGDDynamic(NewtonBodyGetUserData(body)).FSetTransformEvent(body, matrix,
    threadIndex);
end;

procedure TGLNGDDynamic.NotifyAngularDampingChange(Sender: TObject);
begin
  FAngularDamping.OnNotifyChange := nil;
  if (FAngularDamping.X >= 0) and (FAngularDamping.X <= 1) and
    (FAngularDamping.Y >= 0) and (FAngularDamping.Y <= 1) and
    (FAngularDamping.Z >= 0) and (FAngularDamping.Z <= 1) then
    if Assigned(FManager) then
      NewtonBodySetAngularDamping(FNewtonBody, @(FAngularDamping.AsVector));
  FAngularDamping.OnNotifyChange := NotifyAngularDampingChange;
end;

procedure TGLNGDDynamic.NotifyCenterOfMassChange(Sender: TObject);
begin
  FCenterOfMass.OnNotifyChange := nil;
  if Assigned(FManager) then
    NewtonBodySetCentreOfMass(FNewtonBody, @(FCenterOfMass.AsVector));
  FCenterOfMass.OnNotifyChange := NotifyCenterOfMassChange;
end;

procedure TGLNGDDynamic.OnApplyForceAndTorqueEvent(const cbody: PNewtonBody;
  timestep: NGDFloat; threadIndex: Integer);
var
  worldGravity: TGLVector;
begin
  // Read Only: We get the force and torque resulting from every interaction on this body
  NewtonBodyGetForce(cbody, @(FAppliedForce.AsVector));
  NewtonBodyGetTorque(cbody, @(FAppliedTorque.AsVector));
  NewtonBodyGetVelocity(cbody, @(FAppliedVelocity.AsVector));
  NewtonBodyGetOmega(cbody, @(FAppliedOmega.AsVector));
  // Raise Custom event
  if Assigned(FCustomForceAndTorqueEvent) then
    FCustomForceAndTorqueEvent(cbody, timestep, threadIndex)
  else
  begin
    NewtonBodySetForce(cbody, @(Force.AsVector));
    NewtonBodySetTorque(cbody, @(Torque.AsVector));
    // Add Gravity from World
    if FUseGravity then
    begin
      worldGravity := VectorScale(FManager.Gravity.AsVector, FMass);
      NewtonBodyAddForce(cbody, @(worldGravity));
    end;
  end;
end;

procedure TGLNGDDynamic.OnSetTransformEvent(const cbody: PNewtonBody;
  const cmatrix: PNGDFloat; threadIndex: Integer);
var
  epsi: Single;
begin
  // The Newton API does not support scale [scale modifie value in matrix],
  // so this line reset scale of the glsceneObject to (1,1,1)
  // to avoid crashing the application
  epsi := 0.0001;
  with FOwnerBaseSceneObject do
    if not SameValue(Scale.X, 1.0, epsi) or not SameValue(Scale.Y, 1.0, epsi) or
      not SameValue(Scale.Z, 1.0, epsi) then
    begin
      Scale.SetVector(1, 1, 1);
      SetNewtonBodyMatrix(AbsoluteMatrix);
    end
    else
      // Make the Position and orientation of the glscene-Object relative to the
      // NewtonBody position and orientation.
      FOwnerBaseSceneObject.AbsoluteMatrix := PGLMatrix(cmatrix)^;
end;

// ------------------------
// TGLNGDStatic
// ------------------------

procedure TGLNGDStatic.Render;
begin
  inherited;
  // Move/Rotate NewtonObject if matrix are not equal in run time.
  if not MatrixEquals(NewtonBodyMatrix, FOwnerBaseSceneObject.AbsoluteMatrix)
  then
    SetNewtonBodyMatrix(FOwnerBaseSceneObject.AbsoluteMatrix);
end;

class function TGLNGDStatic.FriendlyName: string;
begin
  Result := 'NGD Static';
end;

// ------------------------
// TGLNGDSurfaceItem
// ------------------------

function TGLNGDSurfaceItem.GetDisplayName: string;
begin
  if FDisplayName = '' then
    FDisplayName := 'Iron';
  Result := FDisplayName;
end;

procedure TGLNGDSurfaceItem.SetDisplayName(const Value: string);
begin
  inherited;
  FDisplayName := Value;
end;

// ------------------------
// TGLNGDSurfacePair
// ------------------------

constructor TGLNGDSurfacePair.Create(Collection: TCollection);
begin
  inherited;
  FSoftness := 0.1;
  FElasticity := 0.4;
  FCollidable := True;
  FStaticFriction := 0.9;
  FKineticFriction := 0.5;
  FContinuousCollisionMode := False;
  FThickness := False;

  FAABBOverlapEvent := OnNewtonAABBOverlapEvent;
  FContactProcessEvent := OnNewtonContactsProcessEvent;
  FManager := TGLNGDManager(Collection.Owner);
  FManager.RebuildAllMaterial;
end;

class function TGLNGDSurfacePair.NewtonAABBOverlap(const material
  : PNewtonMaterial; const body0, body1: PNewtonBody; threadIndex: Integer)
  : Integer; cdecl;
begin
  Result := Ord(TGLNGDSurfacePair(NewtonMaterialGetMaterialPairUserData
    (material)).FAABBOverlapEvent(material, body0, body1, threadIndex));
end;

class procedure TGLNGDSurfacePair.NewtonContactsProcess(const contact
  : PNewtonJoint; timestep: NGDFloat; threadIndex: Integer); cdecl;
begin
  TGLNGDSurfacePair(NewtonMaterialGetMaterialPairUserData
    (NewtonContactGetMaterial(NewtonContactJointGetFirstContact(contact))))
    .FContactProcessEvent(contact, timestep, threadIndex);
end;

function TGLNGDSurfacePair.OnNewtonAABBOverlapEvent(const cmaterial
  : PNewtonMaterial; const cbody0, cbody1: PNewtonBody;
  threadIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TGLNGDSurfacePair.OnNewtonContactsProcessEvent(const ccontact
  : PNewtonJoint; timestep: NGDFloat; threadIndex: Integer);
begin
  //
end;

procedure TGLNGDSurfacePair.SetCollidable(const Value: Boolean);
begin
  FCollidable := Value;
  FManager.RebuildAllMaterial;
end;

procedure TGLNGDSurfacePair.SetContinuousCollisionMode(const Value: Boolean);
begin
  FContinuousCollisionMode := Value;
  FManager.RebuildAllMaterial;
end;

procedure TGLNGDSurfacePair.SetElasticity(const Value: Single);
begin
  if (Value >= 0) then
    FElasticity := Value;
  FManager.RebuildAllMaterial;
end;

procedure TGLNGDSurfacePair.SetKineticFriction(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FKineticFriction := Value;
  FManager.RebuildAllMaterial;
end;

procedure TGLNGDSurfacePair.SetMaterialItems(const item1,
  item2: TGLNGDSurfaceItem);
begin
  FNGDSurfaceItem1 := item1;
  FNGDSurfaceItem2 := item2;
  FManager.RebuildAllMaterial;
end;

procedure TGLNGDSurfacePair.SetSoftness(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FSoftness := Value;
  FManager.RebuildAllMaterial;
end;

procedure TGLNGDSurfacePair.SetStaticFriction(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FStaticFriction := Value;
  FManager.RebuildAllMaterial;
end;

procedure TGLNGDSurfacePair.SetThickness(const Value: Boolean);
begin
  FThickness := Value;
  FManager.RebuildAllMaterial;
end;

function TGLNGDSurfacePair.StoredElasticity: Boolean;
begin
  Result := not SameValue(FElasticity, 0.4, epsilon);
end;

function TGLNGDSurfacePair.StoredKineticFriction: Boolean;
begin
  Result := not SameValue(FKineticFriction, 0.5, epsilon);
end;

function TGLNGDSurfacePair.StoredSoftness: Boolean;
begin
  Result := not SameValue(FSoftness, 0.1, epsilon);
end;

function TGLNGDSurfacePair.StoredStaticFriction: Boolean;
begin
  Result := not SameValue(FStaticFriction, 0.9, epsilon);
end;

// ------------------------
//  TGLNGDJoint
// ------------------------

constructor TGLNGDJoint.Create(Collection: TCollection);
begin
  inherited;
  FCollisionState := False;
  FStiffness := 0.9;
  FNewtonJoint := nil;
  FNewtonUserJoint := nil;
  FParentObject := nil;
  FChildObject := nil;

  FManager := TGLNGDManager(Collection.Owner);

  FBallAndSocketOptions := TGLNGDJointPivot.Create(FManager, self);
  FHingeOptions := TGLNGDJointPin.Create(FManager, self);
  FSliderOptions := TGLNGDJointPin.Create(FManager, self);
  FCorkscrewOptions := TGLNGDJointPin.Create(FManager, self);
  FUniversalOptions := TGLNGDJointPin2.Create(FManager, self);

  FCustomBallAndSocketOptions := TGLNGDJointBallAndSocket.Create
    (FManager, self);
  FCustomHingeOptions := TGLNGDJointHinge.Create(FManager, self);
  FCustomSliderOptions := TGLNGDJointSlider.Create(FManager, self);
  FKinematicOptions := TGLNGDJointKinematicController.Create;

  FUPVectorDirection := TGLCoordinates.CreateInitialized(self, YHmgVector,
    csVector);
  FUPVectorDirection.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TGLNGDJoint.Destroy;
begin
  DestroyNewtonData;
  FParentObject := nil;
  FChildObject := nil;
  // Free options
  FBallAndSocketOptions.Free;
  FHingeOptions.Free;
  FSliderOptions.Free;
  FCorkscrewOptions.Free;
  FUniversalOptions.Free;
  FCustomBallAndSocketOptions.Free;
  FCustomHingeOptions.Free;
  FCustomSliderOptions.Free;
  FKinematicOptions.Free;
  FUPVectorDirection.Free;
  inherited;
end;

procedure TGLNGDJoint.DestroyNewtonData;
begin
  if FNewtonJoint <> nil then
  begin
    Assert((FManager <> nil) and (FManager.FNewtonWorld <> nil));
    NewtonDestroyJoint(FManager.FNewtonWorld, FNewtonJoint);
    FNewtonJoint := nil;
  end;
  if FNewtonUserJoint <> nil then
  begin
    (* CustomDestroyJoint(FNewtonUserJoint); *) //from dJointLibrary.dll
    FNewtonUserJoint := nil;
  end;
end;

procedure TGLNGDJoint.KinematicControllerPick(pickpoint: TGLVector;
  PickedActions: TGLNGDPickedActions);
begin
  (* CustomDestroyJoint(FNewtonUserJoint);  //from dJointLibrary.dll

  if FJointType = nj_KinematicController then
    if Assigned(FParentObject) then
    begin
      // Creates the joint
      if PickedActions = paAttach then
      begin
        if not Assigned(FNewtonUserJoint) then
          if Assigned(GetNGDDynamic(FParentObject).FNewtonBody) then
            FNewtonUserJoint := CreateCustomKinematicController
              (GetNGDDynamic(FParentObject).FNewtonBody, @pickpoint);
      end;
      // Change the TargetPoint
      if (PickedActions = paMove) or (PickedActions = paAttach) then
      begin
        if Assigned(FNewtonUserJoint) then
        begin
          CustomKinematicControllerSetPickMode(FNewtonUserJoint,
            Ord(FKinematicOptions.FPickModeLinear));
          CustomKinematicControllerSetMaxLinearFriction(FNewtonUserJoint,
            FKinematicOptions.FLinearFriction);
          CustomKinematicControllerSetMaxAngularFriction(FNewtonUserJoint,
            FKinematicOptions.FAngularFriction);
          CustomKinematicControllerSetTargetPosit(FNewtonUserJoint, @pickpoint);
        end;
      end;
      // Delete the joint
      if PickedActions = paDetach then
      begin
        if Assigned(FNewtonUserJoint) then
        begin
          CustomDestroyJoint(FNewtonUserJoint);
          FNewtonUserJoint := nil;
          // Reset autosleep because this joint turns it off
          NewtonBodySetAutoSleep(GetNGDDynamic(FParentObject).FNewtonBody,
            Ord(GetNGDDynamic(FParentObject).AutoSleep));
        end;
        ParentObject := nil;
      end;
    end;
  *)
end;

procedure TGLNGDJoint.Render;

  procedure DrawPivot(pivot: TGLVector);
  var
    size: Single;
  begin
    size := FManager.DebugOption.DotAxisSize;
    FManager.FCurrentColor := FManager.DebugOption.JointPivotColor;
    FManager.AddNode(VectorAdd(pivot, VectorMake(0, 0, size)));
    FManager.AddNode(VectorAdd(pivot, VectorMake(0, 0, -size)));
    FManager.AddNode(VectorAdd(pivot, VectorMake(0, size, 0)));
    FManager.AddNode(VectorAdd(pivot, VectorMake(0, -size, 0)));
    FManager.AddNode(VectorAdd(pivot, VectorMake(size, 0, 0)));
    FManager.AddNode(VectorAdd(pivot, VectorMake(-size, 0, 0)));
  end;

  procedure DrawPin(pin, pivot: TGLVector);
  begin
    FManager.FCurrentColor := FManager.DebugOption.JointAxisColor;
    FManager.AddNode(VectorAdd(pivot, pin));
    FManager.AddNode(VectorAdd(pivot, VectorNegate(pin)));
  end;

  procedure DrawJoint(pivot: TGLVector);
  begin
    FManager.FCurrentColor := FManager.DebugOption.CustomColor;
    FManager.AddNode(FParentObject.AbsolutePosition);
    FManager.AddNode(pivot);
    FManager.AddNode(pivot);
    FManager.AddNode(FChildObject.AbsolutePosition);
  end;

  procedure DrawKinematic;
  var
    pickedMatrix: TGLMatrix;
    size: Single;
  begin
    size := FManager.DebugOption.DotAxisSize;
/// From dJointLibrary.dll
///    CustomKinematicControllerGetTargetMatrix(FNewtonUserJoint, @pickedMatrix);
    FManager.FCurrentColor := FManager.DebugOption.JointAxisColor;

    FManager.AddNode(FParentObject.AbsolutePosition);
    FManager.AddNode(pickedMatrix.W);

    FManager.FCurrentColor := FManager.DebugOption.JointPivotColor;
    FManager.AddNode(VectorAdd(pickedMatrix.W, VectorMake(0, 0, size)));
    FManager.AddNode(VectorAdd(pickedMatrix.W, VectorMake(0, 0, -size)));
    FManager.AddNode(VectorAdd(pickedMatrix.W, VectorMake(0, size, 0)));
    FManager.AddNode(VectorAdd(pickedMatrix.W, VectorMake(0, -size, 0)));
    FManager.AddNode(VectorAdd(pickedMatrix.W, VectorMake(size, 0, 0)));
    FManager.AddNode(VectorAdd(pickedMatrix.W, VectorMake(-size, 0, 0)));
  end;
begin
  case FJointType of
    nj_BallAndSocket:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FBallAndSocketOptions.FPivotPoint.AsVector);
        DrawPivot(FBallAndSocketOptions.FPivotPoint.AsVector);
      end;
    nj_Hinge:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FHingeOptions.FPivotPoint.AsVector);
        DrawPin(FHingeOptions.FPinDirection.AsVector,
          FHingeOptions.FPivotPoint.AsVector);
        DrawPivot(FHingeOptions.FPivotPoint.AsVector);
      end;
    nj_Slider:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FSliderOptions.FPivotPoint.AsVector);
        DrawPin(FSliderOptions.FPinDirection.AsVector,
          FSliderOptions.FPivotPoint.AsVector);
        DrawPivot(FSliderOptions.FPivotPoint.AsVector);
      end;
    nj_Corkscrew:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FCorkscrewOptions.FPivotPoint.AsVector);
        DrawPin(FCorkscrewOptions.FPinDirection.AsVector,
          FCorkscrewOptions.FPivotPoint.AsVector);
        DrawPivot(FCorkscrewOptions.FPivotPoint.AsVector);
      end;
    nj_Universal:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FUniversalOptions.FPivotPoint.AsVector);
        DrawPin(FUniversalOptions.FPinDirection.AsVector,
          FUniversalOptions.FPivotPoint.AsVector);
        DrawPin(FUniversalOptions.FPinDirection2.AsVector,
          FUniversalOptions.FPivotPoint.AsVector);
        DrawPivot(FUniversalOptions.FPivotPoint.AsVector);
      end;
    nj_CustomBallAndSocket:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FCustomBallAndSocketOptions.FPivotPoint.AsVector);
        DrawPivot(FCustomBallAndSocketOptions.FPivotPoint.AsVector);
      end;
    nj_CustomHinge:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FCustomHingeOptions.FPivotPoint.AsVector);
        DrawPin(FCustomHingeOptions.FPinDirection.AsVector,
          FCustomHingeOptions.FPivotPoint.AsVector);
        DrawPivot(FCustomHingeOptions.FPivotPoint.AsVector);
      end;
    nj_CustomSlider:
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        DrawJoint(FCustomSliderOptions.FPivotPoint.AsVector);
        DrawPin(FCustomSliderOptions.FPinDirection.AsVector,
          FCustomSliderOptions.FPivotPoint.AsVector);
        DrawPivot(FCustomSliderOptions.FPivotPoint.AsVector);
      end;
    nj_UpVector:
      if Assigned(FParentObject) then
      begin // special
        FManager.FCurrentColor := FManager.DebugOption.JointAxisColor;
        FManager.AddNode(FParentObject.AbsolutePosition);
        FManager.AddNode(VectorAdd(FParentObject.AbsolutePosition,
          FUPVectorDirection.AsVector));
      end;
    nj_KinematicController:
      if Assigned(FParentObject) and Assigned(FNewtonUserJoint) then
      begin // special
        DrawKinematic;
      end;
  end;
end;

procedure TGLNGDJoint.SetChildObject(const Value: TGLBaseSceneObject);
begin
  FChildObject := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TGLNGDJoint.SetCollisionState(const Value: Boolean);
begin
  FCollisionState := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TGLNGDJoint.SetJointType(const Value: TGLNGDJoints);
begin
  FJointType := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TGLNGDJoint.SetParentObject(const Value: TGLBaseSceneObject);
begin
  FParentObject := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TGLNGDJoint.SetStiffness(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
  begin
    FStiffness := Value;
    FManager.RebuildAllJoint(self);
  end;
end;

function TGLNGDJoint.StoredStiffness: Boolean;
begin
  Result := not SameValue(FStiffness, 0.9, epsilon);
end;

// ------------------------
// TGLNGDJointPivot
// ------------------------

constructor TGLNGDJointPivot.Create(AOwner: TComponent; aOuter: TGLNGDJoint);
begin
  FManager := AOwner as TGLNGDManager;
  FOuter := aOuter;
  FPivotPoint := TGLCoordinates.CreateInitialized(aOuter, NullHMGPoint,
    csPoint);
  FPivotPoint.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TGLNGDJointPivot.Destroy;
begin
  FPivotPoint.Free;
  inherited;
end;

// ------------------------
// TGLNGDJointPin
// ------------------------

constructor TGLNGDJointPin.Create(AOwner: TComponent; aOuter: TGLNGDJoint);
begin
  inherited;
  FPinDirection := TGLCoordinates.CreateInitialized(aOuter, NullHmgVector,
    csVector);
  FPinDirection.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TGLNGDJointPin.Destroy;
begin
  FPinDirection.Free;
  inherited;
end;

// ------------------------
// TGLNGDJointPin2
// ------------------------

constructor TGLNGDJointPin2.Create(AOwner: TComponent; aOuter: TGLNGDJoint);
begin
  inherited;
  FPinDirection2 := TGLCoordinates.CreateInitialized(aOuter, NullHmgVector,
    csVector);
  FPinDirection2.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TGLNGDJointPin2.Destroy;
begin
  FPinDirection2.Free;
  inherited;
end;

// ------------------------
// TGLNGDJointBallAndSocket
// ------------------------

constructor TGLNGDJointBallAndSocket.Create(AOwner: TComponent;
  aOuter: TGLNGDJoint);
begin
  inherited;
  FConeAngle := 90;
  FMinTwistAngle := -90;
  FMaxTwistAngle := 90;
end;

procedure TGLNGDJointBallAndSocket.SetConeAngle(const Value: Single);
begin
  FConeAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TGLNGDJointBallAndSocket.SetMaxTwistAngle(const Value: Single);
begin
  FMaxTwistAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TGLNGDJointBallAndSocket.SetMinTwistAngle(const Value: Single);
begin
  FMinTwistAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

function TGLNGDJointBallAndSocket.StoredConeAngle: Boolean;
begin
  Result := not SameValue(FConeAngle, 90, epsilon);
end;

function TGLNGDJointBallAndSocket.StoredMaxTwistAngle: Boolean;
begin
  Result := not SameValue(FMaxTwistAngle, 90, epsilon);
end;

function TGLNGDJointBallAndSocket.StoredMinTwistAngle: Boolean;
begin
  Result := not SameValue(FMinTwistAngle, -90, epsilon);
end;

// ------------------------
// TGLNGDJointHinge
// ------------------------

constructor TGLNGDJointHinge.Create(AOwner: TComponent; aOuter: TGLNGDJoint);
begin
  inherited;
  FMinAngle := -90;
  FMaxAngle := 90;
end;

procedure TGLNGDJointHinge.SetMaxAngle(const Value: Single);
begin
  FMaxAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TGLNGDJointHinge.SetMinAngle(const Value: Single);
begin
  FMinAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

function TGLNGDJointHinge.StoredMaxAngle: Boolean;
begin
  Result := not SameValue(FMaxAngle, 90, epsilon);
end;

function TGLNGDJointHinge.StoredMinAngle: Boolean;
begin
  Result := not SameValue(FMinAngle, -90, epsilon);
end;

// ------------------------
// TGLNGDJointSlider
// ------------------------

constructor TGLNGDJointSlider.Create(AOwner: TComponent; aOuter: TGLNGDJoint);
begin
  inherited;
  FMinDistance := -10;
  FMaxDistance := 10;
end;

procedure TGLNGDJointSlider.SetMaxDistance(const Value: Single);
begin
  FMaxDistance := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TGLNGDJointSlider.SetMinDistance(const Value: Single);
begin
  FMinDistance := Value;
  FManager.RebuildAllJoint(FOuter);
end;

function TGLNGDJointSlider.StoredMaxDistance: Boolean;
begin
  Result := not SameValue(FMaxDistance, 10, epsilon);
end;

function TGLNGDJointSlider.StoredMinDistance: Boolean;
begin
  Result := not SameValue(FMinDistance, -10, epsilon);
end;

// ----------------------------------
// TGLNGDJointKinematicController
// ----------------------------------

constructor TGLNGDJointKinematicController.Create;
begin
  FPickModeLinear := False;
  FLinearFriction := 750;
  FAngularFriction := 250;
end;

function TGLNGDJointKinematicController.StoredAngularFriction: Boolean;
begin
  Result := not SameValue(FAngularFriction, 250, epsilon);
end;

function TGLNGDJointKinematicController.StoredLinearFriction: Boolean;
begin
  Result := not SameValue(FLinearFriction, 750, epsilon);
end;

// ------------------------
// TGLNGDBehaviourList
// ------------------------

function TGLNGDBehaviourList.GetBehav(index: Integer): TGLNGDBehaviour;
begin
  Result := Items[index];
end;

procedure TGLNGDBehaviourList.PutBehav(index: Integer; Item: TGLNGDBehaviour);
begin
  inherited put(index, Item);
end;

// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------

RegisterXCollectionItemClass(TGLNGDDynamic);
RegisterXCollectionItemClass(TGLNGDStatic);

// ------------------------------------------------------------------
finalization

// ------------------------------------------------------------------

UnregisterXCollectionItemClass(TGLNGDDynamic);
UnregisterXCollectionItemClass(TGLNGDStatic);

// CloseNGD;

end.
