//
// The graphics engine GLXEngine. The unit of GXScene for Delphi
//
unit GXS.NGDManager;
(*
  The Newton Game Dynamics Manager
*)
interface

uses
  System.Classes, // TComponent Tlist TWriter TReader TPersistent
  System.SysUtils, // System utilities
  System.Math, // Samevalue isZero to compare single
  System.Types,

  NGD.Import,

  Stage.VectorTypes,
  Stage.VectorGeometry, // PVector4f TVector4f TMatrix4f PMatrix4f NullHmgVector...
  Stage.Manager,

  GXS.VectorLists, // TgxAffineVectorList for Tree
  GXS.XCollection, // TXCollection file function
  GXS.GeometryBB,
  GXS.BaseClasses,
  GXS.PersistentClasses,
  GXS.Scene,
  GXS.Coordinates,
  GXS.Objects,
  GXS.GeomObjects,
  GXS.VectorFileObjects, // cube cone freeform...
  GXS.Color // For show debug

;

type
  PdFloat = ^dFloat;

  TgxNGDHeightField = record
    heightArray: array of Word;
    width: Integer;
    depth: Integer;
    gridDiagonals: Boolean;
    widthDepthScale: Single;
    heightScale: Single;
  end;

  TgxNGDBehaviour = class;
  TgxNGDManager = class;
  TgxNGDSurfaceItem = class;
  TgxNGDJoint = class;

  TgxNGDSolverModels = (smExact = 0, smLinear1, smLinear2, smLinear3, smLinear4,
    smLinear5, smLinear6, smLinear7, smLinear8, smLinear9);

  TgxNGDFrictionModels = (fmExact = 0, fmAdaptive);
  TgxNGDPickedActions = (paAttach = 0, paMove, paDetach);

  TgxNGDManagerDebug = (mdShowGeometry, mdShowAABB, mdShowCenterOfMass,
    mdShowContact, mdShowJoint, mdShowForce, mdShowAppliedForce,
    mdShowAppliedVelocity);
  TgxNGDManagerDebugs = set of TgxNGDManagerDebug;

  TgxNGDCollisions = (nc_Primitive = 0, nc_Convex, nc_BBox, nc_BSphere, nc_Tree,
    nc_Mesh, nc_Null, nc_HeightField, nc_NGDFile);

  TgxNGDJoints = (nj_BallAndSocket, nj_Hinge, nj_Slider, nj_Corkscrew,
    nj_Universal, nj_CustomBallAndSocket, nj_CustomHinge, nj_CustomSlider,
    nj_UpVector, nj_KinematicController);

  TgxNGDBehaviourList = class(TList)
  protected
    function GetBehav(index: Integer): TgxNGDBehaviour;
    procedure PutBehav(index: Integer; Item: TgxNGDBehaviour);
  public
    property ItemsBehav[index: Integer]: TgxNGDBehaviour read GetBehav
      write PutBehav; default;
  end;

  // Events for Newton Callback
  TCollisionIteratorEvent = procedure(const userData: Pointer;
    vertexCount: Integer; const cfaceArray: PdFloat; faceId: Integer)
    of object;

  TApplyForceAndTorqueEvent = procedure(const cbody: PNewtonBody;
    timestep: dFloat; threadIndex: Integer) of object;

  TSetTransformEvent = procedure(const cbody: PNewtonBody;
    const cmatrix: PdFloat; threadIndex: Integer) of object;

  TSerializeEvent = procedure(serializeHandle: Pointer; const cbuffer: Pointer;
    size: Cardinal) of object;

  TDeSerializeEvent = procedure(serializeHandle: Pointer; buffer: Pointer;
    size: Cardinal) of object;

  TAABBOverlapEvent = function(const cmaterial: PNewtonMaterial;
    const cbody0: PNewtonBody; const cbody1: PNewtonBody; threadIndex: Integer)
    : Boolean of object;

  TContactProcessEvent = procedure(const ccontact: PNewtonJoint;
    timestep: dFloat; threadIndex: Integer) of object;

  TgxNGDDebugOption = class(TPersistent)
  strict private
    FManager: TgxNGDManager;
    FGeomColorDyn: TgxColor; // Green
    FGeomColorStat: TgxColor; // Red
    FAABBColor: TgxColor; // Yellow
    FAABBColorSleep: TgxColor; // Orange
    FCenterOfMassColor: TgxColor; // Purple dot
    FContactColor: TgxColor; // White
    FJointAxisColor: TgxColor; // Blue
    FJointPivotColor: TgxColor; // Aquamarine
    FForceColor: TgxColor; // Black
    FAppliedForceColor: TgxColor; // Silver
    FAppliedVelocityColor: TgxColor; // Lime
    FCustomColor: TgxColor; // Aqua
    FDotAxisSize: Single; // 1
    FManagerDebugs: TgxNGDManagerDebugs; // Default All false
    procedure SetManagerDebugs(const Value: TgxNGDManagerDebugs);
    procedure SetDotAxisSize(const Value: Single);
    function StoredDotAxis: Boolean;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property GeomColorDyn: TgxColor read FGeomColorDyn write FGeomColorDyn;
    property GeomColorStat: TgxColor read FGeomColorStat write FGeomColorStat;
    property AABBColor: TgxColor read FAABBColor write FAABBColor;
    property AABBColorSleep: TgxColor read FAABBColorSleep
      write FAABBColorSleep;
    property CenterOfMassColor: TgxColor read FCenterOfMassColor
      write FCenterOfMassColor;
    property ContactColor: TgxColor read FContactColor write FContactColor;
    property JointAxisColor: TgxColor read FJointAxisColor
      write FJointAxisColor;
    property JointPivotColor: TgxColor read FJointPivotColor
      write FJointPivotColor;
    property ForceColor: TgxColor read FForceColor write FForceColor;
    property AppliedForceColor: TgxColor read FAppliedForceColor
      write FAppliedForceColor;
    property AppliedVelocityColor: TgxColor read FAppliedVelocityColor
      write FAppliedVelocityColor;
    property CustomColor: TgxColor read FCustomColor write FCustomColor;
    property NGDManagerDebugs: TgxNGDManagerDebugs read FManagerDebugs
      write SetManagerDebugs default [];
    property DotAxisSize: Single read FDotAxisSize write SetDotAxisSize
      stored StoredDotAxis;
  end;

  TgxNGDManager = class(TComponent)
  strict private
    FVisible: Boolean; // Show Debug at design time
    FVisibleAtRunTime: Boolean; // Show Debug at run time
    FDllVersion: Integer;
    FSolverModel: TgxNGDSolverModels; // Default=Exact
    FFrictionModel: TgxNGDFrictionModels; // Default=Exact
    FMinimumFrameRate: Integer; // Default=60
    FWorldSizeMin: TgxCoordinates; // Default=-100, -100, -100
    FWorldSizeMax: TgxCoordinates; // Default=100, 100, 100
    FThreadCount: Integer; // Default=1
    FGravity: TgxCoordinates; // Default=(0,-9.81,0)
    FNewtonSurfaceItem: TCollection;
    FNewtonSurfacePair: TOwnedCollection;
    FNewtonJointGroup: TOwnedCollection;
    FNewtonDebugOption: TgxNGDDebugOption;
    FGLLines: TgxLines;
  private
    FNewtonWorld: PNewtonWorld;
    FNGDBehaviours: TgxNGDBehaviourList;
    FCurrentColor: TgxColor;
  protected
    procedure Loaded; override;
    procedure SetVisible(const Value: Boolean);
    procedure SetVisibleAtRunTime(const Value: Boolean);
    procedure SetSolverModel(const Value: TgxNGDSolverModels);
    procedure SetFrictionModel(const Value: TgxNGDFrictionModels);
    procedure SetMinimumFrameRate(const Value: Integer);
    procedure SetThreadCount(const Value: Integer);
    procedure SetGXLines(const Value: TgxLines);
    function GetBodyCount: Integer;
    function GetConstraintCount: Integer;
    procedure AddNode(const coords: TgxCustomCoordinates); overload;
    procedure AddNode(const X, Y, Z: Single); overload;
    procedure AddNode(const Value: TVector4f); overload;
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
    property SolverModel: TgxNGDSolverModels read FSolverModel
      write SetSolverModel default smExact;
    property FrictionModel: TgxNGDFrictionModels read FFrictionModel
      write SetFrictionModel default fmExact;
    property MinimumFrameRate: Integer read FMinimumFrameRate
      write SetMinimumFrameRate default 60;
    property ThreadCount: Integer read FThreadCount write SetThreadCount
      default 1;
    property DllVersion: Integer read FDllVersion;
    property NewtonBodyCount: Integer read GetBodyCount;
    property NewtonConstraintCount: Integer read GetConstraintCount;
    property Gravity: TgxCoordinates read FGravity write FGravity;
    property WorldSizeMin: TgxCoordinates read FWorldSizeMin
      write FWorldSizeMin;
    property WorldSizeMax: TgxCoordinates read FWorldSizeMax
      write FWorldSizeMax;
    property NewtonSurfaceItem: TCollection read FNewtonSurfaceItem
      write FNewtonSurfaceItem;
    property NewtonSurfacePair: TOwnedCollection read FNewtonSurfacePair
      write FNewtonSurfacePair;
    property DebugOption: TgxNGDDebugOption read FNewtonDebugOption
      write FNewtonDebugOption;
    property Line: TgxLines read FGLLines write SetGXLines;
    property NewtonJoint: TOwnedCollection read FNewtonJointGroup
      write FNewtonJointGroup;
  end;

  // Basis structures for behaviour style implementations.
  TgxNGDBehaviour = class(TgxBehaviour)
  private
    FManager: TgxNGDManager;
    FManagerName: string;
    FInitialized: Boolean;
    FNewtonBody: PNewtonBody;
    FCollision: PNewtonCollision;
    FNewtonBodyMatrix: TMatrix4f; // Position and Orientation
    FContinuousCollisionMode: Boolean; // Default=False
    FNewtonCollisions: TgxNGDCollisions;
    FCollisionIteratorEvent: TCollisionIteratorEvent;
    FOwnerBaseSceneObject: TgxBaseSceneObject;
    // FNullCollisionMass: Single; // Default=0
    FTreeCollisionOptimize: Boolean; // Default=True
    FConvexCollisionTolerance: Single; // Default=0.01 1%
    FFileCollision: string;
    FSurfaceItem: TgxNGDSurfaceItem;
    FHeightFieldOptions: TgxNGDHeightField;
  protected
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure SetManager(Value: TgxNGDManager);
    procedure SetNewtonBodyMatrix(const Value: TMatrix4f);
    procedure SetContinuousCollisionMode(const Value: Boolean);
    function GetNewtonBodyMatrix: TMatrix4f;
    function GetNewtonBodyAABB: TAABB;
    procedure UpdCollision; virtual;
    procedure Render; virtual;
    procedure SetNewtonCollisions(const Value: TgxNGDCollisions);
    procedure SetNewtonSurfaceItem(const Value: TgxNGDSurfaceItem);
    procedure SetHeightFieldOptions(const Value: TgxNGDHeightField);
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
      vertexCount: Integer; const cfaceArray: PdFloat; faceId: Integer);
    // CallBack
    class procedure NewtonCollisionIterator(const userData: Pointer;
      vertexCount: Integer; const faceArray: PdFloat; faceId: Integer);
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
    property NewtonBodyMatrix: TMatrix4f read GetNewtonBodyMatrix
      write SetNewtonBodyMatrix;
    property NewtonBodyAABB: TAABB read GetNewtonBodyAABB;
    procedure Serialize(filename: string);
    procedure DeSerialize(filename: string);
    property HeightFieldOptions: TgxNGDHeightField read FHeightFieldOptions
      write SetHeightFieldOptions;
  published
    property Manager: TgxNGDManager read FManager write SetManager;
    property ContinuousCollisionMode: Boolean read FContinuousCollisionMode
      write SetContinuousCollisionMode default False;
    property NGDNewtonCollisions: TgxNGDCollisions read FNewtonCollisions
      write SetNewtonCollisions default nc_Primitive;
    property TreeCollisionOptimize: Boolean read FTreeCollisionOptimize
      write FTreeCollisionOptimize default True;
    property ConvexCollisionTolerance: Single read FConvexCollisionTolerance
      write FConvexCollisionTolerance stored StoredTolerance;
    property FileCollision: string read FFileCollision write FFileCollision;
    property NGDSurfaceItem: TgxNGDSurfaceItem read FSurfaceItem
      write SetNewtonSurfaceItem;
  end;

  TgxNGDDynamic = class(TgxNGDBehaviour)
  strict private
    FAABBmin: TgxCoordinates;
    FAABBmax: TgxCoordinates;
    FForce: TgxCoordinates;
    FTorque: TgxCoordinates;
    FCenterOfMass: TgxCoordinates;
    FAutoSleep: Boolean; // Default=True
    FLinearDamping: Single; // default=0.1
    FAngularDamping: TgxCoordinates; // Default=0.1
    FDensity: Single; // Default=1
    FUseGravity: Boolean; // Default=True
    FNullCollisionVolume: Single; // Default=0
    FApplyForceAndTorqueEvent: TApplyForceAndTorqueEvent;
    FSetTransformEvent: TSetTransformEvent;
    FCustomForceAndTorqueEvent: TApplyForceAndTorqueEvent;
    // Read Only
    FVolume: Single;
    FMass: Single;
    FAppliedForce: TgxCoordinates;
    FAppliedTorque: TgxCoordinates;
    FAppliedOmega: TgxCoordinates;
    FAppliedVelocity: TgxCoordinates;
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
      timestep: dFloat; threadIndex: Integer);
    procedure OnSetTransformEvent(const cbody: PNewtonBody;
      const cmatrix: PdFloat; threadIndex: Integer);
    // Callback
    class procedure NewtonApplyForceAndTorque(const body: PNewtonBody;
      timestep: dFloat; threadIndex: Integer); static; cdecl;
    class procedure NewtonSetTransform(const body: PNewtonBody;
      const matrix: PdFloat; threadIndex: Integer); static; cdecl;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure AddImpulse(const veloc, pointposit: TVector4f);
    function GetOmega: TVector4f;
    procedure SetOmega(const Omega: TVector4f);
    function GetVelocity: TVector4f;
    procedure SetVelocity(const Velocity: TVector4f);
    class function FriendlyName: string; override;
    property CustomForceAndTorqueEvent: TApplyForceAndTorqueEvent
      read FCustomForceAndTorqueEvent write FCustomForceAndTorqueEvent;
    property Velocity: TVector4f read GetVelocity write SetVelocity;
    property Omega: TVector4f read GetOmega write SetOmega;
  published
    property Force: TgxCoordinates read FForce write FForce;
    property Torque: TgxCoordinates read FTorque write FTorque;
    property CenterOfMass: TgxCoordinates read FCenterOfMass
      write FCenterOfMass;
    property AutoSleep: Boolean read FAutoSleep write SetAutoSleep default True;
    property LinearDamping: Single read FLinearDamping write SetLinearDamping
      stored StoredLinearDamping;
    property AngularDamping: TgxCoordinates read FAngularDamping
      write FAngularDamping;
    property Density: Single read FDensity write SetDensity
      stored StoredDensity;
    property UseGravity: Boolean read FUseGravity write FUseGravity
      default True;
    property NullCollisionVolume: Single read FNullCollisionVolume
      write FNullCollisionVolume stored StoredNullCollisionVolume;
    // Read Only
    property AppliedOmega: TgxCoordinates read FAppliedOmega;
    property AppliedVelocity: TgxCoordinates read FAppliedVelocity;
    property AppliedForce: TgxCoordinates read FAppliedForce;
    property AppliedTorque: TgxCoordinates read FAppliedTorque;
    property Volume: Single read FVolume;
    property Mass: Single read FMass;
  end;

  TgxNGDStatic = class(TgxNGDBehaviour)
  protected
    procedure Render; override;
  public
    class function FriendlyName: string; override;
  published
  end;

  TgxNGDSurfaceItem = class(TCollectionItem)
  private
    FDisplayName: string;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  published
    property DisplayName;
    property ID;
  end;

  TgxNGDSurfacePair = class(TCollectionItem)
  strict private
    FManager: TgxNGDManager;
    FSurfaceItem1: TgxNGDSurfaceItem;
    FSurfaceItem2: TgxNGDSurfaceItem;
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
      timestep: dFloat; threadIndex: Integer); static; cdecl;
    // Event
    function OnNewtonAABBOverlapEvent(const cmaterial: PNewtonMaterial;
      const cbody0: PNewtonBody; const cbody1: PNewtonBody;
      threadIndex: Integer): Boolean;
    procedure OnNewtonContactsProcessEvent(const ccontact: PNewtonJoint;
      timestep: dFloat; threadIndex: Integer);
  public
    constructor Create(Collection: TCollection); override;
    procedure SetMaterialItems(const item1, item2: TgxNGDSurfaceItem);
    property NGDSurfaceItem1: TgxNGDSurfaceItem read FSurfaceItem1;
    property NGDSurfaceItem2: TgxNGDSurfaceItem read FSurfaceItem2;
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

  TgxNGDJointPivot = class(TPersistent)
  private
    FManager: TgxNGDManager;
    FPivotPoint: TgxCoordinates;
    FOuter: TgxNGDJoint;
  public
    constructor Create(AOwner: TComponent; aOuter: TgxNGDJoint); virtual;
    destructor Destroy; override;
  published
    property PivotPoint: TgxCoordinates read FPivotPoint write FPivotPoint;
  end;

  TgxNGDJointPin = class(TgxNGDJointPivot)
  private
    FPinDirection: TgxCoordinates;
  public
    constructor Create(AOwner: TComponent; aOuter: TgxNGDJoint); override;
    destructor Destroy; override;
  published
    property PinDirection: TgxCoordinates read FPinDirection
      write FPinDirection;
  end;

  TgxNGDJointPin2 = class(TgxNGDJointPin)
  private
    FPinDirection2: TgxCoordinates;
  public
    constructor Create(AOwner: TComponent; aOuter: TgxNGDJoint); override;
    destructor Destroy; override;
  published
    property PinDirection2: TgxCoordinates read FPinDirection2
      write FPinDirection2;
  end;

  TgxNGDJointBallAndSocket = class(TgxNGDJointPivot)
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
    constructor Create(AOwner: TComponent; aOuter: TgxNGDJoint); override;
  published
    property ConeAngle: Single read FConeAngle write SetConeAngle
      stored StoredConeAngle;
    property MinTwistAngle: Single read FMinTwistAngle write SetMinTwistAngle
      stored StoredMinTwistAngle;
    property MaxTwistAngle: Single read FMaxTwistAngle write SetMaxTwistAngle
      stored StoredMaxTwistAngle;
  end;

  TgxNGDJointHinge = class(TgxNGDJointPin)
  private
    FMinAngle: Single; // -90
    FMaxAngle: Single; // 90
    procedure SetMaxAngle(const Value: Single);
    procedure SetMinAngle(const Value: Single);
    function StoredMaxAngle: Boolean;
    function StoredMinAngle: Boolean;
  public
    constructor Create(AOwner: TComponent; aOuter: TgxNGDJoint); override;
  published
    property MinAngle: Single read FMinAngle write SetMinAngle
      stored StoredMinAngle;
    property MaxAngle: Single read FMaxAngle write SetMaxAngle
      stored StoredMaxAngle;
  end;

  TgxNGDJointSlider = class(TgxNGDJointPin)
  private
    FMinDistance: Single; // -10
    FMaxDistance: Single; // 10
    procedure SetMaxDistance(const Value: Single);
    procedure SetMinDistance(const Value: Single);
    function StoredMaxDistance: Boolean;
    function StoredMinDistance: Boolean;
  public
    constructor Create(AOwner: TComponent; aOuter: TgxNGDJoint); override;
  published
    property MinDistance: Single read FMinDistance write SetMinDistance
      stored StoredMinDistance;
    property MaxDistance: Single read FMaxDistance write SetMaxDistance
      stored StoredMaxDistance;
  end;

  TgxNGDJointKinematicController = class(TPersistent)
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

  TgxNGDJoint = class(TCollectionItem)
  private
    // Global
    FManager: TgxNGDManager;
    FParentObject: TgxBaseSceneObject;
    FJointType: TgxNGDJoints;
    FStiffness: Single; // 0.9
    // With Two object
    // Every joint except nj_UpVector and nj_KinematicController
    FChildObject: TgxBaseSceneObject;
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
    FUPVectorDirection: TgxCoordinates;
    FBallAndSocketOptions: TgxNGDJointPivot;
    FHingeOptions: TgxNGDJointPin;
    FSliderOptions: TgxNGDJointPin;
    FCorkscrewOptions: TgxNGDJointPin;
    FUniversalOptions: TgxNGDJointPin2;
    FCustomBallAndSocketOptions: TgxNGDJointBallAndSocket;
    FCustomHingeOptions: TgxNGDJointHinge;
    FCustomSliderOptions: TgxNGDJointSlider;
    FKinematicOptions: TgxNGDJointKinematicController;
    procedure SetJointType(const Value: TgxNGDJoints);
    procedure SetChildObject(const Value: TgxBaseSceneObject);
    procedure SetCollisionState(const Value: Boolean);
    procedure SetParentObject(const Value: TgxBaseSceneObject);
    procedure SetStiffness(const Value: Single);
    procedure Render;
    function StoredStiffness: Boolean;
    procedure DestroyNewtonData;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure KinematicControllerPick(pickpoint: TVector4f;
      PickedActions: TgxNGDPickedActions);
  published
    property BallAndSocketOptions: TgxNGDJointPivot read FBallAndSocketOptions
      write FBallAndSocketOptions;
    property HingeOptions: TgxNGDJointPin read FHingeOptions
      write FHingeOptions;
    property SliderOptions: TgxNGDJointPin read FSliderOptions
      write FSliderOptions;
    property CorkscrewOptions: TgxNGDJointPin read FCorkscrewOptions
      write FCorkscrewOptions;
    property UniversalOptions: TgxNGDJointPin2 read FUniversalOptions
      write FUniversalOptions;
    property CustomBallAndSocketOptions: TgxNGDJointBallAndSocket
      read FCustomBallAndSocketOptions write FCustomBallAndSocketOptions;
    property CustomHingeOptions: TgxNGDJointHinge read FCustomHingeOptions
      write FCustomHingeOptions;
    property CustomSliderOptions: TgxNGDJointSlider read FCustomSliderOptions
      write FCustomSliderOptions;
    property KinematicControllerOptions: TgxNGDJointKinematicController
      read FKinematicOptions write FKinematicOptions;
    property JointType: TgxNGDJoints read FJointType write SetJointType;
    property ParentObject: TgxBaseSceneObject read FParentObject
      write SetParentObject;
    property ChildObject: TgxBaseSceneObject read FChildObject
      write SetChildObject;
    property CollisionState: Boolean read FCollisionState
      write SetCollisionState default False;
    property Stiffness: Single read FStiffness write SetStiffness
      stored StoredStiffness;
    property UPVectorDirection: TgxCoordinates read FUPVectorDirection
      write FUPVectorDirection;
  end;

// Global function 
function GetNGDStatic(Obj: TgxBaseSceneObject): TgxNGDStatic;
function GetOrCreateNGDStatic(Obj: TgxBaseSceneObject): TgxNGDStatic;
function GetNGDDynamic(Obj: TgxBaseSceneObject): TgxNGDDynamic;
function GetOrCreateNGDDynamic(Obj: TgxBaseSceneObject): TgxNGDDynamic;
function GetBodyFromGLXceneObject(Obj: TgxBaseSceneObject): PNewtonBody;

// =======================================================================
implementation
// =======================================================================

const
  epsilon = 0.0000001; // 1E-07

function GetNGDStatic(Obj: TgxBaseSceneObject): TgxNGDStatic;
begin
  Result := TgxNGDStatic(Obj.Behaviours.GetByClass(TgxNGDStatic));
end;

function GetOrCreateNGDStatic(Obj: TgxBaseSceneObject): TgxNGDStatic;
begin
  Result := TgxNGDStatic(Obj.GetOrCreateBehaviour(TgxNGDStatic));
end;

function GetNGDDynamic(Obj: TgxBaseSceneObject): TgxNGDDynamic;
begin
  Result := TgxNGDDynamic(Obj.Behaviours.GetByClass(TgxNGDDynamic));
end;

function GetOrCreateNGDDynamic(Obj: TgxBaseSceneObject): TgxNGDDynamic;
begin
  Result := TgxNGDDynamic(Obj.GetOrCreateBehaviour(TgxNGDDynamic));
end;

function GetBodyFromGLXceneObject(Obj: TgxBaseSceneObject): PNewtonBody;
var
  Behaviour: TgxNGDBehaviour;
begin
  Behaviour := TgxNGDBehaviour(Obj.Behaviours.GetByClass(TgxNGDBehaviour));
  Assert(Behaviour <> nil,
    'NGD Behaviour (static or dynamic) is missing for this object');
  Result := Behaviour.FNewtonBody;
end;

// ------------------------------------------------------------------
// -------- TgxNGDDebugOption
// ------------------------------------------------------------------
constructor TgxNGDDebugOption.Create(AOwner: TComponent);
begin
  FManager := AOwner as TgxNGDManager;
  with FManager do
  begin
    FGeomColorDyn := TgxColor.CreateInitialized(self, clrGreen, NotifyChange);
    FGeomColorStat := TgxColor.CreateInitialized(self, clrRed, NotifyChange);
    FAABBColor := TgxColor.CreateInitialized(self, clrYellow, NotifyChange);
    FAABBColorSleep := TgxColor.CreateInitialized(self, clrOrange,
      NotifyChange);
    FCenterOfMassColor := TgxColor.CreateInitialized(self, clrPurple,
      NotifyChange);
    FContactColor := TgxColor.CreateInitialized(self, clrWhite, NotifyChange);
    FJointAxisColor := TgxColor.CreateInitialized(self, clrBlue, NotifyChange);
    FJointPivotColor := TgxColor.CreateInitialized(self, clrAquamarine,
      NotifyChange);

    FForceColor := TgxColor.CreateInitialized(self, clrBlack, NotifyChange);
    FAppliedForceColor := TgxColor.CreateInitialized(self, clrSilver,
      NotifyChange);
    FAppliedVelocityColor := TgxColor.CreateInitialized(self, clrLime,
      NotifyChange);

    FCustomColor := TgxColor.CreateInitialized(self, clrAqua, NotifyChange);
  end;
  FDotAxisSize := 1;
  FManagerDebugs := [];

  FManager := AOwner as TgxNGDManager;
end;

destructor TgxNGDDebugOption.Destroy;
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

procedure TgxNGDDebugOption.SetDotAxisSize(const Value: Single);
begin
  FDotAxisSize := Value;
  FManager.NotifyChange(self);
end;

procedure TgxNGDDebugOption.SetManagerDebugs(const Value
    : TgxNGDManagerDebugs);
begin
  FManagerDebugs := Value;
  FManager.NotifyChange(self);
end;

function TgxNGDDebugOption.StoredDotAxis: Boolean;
begin
  Result := not SameValue(FDotAxisSize, 1, epsilon);
end;

// ------------------------------------------------------------------
{ TgxNGDManager }
// ------------------------------------------------------------------
procedure TgxNGDManager.AddNode(const Value: TVector4f);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(Value);

    with (FGLLines.Nodes.Last as TgxLinesNode) do
      Color := FCurrentColor;
  end;
end;

procedure TgxNGDManager.AddNode(const coords: TgxCustomCoordinates);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(coords);
    (FGLLines.Nodes.Last as TgxLinesNode).Color := FCurrentColor;
  end;
end;

procedure TgxNGDManager.AddNode(const X, Y, Z: Single);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(X, Y, Z);
    (FGLLines.Nodes.Last as TgxLinesNode).Color := FCurrentColor;
  end;
end;

procedure TgxNGDManager.AddNode(const Value: TAffineVector);
begin
  if Assigned(FGLLines) then
  begin
    FGLLines.Nodes.AddNode(Value);
    (FGLLines.Nodes.Last as TgxLinesNode).Color := FCurrentColor;
  end;
end;

constructor TgxNGDManager.Create(AOwner: TComponent);
var
  minworld, maxworld: TVector4f;
begin
  inherited;
  FNGDBehaviours := TgxNGDBehaviourList.Create;
  FVisible := True;
  FVisibleAtRunTime := False;
  FSolverModel := smExact;
  FFrictionModel := fmExact;
  FMinimumFrameRate := 60;
  FWorldSizeMin := TgxCoordinates.CreateInitialized(self,
    VectorMake(-100, -100, -100, 0), csPoint);
  FWorldSizeMax := TgxCoordinates.CreateInitialized(self,
    VectorMake(100, 100, 100, 0), csPoint);

  // Using Events because we need to call API Function when
  // theses TgxCoordinates change.
  FWorldSizeMin.OnNotifyChange := NotifyWorldSizeChange;
  FWorldSizeMax.OnNotifyChange := NotifyWorldSizeChange;

  FThreadCount := 1;
  FGravity := TgxCoordinates3.CreateInitialized(self,
    VectorMake(0, -9.81, 0, 0), csVector);

  FNewtonWorld := NewtonCreate(nil, nil);
  FDllVersion := NewtonWorldGetVersion(FNewtonWorld);

  // This is to prevent body out the world at startTime
  minworld := VectorMake(-1E50, -1E50, -1E50);
  maxworld := VectorMake(1E50, 1E50, 1E50);
  NewtonSetWorldSize(FNewtonWorld, @minworld, @maxworld);

  NewtonWorldSetUserData(FNewtonWorld, self);

  FNewtonSurfaceItem := TCollection.Create(TgxNGDSurfaceItem);
  FNewtonSurfacePair := TOwnedCollection.Create(self, TgxNGDSurfacePair);
  FNewtonJointGroup := TOwnedCollection.Create(self, TgxNGDJoint);

  FNewtonDebugOption := TgxNGDDebugOption.Create(self);

  RegisterManager(self);

end;

destructor TgxNGDManager.Destroy;
begin
  // Destroy joint before body.
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
  FreeAndNil(FNewtonDebugOption);

  NewtonDestroyAllBodies(FNewtonWorld);
  NewtonMaterialDestroyAllGroupID(FNewtonWorld);
  NewtonDestroy(FNewtonWorld);
  FNewtonWorld := nil;

  DeregisterManager(self);
  inherited;
end;

procedure TgxNGDManager.Loaded;
begin
  inherited;
  NotifyWorldSizeChange(self);
  RebuildAllJoint(self);
end;

function TgxNGDManager.GetBodyCount: Integer;
begin
  if (csDesigning in ComponentState) then
    Result := FNGDBehaviours.Count
  else
    Result := NewtonWorldGetBodyCount(FNewtonWorld);
end;

function TgxNGDManager.GetConstraintCount: Integer;
begin
  if (csDesigning in ComponentState) then
    Result := FNewtonJointGroup.Count
  else
    // Constraint is the number of joint
    Result := NewtonWorldGetConstraintCount(FNewtonWorld);
end;

procedure TgxNGDManager.NotifyChange(Sender: TObject);
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

  if mdShowJoint in FNewtonDebugOption.NGDManagerDebugs then
    for I := 0 to NewtonJoint.Count - 1 do //
      (NewtonJoint.Items[I] as TgxNGDJoint).Render;

end;

procedure TgxNGDManager.SetFrictionModel(const Value: TgxNGDFrictionModels);
begin
  FFrictionModel := Value;
  if not(csDesigning in ComponentState) then
    NewtonSetFrictionModel(FNewtonWorld, Ord(FFrictionModel));
end;

procedure TgxNGDManager.SetGXLines(const Value: TgxLines);
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

procedure TgxNGDManager.SetMinimumFrameRate(const Value: Integer);
begin
  if (Value >= 60) and (Value <= 1000) then
    FMinimumFrameRate := Value;
  if not(csDesigning in ComponentState) then
    NewtonSetMinimumFrameRate(FNewtonWorld, FMinimumFrameRate);
end;

procedure TgxNGDManager.SetSolverModel(const Value: TgxNGDSolverModels);
begin
  FSolverModel := Value;
  if not(csDesigning in ComponentState) then
    NewtonSetSolverModel(FNewtonWorld, Ord(FSolverModel));
end;

procedure TgxNGDManager.SetThreadCount(const Value: Integer);
begin
  if Value > 0 then
    FThreadCount := Value;
  NewtonSetThreadsCount(FNewtonWorld, FThreadCount);
  FThreadCount := NewtonGetThreadsCount(FNewtonWorld);
end;

procedure TgxNGDManager.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  if (csDesigning in ComponentState) then
    NotifyChange(self);
end;

procedure TgxNGDManager.SetVisibleAtRunTime(const Value: Boolean);
begin
  FVisibleAtRunTime := Value;
  if (csDesigning in ComponentState) then
    NotifyChange(self);
end;

procedure TgxNGDManager.NotifyWorldSizeChange(Sender: TObject);
begin
  if not(csDesigning in ComponentState) then
    NewtonSetWorldSize(FNewtonWorld, @FWorldSizeMin.AsVector,
      @FWorldSizeMax.AsVector);
end;

procedure TgxNGDManager.RebuildAllJoint(Sender: TObject);

  procedure BuildBallAndSocket(Joint: TgxNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateBall(FNewtonWorld,
          @(FBallAndSocketOptions.FPivotPoint.AsVector),
          GetBodyFromGLXceneObject(FChildObject),
          GetBodyFromGLXceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildHinge(Joint: TgxNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateHinge(FNewtonWorld,
          @(FHingeOptions.FPivotPoint.AsVector),
          @(FHingeOptions.FPinDirection.AsVector),
          GetBodyFromGLXceneObject(FChildObject),
          GetBodyFromGLXceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildSlider(Joint: TgxNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateSlider(FNewtonWorld,
          @(FSliderOptions.FPivotPoint.AsVector),
          @(FSliderOptions.FPinDirection.AsVector),
          GetBodyFromGLXceneObject(FChildObject),
          GetBodyFromGLXceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildCorkscrew(Joint: TgxNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateCorkscrew(FNewtonWorld,
          @(FCorkscrewOptions.FPivotPoint.AsVector),
          @(FCorkscrewOptions.FPinDirection.AsVector),
          GetBodyFromGLXceneObject(FChildObject),
          GetBodyFromGLXceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildUniversal(Joint: TgxNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateUniversal(FNewtonWorld,
          @(FUniversalOptions.FPivotPoint.AsVector),
          @(FUniversalOptions.FPinDirection.AsVector),
          @(FUniversalOptions.FPinDirection2.AsVector),
          GetBodyFromGLXceneObject(FChildObject),
          GetBodyFromGLXceneObject(FParentObject));
        NewtonJointSetCollisionState(FNewtonJoint, Ord(FCollisionState));
        NewtonJointSetStiffness(FNewtonJoint, FStiffness);
      end;
  end;

  procedure BuildCustomBallAndSocket(Joint: TgxNGDJoint);
  var
    pinAndPivot: TMatrix4f;
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

  procedure BuildCustomHinge(Joint: TgxNGDJoint);
  var
    pinAndPivot: TMatrix4f;
    bso: TgxBaseSceneObject;
  begin
    { Newton wait from FPinAndPivotMatrix a structure like that:
      First row: the pin direction
      Second and third rows are set to create an orthogonal matrix
      Fourth: The pivot position

      In glscene, the GLBaseSceneObjects direction is the third row,
      because the first row is the right vector (second row is up vector). }
    with Joint do
      if Assigned(FParentObject) and Assigned(FChildObject) then
      begin
        bso := TgxBaseSceneObject.Create(FManager);
        bso.AbsolutePosition := FCustomHingeOptions.FPivotPoint.AsVector;
        bso.AbsoluteDirection := FCustomHingeOptions.FPinDirection.AsVector;
        pinAndPivot := bso.AbsoluteMatrix;
        pinAndPivot.X := bso.AbsoluteMatrix.Z;
        pinAndPivot.Z := bso.AbsoluteMatrix.X;
        bso.Free;

        (* // from dJointLibrary.dll
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

  procedure BuildCustomSlider(Joint: TgxNGDJoint);
  var
    pinAndPivot: TMatrix4f;
    bso: TgxBaseSceneObject;

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

        bso := TgxBaseSceneObject.Create(FManager);
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

  procedure BuildUpVector(Joint: TgxNGDJoint);
  begin
    with Joint do
      if Assigned(FParentObject) then
      begin
        FNewtonJoint := NewtonConstraintCreateUpVector(FNewtonWorld,
          @FUPVectorDirection.AsVector,
          GetBodyFromGLXceneObject(FParentObject));
      end;
  end;

  procedure BuildKinematicController(Joint: TgxNGDJoint);
  begin
    // do nothing
  end;

  procedure BuildOneJoint(Joint: TgxNGDJoint);
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
    if Sender is TgxNGDManager then
      for I := 0 to NewtonJoint.Count - 1 do
        BuildOneJoint(NewtonJoint.Items[I] as TgxNGDJoint);

    if (Sender is TgxNGDJoint) then
      BuildOneJoint((Sender as TgxNGDJoint));

    if Sender is TgxCoordinates then
      BuildOneJoint(((Sender as TgxCoordinates).Owner as TgxNGDJoint));

    NotifyChange(self);
  end;

end;

procedure TgxNGDManager.RebuildAllMaterial;

  procedure BuildMaterialPair;
  var
    I, ID0, ID1: Integer;
  begin
    for I := 0 to FNewtonSurfacePair.Count - 1 do
      with (FNewtonSurfacePair.Items[I] as TgxNGDSurfacePair) do
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
            FNewtonSurfacePair.Items[I], @TgxNGDSurfacePair.NewtonAABBOverlap,
            @TgxNGDSurfacePair.NewtonContactsProcess);
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
      maxID := MaxInteger((FNewtonSurfaceItem.Items[I] as TgxNGDSurfaceItem)
        .ID, maxID);
    for I := 0 to maxID - 1 do
      NewtonMaterialCreateGroupID(FNewtonWorld);
    // assign matID to bodies
    for I := 0 to FNGDBehaviours.Count - 1 do
      with FNGDBehaviours[I] do
        if Assigned(FSurfaceItem) then
          NewtonBodySetMaterialGroupID(FNewtonBody, FSurfaceItem.ID)
        else
          NewtonBodySetMaterialGroupID(FNewtonBody, 0);
    // Set values to newton material pair :callback userdata friction...
    BuildMaterialPair;
  end;
end;

procedure TgxNGDManager.Step(deltatime: Single);
begin
  if not(csDesigning in ComponentState) then
    NewtonUpdate(FNewtonWorld, deltatime);

  NotifyChange(self);
end;

// ---------------------------
// TgxNGDBehaviour
// ---------------------------

constructor TgxNGDBehaviour.Create(AOwner: TXCollection);
begin
  inherited;
  FInitialized := False;
  FOwnerBaseSceneObject := OwnerBaseSceneObject;

  FContinuousCollisionMode := False;
  FNewtonBody := nil;
  FCollision := nil;

  FNewtonCollisions := nc_Primitive;

  FCollisionIteratorEvent := OnCollisionIteratorEvent;

  FTreeCollisionOptimize := True;
  FConvexCollisionTolerance := 0.01;
  FFileCollision := '';
  name := 'NGD Static';
end;

destructor TgxNGDBehaviour.Destroy;
begin
  if Assigned(FManager) then
    Manager := nil; // This will call finalize
  inherited;
end;

procedure TgxNGDBehaviour.Finalize;
var
  I: Integer;
begin
  FInitialized := False;

  if Assigned(FManager) then
  begin

    if Assigned(FManager.NewtonJoint) then
      for I := FManager.NewtonJoint.Count - 1 downto 0 do
      begin
        if ((FManager.NewtonJoint.Items[I] as TgxNGDJoint)
          .ParentObject = FOwnerBaseSceneObject) or
          ((FManager.NewtonJoint.Items[I] as TgxNGDJoint)
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

function TgxNGDBehaviour.GetBBoxCollision: PNewtonCollision;
var
  vc: array [0 .. 7] of TVector4f;
  I: Integer;
begin
  for I := 0 to 8 - 1 do
    vc[I] := AABBToBB(FOwnerBaseSceneObject.AxisAlignedBoundingBoxEx).BBox[I];
  Result := NewtonCreateConvexHull(FManager.FNewtonWorld, 8, @vc[0],
    SizeOf(TVector4f), 0.01, 0, nil);
end;

function TgxNGDBehaviour.GetBSphereCollision: PNewtonCollision;
var
  boundingSphere: TBSphere;
  collisionOffsetMatrix: TMatrix4f;
begin
  AABBToBSphere(FOwnerBaseSceneObject.AxisAlignedBoundingBoxEx, boundingSphere);

  collisionOffsetMatrix := IdentityHmgMatrix;
  collisionOffsetMatrix.W := VectorMake(boundingSphere.Center, 1);
  Result := NewtonCreateSphere(FManager.FNewtonWorld, boundingSphere.Radius,
    boundingSphere.Radius, boundingSphere.Radius, 0, @collisionOffsetMatrix);
end;

function TgxNGDBehaviour.GetConvexCollision: PNewtonCollision;
var
  I, J: Integer;
  vertexArray: array of TVertex;
begin
  if FOwnerBaseSceneObject is TgxBaseMesh then
  begin
    with (FOwnerBaseSceneObject as TgxBaseMesh) do
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

function TgxNGDBehaviour.GetHeightFieldCollision: PNewtonCollision;
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

function TgxNGDBehaviour.GetMeshCollision: PNewtonCollision;
var
  collisionArray: array of PNewtonCollision;
  I, J: Integer;
  vertexArray: array of TVertex;
begin
  if FOwnerBaseSceneObject is TgxBaseMesh then
  begin
    with (FOwnerBaseSceneObject as TgxBaseMesh) do
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

function TgxNGDBehaviour.GetNewtonBodyMatrix: TMatrix4f;
begin
  if Assigned(FManager) then
    NewtonBodyGetmatrix(FNewtonBody, @FNewtonBodyMatrix);
  Result := FNewtonBodyMatrix;
end;

function TgxNGDBehaviour.GetNewtonBodyAABB: TAABB;
begin
  if Assigned(FManager) then
    NewtonBodyGetAABB(FNewtonBody, @(Result.min), @(Result.max));
end;

function TgxNGDBehaviour.GetNGDFileCollision: PNewtonCollision;
var
  MyFile: TFileStream;
begin
  if FileExists(FFileCollision) then
  begin
    MyFile := TFileStream.Create(FFileCollision, fmOpenRead);
    Result := NewtonCreateCollisionFromSerialization(FManager.FNewtonWorld,
      @TgxNGDBehaviour.NewtonDeserialize, Pointer(MyFile));
    MyFile.Free;
  end
  else
    Result := NewtonCreateNull(FManager.FNewtonWorld);
end;

function TgxNGDBehaviour.GetNullCollision: PNewtonCollision;
begin
  Result := NewtonCreateNull(FManager.FNewtonWorld);
end;

function TgxNGDBehaviour.GetPrimitiveCollision: PNewtonCollision;
var
  collisionOffsetMatrix: TMatrix4f; // For cone capsule and cylinder
begin
  collisionOffsetMatrix := IdentityHmgMatrix;

  if (FOwnerBaseSceneObject is TgxCube) then
  begin
    with (FOwnerBaseSceneObject as TgxCube) do
      Result := NewtonCreateBox(FManager.FNewtonWorld, CubeWidth, CubeHeight,
        CubeDepth, 0, @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TgxSphere) then
  begin
    with (FOwnerBaseSceneObject as TgxSphere) do
      Result := NewtonCreateSphere(FManager.FNewtonWorld, Radius, Radius,
        Radius, 0, @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TgxCone) then
  begin
    collisionOffsetMatrix := MatrixMultiply(collisionOffsetMatrix,
      CreateRotationMatrixZ(Pi / 2.0));
    with (FOwnerBaseSceneObject as TgxCone) do
      Result := NewtonCreateCone(FManager.FNewtonWorld, BottomRadius, Height, 0,
        @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TgxCapsule) then
  begin
    collisionOffsetMatrix := MatrixMultiply(collisionOffsetMatrix,
      CreateRotationMatrixY(Pi / 2.0));
    with (FOwnerBaseSceneObject as TgxCapsule) do
      // Use Cylinder shape for buoyancy
      Result := NewtonCreateCapsule(FManager.FNewtonWorld, Radius,
        Height + 2 * Radius, 0, @collisionOffsetMatrix);
  end

  else if (FOwnerBaseSceneObject is TgxCylinder) then
  begin
    collisionOffsetMatrix := MatrixMultiply(collisionOffsetMatrix,
      CreateRotationMatrixZ(Pi / 2.0));
    with (FOwnerBaseSceneObject as TgxCylinder) do
      Result := NewtonCreateCylinder(FManager.FNewtonWorld, BottomRadius,
        Height, 0, @collisionOffsetMatrix);
  end
  else
    Result := GetNullCollision;
end;

function TgxNGDBehaviour.GetTreeCollision: PNewtonCollision;
var
  meshIndex, triangleIndex: Integer;
  triangleList: TgxAffineVectorList;
  v: array [0 .. 2] of TAffineVector;
begin

  if FOwnerBaseSceneObject is TgxBaseMesh then
  begin
    with (FOwnerBaseSceneObject as TgxBaseMesh) do
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

procedure TgxNGDBehaviour.Initialize;
begin
  FInitialized := True;
  if Assigned(FManager) then
  begin
    // Create NewtonBody with null collision
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

procedure TgxNGDBehaviour.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TgxNGDManager, FManagerName);
    if Assigned(mng) then
      Manager := TgxNGDManager(mng);
    FManagerName := '';
  end;

  if Assigned(FManager) then
  begin
    SetContinuousCollisionMode(FContinuousCollisionMode);
  end;
end;

class procedure TgxNGDBehaviour.NewtonCollisionIterator(const userData: Pointer;
  vertexCount: Integer; const faceArray: PdFloat; faceId: Integer)cdecl;
begin
  TgxNGDBehaviour(userData).FCollisionIteratorEvent(userData, vertexCount,
    faceArray, faceId);
end;

// Serializes are called by NGDBehaviour to save and load collision in file
// It's better to save/load big collisions [over 50000 polygones] to reduce
// loading time
class procedure TgxNGDBehaviour.NewtonDeserialize(serializeHandle,
  buffer: Pointer; size: Cardinal)cdecl;
begin
  TFileStream(serializeHandle).read(buffer^, size);
end;

class procedure TgxNGDBehaviour.NewtonSerialize(serializeHandle: Pointer;
  const buffer: Pointer; size: Cardinal)cdecl;
begin
  TFileStream(serializeHandle).write(buffer^, size);
end;

procedure TgxNGDBehaviour.OnCollisionIteratorEvent(const userData: Pointer;
  vertexCount: Integer; const cfaceArray: PdFloat; faceId: Integer);
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

procedure TgxNGDBehaviour.Reinitialize;
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

procedure TgxNGDBehaviour.Render;
var
  M: TMatrix4f;
begin
  // Rebuild collision in design time
  if (csDesigning in FOwnerBaseSceneObject.ComponentState) then
    Reinitialize;

  if self is TgxNGDDynamic then
    FManager.FCurrentColor := FManager.DebugOption.GeomColorDyn
  else
    FManager.FCurrentColor := FManager.DebugOption.GeomColorStat;
  M := FOwnerBaseSceneObject.AbsoluteMatrix;
  if mdShowGeometry in FManager.DebugOption.NGDManagerDebugs then
    NewtonCollisionForEachPolygonDo(FCollision, @M,
      @TgxNGDBehaviour.NewtonCollisionIterator, self);
end;

// In this procedure, we assign collision to body
// [Because when initialised, the collision for body is type NULL]
procedure TgxNGDBehaviour.UpdCollision;
var
  collisionInfoRecord: TNewtonCollisionInfoRecord;
begin
  case FNewtonCollisions of
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

procedure TgxNGDBehaviour.SetContinuousCollisionMode(const Value: Boolean);
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

procedure TgxNGDBehaviour.SetHeightFieldOptions(const Value: TgxNGDHeightField);
begin
  FHeightFieldOptions := Value;
  Reinitialize;
end;

procedure TgxNGDBehaviour.SetManager(Value: TgxNGDManager);
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

procedure TgxNGDBehaviour.SetNewtonBodyMatrix(const Value: TMatrix4f);
begin
  FNewtonBodyMatrix := Value;
  if Assigned(FManager) then
    NewtonBodySetmatrix(FNewtonBody, @FNewtonBodyMatrix);
end;

procedure TgxNGDBehaviour.SetNewtonCollisions(const Value: TgxNGDCollisions);
begin
  FNewtonCollisions := Value;
  if Assigned(FManager) then
    UpdCollision;
end;

procedure TgxNGDBehaviour.SetNewtonSurfaceItem(const Value: TgxNGDSurfaceItem);
begin
  FSurfaceItem := Value;
  FManager.RebuildAllMaterial;
end;

function TgxNGDBehaviour.StoredTolerance: Boolean;
begin
  Result := not SameValue(FConvexCollisionTolerance, 0.01, epsilon);
end;

class function TgxNGDBehaviour.UniqueItem: Boolean;
begin
  Result := True;
end;

procedure TgxNGDBehaviour.ReadFromFiler(reader: TReader);
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
    read(FNewtonCollisions, SizeOf(TgxNGDCollisions));
    FTreeCollisionOptimize := ReadBoolean;
    if version <= 0 then
      FConvexCollisionTolerance := ReadSingle
    else
      FConvexCollisionTolerance := ReadFloat;
    FFileCollision := ReadString;
  end;
end;

procedure TgxNGDBehaviour.WriteToFiler(writer: TWriter);
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
    write(FNewtonCollisions, SizeOf(TgxNGDCollisions));
    WriteBoolean(FTreeCollisionOptimize);
    WriteFloat(FConvexCollisionTolerance);
    WriteString(FFileCollision);
  end;
end;

procedure TgxNGDBehaviour.Serialize(filename: string);
var
  MyFile: TFileStream;
begin
  MyFile := TFileStream.Create(filename, fmCreate or fmOpenReadWrite);

  NewtonCollisionSerialize(FManager.FNewtonWorld, FCollision,
    @TgxNGDBehaviour.NewtonSerialize, Pointer(MyFile));

  MyFile.Free;
end;

procedure TgxNGDBehaviour.DeSerialize(filename: string);
var
  MyFile: TFileStream;
  collisionInfoRecord: TNewtonCollisionInfoRecord;
begin
  MyFile := TFileStream.Create(filename, fmOpenRead);

  FCollision := NewtonCreateCollisionFromSerialization(FManager.FNewtonWorld,
    @TgxNGDBehaviour.NewtonDeserialize, Pointer(MyFile));

  // SetCollision;
  NewtonBodySetCollision(FNewtonBody, FCollision);

  // Release collision
  NewtonCollisionGetInfo(FCollision, @collisionInfoRecord);
  if collisionInfoRecord.m_collisionType > 2 then
    NewtonReleaseCollision(FManager.FNewtonWorld, FCollision);
  MyFile.Free;
end;

// -------------------------------------
// TgxNGDDynamic
// -------------------------------------

procedure TgxNGDDynamic.AddImpulse(const veloc, pointposit: TVector4f);
begin
  if Assigned(FNewtonBody) then
    NewtonBodyAddImpulse(FNewtonBody, @veloc, @pointposit);
end;

constructor TgxNGDDynamic.Create(AOwner: TXCollection);
begin
  inherited;
  FAutoSleep := True;
  FLinearDamping := 0.1;
  FAngularDamping := TgxCoordinates.CreateInitialized(self,
    VectorMake(0.1, 0.1, 0.1, 0), csPoint);
  FAngularDamping.OnNotifyChange := NotifyAngularDampingChange;
  FDensity := 1;
  FVolume := 1;
  FForce := TgxCoordinates.CreateInitialized(self, NullHmgVector, csVector);
  FTorque := TgxCoordinates.CreateInitialized(self, NullHmgVector, csVector);
  FCenterOfMass := TgxCoordinates.CreateInitialized(self,
    NullHmgVector, csPoint);
  FCenterOfMass.OnNotifyChange := NotifyCenterOfMassChange;
  FAABBmin := TgxCoordinates.CreateInitialized(self, NullHmgVector, csPoint);
  FAABBmax := TgxCoordinates.CreateInitialized(self, NullHmgVector, csPoint);
  FAppliedOmega := TgxCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedVelocity := TgxCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedForce := TgxCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FAppliedTorque := TgxCoordinates.CreateInitialized(self, NullHmgVector,
    csVector);
  FUseGravity := True;
  FNullCollisionVolume := 0;

  FApplyForceAndTorqueEvent := OnApplyForceAndTorqueEvent;
  FSetTransformEvent := OnSetTransformEvent;
  name := 'NGD Dynamic'
end;

destructor TgxNGDDynamic.Destroy;
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

procedure TgxNGDDynamic.Finalize;
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

class function TgxNGDDynamic.FriendlyName: string;
begin
  Result := 'NGD Dynamic';
end;

procedure TgxNGDDynamic.Initialize;
begin
  inherited;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
    begin
      // Set Density, Mass and inertie matrix
      SetDensity(FDensity);

      // Set Callback
      NewtonBodySetForceAndTorqueCallback(FNewtonBody,
        @TgxNGDDynamic.NewtonApplyForceAndTorque);
      NewtonBodySetTransformCallback(FNewtonBody,
        @TgxNGDDynamic.NewtonSetTransform);
    end;
end;

procedure TgxNGDDynamic.Render;

  procedure DrawAABB(min, max: TgxCoordinates3);
  begin

    {
      //    H________G
      //   /.       /|
      //  / .      / |
      // D__._____C  |
      // |  .     |  |
      // | E.-----|--F
      // | .      | /
      // |.       |/
      // A________B
    }
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
    pos, nor: TVector4f;
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

  function GetAbsCom(): TVector4f;
  var
    M: TMatrix4f;
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
    pos: TVector4f;
    nor: TVector4f;
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
    com: TVector4f;
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

procedure TgxNGDDynamic.SetAutoSleep(const Value: Boolean);
begin
  FAutoSleep := Value;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
      NewtonBodySetAutoSleep(FNewtonBody, Ord(FAutoSleep));
end;

procedure TgxNGDDynamic.SetDensity(const Value: Single);
var
  inertia: TVector4f;
  origin: TVector4f;
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

procedure TgxNGDDynamic.SetLinearDamping(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FLinearDamping := Value;
  if not(csDesigning in FOwnerBaseSceneObject.ComponentState) then
    if Assigned(FManager) then
      NewtonBodySetLinearDamping(FNewtonBody, FLinearDamping);
end;

function TgxNGDDynamic.GetOmega: TVector4f;
begin
  NewtonBodyGetOmega(FNewtonBody, @Result);
end;

procedure TgxNGDDynamic.SetOmega(const Omega: TVector4f);
begin
  NewtonBodySetOmega(FNewtonBody, @Omega);
end;

function TgxNGDDynamic.GetVelocity: TVector4f;
begin
  NewtonBodyGetVelocity(FNewtonBody, @Result);
end;

procedure TgxNGDDynamic.SetVelocity(const Velocity: TVector4f);
begin
  NewtonBodySetVelocity(FNewtonBody, @Velocity);
end;

function TgxNGDDynamic.StoredDensity: Boolean;
begin
  Result := not SameValue(FDensity, 1, epsilon);
end;

function TgxNGDDynamic.StoredLinearDamping: Boolean;
begin
  Result := not SameValue(FLinearDamping, 0.1, epsilon);
end;

function TgxNGDDynamic.StoredNullCollisionVolume: Boolean;
begin
  Result := not SameValue(FNullCollisionVolume, 0, epsilon);
end;

// WriteToFiler
//
procedure TgxNGDDynamic.WriteToFiler(writer: TWriter);
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

// ReadFromFiler
//
procedure TgxNGDDynamic.ReadFromFiler(reader: TReader);
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

procedure TgxNGDDynamic.Loaded;
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

class procedure TgxNGDDynamic.NewtonApplyForceAndTorque(const body: PNewtonBody;
  timestep: dFloat; threadIndex: Integer); cdecl;
begin
  TgxNGDDynamic(NewtonBodyGetUserData(body)).FApplyForceAndTorqueEvent(body,
    timestep, threadIndex);
end;

class procedure TgxNGDDynamic.NewtonSetTransform(const body: PNewtonBody;
  const matrix: PdFloat; threadIndex: Integer); cdecl;
begin
  TgxNGDDynamic(NewtonBodyGetUserData(body)).FSetTransformEvent(body, matrix,
    threadIndex);
end;

procedure TgxNGDDynamic.NotifyAngularDampingChange(Sender: TObject);
begin
  FAngularDamping.OnNotifyChange := nil;
  if (FAngularDamping.X >= 0) and (FAngularDamping.X <= 1) and
    (FAngularDamping.Y >= 0) and (FAngularDamping.Y <= 1) and
    (FAngularDamping.Z >= 0) and (FAngularDamping.Z <= 1) then
    if Assigned(FManager) then
      NewtonBodySetAngularDamping(FNewtonBody, @(FAngularDamping.AsVector));
  FAngularDamping.OnNotifyChange := NotifyAngularDampingChange;
end;

procedure TgxNGDDynamic.NotifyCenterOfMassChange(Sender: TObject);
begin
  FCenterOfMass.OnNotifyChange := nil;
  if Assigned(FManager) then
    NewtonBodySetCentreOfMass(FNewtonBody, @(FCenterOfMass.AsVector));
  FCenterOfMass.OnNotifyChange := NotifyCenterOfMassChange;
end;

procedure TgxNGDDynamic.OnApplyForceAndTorqueEvent(const cbody: PNewtonBody;
  timestep: dFloat; threadIndex: Integer);
var
  worldGravity: TVector4f;
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

procedure TgxNGDDynamic.OnSetTransformEvent(const cbody: PNewtonBody;
  const cmatrix: PdFloat; threadIndex: Integer);
var
  epsi: Single;
begin
  // The Newton API does not support scale [scale modifie value in matrix],
  // so this line reset scale of the GLXceneObject to (1,1,1)
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
      FOwnerBaseSceneObject.AbsoluteMatrix := PMatrix4f(cmatrix)^;
end;

// ------------------------------------------------------------------
// TgxNGDStatic 
// ------------------------------------------------------------------
procedure TgxNGDStatic.Render;
begin
  inherited;
  // Move/Rotate NewtonObject if matrix are not equal in run time.
  if not MatrixEquals(NewtonBodyMatrix, FOwnerBaseSceneObject.AbsoluteMatrix)
  then
    SetNewtonBodyMatrix(FOwnerBaseSceneObject.AbsoluteMatrix);

end;

class function TgxNGDStatic.FriendlyName: string;
begin
  Result := 'NGD Static';
end;

// ------------------------
// TgxNGDSurfaceItem
// ------------------------
function TgxNGDSurfaceItem.GetDisplayName: string;
begin
  if FDisplayName = '' then
    FDisplayName := 'Iron';
  Result := FDisplayName;
end;

procedure TgxNGDSurfaceItem.SetDisplayName(const Value: string);
begin
  inherited;
  FDisplayName := Value;
end;

// =------------------------------------
// TgxNGDSurfacePair
// =------------------------------------

constructor TgxNGDSurfacePair.Create(Collection: TCollection);
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
  FManager := TgxNGDManager(Collection.Owner);
  FManager.RebuildAllMaterial;
end;

class function TgxNGDSurfacePair.NewtonAABBOverlap(const material
  : PNewtonMaterial; const body0, body1: PNewtonBody; threadIndex: Integer)
  : Integer; cdecl;
begin
  Result := Ord(TgxNGDSurfacePair(NewtonMaterialGetMaterialPairUserData
    (material)).FAABBOverlapEvent(material, body0, body1, threadIndex));
end;

class procedure TgxNGDSurfacePair.NewtonContactsProcess(const contact
  : PNewtonJoint; timestep: dFloat; threadIndex: Integer); cdecl;
begin
  TgxNGDSurfacePair(NewtonMaterialGetMaterialPairUserData
    (NewtonContactGetMaterial(NewtonContactJointGetFirstContact(contact))))
    .FContactProcessEvent(contact, timestep, threadIndex);
end;

function TgxNGDSurfacePair.OnNewtonAABBOverlapEvent(const cmaterial
  : PNewtonMaterial; const cbody0, cbody1: PNewtonBody;
  threadIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TgxNGDSurfacePair.OnNewtonContactsProcessEvent(const ccontact
  : PNewtonJoint; timestep: dFloat; threadIndex: Integer);
begin

end;

procedure TgxNGDSurfacePair.SetCollidable(const Value: Boolean);
begin
  FCollidable := Value;
  FManager.RebuildAllMaterial;
end;

procedure TgxNGDSurfacePair.SetContinuousCollisionMode(const Value: Boolean);
begin
  FContinuousCollisionMode := Value;
  FManager.RebuildAllMaterial;
end;

procedure TgxNGDSurfacePair.SetElasticity(const Value: Single);
begin
  if (Value >= 0) then
    FElasticity := Value;
  FManager.RebuildAllMaterial;
end;

procedure TgxNGDSurfacePair.SetKineticFriction(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FKineticFriction := Value;
  FManager.RebuildAllMaterial;
end;

procedure TgxNGDSurfacePair.SetMaterialItems(const item1,
  item2: TgxNGDSurfaceItem);
begin
  FSurfaceItem1 := item1;
  FSurfaceItem2 := item2;
  FManager.RebuildAllMaterial;
end;

procedure TgxNGDSurfacePair.SetSoftness(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FSoftness := Value;
  FManager.RebuildAllMaterial;
end;

procedure TgxNGDSurfacePair.SetStaticFriction(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
    FStaticFriction := Value;
  FManager.RebuildAllMaterial;
end;

procedure TgxNGDSurfacePair.SetThickness(const Value: Boolean);
begin
  FThickness := Value;
  FManager.RebuildAllMaterial;
end;

function TgxNGDSurfacePair.StoredElasticity: Boolean;
begin
  Result := not SameValue(FElasticity, 0.4, epsilon);
end;

function TgxNGDSurfacePair.StoredKineticFriction: Boolean;
begin
  Result := not SameValue(FKineticFriction, 0.5, epsilon);
end;

function TgxNGDSurfacePair.StoredSoftness: Boolean;
begin
  Result := not SameValue(FSoftness, 0.1, epsilon);
end;

function TgxNGDSurfacePair.StoredStaticFriction: Boolean;
begin
  Result := not SameValue(FStaticFriction, 0.9, epsilon);
end;

// ------------------------
//  TgxNGDJoint
// ------------------------

constructor TgxNGDJoint.Create(Collection: TCollection);
begin
  inherited;
  FCollisionState := False;
  FStiffness := 0.9;
  FNewtonJoint := nil;
  FNewtonUserJoint := nil;
  FParentObject := nil;
  FChildObject := nil;

  FManager := TgxNGDManager(Collection.Owner);

  FBallAndSocketOptions := TgxNGDJointPivot.Create(FManager, self);
  FHingeOptions := TgxNGDJointPin.Create(FManager, self);
  FSliderOptions := TgxNGDJointPin.Create(FManager, self);
  FCorkscrewOptions := TgxNGDJointPin.Create(FManager, self);
  FUniversalOptions := TgxNGDJointPin2.Create(FManager, self);

  FCustomBallAndSocketOptions := TgxNGDJointBallAndSocket.Create
    (FManager, self);
  FCustomHingeOptions := TgxNGDJointHinge.Create(FManager, self);
  FCustomSliderOptions := TgxNGDJointSlider.Create(FManager, self);
  FKinematicOptions := TgxNGDJointKinematicController.Create;

  FUPVectorDirection := TgxCoordinates.CreateInitialized(self, YHmgVector,
    csVector);
  FUPVectorDirection.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TgxNGDJoint.Destroy;
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

procedure TgxNGDJoint.DestroyNewtonData;
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

procedure TgxNGDJoint.KinematicControllerPick(pickpoint: TVector4f;
  PickedActions: TgxNGDPickedActions);
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

procedure TgxNGDJoint.Render;

  procedure DrawPivot(pivot: TVector4f);
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

  procedure DrawPin(pin, pivot: TVector4f);
  begin
    FManager.FCurrentColor := FManager.DebugOption.JointAxisColor;
    FManager.AddNode(VectorAdd(pivot, pin));
    FManager.AddNode(VectorAdd(pivot, VectorNegate(pin)));
  end;

  procedure DrawJoint(pivot: TVector4f);
  begin
    FManager.FCurrentColor := FManager.DebugOption.CustomColor;
    FManager.AddNode(FParentObject.AbsolutePosition);
    FManager.AddNode(pivot);
    FManager.AddNode(pivot);
    FManager.AddNode(FChildObject.AbsolutePosition);
  end;

  procedure DrawKinematic;
  var
    pickedMatrix: TMatrix4f;
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

procedure TgxNGDJoint.SetChildObject(const Value: TgxBaseSceneObject);
begin
  FChildObject := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TgxNGDJoint.SetCollisionState(const Value: Boolean);
begin
  FCollisionState := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TgxNGDJoint.SetJointType(const Value: TgxNGDJoints);
begin
  FJointType := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TgxNGDJoint.SetParentObject(const Value: TgxBaseSceneObject);
begin
  FParentObject := Value;
  FManager.RebuildAllJoint(self);
end;

procedure TgxNGDJoint.SetStiffness(const Value: Single);
begin
  if (Value >= 0) and (Value <= 1) then
  begin
    FStiffness := Value;
    FManager.RebuildAllJoint(self);
  end;
end;

function TgxNGDJoint.StoredStiffness: Boolean;
begin
  Result := not SameValue(FStiffness, 0.9, epsilon);
end;

// ------------------------------------------------------------------
// TgxNGDJoint.TgxNGDJointPivot
// ------------------------------------------------------------------
constructor TgxNGDJointPivot.Create(AOwner: TComponent; aOuter: TgxNGDJoint);
begin
  FManager := AOwner as TgxNGDManager;
  FOuter := aOuter;
  FPivotPoint := TgxCoordinates.CreateInitialized(aOuter, NullHMGPoint,
    csPoint);
  FPivotPoint.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TgxNGDJointPivot.Destroy;
begin
  FPivotPoint.Free;
  inherited;
end;

// ------------------------------------------------------------------
// TgxNGDJoint.TgxNGDJointPin
// ------------------------------------------------------------------
constructor TgxNGDJointPin.Create(AOwner: TComponent; aOuter: TgxNGDJoint);
begin
  inherited;
  FPinDirection := TgxCoordinates.CreateInitialized(aOuter, NullHmgVector,
    csVector);
  FPinDirection.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TgxNGDJointPin.Destroy;
begin
  FPinDirection.Free;
  inherited;
end;

// ------------------------------------------------------------------
// TgxNGDJoint.TgxNGDJointPin2
// ------------------------------------------------------------------
constructor TgxNGDJointPin2.Create(AOwner: TComponent; aOuter: TgxNGDJoint);
begin
  inherited;
  FPinDirection2 := TgxCoordinates.CreateInitialized(aOuter, NullHmgVector,
    csVector);
  FPinDirection2.OnNotifyChange := FManager.RebuildAllJoint;
end;

destructor TgxNGDJointPin2.Destroy;
begin
  FPinDirection2.Free;
  inherited;
end;

// ------------------------------------------------------------------
// TgxNGDJoint.TgxNGDJointBallAndSocket
// ------------------------------------------------------------------
constructor TgxNGDJointBallAndSocket.Create(AOwner: TComponent;
  aOuter: TgxNGDJoint);
begin
  inherited;
  FConeAngle := 90;
  FMinTwistAngle := -90;
  FMaxTwistAngle := 90;
end;

procedure TgxNGDJointBallAndSocket.SetConeAngle(const Value: Single);
begin
  FConeAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TgxNGDJointBallAndSocket.SetMaxTwistAngle(const Value: Single);
begin
  FMaxTwistAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TgxNGDJointBallAndSocket.SetMinTwistAngle(const Value: Single);
begin
  FMinTwistAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

function TgxNGDJointBallAndSocket.StoredConeAngle: Boolean;
begin
  Result := not SameValue(FConeAngle, 90, epsilon);
end;

function TgxNGDJointBallAndSocket.StoredMaxTwistAngle: Boolean;
begin
  Result := not SameValue(FMaxTwistAngle, 90, epsilon);
end;

function TgxNGDJointBallAndSocket.StoredMinTwistAngle: Boolean;
begin
  Result := not SameValue(FMinTwistAngle, -90, epsilon);
end;

// ------------------------------------------------------------------
// TgxNGDJoint.TgxNGDJointHinge
// ------------------------------------------------------------------
constructor TgxNGDJointHinge.Create(AOwner: TComponent; aOuter: TgxNGDJoint);
begin
  inherited;
  FMinAngle := -90;
  FMaxAngle := 90;
end;

procedure TgxNGDJointHinge.SetMaxAngle(const Value: Single);
begin
  FMaxAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TgxNGDJointHinge.SetMinAngle(const Value: Single);
begin
  FMinAngle := Value;
  FManager.RebuildAllJoint(FOuter);
end;

function TgxNGDJointHinge.StoredMaxAngle: Boolean;
begin
  Result := not SameValue(FMaxAngle, 90, epsilon);
end;

function TgxNGDJointHinge.StoredMinAngle: Boolean;
begin
  Result := not SameValue(FMinAngle, -90, epsilon);
end;

// ------------------------------------------------------------------
// TgxNGDJoint.TgxNGDJointSlider 
// ------------------------------------------------------------------
constructor TgxNGDJointSlider.Create(AOwner: TComponent; aOuter: TgxNGDJoint);
begin
  inherited;
  FMinDistance := -10;
  FMaxDistance := 10;
end;

procedure TgxNGDJointSlider.SetMaxDistance(const Value: Single);
begin
  FMaxDistance := Value;
  FManager.RebuildAllJoint(FOuter);
end;

procedure TgxNGDJointSlider.SetMinDistance(const Value: Single);
begin
  FMinDistance := Value;
  FManager.RebuildAllJoint(FOuter);
end;

function TgxNGDJointSlider.StoredMaxDistance: Boolean;
begin
  Result := not SameValue(FMaxDistance, 10, epsilon);
end;

function TgxNGDJointSlider.StoredMinDistance: Boolean;
begin
  Result := not SameValue(FMinDistance, -10, epsilon);
end;

// ----------------------------------
// TgxNGDJointKinematicController
// ----------------------------------

constructor TgxNGDJointKinematicController.Create;
begin
  FPickModeLinear := False;
  FLinearFriction := 750;
  FAngularFriction := 250;
end;

function TgxNGDJointKinematicController.StoredAngularFriction: Boolean;
begin
  Result := not SameValue(FAngularFriction, 250, epsilon);
end;

function TgxNGDJointKinematicController.StoredLinearFriction: Boolean;
begin
  Result := not SameValue(FLinearFriction, 750, epsilon);
end;

// ------------------------------------------------------------------
// TgxNGDBehaviourList
// ------------------------------------------------------------------
function TgxNGDBehaviourList.GetBehav(index: Integer): TgxNGDBehaviour;
begin
  Result := Items[index];
end;

procedure TgxNGDBehaviourList.PutBehav(index: Integer; Item: TgxNGDBehaviour);
begin
  inherited put(index, Item);
end;

initialization

// ------------------------------------------------------------------

RegisterXCollectionItemClass(TgxNGDDynamic);
RegisterXCollectionItemClass(TgxNGDStatic);

// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------

UnregisterXCollectionItemClass(TgxNGDDynamic);
UnregisterXCollectionItemClass(TgxNGDStatic);

// CloseNGD;

end.
