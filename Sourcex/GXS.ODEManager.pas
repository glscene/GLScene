//
// The graphics engine GLXEngine. The unit of GXScene for Delphi
//
unit GXS.ODEManager;
(*
  An ODE Manager for GXScene.
  Notes:
  This code is still under development so any part of it may change at anytime.
*)
interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  System.Math,

  GXS.XOpenGL,
  GXS.XCollection,
  Stage.VectorGeometry,
  Stage.Manager,

  GXS.Scene,
  GXS.Texture,
  GXS.Objects,
  GXS.PersistentClasses,
  GXS.VectorLists,
  GXS.Color,
  GXS.Coordinates,
  GXS.RenderContextInfo,
  GXS.State,
  Stage.VectorTypes,
  GXS.TerrainRenderer,
  GXS.Graph,

  GXS.ODEUtils,
  ODE.Import;

type

  TgxODECustomCollisionEvent = procedure(Geom1, Geom2: PdxGeom) of object;

  TgxODECollisionEvent = procedure(Sender: TObject; Object1, Object2: TObject;
    var Contact: TdContact; var HandleCollision: Boolean) of object;

  TgxODEObjectCollisionEvent = procedure(Sender: TObject; Object2: TObject;
    var Contact: TdContact; var HandleCollision: Boolean) of object;

  TgxODECollisionSurfaceMode = (csmMu2, csmFDir1, csmBounce, csmSoftERP,
    csmSoftCFM, csmMotion1, csmMotion2, csmSlip1, csmSlip2);
  TgxSurfaceModes = set of TgxODECollisionSurfaceMode;

  TgxODESolverMethod = (osmDefault, osmStepFast, osmQuickStep);

  TgxODEElements = class;
  TgxODEBehaviour = class;
  TgxODEElementBase = class;
  TgxODEJointBase = class;

  TgxODEManager = class(TComponent)
  private
    FWorld: PdxWorld;
    FSpace: PdxSpace;
    FContactGroup: TdJointGroupID;
    FGravity: TgxCoordinates;
    FOnCollision: TgxODECollisionEvent;
    FOnCustomCollision: TgxODECustomCollisionEvent;
    FNumContactJoints, FMaxContacts: Integer;
    FODEBehaviours: TgxPersistentObjectList;
    FRFContactList: TList;
    FIterations: Integer;
    FSolver: TgxODESolverMethod;
    FContacts: array of TdContact;
    FContactGeoms: array of TdContactGeom;
    FRenderPoint: TgxRenderPoint;
    FVisible, 
	FVisibleAtRunTime: Boolean;
    FGeomColorDynD, 
	FGeomColorDynE, 
	FGeomColorStat: TgxColor;
  protected
    procedure Loaded; override;
    procedure CalcContact(Object1, Object2: TObject; var Contact: TdContact);
    procedure Collision(g1, g2: PdxGeom);
    procedure GravityChange(Sender: TObject);
    procedure SetMaxContacts(const Value: Integer);
    procedure SetGravity(Value: TgxCoordinates);
    procedure SetIterations(const val: Integer);
    function GetODEBehaviour(index: Integer): TgxODEBehaviour;
    procedure RegisterODEBehaviour(ODEBehaviour: TgxODEBehaviour);
    procedure UnregisterODEBehaviour(ODEBehaviour: TgxODEBehaviour);
    procedure SetRenderPoint(const Value: TgxRenderPoint);
    procedure RenderEvent(Sender: TObject; var rci: TgxRenderContextInfo);
    procedure RenderPointFreed(Sender: TObject);
    procedure SetVisible(const Value: Boolean);
    procedure SetVisibleAtRunTime(const Value: Boolean);
    procedure SetGeomColorDynE(const Value: TgxColor);
    procedure GeomColorChangeDynE(Sender: TObject);
    procedure SetGeomColorDynD(const Value: TgxColor);
    procedure GeomColorChangeDynD(Sender: TObject);
    procedure SetGeomColorStat(const Value: TgxColor);
    procedure GeomColorChangeStat(Sender: TObject);
    property ODEBehaviours[index: Integer]: TgxODEBehaviour read GetODEBehaviour;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Step(deltaTime: double);
    procedure NotifyChange(Sender: TObject);
    property World: PdxWorld read FWorld;
    property Space: PdxSpace read FSpace;
    property ContactGroup: TdJointGroupID read FContactGroup;
    property NumContactJoints: Integer read FNumContactJoints;
  published
    property Gravity: TgxCoordinates read FGravity write SetGravity;
    property OnCollision: TgxODECollisionEvent read FOnCollision write FOnCollision;
    property OnCustomCollision: TgxODECustomCollisionEvent read FOnCustomCollision write FOnCustomCollision;
    property Solver: TgxODESolverMethod read FSolver write FSolver;
    property Iterations: Integer read FIterations write SetIterations;
    property MaxContacts: Integer read FMaxContacts write SetMaxContacts;
    property RenderPoint: TgxRenderPoint read FRenderPoint write SetRenderPoint;
    property Visible: Boolean read FVisible write SetVisible;
    property VisibleAtRunTime: Boolean read FVisibleAtRunTime write SetVisibleAtRunTime;
    property GeomColorDynD: TgxColor read FGeomColorDynD write SetGeomColorDynD;
    property GeomColorDynE: TgxColor read FGeomColorDynE write SetGeomColorDynE;
    property GeomColorStat: TgxColor read FGeomColorStat write SetGeomColorStat;
  end;

  TgxODECollisionSurface = class(TPersistent)
  private
    FOwner: TPersistent;
    FSurfaceParams: TdSurfaceParameters;
    FRFCoeff: Single;
    FRFEnabled: Boolean;
  protected
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);
    function GetSurfaceMode: TgxSurfaceModes;
    function GetMu: TdReal;
    function GetMu2: TdReal;
    function GetBounce: TdReal;
    function GetBounce_Vel: TdReal;
    function GetSoftERP: TdReal;
    function GetSoftCFM: TdReal;
    function GetMotion1: TdReal;
    function GetMotion2: TdReal;
    function GetSlip1: TdReal;
    function GetSlip2: TdReal;
    procedure SetSurfaceMode(Value: TgxSurfaceModes);
    procedure SetMu(Value: TdReal);
    procedure SetMu2(Value: TdReal);
    procedure SetBounce(Value: TdReal);
    procedure SetBounce_Vel(Value: TdReal);
    procedure SetSoftERP(Value: TdReal);
    procedure SetSoftCFM(Value: TdReal);
    procedure SetMotion1(Value: TdReal);
    procedure SetMotion2(Value: TdReal);
    procedure SetSlip1(Value: TdReal);
    procedure SetSlip2(Value: TdReal);
  public
    constructor Create(AOwner: TPersistent);
    function GetOwner: TPersistent; override;
    procedure Assign(Source: TPersistent); override;
  published
    property RollingFrictionCoeff: Single read FRFCoeff write FRFCoeff;
    property RollingFrictionEnabled: Boolean read FRFEnabled write FRFEnabled;
    property SurfaceMode: TgxSurfaceModes read GetSurfaceMode
      write SetSurfaceMode;
    property Mu: TdReal read GetMu write SetMu;
    property Mu2: TdReal read GetMu2 write SetMu2;
    property Bounce: TdReal read GetBounce write SetBounce;
    property Bounce_Vel: TdReal read GetBounce_Vel write SetBounce_Vel;
    property SoftERP: TdReal read GetSoftERP write SetSoftERP;
    property SoftCFM: TdReal read GetSoftCFM write SetSoftCFM;
    property Motion1: TdReal read GetMotion1 write SetMotion1;
    property Motion2: TdReal read GetMotion2 write SetMotion2;
    property Slip1: TdReal read GetSlip1 write SetSlip1;
    property Slip2: TdReal read GetSlip2 write SetSlip2;
  end;

  TgxODEElementClass = class of TgxODEElementBase;

  // Basis structures for behaviour style implementations. 
  TgxODEBehaviour = class(TgxBehaviour)
  private
    FManager: TgxODEManager;
    FManagerName: String;
    FSurface: TgxODECollisionSurface;
    FOnCollision: TgxODEObjectCollisionEvent;
    FInitialized: Boolean;
    FOwnerBaseSceneObject: TgxBaseSceneObject;
  protected
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    procedure SetManager(Value: TgxODEManager);
    procedure SetSurface(Value: TgxODECollisionSurface);
    function GetAbsoluteMatrix: TMatrix4f;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure NotifyChange(Sender: TObject);
    procedure Render(var rci: TgxRenderContextInfo); virtual;
    procedure Reinitialize;
    property Initialized: Boolean read FInitialized;
    property AbsoluteMatrix: TMatrix4f read GetAbsoluteMatrix;
  published
    property Manager: TgxODEManager read FManager write SetManager;
    property Surface: TgxODECollisionSurface read FSurface write SetSurface;
    property OnCollision: TgxODEObjectCollisionEvent read FOnCollision
      write FOnCollision;
  end;

  TgxODEDynamic = class(TgxODEBehaviour)
  private
    FBody: PdxBody;
    FMass: TdMass;
    FElements: TgxODEElements;
    FEnabled: Boolean;
    FJointRegister: TList;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetMass(const Value: TdMass);
    function GetMass: TdMass;
    procedure AlignBodyToMatrix(Mat: TMatrix4f);
    procedure SetEnabled(const Value: Boolean);
    function GetEnabled: Boolean;
    procedure RegisterJoint(Joint: TgxODEJointBase);
    procedure UnregisterJoint(Joint: TgxODEJointBase);
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Render(var rci: TgxRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function UniqueItem: Boolean; override;
    function AddNewElement(AChild: TgxODEElementClass): TgxODEElementBase; virtual;
    procedure AlignObject;
    function CalculateMass: TdMass;
    procedure CalibrateCenterOfMass;
    procedure AddForce(Force: TAffineVector);
    procedure AddForceAtPos(Force, Pos: TAffineVector);
    procedure AddForceAtRelPos(Force, Pos: TAffineVector);
    procedure AddRelForce(Force: TAffineVector);
    procedure AddRelForceAtPos(Force, Pos: TAffineVector);
    procedure AddRelForceAtRelPos(Force, Pos: TAffineVector);
    procedure AddTorque(Torque: TAffineVector);
    procedure AddRelTorque(Torque: TAffineVector);
    property Body: PdxBody read FBody;
    property Mass: TdMass read GetMass write SetMass;
  published
    property Elements: TgxODEElements read FElements;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

  TgxODEStatic = class(TgxODEBehaviour)
  private
    FElements: TgxODEElements;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure AlignElements;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Render(var rci: TgxRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function UniqueItem: Boolean; override;
    function AddNewElement(AChild: TgxODEElementClass): TgxODEElementBase; virtual;
  published
    property Elements: TgxODEElements read FElements;
  end;

  TgxODEElements = class(TXCollection)
  private
    function GetElement(index: Integer): TgxODEElementBase;
  public
    destructor Destroy; override;
    class function ItemsClass: TXCollectionItemClass; override;
    procedure Initialize;
    procedure Finalize;
    procedure NotifyChange(Sender: TObject);
    procedure Render(var rci: TgxRenderContextInfo);
    property Element[index: Integer]: TgxODEElementBase read GetElement;
  end;

  TgxODEElementBase = class(TXCollectionItem)
  private
    FMass: TdMass;
    FDensity: TdReal;
    FGeomTransform, 
	FGeomElement: PdxGeom;
    FPosition, 
	FDirection, 
	FUp: TgxCoordinates;
    FLocalMatrix: TMatrix4f;
    FRealignODE, 
	FInitialized, 
	FDynamic, 
	FIsCalculating: Boolean;
  protected
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    function CalculateMass: TdMass; virtual;
    procedure ODERebuild; virtual;
    procedure NotifyChange(Sender: TObject);
    procedure CoordinateChanged(Sender: TObject);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function IsODEInitialized: Boolean;
    procedure AlignGeomElementToMatrix(Mat: TMatrix4f); virtual;
    procedure SetGeomElement(aGeom: PdxGeom);
    procedure RebuildMatrix;
    procedure RebuildVectors;
    procedure SetDensity(const Value: TdReal);
    procedure SetMatrix(const Value: TMatrix4f);
    function GetMatrix: TMatrix4f;
    procedure SetPosition(const Value: TgxCoordinates);
    procedure SetDirection(const Value: TgxCoordinates);
    procedure SetUp(const Value: TgxCoordinates);
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Render(var rci: TgxRenderContextInfo); virtual;
    function AbsoluteMatrix: TMatrix4f;
    function AbsolutePosition: TAffineVector;
    property Matrix: TMatrix4f read GetMatrix write SetMatrix;
    property GeomTransform: PdxGeom read FGeomTransform;
    property Geom: PdxGeom read FGeomElement;
    property Initialized: Boolean read FInitialized;
  published
    property Density: TdReal read FDensity write SetDensity;
    property Position: TgxCoordinates read FPosition write SetPosition;
    property Direction: TgxCoordinates read FDirection write SetDirection;
    property Up: TgxCoordinates read FUp write SetUp;
  end;

  // ODE box implementation. 
  TgxODEElementBox = class(TgxODEElementBase)
  private
    FBoxWidth, 
	FBoxHeight, 
	FBoxDepth: TdReal;
  protected
    procedure Initialize; override;
    function CalculateMass: TdMass; override;
    procedure ODERebuild; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function GetBoxWidth: TdReal;
    function GetBoxHeight: TdReal;
    function GetBoxDepth: TdReal;
    procedure SetBoxWidth(const Value: TdReal);
    procedure SetBoxHeight(const Value: TdReal);
    procedure SetBoxDepth(const Value: TdReal);
  public
    constructor Create(AOwner: TXCollection); override;
    procedure Render(var rci: TgxRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
  published
    property BoxWidth: TdReal read GetBoxWidth write SetBoxWidth;
    property BoxHeight: TdReal read GetBoxHeight write SetBoxHeight;
    property BoxDepth: TdReal read GetBoxDepth write SetBoxDepth;
  end;

  // ODE sphere implementation.
  TgxODEElementSphere = class(TgxODEElementBase)
  private
    FRadius: TdReal;
  protected
    procedure Initialize; override;
    function CalculateMass: TdMass; override;
    procedure ODERebuild; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function GetRadius: TdReal;
    procedure SetRadius(const Value: TdReal);
  public
    constructor Create(AOwner: TXCollection); override;
    procedure Render(var rci: TgxRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
  published
    property Radius: TdReal read GetRadius write SetRadius;
  end;

  // ODE capped cylinder implementation. 
  TgxODEElementCapsule = class(TgxODEElementBase)
  private
    FRadius, 
	FLength: TdReal;
  protected
    procedure Initialize; override;
    function CalculateMass: TdMass; override;
    procedure ODERebuild; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function GetRadius: TdReal;
    function GetLength: TdReal;
    procedure SetRadius(const Value: TdReal);
    procedure SetLength(const Value: TdReal);
  public
    constructor Create(AOwner: TXCollection); override;
    procedure Render(var rci: TgxRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
  published
    property Radius: TdReal read GetRadius write SetRadius;
    property Length: TdReal read GetLength write SetLength;
  end;

  // ODE cylinder implementation. 
  TgxODEElementCylinder = class(TgxODEElementBase)
  private
    FRadius, FLength: TdReal;
  protected
    procedure Initialize; override;
    function CalculateMass: TdMass; override;
    procedure ODERebuild; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function GetRadius: TdReal;
    function GetLength: TdReal;
    procedure SetRadius(const Value: TdReal);
    procedure SetLength(const Value: TdReal);
  public
    constructor Create(AOwner: TXCollection); override;
    procedure Render(var rci: TgxRenderContextInfo); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
  published
    property Radius: TdReal read GetRadius write SetRadius;
    property Length: TdReal read GetLength write SetLength;
  end;

  // ODE tri-mesh implementation. 
  TgxODEElementTriMesh = class(TgxODEElementBase)
  private
    FTriMeshData: PdxTriMeshData;
    FVertices: TgxAffineVectorList;
    FIndices: TgxIntegerList;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    function CalculateMass: TdMass; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetVertices(const Value: TgxAffineVectorList);
    procedure SetIndices(const Value: TgxIntegerList);
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
    procedure RefreshTriMeshData;
    property Vertices: TgxAffineVectorList read FVertices write SetVertices;
    property Indices: TgxIntegerList read FIndices write SetIndices;
  end;

  // ODE plane implementation. 
  TgxODEElementPlane = class(TgxODEElementBase)
  protected
    procedure Initialize; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure AlignGeomElementToMatrix(Mat: TMatrix4f); override;
  public
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    class function ItemCategory: String; override;
    class function CanAddTo(collection: TXCollection): Boolean; override;
  end;

  // An XCollection decendant for ODE Joints.
  TgxODEJoints = class(TXCollection)
  protected
    function GetJoint(index: Integer): TgxODEJointBase;
  public
    class function ItemsClass: TXCollectionItemClass; override;
    procedure Initialize;
    procedure Finalize;
    property Joint[index: Integer]: TgxODEJointBase read GetJoint; default;
  end;

  { Component front-end for storing ODE Joints. }
  TgxODEJointList = class(TComponent)
  private
    FJoints: TgxODEJoints;
  protected
    procedure WriteJoints(stream: TStream);
    procedure ReadJoints(stream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Joints: TgxODEJoints read FJoints;
  end;

  TJointOption = (joBothObjectsMustBeAssigned);
  TJointOptions = set of TJointOption;

  { Base structures for ODE Joints. }
  TgxODEJointBase = class(TXCollectionItem)
  private
    FJointID: TdJointID;
    FObject1, FObject2: TgxBaseSceneObject;
    FManager: TgxODEManager;
    FObject1Name, FObject2Name, FManagerName: String;
    FInitialized, FEnabled: Boolean;
    FJointOptions: TJointOptions;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
    function IsODEInitialized: Boolean;
    procedure RegisterJointWithObject(Obj: TgxBaseSceneObject);
    procedure UnregisterJointWithObject(Obj: TgxBaseSceneObject);
    procedure Attach;
    procedure SetManager(const Value: TgxODEManager);
    procedure SetObject1(const Value: TgxBaseSceneObject);
    procedure SetObject2(const Value: TgxBaseSceneObject);
    procedure SetEnabled(const Value: Boolean);
    procedure SetJointOptions(const Value: TJointOptions);
    property JointOptions: TJointOptions read FJointOptions
      write SetJointOptions;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged; virtual;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    function IsAttached: Boolean;
    procedure DoLoaded;
    property JointID: TdJointID read FJointID;
    property Initialized: Boolean read FInitialized;
  published
    property Manager: TgxODEManager read FManager write SetManager;
    property Object1: TgxBaseSceneObject read FObject1 write SetObject1;
    property Object2: TgxBaseSceneObject read FObject2 write SetObject2;
    property Enabled: Boolean read FEnabled write SetEnabled;
  end;

  TgxODESetParamCallback = function(Param: Integer; const Value: TdReal): Boolean of object;
  TgxODEGetParamCallback = function(Param: Integer; var Value: TdReal): Boolean of object;

  TgxODEJointParams = class(TPersistent)
  private
    FOwner: TPersistent;
    FSetCallback: TgxODESetParamCallback;
    FGetCallback: TgxODEGetParamCallback;
    FLoStop, 
	FHiStop, 
	FVel, 
	FFMax,
	FFudgeFactor, 
	FBounce,
	FCFM, 
	FStopERP,
    FStopCFM,
	FSuspensionERP, 
	FSuspensionCFM: TdReal;
    FFlagLoStop,
	FFlagHiStop, 
	FFlagVel, 
	FFlagFMax,
	FFlagFudgeFactor,
    FFlagBounce, 
	FFlagCFM, 
	FFlagStopERP,
	FFlagStopCFM, 
	FFlagSuspensionERP,
    FFlagSuspensionCFM: Boolean;
  protected
    function GetLoStop: TdReal;
    function GetHiStop: TdReal;
    function GetVel: TdReal;
    function GetFMax: TdReal;
    function GetFudgeFactor: TdReal;
    function GetBounce: TdReal;
    function GetCFM: TdReal;
    function GetStopERP: TdReal;
    function GetStopCFM: TdReal;
    function GetSuspensionERP: TdReal;
    function GetSuspensionCFM: TdReal;
    procedure SetLoStop(const Value: TdReal);
    procedure SetHiStop(const Value: TdReal);
    procedure SetVel(const Value: TdReal);
    procedure SetFMax(const Value: TdReal);
    procedure SetFudgeFactor(const Value: TdReal);
    procedure SetBounce(const Value: TdReal);
    procedure SetCFM(const Value: TdReal);
    procedure SetStopERP(const Value: TdReal);
    procedure SetStopCFM(const Value: TdReal);
    procedure SetSuspensionERP(const Value: TdReal);
    procedure SetSuspensionCFM(const Value: TdReal);
    procedure WriteToFiler(writer: TWriter);
    procedure ReadFromFiler(reader: TReader);
  public
    constructor Create(AOwner: TPersistent);
    function GetOwner: TPersistent; override;
    procedure Assign(Source: TPersistent); override;
    procedure ApplyFlagged;
    property SetCallback: TgxODESetParamCallback read FSetCallback write FSetCallback;
    property GetCallback: TgxODEGetParamCallback read FGetCallback write FGetCallback;
  published
    property LoStop: TdReal read GetLoStop write SetLoStop;
    property HiStop: TdReal read GetHiStop write SetHiStop;
    property Vel: TdReal read GetVel write SetVel;
    property FMax: TdReal read GetFMax write SetFMax;
    property FudgeFactor: TdReal read GetFudgeFactor write SetFudgeFactor;
    property Bounce: TdReal read GetBounce write SetBounce;
    property CFM: TdReal read GetCFM write SetCFM;
    property StopERP: TdReal read GetStopERP write SetStopERP;
    property StopCFM: TdReal read GetStopCFM write SetStopCFM;
    property SuspensionERP: TdReal read GetSuspensionERP write SetSuspensionERP;
    property SuspensionCFM: TdReal read GetSuspensionCFM write SetSuspensionCFM;
  end;

  // ODE hinge joint implementation. 
  TgxODEJointHinge = class(TgxODEJointBase)
  private
    FAnchor, 
	FAxis: TgxCoordinates;
    FAxisParams: TgxODEJointParams;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetAnchor(const Value: TgxCoordinates);
    procedure SetAxis(const Value: TgxCoordinates);
    procedure AnchorChange(Sender: TObject);
    procedure AxisChange(Sender: TObject);
    procedure SetAxisParams(const Value: TgxODEJointParams);
    function SetAxisParam(Param: Integer; const Value: TdReal): Boolean;
    function GetAxisParam(Param: Integer; var Value: TdReal): Boolean;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged; override;
    procedure Initialize; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property Anchor: TgxCoordinates read FAnchor write SetAnchor;
    property Axis: TgxCoordinates read FAxis write SetAxis;
    property AxisParams: TgxODEJointParams read FAxisParams write SetAxisParams;
  end;

  // ODE ball joint implementation
  TgxODEJointBall = class(TgxODEJointBase)
  private
    FAnchor: TgxCoordinates;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetAnchor(const Value: TgxCoordinates);
    procedure AnchorChange(Sender: TObject);
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged; override;
    procedure Initialize; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property Anchor: TgxCoordinates read FAnchor write SetAnchor;
  end;

  // ODE slider joint implementation
  TgxODEJointSlider = class(TgxODEJointBase)
  private
    FAxis: TgxCoordinates;
    FAxisParams: TgxODEJointParams;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetAxis(const Value: TgxCoordinates);
    procedure AxisChange(Sender: TObject);
    procedure SetAxisParams(const Value: TgxODEJointParams);
    function SetAxisParam(Param: Integer; const Value: TdReal): Boolean;
    function GetAxisParam(Param: Integer; var Value: TdReal): Boolean;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged; override;
    procedure Initialize; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property Axis: TgxCoordinates read FAxis write SetAxis;
    property AxisParams: TgxODEJointParams read FAxisParams write SetAxisParams;
  end;

  { ODE fixed joint implementation. }
  TgxODEJointFixed = class(TgxODEJointBase)
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
  public
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    procedure Initialize; override;
  end;

  { ODE hinge2 joint implementation. }
  TgxODEJointHinge2 = class(TgxODEJointBase)
  private
    FAnchor, FAxis1, FAxis2: TgxCoordinates;
    FAxis1Params, FAxis2Params: TgxODEJointParams;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetAnchor(const Value: TgxCoordinates);
    procedure SetAxis1(const Value: TgxCoordinates);
    procedure SetAxis2(const Value: TgxCoordinates);
    procedure AnchorChange(Sender: TObject);
    procedure Axis1Change(Sender: TObject);
    procedure Axis2Change(Sender: TObject);
    procedure SetAxis1Params(const Value: TgxODEJointParams);
    procedure SetAxis2Params(const Value: TgxODEJointParams);
    function SetAxis1Param(Param: Integer; const Value: TdReal): Boolean;
    function SetAxis2Param(Param: Integer; const Value: TdReal): Boolean;
    function GetAxis1Param(Param: Integer; var Value: TdReal): Boolean;
    function GetAxis2Param(Param: Integer; var Value: TdReal): Boolean;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure StructureChanged; override;
    procedure Initialize; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property Anchor: TgxCoordinates read FAnchor write SetAnchor;
    property Axis1: TgxCoordinates read FAxis1 write SetAxis1;
    property Axis2: TgxCoordinates read FAxis2 write SetAxis2;
    property Axis1Params: TgxODEJointParams read FAxis1Params write SetAxis1Params;
    property Axis2Params: TgxODEJointParams read FAxis2Params write SetAxis2Params;
  end;

  // ODE universal joint implementation
  TgxODEJointUniversal = class(TgxODEJointBase)
  private
    FAnchor, 
	FAxis1, 
	FAxis2: TgxCoordinates;
    FAxis1Params, 
	FAxis2Params: TgxODEJointParams;
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure SetAnchor(const Value: TgxCoordinates);
    procedure SetAxis1(const Value: TgxCoordinates);
    procedure SetAxis2(const Value: TgxCoordinates);
    procedure AnchorChange(Sender: TObject);
    procedure Axis1Change(Sender: TObject);
    procedure Axis2Change(Sender: TObject);
    procedure SetAxis1Params(const Value: TgxODEJointParams);
    procedure SetAxis2Params(const Value: TgxODEJointParams);
    function SetAxis1Param(Param: Integer; const Value: TdReal): Boolean;
    function SetAxis2Param(Param: Integer; const Value: TdReal): Boolean;
    function GetAxis1Param(Param: Integer; var Value: TdReal): Boolean;
    function GetAxis2Param(Param: Integer; var Value: TdReal): Boolean;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Initialize; override;
    procedure StructureChanged; override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
  published
    property Anchor: TgxCoordinates read FAnchor write SetAnchor;
    property Axis1: TgxCoordinates read FAxis1 write SetAxis1;
    property Axis2: TgxCoordinates read FAxis2 write SetAxis2;
    property Axis1Params: TgxODEJointParams read FAxis1Params write SetAxis1Params;
    property Axis2Params: TgxODEJointParams read FAxis2Params write SetAxis2Params;
  end;

  TgxODEContactPoint = class
  public
    Position: TAffineVector;
    Normal: TAffineVector;
    Depth: Single;
  end;

  (*The custom collider is designed for generic contact handling. There is a
   contact point generator for sphere, box, capped cylinder, cylinder and cone geoms.
   Once the contact points for a collision are generated the abstract Collide
   function is called to generate the depth and the contact position and normal.
   These points are then sorted and the deepest are applied to ODE *)
  TgxODECustomCollider = class(TgxODEBehaviour)
  private
    FGeom: PdxGeom;
    FContactList,
      FContactCache: TList;
    FTransform: TMatrix4f;
    FContactResolution: Single;
    FRenderContacts: Boolean;
    FContactRenderPoints: TgxAffineVectorList;
    FPointSize: Single;
    FContactColor: TgxColor;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    //  Test a position for a collision and fill out the contact information.
    function Collide(aPos: TAffineVector; var Depth: Single;
      var cPos, cNorm: TAffineVector): Boolean; virtual; abstract;
    //  Clears the contact list so it's ready for another collision.
    procedure ClearContacts;
    //  Add a contact point to the list for ApplyContacts to processes.
    procedure AddContact(x, y, z: TdReal); overload;
    procedure AddContact(pos: TAffineVector); overload;
    //  Sort the current contact list and apply the deepest to ODE.
    function ApplyContacts(o1, o2: PdxGeom; flags: Integer;
      contact: PdContactGeom; skip: Integer): Integer;
    // Set the transform used that transforms contact points generated with AddContact
    procedure SetTransform(ATransform: TMatrix4f);
    procedure SetContactResolution(const Value: Single);
    procedure SetRenderContacts(const Value: Boolean);
    procedure SetPointSize(const Value: Single);
    procedure SetContactColor(const Value: TgxColor);
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Render(var rci: TgxRenderContextInfo); override;
    property Geom: PdxGeom read FGeom;
  published
    (* Defines the resolution of the contact points created for the colliding
       Geom. The number of contact points generated change base don the size
       of the object and the ContactResolution. Lower values generate higher
       resolution contact boundaries, and thus smoother but slower collisions. *)
    property ContactResolution: Single read FContactResolution write SetContactResolution;
    (* Toggle contact point rendering on and off. (Rendered through the assigned
       Manager.RenderPoint. *)
    property RenderContacts: Boolean read FRenderContacts write SetRenderContacts;
    //  Contact point rendering size (in pixels).
    property PointSize: Single read FPointSize write SetPointSize;
    //  Contact point rendering color.
    property ContactColor: TgxColor read FContactColor write SetContactColor;
  end;

  (* Add this behaviour to a TgxHeightField or TgxTerrainRenderer to enable
     height based collisions for spheres, boxes, capped cylinders, cylinders and cones. *)
  TgxODEHeightField = class(TgxODECustomCollider)
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    function Collide(aPos: TAffineVector; var Depth: Single;
      var cPos, cNorm: TAffineVector): Boolean; override;
  public
    constructor Create(AOwner: TXCollection); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: Boolean; override;
    class function CanAddTo(collection: TXCollection): Boolean; override;
  end;


(* ODE nearCallBack, throws near callback to the collision procedure
   of the ODE manager linked by the Data pointer *)
procedure nearCallBack(Data: Pointer; o1, o2: PdxGeom); cdecl;
{ Helper functions for extracting data from objects with different inheritance. }
function GetBodyFromObject(anObject: TObject): PdxBody;
function GetBodyFromGLXceneObject(anObject: TgxBaseSceneObject): PdxBody;
function GetSurfaceFromObject(anObject: TObject): TgxODECollisionSurface;

// GLODEObject register methods (used for joint object persistence)
procedure RegisterGLXceneObject(anObject: TgxBaseSceneObject);
procedure UnregisterGLXceneObject(anObject: TgxBaseSceneObject);
function GetGLXceneObject(anObjectName: String): TgxBaseSceneObject;

// Get and GetOrCreate functions for ode behaviours
function GetOdeStatic(obj: TgxBaseSceneObject): TgxODEStatic;
function GetOrCreateOdeStatic(obj: TgxBaseSceneObject): TgxODEStatic;
function GetOdeDynamic(obj: TgxBaseSceneObject): TgxODEDynamic;
function GetOrCreateOdeDynamic(obj: TgxBaseSceneObject): TgxODEDynamic;

// Get and GetOrCreate functions for ODE HeightField
function GetODEHeightField(obj: TgxBaseSceneObject): TgxODEHeightField;
function GetOrCreateODEHeightField(obj: TgxBaseSceneObject): TgxODEHeightField;


var
  vODEObjectRegister: TList;
  vCustomColliderClass: TdGeomClass;
  vCustomColliderClassNum: Integer;

implementation // ------------------------------------------------------------

procedure nearCallBack(Data: Pointer; o1, o2: PdxGeom); cdecl;
begin
  TgxODEManager(Data).Collision(o1, o2);
end;

function GetBodyFromObject(anObject: TObject): PdxBody;
begin
  Result := nil;
  if Assigned(anObject) then
    if anObject is TgxODEDynamic then
      Result := TgxODEDynamic(anObject).Body;
end;

function GetBodyFromGLXceneObject(anObject: TgxBaseSceneObject): PdxBody;
var
  temp: TgxODEDynamic;
begin
  Result := nil;
  if Assigned(anObject) then
  begin
    temp := TgxODEDynamic(anObject.Behaviours.GetByClass(TgxODEDynamic));
    if temp <> nil then
      Result := temp.Body;
  end;
end;

function GetSurfaceFromObject(anObject: TObject): TgxODECollisionSurface;
var
  ODEBehaviour: TgxODEBehaviour;
begin
  Result := nil;
  if Assigned(anObject) then
    if anObject is TgxODEBehaviour then
      Result := TgxODEBehaviour(anObject).Surface
    else
    begin
      if (anObject is TgxBaseSceneObject) then
      begin
        ODEBehaviour := TgxODEBehaviour(TgxBaseSceneObject(anObject)
          .Behaviours.GetByClass(TgxODEBehaviour));
        if Assigned(ODEBehaviour) then
          Result := ODEBehaviour.Surface
      end;
    end;
end;

function IsGLODEObject(Obj: TgxBaseSceneObject): Boolean;
var
  temp: TgxODEDynamic;
begin
  Result := False;
  if Assigned(Obj) then
  begin
    temp := TgxODEDynamic(Obj.Behaviours.GetByClass(TgxODEDynamic));
    Result := Assigned(temp);
  end;
end;

procedure RegisterGLXceneObject(anObject: TgxBaseSceneObject);
begin
  if vODEObjectRegister.IndexOf(anObject) = -1 then
    vODEObjectRegister.Add(anObject);
end;

procedure UnregisterGLXceneObject(anObject: TgxBaseSceneObject);
begin
  vODEObjectRegister.Remove(anObject);
end;

function GetGLXceneObject(anObjectName: String): TgxBaseSceneObject;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to vODEObjectRegister.Count - 1 do
    if TgxBaseSceneObject(vODEObjectRegister[i]).GetNamePath = anObjectName
    then
    begin
      Result := vODEObjectRegister[i];
      Exit;
    end;
end;

function GetOdeStatic(Obj: TgxBaseSceneObject): TgxODEStatic;
begin
  Result := TgxODEStatic(Obj.Behaviours.GetByClass(TgxODEStatic));
end;

function GetOrCreateOdeStatic(Obj: TgxBaseSceneObject): TgxODEStatic;
begin
  Result := TgxODEStatic(Obj.GetOrCreateBehaviour(TgxODEStatic));
end;

function GetOdeDynamic(Obj: TgxBaseSceneObject): TgxODEDynamic;
begin
  Result := TgxODEDynamic(Obj.Behaviours.GetByClass(TgxODEDynamic));
end;

function GetOrCreateOdeDynamic(Obj: TgxBaseSceneObject): TgxODEDynamic;
begin
  Result := TgxODEDynamic(Obj.GetOrCreateBehaviour(TgxODEDynamic));
end;


function GetODEHeightField(obj: TgxBaseSceneObject): TgxODEHeightField;
begin
  Result:= TgxODEHeightField(obj.Behaviours.GetByClass(TgxODEHeightField));
end;

function GetOrCreateODEHeightField(obj: TgxBaseSceneObject): TgxODEHeightField;
begin
  Result:= TgxODEHeightField(obj.GetOrCreateBehaviour(TgxODEHeightField));
end;

function GetColliderFromGeom(aGeom: PdxGeom): TgxODECustomCollider;
var
  temp: TObject;
begin
  Result:= nil;
  temp:= dGeomGetData(aGeom);
  if Assigned(temp) then
    if temp is TgxODECustomCollider then
      Result:= TgxODECustomCollider(temp);
end;

function ContactSort(Item1, Item2: Pointer): Integer;
var
  c1, c2: TgxODEContactPoint;
begin
  c1 := TgxODEContactPoint(Item1);
  c2 := TgxODEContactPoint(Item2);
  if c1.Depth > c2.Depth then
    result := -1
  else if c1.Depth = c2.Depth then
    result := 0
  else
    result := 1;
end;

function CollideSphere(o1, o2: PdxGeom; flags: Integer; contact: PdContactGeom;
  skip: Integer): Integer; cdecl;
var
  Collider: TgxODECustomCollider;
  i, j, res: Integer;
  pos: PdVector3;
  R: PdMatrix3;
  rmat, mat: TMatrix4f;
  rad, dx, dy, dz: TdReal;
begin
  Result := 0;
  Collider := GetColliderFromGeom(o1);
  if not Assigned(Collider) then
    exit;
  pos := dGeomGetPosition(o2);
  R := dGeomGetRotation(o2);
  ODERToSceneMatrix(mat, R^, pos^);
  Collider.SetTransform(mat);

  rad := dGeomSphereGetRadius(o2);

  res := Round(10 * rad / Collider.ContactResolution);
  if res < 8 then
    res := 8;

  Collider.AddContact(0, 0, -rad);
  Collider.AddContact(0, 0, rad);
  rmat := CreateRotationMatrixZ(2 * Pi / res);
  for i := 0 to res - 1 do
  begin
    mat := MatrixMultiply(rmat, mat);
    Collider.SetTransform(mat);
    for j := -(res div 2) + 1 to (res div 2) - 1 do
    begin
      dx := rad * cos(j * Pi / res);
      dy := 0;
      dz := rad * sin(j * Pi / res);
      Collider.AddContact(dx, dy, dz);
    end;
  end;
  Result := Collider.ApplyContacts(o1, o2, flags, contact, skip);
  Collider.SetTransform(IdentityHMGMatrix);
end;

function CollideBox(o1, o2: PdxGeom; flags: Integer; contact: PdContactGeom;
  skip: Integer): Integer; cdecl;
var
  Collider: TgxODECustomCollider;
  i, j, res: Integer;
  rcpres, len1, len2: Single;
  s: TdVector3;
  pos: PdVector3;
  R: PdMatrix3;
  mat: TMatrix4f;
begin
  Result := 0;
  Collider := GetColliderFromGeom(o1);
  if not Assigned(Collider) then
    exit;
  pos := dGeomGetPosition(o2);
  R := dGeomGetRotation(o2);
  ODERToSceneMatrix(mat, R^, pos^);
  Collider.SetTransform(mat);

  dGeomBoxGetLengths(o2, s);

  res := Round(Sqrt(MaxFloat([s[0], s[1], s[2]])) / Collider.ContactResolution);
  if res < 1 then
    res := 1;
  rcpres := 1 / res;

  s[0] := 0.5 * s[0];
  s[1] := 0.5 * s[1];
  s[2] := 0.5 * s[2];

  with Collider do
  begin
    // Corners
    AddContact(s[0], s[1], s[2]);
    AddContact(s[0], s[1], -s[2]);
    AddContact(s[0], -s[1], s[2]);
    AddContact(s[0], -s[1], -s[2]);
    AddContact(-s[0], s[1], s[2]);
    AddContact(-s[0], s[1], -s[2]);
    AddContact(-s[0], -s[1], s[2]);
    AddContact(-s[0], -s[1], -s[2]);
    // Edges
    for i := -(res - 1) to (res - 1) do
    begin
      len1 := i * rcpres * s[0];
      AddContact(len1, s[1], s[2]);
      AddContact(len1, s[1], -s[2]);
      AddContact(len1, -s[1], s[2]);
      AddContact(len1, -s[1], -s[2]);
      len1 := i * rcpres * s[1];
      AddContact(s[0], len1, s[2]);
      AddContact(s[0], len1, -s[2]);
      AddContact(-s[0], len1, s[2]);
      AddContact(-s[0], len1, -s[2]);
      len1 := i * rcpres * s[2];
      AddContact(s[0], s[1], len1);
      AddContact(s[0], -s[1], len1);
      AddContact(-s[0], s[1], len1);
      AddContact(-s[0], -s[1], len1);
    end;
    // Faces
    for i := -(res - 1) to (res - 1) do
      for j := -(res - 1) to (res - 1) do
      begin
        len1 := i * rcpres * s[0];
        len2 := j * rcpres * s[1];
        AddContact(len1, len2, s[2]);
        AddContact(len1, len2, -s[2]);
        len2 := j * rcpres * s[2];
        AddContact(len1, s[1], len2);
        AddContact(len1, -s[1], len2);
        len1 := i * rcpres * s[1];
        AddContact(s[0], len1, len2);
        AddContact(-s[0], len1, len2);
      end;
  end;

  Result := Collider.ApplyContacts(o1, o2, flags, contact, skip);
  Collider.SetTransform(IdentityHMGMatrix);
end;

function CollideCapsule(o1, o2: PdxGeom; flags: Integer; contact: PdContactGeom;
  skip: Integer): Integer; cdecl;
var
  Collider: TgxODECustomCollider;
  i, j, res: Integer;
  pos: PdVector3;
  R: PdMatrix3;
  mat, rmat: TMatrix4f;
  rad, len, dx, dy, dz: TdReal;
begin
  Result := 0;
  Collider := GetColliderFromGeom(o1);
  if not Assigned(Collider) then
    exit;
  pos := dGeomGetPosition(o2);
  R := dGeomGetRotation(o2);
  ODERToSceneMatrix(mat, R^, pos^);
  Collider.SetTransform(mat);
  dGeomCapsuleGetParams(o2, rad, len);
  res := Round(5 * MaxFloat(4 * rad, len) / Collider.ContactResolution);
  if res < 8 then
    res := 8;
  rmat := CreateRotationMatrixZ(2 * Pi / res);
  with Collider do
  begin
    AddContact(0, 0, -rad - 0.5 * len);
    AddContact(0, 0, rad + 0.5 * len);
    for i := 0 to res - 1 do
    begin
      mat := MatrixMultiply(rmat, mat);
      SetTransform(mat);
      for j := 0 to res do
        AddContact(rad, 0, len * (j / res - 0.5));
      for j := 1 to (res div 2) - 1 do
      begin
        dx := rad * cos(j * Pi / res);
        dy := 0;
        dz := rad * sin(j * Pi / res);
        Collider.AddContact(dx, dy, -dz - 0.5 * len);
        Collider.AddContact(dx, dy, dz + 0.5 * len);
      end;
    end;
  end;
  Result := Collider.ApplyContacts(o1, o2, flags, contact, skip);
  Collider.SetTransform(IdentityHMGMatrix);
end;

function CollideCylinder(o1, o2: PdxGeom; flags: Integer;
  contact: PdContactGeom; skip: Integer): Integer; cdecl;
var
  Collider: TgxODECustomCollider;
  i, j, res: Integer;
  pos: PdVector3;
  R: PdMatrix3;
  mat: TMatrix4f;
  rad, len, dx, dy: TdReal;
begin
  Result := 0;
  Collider := GetColliderFromGeom(o1);
  if not Assigned(Collider) then
    exit;

  pos := dGeomGetPosition(o2);
  R := dGeomGetRotation(o2);
  ODERToSceneMatrix(mat, R^, pos^);
  Collider.SetTransform(mat);
  dGeomCylinderGetParams(o2, rad, len);
  res := Round(5 * MaxFloat(4 * rad, len) / Collider.ContactResolution);
  if res < 8 then
    res := 8;

  with Collider do
  begin
    AddContact(0, -0.5 * len, 0);
    AddContact(0, 0.5 * len, 0);
    for i := 0 to res - 1 do
    begin
      SinCosine(2 * Pi * i / res, rad, dy, dx);
      AddContact(dx, -0.5 * len, dy);
      AddContact(dx, 0, dy);
      AddContact(dx, 0.5 * len, dy);

      for j := 0 to res do
        AddContact(dx, len * (j / res - 0.5), dy);

      for j := 1 to (res div 2) - 1 do
      begin
        SinCosine(2 * Pi * i / res, rad * j / (res div 2), dy, dx);
        AddContact(dx, -0.5 * len, dy);
        AddContact(dx, 0.5 * len, dy);
      end;
    end;
  end;

  Result := Collider.ApplyContacts(o1, o2, flags, contact, skip);
  Collider.SetTransform(IdentityHMGMatrix);
end;

function GetCustomColliderFn(num: Integer): TdColliderFn; cdecl;
begin
  if num = dSphereClass then
    Result := CollideSphere
  else if num = dBoxClass then
    Result := CollideBox
  else if num = dCapsuleClass then
    Result := CollideCapsule
  else if num = dCylinderClass then
    Result := CollideCylinder
  else
    Result := nil;
end;


// ---------------
// --------------- TgxODEManager ---------------
// ---------------

constructor TgxODEManager.Create(AOwner: TComponent);
begin
  FWorld := nil;
  if not InitODE('') then
    raise Exception.Create('ODE failed to initialize.');

  inherited;

  FODEBehaviours := TgxPersistentObjectList.Create;
  FRFContactList := TList.Create;

  FGravity := TgxCoordinates.CreateInitialized(Self, NullHmgPoint, csVector);
  FGravity.OnNotifyChange := GravityChange;

  FSolver := osmDefault;
  FIterations := 3;
  MaxContacts := 8;

  if not(csDesigning in ComponentState) then
  begin
    FWorld := dWorldCreate;
    FSpace := dHashSpaceCreate(nil);
    dWorldSetCFM(FWorld, 1E-5);
    dWorldSetQuickStepNumIterations(FWorld, FIterations);
    FContactGroup := dJointGroupCreate(100);
  end;

  FGeomColorDynD := TgxColor.CreateInitialized(Self, clrRed, GeomColorChangeDynD);
  FGeomColorDynE := TgxColor.CreateInitialized(Self, clrLime, GeomColorChangeDynE);
  FGeomColorStat := TgxColor.CreateInitialized(Self, clrBlue, GeomColorChangeStat);

  RegisterManager(Self);
end;

destructor TgxODEManager.Destroy;
begin
  RenderPoint := nil;

  // Unregister everything
  while FODEBehaviours.Count > 0 do
    ODEBehaviours[0].Manager := nil;

  // Clean up everything
  FODEBehaviours.Free;
  FGravity.Free;
  FRFContactList.Free;

  if Assigned(FWorld) then
  begin
    dJointGroupEmpty(FContactGroup);
    dJointGroupDestroy(FContactGroup);
    dSpaceDestroy(FSpace);
    dWorldDestroy(FWorld);
  end;

  FGeomColorDynD.Free;
  FGeomColorDynE.Free;
  FGeomColorStat.Free;

  DeregisterManager(Self);
  inherited Destroy;
end;

procedure TgxODEManager.RegisterODEBehaviour(ODEBehaviour: TgxODEBehaviour);
begin
  FODEBehaviours.Add(ODEBehaviour);
end;

procedure TgxODEManager.UnregisterODEBehaviour(ODEBehaviour: TgxODEBehaviour);
begin
  FODEBehaviours.Remove(ODEBehaviour);
end;

procedure TgxODEManager.Loaded;
begin
  GravityChange(Self);
end;

procedure TgxODEManager.SetGravity(Value: TgxCoordinates);
begin
  FGravity.SetPoint(Value.DirectX, Value.DirectY, Value.DirectZ);
end;

procedure TgxODEManager.GravityChange(Sender: TObject);
begin
  if Assigned(FWorld) then
    dWorldSetGravity(FWorld, FGravity.X, FGravity.Y, FGravity.Z);
end;

procedure TgxODEManager.CalcContact(Object1, Object2: TObject; var Contact: TdContact);
var
  Surface1, Surface2: TgxODECollisionSurface;
  Body1, Body2: PdxBody;
begin
  Surface1 := GetSurfaceFromObject(Object1);
  Surface2 := GetSurfaceFromObject(Object2);
  if not(Assigned(Surface1) and Assigned(Surface2)) then
    Exit;

  with Contact.Surface do
  begin
    // Average the involved contact information and assign it to the contact.
    // Better methods for contact calculation will be looked into in the future.
    mode := Surface1.FSurfaceParams.mode or Surface2.FSurfaceParams.mode;
    Mu := (Surface1.Mu + Surface2.Mu) * 0.5;
    Mu2 := (Surface1.Mu2 + Surface2.Mu2) * 0.5;
    Bounce := (Surface1.Bounce + Surface2.Bounce) * 0.5;
    Bounce_Vel := (Surface1.Bounce_Vel + Surface2.Bounce_Vel) * 0.5;
    soft_erp := (Surface1.SoftERP + Surface2.SoftERP) * 0.5;
    soft_cfm := (Surface1.SoftCFM + Surface2.SoftCFM) * 0.5;
    Motion1 := (Surface1.Motion1 + Surface2.Motion1) * 0.5;
    Motion2 := (Surface1.Motion2 + Surface2.Motion2) * 0.5;
    Slip1 := (Surface1.Slip1 + Surface2.Slip1) * 0.5;
    Slip2 := (Surface1.Slip2 + Surface2.Slip2) * 0.5;
  end;

  // Rolling friction
  Body1 := GetBodyFromObject(Object1);
  Body2 := GetBodyFromObject(Object2);
  if (Surface1.RollingFrictionEnabled) and Assigned(Body1) then
    FRFContactList.Add(Object1);
  if (Surface2.RollingFrictionEnabled) and Assigned(Body2) then
    FRFContactList.Add(Object2);
end;

procedure TgxODEManager.Collision(g1, g2: PdxGeom);
var
  i, flags, num_contacts: Integer;
  Obj1, Obj2: Pointer;
  b1, b2: PdxBody;
  Joint: TdJointID;
  HandleCollision: Boolean;
begin
  // Check for custom collision handling event
  if Assigned(FOnCustomCollision) then
  begin
    FOnCustomCollision(g1, g2);
    Exit;
  end;

  Obj1 := dGeomGetData(g1);
  Obj2 := dGeomGetData(g2);
  b1 := dGeomGetBody(g1);
  b2 := dGeomGetBody(g2);

  // don't create contact between static objects
  if not Assigned(b1) and not Assigned(b2) then
    Exit;

  if Assigned(b1) and Assigned(b2) then
    if dAreConnected(b1, b2) = 1 then
      Exit;

  // Get the collisions
  flags := $0000FFFF and FMaxContacts;
  num_contacts := dCollide(g1, g2, flags, FContactGeoms[0], SizeOf(TdContactGeom));

  // Set up the initial contact info
  for i := 0 to num_contacts - 1 do
  begin
    FContacts[i].Geom := FContactGeoms[i];
  end;

  for i := 0 to num_contacts - 1 do
  begin
    HandleCollision := True;

    if Assigned(Obj1) and Assigned(Obj2) then
    begin
      // Calculate the contact based on Obj1 and Obj2 surface info
      CalcContact(Obj1, Obj2, FContacts[i]);
      if Assigned(FOnCollision) then
      begin
        // Fire the Scene level OnCollision event for last minute
        // customization to the contact before the contact joint
        // is created
        FOnCollision(Self, Obj1, Obj2, FContacts[i], HandleCollision);
      end;
      // Fire the OnCollision event for each object
      if TObject(Obj1) is TgxODEBehaviour then
        if Assigned(TgxODEBehaviour(Obj1).FOnCollision) then
          TgxODEBehaviour(Obj1).FOnCollision(Self, Obj2, FContacts[i], HandleCollision);
      if TObject(Obj2) is TgxODEBehaviour then
        if Assigned(TgxODEBehaviour(Obj2).FOnCollision) then
          TgxODEBehaviour(Obj2).FOnCollision(Self, Obj1, FContacts[i], HandleCollision);
    end
    else
    begin
      // Default surface values
      FContacts[i].Surface.Mu := 1000;
    end;
    if HandleCollision then
    begin
      // Create and assign the contact joint
      Joint := dJointCreateContact(FWorld, FContactGroup, @FContacts[i]);
      dJointAttach(Joint, b1, b2);
      // Increment the number of contact joints this step
      Inc(FNumContactJoints);
    end;
  end;
end;

procedure TgxODEManager.Step(deltaTime: double);
var
  i: Integer;
  vec: PdVector3;
  Body: PdxBody;
  Coeff: Single;
begin
  if not Assigned(World) then
    Exit;

  // Reset the contact joint counter
  FNumContactJoints := 0;

  // Align static elements to their GLScene parent objects
  for i := 0 to FODEBehaviours.Count - 1 do
    if ODEBehaviours[i] is TgxODEStatic then
      if ODEBehaviours[i].Initialized then
        TgxODEStatic(ODEBehaviours[i]).AlignElements;

  // Run ODE collisions and step the scene
  dSpaceCollide(FSpace, Self, nearCallBack);
  case FSolver of
    osmDefault:
      dWorldStep(FWorld, deltaTime);
    osmQuickStep:
      dWorldQuickStep(FWorld, deltaTime);
  end;
  dJointGroupEmpty(FContactGroup);

  // Align dynamic objects to their ODE bodies
  for i := 0 to FODEBehaviours.Count - 1 do
    if ODEBehaviours[i] is TgxODEDynamic then
      if ODEBehaviours[i].Initialized then
        TgxODEDynamic(ODEBehaviours[i]).AlignObject;

  // Process rolling friction
  Coeff := 0;
  Body := nil;
  while FRFContactList.Count > 0 do
  begin
    if TObject(FRFContactList[0]) is TgxODEDynamic then
    begin
      Body := TgxODEDynamic(FRFContactList[0]).Body;
      Coeff := 1 - (TgxODEDynamic(FRFContactList[0])
        .Surface.RollingFrictionCoeff / TgxODEDynamic(FRFContactList[0])
        .Mass.Mass);
    end;
    vec := dBodyGetAngularVel(Body);
    dBodySetAngularVel(Body, vec[0] * Coeff, vec[1] * Coeff, vec[2] * Coeff);
    FRFContactList.Delete(0);
  end;
end;

procedure TgxODEManager.NotifyChange(Sender: TObject);
begin
  if Assigned(RenderPoint) then
    RenderPoint.StructureChanged;
end;

procedure TgxODEManager.SetIterations(const val: Integer);
begin
  FIterations := val;
  if Assigned(FWorld) then
    dWorldSetQuickStepNumIterations(FWorld, FIterations);
end;

procedure TgxODEManager.SetMaxContacts(const Value: Integer);
begin
  if Value <> FMaxContacts then
  begin
    FMaxContacts := Value;
    SetLength(FContacts, FMaxContacts);
    SetLength(FContactGeoms, FMaxContacts);
  end;
end;

function TgxODEManager.GetODEBehaviour(index: Integer): TgxODEBehaviour;
begin
  Result := TgxODEBehaviour(FODEBehaviours[index]);
end;

procedure TgxODEManager.SetRenderPoint(const Value: TgxRenderPoint);
begin
  if FRenderPoint <> Value then
  begin
    if Assigned(FRenderPoint) then
      FRenderPoint.UnRegisterCallBack(RenderEvent);
    FRenderPoint := Value;
    if Assigned(FRenderPoint) then
      FRenderPoint.RegisterCallBack(RenderEvent, RenderPointFreed);
  end;
end;

procedure TgxODEManager.RenderEvent(Sender: TObject;
  var rci: TgxRenderContextInfo);
var
  i: Integer;
begin
  if not Visible then
    Exit;
  if not(csDesigning in ComponentState) then
    if not VisibleAtRunTime then
      Exit;
  glPushAttrib(GL_ENABLE_BIT + GL_CURRENT_BIT + GL_POLYGON_BIT);
  glDisable(GL_LIGHTING);
  glEnable(GL_POLYGON_OFFSET_LINE);
  glPolygonOffset(1, 2);

  for i := 0 to FODEBehaviours.Count - 1 do
  begin
    if ODEBehaviours[i] is TgxODEDynamic then
      if TgxODEDynamic(ODEBehaviours[i]).GetEnabled then
        glColor4fv(@GeomColorDynE.AsAddress^)
      else
        glColor4fv(@GeomColorDynD.AsAddress^)
    else
      glColor4fv(@GeomColorStat.AsAddress^);

    ODEBehaviours[i].Render(rci);
  end;
end;

procedure TgxODEManager.RenderPointFreed(Sender: TObject);
begin
  FRenderPoint := nil;
end;

procedure TgxODEManager.SetVisible(const Value: Boolean);
begin
  if Value <> FVisible then
  begin
    FVisible := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxODEManager.SetVisibleAtRunTime(const Value: Boolean);
begin
  if Value <> FVisibleAtRunTime then
  begin
    FVisibleAtRunTime := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxODEManager.SetGeomColorDynD(const Value: TgxColor);
begin
  FGeomColorDynD.Assign(Value);
  NotifyChange(Self);
end;

procedure TgxODEManager.GeomColorChangeDynD(Sender: TObject);
begin
  NotifyChange(Self);
end;

procedure TgxODEManager.SetGeomColorDynE(const Value: TgxColor);
begin
  FGeomColorDynE.Assign(Value);
  NotifyChange(Self);
end;

procedure TgxODEManager.GeomColorChangeDynE(Sender: TObject);
begin
  NotifyChange(Self);
end;

procedure TgxODEManager.SetGeomColorStat(const Value: TgxColor);
begin
  FGeomColorStat.Assign(Value);
  NotifyChange(Self);
end;

procedure TgxODEManager.GeomColorChangeStat(Sender: TObject);
begin
  NotifyChange(Self);
end;

// ---------------
// --------------- TgxODECollisionSurface ---------------
// ---------------

constructor TgxODECollisionSurface.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  Mu := 1000;
  RollingFrictionEnabled := False;
  RollingFrictionCoeff := 0.001; // Larger Coeff = more friction
end;

function TgxODECollisionSurface.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TgxODECollisionSurface.Assign(Source: TPersistent);
begin
  inherited;
  if not Assigned(Source) then
    Exit;
  if Source is TgxODECollisionSurface then
  begin
    RollingFrictionCoeff := TgxODECollisionSurface(Source).RollingFrictionCoeff;
    RollingFrictionEnabled := TgxODECollisionSurface(Source).RollingFrictionEnabled;
    SurfaceMode := TgxODECollisionSurface(Source).SurfaceMode;
    Mu := TgxODECollisionSurface(Source).Mu;
    Mu2 := TgxODECollisionSurface(Source).Mu2;
    Bounce := TgxODECollisionSurface(Source).Bounce;
    Bounce_Vel := TgxODECollisionSurface(Source).Bounce_Vel;
    SoftERP := TgxODECollisionSurface(Source).SoftERP;
    SoftCFM := TgxODECollisionSurface(Source).SoftCFM;
    Motion1 := TgxODECollisionSurface(Source).Motion1;
    Motion2 := TgxODECollisionSurface(Source).Motion2;
    Slip1 := TgxODECollisionSurface(Source).Slip1;
    Slip2 := TgxODECollisionSurface(Source).Slip2;
  end;
end;

procedure TgxODECollisionSurface.WriteToFiler(writer: TWriter);
var
  mode: TgxSurfaceModes;
begin
  with writer do
  begin
    WriteInteger(0);
    WriteFloat(RollingFrictionCoeff);
    WriteBoolean(RollingFrictionEnabled);
    mode := SurfaceMode;
    Write(mode, SizeOf(TgxSurfaceModes));
    WriteFloat(Mu);
    WriteFloat(Mu2);
    WriteFloat(Bounce);
    WriteFloat(Bounce_Vel);
    WriteFloat(SoftERP);
    WriteFloat(SoftCFM);
    WriteFloat(Motion1);
    WriteFloat(Motion2);
    WriteFloat(Slip1);
    WriteFloat(Slip2);
  end;
end;

procedure TgxODECollisionSurface.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
  mode: TgxSurfaceModes;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion = 0);
    RollingFrictionCoeff := ReadFloat;
    RollingFrictionEnabled := ReadBoolean;
    Read(mode, SizeOf(TgxSurfaceModes));
    SurfaceMode := mode;
    Mu := ReadFloat;
    Mu2 := ReadFloat;
    Bounce := ReadFloat;
    Bounce_Vel := ReadFloat;
    SoftERP := ReadFloat;
    SoftCFM := ReadFloat;
    Motion1 := ReadFloat;
    Motion2 := ReadFloat;
    Slip1 := ReadFloat;
    Slip2 := ReadFloat;
  end;
end;

// GetSurfaceMode
//
function TgxODECollisionSurface.GetSurfaceMode: TgxSurfaceModes;
var
  ASurfaceModes: TgxSurfaceModes;
begin
  ASurfaceModes := [];
  if (FSurfaceParams.mode and dContactSlip2) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmSlip2];
  if (FSurfaceParams.mode and dContactSlip1) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmSlip1];
  if (FSurfaceParams.mode and dContactMotion2) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmMotion2];
  if (FSurfaceParams.mode and dContactMotion1) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmMotion1];
  if (FSurfaceParams.mode and dContactSoftCFM) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmSoftCFM];
  if (FSurfaceParams.mode and dContactSoftERP) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmSoftERP];
  if (FSurfaceParams.mode and dContactBounce) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmBounce];
  if (FSurfaceParams.mode and dContactFDir1) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmFDir1];
  if (FSurfaceParams.mode and dContactMu2) <> 0 then
    ASurfaceModes := ASurfaceModes + [csmMu2];
  Result := ASurfaceModes;
end;

procedure TgxODECollisionSurface.SetSurfaceMode(Value: TgxSurfaceModes);
var
  AMode: Integer;
begin
  AMode := 0;
  if csmSlip2 in Value then
    AMode := AMode or dContactSlip2;
  if csmSlip1 in Value then
    AMode := AMode or dContactSlip1;
  if csmMotion2 in Value then
    AMode := AMode or dContactMotion2;
  if csmMotion1 in Value then
    AMode := AMode or dContactMotion1;
  if csmSoftCFM in Value then
    AMode := AMode or dContactSoftCFM;
  if csmSoftERP in Value then
    AMode := AMode or dContactSoftERP;
  if csmBounce in Value then
    AMode := AMode or dContactBounce;
  if csmFDir1 in Value then
    AMode := AMode or dContactFDir1;
  if csmMu2 in Value then
    AMode := AMode or dContactMu2;
  FSurfaceParams.mode := AMode;
end;

function TgxODECollisionSurface.GetMu: TdReal;
begin
  Result := FSurfaceParams.Mu;
end;

function TgxODECollisionSurface.GetMu2: TdReal;
begin
  Result := FSurfaceParams.Mu2;
end;

function TgxODECollisionSurface.GetBounce: TdReal;
begin
  Result := FSurfaceParams.Bounce;
end;

function TgxODECollisionSurface.GetBounce_Vel: TdReal;
begin
  Result := FSurfaceParams.Bounce_Vel;
end;

function TgxODECollisionSurface.GetSoftERP: TdReal;
begin
  Result := FSurfaceParams.soft_erp;
end;

function TgxODECollisionSurface.GetSoftCFM: TdReal;
begin
  Result := FSurfaceParams.soft_cfm;
end;

function TgxODECollisionSurface.GetMotion1: TdReal;
begin
  Result := FSurfaceParams.Motion1;
end;

function TgxODECollisionSurface.GetMotion2: TdReal;
begin
  Result := FSurfaceParams.Motion2;
end;

function TgxODECollisionSurface.GetSlip1: TdReal;
begin
  Result := FSurfaceParams.Slip1;
end;

function TgxODECollisionSurface.GetSlip2: TdReal;
begin
  Result := FSurfaceParams.Slip2;
end;

procedure TgxODECollisionSurface.SetMu(Value: TdReal);
begin
  FSurfaceParams.Mu := Value;
end;

procedure TgxODECollisionSurface.SetMu2(Value: TdReal);
begin
  FSurfaceParams.Mu2 := Value;
end;

procedure TgxODECollisionSurface.SetBounce(Value: TdReal);
begin
  FSurfaceParams.Bounce := Value;
end;

procedure TgxODECollisionSurface.SetBounce_Vel(Value: TdReal);
begin
  FSurfaceParams.Bounce_Vel := Value;
end;

procedure TgxODECollisionSurface.SetSoftERP(Value: TdReal);
begin
  FSurfaceParams.soft_erp := Value;
end;

procedure TgxODECollisionSurface.SetSoftCFM(Value: TdReal);
begin
  FSurfaceParams.soft_cfm := Value;
end;

procedure TgxODECollisionSurface.SetMotion1(Value: TdReal);
begin
  FSurfaceParams.Motion1 := Value;
end;

procedure TgxODECollisionSurface.SetMotion2(Value: TdReal);
begin
  FSurfaceParams.Motion2 := Value;
end;

procedure TgxODECollisionSurface.SetSlip1(Value: TdReal);
begin
  FSurfaceParams.Slip1 := Value;
end;

procedure TgxODECollisionSurface.SetSlip2(Value: TdReal);
begin
  FSurfaceParams.Slip2 := Value;
end;


// ---------------
// --------------- TgxODEBehaviour --------------
// ---------------

constructor TgxODEBehaviour.Create(AOwner: TXCollection);
begin
  inherited;
  FSurface := TgxODECollisionSurface.Create(Self);
  FInitialized := False;
  FOwnerBaseSceneObject := OwnerBaseSceneObject;
  if Assigned(FOwnerBaseSceneObject) then
    RegisterGLXceneObject(OwnerBaseSceneObject);
end;

destructor TgxODEBehaviour.Destroy;
begin
  if Assigned(Manager) then
    Manager := nil;
  if Assigned(FOwnerBaseSceneObject) then
    UnregisterGLXceneObject(FOwnerBaseSceneObject);
  FSurface.Free;
  inherited;
end;

procedure TgxODEBehaviour.Initialize;
begin
  FInitialized := True;
end;

procedure TgxODEBehaviour.Finalize;
begin
  FInitialized := False;
end;

procedure TgxODEBehaviour.Reinitialize;
begin
  if Initialized then
    Finalize;
  Initialize;
end;

procedure TgxODEBehaviour.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
    Surface.WriteToFiler(writer);
  end;
end;

procedure TgxODEBehaviour.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FManagerName := ReadString;
    Surface.ReadFromFiler(reader);
  end;
end;

procedure TgxODEBehaviour.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TgxODEManager, FManagerName);
    if Assigned(mng) then
      Manager := TgxODEManager(mng);
    FManagerName := '';
  end
end;

procedure TgxODEBehaviour.Render(var rci: TgxRenderContextInfo);
begin
  // virtual
end;

procedure TgxODEBehaviour.NotifyChange(Sender: TObject);
begin
  if Assigned(Manager) then
    Manager.NotifyChange(Self);
end;

procedure TgxODEBehaviour.SetManager(Value: TgxODEManager);
begin
  if FManager <> Value then
  begin
    if Assigned(FManager) then
    begin
      if Initialized then
        Finalize;
      FManager.UnregisterODEBehaviour(Self);
    end;
    FManager := Value;
    if Assigned(FManager) then
    begin
      if not(csDesigning in TComponent(Owner.Owner).ComponentState) then
      // mrqzzz moved here
        Initialize;
      FManager.RegisterODEBehaviour(Self);
    end;
  end;
end;

procedure TgxODEBehaviour.SetSurface(Value: TgxODECollisionSurface);
begin
  FSurface.Assign(Value);
end;

function TgxODEBehaviour.GetAbsoluteMatrix: TMatrix4f;
begin
  Result := IdentityHMGMatrix;
  if Assigned(Owner.Owner) then
    if Owner.Owner is TgxBaseSceneObject then
      Result := TgxBaseSceneObject(Owner.Owner).AbsoluteMatrix;
end;


// ---------------
// --------------- TgxODEDynamic ---------------
// ---------------

constructor TgxODEDynamic.Create(AOwner: TXCollection);
begin
  inherited;
  FElements := TgxODEElements.Create(Self);
  FJointRegister := TList.Create;
  FEnabled := True;
end;

destructor TgxODEDynamic.Destroy;
begin
  FElements.Free;
  FJointRegister.Free;
  inherited;
end;

procedure TgxODEDynamic.Render(var rci: TgxRenderContextInfo);
var
  Mat: TMatrix4f;
begin
  if Assigned(Owner.Owner) then
  begin
    rci.PipelineTransformation.Push;
    Mat := TgxBaseSceneObject(Owner.Owner).AbsoluteMatrix;
    rci.PipelineTransformation.ModelMatrix^ := Mat;
  end;

  Elements.Render(rci);

  if Assigned(Owner.Owner) then
    rci.PipelineTransformation.Pop;
end;

class function TgxODEDynamic.FriendlyName: String;
begin
  Result := 'ODE Dynamic';
end;

procedure TgxODEDynamic.Initialize;
var
  i: Integer;
begin
  if (not Assigned(Manager)) or Assigned(FBody) or (FInitialized) then
    Exit;
  if not Assigned(Manager.World) then
    Exit;

  FBody := dBodyCreate(Manager.World);
  AlignBodyToMatrix(OwnerBaseSceneObject.AbsoluteMatrix);
  dMassSetZero(FMass);
  FElements.Initialize;
  CalculateMass;
  CalibrateCenterOfMass;
  if (FMass.Mass > 0) and (FBody <> nil) then // mrqzzz
    dBodySetMass(FBody, @FMass);
  Enabled := FEnabled;

  for i := 0 to FJointRegister.Count - 1 do
    TgxODEJointBase(FJointRegister[i]).Attach;

  inherited;
end;

procedure TgxODEDynamic.Finalize;
var
  i: Integer;
begin
  if not FInitialized then
    Exit;
  FElements.Finalize;
  if Assigned(FBody) then
  begin
    dBodyDestroy(FBody);
    FBody := nil;
  end;
  dMassSetZero(FMass);
  for i := 0 to FJointRegister.Count - 1 do
    TgxODEJointBase(FJointRegister[i]).Attach;
  inherited;
end;

procedure TgxODEDynamic.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(1); // Archive version
    FElements.WriteToFiler(writer);
    writer.WriteBoolean(FEnabled);
  end;
end;

procedure TgxODEDynamic.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  inherited;
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert((archiveVersion >= 0) and (archiveVersion <= 1)); // Archive version

    // version 0
    FElements.ReadFromFiler(reader);

    // version 1
    if archiveVersion >= 1 then
    begin
      FEnabled := ReadBoolean;
    end;
  end;
end;

procedure TgxODEDynamic.RegisterJoint(Joint: TgxODEJointBase);
begin
  if FJointRegister.IndexOf(Joint) = -1 then
    FJointRegister.Add(Joint);
end;

procedure TgxODEDynamic.UnregisterJoint(Joint: TgxODEJointBase);
begin
  if FJointRegister.IndexOf(Joint) > -1 then
    FJointRegister.Remove(Joint);
end;

function TgxODEDynamic.AddNewElement(AChild: TgxODEElementClass): TgxODEElementBase;
var
  calcmass: TdMass;
begin
  Result := AChild.Create(FElements);
  // FElements.Add(Result);
  Result.Initialize;
  calcmass := CalculateMass;
  if (calcmass.Mass > 0) and (FBody <> nil) then // mrqzzz
    dBodySetMass(FBody, @calcmass);
end;

procedure TgxODEDynamic.AlignObject;
var
  Pos: PdVector3;
  R: PdMatrix3;
  m: TMatrix4f;
begin
  Pos := dBodyGetPosition(Body);
  R := dBodyGetRotation(Body);
  ODERToSceneMatrix(m, R^, Pos^);
  if OwnerBaseSceneObject.Parent is TgxBaseSceneObject then
    m := MatrixMultiply(m, OwnerBaseSceneObject.Parent.InvAbsoluteMatrix);
  OwnerBaseSceneObject.Matrix^ := m;
end;

procedure TgxODEDynamic.AlignBodyToMatrix(Mat: TMatrix4f);
var
  R: TdMatrix3;
begin
  if not Assigned(FBody) then
    Exit;
  R[0] := Mat.X.X;
  R[1] := Mat.Y.X;
  R[2] := Mat.Z.X;
  R[3] := 0;
  R[4] := Mat.X.Y;
  R[5] := Mat.Y.Y;
  R[6] := Mat.Z.Y;
  R[7] := 0;
  R[8] := Mat.X.Z;
  R[9] := Mat.Y.Z;
  R[10] := Mat.Z.Z;
  R[11] := 0;
  dBodySetRotation(FBody, R);
  dBodySetPosition(FBody, Mat.W.X, Mat.W.Y, Mat.W.Z);
end;

// CalculateMass
//
function TgxODEDynamic.CalculateMass: TdMass;
var
  i: Integer;
  m: TdMass;
begin
  dMassSetZero(FMass);
  for i := 0 to Elements.Count - 1 do
  begin
    m := TgxODEElementBase(Elements[i]).CalculateMass;
    dMassAdd(FMass, m);
  end;
  Result := FMass;
end;

procedure TgxODEDynamic.CalibrateCenterOfMass;
var
  Pos: TAffineVector;
begin
  SetAffineVector(Pos, FMass.c[0], FMass.c[1], FMass.c[2]);
  NegateVector(Pos);
  dMassTranslate(FMass, Pos.X, Pos.Y, Pos.Z);
end;

function TgxODEDynamic.GetMass: TdMass;
begin
  dBodyGetMass(FBody, FMass);
  Result := FMass;
end;

procedure TgxODEDynamic.SetMass(const Value: TdMass);
begin
  FMass := Value;
  if FMass.Mass > 0 then
    dBodySetMass(FBody, @FMass);
end;

class function TgxODEDynamic.UniqueItem: Boolean;
begin
  Result := True;
end;

procedure TgxODEDynamic.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if Assigned(FBody) then
  begin
    if FEnabled then
      dBodyEnable(FBody)
    else
      dBodyDisable(FBody);
  end;
end;

function TgxODEDynamic.GetEnabled: Boolean;
begin
  if Assigned(FBody) then
    FEnabled := (dBodyIsEnabled(FBody) = 1);
  Result := FEnabled;
end;

procedure TgxODEDynamic.AddForce(Force: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForce(FBody, Force.X, Force.Y, Force.Z);
end;

procedure TgxODEDynamic.AddForceAtPos(Force, Pos: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtPos(FBody, Force.X, Force.Y, Force.Z, Pos.X, Pos.Y, Pos.Z);
end;

procedure TgxODEDynamic.AddForceAtRelPos(Force, Pos: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtRelPos(FBody, Force.X, Force.Y, Force.Z, Pos.X,
      Pos.Y, Pos.Z);
end;

procedure TgxODEDynamic.AddRelForce(Force: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelForce(FBody, Force.X, Force.Y, Force.Z);
end;

procedure TgxODEDynamic.AddRelForceAtPos(Force, Pos: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddForceAtPos(FBody, Force.X, Force.Y, Force.Z, Pos.X, Pos.Y, Pos.Z);
end;

procedure TgxODEDynamic.AddRelForceAtRelPos(Force, Pos: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelForceAtRelPos(FBody, Force.X, Force.Y, Force.Z, Pos.X, Pos.Y, Pos.Z);
end;

procedure TgxODEDynamic.AddTorque(Torque: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddTorque(FBody, Torque.X, Torque.Y, Torque.Z);
end;

procedure TgxODEDynamic.AddRelTorque(Torque: TAffineVector);
begin
  if Assigned(FBody) then
    dBodyAddRelTorque(FBody, Torque.X, Torque.Y, Torque.Z);
end;


// ---------------
// --------------- TgxODEStatic ---------------
// ---------------

constructor TgxODEStatic.Create(AOwner: TXCollection);
begin
  inherited;
  FElements := TgxODEElements.Create(Self);
end;

destructor TgxODEStatic.Destroy;
begin
  FElements.Free;
  inherited;
end;

procedure TgxODEStatic.Render(var rci: TgxRenderContextInfo);
var
  Mat: TMatrix4f;
begin
  if Assigned(Owner.Owner) then
  begin
    rci.PipelineTransformation.Push;
    Mat := TgxBaseSceneObject(Owner.Owner).AbsoluteMatrix;
    rci.PipelineTransformation.ModelMatrix^ := Mat;
  end;

  Elements.Render(rci);

  if Assigned(Owner.Owner) then
    rci.PipelineTransformation.Pop;
end;

class function TgxODEStatic.FriendlyName: String;
begin
  Result := 'ODE Static';
end;

class function TgxODEStatic.UniqueItem: Boolean;
begin
  Result := True;
end;

procedure TgxODEStatic.Initialize;
begin
  if (not Assigned(Manager)) or (FInitialized) then
    Exit;
  if not Assigned(Manager.Space) then
    Exit;

  FElements.Initialize;

  inherited;
end;

procedure TgxODEStatic.Finalize;
begin
  if not FInitialized then
    Exit;
  FElements.Finalize;

  inherited;
end;

procedure TgxODEStatic.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FElements.WriteToFiler(writer);
  end;
end;

procedure TgxODEStatic.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FElements.ReadFromFiler(reader);
  end;
end;

function TgxODEStatic.AddNewElement(AChild: TgxODEElementClass): TgxODEElementBase;
begin
  Result := nil;
  if not Assigned(Manager) then
    Exit;
  Result := AChild.Create(FElements);
  FElements.Add(Result);
  Result.Initialize;
end;

procedure TgxODEStatic.AlignElements;
var
  i: Integer;
begin
  if not FInitialized then
    Exit;

  for i := 0 to FElements.Count - 1 do
    TgxODEElementBase(FElements[i]).AlignGeomElementToMatrix
      (TgxODEElementBase(FElements[i]).AbsoluteMatrix);
end;


// ---------------
// --------------- TgxODEElements ---------------
// ---------------

destructor TgxODEElements.Destroy;
begin
  Finalize;
  inherited;
end;

function TgxODEElements.GetElement(index: Integer): TgxODEElementBase;
begin
  Result := TgxODEElementBase(Items[index]);
end;

class function TgxODEElements.ItemsClass: TXCollectionItemClass;
begin
  Result := TgxODEElementBase;
end;

procedure TgxODEElements.Initialize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TgxODEElementBase(Items[i]).Initialize;
end;

procedure TgxODEElements.Finalize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TgxODEElementBase(Items[i]).Finalize;
end;

procedure TgxODEElements.Render(var rci: TgxRenderContextInfo);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TgxODEElementBase(Items[i]).Render(rci);
end;

procedure TgxODEElements.NotifyChange(Sender: TObject);
begin
  if Assigned(Owner) then
    if Owner is TgxODEBehaviour then
      TgxODEBehaviour(Owner).NotifyChange(Self);
end;


// ---------------
// --------------- TgxODEElementBase ---------------
// ---------------

constructor TgxODEElementBase.Create(AOwner: TXCollection);
begin
  inherited;
  FPosition := TgxCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FPosition.OnNotifyChange := NotifyChange;
  FDirection := TgxCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FDirection.OnNotifyChange := CoordinateChanged;
  FUp := TgxCoordinates.CreateInitialized(Self, YHmgVector, csVector);
  FUp.OnNotifyChange := CoordinateChanged;
  FDensity := 1;
  FInitialized := False;
  FDynamic := (Owner.Owner is TgxODEDynamic);
  FLocalMatrix := IdentityHMGMatrix;
  FIsCalculating := False;
end;

destructor TgxODEElementBase.Destroy;
begin
  if FInitialized then
    Finalize;
  FPosition.Free;
  FDirection.Free;
  FUp.Free;
  inherited;
end;

procedure TgxODEElementBase.Render(var rci: TgxRenderContextInfo);
begin
  // Override this procedure with element drawing OpenGL code
end;

procedure TgxODEElementBase.Initialize;
var
  Manager: TgxODEManager;
  Body: PdxBody;
begin
  Manager := nil;
  Body := nil;

  if Owner.Owner is TgxODEBehaviour then
    Manager := TgxODEBehaviour(Owner.Owner).Manager;
  if not Assigned(Manager) then
    Exit;

  if FDynamic then
  begin
    if Owner.Owner is TgxODEDynamic then
      Body := TgxODEDynamic(Owner.Owner).Body;
    if not Assigned(Body) then
      Exit;
  end;

  if not Assigned(Manager.World) then
    Exit;

  if FDynamic then
  begin
    FGeomTransform := dCreateGeomTransform(Manager.Space);
    dGeomSetBody(FGeomTransform, Body);
    dGeomTransformSetCleanup(FGeomTransform, 0);
    dGeomTransformSetGeom(FGeomTransform, FGeomElement);
    dGeomSetData(FGeomTransform, Owner.Owner);
    AlignGeomElementToMatrix(FLocalMatrix);
  end
  else
  begin
    dSpaceAdd(Manager.Space, FGeomElement);
    dGeomSetData(FGeomElement, Owner.Owner);
    AlignGeomElementToMatrix(AbsoluteMatrix);
  end;

  FInitialized := True;
end;

procedure TgxODEElementBase.Finalize;
begin
  if not FInitialized then
    Exit;
  if Assigned(FGeomTransform) then
  begin
    dGeomDestroy(FGeomTransform);
    FGeomTransform := nil;
  end;
  if Assigned(FGeomElement) then
  begin
    dGeomDestroy(FGeomElement);
    FGeomElement := nil;
  end;
  FInitialized := False;
end;

procedure TgxODEElementBase.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FPosition.WriteToFiler(writer);
    FDirection.WriteToFiler(writer);
    FUp.WriteToFiler(writer);
    WriteFloat(Density);
  end;
end;

procedure TgxODEElementBase.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FPosition.ReadFromFiler(reader);
    FDirection.ReadFromFiler(reader);
    FUp.ReadFromFiler(reader);
    Density := ReadFloat;
  end;
  NotifyChange(Self);
end;

function TgxODEElementBase.AbsoluteMatrix: TMatrix4f;
var
  Mat: TMatrix4f;
begin
  Mat := IdentityHMGMatrix;
  if Owner.Owner is TgxODEBehaviour then
    Mat := TgxODEBehaviour(Owner.Owner).AbsoluteMatrix;
  Result := MatrixMultiply(Mat, FLocalMatrix);
end;

function TgxODEElementBase.AbsolutePosition: TAffineVector;
begin
  Result := AffineVectorMake(AbsoluteMatrix.W);
end;

procedure TgxODEElementBase.AlignGeomElementToMatrix(Mat: TMatrix4f);
var
  R: TdMatrix3;
begin
  if not Assigned(FGeomElement) then
    Exit;
  dGeomSetPosition(FGeomElement, Mat.W.X, Mat.W.Y, Mat.W.Z);
  R[0] := Mat.X.X;
  R[1] := Mat.Y.X;
  R[2] := Mat.Z.X;
  R[3] := 0;
  R[4] := Mat.X.Y;
  R[5] := Mat.Y.Y;
  R[6] := Mat.Z.Y;
  R[7] := 0;
  R[8] := Mat.X.Z;
  R[9] := Mat.Y.Z;
  R[10] := Mat.Z.Z;
  R[11] := 0;
  dGeomSetRotation(FGeomElement, R);
  FRealignODE := False;
end;

procedure TgxODEElementBase.SetGeomElement(aGeom: PdxGeom);
begin
  FGeomElement := aGeom;
end;

function TgxODEElementBase.IsODEInitialized: Boolean;
var
  Manager: TgxODEManager;
begin
  Result := False;
  Manager := nil;
  if Owner.Owner is TgxODEBehaviour then
    Manager := TgxODEBehaviour(Owner.Owner).Manager;
  if not Assigned(Manager) then
    Exit;
  Result := Assigned(Manager.Space);
end;

function TgxODEElementBase.CalculateMass: TdMass;
var
  R: TdMatrix3;
begin
  R[0] := FLocalMatrix.X.X;
  R[1] := FLocalMatrix.Y.X;
  R[2] := FLocalMatrix.Z.X;
  R[3] := 0;
  R[4] := FLocalMatrix.X.Y;
  R[5] := FLocalMatrix.Y.Y;
  R[6] := FLocalMatrix.Z.Y;
  R[7] := 0;
  R[8] := FLocalMatrix.X.Z;
  R[9] := FLocalMatrix.Y.Z;
  R[10] := FLocalMatrix.Z.Z;
  R[11] := 0;
  dMassRotate(FMass, R);
  dMassTranslate(FMass, FLocalMatrix.W.X, FLocalMatrix.W.Y, FLocalMatrix.W.Z);
  Result := FMass;
end;

procedure TgxODEElementBase.CoordinateChanged(Sender: TObject);
var
  rightVector: TVector4f;
begin
  if FIsCalculating then
    Exit;
  FIsCalculating := True;
  try
    if Sender = FDirection then
    begin
      if FDirection.VectorLength = 0 then
        FDirection.DirectVector := ZHmgVector;
      FDirection.Normalize;
      rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
      if VectorLength(rightVector) < 1E-5 then
      begin
        rightVector := VectorCrossProduct(ZHmgVector, FUp.AsVector);
        if VectorLength(rightVector) < 1E-5 then
          rightVector := VectorCrossProduct(XHmgVector, FUp.AsVector);
      end;
      FUp.DirectVector := VectorCrossProduct(rightVector, FDirection.AsVector);
      FUp.Normalize;

    end
    else if Sender = FUp then
    begin
      if FUp.VectorLength = 0 then
        FUp.DirectVector := YHmgVector;
      FUp.Normalize;
      rightVector := VectorCrossProduct(FDirection.AsVector, FUp.AsVector);
      if VectorLength(rightVector) < 1E-5 then
      begin
        rightVector := VectorCrossProduct(ZHmgVector, FUp.AsVector);
        if VectorLength(rightVector) < 1E-5 then
          rightVector := VectorCrossProduct(XHmgVector, FUp.AsVector);
      end;
      FDirection.DirectVector := VectorCrossProduct(FUp.AsVector, rightVector);
      FDirection.Normalize;
    end;
    NotifyChange(Self);
  finally
    FIsCalculating := False;
  end;
end;

procedure TgxODEElementBase.NotifyChange(Sender: TObject);
begin
  RebuildMatrix;
  ODERebuild;
end;

function TgxODEElementBase.GetMatrix: TMatrix4f;
begin
  Result := FLocalMatrix;
end;

procedure TgxODEElementBase.RebuildMatrix;
begin
  VectorCrossProduct(FUp.AsVector, FDirection.AsVector, FLocalMatrix.X);
  SetVector(FLocalMatrix.Y, FUp.AsVector);
  SetVector(FLocalMatrix.Z, FDirection.AsVector);
  SetVector(FLocalMatrix.W, FPosition.AsVector);
end;

procedure TgxODEElementBase.RebuildVectors;
begin
  FUp.SetVector(FLocalMatrix.Y.X, FLocalMatrix.Y.Y, FLocalMatrix.Y.Z);
  FDirection.SetVector(FLocalMatrix.Z.X, FLocalMatrix.Z.Y, FLocalMatrix.Z.Z);
  FPosition.SetPoint(FLocalMatrix.W.X, FLocalMatrix.W.Y, FLocalMatrix.W.Z);
end;

procedure TgxODEElementBase.SetDensity(const Value: TdReal);
begin
  FDensity := Value;
end;

procedure TgxODEElementBase.SetMatrix(const Value: TMatrix4f);
begin
  FLocalMatrix := Value;
  RebuildVectors;
  ODERebuild;
end;

procedure TgxODEElementBase.ODERebuild;
begin
  if Initialized then
  begin
    if FDynamic then
    begin
      CalculateMass;
      AlignGeomElementToMatrix(FLocalMatrix);
    end
    else
      AlignGeomElementToMatrix(AbsoluteMatrix);
  end;
  if Assigned(Owner) then
    TgxODEElements(Owner).NotifyChange(Self);
end;

procedure TgxODEElementBase.SetPosition(const Value: TgxCoordinates);
begin
  FPosition.Assign(Value);
end;

procedure TgxODEElementBase.SetDirection(const Value: TgxCoordinates);
begin
  FDirection.Assign(Value);
end;

procedure TgxODEElementBase.SetUp(const Value: TgxCoordinates);
begin
  FUp.Assign(Value);
end;


// ---------------
// --------------- TgxODEElementBox ---------------
// ---------------

procedure TgxODEElementBox.Render(var rci: TgxRenderContextInfo);
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);

  glBegin(GL_LINE_LOOP);
  glVertex3f(-FBoxWidth / 2, -FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, -FBoxHeight / 2, FBoxDepth / 2);
  glEnd;

  glBegin(GL_LINE_LOOP);
  glVertex3f(FBoxWidth / 2, FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, -FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, -FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, FBoxHeight / 2, -FBoxDepth / 2);
  glEnd;

  glBegin(GL_LINES);
  glVertex3f(-FBoxWidth / 2, FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, -FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, -FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, -FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, -FBoxHeight / 2, -FBoxDepth / 2);
  glVertex3f(-FBoxWidth / 2, FBoxHeight / 2, FBoxDepth / 2);
  glVertex3f(FBoxWidth / 2, FBoxHeight / 2, FBoxDepth / 2);
  glEnd;

  glPopMatrix;
end;

constructor TgxODEElementBox.Create(AOwner: TXCollection);
begin
  inherited;
  BoxWidth := 1;
  BoxHeight := 1;
  BoxDepth := 1;
end;

procedure TgxODEElementBox.Initialize;
begin
  if FInitialized then
    Exit;
  if not IsODEInitialized then
    Exit;

  FGeomElement := dCreateBox(nil, FBoxWidth, FBoxHeight, FBoxDepth);
  inherited;
end;

procedure TgxODEElementBox.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(BoxWidth);
    WriteFloat(BoxHeight);
    WriteFloat(BoxDepth);
  end;
end;

procedure TgxODEElementBox.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    BoxWidth := ReadFloat;
    BoxHeight := ReadFloat;
    BoxDepth := ReadFloat;
  end;
end;

class function TgxODEElementBox.FriendlyName: String;
begin
  Result := 'Box';
end;

class function TgxODEElementBox.FriendlyDescription: String;
begin
  Result := 'The ODE box element implementation';
end;

class function TgxODEElementBox.ItemCategory: String;
begin
  Result := 'Primitives';
end;

function TgxODEElementBox.CalculateMass: TdMass;
begin
  dMassSetBox(FMass, FDensity, BoxWidth, BoxHeight, BoxDepth);
  Result := inherited CalculateMass;
end;

function TgxODEElementBox.GetBoxWidth: TdReal;
var
  vec: TdVector3;
begin
  if Assigned(FGeomTransform) then
  begin
    dGeomBoxGetLengths(Geom, vec);
    FBoxWidth := vec[0];
  end;
  Result := FBoxWidth;
end;

function TgxODEElementBox.GetBoxHeight: TdReal;
var
  vec: TdVector3;
begin
  if Assigned(FGeomTransform) then
  begin
    dGeomBoxGetLengths(Geom, vec);
    FBoxHeight := vec[1];
  end;
  Result := FBoxHeight;
end;

function TgxODEElementBox.GetBoxDepth: TdReal;
var
  vec: TdVector3;
begin
  if Assigned(FGeomTransform) then
  begin
    dGeomBoxGetLengths(Geom, vec);
    FBoxDepth := vec[2];
  end;
  Result := FBoxDepth;
end;

procedure TgxODEElementBox.ODERebuild;
begin
  if Assigned(Geom) then
    dGeomBoxSetLengths(Geom, FBoxWidth, FBoxHeight, FBoxDepth);
  inherited;
end;

procedure TgxODEElementBox.SetBoxWidth(const Value: TdReal);
begin
  FBoxWidth := Value;
  ODERebuild;
end;

procedure TgxODEElementBox.SetBoxHeight(const Value: TdReal);
begin
  FBoxHeight := Value;
  ODERebuild;
end;

procedure TgxODEElementBox.SetBoxDepth(const Value: TdReal);
begin
  FBoxDepth := Value;
  ODERebuild;
end;


// ---------------
// --------------- TgxODEElementSphere ---------------
// ---------------

procedure TgxODEElementSphere.Render(var rci: TgxRenderContextInfo);
var
  AngTop, AngBottom, AngStart, AngStop, StepV, StepH: double;
  SinP, CosP, SinP2, CosP2, SinT, CosT, Phi, Phi2, Theta: double;
  FTop, FBottom, FStart, FStop: Single;
  i, J, FSlices, FStacks: Integer;
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);
  glScalef(Radius, Radius, Radius);

  FTop := 90;
  FBottom := -90;
  FStart := 0;
  FStop := 360;
  FSlices := 16;
  FStacks := 16;

  AngTop := DegToRadian(FTop);
  AngBottom := DegToRadian(FBottom);
  AngStart := DegToRadian(FStart);
  AngStop := DegToRadian(FStop);
  StepH := (AngStop - AngStart) / FSlices;
  StepV := (AngTop - AngBottom) / FStacks;

  Phi := AngTop;
  Phi2 := Phi - StepV;
  for J := 0 to FStacks - 1 do
  begin
    Theta := AngStart;
    SinCosine(Phi, SinP, CosP);
    SinCosine(Phi2, SinP2, CosP2);

    glBegin(GL_LINE_LOOP);
    for i := 0 to FSlices do
    begin
      SinCosine(Theta, SinT, CosT);
      glVertex3f(CosP * SinT, SinP, CosP * CosT);
      Theta := Theta + StepH;
    end;
    glEnd;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  Phi := AngTop;
  Phi2 := Phi - StepV;
  for J := 0 to FStacks - 1 do
  begin
    Theta := AngStart;
    SinCos(Phi, SinP, CosP);
    SinCos(Phi2, SinP2, CosP2);

    glBegin(GL_LINE_LOOP);
    for i := 0 to FSlices do
    begin
      SinCos(Theta, SinT, CosT);
      glVertex3f(SinP, CosP * SinT, CosP * CosT);
      Theta := Theta + StepH;
    end;
    glEnd;
    Phi := Phi2;
    Phi2 := Phi2 - StepV;
  end;

  glPopMatrix;
end;

constructor TgxODEElementSphere.Create(AOwner: TXCollection);
begin
  inherited;
  FRadius := 0.5;
end;

procedure TgxODEElementSphere.Initialize;
begin
  if FInitialized then
    Exit;
  if not IsODEInitialized then
    Exit;

  FGeomElement := dCreateSphere(nil, FRadius);
  inherited;
end;

procedure TgxODEElementSphere.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(Radius);
  end;
end;

procedure TgxODEElementSphere.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    Radius := ReadFloat;
  end;
end;

class function TgxODEElementSphere.FriendlyName: String;
begin
  Result := 'Sphere';
end;

class function TgxODEElementSphere.FriendlyDescription: String;
begin
  Result := 'The ODE sphere element implementation';
end;

class function TgxODEElementSphere.ItemCategory: String;
begin
  Result := 'Primitives';
end;

function TgxODEElementSphere.CalculateMass: TdMass;
begin
  dMassSetSphere(FMass, FDensity, Radius);
  Result := inherited CalculateMass;
end;

function TgxODEElementSphere.GetRadius: TdReal;
begin
  if Assigned(FGeomElement) then
    FRadius := dGeomSphereGetRadius(FGeomElement);
  Result := FRadius;
end;

procedure TgxODEElementSphere.ODERebuild;
begin
  if Assigned(Geom) then
  begin
    dGeomSphereSetRadius(Geom, FRadius);
  end;
  inherited;
end;

procedure TgxODEElementSphere.SetRadius(const Value: TdReal);
begin
  FRadius := Value;
  ODERebuild;
end;


// ---------------
// --------------- TgxODEElementCapsule ---------------
// ---------------

procedure TgxODEElementCapsule.Render(var rci: TgxRenderContextInfo);
var
  i, J, Stacks, Slices: Integer;
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);

  Stacks := 8;
  Slices := 16;

  // Middle horizontal circles
  for J := 0 to Stacks - 1 do
  begin
    glBegin(GL_LINE_LOOP);
    for i := 0 to Slices - 1 do
      glVertex3f(FRadius * sin(2 * i * PI / Slices),
        FRadius * cos(2 * i * PI / Slices), -FLength / 2 + FLength * J /
        (Stacks - 1));
    glEnd;
  end;

  // Middle vertical lines
  glBegin(GL_LINES);
  for i := 0 to (Slices div 2) - 1 do
  begin
    glVertex3f(FRadius * sin(2 * i * PI / Slices),
      FRadius * cos(2 * i * PI / Slices), -FLength / 2);
    glVertex3f(FRadius * sin(2 * i * PI / Slices),
      FRadius * cos(2 * i * PI / Slices), FLength / 2);
    glVertex3f(-FRadius * sin(2 * i * PI / Slices),
      -FRadius * cos(2 * i * PI / Slices), -FLength / 2);
    glVertex3f(-FRadius * sin(2 * i * PI / Slices),
      -FRadius * cos(2 * i * PI / Slices), FLength / 2);
  end;
  glEnd;

  // Cap XZ half-circles
  glPushMatrix;
  for J := 0 to (Slices div 2) - 1 do
  begin
    // Top
    glBegin(GL_LINE_STRIP);
    for i := 0 to Slices do
      glVertex3f(FRadius * cos(i * PI / Slices), 0,
        FRadius * sin(i * PI / Slices) + FLength / 2);
    glEnd;

    // Bottom
    glBegin(GL_LINE_STRIP);
    for i := 0 to Slices do
      glVertex3f(FRadius * cos(i * PI / Slices), 0,
        -(FRadius * sin(i * PI / Slices) + FLength / 2));
    glEnd;
    glRotatef(360 / Slices, 0, 0, 1);
  end;
  glPopMatrix;
  glPopMatrix;
end;

constructor TgxODEElementCapsule.Create(AOwner: TXCollection);
begin
  inherited;
  FRadius := 0.5;
  FLength := 1;
end;

procedure TgxODEElementCapsule.Initialize;
begin
  if FInitialized then
    Exit;
  if not IsODEInitialized then
    Exit;

  FGeomElement := dCreateCapsule(nil, FRadius, FLength);
  inherited;
end;

procedure TgxODEElementCapsule.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(Radius);
    WriteFloat(Length);
  end;
end;

procedure TgxODEElementCapsule.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    Radius := ReadFloat;
    Length := ReadFloat;
  end;
end;

class function TgxODEElementCapsule.FriendlyName: String;
begin
  Result := 'Capsule';
end;

class function TgxODEElementCapsule.FriendlyDescription: String;
begin
  Result := 'The ODE capped cylinder element implementation';
end;

class function TgxODEElementCapsule.ItemCategory: String;
begin
  Result := 'Primitives';
end;

function TgxODEElementCapsule.CalculateMass: TdMass;
begin
  dMassSetCapsule(FMass, FDensity, 3, FRadius, FLength);
  Result := inherited CalculateMass;
end;

function TgxODEElementCapsule.GetRadius: TdReal;
var
  rad, len: TdReal;
begin
  if Assigned(FGeomElement) then
  begin
    dGeomCapsuleGetParams(Geom, rad, len);
    FRadius := rad;
  end;
  Result := FRadius;
end;

function TgxODEElementCapsule.GetLength: TdReal;
var
  rad, len: TdReal;
begin
  if Assigned(FGeomElement) then
  begin
    dGeomCapsuleGetParams(Geom, rad, len);
    FLength := len;
  end;
  Result := FLength;
end;

procedure TgxODEElementCapsule.ODERebuild;
begin
  if Assigned(Geom) then
    dGeomCapsuleSetParams(Geom, FRadius, FLength);
  inherited;
end;

procedure TgxODEElementCapsule.SetRadius(const Value: TdReal);
begin
  FRadius := Value;
  ODERebuild;
end;

procedure TgxODEElementCapsule.SetLength(const Value: TdReal);
begin
  FLength := Value;
  ODERebuild;
end;


// ---------------
// --------------- TgxODEElementCylinder ---------------
// ---------------

procedure TgxODEElementCylinder.Render(var rci: TgxRenderContextInfo);
var
  i, J, Stacks, Slices: Integer;
begin
  glPushMatrix;

  glMultMatrixf(@FLocalMatrix);

  Stacks := 8;
  Slices := 16;

  // Middle horizontal circles
  for J := 0 to Stacks - 1 do
  begin
    glBegin(GL_LINE_LOOP);
    for i := 0 to Slices - 1 do
      glVertex3f(FRadius * sin(2 * i * PI / Slices), -FLength / 2 + FLength * J
        / (Stacks - 1), FRadius * cos(2 * i * PI / Slices));
    glEnd;
  end;

  // Middle vertical lines
  glBegin(GL_LINES);
  for i := 0 to (Slices div 2) - 1 do
  begin
    glVertex3f(FRadius * sin(2 * i * PI / Slices), -FLength / 2,
      FRadius * cos(2 * i * PI / Slices));
    glVertex3f(FRadius * sin(2 * i * PI / Slices), FLength / 2,
      FRadius * cos(2 * i * PI / Slices));
    glVertex3f(-FRadius * sin(2 * i * PI / Slices), -FLength / 2,
      -FRadius * cos(2 * i * PI / Slices));
    glVertex3f(-FRadius * sin(2 * i * PI / Slices), FLength / 2,
      -FRadius * cos(2 * i * PI / Slices));
  end;
  glEnd;

  // Caps
  glPushMatrix;
  for J := 0 to (Slices div 2) - 1 do
  begin
    glBegin(GL_LINES);
    glVertex3f(-FRadius, FLength / 2, 0);
    glVertex3f(FRadius, FLength / 2, 0);
    glVertex3f(-FRadius, -FLength / 2, 0);
    glVertex3f(FRadius, -FLength / 2, 0);
    glEnd;
    glRotatef(360 / Slices, 0, 1, 0);
  end;
  glPopMatrix;

  glPopMatrix;
end;

constructor TgxODEElementCylinder.Create(AOwner: TXCollection);
begin
  inherited;
  FRadius := 0.5;
  FLength := 1;
end;

procedure TgxODEElementCylinder.Initialize;
begin
  if FInitialized then
    Exit;
  if not IsODEInitialized then
    Exit;

  FGeomElement := dCreateCylinder(nil, FRadius, FLength);
  inherited;
end;

procedure TgxODEElementCylinder.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(Radius);
    WriteFloat(Length);
  end;
end;

procedure TgxODEElementCylinder.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    Radius := ReadFloat;
    Length := ReadFloat;
  end;
end;

class function TgxODEElementCylinder.FriendlyName: String;
begin
  Result := 'Cylinder';
end;

class function TgxODEElementCylinder.FriendlyDescription: String;
begin
  Result := 'The ODE cylinder element implementation';
end;

class function TgxODEElementCylinder.ItemCategory: String;
begin
  Result := 'Primitives';
end;

function TgxODEElementCylinder.CalculateMass: TdMass;
begin
  dMassSetCylinder(FMass, FDensity, 3, FRadius, FLength);
  Result := inherited CalculateMass;
end;

function TgxODEElementCylinder.GetRadius: TdReal;
var
  rad, len: TdReal;
begin
  if Assigned(FGeomElement) then
  begin
    dGeomCylinderGetParams(Geom, rad, len);
    FRadius := rad;
  end;
  Result := FRadius;
end;

function TgxODEElementCylinder.GetLength: TdReal;
var
  rad, len: TdReal;
begin
  if Assigned(FGeomElement) then
  begin
    dGeomCylinderGetParams(Geom, rad, len);
    FLength := len;
  end;
  Result := FLength;
end;

procedure TgxODEElementCylinder.ODERebuild;
begin
  if Assigned(Geom) then
    dGeomCylinderSetParams(Geom, FRadius, FLength);
  inherited;
end;

procedure TgxODEElementCylinder.SetRadius(const Value: TdReal);
begin
  FRadius := Value;
  ODERebuild;
end;

procedure TgxODEElementCylinder.SetLength(const Value: TdReal);
begin
  FLength := Value;
  ODERebuild;
end;



// ---------------
// --------------- TgxODEElementTriMesh ---------------
// ---------------

constructor TgxODEElementTriMesh.Create(AOwner: TXCollection);
begin
  inherited;
  FVertices := TgxAffineVectorList.Create;
  FIndices := TgxIntegerList.Create;
end;

destructor TgxODEElementTriMesh.Destroy;
begin
  FVertices.Free;
  FIndices.Free;
  inherited;
end;

procedure TgxODEElementTriMesh.Initialize;
begin
  if not IsODEInitialized then
    Exit;
  if FInitialized or not((FVertices.Count > 0) and (FIndices.Count > 0)) then
    Exit;

  FTriMeshData := dGeomTriMeshDataCreate;
  dGeomTriMeshDataBuildSingle(FTriMeshData, @FVertices.List[0],
    3 * SizeOf(Single), FVertices.Count, @FIndices.List[0], FIndices.Count,
    3 * SizeOf(Integer));
  FGeomElement := dCreateTriMesh(nil, FTriMeshData, nil, nil, nil);

  inherited;
end;

procedure TgxODEElementTriMesh.Finalize;
begin
  if not FInitialized then
    Exit;
  if Assigned(FTriMeshData) then
    dGeomTriMeshDataDestroy(FTriMeshData);
  inherited;
end;

procedure TgxODEElementTriMesh.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
  end;
end;

procedure TgxODEElementTriMesh.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
  end;
end;

class function TgxODEElementTriMesh.FriendlyName: String;
begin
  Result := 'Tri-Mesh';
end;

class function TgxODEElementTriMesh.FriendlyDescription: String;
begin
  Result := 'The ODE tri-mesh element implementation';
end;

class function TgxODEElementTriMesh.ItemCategory: String;
begin
  Result := 'Meshes';
end;

function TgxODEElementTriMesh.CalculateMass: TdMass;
var
  R: Single;
  min, max: TAffineVector;
begin
  if Vertices.Count > 0 then
  begin
    Vertices.GetExtents(min, max);
    R := MaxFloat(VectorLength(min), VectorLength(max));
  end
  else
    R := 1;
  dMassSetSphere(FMass, FDensity, R);
  Result := inherited CalculateMass;
end;

procedure TgxODEElementTriMesh.SetVertices(const Value: TgxAffineVectorList);
begin
  FVertices.Assign(Value);
  RefreshTriMeshData;
end;

procedure TgxODEElementTriMesh.SetIndices(const Value: TgxIntegerList);
begin
  FIndices.Assign(Value);
  RefreshTriMeshData;
end;

procedure TgxODEElementTriMesh.RefreshTriMeshData;
begin
  if FInitialized then
    Finalize;
  Initialize;
end;


// ---------------
// --------------- TgxODEElementPlane ---------------
// ---------------

procedure TgxODEElementPlane.Initialize;
begin
  if FInitialized then
    Exit;
  if not IsODEInitialized then
    Exit;

  FGeomElement := dCreatePlane(nil, 0, 0, 1, 0);
  inherited;
end;

procedure TgxODEElementPlane.WriteToFiler(writer: TWriter);
begin
  // ArchiveVersion 1, added inherited call
  writer.WriteInteger(1);
  inherited;
end;

procedure TgxODEElementPlane.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  archiveVersion := reader.ReadInteger;
  Assert(archiveVersion in [0 .. 1]);
  if archiveVersion >= 1 then
    inherited;
end;

class function TgxODEElementPlane.FriendlyName: String;
begin
  Result := 'Plane';
end;

class function TgxODEElementPlane.FriendlyDescription: String;
begin
  Result := 'The ODE plane element implementation';
end;

class function TgxODEElementPlane.ItemCategory: String;
begin
  Result := 'Primitives';
end;

class function TgxODEElementPlane.CanAddTo(collection: TXCollection): Boolean;
begin
  Result := False;
  if Assigned(TgxODEElements(collection).Owner) then
    if TgxODEElements(collection).Owner is TgxODEStatic then
      Result := True;
end;

procedure TgxODEElementPlane.AlignGeomElementToMatrix(Mat: TMatrix4f);
var
  d: Single;
begin
  if not Assigned(FGeomElement) then
    Exit;
  d := VectorDotProduct(Mat.Z, Mat.W);
  dGeomPlaneSetParams(FGeomElement, Mat.Z.X, Mat.Z.Y, Mat.Z.Z, d);
end;


// ---------------
// --------------- TgxODEJoints ---------------
// ---------------

class function TgxODEJoints.ItemsClass: TXCollectionItemClass;
begin
  Result := TgxODEJointBase;
end;

procedure TgxODEJoints.Initialize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Joint[i].Initialize;
end;

procedure TgxODEJoints.Finalize;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Joint[i].Finalize;
end;

function TgxODEJoints.GetJoint(index: Integer): TgxODEJointBase;
begin
  Result := TgxODEJointBase(Items[index]);
end;


// ---------------
// --------------- TgxODEJointList ---------------
// ---------------

constructor TgxODEJointList.Create(AOwner: TComponent);
begin
  inherited;
  FJoints := TgxODEJoints.Create(Self);
end;

destructor TgxODEJointList.Destroy;
begin
  FJoints.Free;
  inherited;
end;

procedure TgxODEJointList.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ODEJointsData', ReadJoints, WriteJoints,
    (Assigned(FJoints) and (FJoints.Count > 0)));
end;

procedure TgxODEJointList.WriteJoints(stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(stream, 16384);
  try
    Joints.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TgxODEJointList.ReadJoints(stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  try
    Joints.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TgxODEJointList.Loaded;
var
  i: Integer;
begin
  inherited;
  for i := 0 to FJoints.Count - 1 do
    FJoints[i].Loaded;
end;

procedure TgxODEJointList.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TgxBaseSceneObject) then
    for i := 0 to Joints.Count - 1 do
    begin
      if TgxBaseSceneObject(AComponent) = Joints[i].Object1 then
        Joints[i].Object1 := nil;
      if TgxBaseSceneObject(AComponent) = Joints[i].Object2 then
        Joints[i].Object2 := nil;
    end;
end;


// ---------------
// --------------- TgxODEJointBase ---------------
// ---------------

constructor TgxODEJointBase.Create(AOwner: TXCollection);
begin
  inherited;
  FJointID := nil;
  FEnabled := True;
  FInitialized := False;
end;


destructor TgxODEJointBase.Destroy;
begin
  Finalize;
  inherited;
end;

procedure TgxODEJointBase.Initialize;
begin
  if not IsODEInitialized then
    Exit;

  if Assigned(FObject1) then
    RegisterJointWithObject(FObject1);
  if Assigned(FObject2) then
    RegisterJointWithObject(FObject2);
  Attach;

  FInitialized := True;
end;

procedure TgxODEJointBase.Finalize;
begin
  if not Initialized then
    Exit;

  if Assigned(FObject1) then
    UnregisterJointWithObject(FObject1);
  if Assigned(FObject2) then
    UnregisterJointWithObject(FObject2);
  if FJointID <> nil then
    dJointDestroy(FJointID);

  FInitialized := False;
end;

procedure TgxODEJointBase.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
    if Assigned(FObject1) then
      WriteString(FObject1.GetNamePath)
    else
      WriteString('');
    if Assigned(FObject2) then
      WriteString(FObject2.GetNamePath)
    else
      WriteString('');
    WriteBoolean(FEnabled);
  end;
end;

procedure TgxODEJointBase.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FManagerName := ReadString;
    FObject1Name := ReadString;
    FObject2Name := ReadString;
    FEnabled := ReadBoolean;
  end;
end;

procedure TgxODEJointBase.Loaded;
begin
  DoLoaded;
end;

procedure TgxODEJointBase.RegisterJointWithObject(Obj: TgxBaseSceneObject);
var
  temp: TgxODEDynamic;
begin
  if Assigned(Obj) then
  begin
    temp := TgxODEDynamic(Obj.Behaviours.GetByClass(TgxODEDynamic));
    if Assigned(temp) then
      temp.RegisterJoint(Self);
  end;
end;

procedure TgxODEJointBase.UnregisterJointWithObject(Obj: TgxBaseSceneObject);
var
  temp: TgxODEDynamic;
begin
  if Assigned(Obj) then
  begin
    temp := TgxODEDynamic(Obj.Behaviours.GetByClass(TgxODEDynamic));
    if Assigned(temp) then
      temp.UnregisterJoint(Self);
  end;
end;

function TgxODEJointBase.IsODEInitialized: Boolean;
begin
  Result := False;
  if not Assigned(Manager) then
    Exit;
  Result := Assigned(Manager.World);
end;

procedure TgxODEJointBase.Attach;
var
  Body1, Body2: PdxBody;
begin
  if (FJointID = nil) or not FInitialized then
    Exit;

  if Enabled then
  begin
    Body1 := GetBodyFromGLXceneObject(FObject1);
    Body2 := GetBodyFromGLXceneObject(FObject2);
  end
  else
  begin
    Body1 := nil;
    Body2 := nil;
  end;

  if (joBothObjectsMustBeAssigned in JointOptions) then
    if not(Assigned(Body1) and Assigned(Body2)) then
      Exit;

  dJointAttach(FJointID, Body1, Body2);
  if Assigned(Body1) or Assigned(Body2) then
    StructureChanged;
end;

procedure TgxODEJointBase.SetManager(const Value: TgxODEManager);
begin
  if FManager <> Value then
  begin
    if Assigned(FManager) then
      if not(csDesigning in FManager.ComponentState) then
        Finalize;
    FManager := Value;
    if Assigned(FManager) then
      if not(csDesigning in FManager.ComponentState) then
        Initialize;
  end;
end;

procedure TgxODEJointBase.SetObject1(const Value: TgxBaseSceneObject);
begin
  if FObject1 <> Value then
  begin
    if Assigned(FObject1) then
      UnregisterJointWithObject(FObject1);
    FObject1 := Value;
    if Assigned(FObject1) then
      if IsGLODEObject(FObject1) then
        RegisterJointWithObject(FObject1)
      else
        FObject1 := nil;
    Attach;
  end;
end;

procedure TgxODEJointBase.SetObject2(const Value: TgxBaseSceneObject);
begin
  if FObject2 <> Value then
  begin
    if Assigned(FObject2) then
      UnregisterJointWithObject(FObject2);
    FObject2 := Value;
    if Assigned(FObject2) then
      if IsGLODEObject(FObject2) then
        RegisterJointWithObject(FObject2)
      else
        FObject2 := nil;
    Attach;
  end;
end;

procedure TgxODEJointBase.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if IsODEInitialized then
      Attach;
  end;
end;

procedure TgxODEJointBase.StructureChanged;
begin
  // nothing yet
end;

procedure TgxODEJointBase.DoLoaded;
var
  mng: TComponent;
  Obj: TgxBaseSceneObject;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TgxODEManager, FManagerName);
    if Assigned(mng) then
      Manager := TgxODEManager(mng);
    FManagerName := '';
  end;
  if FObject1Name <> '' then
  begin
    Obj := GetGLXceneObject(FObject1Name);
    if Assigned(Obj) then
      Object1 := Obj;
    FObject1Name := '';
  end;
  if FObject2Name <> '' then
  begin
    Obj := GetGLXceneObject(FObject2Name);
    if Assigned(Obj) then
      Object2 := Obj;
    FObject2Name := '';
  end;
  Attach;
end;

function TgxODEJointBase.IsAttached: Boolean;
var
  Body1, Body2: PdxBody;
begin
  Result := False;
  if JointID <> nil then
  begin
    Body1 := dJointGetBody(JointID, 0);
    Body2 := dJointGetBody(JointID, 1);
    if joBothObjectsMustBeAssigned in JointOptions then
      Result := Assigned(Body1) and Assigned(Body2)
    else
      Result := Assigned(Body1) or Assigned(Body2);
  end;
end;

procedure TgxODEJointBase.SetJointOptions(const Value: TJointOptions);
begin
  if Value <> FJointOptions then
  begin
    FJointOptions := Value;
    Attach;
  end;
end;


// ---------------
// --------------- TgxODEJointParams ---------------
// ---------------

constructor TgxODEJointParams.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TgxODEJointParams.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TgxODEJointParams.Assign(Source: TPersistent);
begin
  inherited;
  if not Assigned(Source) then
    Exit;
  if Source is TgxODEJointParams then
  begin
    LoStop := TgxODEJointParams(Source).LoStop;
    HiStop := TgxODEJointParams(Source).HiStop;
    Vel := TgxODEJointParams(Source).Vel;
    FMax := TgxODEJointParams(Source).FMax;
    FudgeFactor := TgxODEJointParams(Source).FudgeFactor;
    Bounce := TgxODEJointParams(Source).Bounce;
    CFM := TgxODEJointParams(Source).CFM;
    StopERP := TgxODEJointParams(Source).StopERP;
    StopCFM := TgxODEJointParams(Source).StopCFM;
    SuspensionERP := TgxODEJointParams(Source).SuspensionERP;
    SuspensionCFM := TgxODEJointParams(Source).SuspensionCFM;
  end;
end;

procedure TgxODEJointParams.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(LoStop);
    WriteFloat(HiStop);
    WriteFloat(Vel);
    WriteFloat(FMax);
    WriteFloat(FudgeFactor);
    WriteFloat(Bounce);
    WriteFloat(CFM);
    WriteFloat(StopERP);
    WriteFloat(StopCFM);
    WriteFloat(SuspensionERP);
    WriteFloat(SuspensionCFM);
  end;
end;

procedure TgxODEJointParams.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion = 0);

    LoStop := ReadFloat;
    HiStop := ReadFloat;
    Vel := ReadFloat;
    FMax := ReadFloat;
    FudgeFactor := ReadFloat;
    Bounce := ReadFloat;
    CFM := ReadFloat;
    StopERP := ReadFloat;
    StopCFM := ReadFloat;
    SuspensionERP := ReadFloat;
    SuspensionCFM := ReadFloat;
  end;
end;

function TgxODEJointParams.GetLoStop: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamLoStop1, FLoStop);
  Result := FLoStop;
end;

function TgxODEJointParams.GetHiStop: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamHiStop1, FHiStop);
  Result := FHiStop;
end;

function TgxODEJointParams.GetVel: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamVel1, FVel);
  Result := FVel;
end;

function TgxODEJointParams.GetFMax: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamFMax1, FFMax);
  Result := FFMax;
end;

function TgxODEJointParams.GetFudgeFactor: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamFudgeFactor1, FFudgeFactor);
  Result := FFudgeFactor;
end;

function TgxODEJointParams.GetBounce: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamBounce1, FBounce);
  Result := FBounce;
end;

function TgxODEJointParams.GetCFM: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamCFM1, FCFM);
  Result := FCFM;
end;

function TgxODEJointParams.GetStopERP: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamStopERP1, FStopERP);
  Result := FStopERP;
end;

function TgxODEJointParams.GetStopCFM: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamStopCFM1, FStopCFM);
  Result := FStopCFM;
end;

function TgxODEJointParams.GetSuspensionERP: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamSuspensionERP, FSuspensionERP);
  Result := FSuspensionERP;
end;

function TgxODEJointParams.GetSuspensionCFM: TdReal;
begin
  if Assigned(GetCallback) then
    GetCallback(dParamSuspensionCFM, FSuspensionCFM);
  Result := FSuspensionCFM;
end;

procedure TgxODEJointParams.SetLoStop(const Value: TdReal);
begin
  if Value <> FLoStop then
  begin
    FLoStop := Value;
    if Assigned(SetCallback) then
      FFlagLoStop := not SetCallback(dParamLoStop1, FLoStop)
    else
      FFlagLoStop := True;
  end;
end;

procedure TgxODEJointParams.SetHiStop(const Value: TdReal);
begin
  if Value <> FHiStop then
  begin
    FHiStop := Value;
    if Assigned(SetCallback) then
      FFlagHiStop := not SetCallback(dParamHiStop1, FHiStop)
    else
      FFlagHiStop := True;
  end;
end;

procedure TgxODEJointParams.SetVel(const Value: TdReal);
begin
  if Value <> FVel then
  begin
    FVel := Value;
    if Assigned(SetCallback) then
      FFlagVel := not SetCallback(dParamVel1, FVel)
    else
      FFlagVel := True;
  end;
end;

procedure TgxODEJointParams.SetFMax(const Value: TdReal);
begin
  if Value <> FFMax then
  begin
    FFMax := Value;
    if Assigned(SetCallback) then
      FFlagFMax := not SetCallback(dParamFMax1, FFMax)
    else
      FFlagFMax := True;
  end;
end;

procedure TgxODEJointParams.SetFudgeFactor(const Value: TdReal);
begin
  if Value <> FFudgeFactor then
  begin
    FFudgeFactor := Value;
    if Assigned(SetCallback) then
      FFlagFudgeFactor := not SetCallback(dParamFudgeFactor1, FFudgeFactor)
    else
      FFlagFudgeFactor := True;
  end;
end;

procedure TgxODEJointParams.SetBounce(const Value: TdReal);
begin
  if Value <> FBounce then
  begin
    FBounce := Value;
    if Assigned(SetCallback) then
      FFlagBounce := not SetCallback(dParamBounce1, FBounce)
    else
      FFlagBounce := True;
  end;
end;

procedure TgxODEJointParams.SetCFM(const Value: TdReal);
begin
  if Value <> FCFM then
  begin
    FCFM := Value;
    if Assigned(SetCallback) then
      FFlagCFM := not SetCallback(dParamCFM1, FCFM)
    else
      FFlagCFM := True;
  end;
end;

procedure TgxODEJointParams.SetStopERP(const Value: TdReal);
begin
  if Value <> FStopERP then
  begin
    FStopERP := Value;
    if Assigned(SetCallback) then
      FFlagStopERP := not SetCallback(dParamStopERP1, FStopERP)
    else
      FFlagStopERP := True;
  end;
end;

procedure TgxODEJointParams.SetStopCFM(const Value: TdReal);
begin
  if Value <> FStopCFM then
  begin
    FStopCFM := Value;
    if Assigned(SetCallback) then
      FFlagStopCFM := not SetCallback(dParamStopCFM1, FStopCFM)
    else
      FFlagStopCFM := True;
  end;
end;

procedure TgxODEJointParams.SetSuspensionERP(const Value: TdReal);
begin
  if Value <> FSuspensionERP then
  begin
    FSuspensionERP := Value;
    if Assigned(SetCallback) then
      FFlagSuspensionERP := not SetCallback(dParamSuspensionERP, FSuspensionERP)
    else
      FFlagSuspensionERP := True;
  end;
end;

procedure TgxODEJointParams.SetSuspensionCFM(const Value: TdReal);
begin
  if Value <> FSuspensionCFM then
  begin
    FSuspensionCFM := Value;
    if Assigned(SetCallback) then
      FFlagSuspensionCFM := not SetCallback(dParamSuspensionCFM, FSuspensionCFM)
    else
      FFlagSuspensionCFM := True;
  end;
end;

procedure TgxODEJointParams.ApplyFlagged;
begin
  if not Assigned(SetCallback) then
    Exit;
  if FFlagLoStop then
    SetCallback(dParamLoStop1, FLoStop);
  if FFlagHiStop then
    SetCallback(dParamHiStop1, FHiStop);
  if FFlagVel then
    SetCallback(dParamVel1, FVel);
  if FFlagFMax then
    SetCallback(dParamFMax1, FFMax);
  if FFlagFudgeFactor then
    SetCallback(dParamFudgeFactor1, FFudgeFactor);
  if FFlagBounce then
    SetCallback(dParamBounce1, FBounce);
  if FFlagCFM then
    SetCallback(dParamCFM1, FCFM);
  if FFlagStopERP then
    SetCallback(dParamStopERP1, FStopERP);
  if FFlagStopCFM then
    SetCallback(dParamStopCFM1, FStopCFM);
  if FFlagSuspensionERP then
    SetCallback(dParamSuspensionERP, FSuspensionERP);
  if FFlagSuspensionCFM then
    SetCallback(dParamSuspensionCFM, FSuspensionCFM);
end;


// ---------------
// --------------- TgxODEJointHinge ---------------
// ---------------

constructor TgxODEJointHinge.Create(AOwner: TXCollection);
begin
  inherited;
  FAnchor := TgxCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FAnchor.OnNotifyChange := AnchorChange;
  FAxis := TgxCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FAxis.OnNotifyChange := AxisChange;
  FAxisParams := TgxODEJointParams.Create(Self);
  FAxisParams.SetCallback := SetAxisParam;
  FAxisParams.GetCallback := GetAxisParam;

end;

destructor TgxODEJointHinge.Destroy;
begin
  FAnchor.Free;
  FAxis.Free;
  FAxisParams.Free;
  inherited;
end;

procedure TgxODEJointHinge.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateHinge(FManager.World, nil);
  inherited;
end;

procedure TgxODEJointHinge.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FAnchor.WriteToFiler(writer);
    FAxis.WriteToFiler(writer);
    FAxisParams.WriteToFiler(writer);
  end;
end;

procedure TgxODEJointHinge.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FAnchor.ReadFromFiler(reader);
    FAxis.ReadFromFiler(reader);
    FAxisParams.ReadFromFiler(reader);
  end;
end;

procedure TgxODEJointHinge.StructureChanged;
begin
  AnchorChange(nil);
  AxisChange(nil);
  FAxisParams.ApplyFlagged;
end;

procedure TgxODEJointHinge.AnchorChange(Sender: TObject);
begin
  if IsAttached then
    dJointSetHingeAnchor(FJointID, FAnchor.X, FAnchor.Y, FAnchor.Z);
end;

procedure TgxODEJointHinge.AxisChange(Sender: TObject);
var
  vec: TVector4f;
begin
  vec := FAxis.DirectVector;
  NormalizeVector(vec);
  FAxis.DirectVector := vec;
  if IsAttached then
    dJointSetHingeAxis(FJointID, FAxis.X, FAxis.Y, FAxis.Z);
end;

class function TgxODEJointHinge.FriendlyName: String;
begin
  Result := 'Hinge';
end;

class function TgxODEJointHinge.FriendlyDescription: String;
begin
  Result := 'ODE Hinge joint';
end;

procedure TgxODEJointHinge.SetAnchor(const Value: TgxCoordinates);
begin
  FAnchor.Assign(Value);
end;

procedure TgxODEJointHinge.SetAxis(const Value: TgxCoordinates);
begin
  FAxis.Assign(Value);
end;

procedure TgxODEJointHinge.SetAxisParams(const Value: TgxODEJointParams);
begin
  AxisParams.Assign(Value);
end;

function TgxODEJointHinge.SetAxisParam(Param: Integer; const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetHingeParam(JointID, Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

function TgxODEJointHinge.GetAxisParam(Param: Integer; var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetHingeParam(JointID, Param);
    Result := True;
  end
  else
    Result := False;
end;


// ---------------
// --------------- TgxODEJointBall ---------------
// ---------------

constructor TgxODEJointBall.Create(AOwner: TXCollection);
begin
  inherited;
  FAnchor := TgxCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FAnchor.OnNotifyChange := AnchorChange;
end;

destructor TgxODEJointBall.Destroy;
begin
  FAnchor.Free;
  inherited;
end;

procedure TgxODEJointBall.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateBall(FManager.World, nil);
  inherited;
end;

procedure TgxODEJointBall.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FAnchor.WriteToFiler(writer);
  end;
end;

procedure TgxODEJointBall.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FAnchor.ReadFromFiler(reader);
  end;
end;

procedure TgxODEJointBall.StructureChanged;
begin
  AnchorChange(nil);
end;

procedure TgxODEJointBall.AnchorChange(Sender: TObject);
begin
  if IsAttached then
    dJointSetBallAnchor(FJointID, FAnchor.X, FAnchor.Y, FAnchor.Z);
end;

class function TgxODEJointBall.FriendlyName: String;
begin
  Result := 'Ball';
end;

class function TgxODEJointBall.FriendlyDescription: String;
begin
  Result := 'ODE Ball joint implementation';
end;

procedure TgxODEJointBall.SetAnchor(const Value: TgxCoordinates);
begin
  FAnchor.Assign(Value);
end;


// ---------------
// --------------- TgxODEJointSlider ---------------
// ---------------

constructor TgxODEJointSlider.Create(AOwner: TXCollection);
begin
  inherited;
  FAxis := TgxCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FAxis.OnNotifyChange := AxisChange;
  FAxisParams := TgxODEJointParams.Create(Self);
  FAxisParams.SetCallback := SetAxisParam;
  FAxisParams.GetCallback := GetAxisParam;
end;

destructor TgxODEJointSlider.Destroy;
begin
  FAxis.Free;
  FAxisParams.Free;
  inherited;
end;

procedure TgxODEJointSlider.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateSlider(FManager.World, nil);
  inherited;
end;

procedure TgxODEJointSlider.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FAxis.WriteToFiler(writer);
    FAxisParams.WriteToFiler(writer);
  end;
end;

procedure TgxODEJointSlider.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FAxis.ReadFromFiler(reader);
    FAxisParams.ReadFromFiler(reader);
  end;
end;

procedure TgxODEJointSlider.StructureChanged;
begin
  AxisChange(nil);
  AxisParams.ApplyFlagged;
end;

procedure TgxODEJointSlider.AxisChange(Sender: TObject);
var
  vec: TVector4f;
begin
  vec := FAxis.DirectVector;
  NormalizeVector(vec);
  FAxis.DirectVector := vec;
  if IsAttached then
    dJointSetSliderAxis(FJointID, FAxis.X, FAxis.Y, FAxis.Z);
end;

class function TgxODEJointSlider.FriendlyName: String;
begin
  Result := 'Slider';
end;

class function TgxODEJointSlider.FriendlyDescription: String;
begin
  Result := 'ODE Slider joint implementation';
end;

procedure TgxODEJointSlider.SetAxis(const Value: TgxCoordinates);
begin
  FAxis.Assign(Value);
end;

procedure TgxODEJointSlider.SetAxisParams(const Value: TgxODEJointParams);
begin
  AxisParams.Assign(Value);
end;

function TgxODEJointSlider.SetAxisParam(Param: Integer;
  const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetSliderParam(JointID, Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

function TgxODEJointSlider.GetAxisParam(Param: Integer; var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetSliderParam(JointID, Param);
    Result := True;
  end
  else
    Result := False;
end;


// ---------------
// --------------- TgxODEJointFixed ---------------
// ---------------

procedure TgxODEJointFixed.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateFixed(FManager.World, nil);
  inherited;
end;

procedure TgxODEJointFixed.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
  end;
end;

procedure TgxODEJointFixed.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
  end;
end;

class function TgxODEJointFixed.FriendlyName: String;
begin
  Result := 'Fixed';
end;

class function TgxODEJointFixed.FriendlyDescription: String;
begin
  Result := 'ODE Fixed joint implementation';
end;


// ---------------
// --------------- TgxODEJointHinge2 ---------------
// ---------------

constructor TgxODEJointHinge2.Create(AOwner: TXCollection);
begin
  inherited;
  FAnchor := TgxCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FAnchor.OnNotifyChange := AnchorChange;
  FAxis1 := TgxCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FAxis1.OnNotifyChange := Axis1Change;
  FAxis2 := TgxCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FAxis2.OnNotifyChange := Axis2Change;
  FAxis1Params := TgxODEJointParams.Create(Self);
  FAxis1Params.SetCallback := SetAxis1Param;
  FAxis1Params.GetCallback := GetAxis1Param;
  FAxis2Params := TgxODEJointParams.Create(Self);
  FAxis2Params.SetCallback := SetAxis2Param;
  FAxis2Params.GetCallback := GetAxis2Param;

  JointOptions := [joBothObjectsMustBeAssigned];
end;

destructor TgxODEJointHinge2.Destroy;
begin
  FAnchor.Free;
  FAxis1.Free;
  FAxis2.Free;
  FAxis1Params.Free;
  FAxis2Params.Free;
  inherited;
end;

procedure TgxODEJointHinge2.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateHinge2(FManager.World, nil);
  inherited;
end;

procedure TgxODEJointHinge2.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FAnchor.WriteToFiler(writer);
    FAxis1.WriteToFiler(writer);
    FAxis2.WriteToFiler(writer);
    FAxis1Params.WriteToFiler(writer);
    FAxis2Params.WriteToFiler(writer);
  end;
end;

procedure TgxODEJointHinge2.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FAnchor.ReadFromFiler(reader);
    FAxis1.ReadFromFiler(reader);
    FAxis2.ReadFromFiler(reader);
    FAxis1Params.ReadFromFiler(reader);
    FAxis2Params.ReadFromFiler(reader);
  end;
end;

procedure TgxODEJointHinge2.StructureChanged;
begin
  AnchorChange(nil);
  Axis1Change(nil);
  Axis2Change(nil);
  Axis1Params.ApplyFlagged;
  Axis2Params.ApplyFlagged;
end;

procedure TgxODEJointHinge2.AnchorChange(Sender: TObject);
begin
  if IsAttached then
    dJointSetHinge2Anchor(FJointID, FAnchor.X, FAnchor.Y, FAnchor.Z);
end;

procedure TgxODEJointHinge2.Axis1Change(Sender: TObject);
var
  vec: TVector4f;
begin
  vec := FAxis1.DirectVector;
  NormalizeVector(vec);
  FAxis1.DirectVector := vec;
  if IsAttached then
    dJointSetHinge2Axis1(FJointID, FAxis1.X, FAxis1.Y, FAxis1.Z);
end;

procedure TgxODEJointHinge2.Axis2Change(Sender: TObject);
var
  vec: TVector4f;
begin
  vec := FAxis2.DirectVector;
  NormalizeVector(vec);
  FAxis2.DirectVector := vec;
  if IsAttached then
    dJointSetHinge2Axis2(FJointID, FAxis2.X, FAxis2.Y, FAxis2.Z);
end;

class function TgxODEJointHinge2.FriendlyName: String;
begin
  Result := 'Hinge2';
end;

class function TgxODEJointHinge2.FriendlyDescription: String;
begin
  Result := 'ODE Double Axis Hinge joint implementation';
end;

procedure TgxODEJointHinge2.SetAnchor(const Value: TgxCoordinates);
begin
  FAnchor.Assign(Value);
end;

procedure TgxODEJointHinge2.SetAxis1(const Value: TgxCoordinates);
begin
  FAxis1.Assign(Value);
end;

procedure TgxODEJointHinge2.SetAxis2(const Value: TgxCoordinates);
begin
  FAxis2.Assign(Value);
end;

procedure TgxODEJointHinge2.SetAxis1Params(const Value: TgxODEJointParams);
begin
  Axis1Params.Assign(Value);
end;

procedure TgxODEJointHinge2.SetAxis2Params(const Value: TgxODEJointParams);
begin
  Axis2Params.Assign(Value);
end;

function TgxODEJointHinge2.SetAxis1Param(Param: Integer; const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetHinge2Param(JointID, Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

function TgxODEJointHinge2.SetAxis2Param(Param: Integer; const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetHinge2Param(JointID, dParamLoStop2 + Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

function TgxODEJointHinge2.GetAxis1Param(Param: Integer; var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetHinge2Param(JointID, Param);
    Result := True;
  end
  else
    Result := False;
end;

function TgxODEJointHinge2.GetAxis2Param(Param: Integer; var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetHinge2Param(JointID, dParamLoStop2 + Param);
    Result := True;
  end
  else
    Result := False;
end;


// ---------------
// --------------- TgxODEJointUniversal ---------------
// ---------------

constructor TgxODEJointUniversal.Create(AOwner: TXCollection);
begin
  inherited;
  FAnchor := TgxCoordinates.CreateInitialized(Self, NullHmgPoint, csPoint);
  FAnchor.OnNotifyChange := AnchorChange;
  FAxis1 := TgxCoordinates.CreateInitialized(Self, ZHmgVector, csVector);
  FAxis1.OnNotifyChange := Axis1Change;
  FAxis2 := TgxCoordinates.CreateInitialized(Self, XHmgVector, csVector);
  FAxis2.OnNotifyChange := Axis2Change;
  FAxis1Params := TgxODEJointParams.Create(Self);
  FAxis1Params.SetCallback := SetAxis1Param;
  FAxis1Params.GetCallback := GetAxis1Param;
  FAxis2Params := TgxODEJointParams.Create(Self);
  FAxis2Params.SetCallback := SetAxis2Param;
  FAxis2Params.GetCallback := GetAxis2Param;

  JointOptions := [joBothObjectsMustBeAssigned];
end;

destructor TgxODEJointUniversal.Destroy;
begin
  FAnchor.Free;
  FAxis1.Free;
  FAxis2.Free;
  FAxis1Params.Free;
  FAxis2Params.Free;
  inherited;
end;

procedure TgxODEJointUniversal.Initialize;
begin
  if (not IsODEInitialized) or (FInitialized) then
    Exit;
  FJointID := dJointCreateUniversal(FManager.World, nil);
  inherited;
end;

procedure TgxODEJointUniversal.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    FAnchor.WriteToFiler(writer);
    FAxis1.WriteToFiler(writer);
    FAxis2.WriteToFiler(writer);
    FAxis1Params.WriteToFiler(writer);
    FAxis2Params.WriteToFiler(writer);
  end;
end;

procedure TgxODEJointUniversal.ReadFromFiler(reader: TReader);
begin
  inherited;
  with reader do
  begin
    Assert(ReadInteger = 0); // Archive version
    FAnchor.ReadFromFiler(reader);
    FAxis1.ReadFromFiler(reader);
    FAxis2.ReadFromFiler(reader);
    FAxis1Params.ReadFromFiler(reader);
    FAxis2Params.ReadFromFiler(reader);
  end;
end;

procedure TgxODEJointUniversal.StructureChanged;
begin
  AnchorChange(nil);
  Axis1Change(nil);
  Axis2Change(nil);
  Axis1Params.ApplyFlagged;
  Axis2Params.ApplyFlagged;
end;

procedure TgxODEJointUniversal.AnchorChange(Sender: TObject);
begin
  if IsAttached then
    dJointSetUniversalAnchor(FJointID, FAnchor.X, FAnchor.Y, FAnchor.Z);
end;

procedure TgxODEJointUniversal.Axis1Change(Sender: TObject);
var
  vec: TVector4f;
begin
  vec := FAxis1.DirectVector;
  NormalizeVector(vec);
  FAxis1.DirectVector := vec;
  if IsAttached then
    dJointSetUniversalAxis1(FJointID, FAxis1.X, FAxis1.Y, FAxis1.Z);
end;

procedure TgxODEJointUniversal.Axis2Change(Sender: TObject);
var
  vec: TVector4f;
begin
  vec := FAxis2.DirectVector;
  NormalizeVector(vec);
  FAxis2.DirectVector := vec;
  if IsAttached then
    dJointSetUniversalAxis2(FJointID, FAxis2.X, FAxis2.Y, FAxis2.Z);
end;

class function TgxODEJointUniversal.FriendlyName: String;
begin
  Result := 'Universal';
end;

class function TgxODEJointUniversal.FriendlyDescription: String;
begin
  Result := 'ODE Universal joint implementation';
end;

procedure TgxODEJointUniversal.SetAnchor(const Value: TgxCoordinates);
begin
  FAnchor.Assign(Value);
end;

procedure TgxODEJointUniversal.SetAxis1(const Value: TgxCoordinates);
begin
  FAxis1.Assign(Value);
end;

procedure TgxODEJointUniversal.SetAxis2(const Value: TgxCoordinates);
begin
  FAxis2.Assign(Value);
end;

procedure TgxODEJointUniversal.SetAxis1Params(const Value: TgxODEJointParams);
begin
  Axis1Params.Assign(Value);
end;

procedure TgxODEJointUniversal.SetAxis2Params(const Value: TgxODEJointParams);
begin
  Axis2Params.Assign(Value);
end;

function TgxODEJointUniversal.SetAxis1Param(Param: Integer; const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetUniversalParam(JointID, Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

function TgxODEJointUniversal.SetAxis2Param(Param: Integer; const Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    dJointSetUniversalParam(JointID, dParamLoStop2 + Param, Value);
    Result := True;
  end
  else
    Result := False;
end;

function TgxODEJointUniversal.GetAxis1Param(Param: Integer; var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetUniversalParam(JointID, Param);
    Result := True;
  end
  else
    Result := False;
end;

function TgxODEJointUniversal.GetAxis2Param(Param: Integer; var Value: TdReal): Boolean;
begin
  if IsAttached then
  begin
    Value := dJointGetUniversalParam(JointID, dParamLoStop2 + Param);
    Result := True;
  end
  else
    Result := False;
end;

// ---------------
// --------------- TgxODECustomCollider --------------
// ---------------

constructor TgxODECustomCollider.Create(AOwner: TXCollection);
begin
  inherited;
  FContactList := TList.Create;
  FContactCache := TList.Create;

  FContactResolution := 1;

  FRenderContacts := False;
  FContactRenderPoints := TgxAffineVectorList.Create;
  FContactColor := TgxColor.CreateInitialized(Self, clrRed, NotifyChange);
  FPointSize := 3;
end;

destructor TgxODECustomCollider.Destroy;
var
  i: Integer;
begin
  FContactList.Free;
  for i := 0 to FContactCache.Count - 1 do
    TgxODEContactPoint(FContactCache[i]).Free;
  FContactCache.Free;
  FContactRenderPoints.Free;
  FContactColor.Free;
  inherited;
end;

procedure TgxODECustomCollider.Initialize;
begin
  if not Assigned(Manager) then
    exit;
  if not Assigned(Manager.Space) then
    exit;
  if vCustomColliderClassNum = 0 then
  begin
    with vCustomColliderClass do
    begin
      bytes := 0;
      Collider := GetCustomColliderFn;
      aabb := dInfiniteAABB;
      aabb_test := nil;
      dtor := nil;
    end;
    vCustomColliderClassNum := dCreateGeomClass(vCustomColliderClass);
  end;
  FGeom := dCreateGeom(vCustomColliderClassNum);
  dGeomSetData(FGeom, Self);
  dSpaceAdd(Manager.Space, FGeom);
  inherited;
end;

procedure TgxODECustomCollider.Finalize;
begin
  if not Initialized then
    exit;
  if Assigned(FGeom) then
  begin
    dGeomDestroy(FGeom);
    FGeom := nil;
  end;
  inherited;
end;

procedure TgxODECustomCollider.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
    WriteFloat(FContactResolution);
    WriteBoolean(FRenderContacts);
    WriteFloat(FPointSize);
    Write(PByte(FContactColor.AsAddress)^, 4);
  end;
end;

procedure TgxODECustomCollider.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  inherited;
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion = 0); // Archive version
    FContactResolution := ReadFloat;
    FRenderContacts := ReadBoolean;
    FPointSize := ReadFloat;
    Read(PByte(FContactColor.AsAddress)^, 4);
  end;
end;

procedure TgxODECustomCollider.ClearContacts;
begin
  FContactList.Clear;
end;

procedure TgxODECustomCollider.AddContact(x, y, z: TdReal);
begin
  AddContact(AffineVectorMake(x, y, z));
end;

procedure TgxODECustomCollider.AddContact(pos: TAffineVector);
var
  absPos, colPos, colNorm: TAffineVector;
  Depth: Single;
  ContactPoint: TgxODEContactPoint;
begin
  absPos := AffineVectorMake(VectorTransform(PointMake(pos), FTransform));
  if Collide(absPos, Depth, colPos, colNorm) then
  begin
    if FContactList.Count < FContactCache.Count then
      ContactPoint := FContactCache[FContactList.Count]
    else
    begin
      ContactPoint := TgxODEContactPoint.Create;
      FContactCache.Add(ContactPoint);
    end;
    ContactPoint.Position := colPos;
    ContactPoint.Normal := colNorm;
    ContactPoint.Depth := Depth;
    FContactList.Add(ContactPoint);
  end;
  if FRenderContacts and Manager.Visible and Manager.VisibleAtRunTime then
    FContactRenderPoints.Add(absPos);
end;

function TgxODECustomCollider.ApplyContacts(o1, o2: PdxGeom; flags: Integer;
  contact: PdContactGeom; skip: Integer): Integer;
var
  i, maxContacts: Integer;
begin
  FContactList.Sort(ContactSort);
  Result := 0;
  maxContacts := flags and $FFFF;
  try
    for i := 0 to FContactList.Count - 1 do
    begin
      if Result >= maxContacts then
        Exit;
      with TgxODEContactPoint(FContactList[i]) do
      begin
        contact.Depth := Depth;
        contact.pos[0] := Position.x;
        contact.pos[1] := Position.y;
        contact.pos[2] := Position.z;
        contact.pos[3] := 1;
        contact.Normal[0] := -Normal.x;
        contact.Normal[1] := -Normal.y;
        contact.Normal[2] := -Normal.z;
        contact.Normal[3] := 0;
      end;
      contact.g1 := o1;
      contact.g2 := o2;
      contact := PdContactGeom(Integer(contact) + skip);
      Inc(Result);
    end;
  finally
    ClearContacts;
  end;
end;

procedure TgxODECustomCollider.SetTransform(ATransform: TMatrix4f);
begin
  FTransform := ATransform;
end;

procedure TgxODECustomCollider.SetContactResolution(const Value: Single);
begin
  FContactResolution := Value;
  if FContactResolution <= 0 then
    FContactResolution := 0.01;
end;

procedure TgxODECustomCollider.Render(var rci: TgxRenderContextInfo);
var
  i: Integer;
begin
  if FRenderContacts and (FContactRenderPoints.Count>0) then
  begin
    glPushAttrib(GL_CURRENT_BIT);
    glColor3fv(PGLfloat(FContactColor.AsAddress));
    glPointSize(FPointSize);
    glBegin(GL_POINTS);
    for i := 0 to FContactRenderPoints.Count - 1 do
      glVertex3fv(@FContactRenderPoints.List[i]);
    glEnd;
    glPopAttrib;
  end;
  FContactRenderPoints.Clear;
end;

procedure TgxODECustomCollider.SetRenderContacts(const Value: Boolean);
begin
  if Value <> FRenderContacts then
  begin
    FRenderContacts := Value;
    NotifyChange(Self);
  end;
end;

procedure TgxODECustomCollider.SetContactColor(const Value: TgxColor);
begin
  FContactColor.Assign(Value);
end;

procedure TgxODECustomCollider.SetPointSize(const Value: Single);
begin
  if Value <> FPointSize then
  begin
    FPointSize := Value;
    NotifyChange(Self);
  end;
end;

// ---------------
// --------------- TgxODEHeightField --------------
// ---------------

constructor TgxODEHeightField.Create(AOwner: TXCollection);
var
  Allow: Boolean;
begin
  Allow := False;
  if Assigned(AOwner) then
  begin
    if Assigned(AOwner.Owner) then
    begin
      if ((AOwner.Owner) is TgxTerrainRenderer) or
        ((AOwner.Owner) is TgxHeightField) then
        Allow := True;
    end;
  end;
  if not Allow then
    raise Exception.Create
      ('This element must be a behaviour of a TgxTerrainRenderer or TgxHeightField');
  inherited Create(AOwner);
end;

procedure TgxODEHeightField.WriteToFiler(writer: TWriter);
begin
  inherited;
  with writer do
  begin
    WriteInteger(0); // Archive version
  end;
end;

procedure TgxODEHeightField.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  inherited;
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion = 0); // Archive version
  end;
end;

class function TgxODEHeightField.FriendlyName: string;
begin
  Result := 'ODE HeightField Collider';
end;

class function TgxODEHeightField.FriendlyDescription: string;
begin
  Result := 'A custom ODE collider powered by it''s parent TgxTerrainRenderer or TgxHeightField';
end;

class function TgxODEHeightField.UniqueItem: Boolean;
begin
  Result := True;
end;

class function TgxODEHeightField.CanAddTo(collection: TXCollection): Boolean;
begin
  Result := False;
  if collection is TgxBehaviours then
    if Assigned(TgxBehaviours(collection).Owner) then
      if (TgxBehaviours(collection).Owner is TgxHeightField)
        or (TgxBehaviours(collection).Owner is TgxTerrainRenderer) then
        Result := True;
end;

function TgxODEHeightField.Collide(aPos: TAffineVector; var Depth: Single;
  var cPos, cNorm: TAffineVector): Boolean;

  function AbsoluteToLocal(vec: TVector4f): TVector4f;
  var
    mat: TMatrix4f;
  begin
    if Owner.Owner is TgxHeightField then
      Result := TgxHeightField(Owner.Owner).AbsoluteToLocal(vec)
    else if Owner.Owner is TgxTerrainRenderer then
    begin
      mat := TgxTerrainRenderer(Owner.Owner).AbsoluteMatrix;
      NormalizeMatrix(mat);
      InvertMatrix(mat);
      Result := VectorTransform(vec, mat);
    end
    else
      Assert(False);
  end;

  function LocalToAbsolute(vec: TVector4f): TVector4f;
  var
    mat: TMatrix4f;
  begin
    if Owner.Owner is TgxHeightField then
      Result := TgxHeightField(Owner.Owner).LocalToAbsolute(vec)
    else if Owner.Owner is TgxTerrainRenderer then
    begin
      mat := TgxTerrainRenderer(Owner.Owner).AbsoluteMatrix;
      NormalizeMatrix(mat);
      Result := VectorTransform(vec, mat);
    end
    else
      Assert(False);
  end;

  function GetHeight(pos: TVector4f; var height: Single): Boolean;
  var
    dummy1: TVector4f;
    dummy2: TTexPoint;
  begin
    Result := False;
    if Owner.Owner is TgxTerrainRenderer then
    begin
      height := TgxTerrainRenderer(Owner.Owner).InterpolatedHeight(LocalToAbsolute(pos));
      Result := True;
    end
    else
	if Owner.Owner is TgxHeightField then
    begin
      if Assigned(TgxHeightField(Owner.Owner).OnGetHeight) then
      begin
        TgxHeightField(Owner.Owner).OnGetHeight(pos.x, pos.y, height, dummy1, dummy2);
        Result := True;
      end;
    end;
  end;

const
  cDelta = 0.1;
var
  localPos: TVector4f;
  height: Single;
  temp1, temp2: TAffineVector;
begin
  localPos := AbsoluteToLocal(PointMake(aPos));
  if GetHeight(localPos, height) then
  begin
    Depth := height - localPos.z;
    Result := (Depth > 0);
    if Result then
    begin
      localPos.z := height;
      cPos := AffineVectorMake(LocalToAbsolute(localPos));
      temp1.x:= localPos.x + cDelta;
      temp1.y := localPos.y;
      temp1.z := localPos.z;
      GetHeight(PointMake(temp1), temp1.z);
      temp2.x := localPos.x;
      temp2.y := localPos.y + cDelta;
      temp2.z := localPos.z;
      GetHeight(PointMake(temp2), temp2.z);
      cNorm := CalcPlaneNormal(AffineVectorMake(localPos), temp1, temp2);
      cNorm := AffineVectorMake(LocalToAbsolute(VectorMake(cNorm)));
    end;
  end
  else
    Result := False;
end;


// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

vODEObjectRegister := TList.Create;

RegisterXCollectionItemClass(TgxODEDynamic);
RegisterXCollectionItemClass(TgxODEStatic);

RegisterXCollectionItemClass(TgxODEElementBox);
RegisterXCollectionItemClass(TgxODEElementSphere);
RegisterXCollectionItemClass(TgxODEElementCapsule);
RegisterXCollectionItemClass(TgxODEElementCylinder);
RegisterXCollectionItemClass(TgxODEElementTriMesh);
RegisterXCollectionItemClass(TgxODEElementPlane);

RegisterXCollectionItemClass(TgxODEJointHinge);
RegisterXCollectionItemClass(TgxODEJointBall);
RegisterXCollectionItemClass(TgxODEJointSlider);
RegisterXCollectionItemClass(TgxODEJointFixed);
RegisterXCollectionItemClass(TgxODEJointHinge2);
RegisterXCollectionItemClass(TgxODEJointUniversal);

RegisterXCollectionItemClass(TgxODEHeightField);

// ------------------------------------------------------------------
finalization
// ------------------------------------------------------------------

vODEObjectRegister.Free;

UnregisterXCollectionItemClass(TgxODEDynamic);
UnregisterXCollectionItemClass(TgxODEStatic);

UnregisterXCollectionItemClass(TgxODEElementBox);
UnregisterXCollectionItemClass(TgxODEElementSphere);
UnregisterXCollectionItemClass(TgxODEElementCapsule);
UnregisterXCollectionItemClass(TgxODEElementCylinder);
UnregisterXCollectionItemClass(TgxODEElementTriMesh);
UnregisterXCollectionItemClass(TgxODEElementPlane);

UnregisterXCollectionItemClass(TgxODEJointHinge);
UnregisterXCollectionItemClass(TgxODEJointBall);
UnregisterXCollectionItemClass(TgxODEJointSlider);
UnregisterXCollectionItemClass(TgxODEJointFixed);
UnregisterXCollectionItemClass(TgxODEJointHinge2);
UnregisterXCollectionItemClass(TgxODEJointUniversal);

UnregisterXCollectionItemClass(TgxODEHeightField);

// CloseODE;

end.
