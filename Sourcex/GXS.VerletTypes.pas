//
// The graphics rendering engine GLScene http://glscene.org
//
unit GXS.VerletTypes;

(*
  Base Verlet modelling/simulation classes.
  This unit is generic, GLScene-specific sub-classes are in GLVerletClasses.

  Note that currently, the SatisfyConstraintForEdge methods push the nodes in
  the edge uniformly - it should push the closer node more for correct physics.
  It's a matter of leverage.
*)
interface

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Types,

  GXS.VectorTypes,
  GXS.PersistentClasses,

  GXS.BaseClasses,
  GXS.Objects,
  GXS.Scene,
  GXS.Coordinates,
  GXS.VectorGeometry,
  GXS.VectorLists,
  GXS.VectorFileObjects,
  GXS.SpacePartition,
  GXS.GeometryBB;

const
  G_DRAG = 0.0001;
  cDEFAULT_CONSTRAINT_FRICTION = 0.6;


type
  TgxStiffnessVH = (vhsFull, vhsSkip1Node, vhsSkip2Node, vhsSkip3Node,
    vhsSkip4Node, vhsSkip5Node, vhsSkip6Node, vhsSkip7Node, vhsSkip8Node,
    vhsSkip9Node);
  TgxStiffnessSetVH = set of TgxStiffnessVH;

type
  TgxVerletEdgeList = class;
  TgxVerletWorld = class;

  // Basic Verlet Node
  TgxBaseVerletNode = class(TSpacePartitionLeaf)
  private
    FForce: TAffineVector;
    FOwner: TgxVerletWorld;
    FWeight, FInvWeight: Single;
    FRadius: Single;
    FNailedDown: Boolean;
    FFriction: Single;
    FChangedOnStep: Integer;
    function GetSpeed: TAffineVector;
  protected
    FLocation, FOldLocation: TAffineVector;
    procedure SetLocation(const Value: TAffineVector); virtual;
    procedure SetWeight(const Value: Single);
    procedure AfterProgress; virtual;
  public
    constructor CreateOwned(const aOwner: TgxVerletWorld); virtual;
    destructor Destroy; override;
    // Applies friction
    procedure ApplyFriction(const friction, penetrationDepth: Single;
      const surfaceNormal: TAffineVector);
    // Simple and less accurate method for friction
    procedure OldApplyFriction(const friction, penetrationDepth: Single);
    // Perform Verlet integration
    procedure Verlet(const vpt: TgxProgressTimes); virtual;
    (* Initlializes the node. For the base class, it just makes sure that
      FOldPosition = FPosition, so that speed is zero *)
    procedure Initialize; dynamic;
    // Calculates the distance to another node
    function DistanceToNode(const node: TgxBaseVerletNode): Single;
    // Calculates the movement of the node
    function GetMovement: TAffineVector;
    (* The TgxBaseVerletNode inherits from TSpacePartitionLeaf, and it needs to
      know how to publish itself. The owner (a TgxVerletWorld) has a spatial
      partitioning object *)
    procedure UpdateCachedAABBAndBSphere; override;
    // The VerletWorld that owns this verlet
    property Owner: TgxVerletWorld read FOwner;
    // The location of the verlet
    property Location: TAffineVector read FLocation write SetLocation;
    // The old location of the verlet. This is used for verlet integration
    property OldLocation: TAffineVector read FOldLocation write FOldLocation;
    // The radius of the verlet node - this has been more or less deprecated
    property Radius: Single read FRadius write FRadius;
    // A sum of all forces that has been applied to this verlet node during a step
    property Force: TAffineVector read FForce write FForce;
    (* If the node is nailed down, it can't be moved by either force,
      constraint or verlet integration - but you can still move it by hand *)
    property NailedDown: Boolean read FNailedDown write FNailedDown;
    // The weight of a node determines how much it's affected by a force
    property Weight: Single read FWeight write SetWeight;
    // InvWeight is 1/Weight, and is kept up to date automatically
    property InvWeight: Single read FInvWeight;
    // Returns the speed of the verlet node. Speed = Movement / deltatime
    property Speed: TAffineVector read GetSpeed;
    // Each node has a friction that effects how it reacts during contacts.
    property friction: Single read FFriction write FFriction;
    (* What phyisics step was this node last changed? Used to keep track
      of when the spatial partitioning needs to be updated *)
    property ChangedOnStep: Integer read FChangedOnStep;
  end;

  TgxVerletNodeClass = class of TgxBaseVerletNode;

  TgxVerletNodeList = class(TList)
  private
    function GetItems(i: Integer): TgxBaseVerletNode;
    procedure SetItems(i: Integer; const Value: TgxBaseVerletNode);
  public
    property Items[i: Integer]: TgxBaseVerletNode read GetItems
      write SetItems; default;
  end;

  TgxVerletConstraint = class(TObject)
  private
    FOwner: TgxVerletWorld;
    FEnabled: Boolean;
    FTag: Integer;
  public
    constructor Create(const aOwner: TgxVerletWorld); virtual;
    destructor Destroy; override;
    (* Updates the position of one or several nodes to make sure that they
      don't violate the constraint *)
    procedure SatisfyConstraint(const iteration, maxIterations: Integer);
      virtual; abstract;
    // Notifies removal of a node
    procedure RemoveNode(const aNode: TgxBaseVerletNode); virtual; abstract;
    // Method that's fired before the physics iterations are performed
    procedure BeforeIterations; virtual;
    // Onwer of the constraint
    property Owner: TgxVerletWorld read FOwner;
    // Determines if the constraint should be enforced or not
    property Enabled: Boolean read FEnabled write FEnabled;
    // Tag field reserved for the user.
    property Tag: Integer read FTag write FTag;
  end;

  TgxVerletDualConstraint = class(TgxVerletConstraint)
  private
    FNodeA, FNodeB: TgxBaseVerletNode;
  public
    procedure RemoveNode(const aNode: TgxBaseVerletNode); override;
    // Reference to NodeA.
    property NodeA: TgxBaseVerletNode read FNodeA write FNodeA;
    // Reference to NodeB.
    property NodeB: TgxBaseVerletNode read FNodeB write FNodeB;
  end;

  TgxVerletGroupConstraint = class(TgxVerletConstraint)
  private
    FNodes: TgxVerletNodeList;
  public
    constructor Create(const aOwner: TgxVerletWorld); override;
    destructor Destroy; override;
    procedure RemoveNode(const aNode: TgxBaseVerletNode); override;
    // The list of nodes that this constraint will effect
    property Nodes: TgxVerletNodeList read FNodes;
  end;

  // Verlet edges simulate rigid collission edges
  TgxVerletEdge = class(TSpacePartitionLeaf)
  private
    FNodeA: TgxBaseVerletNode;
    FNodeB: TgxBaseVerletNode;
  public
    (* The TgxVerletEdge inherits from TSpacePartitionLeaf, and it needs to
      know how to publish itself. The owner ( a TgxVerletWorld ) has a spatial
      partitioning object *)
    procedure UpdateCachedAABBAndBSphere; override;
    constructor CreateEdgeOwned(const aNodeA, aNodeB: TgxBaseVerletNode);
    // One of the nodes in the edge
    property NodeA: TgxBaseVerletNode read FNodeA write FNodeA;
    // One of the nodes in the edge
    property NodeB: TgxBaseVerletNode read FNodeB write FNodeB;
  end;

  TgxVerletEdgeList = class(TList)
  private
    function GetItems(i: Integer): TgxVerletEdge;
    procedure SetItems(i: Integer; const Value: TgxVerletEdge);
  public
    property Items[i: Integer]: TgxVerletEdge read GetItems
      write SetItems; default;
  end;

  TgxVerletGlobalConstraint = class(TgxVerletConstraint)
  private
    FKickbackForce: TAffineVector;
    FKickbackTorque: TAffineVector;
    FLocation: TAffineVector;
    procedure SetLocation(const Value: TAffineVector); virtual;
  public
    constructor Create(const aOwner: TgxVerletWorld); override;
    destructor Destroy; override;
    procedure RemoveNode(const aNode: TgxBaseVerletNode); override;
    procedure BeforeIterations; override;
    procedure SatisfyConstraint(const iteration, maxIterations
      : Integer); override;
    procedure SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
      const iteration, maxIterations: Integer); virtual; abstract;
    procedure SatisfyConstraintForEdge(const aEdge: TgxVerletEdge;
      const iteration, maxIterations: Integer); virtual;
    property Location: TAffineVector read FLocation write SetLocation;
    (* The force that this collider has experienced while correcting the
      verlet possitions. This force can be applied to ODE bodies, for instance *)
    property KickbackForce: TAffineVector read FKickbackForce
      write FKickbackForce;
    (* The torque that this collider has experienced while correcting the
      verlet possitions, in reference to the center of the collider. The
      torque  force can be applied to ODE bodies, but it must first be
      translated. A torque can be trasnalted by
      EM(b) = EM(a) + EF x VectorSubtract(b, a).
      Simply adding the torque to the body will NOT work correctly.
      See TranslateKickbackTorque *)
    property KickbackTorque: TAffineVector read FKickbackTorque
      write FKickbackTorque;
    procedure AddKickbackForceAt(const Pos: TAffineVector;
      const Force: TAffineVector);
    function TranslateKickbackTorque(const TorqueCenter: TAffineVector)
      : TAffineVector;
  end;

  TgxVerletGlobalFrictionConstraint = class(TgxVerletGlobalConstraint)
  private
    FFrictionRatio: Single;
  public
    constructor Create(const aOwner: TgxVerletWorld); override;
    property FrictionRatio: Single read FFrictionRatio write FFrictionRatio;
  end;

  TgxVerletGlobalFrictionConstraintSP = class(TgxVerletGlobalFrictionConstraint)
  public
    procedure SatisfyConstraint(const iteration, maxIterations
      : Integer); override;
    procedure PerformSpaceQuery; virtual; abstract;
  end;

  TgxVerletGlobalFrictionConstraintSphere = class
    (TgxVerletGlobalFrictionConstraintSP)
  private
    FCachedBSphere: TBSphere;
    procedure SetLocation(const Value: TAffineVector); override;
  public
    procedure UpdateCachedBSphere;
    procedure PerformSpaceQuery; override;
    function GetBSphere: TBSphere; virtual; abstract;
    property CachedBSphere: TBSphere read FCachedBSphere;
  end;

  TgxVerletGlobalFrictionConstraintBox = class
    (TgxVerletGlobalFrictionConstraintSP)
  private
    FCachedAABB: TAABB;
    procedure SetLocation(const Value: TAffineVector); override;
  public
    procedure UpdateCachedAABB;
    procedure PerformSpaceQuery; override;
    function GetAABB: TAABB; virtual; abstract;
    property CachedAABB: TAABB read FCachedAABB;
  end;

  TgxVerletConstraintList = class(TList)
  private
    function GetItems(i: Integer): TgxVerletConstraint;
    procedure SetItems(i: Integer; const Value: TgxVerletConstraint);
  public
    property Items[i: Integer]: TgxVerletConstraint read GetItems
      write SetItems; default;
  end;

  // Generic verlet force.
  TgxVerletForce = class(TObject)
  private
    FOwner: TgxVerletWorld;
  public
    constructor Create(const aOwner: TgxVerletWorld); virtual;
    destructor Destroy; override;
    // Implementation should add force to force resultant for all relevant nodes
    procedure AddForce(const vpt: TgxProgressTimes); virtual; abstract;
    // Notifies removal of a node
    procedure RemoveNode(const aNode: TgxBaseVerletNode); virtual; abstract;
    property Owner: TgxVerletWorld read FOwner;
  end;

  // A verlet force that applies to two specified nodes.
  TgxVerletDualForce = class(TgxVerletForce)
  private
    FNodeA, FNodeB: TgxBaseVerletNode;
  public
    procedure RemoveNode(const aNode: TgxBaseVerletNode); override;
    // Reference to NodeA.
    property NodeA: TgxBaseVerletNode read FNodeA write FNodeA;
    // Reference to NodeB.
    property NodeB: TgxBaseVerletNode read FNodeB write FNodeB;
  end;

  // A verlet force that applies to a specified group of nodes.
  TgxVerletGroupForce = class(TgxVerletForce)
  private
    FNodes: TgxVerletNodeList;
  public
    constructor Create(const aOwner: TgxVerletWorld); override;
    destructor Destroy; override;
    procedure RemoveNode(const aNode: TgxBaseVerletNode); override;
    // Nodes of the force group, referred, NOT owned.
    property Nodes: TgxVerletNodeList read FNodes;
  end;

  // A global force (applied to all verlet nodes).
  TgxVerletGlobalForce = class(TgxVerletForce)
  public
    procedure RemoveNode(const aNode: TgxBaseVerletNode); override;
    procedure AddForce(const vpt: TgxProgressTimes); override;
    procedure AddForceToNode(const aNode: TgxBaseVerletNode); virtual; abstract;
  end;

  TgxVerletForceList = class(TList)
  private
    function GetItems(i: Integer): TgxVerletForce;
    procedure SetItems(i: Integer; const Value: TgxVerletForce);
  public
    property Items[i: Integer]: TgxVerletForce read GetItems
      write SetItems; default;
  end;

  TgxVerletStick = class;
  TgxVerletSpring = class;
  TgxVerletSlider = class;

  TUpdateSpacePartion = (uspEveryIteration, uspEveryFrame, uspNever);
  TCollisionConstraintTypes = (cctEdge, cctNode);
  TCollisionConstraintTypesSet = set of TCollisionConstraintTypes;

  TgxVerletWorld = class(TObject)
  private
    FIterations: Integer;
    FNodes: TgxVerletNodeList;
    FConstraints: TgxVerletConstraintList;
    FForces: TgxVerletForceList;
    FMaxDeltaTime, FSimTime: Single;
    FDrag: Single;
    FCurrentDeltaTime: Single;
    FInvCurrentDeltaTime: Single;
    FSolidEdges: TgxVerletEdgeList;
    FSpacePartition: TBaseSpacePartition;
    FCurrentStepCount: Integer;
    FUpdateSpacePartion: TUpdateSpacePartion;
    FCollisionConstraintTypes: TCollisionConstraintTypesSet;
    FConstraintsWithBeforeIterations: TgxVerletConstraintList;
    FVerletNodeClass: TgxVerletNodeClass;
    FInertia: Boolean;
    FInertaPauseSteps: Integer;
  protected
    procedure AccumulateForces(const vpt: TgxProgressTimes); virtual;
    procedure Verlet(const vpt: TgxProgressTimes); virtual;
    procedure SatisfyConstraints(const vpt: TgxProgressTimes); virtual;
    procedure DoUpdateSpacePartition;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddNode(const aNode: TgxBaseVerletNode): Integer;
    procedure RemoveNode(const aNode: TgxBaseVerletNode);
    function AddConstraint(const aConstraint: TgxVerletConstraint): Integer;
    procedure RemoveConstraint(const aConstraint: TgxVerletConstraint);
    function AddForce(const aForce: TgxVerletForce): Integer;
    procedure RemoveForce(const aForce: TgxVerletForce);
    procedure AddSolidEdge(const aNodeA, aNodeB: TgxBaseVerletNode);
    procedure PauseInertia(const IterationSteps: Integer);
    function CreateOwnedNode(const Location: TAffineVector;
      const aRadius: Single = 0; const aWeight: Single = 1): TgxBaseVerletNode;
    function CreateStick(const aNodeA, aNodeB: TgxBaseVerletNode;
      const Slack: Single = 0): TgxVerletStick;
    function CreateSpring(const aNodeA, aNodeB: TgxBaseVerletNode;
      const aStrength, aDamping: Single; const aSlack: Single = 0): TgxVerletSpring;
    function CreateSlider(const aNodeA, aNodeB: TgxBaseVerletNode;
      const aSlideDirection: TAffineVector): TgxVerletSlider;
    procedure Initialize; virtual;
    procedure CreateOctree(const OctreeMin, OctreeMax: TAffineVector;
      const LeafThreshold, MaxTreeDepth: Integer);
    function Progress(const deltaTime, newTime: Double): Integer; virtual;
    function FirstNode: TgxBaseVerletNode;
    function LastNode: TgxBaseVerletNode;
    property Drag: Single read FDrag write FDrag;
    property Iterations: Integer read FIterations write FIterations;
    property Nodes: TgxVerletNodeList read FNodes;
    property Constraints: TgxVerletConstraintList read FConstraints;
    property ConstraintsWithBeforeIterations: TgxVerletConstraintList
      read FConstraintsWithBeforeIterations;
    property SimTime: Single read FSimTime write FSimTime;
    property MaxDeltaTime: Single read FMaxDeltaTime write FMaxDeltaTime;
    property CurrentDeltaTime: Single read FCurrentDeltaTime;
    property SolidEdges: TgxVerletEdgeList read FSolidEdges write FSolidEdges;
    property CurrentStepCount: Integer read FCurrentStepCount;
    property SpacePartition: TBaseSpacePartition read FSpacePartition;
    property UpdateSpacePartion: TUpdateSpacePartion read FUpdateSpacePartion
      write FUpdateSpacePartion;
    property CollisionConstraintTypes: TCollisionConstraintTypesSet
      read FCollisionConstraintTypes write FCollisionConstraintTypes;
    property VerletNodeClass: TgxVerletNodeClass read FVerletNodeClass
      write FVerletNodeClass;
    property Inertia: Boolean read FInertia write FInertia;
  end;

  TgxVerletGravity = class(TgxVerletGlobalForce)
  private
    FGravity: TAffineVector;
  public
    constructor Create(const aOwner: TgxVerletWorld); override;
    procedure AddForceToNode(const aNode: TgxBaseVerletNode); override;
    property Gravity: TAffineVector read FGravity write FGravity;
  end;

  TgxVerletAirResistance = class(TgxVerletGlobalForce)
  private
    FDragCoeff: Single;
    FWindDirection: TAffineVector;
    FWindMagnitude: Single;
    FWindChaos: Single;
    procedure SetWindDirection(const Value: TAffineVector);
  public
    constructor Create(const aOwner: TgxVerletWorld); override;
    procedure AddForceToNode(const aNode: TgxBaseVerletNode); override;
    property DragCoeff: Single read FDragCoeff write FDragCoeff;
    property WindDirection: TAffineVector read FWindDirection
      write SetWindDirection;
    property WindMagnitude: Single read FWindMagnitude write FWindMagnitude;
    // Measures how chaotic the wind is, as a fraction of the wind magnitude
    property WindChaos: Single read FWindChaos write FWindChaos;
  end;

  TgxVerletSpring = class(TgxVerletDualForce)
  private
    FRestLength: Single;
    FStrength: Single;
    FDamping: Single;
    FSlack: Single;
    FForceFactor: Single;
  protected
    procedure SetSlack(const Value: Single);
  public
    procedure AddForce(const vpt: TgxProgressTimes); override;
    // Must be invoked after adjust node locations or strength
    procedure SetRestLengthToCurrent;
    property Strength: Single read FStrength write FStrength;
    property Damping: Single read FDamping write FDamping;
    property Slack: Single read FSlack write SetSlack;
  end;

  // Floor Collision Constraint
  TgxVerletFloor = class(TgxVerletGlobalFrictionConstraintSP)
  private
    FBounceRatio, FFloorLevel: Single;
    FNormal: TAffineVector;
  protected
    procedure SetNormal(const Value: TAffineVector);
  public
    constructor Create(const aOwner: TgxVerletWorld); override;
    procedure PerformSpaceQuery; override;
    procedure SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    property BounceRatio: Single read FBounceRatio write FBounceRatio;
    property FloorLevel: Single read FFloorLevel write FFloorLevel;
    property Normal: TAffineVector read FNormal write SetNormal;
  end;

  TgxVerletHeightField = class;
  TgxVerletHeightFieldOnNeedHeight = function(hfConstraint: TgxVerletHeightField;
    node: TgxBaseVerletNode): Single of object;

  // HeightField collision constraint (punctual!)
  TgxVerletHeightField = class(TgxVerletFloor)
  private
    FOnNeedHeight: TgxVerletHeightFieldOnNeedHeight;
  public
    procedure SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    property OnNeedHeight: TgxVerletHeightFieldOnNeedHeight read FOnNeedHeight
      write FOnNeedHeight;
  end;

  // Stick Verlet Collision Constraint. Imposes a fixed distance between two nodes
  TgxVerletStick = class(TgxVerletDualConstraint)
  private
    FSlack: Single;
    FRestLength: Single;
  public
    procedure SatisfyConstraint(const iteration, maxIterations: Integer); override;
    procedure SetRestLengthToCurrent;
    property Slack: Single read FSlack write FSlack;
    property RestLength: Single read FRestLength write FRestLength;
  end;

  (* Rigid body verlet Collision constraint.
    Regroups several nodes in a rigid body conformation, somewhat similar
    to a stick but for multiple nodes. EXPERIMENTAL, STILL DOES NOT WORK! *)
  TgxVerletRigidBody = class(TgxVerletGroupConstraint)
  private
    FNodeParams: array of TAffineVector;
    FNodeCoords: array of TAffineVector;
    FNatMatrix, FInvNatMatrix: TAffineMatrix;
  protected
    procedure ComputeBarycenter(var barycenter: TAffineVector);
    procedure ComputeNaturals(const barycenter: TAffineVector;
      var natX, natY, natZ: TAffineVector);
  public
    procedure ComputeRigidityParameters;
    procedure SatisfyConstraint(const iteration, maxIterations: Integer); override;
  end;

  (* Slider constraint.
    Imposes that two nodes be aligned on a defined direction, on which they
    can slide freely. Note that the direction is fixed and won't rotate
    with the verlet assembly!. *)
  TgxVerletSlider = class(TgxVerletDualConstraint)
  private
    FSlideDirection: TAffineVector;
    FConstrained: Boolean;
  protected
    procedure SetSlideDirection(const Value: TAffineVector);
  public
    procedure SatisfyConstraint(const iteration, maxIterations: Integer); override;
    property SlideDirection: TAffineVector read FSlideDirection write SetSlideDirection;
    // Constrain NodeB to the halfplane defined by NodeA and SlideDirection.
    property Constrained: Boolean read FConstrained write FConstrained;
  end;

  // Sphere Ñollision Friction Constraint
  TgxVerletFrictionSphere = class(TgxVerletGlobalFrictionConstraintSphere)
  private
    FRadius: Single;
  public
    function GetBSphere: TBSphere; override;
    procedure SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    procedure SatisfyConstraintForEdge(const aEdge: TgxVerletEdge;
      const iteration, maxIterations: Integer); override;
    property Radius: Single read FRadius write FRadius;
  end;

  (* Cylinder collision Friction Constraint.
    The cylinder is considered infinite by this constraint. *)
  TgxVerletFrictionCylinder = class(TgxVerletGlobalFrictionConstraint)
  private
    FAxis: TAffineVector;
    FRadius, FRadius2: Single;
  protected
    procedure SetRadius(const val: Single);
  public
    procedure SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    (* A base point on the cylinder axis.
      Can theoretically be anywhere, however, to reduce floating point
      precision issues, choose it in the area where collision detection
      will occur. *)
    /// property Base : TAffineVector read FBase write FBase;
    (* Cylinder axis vector. Must be normalized. *)
    property Axis: TAffineVector read FAxis write FAxis;
    // Cylinder radius.
    property Radius: Single read FRadius write SetRadius;
  end;

  // Cube Ñollision Friction Constraint.
  TgxVerletFrictionCube = class(TgxVerletGlobalFrictionConstraintBox)
  private
    FHalfSides: TAffineVector;
    FSides: TAffineVector;
    FDirection: TAffineVector;
    procedure SetSides(const Value: TAffineVector);
  public
    function GetAABB: TAABB; override;
    procedure SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    // Broken and very slow!
    procedure SatisfyConstraintForEdge(const aEdge: TgxVerletEdge;
      const iteration, maxIterations: Integer); override;
    property Direction: TAffineVector read FDirection write FDirection;
    property Sides: TAffineVector read FSides write SetSides;
  end;

  // Capsule collision Friction Constraint.
  TgxVerletFrictionCapsule = class(TgxVerletGlobalFrictionConstraintSphere)
  private
    FAxis: TAffineVector;
    FRadius, FRadius2, FLength, FLengthDiv2: Single;
  protected
    procedure SetAxis(const val: TAffineVector);
    procedure SetRadius(const val: Single);
    procedure SetLength(const val: Single);
  public
    function GetBSphere: TBSphere; override;
    procedure SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    procedure SatisfyConstraintForEdge(const aEdge: TgxVerletEdge;
      const iteration, maxIterations: Integer); override;
    // property Base : TAffineVector read FBase write FBase;
    property Axis: TAffineVector read FAxis write SetAxis;
    property Radius: Single read FRadius write SetRadius;
    property Length: Single read FLength write SetLength;
  end;

  (* Specialized verlet node that can be anchored to a GLScene object. If it's
    anchored and has the property "NailedDown" set, it will remain in the same
    relative position to the GLScene object. *)
  TgxVerletNode = class(TgxBaseVerletNode)
  private
    FRelativePosition: TAffineVector;
    FGXBaseSceneObject: TgxBaseSceneObject;
    procedure SetBaseSceneObject(const Value: TgxBaseSceneObject);
  protected
    procedure SetLocation(const Value: TAffineVector); override;
  public
    procedure Verlet(const vpt: TgxProgressTimes); override;
    property GXBaseSceneObject: TgxBaseSceneObject read FGXBaseSceneObject
      write SetBaseSceneObject;
    property RelativePosition: TAffineVector read FRelativePosition
      write FRelativePosition;
  end;

  // Verlet Hair class
  TgxVerletHair = class
  private
    FNodeList: TgxVerletNodeList;
    FLinkCount: integer;
    FRootDepth: single;
    FVerletWorld: TgxVerletWorld;
    FHairLength: single;
    FData: pointer;
    FStiffness: TgxStiffnessSetVH;
    FStiffnessList: TList;
    function GetAnchor: TgxBaseVerletNode;
    function GetRoot: TgxBaseVerletNode;
    function GetLinkLength: single;
    procedure AddStickStiffness(const ANodeSkip: integer);
    procedure SetStiffness(const Value: TgxStiffnessSetVH);
  public
    procedure BuildHair(const AAnchorPosition, AHairDirection: TAffineVector);
    procedure BuildStiffness;
    procedure ClearStiffness;
    procedure Clear;
    constructor Create(const AVerletWorld: TgxVerletWorld;
      const ARootDepth, AHairLength: single; ALinkCount: integer;
      const AAnchorPosition, AHairDirection: TAffineVector;
      const AStiffness: TgxStiffnessSetVH);
    destructor Destroy; override;
    property NodeList: TgxVerletNodeList read FNodeList;
    property VerletWorld: TgxVerletWorld read FVerletWorld;
    property RootDepth: single read FRootDepth;
    property LinkLength: single read GetLinkLength;
    property LinkCount: integer read FLinkCount;
    property HairLength: single read FHairLength;
    property Stiffness: TgxStiffnessSetVH read FStiffness write SetStiffness;
    property Data: pointer read FData write FData;
    // Anchor should be nailed down to give the hair stability
    property Anchor: TgxBaseVerletNode read GetAnchor;
    // Root should be nailed down to give the hair stability
    property Root: TgxBaseVerletNode read GetRoot;
  end;

  // Base Verlet Skeleton Collider class.
  TgxVerletSkeletonCollider = class(TgxSkeletonCollider)
  private
    FVerletConstraint: TgxVerletConstraint;
  public
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
    procedure AddToVerletWorld(VerletWorld: TgxVerletWorld); virtual;
    // The verlet constraint is created through the AddToVerletWorld procedure
    property VerletConstraint: TgxVerletConstraint read FVerletConstraint;
  end;

  // Sphere shaped verlet constraint in a skeleton collider
  TgxVerletSphere = class(TgxVerletSkeletonCollider)
  private
    FRadius: Single;
  protected
    procedure SetRadius(const Val: Single);
  public
    constructor Create; override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
    procedure AddToVerletWorld(VerletWorld: TgxVerletWorld); override;
    procedure AlignCollider; override;
    property Radius: Single read FRadius write SetRadius;
  end;

  // Capsule shaped verlet constraint in a skeleton collider
  TgxVerletCapsule = class(TgxVerletSkeletonCollider)
  private
    FRadius, FLength: Single;
  protected
    procedure SetRadius(const Val: Single);
    procedure SetLength(const Val: Single);
  public
    constructor Create; override;
    procedure WriteToFiler(Writer: TgxVirtualWriter); override;
    procedure ReadFromFiler(Reader: TgxVirtualReader); override;
    procedure AddToVerletWorld(VerletWorld: TgxVerletWorld); override;
    procedure AlignCollider; override;
    property Radius: Single read FRadius write SetRadius;
    property Length: Single read FLength write SetLength;
  end;

(* After loading call this function to add all the constraints in a
   skeleton collider list to a given verlet world. *)
procedure AddVerletConstriantsToVerletWorld
  (Colliders: TgxSkeletonColliderList; World: TgxVerletWorld);

function CreateVerletPlaneFromGLPlane(Plane: TgxPlane; VerletWorld: TgxVerletWorld;
  Offset: Single): TgxVerletFloor;

// ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------

function CreateVerletPlaneFromGLPlane(Plane: TgxPlane; VerletWorld: TgxVerletWorld;
  Offset: Single): TgxVerletFloor;
begin
  result := TgxVerletFloor.Create(VerletWorld);
  with result do
  begin
    Normal := VectorNormalize(Plane.Direction.AsAffineVector);

    Location := VectorAdd(Plane.Position.AsAffineVector,
      VectorScale(Normal, Offset));
  end;
end;

// ----------------------------
// TgxVerletNode
// ----------------------------
procedure TgxVerletNode.SetBaseSceneObject(const Value: TgxBaseSceneObject);
begin
  FGXBaseSceneObject := Value;

  if Assigned(GXBaseSceneObject) and NailedDown then
    FRelativePosition := AffineVectorMake
      (GXBaseSceneObject.AbsoluteToLocal(VectorMake(FLocation, 1)));
end;

procedure TgxVerletNode.SetLocation(const Value: TAffineVector);
begin
  inherited;
  if Assigned(GXBaseSceneObject) and NailedDown then
    FRelativePosition := GXBaseSceneObject.AbsoluteToLocal(Value);
end;

procedure TgxVerletNode.Verlet(const vpt: TgxProgressTimes);
begin
  if Assigned(GXBaseSceneObject) and NailedDown then
  begin
    FLocation := GXBaseSceneObject.LocalToAbsolute(FRelativePosition);
  end
  else
    inherited;
end;

// ------------------
// ------------------ TgxBaseVerletNode ------------------
// ------------------

constructor TgxBaseVerletNode.CreateOwned(const aOwner: TgxVerletWorld);
begin
  inherited CreateOwned(aOwner.SpacePartition);
  if Assigned(aOwner) then
    aOwner.AddNode(Self);

  FWeight := 1;
  FInvWeight := 1;
  FRadius := 0;
  FFriction := 1;
end;

destructor TgxBaseVerletNode.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.RemoveNode(Self);

  inherited;
end;

(*
  TODO: Improve the friction calculations
  Friction = - NormalForce * FrictionConstant
  To compute the NormalForce, which is the force acting on the normal of the
  collider, we can use the fact that F = m*a.
  m is the weight of the node, a is the acceleration (retardation) caused by the
  collission.
  Acceleration := - PenetrationDepth / Owner.FCurrentDeltaTime;
  The force with which the node has been "stopped" from penetration
  NormalForce := Weight * Acceleration;
  This force should be applied to stopping the movement.
*)
procedure TgxBaseVerletNode.ApplyFriction(const friction, penetrationDepth: Single;
  const surfaceNormal: TAffineVector);
var
  frictionMove, move, moveNormal: TAffineVector;
  realFriction: Single;
begin
  if (penetrationDepth > 0) then
  begin
    realFriction := friction * FFriction;
    if realFriction > 0 then
    begin
      VectorSubtract(Location, OldLocation, move);
      moveNormal := VectorScale(surfaceNormal,
        VectorDotProduct(move, surfaceNormal));
      frictionMove := VectorSubtract(move, moveNormal);
      if penetrationDepth > Radius then
        ScaleVector(frictionMove, realFriction)
      else
        ScaleVector(frictionMove, realFriction *
          Sqrt(penetrationDepth / Radius));
      VectorAdd(OldLocation, frictionMove, FOldLocation);
    end;
  end;
end;

procedure TgxBaseVerletNode.OldApplyFriction(const friction, penetrationDepth
  : Single);
var
  frictionMove, move: TAffineVector;
  // pd : Single;
begin
  VectorSubtract(Location, OldLocation, move);
  VectorScale(move, friction * FFriction, frictionMove);
  // pd:=Abs(penetrationDepth);
  // ScaleVector(frictionMove, friction*pd);
  VectorAdd(OldLocation, frictionMove, FOldLocation);
end;

function TgxBaseVerletNode.DistanceToNode(const node: TgxBaseVerletNode): Single;
begin
  result := VectorDistance(Location, node.Location);
end;

function TgxBaseVerletNode.GetMovement: TAffineVector;
begin
  result := VectorSubtract(Location, OldLocation);
end;

procedure TgxBaseVerletNode.Initialize;
begin
  FOldLocation := Location;
end;

procedure TgxBaseVerletNode.SetWeight(const Value: Single);
begin
  FWeight := Value;
  if Value <> 0 then
    FInvWeight := 1 / Value
  else
    FInvWeight := 1;
end;

procedure TgxBaseVerletNode.Verlet(const vpt: TgxProgressTimes);
var
  newLocation, temp, move, accel: TAffineVector;
begin
  if NailedDown then
  begin
    FOldLocation := Location;
  end
  else
  begin
    if Owner.Inertia then
    begin
      temp := Location;
      VectorSubtract(Location, OldLocation, move);
      ScaleVector(move, 1 - Owner.Drag); // *Sqr(deltaTime));
      VectorAdd(Location, move, newLocation);
      VectorScale(Force, vpt.sqrDeltaTime * FInvWeight, accel);
      AddVector(newLocation, accel);
      Location := newLocation;
      FOldLocation := temp;
    end
    else
    begin
      newLocation := Location;
      VectorScale(Force, vpt.sqrDeltaTime * FInvWeight, accel);
      AddVector(newLocation, accel);
      Location := newLocation;
      FOldLocation := Location;
    end;
  end;
end;

procedure TgxBaseVerletNode.AfterProgress;
begin
  // nothing here, reserved for subclass use
end;

// ------------------
// ------------------ TgxVerletNodeList ------------------
// ------------------
function TgxVerletNodeList.GetItems(i: Integer): TgxBaseVerletNode;
begin
  result := Get(i);
end;

procedure TgxVerletNodeList.SetItems(i: Integer; const Value: TgxBaseVerletNode);
begin
  Put(i, Value);
end;

function TgxBaseVerletNode.GetSpeed: TAffineVector;
begin
  result := VectorScale(VectorSubtract(FLocation, FOldLocation),
    1 / Owner.CurrentDeltaTime);
end;

// ------------------
// ------------------ TgxVerletConstraint ------------------
// ------------------
constructor TgxVerletConstraint.Create(const aOwner: TgxVerletWorld);
begin
  inherited Create;
  if Assigned(aOwner) then
    aOwner.AddConstraint(Self);
  FEnabled := True;
end;

destructor TgxVerletConstraint.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.RemoveConstraint(Self);
  inherited;
end;

procedure TgxVerletConstraint.BeforeIterations;
begin
  // NADA!
end;

// ------------------
// ------------------ TgxVerletDualConstraint ------------------
// ------------------
procedure TgxVerletDualConstraint.RemoveNode(const aNode: TgxBaseVerletNode);
begin
  if FNodeA = aNode then
    FNodeA := nil;
  if FNodeB = aNode then
    FNodeB := nil;
  if (FNodeA = nil) and (FNodeA = nil) then
    Free;
end;

// ------------------
// ------------------ TgxVerletGroupConstraint ------------------
// ------------------
constructor TgxVerletGroupConstraint.Create(const aOwner: TgxVerletWorld);
begin
  inherited Create(aOwner);
  FNodes := TgxVerletNodeList.Create;
end;

destructor TgxVerletGroupConstraint.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TgxVerletGroupConstraint.RemoveNode(const aNode: TgxBaseVerletNode);
begin
  FNodes.Remove(aNode);
end;

// ------------------
// ------------------ TgxVerletGlobalConstraint ------------------
// ------------------
procedure TgxVerletGlobalConstraint.AddKickbackForceAt(const Pos: TAffineVector;
  const Force: TAffineVector);
var
  dPos: TAffineVector;
begin
  // Sum forces
  AddVector(FKickbackForce, Force);
  // Sum torques
  dPos := VectorSubtract(Pos, FLocation);
  AddVector(FKickbackTorque, VectorCrossProduct(dPos, Force));
end;

function TgxVerletGlobalConstraint.TranslateKickbackTorque(const TorqueCenter
  : TAffineVector): TAffineVector;
begin
  // EM(b) = EM(a) + EF x VectorSubtract(b, a).
  result := VectorAdd(FKickbackTorque,
    VectorCrossProduct(VectorSubtract(TorqueCenter, FLocation),
    FKickbackForce));
end;

procedure TgxVerletGlobalConstraint.BeforeIterations;
begin
  inherited;
  FKickbackForce := NullVector;
  FKickbackTorque := NullVector;
end;

procedure TgxVerletGlobalConstraint.RemoveNode(const aNode: TgxBaseVerletNode);
begin
  // nothing to do here
end;

procedure TgxVerletGlobalConstraint.SetLocation(const Value: TAffineVector);
begin
  FLocation := Value;
end;

procedure TgxVerletGlobalConstraint.SatisfyConstraint(const iteration,
  maxIterations: Integer);
var
  i: Integer;
  node: TgxBaseVerletNode;
begin
  if cctNode in Owner.CollisionConstraintTypes then
    for i := 0 to Owner.Nodes.Count - 1 do
    begin
      node := TgxBaseVerletNode(Owner.Nodes[i]);
      if not node.NailedDown then
        SatisfyConstraintForNode(node, iteration, maxIterations);
    end; // }

  if cctEdge in Owner.CollisionConstraintTypes then
    for i := 0 to Owner.SolidEdges.Count - 1 do
    begin
      SatisfyConstraintForEdge(Owner.SolidEdges[i], iteration, maxIterations);
    end; // }
end;

procedure TgxVerletGlobalConstraint.SatisfyConstraintForEdge
  (const aEdge: TgxVerletEdge; const iteration, maxIterations: Integer);
begin
  // Purely virtual, but can't be abstract...
end;

// ------------------
// ------------------ TgxVerletGlobalFrictionConstraint ------------------
// ------------------
constructor TgxVerletGlobalFrictionConstraint.Create(const aOwner
  : TgxVerletWorld);
begin
  inherited;
  FFrictionRatio := cDEFAULT_CONSTRAINT_FRICTION;
end;

// ------------------
// ------------------ TgxVerletGlobalFrictionConstraintSP ------------------
// ------------------
procedure TgxVerletGlobalFrictionConstraintSP.SatisfyConstraint(const iteration,
  maxIterations: Integer);
var
  i: Integer;
  node: TgxBaseVerletNode;
  edge: TgxVerletEdge;
  SP: TBaseSpacePartition;
  Leaf: TSpacePartitionLeaf;
begin
  if Owner.SpacePartition = nil then
  begin
    inherited;
    Exit;
  end;

  PerformSpaceQuery;
  SP := Owner.SpacePartition;
  for i := 0 to SP.QueryResult.Count - 1 do
  begin
    Leaf := SP.QueryResult[i];
    if Leaf is TgxBaseVerletNode then
    begin
      if cctNode in Owner.CollisionConstraintTypes then
      begin
        node := Leaf as TgxBaseVerletNode;
        if not node.NailedDown then
          SatisfyConstraintForNode(node, iteration, maxIterations);
      end;
    end
    else if Leaf is TgxVerletEdge then
    begin
      if cctEdge in Owner.CollisionConstraintTypes then
      begin
        edge := Leaf as TgxVerletEdge;
        SatisfyConstraintForEdge(edge, iteration, maxIterations);
      end;
    end
    else
      Assert(False, 'Bad objects in list!');
  end;
end;

// ------------------
// ------------------ TgxVerletConstraintList ------------------
// ------------------
function TgxVerletConstraintList.GetItems(i: Integer): TgxVerletConstraint;
begin
  result := Get(i);
end;

procedure TgxVerletConstraintList.SetItems(i: Integer;
  const Value: TgxVerletConstraint);
begin
  Put(i, Value);
end;

// ------------------
// ------------------ TgxVerletForce ------------------
// ------------------
constructor TgxVerletForce.Create(const aOwner: TgxVerletWorld);
begin
  inherited Create;
  if Assigned(aOwner) then
    aOwner.AddForce(Self);
end;

destructor TgxVerletForce.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.RemoveForce(Self);
  inherited;
end;

// ------------------
// ------------------ TgxVerletGroupForce ------------------
// ------------------
constructor TgxVerletGroupForce.Create(const aOwner: TgxVerletWorld);
begin
  inherited Create(aOwner);
  FNodes := TgxVerletNodeList.Create;
end;

destructor TgxVerletGroupForce.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TgxVerletGroupForce.RemoveNode(const aNode: TgxBaseVerletNode);
begin
  FNodes.Remove(aNode);
end;

// ------------------
// ------------------ TgxVerletGlobalForce ------------------
// ------------------
procedure TgxVerletGlobalForce.RemoveNode(const aNode: TgxBaseVerletNode);
begin
  // nothing to do here
end;

procedure TgxVerletGlobalForce.AddForce;
var
  i: Integer;
  node: TgxBaseVerletNode;
begin
  for i := 0 to Owner.Nodes.Count - 1 do
  begin
    node := TgxBaseVerletNode(Owner.Nodes.List[i]);
    if not node.NailedDown then
      AddForceToNode(node);
  end;
end;

// ------------------
// ------------------ TgxVerletDualForce ------------------
// ------------------
procedure TgxVerletDualForce.RemoveNode(const aNode: TgxBaseVerletNode);
begin
  if FNodeA = aNode then
    FNodeA := nil;
  if FNodeB = aNode then
    FNodeB := nil;
end;

// ------------------
// ------------------ TgxVerletForceList ------------------
// ------------------
function TgxVerletForceList.GetItems(i: Integer): TgxVerletForce;
begin
  result := Get(i);
end;

procedure TgxVerletForceList.SetItems(i: Integer; const Value: TgxVerletForce);
begin
  Put(i, Value);
end;

// ------------------
// ------------------ TgxVerletWorld ------------------
// ------------------
constructor TgxVerletWorld.Create;
begin
  inherited;
  FDrag := G_DRAG;
  FNodes := TgxVerletNodeList.Create;
  FConstraints := TgxVerletConstraintList.Create;
  FConstraintsWithBeforeIterations := TgxVerletConstraintList.Create;
  FForces := TgxVerletForceList.Create;
  FMaxDeltaTime := 0.02;
  FIterations := 3;
  FSolidEdges := TgxVerletEdgeList.Create;
  FCurrentStepCount := 0;
  FUpdateSpacePartion := uspNever;
  FCollisionConstraintTypes := [cctNode, cctEdge];
  FSpacePartition := nil;
  FVerletNodeClass := TgxBaseVerletNode;
  FInertia := True;
end;

destructor TgxVerletWorld.Destroy;
var
  i: Integer;
begin
  // Delete all nodes
  for i := 0 to FNodes.Count - 1 do
    with FNodes[i] do
    begin
      FOwner := nil;
      Free;
    end;
  FreeAndNil(FNodes);
  // Delete all constraints
  for i := 0 to FConstraints.Count - 1 do
    with FConstraints[i] do
    begin
      FOwner := nil;
      Free;
    end;
  FreeAndNil(FConstraints);
  // Delete all forces
  for i := 0 to FForces.Count - 1 do
    with FForces[i] do
    begin
      FOwner := nil;
      Free;
    end;
  FreeAndNil(FForces);
  FreeAndNil(FConstraintsWithBeforeIterations);
  for i := 0 to FSolidEdges.Count - 1 do
    FSolidEdges[i].Free;
  FreeAndNil(FSolidEdges);
  FreeAndNil(FSpacePartition);
  inherited;
end;

procedure TgxVerletWorld.AccumulateForces(const vpt: TgxProgressTimes);
var
  i: Integer;
begin
  // First of all, reset all forces
  for i := 0 to FNodes.Count - 1 do
    FNodes[i].FForce := NullVector;
  // Now, update all forces in the assembly!
  for i := 0 to FForces.Count - 1 do
    FForces[i].AddForce(vpt);
end;

function TgxVerletWorld.AddNode(const aNode: TgxBaseVerletNode): Integer;
begin
  if Assigned(aNode.FOwner) then
    aNode.Owner.FNodes.Remove(aNode);
  result := FNodes.Add(aNode);
  aNode.FOwner := Self;
end;

procedure TgxVerletWorld.RemoveNode(const aNode: TgxBaseVerletNode);
var
  i: Integer;
begin
  if aNode.Owner = Self then
  begin
    FNodes.Remove(aNode);
    aNode.FOwner := nil;
    // drop refs in constraints
    for i := FConstraints.Count - 1 downto 0 do
      FConstraints[i].RemoveNode(aNode);
    // drop refs in forces
    for i := FForces.Count - 1 downto 0 do
      FForces[i].RemoveNode(aNode);
  end;
end;

function TgxVerletWorld.AddConstraint(const aConstraint
  : TgxVerletConstraint): Integer;
begin
  if Assigned(aConstraint.FOwner) then
    aConstraint.Owner.FConstraints.Remove(aConstraint);
  result := FConstraints.Add(aConstraint);
  aConstraint.FOwner := Self;
end;

procedure TgxVerletWorld.RemoveConstraint(const aConstraint
  : TgxVerletConstraint);
begin
  if aConstraint.Owner = Self then
  begin
    FConstraints.Remove(aConstraint);
    aConstraint.FOwner := nil;
  end;
end;

function TgxVerletWorld.AddForce(const aForce: TgxVerletForce): Integer;
begin
  if Assigned(aForce.FOwner) then
    aForce.Owner.FForces.Remove(aForce);
  result := FForces.Add(aForce);
  aForce.FOwner := Self;
end;

procedure TgxVerletWorld.RemoveForce(const aForce: TgxVerletForce);
begin
  if aForce.Owner = Self then
  begin
    FForces.Remove(aForce);
    aForce.FOwner := nil;
  end;
end;

procedure TgxVerletWorld.AddSolidEdge(const aNodeA, aNodeB: TgxBaseVerletNode);
var
  VerletEdge: TgxVerletEdge;
begin
  VerletEdge := TgxVerletEdge.CreateEdgeOwned(aNodeA, aNodeB);
  SolidEdges.Add(VerletEdge);
end;

function TgxVerletWorld.FirstNode: TgxBaseVerletNode;
begin
  Assert(FNodes.Count > 0, 'There are no nodes in the assembly!');
  result := FNodes[0];
end;

function TgxVerletWorld.LastNode: TgxBaseVerletNode;
begin
  Assert(FNodes.Count > 0, 'There are no nodes in the assembly!');
  result := FNodes[FNodes.Count - 1];
end;

function TgxVerletWorld.CreateOwnedNode(const Location: TAffineVector;
  const aRadius: Single = 0; const aWeight: Single = 1): TgxBaseVerletNode;
begin
  result := VerletNodeClass.CreateOwned(Self);
  result.Location := Location;
  result.OldLocation := Location;
  result.Weight := aWeight;
  result.Radius := aRadius;
end;

function TgxVerletWorld.CreateStick(const aNodeA, aNodeB: TgxBaseVerletNode;
  const Slack: Single = 0): TgxVerletStick;
begin
  Assert(aNodeA <> aNodeB, 'Can''t create stick between same node!');
  result := TgxVerletStick.Create(Self);
  result.NodeA := aNodeA;
  result.NodeB := aNodeB;
  result.SetRestLengthToCurrent;
  result.Slack := Slack;
end;

function TgxVerletWorld.CreateSpring(const aNodeA, aNodeB: TgxBaseVerletNode;
  const aStrength, aDamping: Single; const aSlack: Single = 0): TgxVerletSpring;
begin
  result := TgxVerletSpring.Create(Self);
  result.NodeA := aNodeA;
  result.NodeB := aNodeB;
  result.Strength := aStrength;
  result.Damping := aDamping;
  result.Slack := aSlack;
  result.SetRestLengthToCurrent;
end;

function TgxVerletWorld.CreateSlider(const aNodeA, aNodeB: TgxBaseVerletNode;
  const aSlideDirection: TAffineVector): TgxVerletSlider;
begin
  result := TgxVerletSlider.Create(Self);
  result.NodeA := aNodeA;
  result.NodeB := aNodeB;
  result.SlideDirection := aSlideDirection;
end;

procedure TgxVerletWorld.Initialize;
var
  i: Integer;
begin
  for i := 0 to FNodes.Count - 1 do
    FNodes[i].Initialize;
end;

function TgxVerletWorld.Progress(const deltaTime, newTime: Double): Integer;
var
  i: Integer;
  ticks: Integer;
  myDeltaTime: Single;
  vpt: TgxProgressTimes;
begin
  ticks := 0;
  myDeltaTime := FMaxDeltaTime;
  FCurrentDeltaTime := FMaxDeltaTime;
  FInvCurrentDeltaTime := 1 / FCurrentDeltaTime;

  vpt.deltaTime := myDeltaTime;
  vpt.sqrDeltaTime := Sqr(myDeltaTime);
  vpt.invSqrDeltaTime := 1 / vpt.sqrDeltaTime;

  while FSimTime < newTime do
  begin
    Inc(ticks);
    FSimTime := FSimTime + myDeltaTime;
    vpt.newTime := FSimTime;
    Verlet(vpt);
    AccumulateForces(vpt);
    SatisfyConstraints(vpt);

    if FInertaPauseSteps > 0 then
    begin
      dec(FInertaPauseSteps);
      if FInertaPauseSteps = 0 then
        Inertia := True;
    end;
    Break;
  end;

  result := ticks;
  for i := 0 to FNodes.Count - 1 do
    FNodes[i].AfterProgress;
end;

procedure TgxVerletWorld.DoUpdateSpacePartition;
var
  i: Integer;
begin
  if Assigned(SpacePartition) then
  begin
    for i := 0 to FSolidEdges.Count - 1 do
      if (FSolidEdges[i].FNodeA.FChangedOnStep = FCurrentStepCount) or
        (FSolidEdges[i].FNodeB.FChangedOnStep = FCurrentStepCount) then
        FSolidEdges[i].Changed;

    for i := 0 to FNodes.Count - 1 do
      if (FNodes[i].FChangedOnStep = FCurrentStepCount) then
        FNodes[i].Changed;
  end;
end;

procedure TgxVerletWorld.SatisfyConstraints(const vpt: TgxProgressTimes);
var
  i, j: Integer;
  Constraint: TgxVerletConstraint;
begin
  for i := 0 to FConstraintsWithBeforeIterations.Count - 1 do
  begin
    Constraint := FConstraintsWithBeforeIterations[i];
    Constraint.BeforeIterations;
  end;
  if UpdateSpacePartion = uspEveryFrame then
    Inc(FCurrentStepCount);
  for j := 0 to Iterations - 1 do
  begin
    for i := 0 to FConstraints.Count - 1 do
      with FConstraints[i] do
        if Enabled then
          SatisfyConstraint(j, Iterations); // }
    if UpdateSpacePartion = uspEveryIteration then
      DoUpdateSpacePartition;
  end;

  if UpdateSpacePartion = uspEveryFrame then
    DoUpdateSpacePartition; // }
end;

procedure TgxVerletWorld.Verlet(const vpt: TgxProgressTimes);
var
  i: Integer;
begin
  if UpdateSpacePartion <> uspNever then
    Inc(FCurrentStepCount);

  for i := 0 to FNodes.Count - 1 do
    FNodes[i].Verlet(vpt);

  if UpdateSpacePartion <> uspNever then
    DoUpdateSpacePartition;
end;

// ------------------
// ------------------ TgxVerletGravity ------------------
// ------------------
constructor TgxVerletGravity.Create(const aOwner: TgxVerletWorld);
begin
  inherited;
  FGravity.X := 0;
  FGravity.Y := -9.81;
  FGravity.Z := 0;
end;

procedure TgxVerletGravity.AddForceToNode(const aNode: TgxBaseVerletNode);
begin
  CombineVector(aNode.FForce, Gravity, @aNode.Weight);
end;

// ------------------
// TgxVerletSpring
// ------------------
procedure TgxVerletSpring.SetSlack(const Value: Single);
begin
  if Value <= 0 then
    FSlack := 0
  else
    FSlack := Value;
end;

procedure TgxVerletSpring.AddForce;
var
  hTerm, dTerm: Single;
  deltaV, Force: TAffineVector;
  deltaLength: Single;
begin
  VectorSubtract(NodeA.Location, NodeB.Location, Force);
  deltaLength := VectorLength(Force);
  if deltaLength > FSlack then
  begin
    hTerm := (FRestLength - deltaLength) * FForceFactor;
    Force := VectorScale(Force, hTerm / deltaLength);
  end
  else
    Force := NullVector;
  if FDamping <> 0 then
  begin
    VectorSubtract(NodeA.GetMovement, NodeB.GetMovement, deltaV);
    dTerm := -0.25 * FDamping * vpt.invSqrDeltaTime;
    CombineVector(Force, deltaV, dTerm);
  end;
  AddVector(NodeA.FForce, Force);
  SubtractVector(NodeB.FForce, Force);
end;

procedure TgxVerletSpring.SetRestLengthToCurrent;
begin
  FRestLength := VectorDistance(NodeA.Location, NodeB.Location);
  FForceFactor := FStrength / FRestLength;
end;

// ------------------
// ------------------ TgxVerletAirResistance ------------------
// ------------------
procedure TgxVerletAirResistance.AddForceToNode(const aNode: TgxBaseVerletNode);
var
  s, F, FCurrentWindBurst: TAffineVector;
  sMag: Single;
  r: Single;
  Chaos: Single;
begin
  s := aNode.Speed;

  if FWindMagnitude <> 0 then
  begin
    Chaos := FWindMagnitude * FWindChaos;
    FCurrentWindBurst.X := FWindDirection.X * FWindMagnitude + Chaos *
      (Random - 0.5) * 2;
    FCurrentWindBurst.Y := FWindDirection.Y * FWindMagnitude + Chaos *
      (Random - 0.5) * 2;
    FCurrentWindBurst.Z := FWindDirection.Z * FWindMagnitude + Chaos *
      (Random - 0.5) * 2;

    s := VectorSubtract(s, FCurrentWindBurst);
  end;
  sMag := VectorLength(s);
  r := aNode.Radius + 1;
  if sMag <> 0 then
  begin
    F := VectorScale(s, -Sqr(sMag) * Sqr(r) * pi * FDragCoeff);
    aNode.FForce := VectorAdd(aNode.FForce, F);
  end;
end;

constructor TgxVerletAirResistance.Create(const aOwner: TgxVerletWorld);
begin
  inherited;
  FDragCoeff := 0.001;
  FWindDirection.X := 0;
  FWindDirection.Y := 0;
  FWindDirection.Z := 0;
  FWindMagnitude := 0;
  FWindChaos := 0;
end;

procedure TgxVerletAirResistance.SetWindDirection(const Value: TAffineVector);
begin
  FWindDirection := VectorNormalize(Value);
end;

// ------------------
// ------------------ TgxVerletFloor ------------------
// ------------------
constructor TgxVerletFloor.Create(const aOwner: TgxVerletWorld);
begin
  inherited;
  MakeVector(FNormal, 0, 1, 0);
  MakeVector(FLocation, 0, 0, 0);
end;

procedure TgxVerletFloor.PerformSpaceQuery;
begin
  Owner.SpacePartition.QueryPlane(FLocation, FNormal);
end;

procedure TgxVerletFloor.SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
  const iteration, maxIterations: Integer);
var
  penetrationDepth: Single;
  currentPenetrationDepth: Single;
  d: TAffineVector;
  correction: TAffineVector;
begin
  currentPenetrationDepth := -PointPlaneDistance(aNode.Location, FLocation,
    FNormal) + aNode.Radius + FFloorLevel;
  // Record how far down the node goes
  penetrationDepth := currentPenetrationDepth;
  // Correct the node location
  if currentPenetrationDepth > 0 then
  begin
    correction := VectorScale(FNormal, currentPenetrationDepth);
    if BounceRatio > 0 then
    begin
      d := VectorSubtract(aNode.FLocation, aNode.FOldLocation);
      if FrictionRatio > 0 then
        aNode.ApplyFriction(FrictionRatio, penetrationDepth, FNormal);
      AddVector(aNode.FLocation, correction);
      aNode.FOldLocation := VectorAdd(aNode.FLocation,
        VectorScale(d, BounceRatio));
    end
    else
    begin
      AddVector(aNode.FLocation, correction);
      if FrictionRatio > 0 then
        aNode.ApplyFriction(FrictionRatio, penetrationDepth, FNormal);
      aNode.FChangedOnStep := Owner.CurrentStepCount;
    end;
  end;
end;

procedure TgxVerletFloor.SetNormal(const Value: TAffineVector);
begin
  FNormal := Value;
  NormalizeVector(FNormal);
end;

// ------------------
// TgxVerletHeightField
// ------------------
procedure TgxVerletHeightField.SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
  const iteration, maxIterations: Integer);
var
  penetrationDepth: Single;
  currentPenetrationDepth: Single;
  d: TAffineVector;
  correction: TAffineVector;
begin
  currentPenetrationDepth := -PointPlaneDistance(aNode.Location, FLocation,
    FNormal) + aNode.Radius;
  if Assigned(FOnNeedHeight) then
    currentPenetrationDepth := currentPenetrationDepth +
      FOnNeedHeight(Self, aNode);

  // Record how far down the node goes
  penetrationDepth := currentPenetrationDepth;
  // Correct the node location
  if currentPenetrationDepth > 0 then
  begin
    correction := VectorScale(FNormal, currentPenetrationDepth);
    if BounceRatio > 0 then
    begin
      d := VectorSubtract(aNode.FLocation, aNode.FOldLocation);
      if FrictionRatio > 0 then
        aNode.ApplyFriction(FrictionRatio, penetrationDepth, FNormal);
      AddVector(aNode.FLocation, correction);
      aNode.FOldLocation := VectorAdd(aNode.FLocation,
        VectorScale(d, BounceRatio));
    end
    else
    begin
      AddVector(aNode.FLocation, correction);
      if FrictionRatio > 0 then
        aNode.ApplyFriction(FrictionRatio, penetrationDepth, FNormal);
      aNode.FChangedOnStep := Owner.CurrentStepCount;
    end;
  end;
end;

// ------------------
// TgxVerletStick
// ------------------
procedure TgxVerletStick.SatisfyConstraint(const iteration, maxIterations: Integer);
var
  delta: TAffineVector;
  F, r: Single;
  deltaLength, diff: Single;
const
  cDefaultDelta: TAffineVector = (X: 0.01; Y: 0; Z: 0);
begin
  Assert((NodeA <> NodeB),
    'The nodes are identical - that causes division by zero!');

  VectorSubtract(NodeB.Location, NodeA.Location, delta);
  deltaLength := VectorLength(delta);
  // Avoid div by zero!
  if deltaLength < 1E-3 then
  begin
    delta := cDefaultDelta;
    deltaLength := 0.01;
  end;
  diff := (deltaLength - RestLength) / deltaLength;
  if Abs(diff) > Slack then
  begin
    r := 1 / (NodeA.InvWeight + NodeB.InvWeight);
    if diff < 0 then
      diff := (diff + Slack) * r
    else
      diff := (diff - Slack) * r;
    // Take into acount the different weights of the nodes!
    if not NodeA.NailedDown then
    begin
      F := diff * NodeA.InvWeight;
      CombineVector(NodeA.FLocation, delta, F);
      NodeA.FChangedOnStep := Owner.CurrentStepCount;
    end;
    if not NodeB.NailedDown then
    begin
      F := -diff * NodeB.InvWeight;
      CombineVector(NodeB.FLocation, delta, F);
      NodeB.FChangedOnStep := Owner.CurrentStepCount;
    end;
  end;
end;

procedure TgxVerletStick.SetRestLengthToCurrent;
begin
  FRestLength := VectorDistance(NodeA.Location, NodeB.Location);
end;

// ------------------
// TgxVerletRigidBody
// ------------------
procedure TgxVerletRigidBody.ComputeBarycenter(var barycenter: TAffineVector);
var
  i: Integer;
  totWeight: Single;
begin
  // first we compute the barycenter
  totWeight := 0;
  barycenter := NullVector;
  for i := 0 to Nodes.Count - 1 do
    with Nodes[i] do
    begin
      CombineVector(barycenter, Location, @Weight);
      totWeight := totWeight + Weight;
    end;
  if totWeight > 0 then
    ScaleVector(barycenter, 1 / totWeight);
end;

procedure TgxVerletRigidBody.ComputeNaturals(const barycenter: TAffineVector;
  var natX, natY, natZ: TAffineVector);
var
  i: Integer;
  delta: TAffineVector;
begin
  natX := NullVector;
  natY := NullVector;
  natZ := NullVector;
  for i := 0 to Nodes.Count - 1 do
  begin
    delta := VectorSubtract(Nodes[i].Location, barycenter);
    CombineVector(natX, delta, FNodeParams[i].X);
    CombineVector(natY, delta, FNodeParams[i].Y);
    CombineVector(natZ, delta, FNodeParams[i].Z);
  end;
end;

procedure TgxVerletRigidBody.ComputeRigidityParameters;
var
  i: Integer;
  barycenter: TAffineVector;
  d: Single;
begin
  // first we compute the barycenter
  ComputeBarycenter(barycenter);
  // next the parameters
  SetLength(FNodeParams, Nodes.Count);
  SetLength(FNodeCoords, Nodes.Count);
  for i := 0 to Nodes.Count - 1 do
  begin
    FNodeCoords[i] := VectorSubtract(Nodes[i].Location, barycenter);
    d := Nodes[i].Weight / VectorLength(FNodeCoords[i]);
    FNodeParams[i].X := FNodeCoords[i].X * d;
    FNodeParams[i].Y := FNodeCoords[i].Y * d;
    FNodeParams[i].Z := FNodeCoords[i].Z * d;
  end;
  ComputeNaturals(barycenter, FNatMatrix.X, FNatMatrix.Y, FNatMatrix.Z);
  FNatMatrix.Z := VectorCrossProduct(FNatMatrix.X, FNatMatrix.Y);
  FNatMatrix.Y := VectorCrossProduct(FNatMatrix.Z, FNatMatrix.X);
  NormalizeVector(FNatMatrix.X);
  NormalizeVector(FNatMatrix.Y);
  NormalizeVector(FNatMatrix.Z);
  FInvNatMatrix := FNatMatrix;
  // TransposeMatrix(FInvNatMatrix);
  InvertMatrix(FInvNatMatrix);
end;

procedure TgxVerletRigidBody.SatisfyConstraint(const iteration,
  maxIterations: Integer);
var
  i: Integer;
  barycenter, delta: TAffineVector;
  nrjBase, nrjAdjust: TAffineVector;
  natural: array [0 .. 2] of TAffineVector;
  deltas: array of TAffineVector;
begin
  Assert(Nodes.Count = Length(FNodeParams),
    'You forgot to call ComputeRigidityParameters!');
  // compute the barycenter
  ComputeBarycenter(barycenter);
  // compute the natural axises
  ComputeNaturals(barycenter, natural[0], natural[1], natural[2]);

  natural[2] := VectorCrossProduct(natural[0], natural[1]);
  natural[1] := VectorCrossProduct(natural[2], natural[0]);
  for i := 0 to 2 do
    NormalizeVector(natural[i]);
  natural[0] := VectorTransform(natural[0], FInvNatMatrix);
  natural[1] := VectorTransform(natural[1], FInvNatMatrix);
  natural[2] := VectorTransform(natural[2], FInvNatMatrix);
  // make the natural axises orthonormal, by picking the longest two
  (*
    for i:=0 to 2 do
    vectNorm[i]:=VectorNorm(natural[i]);
    if (vectNorm[0]<vectNorm[1]) and (vectNorm[0]<vectNorm[2]) then begin
    natural[0]:=VectorCrossProduct(natural[1], natural[2]);
    natural[1]:=VectorCrossProduct(natural[2], natural[0]);
    end else if (vectNorm[1]<vectNorm[0]) and (vectNorm[1]<vectNorm[2]) then begin
    natural[1]:=VectorCrossProduct(natural[2], natural[0]);
    natural[2]:=VectorCrossProduct(natural[0], natural[1]);
    end else begin
    natural[2]:=VectorCrossProduct(natural[0], natural[1]);
    natural[0]:=VectorCrossProduct(natural[1], natural[2]);
    end;
  *)

  // now the axises are back, recompute the position of all points
  SetLength(deltas, Nodes.Count);
  nrjBase := NullVector;
  for i := 0 to Nodes.Count - 1 do
  begin
    nrjBase := VectorAdd(nrjBase,
      VectorCrossProduct(VectorSubtract(Nodes[i].Location, barycenter),
      Nodes[i].GetMovement));
  end;

  nrjAdjust := NullVector;
  for i := 0 to Nodes.Count - 1 do
  begin
    delta := VectorCombine3(natural[0], natural[1], natural[2],
      FNodeCoords[i].X, FNodeCoords[i].Y, FNodeCoords[i].Z);
    deltas[i] := VectorSubtract(VectorAdd(barycenter, delta),
      Nodes[i].Location);
    nrjAdjust := VectorAdd(nrjBase,
      VectorCrossProduct(VectorSubtract(Nodes[i].Location, barycenter),
      deltas[i]));
    Nodes[i].Location := VectorAdd(Nodes[i].Location, deltas[i]);
    Nodes[i].FOldLocation := VectorAdd(Nodes[i].FOldLocation, deltas[i]);
    // Nodes[i].FOldLocation:=Nodes[i].Location;
  end;

  deltas[0] := nrjBase;
  deltas[1] := nrjAdjust;
end;

// ------------------
// TgxVerletSlider
// ------------------
procedure TgxVerletSlider.SetSlideDirection(const Value: TAffineVector);
begin
  FSlideDirection := VectorNormalize(Value);
end;

procedure TgxVerletSlider.SatisfyConstraint(const iteration, maxIterations: Integer);
var
  delta: TAffineVector;
  F, r: Single;
  projB: TAffineVector;
begin
  Assert((NodeA <> NodeB),
    'The nodes are identical - that causes division by zero!');

  // project B in the plane defined by A and SlideDirection
  projB := VectorSubtract(NodeB.Location, NodeA.Location);
  F := VectorDotProduct(projB, SlideDirection);
  projB := VectorCombine(NodeB.Location, SlideDirection, 1, -F);
  if Constrained and (F < 0) then
    NodeB.Location := projB;
  VectorSubtract(projB, NodeA.Location, delta);
  // Take into acount the different weights of the nodes!
  r := 1 / (NodeA.InvWeight + NodeB.InvWeight);
  if not NodeA.NailedDown then
  begin
    F := r * NodeA.InvWeight;
    CombineVector(NodeA.FLocation, delta, F);
    NodeA.FChangedOnStep := Owner.CurrentStepCount;
  end;
  if not NodeB.NailedDown then
  begin
    F := -r * NodeB.InvWeight;
    CombineVector(NodeB.FLocation, delta, F);
    NodeB.FChangedOnStep := Owner.CurrentStepCount;
  end;
end;

// ------------------
// ------------------ TgxVerletFrictionSphere ------------------
// ------------------
function TgxVerletFrictionSphere.GetBSphere: TBSphere;
begin
  result.Center := FLocation;
  result.Radius := FRadius;
end;

procedure TgxVerletFrictionSphere.SatisfyConstraintForEdge(const aEdge: TgxVerletEdge;
  const iteration, maxIterations: Integer);
var
  closestPoint, move, delta, contactNormal: TAffineVector;
  deltaLength, diff: Single;
begin
  // If the edge penetrates the sphere, try pushing the nodes until it no
  // longer does
  closestPoint := PointSegmentClosestPoint(FLocation, aEdge.NodeA.FLocation,
    aEdge.NodeB.FLocation);
  // Find the distance between the two
  VectorSubtract(closestPoint, Location, delta);
  deltaLength := VectorLength(delta);
  if deltaLength < Radius then
  begin
    if deltaLength > 0 then
    begin
      contactNormal := VectorScale(delta, 1 / deltaLength);
      aEdge.NodeA.ApplyFriction(FFrictionRatio, Radius - Abs(deltaLength),
        contactNormal);
      aEdge.NodeB.ApplyFriction(FFrictionRatio, Radius - Abs(deltaLength),
        contactNormal);
    end;

    // Move it outside the sphere!
    diff := (Radius - deltaLength) / deltaLength;
    VectorScale(delta, diff, move);

    AddVector(aEdge.NodeA.FLocation, move);
    AddVector(aEdge.NodeB.FLocation, move);

    // Add the force to the kickback
    // F = a * m
    // a = move / deltatime
    AddKickbackForceAt(FLocation, VectorScale(move,
      -(aEdge.NodeA.FWeight + aEdge.NodeB.FWeight) *
      Owner.FInvCurrentDeltaTime));
    aEdge.NodeA.FChangedOnStep := Owner.CurrentStepCount;
    aEdge.NodeB.FChangedOnStep := Owner.CurrentStepCount;
  end;
end;

procedure TgxVerletFrictionSphere.SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
  const iteration, maxIterations: Integer);
var
  delta, move, contactNormal: TAffineVector;
  deltaLength, diff: Single;
begin
  // Find the distance between the two
  VectorSubtract(aNode.Location, Location, delta);

  // Is it inside the sphere?
  deltaLength := VectorLength(delta) - aNode.Radius;
  if Abs(deltaLength) < Radius then
  begin
    if deltaLength > 0 then
    begin
      contactNormal := VectorScale(delta, 1 / deltaLength);
      aNode.ApplyFriction(FFrictionRatio, Radius - Abs(deltaLength),
        contactNormal);
    end
    else
      // Slow it down - this part should not be fired
      aNode.OldApplyFriction(FFrictionRatio, Radius - Abs(deltaLength));

    // Move it outside the sphere!
    diff := (Radius - deltaLength) / deltaLength;
    VectorScale(delta, diff, move);

    AddVector(aNode.FLocation, move);
    aNode.FChangedOnStep := Owner.CurrentStepCount;

    // Add the force to the kickback
    // F = a * m
    // a = move / deltatime
    AddKickbackForceAt(FLocation, VectorScale(move,
      -aNode.FWeight * Owner.FInvCurrentDeltaTime));
  end;
end;

// ------------------
// ------------------ TgxVerletFrictionCylinder ------------------
// ------------------
procedure TgxVerletFrictionCylinder.SetRadius(const val: Single);
begin
  FRadius := val;
  FRadius2 := Sqr(val);
end;

procedure TgxVerletFrictionCylinder.SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
  const iteration, maxIterations: Integer);
var
  Proj, newLocation, move: TAffineVector;
  F, Dist2, penetrationDepth: Single;
begin
  // Compute projection of node position on the axis
  F := PointProject(aNode.Location, FLocation, FAxis);
  Proj := VectorCombine(FLocation, FAxis, 1, F);
  // Sqr distance
  Dist2 := VectorDistance2(Proj, aNode.Location);
  if Dist2 < FRadius2 then
  begin
    // move out of the cylinder
    VectorLerp(Proj, aNode.Location, FRadius * RSqrt(Dist2), newLocation);
    move := VectorSubtract(aNode.FLocation, newLocation);
    penetrationDepth := VectorLength(move);
    aNode.ApplyFriction(FFrictionRatio, penetrationDepth,
      VectorScale(move, 1 / penetrationDepth));
    aNode.FLocation := newLocation;
    aNode.FChangedOnStep := Owner.CurrentStepCount;
  end;
end;

// ------------------
// ------------------ TgxVerletFrictionCube ------------------
// ------------------
function TgxVerletFrictionCube.GetAABB: TAABB;
begin
  VectorAdd(FLocation, FHalfSides, result.max);
  VectorSubtract(FLocation, FHalfSides, result.min);
end;

// BROKEN AND VERY SLOW!
procedure TgxVerletFrictionCube.SatisfyConstraintForEdge(const aEdge: TgxVerletEdge;
  const iteration, maxIterations: Integer);
var
  Corners: array [0 .. 7] of TAffineVector;
  EdgeRelative: array [0 .. 1] of TAffineVector;
  shortestMove (* , contactNormal *) : TAffineVector;
  shortestDeltaLength: Single;
  procedure AddCorner(CornerID: Integer; X, Y, Z: Single);
  begin
    X := (X - 0.5) * 2;
    Y := (Y - 0.5) * 2;
    Z := (Z - 0.5) * 2;
    MakeVector(Corners[CornerID], FHalfSides.X * X, FHalfSides.Y * Y,
      FHalfSides.Z * Z);
    AddVector(Corners[CornerID], FLocation);
  end;

  procedure TryEdge(Corner0, Corner1: Integer);
  var
    CubeEdgeClosest, aEdgeClosest: TAffineVector;
    CenteraEdge, move: TAffineVector;
    deltaLength: Single;
  begin
    SegmentSegmentClosestPoint(Corners[Corner0], Corners[Corner1],
      aEdge.NodeA.FLocation, aEdge.NodeB.FLocation, CubeEdgeClosest,
      aEdgeClosest);
    CenteraEdge := VectorSubtract(aEdgeClosest, FLocation);
    if (Abs(CenteraEdge.X) < FHalfSides.X) and
      (Abs(CenteraEdge.Y) < FHalfSides.Y) and (Abs(CenteraEdge.Z) < FHalfSides.Z)
    then
    begin
      // The distance to move the edge is the difference between CenterCubeEdge and
      // CenteraEdge
      move := VectorSubtract(CubeEdgeClosest, aEdgeClosest);
      deltaLength := VectorLength(move);
      if (deltaLength > 0) and (deltaLength < shortestDeltaLength) then
      begin
        shortestDeltaLength := deltaLength;
        shortestMove := move;
      end;
    end;
  end;

begin
  // DISABLED!
  Exit;
  // Early out test
  EdgeRelative[0] := VectorSubtract(aEdge.FNodeA.FLocation, FLocation);
  EdgeRelative[1] := VectorSubtract(aEdge.FNodeB.FLocation, FLocation);
  // If both edges are on the same side of _any_ box side, the edge can't
  // cut the box
  if ((EdgeRelative[0].X > FHalfSides.X) and (EdgeRelative[1].X > FHalfSides.X))
    or ((EdgeRelative[0].X < -FHalfSides.X) and
    (EdgeRelative[1].X < -FHalfSides.X)) or
    ((EdgeRelative[0].Y > FHalfSides.Y) and (EdgeRelative[1].Y > FHalfSides.Y))
    or ((EdgeRelative[0].Y < -FHalfSides.Y) and
    (EdgeRelative[1].Y < -FHalfSides.Y)) or
    ((EdgeRelative[0].Z > FHalfSides.Z) and (EdgeRelative[1].Z > FHalfSides.Z))
    or ((EdgeRelative[0].Z < -FHalfSides.Z) and
    (EdgeRelative[1].Z < -FHalfSides.Z)) then
  begin
    Exit;
  end;

  // For each cube edge:
  // find closest positions between CubeEdge and aEdge
  // if aEdgeClosestPosition within cube then
  // move nodes until closest position is outside cube
  // exit
  AddCorner(0, 0, 0, 0);
  AddCorner(1, 1, 0, 0);
  AddCorner(2, 1, 1, 0);
  AddCorner(3, 0, 1, 0);

  AddCorner(4, 0, 0, 1);
  AddCorner(5, 1, 0, 1);
  AddCorner(6, 1, 1, 1);
  AddCorner(7, 0, 1, 1);

  shortestDeltaLength := 10E30;

  TryEdge(0, 1);
  TryEdge(1, 2);
  TryEdge(2, 3);
  TryEdge(3, 0);

  TryEdge(4, 5);
  TryEdge(5, 6);
  TryEdge(6, 7);
  TryEdge(7, 4);

  TryEdge(0, 3);
  TryEdge(1, 5);
  TryEdge(2, 6);
  TryEdge(3, 7);

  if shortestDeltaLength < 10E8 then
  begin
    // contactNormal := VectorScale(shortestMove, 1/shortestDeltaLength);
    (* aEdge.NodeA.ApplyFriction(FFrictionRatio, shortestDeltaLength, contactNormal);
      aEdge.NodeB.ApplyFriction(FFrictionRatio, shortestDeltaLength, contactNormal);// *)
    AddVector(aEdge.NodeA.FLocation, shortestMove);
    AddVector(aEdge.NodeB.FLocation, shortestMove);

    aEdge.NodeA.Changed;
    aEdge.NodeB.Changed;
    aEdge.NodeA.FChangedOnStep := Owner.CurrentStepCount;
    aEdge.NodeB.FChangedOnStep := Owner.CurrentStepCount;
  end;
end;

procedure TgxVerletFrictionCube.SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
  const iteration, maxIterations: Integer);
var
  p, absP, contactNormal: TAffineVector;
  dp: Single;
  smallestSide: Integer;
begin
  // TODO: Direction of Cube should be used to rotate the nodes location, as it
  // stands, the cube can only face in one direction.
  p := VectorSubtract(aNode.FLocation, FLocation);

  absP.X := FHalfSides.X - Abs(p.X);
  absP.Y := FHalfSides.Y - Abs(p.Y);
  absP.Z := FHalfSides.Z - Abs(p.Z);
  if (PInteger(@absP.X)^ <= 0) or (PInteger(@absP.Y)^ <= 0) or
    (PInteger(@absP.Z)^ <= 0) then
    Exit;
  if absP.X < absP.Y then
    if absP.X < absP.Z then
      smallestSide := 0
    else
      smallestSide := 2
  else if absP.Y < absP.Z then
    smallestSide := 1
  else
    smallestSide := 2;
  contactNormal := NullVector;
  // Only move along the "shortest" axis
  if PInteger(@p.V[smallestSide])^ >= 0 then
  begin
    dp := absP.V[smallestSide];
    contactNormal.V[smallestSide] := 1;
    aNode.ApplyFriction(FFrictionRatio, dp, contactNormal);
    aNode.FLocation.V[smallestSide] := aNode.FLocation.V[smallestSide] + dp;
  end
  else
  begin
    dp := absP.V[smallestSide];
    contactNormal.V[smallestSide] := -1;
    aNode.ApplyFriction(FFrictionRatio, dp, contactNormal);
    aNode.FLocation.V[smallestSide] := aNode.FLocation.V[smallestSide] - dp;
  end;
  aNode.FChangedOnStep := Owner.CurrentStepCount;
end;

procedure TgxVerletFrictionCube.SetSides(const Value: TAffineVector);
begin
  FSides := Value;
  FHalfSides := VectorScale(Sides, 0.5);
  UpdateCachedAABB;
end;

// ------------------
// ------------------ TgxVerletFrictionCapsule ------------------
// ------------------

procedure TgxVerletFrictionCapsule.SetAxis(const val: TAffineVector);
begin
  FAxis := VectorNormalize(val);
  UpdateCachedBSphere;
end;

procedure TgxVerletFrictionCapsule.SetLength(const val: Single);
begin
  FLength := val;
  FLengthDiv2 := val * 0.5;
  UpdateCachedBSphere;
end;

procedure TgxVerletFrictionCapsule.SetRadius(const val: Single);
begin
  FRadius := val;
  FRadius2 := Sqr(val);
  UpdateCachedBSphere;
end;

function TgxVerletFrictionCapsule.GetBSphere: TBSphere;
begin
  result.Center := FLocation;
  result.Radius := Length + Radius;
end;

procedure TgxVerletFrictionCapsule.SatisfyConstraintForNode(const aNode: TgxBaseVerletNode;
  const iteration, maxIterations: Integer);
var
  p, n2, penetrationDepth: Single;
  Closest, V: TAffineVector;
  newLocation, move: TAffineVector;

begin
  // Find the closest point to location on the capsule axis
  p := ClampValue(PointProject(aNode.Location, FLocation, FAxis), -FLengthDiv2,
    FLengthDiv2);
  Closest := VectorCombine(FLocation, FAxis, 1, p);
  // vector from closest to location
  VectorSubtract(aNode.Location, Closest, V);
  // should it be altered?
  n2 := VectorNorm(V);
  if n2 < FRadius2 then
  begin
    newLocation := VectorCombine(Closest, V, 1, Sqrt(FRadius2 / n2));
    // Do friction calculations
    move := VectorSubtract(newLocation, aNode.FLocation);
    penetrationDepth := VectorLength(move);
    // aNode.OldApplyFriction(FFrictionRatio, penetrationDepth);
    aNode.ApplyFriction(FFrictionRatio, penetrationDepth,
      VectorScale(move, 1 / penetrationDepth));
    aNode.FLocation := newLocation;
    aNode.FChangedOnStep := Owner.CurrentStepCount;
    AddKickbackForceAt(FLocation, VectorScale(move,
      -aNode.FWeight * Owner.FInvCurrentDeltaTime));
  end;
end;

procedure TgxVerletFrictionCapsule.SatisfyConstraintForEdge(const aEdge: TgxVerletEdge;
  const iteration, maxIterations: Integer);
var
  SphereLocation, closestPoint, Dummy, delta, move, contactNormal
    : TAffineVector;
  Ax0, Ax1: TAffineVector;
  deltaLength, diff, penetrationDepth: Single;
begin
  VectorScale(FAxis, FLengthDiv2, Ax0);
  AddVector(Ax0, FLocation);
  VectorScale(FAxis, -FLengthDiv2, Ax1);
  AddVector(Ax1, FLocation);
  SegmentSegmentClosestPoint(aEdge.NodeA.FLocation, aEdge.NodeB.FLocation, Ax0,
    Ax1, Dummy, SphereLocation);
  // If the edge penetrates the sphere, try pushing the nodes until it no
  // longer does
  closestPoint := PointSegmentClosestPoint(SphereLocation,
    aEdge.NodeA.FLocation, aEdge.NodeB.FLocation);
  // Find the distance between the two
  VectorSubtract(closestPoint, SphereLocation, delta);
  deltaLength := VectorLength(delta);
  if deltaLength < Radius then
  begin
    // Move it outside the sphere!
    diff := (Radius - deltaLength) / deltaLength;
    VectorScale(delta, diff, move);
    penetrationDepth := VectorLength(move);
    contactNormal := VectorScale(move, 1 / penetrationDepth);
    aEdge.NodeA.ApplyFriction(FFrictionRatio, penetrationDepth, contactNormal);
    aEdge.NodeB.ApplyFriction(FFrictionRatio, penetrationDepth, contactNormal);
    AddVector(aEdge.NodeA.FLocation, move);
    AddVector(aEdge.NodeB.FLocation, move);
    aEdge.NodeA.FChangedOnStep := Owner.CurrentStepCount;
    aEdge.NodeB.FChangedOnStep := Owner.CurrentStepCount;
    AddKickbackForceAt(FLocation, VectorScale(move,
      -(aEdge.NodeA.FWeight + aEdge.NodeB.FWeight) *
      Owner.FInvCurrentDeltaTime));
  end;
end;

// ------------------
// ------------------ TgxVerletEdge ------------------
// ------------------
constructor TgxVerletEdge.CreateEdgeOwned(const aNodeA, aNodeB: TgxBaseVerletNode);
begin
  FNodeA := aNodeA;
  FNodeB := aNodeB;
  inherited CreateOwned(aNodeA.Owner.SpacePartition);
end;

procedure TgxVerletEdge.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB.min := FNodeA.FLocation;
  FCachedAABB.max := FNodeA.FLocation;
  AABBInclude(FCachedAABB, FNodeB.FLocation);
  AABBToBSphere(FCachedAABB, FCachedBSphere);
end;

// ------------------
// ------------------ TgxVerletEdgeList ------------------
// ------------------
function TgxVerletEdgeList.GetItems(i: Integer): TgxVerletEdge;
begin
  result := Get(i);
end;

procedure TgxVerletEdgeList.SetItems(i: Integer; const Value: TgxVerletEdge);
begin
  Put(i, Value);
end;

procedure TgxBaseVerletNode.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB.min := FLocation;
  FCachedAABB.max := FLocation;
  FCachedBSphere.Center := FLocation;
  FCachedBSphere.Radius := 0;
end;

procedure TgxBaseVerletNode.SetLocation(const Value: TAffineVector);
begin
  FLocation := Value;
  FChangedOnStep := Owner.CurrentStepCount;
end;

procedure TgxVerletWorld.CreateOctree(const OctreeMin, OctreeMax: TAffineVector;
  const LeafThreshold, MaxTreeDepth: Integer);
var
  Octree: TOctreeSpacePartition;
begin
  Assert(FNodes.Count = 0,
    'You can only create an octree while the world is empty!');
  FreeAndNil(FSpacePartition);
  Octree := TOctreeSpacePartition.Create;
  Octree.SetSize(OctreeMin, OctreeMax);
  Octree.MaxTreeDepth := MaxTreeDepth;
  Octree.LeafThreshold := LeafThreshold;
  Octree.CullingMode := cmGrossCulling;
  FSpacePartition := Octree;
  if FUpdateSpacePartion = uspNever then
    FUpdateSpacePartion := uspEveryFrame;
end;

procedure TgxVerletWorld.PauseInertia(const IterationSteps: Integer);
begin
  FInertaPauseSteps := IterationSteps + 1;
  Inertia := False;
end;

// ------------------
// TgxVerletGlobalFrictionConstraintBox
// ------------------
procedure TgxVerletGlobalFrictionConstraintBox.PerformSpaceQuery;
begin
  Owner.SpacePartition.QueryAABB(FCachedAABB);
end;

procedure TgxVerletGlobalFrictionConstraintBox.SetLocation
  (const Value: TAffineVector);
begin
  inherited;

  UpdateCachedAABB;
end;

procedure TgxVerletGlobalFrictionConstraintBox.UpdateCachedAABB;
begin
  FCachedAABB := GetAABB;
end;

// -------------------------------------------
// TgxVerletGlobalFrictionConstraintSphere
// -------------------------------------------
procedure TgxVerletGlobalFrictionConstraintSphere.PerformSpaceQuery;
begin
  Owner.SpacePartition.QueryBSphere(FCachedBSphere);
end;

procedure TgxVerletGlobalFrictionConstraintSphere.SetLocation
  (const Value: TAffineVector);
begin
  inherited;
  UpdateCachedBSphere;
end;

procedure TgxVerletGlobalFrictionConstraintSphere.UpdateCachedBSphere;
begin
  FCachedBSphere := GetBSphere;
end;

constructor TgxVerletGlobalConstraint.Create(const aOwner: TgxVerletWorld);
begin
  inherited;
  if Assigned(aOwner) then
    aOwner.ConstraintsWithBeforeIterations.Add(Self);
end;

destructor TgxVerletGlobalConstraint.Destroy;
begin
  if Assigned(Owner) then
    Owner.ConstraintsWithBeforeIterations.Remove(Self);
  inherited;
end;

//-----------------------------
// TgxVerletHair
//-----------------------------
procedure TgxVerletHair.AddStickStiffness(const ANodeSkip: integer);
var
  i: integer;
begin
  for i := 0 to NodeList.Count - (1 + ANodeSkip * 2) do
    FStiffnessList.Add(VerletWorld.CreateStick(NodeList[i],
      NodeList[i + 2 * ANodeSkip]));
end;

procedure TgxVerletHair.BuildHair(const AAnchorPosition, AHairDirection
  : TAffineVector);
var
  i: integer;
  Position: TAffineVector;
  Node, PrevNode: TgxBaseVerletNode;
  Direction: TAffineVector;
begin
  Clear;
  Direction := VectorNormalize(AHairDirection);
  // Fix the root of the hair
  Position := VectorAdd(AAnchorPosition, VectorScale(Direction, -FRootDepth));
  Node := VerletWorld.CreateOwnedNode(Position);
  NodeList.Add(Node);
  Node.NailedDown := true;
  PrevNode := Node;
  // Now add the links in the hair
  for i := 0 to FLinkCount - 1 do
  begin
    Position := VectorAdd(AAnchorPosition, VectorScale(Direction,
      HairLength * (i / LinkCount)));
    Node := VerletWorld.CreateOwnedNode(Position);
    NodeList.Add(Node);
    // first one is the anchor
    if i = 0 then
      Node.NailedDown := true
    else
      // Creates the hair link
      VerletWorld.CreateStick(PrevNode, Node);
    PrevNode := Node;
  end;
  // Now we must stiffen the hair with either sticks or springs
  BuildStiffness;
end;

procedure TgxVerletHair.BuildStiffness;
var
  i: integer;
begin
  ClearStiffness;
  if vhsFull in FStiffness then
  begin
    for i := 1 to 100 do
      AddStickStiffness(i);
    exit;
  end;
  if vhsSkip1Node in FStiffness then
    AddStickStiffness(1);
  if vhsSkip2Node in FStiffness then
    AddStickStiffness(2);
  if vhsSkip3Node in FStiffness then
    AddStickStiffness(3);
  if vhsSkip4Node in FStiffness then
    AddStickStiffness(4);
  if vhsSkip5Node in FStiffness then
    AddStickStiffness(5);
  if vhsSkip6Node in FStiffness then
    AddStickStiffness(6);
  if vhsSkip7Node in FStiffness then
    AddStickStiffness(7);
  if vhsSkip8Node in FStiffness then
    AddStickStiffness(8);
  if vhsSkip9Node in FStiffness then
    AddStickStiffness(9);
end;

procedure TgxVerletHair.Clear;
var
  i: integer;
begin
  ClearStiffness;
  for i := FNodeList.Count - 1 downto 0 do
    FNodeList[i].Free;
  FNodeList.Clear;
  FStiffnessList.Clear;
end;

procedure TgxVerletHair.ClearStiffness;
var
  i: integer;
begin
  for i := 0 to FStiffnessList.Count - 1 do
    TgxVerletConstraint(FStiffnessList[i]).Free;
  FStiffnessList.Clear;
end;

constructor TgxVerletHair.Create(const AVerletWorld: TgxVerletWorld;
  const ARootDepth, AHairLength: single; ALinkCount: integer;
  const AAnchorPosition, AHairDirection: TAffineVector;
  const AStiffness: TgxStiffnessSetVH);
begin
  FVerletWorld := AVerletWorld;
  FRootDepth := ARootDepth;
  FLinkCount := ALinkCount;
  FHairLength := AHairLength;

  FNodeList := TgxVerletNodeList.Create;
  FStiffness := AStiffness;
  FStiffnessList := TList.Create;

  BuildHair(AAnchorPosition, AHairDirection);
end;

destructor TgxVerletHair.Destroy;
begin
  Clear;
  FreeAndNil(FNodeList);
  FreeAndNil(FStiffnessList);
  inherited;
end;

function TgxVerletHair.GetAnchor: TgxBaseVerletNode;
begin
  result := NodeList[1];
end;

function TgxVerletHair.GetLinkLength: single;
begin
  if LinkCount > 0 then
    result := HairLength / LinkCount
  else
    result := 0;
end;

function TgxVerletHair.GetRoot: TgxBaseVerletNode;
begin
  result := NodeList[0];
end;

procedure TgxVerletHair.SetStiffness(const Value: TgxStiffnessSetVH);
begin
  FStiffness := Value;
  BuildStiffness;
end;

// ------------------
// ------------------ Global methods ------------------
// ------------------

procedure AddVerletConstriantsToVerletWorld
  (Colliders: TgxSkeletonColliderList; World: TgxVerletWorld);
var
  i: Integer;
begin
  for i := 0 to Colliders.Count - 1 do
    if Colliders[i] is TgxVerletSkeletonCollider then
      TgxVerletSkeletonCollider(Colliders[i]).AddToVerletWorld(World);
end;

// ------------------
// ------------------ TgxVerletSkeletonCollider ------------------
// ------------------
procedure TgxVerletSkeletonCollider.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(Writer);
  Writer.WriteInteger(0); // Archive Version 0
end;

procedure TgxVerletSkeletonCollider.ReadFromFiler(Reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(Reader);
  archiveVersion := Reader.ReadInteger;
  if archiveVersion = 0 then
    with Reader do
      // Nothing yet
    else
      RaiseFilerException(archiveVersion);
end;

procedure TgxVerletSkeletonCollider.AddToVerletWorld
  (VerletWorld: TgxVerletWorld);
begin
  AlignCollider;
end;

// ------------------
// ------------------ TgxVerletSphere ------------------
// ------------------
constructor TgxVerletSphere.Create;
begin
  inherited;
  Radius := 0.5;
  AlignCollider;
end;

procedure TgxVerletSphere.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(Writer);
  Writer.WriteInteger(0); // Archive Version 0
  Writer.WriteFloat(FRadius);
end;

procedure TgxVerletSphere.ReadFromFiler(Reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(Reader);
  archiveVersion := Reader.ReadInteger;
  if archiveVersion = 0 then
    with Reader do
      Radius := ReadFloat
  else
    RaiseFilerException(archiveVersion);
end;

procedure TgxVerletSphere.AddToVerletWorld(VerletWorld: TgxVerletWorld);
begin
  FVerletConstraint := TgxVerletFrictionSphere.Create(VerletWorld);
  TgxVerletFrictionSphere(FVerletConstraint).Radius := FRadius;
  inherited;
end;

procedure TgxVerletSphere.AlignCollider;
begin
  inherited;
  if Assigned(FVerletConstraint) then
    TgxVerletFrictionSphere(FVerletConstraint).Location :=
      AffineVectorMake(GlobalMatrix.W);
end;

procedure TgxVerletSphere.SetRadius(const Val: Single);
begin
  if Val <> FRadius then
  begin
    FRadius := Val;
    if Assigned(FVerletConstraint) then
      TgxVerletFrictionSphere(FVerletConstraint).Radius := FRadius;
  end;
end;

// ------------------
// ------------------ TgxVerletCapsule ------------------
// ------------------
constructor TgxVerletCapsule.Create;
begin
  inherited;
  Radius := 0.5;
  Length := 1;
  AlignCollider;
end;

procedure TgxVerletCapsule.WriteToFiler(Writer: TgxVirtualWriter);
begin
  inherited WriteToFiler(Writer);
  Writer.WriteInteger(0); // Archive Version 0
  Writer.WriteFloat(FRadius);
  Writer.WriteFloat(FLength);
end;

procedure TgxVerletCapsule.ReadFromFiler(Reader: TgxVirtualReader);
var
  archiveVersion: Integer;
begin
  inherited ReadFromFiler(Reader);
  archiveVersion := Reader.ReadInteger;
  if archiveVersion = 0 then
    with Reader do
    begin
      Radius := ReadFloat;
      Length := ReadFloat;
    end
  else
    RaiseFilerException(archiveVersion);
end;

procedure TgxVerletCapsule.AddToVerletWorld(VerletWorld: TgxVerletWorld);
begin
  FVerletConstraint := TgxVerletFrictionCapsule.Create(VerletWorld);
  TgxVerletFrictionCapsule(FVerletConstraint).Radius := FRadius;
  TgxVerletFrictionCapsule(FVerletConstraint).Length := FLength;
  inherited;
end;

procedure TgxVerletCapsule.AlignCollider;
begin
  inherited;
  if Assigned(FVerletConstraint) then
  begin
    TgxVerletFrictionCapsule(FVerletConstraint).Location :=
      AffineVectorMake(GlobalMatrix.W);
    TgxVerletFrictionCapsule(FVerletConstraint).Axis :=
      AffineVectorMake(GlobalMatrix.Y);
  end;
end;

procedure TgxVerletCapsule.SetRadius(const Val: Single);
begin
  if Val <> FRadius then
  begin
    FRadius := Val;
    if Assigned(FVerletConstraint) then
      TgxVerletFrictionCapsule(FVerletConstraint).Radius := FRadius;
  end;
end;

procedure TgxVerletCapsule.SetLength(const Val: Single);
begin
  if Val <> FLength then
  begin
    FLength := Val;
    if Assigned(FVerletConstraint) then
      TgxVerletFrictionCapsule(FVerletConstraint).Length := FLength;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterClasses([TgxVerletSkeletonCollider, TgxVerletSphere,
  TgxVerletCapsule]);


end.
