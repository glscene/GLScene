//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.VerletTypes;

(*
  Base Verlet modelling/simulation classes.
  This unit is generic, GLScene-specific sub-classes are in GLVerletClasses.

  Note that currently, the SatisfyConstraintForEdge methods push the nodes in
  the edge uniformly - it should push the closer node more for correct physics.
  It's a matter of leverage.
*)
interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Types,

  GLS.VectorTypes,
  GLS.PersistentClasses,

  GLS.BaseClasses,
  GLS.Objects,
  GLS.Scene,
  GLS.Coordinates,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.SpacePartition,
  GLS.GeometryBB,
  GLS.VectorFileObjects;

const
  G_DRAG = 0.0001;
  cDEFAULT_CONSTRAINT_FRICTION = 0.6;


type
  TGLStiffnessVH = (vhsFull, vhsSkip1Node, vhsSkip2Node, vhsSkip3Node,
    vhsSkip4Node, vhsSkip5Node, vhsSkip6Node, vhsSkip7Node, vhsSkip8Node,
    vhsSkip9Node);
  TGLStiffnessSetVH = set of TGLStiffnessVH;

type
  TGLVerletEdgeList = class;
  TGLVerletWorld = class;

  // Basic Verlet Node
  TGLBaseVerletNode = class(TGLSpacePartitionLeaf)
  private
    FForce: TAffineVector;
    FOwner: TGLVerletWorld;
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
    constructor CreateOwned(const aOwner: TGLVerletWorld); virtual;
    destructor Destroy; override;
    // Applies friction
    procedure ApplyFriction(const friction, penetrationDepth: Single;
      const surfaceNormal: TAffineVector);
    // Simple and less accurate method for friction
    procedure OldApplyFriction(const friction, penetrationDepth: Single);
    // Perform Verlet integration
    procedure Verlet(const vpt: TGLProgressTimes); virtual;
    (* Initlializes the node. For the base class, it just makes sure that
      FOldPosition = FPosition, so that speed is zero *)
    procedure Initialize; dynamic;
    // Calculates the distance to another node
    function DistanceToNode(const node: TGLBaseVerletNode): Single;
    // Calculates the movement of the node
    function GetMovement: TAffineVector;
    (* The TGLBaseVerletNode inherits from TSpacePartitionLeaf, and it needs to
      know how to publish itself. The owner (a TGLVerletWorld) has a spatial
      partitioning object *)
    procedure UpdateCachedAABBAndBSphere; override;
    // The VerletWorld that owns this verlet
    property Owner: TGLVerletWorld read FOwner;
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

  TGLVerletNodeClass = class of TGLBaseVerletNode;

  TGLVerletNodeList = class(TList)
  private
    function GetItems(i: Integer): TGLBaseVerletNode;
    procedure SetItems(i: Integer; const Value: TGLBaseVerletNode);
  public
    property Items[i: Integer]: TGLBaseVerletNode read GetItems
      write SetItems; default;
  end;

  TGLVerletConstraint = class(TObject)
  private
    FOwner: TGLVerletWorld;
    FEnabled: Boolean;
    FTag: Integer;
  public
    constructor Create(const aOwner: TGLVerletWorld); virtual;
    destructor Destroy; override;
    (* Updates the position of one or several nodes to make sure that they
      don't violate the constraint *)
    procedure SatisfyConstraint(const iteration, maxIterations: Integer);
      virtual; abstract;
    // Notifies removal of a node
    procedure RemoveNode(const aNode: TGLBaseVerletNode); virtual; abstract;
    // Method that's fired before the physics iterations are performed
    procedure BeforeIterations; virtual;
    // Onwer of the constraint
    property Owner: TGLVerletWorld read FOwner;
    // Determines if the constraint should be enforced or not
    property Enabled: Boolean read FEnabled write FEnabled;
    // Tag field reserved for the user.
    property Tag: Integer read FTag write FTag;
  end;

  TGLVerletDualConstraint = class(TGLVerletConstraint)
  private
    FNodeA, FNodeB: TGLBaseVerletNode;
  public
    procedure RemoveNode(const aNode: TGLBaseVerletNode); override;
    // Reference to NodeA.
    property NodeA: TGLBaseVerletNode read FNodeA write FNodeA;
    // Reference to NodeB.
    property NodeB: TGLBaseVerletNode read FNodeB write FNodeB;
  end;

  TGLVerletGroupConstraint = class(TGLVerletConstraint)
  private
    FNodes: TGLVerletNodeList;
  public
    constructor Create(const aOwner: TGLVerletWorld); override;
    destructor Destroy; override;
    procedure RemoveNode(const aNode: TGLBaseVerletNode); override;
    // The list of nodes that this constraint will effect
    property Nodes: TGLVerletNodeList read FNodes;
  end;

  // Verlet edges simulate rigid collission edges
  TGLVerletEdge = class(TGLSpacePartitionLeaf)
  private
    FNodeA: TGLBaseVerletNode;
    FNodeB: TGLBaseVerletNode;
  public
    (* The TGLVerletEdge inherits from TSpacePartitionLeaf, and it needs to
      know how to publish itself. The owner ( a TGLVerletWorld ) has a spatial
      partitioning object *)
    procedure UpdateCachedAABBAndBSphere; override;
    constructor CreateEdgeOwned(const aNodeA, aNodeB: TGLBaseVerletNode);
    // One of the nodes in the edge
    property NodeA: TGLBaseVerletNode read FNodeA write FNodeA;
    // One of the nodes in the edge
    property NodeB: TGLBaseVerletNode read FNodeB write FNodeB;
  end;

  TGLVerletEdgeList = class(TList)
  private
    function GetItems(i: Integer): TGLVerletEdge;
    procedure SetItems(i: Integer; const Value: TGLVerletEdge);
  public
    property Items[i: Integer]: TGLVerletEdge read GetItems
      write SetItems; default;
  end;

  TGLVerletGlobalConstraint = class(TGLVerletConstraint)
  private
    FKickbackForce: TAffineVector;
    FKickbackTorque: TAffineVector;
    FLocation: TAffineVector;
    procedure SetLocation(const Value: TAffineVector); virtual;
  public
    constructor Create(const aOwner: TGLVerletWorld); override;
    destructor Destroy; override;
    procedure RemoveNode(const aNode: TGLBaseVerletNode); override;
    procedure BeforeIterations; override;
    procedure SatisfyConstraint(const iteration, maxIterations
      : Integer); override;
    procedure SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
      const iteration, maxIterations: Integer); virtual; abstract;
    procedure SatisfyConstraintForEdge(const aEdge: TGLVerletEdge;
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

  TGLVerletGlobalFrictionConstraint = class(TGLVerletGlobalConstraint)
  private
    FFrictionRatio: Single;
  public
    constructor Create(const aOwner: TGLVerletWorld); override;
    property FrictionRatio: Single read FFrictionRatio write FFrictionRatio;
  end;

  TGLVerletGlobalFrictionConstraintSP = class(TGLVerletGlobalFrictionConstraint)
  public
    procedure SatisfyConstraint(const iteration, maxIterations
      : Integer); override;
    procedure PerformSpaceQuery; virtual; abstract;
  end;

  TGLVerletGlobalFrictionConstraintSphere = class
    (TGLVerletGlobalFrictionConstraintSP)
  private
    FCachedBSphere: TBSphere;
    procedure SetLocation(const Value: TAffineVector); override;
  public
    procedure UpdateCachedBSphere;
    procedure PerformSpaceQuery; override;
    function GetBSphere: TBSphere; virtual; abstract;
    property CachedBSphere: TBSphere read FCachedBSphere;
  end;

  TGLVerletGlobalFrictionConstraintBox = class
    (TGLVerletGlobalFrictionConstraintSP)
  private
    FCachedAABB: TAABB;
    procedure SetLocation(const Value: TAffineVector); override;
  public
    procedure UpdateCachedAABB;
    procedure PerformSpaceQuery; override;
    function GetAABB: TAABB; virtual; abstract;
    property CachedAABB: TAABB read FCachedAABB;
  end;

  TGLVerletConstraintList = class(TList)
  private
    function GetItems(i: Integer): TGLVerletConstraint;
    procedure SetItems(i: Integer; const Value: TGLVerletConstraint);
  public
    property Items[i: Integer]: TGLVerletConstraint read GetItems
      write SetItems; default;
  end;

  // Generic verlet force.
  TGLVerletForce = class(TObject)
  private
    FOwner: TGLVerletWorld;
  public
    constructor Create(const aOwner: TGLVerletWorld); virtual;
    destructor Destroy; override;
    // Implementation should add force to force resultant for all relevant nodes
    procedure AddForce(const vpt: TGLProgressTimes); virtual; abstract;
    // Notifies removal of a node
    procedure RemoveNode(const aNode: TGLBaseVerletNode); virtual; abstract;
    property Owner: TGLVerletWorld read FOwner;
  end;

  // A verlet force that applies to two specified nodes.
  TGLVerletDualForce = class(TGLVerletForce)
  private
    FNodeA, FNodeB: TGLBaseVerletNode;
  public
    procedure RemoveNode(const aNode: TGLBaseVerletNode); override;
    // Reference to NodeA.
    property NodeA: TGLBaseVerletNode read FNodeA write FNodeA;
    // Reference to NodeB.
    property NodeB: TGLBaseVerletNode read FNodeB write FNodeB;
  end;

  // A verlet force that applies to a specified group of nodes.
  TGLVerletGroupForce = class(TGLVerletForce)
  private
    FNodes: TGLVerletNodeList;
  public
    constructor Create(const aOwner: TGLVerletWorld); override;
    destructor Destroy; override;
    procedure RemoveNode(const aNode: TGLBaseVerletNode); override;
    // Nodes of the force group, referred, NOT owned.
    property Nodes: TGLVerletNodeList read FNodes;
  end;

  // A global force (applied to all verlet nodes).
  TGLVerletGlobalForce = class(TGLVerletForce)
  public
    procedure RemoveNode(const aNode: TGLBaseVerletNode); override;
    procedure AddForce(const vpt: TGLProgressTimes); override;
    procedure AddForceToNode(const aNode: TGLBaseVerletNode); virtual; abstract;
  end;

  TGLVerletForceList = class(TList)
  private
    function GetItems(i: Integer): TGLVerletForce;
    procedure SetItems(i: Integer; const Value: TGLVerletForce);
  public
    property Items[i: Integer]: TGLVerletForce read GetItems
      write SetItems; default;
  end;

  TGLVerletStick = class;
  TGLVerletSpring = class;
  TGLVerletSlider = class;

  TUpdateSpacePartion = (uspEveryIteration, uspEveryFrame, uspNever);
  TCollisionConstraintTypes = (cctEdge, cctNode);
  TCollisionConstraintTypesSet = set of TCollisionConstraintTypes;

  TGLVerletWorld = class(TObject)
  private
    FIterations: Integer;
    FNodes: TGLVerletNodeList;
    FConstraints: TGLVerletConstraintList;
    FForces: TGLVerletForceList;
    FMaxDeltaTime, FSimTime: Single;
    FDrag: Single;
    FCurrentDeltaTime: Single;
    FInvCurrentDeltaTime: Single;
    FSolidEdges: TGLVerletEdgeList;
    FSpacePartition: TGLBaseSpacePartition;
    FCurrentStepCount: Integer;
    FUpdateSpacePartion: TUpdateSpacePartion;
    FCollisionConstraintTypes: TCollisionConstraintTypesSet;
    FConstraintsWithBeforeIterations: TGLVerletConstraintList;
    FVerletNodeClass: TGLVerletNodeClass;
    FInertia: Boolean;
    FInertaPauseSteps: Integer;
  protected
    procedure AccumulateForces(const vpt: TGLProgressTimes); virtual;
    procedure Verlet(const vpt: TGLProgressTimes); virtual;
    procedure SatisfyConstraints(const vpt: TGLProgressTimes); virtual;
    procedure DoUpdateSpacePartition;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddNode(const aNode: TGLBaseVerletNode): Integer;
    procedure RemoveNode(const aNode: TGLBaseVerletNode);
    function AddConstraint(const aConstraint: TGLVerletConstraint): Integer;
    procedure RemoveConstraint(const aConstraint: TGLVerletConstraint);
    function AddForce(const aForce: TGLVerletForce): Integer;
    procedure RemoveForce(const aForce: TGLVerletForce);
    procedure AddSolidEdge(const aNodeA, aNodeB: TGLBaseVerletNode);
    procedure PauseInertia(const IterationSteps: Integer);
    function CreateOwnedNode(const Location: TAffineVector;
      const aRadius: Single = 0; const aWeight: Single = 1): TGLBaseVerletNode;
    function CreateStick(const aNodeA, aNodeB: TGLBaseVerletNode;
      const Slack: Single = 0): TGLVerletStick;
    function CreateSpring(const aNodeA, aNodeB: TGLBaseVerletNode;
      const aStrength, aDamping: Single; const aSlack: Single = 0): TGLVerletSpring;
    function CreateSlider(const aNodeA, aNodeB: TGLBaseVerletNode;
      const aSlideDirection: TAffineVector): TGLVerletSlider;
    procedure Initialize; virtual;
    procedure CreateOctree(const OctreeMin, OctreeMax: TAffineVector;
      const LeafThreshold, MaxTreeDepth: Integer);
    function Progress(const deltaTime, newTime: Double): Integer; virtual;
    function FirstNode: TGLBaseVerletNode;
    function LastNode: TGLBaseVerletNode;
    property Drag: Single read FDrag write FDrag;
    property Iterations: Integer read FIterations write FIterations;
    property Nodes: TGLVerletNodeList read FNodes;
    property Constraints: TGLVerletConstraintList read FConstraints;
    property ConstraintsWithBeforeIterations: TGLVerletConstraintList
      read FConstraintsWithBeforeIterations;
    property SimTime: Single read FSimTime write FSimTime;
    property MaxDeltaTime: Single read FMaxDeltaTime write FMaxDeltaTime;
    property CurrentDeltaTime: Single read FCurrentDeltaTime;
    property SolidEdges: TGLVerletEdgeList read FSolidEdges write FSolidEdges;
    property CurrentStepCount: Integer read FCurrentStepCount;
    property SpacePartition: TGLBaseSpacePartition read FSpacePartition;
    property UpdateSpacePartion: TUpdateSpacePartion read FUpdateSpacePartion
      write FUpdateSpacePartion;
    property CollisionConstraintTypes: TCollisionConstraintTypesSet
      read FCollisionConstraintTypes write FCollisionConstraintTypes;
    property VerletNodeClass: TGLVerletNodeClass read FVerletNodeClass
      write FVerletNodeClass;
    property Inertia: Boolean read FInertia write FInertia;
  end;

  TGLVerletGravity = class(TGLVerletGlobalForce)
  private
    FGravity: TAffineVector;
  public
    constructor Create(const aOwner: TGLVerletWorld); override;
    procedure AddForceToNode(const aNode: TGLBaseVerletNode); override;
    property Gravity: TAffineVector read FGravity write FGravity;
  end;

  TGLVerletAirResistance = class(TGLVerletGlobalForce)
  private
    FDragCoeff: Single;
    FWindDirection: TAffineVector;
    FWindMagnitude: Single;
    FWindChaos: Single;
    procedure SetWindDirection(const Value: TAffineVector);
  public
    constructor Create(const aOwner: TGLVerletWorld); override;
    procedure AddForceToNode(const aNode: TGLBaseVerletNode); override;
    property DragCoeff: Single read FDragCoeff write FDragCoeff;
    property WindDirection: TAffineVector read FWindDirection
      write SetWindDirection;
    property WindMagnitude: Single read FWindMagnitude write FWindMagnitude;
    // Measures how chaotic the wind is, as a fraction of the wind magnitude
    property WindChaos: Single read FWindChaos write FWindChaos;
  end;

  TGLVerletSpring = class(TGLVerletDualForce)
  private
    FRestLength: Single;
    FStrength: Single;
    FDamping: Single;
    FSlack: Single;
    FForceFactor: Single;
  protected
    procedure SetSlack(const Value: Single);
  public
    procedure AddForce(const vpt: TGLProgressTimes); override;
    // Must be invoked after adjust node locations or strength
    procedure SetRestLengthToCurrent;
    property Strength: Single read FStrength write FStrength;
    property Damping: Single read FDamping write FDamping;
    property Slack: Single read FSlack write SetSlack;
  end;

  // Floor Collision Constraint
  TGLVerletFloor = class(TGLVerletGlobalFrictionConstraintSP)
  private
    FBounceRatio, FFloorLevel: Single;
    FNormal: TAffineVector;
  protected
    procedure SetNormal(const Value: TAffineVector);
  public
    constructor Create(const aOwner: TGLVerletWorld); override;
    procedure PerformSpaceQuery; override;
    procedure SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    property BounceRatio: Single read FBounceRatio write FBounceRatio;
    property FloorLevel: Single read FFloorLevel write FFloorLevel;
    property Normal: TAffineVector read FNormal write SetNormal;
  end;

  TGLVerletHeightField = class;
  TGLVerletHeightFieldOnNeedHeight = function(hfConstraint: TGLVerletHeightField;
    node: TGLBaseVerletNode): Single of object;

  // HeightField collision constraint (punctual!)
  TGLVerletHeightField = class(TGLVerletFloor)
  private
    FOnNeedHeight: TGLVerletHeightFieldOnNeedHeight;
  public
    procedure SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    property OnNeedHeight: TGLVerletHeightFieldOnNeedHeight read FOnNeedHeight
      write FOnNeedHeight;
  end;

  // Stick Verlet Collision Constraint. Imposes a fixed distance between two nodes
  TGLVerletStick = class(TGLVerletDualConstraint)
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
  TGLVerletRigidBody = class(TGLVerletGroupConstraint)
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
  TGLVerletSlider = class(TGLVerletDualConstraint)
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
  TGLVerletFrictionSphere = class(TGLVerletGlobalFrictionConstraintSphere)
  private
    FRadius: Single;
  public
    function GetBSphere: TBSphere; override;
    procedure SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    procedure SatisfyConstraintForEdge(const aEdge: TGLVerletEdge;
      const iteration, maxIterations: Integer); override;
    property Radius: Single read FRadius write FRadius;
  end;

  (* Cylinder collision Friction Constraint.
    The cylinder is considered infinite by this constraint. *)
  TGLVerletFrictionCylinder = class(TGLVerletGlobalFrictionConstraint)
  private
    FAxis: TAffineVector;
    FRadius, FRadius2: Single;
  protected
    procedure SetRadius(const val: Single);
  public
    procedure SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
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
  TGLVerletFrictionCube = class(TGLVerletGlobalFrictionConstraintBox)
  private
    FHalfSides: TAffineVector;
    FSides: TAffineVector;
    FDirection: TAffineVector;
    procedure SetSides(const Value: TAffineVector);
  public
    function GetAABB: TAABB; override;
    procedure SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    // Broken and very slow!
    procedure SatisfyConstraintForEdge(const aEdge: TGLVerletEdge;
      const iteration, maxIterations: Integer); override;
    property Direction: TAffineVector read FDirection write FDirection;
    property Sides: TAffineVector read FSides write SetSides;
  end;

  // Capsule collision Friction Constraint.
  TGLVerletFrictionCapsule = class(TGLVerletGlobalFrictionConstraintSphere)
  private
    FAxis: TAffineVector;
    FRadius, FRadius2, FLength, FLengthDiv2: Single;
  protected
    procedure SetAxis(const val: TAffineVector);
    procedure SetRadius(const val: Single);
    procedure SetLength(const val: Single);
  public
    function GetBSphere: TBSphere; override;
    procedure SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
      const iteration, maxIterations: Integer); override;
    procedure SatisfyConstraintForEdge(const aEdge: TGLVerletEdge;
      const iteration, maxIterations: Integer); override;
    // property Base : TAffineVector read FBase write FBase;
    property Axis: TAffineVector read FAxis write SetAxis;
    property Radius: Single read FRadius write SetRadius;
    property Length: Single read FLength write SetLength;
  end;

  (* Specialized verlet node that can be anchored to a GLScene object. If it's
    anchored and has the property "NailedDown" set, it will remain in the same
    relative position to the GLScene object. *)
  TGLVerletNode = class(TGLBaseVerletNode)
  private
    FRelativePosition: TAffineVector;
    FGLBaseSceneObject: TGLBaseSceneObject;
    procedure SetGLBaseSceneObject(const Value: TGLBaseSceneObject);
  protected
    procedure SetLocation(const Value: TAffineVector); override;
  public
    procedure Verlet(const vpt: TGLProgressTimes); override;
    property GLBaseSceneObject: TGLBaseSceneObject read FGLBaseSceneObject
      write SetGLBaseSceneObject;
    property RelativePosition: TAffineVector read FRelativePosition
      write FRelativePosition;
  end;

  // Verlet Hair class
  TGLVerletHair = class
  private
    FNodeList: TGLVerletNodeList;
    FLinkCount: integer;
    FRootDepth: single;
    FVerletWorld: TGLVerletWorld;
    FHairLength: single;
    FData: pointer;
    FStiffness: TGLStiffnessSetVH;
    FStiffnessList: TList;
    function GetAnchor: TGLBaseVerletNode;
    function GetRoot: TGLBaseVerletNode;
    function GetLinkLength: single;
    procedure AddStickStiffness(const ANodeSkip: integer);
    procedure SetStiffness(const Value: TGLStiffnessSetVH);
  public
    procedure BuildHair(const AAnchorPosition, AHairDirection: TAffineVector);
    procedure BuildStiffness;
    procedure ClearStiffness;
    procedure Clear;
    constructor Create(const AVerletWorld: TGLVerletWorld;
      const ARootDepth, AHairLength: single; ALinkCount: integer;
      const AAnchorPosition, AHairDirection: TAffineVector;
      const AStiffness: TGLStiffnessSetVH);
    destructor Destroy; override;
    property NodeList: TGLVerletNodeList read FNodeList;
    property VerletWorld: TGLVerletWorld read FVerletWorld;
    property RootDepth: single read FRootDepth;
    property LinkLength: single read GetLinkLength;
    property LinkCount: integer read FLinkCount;
    property HairLength: single read FHairLength;
    property Stiffness: TGLStiffnessSetVH read FStiffness write SetStiffness;
    property Data: pointer read FData write FData;
    // Anchor should be nailed down to give the hair stability
    property Anchor: TGLBaseVerletNode read GetAnchor;
    // Root should be nailed down to give the hair stability
    property Root: TGLBaseVerletNode read GetRoot;
  end;

  // Base Verlet Skeleton Collider class.
  TGLVerletSkeletonCollider = class(TGLSkeletonCollider)
  private
    FVerletConstraint: TGLVerletConstraint;
  public
    procedure WriteToFiler(Writer: TGLVirtualWriter); override;
    procedure ReadFromFiler(Reader: TGLVirtualReader); override;
    procedure AddToVerletWorld(VerletWorld: TGLVerletWorld); virtual;
    // The verlet constraint is created through the AddToVerletWorld procedure
    property VerletConstraint: TGLVerletConstraint read FVerletConstraint;
  end;

  // Sphere shaped verlet constraint in a skeleton collider
  TGLVerletSphere = class(TGLVerletSkeletonCollider)
  private
    FRadius: Single;
  protected
    procedure SetRadius(const Val: Single);
  public
    constructor Create; override;
    procedure WriteToFiler(Writer: TGLVirtualWriter); override;
    procedure ReadFromFiler(Reader: TGLVirtualReader); override;
    procedure AddToVerletWorld(VerletWorld: TGLVerletWorld); override;
    procedure AlignCollider; override;
    property Radius: Single read FRadius write SetRadius;
  end;

  // Capsule shaped verlet constraint in a skeleton collider
  TGLVerletCapsule = class(TGLVerletSkeletonCollider)
  private
    FRadius, FLength: Single;
  protected
    procedure SetRadius(const Val: Single);
    procedure SetLength(const Val: Single);
  public
    constructor Create; override;
    procedure WriteToFiler(Writer: TGLVirtualWriter); override;
    procedure ReadFromFiler(Reader: TGLVirtualReader); override;
    procedure AddToVerletWorld(VerletWorld: TGLVerletWorld); override;
    procedure AlignCollider; override;
    property Radius: Single read FRadius write SetRadius;
    property Length: Single read FLength write SetLength;
  end;

(* After loading call this function to add all the constraints in a
   skeleton collider list to a given verlet world. *)
procedure AddVerletConstriantsToVerletWorld
  (Colliders: TGLSkeletonColliderList; World: TGLVerletWorld);

function CreateVerletPlaneFromGLPlane(Plane: TGLPlane; VerletWorld: TGLVerletWorld;
  Offset: Single): TGLVerletFloor;

// ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------

function CreateVerletPlaneFromGLPlane(Plane: TGLPlane; VerletWorld: TGLVerletWorld;
  Offset: Single): TGLVerletFloor;
begin
  result := TGLVerletFloor.Create(VerletWorld);
  with result do
  begin
    Normal := VectorNormalize(Plane.Direction.AsAffineVector);

    Location := VectorAdd(Plane.Position.AsAffineVector,
      VectorScale(Normal, Offset));
  end;
end;

// ----------------------------
// TGLVerletNode
// ----------------------------
procedure TGLVerletNode.SetGLBaseSceneObject(const Value: TGLBaseSceneObject);
begin
  FGLBaseSceneObject := Value;

  if Assigned(GLBaseSceneObject) and NailedDown then
    FRelativePosition := AffineVectorMake
      (GLBaseSceneObject.AbsoluteToLocal(VectorMake(FLocation, 1)));
end;

procedure TGLVerletNode.SetLocation(const Value: TAffineVector);
begin
  inherited;
  if Assigned(GLBaseSceneObject) and NailedDown then
    FRelativePosition := GLBaseSceneObject.AbsoluteToLocal(Value);
end;

procedure TGLVerletNode.Verlet(const vpt: TGLProgressTimes);
begin
  if Assigned(GLBaseSceneObject) and NailedDown then
  begin
    FLocation := GLBaseSceneObject.LocalToAbsolute(FRelativePosition);
  end
  else
    inherited;
end;

// ------------------
// ------------------ TGLBaseVerletNode ------------------
// ------------------

constructor TGLBaseVerletNode.CreateOwned(const aOwner: TGLVerletWorld);
begin
  inherited CreateOwned(aOwner.SpacePartition);
  if Assigned(aOwner) then
    aOwner.AddNode(Self);

  FWeight := 1;
  FInvWeight := 1;
  FRadius := 0;
  FFriction := 1;
end;

destructor TGLBaseVerletNode.Destroy;
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
procedure TGLBaseVerletNode.ApplyFriction(const friction, penetrationDepth: Single;
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

procedure TGLBaseVerletNode.OldApplyFriction(const friction, penetrationDepth
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

function TGLBaseVerletNode.DistanceToNode(const node: TGLBaseVerletNode): Single;
begin
  result := VectorDistance(Location, node.Location);
end;

function TGLBaseVerletNode.GetMovement: TAffineVector;
begin
  result := VectorSubtract(Location, OldLocation);
end;

procedure TGLBaseVerletNode.Initialize;
begin
  FOldLocation := Location;
end;

procedure TGLBaseVerletNode.SetWeight(const Value: Single);
begin
  FWeight := Value;
  if Value <> 0 then
    FInvWeight := 1 / Value
  else
    FInvWeight := 1;
end;

procedure TGLBaseVerletNode.Verlet(const vpt: TGLProgressTimes);
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

procedure TGLBaseVerletNode.AfterProgress;
begin
  // nothing here, reserved for subclass use
end;

// ------------------
// ------------------ TGLVerletNodeList ------------------
// ------------------
function TGLVerletNodeList.GetItems(i: Integer): TGLBaseVerletNode;
begin
  result := Get(i);
end;

procedure TGLVerletNodeList.SetItems(i: Integer; const Value: TGLBaseVerletNode);
begin
  Put(i, Value);
end;

function TGLBaseVerletNode.GetSpeed: TAffineVector;
begin
  result := VectorScale(VectorSubtract(FLocation, FOldLocation),
    1 / Owner.CurrentDeltaTime);
end;

// ------------------
// ------------------ TGLVerletConstraint ------------------
// ------------------
constructor TGLVerletConstraint.Create(const aOwner: TGLVerletWorld);
begin
  inherited Create;
  if Assigned(aOwner) then
    aOwner.AddConstraint(Self);
  FEnabled := True;
end;

destructor TGLVerletConstraint.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.RemoveConstraint(Self);
  inherited;
end;

procedure TGLVerletConstraint.BeforeIterations;
begin
  // NADA!
end;

// ------------------
// ------------------ TGLVerletDualConstraint ------------------
// ------------------
procedure TGLVerletDualConstraint.RemoveNode(const aNode: TGLBaseVerletNode);
begin
  if FNodeA = aNode then
    FNodeA := nil;
  if FNodeB = aNode then
    FNodeB := nil;
  if (FNodeA = nil) and (FNodeA = nil) then
    Free;
end;

// ------------------
// ------------------ TGLVerletGroupConstraint ------------------
// ------------------
constructor TGLVerletGroupConstraint.Create(const aOwner: TGLVerletWorld);
begin
  inherited Create(aOwner);
  FNodes := TGLVerletNodeList.Create;
end;

destructor TGLVerletGroupConstraint.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TGLVerletGroupConstraint.RemoveNode(const aNode: TGLBaseVerletNode);
begin
  FNodes.Remove(aNode);
end;

// ------------------
// ------------------ TGLVerletGlobalConstraint ------------------
// ------------------
procedure TGLVerletGlobalConstraint.AddKickbackForceAt(const Pos: TAffineVector;
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

function TGLVerletGlobalConstraint.TranslateKickbackTorque(const TorqueCenter
  : TAffineVector): TAffineVector;
begin
  // EM(b) = EM(a) + EF x VectorSubtract(b, a).
  result := VectorAdd(FKickbackTorque,
    VectorCrossProduct(VectorSubtract(TorqueCenter, FLocation),
    FKickbackForce));
end;

procedure TGLVerletGlobalConstraint.BeforeIterations;
begin
  inherited;
  FKickbackForce := NullVector;
  FKickbackTorque := NullVector;
end;

procedure TGLVerletGlobalConstraint.RemoveNode(const aNode: TGLBaseVerletNode);
begin
  // nothing to do here
end;

procedure TGLVerletGlobalConstraint.SetLocation(const Value: TAffineVector);
begin
  FLocation := Value;
end;

procedure TGLVerletGlobalConstraint.SatisfyConstraint(const iteration,
  maxIterations: Integer);
var
  i: Integer;
  node: TGLBaseVerletNode;
begin
  if cctNode in Owner.CollisionConstraintTypes then
    for i := 0 to Owner.Nodes.Count - 1 do
    begin
      node := TGLBaseVerletNode(Owner.Nodes[i]);
      if not node.NailedDown then
        SatisfyConstraintForNode(node, iteration, maxIterations);
    end; // }

  if cctEdge in Owner.CollisionConstraintTypes then
    for i := 0 to Owner.SolidEdges.Count - 1 do
    begin
      SatisfyConstraintForEdge(Owner.SolidEdges[i], iteration, maxIterations);
    end; // }
end;

procedure TGLVerletGlobalConstraint.SatisfyConstraintForEdge
  (const aEdge: TGLVerletEdge; const iteration, maxIterations: Integer);
begin
  // Purely virtual, but can't be abstract...
end;

// ------------------
// ------------------ TGLVerletGlobalFrictionConstraint ------------------
// ------------------
constructor TGLVerletGlobalFrictionConstraint.Create(const aOwner
  : TGLVerletWorld);
begin
  inherited;
  FFrictionRatio := cDEFAULT_CONSTRAINT_FRICTION;
end;

// ------------------
// ------------------ TGLVerletGlobalFrictionConstraintSP ------------------
// ------------------
procedure TGLVerletGlobalFrictionConstraintSP.SatisfyConstraint(const iteration,
  maxIterations: Integer);
var
  i: Integer;
  node: TGLBaseVerletNode;
  edge: TGLVerletEdge;
  SP: TGLBaseSpacePartition;
  Leaf: TGLSpacePartitionLeaf;
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
    if Leaf is TGLBaseVerletNode then
    begin
      if cctNode in Owner.CollisionConstraintTypes then
      begin
        node := Leaf as TGLBaseVerletNode;
        if not node.NailedDown then
          SatisfyConstraintForNode(node, iteration, maxIterations);
      end;
    end
    else if Leaf is TGLVerletEdge then
    begin
      if cctEdge in Owner.CollisionConstraintTypes then
      begin
        edge := Leaf as TGLVerletEdge;
        SatisfyConstraintForEdge(edge, iteration, maxIterations);
      end;
    end
    else
      Assert(False, 'Bad objects in list!');
  end;
end;

// ------------------
// ------------------ TGLVerletConstraintList ------------------
// ------------------
function TGLVerletConstraintList.GetItems(i: Integer): TGLVerletConstraint;
begin
  result := Get(i);
end;

procedure TGLVerletConstraintList.SetItems(i: Integer;
  const Value: TGLVerletConstraint);
begin
  Put(i, Value);
end;

// ------------------
// ------------------ TGLVerletForce ------------------
// ------------------
constructor TGLVerletForce.Create(const aOwner: TGLVerletWorld);
begin
  inherited Create;
  if Assigned(aOwner) then
    aOwner.AddForce(Self);
end;

destructor TGLVerletForce.Destroy;
begin
  if Assigned(FOwner) then
    FOwner.RemoveForce(Self);
  inherited;
end;

// ------------------
// ------------------ TGLVerletGroupForce ------------------
// ------------------
constructor TGLVerletGroupForce.Create(const aOwner: TGLVerletWorld);
begin
  inherited Create(aOwner);
  FNodes := TGLVerletNodeList.Create;
end;

destructor TGLVerletGroupForce.Destroy;
begin
  FNodes.Free;
  inherited;
end;

procedure TGLVerletGroupForce.RemoveNode(const aNode: TGLBaseVerletNode);
begin
  FNodes.Remove(aNode);
end;

// ------------------
// ------------------ TGLVerletGlobalForce ------------------
// ------------------
procedure TGLVerletGlobalForce.RemoveNode(const aNode: TGLBaseVerletNode);
begin
  // nothing to do here
end;

procedure TGLVerletGlobalForce.AddForce;
var
  i: Integer;
  node: TGLBaseVerletNode;
begin
  for i := 0 to Owner.Nodes.Count - 1 do
  begin
    node := TGLBaseVerletNode(Owner.Nodes.List[i]);
    if not node.NailedDown then
      AddForceToNode(node);
  end;
end;

// ------------------
// ------------------ TGLVerletDualForce ------------------
// ------------------
procedure TGLVerletDualForce.RemoveNode(const aNode: TGLBaseVerletNode);
begin
  if FNodeA = aNode then
    FNodeA := nil;
  if FNodeB = aNode then
    FNodeB := nil;
end;

// ------------------
// ------------------ TGLVerletForceList ------------------
// ------------------
function TGLVerletForceList.GetItems(i: Integer): TGLVerletForce;
begin
  result := Get(i);
end;

procedure TGLVerletForceList.SetItems(i: Integer; const Value: TGLVerletForce);
begin
  Put(i, Value);
end;

// ------------------
// ------------------ TGLVerletWorld ------------------
// ------------------
constructor TGLVerletWorld.Create;
begin
  inherited;
  FDrag := G_DRAG;
  FNodes := TGLVerletNodeList.Create;
  FConstraints := TGLVerletConstraintList.Create;
  FConstraintsWithBeforeIterations := TGLVerletConstraintList.Create;
  FForces := TGLVerletForceList.Create;
  FMaxDeltaTime := 0.02;
  FIterations := 3;
  FSolidEdges := TGLVerletEdgeList.Create;
  FCurrentStepCount := 0;
  FUpdateSpacePartion := uspNever;
  FCollisionConstraintTypes := [cctNode, cctEdge];
  FSpacePartition := nil;
  FVerletNodeClass := TGLBaseVerletNode;
  FInertia := True;
end;

destructor TGLVerletWorld.Destroy;
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

procedure TGLVerletWorld.AccumulateForces(const vpt: TGLProgressTimes);
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

function TGLVerletWorld.AddNode(const aNode: TGLBaseVerletNode): Integer;
begin
  if Assigned(aNode.FOwner) then
    aNode.Owner.FNodes.Remove(aNode);
  result := FNodes.Add(aNode);
  aNode.FOwner := Self;
end;

procedure TGLVerletWorld.RemoveNode(const aNode: TGLBaseVerletNode);
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

function TGLVerletWorld.AddConstraint(const aConstraint
  : TGLVerletConstraint): Integer;
begin
  if Assigned(aConstraint.FOwner) then
    aConstraint.Owner.FConstraints.Remove(aConstraint);
  result := FConstraints.Add(aConstraint);
  aConstraint.FOwner := Self;
end;

procedure TGLVerletWorld.RemoveConstraint(const aConstraint
  : TGLVerletConstraint);
begin
  if aConstraint.Owner = Self then
  begin
    FConstraints.Remove(aConstraint);
    aConstraint.FOwner := nil;
  end;
end;

function TGLVerletWorld.AddForce(const aForce: TGLVerletForce): Integer;
begin
  if Assigned(aForce.FOwner) then
    aForce.Owner.FForces.Remove(aForce);
  result := FForces.Add(aForce);
  aForce.FOwner := Self;
end;

procedure TGLVerletWorld.RemoveForce(const aForce: TGLVerletForce);
begin
  if aForce.Owner = Self then
  begin
    FForces.Remove(aForce);
    aForce.FOwner := nil;
  end;
end;

procedure TGLVerletWorld.AddSolidEdge(const aNodeA, aNodeB: TGLBaseVerletNode);
var
  VerletEdge: TGLVerletEdge;
begin
  VerletEdge := TGLVerletEdge.CreateEdgeOwned(aNodeA, aNodeB);
  SolidEdges.Add(VerletEdge);
end;

function TGLVerletWorld.FirstNode: TGLBaseVerletNode;
begin
  Assert(FNodes.Count > 0, 'There are no nodes in the assembly!');
  result := FNodes[0];
end;

function TGLVerletWorld.LastNode: TGLBaseVerletNode;
begin
  Assert(FNodes.Count > 0, 'There are no nodes in the assembly!');
  result := FNodes[FNodes.Count - 1];
end;

function TGLVerletWorld.CreateOwnedNode(const Location: TAffineVector;
  const aRadius: Single = 0; const aWeight: Single = 1): TGLBaseVerletNode;
begin
  result := VerletNodeClass.CreateOwned(Self);
  result.Location := Location;
  result.OldLocation := Location;
  result.Weight := aWeight;
  result.Radius := aRadius;
end;

function TGLVerletWorld.CreateStick(const aNodeA, aNodeB: TGLBaseVerletNode;
  const Slack: Single = 0): TGLVerletStick;
begin
  Assert(aNodeA <> aNodeB, 'Can''t create stick between same node!');
  result := TGLVerletStick.Create(Self);
  result.NodeA := aNodeA;
  result.NodeB := aNodeB;
  result.SetRestLengthToCurrent;
  result.Slack := Slack;
end;

function TGLVerletWorld.CreateSpring(const aNodeA, aNodeB: TGLBaseVerletNode;
  const aStrength, aDamping: Single; const aSlack: Single = 0): TGLVerletSpring;
begin
  result := TGLVerletSpring.Create(Self);
  result.NodeA := aNodeA;
  result.NodeB := aNodeB;
  result.Strength := aStrength;
  result.Damping := aDamping;
  result.Slack := aSlack;
  result.SetRestLengthToCurrent;
end;

function TGLVerletWorld.CreateSlider(const aNodeA, aNodeB: TGLBaseVerletNode;
  const aSlideDirection: TAffineVector): TGLVerletSlider;
begin
  result := TGLVerletSlider.Create(Self);
  result.NodeA := aNodeA;
  result.NodeB := aNodeB;
  result.SlideDirection := aSlideDirection;
end;

procedure TGLVerletWorld.Initialize;
var
  i: Integer;
begin
  for i := 0 to FNodes.Count - 1 do
    FNodes[i].Initialize;
end;

function TGLVerletWorld.Progress(const deltaTime, newTime: Double): Integer;
var
  i: Integer;
  ticks: Integer;
  myDeltaTime: Single;
  vpt: TGLProgressTimes;
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

procedure TGLVerletWorld.DoUpdateSpacePartition;
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

procedure TGLVerletWorld.SatisfyConstraints(const vpt: TGLProgressTimes);
var
  i, j: Integer;
  Constraint: TGLVerletConstraint;
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

procedure TGLVerletWorld.Verlet(const vpt: TGLProgressTimes);
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
// ------------------ TGLVerletGravity ------------------
// ------------------
constructor TGLVerletGravity.Create(const aOwner: TGLVerletWorld);
begin
  inherited;
  FGravity.X := 0;
  FGravity.Y := -9.81;
  FGravity.Z := 0;
end;

procedure TGLVerletGravity.AddForceToNode(const aNode: TGLBaseVerletNode);
begin
  CombineVector(aNode.FForce, Gravity, @aNode.Weight);
end;

// ------------------
// TGLVerletSpring
// ------------------
procedure TGLVerletSpring.SetSlack(const Value: Single);
begin
  if Value <= 0 then
    FSlack := 0
  else
    FSlack := Value;
end;

procedure TGLVerletSpring.AddForce;
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

procedure TGLVerletSpring.SetRestLengthToCurrent;
begin
  FRestLength := VectorDistance(NodeA.Location, NodeB.Location);
  FForceFactor := FStrength / FRestLength;
end;

// ------------------
// ------------------ TGLVerletAirResistance ------------------
// ------------------
procedure TGLVerletAirResistance.AddForceToNode(const aNode: TGLBaseVerletNode);
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

constructor TGLVerletAirResistance.Create(const aOwner: TGLVerletWorld);
begin
  inherited;
  FDragCoeff := 0.001;
  FWindDirection.X := 0;
  FWindDirection.Y := 0;
  FWindDirection.Z := 0;
  FWindMagnitude := 0;
  FWindChaos := 0;
end;

procedure TGLVerletAirResistance.SetWindDirection(const Value: TAffineVector);
begin
  FWindDirection := VectorNormalize(Value);
end;

// ------------------
// ------------------ TGLVerletFloor ------------------
// ------------------
constructor TGLVerletFloor.Create(const aOwner: TGLVerletWorld);
begin
  inherited;
  MakeVector(FNormal, 0, 1, 0);
  MakeVector(FLocation, 0, 0, 0);
end;

procedure TGLVerletFloor.PerformSpaceQuery;
begin
  Owner.SpacePartition.QueryPlane(FLocation, FNormal);
end;

procedure TGLVerletFloor.SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
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

procedure TGLVerletFloor.SetNormal(const Value: TAffineVector);
begin
  FNormal := Value;
  NormalizeVector(FNormal);
end;

// ------------------
// TGLVerletHeightField
// ------------------
procedure TGLVerletHeightField.SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
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
// TGLVerletStick
// ------------------
procedure TGLVerletStick.SatisfyConstraint(const iteration, maxIterations: Integer);
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

procedure TGLVerletStick.SetRestLengthToCurrent;
begin
  FRestLength := VectorDistance(NodeA.Location, NodeB.Location);
end;

// ------------------
// TGLVerletRigidBody
// ------------------
procedure TGLVerletRigidBody.ComputeBarycenter(var barycenter: TAffineVector);
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

procedure TGLVerletRigidBody.ComputeNaturals(const barycenter: TAffineVector;
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

procedure TGLVerletRigidBody.ComputeRigidityParameters;
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

procedure TGLVerletRigidBody.SatisfyConstraint(const iteration,
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
// TGLVerletSlider
// ------------------
procedure TGLVerletSlider.SetSlideDirection(const Value: TAffineVector);
begin
  FSlideDirection := VectorNormalize(Value);
end;

procedure TGLVerletSlider.SatisfyConstraint(const iteration, maxIterations: Integer);
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
// ------------------ TGLVerletFrictionSphere ------------------
// ------------------
function TGLVerletFrictionSphere.GetBSphere: TBSphere;
begin
  result.Center := FLocation;
  result.Radius := FRadius;
end;

procedure TGLVerletFrictionSphere.SatisfyConstraintForEdge(const aEdge: TGLVerletEdge;
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

procedure TGLVerletFrictionSphere.SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
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
// ------------------ TGLVerletFrictionCylinder ------------------
// ------------------
procedure TGLVerletFrictionCylinder.SetRadius(const val: Single);
begin
  FRadius := val;
  FRadius2 := Sqr(val);
end;

procedure TGLVerletFrictionCylinder.SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
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
// ------------------ TGLVerletFrictionCube ------------------
// ------------------
function TGLVerletFrictionCube.GetAABB: TAABB;
begin
  VectorAdd(FLocation, FHalfSides, result.max);
  VectorSubtract(FLocation, FHalfSides, result.min);
end;

// BROKEN AND VERY SLOW!
procedure TGLVerletFrictionCube.SatisfyConstraintForEdge(const aEdge: TGLVerletEdge;
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

procedure TGLVerletFrictionCube.SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
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

procedure TGLVerletFrictionCube.SetSides(const Value: TAffineVector);
begin
  FSides := Value;
  FHalfSides := VectorScale(Sides, 0.5);
  UpdateCachedAABB;
end;

// ------------------
// ------------------ TGLVerletFrictionCapsule ------------------
// ------------------

procedure TGLVerletFrictionCapsule.SetAxis(const val: TAffineVector);
begin
  FAxis := VectorNormalize(val);
  UpdateCachedBSphere;
end;

procedure TGLVerletFrictionCapsule.SetLength(const val: Single);
begin
  FLength := val;
  FLengthDiv2 := val * 0.5;
  UpdateCachedBSphere;
end;

procedure TGLVerletFrictionCapsule.SetRadius(const val: Single);
begin
  FRadius := val;
  FRadius2 := Sqr(val);
  UpdateCachedBSphere;
end;

function TGLVerletFrictionCapsule.GetBSphere: TBSphere;
begin
  result.Center := FLocation;
  result.Radius := Length + Radius;
end;

procedure TGLVerletFrictionCapsule.SatisfyConstraintForNode(const aNode: TGLBaseVerletNode;
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

procedure TGLVerletFrictionCapsule.SatisfyConstraintForEdge(const aEdge: TGLVerletEdge;
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
// ------------------ TGLVerletEdge ------------------
// ------------------
constructor TGLVerletEdge.CreateEdgeOwned(const aNodeA, aNodeB: TGLBaseVerletNode);
begin
  FNodeA := aNodeA;
  FNodeB := aNodeB;
  inherited CreateOwned(aNodeA.Owner.SpacePartition);
end;

procedure TGLVerletEdge.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB.min := FNodeA.FLocation;
  FCachedAABB.max := FNodeA.FLocation;
  AABBInclude(FCachedAABB, FNodeB.FLocation);
  AABBToBSphere(FCachedAABB, FCachedBSphere);
end;

// ------------------
// ------------------ TGLVerletEdgeList ------------------
// ------------------
function TGLVerletEdgeList.GetItems(i: Integer): TGLVerletEdge;
begin
  result := Get(i);
end;

procedure TGLVerletEdgeList.SetItems(i: Integer; const Value: TGLVerletEdge);
begin
  Put(i, Value);
end;

procedure TGLBaseVerletNode.UpdateCachedAABBAndBSphere;
begin
  FCachedAABB.min := FLocation;
  FCachedAABB.max := FLocation;
  FCachedBSphere.Center := FLocation;
  FCachedBSphere.Radius := 0;
end;

procedure TGLBaseVerletNode.SetLocation(const Value: TAffineVector);
begin
  FLocation := Value;
  FChangedOnStep := Owner.CurrentStepCount;
end;

procedure TGLVerletWorld.CreateOctree(const OctreeMin, OctreeMax: TAffineVector;
  const LeafThreshold, MaxTreeDepth: Integer);
var
  Octree: TGLOctreeSpacePartition;
begin
  Assert(FNodes.Count = 0,
    'You can only create an octree while the world is empty!');
  FreeAndNil(FSpacePartition);
  Octree := TGLOctreeSpacePartition.Create;
  Octree.SetSize(OctreeMin, OctreeMax);
  Octree.MaxTreeDepth := MaxTreeDepth;
  Octree.LeafThreshold := LeafThreshold;
  Octree.CullingMode := cmGrossCulling;
  FSpacePartition := Octree;
  if FUpdateSpacePartion = uspNever then
    FUpdateSpacePartion := uspEveryFrame;
end;

procedure TGLVerletWorld.PauseInertia(const IterationSteps: Integer);
begin
  FInertaPauseSteps := IterationSteps + 1;
  Inertia := False;
end;

// ------------------
// TGLVerletGlobalFrictionConstraintBox
// ------------------
procedure TGLVerletGlobalFrictionConstraintBox.PerformSpaceQuery;
begin
  Owner.SpacePartition.QueryAABB(FCachedAABB);
end;

procedure TGLVerletGlobalFrictionConstraintBox.SetLocation
  (const Value: TAffineVector);
begin
  inherited;

  UpdateCachedAABB;
end;

procedure TGLVerletGlobalFrictionConstraintBox.UpdateCachedAABB;
begin
  FCachedAABB := GetAABB;
end;

// -------------------------------------------
// TGLVerletGlobalFrictionConstraintSphere
// -------------------------------------------
procedure TGLVerletGlobalFrictionConstraintSphere.PerformSpaceQuery;
begin
  Owner.SpacePartition.QueryBSphere(FCachedBSphere);
end;

procedure TGLVerletGlobalFrictionConstraintSphere.SetLocation
  (const Value: TAffineVector);
begin
  inherited;
  UpdateCachedBSphere;
end;

procedure TGLVerletGlobalFrictionConstraintSphere.UpdateCachedBSphere;
begin
  FCachedBSphere := GetBSphere;
end;

constructor TGLVerletGlobalConstraint.Create(const aOwner: TGLVerletWorld);
begin
  inherited;
  if Assigned(aOwner) then
    aOwner.ConstraintsWithBeforeIterations.Add(Self);
end;

destructor TGLVerletGlobalConstraint.Destroy;
begin
  if Assigned(Owner) then
    Owner.ConstraintsWithBeforeIterations.Remove(Self);
  inherited;
end;

//-----------------------------
// TGLVerletHair
//-----------------------------
procedure TGLVerletHair.AddStickStiffness(const ANodeSkip: integer);
var
  i: integer;
begin
  for i := 0 to NodeList.Count - (1 + ANodeSkip * 2) do
    FStiffnessList.Add(VerletWorld.CreateStick(NodeList[i],
      NodeList[i + 2 * ANodeSkip]));
end;

procedure TGLVerletHair.BuildHair(const AAnchorPosition, AHairDirection
  : TAffineVector);
var
  i: integer;
  Position: TAffineVector;
  Node, PrevNode: TGLBaseVerletNode;
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

procedure TGLVerletHair.BuildStiffness;
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

procedure TGLVerletHair.Clear;
var
  i: integer;
begin
  ClearStiffness;
  for i := FNodeList.Count - 1 downto 0 do
    FNodeList[i].Free;
  FNodeList.Clear;
  FStiffnessList.Clear;
end;

procedure TGLVerletHair.ClearStiffness;
var
  i: integer;
begin
  for i := 0 to FStiffnessList.Count - 1 do
    TGLVerletConstraint(FStiffnessList[i]).Free;
  FStiffnessList.Clear;
end;

constructor TGLVerletHair.Create(const AVerletWorld: TGLVerletWorld;
  const ARootDepth, AHairLength: single; ALinkCount: integer;
  const AAnchorPosition, AHairDirection: TAffineVector;
  const AStiffness: TGLStiffnessSetVH);
begin
  FVerletWorld := AVerletWorld;
  FRootDepth := ARootDepth;
  FLinkCount := ALinkCount;
  FHairLength := AHairLength;

  FNodeList := TGLVerletNodeList.Create;
  FStiffness := AStiffness;
  FStiffnessList := TList.Create;

  BuildHair(AAnchorPosition, AHairDirection);
end;

destructor TGLVerletHair.Destroy;
begin
  Clear;
  FreeAndNil(FNodeList);
  FreeAndNil(FStiffnessList);
  inherited;
end;

function TGLVerletHair.GetAnchor: TGLBaseVerletNode;
begin
  result := NodeList[1];
end;

function TGLVerletHair.GetLinkLength: single;
begin
  if LinkCount > 0 then
    result := HairLength / LinkCount
  else
    result := 0;
end;

function TGLVerletHair.GetRoot: TGLBaseVerletNode;
begin
  result := NodeList[0];
end;

procedure TGLVerletHair.SetStiffness(const Value: TGLStiffnessSetVH);
begin
  FStiffness := Value;
  BuildStiffness;
end;

// ------------------
// ------------------ Global methods ------------------
// ------------------

procedure AddVerletConstriantsToVerletWorld
  (Colliders: TGLSkeletonColliderList; World: TGLVerletWorld);
var
  i: Integer;
begin
  for i := 0 to Colliders.Count - 1 do
    if Colliders[i] is TGLVerletSkeletonCollider then
      TGLVerletSkeletonCollider(Colliders[i]).AddToVerletWorld(World);
end;

// ------------------
// ------------------ TGLVerletSkeletonCollider ------------------
// ------------------
procedure TGLVerletSkeletonCollider.WriteToFiler(Writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(Writer);
  Writer.WriteInteger(0); // Archive Version 0
end;

procedure TGLVerletSkeletonCollider.ReadFromFiler(Reader: TGLVirtualReader);
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

procedure TGLVerletSkeletonCollider.AddToVerletWorld
  (VerletWorld: TGLVerletWorld);
begin
  AlignCollider;
end;

// ------------------
// ------------------ TGLVerletSphere ------------------
// ------------------
constructor TGLVerletSphere.Create;
begin
  inherited;
  Radius := 0.5;
  AlignCollider;
end;

procedure TGLVerletSphere.WriteToFiler(Writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(Writer);
  Writer.WriteInteger(0); // Archive Version 0
  Writer.WriteFloat(FRadius);
end;

procedure TGLVerletSphere.ReadFromFiler(Reader: TGLVirtualReader);
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

procedure TGLVerletSphere.AddToVerletWorld(VerletWorld: TGLVerletWorld);
begin
  FVerletConstraint := TGLVerletFrictionSphere.Create(VerletWorld);
  TGLVerletFrictionSphere(FVerletConstraint).Radius := FRadius;
  inherited;
end;

procedure TGLVerletSphere.AlignCollider;
begin
  inherited;
  if Assigned(FVerletConstraint) then
    TGLVerletFrictionSphere(FVerletConstraint).Location :=
      AffineVectorMake(GlobalMatrix.W);
end;

procedure TGLVerletSphere.SetRadius(const Val: Single);
begin
  if Val <> FRadius then
  begin
    FRadius := Val;
    if Assigned(FVerletConstraint) then
      TGLVerletFrictionSphere(FVerletConstraint).Radius := FRadius;
  end;
end;

// ------------------
// ------------------ TGLVerletCapsule ------------------
// ------------------
constructor TGLVerletCapsule.Create;
begin
  inherited;
  Radius := 0.5;
  Length := 1;
  AlignCollider;
end;

procedure TGLVerletCapsule.WriteToFiler(Writer: TGLVirtualWriter);
begin
  inherited WriteToFiler(Writer);
  Writer.WriteInteger(0); // Archive Version 0
  Writer.WriteFloat(FRadius);
  Writer.WriteFloat(FLength);
end;

procedure TGLVerletCapsule.ReadFromFiler(Reader: TGLVirtualReader);
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

procedure TGLVerletCapsule.AddToVerletWorld(VerletWorld: TGLVerletWorld);
begin
  FVerletConstraint := TGLVerletFrictionCapsule.Create(VerletWorld);
  TGLVerletFrictionCapsule(FVerletConstraint).Radius := FRadius;
  TGLVerletFrictionCapsule(FVerletConstraint).Length := FLength;
  inherited;
end;

procedure TGLVerletCapsule.AlignCollider;
begin
  inherited;
  if Assigned(FVerletConstraint) then
  begin
    TGLVerletFrictionCapsule(FVerletConstraint).Location :=
      AffineVectorMake(GlobalMatrix.W);
    TGLVerletFrictionCapsule(FVerletConstraint).Axis :=
      AffineVectorMake(GlobalMatrix.Y);
  end;
end;

procedure TGLVerletCapsule.SetRadius(const Val: Single);
begin
  if Val <> FRadius then
  begin
    FRadius := Val;
    if Assigned(FVerletConstraint) then
      TGLVerletFrictionCapsule(FVerletConstraint).Radius := FRadius;
  end;
end;

procedure TGLVerletCapsule.SetLength(const Val: Single);
begin
  if Val <> FLength then
  begin
    FLength := Val;
    if Assigned(FVerletConstraint) then
      TGLVerletFrictionCapsule(FVerletConstraint).Length := FLength;
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterClasses([TGLVerletSkeletonCollider, TGLVerletSphere,
  TGLVerletCapsule]);


end.
