//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.DCE;

(*
  Dynamic Collision Engine
  How to use:
  - Add a DCEManager to you form and configure its properties
  - Add a Dynamic Collision Behavior to you object
  - Add a Static Collision behaviour to objects which yours will collide
  - You can choose the shape of your static object
  - csEllipsoid, csBox
  - csFreeform MUST BE A TGLFreeform, otherwise will raise errors
  - csTerrain MUST BE A TGLTerrainRenderer, same condition above
  - Active: Disable or enable the behaviour for this object
  - Friction: is a value aprox. between 0 (no friction) and 100 (no movement)
  - Layer: An object collides only with lower or equal layers
  - Size: is used for Ellipsoids (Radius) / Boxes (Dimensions)
  - Solid: Object still generate collision events but it doesn't "block" the dynamic object
  - UseGravity: You can disable the gravity for that object
  - SlideOrBounce: The object can bounce like a ball or slide like an FPS
  - BounceFactor: Restituition factor, 1 means that it will bounce forever
*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Types,

  GLS.Scene,
  GLS.XCollection,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.VectorFileObjects,
  GLS.EllipseCollision,
  GLS.TerrainRenderer,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.ProxyObjects,
  GLS.MultiProxy,
  GLS.Manager,
  GLS.VectorTypes,
  GLS.Strings;

type
  // Only csEllipsoid can have dynamic behaviour
  TDCEShape = (csEllipsoid, csBox, csFreeform, csTerrain);

  (* Indicates which type of layer comparison is made when trying to detect
    collisions between 2 bodies (A and B). Possible values are:
    ccsDCEStandard: Collides bodies if A.layer <= B.layer
    ccsCollisionStandard: Collides bodies if either A or B have
    layer equal to zero or if their layers are different.
    ccsHybrid: Collides bodies if either one of the previous
    checks would pass (i.e. if the layer of either body  is
    equal to 0 or if A.layer <= B.layer) *and* if both
    layers are positive (that is, turns off collision
    for bodies whose layer is < 0) *)
  TDCECollisionSelection = (ccsDCEStandard, ccsCollisionStandard, ccsHybrid);

  TDCECollision = record
    Position: TAffineVector;
    Normal: TAffineVector; // Surface normal
    Bounce: TAffineVector; // Surface reflection
    Nearest: Boolean;
    RootCollision: Boolean;
    Distance: single;
  end;

  TGLDCEStatic = class;
  TGLDCEDynamic = class;

  TDCECollisionEvent = procedure(Sender: TObject;
    object1, object2: TGLBaseSceneObject; CollisionInfo: TDCECollision)
    of object;
  TDCEObjectCollisionEvent = procedure(Sender: TObject;
    ObjectCollided: TGLBaseSceneObject; CollisionInfo: TDCECollision) of object;

  // The Dynamic Collision Engine Manager class
  TGLDCEManager = class(TComponent)
  private
    FStatics: TList;
    FDynamics: TList;
    FGravity: single;
    FWorldDirection: TGLCoordinates; // Used to calculate jumps f.i.
    FWorldScale: single;
    FMovimentScale: single;
    FStandardiseLayers: TDCECollisionSelection;
    FManualStep: Boolean;
    FOnCollision: TDCECollisionEvent;
    procedure SetWorldDirection(const Value: TGLCoordinates);
    procedure SetWorldScale(const Value: single);
    function GetDynamicCount: Integer;
    function GetStaticCount: Integer;
  protected
    procedure RegisterStatic(aClient: TGLDCEStatic);
    procedure DeRegisterStatic(aClient: TGLDCEStatic);
    procedure DeRegisterAllStatics;
    procedure RegisterDynamic(aClient: TGLDCEDynamic);
    procedure DeRegisterDynamic(aClient: TGLDCEDynamic);
    procedure DeRegisterAllDynamics;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Moves the body by the distance and returns the average friction
    function MoveByDistance(var Body: TGLDCEDynamic;
      deltaS, deltaAbsS: TAffineVector): single;
    procedure Step(deltaTime: Double);
    property DynamicCount: Integer read GetDynamicCount;
    property StaticCount: Integer read GetStaticCount;
  published
    property Gravity: single read FGravity write FGravity;
    property WorldDirection: TGLCoordinates read FWorldDirection
      write SetWorldDirection;
    property WorldScale: single read FWorldScale write SetWorldScale;
    property MovimentScale: single read FMovimentScale write FMovimentScale;
    Property StandardiseLayers: TDCECollisionSelection read FStandardiseLayers
      write FStandardiseLayers;
    Property ManualStep: Boolean read FManualStep write FManualStep;
    property OnCollision: TDCECollisionEvent read FOnCollision
      write FOnCollision;
  end;

  TGLDCEStatic = class(TGLBehaviour)
  private
    FManager: TGLDCEManager;
    FManagerName: String; // NOT persistent, temporarily used for persistence
    FActive: Boolean;
    FShape: TDCEShape;
    // Collides only with lower or equal layers
    FLayer: Integer;
    // Collide and slide if true, otherwise it "walk thru walls"
    FSolid: Boolean;
    FFriction: single; // 0 (no friction); 100 (no movement)
    FBounceFactor: single; // 0 (don't bounce); 1 (bounce forever)
    FSize: TGLCoordinates;
    // Events
    FOnCollision: TDCEObjectCollisionEvent;
    procedure SetShape(const Value: TDCEShape);
    procedure SetFriction(const Value: single);
    procedure SetBounceFactor(const Value: single);
    procedure SetSize(const Value: TGLCoordinates);
  protected
    procedure SetManager(const val: TGLDCEManager);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    property OnCollision: TDCEObjectCollisionEvent read FOnCollision
      write FOnCollision;
  published
    property Active: Boolean read FActive write FActive;
    property Manager: TGLDCEManager read FManager write SetManager;
    property Shape: TDCEShape read FShape write SetShape;
    property Layer: Integer read FLayer write FLayer;
    property Solid: Boolean read FSolid write FSolid;
    property Friction: single read FFriction write SetFriction;
    property BounceFactor: single read FBounceFactor write SetBounceFactor;
    property Size: TGLCoordinates read FSize write SetSize;
  end;

  TDCESlideOrBounce = (csbSlide, csbBounce);

  TGLDCEDynamic = class(TGLBehaviour)
  private
    FManager: TGLDCEManager;
    FManagerName: String; // NOT persistent, temporarily used for persistence
    FActive: Boolean;
    FUseGravity: Boolean;
    FLayer: Integer; // Collides only with lower or equal layers
    FSolid: Boolean;
    // Collide and slide if true, otherwise it "walk thru walls"
    FFriction: single; // 0 (no friction); 100 (no movement)
    FBounceFactor: single; // 0 (don't bounce); 1 (bounce forever)
    FSize: TGLCoordinates;
    // Number of iterations of the collision method
    FMaxRecursionDepth: byte;
    FSlideOrBounce: TDCESlideOrBounce; // gak20041122
    // Movement
    FAccel: TAffineVector; // Current acceleration
    FSpeed: TAffineVector; // Current speed
    FAbsAccel: TAffineVector; // Current absolute accel
    FAbsSpeed: TAffineVector; // Current absolute speed
    FGravSpeed: TAffineVector; // Current gravity speed
    FTotalFriction: single; // Current sum of all contatcs friction
    FInGround: Boolean;
    FGroundNormal: TAffineVector;
    FJumpHeight, FJumpForce, FJumpSpeed: single;
    FJumping: Boolean;
    // Events
    FOnCollision: TDCEObjectCollisionEvent;
    procedure SetFriction(const Value: single);
    procedure SetBounceFactor(const Value: single);
    procedure SetSize(const Value: TGLCoordinates);
  protected
    procedure SetManager(const val: TGLDCEManager);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    procedure ApplyAccel(NewAccel: TAffineVector); overload;
    procedure ApplyAccel(x, y, z: single); overload;
    procedure ApplyAbsAccel(NewAccel: TAffineVector); overload;
    procedure ApplyAbsAccel(x, y, z: single); overload;
    procedure StopAccel;
    procedure StopAbsAccel;
    procedure Jump(jHeight, jSpeed: single);
    procedure Move(deltaS: TAffineVector; deltaTime: Double);
    procedure MoveTo(Position: TAffineVector; Amount: single);
    procedure DoMove(deltaTime: Double);
    procedure DoProgress(const progressTime: TGLProgressTimes); override;
    // Runtime only
    property Speed: TAffineVector read FSpeed write FSpeed;
    property InGround: Boolean read FInGround;
    property MaxRecursionDepth: byte read FMaxRecursionDepth
      write FMaxRecursionDepth;
    property OnCollision: TDCEObjectCollisionEvent read FOnCollision
      write FOnCollision;
  published
    property Active: Boolean read FActive write FActive;
    property Manager: TGLDCEManager read FManager write SetManager;
    property UseGravity: Boolean read FUseGravity write FUseGravity;
    property Layer: Integer read FLayer write FLayer;
    property Solid: Boolean read FSolid write FSolid;
    property Friction: single read FFriction write SetFriction;
    property BounceFactor: single read FBounceFactor write SetBounceFactor;
    property Size: TGLCoordinates read FSize write SetSize;
    property SlideOrBounce: TDCESlideOrBounce read FSlideOrBounce
      write FSlideOrBounce;
  end;

  // ---------------------- DCE Misc functions ---------------------------

  // Calculate and set the collision range
procedure ECSetCollisionRange(var MovePack: TECMovePack);
// Set the collider lists to null
procedure ECResetColliders(var MovePack: TECMovePack);
// Add freeform's octree data
procedure ECAddFreeForm(var MovePack: TECMovePack; FreeForm: TGLBaseSceneObject;
  Solid: Boolean; ObjectID: Integer);
// Add a TriMesh box
procedure ECAddBox(var MovePack: TECMovePack; BoxObj: TGLBaseSceneObject;
  BoxSize: TAffineVector; Solid: Boolean; ObjectID: Integer);
// Add the terrain as a TriMesh
procedure ECAddTerrain(var MovePack: TECMovePack;
  TerrainRenderer: TGLTerrainRenderer; Resolution: single; Solid: Boolean;
  ObjectID: Integer);
// Add a static ellipsoid
procedure ECAddEllipsoid(var MovePack: TECMovePack;
  ePos, eRadius: TAffineVector; Solid: Boolean; ObjectID: Integer);

function GetOrCreateDCEStatic(behaviours: TGLBehaviours): TGLDCEStatic;
  overload;
function GetOrCreateDCEStatic(obj: TGLBaseSceneObject): TGLDCEStatic; overload;
function GetOrCreateDCEDynamic(behaviours: TGLBehaviours)
  : TGLDCEDynamic; overload;
function GetOrCreateDCEDynamic(obj: TGLBaseSceneObject): TGLDCEDynamic;
  overload;

const
  DCEBox: array [0 .. 35] of TAffineVector = ((x: 1; y: - 1; z: - 1), (x: 1;
    y: 1; z: - 1), (x: 1; y: - 1; z: 1), (x: 1; y: 1; z: - 1), (x: 1; y: 1;
    z: 1), (x: 1; y: - 1; z: 1), (x: 1; y: 1; z: - 1), (x: - 1; y: 1; z: - 1),
    (x: - 1; y: 1; z: 1), (x: 1; y: 1; z: 1), (x: 1; y: 1; z: - 1), (x: - 1;
    y: 1; z: 1), (x: - 1; y: 1; z: 1), (x: - 1; y: - 1; z: 1), (x: 1; y: - 1;
    z: 1), (x: 1; y: 1; z: 1), (x: - 1; y: 1; z: 1), (x: 1; y: - 1; z: 1),
    (x: - 1; y: - 1; z: 1), (x: - 1; y: 1; z: 1), (x: - 1; y: 1; z: - 1),
    (x: - 1; y: - 1; z: - 1), (x: - 1; y: - 1; z: 1), (x: - 1; y: 1; z: - 1),
    (x: 1; y: - 1; z: 1), (x: - 1; y: - 1; z: 1), (x: 1; y: - 1; z: - 1),
    (x: - 1; y: - 1; z: 1), (x: - 1; y: - 1; z: - 1), (x: 1; y: - 1; z: - 1),
    (x: 1; y: 1; z: - 1), (x: 1; y: - 1; z: - 1), (x: - 1; y: 1; z: - 1), (x: 1;
    y: - 1; z: - 1), (x: - 1; y: - 1; z: - 1), (x: - 1; y: 1; z: - 1));


// ----------------------------------------------------------------
implementation
// ----------------------------------------------------------------

// ------------------------ DCE Misc -----------------------------

procedure ECSetCollisionRange(var MovePack: TECMovePack);
var
  N: TAffineVector;
begin
  N.x := Abs(MovePack.Velocity.x) + Abs(MovePack.Gravity.x) +
    (MovePack.Radius.x);
  N.y := Abs(MovePack.Velocity.y) + Abs(MovePack.Gravity.y) +
    (MovePack.Radius.y);
  N.z := Abs(MovePack.Velocity.z) + Abs(MovePack.Gravity.z) +
    (MovePack.Radius.z);
  MovePack.CollisionRange := MaxXYZComponent(N);
end;

procedure ECResetColliders(var MovePack: TECMovePack);
begin
  SetLength(MovePack.TriMeshes, 0);
  SetLength(MovePack.Freeforms, 0);
  SetLength(MovePack.Colliders, 0);
end;

procedure ECAddFreeForm(var MovePack: TECMovePack; FreeForm: TGLBaseSceneObject;
  Solid: Boolean; ObjectID: Integer);
var
  i, count: Integer;
  Pos: TGLVector;
  Master: TGLBaseSceneObject;
  d1, d2: single;
begin
  Master := FreeForm;
  if Master is TGLFreeFormProxy then
    Master := TGLFreeFormProxy(Master).MasterObject;
  if Master is TGLMultiProxy then
    if TGLMultiProxy(Master).MasterObjects.count > 0 then
      Master := TGLMultiProxy(Master).MasterObjects[0].MasterObject;
  Assert((Master is TGLFreeForm),
    'Object must be freeform, freeformproxy or freeformbased Multiproxy.');
  Assert(Assigned(TGLFreeForm(Master).Octree),
    'Octree must have been prepared and setup before use.');
  SetVector(Pos, FreeForm.AbsoluteToLocal(MovePack.Position));

  // Is in boundingsphere?
  d1 := VectorDistance2(MovePack.Position,
    AffineVectorMake(FreeForm.AbsolutePosition));
  d2 := sqr(MovePack.CollisionRange + FreeForm.BoundingSphereRadius);
  if d1 > d2 then
    exit;

  count := Length(MovePack.Freeforms);
  with TGLFreeForm(Master).Octree do
  begin
    WalkSphereToLeaf(RootNode, Pos, MovePack.CollisionRange);

    if not Assigned(resultarray) then
      exit;
    // Copy the result array
    SetLength(MovePack.Freeforms, count + 1);
    SetLength(MovePack.Freeforms[count].OctreeNodes, Length(resultarray));
    for i := 0 to High(resultarray) do
      MovePack.Freeforms[count].OctreeNodes[i] := resultarray[i];
    // Reference to this octree
    MovePack.Freeforms[count].triangleFiler := @triangleFiler;
    MovePack.Freeforms[count].ObjectInfo.AbsoluteMatrix :=
      FreeForm.AbsoluteMatrix;
    MovePack.Freeforms[count].ObjectInfo.Solid := Solid;
    MovePack.Freeforms[count].ObjectInfo.ObjectID := ObjectID;
    MovePack.Freeforms[count].InvertedNormals := TGLFreeForm(Master)
      .NormalsOrientation = mnoInvert;
  end;
end;

procedure ECAddBox(var MovePack: TECMovePack; BoxObj: TGLBaseSceneObject;
  BoxSize: TAffineVector; Solid: Boolean; ObjectID: Integer);
var
  t, count, i: Integer;
  p1, p2, p3: TAffineVector;
  BoxRadius, d1, d2: single;
begin

  BoxRadius := MaxXYZComponent(BoxSize) *
    MaxXYZComponent(BoxObj.Scale.AsAffineVector);
  d1 := VectorDistance2(MovePack.Position,
    AffineVectorMake(BoxObj.AbsolutePosition));
  d2 := sqr(MovePack.CollisionRange + BoxRadius);
  if d1 > d2 then
    exit;

  // Add the box to the triangle list
  t := Length(MovePack.TriMeshes);
  SetLength(MovePack.TriMeshes, t + 1);
  ScaleVector(BoxSize, 0.5);
  count := 0;
  i := 0;
  while i < 36 do
  begin
    count := count + 1;
    SetLength(MovePack.TriMeshes[t].Triangles, count);
    with MovePack.TriMeshes[t] do
    begin
      p1 := DCEBox[i];
      ScaleVector(p1, BoxSize);
      p1 := BoxObj.LocalToAbsolute(p1);
      p2 := DCEBox[i + 1];
      ScaleVector(p2, BoxSize);
      p2 := BoxObj.LocalToAbsolute(p2);
      p3 := DCEBox[i + 2];
      ScaleVector(p3, BoxSize);
      p3 := BoxObj.LocalToAbsolute(p3);
      i := i + 3;
      SetVector(Triangles[count - 1].p1, p1);
      SetVector(Triangles[count - 1].p2, p2);
      SetVector(Triangles[count - 1].p3, p3);
      ObjectInfo.Solid := Solid;
      ObjectInfo.ObjectID := ObjectID;
    end;
  end;
end;

procedure ECAddTerrain(var MovePack: TECMovePack;
  TerrainRenderer: TGLTerrainRenderer; Resolution: single; Solid: Boolean;
  ObjectID: Integer);

  function intvec(x, z: single): TAffineVector;
  begin
    result.x := x + MovePack.Position.x;
    result.y := 0 + MovePack.Position.y;
    result.z := z + MovePack.Position.z;
  end;

  function locabs(x, y, z: single): TAffineVector;
  begin
    // result := TerrainRenderer.LocalToAbsolute(AffineVectorMake(x,y,z));
    // result := AffineVectorMake(x,y,z);
    result.x := x + MovePack.Position.x;
    result.y := y + TerrainRenderer.AbsolutePosition.y;
    result.z := z + MovePack.Position.z;
  end;

var
  count, t: Integer;
  x, y, z: single;
begin
  // Add the terrain to the list
  count := Length(MovePack.TriMeshes);
  SetLength(MovePack.TriMeshes, count + 1);
  with MovePack.TriMeshes[count] do
  begin
    ObjectInfo.Solid := Solid;
    ObjectInfo.ObjectID := ObjectID;
    t := 0;
    x := -MovePack.CollisionRange;
    while x < MovePack.CollisionRange do
    begin
      z := -MovePack.CollisionRange;
      while z < MovePack.CollisionRange do
      begin
        // Add 2 triangles
        t := t + 2;
        SetLength(Triangles, t);
        // Tri 1
        y := TerrainRenderer.InterpolatedHeight(intvec(x, z));
        Triangles[t - 2].p1 := locabs(x, y, z);
        y := TerrainRenderer.InterpolatedHeight(intvec(x, z + Resolution));
        Triangles[t - 2].p2 := locabs(x, y, z + Resolution);
        y := TerrainRenderer.InterpolatedHeight(intvec(x + Resolution, z));
        Triangles[t - 2].p3 := locabs(x + Resolution, y, z);
        // Tri 2
        y := TerrainRenderer.InterpolatedHeight
          (intvec(x + Resolution, z + Resolution));
        Triangles[t - 1].p1 := locabs(x + Resolution, y, z + Resolution);
        y := TerrainRenderer.InterpolatedHeight(intvec(x + Resolution, z));
        Triangles[t - 1].p2 := locabs(x + Resolution, y, z);
        y := TerrainRenderer.InterpolatedHeight(intvec(x, z + Resolution));
        Triangles[t - 1].p3 := locabs(x, y, z + Resolution);
        z := z + Resolution;
      end;
      x := x + Resolution;
    end;
  end;
end;

procedure ECAddEllipsoid(var MovePack: TECMovePack;
  ePos, eRadius: TAffineVector; Solid: Boolean; ObjectID: Integer);
var
  count: Integer;
  d1, d2, r: single;
begin
  r := MaxXYZComponent(eRadius);
  d1 := VectorDistance2(MovePack.Position, ePos);
  d2 := sqr(MovePack.CollisionRange + r);
  if d1 > d2 then
    exit;
  // Add possible collider
  count := Length(MovePack.Colliders);
  SetLength(MovePack.Colliders, count + 1);
  with MovePack.Colliders[count] do
  begin
    Position := ePos;
    Radius := eRadius;
    ObjectInfo.Solid := Solid;
    ObjectInfo.ObjectID := ObjectID;
  end;
end;

// --------------------------- DCE ---------------------------------

function RotateVectorByObject(obj: TGLBaseSceneObject; const v: TAffineVector)
  : TAffineVector;
var
  v2: TGLVector;
begin
  SetVector(v2, v);
  SetVector(result, VectorTransform(v2, obj.Matrix^));
end;

constructor TGLDCEManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatics := TList.Create;
  FDynamics := TList.Create;
  FGravity := 0;
  FWorldDirection := TGLCoordinates.CreateInitialized(Self, YHmgVector,
    csVector);
  FWorldScale := 1;
  FMovimentScale := 1;
  FStandardiseLayers := ccsDCEStandard;
  FManualStep := False;
  RegisterManager(Self);
end;

destructor TGLDCEManager.Destroy;
begin
  DeRegisterAllStatics;
  DeRegisterAllDynamics;
  DeRegisterManager(Self);
  FStatics.Free;
  FDynamics.Free;
  FWorldDirection.Free;
  inherited Destroy;
end;

function TGLDCEManager.GetDynamicCount: Integer;
begin
  result := FDynamics.count;
end;

function TGLDCEManager.GetStaticCount: Integer;
begin
  result := FStatics.count;
end;

function TGLDCEManager.MoveByDistance(var Body: TGLDCEDynamic;
  deltaS, deltaAbsS: TAffineVector): single;
var
  // Friction and bounce
  TotalFriction, Bounce, f, m, restitution: single;
  ContactList: TGLIntegerList;
  // Temporary properties (Static or Dynamic)
  tFriction, tBounceFactor: single;
  TObject: TGLBaseSceneObject;
  // Collision results
  ColInfo: TDCECollision;
  lastobj: Integer;
  i, oi: Integer;
  MP: TECMovePack;
  CanCollide, GravCollided: Boolean;
  // Vars used to calculate high velocities
  ColRange, MaxRange: single;
  dCR, dT, deltaCR: Double;
begin
  // Set collider parameters
  MP.Radius := Body.Size.AsAffineVector;
  MP.Position := AffineVectorMake(Body.OwnerBaseSceneObject.AbsolutePosition);
  MP.Velocity := deltaS;
  MP.Gravity := deltaAbsS;
  MP.ObjectInfo.Solid := Body.Solid;
  MP.UnitScale := FWorldScale;
  MP.MaxRecursionDepth := Body.MaxRecursionDepth;
  // Get collision range, if it is too big separate into small pieces
  ECSetCollisionRange(MP);
  ColRange := MP.CollisionRange;
  deltaCR := ColRange;
  MaxRange := MaxXYZComponent(MP.Radius) * 2.1;
  SetLength(MP.Contacts, 0);
  GravCollided := False; // Is colliding with the ground
  Body.FGroundNormal := NullVector;
  while deltaCR > 0 do
  begin
    if deltaCR > MaxRange then
    begin
      dCR := MaxRange;
      deltaCR := deltaCR - MaxRange;
    end
    else
    begin
      dCR := deltaCR;
      deltaCR := 0;
    end;
    dT := dCR / ColRange;
    MP.Velocity := VectorScale(deltaS, dT);
    MP.Gravity := VectorScale(deltaAbsS, dT);

    ECSetCollisionRange(MP);
    ECResetColliders(MP);

    // For each static collider
    for i := 0 to FStatics.count - 1 do
      with TGLDCEStatic(FStatics[i]) do
      begin
        CanCollide := False;
        if (Active) then
          case FStandardiseLayers of
            ccsDCEStandard:
              CanCollide := (Layer <= Body.Layer);
            ccsCollisionStandard:
              CanCollide := (Layer = 0) or (Body.Layer = 0) or
                (Layer <> Body.Layer);
            ccsHybrid:
              CanCollide := ((Layer = 0) or (Body.Layer = 0) or
                (Layer <= Body.Layer)) and (Layer >= 0) and (Body.Layer >= 0);
          end;
        // Add colliders to move pack
        if CanCollide then
        begin
          case Shape of
            csFreeform:
              ECAddFreeForm(MP, OwnerBaseSceneObject, Solid, i);
            csEllipsoid:
              ECAddEllipsoid(MP,
                AffineVectorMake(OwnerBaseSceneObject.AbsolutePosition),
                Size.AsAffineVector, Solid, i);
            csBox:
              ECAddBox(MP, OwnerBaseSceneObject, Size.AsAffineVector, Solid, i);
            csTerrain:
              ECAddTerrain(MP, TGLTerrainRenderer(OwnerBaseSceneObject),
                FWorldScale * 2, Solid, i);
          end;
        end;
      end;
    // For each dynamic collider add a static ellipsoid
    for i := 0 to FDynamics.count - 1 do
      with TGLDCEDynamic(FDynamics[i]) do
      begin
        CanCollide := False;
        if (Active) and (TGLDCEDynamic(FDynamics[i]) <> Body) then
          case FStandardiseLayers of
            ccsDCEStandard:
              CanCollide := (Layer <= Body.Layer);
            ccsCollisionStandard:
              CanCollide := (Layer = 0) or (Body.Layer = 0) or
                (Layer <> Body.Layer);
            ccsHybrid:
              CanCollide := ((Layer = 0) or (Body.Layer = 0) or
                (Layer <= Body.Layer)) and (Layer >= 0) and (Body.Layer >= 0);
          end;
        // Add collider to move pack
        // To differ from static it is added with a negative ID (id < 0)
        if CanCollide then
          ECAddEllipsoid(MP,
            AffineVectorMake(OwnerBaseSceneObject.AbsolutePosition),
            Size.AsAffineVector, Solid, -1 - i);
      end;
    CollideAndSlide(MP);
    if MP.GravityCollided then
    begin
      GravCollided := True;
      Body.FGroundNormal := MP.GroundNormal;
    end;
    MP.Position := MP.ResultPos;
  end;
  // Set the result
  Body.OwnerBaseSceneObject.AbsolutePosition := VectorMake(MP.ResultPos);
  Body.FInGround := GravCollided;
  // Generate events and calculate average friction
  lastobj := -1;
  TotalFriction := Body.Friction;
  ContactList := TGLIntegerList.Create;
  try
    for i := 0 to High(MP.Contacts) do
      with MP do
      begin
        oi := Contacts[i].ObjectInfo.ObjectID;
        // Don't repeat objects with same ID
        if ContactList.IndexOf(oi) >= 0 then
          Continue
        else
          ContactList.Add(oi);
        // Check if it is static or dynamic
        if oi < 0 then
        begin
          tFriction := TGLDCEDynamic(FDynamics[System.Abs(oi) - 1]).Friction;
          tBounceFactor := TGLDCEDynamic(FDynamics[System.Abs(oi) - 1])
            .BounceFactor;
          TObject := TGLDCEDynamic(FDynamics[System.Abs(oi) - 1])
            .OwnerBaseSceneObject;
        end
        else
        begin
          tFriction := TGLDCEStatic(FStatics[oi]).Friction;
          tBounceFactor := TGLDCEStatic(FStatics[oi]).BounceFactor;
          TObject := TGLDCEStatic(FStatics[oi]).OwnerBaseSceneObject;
        end;
        TotalFriction := TotalFriction + tFriction;
        ColInfo.Position := Contacts[i].Position;
        ColInfo.Normal := Contacts[i].SurfaceNormal;
        ColInfo.Bounce := VectorNormalize
          (VectorReflect(VectorAdd(deltaS, deltaAbsS), ColInfo.Normal));
        ColInfo.Nearest := oi = MP.NearestObject;
        // Calculate bounce
        if (Body.SlideOrBounce = csbBounce) and ColInfo.Nearest then
        begin
          Bounce := VectorDotProduct(Body.FSpeed, ColInfo.Normal);
          if Bounce < 0 then
          begin
            restitution := (Body.BounceFactor + tBounceFactor) / 2;
            m := VectorLength(Body.FSpeed);
            f := -Bounce / VectorNorm(ColInfo.Normal) * (1 + restitution);
            CombineVector(Body.FSpeed, ColInfo.Normal, f);
            // Limit bounce speed
            if VectorLength(Body.FSpeed) > m * 2 then
              Body.FSpeed := NullVector;
          end;
          Bounce := VectorDotProduct(Body.FAbsSpeed, ColInfo.Normal);
          if Bounce < 0 then
          begin
            restitution := (Body.BounceFactor + tBounceFactor) / 2;
            m := VectorLength(Body.FAbsSpeed);
            f := -Bounce / VectorNorm(ColInfo.Normal) * (1 + restitution);
            CombineVector(Body.FAbsSpeed, ColInfo.Normal, f);
            // Limit
            if VectorLength(Body.FAbsSpeed) > m * 2 then
              Body.FAbsSpeed := NullVector;
          end;
          Bounce := VectorDotProduct(Body.FGravSpeed, ColInfo.Normal);
          if Bounce < 0 then
          begin
            restitution := (Body.BounceFactor + tBounceFactor) / 2;
            m := VectorLength(Body.FGravSpeed);
            f := -Bounce / VectorNorm(ColInfo.Normal) * (1 + restitution);
            CombineVector(Body.FGravSpeed, ColInfo.Normal, f);
            // Limit
            if VectorLength(Body.FGravSpeed) > m * 2 then
              Body.FGravSpeed := NullVector;
          end;
        end;
        ColInfo.RootCollision := (lastobj <> oi);
        ColInfo.Distance := Contacts[i].Distance;
        lastobj := oi;
        if Assigned(FOnCollision) then
          FOnCollision(Self, Body.OwnerBaseSceneObject, TObject, ColInfo);
        if Assigned(Body.FOnCollision) then
          Body.FOnCollision(Self, TObject, ColInfo);
        if Assigned(Body.FOnCollision) then
          Body.FOnCollision(Self, TObject, ColInfo);
        // If the collided object is static trigger its event
        if (oi >= 0) and Assigned(TGLDCEStatic(FStatics[oi]).FOnCollision) then
          TGLDCEStatic(FStatics[oi]).FOnCollision(Self,
            Body.OwnerBaseSceneObject, ColInfo);
      end;
  finally
    ContactList.Free;
  end;
  result := TotalFriction;
end;

procedure TGLDCEManager.Step(deltaTime: Double);
var
  i: Integer;
begin
  if deltaTime > 0.1 then
    deltaTime := 0.1;
  for i := 0 to FDynamics.count - 1 do
    with TGLDCEDynamic(FDynamics[i]) do
      if Active then
        DoMove(deltaTime);
end;

procedure TGLDCEManager.SetWorldDirection(const Value: TGLCoordinates);
begin
  FWorldDirection := Value;
  FWorldDirection.Normalize;
end;

procedure TGLDCEManager.SetWorldScale(const Value: single);
begin
  if Value = 0 then
    FWorldScale := 0.001
  else if Value < 0 then
    FWorldScale := Abs(Value)
  else
    FWorldScale := Value;
end;

procedure TGLDCEManager.RegisterStatic(aClient: TGLDCEStatic);
begin
  if Assigned(aClient) then
    if FStatics.IndexOf(aClient) < 0 then
    begin
      FStatics.Add(aClient);
      aClient.FManager := Self;
    end;
end;

procedure TGLDCEManager.DeRegisterStatic(aClient: TGLDCEStatic);
begin
  if Assigned(aClient) then
  begin
    aClient.FManager := nil;
    FStatics.Remove(aClient);
  end;
end;

procedure TGLDCEManager.DeRegisterAllStatics;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to FStatics.count - 1 do
    TGLDCEStatic(FStatics[i]).FManager := nil;
  FStatics.Clear;
end;

procedure TGLDCEManager.RegisterDynamic(aClient: TGLDCEDynamic);
begin
  if Assigned(aClient) then
    if FDynamics.IndexOf(aClient) < 0 then
    begin
      FDynamics.Add(aClient);
      aClient.FManager := Self;
    end;
end;

procedure TGLDCEManager.DeRegisterDynamic(aClient: TGLDCEDynamic);
begin
  if Assigned(aClient) then
  begin
    aClient.FManager := nil;
    FDynamics.Remove(aClient);
  end;
end;

procedure TGLDCEManager.DeRegisterAllDynamics;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to FDynamics.count - 1 do
    TGLDCEDynamic(FDynamics[i]).FManager := nil;
  FDynamics.Clear;
end;

// ---------------------
// TGLDCEStatic
// ---------------------

procedure TGLDCEStatic.Assign(Source: TPersistent);
begin
  if Source is TGLDCEStatic then
  begin
    Active := TGLDCEStatic(Source).Active;
    Manager := TGLDCEStatic(Source).Manager;
    Shape := TGLDCEStatic(Source).Shape;
    Layer := TGLDCEStatic(Source).Layer;
    Solid := TGLDCEStatic(Source).Solid;
    Size.Assign(TGLDCEStatic(Source).Size);
    Friction := TGLDCEStatic(Source).Friction;
    BounceFactor := TGLDCEStatic(Source).BounceFactor;
  end;
  inherited Assign(Source);
end;

constructor TGLDCEStatic.Create(AOwner: TXCollection);
begin
  inherited Create(AOwner);
  FActive := True;
  FSize := TGLCoordinates.CreateInitialized(Self, XYZHmgVector, csVector);
  FShape := csEllipsoid;
  FSolid := True;
  FFriction := 1;
  FBounceFactor := 0;
end;

destructor TGLDCEStatic.Destroy;
begin
  Manager := nil;
  FSize.Free;
  inherited Destroy;
end;

class function TGLDCEStatic.FriendlyDescription: String;
begin
  result := 'Static Collision-detection registration';
end;

class function TGLDCEStatic.FriendlyName: String;
begin
  result := 'DCE Static Collider';
end;

procedure TGLDCEStatic.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TGLDCEManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLDCEManager(mng);
    FManagerName := '';
  end;
end;

procedure TGLDCEStatic.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    // ArchiveVersion 1, added inherited call
    WriteInteger(1);
    inherited;
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
    WriteInteger(Integer(FShape));
    WriteInteger(FLayer);
    WriteBoolean(FSolid);
    WriteBoolean(FActive);
    WriteSingle(FFriction);
    WriteSingle(FBounceFactor);
    FSize.WriteToFiler(writer);
  end;
end;

procedure TGLDCEStatic.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion in [0 .. 1]);
    if archiveVersion >= 1 then
      inherited;
    FManagerName := ReadString;
    Manager := nil;
    FShape := TDCEShape(ReadInteger);
    FLayer := ReadInteger;
    FSolid := ReadBoolean;
    FActive := ReadBoolean;
    FFriction := ReadSingle;
    FBounceFactor := ReadSingle;
    FSize.ReadFromFiler(reader);
  end;
end;

procedure TGLDCEStatic.SetBounceFactor(const Value: single);
begin
  FBounceFactor := Value;
  if FBounceFactor < 0 then
    FBounceFactor := 0;
  if FBounceFactor > 1 then
    FBounceFactor := 1;
end;

procedure TGLDCEStatic.SetFriction(const Value: single);
begin
  FFriction := Value;
  if FFriction < 0 then
    FFriction := 0;
  if FFriction > 100 then
    FFriction := 100;
end;

procedure TGLDCEStatic.SetManager(const val: TGLDCEManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterStatic(Self);
    if Assigned(val) then
      val.RegisterStatic(Self);
  end;
end;

procedure TGLDCEStatic.SetShape(const Value: TDCEShape);
begin
  FShape := Value;
end;

procedure TGLDCEStatic.SetSize(const Value: TGLCoordinates);
begin
  FSize.Assign(Value);
  if FSize.x <= 0 then
    FSize.x := 0.1;
  if FSize.y <= 0 then
    FSize.y := 0.1;
  if FSize.z <= 0 then
    FSize.z := 0.1;
end;

//-----------------------------
// TGLDCEDynamic
//-----------------------------

procedure TGLDCEDynamic.ApplyAccel(NewAccel: TAffineVector);
begin
  AddVector(FAccel, NewAccel);
end;

procedure TGLDCEDynamic.ApplyAccel(x, y, z: single);
begin
  AddVector(FAccel, AffineVectorMake(x, y, z));
end;

procedure TGLDCEDynamic.ApplyAbsAccel(NewAccel: TAffineVector);
begin
  AddVector(FAbsAccel, NewAccel);
end;

procedure TGLDCEDynamic.ApplyAbsAccel(x, y, z: single);
begin
  AddVector(FAbsAccel, AffineVectorMake(x, y, z));
end;

procedure TGLDCEDynamic.StopAccel;
begin
  SetVector(FAccel, NullVector);
end;

procedure TGLDCEDynamic.StopAbsAccel;
begin
  SetVector(FAbsAccel, NullVector);
end;

procedure TGLDCEDynamic.Assign(Source: TPersistent);
begin
  if Source is TGLDCEDynamic then
  begin
    Manager := TGLDCEDynamic(Source).Manager;
    Active := TGLDCEDynamic(Source).Active;
    UseGravity := TGLDCEDynamic(Source).UseGravity;
    Layer := TGLDCEDynamic(Source).Layer;
    Solid := TGLDCEDynamic(Source).Solid;
    Size.Assign(TGLDCEDynamic(Source).Size);
    Friction := TGLDCEDynamic(Source).Friction;
    BounceFactor := TGLDCEDynamic(Source).BounceFactor;
    SlideOrBounce := TGLDCEDynamic(Source).SlideOrBounce;
    MaxRecursionDepth := TGLDCEDynamic(Source).MaxRecursionDepth;
  end;
  inherited Assign(Source);
end;

constructor TGLDCEDynamic.Create(AOwner: TXCollection);
begin
  inherited Create(AOwner);
  FActive := True;
  FUseGravity := True;
  FSize := TGLCoordinates.CreateInitialized(Self, XYZHmgVector, csVector);
  FSolid := True;
  FFriction := 1;
  FBounceFactor := 0;
  FMaxRecursionDepth := 5;
  FSlideOrBounce := csbSlide;
  FInGround := False;
  FAccel := NullVector;
  FAbsAccel := NullVector;
  FSpeed := NullVector;
  FAbsSpeed := NullVector;
  FGravSpeed := NullVector;
end;

destructor TGLDCEDynamic.Destroy;
begin
  Manager := nil;
  FSize.Free;
  inherited Destroy;
end;

procedure TGLDCEDynamic.DoMove(deltaTime: Double);
var
  fGround, fAir, G: single;
  v, deltaS, deltaAbsS: TAffineVector;

  procedure Accel(var aSpeed: TAffineVector; aFric: single;
    aForce: TAffineVector);
  begin
    ScaleVector(aForce, deltaTime);
    ScaleVector(aSpeed, aFric);
    aSpeed := VectorAdd(aForce, aSpeed);
  end;

begin
  if (FSlideOrBounce = csbBounce) then
    FAccel := RotateVectorByObject(OwnerBaseSceneObject, FAccel);
  // Ground friction
  fGround := 1 - deltaTime * FTotalFriction;
  if fGround < 0 then
    fGround := 0;
  // Air friction
  fAir := 1 - deltaTime * FFriction;
  if fAir < 0 then
    fAir := 0;
  if FUseGravity and (not FInGround) then
    ScaleVector(FAccel, 0.01);
  // v = TIME * force + max(1-TIME*Friction,0) * v;
  Accel(FSpeed, fGround, FAccel);
  Accel(FAbsSpeed, fGround, FAbsAccel);
  (* FSpeed[0] := deltaTime * FAccel[0] + fGround * FSpeed[0];
    FSpeed[1] := deltaTime * FAccel[1] + fGround * FSpeed[1];
    FSpeed[2] := deltaTime * FAccel[2] + fGround * FSpeed[2];

    FAbsSpeed[0] := deltaTime * FAbsAccel[0] + fGround * FAbsSpeed[0];
    FAbsSpeed[1] := deltaTime * FAbsAccel[1] + fGround * FAbsSpeed[1];
    FAbsSpeed[2] := deltaTime * FAbsAccel[2] + fGround * FAbsSpeed[2]; *)
  if FUseGravity then
  begin
    // Calculate gravity acceleration
    if FInGround then
      G := FManager.Gravity * Abs(1 - VectorDotProduct(FGroundNormal,
        FManager.WorldDirection.AsAffineVector))
    else
      G := FManager.Gravity;
    if FJumping then
      G := 0;
    v := VectorScale(FManager.WorldDirection.AsAffineVector, G);
    Accel(FGravSpeed, fAir, v);
    (* FGravSpeed[0] := deltaTime * v[0] + fAir * FGravSpeed[0];
      FGravSpeed[1] := deltaTime * v[1] + fAir * FGravSpeed[1];
      FGravSpeed[2] := deltaTime * v[2] + fAir * FGravSpeed[2]; *)
  end
  else
    FGravSpeed := NullVector;
  if FJumping then
  begin
    FJumpSpeed := FJumpForce;
    FJumpHeight := FJumpHeight - (FJumpSpeed * deltaTime);
    FJumping := FJumpHeight > 0;
    if FJumping then
      FGravSpeed := NullVector
    else
    begin
      v := VectorScale(FManager.WorldDirection.AsAffineVector, FJumpSpeed);
      AddVector(FGravSpeed, v);
      FJumpForce := 0;
      FJumpSpeed := 0;
    end;
  end;
  // s = s0 + vt (add relative speed)
  if FSlideOrBounce = csbBounce then
    deltaS := FSpeed
  else
    deltaS := RotateVectorByObject(OwnerBaseSceneObject, FSpeed);
  // Add absolute speed
  AddVector(deltaS, FAbsSpeed);
  // Add jump speed
  v := VectorScale(FManager.WorldDirection.AsAffineVector, FJumpSpeed);
  AddVector(deltaS, v);
  (* The absolute space must be only the gravity
    so it can calculate when it is in the ground *)
  deltaAbsS := FGravSpeed;
  ScaleVector(deltaS, deltaTime);
  ScaleVector(deltaAbsS, deltaTime);
  // Returns the friction of all collided objects
  FTotalFriction := FManager.MoveByDistance(Self, deltaS, deltaAbsS);
  FAccel := NullVector;
  FAbsAccel := NullVector;
end;

procedure TGLDCEDynamic.DoProgress(const progressTime: TGLProgressTimes);
begin
  inherited DoProgress(progressTime);
  Assert(Assigned(Manager), 'DCE Manager not assigned to behaviour.');
  if (not FManager.ManualStep) and FActive then
  begin
    if progressTime.deltaTime > 0.1 then
      DoMove(0.1)
    else
      DoMove(progressTime.deltaTime);
  end;
end;

class function TGLDCEDynamic.FriendlyDescription: String;
begin
  result := 'Dynamic Collision-detection registration';
end;

class function TGLDCEDynamic.FriendlyName: String;
begin
  result := 'DCE Dynamic Collider';
end;

procedure TGLDCEDynamic.Jump(jHeight, jSpeed: single);
begin
  if (not FJumping) and (FInGround) and
    (VectorDotProduct(FGroundNormal, FManager.WorldDirection.AsAffineVector)
    > 0.5) then
  begin
    FJumpHeight := jHeight;
    FJumpForce := jSpeed;
    FJumpSpeed := FJumpForce;
    FJumping := True;
    FInGround := False;
    AddVector(FAbsSpeed, RotateVectorByObject(OwnerBaseSceneObject, FSpeed));
    FSpeed := NullVector;
  end;
end;

procedure TGLDCEDynamic.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TGLDCEManager, FManagerName);
    if Assigned(mng) then
      Manager := TGLDCEManager(mng);
    FManagerName := '';
  end;
end;

procedure TGLDCEDynamic.Move(deltaS: TAffineVector; deltaTime: Double);
begin
  ScaleVector(deltaS, deltaTime);
  FManager.MoveByDistance(Self, NullVector, deltaS);
end;

procedure TGLDCEDynamic.MoveTo(Position: TAffineVector; Amount: single);
begin
  SubtractVector(Position,
    AffineVectorMake(OwnerBaseSceneObject.AbsolutePosition));
  Move(Position, Amount);
end;

procedure TGLDCEDynamic.WriteToFiler(writer: TWriter);
begin
  with writer do
  begin
    // ArchiveVersion 1, added inherited call
    WriteInteger(1);
    inherited;
    if Assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
    WriteInteger(FLayer);
    WriteBoolean(FSolid);
    WriteBoolean(FActive);
    WriteBoolean(FUseGravity);
    WriteSingle(FFriction);
    WriteSingle(FBounceFactor);
    WriteInteger(FMaxRecursionDepth);
    WriteInteger(ord(FSlideOrBounce));
    FSize.WriteToFiler(writer);
  end;
end;

procedure TGLDCEDynamic.ReadFromFiler(reader: TReader);
var
  archiveVersion: Integer;
begin
  with reader do
  begin
    archiveVersion := ReadInteger;
    Assert(archiveVersion in [0 .. 1]);
    if archiveVersion >= 1 then
      inherited;
    FManagerName := ReadString;
    Manager := nil;
    FLayer := ReadInteger;
    FSolid := ReadBoolean;
    FActive := ReadBoolean;
    FUseGravity := ReadBoolean;
    FFriction := ReadSingle;
    FBounceFactor := ReadSingle;
    FMaxRecursionDepth := ReadInteger;
    FSlideOrBounce := TDCESlideOrBounce(ReadInteger);
    FSize.ReadFromFiler(reader);
  end;
end;

procedure TGLDCEDynamic.SetBounceFactor(const Value: single);
begin
  FBounceFactor := Value;
  if FBounceFactor < 0 then
    FBounceFactor := 0;
  if FBounceFactor > 1 then
    FBounceFactor := 1;
end;

procedure TGLDCEDynamic.SetFriction(const Value: single);
begin
  FFriction := Value;
  if FFriction < 0 then
    FFriction := 0;
  if FFriction > 100 then
    FFriction := 100;
end;

procedure TGLDCEDynamic.SetManager(const val: TGLDCEManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterDynamic(Self);
    if Assigned(val) then
      val.RegisterDynamic(Self);
  end;
end;

procedure TGLDCEDynamic.SetSize(const Value: TGLCoordinates);
begin
  FSize.Assign(Value);
  if FSize.x <= 0 then
    FSize.x := 0.1;
  if FSize.y <= 0 then
    FSize.y := 0.1;
  if FSize.z <= 0 then
    FSize.z := 0.1;
end;

// ----------------------------------------------------------------

function GetOrCreateDCEStatic(behaviours: TGLBehaviours): TGLDCEStatic;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TGLDCEStatic);
  if i >= 0 then
    result := TGLDCEStatic(behaviours[i])
  else
    result := TGLDCEStatic.Create(behaviours);
end;

function GetOrCreateDCEStatic(obj: TGLBaseSceneObject): TGLDCEStatic;
begin
  result := GetOrCreateDCEStatic(obj.behaviours);
end;

function GetOrCreateDCEDynamic(behaviours: TGLBehaviours): TGLDCEDynamic;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TGLDCEDynamic);
  if i >= 0 then
    result := TGLDCEDynamic(behaviours[i])
  else
    result := TGLDCEDynamic.Create(behaviours);
end;

function GetOrCreateDCEDynamic(obj: TGLBaseSceneObject): TGLDCEDynamic;
begin
  result := GetOrCreateDCEDynamic(obj.behaviours);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

// class registrations
RegisterXCollectionItemClass(TGLDCEStatic);
RegisterXCollectionItemClass(TGLDCEDynamic);

finalization

UnregisterXCollectionItemClass(TGLDCEStatic);
UnregisterXCollectionItemClass(TGLDCEDynamic);

end.
