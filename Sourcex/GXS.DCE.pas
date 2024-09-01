//                                           *
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.DCE;

(*
  How to use:
  - Add a DCEManager to you form and configure its properties
  - Add a Dynamic Collision Behavior to you object
  - Add a Static Collision behaviour to objects which yours will collide
  - You can choose the shape of your static object
  - csEllipsoid, csBox
  - csFreeform MUST BE A TgxFreeform, otherwise will raise errors
  - csTerrain MUST BE A TgxTerrainRenderer, same condition above
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

{$I GXS.Scene.inc}

uses
  System.Classes,
  System.SysUtils,

  GXS.XCollection,
  GXS.VectorGeometry,
  GXS.VectorLists,
  GXS.BaseClasses,
  GXS.Manager,
  GXS.VectorTypes,
  GXS.Strings,

  GXS.Scene,
  GXS.VectorFileObjects,
  GXS.DCEMisc,
  GXS.EllipseCollision,
  GXS.TerrainRenderer,
  GXS.Coordinates;

type
  // Only csEllipsoid can have dynamic behaviour 
  TgxDCEShape = (csEllipsoid, csBox, csFreeform, csTerrain);

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
  TgxDCECollisionSelection = (ccsDCEStandard, ccsCollisionStandard, ccsHybrid);

  TgxDCECollision = record
    Position: TAffineVector;
    Normal: TAffineVector; // Surface normal
    Bounce: TAffineVector; // Surface reflection
    Nearest: Boolean;
    RootCollision: Boolean;
    Distance: single;
  end;

  TgxDCEStatic = class;
  TgxDCEDynamic = class;

  TgxDCECollisionEvent = procedure(Sender: TObject;
    object1, object2: TgxBaseSceneObject; CollisionInfo: TgxDCECollision)
    of object;
  TgxDCEObjectCollisionEvent = procedure(Sender: TObject;
    ObjectCollided: TgxBaseSceneObject; CollisionInfo: TgxDCECollision) of object;

  TgxDCEManager = class(TComponent)
  private
    FStatics: TList;
    FDynamics: TList;
    FGravity: single;
    FWorldDirection: TgxCoordinates; // Used to calculate jumps f.i.
    FWorldScale: single;
    FMovimentScale: single;
    FStandardiseLayers: TgxDCECollisionSelection;
    FManualStep: Boolean;
    FOnCollision: TgxDCECollisionEvent;
    procedure SetWorldDirection(const Value: TgxCoordinates);
    procedure SetWorldScale(const Value: single);
    function GetDynamicCount: Integer;
    function GetStaticCount: Integer;
  protected
    procedure RegisterStatic(aClient: TgxDCEStatic);
    procedure DeRegisterStatic(aClient: TgxDCEStatic);
    procedure DeRegisterAllStatics;
    procedure RegisterDynamic(aClient: TgxDCEDynamic);
    procedure DeRegisterDynamic(aClient: TgxDCEDynamic);
    procedure DeRegisterAllDynamics;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Moves the body by the distance and returns the average friction
    function MoveByDistance(var Body: TgxDCEDynamic;
      deltaS, deltaAbsS: TAffineVector): single;
    procedure Step(deltaTime: Double);
    property DynamicCount: Integer read GetDynamicCount;
    property StaticCount: Integer read GetStaticCount;
  published
    property Gravity: single read FGravity write FGravity;
    property WorldDirection: TgxCoordinates read FWorldDirection write SetWorldDirection;
    property WorldScale: single read FWorldScale write SetWorldScale;
    property MovimentScale: single read FMovimentScale write FMovimentScale;
    Property StandardiseLayers: TgxDCECollisionSelection read FStandardiseLayers write FStandardiseLayers;
    Property ManualStep: Boolean read FManualStep write FManualStep;
    property OnCollision: TgxDCECollisionEvent read FOnCollision write FOnCollision;
  end;

  TgxDCEStatic = class(TgxBehaviour)
  private
    FManager: TgxDCEManager;
    FManagerName: String; // NOT persistent, temporarily used for persistence
    FActive: Boolean;
    FShape: TgxDCEShape;
  	// Collides only with lower or equal layers
    FLayer: Integer;
    // Collide and slide if true, otherwise it "walk thru walls"
    FSolid: Boolean;
    FFriction: single; // 0 (no friction); 100 (no movement)
    FBounceFactor: single; // 0 (don't bounce); 1 (bounce forever)
    FSize: TgxCoordinates;
    // Events
    FOnCollision: TgxDCEObjectCollisionEvent;
    procedure SetShape(const Value: TgxDCEShape);
    procedure SetFriction(const Value: single);
    procedure SetBounceFactor(const Value: single);
    procedure SetSize(const Value: TgxCoordinates);
  protected
    procedure SetManager(const val: TgxDCEManager);
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TXCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: String; override;
    class function FriendlyDescription: String; override;
    property OnCollision: TgxDCEObjectCollisionEvent read FOnCollision write FOnCollision;
  published
    property Active: Boolean read FActive write FActive;
    property Manager: TgxDCEManager read FManager write SetManager;
    property Shape: TgxDCEShape read FShape write SetShape;
    property Layer: Integer read FLayer write FLayer;
    property Solid: Boolean read FSolid write FSolid;
    property Friction: single read FFriction write SetFriction;
    property BounceFactor: single read FBounceFactor write SetBounceFactor;
    property Size: TgxCoordinates read FSize write SetSize;
  end;

  TgxDCESlideOrBounce = (csbSlide, csbBounce);

  TgxDCEDynamic = class(TgxBehaviour)
  private
    FManager: TgxDCEManager;
    FManagerName: String; // NOT persistent, temporarily used for persistence
    FActive: Boolean;
    FUseGravity: Boolean;
    FLayer: Integer; // Collides only with lower or equal layers
    FSolid: Boolean;
    // Collide and slide if true, otherwise it "walk thru walls"
    FFriction: single; // 0 (no friction); 100 (no movement)
    FBounceFactor: single; // 0 (don't bounce); 1 (bounce forever)
    FSize: TgxCoordinates;
    //Number of iterations of the collision method
	FMaxRecursionDepth: byte;
    FSlideOrBounce: TgxDCESlideOrBounce; // gak20041122
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
    FOnCollision: TgxDCEObjectCollisionEvent;
    procedure SetFriction(const Value: single);
    procedure SetBounceFactor(const Value: single);
    procedure SetSize(const Value: TgxCoordinates);
  protected
    procedure SetManager(const val: TgxDCEManager);
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
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    // Runtime only
    property Speed: TAffineVector read FSpeed write FSpeed;
    property InGround: Boolean read FInGround;
    property MaxRecursionDepth: byte read FMaxRecursionDepth write FMaxRecursionDepth; 
    property OnCollision: TgxDCEObjectCollisionEvent read FOnCollision write FOnCollision;
  published
    property Active: Boolean read FActive write FActive;
    property Manager: TgxDCEManager read FManager write SetManager;
    property UseGravity: Boolean read FUseGravity write FUseGravity;
    property Layer: Integer read FLayer write FLayer;
    property Solid: Boolean read FSolid write FSolid;
    property Friction: single read FFriction write SetFriction;
    property BounceFactor: single read FBounceFactor write SetBounceFactor;
    property Size: TgxCoordinates read FSize write SetSize;
    property SlideOrBounce: TgxDCESlideOrBounce read FSlideOrBounce write FSlideOrBounce;
  end;

function GetOrCreateDCEStatic(behaviours: TgxBehaviours): TgxDCEStatic; overload;
function GetOrCreateDCEStatic(obj: TgxBaseSceneObject): TgxDCEStatic; overload;
function GetOrCreateDCEDynamic(behaviours: TgxBehaviours): TgxDCEDynamic; overload;
function GetOrCreateDCEDynamic(obj: TgxBaseSceneObject): TgxDCEDynamic; overload;

// -------------------------------------------------------------------
implementation
// -------------------------------------------------------------------

function RotateVectorByObject(obj: TgxBaseSceneObject; v: TAffineVector): TAffineVector;
var
  v2: TVector4f;
begin
  SetVector(v2, v);
  SetVector(result, VectorTransform(v2, obj.Matrix^));
end;

constructor TgxDCEManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStatics := TList.Create;
  FDynamics := TList.Create;
  FGravity := 0;
  FWorldDirection := TgxCoordinates.CreateInitialized(Self, YHmgVector, csVector);
  FWorldScale := 1;
  FMovimentScale := 1;
  FStandardiseLayers := ccsDCEStandard;
  FManualStep := False;
  RegisterManager(Self);
end;

destructor TgxDCEManager.Destroy;
begin
  DeRegisterAllStatics;
  DeRegisterAllDynamics;
  DeRegisterManager(Self);
  FStatics.Free;
  FDynamics.Free;
  FWorldDirection.Free;
  inherited Destroy;
end;

function TgxDCEManager.GetDynamicCount: Integer;
begin
  result := FDynamics.Count;
end;

function TgxDCEManager.GetStaticCount: Integer;
begin
  result := FStatics.Count;
end;

function TgxDCEManager.MoveByDistance(var Body: TgxDCEDynamic;
  deltaS, deltaAbsS: TAffineVector): single;
var
  // Friction and bounce
  TotalFriction, Bounce, f, m, restitution: single;
  ContactList: TgxIntegerList;
  // Temporary properties (Static or Dynamic)
  tFriction, tBounceFactor: single;
  TObject: TgxBaseSceneObject;
  // Collision results
  ColInfo: TgxDCECollision;
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
    for i := 0 to FStatics.Count - 1 do
      with TgxDCEStatic(FStatics[i]) do
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
              ECAddTerrain(MP, TgxTerrainRenderer(OwnerBaseSceneObject),
                FWorldScale * 2, Solid, i);
          end;
        end;

      end;

    // For each dynamic collider add a static ellipsoid
    for i := 0 to FDynamics.Count - 1 do
      with TgxDCEDynamic(FDynamics[i]) do
      begin
        CanCollide := False;
        if (Active) and (TgxDCEDynamic(FDynamics[i]) <> Body) then
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
  ContactList := TgxIntegerList.Create;

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
          tFriction := TgxDCEDynamic(FDynamics[abs(oi) - 1]).Friction;
          tBounceFactor := TgxDCEDynamic(FDynamics[abs(oi) - 1]).BounceFactor;
          TObject := TgxDCEDynamic(FDynamics[abs(oi) - 1]).OwnerBaseSceneObject;
        end
        else
        begin
          tFriction := TgxDCEStatic(FStatics[oi]).Friction;
          tBounceFactor := TgxDCEStatic(FStatics[oi]).BounceFactor;
          TObject := TgxDCEStatic(FStatics[oi]).OwnerBaseSceneObject;
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
        if (oi >= 0) and Assigned(TgxDCEStatic(FStatics[oi]).FOnCollision) then
          TgxDCEStatic(FStatics[oi]).FOnCollision(Self,
            Body.OwnerBaseSceneObject, ColInfo);
      end;
  finally
    ContactList.Free;
  end;
  result := TotalFriction;
end;

procedure TgxDCEManager.Step(deltaTime: Double);
var
  i: Integer;
begin
  if deltaTime > 0.1 then
    deltaTime := 0.1;
  for i := 0 to FDynamics.Count - 1 do
    with TgxDCEDynamic(FDynamics[i]) do
      if Active then
        DoMove(deltaTime);
end;

procedure TgxDCEManager.SetWorldDirection(const Value: TgxCoordinates);
begin
  FWorldDirection := Value;
  FWorldDirection.Normalize;
end;

procedure TgxDCEManager.SetWorldScale(const Value: single);
begin
  if Value = 0 then
    FWorldScale := 0.001
  else if Value < 0 then
    FWorldScale := abs(Value)
  else
    FWorldScale := Value;
end;

procedure TgxDCEManager.RegisterStatic(aClient: TgxDCEStatic);
begin
  if Assigned(aClient) then
    if FStatics.IndexOf(aClient) < 0 then
    begin
      FStatics.Add(aClient);
      aClient.FManager := Self;
    end;
end;

procedure TgxDCEManager.DeRegisterStatic(aClient: TgxDCEStatic);
begin
  if Assigned(aClient) then
  begin
    aClient.FManager := nil;
    FStatics.Remove(aClient);
  end;
end;

procedure TgxDCEManager.DeRegisterAllStatics;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to FStatics.Count - 1 do
    TgxDCEStatic(FStatics[i]).FManager := nil;
  FStatics.Clear;
end;

procedure TgxDCEManager.RegisterDynamic(aClient: TgxDCEDynamic);
begin
  if Assigned(aClient) then
    if FDynamics.IndexOf(aClient) < 0 then
    begin
      FDynamics.Add(aClient);
      aClient.FManager := Self;
    end;
end;

procedure TgxDCEManager.DeRegisterDynamic(aClient: TgxDCEDynamic);
begin
  if Assigned(aClient) then
  begin
    aClient.FManager := nil;
    FDynamics.Remove(aClient);
  end;
end;

procedure TgxDCEManager.DeRegisterAllDynamics;
var
  i: Integer;
begin
  // Fast deregistration
  for i := 0 to FDynamics.Count - 1 do
    TgxDCEDynamic(FDynamics[i]).FManager := nil;
  FDynamics.Clear;
end;

//---------------------------------------
// TgxDCEStatic
//---------------------------------------

procedure TgxDCEStatic.Assign(Source: TPersistent);
begin
  if Source is TgxDCEStatic then
  begin
    Active := TgxDCEStatic(Source).Active;
    Manager := TgxDCEStatic(Source).Manager;
    Shape := TgxDCEStatic(Source).Shape;
    Layer := TgxDCEStatic(Source).Layer;
    Solid := TgxDCEStatic(Source).Solid;
    Size.Assign(TgxDCEStatic(Source).Size);
    Friction := TgxDCEStatic(Source).Friction;
    BounceFactor := TgxDCEStatic(Source).BounceFactor;
  end;
  inherited Assign(Source);
end;

constructor TgxDCEStatic.Create(AOwner: TXCollection);
begin
  inherited Create(AOwner);
  FActive := True;
  FSize := TgxCoordinates.CreateInitialized(Self, XYZHmgVector, csVector);
  FShape := csEllipsoid;
  FSolid := True;
  FFriction := 1;
  FBounceFactor := 0;
end;

destructor TgxDCEStatic.Destroy;
begin
  Manager := nil;
  FSize.Free;
  inherited Destroy;
end;

class function TgxDCEStatic.FriendlyDescription: String;
begin
  result := 'Static Collision-detection registration';
end;

class function TgxDCEStatic.FriendlyName: String;
begin
  result := 'DCE Static Collider';
end;

procedure TgxDCEStatic.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TgxDCEManager, FManagerName);
    if Assigned(mng) then
      Manager := TgxDCEManager(mng);
    FManagerName := '';
  end;
end;

procedure TgxDCEStatic.WriteToFiler(writer: TWriter);
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

procedure TgxDCEStatic.ReadFromFiler(reader: TReader);
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
    FShape := TgxDCEShape(ReadInteger);
    FLayer := ReadInteger;
    FSolid := ReadBoolean;
    FActive := ReadBoolean;
    FFriction := ReadSingle;
    FBounceFactor := ReadSingle;
    FSize.ReadFromFiler(reader);
  end;
end;

procedure TgxDCEStatic.SetBounceFactor(const Value: single);
begin
  FBounceFactor := Value;
  if FBounceFactor < 0 then
    FBounceFactor := 0;
  if FBounceFactor > 1 then
    FBounceFactor := 1;
end;

procedure TgxDCEStatic.SetFriction(const Value: single);
begin
  FFriction := Value;
  if FFriction < 0 then
    FFriction := 0;
  if FFriction > 100 then
    FFriction := 100;
end;

procedure TgxDCEStatic.SetManager(const val: TgxDCEManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterStatic(Self);
    if Assigned(val) then
      val.RegisterStatic(Self);
  end;
end;

procedure TgxDCEStatic.SetShape(const Value: TgxDCEShape);
begin
  FShape := Value;
end;

procedure TgxDCEStatic.SetSize(const Value: TgxCoordinates);
begin
  FSize.Assign(Value);
  if FSize.x <= 0 then
    FSize.x := 0.1;
  if FSize.y <= 0 then
    FSize.y := 0.1;
  if FSize.z <= 0 then
    FSize.z := 0.1;
end;

//-------------------------------------
// TgxDCEDynamic
//-------------------------------------

procedure TgxDCEDynamic.ApplyAccel(NewAccel: TAffineVector);
begin
  AddVector(FAccel, NewAccel);
end;

procedure TgxDCEDynamic.ApplyAccel(x, y, z: single);
begin
  AddVector(FAccel, AffineVectorMake(x, y, z));
end;

procedure TgxDCEDynamic.ApplyAbsAccel(NewAccel: TAffineVector);
begin
  AddVector(FAbsAccel, NewAccel);
end;

procedure TgxDCEDynamic.ApplyAbsAccel(x, y, z: single);
begin
  AddVector(FAbsAccel, AffineVectorMake(x, y, z));
end;

procedure TgxDCEDynamic.StopAccel;
begin
  SetVector(FAccel, NullVector);
end;

procedure TgxDCEDynamic.StopAbsAccel;
begin
  SetVector(FAbsAccel, NullVector);
end;

procedure TgxDCEDynamic.Assign(Source: TPersistent);
begin
  if Source is TgxDCEDynamic then
  begin
    Manager := TgxDCEDynamic(Source).Manager;
    Active := TgxDCEDynamic(Source).Active;
    UseGravity := TgxDCEDynamic(Source).UseGravity;
    Layer := TgxDCEDynamic(Source).Layer;
    Solid := TgxDCEDynamic(Source).Solid;
    Size.Assign(TgxDCEDynamic(Source).Size);
    Friction := TgxDCEDynamic(Source).Friction;
    BounceFactor := TgxDCEDynamic(Source).BounceFactor;
    SlideOrBounce := TgxDCEDynamic(Source).SlideOrBounce;
    MaxRecursionDepth := TgxDCEDynamic(Source).MaxRecursionDepth;
  end;
  inherited Assign(Source);
end;

constructor TgxDCEDynamic.Create(AOwner: TXCollection);
begin
  inherited Create(AOwner);
  FActive := True;
  FUseGravity := True;
  FSize := TgxCoordinates.CreateInitialized(Self, XYZHmgVector, csVector);
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

destructor TgxDCEDynamic.Destroy;
begin
  Manager := nil;
  FSize.Free;
  inherited Destroy;
end;

procedure TgxDCEDynamic.DoMove(deltaTime: Double);
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
      G := FManager.Gravity * abs(1 - VectorDotProduct(FGroundNormal,
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
  // The absolute space must be only the gravity so it can calculate when it is in the ground
  deltaAbsS := FGravSpeed;

  ScaleVector(deltaS, deltaTime);
  ScaleVector(deltaAbsS, deltaTime);

  // Returns the friction of all collided objects
  FTotalFriction := FManager.MoveByDistance(Self, deltaS, deltaAbsS);
  FAccel := NullVector;
  FAbsAccel := NullVector;
end;

procedure TgxDCEDynamic.DoProgress(const progressTime: TgxProgressTimes);
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

class function TgxDCEDynamic.FriendlyDescription: String;
begin
  Result := 'Dynamic Collision-detection registration';
end;

class function TgxDCEDynamic.FriendlyName: String;
begin
  Result := 'DCE Dynamic Collider';
end;

procedure TgxDCEDynamic.Jump(jHeight, jSpeed: single);
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

procedure TgxDCEDynamic.Loaded;
var
  mng: TComponent;
begin
  inherited;
  if FManagerName <> '' then
  begin
    mng := FindManager(TgxDCEManager, FManagerName);
    if Assigned(mng) then
      Manager := TgxDCEManager(mng);
    FManagerName := '';
  end;
end;

procedure TgxDCEDynamic.Move(deltaS: TAffineVector; deltaTime: Double);
begin
  ScaleVector(deltaS, deltaTime);
  FManager.MoveByDistance(Self, NullVector, deltaS);
end;

procedure TgxDCEDynamic.MoveTo(Position: TAffineVector; Amount: single);
begin
  SubtractVector(Position,
    AffineVectorMake(OwnerBaseSceneObject.AbsolutePosition));
  Move(Position, Amount);
end;

procedure TgxDCEDynamic.WriteToFiler(writer: TWriter);
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

procedure TgxDCEDynamic.ReadFromFiler(reader: TReader);
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
    FSlideOrBounce := TgxDCESlideOrBounce(ReadInteger);
    FSize.ReadFromFiler(reader);
  end;
end;

procedure TgxDCEDynamic.SetBounceFactor(const Value: single);
begin
  FBounceFactor := Value;
  if FBounceFactor < 0 then
    FBounceFactor := 0;
  if FBounceFactor > 1 then
    FBounceFactor := 1;
end;

procedure TgxDCEDynamic.SetFriction(const Value: single);
begin
  FFriction := Value;
  if FFriction < 0 then
    FFriction := 0;
  if FFriction > 100 then
    FFriction := 100;
end;

procedure TgxDCEDynamic.SetManager(const val: TgxDCEManager);
begin
  if val <> FManager then
  begin
    if Assigned(FManager) then
      FManager.DeRegisterDynamic(Self);
    if Assigned(val) then
      val.RegisterDynamic(Self);
  end;
end;

procedure TgxDCEDynamic.SetSize(const Value: TgxCoordinates);
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

function GetOrCreateDCEStatic(behaviours: TgxBehaviours): TgxDCEStatic;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TgxDCEStatic);
  if i >= 0 then
    result := TgxDCEStatic(behaviours[i])
  else
    result := TgxDCEStatic.Create(behaviours);
end;

function GetOrCreateDCEStatic(obj: TgxBaseSceneObject): TgxDCEStatic;
begin
  result := GetOrCreateDCEStatic(obj.behaviours);
end;

function GetOrCreateDCEDynamic(behaviours: TgxBehaviours): TgxDCEDynamic;
var
  i: Integer;
begin
  i := behaviours.IndexOfClass(TgxDCEDynamic);
  if i >= 0 then
    result := TgxDCEDynamic(behaviours[i])
  else
    result := TgxDCEDynamic.Create(behaviours);
end;

function GetOrCreateDCEDynamic(obj: TgxBaseSceneObject): TgxDCEDynamic;
begin
  result := GetOrCreateDCEDynamic(obj.behaviours);
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------


RegisterXCollectionItemClass(TgxDCEStatic);
RegisterXCollectionItemClass(TgxDCEDynamic);

finalization

UnregisterXCollectionItemClass(TgxDCEStatic);
UnregisterXCollectionItemClass(TgxDCEDynamic);

end.
