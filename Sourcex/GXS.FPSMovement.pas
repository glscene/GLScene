//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.FPSMovement;

(* FPS-like movement behaviour and manager *)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  FMX.Graphics,

  GXS.XCollection,
  GXS.VectorTypes,
  GXS.Context,
  GXS.VectorGeometry,
  GXS.Scene,
  GXS.Coordinates,
  GXS.VectorFileObjects,
  GXS.VectorLists,
  GXS.GeomObjects,
  GXS.Navigator,
  GXS.RenderContextInfo,
  GXS.BaseClasses,
  GXS.Manager,
  GXS.State;

type
  TContactPoint = record
    intPoint, intNormal: TVector4f;
  end;

  TCollisionState = class
  public
    Position: TVector4f;
    Contact: TContactPoint;
    Time: Int64;
  end;

  TCollisionStates = class(TList)
  end;

  TgxBFPSMovement = class;

  TgxMapCollectionItem = class(TXCollectionItem)
  private
    FMap: TgxFreeForm;
    FMapName: string;
    FCollisionGroup: integer;

    procedure setMap(value: TgxFreeForm);
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    constructor Create(aOwner: TXCollection); override;
    class function FriendlyName: String; override;
  published

    property Map: TgxFreeForm read FMap write setMap;

    (* Indicates the collision group of this map. A Collision Group
      is a set of logical maps and movers that can collide between
      themselves (i.e. a Behaviour with group 1 can only collide with
      maps that are also on group 1). *)
    property CollisionGroup: integer read FCollisionGroup write FCollisionGroup;
  end;

  TgxMapCollectionItemClass = class of TgxMapCollectionItem;

  TgxMapCollection = class(TXCollection)
  public
    class function ItemsClass: TXCollectionItemClass; override;
    function addMap(Map: TgxFreeForm; CollisionGroup: integer = 0)
      : TgxMapCollectionItem;
    function findMap(mapFreeForm: TgxFreeForm): TgxMapCollectionItem;
  end;

  TgxFPSMovementManager = class(TComponent)
  private
    FNavigator: TgxNavigator;
    FDisplayTime: integer;
    FMovementScale: single;
    FMaps: TgxMapCollection;
    FScene: TgxScene;

    procedure SetNavigator(value: TgxNavigator);
    procedure setScene(value: TgxScene);
    procedure DrawArrows(intPoint, intNormal, Ray: TVector4f;
      Arrow1, Arrow2: TgxArrowLine);
  protected
    procedure Loaded; override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure WriteMaps(stream: TStream);
    procedure ReadMaps(stream: TStream);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    (* Basic idea is to OctreeSphereSweepIntersect to plane, update position then change
       velocity to slide along the plane
       Camera can collide with multiple planes (e.g. floor + multiple walls + ceiling)
       limit iterations to 4 or 5 for now, may need to be higher
      for more complex maps or fast motion *)
    function SphereSweepAndSlide(freeform: TgxFreeForm;
      behaviour: TgxBFPSMovement; SphereStart: TVector4f;
      var Velocity, newPosition: TVector4f; sphereRadius: single)
      : boolean; overload;

    procedure SphereSweepAndSlide(behaviour: TgxBFPSMovement;
      SphereStart: TVector4f; var Velocity, newPosition: TVector4f;
      sphereRadius: single); overload;

  published
    property Maps: TgxMapCollection read FMaps write FMaps;
    property Navigator: TgxNavigator read FNavigator write SetNavigator;
    property Scene: TgxScene read FScene write setScene;

    // Display Time for the arrow lines.
    property DisplayTime: integer read FDisplayTime write FDisplayTime;
    property MovementScale: single read FMovementScale write FMovementScale;
  end;

  TgxBFPSMovement = class(TgxBehaviour)
  private
    FManager: TgxFPSMovementManager;
    CollisionStates: TCollisionStates;
    ArrowLine1, ArrowLine2, ArrowLine3, ArrowLine4, ArrowLine5,
      ArrowLine6: TgxArrowLine;
    dirGl: TgxDirectOpenGL;
    tickCount: Int64;
    oldPosition: TVector4f;
    FGravityEnabled: boolean;
    FSphereRadius: single;
    FShowArrows: boolean;
    FCollisionGroup: integer;
    FManagerName: string;
    procedure setShowArrows(value: boolean);
    procedure RenderArrowLines(Sender: TObject; var rci: TgxRenderContextInfo);
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    Velocity: TVector4f;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TgxProgressTimes); override;
    class function FriendlyName: string; override;
    Procedure TurnHorizontal(Angle: single);
    Procedure TurnVertical(Angle: single);
    Procedure MoveForward(Distance: single);
    Procedure StrafeHorizontal(Distance: single);
    Procedure StrafeVertical(Distance: single);
    Procedure Straighten;
  published
    property Manager: TgxFPSMovementManager read FManager write FManager;
    (*
      Radius to execute the testing with. A value < 0 indicates to use
      the boundingSphereRadius of the object.
    *)
    property sphereRadius: single read FSphereRadius write FSphereRadius;
    // Show Arrows and trailing for debuging.
    property ShowArrows: boolean read FShowArrows write setShowArrows;
    (* Indicates the collision group of this behaviour. A Collision Group
      is a set of logical maps and movers that can collide between
      themselves (i.e. a Behaviour with group 1 can only collide with
      maps that are also on group 1) *)
    property CollisionGroup: integer read FCollisionGroup write FCollisionGroup;
    property GravityEnabled: boolean read FGravityEnabled write FGravityEnabled;
  end;

function GetFPSMovement(behaviours: TgxBehaviours): TgxBFPSMovement; overload;
function GetFPSMovement(obj: TgxBaseSceneObject): TgxBFPSMovement; overload;
function GetOrCreateFPSMovement(behaviours: TgxBehaviours): TgxBFPSMovement; overload;
function GetOrCreateFPSMovement(obj: TgxBaseSceneObject): TgxBFPSMovement; overload;

//-------------------------------------------------------------------------
implementation
//-------------------------------------------------------------------------

function GetFPSMovement(behaviours: TgxBehaviours): TgxBFPSMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TgxBFPSMovement);
  if i >= 0 then
    Result := TgxBFPSMovement(behaviours[i])
  else
    Result := nil;
end;

function GetFPSMovement(obj: TgxBaseSceneObject): TgxBFPSMovement; overload;
begin
  Result := GetFPSMovement(obj.behaviours);
end;

function GetOrCreateFPSMovement(behaviours: TgxBehaviours)
  : TgxBFPSMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TgxBFPSMovement);
  if i >= 0 then
    Result := TgxBFPSMovement(behaviours[i])
  else
    Result := TgxBFPSMovement.Create(behaviours);
end;

function GetOrCreateFPSMovement(obj: TgxBaseSceneObject)
  : TgxBFPSMovement; overload;
begin
  Result := GetOrCreateFPSMovement(obj.behaviours);
end;

// ------------------
// ------------------ TgxMapCollectionItem ------------------
// ------------------
constructor TgxMapCollectionItem.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);

  FCollisionGroup := 0;
end;

procedure TgxMapCollectionItem.setMap(value: TgxFreeForm);
begin
  assert(owner.owner.InheritsFrom(TgxFPSMovementManager));
  if assigned(FMap) then
    FMap.RemoveFreeNotification(TComponent(owner.owner));
  FMap := value;
  if assigned(FMap) then
    FMap.FreeNotification(TComponent(owner.owner));
end;

procedure TgxMapCollectionItem.WriteToFiler(writer: TWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    writeInteger(0); // ArchiveVersion
    writeInteger(FCollisionGroup);
    if assigned(FMap) then
      WriteString(FMap.Name)
    else
      WriteString('');
  end;
end;

procedure TgxMapCollectionItem.ReadFromFiler(reader: TReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  with reader do
  begin
    archiveVersion := readInteger;
    assert(archiveVersion = 0, 'Wrong ArchiveVersion for TgxMapCollectionItem');
    FCollisionGroup := readInteger;
    FMapName := ReadString;
  end;
end;

procedure TgxMapCollectionItem.Loaded;
begin
  if FMapName <> '' then
  begin
    assert(owner.owner.InheritsFrom(TgxFPSMovementManager));
    Map := TgxFreeForm(TgxFPSMovementManager(owner.owner)
      .Scene.FindSceneObject(FMapName));
  end;
end;

class function TgxMapCollectionItem.FriendlyName: String;
begin
  Result := 'FPSMovementMap';
end;

// ------------------
// ------------------ TgxMapCollection ------------------
// ------------------
class function TgxMapCollection.ItemsClass: TXCollectionItemClass;
begin
  Result := TgxMapCollectionItem;
end;

function TgxMapCollection.addMap(Map: TgxFreeForm; CollisionGroup: integer = 0)
  : TgxMapCollectionItem;
begin
  // no repeated maps (would only present delays...)
  Result := findMap(Map);
  if assigned(Result) then
    exit;

  Result := TgxMapCollectionItem.Create(self);
  Result.Map := Map;
  Result.CollisionGroup := CollisionGroup;
  add(Result);
end;

function TgxMapCollection.findMap(mapFreeForm: TgxFreeForm)
  : TgxMapCollectionItem;
var
  i: integer;
  aux: TgxMapCollectionItem;
begin
  Result := nil;
  for i := 0 to count - 1 do
  begin
    aux := TgxMapCollectionItem(Items[i]);
    if aux.Map = mapFreeForm then
    begin
      Result := aux;
      break;
    end;
  end;
end;

// ------------------
// ------------------ TgxFPSMovementManager ------------------
// ------------------
constructor TgxFPSMovementManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  Maps := TgxMapCollection.Create(self);

  MovementScale := 4.0;
  DisplayTime := 2000;

  RegisterManager(self);
end;

destructor TgxFPSMovementManager.Destroy;
begin
  DeRegisterManager(self);
  Maps.Free;
  inherited Destroy;
end;

procedure TgxFPSMovementManager.Loaded;
begin
  inherited Loaded;
  if assigned(FMaps) then
    Maps.Loaded;
end;

// DefineProperties
//
procedure TgxFPSMovementManager.DefineProperties(Filer: TFiler);
begin
  inherited;
  { FOriginalFiler := Filer; }

  Filer.DefineBinaryProperty('MapsData', ReadMaps, WriteMaps,
    (assigned(FMaps) and (FMaps.count > 0)));
  { FOriginalFiler:=nil; }
end;

// WriteBehaviours
//
procedure TgxFPSMovementManager.WriteMaps(stream: TStream);
var
  writer: TWriter;
begin
  writer := TWriter.Create(stream, 16384);
  try
    Maps.WriteToFiler(writer);
  finally
    writer.Free;
  end;
end;

procedure TgxFPSMovementManager.ReadMaps(stream: TStream);
var
  reader: TReader;
begin
  reader := TReader.Create(stream, 16384);
  try
    Maps.ReadFromFiler(reader);
  finally
    reader.Free;
  end;
end;

procedure TgxFPSMovementManager.SetNavigator(value: TgxNavigator);
begin
  if assigned(FNavigator) then
    FNavigator.RemoveFreeNotification(self);
  FNavigator := value;
  if assigned(value) then
    value.FreeNotification(self);
end;

procedure TgxFPSMovementManager.setScene(value: TgxScene);
begin
  if assigned(FScene) then
    FScene.RemoveFreeNotification(self);
  FScene := value;
  if assigned(FScene) then
    FScene.FreeNotification(self);
end;

procedure TgxFPSMovementManager.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Map: TgxMapCollectionItem;
begin
  inherited Notification(AComponent, Operation);

  if Operation <> opRemove then
    exit;

  if (AComponent = FNavigator) then
    Navigator := nil;
  if (AComponent = FScene) then
    FScene := nil;
  if AComponent.InheritsFrom(TgxFreeForm) then
  begin
    Map := Maps.findMap(TgxFreeForm(AComponent));
    if assigned(Map) then
      Map.Map := nil;
  end;
end;

procedure TgxFPSMovementManager.DrawArrows(intPoint, intNormal, Ray: TVector4f;
  Arrow1, Arrow2: TgxArrowLine);
begin
  Arrow1.Position.AsVector := intPoint;
  Arrow1.Direction.AsVector := intNormal;
  Arrow1.Scale.z := VectorLength(intNormal);
  Arrow1.Move(Arrow1.Scale.z / 2);
  Arrow1.Visible := True;

  Arrow2.Position.AsVector := intPoint;
  Arrow2.Direction.AsVector := Ray;
  Arrow2.Visible := True;
end;

procedure TgxFPSMovementManager.SphereSweepAndSlide(behaviour: TgxBFPSMovement;
  SphereStart: TVector4f; var Velocity, newPosition: TVector4f;
  sphereRadius: single);
var
  i: integer;
  Map: TgxMapCollectionItem;
begin
  for i := 0 to Maps.count - 1 do
  begin
    Map := TgxMapCollectionItem(Maps.GetItems(i));
    if Map.CollisionGroup = behaviour.CollisionGroup then
      SphereSweepAndSlide(Map.Map, behaviour, SphereStart, Velocity,
        newPosition, sphereRadius)
  end;
end;

function TgxFPSMovementManager.SphereSweepAndSlide(freeform: TgxFreeForm;
  behaviour: TgxBFPSMovement; SphereStart: TVector4f;
  var Velocity, newPosition: TVector4f; sphereRadius: single): boolean;
var
  oldPosition, Ray: TVector4f;
  vel, slidedistance: single;
  intPoint, intNormal: TVector4f;
  newDirection, newRay, collisionPosition, pointOnSphere,
    point2OnSphere: TVector4f;
  i: integer;
  CollisionState: TCollisionState;
  SphereRadiusRel: single; // mrqzzz
begin
  SphereRadiusRel := sphereRadius / freeform.Scale.x;
  // could be Scale.y, or Scale.z assuming they are the same

  oldPosition := SphereStart;

  Result := True;

  // Direction sphere is moving in
  Ray := VectorSubtract(newPosition, oldPosition);
  // ray:=Velocity;
  // newPosition:=VectorAdd(newPosition,ray);
  // Speed of sphere
  vel := VectorLength(Ray);

  // if the Sphere is not moving, nothing is required
  // else do up to 7 loops

  if vel > 0 then
    for i := 0 to 6 do
    begin
      // if an intersection occurs, will need to do further calculations
      if (freeform.OctreeSphereSweepIntersect(oldPosition, Ray, vel,
        SphereRadiusRel, @intPoint, @intNormal)) then
      begin
        if VectorDistance2(oldPosition, intPoint) <= sqr(sphereRadius) then
        begin
          // sphere is intersecting triangle
          intNormal := VectorScale(VectorSubtract(oldPosition,
            intPoint), 1.0001);
        end
        else
        begin
          // sphere is not intersecting triangle
          // intNormal:=VectorSubtract(oldPosition,intPoint);  //not correct but works okay at small time steps
          // intNormal:=VectorScale(VectorNormalize(intNormal),SphereRadius+0.0001);
          if RayCastSphereInterSect(intPoint, VectorNormalize(VectorNegate(Ray)
            ), oldPosition, sphereRadius, pointOnSphere, point2OnSphere) > 0
          then
            intNormal := VectorScale(VectorSubtract(oldPosition,
              pointOnSphere), 1.0001)
            // intNormal:=VectorScale(VectorNormalize(VectorSubtract(oldPosition,PointOnSphere)),SphereRadius+0.001)//VectorDistance(oldPosition,PointOnSphere));
          else
          begin
            // Assert(False);  //this shouldn't happen (this is here for debugging)
            intNormal := VectorScale(VectorSubtract(oldPosition,
              intPoint), 1.0001);
          end;

        end;

        // calculate position of centre of sphere when collision occurs
        collisionPosition := VectorAdd(intPoint, intNormal);
        oldPosition := collisionPosition;

        // calculate distance that wasn't travelled, due to obstacle
        newRay := VectorSubtract(newPosition, collisionPosition);

        // calculate new direction when a wall is hit (could add bouncing to this)
        newDirection := VectorCrossProduct(intNormal,
          VectorCrossProduct(newRay, intNormal));
        if VectorNorm(newDirection) > 0 then
          NormalizeVector(newDirection);

        // calculate distance that it should slide (depends on angle between plane & ray)
        slidedistance := vectorDotProduct(newRay, newDirection);
        // still need to implement friction properly
        // if abs(SlideDistance)<10*deltaTime then SlideDistance:=0;
        ScaleVector(newDirection, slidedistance);

        // calculate new position sphere is heading towards
        newPosition := VectorAdd(collisionPosition, newDirection);
        Ray := newDirection;
        vel := VectorLength(Ray);

        // display arrows for collision normals & slide direction
        if (i = 0) and (behaviour.ShowArrows) then
          DrawArrows(intPoint, intNormal, Ray, behaviour.ArrowLine1,
            behaviour.ArrowLine4)
        else if (i = 1) and (behaviour.ShowArrows) then
          DrawArrows(intPoint, intNormal, Ray, behaviour.ArrowLine2,
            behaviour.ArrowLine5)
        else if (i = 2) and (behaviour.ShowArrows) then
          DrawArrows(intPoint, intNormal, Ray, behaviour.ArrowLine3,
            behaviour.ArrowLine6)
        else if i = 6 then
        begin
          // caption:=FloatToStr(vectordistance(newPosition,oldPosition));
          newPosition := oldPosition;
          break;
        end;

        // check if very small motion (e.g. when stuck in a corner)
        if vel < 1E-10 then // deltaTime then
        begin
          newPosition := oldPosition;
          break;
        end;

        CollisionState := TCollisionState.Create();
        CollisionState.Position := oldPosition;
        CollisionState.Contact.intNormal := intNormal;
        CollisionState.Contact.intPoint := intPoint;
        CollisionState.Time := TThread.GetTickCount();

        behaviour.CollisionStates.add(CollisionState);

      end
      else // no collision occured, so quit loop
      begin
        if i = 0 then
          Result := false;
        break;
      end;
    end; // end i loop
  Velocity := Ray;
end;


// ------------------
// ------------------ TgxBFPSMovement ------------------
// ------------------

constructor TgxBFPSMovement.Create(aOwner: TXCollection);

  procedure setupArrow(arrow: TgxArrowLine; color: TColor); 
  begin
    with arrow do
    begin
      slices := 16;
      stacks := 4;
      TopArrowHeadHeight := 0.1;
      TopArrowHeadRadius := 0.04;
      TopRadius := 0.02;
      BottomArrowHeadHeight := 0.05;
      BottomArrowHeadRadius := 0.02;
      BottomRadius := 0.02;
      Material.FrontProperties.Diffuse.AsWinColor := color;
    end;
  end;

begin
  inherited Create(aOwner);

  Velocity := NullHmgVector;
  sphereRadius := -1;
  CollisionGroup := 0;

  CollisionStates := TCollisionStates.Create;

  // FIXME: Creating arrows here, but they should be only added when
  // a "showArrows" property changed
  ArrowLine1 := TgxArrowLine.Create(nil);
  setupArrow(ArrowLine1, TColors.Red);
  ArrowLine2 := TgxArrowLine.Create(nil);
  setupArrow(ArrowLine2, TColors.Green);
  ArrowLine3 := TgxArrowLine.Create(nil);
  setupArrow(ArrowLine3, TColors.Blue);
  ArrowLine4 := TgxArrowLine.Create(nil);
  setupArrow(ArrowLine4, TColors.Silver);
  ArrowLine5 := TgxArrowLine.Create(nil);
  setupArrow(ArrowLine5, TColors.Silver);
  ArrowLine6 := TgxArrowLine.Create(nil);
  setupArrow(ArrowLine6, TColors.Silver);

  dirGl := TgxDirectOpenGL.Create(nil);
  dirGl.OnRender := RenderArrowLines;

  oldPosition := OwnerBaseSceneObject.Position.AsVector;
  FManagerName := '';
end;

destructor TgxBFPSMovement.Destroy;
var
  i: integer;
begin
  // remove all states
  for i := 0 to CollisionStates.count - 1 do
    TCollisionState(CollisionStates[i]).Free;
  FreeAndNil(CollisionStates);
  // remove all objects used to display graphical results of collisions
  FreeAndNil(ArrowLine1);
  FreeAndNil(ArrowLine2);
  FreeAndNil(ArrowLine3);
  FreeAndNil(ArrowLine4);
  FreeAndNil(ArrowLine5);
  FreeAndNil(ArrowLine6);
  FreeAndNil(dirGl);
  inherited Destroy;
end;

class function TgxBFPSMovement.FriendlyName: String;
begin
  Result := 'FPS Movement';
end;

procedure TgxBFPSMovement.WriteToFiler(writer: TWriter);
begin
  inherited WriteToFiler(writer);
  with writer do
  begin
    writeInteger(0); // ArchiveVersion 0 (initial)
    writeInteger(FCollisionGroup);
    WriteSingle(FSphereRadius);
    WriteBoolean(FGravityEnabled);
    WriteBoolean(FShowArrows);
    if assigned(FManager) then
      WriteString(FManager.GetNamePath)
    else
      WriteString('');
  end;
end;

procedure TgxBFPSMovement.ReadFromFiler(reader: TReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  with reader do
  begin
    archiveVersion := readInteger;
    assert(archiveVersion = 0, 'Wrong ArchiveVersion for TgxBFPSMovement');
    CollisionGroup := readInteger;
    sphereRadius := ReadSingle;
    GravityEnabled := ReadBoolean;
    ShowArrows := ReadBoolean;
    FManagerName := ReadString;
  end;
end;

procedure TgxBFPSMovement.Loaded;
var
  mng: TComponent;
begin
  inherited Loaded;
  if FManagerName <> '' then
  begin
    mng := FindManager(TgxFPSMovementManager, FManagerName);
    if assigned(mng) then
      Manager := TgxFPSMovementManager(mng);
    FManagerName := '';
  end;
end;

procedure TgxBFPSMovement.setShowArrows(value: boolean);
begin
  FShowArrows := value;
  dirGl.Visible := value;
  if (OwnerBaseSceneObject <> nil) and
    not(csDesigning in OwnerBaseSceneObject.ComponentState) then
  begin
    ArrowLine1.MoveTo(OwnerBaseSceneObject.Parent);
    ArrowLine2.MoveTo(OwnerBaseSceneObject.Parent);
    ArrowLine3.MoveTo(OwnerBaseSceneObject.Parent);
    ArrowLine4.MoveTo(OwnerBaseSceneObject.Parent);
    ArrowLine5.MoveTo(OwnerBaseSceneObject.Parent);
    ArrowLine6.MoveTo(OwnerBaseSceneObject.Parent);
    dirGl.MoveTo(OwnerBaseSceneObject.Parent);
  end;
end;

procedure TgxBFPSMovement.MoveForward(Distance: single);
var
  prevObj: TgxBaseSceneObject;
begin
  Assert(assigned(Manager),
    'Manager not assigned on TgxBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.MoveForward(Distance);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TgxBFPSMovement.StrafeHorizontal(Distance: single);
var
  prevObj: TgxBaseSceneObject;
begin
  Assert(assigned(Manager),
    'Manager not assigned on TgxBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.StrafeHorizontal(Distance);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TgxBFPSMovement.StrafeVertical(Distance: single);
var
  prevObj: TgxBaseSceneObject;
begin
  Assert(assigned(Manager),
    'Manager not assigned on TgxBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.StrafeVertical(Distance);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TgxBFPSMovement.TurnHorizontal(Angle: single);
var
  prevObj: TgxBaseSceneObject;
begin
  Assert(assigned(Manager),
    'Manager not assigned on TgxBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.TurnHorizontal(Angle);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TgxBFPSMovement.TurnVertical(Angle: single);
var
  prevObj: TgxBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TgxBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.TurnVertical(Angle);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TgxBFPSMovement.Straighten;
var
  prevObj: TgxBaseSceneObject;
begin
  Assert(assigned(Manager),
    'Manager not assigned on TgxBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.Straighten;
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TgxBFPSMovement.DoProgress(const progressTime: TgxProgressTimes);
var
  newPosition: TVector4f;
  CollisionState: TCollisionState;
begin
  inherited DoProgress(progressTime);

  Assert(assigned(Manager), 'FPS Manager not assigned to behaviour.');

  // make arrowlines invisible (they are made visible in SphereSweepAndSlide)
  ArrowLine1.Visible := false;
  ArrowLine2.Visible := false;
  ArrowLine3.Visible := false;
  ArrowLine4.Visible := false;
  ArrowLine5.Visible := false;
  ArrowLine6.Visible := false;

  CollisionState := TCollisionState.Create();
  CollisionState.Position := oldPosition;
  CollisionStates.add(CollisionState);

  // this is the position we are trying to move to with controls
  newPosition := OwnerBaseSceneObject.Position.AsVector;

  // Change in position = velocity * time taken
  if GravityEnabled then
    newPosition.Y := newPosition.Y - Manager.MovementScale * 0.5 *
      progressTime.deltaTime;

  // do some magic!!!  and store new position in newPosition
  if sphereRadius < 0 then
    Manager.SphereSweepAndSlide(self, oldPosition, Velocity, newPosition,
      OwnerBaseSceneObject.boundingSphereRadius)
  else
    Manager.SphereSweepAndSlide(self, oldPosition, Velocity, newPosition,
      sphereRadius);

  OwnerBaseSceneObject.Position.AsVector := newPosition;
  oldPosition := newPosition;

  if CollisionStates.count > 0 then
  begin
    CollisionState := TCollisionState(CollisionStates.First);
    TickCount := TThread.GetTickCount();
    // remove all old states
    while (CollisionState <> nil) and
      (CollisionState.Time < tickCount - Manager.DisplayTime) do
    begin
      CollisionStates.Remove(CollisionState);
      CollisionState.Free;
      if CollisionStates.count = 0 then
        exit;
      CollisionState := TCollisionState(CollisionStates.First);
    end;
  end;
end;

procedure TgxBFPSMovement.RenderArrowLines(Sender: TObject;
  var rci: TgxRenderContextInfo);
var
  x, y, z, t: single;
  i: integer;
  CollisionState: TCollisionState;
begin
  // caption:= IntToStr(CollisionStates.Count);
  glColor3f(1, 1, 1);
  rci.gxStates.Disable(stLighting);
  // draw position trail
  glBegin(GL_LINE_STRIP);
  for i := 0 to CollisionStates.count - 1 do
  begin
    CollisionState := TCollisionState(CollisionStates.Items[i]);
    x := CollisionState.Position.X;
    y := CollisionState.Position.Y;
    z := CollisionState.Position.Z;
    glVertex3f(x, y, z);
  end;
  glEnd();
  // draw normals trail
  glBegin(GL_LINES);
  for i := 0 to CollisionStates.count - 1 do
  begin
    CollisionState := TCollisionState(CollisionStates.Items[i]);
    t := (Manager.DisplayTime - (tickCount - CollisionState.Time)) /
      Manager.DisplayTime;
    glColor3f(t, t, t);
    glVertex3f(CollisionState.Contact.intPoint.X,
      CollisionState.Contact.intPoint.Y, CollisionState.Contact.intPoint.Z);
    glVertex3f(CollisionState.Contact.intPoint.X +
      CollisionState.Contact.intNormal.X, //VKSphere4.Radius,
      CollisionState.Contact.intPoint.Y + CollisionState.Contact.intNormal.Y,
      //VKSphere4.Radius,
      CollisionState.Contact.intPoint.Z + CollisionState.Contact.intNormal.Z);
    //VKSphere4.Radius);
  end;
  glEnd();
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------


RegisterXCollectionItemClass(TgxMapCollectionItem);
RegisterXCollectionItemClass(TgxBFPSMovement);

finalization

UnregisterXCollectionItemClass(TgxMapCollectionItem);
UnregisterXCollectionItemClass(TgxBFPSMovement);

end.
