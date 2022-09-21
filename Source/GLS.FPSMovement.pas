//
// The graphics rendering engine GLScene http://glscene.org
//
unit GLS.FPSMovement;

(* FPS-like movement behaviour and manager. *)

interface

{$I GLScene.inc}

uses
  Winapi.OpenGL,
  Winapi.Windows,
  System.Classes,
  System.Types,
  System.SysUtils,
  Vcl.Graphics,

  GLS.OpenGLTokens,
  GLS.Coordinates,
  GLS.VectorTypes,
  GLS.Context,
  GLS.VectorGeometry,
  GLS.Scene,
  GLS.VectorFileObjects,
  GLS.VectorLists,
  GLS.XCollection,
  GLS.GeomObjects,
  GLS.Navigator,
  GLS.RenderContextInfo,
  GLS.BaseClasses,
  GLS.Manager,
  GLS.State;

type
  TGLContactPoint = record
    intPoint, intNormal: TGLVector;
  end;

  TGLCollisionState = class
  public
    Position: TGLVector;
    Contact: TGLContactPoint;
    Time: Int64;
  end;

  TGLCollisionStates = class(TList)
  end;

  TGLBFPSMovement = class;

  TGLMapCollectionItem = class(TXCollectionItem)
  private
    FMap: TGLFreeForm;
    FMapName: string;
    FCollisionGroup: integer;
     procedure setMap(value: TGLFreeForm);
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    constructor Create(aOwner: TXCollection); override;
    class function FriendlyName: String; override;
  published
     property Map: TGLFreeForm read FMap write setMap;
     (* Indicates the collision group of this map. A Collision Group
      is a set of logical maps and movers that can collide between
      themselves (i.e. a Behaviour with group 1 can only collide with
      maps that are also on group 1). *)
    property CollisionGroup: integer read FCollisionGroup write FCollisionGroup;
  end;

  TGLMapCollectionItemClass = class of TGLMapCollectionItem;

  TGLMapCollection = class(TXCollection)
  public
    class function ItemsClass: TXCollectionItemClass; override;
    function addMap(Map: TGLFreeForm; CollisionGroup: integer = 0)
      : TGLMapCollectionItem;
    function findMap(mapFreeForm: TGLFreeForm): TGLMapCollectionItem;
  end;

  TGLFPSMovementManager = class(TComponent)
  private
    FNavigator: TGLNavigator;
    FDisplayTime: integer;
    FMovementScale: single;
    FMaps: TGLMapCollection;
    FScene: TGLScene;
    procedure SetNavigator(value: TGLNavigator);
    procedure setScene(value: TGLScene);
    procedure DrawArrows(intPoint, intNormal, Ray: TGLVector;
      Arrow1, Arrow2: TGLArrowLine);
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
       velocity to slide along the plane. Camera can collide with multiple planes
      (e.g. floor + multiple walls + ceiling) limit iterations to 4 or 5 for now,
      may need to be higher for more complex maps or fast motion *)
    function SphereSweepAndSlide(freeform: TGLFreeForm;
      behaviour: TGLBFPSMovement; SphereStart: TGLVector;
      var Velocity, newPosition: TGLVector; sphereRadius: single): boolean; overload;
    procedure SphereSweepAndSlide(behaviour: TGLBFPSMovement;
      SphereStart: TGLVector; var Velocity, newPosition: TGLVector;
      sphereRadius: single); overload;
  published
    property Maps: TGLMapCollection read FMaps write FMaps;
    property Navigator: TGLNavigator read FNavigator write SetNavigator;
    property Scene: TGLScene read FScene write setScene;
    // Display Time for the arrow lines.
    property DisplayTime: integer read FDisplayTime write FDisplayTime;
    property MovementScale: single read FMovementScale write FMovementScale;
  end;

  TGLBFPSMovement = class(TGLBehaviour)
  private
    FManager: TGLFPSMovementManager;
    CollisionStates: TGLCollisionStates;
    ArrowLine1, ArrowLine2, ArrowLine3, ArrowLine4, ArrowLine5,
      ArrowLine6: TGLArrowLine;
    dirGl: TGLDirectOpenGL;
    tickCount: Int64;
    oldPosition: TGLVector;
    FGravityEnabled: boolean;
    FSphereRadius: single;
    FShowArrows: boolean;
    FCollisionGroup: integer;
    FManagerName: string;
    procedure setShowArrows(value: boolean);
    procedure RenderArrowLines(Sender: TObject; var rci: TGLRenderContextInfo);
  protected
    procedure WriteToFiler(writer: TWriter); override;
    procedure ReadFromFiler(reader: TReader); override;
    procedure Loaded; override;
  public
    Velocity: TGLVector;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TGLProgressTimes); override;
    class function FriendlyName: string; override;
    procedure TurnHorizontal(Angle: single);
    procedure TurnVertical(Angle: single);
    procedure MoveForward(Distance: single);
    procedure StrafeHorizontal(Distance: single);
    procedure StrafeVertical(Distance: single);
    procedure Straighten;
  published
    property Manager: TGLFPSMovementManager read FManager write FManager;
    (*  Radius to execute the testing with. A value < 0 indicates to use
      the boundingSphereRadius of the object. *)
    property sphereRadius: single read FSphereRadius write FSphereRadius;
    (*  Show Arrows and trailing for debuging. *)
    property ShowArrows: boolean read FShowArrows write setShowArrows;
    (*  Indicates the collision group of this behaviour. A Collision Group
      is a set of logical maps and movers that can collide between
      themselves (i.e. a Behaviour with group 1 can only collide with
      maps that are also on group 1) *)
    property CollisionGroup: integer read FCollisionGroup write FCollisionGroup;
    property GravityEnabled: boolean read FGravityEnabled write FGravityEnabled;
  end;

function GetFPSMovement(behaviours: TGLBehaviours): TGLBFPSMovement; overload;
function GetFPSMovement(obj: TGLBaseSceneObject): TGLBFPSMovement; overload;
function GetOrCreateFPSMovement(behaviours: TGLBehaviours): TGLBFPSMovement; overload;
function GetOrCreateFPSMovement(obj: TGLBaseSceneObject): TGLBFPSMovement; overload;

//=======================================================================
implementation
//=======================================================================

function GetFPSMovement(behaviours: TGLBehaviours): TGLBFPSMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLBFPSMovement);
  if i >= 0 then
    Result := TGLBFPSMovement(behaviours[i])
  else
    Result := nil;
end;

function GetFPSMovement(obj: TGLBaseSceneObject): TGLBFPSMovement; overload;
begin
  Result := GetFPSMovement(obj.behaviours);
end;

function GetOrCreateFPSMovement(behaviours: TGLBehaviours)
  : TGLBFPSMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLBFPSMovement);
  if i >= 0 then
    Result := TGLBFPSMovement(behaviours[i])
  else
    Result := TGLBFPSMovement.Create(behaviours);
end;

function GetOrCreateFPSMovement(obj: TGLBaseSceneObject)
  : TGLBFPSMovement; overload;
begin
  Result := GetOrCreateFPSMovement(obj.behaviours);
end;

// ------------------
// ------------------ TGLMapCollectionItem ------------------
// ------------------
constructor TGLMapCollectionItem.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);

  FCollisionGroup := 0;
end;

procedure TGLMapCollectionItem.setMap(value: TGLFreeForm);
begin
  assert(owner.owner.InheritsFrom(TGLFPSMovementManager));
  if assigned(FMap) then
    FMap.RemoveFreeNotification(TComponent(owner.owner));
  FMap := value;
  if assigned(FMap) then
    FMap.FreeNotification(TComponent(owner.owner));
end;

procedure TGLMapCollectionItem.WriteToFiler(writer: TWriter);
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

procedure TGLMapCollectionItem.ReadFromFiler(reader: TReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  with reader do
  begin
    archiveVersion := readInteger;
    assert(archiveVersion = 0, 'Wrong ArchiveVersion for TGLMapCollectionItem');
    FCollisionGroup := readInteger;
    FMapName := ReadString;
  end;
end;

procedure TGLMapCollectionItem.Loaded;
begin
  if FMapName <> '' then
  begin
    assert(owner.owner.InheritsFrom(TGLFPSMovementManager));
    Map := TGLFreeForm(TGLFPSMovementManager(owner.owner)
      .Scene.FindSceneObject(FMapName));
  end;
end;

class function TGLMapCollectionItem.FriendlyName: String;
begin
  Result := 'FPSMovementMap';
end;

// ------------------
// ------------------ TGLMapCollection ------------------
// ------------------
class function TGLMapCollection.ItemsClass: TXCollectionItemClass;
begin
  Result := TGLMapCollectionItem;
end;

function TGLMapCollection.addMap(Map: TGLFreeForm; CollisionGroup: integer = 0)
  : TGLMapCollectionItem;
begin
  // no repeated maps (would only present delays...)
  Result := findMap(Map);
  if assigned(Result) then
    exit;

  Result := TGLMapCollectionItem.Create(self);
  Result.Map := Map;
  Result.CollisionGroup := CollisionGroup;
  add(Result);
end;

function TGLMapCollection.findMap(mapFreeForm: TGLFreeForm)
  : TGLMapCollectionItem;
var
  i: integer;
  aux: TGLMapCollectionItem;
begin
  Result := nil;
  for i := 0 to count - 1 do
  begin
    aux := TGLMapCollectionItem(Items[i]);
    if aux.Map = mapFreeForm then
    begin
      Result := aux;
      break;
    end;
  end;
end;

// ------------------
// ------------------ TGLFPSMovementManager ------------------
// ------------------
constructor TGLFPSMovementManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  Maps := TGLMapCollection.Create(self);

  MovementScale := 4.0;
  DisplayTime := 2000;

  RegisterManager(self);
end;

destructor TGLFPSMovementManager.Destroy;
begin
  DeRegisterManager(self);
  Maps.Free;
  inherited Destroy;
end;

procedure TGLFPSMovementManager.Loaded;
begin
  inherited Loaded;
  if assigned(FMaps) then
    Maps.Loaded;
end;

procedure TGLFPSMovementManager.DefineProperties(Filer: TFiler);
begin
  inherited;
  // FOriginalFiler := Filer;

  Filer.DefineBinaryProperty('MapsData', ReadMaps, WriteMaps,
    (assigned(FMaps) and (FMaps.count > 0)));
  // FOriginalFiler:=nil;
end;

procedure TGLFPSMovementManager.WriteMaps(stream: TStream);
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

procedure TGLFPSMovementManager.ReadMaps(stream: TStream);
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

procedure TGLFPSMovementManager.SetNavigator(value: TGLNavigator);
begin
  if assigned(FNavigator) then
    FNavigator.RemoveFreeNotification(self);
  FNavigator := value;
  if assigned(value) then
    value.FreeNotification(self);
end;

procedure TGLFPSMovementManager.setScene(value: TGLScene);
begin
  if assigned(FScene) then
    FScene.RemoveFreeNotification(self);
  FScene := value;
  if assigned(FScene) then
    FScene.FreeNotification(self);
end;

procedure TGLFPSMovementManager.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  Map: TGLMapCollectionItem;
begin
  inherited Notification(AComponent, Operation);

  if Operation <> opRemove then
    exit;

  if (AComponent = FNavigator) then
    Navigator := nil;
  if (AComponent = FScene) then
    FScene := nil;
  if AComponent.InheritsFrom(TGLFreeForm) then
  begin
    Map := Maps.findMap(TGLFreeForm(AComponent));
    if assigned(Map) then
      Map.Map := nil;
  end;
end;

procedure TGLFPSMovementManager.DrawArrows(intPoint, intNormal, Ray: TGLVector;
  Arrow1, Arrow2: TGLArrowLine);
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

procedure TGLFPSMovementManager.SphereSweepAndSlide(behaviour: TGLBFPSMovement;
  SphereStart: TGLVector; var Velocity, newPosition: TGLVector;
  sphereRadius: single);
var
  i: integer;
  Map: TGLMapCollectionItem;
begin
  for i := 0 to Maps.count - 1 do
  begin
    Map := TGLMapCollectionItem(Maps.GetItems(i));
    if Map.CollisionGroup = behaviour.CollisionGroup then
      SphereSweepAndSlide(Map.Map, behaviour, SphereStart, Velocity,
        newPosition, sphereRadius)
  end;
end;

function TGLFPSMovementManager.SphereSweepAndSlide(freeform: TGLFreeForm;
  behaviour: TGLBFPSMovement; SphereStart: TGLVector;
  var Velocity, newPosition: TGLVector; sphereRadius: single): boolean;
var
  oldPosition, Ray: TGLVector;
  vel, slidedistance: single;
  intPoint, intNormal: TGLVector;
  newDirection, newRay, collisionPosition, pointOnSphere,
    point2OnSphere: TGLVector;
  i: integer;
  CollisionState: TGLCollisionState;
  SphereRadiusRel: single; // mrqzzz
begin
  SphereRadiusRel := sphereRadius / freeform.Scale.x;
  // could be Scale.y, or Scale.z assuming they are the same

  oldPosition := SphereStart;

  Result := True;

  // Direction sphere is moving in
  Ray := VectorSubtract(newPosition, oldPosition);
  // ray := Velocity;
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

        CollisionState := TGLCollisionState.Create();
        CollisionState.Position := oldPosition;
        CollisionState.Contact.intNormal := intNormal;
        CollisionState.Contact.intPoint := intPoint;
        CollisionState.Time := GetTickCount();

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
// ------------------ TGLBFPSMovement ------------------
// ------------------

constructor TGLBFPSMovement.Create(aOwner: TXCollection);

  procedure setupArrow(arrow: TGLArrowLine; color: TColor);
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

  CollisionStates := TGLCollisionStates.Create;

  // FIXME: Creating arrows here, but they should be only added when
  // a "showArrows" property changed
  ArrowLine1 := TGLArrowLine.Create(nil);
  setupArrow(ArrowLine1, clRed);
  ArrowLine2 := TGLArrowLine.Create(nil);
  setupArrow(ArrowLine2, clGreen);
  ArrowLine3 := TGLArrowLine.Create(nil);
  setupArrow(ArrowLine3, clBlue);
  ArrowLine4 := TGLArrowLine.Create(nil);
  setupArrow(ArrowLine4, clSilver);
  ArrowLine5 := TGLArrowLine.Create(nil);
  setupArrow(ArrowLine5, clSilver);
  ArrowLine6 := TGLArrowLine.Create(nil);
  setupArrow(ArrowLine6, clSilver);

  dirGl := TGLDirectOpenGL.Create(nil);
  dirGl.OnRender := RenderArrowLines;

  oldPosition := OwnerBaseSceneObject.Position.AsVector;
  FManagerName := '';
end;

destructor TGLBFPSMovement.Destroy;
var
  i: integer;
begin
  // remove all states
  for i := 0 to CollisionStates.count - 1 do
    TGLCollisionState(CollisionStates[i]).Free;
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

class function TGLBFPSMovement.FriendlyName: String;
begin
  Result := 'FPS Movement';
end;

procedure TGLBFPSMovement.WriteToFiler(writer: TWriter);
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

procedure TGLBFPSMovement.ReadFromFiler(reader: TReader);
var
  archiveVersion: integer;
begin
  inherited ReadFromFiler(reader);
  with reader do
  begin
    archiveVersion := readInteger;
    assert(archiveVersion = 0, 'Wrong ArchiveVersion for TGLBFPSMovement');
    CollisionGroup := readInteger;
    sphereRadius := ReadSingle;
    GravityEnabled := ReadBoolean;
    ShowArrows := ReadBoolean;
    FManagerName := ReadString;
  end;
end;

procedure TGLBFPSMovement.Loaded;
var
  mng: TComponent;
begin
  inherited Loaded;
  if FManagerName <> '' then
  begin
    mng := FindManager(TGLFPSMovementManager, FManagerName);
    if assigned(mng) then
      Manager := TGLFPSMovementManager(mng);
    FManagerName := '';
  end;
end;

procedure TGLBFPSMovement.setShowArrows(value: boolean);
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

procedure TGLBFPSMovement.MoveForward(Distance: single);
var
  prevObj: TGLBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TGLBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.MoveForward(Distance);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TGLBFPSMovement.StrafeHorizontal(Distance: single);
var
  prevObj: TGLBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TGLBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.StrafeHorizontal(Distance);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TGLBFPSMovement.StrafeVertical(Distance: single);
var
  prevObj: TGLBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TGLBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.StrafeVertical(Distance);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TGLBFPSMovement.TurnHorizontal(Angle: single);
var
  prevObj: TGLBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TGLBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.TurnHorizontal(Angle);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TGLBFPSMovement.TurnVertical(Angle: single);
var
  prevObj: TGLBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TGLBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.TurnVertical(Angle);
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TGLBFPSMovement.Straighten;
var
  prevObj: TGLBaseSceneObject;
begin
  assert(assigned(Manager),
    'Manager not assigned on TGLBFPSMovement behaviour!');
  prevObj := Manager.Navigator.MovingObject;
  Manager.Navigator.MovingObject := OwnerBaseSceneObject;
  Manager.Navigator.Straighten;
  Manager.Navigator.MovingObject := prevObj;
end;

procedure TGLBFPSMovement.DoProgress(const progressTime: TGLProgressTimes);
var
  newPosition: TGLVector;
  CollisionState: TGLCollisionState;
begin
  inherited DoProgress(progressTime);

  assert(assigned(Manager), 'FPS Manager not assigned to behaviour.');

  // make arrowlines invisible (they are made visible in SphereSweepAndSlide)
  ArrowLine1.Visible := false;
  ArrowLine2.Visible := false;
  ArrowLine3.Visible := false;
  ArrowLine4.Visible := false;
  ArrowLine5.Visible := false;
  ArrowLine6.Visible := false;

  CollisionState := TGLCollisionState.Create();
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
    CollisionState := TGLCollisionState(CollisionStates.First);
    tickCount := GetTickCount();
    // remove all old states
    while (CollisionState <> nil) and
      (CollisionState.Time < tickCount - Manager.DisplayTime) do
    begin
      CollisionStates.Remove(CollisionState);
      CollisionState.Free;
      if CollisionStates.count = 0 then
        exit;
      CollisionState := TGLCollisionState(CollisionStates.First);
    end;
  end;
end;

procedure TGLBFPSMovement.RenderArrowLines(Sender: TObject;
  var rci: TGLRenderContextInfo);
var
  x, y, z, t: single;
  i: integer;
  CollisionState: TGLCollisionState;
begin
  // caption:= IntToStr(CollisionStates.Count);
  gl.Color3f(1, 1, 1);
  rci.GLStates.Disable(stLighting);
  // draw position trail
  gl.Begin_(GL_LINE_STRIP);
  for i := 0 to CollisionStates.count - 1 do
  begin
    CollisionState := TGLCollisionState(CollisionStates.Items[i]);
    x := CollisionState.Position.X;
    y := CollisionState.Position.Y;
    z := CollisionState.Position.Z;
    gl.Vertex3f(x, y, z);
  end;
  gl.End_();
  // draw normals trail
  gl.Begin_(GL_LINES);
  for i := 0 to CollisionStates.count - 1 do
  begin
    CollisionState := TGLCollisionState(CollisionStates.Items[i]);
    t := (Manager.DisplayTime - (tickCount - CollisionState.Time)) /
      Manager.DisplayTime;
    gl.Color3f(t, t, t);
    gl.Vertex3f(CollisionState.Contact.intPoint.X,
      CollisionState.Contact.intPoint.Y, CollisionState.Contact.intPoint.Z);
    gl.Vertex3f(CollisionState.Contact.intPoint.X +
      CollisionState.Contact.intNormal.X, // GLSphere4.Radius,
      CollisionState.Contact.intPoint.Y + CollisionState.Contact.intNormal.Y,
      // GLSphere4.Radius,
      CollisionState.Contact.intPoint.Z + CollisionState.Contact.intNormal.Z);
    // GLSphere4.Radius);
  end;
  gl.End_();
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

RegisterXCollectionItemClass(TGLMapCollectionItem);
RegisterXCollectionItemClass(TGLBFPSMovement);

finalization

UnregisterXCollectionItemClass(TGLMapCollectionItem);
UnregisterXCollectionItemClass(TGLBFPSMovement);

end.
