//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Movement;

(*
   Movement path behaviour by Roger Cao
   Note: It is recommended to set TgxMovementPath.RotationMode = rmUpDirection,
   but the default value is rmTurnPitchRoll for backwards compatibility.

*)

interface

{$I GXS.Scene.inc}

uses
  Winapi.OpenGL,

  System.Classes,
  System.SysUtils,

  GXS.XCollection,
  GXS.PersistentClasses,
  GXS.BaseClasses,
  GXS.VectorTypes,
  GXS.VectorGeometry,
  GXS.Spline,
  GXS.Strings,

  GXS.Scene,
  GXS.Objects,
  GXS.Utils;

type

  TgxPathNode = class (TCollectionItem)
  private
    FPosition: TVector4f;
    FScale: TVector4f;
    FRotation: TVector4f;
    FDirection: TVector4f;
    FUp: TVector4f;
    FSpeed: single;
    procedure SetPositionAsVector(const Value: TVector4f);
    procedure SetRotationAsVector(const Value: TVector4f);
    procedure SetScaleAsVector(const Value: TVector4f);
    function GetPositionCoordinate(const Index: Integer): Single;
    procedure SetPositionCoordinate(const Index: integer; const AValue: Single);
    function GetRotationCoordinate(const Index: Integer): Single;
    procedure SetRotationCoordinate(const Index: integer; const AValue: Single);
    function GetScaleCoordinate(const Index: Integer): Single;
    procedure SetScaleCoordinate(const Index: integer; const AValue: Single);
    procedure SetSpeed(const Value: single);
    function GetDirectionCoordinate(const Index: Integer): Single;
    procedure SetDirectionCoordinate(const Index: integer; const AValue: Single);
    function GetUpCoordinate(const Index: Integer): Single;
    procedure SetUpCoordinate(const Index: integer; const AValue: Single);
  protected
    function GetDisplayName: string; override;
    procedure WriteToFiler(writer : TWriter);
    procedure ReadFromFiler(reader : TReader);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function PositionAsAddress: PGLFloat;
    function RotationAsAddress: PGLFloat;
    function ScaleAsAddress: PGLFloat;
    procedure Assign(Source: TPersistent); override;
    procedure InitializeByObject(const Obj: TgxBaseSceneObject);
    // Warning: does not take speed into account.
    function EqualNode(const aNode: TgxPathNode): boolean;
    // Rotation.X means PitchAngle, Rotation.Y means TurnAngle, Rotation.Z means RollAngle.
    property RotationAsVector: TVector4f Read FRotation Write SetRotationAsVector;
    property PositionAsVector: TVector4f Read FPosition Write SetPositionAsVector;
    property ScaleAsVector: TVector4f Read FScale Write SetScaleAsVector;
    property UpAsVector: TVector4f read FUp write FUp;
    property DirectionAsVector: TVector4f read FDirection write FDirection;
  published
    property X: Single index 0 Read GetPositionCoordinate Write SetPositionCoordinate;
    property Y: Single index 1 Read GetPositionCoordinate Write SetPositionCoordinate;
    property Z: Single index 2 Read GetPositionCoordinate Write SetPositionCoordinate;
    (* Rotation.X means PitchAngle;
       Rotation.Y means TurnAngle;
       Rotation.Z means RollAngle; *)
    property PitchAngle: Single index 0 Read GetRotationCoordinate Write SetRotationCoordinate;
    property TurnAngle: Single index 1 Read GetRotationCoordinate Write SetRotationCoordinate;
    property RollAngle: Single index 2 Read GetRotationCoordinate Write SetRotationCoordinate;
    property ScaleX: Single index 0 Read GetScaleCoordinate Write SetScaleCoordinate;
    property ScaleY: Single index 1 Read GetScaleCoordinate Write SetScaleCoordinate;
    property ScaleZ: Single index 2 Read GetScaleCoordinate Write SetScaleCoordinate;
    property DirectionX: Single index 0 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property DirectionY: Single index 1 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property DirectionZ: Single index 2 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property UpX: Single index 0 Read GetUpCoordinate Write SetUpCoordinate;
    property UpY: Single index 1 Read GetUpCoordinate Write SetUpCoordinate;
    property UpZ: Single index 2 Read GetUpCoordinate Write SetUpCoordinate;
    property Speed: single Read FSpeed Write SetSpeed;
  end;


  TgxMovementRotationMode = (rmTurnPitchRoll, rmUpDirection);
  TgxMovementPath = class;

  TgxPathNodes = class (TOwnedCollection)
  protected
    procedure SetItems(const index: integer; const val: TgxPathNode);
    function GetItems(const index: integer): TgxPathNode;
  public
    constructor Create(aOwner: TgxMovementPath);
    function GetOwnerMovementPath: TgxMovementPath;
    function Add: TgxPathNode;
    function FindItemID(const ID: integer): TgxPathNode;
    property Items[const index: integer]: TgxPathNode Read GetItems Write SetItems; default;
    procedure NotifyChange; virtual;
  end;

  TgxMovement = class;
  TgxMovementPaths = class;

  TgxMovementPath = class(TCollectionItem)
  private
    FPathLine: TgxLines;
    FShowPath: Boolean;
    FPathSplineMode: TgxLineSplineMode;
    FNodes: TgxPathNodes;
    //All the time saved in ms
    FStartTimeApplied: Boolean;
    FStartTime: double;
    FInitialTime: Double;
    FEstimateTime: double;
    FCurrentNode: TgxPathNode;
    FInTravel: boolean;
    FLooped: boolean;
    FName: string;
    FRotationMode: TgxMovementRotationMode;
    MotionSplineControl: TCubicSpline;
    RotationSplineControl: TCubicSpline;
    ScaleSplineControl: TCubicSpline;
    DirectionSplineControl: TCubicSpline;
    UpSplineControl: TCubicSpline;
    FOnTravelStart: TNotifyEvent;
    FOnTravelStop: TNotifyEvent;
    FCurrentNodeIndex: integer;
    function GetNodeCount: integer;
    procedure SetStartTime(const Value: double);
    procedure SetCurrentNodeIndex(const Value: integer);
    procedure SetShowPath(const Value: Boolean);
    procedure SetPathSplineMode(const Value: TgxLineSplineMode);
  protected
    procedure WriteToFiler(writer : TWriter);
    procedure ReadFromFiler(reader : TReader);
    function CanTravel: boolean;
    function GetCollection: TgxMovementPaths;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetMovement: TgxMovement;
    function AddNode: TgxPathNode; overload;
    function AddNode(const Node: TgxPathNode): TgxPathNode; overload;
    function AddNodeFromObject(const Obj: TgxBaseSceneObject): TgxPathNode;
    function InsertNodeFromObject(const Obj: TgxBaseSceneObject; const Index: integer): TgxPathNode;
    function InsertNode(const Node: TgxPathNode; const Index: integer): TgxPathNode; overload;
    function InsertNode(const Index: integer): TgxPathNode; overload;
    function DeleteNode(const Index: integer): TgxPathNode; overload;
    function DeleteNode(const Node: TgxPathNode): TgxPathNode; overload;
    procedure ClearNodes;
    procedure UpdatePathLine;
    function NodeDistance(const Node1, Node2: TgxPathNode): double;
    procedure CalculateState(const CurrentTime: double);
    procedure TravelPath(const Start: boolean); overload;
    procedure TravelPath(const Start: boolean; const aStartTime: double); overload;
    property NodeCount: integer Read GetNodeCount;
    property CurrentNode: TgxPathNode Read FCurrentNode;
    property InTravel: boolean Read FInTravel;
    function PrevNode: integer;
    function NextNode: integer;
    property CurrentNodeIndex: integer Read FCurrentNodeIndex Write SetCurrentNodeIndex;
    property OnTravelStart: TNotifyEvent Read FOnTravelStart Write FOnTravelStart;
    property OnTravelStop: TNotifyEvent Read FOnTravelStop Write FOnTravelStop;
  published
    property Name: string Read FName Write FName;
    { This property is currently ignored. }
    property PathSplineMode: TgxLineSplineMode read FPathSplineMode write SetPathSplineMode default lsmLines;
    property RotationMode: TgxMovementRotationMode read FRotationMode write FRotationMode default rmTurnPitchRoll;
    property StartTime: double Read FStartTime Write SetStartTime;
    property EstimateTime: double Read FEstimateTime;
    property Looped: boolean Read FLooped Write FLooped;
    property Nodes: TgxPathNodes Read FNodes;
    property ShowPath: Boolean read FShowPath write SetShowPath;
  end;

  TgxMovementPaths = class(TOwnedCollection)
  protected
    procedure SetItems(const index: integer; const val: TgxMovementPath);
    function GetItems(const index: integer): TgxMovementPath;
    function GetMovement: TgxMovement;
  public
    constructor Create(aOwner: TgxMovement);
    function Add: TgxMovementPath;
    function FindItemID(const ID: integer): TgxMovementPath;
    property Items[const index: integer]: TgxMovementPath Read GetItems Write SetItems; default;
    procedure NotifyChange; virtual;
  end;


  //Event for path related event
  TPathTravelStartEvent = procedure (Sender: TObject;
    Path: TgxMovementPath) of object;
  TPathTravelStopEvent = procedure (Sender: TObject;
    Path: TgxMovementPath; var Looped: boolean) of object;

  TgxMovement = class(TgxBehaviour)
  private
    FPaths: TgxMovementPaths;
    FAutoStartNextPath: boolean;
    FActivePathIndex: integer;
    FOnAllPathTravelledOver: TNotifyEvent;
    FOnPathTravelStart: TPathTravelStartEvent;
    FOnPathTravelStop: TPathTravelStopEvent;

    ///function GetMovementPath(Index: integer): TgxMovementPath;
    ///procedure SetMovementPath(Index: integer; AValue: TgxMovementPath);

    function GetPathCount: integer;
    procedure SetActivePathIndex(Value: integer);
    function GetActivePath: TgxMovementPath;
    procedure SetActivePath(Value: TgxMovementPath);
  protected
    procedure WriteToFiler(writer : TWriter); override;
    procedure ReadFromFiler(reader : TReader); override;
    procedure PathTravelStart(Sender: TObject);
    procedure PathTravelStop(Sender: TObject);
    function GetSceneObject: TgxBaseSceneObject;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    //add an empty path;
    function AddPath: TgxMovementPath; overload;
    //add an path with one node, and the node is based on aObject
    function AddPath(aObject: TgxBaseSceneObject): TgxMovementPath; overload;
    //add one path to the new one
    function AddPath(Path: TgxMovementPath): TgxMovementPath; overload;
    procedure ClearPaths;
      //Result is current path
    function DeletePath(Path: TgxMovementPath): TgxMovementPath; overload;
    function DeletePath(Index: integer): TgxMovementPath; overload;
    function DeletePath: TgxMovementPath; overload;
    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;
    procedure StartPathTravel;
    procedure StopPathTravel;
    procedure DoProgress(const progressTime : TgxProgressTimes); override;
    function NextPath: integer;
    function PrevPath: integer;
    function FirstPath: integer;
    function LastPath: integer;
      //property Paths[index: Integer]: TgxMovementPath read GetMovementPath write SetMovementPath;
    property PathCount: integer Read GetPathCount;
    //why do these property can't be saved in IDE ?
    property OnAllPathTravelledOver: TNotifyEvent Read FOnAllPathTravelledOver Write FOnAllPathTravelledOver;
    property OnPathTravelStart: TPathTravelStartEvent Read FOnPathTravelStart Write FOnPathTravelStart;
    property OnPathTravelStop: TPathTravelStopEvent Read FOnPathTravelStop Write FOnPathTravelStop;
  published
    property Paths: TgxMovementPaths Read FPaths;
    property AutoStartNextPath: boolean Read FAutoStartNextPath Write FAutoStartNextPath;
    property ActivePathIndex: integer Read FActivePathIndex Write SetActivePathIndex;
    property ActivePath: TgxMovementPath Read GetActivePath Write SetActivePath;
  end;

function GetMovement(const behaviours: TgxBehaviours): TgxMovement; overload;
function GetMovement(const obj: TgxBaseSceneObject): TgxMovement; overload;
function GetOrCreateMovement(const behaviours: TgxBehaviours): TgxMovement; overload;
function GetOrCreateMovement(const obj: TgxBaseSceneObject): TgxMovement; overload;
procedure StartAllMovements(const Scene: TgxScene; const StartCamerasMove, StartObjectsMove: Boolean);
procedure StopAllMovements(const Scene: TgxScene; const StopCamerasMove, StopObjectsMove: Boolean);

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

//----------------------------- TgxPathNode ------------------------------------
constructor TgxPathNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPosition  := VectorMake(0, 0, 0, 1);
  FRotation  := VectorMake(0, 0, 0, 1);
  FScale     := VectorMake(1, 1, 1, 1);
  FDirection := ZHmgVector;
  FUp        := YHmgVector;
  FSpeed     := 0;
end;

destructor TgxPathNode.Destroy;
begin
  inherited Destroy;
end;

procedure TgxPathNode.SetPositionAsVector(const Value: TVector4f);
begin
  FPosition := Value;
    (Collection as TgxPathNodes).NotifyChange;
end;

procedure TgxPathNode.SetRotationAsVector(const Value: TVector4f);
begin
  FRotation := Value;
    (Collection as TgxPathNodes).NotifyChange;
end;

procedure TgxPathNode.SetScaleAsVector(const Value: TVector4f);
begin
  FScale := Value;
    (Collection as TgxPathNodes).NotifyChange;
end;

function TgxPathNode.PositionAsAddress: PGLFloat;
begin
  Result := @FPosition;
end;

function TgxPathNode.RotationAsAddress: PGLFloat;
begin
  Result := @FRotation;
end;

function TgxPathNode.ScaleAsAddress: PGLFloat;
begin
  Result := @FScale;
end;

procedure TgxPathNode.WriteToFiler(writer : TWriter);
var
  WriteStuff: boolean;
begin
  with Writer do
  begin
    WriteInteger(1); // Archive Version 1.
    WriteStuff := not (VectorEquals(FPosition, NullHmgPoint) and
                       VectorEquals(FRotation, NullHmgPoint) and
                       VectorEquals(FScale, XYZHmgVector) and
                       (Speed = 0) and
                       VectorEquals(FDirection, ZHmgVector) and
                       VectorEquals(FUp, YHmgVector));

    WriteBoolean(writeStuff);

    if WriteStuff then
    begin
      // Archive Version 0.
      Write(FPosition, SizeOf(FPosition));
      Write(FRotation, SizeOf(FRotation));
      Write(FScale, SizeOf(FScale));
      WriteFloat(FSpeed);
      
      // Archive Version 1.
      Write(FDirection, SizeOf(FDirection));
      Write(FUp, SizeOf(FUp));
    end;
  end;
end;

procedure TgxPathNode.ReadFromFiler(reader : TReader);
var
  lVersion: Integer;
begin
  with Reader do
  begin
    lVersion := ReadInteger;
    if ReadBoolean then
    begin
      // Archive Version 0.
      Read(FPosition, SizeOf(FPosition));
      Read(FRotation, SizeOf(FRotation));
      Read(FScale, SizeOf(FScale));
      FSpeed := ReadFloat;

      // Archive Version 1.
      if lVersion >= 1 then
      begin
        Read(FDirection, SizeOf(FDirection));
        Read(FUp, SizeOf(FUp));
      end;
    end
    else
    begin
      // Default parameters.
      FPosition := NullHmgPoint;
      FRotation := NullHmgPoint;
      FScale := VectorMake(1, 1, 1, 1);
      FSpeed := 0;
      FDirection := ZHmgVector;
      FUp        := YHmgVector;
    end;
  end;
end;

procedure TgxPathNode.InitializeByObject(const Obj: TgxBaseSceneObject);
begin
  if Assigned(Obj) then
  begin
    FPosition := Obj.Position.AsVector;
    FScale    := Obj.Scale.AsVector;
    FRotation := Obj.Rotation.AsVector;
    FDirection := Obj.Direction.AsVector;
    FUp        := Obj.Up.AsVector;
  end;
end;

procedure TgxPathNode.Assign(Source: TPersistent);
begin
  if Source is TgxPathNode then
  begin
    FPosition := TgxPathNode(Source).FPosition;
    FRotation := TgxPathNode(Source).FRotation;
    FScale    := TgxPathNode(Source).FScale;
    FSpeed    := TgxPathNode(Source).FSpeed;

    FDirection := TgxPathNode(Source).FDirection;
    FUp        := TgxPathNode(Source).FUp;
  end else
    inherited Assign(Source);
end;

function TgxPathNode.EqualNode(const aNode: TgxPathNode): boolean;
begin
  Result := VectorEquals(FPosition, aNode.FPosition) and
            VectorEquals(FRotation, aNode.FRotation) and
            VectorEquals(FScale, aNode.FScale) and
            VectorEquals(FDirection, aNode.FDirection) and
            VectorEquals(FUp, aNode.FUp);
end;

procedure TgxPathNode.SetSpeed(const Value: single);
begin
  FSpeed := Value;
end;

function TgxPathNode.GetDisplayName: string;
begin
  Result := 'PathNode';
end;

function TgxPathNode.GetPositionCoordinate(const Index: Integer): Single;
begin
  result := FPosition.V[Index];
end;

procedure TgxPathNode.SetPositionCoordinate(const Index: integer; const AValue: Single);
begin
  FPosition.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TgxPathNodes).NotifyChange;
end;

function TgxPathNode.GetRotationCoordinate(const Index: Integer): Single;
begin
  result := FRotation.V[Index];
end;

procedure TgxPathNode.SetRotationCoordinate(const Index: integer; const AValue: Single);
begin
  FRotation.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TgxPathNodes).NotifyChange;
end;

function TgxPathNode.GetScaleCoordinate(const Index: Integer): Single;
begin
  result := FScale.V[Index];
end;

procedure TgxPathNode.SetScaleCoordinate(const Index: integer; const AValue: Single);
begin
  FScale.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TgxPathNodes).NotifyChange;
end;


function TgxPathNode.GetDirectionCoordinate(const Index: Integer): Single;
begin
  result := FDirection.V[Index];
end;


procedure TgxPathNode.SetDirectionCoordinate(const Index: integer;
  const AValue: Single);
begin
  FDirection.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TgxPathNodes).NotifyChange;
end;

function TgxPathNode.GetUpCoordinate(const Index: Integer): Single;
begin
  result := FUp.V[Index];
end;

procedure TgxPathNode.SetUpCoordinate(const Index: integer; const AValue: Single);
begin
  FUp.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TgxPathNodes).NotifyChange;
end;

//--------------------------- TgxPathNodes -------------------------------------
constructor TgxPathNodes.Create(aOwner: TgxMovementPath);
begin
  inherited Create(aOwner, TgxPathNode);
end;

procedure TgxPathNodes.SetItems(const index: integer; const val: TgxPathNode);
begin
  inherited Items[index] := val;
end;

function TgxPathNodes.GetItems(const index: integer): TgxPathNode;
begin
  Result := TgxPathNode(inherited Items[index]);
end;

function TgxPathNodes.Add: TgxPathNode;
begin
  Result := (inherited Add) as TgxPathNode;
end;

function TgxPathNodes.GetOwnerMovementPath: TgxMovementPath;
begin
  Result := TgxMovementPath(GetOwner);
end;

function TgxPathNodes.FindItemID(const ID: integer): TgxPathNode;
begin
  Result := (inherited FindItemID(ID)) as TgxPathNode;
end;

procedure TgxPathNodes.NotifyChange;
begin
  // Update the path-line if avalible in TgxMovementPath.
  GetOwnerMovementPath.UpdatePathLine;
end;

//--------------------------- TgxMovementPath ----------------------------------
constructor TgxMovementPath.Create(Collection: TCollection);
begin
  // This object can only be added to a TgxMovement class.
  inherited Create(Collection);

  FNodes := TgxPathNodes.Create(Self);
  FCurrentNodeIndex := -1;
  FRotationMode := rmTurnPitchRoll;
  FPathSplineMode := lsmCubicSpline;
  FStartTimeApplied := False;  
end;

destructor TgxMovementPath.Destroy;
begin
  // Make sure the splines are freed.
  FLooped:= false;
  
  ClearNodes;
  FNodes.Free;

  inherited Destroy;
end;

procedure TgxMovementPath.WriteToFiler(writer : TWriter);
var
  WriteStuff: boolean;
  I: Integer;
begin
  with Writer do
  begin
    WriteInteger(1); // Archive Version 1.
    WriteStuff := (FNodes.Count>0) or (FLooped) or (FCurrentNodeIndex<>-1) or (FShowPath) or
                  (FPathSplineMode <> lsmCubicSpline) or (FRotationMode <> rmTurnPitchRoll);
    WriteBoolean(writeStuff);
    if WriteStuff then
    begin
      // Archive Version 0.
      WriteBoolean(FLooped);
      WriteInteger(FCurrentNodeIndex);
      WriteBoolean(FShowPath);
      Write(FPathSplineMode, SizeOf(FPathSplineMode));
      WriteInteger(FNodes.Count);
      for I:=0 to FNodes.Count-1 do
        FNodes.Items[I].WriteToFiler(Writer);

      // Archive Version 1.
      WriteInteger(Ord(FRotationMode));
    end;
  end;
end;

procedure TgxMovementPath.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  Node: TgxPathNode;
  lVersion: Integer;
begin
  ClearNodes;
  with Reader do
  begin
    lVersion := ReadInteger; // Archive Version.
    if ReadBoolean then
    begin
      // Archive Version 0.
      FLooped := ReadBoolean;
      FCurrentNodeIndex := ReadInteger;
      ShowPath := ReadBoolean;
      Read(FPathSplineMode, SizeOf(FPathSplineMode));

      Count := ReadInteger;
      for I:=0 to Count-1 do
      begin
        Node := AddNode;
        Node.ReadFromFiler(Reader);
      end;

      // Archive Version 1.
      if lVersion >= 1 then
      begin
        FRotationMode := TgxMovementRotationMode(ReadInteger);
      end;  
    end
    else
    begin
      FLooped := False;
      FCurrentNodeIndex := -1;
      FShowPath := False;
      FPathSplineMode := lsmCubicSpline;
      FRotationMode := rmTurnPitchRoll;
    end;
  end;
  UpdatePathLine;
end;

procedure TgxMovementPath.SetPathSplineMode(const Value: TgxLineSplineMode);
begin
  if Value<>FPathSplineMode then
  begin
    FPathSplineMode := Value;
    if FShowPath then
      FPathLine.SplineMode := FPathSplineMode;
  end;
end;

procedure TgxMovementPath.UpdatePathLine;
var
  I: Integer;
  Node: TgxPathNode;
begin
  if FShowPath then
  begin
    FPathLine.Nodes.Clear;
    for I:=0 to Nodes.Count-1 do
    begin
      Node := Nodes.Items[I];
      FPathLine.AddNode(Node.PositionAsVector);
    end;
  end;
end;

procedure TgxMovementPath.SetShowPath(const Value: Boolean);
var
  OwnerObj: TgxBaseSceneObject;
begin
  if FShowPath<>Value then
  begin
    FShowPath := Value;
    OwnerObj := GetMovement.GetSceneObject;
    if FShowPath then
    begin
      FPathLine := TgxLines.Create(OwnerObj);
      MakeSubComponent(FPathLine, True);
      OwnerObj.Scene.Objects.AddChild(FPathLine);
      FPathLine.SplineMode := FPathSplineMode;
      UpdatePathLine;
    end
    else
      FreeAndNil(FPathLine);
  end;
end;

procedure TgxMovementPath.ClearNodes;
begin
  TravelPath(False);
  FNodes.Clear;
  if Assigned(FCurrentNode) then
  begin
    FCurrentNode.Free;
    FCurrentNode := nil;
  end;
  FCurrentNodeIndex := -1;
  UpdatePathLine;
end;

procedure TgxMovementPath.SetCurrentNodeIndex(const Value: integer);
begin
  if FNodes.Count = 0 then
  begin
    FCurrentNodeIndex := -1;
    exit;
  end;
  if (FInTravel) or (Value > FNodes.Count - 1) or (Value < 0) then
    exit
  else
  begin
    FCurrentNodeIndex := Value;
    if not Assigned(FCurrentNode) then
      FCurrentNode := TgxPathNode.Create(nil);
    FCurrentNode.Assign(Nodes[FCurrentNodeIndex]);
  end;
end;

function TgxMovementPath.InsertNode(const Node: TgxPathNode; const Index: integer): TgxPathNode;
var
  N: TgxPathNode;
begin
  Result := nil;
  //Intravel, can't insert
  if FInTravel then
    exit;
  //Insert into the position
  if (Assigned(Node)) and (Assigned(Nodes[Index])) then
  begin
    N := TgxPathNode(FNodes.Insert(Index));
    if Index >0 then
      N.Assign(Nodes[Index -1]);
  end
  else
    //add to the tail of list
    N    := FNodes.Add;
  Result := N;
  UpdatePathLine;
end;

function TgxMovementPath.InsertNode(const Index: integer): TgxPathNode;
var
  N: TgxPathNode;
begin
  Result := nil;
  //Intravel, can't insert
  if FInTravel then
    exit;
  //Insert into the position
  if (Assigned(Nodes[Index])) then
  begin
    N := TgxPathNode(FNodes.Insert(Index));
    if Index >0 then
      N.Assign(Nodes[Index -1]);
    Result := N;
  end
  else
    //add to the tail of list
    Result := AddNode;
  UpdatePathLine;
end;

function TgxMovementPath.AddNodeFromObject(const Obj: TgxBaseSceneObject): TgxPathNode;
begin
  Result := nil;
  if (FInTravel) or (not Assigned(Obj)) then
    exit;
  Result           := AddNode;
  Result.FPosition := Obj.Position.AsVector;
  Result.FScale    := Obj.Scale.AsVector;
  Result.FRotation := Obj.Rotation.AsVector;
  Result.FDirection:=  Obj.Direction.AsVector;
  Result.FUp:=         Obj.Up.AsVector;

  UpdatePathLine;
end;

function TgxMovementPath.InsertNodeFromObject(const Obj: TgxBaseSceneObject; const Index: integer): TgxPathNode;
begin
  Result := nil;
  if (FInTravel) or (not Assigned(Obj)) then
    exit;

  Result      := InsertNode(Index);
  Result.FPosition := Obj.Position.AsVector;
  Result.FScale    := Obj.Scale.AsVector;
  Result.FRotation := Obj.Rotation.AsVector;
  Result.FDirection:= Obj.Direction.AsVector;
  Result.FUp:= Obj.Up.AsVector;

  UpdatePathLine;
end;

function TgxMovementPath.DeleteNode(const Index: integer): TgxPathNode;
var
  Node: TgxPathNode;
begin
  Result := nil;
  //Ontravel, can't delete
  if FInTravel then
    exit;
  Node := Nodes[Index];
  if Assigned(Node) then
  begin
    FNodes.Delete(Index);
    if FCurrentNodeIndex < 0 then
      exit;
    if (Index =0) then
    begin
      if FNodes.Count > 0 then
        FCurrentNodeIndex := 0
      else
        FCurrentNodeIndex := -1;
    end
    else
    begin
      //one has been deleted, so the index should be equal to FNodeList.Count
      if Index =FNodes.Count then
        FCurrentNodeIndex := Index -1
      else
        FCurrentNodeIndex := Index;
    end;
    Result := Nodes[FCurrentNodeIndex];
  end;
  UpdatePathLine;
end;

function TgxMovementPath.DeleteNode(const Node: TgxPathNode): TgxPathNode;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to FNodes.Count - 1 do
  begin
    if Node = Nodes[I] then
    begin
      Result := DeleteNode(I);
      break;
    end;
  end;
  UpdatePathLine;
end;

function TgxMovementPath.PrevNode: integer;
begin
  Result := FCurrentNodeIndex;
  if FNodes.Count = 0 then
    exit;
  Dec(FCurrentNodeIndex);
  if (FCurrentNodeIndex < 0) then
    FCurrentNodeIndex := 0
  else
    //this line can cause the CurrentNode generated
    CurrentNodeIndex := FCurrentNodeIndex;
  Result             := FCurrentNodeIndex;
end;

function TgxMovementPath.NextNode: integer;
begin
  Result := FCurrentNodeIndex;
  if FNodes.Count = 0 then
    exit;
  Inc(FCurrentNodeIndex);
  if (FCurrentNodeIndex = FNodes.Count) then
    Dec(FCurrentNodeIndex)
  else
    //this line can cause the CurrentNode generated
    CurrentNodeIndex := FCurrentNodeIndex;
  Result             := FCurrentNodeIndex;
end;

function TgxMovementPath.NodeDistance(const Node1, Node2: TgxPathNode): double;
begin
  Result := VectorDistance(Node1.FPosition, Node2.FPosition);
end;

//need to do
//1 No acceleration implemented
//2 The travel-time of a segment is based a simple linear movement, at the start and the end
//  of the segment, the speed will be more high than in the middle
//3 Rotation Interpolation has not been tested
procedure TgxMovementPath.CalculateState(const CurrentTime: double);
var
  I:       integer;
  SumTime: double;
  L, L2:       single;
  Interpolated: boolean;
  T:       double;
  a:double;

  procedure Interpolation(ReturnNode: TgxPathNode; Time1, Time2: double; Index: integer);
  var
    Ratio: double;
    x, y, z, p, t, r, sx, sy, sz: single;
    dx, dy, dz,ux, uy, uz: single;
  begin
    Ratio:=(Nodes[I - 1].Speed*Time2+0.5*a*time2*time2)/L + Index;

    MotionSplineControl.SplineXYZ(Ratio, x, y, z);
    RotationSplineControl.SplineXYZ(Ratio, p, t, r);
    ScaleSplineControl.SplineXYZ(Ratio, sx, sy, sz);

    DirectionSplineControl.SplineXYZ(Ratio,dx,dy,dz);
    UpSplineControl.SplineXYZ(Ratio,ux,uy,uz);


    ReturnNode.FPosition := VectorMake(x, y, z, 1);
    ReturnNode.FRotation := VectorMake(p, t, r, 1);
    ReturnNode.FScale    := VectorMake(sx, sy, sz, 1);

    ReturnNode.FDirection := VectorMake(dx,dy,dz, 1);
    ReturnNode.FUp := VectorMake(ux,uy,uz, 1);
  end;

begin
  I := 1;

  if (FInitialTime = 0) or (FInitialTime > CurrentTime) then
    FInitialTime := CurrentTime;


  if (FStartTime <> 0) and not FStartTimeApplied then
  begin
    if FInitialTime + FStartTime < CurrentTime then
    begin
      FInitialTime := CurrentTime;
      FStartTimeApplied := True;
    end
    else
      Exit;
  end;

  SumTime      := FInitialTime;
  Interpolated := False;
  while I < FNodes.Count do
  begin
    L := NodeDistance(Nodes[I], Nodes[I - 1]);

    if L = 0 then
      L := VectorDistance(Nodes[i].FScale, Nodes[i-1].FScale);

    if L = 0 then
    begin
      L := VectorDistance(Nodes[i].FDirection, Nodes[i-1].FDirection);
      L2 := VectorDistance(Nodes[i].FUp, Nodes[i-1].Fup);
      if (L2 > L) then L:= L2;
    end;

    if L = 0 then
      L := Nodes[I - 0].Speed;

    T := L / (Nodes[I - 1].Speed + Nodes[I - 0].Speed) * 2;
    if (SumTime + T) >= CurrentTime then
    begin
      a:=(Nodes[I - 0].Speed-Nodes[I - 1].Speed)/T;
      Interpolation(FCurrentNode, T, CurrentTime - SumTime, I - 1);
      Interpolated := True;
      break;
    end
    else
    begin
      Inc(I);
      SumTime := SumTime + T;
    end;
  end;

  if (not Interpolated) then
  begin
    Interpolation(FCurrentNode, 1.0, 0.0, FNodes.Count - 1);
    TravelPath(False);
  end;
end;


function TgxMovementPath.CanTravel: boolean;
var
  I: integer;
begin
  Result := True;
  if FNodes.Count < 2 then
  begin
    Result := False;
    exit;
  end;
  for I := 0 to FNodes.Count - 1 do
    if Abs(Nodes[I].Speed) < 0.01 then
    begin
      Result := False;
      break;
    end;
end;

function TgxMovementPath.GetCollection: TgxMovementPaths;
begin
  Result := TgxMovementPaths(GetOwner);
end;

function TgxMovementPath.GetMovement: TgxMovement;
begin
  Result := GetCollection.GetMovement;
end;

procedure TgxMovementPath.TravelPath(const Start: boolean);
var
  x, y, z:    PFloatArray;
  p, t, r:    PFloatArray;
  sx, sy, sz: PFloatArray;
  dx, dy, dz: PFloatArray;
  ux, uy, uz: PFloatArray;

  I:          integer;
begin
  if (FInTravel = Start) or (FNodes.Count = 0) then
    exit;
  //One of the node speed < 0.01;
  if (Start) and (not CanTravel) then
    exit;
  FInTravel := Start;
  if FInTravel then
  begin
    GetMem(x, sizeof(single) * FNodes.Count);
    GetMem(y, sizeof(single) * FNodes.Count);
    GetMem(z, sizeof(single) * FNodes.Count);
    GetMem(p, sizeof(single) * FNodes.Count);
    GetMem(t, sizeof(single) * FNodes.Count);
    GetMem(r, sizeof(single) * FNodes.Count);
    GetMem(sx, sizeof(single) * FNodes.Count);
    GetMem(sy, sizeof(single) * FNodes.Count);
    GetMem(sz, sizeof(single) * FNodes.Count);
    GetMem(dx, sizeof(single) * FNodes.Count);
    GetMem(dy, sizeof(single) * FNodes.Count);
    GetMem(dz, sizeof(single) * FNodes.Count);
    GetMem(ux, sizeof(single) * FNodes.Count);
    GetMem(uy, sizeof(single) * FNodes.Count);
    GetMem(uz, sizeof(single) * FNodes.Count);

    for I := 0 to FNodes.Count - 1 do
    begin
      PFloatArray(x)[I]  := Nodes[I].FPosition.X;
      PFloatArray(y)[I]  := Nodes[I].FPosition.Y;
      PFloatArray(z)[I]  := Nodes[I].FPosition.Z;
      PFloatArray(p)[I]  := Nodes[I].FRotation.X;
      PFloatArray(t)[I]  := Nodes[I].FRotation.Y;
      PFloatArray(r)[I]  := Nodes[I].FRotation.Z;
      PFloatArray(sx)[I] := Nodes[I].FScale.X;
      PFloatArray(sy)[I] := Nodes[I].FScale.Y;
      PFloatArray(sz)[I] := Nodes[I].FScale.Z;

      PFloatArray(dx)[I] := Nodes[I].FDirection.X;
      PFloatArray(dy)[I] := Nodes[I].FDirection.Y;
      PFloatArray(dz)[I] := Nodes[I].FDirection.Z;

      PFloatArray(ux)[I] := Nodes[I].FUp.X;
      PFloatArray(uy)[I] := Nodes[I].FUp.Y;
      PFloatArray(uz)[I] := Nodes[I].FUp.Z;

    end;
    MotionSplineControl   := TCubicSpline.Create(x, y, z, nil, FNodes.Count);
    RotationSplineControl := TCubicSpline.Create(p, t, r, nil, FNodes.Count);
    ScaleSplineControl    := TCubicSpline.Create(sx, sy, sz, nil, FNodes.Count);
    DirectionSplineControl:= TCubicSpline.Create(dx, dy, dz, nil, FNodes.Count);
    UpSplineControl:= TCubicSpline.Create(ux, uy, uz, nil, FNodes.Count);

    FreeMem(x);
    FreeMem(y);
    FreeMem(z);
    FreeMem(p);
    FreeMem(t);
    FreeMem(r);
    FreeMem(sx);
    FreeMem(sy);
    FreeMem(sz);
    FreeMem(dx);
    FreeMem(dy);
    FreeMem(dz);
    FreeMem(ux);
    FreeMem(uy);
    FreeMem(uz);


    FreeAndNil(FCurrentNode);
    FCurrentNode := TgxPathNode.Create(nil);
    FCurrentNode.Assign(Nodes[0]);
    FCurrentNodeIndex := -1;

    FEstimateTime := 0;
    for I := 1 to FNodes.Count - 1 do
      FEstimateTime := FEstimateTime + NodeDistance(Nodes[I], Nodes[I - 1]) / Nodes[I - 1].Speed;

    if Assigned(FOnTravelStart) then
      FOnTravelStart(self);
  end
  else
  begin
    FreeAndNil(MotionSplineControl);
    FreeAndNil(RotationSplineControl);
    FreeAndNil(ScaleSplineControl);
    FreeAndNil(DirectionSplineControl);
    FreeAndNil(UpSplineControl);

    if Assigned(FOnTravelStop) then
      FOnTravelStop(self);
  end;
end;

procedure TgxMovementPath.TravelPath(const Start: boolean; const aStartTime: double);
begin
  if FInTravel = Start then
    exit;
  FInitialTime := aStartTime;
  FStartTimeApplied := False;
  TravelPath(Start);
end;

function TgxMovementPath.GetNodeCount: integer;
begin
  Result := FNodes.Count;
end;

//-------------------------- This function need modified -----------------------
procedure TgxMovementPath.SetStartTime(const Value: double);
begin
  FStartTime := Value;
end;

procedure TgxMovementPath.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TgxMovementPath then
  begin
    ClearNodes;
    for I := 0 to TgxMovementPath(Source).NodeCount - 1 do
    begin
      AddNode;
      Nodes[I].Assign(TgxMovementPath(Source).Nodes[I]);
      FStartTime := TgxMovementPath(Source).FStartTime;
      //FEstimateTime := TgxMovementPath(Source).FEstimateTime;
      FLooped := TgxMovementPath(Source).FLooped;
      FRotationMode := TgxMovementPath(Source).FRotationMode;
    end;
  end;
end;

function TgxMovementPath.AddNode: TgxPathNode;
var
  Node: TgxPathNode;
  I:    integer;
begin
  //Add a empty node, if it's not the first one, try locate the node to the previous one
  Node := FNodes.Add;
  I    := FNodes.Count;
  if I > 1 then
    Node.Assign(Nodes[I - 2]);
  Result := Node;
end;

function TgxMovementPath.AddNode(const Node: TgxPathNode): TgxPathNode;
begin
  Result := AddNode;
  if Assigned(Node) then
    Result.Assign(Node);
end;

//------------------------- TgxMovementPaths ----------------------------------
constructor TgxMovementPaths.Create(aOwner: TgxMovement);
begin
  inherited Create(aOwner, TgxMovementPath);
end;

procedure TgxMovementPaths.SetItems(const index: integer; const val: TgxMovementPath);
begin
  inherited Items[index] := val;
end;

function TgxMovementPaths.GetItems(const index: integer): TgxMovementPath;
begin
  Result := TgxMovementPath(inherited Items[index]);
end;

function TgxMovementPaths.Add: TgxMovementPath;
begin
  Result := (inherited Add) as TgxMovementPath;
end;

function TgxMovementPaths.FindItemID(const ID: integer): TgxMovementPath;
begin
  Result := (inherited FindItemID(ID)) as TgxMovementPath;
end;

procedure TgxMovementPaths.NotifyChange;
begin
  // Do nothing here.
end;

function TgxMovementPaths.GetMovement: TgxMovement;
begin
  Result := TgxMovement(GetOwner);
end;


//--------------------------- TgxMovement --------------------------------------
constructor TgxMovement.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FPaths           := TgxMovementPaths.Create(Self);
  FAutoStartNextPath := True;
  FActivePathIndex := -1;
  FOnAllPathTravelledOver := nil;
  FOnPathTravelStart := nil;
  FOnPathTravelStop := nil;
end;

destructor TgxMovement.Destroy;
begin
  ClearPaths;
  FPaths.Free;
  inherited Destroy;
end;

procedure TgxMovement.WriteToFiler(writer : TWriter);
var
  WriteStuff: boolean;
  I: Integer;
begin
  with Writer do
  begin
    // Archive Version 1, added inherited call
    WriteInteger(1);
    inherited;
    WriteStuff := (FPaths.Count>0) or (not FAutoStartNextPath) or (FActivePathIndex<>-1);
    WriteBoolean(WriteStuff);
    if WriteStuff then
    begin
      WriteBoolean(FAutoStartNextPath);
      WriteInteger(FActivePathIndex);

      WriteInteger(FPaths.Count);
      for I:=0 to FPaths.Count-1 do
        FPaths.Items[I].WriteToFiler(Writer);
    end;
  end;
end;

procedure TgxMovement.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  Path: TgxMovementPath;
  archiveVersion: Integer;
begin
  ClearPaths;
  with Reader do
  begin
    archiveVersion := ReadInteger;
    if archiveVersion >= 1 then
      inherited;
    if ReadBoolean then
    begin
      FAutoStartNextPath := ReadBoolean;
      FActivePathIndex := ReadInteger;

      Count := ReadInteger;
      for I:=0 to Count-1 do
      begin
        Path := AddPath;
        Path.ReadFromFiler(Reader);
      end;
    end else
    begin
      FAutoStartNextPath := True;
      FActivePathIndex := -1;
    end;
  end;
end;

procedure TgxMovement.ClearPaths;
begin
  StopPathTravel;
  FPaths.Clear;
  FActivePathIndex := -1;
end;

procedure TgxMovement.PathTravelStart(Sender: TObject);
begin
  if Assigned(FOnPathTravelStart) then
    FOnPathTravelStart(Self, TgxMovementPath(Sender));
end;

procedure TgxMovement.PathTravelStop(Sender: TObject);
begin
  if Assigned(FOnPathTravelStop) then
    FOnPathTravelStop(Self, TgxMovementPath(Sender), TgxMovementPath(Sender).FLooped);
  if TgxMovementPath(Sender).FLooped then
  begin
    //if looped, then re-start the path
    StartPathTravel;
  end
  else if (FActivePathIndex = FPaths.Count - 1) then
  begin
    if (Assigned(FOnAllPathTravelledOver)) then
      FOnAllPathTravelledOver(Self);
  end
  else //auto-start next path
  if FAutoStartNextPath then
  begin
    Inc(FActivePathIndex);
    StartPathTravel;
  end;
end;

function TgxMovement.GetSceneObject: TgxBaseSceneObject;
begin
  Result := TgxBaseSceneObject(Owner{TgxBehavours}.Owner);
end;

function TgxMovement.AddPath: TgxMovementPath;
var
  Path: TgxMovementPath;
begin
  Path   := FPaths.Add;
  Path.OnTravelStart := PathTravelStart;
  Path.OnTravelStop := PathTravelStop;
  Result := Path;
end;

function TgxMovement.AddPath(aObject: TgxBaseSceneObject): TgxMovementPath;
begin
  Result := AddPath;
  Result.AddNodeFromObject(aObject);
end;

function TgxMovement.AddPath(Path: TgxMovementPath): TgxMovementPath;
begin
  Result := AddPath;
  if Assigned(Path) then
    Result.Assign(Path);
end;

function TgxMovement.DeletePath: TgxMovementPath;
begin
  Result := DeletePath(FActivePathIndex);
end;

function TgxMovement.DeletePath(Path: TgxMovementPath): TgxMovementPath;
var
  I: integer;
begin
  Result := nil;
  for I := 0 to FPaths.Count - 1 do
  begin
    if Path = Paths[I] then
    begin
      Result := DeletePath(I);
      break;
    end;
  end;
end;

function TgxMovement.DeletePath(Index: integer): TgxMovementPath;
begin
  Result := nil;
  if (Index <0) or (Index >=FPaths.Count) then
    exit;

  if Index >=0 then
  begin
    TgxMovementPath(FPaths[Index]).Free;
    FPaths.Delete(Index);
    if FActivePathIndex < 0 then
      exit;
    if (Index =0) then
    begin
      if FPaths.Count > 0 then
        FActivePathIndex := 0
      else
        FActivePathIndex := -1;
    end 
    else
    begin
      //one has been deleted, so the index should be equal to FPathList.Count
      if Index =FPaths.Count then
        FActivePathIndex := Index -1
      else
        FActivePathIndex := Index;
    end;
    Result := ActivePath;
  end;
end;

procedure TgxMovement.SetActivePathIndex(Value: integer);
begin
  if FActivePathIndex = Value then
    exit;
  //if current has a Active path in travelling, then exit the method
  if (Assigned(ActivePath)) and (ActivePath.InTravel) then
    exit;
  if (Value >= 0) and (Value < FPaths.Count) then
  begin
    FActivePathIndex := Value;
    //Start the new path or wait for the start-command
  end 
  else if Value < 0 then
  begin
    FActivePathIndex := -1;
    //Stop all the running path
  end;
end;

function TgxMovement.NextPath: integer;
begin
  ActivePathIndex := FActivePathIndex + 1;
  Result           := FActivePathIndex;
end;

function TgxMovement.PrevPath: integer;
begin
  ActivePathIndex := FActivePathIndex - 1;
  if (FActivePathIndex < 0) and (FPaths.Count > 0) then
    Result := 0
  else
    Result := FActivePathIndex;
end;

function TgxMovement.FirstPath: integer;
begin
  if FPaths.Count > 0 then
    FActivePathIndex := 0;
  Result              := FActivePathIndex;
end;

function TgxMovement.LastPath: integer;
begin
  if FPaths.Count > 0 then
    FActivePathIndex := FPaths.Count - 1;
  Result              := FActivePathIndex;
end;

function TgxMovement.GetActivePath: TgxMovementPath;
begin
  if FActivePathIndex >= 0 then
    Result := Paths[FActivePathIndex]
  else
    Result := nil;
end;

procedure TgxMovement.SetActivePath(Value: TgxMovementPath);
var
  I: integer;
begin
  ActivePathIndex := -1;
  for I := 0 to FPaths.Count - 1 do
  begin
    if Value = Paths[I] then
    begin
      ActivePathIndex := I;
      break;
    end;
  end;
end;

function TgxMovement.GetPathCount: integer;
begin
  Result := FPaths.Count;
end;

procedure TgxMovement.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TgxMovement then
  begin
    ClearPaths;
    for I := 0 to TgxMovement(Source).PathCount - 1 do
    begin
      AddPath;
      Paths[I].Assign(TgxMovement(Source).Paths[I]);
    end;
    FAutoStartNextPath := TgxMovement(Source).FAutoStartNextPath;
  end;
end;

class function TgxMovement.FriendlyName: string;
begin
  Result := 'Movement controls'
end;

class function TgxMovement.FriendlyDescription: string;
begin
  Result := 'Object movement path controls'
end;

class function TgxMovement.UniqueItem: boolean;
begin
  Result := True;
end;

procedure TgxMovement.StartPathTravel;
begin
  if FActivePathIndex < 0 then
    exit;
  //convert the time to second
  Paths[FActivePathIndex].TravelPath(True, 0);
end;

procedure TgxMovement.StopPathTravel;
var
  I: Integer;
begin
  if FPaths.Count <> 0 then
    for I := 0 to FPaths.Count - 1 do
      Paths[I].TravelPath(False);
end;

//Calculate functions add into this method
procedure TgxMovement.DoProgress(const progressTime : TgxProgressTimes);
var
  Path: TgxMovementPath;
begin
  if (FActivePathIndex >= 0) and (Paths[FActivePathIndex].InTravel) then
    begin
      Path := Paths[FActivePathIndex];
      Path.CalculateState(progressTime.newTime);
      if Assigned(Path.CurrentNode) then
      begin
        if Owner.Owner is TgxBaseSceneObject then
          with TgxBaseSceneObject(Owner.Owner) do
          begin
            Position.AsVector := Path.CurrentNode.FPosition;
            Scale.AsVector    := Path.CurrentNode.FScale;

            case Path.FRotationMode of
              rmTurnPitchRoll:
              begin
                PitchAngle := Path.CurrentNode.PitchAngle;
                TurnAngle := Path.CurrentNode.TurnAngle;
                RollAngle := Path.CurrentNode.RollAngle;
              end;

              rmUpDirection:
              begin
                Direction.AsVector := Path.CurrentNode.FDirection;
                Up.AsVector := Path.CurrentNode.FUp;
              end;
            else
              Assert(False, strErrorEx + strUnknownType);
            end
          end;
      end;
    end;
end;


function GetMovement(const behaviours: TgxBehaviours): TgxMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TgxMovement);
  if i >= 0 then
    Result := TgxMovement(behaviours[i])
  else
    Result := nil;
end;

function GetMovement(const obj: TgxBaseSceneObject): TgxMovement; overload;
begin
  Result := GetMovement(obj.behaviours);
end;

function GetOrCreateMovement(const behaviours: TgxBehaviours): TgxMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TgxMovement);
  if i >= 0 then
    Result := TgxMovement(behaviours[i])
  else
    Result := TgxMovement.Create(behaviours);
end;

function GetOrCreateMovement(const obj: TgxBaseSceneObject): TgxMovement; overload;
begin
  Result := GetOrCreateMovement(obj.behaviours);
end;

procedure StartStopTravel(const Obj: TgxBaseSceneObject; Start: Boolean; ChangeCameras, ChangeObjects: Boolean);
var
  NewObj: TgxBaseSceneObject;
  I: Integer;
  Movement: TgxMovement;
begin
  if ((Obj is TgxCamera)and(ChangeCameras))or
     ((not(Obj is TgxCamera))and(ChangeObjects))  then
  begin
    Movement := GetMovement(Obj);
    if Assigned(Movement) then
      if Start then
      begin
        if (Movement.PathCount>0) and (Movement.ActivePathIndex=-1) then
          Movement.ActivePathIndex := 0;
        Movement.StartPathTravel;
      end else
        Movement.StopPathTravel;
  end;
  for I:=0 to Obj.Count-1 do
  begin
    NewObj := Obj.Children[I];
    StartStopTravel(NewObj, Start, ChangeCameras, ChangeObjects);
  end;
end;

procedure StartAllMovements(const Scene: TgxScene; const StartCamerasMove, StartObjectsMove: Boolean);
begin
  if Assigned(Scene) then
  begin
    if StartCamerasMove or StartObjectsMove then
      StartStopTravel(Scene.Objects, True, StartCamerasMove, StartObjectsMove);
  end;
end;

procedure StopAllMovements(const Scene: TgxScene; const StopCamerasMove, StopObjectsMove: Boolean);
begin
  if Assigned(Scene) then
  begin
    if StopCamerasMove or StopObjectsMove then
      StartStopTravel(Scene.Objects, False, StopCamerasMove, StopObjectsMove);
  end;
end;

// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------

  
  RegisterXCollectionItemClass(TgxMovement);

finalization

  UnregisterXCollectionItemClass(TgxMovement);

end.



