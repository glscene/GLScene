//
// The multimedia graphics platform GLScene https://github.com/glscene
//
unit GLS.Movement;

(*
   Movement path behaviour by Roger Cao
   Note: It is recommended to set TGLMovementPath.RotationMode = rmUpDirection,
   but the default value is rmTurnPitchRoll for backwards compatibility.
*)

interface

{$I GLScene.inc}

uses
  System.Classes,
  System.SysUtils,

  GLS.VectorTypes,
  GLS.OpenGLTokens,
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.VectorGeometry,
  GLS.XCollection,
  GLS.Spline,
  GLS.Objects,
  GLS.Strings,
  GLS.BaseClasses,
  GLS.Utils;

type

  TGLPathNode = class (TCollectionItem)
  private
    FPosition: TGLVector;
    FScale: TGLVector;
    FRotation: TGLVector;
    FDirection: TGLVector;
    FUp: TGLVector;
    FSpeed: single;
    procedure SetPositionAsVector(const Value: TGLVector);
    procedure SetRotationAsVector(const Value: TGLVector);
    procedure SetScaleAsVector(const Value: TGLVector);
    function GetPositionCoordinate(const Index: Integer): TGLFloat;
    procedure SetPositionCoordinate(const Index: integer; const AValue: TGLFloat);
    function GetRotationCoordinate(const Index: Integer): TGLFloat; inline;
    procedure SetRotationCoordinate(const Index: integer; const AValue: TGLFloat);
    function GetScaleCoordinate(const Index: Integer): TGLFloat; inline;
    procedure SetScaleCoordinate(const Index: integer; const AValue: TGLFloat);
    procedure SetSpeed(const Value: single);
    function GetDirectionCoordinate(const Index: Integer): TGLFloat; inline;
    procedure SetDirectionCoordinate(const Index: integer; const AValue: TGLFloat);
    function GetUpCoordinate(const Index: Integer): TGLFloat; inline;
    procedure SetUpCoordinate(const Index: integer; const AValue: TGLFloat);
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
    procedure InitializeByObject(const Obj: TGLBaseSceneObject);
    {Warning: does not take speed into account. }
    function EqualNode(const aNode: TGLPathNode): boolean;
    {Rotation.X means PitchAngle, Rotation.Y means TurnAngle, Rotation.Z means RollAngle.}
    property RotationAsVector: TGLVector Read FRotation Write SetRotationAsVector;
    property PositionAsVector: TGLVector Read FPosition Write SetPositionAsVector;
    property ScaleAsVector: TGLVector Read FScale Write SetScaleAsVector;
    property UpAsVector: TGLVector read FUp write FUp;
    property DirectionAsVector: TGLVector read FDirection write FDirection;
  published
    property X: TGLFloat index 0 Read GetPositionCoordinate Write SetPositionCoordinate;
    property Y: TGLFloat index 1 Read GetPositionCoordinate Write SetPositionCoordinate;
    property Z: TGLFloat index 2 Read GetPositionCoordinate Write SetPositionCoordinate;
    //Rotation.X means PitchAngle;
    //Rotation.Y means TurnAngle;
    //Rotation.Z means RollAngle;
    property PitchAngle: TGLFloat index 0 Read GetRotationCoordinate Write SetRotationCoordinate;
    property TurnAngle: TGLFloat index 1 Read GetRotationCoordinate Write SetRotationCoordinate;
    property RollAngle: TGLFloat index 2 Read GetRotationCoordinate Write SetRotationCoordinate;
    property ScaleX: TGLFloat index 0 Read GetScaleCoordinate Write SetScaleCoordinate;
    property ScaleY: TGLFloat index 1 Read GetScaleCoordinate Write SetScaleCoordinate;
    property ScaleZ: TGLFloat index 2 Read GetScaleCoordinate Write SetScaleCoordinate;
    property DirectionX: TGLFloat index 0 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property DirectionY: TGLFloat index 1 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property DirectionZ: TGLFloat index 2 Read GetDirectionCoordinate Write SetDirectionCoordinate;
    property UpX: TGLFloat index 0 Read GetUpCoordinate Write SetUpCoordinate;
    property UpY: TGLFloat index 1 Read GetUpCoordinate Write SetUpCoordinate;
    property UpZ: TGLFloat index 2 Read GetUpCoordinate Write SetUpCoordinate;
    property Speed: single Read FSpeed Write SetSpeed;
  end;

  TGLMovementRotationMode = (rmTurnPitchRoll, rmUpDirection);

  TGLMovementPath = class;

  TGLPathNodes = class (TOwnedCollection)
  protected
    procedure SetItems(const index: integer; const val: TGLPathNode);
    function GetItems(const index: integer): TGLPathNode;
  public
    constructor Create(aOwner: TGLMovementPath);
    function GetOwnerMovementPath: TGLMovementPath;
    function Add: TGLPathNode;
    function FindItemID(const ID: integer): TGLPathNode;
    property Items[const index: integer]: TGLPathNode Read GetItems Write SetItems; default;
    procedure NotifyChange; virtual;
  end;

  TGLMovement = class;
  TGLMovementPaths = class;

  TGLMovementPath = class(TCollectionItem)
  private
    FPathLine: TGLLines;
    FShowPath: Boolean;
    FPathSplineMode: TGLLineSplineMode;
    FNodes: TGLPathNodes;
    //All the time saved in ms
    FStartTimeApplied: Boolean;
    FStartTime: double;
    FInitialTime: Double;
    FEstimateTime: double;
    FCurrentNode: TGLPathNode;
    FInTravel: boolean;
    FLooped: boolean;
    FName: string;
    FRotationMode: TGLMovementRotationMode;
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
    procedure SetPathSplineMode(const Value: TGLLineSplineMode);
  protected
    procedure WriteToFiler(writer : TWriter);
    procedure ReadFromFiler(reader : TReader);
    function CanTravel: boolean;
    function GetCollection: TGLMovementPaths;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetMovement: TGLMovement;
    function AddNode: TGLPathNode; overload;
    function AddNode(const Node: TGLPathNode): TGLPathNode; overload;
    function AddNodeFromObject(const Obj: TGLBaseSceneObject): TGLPathNode;
    function InsertNodeFromObject(const Obj: TGLBaseSceneObject; const Index: integer): TGLPathNode;
    function InsertNode(const Node: TGLPathNode; const Index: integer): TGLPathNode; overload;
    function InsertNode(const Index: integer): TGLPathNode; overload;
    function DeleteNode(const Index: integer): TGLPathNode; overload;
    function DeleteNode(const Node: TGLPathNode): TGLPathNode; overload;
    procedure ClearNodes;
    procedure UpdatePathLine;
    function NodeDistance(const Node1, Node2: TGLPathNode): double;
    procedure CalculateState(const CurrentTime: double);
    procedure TravelPath(const Start: boolean); overload;
    procedure TravelPath(const Start: boolean; const aStartTime: double); overload;
    property NodeCount: integer Read GetNodeCount;
    property CurrentNode: TGLPathNode Read FCurrentNode;
    property InTravel: boolean Read FInTravel;
    function PrevNode: integer;
    function NextNode: integer;
    property CurrentNodeIndex: integer Read FCurrentNodeIndex Write SetCurrentNodeIndex;
    property OnTravelStart: TNotifyEvent Read FOnTravelStart Write FOnTravelStart;
    property OnTravelStop: TNotifyEvent Read FOnTravelStop Write FOnTravelStop;
  published
    property Name: string Read FName Write FName;
    {This property is currently ignored. }
    property PathSplineMode: TGLLineSplineMode read FPathSplineMode write SetPathSplineMode default lsmLines;
    property RotationMode: TGLMovementRotationMode read FRotationMode write FRotationMode default rmTurnPitchRoll;

    property StartTime: double Read FStartTime Write SetStartTime;
    property EstimateTime: double Read FEstimateTime;
    property Looped: boolean Read FLooped Write FLooped;
    property Nodes: TGLPathNodes Read FNodes;
    property ShowPath: Boolean read FShowPath write SetShowPath;
  end;

  TGLMovementPaths = class(TOwnedCollection)
  protected
    procedure SetItems(const index: integer; const val: TGLMovementPath);
    function GetItems(const index: integer): TGLMovementPath;
    function GetMovement: TGLMovement;
  public
    constructor Create(aOwner: TGLMovement);
    function Add: TGLMovementPath;
    function FindItemID(const ID: integer): TGLMovementPath;
    property Items[const index: integer]: TGLMovementPath Read GetItems Write SetItems; default;
    procedure NotifyChange; virtual;
  end;

  //Event for path related event
  TPathTravelStartEvent = procedure (Sender: TObject;
    Path: TGLMovementPath) of object;
  TPathTravelStopEvent = procedure (Sender: TObject;
    Path: TGLMovementPath; var Looped: boolean) of object;

  TGLMovement = class(TGLBehaviour)
  private
    FPaths: TGLMovementPaths;
    FAutoStartNextPath: boolean;
    FActivePathIndex: integer;

    FOnAllPathTravelledOver: TNotifyEvent;
    FOnPathTravelStart: TPathTravelStartEvent;
    FOnPathTravelStop: TPathTravelStopEvent;
    (*
    function GetMovementPath(Index: integer): TGLMovementPath;
    procedure SetMovementPath(Index: integer; AValue: TGLMovementPath);
    *)
    function GetPathCount: integer;
    procedure SetActivePathIndex(Value: integer);

    function GetActivePath: TGLMovementPath;
    procedure SetActivePath(Value: TGLMovementPath);
  protected
    procedure WriteToFiler(writer : TWriter); override;
    procedure ReadFromFiler(reader : TReader); override;
    procedure PathTravelStart(Sender: TObject);
    procedure PathTravelStop(Sender: TObject);
    function GetSceneObject: TGLBaseSceneObject;
  public
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;

    //add an empty path;
    function AddPath: TGLMovementPath; overload;
    //add an path with one node, and the node is based on aObject
    function AddPath(aObject: TGLBaseSceneObject): TGLMovementPath; overload;
    //add one path to the new one
    function AddPath(Path: TGLMovementPath): TGLMovementPath; overload;
    procedure ClearPaths;
    //Result is current path
    function DeletePath(Path: TGLMovementPath): TGLMovementPath; overload;
    function DeletePath(Index: integer): TGLMovementPath; overload;
    function DeletePath: TGLMovementPath; overload;

    procedure Assign(Source: TPersistent); override;
    class function FriendlyName: string; override;
    class function FriendlyDescription: string; override;
    class function UniqueItem: boolean; override;

    procedure StartPathTravel;
    procedure StopPathTravel;
    procedure DoProgress(const progressTime : TGLProgressTimes); override;
    function NextPath: integer;
    function PrevPath: integer;
    function FirstPath: integer;
    function LastPath: integer;
    //property Paths[index: Integer]: TGLMovementPath read GetMovementPath write SetMovementPath;
    property PathCount: integer Read GetPathCount;
    //why do these property can't be saved in IDE ?
    property OnAllPathTravelledOver: TNotifyEvent Read FOnAllPathTravelledOver Write FOnAllPathTravelledOver;
    property OnPathTravelStart: TPathTravelStartEvent Read FOnPathTravelStart Write FOnPathTravelStart;
    property OnPathTravelStop: TPathTravelStopEvent Read FOnPathTravelStop Write FOnPathTravelStop;
  published
    property Paths: TGLMovementPaths Read FPaths;
    property AutoStartNextPath: boolean Read FAutoStartNextPath Write FAutoStartNextPath;
    property ActivePathIndex: integer Read FActivePathIndex Write SetActivePathIndex;
    property ActivePath: TGLMovementPath Read GetActivePath Write SetActivePath;
  end;

function GetMovement(const behaviours: TGLBehaviours): TGLMovement; overload;
function GetMovement(const obj: TGLBaseSceneObject): TGLMovement; overload;
function GetOrCreateMovement(const behaviours: TGLBehaviours): TGLMovement; overload;
function GetOrCreateMovement(const obj: TGLBaseSceneObject): TGLMovement; overload;
procedure StartAllMovements(const Scene: TGLScene; const StartCamerasMove, StartObjectsMove: Boolean);
procedure StopAllMovements(const Scene: TGLScene; const StopCamerasMove, StopObjectsMove: Boolean);

// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------

 //----------------------------- TGLPathNode ------------------------------------
constructor TGLPathNode.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPosition  := VectorMake(0, 0, 0, 1);
  FRotation  := VectorMake(0, 0, 0, 1);
  FScale     := VectorMake(1, 1, 1, 1);
  FDirection := ZHmgVector;
  FUp        := YHmgVector;
  FSpeed     := 0;
end;

destructor TGLPathNode.Destroy;
begin
  inherited Destroy;
end;

procedure TGLPathNode.SetPositionAsVector(const Value: TGLVector);
begin
  FPosition := Value;
    (Collection as TGLPathNodes).NotifyChange;
end;

procedure TGLPathNode.SetRotationAsVector(const Value: TGLVector);
begin
  FRotation := Value;
    (Collection as TGLPathNodes).NotifyChange;
end;

procedure TGLPathNode.SetScaleAsVector(const Value: TGLVector);
begin
  FScale := Value;
    (Collection as TGLPathNodes).NotifyChange;
end;

function TGLPathNode.PositionAsAddress: PGLFloat;
begin
  Result := @FPosition;
end;

function TGLPathNode.RotationAsAddress: PGLFloat;
begin
  Result := @FRotation;
end;

function TGLPathNode.ScaleAsAddress: PGLFloat;
begin
  Result := @FScale;
end;

procedure TGLPathNode.WriteToFiler(writer : TWriter);
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

procedure TGLPathNode.ReadFromFiler(reader : TReader);
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

procedure TGLPathNode.InitializeByObject(const Obj: TGLBaseSceneObject);
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

procedure TGLPathNode.Assign(Source: TPersistent);
begin
  if Source is TGLPathNode then
  begin
    FPosition := TGLPathNode(Source).FPosition;
    FRotation := TGLPathNode(Source).FRotation;
    FScale    := TGLPathNode(Source).FScale;
    FSpeed    := TGLPathNode(Source).FSpeed;

    FDirection := TGLPathNode(Source).FDirection;
    FUp        := TGLPathNode(Source).FUp;
  end else
    inherited Assign(Source);
end;

function TGLPathNode.EqualNode(const aNode: TGLPathNode): boolean;
begin
  Result := VectorEquals(FPosition, aNode.FPosition) and
            VectorEquals(FRotation, aNode.FRotation) and
            VectorEquals(FScale, aNode.FScale) and
            VectorEquals(FDirection, aNode.FDirection) and
            VectorEquals(FUp, aNode.FUp);
end;

procedure TGLPathNode.SetSpeed(const Value: single);
begin
  FSpeed := Value;
end;

function TGLPathNode.GetDisplayName: string;
begin
  Result := 'PathNode';
end;

function TGLPathNode.GetPositionCoordinate(const Index: Integer): TGLFloat;
begin
  result := FPosition.V[Index];
end;

procedure TGLPathNode.SetPositionCoordinate(const Index: integer; const AValue: TGLFloat);
begin
  FPosition.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TGLPathNodes).NotifyChange;
end;

function TGLPathNode.GetRotationCoordinate(const Index: Integer): TGLFloat;
begin
  result := FRotation.V[Index];
end;

procedure TGLPathNode.SetRotationCoordinate(const Index: integer; const AValue: TGLFloat);
begin
  FRotation.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TGLPathNodes).NotifyChange;
end;

function TGLPathNode.GetScaleCoordinate(const Index: Integer): TGLFloat;
begin
  result := FScale.V[Index];
end;

procedure TGLPathNode.SetScaleCoordinate(const Index: integer; const AValue: TGLFloat);
begin
  FScale.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TGLPathNodes).NotifyChange;
end;

function TGLPathNode.GetDirectionCoordinate(const Index: Integer): TGLFloat;
begin
  result := FDirection.V[Index];
end;

procedure TGLPathNode.SetDirectionCoordinate(const Index: integer;
  const AValue: TGLFloat);
begin
  FDirection.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TGLPathNodes).NotifyChange;
end;

function TGLPathNode.GetUpCoordinate(const Index: Integer): TGLFloat;
begin
  result := FUp.V[Index];
end;

procedure TGLPathNode.SetUpCoordinate(const Index: integer; const AValue: TGLFloat);
begin
  FUp.V[Index] := AValue;
  if Collection <> nil then
    (Collection as TGLPathNodes).NotifyChange;
end;

//--------------------------- TGLPathNodes -------------------------------------
constructor TGLPathNodes.Create(aOwner: TGLMovementPath);
begin
  inherited Create(aOwner, TGLPathNode);
end;

procedure TGLPathNodes.SetItems(const index: integer; const val: TGLPathNode);
begin
  inherited Items[index] := val;
end;

function TGLPathNodes.GetItems(const index: integer): TGLPathNode;
begin
  Result := TGLPathNode(inherited Items[index]);
end;

function TGLPathNodes.Add: TGLPathNode;
begin
  Result := (inherited Add) as TGLPathNode;
end;

function TGLPathNodes.GetOwnerMovementPath: TGLMovementPath;
begin
  Result := TGLMovementPath(GetOwner);
end;

function TGLPathNodes.FindItemID(const ID: integer): TGLPathNode;
begin
  Result := (inherited FindItemID(ID)) as TGLPathNode;
end;

procedure TGLPathNodes.NotifyChange;
begin
  // Update the path-line if avalible in TGLMovementPath.
  GetOwnerMovementPath.UpdatePathLine;
end;

//--------------------------- TGLMovementPath ----------------------------------
constructor TGLMovementPath.Create(Collection: TCollection);
begin
  // This object can only be added to a TGLMovement class.
  inherited Create(Collection);

  FNodes := TGLPathNodes.Create(Self);
  FCurrentNodeIndex := -1;
  FRotationMode := rmTurnPitchRoll;
  FPathSplineMode := lsmCubicSpline;
  FStartTimeApplied := False;  
end;

destructor TGLMovementPath.Destroy;
begin
  // Make sure the splines are freed.
  FLooped:= false;
  
  ClearNodes;
  FNodes.Free;

  inherited Destroy;
end;

procedure TGLMovementPath.WriteToFiler(writer : TWriter);
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

procedure TGLMovementPath.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  Node: TGLPathNode;
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
        FRotationMode := TGLMovementRotationMode(ReadInteger);
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

procedure TGLMovementPath.SetPathSplineMode(const Value: TGLLineSplineMode);
begin
  if Value<>FPathSplineMode then
  begin
    FPathSplineMode := Value;
    if FShowPath then
      FPathLine.SplineMode := FPathSplineMode;
  end;
end;

procedure TGLMovementPath.UpdatePathLine;
var
  I: Integer;
  Node: TGLPathNode;
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

procedure TGLMovementPath.SetShowPath(const Value: Boolean);
var
  OwnerObj: TGLBaseSceneObject;
begin
  if FShowPath<>Value then
  begin
    FShowPath := Value;
    OwnerObj := GetMovement.GetSceneObject;
    if FShowPath then
    begin
      FPathLine := TGLLines.Create(OwnerObj);
      MakeSubComponent(FPathLine, True);
      OwnerObj.Scene.Objects.AddChild(FPathLine);
      FPathLine.SplineMode := FPathSplineMode;
      UpdatePathLine;
    end
    else
      FreeAndNil(FPathLine);
  end;
end;

procedure TGLMovementPath.ClearNodes;
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

procedure TGLMovementPath.SetCurrentNodeIndex(const Value: integer);
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
      FCurrentNode := TGLPathNode.Create(nil);
    FCurrentNode.Assign(Nodes[FCurrentNodeIndex]);
  end;
end;

function TGLMovementPath.InsertNode(const Node: TGLPathNode; const Index: integer): TGLPathNode;
var
  N: TGLPathNode;
begin
  Result := nil;
  //Intravel, can't insert
  if FInTravel then
    exit;
  //Insert into the position
  if (Assigned(Node)) and (Assigned(Nodes[Index])) then
  begin
    N := TGLPathNode(FNodes.Insert(Index));
    if Index >0 then
      N.Assign(Nodes[Index -1]);
  end
  else
    //add to the tail of list
    N    := FNodes.Add;
  Result := N;
  UpdatePathLine;
end;

function TGLMovementPath.InsertNode(const Index: integer): TGLPathNode;
var
  N: TGLPathNode;
begin
  Result := nil;
  //Intravel, can't insert
  if FInTravel then
    exit;
  //Insert into the position
  if (Assigned(Nodes[Index])) then
  begin
    N := TGLPathNode(FNodes.Insert(Index));
    if Index >0 then
      N.Assign(Nodes[Index -1]);
    Result := N;
  end
  else
    //add to the tail of list
    Result := AddNode;
  UpdatePathLine;
end;

function TGLMovementPath.AddNodeFromObject(const Obj: TGLBaseSceneObject): TGLPathNode;
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

function TGLMovementPath.InsertNodeFromObject(const Obj: TGLBaseSceneObject; const Index: integer): TGLPathNode;
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

function TGLMovementPath.DeleteNode(const Index: integer): TGLPathNode;
var
  Node: TGLPathNode;
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

function TGLMovementPath.DeleteNode(const Node: TGLPathNode): TGLPathNode;
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

function TGLMovementPath.PrevNode: integer;
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

function TGLMovementPath.NextNode: integer;
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

function TGLMovementPath.NodeDistance(const Node1, Node2: TGLPathNode): double;
begin
  Result := VectorDistance(Node1.FPosition, Node2.FPosition);
end;

//need to do
//1 No acceleration implemented
//2 The travel-time of a segment is based a simple linear movement, at the start and the end
//  of the segment, the speed will be more high than in the middle
//3 Rotation Interpolation has not been tested
procedure TGLMovementPath.CalculateState(const CurrentTime: double);
var
  I:       integer;
  SumTime: double;
  L, L2:       single;
  Interpolated: boolean;
  T:       double;
  a:double;

  procedure Interpolation(ReturnNode: TGLPathNode; Time1, Time2: double; Index: integer);
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


function TGLMovementPath.CanTravel: boolean;
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

function TGLMovementPath.GetCollection: TGLMovementPaths;
begin
  Result := TGLMovementPaths(GetOwner);
end;

function TGLMovementPath.GetMovement: TGLMovement;
begin
  Result := GetCollection.GetMovement;
end;

procedure TGLMovementPath.TravelPath(const Start: boolean);
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
    FCurrentNode := TGLPathNode.Create(nil);
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

procedure TGLMovementPath.TravelPath(const Start: boolean; const aStartTime: double);
begin
  if FInTravel = Start then
    exit;
  FInitialTime := aStartTime;
  FStartTimeApplied := False;
  TravelPath(Start);
end;

function TGLMovementPath.GetNodeCount: integer;
begin
  Result := FNodes.Count;
end;

//-------------------------- This function need modified -----------------------
procedure TGLMovementPath.SetStartTime(const Value: double);
begin
  FStartTime := Value;
end;

procedure TGLMovementPath.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TGLMovementPath then
  begin
    ClearNodes;
    for I := 0 to TGLMovementPath(Source).NodeCount - 1 do
    begin
      AddNode;
      Nodes[I].Assign(TGLMovementPath(Source).Nodes[I]);
      FStartTime := TGLMovementPath(Source).FStartTime;
      //FEstimateTime := TGLMovementPath(Source).FEstimateTime;
      FLooped := TGLMovementPath(Source).FLooped;
      FRotationMode := TGLMovementPath(Source).FRotationMode;
    end;
  end;
end;

function TGLMovementPath.AddNode: TGLPathNode;
var
  Node: TGLPathNode;
  I:    integer;
begin
  //Add a empty node, if it's not the first one, try locate the node to the previous one
  Node := FNodes.Add;
  I    := FNodes.Count;
  if I > 1 then
    Node.Assign(Nodes[I - 2]);
  Result := Node;
end;

function TGLMovementPath.AddNode(const Node: TGLPathNode): TGLPathNode;
begin
  Result := AddNode;
  if Assigned(Node) then
    Result.Assign(Node);
end;

//------------------------- TGLMovementPaths ----------------------------------
constructor TGLMovementPaths.Create(aOwner: TGLMovement);
begin
  inherited Create(aOwner, TGLMovementPath);
end;

procedure TGLMovementPaths.SetItems(const index: integer; const val: TGLMovementPath);
begin
  inherited Items[index] := val;
end;

function TGLMovementPaths.GetItems(const index: integer): TGLMovementPath;
begin
  Result := TGLMovementPath(inherited Items[index]);
end;

function TGLMovementPaths.Add: TGLMovementPath;
begin
  Result := (inherited Add) as TGLMovementPath;
end;

function TGLMovementPaths.FindItemID(const ID: integer): TGLMovementPath;
begin
  Result := (inherited FindItemID(ID)) as TGLMovementPath;
end;

procedure TGLMovementPaths.NotifyChange;
begin
  // Do nothing here.
end;

function TGLMovementPaths.GetMovement: TGLMovement;
begin
  Result := TGLMovement(GetOwner);
end;


//--------------------------- TGLMovement --------------------------------------
constructor TGLMovement.Create(aOwner: TXCollection);
begin
  inherited Create(aOwner);
  FPaths           := TGLMovementPaths.Create(Self);
  FAutoStartNextPath := True;
  FActivePathIndex := -1;
  FOnAllPathTravelledOver := nil;
  FOnPathTravelStart := nil;
  FOnPathTravelStop := nil;
end;

destructor TGLMovement.Destroy;
begin
  ClearPaths;
  FPaths.Free;
  inherited Destroy;
end;

procedure TGLMovement.WriteToFiler(writer : TWriter);
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

procedure TGLMovement.ReadFromFiler(reader : TReader);
var
  I: Integer;
  Count: Integer;
  Path: TGLMovementPath;
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

procedure TGLMovement.ClearPaths;
begin
  StopPathTravel;
  FPaths.Clear;
  FActivePathIndex := -1;
end;

procedure TGLMovement.PathTravelStart(Sender: TObject);
begin
  if Assigned(FOnPathTravelStart) then
    FOnPathTravelStart(Self, TGLMovementPath(Sender));
end;

procedure TGLMovement.PathTravelStop(Sender: TObject);
begin
  if Assigned(FOnPathTravelStop) then
    FOnPathTravelStop(Self, TGLMovementPath(Sender), TGLMovementPath(Sender).FLooped);
  if TGLMovementPath(Sender).FLooped then
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

function TGLMovement.GetSceneObject: TGLBaseSceneObject;
begin
  Result := TGLBaseSceneObject(Owner{TGLBehavours}.Owner);
end;

function TGLMovement.AddPath: TGLMovementPath;
var
  Path: TGLMovementPath;
begin
  Path   := FPaths.Add;
  Path.OnTravelStart := PathTravelStart;
  Path.OnTravelStop := PathTravelStop;
  Result := Path;
end;

function TGLMovement.AddPath(aObject: TGLBaseSceneObject): TGLMovementPath;
begin
  Result := AddPath;
  Result.AddNodeFromObject(aObject);
end;

function TGLMovement.AddPath(Path: TGLMovementPath): TGLMovementPath;
begin
  Result := AddPath;
  if Assigned(Path) then
    Result.Assign(Path);
end;

function TGLMovement.DeletePath: TGLMovementPath;
begin
  Result := DeletePath(FActivePathIndex);
end;

function TGLMovement.DeletePath(Path: TGLMovementPath): TGLMovementPath;
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

function TGLMovement.DeletePath(Index: integer): TGLMovementPath;
begin
  Result := nil;
  if (Index <0) or (Index >=FPaths.Count) then
    exit;

  if Index >=0 then
  begin
    TGLMovementPath(FPaths[Index]).Free;
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

procedure TGLMovement.SetActivePathIndex(Value: integer);
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

function TGLMovement.NextPath: integer;
begin
  ActivePathIndex := FActivePathIndex + 1;
  Result           := FActivePathIndex;
end;

function TGLMovement.PrevPath: integer;
begin
  ActivePathIndex := FActivePathIndex - 1;
  if (FActivePathIndex < 0) and (FPaths.Count > 0) then
    Result := 0
  else
    Result := FActivePathIndex;
end;

function TGLMovement.FirstPath: integer;
begin
  if FPaths.Count > 0 then
    FActivePathIndex := 0;
  Result              := FActivePathIndex;
end;

function TGLMovement.LastPath: integer;
begin
  if FPaths.Count > 0 then
    FActivePathIndex := FPaths.Count - 1;
  Result              := FActivePathIndex;
end;

function TGLMovement.GetActivePath: TGLMovementPath;
begin
  if FActivePathIndex >= 0 then
    Result := Paths[FActivePathIndex]
  else
    Result := nil;
end;

procedure TGLMovement.SetActivePath(Value: TGLMovementPath);
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

function TGLMovement.GetPathCount: integer;
begin
  Result := FPaths.Count;
end;

procedure TGLMovement.Assign(Source: TPersistent);
var
  I: integer;
begin
  if Source is TGLMovement then
  begin
    ClearPaths;
    for I := 0 to TGLMovement(Source).PathCount - 1 do
    begin
      AddPath;
      Paths[I].Assign(TGLMovement(Source).Paths[I]);
    end;
    FAutoStartNextPath := TGLMovement(Source).FAutoStartNextPath;
  end;
end;

class function TGLMovement.FriendlyName: string;
begin
  Result := 'Movement controls'
end;

class function TGLMovement.FriendlyDescription: string;
begin
  Result := 'Object movement path controls'
end;

class function TGLMovement.UniqueItem: boolean;
begin
  Result := True;
end;

procedure TGLMovement.StartPathTravel;
begin
  if FActivePathIndex < 0 then
    exit;
  //convert the time to second
  Paths[FActivePathIndex].TravelPath(True, 0);
end;

procedure TGLMovement.StopPathTravel;
var
  I: Integer;
begin
  if FPaths.Count <> 0 then
    for I := 0 to FPaths.Count - 1 do
      Paths[I].TravelPath(False);
end;

//Calculate functions add into this method
procedure TGLMovement.DoProgress(const progressTime : TGLProgressTimes);
var
  Path: TGLMovementPath;
begin
  if (FActivePathIndex >= 0) and (Paths[FActivePathIndex].InTravel) then
    begin
      Path := Paths[FActivePathIndex];
      Path.CalculateState(progressTime.newTime);
      if Assigned(Path.CurrentNode) then
      begin
        if Owner.Owner is TGLBaseSceneObject then
          with TGLBaseSceneObject(Owner.Owner) do
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


function GetMovement(const behaviours: TGLBehaviours): TGLMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLMovement);
  if i >= 0 then
    Result := TGLMovement(behaviours[i])
  else
    Result := nil;
end;

function GetMovement(const obj: TGLBaseSceneObject): TGLMovement; overload;
begin
  Result := GetMovement(obj.behaviours);
end;

function GetOrCreateMovement(const behaviours: TGLBehaviours): TGLMovement; overload;
var
  i: integer;
begin
  i := behaviours.IndexOfClass(TGLMovement);
  if i >= 0 then
    Result := TGLMovement(behaviours[i])
  else
    Result := TGLMovement.Create(behaviours);
end;

function GetOrCreateMovement(const obj: TGLBaseSceneObject): TGLMovement; overload;
begin
  Result := GetOrCreateMovement(obj.behaviours);
end;

procedure StartStopTravel(const Obj: TGLBaseSceneObject; Start: Boolean; ChangeCameras, ChangeObjects: Boolean);
var
  NewObj: TGLBaseSceneObject;
  I: Integer;
  Movement: TGLMovement;
begin
  if ((Obj is TGLCamera)and(ChangeCameras))or
     ((not(Obj is TGLCamera))and(ChangeObjects))  then
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

procedure StartAllMovements(const Scene: TGLScene; const StartCamerasMove, StartObjectsMove: Boolean);
begin
  if Assigned(Scene) then
  begin
    if StartCamerasMove or StartObjectsMove then
      StartStopTravel(Scene.Objects, True, StartCamerasMove, StartObjectsMove);
  end;
end;

procedure StopAllMovements(const Scene: TGLScene; const StopCamerasMove, StopObjectsMove: Boolean);
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

  RegisterXCollectionItemClass(TGLMovement);

finalization

  UnregisterXCollectionItemClass(TGLMovement);

end.



