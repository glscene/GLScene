//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.CameraController;

(*
  Component for animating camera movement.
  Can be used to zoom in/out, for linear movement, orbiting and Google Earth - like "fly-to"
  Main purpose was the SafeOrbitAndZoomToPos method, the others are usable as well

IMPORTANT!
You should block user GUI access to the GLSceneViewer
while movement is being done, check the AllowUserAction property!
Block user GUI access while AllowUserAction is false to avoid behaviour errors
simply put
if GLCameraController1.AllowUserAction then
do whatever you want on mouse move, form wheel etc

methods and properties are explained in the interface section (through comments)
additional comments might apear in implementation section where needed
*)

interface

uses
  System.Classes, 
  System.SysUtils, 
  System.Math, 
  System.Contnrs,
  
  GXS.PersistentClasses,
  GXS.Scene,
  GXS.VectorGeometry,
  GXS.Coordinates,
  GXS.SmoothNavigator, 
  GXS.VectorTypes;

type

  EGLCameraController = class(Exception);

  // Forward declaration of the camera controller main class
  TgxCameraController = class;

  // Forward declaration of a generic camera job
  TgxCameraJob = class;

  TgxCameraJobList = class(TObjectList)
  private
    FController : TgxCameraController;
    function GetCameraJob(const AIndex: integer): TgxCameraJob;
    procedure SetCameraJob(const AIndex: integer; const Value: TgxCameraJob);
  public
    constructor Create(AController: TgxCameraController);
    function Add(ACameraJob: TgxCameraJob): integer;
    property Items[const AIndex: integer]: TgxCameraJob read GetCameraJob write SetCameraJob; default;
    function First: TgxCameraJob;
    function Last: TgxCameraJob;
  end;

  TgxCameraJob = class(TObject)
  private
    FJoblist : TgxCameraJobList;
  protected
    FAbort         : boolean;
    FInit          : boolean;
    FRunning       : Boolean;

    FElapsedTime   : Double;
    FDeltaTime      : Double;
    FStartTime     : Double;
    FProceedTime   : Double;
  public
    constructor Create(const AJoblist : TgxCameraJobList); virtual;
    destructor Destroy; override;

    procedure Abort;
    procedure Step; virtual; abstract;
    procedure Init; virtual; abstract;

    property Running: Boolean read FRunning write FRunning;
    property ElapsedTime: Double read FElapsedTime write FElapsedTime;
    property StartTime: Double read FStartTime write FStartTime;
    property ProceedTime: Double read FProceedTime write FProceedTime;
  end;

  TgxMoveToPosJob = class(TgxCameraJob)
  private
    FInitialPos    : TVector4f;
    FFinalPos      : TVector4f;
  public
    X : Double;
    Y : Double;
    Z : Double;
    Time : Double;
    procedure Step; override;
    procedure Init; override;

    // Properties.
    property InitialPos: TVector4f read FInitialPos;
    property FinalPos: TVector4f read FFinalPos;    
  end;

  TgxZoomToDistanceJob = class(TgxCameraJob)
  private
    FInitialPos    : TVector4f;
    FFinalPos      : TVector4f;
  public
    Distance : Double;
    Time : Double;
    procedure Step; override;
    procedure Init; override;

    // Properties.
    property InitialPos: TVector4f read FInitialPos;
    property FinalPos: TVector4f read FFinalPos;
  end;

  TgxOrbitToPosJob = class(TgxCameraJob)
  private
    FFinalPos: TVector4f; // Yep, FFinalPos is stored in relative coordinates.
    FRotateSpeed: TVector2f;
    FCameraUpVector: TVector4f;

    // Absolute Coordinates, can even be not normalized by radius.
    // Procesed in Init, not used anywhere else.
    FTargetPosition: TVector4f;
    FTime : Double;
  public
    procedure Step; override;
    procedure Init; override;

    // Properties.
    property RotateSpeed: TVector2f read FRotateSpeed;
    property CameraUpVector: TVector4f read FCameraUpVector;
    property TargetPosition: TVector4f read FTargetPosition;
    property FinalPos: TVector4f read FFinalPos;
    property Time: Double read FTime;
  end;

  TgxSmoothOrbitToPos = class(TgxOrbitToPosJob)
  private
    FCutoffAngle: Single;
    FNeedToRecalculateZoom: Boolean;
    FShouldBeMatrix: TMatrix4f;
    FSmoothNavigator: TgxNavigatorSmoothChangeVector;
  public
    constructor Create(const AJoblist : TgxCameraJobList); override;
    procedure Step; override;
    property CutoffAngle: Single read FCutoffAngle write FCutoffAngle;
    property NeedToRecalculateZoom: Boolean read FNeedToRecalculateZoom write FNeedToRecalculateZoom;
  end;

  TgxOrbitToPosAdvJob = class(TgxCameraJob)
  private
    FInitialPos    : TVector4f;
    FFinalPos      : TVector4f;
    FInitialUp     : TVector4f;
    FInitialDir    : TVector4f;

    FRotAxis : TVector4f;
    FAngle   : Double;
  public
    X : Double;
    Y : Double;
    Z : Double;
    Time : Double;
    PreferUpAxis : Boolean;
    procedure Step; override;
    procedure Init; override;

    // Properties.
    property InitialPos: TVector4f read FInitialPos;
    property InitialUp: TVector4f read FInitialUp;
    property InitialDir: TVector4f read FInitialDir;
    property FinalPos: TVector4f read FFinalPos;
  end;

  TgxSmoothOrbitToPosAdvJob = class(TgxOrbitToPosAdvJob)
  private
    FPreviousPosition: TVector4f;
    FSmoothNavigator: TgxNavigatorSmoothChangeVector;
    FRestoreUpVector: Boolean;
  public
    procedure Step; override;
    procedure Init; override;
  end;

  TgxCameraJobEvent = procedure(Sender : TgxCameraJob) of object;

  TgxCameraController = class(TComponent)
  private
    // Objects.
    FCameraJobList : TgxCameraJobList;
    FCamera: TgxBaseSceneObject;
    FCameraTarget: TgxBaseSceneObject;

    // Events.
    FOnJobAdded: TgxCameraJobEvent;
    FOnJobFinished: TgxCameraJobEvent;
    FOnJobStep: TgxCameraJobEvent;

    //fields used by SafeOrbitAndZoomToPos
    FsoSafeDist, FsoTimeToSafePlacement, FsoTimeToOrbit, FsoTimeToZoomBackIn:double;

    //private methods
    //used to test whether camera and cadencer are assigned
    //Extended = true -> will test also for Camera.TargetObject
    procedure CheckAssignments(Extended: boolean);

    //after AdjustScene the Camera.DepthofView will be modified
    //if you want to zoom back in from GUI
    //you should use something like
    //  Camera.DepthOfView:=2*Camera.DistanceToTarget+2*camera.TargetObject.BoundingSphereRadius;
    procedure SetOnJobAdded(const Value: TgxCameraJobEvent);
    procedure SetOnJobFinished(const Value: TgxCameraJobEvent);
    procedure SetOnJobStep(const Value: TgxCameraJobEvent);
    procedure SetCamera(const Value: TgxBaseSceneObject);
    procedure SetCameraTarget(const Value: TgxBaseSceneObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    // Constructor.
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    //methods
    //linear movement from current pos
    function MoveToPos(x,y,z,time:double): TgxMoveToPosJob;

    //orbiting from current pos to the pos where
    //the camera points at the camera.targetObject TROUGH the given point
    //it will not move to the given point(!), use SafeOrbitAndZoomToPos instead
    //there has to be a camera.targetObject assigned!
    function OrbitToPos(x,y,z,time:double): TgxOrbitToPosJob;

    // Same as OrbitToPos(), but makes use of SmoothNavigator to make
    // sure all camera movements are smooth.
    function OrbitToPosSmooth(const ATargetPosition: TVector4f; const ATime: Double;
      const ASmoothNavigator: TgxNavigatorSmoothChangeVector; const AFNeedToRecalculateZoom: Boolean;
      const ACameraUpVector: PVector4f = nil): TgxSmoothOrbitToPos;

    //Same function as OrbitToPos but support all camera states
    //PreferUpAxis value is to setup if function use Camera Up based rotation axis
    //instead of Camera direction based rotation axis when destination and camera
    //position are opposite from Camera Target
    function OrbitToPosAdvanced(x,y,z,time:double; PreferUpAxis: Boolean = True): TgxOrbitToPosAdvJob;


    // Same as OrbitToPosAdvanced(), but makes use of SmoothNavigator to make
    // sure all camera movements are smooth.
    function OrbitToPosAdvancedSmooth(const x,y,z, time: double;
      const ASmoothNavigator: TgxNavigatorSmoothChangeVector;
      const PreferUpAxis: Boolean = True): TgxSmoothOrbitToPosAdvJob;

    //zooms in/out by moving to the given distance from camera.targetObject
    //there has to be a camera.targetObject assigned!
    function ZoomToDistance(Distance,Time:double): TgxZoomToDistanceJob;

    //google earth - like "fly-to"
    // = zoom out to safe distance, orbit, and then zoom in to the given point
    //there has to be a camera.targetObject assigned!
    procedure SafeOrbitAndZoomToPos(x,y,z:double);

    //Dan Bartlett said in the GLScene newsgroup that it might be a good idea
    //to introduce ability to stop movement and return control to user
    //here it is
    procedure StopMovement;

    // Called by the cadencer to animate the camera
    procedure Step(const deltaTime, newTime: Double);

    property CameraJobList: TgxCameraJobList read FCameraJobList;
  published
    // Assign a Moving object (usually a TgxCamera).
    property Camera: TgxBaseSceneObject read FCamera write SetCamera;

    // Assign a target, around which Moving object should rotate(usually TgxCamera.TargetObject).
    property CameraTarget: TgxBaseSceneObject read FCameraTarget write SetCameraTarget;

    //specifies whether user should be able interract with the GLSceneViewer
    //it is set to false while the camera is moving and
    //coders should check this value and block GUI access to GLSceneViewer
    //property AllowUserAction:boolean read FAllowUserAction;

    //safe distance to avoid moving the camera trough the camera.targetObject
    //while performing  SafeOrbitAndZoomToPos
    property soSafeDistance:double read FsoSafeDist write FsoSafeDist;
    //time to zoom in/out to the safe position while performing  SafeOrbitAndZoomToPos
    property soTimeToSafePlacement:double read FsoTimeToSafePlacement write FsoTimeToSafePlacement;
    //time to orbit while performing  SafeOrbitAndZoomToPos
    property soTimeToOrbit:double read FsoTimeToOrbit write FsoTimeToOrbit;
    //time to zoom in/out to the given final position while performing  SafeOrbitAndZoomToPos
    property soTimeToZoomBackIn:double read FsoTimeToZoomBackIn write FsoTimeToZoomBackIn;

    //this event is triggered when a job is init
    property OnJobAdded : TgxCameraJobEvent read FOnJobAdded write SetOnJobAdded;

    //this event is triggered when a job is step (like an OnMove)
    property OnJobStep : TgxCameraJobEvent read FOnJobStep write SetOnJobStep;

    //this event is triggered when a job is finished (not canceled)
    property OnJobFinished : TgxCameraJobEvent read FOnJobFinished write SetOnJobFinished;
  end;

implementation


const
  cGLCAMERACONTROLLER_CHECK_EXTENDED = TRUE;
  cEPSILON = 0.001;

{ TgxCameraController }

constructor TgxCameraController.Create(AOwner:TComponent);
begin
  inherited;
  //create the job list container
  FCameraJobList := TgxCameraJobList.Create(Self);
  FCameraJobList.OwnsObjects := true;

  //initialize values
  soSafeDistance:=10;
  soTimeToSafePlacement:=1;
  soTimeToOrbit:=2;
  soTimeToZoomBackIn:=1;
end;

destructor TgxCameraController.Destroy;
begin
  //delete job list and all jobs inside
  FCameraJobList.Free;
  inherited;
end;


procedure TgxCameraController.CheckAssignments(Extended:boolean);
begin
  /// Check camera assignment
  if not Assigned(FCamera) then
  begin
    Raise EGLCameraController.CreateFmt('%s (%s) needs to have a Camera assigned',[Self.Name, Self.ClassName]);
  end;

  if Extended then
    /// Check camera;TargetObject assignment
    if not Assigned(FCameraTarget) then
    begin
      Raise EGLCameraController.CreateFmt('%s (%s) needs Camera to have a TargetObject assigned',[Self.Name, Self.ClassName]);
    end;
end;

procedure TgxCameraController.Step(const deltaTime, newTime: Double);
var
  CurrentJob : TgxCameraJob;
begin

  if FCameraJobList.Count > 0 then
  begin
    CurrentJob := FCameraJobList.First;

    if CurrentJob.FInit then
    begin
      CurrentJob.Init;
      CurrentJob.FStartTime := newTime;
      CurrentJob.FRunning := True;
      CurrentJob.FInit := False;

      // Notify job
      if Assigned(FOnJobAdded) then
        FOnJobAdded(CurrentJob);
    end;

    if CurrentJob.FRunning then
    begin
      CurrentJob.FElapsedTime := newTime - CurrentJob.FStartTime;
      CurrentJob.FDeltaTime := deltaTime;// newTime - CurrentJob.FElapsedTime;
      CurrentJob.Step;

      // Notify job
      if Assigned(FOnJobStep) then
        FOnJobStep(CurrentJob);
    end;

    if not CurrentJob.FRunning then
    begin
      FCameraJobList.Remove(CurrentJob);

      // Notify job
      if Assigned(FOnJobFinished) then
        FOnJobFinished(CurrentJob);
    end;

  end;

  //AdjustScene;
end;


function TgxCameraController.MoveToPos(x,y,z, time:double): TgxMoveToPosJob;
begin
  Result := TgxMoveToPosJob.Create(FCameraJobList);

  Result.X := x;
  Result.Y := y;
  Result.Z := z;
  Result.Time := time;
end;


function TgxCameraController.ZoomToDistance(Distance, Time:double): TgxZoomToDistanceJob;
begin
  Result := TgxZoomToDistanceJob.Create(FCameraJobList);
  Result.Distance := Distance;
  Result.Time := Time;
end;


function TgxCameraController.OrbitToPos(x,y,z,time:double): TgxOrbitToPosJob;
begin
  Result := TgxOrbitToPosJob.Create(FCameraJobList);
  Result.FTargetPosition := PointMake(x, y, z);
  Result.FCameraUpVector := FCameraJobList.FController.FCamera.AbsoluteUp;
  Result.FTime := time;
end;


function TgxCameraController.OrbitToPosSmooth(const ATargetPosition: TVector4f; const ATime: Double;
  const ASmoothNavigator: TgxNavigatorSmoothChangeVector; const AFNeedToRecalculateZoom: Boolean;
  const ACameraUpVector: PVector4f = nil): TgxSmoothOrbitToPos;
begin
  Result := TgxSmoothOrbitToPos.Create(FCameraJobList);

  Result.FTargetPosition := ATargetPosition;
  Result.FTime := ATime;
  Result.FSmoothNavigator := ASmoothNavigator;
  Result.FShouldBeMatrix := FCameraJobList.FController.FCamera.Matrix^;
  Result.FNeedToRecalculateZoom := AFNeedToRecalculateZoom;
  if ACameraUpVector = nil then
    Result.FCameraUpVector := FCameraJobList.FController.FCamera.AbsoluteUp
  else
    Result.FCameraUpVector := ACameraUpVector^;
end;

function TgxCameraController.OrbitToPosAdvanced(x,y,z,time:double; PreferUpAxis: Boolean = True): TgxOrbitToPosAdvJob;
begin
  Result := TgxOrbitToPosAdvJob.Create(FCameraJobList);

  Result.X := x;
  Result.Y := y;
  Result.Z := z;
  Result.PreferUpAxis := PreferUpAxis;
  Result.Time := time;
end;

function TgxCameraController.OrbitToPosAdvancedSmooth(const x,y,z, time: double;
  const ASmoothNavigator: TgxNavigatorSmoothChangeVector; const PreferUpAxis: Boolean = True): TgxSmoothOrbitToPosAdvJob;
begin
  Result := TgxSmoothOrbitToPosAdvJob.Create(FCameraJobList);

  Result.X := x;
  Result.Y := y;
  Result.Z := z;
  Result.PreferUpAxis := PreferUpAxis;
  Result.Time := time;
  Result.FSmoothNavigator := ASmoothNavigator;
  Result.FPreviousPosition := ASmoothNavigator.OnGetCurrentValue(ASmoothNavigator);
  Result.FRestoreUpVector := True;
end;

procedure TgxCameraController.SafeOrbitAndZoomToPos(x,y,z:double);
begin
  //this was the main purpose of this component
  //as you can see, it actually is a combination of the other 3 methods
  CheckAssignments(cGLCAMERACONTROLLER_CHECK_EXTENDED);
  ZoomToDistance(soSafeDistance,soTimeToSafePlacement);
  OrbitToPos(x,y,z,soTimeToOrbit);
  MoveToPos(x,y,z,soTimeToZoomBackIn);
end;


procedure TgxCameraController.StopMovement;
begin
  FCameraJobList.Clear;
end;


procedure TgxCameraController.SetOnJobAdded(const Value: TgxCameraJobEvent);
begin
  FOnJobAdded := Value;
end;

procedure TgxCameraController.SetOnJobStep(const Value: TgxCameraJobEvent);
begin
  FOnJobStep := Value;
end;

procedure TgxCameraController.SetOnJobFinished(const Value: TgxCameraJobEvent);
begin
  FOnJobFinished := Value;
end;

procedure TgxCameraController.SetCamera(const Value: TgxBaseSceneObject);
begin
  if FCamera <> nil then FCamera.RemoveFreeNotification(Self);
  FCamera := Value;
  if FCamera <> nil then FCamera.FreeNotification(Self);
  
  if (FCamera is TgxCamera) and (FCameraTarget = nil) then
    SetCameraTarget(TgxCamera(FCamera).TargetObject);
end;

procedure TgxCameraController.SetCameraTarget(
  const Value: TgxBaseSceneObject);
begin
  if FCameraTarget <> nil then FCameraTarget.RemoveFreeNotification(Self);
  FCameraTarget := Value;
  if FCameraTarget <> nil then FCameraTarget.FreeNotification(Self);
end;

procedure TgxCameraController.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FCamera then
      FCamera := nil
    else if AComponent = FCameraTarget then
      FCameraTarget := nil;
  end;
end;

{ TgxCameraJobList }


constructor TgxCameraJobList.Create(AController: TgxCameraController);
begin
  inherited Create;
  FController := AController;
end;

function TgxCameraJobList.GetCameraJob(const AIndex: integer): TgxCameraJob;
begin
  Result := inherited Get(AIndex);
end;

procedure TgxCameraJobList.SetCameraJob(const AIndex: integer;
  const Value: TgxCameraJob);
begin
  inherited Put(AIndex, Value);
end;

function TgxCameraJobList.Add(ACameraJob: TgxCameraJob): integer;
begin
  Result := inherited Add(ACameraJob);
end;

function TgxCameraJobList.First: TgxCameraJob;
begin
  Result := TgxCameraJob(inherited First);
end;

function TgxCameraJobList.Last: TgxCameraJob;
begin
  Result := TgxCameraJob(inherited Last);
end;

{ TgxCameraJob }

constructor TgxCameraJob.Create(const AJoblist : TgxCameraJobList);
begin
  FJoblist := AJoblist;
  FJoblist.Add(Self);

  FInit := True;
  FStartTime := 0;
  FProceedTime := 0;
end;

destructor TgxCameraJob.Destroy;
begin

  inherited;
end;

procedure TgxCameraJob.Abort;
begin

end;


{ TgxMoveToPosJob }

procedure TgxMoveToPosJob.Init;
begin
  FProceedTime := Time;
  FInitialPos := VectorSubtract(FJobList.FController.FCamera.AbsolutePosition, FJobList.FController.FCameraTarget.AbsolutePosition);
  MakeVector(FFinalPos, X, Y, Z);
end;

procedure TgxMoveToPosJob.Step;
var
  Vect : TVector4f;
begin
  if FElapsedTime < FProceedTime then
  begin
    Vect := VectorLerp(FInitialPos, FFinalPos, FElapsedTime/FProceedTime);
  end
    else
  begin
    Vect := FFinalPos;
    FRunning := false;
  end;

  if Assigned(FJobList.FController.FCamera.Parent) then
    Vect:=FJobList.FController.FCamera.Parent.AbsoluteToLocal(Vect);

  FJobList.FController.FCamera.Position.AsVector := Vect;
end;

{ TgxZoomToDistanceJob }

procedure TgxZoomToDistanceJob.Init;
begin
  FProceedTime := Time;
  FInitialPos := VectorSubtract(FJobList.FController.FCamera.AbsolutePosition, FJobList.FController.FCameraTarget.AbsolutePosition);
  // To determine final position, we normalize original position and scale it with final distance
  SetVector(FFinalPos, FInitialPos);
  NormalizeVector(FFinalPos);
  ScaleVector(FFinalPos, Distance);
end;

procedure TgxZoomToDistanceJob.Step;
var
  Vect : TVector4f;
begin
  if FElapsedTime < FProceedTime then
  begin
    Vect := VectorLerp(FInitialPos, FFinalPos, FElapsedTime/FProceedTime);
  end
    else
  begin
    Vect := FFinalPos;
    FRunning := false;
  end;

  if Assigned(FJobList.FController.FCamera.Parent) then
    Vect:=FJobList.FController.FCamera.Parent.AbsoluteToLocal(Vect);

  FJobList.FController.FCamera.Position.AsVector := Vect;
end;



{ TgxOrbitToPosJob }


procedure TgxOrbitToPosJob.Init;
begin
  FProceedTime := FTime;

  FFinalPos := ShiftObjectFromCenter(FTargetPosition, FJobList.FController.FCameraTarget.AbsolutePosition,
    VectorDistance(FJobList.FController.FCamera.AbsolutePosition, FJobList.FController.FCameraTarget.AbsolutePosition), True);

  // Yep, FFinalPos is stored in relative coordinates.
  if FJobList.FController.FCamera.Parent <> nil then
    FFinalPos := FJobList.FController.FCamera.Parent.AbsoluteToLocal(FFinalPos);

  FRotateSpeed := GetSafeTurnAngle(
    FJobList.FController.FCamera.AbsolutePosition, FCameraUpVector, FTargetPosition,
    FJobList.FController.FCameraTarget.AbsolutePosition);

  ScaleVector(FRotateSpeed, 1 / FProceedTime);

  FInit := True;
end;

procedure TgxOrbitToPosJob.Step;
begin

  if FElapsedTime < FProceedTime then
  begin
    FJobList.FController.FCamera.AbsolutePosition := MoveObjectAround(
      FJobList.FController.FCamera.AbsolutePosition, FCameraUpVector,
      FJobList.FController.FCameraTarget.AbsolutePosition,
      FRotateSpeed.X * FDeltaTime, FRotateSpeed.Y * FDeltaTime);
  end
    else
  begin
    // Yep, FFinalPos is stored in ralative coordinates.
    FJobList.FController.FCamera.Position.AsVector := FFinalPos;
    FRunning := False;
  end;

end;


{ TgxOrbitToPosAdvJob }

procedure TgxOrbitToPosAdvJob.Init;
var
  Right: TVector4f;
  lAbsVectorToTarget: TVector4f;
begin

  FProceedTime := time;
  FInitialPos := VectorSubtract(FJobList.FController.FCamera.AbsolutePosition, FJobList.FController.FCameraTarget.AbsolutePosition);

  if Assigned(FJobList.FController.FCamera.Parent) then
    FFinalPos := VectorSubtract(FJobList.FController.FCamera.Parent.LocalToAbsolute(VectorMake(x,y,z,1)), FJobList.FController.FCameraTarget.AbsolutePosition)
  else
    FFinalPos := VectorSubtract(VectorMake(x,y,z,1), FJobList.FController.FCameraTarget.AbsolutePosition);

  //if destination is Target Pos, we can't compute
  if VectorLength(FFinalPos)<cEPSILON then
  begin
    //FAllowUserAction := True;
    Exit;
  end;

  //Compute Angle of Rotation
  FAngle:= ArcCos(VectorAngleCosine(Vector3fMake(FFinalPos), Vector3fMake(FInitialPos)));

  lAbsVectorToTarget := VectorNormalize(VectorSubtract(
      FJobList.FController.FCameraTarget.AbsolutePosition,
      FJobList.FController.FCamera.AbsolutePosition));

  Right := VectorNormalize(VectorCrossProduct(lAbsVectorToTarget, FJobList.FController.FCamera.AbsoluteUp));

  FInitialDir := FJobList.FController.FCamera.AbsoluteDirection;
  FInitialUp := FJobList.FController.FCamera.AbsoluteUp;

  // Determine rotation Axis
  // if Angle equals 0 degrees.
  if FAngle < cEPSILON then
    if PreferUpAxis then
      FRotAxis := VectorNormalize(VectorCrossProduct(
                   VectorCrossProduct(FFinalPos, FInitialUp), FFinalPos))
    else
      FRotAxis := Right
  else
    // if Angle equals 180 degrees.
    if FAngle >Pi - cEPSILON  then
      if PreferUpAxis then
        FRotAxis := VectorNormalize(VectorCrossProduct(VectorCrossProduct(FFinalPos, FInitialUp), FFinalPos))
      else
        FRotAxis := Right
    else
      FRotAxis:= VectorNormalize(VectorCrossProduct(FFinalPos, FInitialPos));

end;

procedure TgxOrbitToPosAdvJob.Step;
var
  tempUp, tempDir, tempPos : TVector4f;
begin

  if FElapsedTime < FProceedTime then
  begin
    //Compute Position
    tempPos := FInitialPos;
    RotateVector(tempPos, Vector3fMake(FRotAxis), FAngle * FElapsedTime/FProceedTime);
    FJobList.FController.FCamera.AbsolutePosition := VectorAdd(FJobList.FController.FCameraTarget.AbsolutePosition, tempPos);

    //Compute Direction vector
    tempDir := FInitialDir;
    RotateVector(tempDir, Vector3fMake(FRotAxis), FAngle * FElapsedTime/FProceedTime);
    FJobList.FController.FCamera.AbsoluteDirection := tempDir;

    //Compute Up Vector
    tempUp := FInitialUp;
    RotateVector(tempUp, Vector3fMake(FRotAxis), FAngle * FElapsedTime/FProceedTime);
    FJobList.FController.FCamera.AbsoluteUp := tempUp;
  end
    else
  begin
    //Compute Position
    tempPos := FInitialPos;
    RotateVector(tempPos, Vector3fMake(FRotAxis), FAngle);
    FJoblist.FController.FCamera.AbsolutePosition := VectorAdd(
       FJoblist.FController.FCameraTarget.AbsolutePosition, tempPos);

    //Compute Direction vector
    tempDir := FInitialDir;
    RotateVector(tempDir, Vector3fMake(FRotAxis), FAngle);
    FJoblist.FController.FCamera.AbsoluteDirection := tempDir;

    //Compute Up Vector
    tempUp := FInitialUp;
    RotateVector(tempUp, Vector3fMake(FRotAxis), FAngle);
    FJoblist.FController.FCamera.AbsoluteUp := tempUp;

    FRunning := false;
  end;

end;

{ TgxSmoothOrbitToPosAdvJob }

procedure TgxSmoothOrbitToPosAdvJob.Init;
var
  Right: TVector4f;
begin
  FProceedTime := time;
  FInitialPos:= VectorSubtract(FPreviousPosition, FJobList.FController.FCameraTarget.AbsolutePosition);

  if Assigned(FJobList.FController.FCamera.Parent) then
    FFinalPos := VectorSubtract(FJobList.FController.FCamera.Parent.LocalToAbsolute(VectorMake(x,y,z,1)), FJobList.FController.FCameraTarget.AbsolutePosition)
  else
    FFinalPos := VectorSubtract(VectorMake(x,y,z,1), FJobList.FController.FCameraTarget.AbsolutePosition);

  //if destination is Target Pos, we can't compute
  if VectorLength(FFinalPos)<cEPSILON then
  begin
    //FAllowUserAction := True;
    Exit;
  end;

  //Compute Angle of Rotation
  FAngle:= ArcCos(VectorAngleCosine(Vector3fMake(FFinalPos), Vector3fMake(FInitialPos)));

  Right := VectorNormalize(VectorCrossProduct(
//    FJobList.FController.FCamera.AbsoluteVectorToTarget,
    VectorNormalize(VectorSubtract(FJobList.FController.FCameraTarget.AbsolutePosition, FPreviousPosition)),
    FJobList.FController.FCamera.AbsoluteUp));

  FInitialDir := FJobList.FController.FCamera.AbsoluteDirection;
  FInitialUp := FJobList.FController.FCamera.AbsoluteUp;

  // Determine rotation Axis
  // if Angle equals 0 degrees.
  if FAngle < cEPSILON then
    if PreferUpAxis then
      FRotAxis := VectorNormalize(VectorCrossProduct(
                   VectorCrossProduct(FFinalPos, FInitialUp), FFinalPos))
    else
      FRotAxis := Right
  else
    // if Angle equals 180 degrees.
    if FAngle >Pi - cEPSILON  then
      if PreferUpAxis then
        FRotAxis := VectorNormalize(VectorCrossProduct(VectorCrossProduct(FFinalPos, FInitialUp), FFinalPos))
      else
        FRotAxis := Right
    else
      FRotAxis:= VectorNormalize(VectorCrossProduct(FFinalPos, FInitialPos));

end;

procedure TgxSmoothOrbitToPosAdvJob.Step;
var
  tempUp, tempDir, tempPos : TVector4f;
begin

  if FElapsedTime < FProceedTime then
  begin
    //Compute Position
    tempPos := FInitialPos;
    RotateVector(tempPos, Vector3fMake(FRotAxis), FAngle * FElapsedTime/FProceedTime);
    FSmoothNavigator.TargetValue.DirectVector := VectorAdd(FJobList.FController.FCameraTarget.AbsolutePosition, tempPos);
    FPreviousPosition := FSmoothNavigator.TargetValue.DirectVector;

    //Compute Direction vector
    tempDir := FInitialDir;
    RotateVector(tempDir, Vector3fMake(FRotAxis), FAngle * FElapsedTime/FProceedTime);
    FJobList.FController.FCamera.AbsoluteDirection := tempDir;

    //Compute Up Vector
    if FRestoreUpVector then
      FJobList.FController.FCamera.AbsoluteUp := FInitialUp
    else
    begin
      tempUp := FInitialUp;
      RotateVector(tempUp, Vector3fMake(FRotAxis), FAngle * FElapsedTime/FProceedTime);
      FJobList.FController.FCamera.AbsoluteUp := tempUp;
    end;
  end
  else
  begin
    //Compute Position
    tempPos := FInitialPos;
    RotateVector(tempPos, Vector3fMake(FRotAxis), FAngle);
    FJoblist.FController.FCamera.AbsolutePosition := VectorAdd(
      FJoblist.FController.CameraTarget.AbsolutePosition, tempPos);

    //Compute Direction vector
    tempDir := FInitialDir;
    RotateVector(tempDir, Vector3fMake(FRotAxis), FAngle);
    FJoblist.FController.FCamera.AbsoluteDirection := tempDir;

    //Compute Up Vector
    if FRestoreUpVector then
      FJoblist.FController.FCamera.AbsoluteUp := FInitialUp
    else
    begin
      tempUp := FInitialUp;
      RotateVector(tempUp, Vector3fMake(FRotAxis), FAngle);
      FJoblist.FController.FCamera.AbsoluteUp := tempUp;
      FRunning := false;
    end;

    FRunning := false;
  end;
end;

{ TgxSmoothOrbitToPosAdv }

constructor TgxSmoothOrbitToPos.Create(const AJoblist: TgxCameraJobList);
begin
  inherited;
  FCutoffAngle := 0.1;
end;

procedure TgxSmoothOrbitToPos.Step;
var
  lCurrentDistanceToTarget: Single;
  lTargetPosition: TVector4f;
  lCurrentMatrix: TMatrix4f;
  lAngle: Single;
  lAbsTargetPosition: TVector4f;

  procedure RestoreDistanceToTarget();
  var
    lDirection: TVector4f;
  begin
    lDirection := VectorNormalize(VectorSubtract(
      FJobList.FController.FCameraTarget.AbsolutePosition,
      FJobList.FController.FCamera.AbsolutePosition));

    FJobList.FController.FCamera.AbsolutePosition := VectorAdd(
      FJobList.FController.FCameraTarget.AbsolutePosition,
      VectorScale(lDirection, - lCurrentDistanceToTarget));
  end;


  procedure SetTargetValueRelative(const AAbsolutePosition: TVector4f);
  begin
    if FJobList.FController.FCamera.Parent = nil then
      FSmoothNavigator.TargetValue.DirectVector := AAbsolutePosition
    else
      FSmoothNavigator.TargetValue.DirectVector := FJobList.FController.FCamera.Parent.AbsoluteToLocal(AAbsolutePosition);
  end;

  procedure ApplyDistanceToResult();
  var
    lDirection, lNewTargetPosition: TVector4f;
  begin
    lDirection := VectorNormalize(VectorSubtract(
      FJobList.FController.FCameraTarget.AbsolutePosition,
      lAbsTargetPosition));

    lNewTargetPosition := VectorAdd(
      FJobList.FController.FCameraTarget.AbsolutePosition,
      VectorScale(lDirection, - lCurrentDistanceToTarget));

    SetTargetValueRelative(lNewTargetPosition);
  end;

begin
  if FElapsedTime < FProceedTime then
  begin
    // Save current matrix.
    lCurrentMatrix := FJobList.FController.FCamera.Matrix^;

    if FNeedToRecalculateZoom then
      lCurrentDistanceToTarget := FJobList.FController.FCamera.DistanceTo(FJobList.FController.FCameraTarget)
    else
      lCurrentDistanceToTarget := 0; // To avoid warning message.

    // Calculate the position, in which camera should have been.
    FJobList.FController.FCamera.SetMatrix(FShouldBeMatrix);

    FJobList.FController.FCamera.AbsolutePosition := MoveObjectAround(
      FJobList.FController.FCamera.AbsolutePosition, FCameraUpVector,
      FJobList.FController.FCameraTarget.AbsolutePosition,
      FRotateSpeed.X * FDeltaTime, FRotateSpeed.Y * FDeltaTime);

    if FNeedToRecalculateZoom then
      RestoreDistanceToTarget();

    lTargetPosition := FJobList.FController.FCamera.AbsolutePosition;
    FShouldBeMatrix := FJobList.FController.FCamera.Matrix^;

    // Restore Camera position and move it to the desired vector.
    FJobList.FController.FCamera.SetMatrix(lCurrentMatrix);
    SetTargetValueRelative(lTargetPosition);
  end
  else
  begin
    if FNeedToRecalculateZoom then
    begin
      if FJobList.FController.FCamera.Parent = nil then
        lAbsTargetPosition := FFinalPos
      else
        lAbsTargetPosition := FJobList.FController.FCamera.Parent.LocalToAbsolute(FFinalPos);

      lAngle := RadToDeg(AngleBetweenVectors(FJobList.FController.FCamera.AbsolutePosition,
         lAbsTargetPosition, FJobList.FController.FCameraTarget.AbsolutePosition));
      if lAngle < FCutoffAngle then
      begin
        FSmoothNavigator.Enabled := False;
        FRunning := False;
      end
      else
      begin
        lCurrentDistanceToTarget := FJobList.FController.FCamera.DistanceTo(FJobList.FController.FCameraTarget);
        ApplyDistanceToResult();
      end;
    end
    else
    begin
      FSmoothNavigator.TargetValue.DirectVector := FFinalPos;
      FRunning := False;
    end;
  end;
end;

end.
