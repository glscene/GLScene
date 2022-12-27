//
// The multimedia graphics platform GLScene https://github.com/glscene
//

unit GLS.CameraController;

(*
  Component for animating camera movement.
  Can be used to zoom in/out, for linear movement, orbiting and Google Earth - like "fly-to"
  Main purpose was the SafeOrbitAndZoomToPos method, the others are usable as well
*)

interface

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Contnrs,
  System.Types,

  GLS.Scene,
  GLS.Coordinates,
  GLS.PersistentClasses,
  GLS.VectorGeometry,
  GLS.SmoothNavigator,
  GLS.VectorTypes;

type

  EGLCameraController = class(Exception);

  // Forward declaration of the camera controller main class
  TGLCameraController = class;

  // Forward declaration of a generic camera job
  TGLCameraJob = class;

  TGLCameraJobList = class(TObjectList)
  private
    FController: TGLCameraController;
    function GetCameraJob(const AIndex: integer): TGLCameraJob;
    procedure SetCameraJob(const AIndex: integer; const Value: TGLCameraJob);
  public
    constructor Create(AController: TGLCameraController);
    function Add(ACameraJob: TGLCameraJob): integer;
    property Items[const AIndex: integer]: TGLCameraJob read GetCameraJob
      write SetCameraJob; default;
    function First: TGLCameraJob;
    function Last: TGLCameraJob;
  end;

  TGLCameraJob = class(TObject)
  private
    FJoblist: TGLCameraJobList;
  protected
    FAbort: boolean;
    FInit: boolean;
    FRunning: boolean;
    FElapsedTime: Double;
    FDeltaTime: Double;
    FStartTime: Double;
    FProceedTime: Double;
  public
    constructor Create(const AJoblist: TGLCameraJobList); virtual;
    destructor Destroy; override;
    procedure Abort;
    procedure Step; virtual; abstract;
    procedure Init; virtual; abstract;
    property Running: boolean read FRunning write FRunning;
    property ElapsedTime: Double read FElapsedTime write FElapsedTime;
    property StartTime: Double read FStartTime write FStartTime;
    property ProceedTime: Double read FProceedTime write FProceedTime;
  end;

  TGLMoveToPosJob = class(TGLCameraJob)
  private
    FInitialPos: TGLVector;
    FFinalPos: TGLVector;
  public
    X: Double;
    Y: Double;
    Z: Double;
    Time: Double;
    procedure Step; override;
    procedure Init; override;
    // Properties.
    property InitialPos: TGLVector read FInitialPos;
    property FinalPos: TGLVector read FFinalPos;
  end;

  TGLZoomToDistanceJob = class(TGLCameraJob)
  private
    FInitialPos: TGLVector;
    FFinalPos: TGLVector;
  public
    Distance: Double;
    Time: Double;
    procedure Step; override;
    procedure Init; override;
    // Properties.
    property InitialPos: TGLVector read FInitialPos;
    property FinalPos: TGLVector read FFinalPos;
  end;

  TGLOrbitToPosJob = class(TGLCameraJob)
  private
    FFinalPos: TGLVector; // Yep, FFinalPos is stored in relative coordinates.
    FRotateSpeed: TVector2f;
    FCameraUpVector: TGLVector;
    // Absolute Coordinates, can even be not normalized by radius.
    // Procesed in Init, not used anywhere else.
    FTargetPosition: TGLVector;
    FTime: Double;
  public
    procedure Step; override;
    procedure Init; override;
    property RotateSpeed: TVector2f read FRotateSpeed;
    property CameraUpVector: TGLVector read FCameraUpVector;
    property TargetPosition: TGLVector read FTargetPosition;
    property FinalPos: TGLVector read FFinalPos;
    property Time: Double read FTime;
  end;

  TGLSmoothOrbitToPos = class(TGLOrbitToPosJob)
  private
    FCutoffAngle: Single;
    FNeedToRecalculateZoom: boolean;
    FShouldBeMatrix: TGLMatrix;
    FSmoothNavigator: TGLNavigatorSmoothChangeVector;
  public
    constructor Create(const AJoblist: TGLCameraJobList); override;
    procedure Step; override;
    property CutoffAngle: Single read FCutoffAngle write FCutoffAngle;
    property NeedToRecalculateZoom: boolean read FNeedToRecalculateZoom
      write FNeedToRecalculateZoom;
  end;

  TGLOrbitToPosAdvJob = class(TGLCameraJob)
  private
    FInitialPos: TGLVector;
    FFinalPos: TGLVector;
    FInitialUp: TGLVector;
    FInitialDir: TGLVector;
    FRotAxis: TGLVector;
    FAngle: Double;
  public
    X: Double;
    Y: Double;
    Z: Double;
    Time: Double;
    PreferUpAxis: boolean;
    procedure Step; override;
    procedure Init; override;
    // Properties.
    property InitialPos: TGLVector read FInitialPos;
    property InitialUp: TGLVector read FInitialUp;
    property InitialDir: TGLVector read FInitialDir;
    property FinalPos: TGLVector read FFinalPos;
  end;

  TGLSmoothOrbitToPosAdvJob = class(TGLOrbitToPosAdvJob)
  private
    FPreviousPosition: TGLVector;
    FSmoothNavigator: TGLNavigatorSmoothChangeVector;
    FRestoreUpVector: boolean;
  public
    procedure Step; override;
    procedure Init; override;
  end;

  TGLCameraJobEvent = procedure(Sender: TGLCameraJob) of object;

  TGLCameraController = class(TComponent)
  private
    // Objects.
    FCameraJobList: TGLCameraJobList;
    FCamera: TGLBaseSceneObject;
    FCameraTarget: TGLBaseSceneObject;
    // Events.
    FOnJobAdded: TGLCameraJobEvent;
    FOnJobFinished: TGLCameraJobEvent;
    FOnJobStep: TGLCameraJobEvent;
    // fields used by SafeOrbitAndZoomToPos
    FsoSafeDist, FsoTimeToSafePlacement, FsoTimeToOrbit,
      FsoTimeToZoomBackIn: Double;
    // private methods
    // used to test whether camera and cadencer are assigned
    // Extended = true -> will test also for Camera.TargetObject
    procedure CheckAssignments(Extended: boolean);
    // after AdjustScene the Camera.DepthofView will be modified
    // if you want to zoom back in from GUI
    // you should use something like
    // Camera.DepthOfView:=2*Camera.DistanceToTarget+2*camera.TargetObject.BoundingSphereRadius;
    procedure SetOnJobAdded(const Value: TGLCameraJobEvent);
    procedure SetOnJobFinished(const Value: TGLCameraJobEvent);
    procedure SetOnJobStep(const Value: TGLCameraJobEvent);
    procedure SetCamera(const Value: TGLBaseSceneObject);
    procedure SetCameraTarget(const Value: TGLBaseSceneObject);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* linear movement from current pos *)
    function MoveToPos(X, Y, Z, Time: Double): TGLMoveToPosJob;
    (* orbiting from current pos to the pos where
      the camera points at the camera.targetObject TROUGH the given point
      it will not move to the given point(!), use SafeOrbitAndZoomToPos instead
      there has to be a camera.targetObject assigned! *)
    function OrbitToPos(X, Y, Z, Time: Double): TGLOrbitToPosJob;
    (* Same as OrbitToPos(), but makes use of SmoothNavigator to make
      sure all camera movements are smooth. *)
    function OrbitToPosSmooth(const ATargetPosition: TGLVector;
      const ATime: Double;
      const ASmoothNavigator: TGLNavigatorSmoothChangeVector;
      const AFNeedToRecalculateZoom: boolean;
      const ACameraUpVector: PGLVector = nil): TGLSmoothOrbitToPos;
    (* Same function as OrbitToPos but support all camera states
      PreferUpAxis value is to setup if function use Camera Up based rotation axis
      instead of Camera direction based rotation axis when destination and camera
      position are opposite from Camera Target *)
    function OrbitToPosAdvanced(X, Y, Z, Time: Double;
      PreferUpAxis: boolean = True): TGLOrbitToPosAdvJob;
    (* Same as OrbitToPosAdvanced(), but makes use of SmoothNavigator to make
      sure all camera movements are smooth. *)
    function OrbitToPosAdvancedSmooth(const X, Y, Z, Time: Double;
      const ASmoothNavigator: TGLNavigatorSmoothChangeVector;
      const PreferUpAxis: boolean = True): TGLSmoothOrbitToPosAdvJob;
    (* zooms in/out by moving to the given distance from camera.targetObject
      there has to be a camera.targetObject assigned! *)
    function ZoomToDistance(Distance, Time: Double): TGLZoomToDistanceJob;
    (* google earth - like "fly-to" = zoom out to safe distance, orbit,
      and then zoom in to the given point
      there has to be a camera.targetObject assigned! *)
    procedure SafeOrbitAndZoomToPos(X, Y, Z: Double);
    (* It might be a good idea to introduce ability to stop movement
      and return control to user, here it is *)
    procedure StopMovement;
    // Called by the cadencer to animate the camera
    procedure Step(const deltaTime, newTime: Double);
    property CameraJobList: TGLCameraJobList read FCameraJobList;
  published
    // Assign a Moving object (usually a TGLCamera).
    property Camera: TGLBaseSceneObject read FCamera write SetCamera;
    // Assign a target, around which Moving object should rotate(usually TGLCamera.TargetObject).
    property CameraTarget: TGLBaseSceneObject read FCameraTarget
      write SetCameraTarget;
    (* specifies whether user should be able interract with the GLSceneViewer
      it is set to false while the camera is moving and
      coders should check this value and block GUI access to GLSceneViewer *)
    // property AllowUserAction:boolean read FAllowUserAction;
    (* safe distance to avoid moving the camera trough the camera.targetObject
      while performing  SafeOrbitAndZoomToPos *)
    property soSafeDistance: Double read FsoSafeDist write FsoSafeDist;
    // time to zoom in/out to the safe position while performing  SafeOrbitAndZoomToPos
    property soTimeToSafePlacement: Double read FsoTimeToSafePlacement
      write FsoTimeToSafePlacement;
    // time to orbit while performing  SafeOrbitAndZoomToPos
    property soTimeToOrbit: Double read FsoTimeToOrbit write FsoTimeToOrbit;
    // time to zoom in/out to the given final position while performing  SafeOrbitAndZoomToPos
    property soTimeToZoomBackIn: Double read FsoTimeToZoomBackIn
      write FsoTimeToZoomBackIn;
    // this event is triggered when a job is init
    property OnJobAdded: TGLCameraJobEvent read FOnJobAdded write SetOnJobAdded;
    // this event is triggered when a job is step (like an OnMove)
    property OnJobStep: TGLCameraJobEvent read FOnJobStep write SetOnJobStep;
    // this event is triggered when a job is finished (not canceled)
    property OnJobFinished: TGLCameraJobEvent read FOnJobFinished
      write SetOnJobFinished;
  end;

  // ====================================================================
implementation

// ====================================================================

const
  cGLCAMERACONTROLLER_CHECK_EXTENDED = True;
  cEPSILON = 0.001;

//-------------------------------------
// TGLCameraController
//-------------------------------------

constructor TGLCameraController.Create(AOwner: TComponent);
begin
  inherited;
  // create the job list container
  FCameraJobList := TGLCameraJobList.Create(Self);
  FCameraJobList.OwnsObjects := True;
  // initialize values
  soSafeDistance := 10;
  soTimeToSafePlacement := 1;
  soTimeToOrbit := 2;
  soTimeToZoomBackIn := 1;
end;

destructor TGLCameraController.Destroy;
begin
  // delete job list and all jobs inside
  FCameraJobList.Free;
  inherited;
end;

procedure TGLCameraController.CheckAssignments(Extended: boolean);
begin
  /// Check camera assignment
  if not Assigned(FCamera) then
  begin
    Raise EGLCameraController.CreateFmt
      ('%s (%s) needs to have a Camera assigned', [Self.Name, Self.ClassName]);
  end;
  if Extended then
    /// Check camera;TargetObject assignment
    if not Assigned(FCameraTarget) then
    begin
      Raise EGLCameraController.CreateFmt
        ('%s (%s) needs Camera to have a TargetObject assigned',
        [Self.Name, Self.ClassName]);
    end;
end;

procedure TGLCameraController.Step(const deltaTime, newTime: Double);
var
  CurrentJob: TGLCameraJob;
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
      CurrentJob.FDeltaTime := deltaTime; // newTime - CurrentJob.FElapsedTime;
      CurrentJob.Step;

      // Notify job
      if Assigned(FOnJobStep) then
        FOnJobStep(CurrentJob);
    end;
    if not CurrentJob.FRunning then
    begin
      // Notify job
      if Assigned(FOnJobFinished) then
        FOnJobFinished(CurrentJob);
      FCameraJobList.Remove(CurrentJob);		
    end;
  end;
  // AdjustScene;
end;

function TGLCameraController.MoveToPos(X, Y, Z, Time: Double): TGLMoveToPosJob;
begin
  Result := TGLMoveToPosJob.Create(FCameraJobList);
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.Time := Time;
end;

function TGLCameraController.ZoomToDistance(Distance, Time: Double)
  : TGLZoomToDistanceJob;
begin
  Result := TGLZoomToDistanceJob.Create(FCameraJobList);
  Result.Distance := Distance;
  Result.Time := Time;
end;

function TGLCameraController.OrbitToPos(X, Y, Z, Time: Double)
  : TGLOrbitToPosJob;
begin
  Result := TGLOrbitToPosJob.Create(FCameraJobList);
  Result.FTargetPosition := PointMake(X, Y, Z);
  Result.FCameraUpVector := FCameraJobList.FController.FCamera.AbsoluteUp;
  Result.FTime := Time;
end;

function TGLCameraController.OrbitToPosSmooth(const ATargetPosition: TGLVector;
  const ATime: Double; const ASmoothNavigator: TGLNavigatorSmoothChangeVector;
  const AFNeedToRecalculateZoom: boolean; const ACameraUpVector: PGLVector = nil)
  : TGLSmoothOrbitToPos;
begin
  Result := TGLSmoothOrbitToPos.Create(FCameraJobList);

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

function TGLCameraController.OrbitToPosAdvanced(X, Y, Z, Time: Double;
  PreferUpAxis: boolean = True): TGLOrbitToPosAdvJob;
begin
  Result := TGLOrbitToPosAdvJob.Create(FCameraJobList);

  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.PreferUpAxis := PreferUpAxis;
  Result.Time := Time;
end;

function TGLCameraController.OrbitToPosAdvancedSmooth(const X, Y, Z,
  Time: Double; const ASmoothNavigator: TGLNavigatorSmoothChangeVector;
  const PreferUpAxis: boolean = True): TGLSmoothOrbitToPosAdvJob;
begin
  Result := TGLSmoothOrbitToPosAdvJob.Create(FCameraJobList);

  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.PreferUpAxis := PreferUpAxis;
  Result.Time := Time;
  Result.FSmoothNavigator := ASmoothNavigator;
  Result.FPreviousPosition := ASmoothNavigator.OnGetCurrentValue
    (ASmoothNavigator);
  Result.FRestoreUpVector := True;
end;

procedure TGLCameraController.SafeOrbitAndZoomToPos(X, Y, Z: Double);
begin
  // this was the main purpose of this component
  // as you can see, it actually is a combination of the other 3 methods
  CheckAssignments(cGLCAMERACONTROLLER_CHECK_EXTENDED);
  ZoomToDistance(soSafeDistance, soTimeToSafePlacement);
  OrbitToPos(X, Y, Z, soTimeToOrbit);
  MoveToPos(X, Y, Z, soTimeToZoomBackIn);
end;

procedure TGLCameraController.StopMovement;
begin
  FCameraJobList.Clear;
end;

procedure TGLCameraController.SetOnJobAdded(const Value: TGLCameraJobEvent);
begin
  FOnJobAdded := Value;
end;

procedure TGLCameraController.SetOnJobStep(const Value: TGLCameraJobEvent);
begin
  FOnJobStep := Value;
end;

procedure TGLCameraController.SetOnJobFinished(const Value: TGLCameraJobEvent);
begin
  FOnJobFinished := Value;
end;

procedure TGLCameraController.SetCamera(const Value: TGLBaseSceneObject);
begin
  if FCamera <> nil then
    FCamera.RemoveFreeNotification(Self);
  FCamera := Value;
  if FCamera <> nil then
    FCamera.FreeNotification(Self);

  if (FCamera is TGLCamera) and (FCameraTarget = nil) then
    SetCameraTarget(TGLCamera(FCamera).TargetObject);
end;

procedure TGLCameraController.SetCameraTarget(const Value: TGLBaseSceneObject);
begin
  if FCameraTarget <> nil then
    FCameraTarget.RemoveFreeNotification(Self);
  FCameraTarget := Value;
  if FCameraTarget <> nil then
    FCameraTarget.FreeNotification(Self);
end;

procedure TGLCameraController.Notification(AComponent: TComponent;
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

//-------------------------------------
// TGLCameraJobList
//-------------------------------------

constructor TGLCameraJobList.Create(AController: TGLCameraController);
begin
  inherited Create;
  FController := AController;
end;

function TGLCameraJobList.GetCameraJob(const AIndex: integer): TGLCameraJob;
begin
  Result := inherited Get(AIndex);
end;

procedure TGLCameraJobList.SetCameraJob(const AIndex: integer;
  const Value: TGLCameraJob);
begin
  inherited Put(AIndex, Value);
end;

function TGLCameraJobList.Add(ACameraJob: TGLCameraJob): integer;
begin
  Result := inherited Add(ACameraJob);
end;

function TGLCameraJobList.First: TGLCameraJob;
begin
  Result := TGLCameraJob(inherited First);
end;

function TGLCameraJobList.Last: TGLCameraJob;
begin
  Result := TGLCameraJob(inherited Last);
end;

//-------------------------------------
// TGLCameraJob
//-------------------------------------

constructor TGLCameraJob.Create(const AJoblist: TGLCameraJobList);
begin
  FJoblist := AJoblist;
  FJoblist.Add(Self);

  FInit := True;
  FStartTime := 0;
  FProceedTime := 0;
end;

destructor TGLCameraJob.Destroy;
begin

  inherited;
end;

procedure TGLCameraJob.Abort;
begin

end;

//-------------------------------------
// TGLMoveToPosJob
//-------------------------------------

procedure TGLMoveToPosJob.Init;
begin
  FProceedTime := Time;
  FInitialPos := VectorSubtract(FJoblist.FController.FCamera.AbsolutePosition,
    FJoblist.FController.FCameraTarget.AbsolutePosition);
  MakeVector(FFinalPos, X, Y, Z);
end;

procedure TGLMoveToPosJob.Step;
var
  Vect: TGLVector;
begin
  if FElapsedTime < FProceedTime then
  begin
    Vect := VectorLerp(FInitialPos, FFinalPos, FElapsedTime / FProceedTime);
  end
  else
  begin
    Vect := FFinalPos;
    FRunning := False;
  end;

  if Assigned(FJoblist.FController.FCamera.Parent) then
    Vect := FJoblist.FController.FCamera.Parent.AbsoluteToLocal(Vect);

  FJoblist.FController.FCamera.Position.AsVector := Vect;
end;

//-------------------------------------
// TGLZoomToDistanceJob
//-------------------------------------

procedure TGLZoomToDistanceJob.Init;
begin
  FProceedTime := Time;
  FInitialPos := VectorSubtract(FJoblist.FController.FCamera.AbsolutePosition,
    FJoblist.FController.FCameraTarget.AbsolutePosition);
  // To determine final position, we normalize original position and scale it with final distance
  SetVector(FFinalPos, FInitialPos);
  NormalizeVector(FFinalPos);
  ScaleVector(FFinalPos, Distance);
end;

procedure TGLZoomToDistanceJob.Step;
var
  Vect: TGLVector;
begin
  if FElapsedTime < FProceedTime then
  begin
    Vect := VectorLerp(FInitialPos, FFinalPos, FElapsedTime / FProceedTime);
  end
  else
  begin
    Vect := FFinalPos;
    FRunning := False;
  end;

  if Assigned(FJoblist.FController.FCamera.Parent) then
    Vect := FJoblist.FController.FCamera.Parent.AbsoluteToLocal(Vect);

  FJoblist.FController.FCamera.Position.AsVector := Vect;
end;

//-------------------------------------
// TGLOrbitToPosJob
//-------------------------------------

procedure TGLOrbitToPosJob.Init;
begin
  FProceedTime := FTime;
  FFinalPos := ShiftObjectFromCenter(FTargetPosition,
    FJoblist.FController.FCameraTarget.AbsolutePosition,
    VectorDistance(FJoblist.FController.FCamera.AbsolutePosition,
    FJoblist.FController.FCameraTarget.AbsolutePosition), True);
  // Yep, FFinalPos is stored in relative coordinates.
  if FJoblist.FController.FCamera.Parent <> nil then
    FFinalPos := FJoblist.FController.FCamera.Parent.AbsoluteToLocal(FFinalPos);

  FRotateSpeed := GetSafeTurnAngle
    (FJoblist.FController.FCamera.AbsolutePosition, FCameraUpVector,
    FTargetPosition, FJoblist.FController.FCameraTarget.AbsolutePosition);
  ScaleVector(FRotateSpeed, 1 / FProceedTime);
  FInit := True;
end;

procedure TGLOrbitToPosJob.Step;
begin

  if FElapsedTime < FProceedTime then
  begin
    FJoblist.FController.FCamera.AbsolutePosition :=
      MoveObjectAround(FJoblist.FController.FCamera.AbsolutePosition,
      FCameraUpVector, FJoblist.FController.FCameraTarget.AbsolutePosition,
      FRotateSpeed.X * FDeltaTime, FRotateSpeed.Y * FDeltaTime);
  end
  else
  begin
    // Yep, FFinalPos is stored in ralative coordinates.
    FJoblist.FController.FCamera.Position.AsVector := FFinalPos;
    FRunning := False;
  end;

end;

//-------------------------------------
// TGLOrbitToPosAdvJob
//-------------------------------------

procedure TGLOrbitToPosAdvJob.Init;
var
  Right: TGLVector;
  lAbsVectorToTarget: TGLVector;
begin
  FProceedTime := Time;
  FInitialPos := VectorSubtract(FJoblist.FController.FCamera.AbsolutePosition,
    FJoblist.FController.FCameraTarget.AbsolutePosition);
  if Assigned(FJoblist.FController.FCamera.Parent) then
    FFinalPos := VectorSubtract
      (FJoblist.FController.FCamera.Parent.LocalToAbsolute(VectorMake(X, Y, Z,
      1)), FJoblist.FController.FCameraTarget.AbsolutePosition)
  else
    FFinalPos := VectorSubtract(VectorMake(X, Y, Z, 1),
      FJoblist.FController.FCameraTarget.AbsolutePosition);
  // if destination is Target Pos, we can't compute
  if VectorLength(FFinalPos) < cEPSILON then
  begin
    // FAllowUserAction := True;
    Exit;
  end;
  // Compute Angle of Rotation
  FAngle := ArcCos(VectorAngleCosine(Vector3fMake(FFinalPos),
    Vector3fMake(FInitialPos)));
  lAbsVectorToTarget := VectorNormalize
    (VectorSubtract(FJoblist.FController.FCameraTarget.AbsolutePosition,
    FJoblist.FController.FCamera.AbsolutePosition));
  Right := VectorNormalize(VectorCrossProduct(lAbsVectorToTarget,
    FJoblist.FController.FCamera.AbsoluteUp));
  FInitialDir := FJoblist.FController.FCamera.AbsoluteDirection;
  FInitialUp := FJoblist.FController.FCamera.AbsoluteUp;
  // Determine rotation Axis
  // if Angle equals 0 degrees.
  if FAngle < cEPSILON then
    if PreferUpAxis then
      FRotAxis := VectorNormalize
        (VectorCrossProduct(VectorCrossProduct(FFinalPos, FInitialUp),
        FFinalPos))
    else
      FRotAxis := Right
  else
    // if Angle equals 180 degrees.
    if FAngle > Pi - cEPSILON then
      if PreferUpAxis then
        FRotAxis := VectorNormalize
          (VectorCrossProduct(VectorCrossProduct(FFinalPos, FInitialUp),
          FFinalPos))
      else
        FRotAxis := Right
    else
      FRotAxis := VectorNormalize(VectorCrossProduct(FFinalPos, FInitialPos));
end;

procedure TGLOrbitToPosAdvJob.Step;
var
  tempUp, tempDir, tempPos: TGLVector;
begin
  if FElapsedTime < FProceedTime then
  begin
    // Compute Position
    tempPos := FInitialPos;
    RotateVector(tempPos, Vector3fMake(FRotAxis), FAngle * FElapsedTime /
      FProceedTime);
    FJoblist.FController.FCamera.AbsolutePosition :=
      VectorAdd(FJoblist.FController.FCameraTarget.AbsolutePosition, tempPos);
    // Compute Direction vector
    tempDir := FInitialDir;
    RotateVector(tempDir, Vector3fMake(FRotAxis), FAngle * FElapsedTime /
      FProceedTime);
    FJoblist.FController.FCamera.AbsoluteDirection := tempDir;
    // Compute Up Vector
    tempUp := FInitialUp;
    RotateVector(tempUp, Vector3fMake(FRotAxis), FAngle * FElapsedTime /
      FProceedTime);
    FJoblist.FController.FCamera.AbsoluteUp := tempUp;
  end
  else
  begin
    // Compute Position
    tempPos := FInitialPos;
    RotateVector(tempPos, Vector3fMake(FRotAxis), FAngle);
    FJoblist.FController.FCamera.AbsolutePosition :=
      VectorAdd(FJoblist.FController.FCameraTarget.AbsolutePosition, tempPos);
    // Compute Direction vector
    tempDir := FInitialDir;
    RotateVector(tempDir, Vector3fMake(FRotAxis), FAngle);
    FJoblist.FController.FCamera.AbsoluteDirection := tempDir;
    // Compute Up Vector
    tempUp := FInitialUp;
    RotateVector(tempUp, Vector3fMake(FRotAxis), FAngle);
    FJoblist.FController.FCamera.AbsoluteUp := tempUp;
    FRunning := False;
  end;
end;

//-------------------------------------
// TGLSmoothOrbitToPosAdvJob
//-------------------------------------

procedure TGLSmoothOrbitToPosAdvJob.Init;
var
  Right: TGLVector;
begin
  FProceedTime := Time;
  FInitialPos := VectorSubtract(FPreviousPosition,
    FJoblist.FController.FCameraTarget.AbsolutePosition);
  if Assigned(FJoblist.FController.FCamera.Parent) then
    FFinalPos := VectorSubtract
      (FJoblist.FController.FCamera.Parent.LocalToAbsolute(VectorMake(X, Y, Z,
      1)), FJoblist.FController.FCameraTarget.AbsolutePosition)
  else
    FFinalPos := VectorSubtract(VectorMake(X, Y, Z, 1),
      FJoblist.FController.FCameraTarget.AbsolutePosition);
  // if destination is Target Pos, we can't compute
  if VectorLength(FFinalPos) < cEPSILON then
  begin
    // FAllowUserAction := True;
    Exit;
  end;
  // Compute Angle of Rotation
  FAngle := ArcCos(VectorAngleCosine(Vector3fMake(FFinalPos),
    Vector3fMake(FInitialPos)));
  Right := VectorNormalize(VectorCrossProduct(
    // FJobList.FController.FCamera.AbsoluteVectorToTarget,
    VectorNormalize(VectorSubtract(FJoblist.FController.FCameraTarget.
    AbsolutePosition, FPreviousPosition)),
    FJoblist.FController.FCamera.AbsoluteUp));
  FInitialDir := FJoblist.FController.FCamera.AbsoluteDirection;
  FInitialUp := FJoblist.FController.FCamera.AbsoluteUp;
  // Determine rotation Axis
  // if Angle equals 0 degrees.
  if FAngle < cEPSILON then
    if PreferUpAxis then
      FRotAxis := VectorNormalize
        (VectorCrossProduct(VectorCrossProduct(FFinalPos, FInitialUp),
        FFinalPos))
    else
      FRotAxis := Right
  else
    // if Angle equals 180 degrees.
    if FAngle > Pi - cEPSILON then
      if PreferUpAxis then
        FRotAxis := VectorNormalize
          (VectorCrossProduct(VectorCrossProduct(FFinalPos, FInitialUp),
          FFinalPos))
      else
        FRotAxis := Right
    else
      FRotAxis := VectorNormalize(VectorCrossProduct(FFinalPos, FInitialPos));
end;

procedure TGLSmoothOrbitToPosAdvJob.Step;
var
  tempUp, tempDir, tempPos: TGLVector;
begin
  if FElapsedTime < FProceedTime then
  begin
    // Compute Position
    tempPos := FInitialPos;
    RotateVector(tempPos, Vector3fMake(FRotAxis), FAngle * FElapsedTime /
      FProceedTime);
    FSmoothNavigator.TargetValue.DirectVector :=
      VectorAdd(FJoblist.FController.FCameraTarget.AbsolutePosition, tempPos);
    FPreviousPosition := FSmoothNavigator.TargetValue.DirectVector;
    // Compute Direction vector
    tempDir := FInitialDir;
    RotateVector(tempDir, Vector3fMake(FRotAxis), FAngle * FElapsedTime /
      FProceedTime);
    FJoblist.FController.FCamera.AbsoluteDirection := tempDir;
    // Compute Up Vector
    if FRestoreUpVector then
      FJoblist.FController.FCamera.AbsoluteUp := FInitialUp
    else
    begin
      tempUp := FInitialUp;
      RotateVector(tempUp, Vector3fMake(FRotAxis), FAngle * FElapsedTime /
        FProceedTime);
      FJoblist.FController.FCamera.AbsoluteUp := tempUp;
    end;
  end
  else
  begin
    // Compute Position
    tempPos := FInitialPos;
    RotateVector(tempPos, Vector3fMake(FRotAxis), FAngle);
    FJoblist.FController.FCamera.AbsolutePosition :=
      VectorAdd(FJoblist.FController.CameraTarget.AbsolutePosition, tempPos);
    // Compute Direction vector
    tempDir := FInitialDir;
    RotateVector(tempDir, Vector3fMake(FRotAxis), FAngle);
    FJoblist.FController.FCamera.AbsoluteDirection := tempDir;

    // Compute Up Vector
    if FRestoreUpVector then
      FJoblist.FController.FCamera.AbsoluteUp := FInitialUp
    else
    begin
      tempUp := FInitialUp;
      RotateVector(tempUp, Vector3fMake(FRotAxis), FAngle);
      FJoblist.FController.FCamera.AbsoluteUp := tempUp;
      FRunning := False;
    end;
    FRunning := False;
  end;
end;

//-------------------------------------
// TGLSmoothOrbitToPosAdv
//-------------------------------------

constructor TGLSmoothOrbitToPos.Create(const AJoblist: TGLCameraJobList);
begin
  inherited;
  FCutoffAngle := 0.1;
end;

procedure TGLSmoothOrbitToPos.Step;
var
  lCurrentDistanceToTarget: Single;
  lTargetPosition: TGLVector;
  lCurrentMatrix: TGLMatrix;
  lAngle: Single;
  lAbsTargetPosition: TGLVector;

  procedure RestoreDistanceToTarget();
  var
    lDirection: TGLVector;
  begin
    lDirection := VectorNormalize
      (VectorSubtract(FJoblist.FController.FCameraTarget.AbsolutePosition,
      FJoblist.FController.FCamera.AbsolutePosition));

    FJoblist.FController.FCamera.AbsolutePosition :=
      VectorAdd(FJoblist.FController.FCameraTarget.AbsolutePosition,
      VectorScale(lDirection, -lCurrentDistanceToTarget));
  end;

  procedure SetTargetValueRelative(const AAbsolutePosition: TGLVector);
  begin
    if FJoblist.FController.FCamera.Parent = nil then
      FSmoothNavigator.TargetValue.DirectVector := AAbsolutePosition
    else
      FSmoothNavigator.TargetValue.DirectVector :=
        FJoblist.FController.FCamera.Parent.AbsoluteToLocal(AAbsolutePosition);
  end;

  procedure ApplyDistanceToResult();
  var
    lDirection, lNewTargetPosition: TGLVector;
  begin
    lDirection := VectorNormalize
      (VectorSubtract(FJoblist.FController.FCameraTarget.AbsolutePosition,
      lAbsTargetPosition));
    lNewTargetPosition :=
      VectorAdd(FJoblist.FController.FCameraTarget.AbsolutePosition,
      VectorScale(lDirection, -lCurrentDistanceToTarget));
    SetTargetValueRelative(lNewTargetPosition);
  end;

begin
  if FElapsedTime < FProceedTime then
  begin
    // Save current matrix.
    lCurrentMatrix := FJoblist.FController.FCamera.Matrix^;

    if FNeedToRecalculateZoom then
      lCurrentDistanceToTarget := FJoblist.FController.FCamera.DistanceTo
        (FJoblist.FController.FCameraTarget)
    else
      lCurrentDistanceToTarget := 0; // To avoid warning message.

    // Calculate the position, in which camera should have been.
    FJoblist.FController.FCamera.SetMatrix(FShouldBeMatrix);

    FJoblist.FController.FCamera.AbsolutePosition :=
      MoveObjectAround(FJoblist.FController.FCamera.AbsolutePosition,
      FCameraUpVector, FJoblist.FController.FCameraTarget.AbsolutePosition,
      FRotateSpeed.X * FDeltaTime, FRotateSpeed.Y * FDeltaTime);

    if FNeedToRecalculateZoom then
      RestoreDistanceToTarget();

    lTargetPosition := FJoblist.FController.FCamera.AbsolutePosition;
    FShouldBeMatrix := FJoblist.FController.FCamera.Matrix^;

    // Restore Camera position and move it to the desired vector.
    FJoblist.FController.FCamera.SetMatrix(lCurrentMatrix);
    SetTargetValueRelative(lTargetPosition);
  end
  else
  begin
    if FNeedToRecalculateZoom then
    begin
      if FJoblist.FController.FCamera.Parent = nil then
        lAbsTargetPosition := FFinalPos
      else
        lAbsTargetPosition := FJoblist.FController.FCamera.Parent.
          LocalToAbsolute(FFinalPos);

      lAngle := RadToDeg
        (AngleBetweenVectors(FJoblist.FController.FCamera.AbsolutePosition,
        lAbsTargetPosition,
        FJoblist.FController.FCameraTarget.AbsolutePosition));
      if lAngle < FCutoffAngle then
      begin
        FSmoothNavigator.Enabled := False;
        FRunning := False;
      end
      else
      begin
        lCurrentDistanceToTarget := FJoblist.FController.FCamera.DistanceTo
          (FJoblist.FController.FCameraTarget);
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
