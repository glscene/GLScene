//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.SmoothNavigator;

(*
   An extention of TGLNavigator, which allows to move objects with inertia
   Note: it is not completely FPS-independant. Only Moving code is, but
   MoveAroundTarget, Turn[Vertical/Horizontal] and AdjustDistanceTo[..] is not.
     Don't know why, but when I make their code identical, these function stop
   working completely. So you probably have to call the AutoScaleParameters
   procedure once in a while for it to adjust to the current framerate.
   If someone knows a better way to solve this issue, please contact me via
   glscene newsgroups. 

    TODO:
      1) Scale "Old values" too, when callin the Scale parameter procedure to
         avoid the temporary "freeze" of controls.
      2) AddImpulse procedures.
*)

interface

{$I GLScene.inc}

uses
  System.Types,
  System.Classes,
  
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.VectorTypes, 
  GLS.Navigator,
  GLS.VectorGeometry,
  GLS.Coordinates,
  GLS.Screen, 
  GLS.XCollection;

type

  (* Includes a basic set of parameters
     that control the smoothness of movement. *)
  TGLNavigatorAbstractParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    FInertia: Single;
    FSpeed: Single;
    FCutoff: Single;
    function StoreCutoff: Boolean;
  protected
    function StoreInertia: Boolean; virtual;
    function StoreSpeed: Boolean; virtual;
      
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;
  published
    property Inertia: Single read FInertia write FInertia stored StoreInertia;
    property Speed: Single read FSpeed write FSpeed stored StoreSpeed;
    property Cutoff: Single read FCutoff write FCutoff stored StoreCutoff; 
  end;

  TGLSmoothNavigator = class;

  (* Includes a basic set of parameters
     that control the smoothness of movement. *)
  TGLNavigatorSmoothChangeItem = class(TXCollectionItem)
  private
    FInertia: Single;
    FSpeed: Single;
    FEnabled: Boolean;
    FSpeedLimit: Single;
    FCutoff: Double;
    function StoreInertia: Boolean;
    function StoreSpeed: Boolean;
    function StoreSpeedLimit: Boolean;
    function StoreCutoff: Boolean;
  protected
    function GetNavigator: TGLSmoothNavigator;
  public
    // Returns False if there was no change.
    function Proceed(ADeltaTime: Double): Boolean; virtual; abstract;
    constructor Create(aOwner: TXCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;
    procedure ResetTargetValue(); virtual; abstract;
  published
    property Inertia: Single read FInertia write FInertia stored StoreInertia;
    property Speed: Single read FSpeed write FSpeed stored StoreSpeed;
    property SpeedLimit: Single read FSpeedLimit write FSpeedLimit stored StoreSpeedLimit;
    property Cutoff: Double read FCutoff write FCutoff stored StoreCutoff;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;

  TGLNavigatorSmoothChangeSingle = class;
  TGLNavigatorSmoothChangeSingleGetEvent = function(const ASender: TGLNavigatorSmoothChangeSingle): Single of object;
  TGLNavigatorSmoothChangeSingleSetEvent = procedure(const ASender: TGLNavigatorSmoothChangeSingle; const AValue: Single) of object;

  // Smoothly change any Single value, so it will become TargetValue in the end.
  TGLNavigatorSmoothChangeSingle = class(TGLNavigatorSmoothChangeItem)
  private
    FTargetValue: Single;
    FOnGetCurrentValue: TGLNavigatorSmoothChangeSingleGetEvent;
    FOnSetCurrentValue: TGLNavigatorSmoothChangeSingleSetEvent;
  public
    class function FriendlyName: string; override;
    function Proceed(ADeltaTime: Double): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure ResetTargetValue(); override;    
  published
    property TargetValue: Single read FTargetValue write FTargetValue;
    property OnGetCurrentValue: TGLNavigatorSmoothChangeSingleGetEvent read FOnGetCurrentValue write FOnGetCurrentValue;
    property OnSetCurrentValue: TGLNavigatorSmoothChangeSingleSetEvent read FOnSetCurrentValue write FOnSetCurrentValue;
  end;

  TGLNavigatorSmoothChangeVector = class;
  TGLNavigatorSmoothChangeVectorGetEvent = function(const ASender: TGLNavigatorSmoothChangeVector): TGLVector of object;
  TGLNavigatorSmoothChangeVectorSetEvent = procedure(const ASender: TGLNavigatorSmoothChangeVector; const AValue: TGLVector) of object;

  // Smoothly change any Vector4f value, so it will become TargetValue in the end.
  TGLNavigatorSmoothChangeVector = class(TGLNavigatorSmoothChangeItem)
  private
    FTargetValue: TGLCoordinates;
    FOnGetCurrentValue: TGLNavigatorSmoothChangeVectorGetEvent;
    FOnSetCurrentValue: TGLNavigatorSmoothChangeVectorSetEvent;
    procedure SetTargetValue(const Value: TGLCoordinates);
  public
    class function FriendlyName: string; override;
    function Proceed(ADeltaTime: Double): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure ResetTargetValue(); override;
  published
    property TargetValue: TGLCoordinates read FTargetValue write SetTargetValue;
    property OnGetCurrentValue: TGLNavigatorSmoothChangeVectorGetEvent read FOnGetCurrentValue write FOnGetCurrentValue;
    property OnSetCurrentValue: TGLNavigatorSmoothChangeVectorSetEvent read FOnSetCurrentValue write FOnSetCurrentValue;
  end;

  TGLNavigatorSmoothChangeItemClass = class of TGLNavigatorSmoothChangeItem;

  // XCollection of TGLNavigatorSmoothChangeItem.
  TGLNavigatorSmoothChangeItems = class(TXCollection)
  private
    function GetItems(const Index : Integer): TGLNavigatorSmoothChangeItem;
    procedure SetItems(const Index : Integer; const Value: TGLNavigatorSmoothChangeItem);
  protected
    procedure DoProceed(ADeltaTime: Double);
  public
    function Add(AClass : TGLNavigatorSmoothChangeItemClass): TGLNavigatorSmoothChangeItem;
    function CanAdd(AClass: TXCollectionItemClass): Boolean; override;
    class function ItemsClass: TXCollectionItemClass; override;
    property Items[const Index : Integer]: TGLNavigatorSmoothChangeItem read GetItems write
            SetItems; default;
  end;

  (* The wrapper for all parameters that
     affect how the AdjustDisanceTo[...] methods work *)
  TGLNavigatorAdjustDistanceParameters = class(TGLNavigatorAbstractParameters)
  private
    FOldDistanceRatio: Single;
    FImpulseSpeed: Single;
    function StoreImpulseSpeed: Boolean;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); override;

    procedure AddImpulse(const Impulse: Single); virtual;
  published
    property ImpulseSpeed: Single read FImpulseSpeed write FImpulseSpeed stored StoreImpulseSpeed;
  end;

  (* The wrapper for all parameters that
     affect how the AdjustDisanceTo[...]Ex methods work
     You need to set the TargetObject and desired distance to it,
     then call AdjustDisanceTo[...]Ex() in your Cadencer.OnProgress code. *)
  TGLNavigatorAdjustDistanceParametersEx = class(TGLNavigatorAbstractParameters)
  private
    FSpeedLimit: Single;
    FTargetDistance: Single;
    function StoreSpeedLimit: Boolean;
    function StoreTargetDistance: Boolean;
  protected
    function StoreSpeed: Boolean; override;
    function StoreInertia: Boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property TargetDistance: Single read FTargetDistance write FTargetDistance stored StoreTargetDistance;
    property SpeedLimit: Single read FSpeedLimit write FSpeedLimit stored StoreSpeedLimit;
  end;

  (* The wrapper for all parameters that affect the
       smoothness of movement *)
  TGLNavigatorInertiaParameters = class(TPersistent)
  private
    FOwner: TPersistent;

    OldTurnHorizontalAngle: Single;
    OldTurnVerticalAngle: Single;

    OldMoveForwardDistance: Single;
    OldStrafeHorizontalDistance: Single;
    OldStrafeVerticalDistance: Single;
    FTurnInertia: Single;
    FTurnSpeed: Single;
    FTurnMaxAngle: Single;
    FMovementAcceleration: Single;
    FMovementInertia: Single;
    FMovementSpeed: Single;
    function StoreTurnMaxAngle: Boolean;
    function StoreMovementAcceleration: Boolean;
    function StoreMovementInertia: Boolean;
    function StoreMovementSpeed: Boolean;
    function StoreTurnInertia: Boolean;
    function StoreTurnSpeed: Boolean;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;
  published
    property MovementAcceleration: Single read FMovementAcceleration write FMovementAcceleration stored StoreMovementAcceleration;
    property MovementInertia: Single read FMovementInertia write FMovementInertia stored StoreMovementInertia;
    property MovementSpeed: Single read FMovementSpeed write FMovementSpeed stored StoreMovementSpeed;
    property TurnMaxAngle: Single read FTurnMaxAngle write FTurnMaxAngle stored StoreTurnMaxAngle;
    property TurnInertia: Single read FTurnInertia write FTurnInertia stored StoreTurnInertia;
    property TurnSpeed: Single read FTurnSpeed write FTurnSpeed stored StoreTurnSpeed;
  end;

  (* The wrapper for all general inertia parameters.
     These properties mean that if ExpectedMaxFPS is 100, FAutoScaleMin is 0.1,
     FAutoScaleMax is 0.75 then the "safe range" for it to change is [10..75].
     If these bounds are violated, then ExpectedMaxFPS is automaticly increased
     or decreased by AutoScaleMult. *)
  TGLNavigatorGeneralParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    FAutoScaleMin: Single;
    FAutoScaleMax: Single;
    FAutoScaleMult: Single;
    function StoreAutoScaleMax: Boolean;
    function StoreAutoScaleMin: Boolean;
    function StoreAutoScaleMult: Boolean;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoScaleMin: Single read FAutoScaleMin write FAutoScaleMin stored StoreAutoScaleMin;
    property AutoScaleMax: Single read FAutoScaleMax write FAutoScaleMax stored StoreAutoScaleMax;
    property AutoScaleMult: Single read FAutoScaleMult write FAutoScaleMult stored StoreAutoScaleMult;
  end;

  (* The wrapper for all parameters that
     effect how the TGLBaseSceneObject.MoveObjectAround() procedure works *)
  TGLNavigatorMoveAroundParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    FTargetObject: TGLBaseSceneObject;
    FOldPitchInertiaAngle : Single;
    FOldTurnInertiaAngle  : Single;
    FPitchSpeed : Single;
    FTurnSpeed  : Single;
    FInertia          : Single;
    FMaxAngle         : Single;
    FCutoff: Double;
    function StoreInertia: Boolean;
    function StoreMaxAngle: Boolean;
    function StorePitchSpeed: Boolean;
    function StoreTurnSpeed: Boolean;
    procedure SetTargetObject(const Value: TGLBaseSceneObject);
    function StoreCutoff: Boolean;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure ScaleParameters(const Value: Single); virtual;
  published
    property Inertia: Single read FInertia write FInertia stored StoreInertia;
    property MaxAngle: Single read FMaxAngle write FMaxAngle stored StoreMaxAngle;
    property PitchSpeed: Single read FPitchSpeed write FPitchSpeed stored StorePitchSpeed;
    property TurnSpeed: Single read FTurnSpeed write FTurnSpeed stored StoreTurnSpeed;
    property TargetObject: TGLBaseSceneObject read FTargetObject write SetTargetObject;
    property Cutoff: Double read FCutoff write FCutoff stored StoreCutoff;
  end;

  (* The component for moving a TGLBaseSceneObject, and all
     classes based on it, this includes all the objects from the Scene Editor.
     It uses complex smoothing algorithms, most of which are FPS-dependant.
     Make sure your limit your FPS and set MaxExpectedDeltaTime to a value
     that is aproximatly 5 times less than your usual deltatime. *)
  TGLSmoothNavigator = class(TGLNavigator)
  private
    FMaxExpectedDeltaTime: Double;
    FInertiaParams: TGLNavigatorInertiaParameters;
    FGeneralParams: TGLNavigatorGeneralParameters;
    FMoveAroundParams: TGLNavigatorMoveAroundParameters;
    FAdjustDistanceParams: TGLNavigatorAdjustDistanceParameters;
    FAdjustDistanceParamsEx: TGLNavigatorAdjustDistanceParametersEx;
    FCustomAnimatedItems: TGLNavigatorSmoothChangeItems;
    procedure SetInertiaParams(const Value: TGLNavigatorInertiaParameters);
    function StoreMaxExpectedDeltaTime: Boolean;
    procedure SetGeneralParams(const Value: TGLNavigatorGeneralParameters);
    procedure SetMoveAroundParams(const Value: TGLNavigatorMoveAroundParameters);
    procedure SetAdjustDistanceParams(const Value: TGLNavigatorAdjustDistanceParameters);
    procedure SetAdjustDistanceParamsEx(
      const Value: TGLNavigatorAdjustDistanceParametersEx);
    procedure SetCustomAnimatedItems(
      const Value: TGLNavigatorSmoothChangeItems);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    // Constructors-destructors.
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // From TGLNavigator. Probably, should not be public.
    procedure SetObject(Value: TGLBaseSceneObject); override;
    // Uses InertiaParams.
    procedure TurnHorizontal(Angle: Single; ADeltaTime: Double); virtual;
    procedure TurnVertical(Angle: Single; ADeltaTime: Double); virtual;
    procedure FlyForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure MoveForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure StrafeHorizontal(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure StrafeVertical(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    // Uses MoveAroundParams. Returns True, if object was actually moved.
    function MoveAroundTarget(const PitchDelta, TurnDelta : Single; const ADeltaTime: Double): Boolean;
    function MoveObjectAround(const AObject: TGLBaseSceneObject; PitchDelta, TurnDelta : Single; ADeltaTime: Double): Boolean;
    // Uses AdjustDistanceParams.
    function AdjustDistanceToPoint(const  APoint: TGLVector; const DistanceRatio : Single; ADeltaTime: Double): Boolean;
    function AdjustDistanceToTarget(const DistanceRatio : Single; const ADeltaTime: Double): Boolean;
    // Uses AdjustDistanceParamsEx.
    function AdjustDistanceToPointEx(const  APoint: TGLVector; ADeltaTime: Double): Boolean;
    function AdjustDistanceToTargetEx(const ADeltaTime: Double): Boolean;
    // Uses CustomAnimatedItems.
    procedure AnimateCustomItems(const ADeltaTime: Double); virtual;
    // Uses GeneralParams. In ScaleParameters, Value should be around 1.
    procedure ScaleParameters(const Value: Single); virtual;
    procedure AutoScaleParameters(const FPS: Single); virtual;
    procedure AutoScaleParametersUp(const FPS: Single); virtual;
  published
    property MaxExpectedDeltaTime: Double read FMaxExpectedDeltaTime write FMaxExpectedDeltaTime stored StoreMaxExpectedDeltaTime;
    property InertiaParams: TGLNavigatorInertiaParameters read FInertiaParams write SetInertiaParams;
    property GeneralParams: TGLNavigatorGeneralParameters read FGeneralParams write SetGeneralParams;
    property MoveAroundParams: TGLNavigatorMoveAroundParameters read FMoveAroundParams write SetMoveAroundParams;
    property AdjustDistanceParams: TGLNavigatorAdjustDistanceParameters read FAdjustDistanceParams write SetAdjustDistanceParams;
    property AdjustDistanceParamsEx: TGLNavigatorAdjustDistanceParametersEx read FAdjustDistanceParamsEx write SetAdjustDistanceParamsEx;
    property CustomAnimatedItems: TGLNavigatorSmoothChangeItems read FCustomAnimatedItems write SetCustomAnimatedItems;
  end;

  (* The component which reads the userinput and transform it into action.
	   Mouselook(ADeltaTime: double) : handles mouse look... Should be called
        in the Cadencer event. (Though it works everywhere!)
	   The four properties to get you started are:
           InvertMouse     : Inverts the mouse Y axis.
	   AutoUpdateMouse : If enabled (by defaul), than handles all mouse updates.
	   GLNavigator     : The Navigator which receives the user movement.
	   GLVertNavigator : The Navigator which if set receives the vertical user
                       movement. Used mostly for cameras.... *)
  TGLSmoothUserInterface = class(TComponent)
  private
    FAutoUpdateMouse: Boolean;
    FMouseLookActive: Boolean;
    FSmoothNavigator: TGLSmoothNavigator;
    FSmoothVertNavigator: TGLSmoothNavigator;
    FInvertMouse: Boolean;
    FOriginalMousePos: TGLCoordinates2;
    procedure SetSmoothNavigator(const Value: TGLSmoothNavigator); virtual;
    procedure SetOriginalMousePos(const Value: TGLCoordinates2); virtual;
    procedure SetSmoothVertNavigator(const Value: TGLSmoothNavigator); virtual;
    procedure SetMouseLookActive(const Value: Boolean); virtual;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TurnHorizontal(const Angle : Single; const ADeltaTime: Double); virtual;
    procedure TurnVertical(const Angle : Single; const ADeltaTime: Double); virtual;
    procedure MouseLookActiveToggle; virtual;
    function MouseLook(const ADeltaTime: Double): Boolean; overload;
    function MouseLook(const NewXY: TPoint; const ADeltaTime: Double): Boolean; overload;
    function MouseLook(const NewX, NewY: Integer; const ADeltaTime: Double): Boolean; overload;
  published
    property AutoUpdateMouse: Boolean read FAutoUpdateMouse write FAutoUpdateMouse default True;
    property MouseLookActive: Boolean read FMouseLookActive write SetMouseLookActive default False;
    property SmoothVertNavigator: TGLSmoothNavigator read FSmoothVertNavigator write SetSmoothVertNavigator;
    property SmoothNavigator: TGLSmoothNavigator read FSmoothNavigator write SetSmoothNavigator;
    property InvertMouse: Boolean read FInvertMouse write FInvertMouse default False;
    property OriginalMousePos: TGLCoordinates2 read FOriginalMousePos write SetOriginalMousePos;
  end;

//----------------------------------------------------
implementation
//----------------------------------------------------

const
  EPS =  0.001;
  EPS2 = 0.0001;
  EPS8 = 0.00000001;

(*******************************************
 TGLSmoothNavigator
*******************************************)

constructor TGLSmoothNavigator.Create(AOwner: TComponent);
begin
  inherited;
  FMaxExpectedDeltaTime := 0.001;
  FInertiaParams := TGLNavigatorInertiaParameters.Create(Self);
  FGeneralParams := TGLNavigatorGeneralParameters.Create(Self);
  FMoveAroundParams := TGLNavigatorMoveAroundParameters.Create(Self);
  FAdjustDistanceParams := TGLNavigatorAdjustDistanceParameters.Create(Self);
  FAdjustDistanceParamsEx := TGLNavigatorAdjustDistanceParametersEx.Create(Self);
  FCustomAnimatedItems := TGLNavigatorSmoothChangeItems.Create(Self);
end;

destructor TGLSmoothNavigator.Destroy;
begin
  FInertiaParams.Free;
  FGeneralParams.Free;
  FMoveAroundParams.Free;
  FAdjustDistanceParams.Free;
  FAdjustDistanceParamsEx.Free;
  FCustomAnimatedItems.Free;
  inherited;
end;

procedure TGLSmoothNavigator.SetInertiaParams(
  const Value: TGLNavigatorInertiaParameters);
begin
  FInertiaParams.Assign(Value);
end;

procedure TGLSmoothNavigator.TurnHorizontal(Angle: Single; ADeltaTime: Double);
var
  FinalAngle: Single;
begin
  with FInertiaParams do
  begin
    FinalAngle := 0;
    Angle := Angle * FTurnSpeed;
    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      Angle := ClampValue((Angle * FMaxExpectedDeltaTime + OldTurnHorizontalAngle * FTurnInertia) / (FTurnInertia + 1), -FTurnMaxAngle, FTurnMaxAngle);
      OldTurnHorizontalAngle := Angle;
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalAngle := FinalAngle + Angle;
    end;
  end;

  if (Abs(FinalAngle) > EPS) then
    inherited TurnHorizontal(FinalAngle);
end;

procedure TGLSmoothNavigator.TurnVertical(Angle: Single; ADeltaTime: Double);
var
  FinalAngle: Single;
begin
  with FInertiaParams do
  begin
    FinalAngle := 0;
    Angle := Angle * FTurnSpeed;
    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      Angle := ClampValue((Angle * FMaxExpectedDeltaTime + OldTurnVerticalAngle * FTurnInertia) / (FTurnInertia + 1), -FTurnMaxAngle, FTurnMaxAngle);
      OldTurnVerticalAngle := Angle;
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalAngle := FinalAngle + Angle;
    end;
  end;

  if (Abs(FinalAngle) > EPS) then
    inherited TurnVertical(FinalAngle);
end;


procedure TGLSmoothNavigator.MoveForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;

    if Accelerate then
      Distance := Distance * FMovementAcceleration;

    FinalDistance := 0;

    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      OldMoveForwardDistance := (Distance * FMaxExpectedDeltaTime + OldMoveForwardDistance * FMovementInertia) / (FMovementInertia + 1);
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldMoveForwardDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited MoveForward(FinalDistance);
end;

procedure TGLSmoothNavigator.FlyForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;
    if Accelerate then
      Distance := Distance * FMovementAcceleration;
    FinalDistance := 0;
    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      OldMoveForwardDistance := (Distance * FMaxExpectedDeltaTime + OldMoveForwardDistance * FMovementInertia) / (FMovementInertia + 1);
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldMoveForwardDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited FlyForward(FinalDistance);
end;

procedure TGLSmoothNavigator.StrafeHorizontal(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;

    if Accelerate then
      Distance := Distance * FMovementAcceleration;

    FinalDistance := 0;

    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      OldStrafeHorizontalDistance := (Distance * FMaxExpectedDeltaTime + OldStrafeHorizontalDistance * FMovementInertia) / (FMovementInertia + 1);
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldStrafeHorizontalDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited StrafeHorizontal(FinalDistance);
end;

procedure TGLSmoothNavigator.StrafeVertical(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
var
  FinalDistance: Single;
  Distance:      Single;
begin
  with FInertiaParams do
  begin
    if Plus then
      Distance := FMovementSpeed
    else if Minus then
      Distance := -FMovementSpeed
    else
      Distance := 0;
    if Accelerate then
      Distance := Distance * FMovementAcceleration;
    FinalDistance := 0;
    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      OldStrafeVerticalDistance := (Distance * FMaxExpectedDeltaTime + OldStrafeVerticalDistance * FMovementInertia) / (FMovementInertia + 1);
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalDistance := FinalDistance + OldStrafeVerticalDistance;
    end;
  end;

  if Abs(FinalDistance) > EPS then
    inherited StrafeVertical(FinalDistance);
end;

procedure TGLSmoothNavigator.AutoScaleParameters(const FPS: Single);
begin
  with FGeneralParams do
  begin
    if FPS > FAutoScaleMax / FMaxExpectedDeltatime then
      ScaleParameters(FAutoScaleMult)
    else if FPS < FAutoScaleMin / FMaxExpectedDeltatime then
      ScaleParameters(1/FAutoScaleMult);
  end;
end;

procedure TGLSmoothNavigator.AutoScaleParametersUp(const FPS: Single);
begin
  with FGeneralParams do
  begin
    if FPS > FAutoScaleMax / FMaxExpectedDeltatime then
      ScaleParameters(FAutoScaleMult)
  end;
end;

procedure TGLSmoothNavigator.ScaleParameters(const Value: Single);
begin
  Assert(Value > 0);
  FMaxExpectedDeltatime := FMaxExpectedDeltatime / Value;
  FInertiaParams.ScaleParameters(Value);
  FMoveAroundParams.ScaleParameters(Value);
  FAdjustDistanceParams.ScaleParameters(Value);
end;

function TGLSmoothNavigator.StoreMaxExpectedDeltaTime: Boolean;
begin
  Result := Abs(FMaxExpectedDeltaTime - 0.001) > EPS2;
end;

procedure TGLSmoothNavigator.SetGeneralParams(
  const Value: TGLNavigatorGeneralParameters);
begin
  FGeneralParams.Assign(Value);
end;

procedure TGLSmoothNavigator.SetMoveAroundParams(
  const Value: TGLNavigatorMoveAroundParameters);
begin
  FMoveAroundParams.Assign(Value);
end;

procedure TGLSmoothNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMoveAroundParams.FTargetObject then
      FMoveAroundParams.FTargetObject := nil;
  end;
end;

procedure TGLSmoothNavigator.SetObject(Value: TGLBaseSceneObject);
var
  I: Integer;
begin
  inherited;
  // Try to detect a TargetObject.
  if Value <> nil then
    if FMoveAroundParams.TargetObject = nil then
    begin
      // May be it is a camera...
      if Value is TGLCamera then
        FMoveAroundParams.TargetObject := TGLCamera(Value).TargetObject
      else
      begin
        // May be it has camera children...
        if Value.Count <> 0 then
          for I := 0 to Value.Count - 1 do
            if Value.Children[I] is TGLCamera then
            begin
              FMoveAroundParams.TargetObject := TGLCamera(Value.Children[I]).TargetObject;
              Exit;
            end;
      end;
    end;
end;

function TGLSmoothNavigator.MoveAroundTarget(const PitchDelta, TurnDelta: Single;
  const ADeltaTime: Double): Boolean;
begin
  Result := MoveObjectAround(FMoveAroundParams.FTargetObject, PitchDelta, TurnDelta, ADeltaTime);
end;

function TGLSmoothNavigator.MoveObjectAround(
  const AObject: TGLBaseSceneObject; PitchDelta, TurnDelta: Single;
  ADeltaTime: Double): Boolean;
var
  FinalPitch: Single;
  FinalTurn:  Single;

  lUp: TGLVector;
begin
  Result := False;
  FinalPitch := 0;
  FinalTurn := 0;
  with FMoveAroundParams do
  begin
    PitchDelta := PitchDelta * FPitchSpeed;
    TurnDelta := TurnDelta * FTurnSpeed;

    while ADeltaTime > FMaxExpectedDeltatime do
    begin
      PitchDelta := ClampValue((PitchDelta * FMaxExpectedDeltatime + FOldPitchInertiaAngle * FInertia) / (FInertia + 1), - FMaxAngle, FMaxAngle);
      FOldPitchInertiaAngle := PitchDelta;
      FinalPitch := FinalPitch + PitchDelta;
      TurnDelta := ClampValue((TurnDelta * FMaxExpectedDeltatime + FOldTurnInertiaAngle * FInertia) / (FInertia + 1), - FMaxAngle, FMaxAngle);
      FOldTurnInertiaAngle := TurnDelta;
      FinalTurn := FinalTurn + TurnDelta;

      ADeltaTime := ADeltaTime - FMaxExpectedDeltatime;
    end;

    if UseVirtualUp then
      lUp := VirtualUp.AsVector
    else
      lUp := MovingObject.AbsoluteUp;

    if (Abs(FinalPitch) > FCutOff) or (Abs(FinalTurn) > FCutOff) then
    begin
      MovingObject.AbsolutePosition := GLS.VectorGeometry.MoveObjectAround(
        MovingObject.AbsolutePosition, lUp, AObject.AbsolutePosition, FinalPitch, FinalTurn);
      Result := True;
    end;
  end;
end;


function TGLSmoothNavigator.AdjustDistanceToPoint(const APoint: TGLVector;
  const DistanceRatio: Single; ADeltaTime: Double): Boolean;

  // Based on TGLCamera.AdjustDistanceToTarget
  procedure DoAdjustDistanceToPoint(const DistanceRatio: Single);
  var
    vect: TGLVector;
  begin
    vect := VectorSubtract(MovingObject.AbsolutePosition, APoint);
    ScaleVector(vect, (distanceRatio - 1));
    AddVector(vect, MovingObject.AbsolutePosition);
    if Assigned(MovingObject.Parent) then
       vect := MovingObject.Parent.AbsoluteToLocal(vect);
    MovingObject.Position.AsVector := vect;
    Result := True;
  end;

var
  FinalDistanceRatio: Single;
  TempDistanceRatio:  Single;
begin
  with FAdjustDistanceParams do
  begin
    TempDistanceRatio := DistanceRatio * FSpeed;
    FinalDistanceRatio := 0;
    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      TempDistanceRatio := (TempDistanceRatio * FMaxExpectedDeltaTime + FOldDistanceRatio * FInertia) / (FInertia + 1);
      FOldDistanceRatio := TempDistanceRatio;
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
      FinalDistanceRatio := FinalDistanceRatio + FOldDistanceRatio / FMaxExpectedDeltaTime;
    end;
    if Abs(FinalDistanceRatio) > FCutoff then
    begin
      if FinalDistanceRatio > 0 then
        DoAdjustDistanceToPoint(1 / (1 + FinalDistanceRatio))
      else
        DoAdjustDistanceToPoint(1 * (1 - FinalDistanceRatio))
    end
    else
      Result := False;
  end;
end;

function TGLSmoothNavigator.AdjustDistanceToTarget(const DistanceRatio: Single;
  const ADeltaTime: Double): Boolean;
begin
  Assert(FMoveAroundParams.FTargetObject <> nil);
  Result := AdjustDistanceToPoint(FMoveAroundParams.FTargetObject.AbsolutePosition,
                        DistanceRatio, ADeltaTime);
end;

procedure TGLSmoothNavigator.SetAdjustDistanceParams(
  const Value: TGLNavigatorAdjustDistanceParameters);
begin
  FAdjustDistanceParams.Assign(Value);
end;

function TGLSmoothNavigator.AdjustDistanceToPointEx(const APoint: TGLVector;
  ADeltaTime: Double): Boolean;

var
  lAbsolutePosition: TGLVector;
  lCurrentDistance: Single;
  lDistanceDifference, lTempCurrentDistance: Single;

  procedure DoAdjustDistanceToPoint(const DistanceValue: Single);
  var
    vect: TGLVector;
  begin
    vect := VectorSubtract(APoint, lAbsolutePosition);
    NormalizeVector(vect);
    ScaleVector(vect, DistanceValue);
    MovingObject.AbsolutePosition := VectorAdd(lAbsolutePosition, vect);
    Result := True;
  end;

begin
  lAbsolutePosition := MovingObject.AbsolutePosition;
  lCurrentDistance := VectorDistance(lAbsolutePosition, APoint);
  lDistanceDifference := lCurrentDistance - FAdjustDistanceParamsEx.FTargetDistance;

  with FAdjustDistanceParamsEx do
  begin
    lTempCurrentDistance := 0;
    while ADeltaTime > FMaxExpectedDeltaTime do
    begin
      lTempCurrentDistance := (FSpeed * FMaxExpectedDeltaTime * lDistanceDifference * FInertia) / (FInertia + 1);
//      lTempCurrentDistance := (FSpeed * FMaxExpectedDeltaTime + lDistanceDifference * FInertia) / (FInertia + 1);-  this also works, but a bit different.
      ADeltaTime := ADeltaTime - FMaxExpectedDeltaTime;
    end;
    lTempCurrentDistance :=  ClampValue(lTempCurrentDistance, -FSpeedLimit * ADeltaTime, FSpeedLimit * ADeltaTime);
    if Abs(lTempCurrentDistance) > FCutoff then
      DoAdjustDistanceToPoint(lTempCurrentDistance)
    else
      Result := False;
  end;
end;

function TGLSmoothNavigator.AdjustDistanceToTargetEx(
  const ADeltaTime: Double): Boolean;
begin
  Assert(FMoveAroundParams.FTargetObject <> nil);
  Result := AdjustDistanceToPointEx(FMoveAroundParams.FTargetObject.AbsolutePosition,
                          ADeltaTime);
end;

procedure TGLSmoothNavigator.SetAdjustDistanceParamsEx(
  const Value: TGLNavigatorAdjustDistanceParametersEx);
begin
  FAdjustDistanceParamsEx.Assign(Value);
end;

procedure TGLSmoothNavigator.AnimateCustomItems(const ADeltaTime: Double);
begin
  FCustomAnimatedItems.DoProceed(ADeltaTime);
end;

procedure TGLSmoothNavigator.SetCustomAnimatedItems(
  const Value: TGLNavigatorSmoothChangeItems);
begin
  FCustomAnimatedItems.Assign(Value);
end;

(*******************************************
 TGLSmoothUserInterface
*******************************************)

function TGLSmoothUserInterface.MouseLook(
  const ADeltaTime: Double): Boolean;
var
  MousePos: TPoint;
begin
  Assert(FAutoUpdateMouse, 'AutoUpdateMouse must be True to use this function');
  if FMouseLookActive then
  begin
    GLGetCursorPos(MousePos);
    Result := Mouselook(MousePos.X, MousePos.Y, ADeltaTime);
    GLSetCursorPos(Round(OriginalMousePos.X), Round(OriginalMousePos.Y));
  end
  else
    Result := False;
end;

function TGLSmoothUserInterface.Mouselook(const NewX, NewY: Integer; const ADeltaTime: Double): Boolean;
var
  DeltaX, DeltaY: Single;
begin
  Result := False;
  if FMouseLookActive then
  begin
    Deltax := (NewX - FOriginalMousePos.X);
    Deltay := (FOriginalMousePos.Y - NewY);
    if InvertMouse then
      DeltaY := -DeltaY;
    SmoothNavigator.TurnHorizontal(DeltaX, ADeltaTime);
    SmoothNavigator.TurnVertical(DeltaY, ADeltaTime);

    Result := (DeltaX <> 0) or (DeltaY <> 0);
  end;
end;


function TGLSmoothUserInterface.MouseLook(const NewXY: TPoint; const ADeltaTime: Double): Boolean;
begin
  Result := Mouselook(NewXY.X, NewXY.Y, ADeltaTime);
end;

constructor TGLSmoothUserInterface.Create(AOwner: TComponent);
begin
  inherited;
  FMouseLookActive := False;
  FAutoUpdateMouse := True;
  FOriginalMousePos := TGLCoordinates2.CreateInitialized(Self,
                             VectorMake(GLGetScreenWidth div 2,
                             GLGetScreenHeight div 2, 0, 0), csPoint2D);
end;

procedure TGLSmoothUserInterface.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if AComponent = FSmoothNavigator then
      FSmoothNavigator := nil;
    if AComponent = FSmoothVertNavigator then
      FSmoothNavigator := nil;
  end;
end;

procedure TGLSmoothUserInterface.SetSmoothNavigator(
  const Value: TGLSmoothNavigator);
begin
  if FSmoothNavigator <> nil then
    FSmoothNavigator.RemoveFreeNotification(Self);

  FSmoothNavigator := Value;

  if FSmoothNavigator <> nil then
    FSmoothNavigator.FreeNotification(Self);
end;

destructor TGLSmoothUserInterface.Destroy;
begin
  FOriginalMousePos.Destroy;
  inherited;
end;

procedure TGLSmoothUserInterface.SetOriginalMousePos(
  const Value: TGLCoordinates2);
begin
  FOriginalMousePos.Assign(Value);
end;

procedure TGLSmoothUserInterface.SetSmoothVertNavigator(
  const Value: TGLSmoothNavigator);
begin
  if FSmoothVertNavigator <> nil then
    FSmoothVertNavigator.RemoveFreeNotification(Self);

  FSmoothVertNavigator := Value;

  if FSmoothVertNavigator <> nil then
    FSmoothVertNavigator.FreeNotification(Self);
end;

procedure TGLSmoothUserInterface.MouseLookActiveToggle;
begin
  if FMouseLookActive then
    SetMouseLookActive(False)
  else
    SetMouseLookActive(True)
end;

procedure TGLSmoothUserInterface.SetMouseLookActive(const Value: Boolean);
var
  MousePos: TPoint;
begin
  if FMouseLookActive = Value then Exit;
  FMouseLookActive := Value;
  if FMouseLookActive then
  begin
    if FAutoUpdateMouse then
    begin
      GLGetCursorPos(MousePos);
      FOriginalMousePos.SetPoint2D(MousePos.X, MousePos.Y);
      GLShowCursor(False);
    end;
  end
  else
  begin
    if FAutoUpdateMouse then
      GLShowCursor(True);
  end;
end;

procedure TGLSmoothUserInterface.TurnHorizontal(const Angle: Single;
  const ADeltaTime: Double);
begin
  FSmoothNavigator.TurnHorizontal(Angle, ADeltaTime);
end;

procedure TGLSmoothUserInterface.TurnVertical(const Angle: Single;
  const ADeltaTime: Double);
begin
  if Assigned(FSmoothNavigator) then
    FSmoothNavigator.TurnVertical(Angle, ADeltaTime)
  else
    FSmoothVertNavigator.TurnVertical(Angle, ADeltaTime);
end;

(*******************************************
 TGLNavigatorInertiaParameters
*******************************************)

procedure TGLNavigatorInertiaParameters.Assign(Source: TPersistent);
begin
  if Source is TGLNavigatorInertiaParameters then
  begin
    FMovementAcceleration := TGLNavigatorInertiaParameters(Source).FMovementAcceleration;
    FMovementInertia := TGLNavigatorInertiaParameters(Source).FMovementInertia;
    FMovementSpeed := TGLNavigatorInertiaParameters(Source).FMovementSpeed;
    FTurnMaxAngle := TGLNavigatorInertiaParameters(Source).FTurnMaxAngle;
    FTurnInertia := TGLNavigatorInertiaParameters(Source).FTurnInertia;
    FTurnSpeed := TGLNavigatorInertiaParameters(Source).FTurnSpeed;
  end
  else
    inherited; //to the pit of doom ;)
end;

constructor TGLNavigatorInertiaParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;

  FTurnInertia := 150;
  FTurnSpeed := 50;
  FTurnMaxAngle := 0.5;

  FMovementAcceleration := 7;
  FMovementInertia := 200;
  FMovementSpeed := 200;
end;

function TGLNavigatorInertiaParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGLNavigatorInertiaParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value > 1 then
  begin
    FMovementInertia := FMovementInertia * PowerSingle(2, 1 / Value);
    FTurnInertia := FTurnInertia * PowerSingle(2, 1 / Value);
  end
  else
  begin
    FMovementInertia := FMovementInertia / PowerSingle(2, Value);
    FTurnInertia := FTurnInertia / PowerSingle(2, Value);
  end;
  FTurnMaxAngle := FTurnMaxAngle / Value;
  FTurnSpeed := FTurnSpeed * Value;
end;

function TGLNavigatorInertiaParameters.StoreTurnMaxAngle: Boolean;
begin
  Result := Abs(FTurnMaxAngle - 0.5) > EPS;
end;

function TGLNavigatorInertiaParameters.StoreMovementAcceleration: Boolean;
begin
  Result := Abs(FMovementAcceleration - 7) > EPS;
end;

function TGLNavigatorInertiaParameters.StoreMovementInertia: Boolean;
begin
  Result := Abs(FMovementInertia - 200) > EPS;
end;

function TGLNavigatorInertiaParameters.StoreMovementSpeed: Boolean;
begin
  Result := Abs(FMovementSpeed - 200) > EPS;
end;

function TGLNavigatorInertiaParameters.StoreTurnInertia: Boolean;
begin
  Result := Abs(FTurnInertia - 150) > EPS;
end;

function TGLNavigatorInertiaParameters.StoreTurnSpeed: Boolean;
begin
  Result := Abs(FTurnSpeed - 50) > EPS;
end;

(*******************************************
 TGLNavigatorGeneralParameters
*******************************************)

procedure TGLNavigatorGeneralParameters.Assign(Source: TPersistent);
begin
  if Source is TGLNavigatorGeneralParameters then
  begin
    FAutoScaleMin := TGLNavigatorGeneralParameters(Source).FAutoScaleMin;
    FAutoScaleMax := TGLNavigatorGeneralParameters(Source).FAutoScaleMax;
    FAutoScaleMult := TGLNavigatorGeneralParameters(Source).FAutoScaleMult;
  end
  else
    inherited; //die!
end;

constructor TGLNavigatorGeneralParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FAutoScaleMin := 0.1;
  FAutoScaleMax := 0.75;
  FAutoScaleMult := 2;
end;

function TGLNavigatorGeneralParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TGLNavigatorGeneralParameters.StoreAutoScaleMax: Boolean;
begin
  Result := Abs(FAutoScaleMax - 0.75) > EPS;
end;

function TGLNavigatorGeneralParameters.StoreAutoScaleMin: Boolean;
begin
  Result := Abs(FAutoScaleMin - 0.1) > EPS;
end;

function TGLNavigatorGeneralParameters.StoreAutoScaleMult: Boolean;
begin
  Result := Abs(FAutoScaleMult - 2) > EPS;
end;

(*******************************************
 TGLNavigatorMoveAroundParameters
*******************************************)

procedure TGLNavigatorMoveAroundParameters.Assign(Source: TPersistent);
begin
  if Source is TGLNavigatorMoveAroundParameters then
  begin
    FMaxAngle := TGLNavigatorMoveAroundParameters(Source).FMaxAngle;
    FInertia :=  TGLNavigatorMoveAroundParameters(Source).FInertia;
    FPitchSpeed :=  TGLNavigatorMoveAroundParameters(Source).FPitchSpeed;
    FTurnSpeed :=  TGLNavigatorMoveAroundParameters(Source).FTurnSpeed;
    FCutoff :=  TGLNavigatorMoveAroundParameters(Source).FCutoff;
    SetTargetObject(TGLNavigatorMoveAroundParameters(Source).FTargetObject);
  end
  else
    inherited; //die
end;

constructor TGLNavigatorMoveAroundParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FPitchSpeed := 500;
  FTurnSpeed  := 500;
  FInertia    := 65;
  FMaxAngle   := 1.5;
  FCutoff     := EPS2;
end;

function TGLNavigatorMoveAroundParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGLNavigatorMoveAroundParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / PowerSingle(2, Value)
  else
    FInertia := FInertia * PowerSingle(2, 1 / Value);

  FMaxAngle := FMaxAngle / Value;
  FPitchSpeed := FPitchSpeed * Value;
  FTurnSpeed := FTurnSpeed * Value;
end;

procedure TGLNavigatorMoveAroundParameters.SetTargetObject(
  const Value: TGLBaseSceneObject);
begin
  if FTargetObject <> nil then
    if FOwner is TGLSmoothNavigator then
      FTargetObject.RemoveFreeNotification(TGLSmoothNavigator(FOwner));

  FTargetObject := Value;

  if FTargetObject <> nil then
    if FOwner is TGLSmoothNavigator then
      FTargetObject.FreeNotification(TGLSmoothNavigator(FOwner));
end;

function TGLNavigatorMoveAroundParameters.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS2) > EPS8;
end;

function TGLNavigatorMoveAroundParameters.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 65) > EPS;
end;

function TGLNavigatorMoveAroundParameters.StoreMaxAngle: Boolean;
begin
  Result := Abs(FMaxAngle - 1.5) > EPS;
end;

function TGLNavigatorMoveAroundParameters.StorePitchSpeed: Boolean;
begin
  Result := Abs(FPitchSpeed - 500) > EPS;
end;

function TGLNavigatorMoveAroundParameters.StoreTurnSpeed: Boolean;
begin
  Result := Abs(FTurnSpeed - 500) > EPS;
end;

(*******************************************
 TGLNavigatorAdjustDistanceParameters
*******************************************)

procedure TGLNavigatorAdjustDistanceParameters.AddImpulse(
  const Impulse: Single);
begin
  FOldDistanceRatio := FOldDistanceRatio + Impulse * FSpeed / FInertia * FImpulseSpeed;
end;

procedure TGLNavigatorAdjustDistanceParameters.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TGLNavigatorAdjustDistanceParameters then
  begin
    FImpulseSpeed := TGLNavigatorAdjustDistanceParameters(Source).FImpulseSpeed;
  end;
end;

constructor TGLNavigatorAdjustDistanceParameters.Create(
  AOwner: TPersistent);
begin
  inherited;
  FImpulseSpeed := 0.02;
end;


procedure TGLNavigatorAdjustDistanceParameters.ScaleParameters(
  const Value: Single);
begin
  inherited;
  FImpulseSpeed := FImpulseSpeed / Value;
end;

function TGLNavigatorAdjustDistanceParameters.StoreImpulseSpeed: Boolean;
begin
  Result := Abs(FImpulseSpeed - 0.02) > EPS;
end;


(*******************************************
 TGLNavigatorAbstractParameters
*******************************************)

procedure TGLNavigatorAbstractParameters.Assign(Source: TPersistent);
begin
  if Source is TGLNavigatorAbstractParameters then
  begin
    FInertia := TGLNavigatorAbstractParameters(Source).FInertia;
    FSpeed :=   TGLNavigatorAbstractParameters(Source).FSpeed;
    FCutoff :=  TGLNavigatorAbstractParameters(Source).FCutoff;
  end
  else
    inherited; //to the pit of doom ;)
end;

constructor TGLNavigatorAbstractParameters.Create(
  AOwner: TPersistent);
begin
  FOwner := AOwner;
  FInertia := 100;
  FSpeed := 0.005;
  FCutoff := EPS;
end;

function TGLNavigatorAbstractParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TGLNavigatorAbstractParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / PowerSingle(2, Value)
  else
    FInertia := FInertia * PowerSingle(2, 1 / Value);
end;

function TGLNavigatorAbstractParameters.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS) > EPS2;
end;

function TGLNavigatorAbstractParameters.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 100) > EPS;
end;

function TGLNavigatorAbstractParameters.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 0.005) > EPS2;
end;

(*******************************************
 TGLNavigatorAdjustDistanceParametersEx
*******************************************)

procedure TGLNavigatorAdjustDistanceParametersEx.Assign(
  Source: TPersistent);
begin
  if Source is TGLNavigatorAdjustDistanceParametersEx then
  begin
    FTargetDistance := TGLNavigatorAdjustDistanceParametersEx(Source).FTargetDistance;
    FSpeedLimit := TGLNavigatorAdjustDistanceParametersEx(Source).FSpeedLimit;
  end
  else
    inherited;
end;

constructor TGLNavigatorAdjustDistanceParametersEx.Create(
  AOwner: TPersistent);
begin
  inherited;
  FInertia := 0.5;
  FTargetDistance := 100;
  FSpeed := 100;
  FSpeedLimit := 20000;
end;

function TGLNavigatorAdjustDistanceParametersEx.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 0.5) > EPS2;
end;

function TGLNavigatorAdjustDistanceParametersEx.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 100) > EPS2;
end;

function TGLNavigatorAdjustDistanceParametersEx.StoreSpeedLimit: Boolean;
begin
  Result := Abs(FSpeedLimit - 20000) > EPS2;
end;

function TGLNavigatorAdjustDistanceParametersEx.StoreTargetDistance: Boolean;
begin
  Result := Abs(FTargetDistance - 100) > EPS2;
end;

(*******************************************
 TGLNavigatorSmoothChangeItem
*******************************************)

procedure TGLNavigatorSmoothChangeItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TGLNavigatorSmoothChangeItem then
  begin
    FInertia :=    TGLNavigatorSmoothChangeItem(Source).FInertia;
    FSpeed :=      TGLNavigatorSmoothChangeItem(Source).FSpeed;
    FSpeedLimit := TGLNavigatorSmoothChangeItem(Source).FSpeedLimit;
    FCutoff :=     TGLNavigatorSmoothChangeItem(Source).FCutoff;
    FEnabled :=    TGLNavigatorSmoothChangeItem(Source).FEnabled;
  end;
end;

constructor TGLNavigatorSmoothChangeItem.Create(aOwner: TXCollection);
begin
  inherited;
  FInertia := 1;
  FSpeed := 5.5;
  FSpeedLimit := 20000;
  FCutoff := EPS;
  FEnabled := True;
end;

function TGLNavigatorSmoothChangeItem.GetNavigator: TGLSmoothNavigator;
begin
  Result := TGLSmoothNavigator(TGLNavigatorSmoothChangeItems(GetOwner).Owner);
end;

procedure TGLNavigatorSmoothChangeItem.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / PowerSingle(2, Value)
  else
    FInertia := FInertia * PowerSingle(2, 1 / Value);
end;

function TGLNavigatorSmoothChangeItem.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS) > EPS8;
end;

function TGLNavigatorSmoothChangeItem.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 1) > EPS;
end;

function TGLNavigatorSmoothChangeItem.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 5.5) > EPS2;
end;

function TGLNavigatorSmoothChangeItem.StoreSpeedLimit: Boolean;
begin
  Result := Abs(FSpeedLimit - 20000) > EPS2;
end;

(*******************************************
 TGLNavigatorSmoothChangeItems
*******************************************)

function TGLNavigatorSmoothChangeItems.Add(AClass : TGLNavigatorSmoothChangeItemClass): TGLNavigatorSmoothChangeItem;
begin
  Result := AClass.Create(Self);
end;

function TGLNavigatorSmoothChangeItems.CanAdd(AClass: TXCollectionItemClass): Boolean;
begin
  Result := AClass.InheritsFrom(TGLNavigatorSmoothChangeItem);
end;

procedure TGLNavigatorSmoothChangeItems.DoProceed(ADeltaTime: Double);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItems(I).Proceed(ADeltaTime);
end;

function TGLNavigatorSmoothChangeItems.GetItems(const Index : Integer): TGLNavigatorSmoothChangeItem;
begin
  Result := TGLNavigatorSmoothChangeItem(inherited GetItems(Index));
end;

class function TGLNavigatorSmoothChangeItems.ItemsClass: TXCollectionItemClass;
begin
  Result := TGLNavigatorSmoothChangeItem;
end;

procedure TGLNavigatorSmoothChangeItems.SetItems(const Index : Integer; const Value:
        TGLNavigatorSmoothChangeItem);
begin
  GetItems(Index).Assign(Value);
end;

(*******************************************
 TGLNavigatorSmoothChangeSingle
*******************************************)

procedure TGLNavigatorSmoothChangeSingle.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TGLNavigatorSmoothChangeVector then
  begin
    FTargetValue := TGLNavigatorSmoothChangeSingle(Source).TargetValue;
    FOnGetCurrentValue := TGLNavigatorSmoothChangeSingle(Source).FOnGetCurrentValue;
    FOnSetCurrentValue := TGLNavigatorSmoothChangeSingle(Source).FOnSetCurrentValue;
  end;
end;

class function TGLNavigatorSmoothChangeSingle.FriendlyName: string;
begin
  Result := 'Navigator SmoothChange Single';
end;

function TGLNavigatorSmoothChangeSingle.Proceed(ADeltaTime: Double): Boolean;
var
  lCurrentValue: Single;
  lCurrentDifference: Single;
  lTotalDistanceToTravelThisTime, lDistanceToTravelThisTime: Single;
  lMaxExpectedDeltaTime: Double;

begin
  Result := False;
  if not FEnabled then Exit;
  if not Assigned(FOnGetCurrentValue) then Exit;
  if not Assigned(FOnSetCurrentValue) then Exit;

  lMaxExpectedDeltaTime := GetNavigator.FMaxExpectedDeltaTime;
  lCurrentValue := FOnGetCurrentValue(Self);
  lCurrentDifference := FTargetValue - lCurrentValue;

  lTotalDistanceToTravelThisTime := 0;

  while ADeltaTime > lMaxExpectedDeltaTime do
  begin
    lDistanceToTravelThisTime := MinFloat((lCurrentDifference * ADeltaTime * FSpeed * FInertia) / (FInertia + 1), FSpeedLimit);
//  lDistanceToTravelThisTime := (lCurrentDistance * ADeltaTime + FSpeed * FInertia) / (FInertia + 1);-  this also works, but a bit different.

    lCurrentDifference := lCurrentDifference - lDistanceToTravelThisTime;
    lTotalDistanceToTravelThisTime := lTotalDistanceToTravelThisTime + lDistanceToTravelThisTime;
    ADeltaTime := ADeltaTime - lMaxExpectedDeltaTime;
  end;

  if Abs(lTotalDistanceToTravelThisTime) > FCutoff then
  begin
    FOnSetCurrentValue(Self, lCurrentValue + lTotalDistanceToTravelThisTime);
    Result := True;
  end;  
end;

procedure TGLNavigatorSmoothChangeSingle.ResetTargetValue;
begin
  FTargetValue := FOnGetCurrentValue(Self);
end;

(*******************************************
 TGLNavigatorSmoothChangeVector
*******************************************)

procedure TGLNavigatorSmoothChangeVector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  
  if Source is TGLNavigatorSmoothChangeVector then
  begin
    FTargetValue.Assign(TGLNavigatorSmoothChangeVector(Source).TargetValue);
    FOnGetCurrentValue := TGLNavigatorSmoothChangeVector(Source).FOnGetCurrentValue;
    FOnSetCurrentValue := TGLNavigatorSmoothChangeVector(Source).FOnSetCurrentValue;
  end;
end;

constructor TGLNavigatorSmoothChangeVector.Create(aOwner: TXCollection);
begin
  inherited;
  FTargetValue := TGLCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
end;

destructor TGLNavigatorSmoothChangeVector.Destroy;
begin
  FTargetValue.Free;
  inherited;
end;

class function TGLNavigatorSmoothChangeVector.FriendlyName: string;
begin
  Result := 'Navigator SmoothChange Vector';
end;

function TGLNavigatorSmoothChangeVector.Proceed(ADeltaTime: Double): Boolean;
var
  lAbsolutePosition: TGLVector;
  lCurrentDistance: Single;
  lTotalDistanceToTravelThisTime, lDistanceToTravelThisTime: Single;
  lMaxExpectedDeltaTime: Double;

  procedure DoAdjustDistanceToPoint();
  var
    vect: TGLVector;
  begin
    vect := VectorScale(VectorNormalize(VectorSubtract(FTargetValue.DirectVector, lAbsolutePosition)), lTotalDistanceToTravelThisTime);
    AddVector(vect, lAbsolutePosition);

    // Did we go too far?
    if VectorDistance(vect, FTargetValue.DirectVector) > VectorDistance(lAbsolutePosition, FTargetValue.DirectVector) then
      vect := FTargetValue.DirectVector;

    FOnSetCurrentValue(Self, vect);
    Result := True;
  end;

begin
  Result := False;
  if not FEnabled then Exit;
  if not Assigned(FOnGetCurrentValue) then Exit;
  if not Assigned(FOnSetCurrentValue) then Exit;

  lMaxExpectedDeltaTime := GetNavigator.FMaxExpectedDeltaTime;
  lAbsolutePosition := FOnGetCurrentValue(Self);
  lCurrentDistance := VectorDistance(lAbsolutePosition, FTargetValue.DirectVector);

  lTotalDistanceToTravelThisTime := 0;


  while ADeltaTime > lMaxExpectedDeltaTime do
  begin
    lDistanceToTravelThisTime := MinFloat((lCurrentDistance * ADeltaTime * FSpeed * FInertia) / (FInertia + 1), FSpeedLimit);
//  lDistanceToTravelThisTime := (lCurrentDistance * ADeltaTime + FSpeed * FInertia) / (FInertia + 1);-  this also works, but a bit different.

    lCurrentDistance := lCurrentDistance - lDistanceToTravelThisTime;
    lTotalDistanceToTravelThisTime := lTotalDistanceToTravelThisTime + lDistanceToTravelThisTime;
    ADeltaTime := ADeltaTime - lMaxExpectedDeltaTime;
  end;

  if Abs(lTotalDistanceToTravelThisTime) > FCutoff then
    DoAdjustDistanceToPoint();
end;

procedure TGLNavigatorSmoothChangeVector.ResetTargetValue;
begin
  FTargetValue.DirectVector := FOnGetCurrentValue(Self);
end;

procedure TGLNavigatorSmoothChangeVector.SetTargetValue(
  const Value: TGLCoordinates);
begin
  FTargetValue.Assign(Value);
end;

//==========================================================
initialization
//==========================================================

  RegisterClasses([
      TGLSmoothNavigator, TGLSmoothUserInterface,
      TGLNavigatorInertiaParameters, TGLNavigatorGeneralParameters,
      TGLNavigatorMoveAroundParameters,
      TGLNavigatorAdjustDistanceParameters, TGLNavigatorAdjustDistanceParametersEx
                   ]);

  RegisterXCollectionItemClass(TGLNavigatorSmoothChangeSingle);
  RegisterXCollectionItemClass(TGLNavigatorSmoothChangeVector);
end.


