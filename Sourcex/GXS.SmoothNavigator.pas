//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.SmoothNavigator;

(*
   An extention of TgxNavigator, which allows to move objects with inertia
   Note: it is not completely FPS-independant. Only Moving code is, but
   MoveAroundTarget, Turn[Vertical/Horizontal] and AdjustDistanceTo[..] is not.

   Don't know why, but when I make their code identical, these function stop
   working completely. So you probably have to call the AutoScaleParameters
   procedure once in a while for it to adjust to the current framerate.

    TODO:
      1) Scale "Old values" too, when callin the Scale parameter procedure to
         avoid the temporary "freeze" of controls.
      2) AddImpulse procedures.
*)

interface

{$I GXS.Scene.inc}

uses
  System.Types,
  System.Classes,

  GXS.XCollection,
  GXS.VectorTypes,
  GXS.Navigator,
  GXS.VectorGeometry,
  GXS.Scene,
  GXS.Coordinates,
  GXS.Screen,
  GXS.PersistentClasses;

type

  { Includes a basic set of parameters that control the smoothness of movement. }
  TgxNavigatorAbstractParameters = class(TPersistent)
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

  TgxSmoothNavigator = class;

  { Includes a basic set of parameters that control the smoothness of movement }
  TgxNavigatorSmoothChangeItem = class(TXCollectionItem)
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
    function GetNavigator: TgxSmoothNavigator;
  public
    { Returns False if there was no change. }
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

  TgxNavigatorSmoothChangeSingle = class;
  TgxNavigatorSmoothChangeSingleGetEvent = function(const ASender: TgxNavigatorSmoothChangeSingle): Single of object;
  TgxNavigatorSmoothChangeSingleSetEvent = procedure(const ASender: TgxNavigatorSmoothChangeSingle; const AValue: Single) of object;

  { Smoothly change any Single value, so it will become TargetValue in the end.  }
  TgxNavigatorSmoothChangeSingle = class(TgxNavigatorSmoothChangeItem)
  private
    FTargetValue: Single;
    FOnGetCurrentValue: TgxNavigatorSmoothChangeSingleGetEvent;
    FOnSetCurrentValue: TgxNavigatorSmoothChangeSingleSetEvent;
  public
    class function FriendlyName: string; override;
    function Proceed(ADeltaTime: Double): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure ResetTargetValue(); override;
  published
    property TargetValue: Single read FTargetValue write FTargetValue;
    property OnGetCurrentValue: TgxNavigatorSmoothChangeSingleGetEvent read FOnGetCurrentValue write FOnGetCurrentValue;
    property OnSetCurrentValue: TgxNavigatorSmoothChangeSingleSetEvent read FOnSetCurrentValue write FOnSetCurrentValue;
  end;

  TgxNavigatorSmoothChangeVector = class;
  TgxNavigatorSmoothChangeVectorGetEvent = function(const ASender: TgxNavigatorSmoothChangeVector): TVector4f of object;
  TgxNavigatorSmoothChangeVectorSetEvent = procedure(const ASender: TgxNavigatorSmoothChangeVector; const AValue: TVector4f) of object;

  { Smoothly change any Vector4f value, so it will become TargetValue in the end.  }
  TgxNavigatorSmoothChangeVector = class(TgxNavigatorSmoothChangeItem)
  private
    FTargetValue: TgxCoordinates;
    FOnGetCurrentValue: TgxNavigatorSmoothChangeVectorGetEvent;
    FOnSetCurrentValue: TgxNavigatorSmoothChangeVectorSetEvent;
    procedure SetTargetValue(const Value: TgxCoordinates);
  public
    class function FriendlyName: string; override;
    function Proceed(ADeltaTime: Double): Boolean; override;
    procedure Assign(Source: TPersistent); override;
    constructor Create(aOwner: TXCollection); override;
    destructor Destroy; override;
    procedure ResetTargetValue(); override;
  published
    property TargetValue: TgxCoordinates read FTargetValue write SetTargetValue;
    property OnGetCurrentValue: TgxNavigatorSmoothChangeVectorGetEvent read FOnGetCurrentValue write FOnGetCurrentValue;
    property OnSetCurrentValue: TgxNavigatorSmoothChangeVectorSetEvent read FOnSetCurrentValue write FOnSetCurrentValue;
  end;

  TgxNavigatorSmoothChangeItemClass = class of TgxNavigatorSmoothChangeItem;

  { XCollection of TgxNavigatorSmoothChangeItem. }
  TgxNavigatorSmoothChangeItems = class(TXCollection)
  private
    function GetItems(const Index : Integer): TgxNavigatorSmoothChangeItem;
    procedure SetItems(const Index : Integer; const Value: TgxNavigatorSmoothChangeItem);
  protected
    procedure DoProceed(ADeltaTime: Double);
  public
    function Add(AClass : TgxNavigatorSmoothChangeItemClass): TgxNavigatorSmoothChangeItem;
    function CanAdd(AClass: TXCollectionItemClass): Boolean; override;
    class function ItemsClass: TXCollectionItemClass; override;
    property Items[const Index : Integer]: TgxNavigatorSmoothChangeItem read GetItems write
            SetItems; default;
  end;

  { This is wrapper for all parameters that affect how the AdjustDisanceTo[...] methods work }
  TgxNavigatorAdjustDistanceParameters = class(TgxNavigatorAbstractParameters)
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

  { This is a wrapper for all parameters that affect how the AdjustDisanceTo[...]Ex methods work
     You need to set the TargetObject and desired distance to it,
     then call AdjustDisanceTo[...]Ex() in your Cadencer.OnProgress code. }
  TgxNavigatorAdjustDistanceParametersEx = class(TgxNavigatorAbstractParameters)
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

  { This is a wrapper for all parameters that affect the smoothness of movement }
  TgxNavigatorInertiaParameters = class(TPersistent)
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


  { This is a wrapper for all general inertia parameters.
     These properties mean that if ExpectedMaxFPS is 100, FAutoScaleMin is 0.1,
     FAutoScaleMax is 0.75 then the "safe range" for it to change is [10..75].
     If these bounds are violated, then ExpectedMaxFPS is automaticly increased
     or decreased by AutoScaleMult. }
  TgxNavigatorGeneralParameters = class(TPersistent)
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


  { This is a wrapper for all parameters that effect how the TgxBaseSceneObject.MoveObjectAround() procedure works}
  TgxNavigatorMoveAroundParameters = class(TPersistent)
  private
    FOwner: TPersistent;
    FTargetObject: TgxBaseSceneObject;
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
    procedure SetTargetObject(const Value: TgxBaseSceneObject);
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
    property TargetObject: TgxBaseSceneObject read FTargetObject write SetTargetObject;
    property Cutoff: Double read FCutoff write FCutoff stored StoreCutoff;    
  end;


  { This is the component for moving a TgxBaseSceneObject, and all
       classes based on it, this includes all the objects from the Scene Editor.
     It uses complex smoothing algorithms, most of which are FPS-dependant.
     Make sure your limit your FPS and set MaxExpectedDeltaTime to a value
     that is aproximatly 5 times less than your usual deltatime. }
  TgxSmoothNavigator = class(TgxNavigator)
  private
    FMaxExpectedDeltaTime: Double;
    FInertiaParams: TgxNavigatorInertiaParameters;
    FGeneralParams: TgxNavigatorGeneralParameters;
    FMoveAroundParams: TgxNavigatorMoveAroundParameters;
    FAdjustDistanceParams: TgxNavigatorAdjustDistanceParameters;
    FAdjustDistanceParamsEx: TgxNavigatorAdjustDistanceParametersEx;
    FCustomAnimatedItems: TgxNavigatorSmoothChangeItems;
    procedure SetInertiaParams(const Value: TgxNavigatorInertiaParameters);
    function StoreMaxExpectedDeltaTime: Boolean;
    procedure SetGeneralParams(const Value: TgxNavigatorGeneralParameters);
    procedure SetMoveAroundParams(const Value: TgxNavigatorMoveAroundParameters);
    procedure SetAdjustDistanceParams(const Value: TgxNavigatorAdjustDistanceParameters);
    procedure SetAdjustDistanceParamsEx(
      const Value: TgxNavigatorAdjustDistanceParametersEx);
    procedure SetCustomAnimatedItems(
      const Value: TgxNavigatorSmoothChangeItems);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    // Constructors-destructors.
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // From TgxNavigator. Probably, should not be public.
    procedure SetObject(Value: TgxBaseSceneObject); override;
    // Uses InertiaParams.
    procedure TurnHorizontal(Angle: Single; ADeltaTime: Double); virtual;
    procedure TurnVertical(Angle: Single; ADeltaTime: Double); virtual;
    procedure FlyForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure MoveForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure StrafeHorizontal(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    procedure StrafeVertical(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False); virtual;
    // Uses MoveAroundParams. Returns True, if object was actually moved.
    function MoveAroundTarget(const PitchDelta, TurnDelta : Single; const ADeltaTime: Double): Boolean; virtual;
    function MoveObjectAround(const AObject: TgxBaseSceneObject; PitchDelta, TurnDelta : Single; ADeltaTime: Double): Boolean; virtual;
    // Uses AdjustDistanceParams.
    function AdjustDistanceToPoint(const  APoint: TVector4f; const DistanceRatio : Single; ADeltaTime: Double): Boolean; virtual;
    function AdjustDistanceToTarget(const DistanceRatio : Single; const ADeltaTime: Double): Boolean; virtual;
    // Uses AdjustDistanceParamsEx.
    function AdjustDistanceToPointEx(const  APoint: TVector4f; ADeltaTime: Double): Boolean; virtual;
    function AdjustDistanceToTargetEx(const ADeltaTime: Double): Boolean; virtual;
    // Uses CustomAnimatedItems.
    procedure AnimateCustomItems(const ADeltaTime: Double); virtual;
    // Uses GeneralParams.
      { In ScaleParameters, Value should be around 1. }
    procedure ScaleParameters(const Value: Single); virtual;
    procedure AutoScaleParameters(const FPS: Single); virtual;
    procedure AutoScaleParametersUp(const FPS: Single); virtual;
  published
    property MaxExpectedDeltaTime: Double read FMaxExpectedDeltaTime write FMaxExpectedDeltaTime stored StoreMaxExpectedDeltaTime;
    property InertiaParams: TgxNavigatorInertiaParameters read FInertiaParams write SetInertiaParams;
    property GeneralParams: TgxNavigatorGeneralParameters read FGeneralParams write SetGeneralParams;
    property MoveAroundParams: TgxNavigatorMoveAroundParameters read FMoveAroundParams write SetMoveAroundParams;
    property AdjustDistanceParams: TgxNavigatorAdjustDistanceParameters read FAdjustDistanceParams write SetAdjustDistanceParams;
    property AdjustDistanceParamsEx: TgxNavigatorAdjustDistanceParametersEx read FAdjustDistanceParamsEx write SetAdjustDistanceParamsEx;
    property CustomAnimatedItems: TgxNavigatorSmoothChangeItems read FCustomAnimatedItems write SetCustomAnimatedItems;
  end;


  { This is the component which reads the userinput and transform it into action.
	    Mouselook(ADeltaTime: double) : handles mouse look... Should be called
                           in the Cadencer event. (Though it works everywhere!)
	   The four properties to get you started are:
	    InvertMouse     : Inverts the mouse Y axis.
	    AutoUpdateMouse : If enabled (by defaul), than handles all mouse updates.
	    GLNavigator     : The Navigator which receives the user movement.
	    GLVertNavigator : The Navigator which if set receives the vertical user
                           movement. Used mostly for cameras.... }
  TgxSmoothUserInterface = class(TComponent)
  private
    FAutoUpdateMouse: Boolean;
    FMouseLookActive: Boolean;
    FSmoothNavigator: TgxSmoothNavigator;
    FSmoothVertNavigator: TgxSmoothNavigator;
    FInvertMouse: Boolean;
    FOriginalMousePos: TgxCoordinates2;
    procedure SetSmoothNavigator(const Value: TgxSmoothNavigator); virtual;
    procedure SetOriginalMousePos(const Value: TgxCoordinates2); virtual;
    procedure SetSmoothVertNavigator(const Value: TgxSmoothNavigator); virtual;
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
    property SmoothVertNavigator: TgxSmoothNavigator read FSmoothVertNavigator write SetSmoothVertNavigator;
    property SmoothNavigator: TgxSmoothNavigator read FSmoothNavigator write SetSmoothNavigator;
    property InvertMouse: Boolean read FInvertMouse write FInvertMouse default False;
    property OriginalMousePos: TgxCoordinates2 read FOriginalMousePos write SetOriginalMousePos;
  end;

//-----------------------------------------------------------
implementation
//-----------------------------------------------------------

const
  EPS =  0.001;
  EPS2 = 0.0001;
  EPS8 = 0.00000001;

{ TgxSmoothNavigator }

constructor TgxSmoothNavigator.Create(AOwner: TComponent);
begin
  inherited;
  FMaxExpectedDeltaTime := 0.001;
  FInertiaParams := TgxNavigatorInertiaParameters.Create(Self);
  FGeneralParams := TgxNavigatorGeneralParameters.Create(Self);
  FMoveAroundParams := TgxNavigatorMoveAroundParameters.Create(Self);
  FAdjustDistanceParams := TgxNavigatorAdjustDistanceParameters.Create(Self);
  FAdjustDistanceParamsEx := TgxNavigatorAdjustDistanceParametersEx.Create(Self);
  FCustomAnimatedItems := TgxNavigatorSmoothChangeItems.Create(Self);
end;

destructor TgxSmoothNavigator.Destroy;
begin
  FInertiaParams.Free;
  FGeneralParams.Free;
  FMoveAroundParams.Free;
  FAdjustDistanceParams.Free;
  FAdjustDistanceParamsEx.Free;
  FCustomAnimatedItems.Free;
  inherited;
end;

procedure TgxSmoothNavigator.SetInertiaParams(
  const Value: TgxNavigatorInertiaParameters);
begin
  FInertiaParams.Assign(Value);
end;

procedure TgxSmoothNavigator.TurnHorizontal(Angle: Single; ADeltaTime: Double);
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

procedure TgxSmoothNavigator.TurnVertical(Angle: Single; ADeltaTime: Double);
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


procedure TgxSmoothNavigator.MoveForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
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

procedure TgxSmoothNavigator.FlyForward(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
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

procedure TgxSmoothNavigator.StrafeHorizontal(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
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

procedure TgxSmoothNavigator.StrafeVertical(const Plus, Minus: Boolean; ADeltaTime: Double; const Accelerate: Boolean = False);
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

procedure TgxSmoothNavigator.AutoScaleParameters(const FPS: Single);
begin
  with FGeneralParams do
  begin
    if FPS > FAutoScaleMax / FMaxExpectedDeltatime then
      ScaleParameters(FAutoScaleMult)
    else if FPS < FAutoScaleMin / FMaxExpectedDeltatime then
      ScaleParameters(1/FAutoScaleMult);
  end;
end;


procedure TgxSmoothNavigator.AutoScaleParametersUp(const FPS: Single);
begin
  with FGeneralParams do
  begin
    if FPS > FAutoScaleMax / FMaxExpectedDeltatime then
      ScaleParameters(FAutoScaleMult)
  end;
end;

procedure TgxSmoothNavigator.ScaleParameters(const Value: Single);
begin
  Assert(Value > 0);
  FMaxExpectedDeltatime := FMaxExpectedDeltatime / Value;
  FInertiaParams.ScaleParameters(Value);
  FMoveAroundParams.ScaleParameters(Value);
  FAdjustDistanceParams.ScaleParameters(Value);
end;

function TgxSmoothNavigator.StoreMaxExpectedDeltaTime: Boolean;
begin
  Result := Abs(FMaxExpectedDeltaTime - 0.001) > EPS2;
end;

procedure TgxSmoothNavigator.SetGeneralParams(
  const Value: TgxNavigatorGeneralParameters);
begin
  FGeneralParams.Assign(Value);
end;

procedure TgxSmoothNavigator.SetMoveAroundParams(
  const Value: TgxNavigatorMoveAroundParameters);
begin
  FMoveAroundParams.Assign(Value);
end;

procedure TgxSmoothNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent = FMoveAroundParams.FTargetObject then
      FMoveAroundParams.FTargetObject := nil;
  end;
end;

procedure TgxSmoothNavigator.SetObject(Value: TgxBaseSceneObject);
var
  I: Integer;
begin
  inherited;
  // Try to detect a TargetObject.
  if Value <> nil then
    if FMoveAroundParams.TargetObject = nil then
    begin
      // May be it is a camera...
      if Value is TgxCamera then
        FMoveAroundParams.TargetObject := TgxCamera(Value).TargetObject
      else
      begin
        // May be it has camera children...
        if Value.Count <> 0 then
          for I := 0 to Value.Count - 1 do
            if Value.Children[I] is TgxCamera then
            begin
              FMoveAroundParams.TargetObject := TgxCamera(Value.Children[I]).TargetObject;
              Exit;
            end;
      end;
    end;
end;

function TgxSmoothNavigator.MoveAroundTarget(const PitchDelta, TurnDelta: Single;
  const ADeltaTime: Double): Boolean;
begin
  Result := MoveObjectAround(FMoveAroundParams.FTargetObject, PitchDelta, TurnDelta, ADeltaTime);
end;

function TgxSmoothNavigator.MoveObjectAround(
  const AObject: TgxBaseSceneObject; PitchDelta, TurnDelta: Single;
  ADeltaTime: Double): Boolean;
var
  FinalPitch: Single;
  FinalTurn:  Single;

  lUp: TVector4f;
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
      MovingObject.AbsolutePosition := GXS.VectorGeometry.MoveObjectAround(
        MovingObject.AbsolutePosition, lUp, AObject.AbsolutePosition, FinalPitch, FinalTurn);
      Result := True;
    end;
  end;
end;


function TgxSmoothNavigator.AdjustDistanceToPoint(const APoint: TVector4f;
  const DistanceRatio: Single; ADeltaTime: Double): Boolean;

  // Based on TgxCamera.AdjustDistanceToTarget
  procedure DoAdjustDistanceToPoint(const DistanceRatio: Single);
  var
    vect: TVector4f;
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

function TgxSmoothNavigator.AdjustDistanceToTarget(const DistanceRatio: Single;
  const ADeltaTime: Double): Boolean;
begin
  Assert(FMoveAroundParams.FTargetObject <> nil);
  Result := AdjustDistanceToPoint(FMoveAroundParams.FTargetObject.AbsolutePosition,
                        DistanceRatio, ADeltaTime);
end;

procedure TgxSmoothNavigator.SetAdjustDistanceParams(
  const Value: TgxNavigatorAdjustDistanceParameters);
begin
  FAdjustDistanceParams.Assign(Value);
end;

function TgxSmoothNavigator.AdjustDistanceToPointEx(const APoint: TVector4f;
  ADeltaTime: Double): Boolean;

var
  lAbsolutePosition: TVector4f;
  lCurrentDistance: Single;
  lDistanceDifference, lTempCurrentDistance: Single;

  procedure DoAdjustDistanceToPoint(const DistanceValue: Single);
  var
    vect: TVector4f;
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

function TgxSmoothNavigator.AdjustDistanceToTargetEx(
  const ADeltaTime: Double): Boolean;
begin
  Assert(FMoveAroundParams.FTargetObject <> nil);
  Result := AdjustDistanceToPointEx(FMoveAroundParams.FTargetObject.AbsolutePosition,
                          ADeltaTime);
end;

procedure TgxSmoothNavigator.SetAdjustDistanceParamsEx(
  const Value: TgxNavigatorAdjustDistanceParametersEx);
begin
  FAdjustDistanceParamsEx.Assign(Value);
end;

procedure TgxSmoothNavigator.AnimateCustomItems(const ADeltaTime: Double);
begin
  FCustomAnimatedItems.DoProceed(ADeltaTime);
end;

procedure TgxSmoothNavigator.SetCustomAnimatedItems(
  const Value: TgxNavigatorSmoothChangeItems);
begin
  FCustomAnimatedItems.Assign(Value);
end;

{ TgxSmoothUserInterface }

function TgxSmoothUserInterface.MouseLook(
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

function TgxSmoothUserInterface.Mouselook(const NewX, NewY: Integer; const ADeltaTime: Double): Boolean;
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


function TgxSmoothUserInterface.MouseLook(const NewXY: TPoint; const ADeltaTime: Double): Boolean;
begin
  Result := Mouselook(NewXY.X, NewXY.Y, ADeltaTime);
end;

constructor TgxSmoothUserInterface.Create(AOwner: TComponent);
begin
  inherited;
  FMouseLookActive := False;
  FAutoUpdateMouse := True;
  FOriginalMousePos := TgxCoordinates2.CreateInitialized(Self,
                             VectorMake(GLGetScreenWidth div 2,
                             GLGetScreenHeight div 2, 0, 0), csPoint2D);
end;

procedure TgxSmoothUserInterface.Notification(AComponent: TComponent;
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

procedure TgxSmoothUserInterface.SetSmoothNavigator(
  const Value: TgxSmoothNavigator);
begin
  if FSmoothNavigator <> nil then
    FSmoothNavigator.RemoveFreeNotification(Self);

  FSmoothNavigator := Value;

  if FSmoothNavigator <> nil then
    FSmoothNavigator.FreeNotification(Self);
end;

destructor TgxSmoothUserInterface.Destroy;
begin
  FOriginalMousePos.Destroy;
  inherited;
end;

procedure TgxSmoothUserInterface.SetOriginalMousePos(
  const Value: TgxCoordinates2);
begin
  FOriginalMousePos.Assign(Value);
end;

procedure TgxSmoothUserInterface.SetSmoothVertNavigator(
  const Value: TgxSmoothNavigator);
begin
  if FSmoothVertNavigator <> nil then
    FSmoothVertNavigator.RemoveFreeNotification(Self);

  FSmoothVertNavigator := Value;

  if FSmoothVertNavigator <> nil then
    FSmoothVertNavigator.FreeNotification(Self);
end;

procedure TgxSmoothUserInterface.MouseLookActiveToggle;
begin
  if FMouseLookActive then
    SetMouseLookActive(False)
  else
    SetMouseLookActive(True)
end;

procedure TgxSmoothUserInterface.SetMouseLookActive(const Value: Boolean);
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

procedure TgxSmoothUserInterface.TurnHorizontal(const Angle: Single;
  const ADeltaTime: Double);
begin
  FSmoothNavigator.TurnHorizontal(Angle, ADeltaTime);
end;

procedure TgxSmoothUserInterface.TurnVertical(const Angle: Single;
  const ADeltaTime: Double);
begin
  if Assigned(FSmoothNavigator) then
    FSmoothNavigator.TurnVertical(Angle, ADeltaTime)
  else
    FSmoothVertNavigator.TurnVertical(Angle, ADeltaTime);
end;

{ TgxNavigatorInertiaParameters }

procedure TgxNavigatorInertiaParameters.Assign(Source: TPersistent);
begin
  if Source is TgxNavigatorInertiaParameters then
  begin
    FMovementAcceleration := TgxNavigatorInertiaParameters(Source).FMovementAcceleration;
    FMovementInertia := TgxNavigatorInertiaParameters(Source).FMovementInertia;
    FMovementSpeed := TgxNavigatorInertiaParameters(Source).FMovementSpeed;
    FTurnMaxAngle := TgxNavigatorInertiaParameters(Source).FTurnMaxAngle;
    FTurnInertia := TgxNavigatorInertiaParameters(Source).FTurnInertia;
    FTurnSpeed := TgxNavigatorInertiaParameters(Source).FTurnSpeed;
  end
  else
    inherited; //to the pit of doom ;)
end;

constructor TgxNavigatorInertiaParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;

  FTurnInertia := 150;
  FTurnSpeed := 50;
  FTurnMaxAngle := 0.5;

  FMovementAcceleration := 7;
  FMovementInertia := 200;
  FMovementSpeed := 200;
end;

function TgxNavigatorInertiaParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TgxNavigatorInertiaParameters.ScaleParameters(
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

function TgxNavigatorInertiaParameters.StoreTurnMaxAngle: Boolean;
begin
  Result := Abs(FTurnMaxAngle - 0.5) > EPS;
end;

function TgxNavigatorInertiaParameters.StoreMovementAcceleration: Boolean;
begin
  Result := Abs(FMovementAcceleration - 7) > EPS;
end;

function TgxNavigatorInertiaParameters.StoreMovementInertia: Boolean;
begin
  Result := Abs(FMovementInertia - 200) > EPS;
end;

function TgxNavigatorInertiaParameters.StoreMovementSpeed: Boolean;
begin
  Result := Abs(FMovementSpeed - 200) > EPS;
end;

function TgxNavigatorInertiaParameters.StoreTurnInertia: Boolean;
begin
  Result := Abs(FTurnInertia - 150) > EPS;
end;

function TgxNavigatorInertiaParameters.StoreTurnSpeed: Boolean;
begin
  Result := Abs(FTurnSpeed - 50) > EPS;
end;

{ TgxNavigatorGeneralParameters }

procedure TgxNavigatorGeneralParameters.Assign(Source: TPersistent);
begin
  if Source is TgxNavigatorGeneralParameters then
  begin
    FAutoScaleMin := TgxNavigatorGeneralParameters(Source).FAutoScaleMin;
    FAutoScaleMax := TgxNavigatorGeneralParameters(Source).FAutoScaleMax;
    FAutoScaleMult := TgxNavigatorGeneralParameters(Source).FAutoScaleMult;
  end
  else
    inherited; //die!
end;

constructor TgxNavigatorGeneralParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FAutoScaleMin := 0.1;
  FAutoScaleMax := 0.75;
  FAutoScaleMult := 2;
end;

function TgxNavigatorGeneralParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TgxNavigatorGeneralParameters.StoreAutoScaleMax: Boolean;
begin
  Result := Abs(FAutoScaleMax - 0.75) > EPS;
end;

function TgxNavigatorGeneralParameters.StoreAutoScaleMin: Boolean;
begin
  Result := Abs(FAutoScaleMin - 0.1) > EPS;
end;

function TgxNavigatorGeneralParameters.StoreAutoScaleMult: Boolean;
begin
  Result := Abs(FAutoScaleMult - 2) > EPS;
end;

{ TgxNavigatorMoveAroundParameters }

procedure TgxNavigatorMoveAroundParameters.Assign(Source: TPersistent);
begin
  if Source is TgxNavigatorMoveAroundParameters then
  begin
    FMaxAngle := TgxNavigatorMoveAroundParameters(Source).FMaxAngle;
    FInertia :=  TgxNavigatorMoveAroundParameters(Source).FInertia;
    FPitchSpeed :=  TgxNavigatorMoveAroundParameters(Source).FPitchSpeed;
    FTurnSpeed :=  TgxNavigatorMoveAroundParameters(Source).FTurnSpeed;
    FCutoff :=  TgxNavigatorMoveAroundParameters(Source).FCutoff;
    SetTargetObject(TgxNavigatorMoveAroundParameters(Source).FTargetObject);
  end
  else
    inherited; //die
end;

constructor TgxNavigatorMoveAroundParameters.Create(AOwner: TPersistent);
begin
  FOwner := AOwner;
  FPitchSpeed := 500;
  FTurnSpeed  := 500;
  FInertia    := 65;
  FMaxAngle   := 1.5;
  FCutoff     := EPS2;
end;

function TgxNavigatorMoveAroundParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TgxNavigatorMoveAroundParameters.ScaleParameters(
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

procedure TgxNavigatorMoveAroundParameters.SetTargetObject(
  const Value: TgxBaseSceneObject);
begin
  if FTargetObject <> nil then
    if FOwner is TgxSmoothNavigator then
      FTargetObject.RemoveFreeNotification(TgxSmoothNavigator(FOwner));

  FTargetObject := Value;

  if FTargetObject <> nil then
    if FOwner is TgxSmoothNavigator then
      FTargetObject.FreeNotification(TgxSmoothNavigator(FOwner));
end;

function TgxNavigatorMoveAroundParameters.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS2) > EPS8;
end;

function TgxNavigatorMoveAroundParameters.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 65) > EPS;
end;

function TgxNavigatorMoveAroundParameters.StoreMaxAngle: Boolean;
begin
  Result := Abs(FMaxAngle - 1.5) > EPS;
end;

function TgxNavigatorMoveAroundParameters.StorePitchSpeed: Boolean;
begin
  Result := Abs(FPitchSpeed - 500) > EPS;
end;

function TgxNavigatorMoveAroundParameters.StoreTurnSpeed: Boolean;
begin
  Result := Abs(FTurnSpeed - 500) > EPS;
end;

{ TgxNavigatorAdjustDistanceParameters }

procedure TgxNavigatorAdjustDistanceParameters.AddImpulse(
  const Impulse: Single);
begin
  FOldDistanceRatio := FOldDistanceRatio + Impulse * FSpeed / FInertia * FImpulseSpeed;
end;

procedure TgxNavigatorAdjustDistanceParameters.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TgxNavigatorAdjustDistanceParameters then
  begin
    FImpulseSpeed := TgxNavigatorAdjustDistanceParameters(Source).FImpulseSpeed;
  end;
end;

constructor TgxNavigatorAdjustDistanceParameters.Create(
  AOwner: TPersistent);
begin
  inherited;
  FImpulseSpeed := 0.02;
end;


procedure TgxNavigatorAdjustDistanceParameters.ScaleParameters(
  const Value: Single);
begin
  inherited;
  FImpulseSpeed := FImpulseSpeed / Value;
end;

function TgxNavigatorAdjustDistanceParameters.StoreImpulseSpeed: Boolean;
begin
  Result := Abs(FImpulseSpeed - 0.02) > EPS;
end;

{ TgxNavigatorAbstractParameters }


procedure TgxNavigatorAbstractParameters.Assign(Source: TPersistent);
begin
  if Source is TgxNavigatorAbstractParameters then
  begin
    FInertia := TgxNavigatorAbstractParameters(Source).FInertia;
    FSpeed :=   TgxNavigatorAbstractParameters(Source).FSpeed;
    FCutoff :=  TgxNavigatorAbstractParameters(Source).FCutoff;
  end
  else
    inherited; //to the pit of doom ;)
end;

constructor TgxNavigatorAbstractParameters.Create(
  AOwner: TPersistent);
begin
  FOwner := AOwner;
  FInertia := 100;
  FSpeed := 0.005;
  FCutoff := EPS;
end;

function TgxNavigatorAbstractParameters.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TgxNavigatorAbstractParameters.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / PowerSingle(2, Value)
  else
    FInertia := FInertia * PowerSingle(2, 1 / Value);
end;

function TgxNavigatorAbstractParameters.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS) > EPS2;
end;

function TgxNavigatorAbstractParameters.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 100) > EPS;
end;

function TgxNavigatorAbstractParameters.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 0.005) > EPS2;
end;

{ TgxNavigatorAdjustDistanceParametersEx }

procedure TgxNavigatorAdjustDistanceParametersEx.Assign(
  Source: TPersistent);
begin
  if Source is TgxNavigatorAdjustDistanceParametersEx then
  begin
    FTargetDistance := TgxNavigatorAdjustDistanceParametersEx(Source).FTargetDistance;
    FSpeedLimit := TgxNavigatorAdjustDistanceParametersEx(Source).FSpeedLimit;
  end
  else
    inherited;
end;

constructor TgxNavigatorAdjustDistanceParametersEx.Create(
  AOwner: TPersistent);
begin
  inherited;
  FInertia := 0.5;
  FTargetDistance := 100;
  FSpeed := 100;
  FSpeedLimit := 20000;
end;

function TgxNavigatorAdjustDistanceParametersEx.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 0.5) > EPS2;
end;

function TgxNavigatorAdjustDistanceParametersEx.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 100) > EPS2;
end;

function TgxNavigatorAdjustDistanceParametersEx.StoreSpeedLimit: Boolean;
begin
  Result := Abs(FSpeedLimit - 20000) > EPS2;
end;

function TgxNavigatorAdjustDistanceParametersEx.StoreTargetDistance: Boolean;
begin
  Result := Abs(FTargetDistance - 100) > EPS2;
end;

{ TgxNavigatorSmoothChangeItem }

procedure TgxNavigatorSmoothChangeItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TgxNavigatorSmoothChangeItem then
  begin
    FInertia :=    TgxNavigatorSmoothChangeItem(Source).FInertia;
    FSpeed :=      TgxNavigatorSmoothChangeItem(Source).FSpeed;
    FSpeedLimit := TgxNavigatorSmoothChangeItem(Source).FSpeedLimit;
    FCutoff :=     TgxNavigatorSmoothChangeItem(Source).FCutoff;
    FEnabled :=    TgxNavigatorSmoothChangeItem(Source).FEnabled;
  end;
end;

constructor TgxNavigatorSmoothChangeItem.Create(aOwner: TXCollection);
begin
  inherited;
  FInertia := 1;
  FSpeed := 5.5;
  FSpeedLimit := 20000;
  FCutoff := EPS;
  FEnabled := True;
end;

function TgxNavigatorSmoothChangeItem.GetNavigator: TgxSmoothNavigator;
begin
  Result := TgxSmoothNavigator(TgxNavigatorSmoothChangeItems(GetOwner).Owner);
end;

procedure TgxNavigatorSmoothChangeItem.ScaleParameters(
  const Value: Single);
begin
  Assert(Value > 0);

  if Value < 1 then
    FInertia := FInertia / PowerSingle(2, Value)
  else
    FInertia := FInertia * PowerSingle(2, 1 / Value);
end;

function TgxNavigatorSmoothChangeItem.StoreCutoff: Boolean;
begin
  Result := Abs(FCutoff - EPS) > EPS8;
end;

function TgxNavigatorSmoothChangeItem.StoreInertia: Boolean;
begin
  Result := Abs(FInertia - 1) > EPS;
end;

function TgxNavigatorSmoothChangeItem.StoreSpeed: Boolean;
begin
  Result := Abs(FSpeed - 5.5) > EPS2;
end;

function TgxNavigatorSmoothChangeItem.StoreSpeedLimit: Boolean;
begin
  Result := Abs(FSpeedLimit - 20000) > EPS2;
end;

{ TgxNavigatorSmoothChangeItems }

function TgxNavigatorSmoothChangeItems.Add(AClass : TgxNavigatorSmoothChangeItemClass): TgxNavigatorSmoothChangeItem;
begin
  Result := AClass.Create(Self);
end;

function TgxNavigatorSmoothChangeItems.CanAdd(AClass: TXCollectionItemClass): Boolean;
begin
  Result := AClass.InheritsFrom(TgxNavigatorSmoothChangeItem);
end;

procedure TgxNavigatorSmoothChangeItems.DoProceed(ADeltaTime: Double);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItems(I).Proceed(ADeltaTime);
end;

function TgxNavigatorSmoothChangeItems.GetItems(const Index : Integer): TgxNavigatorSmoothChangeItem;
begin
  Result := TgxNavigatorSmoothChangeItem(inherited GetItems(Index));
end;

class function TgxNavigatorSmoothChangeItems.ItemsClass: TXCollectionItemClass;
begin
  Result := TgxNavigatorSmoothChangeItem;
end;

procedure TgxNavigatorSmoothChangeItems.SetItems(const Index : Integer; const Value:
        TgxNavigatorSmoothChangeItem);
begin
  GetItems(Index).Assign(Value);
end;

{ TgxNavigatorSmoothChangeSingle }

procedure TgxNavigatorSmoothChangeSingle.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  
  if Source is TgxNavigatorSmoothChangeVector then
  begin
    FTargetValue := TgxNavigatorSmoothChangeSingle(Source).TargetValue;
    FOnGetCurrentValue := TgxNavigatorSmoothChangeSingle(Source).FOnGetCurrentValue;
    FOnSetCurrentValue := TgxNavigatorSmoothChangeSingle(Source).FOnSetCurrentValue;
  end;
end;

class function TgxNavigatorSmoothChangeSingle.FriendlyName: string;
begin
  Result := 'Navigator SmoothChange Single';
end;

function TgxNavigatorSmoothChangeSingle.Proceed(ADeltaTime: Double): Boolean;
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

procedure TgxNavigatorSmoothChangeSingle.ResetTargetValue;
begin
  FTargetValue := FOnGetCurrentValue(Self);
end;

{ TgxNavigatorSmoothChangeVector }

procedure TgxNavigatorSmoothChangeVector.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  
  if Source is TgxNavigatorSmoothChangeVector then
  begin
    FTargetValue.Assign(TgxNavigatorSmoothChangeVector(Source).TargetValue);
    FOnGetCurrentValue := TgxNavigatorSmoothChangeVector(Source).FOnGetCurrentValue;
    FOnSetCurrentValue := TgxNavigatorSmoothChangeVector(Source).FOnSetCurrentValue;
  end;
end;

constructor TgxNavigatorSmoothChangeVector.Create(aOwner: TXCollection);
begin
  inherited;
  FTargetValue := TgxCoordinates.CreateInitialized(Self, NullHmgVector, csVector);
end;

destructor TgxNavigatorSmoothChangeVector.Destroy;
begin
  FTargetValue.Free;
  inherited;
end;

class function TgxNavigatorSmoothChangeVector.FriendlyName: string;
begin
  Result := 'Navigator SmoothChange Vector';
end;

function TgxNavigatorSmoothChangeVector.Proceed(ADeltaTime: Double): Boolean;
var
  lAbsolutePosition: TVector4f;
  lCurrentDistance: Single;
  lTotalDistanceToTravelThisTime, lDistanceToTravelThisTime: Single;
  lMaxExpectedDeltaTime: Double;

  procedure DoAdjustDistanceToPoint();
  var
    vect: TVector4f;
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

procedure TgxNavigatorSmoothChangeVector.ResetTargetValue;
begin
  FTargetValue.DirectVector := FOnGetCurrentValue(Self);
end;

procedure TgxNavigatorSmoothChangeVector.SetTargetValue(
  const Value: TgxCoordinates);
begin
  FTargetValue.Assign(Value);
end;

initialization
  RegisterClasses([
      TgxSmoothNavigator, TgxSmoothUserInterface,
      TgxNavigatorInertiaParameters, TgxNavigatorGeneralParameters,
      TgxNavigatorMoveAroundParameters,
      TgxNavigatorAdjustDistanceParameters, TgxNavigatorAdjustDistanceParametersEx
                   ]);

  RegisterXCollectionItemClass(TgxNavigatorSmoothChangeSingle);
  RegisterXCollectionItemClass(TgxNavigatorSmoothChangeVector);
end.


