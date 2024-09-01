//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.SimpleNavigation;

(*
    A simple component written by request from someone at the www.glscene.ru forums.
    Allows to view the FPS and do the usual Zoom and MoveAroundTarget stuff
    that all demos usually have in themselves. All that is just by dropping
    this component on the form.
*)

interface

{$I GXS.Scene.inc}

uses
  System.Types,
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  System.Math,
  FMX.Forms,
  FMX.Controls,
  FMX.ExtCtrls,
  FMX.Types,
  GXS.VectorGeometry,
  GXS.Scene,
  GXS.SceneViewer,
  GXS.Strings;

type

  TgxSimpleNavigationOption = (
    snoInvertMoveAroundX, snoInvertMoveAroundY, // MoveAroundTarget.
    snoInvertZoom, snoInvertMouseWheel, // Zoom.
    snoInvertRotateX, snoInvertRotateY, // RotateTarget.
    snoMouseWheelHandled, // MouseWheel.
    snoShowFPS // Show FPS
    );

  TgxSimpleNavigationOptions = set of TgxSimpleNavigationOption;

  TgxSimpleNavigationAction = (snaNone, snaMoveAroundTarget, snaZoom, snaRotateTarget, snaCustom);

  TgxSimpleNavigationKeyCombination = class;
  TSimpleNavigationCustomActionEvent =
    procedure(Sender: TgxSimpleNavigationKeyCombination; Shift: TShiftState; X, Y: Single) of object;

  TgxSimpleNavigationKeyCombination = class(TCollectionItem)
  private
    FExitOnMatch: Boolean;
    FAction: TgxSimpleNavigationAction;
    FOnCustomAction: TSimpleNavigationCustomActionEvent;
    FShiftState: TShiftState;
  protected
    function GetDisplayName: string; override;
    procedure DoOnCustomAction(Shift: TShiftState; X, Y: Single); virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ShiftState: TShiftState read FShiftState write FShiftState default [];
    property ExitOnMatch: Boolean read FExitOnMatch write FExitOnMatch default True;
    property Action: TgxSimpleNavigationAction read FAction write FAction default snaNone;
    property OnCustomAction: TSimpleNavigationCustomActionEvent read FOnCustomAction write FOnCustomAction;
  end;

  TgxSimpleNavigationKeyCombinations = class(TOwnedCollection)
  private
    function GetItems(Index: Integer): TgxSimpleNavigationKeyCombination;
    procedure SetItems(Index: Integer; const Value: TgxSimpleNavigationKeyCombination);
  public
    function Add: TgxSimpleNavigationKeyCombination; overload;
    function Add(const AShiftState: TShiftState; const AAction: TgxSimpleNavigationAction; const AExitOnMatch: Boolean = True): TgxSimpleNavigationKeyCombination; overload;
    property Items[Index: Integer]: TgxSimpleNavigationKeyCombination read GetItems write SetItems; default;
  end;

  TgxSimpleNavigation = class(TComponent)
  private
    FTimer: TTimer;
    FForm: TCustomForm;
    FGLXceneViewer: TgxSceneViewer;
    FOldX, FOldY: Single;
    FFormCaption: string;
    FMoveAroundTargetSpeed: Single;
    FZoomSpeed: Single;
    FOptions: TgxSimpleNavigationOptions;
    FKeyCombinations: TgxSimpleNavigationKeyCombinations;
    FRotateTargetSpeed: Single;
    FOnMouseMove: TMouseMoveEvent;
    procedure ShowFPS(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Single);
    procedure ViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure SetGLXceneViewer(const Value: TgxSceneViewer);
    procedure SetForm(const Value: TCustomForm);
    function StoreFormCaption: Boolean;
    function StoreMoveAroundTargetSpeed: Boolean;
    function StoreZoomSpeed: Boolean;
    procedure SetKeyCombinations(const Value: TgxSimpleNavigationKeyCombinations);
    function StoreRotateTargetSpeed: Boolean;
    procedure SetOptions(const Value: TgxSimpleNavigationOptions);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Form: TCustomForm read FForm write SetForm;
    property GLXceneViewer: TgxSceneViewer read FGLXceneViewer write SetGLXceneViewer;
    property ZoomSpeed: Single read FZoomSpeed write FZoomSpeed stored StoreZoomSpeed;
    property MoveAroundTargetSpeed: Single read FMoveAroundTargetSpeed write FMoveAroundTargetSpeed stored StoreMoveAroundTargetSpeed;
    property RotateTargetSpeed: Single read FRotateTargetSpeed write FRotateTargetSpeed stored StoreRotateTargetSpeed;
    property FormCaption: string read FFormCaption write FFormCaption stored StoreFormCaption;
    property Options: TgxSimpleNavigationOptions read FOptions write SetOptions default [snoMouseWheelHandled, snoShowFPS];
    property KeyCombinations: TgxSimpleNavigationKeyCombinations read FKeyCombinations write SetKeyCombinations;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
  end;

//----------------------------------------------
implementation
//----------------------------------------------

const
  vFPSString = '%FPS';
  EPS = 0.001;

  { TgxSimpleNavigation }

procedure TgxSimpleNavigation.Assign(Source: TPersistent);
begin
  if Source is TgxSimpleNavigation then
  begin
    { Don't do that, because that might overide the original component's event handlers
    SetForm(TgxSimpleNavigation(Source).FForm);
    SetGLXceneViewer(TgxSimpleNavigation(Source).FGLXceneViewer);
    }
    FZoomSpeed := TgxSimpleNavigation(Source).FZoomSpeed;
    FMoveAroundTargetSpeed := TgxSimpleNavigation(Source).FMoveAroundTargetSpeed;
    FRotateTargetSpeed := TgxSimpleNavigation(Source).FRotateTargetSpeed;
    FFormCaption := TgxSimpleNavigation(Source).FFormCaption;
    FOptions := TgxSimpleNavigation(Source).FOptions;
    FKeyCombinations.Assign(TgxSimpleNavigation(Source).FKeyCombinations);
  end
  else
    inherited; // Die!
end;

constructor TgxSimpleNavigation.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FKeyCombinations := TgxSimpleNavigationKeyCombinations.Create(Self, TgxSimpleNavigationKeyCombination);
  FKeyCombinations.Add([ssLeft, ssRight], snaZoom, True);
  FKeyCombinations.Add([ssLeft], snaMoveAroundTarget, True);
  FKeyCombinations.Add([ssRight], snaMoveAroundTarget, True);

  FMoveAroundTargetSpeed := 1;
  FRotateTargetSpeed := 1;
  FZoomSpeed := 1.5;
  FOptions := [snoMouseWheelHandled, snoShowFPS];
  FFormCaption := vFPSString;

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := ShowFPS;

  FOnMouseMove := nil;
  //Detect form
  if AOwner is TCustomForm then SetForm(TCustomForm(AOwner));

  //Detect SceneViewer
  if FForm <> nil then
  begin
    if FForm.ComponentCount <> 0 then
      for I := 0 to FForm.ComponentCount - 1 do
        if FForm.Components[I] is TgxSceneViewer then
        begin
          SetGLXceneViewer(TgxSceneViewer(FForm.Components[I]));
          Exit;
        end;
  end;
end;

destructor TgxSimpleNavigation.Destroy;
begin
  FTimer.Free;
  FKeyCombinations.Free;

  if FForm <> nil then
    TForm(FForm).OnMouseWheel := nil;

  if FGLXceneViewer <> nil then
    FGLXceneViewer.OnMouseMove := nil;

  inherited;
end;

procedure TgxSimpleNavigation.ViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Sign: SmallInt;
begin
  if (csDesigning in ComponentState) or (WheelDelta = 0) then
    Exit;

  if snoInvertMouseWheel in FOptions then
    Sign := 1
  else
    Sign := -1;

  if FGLXceneViewer <> nil then
    if FGLXceneViewer.Camera <> nil then
      FGLXceneViewer.Camera.AdjustDistanceToTarget(
                      Power(FZoomSpeed, Sign * WheelDelta div Abs(WheelDelta)));

  Handled := snoMouseWheelHandled in FOptions;
end;

procedure TgxSimpleNavigation.ViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);

  procedure DoZoom;
  var
    Sign: SmallInt;
  begin
    if snoInvertZoom in FOptions then
      Sign := -1
    else
      Sign := 1;
    FGLXceneViewer.Camera.AdjustDistanceToTarget(
                                    Power(FZoomSpeed, Sign * (Y - FOldY) / 20));
  end;

  procedure DoMoveAroundTarget;
  var
    SignX: SmallInt;
    SignY: SmallInt;
  begin
    if snoInvertMoveAroundX in FOptions then
      SignX := -1
    else
      SignX := 1;

    if snoInvertMoveAroundY in FOptions then
      SignY := -1
    else
      SignY := 1;

    FGLXceneViewer.Camera.MoveAroundTarget(SignX * FMoveAroundTargetSpeed * (FOldY - Y),
                                           SignY * FMoveAroundTargetSpeed * (FOldX - X));
  end;

  procedure DoRotateTarget;
  var
    SignX: SmallInt;
    SignY: SmallInt;
  begin
    if snoInvertRotateX in FOptions then
      SignX := -1
    else
      SignX := 1;

    if snoInvertRotateY in FOptions then
      SignY := -1
    else
      SignY := 1;

    FGLXceneViewer.Camera.RotateTarget(SignY * FRotateTargetSpeed * (FOldY - Y),
                                       SignX * FRotateTargetSpeed * (FOldX - X));
  end;

var
  I: Integer;

begin
  if csDesigning in ComponentState then
    exit;

  if FGLXceneViewer <> nil then
    if FGLXceneViewer.Camera <> nil then
    begin
    if FKeyCombinations.Count <> 0 then
      for I := 0 to FKeyCombinations.Count - 1 do
        if FKeyCombinations[I].FShiftState <= Shift then
        begin
          case FKeyCombinations[I].FAction of
            snaNone: ; //Ignore.
            snaMoveAroundTarget: DoMoveAroundTarget;
            snaZoom: DoZoom;
            snaRotateTarget: DoRotateTarget;
            snaCustom: FKeyCombinations[I].DoOnCustomAction(Shift, X, Y);
          else
            Assert(False, strErrorEx + strUnknownType);
          end;

          if FKeyCombinations[I].FExitOnMatch then
            Break;
        end;
  end;

  FOldX := X;
  FOldY := Y;

  if Assigned(FOnMouseMove) then FOnMouseMove(Self, Shift, X, Y);
end;

procedure TgxSimpleNavigation.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FGLXceneViewer) and (Operation = opRemove) then
    FGLXceneViewer := nil;
  if (AComponent = FForm) and (Operation = opRemove) then
    FForm := nil;
end;

procedure TgxSimpleNavigation.SetKeyCombinations(
  const Value: TgxSimpleNavigationKeyCombinations);
begin
  FKeyCombinations.Assign(Value);
end;

procedure TgxSimpleNavigation.SetForm(const Value: TCustomForm);
begin
  if FForm <> nil then
  begin
    FForm.RemoveFreeNotification(Self);
    TForm(FForm).OnMouseWheel := nil;
    TForm(FForm).OnMouseMove := nil;
    if FFormCaption = vFPSString then
      FFormCaption := FForm.Caption + ' - ' + vFPSString;
    FForm.FreeNotification(Self);
  end;
  FForm := Value;

end;

procedure TgxSimpleNavigation.SetGLXceneViewer(
  const Value: TgxSceneViewer);
begin
  if FGLXceneViewer <> nil then
  begin
    FGLXceneViewer.RemoveFreeNotification(Self);
    FGLXceneViewer.OnMouseMove := nil;
  end;

  FGLXceneViewer := Value;

  if FGLXceneViewer <> nil then
  begin
    FGLXceneViewer.OnMouseMove := ViewerMouseMove;
    FGLXceneViewer.FreeNotification(Self);
  end;
end;

procedure TgxSimpleNavigation.ShowFPS(Sender: TObject);
var
  Index: Integer;
  Temp: string;
begin
  if (FGLXceneViewer <> nil) and
     (FForm <> nil) and
     not(csDesigning in ComponentState) and
     (snoShowFPS in FOptions) then
  begin
    Temp := FFormCaption;
    Index := Pos(vFPSString, Temp);
    if Index <> 0 then
    begin
      Delete(Temp, Index, Length(vFPSString));
      Insert(FGLXceneViewer.FramesPerSecondText, Temp, Index);
    end;
    FForm.Caption := Temp;
    FGLXceneViewer.ResetPerformanceMonitor;
  end;
end;

function TgxSimpleNavigation.StoreFormCaption: Boolean;
begin
  Result := (FFormCaption <> vFPSString);
end;

function TgxSimpleNavigation.StoreMoveAroundTargetSpeed: Boolean;
begin
  Result := Abs(FMoveAroundTargetSpeed - 1) > EPS;
end;

function TgxSimpleNavigation.StoreZoomSpeed: Boolean;
begin
  Result := Abs(FZoomSpeed - 1.5) > EPS;
end;

function TgxSimpleNavigation.StoreRotateTargetSpeed: Boolean;
begin
  Result := Abs(FRotateTargetSpeed - 1) > EPS;
end;

procedure TgxSimpleNavigation.SetOptions(
  const Value: TgxSimpleNavigationOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;

  end;
end;

{ TgxSimpleNavigationKeyCombination }

procedure TgxSimpleNavigationKeyCombination.Assign(Source: TPersistent);
begin
  if Source is TgxSimpleNavigationKeyCombination then
  begin
    FExitOnMatch := TgxSimpleNavigationKeyCombination(Source).FExitOnMatch;
    FAction := TgxSimpleNavigationKeyCombination(Source).FAction;
    FOnCustomAction := TgxSimpleNavigationKeyCombination(Source).FOnCustomAction;
    FShiftState := TgxSimpleNavigationKeyCombination(Source).FShiftState;
  end
  else
    inherited; // Die!
end;

constructor TgxSimpleNavigationKeyCombination.Create(Collection: TCollection);
begin
  inherited;
  FAction := snaNone;
  FExitOnMatch := True;
end;

procedure TgxSimpleNavigationKeyCombination.DoOnCustomAction(
  Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FOnCustomAction) then
    FOnCustomAction(Self, Shift, X, Y);
end;

function TgxSimpleNavigationKeyCombination.GetDisplayName: string;
begin
  Result := GetSetProp(Self, 'ShiftState', True) + '  -  ' +
    GetEnumName(TypeInfo(TgxSimpleNavigationAction), Integer(FAction));
end;

{ TgxSimpleNavigationKeyCombinations }

function TgxSimpleNavigationKeyCombinations.Add: TgxSimpleNavigationKeyCombination;
begin
  Result := TgxSimpleNavigationKeyCombination(inherited Add);
end;

function TgxSimpleNavigationKeyCombinations.Add(
  const AShiftState: TShiftState; const AAction: TgxSimpleNavigationAction;
  const AExitOnMatch: Boolean): TgxSimpleNavigationKeyCombination;
begin
  Result := Add;
  with Result do
  begin
    FShiftState := AShiftState;
    FAction := AAction;
    FExitOnMatch := AExitOnMatch;
  end;
end;

function TgxSimpleNavigationKeyCombinations.GetItems(
  Index: Integer): TgxSimpleNavigationKeyCombination;
begin
  Result := TgxSimpleNavigationKeyCombination(inherited GetItem(Index));
end;

procedure TgxSimpleNavigationKeyCombinations.SetItems(Index: Integer;
  const Value: TgxSimpleNavigationKeyCombination);
begin
  inherited SetItem(Index, Value);
end;

end.

