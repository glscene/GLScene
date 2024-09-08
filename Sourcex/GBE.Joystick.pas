unit GBE.Joystick;

interface

uses
  System.SysUtils,
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Layouts,
  GBE.PlayerPosition,
  System.Math.Vectors,
  System.Types,
  FMX.Viewport3D,
  System.UITypes,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Graphics,
  FMX.Ani,
  uGBEUtils3D;

type
  TGBEJoystickType = (jtOrientation, jtDeplacement, jtOrientationDeplacement);

  TGBEJoystick = class(TLayout)
  private
    fPlayerPosition: TGBEPlayerPosition;
    FPosDepartCurseur: TPointF;
    // Position of the mouse mark at the start of the mouse movement
    fViewport3D: TViewport3D;
    fCircle, fCircle2: TCircle;
    fSensitivity: Integer;
    fShowIntegrateJoystick, useJoystick, fMouseCapture: Boolean;
    fPoint: TPoint3D;
    fJoystickType: TGBEJoystickType;
    Offset: TPointF; // Offset between click location and joystick circle center
    fAcceleration: Single;
    procedure SetAngleDeVue(const Value: TPointF); // Changing the viewing angle
    function GetDirection: TPoint3D;
    procedure SetShowIntegrateJoystick(const Value: Boolean);
    procedure SetJoystickType(const Value: TGBEJoystickType);
    function GetDirectionSidewayRight: TPoint3D;
    function GetDirectionSidewayLeft: TPoint3D;
    function GetMouseCapture: Boolean;
    procedure SetMouseCapture(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure DoMouseLeave; override;
    procedure Resize; override;
    procedure Paint; override;
    procedure InitialiserJoystick;
    function OrientationKeyManagement(rightKey, leftKey, upKey, downKey, goUp,
      goDown, sideWayRight, sideWayLeft: Boolean;
      sensibility, speed, maxspeed: Single): Single;
  published
    property PlayerPosition: TGBEPlayerPosition read fPlayerPosition
      write fPlayerPosition;
    property JoystickType: TGBEJoystickType read fJoystickType
      write SetJoystickType;
    property AngleDeVue: TPointF write SetAngleDeVue; // Viewing angle property
    property Direction: TPoint3D read GetDirection;
    property DirectionSidewayRight: TPoint3D read GetDirectionSidewayRight;
    property DirectionSidewayLeft: TPoint3D read GetDirectionSidewayLeft;
    property Deplacement: TPoint3D read fPoint write fPoint;
    property HitTest default true;
    property Viewport3D: TViewport3D read fViewport3D write fViewport3D;
    property ShowIntegrateJoystick: Boolean read fShowIntegrateJoystick
      write SetShowIntegrateJoystick;
    property Acceleration: Single read fAcceleration write fAcceleration;
    property Sensitivity: Integer read fSensitivity write fSensitivity;
    property MouseCapture: Boolean read GetMouseCapture write SetMouseCapture;
  end;

procedure Register;

implementation // --------------------------------------------------------------

// TGBEJoystick

constructor TGBEJoystick.Create(AOwner: TComponent);
begin
  inherited;
  HitTest := true;
  fCircle := TCircle.Create(nil);
  fCircle.Parent := self;
  fCircle.Stored := false;
  fCircle.Locked := true;
  fCircle.Fill.Kind := TBrushKind.Gradient;
  fCircle.Fill.Gradient.Color := $FFB6B6B6;
  fCircle.Fill.Gradient.Color1 := $FF888888;
  fCircle.Fill.Gradient.Style := TGradientStyle.Linear;
  fCircle.HitTest := false;

  fCircle2 := TCircle.Create(nil);
  fCircle2.Parent := fCircle;
  fCircle2.Stored := false;
  fCircle2.Locked := true;
  fCircle2.Fill.Kind := TBrushKind.Gradient;
  fCircle2.Fill.Gradient.Color := $FF888888;
  fCircle2.Fill.Gradient.Color1 := $FFB6B6B6;
  fCircle2.Fill.Gradient.Style := TGradientStyle.Linear;
  fCircle.Stroke.Thickness := 2;
  fCircle2.width := fCircle.width - 20;
  fCircle2.height := fCircle.height - 20;
  fCircle2.position.X := (fCircle.width - fCircle2.width) / 2;
  fCircle2.position.Y := (fCircle.height - fCircle2.height) / 2;
  fCircle2.HitTest := false;
  fCircle2.Opacity := 0.7;

  fShowIntegrateJoystick := true;
  fSensitivity := 90;
  fCircle.Align := TAlignLayout.Client;
  fPoint := Point3D(1, 0, 1);
  fAcceleration := 0;

  useJoystick := false;
  fMouseCapture := false;
  fJoystickType := TGBEJoystickType.jtDeplacement;
end;

function TGBEJoystick.GetDirection: TPoint3D;
begin
  if (fJoystickType = jtDeplacement) or
    (fJoystickType = jtOrientationDeplacement) then
  begin
    if assigned(fPlayerPosition) then
    begin
      result := fPoint * (fPlayerPosition.getPositionDirection.AbsolutePosition
        - fPlayerPosition.AbsolutePosition).Normalize;
    end
    else
      result := fPoint;
  end
  else
    result := Point3D(0, 0, 0);
end;

function TGBEJoystick.GetDirectionSidewayRight: TPoint3D;
begin
  result := GetDirection.Rotate(Point3D(0, 1, 0), -Pi * 0.5);
  // if (fJoystickType = jtDeplacement) or (fJoystickType = jtOrientationDeplacement) then
  // begin
  // if assigned(fPlayerPosition) then
  // begin
  // result := fPoint * (fPlayerPosition.getSidewayRightDirection.AbsolutePosition - fPlayerPosition.AbsolutePosition).Normalize;
  // end
  // else result := fPoint;
  // end
  // else result := Point3D(0,0,0);
end;

function TGBEJoystick.GetMouseCapture: Boolean;
begin
  result := fMouseCapture;
end;

function TGBEJoystick.GetDirectionSidewayLeft: TPoint3D;
begin
  result := GetDirection.Rotate(Point3D(0, 1, 0), Pi * 0.5);
  // if (fJoystickType = jtDeplacement) or (fJoystickType = jtOrientationDeplacement) then
  // begin
  // if assigned(fPlayerPosition) then
  // begin
  // result := fPoint * (fPlayerPosition.getSidewayLeftDirection.AbsolutePosition - fPlayerPosition.AbsolutePosition).Normalize;
  // end
  // else result := fPoint;
  // end
  // else result := Point3D(0,0,0);
end;

procedure TGBEJoystick.InitialiserJoystick;
begin
  useJoystick := false;
  TAnimator.AnimateFloat(fCircle2, 'Position.X',
    (fCircle.width - fCircle2.width) / 2);
  TAnimator.AnimateFloat(fCircle2, 'Position.Y',
    (fCircle.height - fCircle2.height) / 2);
  if (fJoystickType = jtDeplacement) or
    (fJoystickType = jtOrientationDeplacement) then
    fAcceleration := 0;
end;

procedure TGBEJoystick.SetAngleDeVue(const Value: TPointF);
var
  // ptA arrival point, ptD departure point, S sensitivity
  ptA, ptD, S: TPointF;
begin
  if assigned(fPlayerPosition) then
  begin
    if assigned(fViewport3D) then
    begin
      // Sensitivity adjustment for right/left orientation
      S.X := fSensitivity / self.width;
      // Sensitivity adjustment for up/down orientation
      S.Y := fSensitivity / self.height;
      ptA := Value * S; // Arrival point adapted to sensitivity
      ptD := FPosDepartCurseur * S; // Starting point adapted to sensitivity
      // Right/Left view
      with fPlayerPosition.RotationAngle do
        Y := Y + (ptA.X - ptD.X);
      // right/left orientation (y axis) based on mouse movement in X
      // Top/Bottom View
      with fPlayerPosition.getDummyOrientation.RotationAngle do
        X := X + (ptD.Y - ptA.Y);
      // the same for the up/down orientation by adapting
      // (rotation on the x axis, e function of the movement of the mouse in Y
      FPosDepartCurseur := Value;
      // the cursor position when the user clicked (the origin of the direction),
      // is updated with the new cursor position: on the next call to OnMouseMove,
      // the starting position must be the ending position of the previous move
    end;
  end;
end;

procedure TGBEJoystick.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  if ssLeft in Shift then
  begin
    FPosDepartCurseur := PointF(X, Y);
    useJoystick := true;
  end;
  Offset.X := X;
  Offset.Y := Y;
end;

procedure TGBEJoystick.DoMouseLeave;
begin
  inherited;
  InitialiserJoystick;
end;

procedure TGBEJoystick.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  if ssLeft in Shift then
  begin
    if (Viewport3D <> nil) and (PlayerPosition <> nil) then
    begin
      if (fJoystickType = jtOrientation) or
        (fJoystickType = jtOrientationDeplacement) then
        AngleDeVue := PointF(X, Y);
      fCircle2.position.X := X - Offset.X;
      fCircle2.position.Y := Y - Offset.Y;
      interactionIHM(Viewport3D);
    end;
  end;
end;

procedure TGBEJoystick.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  InitialiserJoystick;
end;

procedure TGBEJoystick.Paint;
begin
  inherited;
  // if useJoystick then
  // begin
  // if (fJoystickType = jtDeplacement) or (fJoystickType = jtOrientationDeplacement) then
  // begin
  // if assigned(fPlayerPosition) then
  // begin
  /// /        FAcceleration := FAcceleration + ((fCircle.Height - fCircle2.Height)*0.5 + fCircle2.position.Y) / Sensitivity;
  /// /        fPlayerPosition.RotationAngle.Y := fPlayerPosition.RotationAngle.Y - ((fCircle.Width - fCircle2.Width)*0.5 - fCircle2.Position.X) / Sensitivity;
  // end;
  // end;
  // end;
end;

procedure TGBEJoystick.Resize;
begin
  inherited;
  fCircle2.width := fCircle.width - 20;
  fCircle2.height := fCircle.height - 20;
  fCircle2.position.X := (fCircle.width - fCircle2.width) * 0.5;
  fCircle2.position.Y := (fCircle.height - fCircle2.height) * 0.5;
end;

procedure TGBEJoystick.SetJoystickType(const Value: TGBEJoystickType);
begin
  fJoystickType := Value;
  case Value of
    jtOrientation:
      begin // To improve
      end;
    jtDeplacement:
      begin // To improve
      end;
    jtOrientationDeplacement:
      begin // To improve
      end;
  end;
end;

procedure TGBEJoystick.SetMouseCapture(const Value: Boolean);
begin
  if Value <> fMouseCapture then
  begin
    fMouseCapture := Value;
    AutoCapture := Value;
  end;
end;

procedure TGBEJoystick.SetShowIntegrateJoystick(const Value: Boolean);
begin
  fShowIntegrateJoystick := Value;
  fCircle.Visible := fShowIntegrateJoystick;
  fCircle2.Visible := fShowIntegrateJoystick;
end;

destructor TGBEJoystick.Destroy;
begin
  DoDeleteChildren;
  inherited;
end;

function TGBEJoystick.OrientationKeyManagement(rightKey, leftKey, upKey,
  downKey, goUp, goDown, sideWayRight, sideWayLeft: Boolean;
  sensibility, speed, maxspeed: Single): Single;
begin
  if assigned(PlayerPosition) then
  begin
    if rightKey then
      PlayerPosition.RotationAngle.Y := PlayerPosition.RotationAngle.Y +
        sensibility;
    if leftKey then
      PlayerPosition.RotationAngle.Y := PlayerPosition.RotationAngle.Y -
        sensibility;
    if goUp then
      PlayerPosition.getDummyOrientation.RotationAngle.X :=
        PlayerPosition.getDummyOrientation.RotationAngle.X + sensibility;
    if goDown then
      PlayerPosition.getDummyOrientation.RotationAngle.X :=
        PlayerPosition.getDummyOrientation.RotationAngle.X - sensibility;

    if upKey or sideWayRight or sideWayLeft then
    begin
      if speed > -maxspeed then
        speed := speed - sensibility
      else
        speed := -maxspeed;
    end;
    if downKey then
    begin
      if speed < maxspeed then
        speed := speed + sensibility
      else
        speed := maxspeed;
    end;
  end;
  result := speed;
end;

// ----------------------------------------------------------------------

procedure Register;
begin
  RegisterComponents('GXScene GBE', [TGBEJoystick]);
end;

end.
