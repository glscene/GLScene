//
// The graphics rendering engine GLScene http://glscene.org
//

unit GLS.Navigator;

(* Unit for navigating GLBaseObjects and GLSceneViewer. *)

interface

{$I GLScene.inc}

uses
  Winapi.Windows,
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Math,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,

  GLS.Scene,
  GLS.SceneViewer,
  GLS.Objects,
  GLS.GeomObjects,
  GLS.Context,
  GLS.BaseClasses,
  GLS.PersistentClasses,
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.Keyboard,
  GLS.HudObjects,
  GLS.Coordinates,
  GLS.Screen,
  GLS.Material,
  GLS.Texture,
  GLS.TextureFormat,
  GLS.RenderContextInfo;

type
  (* TGLNavigator is the component for moving a TGLBaseSceneObject, and all Classes based on it,
    this includes all the objects from the Scene Editor.
    The four calls to get you started is
    TurnHorisontal : it turns left and right.
    TurnVertical : it turns up and down.
    MoveForward :	moves back and forth.
    FlyForward : moves back and forth in the movingobject's direction
    The three properties to get you started is
    MovingObject : The Object that you are moving.
    UseVirtualUp : When UseVirtualUp is set you navigate Quake style. If it isn't	more like Descent.
    AngleLock : Allows you to block the Vertical angles.
    Should only be used in	conjunction with UseVirtualUp.
    MoveUpWhenMovingForward : Changes movement from Quake to Arcade Airplane...(no tilt and flying)
    InvertHorizontalSteeringWhenUpsideDown : When using virtual up, and vertically
    rotating beyond 90 degrees, will make steering seem inverted, so we "invert" back to normal *)
  TGLNavigator = class(TComponent)
  private
    FObject: TGLBaseSceneObject;
    FVirtualRight: TGLVector;
    FVirtualUp: TGLCoordinates;
    FUseVirtualUp: boolean;
    FAutoUpdateObject: boolean;
    FMaxAngle: single;
    FMinAngle: single;
    FCurrentVAngle: single;
    FCurrentHAngle: single;
    FAngleLock: boolean;
    FMoveUpWhenMovingForward: boolean;
    FInvertHorizontalSteeringWhenUpsideDown: boolean;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure SetObject(NewObject: TGLBaseSceneObject); virtual;
    procedure SetUseVirtualUp(UseIt: boolean);
    procedure SetVirtualUp(Up: TGLCoordinates);
    function CalcRight: TGLVector;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure TurnHorizontal(Angle: single);
    procedure TurnVertical(Angle: single);
    procedure MoveForward(Distance: single);
    procedure StrafeHorizontal(Distance: single);
    procedure StrafeVertical(Distance: single);
    procedure Straighten;
    procedure FlyForward(Distance: single);
    procedure LoadState(Stream: TStream);
    procedure SaveState(Stream: TStream);
    property CurrentVAngle: single read FCurrentVAngle;
    property CurrentHAngle: single read FCurrentHAngle;
  published
    property MoveUpWhenMovingForward: boolean read FMoveUpWhenMovingForward
      write FMoveUpWhenMovingForward default False;
    property InvertHorizontalSteeringWhenUpsideDown: boolean
      read FInvertHorizontalSteeringWhenUpsideDown
      write FInvertHorizontalSteeringWhenUpsideDown default False;
    property VirtualUp: TGLCoordinates read FVirtualUp write SetVirtualUp;
    property MovingObject: TGLBaseSceneObject read FObject write SetObject;
    property UseVirtualUp: boolean read FUseVirtualUp write SetUseVirtualUp
      default False;
    property AutoUpdateObject: boolean read FAutoUpdateObject
      write FAutoUpdateObject default False;
    property MaxAngle: single read FMaxAngle write FMaxAngle;
    property MinAngle: single read FMinAngle write FMinAngle;
    property AngleLock: boolean read FAngleLock write FAngleLock default False;
  end;

  (* TGLUserInterface is the component which reads the user input and transform it into action.
    The four calls to get you started is
    MouseLookActivate : set us up the bomb.
    MouseLookDeActivate : defuses it.
    Mouselook(deltaTime: double) : handles mouse look... Should be called in the Cadencer event. (Though it works every where!)
    MouseUpdate : Resets mouse position so that you don't notice that the mouse is limited to the screen should be called after Mouselook.
    The four properties to get you started are:
    InvertMouse     : Inverts the mouse Y axis.
    MouseSpeed      : Also known as mouse sensitivity.
    GLNavigator     : The Navigator which receives the user movement.
    GLVertNavigator : The Navigator which if set receives the vertical user movement. Used mostly for cameras.... *)
  TGLUserInterface = class(TComponent)
  private
    FPrevPoint: TPoint;
    midScreenX, midScreenY: integer;
    FMouseActive: boolean;
    FMouseSpeed: single;
    FGLNavigator: TGLNavigator;
    FGLVertNavigator: TGLNavigator;
    FInvertMouse: boolean;
    procedure MouseInitialize;
    procedure SetMouseLookActive(const val: boolean);
    procedure setNavigator(val: TGLNavigator);
    procedure setVertNavigator(val: TGLNavigator);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseUpdate;
    function MouseLook: boolean;
    procedure MouseLookActiveToggle;
    procedure MouseLookActivate;
    procedure MouseLookDeactivate;
    function IsMouseLookOn: boolean;
    procedure TurnHorizontal(Angle: Double);
    procedure TurnVertical(Angle: Double);
    property MouseLookActive: boolean read FMouseActive
      write SetMouseLookActive;
  published
    property InvertMouse: boolean read FInvertMouse write FInvertMouse
      default False;
    property MouseSpeed: single read FMouseSpeed write FMouseSpeed;
    property GLNavigator: TGLNavigator read FGLNavigator write setNavigator;
    property GLVertNavigator: TGLNavigator read FGLVertNavigator
      write setVertNavigator;
  end;

  TGLNaviCube = class(TGLBaseSceneObject)
  private
    FDelta, FFps, FTimer, FInactiveTime: single;
    FCube: TGLDummyCube;
    FSel: integer;
    FSelPos: TGLVector;
    FCam, FNaviCam: TGLCamera;
    FHud: TGLHUDSprite;
    FMem: TGLMemoryViewer;
    FViewer: TGLSceneViewer;
    FReady, FMouse: boolean;
    FMouseRotation: boolean;
    FMousePos: TPoint;
    FPosAnimationStart: TGLVector;
    FPosAnimationEnd: TGLVector;
  public
    constructor CreateAsChild(aParentOwner: TGLBaseSceneObject); reintroduce;
    procedure DoProgress(const pt: TGLProgressTimes); override;
    procedure DoRender(var ARci: TGLRenderContextInfo;
      ARenderSelf, ARenderChildren: boolean); override;
    property SceneViewer: TGLSceneViewer read FViewer write FViewer;
    property Camera: TGLCamera read FCam write FCam;
    property FPS: single read FFps write FFps;
    property ActiveMouse: boolean read FMouse write FMouse;
    property InactiveTime: single read FInactiveTime write FInactiveTime;
  end;

var
  sW2, sH2: integer;

// -------------------------------------------------------------
implementation
// -------------------------------------------------------------

constructor TGLNavigator.Create(AOwner: TComponent);
begin
  inherited;
  FVirtualUp := TGLCoordinates.CreateInitialized(Self, ZHmgVector, csPoint);
  FCurrentVAngle := 0;
  FCurrentHAngle := 0;
end;

destructor TGLNavigator.Destroy;

begin
  FVirtualUp.Free;
  inherited;
end;

procedure TGLNavigator.SetObject(NewObject: TGLBaseSceneObject);
begin
  If FObject <> NewObject then
  begin
    If Assigned(FObject) then
      FObject.RemoveFreeNotification(Self);

    FObject := NewObject;
    If Assigned(FObject) then
    begin
      if csdesigning in componentstate then
      begin
        If VectorLength(FVirtualUp.AsVector) = 0 then
        begin
          FVirtualUp.AsVector := FObject.Up.AsVector;
        end;
        Exit;
      end;

      If FUseVirtualUp Then
        FVirtualRight := CalcRight;

      FObject.FreeNotification(Self);
    end;
  end;
end;

procedure TGLNavigator.Notification(AComponent: TComponent;
  Operation: TOperation);

begin
  If Operation = opRemove then
    If AComponent = FObject then
      MovingObject := Nil;
  inherited;
end;

Function TGLNavigator.CalcRight: TGLVector;

begin
  If Assigned(FObject) then
    If FUseVirtualUp Then
    begin
      VectorCrossProduct(FObject.Direction.AsVector,
        FVirtualUp.AsVector, Result);
      ScaleVector(Result, 1 / VectorLength(Result));
    End
    else
      VectorCrossProduct(FObject.Direction.AsVector, FObject.Up.AsVector,
        Result); { automaticly length(1), if not this is a bug }
end;

procedure TGLNavigator.TurnHorizontal(Angle: single);

Var
  T: TGLVector;
  U: TAffineVector;
  TempVal: single;

begin
  If InvertHorizontalSteeringWhenUpsideDown and
    ((CurrentVAngle < -90) or (CurrentVAngle > 90)) then
    Angle := -Angle;

  FCurrentHAngle := (FCurrentHAngle - Angle);

  If (FCurrentHAngle < 0) or (FCurrentHAngle > 360) then
  begin
    TempVal := (FCurrentHAngle) / 360;
    FCurrentHAngle := (TempVal - Floor(TempVal)) * 360;
  end;

  Angle := DegToRadian(Angle); { make it ready for Cos and Sin }
  If FUseVirtualUp Then
  begin
    SetVector(U, VirtualUp.AsVector);
    T := FObject.Up.AsVector;
    RotateVector(T, U, Angle);
    FObject.Up.AsVector := T;

    T := FObject.Direction.AsVector;
    RotateVector(T, U, Angle);
    FObject.Direction.AsVector := T;
  End
  else
    FObject.Direction.AsVector := VectorCombine(FObject.Direction.AsVector,
      CalcRight, Cos(Angle), Sin(Angle));
end;

procedure TGLNavigator.TurnVertical(Angle: single);

Var
  ExpectedAngle: single;
  CosAngle, SinAngle: single;
  TempVal: single;
  Direction: TGLVector;

begin
  ExpectedAngle := FCurrentVAngle + Angle;
  If FAngleLock then
  begin
    If ExpectedAngle > FMaxAngle then
    begin
      If FCurrentVAngle = FMaxAngle then
        Exit;
      Angle := FMaxAngle - FCurrentVAngle;
      ExpectedAngle := FMaxAngle;
    End
    else
    begin
      If ExpectedAngle < FMinAngle then
      begin
        If FCurrentVAngle = FMinAngle then
          Exit;
        Angle := FMinAngle - FCurrentVAngle;
        ExpectedAngle := FMinAngle;
      end;
    end;
  end;
  FCurrentVAngle := ExpectedAngle;

  If (FCurrentVAngle < -180) or (FCurrentVAngle > 180) then
  begin
    TempVal := (FCurrentVAngle + 180) / 360;
    FCurrentVAngle := (TempVal - Floor(TempVal)) * 360 - 180;
  end;

  Angle := DegToRadian(Angle); { make it ready for Cos and Sin }
  SinCosine(Angle, SinAngle, CosAngle);
  Direction := VectorCombine(MovingObject.Direction.AsVector,
    MovingObject.Up.AsVector, CosAngle, SinAngle);
  MovingObject.Up.AsVector := VectorCombine(MovingObject.Direction.AsVector,
    MovingObject.Up.AsVector, SinAngle, CosAngle);
  MovingObject.Direction.AsVector := Direction;
end;

procedure TGLNavigator.MoveForward(Distance: single);
begin
  If (FUseVirtualUp and (not MoveUpWhenMovingForward)) Then
  begin
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
      VectorCrossProduct(FVirtualUp.AsVector, CalcRight), 1, Distance);
  End
  else
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
      FObject.Direction.AsVector, 1, Distance);
end;

procedure TGLNavigator.StrafeHorizontal(Distance: single);
begin
  FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
    CalcRight, 1, Distance);
end;

procedure TGLNavigator.StrafeVertical(Distance: single);
begin
  If UseVirtualUp Then
  begin
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
      FVirtualUp.AsVector, 1, Distance);
  End
  else
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
      FObject.Up.AsVector, 1, Distance);
end;

procedure TGLNavigator.FlyForward(Distance: single);
begin
  FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,
    FObject.Direction.AsVector, 1, Distance);
end;

procedure TGLNavigator.Straighten;

Var
  R: TGLVector;
  D: TGLVector;
  A: single;

begin
  FCurrentVAngle := 0;
  FCurrentHAngle := 0;

  R := CalcRight;
  A := VectorAngleCosine(AffineVectorMake(MovingObject.Up.AsVector),
    AffineVectorMake(VirtualUp.AsVector));
  MovingObject.Up.AsVector := VirtualUp.AsVector;

  VectorCrossProduct(R, FVirtualUp.AsVector, D);

  If A >= 0 then
    ScaleVector(D, -1 / VectorLength(D))
  else
    ScaleVector(D, 1 / VectorLength(D));

  MovingObject.Direction.AsVector := D;
end;

procedure TGLNavigator.SetUseVirtualUp(UseIt: boolean);

begin
  FUseVirtualUp := UseIt;
  if csdesigning in componentstate then
    Exit;
  If FUseVirtualUp then
    FVirtualRight := CalcRight;
end;

procedure TGLNavigator.SetVirtualUp(Up: TGLCoordinates);
begin
  FVirtualUp.Assign(Up);
  if csdesigning in componentstate then
    Exit;
  If FUseVirtualUp then
    FVirtualRight := CalcRight;
end;

procedure TGLNavigator.LoadState(Stream: TStream);

Var
  Vector: TAffineVector;
  B: ByteBool;
  S: single;

begin
  Stream.Read(Vector, SizeOf(TAffineVector));
  FObject.Position.AsAffineVector := Vector;
  Stream.Read(Vector, SizeOf(TAffineVector));
  FObject.Direction.AsAffineVector := Vector;
  Stream.Read(Vector, SizeOf(TAffineVector));
  FObject.Up.AsAffineVector := Vector;
  Stream.Read(B, SizeOf(ByteBool));
  UseVirtualUp := B;
  Stream.Read(B, SizeOf(ByteBool));
  FAngleLock := B;
  Stream.Read(S, SizeOf(single));
  FMaxAngle := S;
  Stream.Read(S, SizeOf(single));
  FMinAngle := S;
  Stream.Read(S, SizeOf(single));
  FCurrentVAngle := S;
  Stream.Read(S, SizeOf(single));
  FCurrentHAngle := S;
end;

procedure TGLNavigator.SaveState(Stream: TStream);

Var
  Vector: TAffineVector;
  B: ByteBool;
  S: single;

begin
  Vector := FObject.Position.AsAffineVector;
  Stream.Write(Vector, SizeOf(TAffineVector));
  Vector := FObject.Direction.AsAffineVector;
  Stream.Write(Vector, SizeOf(TAffineVector));
  Vector := FObject.Up.AsAffineVector;
  Stream.Write(Vector, SizeOf(TAffineVector));
  B := UseVirtualUp;
  Stream.Write(B, SizeOf(ByteBool));
  B := FAngleLock;
  Stream.Write(B, SizeOf(ByteBool));
  S := FMaxAngle;
  Stream.Write(S, SizeOf(single));
  S := FMinAngle;
  Stream.Write(S, SizeOf(single));
  S := FCurrentVAngle;
  Stream.Write(S, SizeOf(single));
  S := FCurrentHAngle;
  Stream.Write(S, SizeOf(single));
end;

function TGLUserInterface.IsMouseLookOn: boolean;
begin
  Result := FMouseActive;
end;

procedure TGLUserInterface.TurnHorizontal(Angle: Double);

begin
  GLNavigator.TurnHorizontal(Angle);
end;

procedure TGLUserInterface.TurnVertical(Angle: Double);

begin
  If Assigned(GLVertNavigator) then
    GLVertNavigator.TurnVertical(Angle)
  else
    GLNavigator.TurnVertical(Angle);
end;

procedure TGLUserInterface.MouseLookActiveToggle;
begin
  if FMouseActive then
    MouseLookDeactivate
  else
    MouseLookActivate;
end;

procedure TGLUserInterface.MouseLookActivate;
begin
  if not FMouseActive then
  begin
    FMouseActive := True;
    MouseInitialize;
    GLShowCursor(False);
  end;
end;

procedure TGLUserInterface.MouseLookDeactivate;
begin
  if FMouseActive then
  begin
    FMouseActive := False;
    GLShowCursor(True);
  end;
end;

procedure TGLUserInterface.MouseInitialize;
begin
  midScreenX := GLGetScreenWidth div 2;
  midScreenY := GLGetScreenHeight div 2;

  FPrevPoint.x := midScreenX;
  FPrevPoint.Y := midScreenY;
  GLSetCursorPos(midScreenX, midScreenY);
end;

procedure TGLUserInterface.SetMouseLookActive(const val: boolean);
begin
  if val <> FMouseActive then
    if val then
      MouseLookActivate
    else
      MouseLookDeactivate;
end;

procedure TGLUserInterface.MouseUpdate;
begin
  if FMouseActive then
    GLGetCursorPos(FPrevPoint);
end;

function TGLUserInterface.MouseLook: boolean;
var
  deltaX, deltaY: single;
begin
  Result := False;
  if not FMouseActive then
    Exit;

  deltaX := (FPrevPoint.x - midScreenX) * MouseSpeed;
  deltaY := -(FPrevPoint.Y - midScreenY) * MouseSpeed;
  If InvertMouse then
    deltaY := -deltaY;

  if deltaX <> 0 then
  begin
    TurnHorizontal(deltaX * 0.01);
    Result := True;
  end;
  if deltaY <> 0 then
  begin
    TurnVertical(deltaY * 0.01);
    Result := True;
  end;

  if (FPrevPoint.x <> midScreenX) or (FPrevPoint.Y <> midScreenY) then
    GLSetCursorPos(midScreenX, midScreenY);
end;

constructor TGLUserInterface.Create(AOwner: TComponent);
begin
  inherited;
  FMouseSpeed := 0;
  FMouseActive := False;
  midScreenX := GLGetScreenWidth div 2;
  midScreenY := GLGetScreenHeight div 2;
  FPrevPoint.x := midScreenX;
  FPrevPoint.Y := midScreenY;
end;

destructor TGLUserInterface.Destroy;

begin
  if FMouseActive then
    MouseLookDeactivate; // added by JAJ
  inherited;
end;

procedure TGLUserInterface.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
  begin
    if AComponent = FGLNavigator then
      setNavigator(nil);
    if AComponent = FGLVertNavigator then
      setVertNavigator(nil);
  end;
  inherited;
end;

procedure TGLUserInterface.setNavigator(val: TGLNavigator);
begin
  if Assigned(FGLNavigator) then
    FGLNavigator.RemoveFreeNotification(Self);
  FGLNavigator := val;
  if Assigned(val) then
    val.FreeNotification(Self);
end;

procedure TGLUserInterface.setVertNavigator(val: TGLNavigator);
begin
  if Assigned(FGLVertNavigator) then
    FGLVertNavigator.RemoveFreeNotification(Self);
  FGLVertNavigator := val;
  if Assigned(val) then
    val.FreeNotification(Self);
end;

constructor TGLNaviCube.CreateAsChild(aParentOwner: TGLBaseSceneObject);

  procedure genTex(S: string; mat: TGLMaterial);
  var
    bmp: TBitmap;
  begin
    bmp := TBitmap.Create;
    bmp.Width := 64;
    bmp.Height := 64;
    with bmp.Canvas do
    begin
      Font.Name := 'Verdana';
      Font.Size := 10;
      TextOut(32 - TextWidth(S) div 2, 24, S);
    end;
    mat.FrontProperties.Diffuse.SetColor(1, 1, 1);
    mat.Texture.Image.Assign(bmp);
    mat.Texture.Disabled := False;
    mat.Texture.FilteringQuality := tfAnisotropic;
    mat.Texture.TextureMode := tmModulate;
    bmp.Free;
  end;

  procedure SetColor(m: TGLMaterial; c: single);
  begin
    m.FrontProperties.Diffuse.SetColor(c, c, 1);
  end;

  procedure addPlane(T: integer; ttl: string; c, x, Y, z, dx, dy, dz: single);
  begin
    with TGLPlane.CreateAsChild(FCube) do
    begin
      tag := T;
      tagfloat := c;
      Position.SetPoint(x, Y, z);
      Direction.SetVector(dx, dy, dz);
      genTex(ttl, Material);
    end;
  end;

  procedure addCube(T: integer; c, x, Y, z, sx, sy, sz: single);
  begin
    with TGLCube.CreateAsChild(FCube) do
    begin
      tag := T;
      tagfloat := c;
      Position.SetPoint(x, Y, z);
      Scale.SetVector(sx, sy, sz);
      SetColor(Material, c);
    end;
  end;

begin
  inherited CreateAsChild(aParentOwner);
  FDelta := 2;
  FFps := 30;
  FTimer := 10;
  FMouse := True;
  FInactiveTime := 0;

  FHud := TGLHUDSprite.CreateAsChild(Self);
  FHud.Width := 128;
  FHud.Height := 128;
  FHud.Material.BlendingMode := bmTransparency;
  with FHud.Material.Texture do
  begin
    Disabled := False;
    ImageClassName := 'TGLBlankImage';
    MinFilter := miNearest;
    TGLBlankImage(Image).Width := 128;
    TGLBlankImage(Image).Height := 128;
    TextureMode := tmReplace;
  end;
  FHud.Position.SetPoint(-200, 50, 0);

  FNaviCam := TGLCamera.CreateAsChild(aParentOwner);
  FNaviCam.FocalLength := 55;
  FNaviCam.TargetObject := Self;

  FMem := TGLMemoryViewer.Create(aParentOwner);
  FMem.Width := 128;
  FMem.Height := 128;
  FMem.Camera := FNaviCam;
  with FMem.Buffer do
  begin
    BackgroundAlpha := 0;
    Antialiasing := aa6x;
    ContextOptions := [roDestinationAlpha];
    Lighting := False;
  end;

  FCube := TGLDummyCube.CreateAsChild(Self);
  FCube.Visible := False;

  with TGLDisk.CreateAsChild(FCube) do
  begin
    Position.SetPoint(0, -0.805, 0);
    Direction.SetVector(0, 1, 0);
    InnerRadius := 0.9;
    OuterRadius := 1.3;
    Slices := 60;
    Loops := 1;
    SetColor(Material, 0.6);
  end;

  with TGLDisk.CreateAsChild(FCube) do
  begin
    Position.SetPoint(0, -0.8, 0);
    Direction.SetVector(0, 1, 0);
    InnerRadius := 0.95;
    OuterRadius := 1.25;
    Slices := 60;
    Loops := 1;
    SetColor(Material, 1);
  end;

  addPlane(0, 'FRONT', 1, 0, 0, 0.7, 0, 0, 1);
  addPlane(1, 'RIGHT', 1, 0.7, 0, 0, 1, 0, 0);
  addPlane(2, 'LEFT', 1, -0.7, 0, 0, -1, 0, 0);
  addPlane(3, 'BACK', 1, 0, 0, -0.7, 0, 0, -1);
  addPlane(4, 'TOP', 1, 0, 0.7, 0, 0, 1, 0);
  addPlane(5, 'BOTTOM', 1, 0, -0.7, 0, 0, -1, 0);

  addCube(6, 0.9, 0, 0.6, 0.6, 1, 0.2, 0.2);
  addCube(7, 0.9, 0, 0.6, -0.6, 1, 0.2, 0.2);
  addCube(8, 0.9, 0, -0.6, 0.6, 1, 0.2, 0.2);
  addCube(9, 0.9, 0, -0.6, -0.6, 1, 0.2, 0.2);

  addCube(10, 0.9, 0.6, 0.6, 0, 0.2, 0.2, 1);
  addCube(11, 0.9, 0.6, -0.6, 0, 0.2, 0.2, 1);
  addCube(12, 0.9, -0.6, 0.6, 0, 0.2, 0.2, 1);
  addCube(13, 0.9, -0.6, -0.6, 0, 0.2, 0.2, 1);

  addCube(14, 0.9, 0.6, 0, 0.6, 0.2, 1, 0.2);
  addCube(15, 0.9, 0.6, 0, -0.6, 0.2, 1, 0.2);
  addCube(16, 0.9, -0.6, 0, 0.6, 0.2, 1, 0.2);
  addCube(17, 0.9, -0.6, 0, -0.6, 0.2, 1, 0.2);

  addCube(18, 0.8, 0.6, 0.6, 0.6, 0.2, 0.2, 0.2);
  addCube(19, 0.8, 0.6, 0.6, -0.6, 0.2, 0.2, 0.2);
  addCube(20, 0.8, 0.6, -0.6, 0.6, 0.2, 0.2, 0.2);
  addCube(21, 0.8, -0.6, 0.6, 0.6, 0.2, 0.2, 0.2);
  addCube(22, 0.8, 0.6, -0.6, -0.6, 0.2, 0.2, 0.2);
  addCube(23, 0.8, -0.6, -0.6, 0.6, 0.2, 0.2, 0.2);
  addCube(24, 0.8, -0.6, 0.6, -0.6, 0.2, 0.2, 0.2);
  addCube(25, 0.8, -0.6, -0.6, -0.6, 0.2, 0.2, 0.2);

end;

procedure TGLNaviCube.DoProgress(const pt: TGLProgressTimes);
const
  tb: array [0 .. 1] of array [0 .. 3] of TGLVector = (((x: 0; Y: 20; z: 1;
    W: 0), (x: 1; Y: 20; z: 0; W: 0), (x: 0; Y: 20; z: - 1; W: 0), (x: - 1;
    Y: 20; z: 0; W: 0)), ((x: 0; Y: - 20; z: 1; W: 0), (x: 1; Y: - 20; z: 0;
    W: 0), (x: 0; Y: - 20; z: - 1; W: 0), (x: - 1; Y: - 20; z: 0; W: 0)));
var
  mp: TPoint;
  mover: boolean;
  i: integer;
  v0, v1, v2, v: TGLVector;
  obj: TGLBaseSceneObject;

  procedure moveTo(trgv: TGLVector);
  begin
    FPosAnimationStart := FCam.Position.AsVector;
    FPosAnimationEnd := FCam.TargetObject.AbsoluteToLocal
      (VectorScale(VectorNormalize(trgv), FCam.DistanceToTarget));
    FDelta := 0;
  end;

begin
  mp := FViewer.ScreenToClient(Mouse.CursorPos);
  mover := (mp.x > FHud.Position.x - 64) and (mp.x < FHud.Position.x + 64) and
    (mp.Y > FHud.Position.Y - 64) and (mp.Y < FHud.Position.Y + 64);
  // mouse Down/Up
  if FDelta > 1 then
  begin
    if IsKeyDown(VK_LBUTTON) and (not FMouseRotation) then
    begin
      // selection > start auto rotation
      if mover and (FSel >= 0) then
      begin

        v := FCam.AbsoluteVectorToTarget;
        v.Y := 0;
        if v.x < 0 then
          i := -1
        else
          i := 1;
        i := round((ArcCosine(VectorAngleCosine(v, ZHmgPoint)) * i + PI) / PI
          * 2) mod 4;
        if (FSel = 4) or (FSel = 5) then
          moveTo(tb[FSel - 4][i])
        else
          moveTo(FSelPos);
        FInactiveTime := 0;
      end // start manual rotation
      else if FMouse then
      begin
        FMouseRotation := True;
        FMousePos := Mouse.CursorPos;
        ShowCursor(False);
        Mouse.CursorPos := point(sW2, sH2);
        FInactiveTime := 0;
      end;
    end;
    // stop rotation, restore cursor
    if (not IsKeyDown(VK_LBUTTON)) and FMouseRotation and FMouse then
    begin
      ShowCursor(True);
      FMouseRotation := False;
      Mouse.CursorPos := FMousePos;

      FInactiveTime := 0;
    end;
  end
  // auto rotation progress
  else
  begin
    FDelta := FDelta + pt.deltaTime * 2;
    v := VectorLerp(FPosAnimationStart, FPosAnimationEnd,
      FDelta * FDelta * (3 - 2 * FDelta));
    v := VectorScale(VectorNormalize(v), VectorLength(FPosAnimationStart));
    if FDelta < 1 then
      FCam.Position.SetPoint(v)
    else
      FCam.Position.SetPoint(FPosAnimationEnd);

    v := VectorScale(VectorNormalize(v), 10);
    if FDelta < 1 then
      v := VectorScale(VectorNormalize(v), 10)
    else
      v := VectorScale(VectorNormalize(FPosAnimationEnd), 10);
    FNaviCam.Position.SetPoint(v);

    for i := 2 to FCube.Count - 1 do
      with TGLSceneObject(FCube.Children[i]) do
        Material.FrontProperties.Diffuse.SetColor(tagfloat, tagfloat, 1);
    FInactiveTime := 0;
  end;
  FSel := -1;
  // manual rotation progress
  if FMouseRotation and FMouse then
  begin

    mp := Mouse.CursorPos;
    if FCam <> nil then
      FCam.MoveAroundTarget((sH2 - mp.Y) * 0.2, (sW2 - mp.x) * 0.2);
    FNaviCam.MoveAroundTarget((sH2 - mp.Y) * 0.2, (sW2 - mp.x) * 0.2);
    Mouse.CursorPos := point(sW2, sH2);

    FInactiveTime := 0;
  end
  else if FReady then
  begin
    // selection
    if mover and (FDelta > 1) then
    begin
      v0 := FNaviCam.AbsolutePosition;
      v1 := FMem.Buffer.ScreenToVector(mp.x - round(FHud.Position.x) + 64,
        round(FHud.Position.Y) - mp.Y + 64);
      SetVector(v2, 99999, 99999, 99999);

      obj := nil;
      for i := 2 to FCube.Count - 1 do
        with TGLSceneObject(FCube.Children[i]) do
        begin
          Material.FrontProperties.Diffuse.SetColor(tagfloat, tagfloat, 1);
          if RayCastIntersect(v0, v1, @v) then
            if VectorDistance2(v2, v0) > VectorDistance2(v, v0) then
            begin
              SetVector(v2, v);
              FSel := FCube.Children[i].tag;
              FSelPos := FCube.Children[i].Position.AsVector;
              obj := FCube.Children[i];
            end;
        end;
      if FSel >= 0 then
      begin
        FViewer.cursor := -21;
        TGLSceneObject(obj).Material.FrontProperties.Diffuse.SetColor
          (1, 0.6, 0);
      end
      else
        FViewer.cursor := 0;
    end;
    v := VectorScale(VectorNormalize(FCam.AbsoluteVectorToTarget), 10);
    FNaviCam.Position.SetPoint(VectorNegate(v));
    FInactiveTime := FInactiveTime + pt.deltaTime;
  end;
  // rendering
  FTimer := FTimer + pt.deltaTime;
  if FTimer > 1 / FFps then
  begin
    FTimer := FTimer - Floor(FTimer * FFps) / FFps;
    FMem.Render(FCube);
    FMem.CopyToTexture(FHud.Material.Texture);
    FReady := True;
  end;
end;

procedure TGLNaviCube.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: boolean);
begin
  inherited;
  if (FCam = nil) and (Scene.CurrentGLCamera <> nil) then
  begin
    FCam := Scene.CurrentGLCamera;
    FNaviCam.Position.SetPoint
      (VectorScale(VectorNormalize(FCam.Position.AsVector), 10));
  end;
  if FViewer <> nil then
    FHud.Position.SetPoint(FViewer.Width - 80, 50, 0);
end;

// ------------------------------------------------
initialization

// ------------------------------------------------

sW2 := Screen.Width div 2;
sH2 := Screen.Height div 2;

end.

