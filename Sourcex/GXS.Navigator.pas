//
// The graphics engine GXScene https://github.com/glscene
//
unit GXS.Navigator;

(* Unit for navigating TgxBaseObjects *)

interface

{$I GXS.Scene.inc}

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,

  GXS.VectorGeometry,
  GXS.Scene,
  GXS.Coordinates,
  GXS.Screen,
  GXS.VectorTypes;

type

	(* TgxNavigator is the component for moving a TgxBaseSceneObject, and all Classes based on it,
     this includes all the objects from the Scene Editor.
	   The four calls to get you started is
	    TurnHorisontal : it turns left and right.
	    TurnVertical : it turns up and down.
	    MoveForward :	moves back and forth.
      FlyForward : moves back and forth in the movingobject's direction

  	 The three properties to get you started is
	    MovingObject : The Object that you are moving.
	    UseVirtualUp : When UseVirtualUp is set you navigate Quake style. If it isn't
   		it's more like Descent.
	    AngleLock : Allows you to block the Vertical angles. Should only be used in
			conjunction with UseVirtualUp.
	    MoveUpWhenMovingForward : Changes movement from Quake to Arcade Airplane...
      (no tilt and flying)
	    InvertHorizontalSteeringWhenUpsideDown : When using virtual up, and vertically
      rotating beyond 90 degrees, will make steering seem inverted, so we "invert" back
      to normal.  *)
  TgxNavigator = class(TComponent)
  private
    FObject: TgxBaseSceneObject;
    FVirtualRight: TVector4f;
    FVirtualUp: TgxCoordinates;
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
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetObject(NewObject: TgxBaseSceneObject); virtual;
    procedure SetUseVirtualUp(UseIt: boolean);
    procedure SetVirtualUp(Up: TgxCoordinates);
    function CalcRight: TVector4f;
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
    property MoveUpWhenMovingForward: boolean read FMoveUpWhenMovingForward write FMoveUpWhenMovingForward default False;
    property InvertHorizontalSteeringWhenUpsideDown: boolean read FInvertHorizontalSteeringWhenUpsideDown write FInvertHorizontalSteeringWhenUpsideDown default False;
    property VirtualUp: TgxCoordinates read FVirtualUp write SetVirtualUp;
    property MovingObject: TgxBaseSceneObject read FObject write SetObject;
    property UseVirtualUp: boolean read FUseVirtualUp write SetUseVirtualUp default False;
    property AutoUpdateObject: boolean read FAutoUpdateObject write FAutoUpdateObject default False;
    property MaxAngle: single read FMaxAngle write FMaxAngle;
    property MinAngle: single read FMinAngle write FMinAngle;
    property AngleLock: boolean read FAngleLock write FAngleLock default False;
  end;

	(* TgxUserInterface is the component which reads the userinput and transform it into action.
 	   The four calls to get you started is
 	    MouseLookActivate : set us up the bomb.
 	    MouseLookDeActivate : defuses it.
	    Mouselook(deltaTime: double) : handles mouse look... Should be called in the Cadencer event. (Though it works every where!)
	    MouseUpdate : Resets mouse position so that you don't notice that the mouse is limited to the screen should be called after Mouselook.
 	   The four properties to get you started are:
 	    InvertMouse     : Inverts the mouse Y axis.
	    MouseSpeed      : Also known as mouse sensitivity.
	    GXNavigator     : The Navigator which receives the user movement.
	    GXVertNavigator : The Navigator which if set receives the vertical user movement. Used mostly for cameras....   *)
  TgxUserInterface = class(TComponent)
  private
    FPrevPoint: TPoint;
    midScreenX, midScreenY: integer;
    FMouseActive: boolean;
    FMouseSpeed: single;
    FGLNavigator: TgxNavigator;
    FGLVertNavigator: TgxNavigator;
    FInvertMouse: boolean;
    procedure MouseInitialize;
    procedure SetMouseLookActive(const val: boolean);
    procedure setNavigator(val: TgxNavigator);
    procedure setVertNavigator(val: TgxNavigator);
  protected
    procedure Notification(AComponent: TComponent; operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseUpdate;
    function MouseLook : Boolean;
    procedure MouseLookActiveToggle;
    procedure MouseLookActivate;
    procedure MouseLookDeactivate;
    function IsMouseLookOn: Boolean;
    procedure TurnHorizontal(Angle : Double);
    procedure TurnVertical(Angle : Double);
    property MouseLookActive : Boolean read FMouseActive write SetMouseLookActive;
  published
    property InvertMouse: boolean read FInvertMouse write FInvertMouse default False;
    property MouseSpeed: single read FMouseSpeed write FMouseSpeed;
    property GLNavigator: TgxNavigator read FGLNavigator write setNavigator;
    property GLVertNavigator: TgxNavigator read FGLVertNavigator write setVertNavigator;
  end;

//-------------------------------------------------------------------------
implementation
//-------------------------------------------------------------------------

constructor TgxNavigator.Create(AOwner : TComponent);
Begin
  inherited;
  FVirtualUp := TgxCoordinates.CreateInitialized(Self, ZHmgVector, csPoint);
  FCurrentVAngle := 0;
  FCurrentHAngle := 0;
End;

destructor  TgxNavigator.Destroy;

Begin
  FVirtualUp.Free;
  inherited;
End;


procedure   TgxNavigator.SetObject(NewObject : TgxBaseSceneObject);
Begin
  If FObject <> NewObject then
  Begin
    If Assigned(FObject) then
      FObject.RemoveFreeNotification(Self);

    FObject := NewObject;
    If Assigned(FObject) then
    Begin
      if csdesigning in componentstate then
      Begin
        If VectorLength(FVirtualUp.AsVector) = 0 then
        Begin
          FVirtualUp.AsVector := FObject.Up.AsVector;
        End;
        Exit;
      End;

      If FUseVirtualUp Then FVirtualRight := CalcRight;

      FObject.FreeNotification(Self);
    End;
  End;
End;

procedure   TgxNavigator.Notification(AComponent: TComponent; Operation: TOperation);

begin
  If Operation = opRemove then
  If AComponent = FObject then
    MovingObject := Nil;
  inherited;
end;

function    TgxNavigator.CalcRight : TVector4f;

begin
  If Assigned(FObject) then
  If FUseVirtualUp Then
  Begin
    VectorCrossProduct(FObject.Direction.AsVector, FVirtualUp.AsVector, Result);
    ScaleVector(Result,1/VectorLength(Result));
  End else VectorCrossProduct(FObject.Direction.AsVector, FObject.Up.AsVector, Result); { automaticly length(1), if not this is a bug }
end;

procedure   TgxNavigator.TurnHorizontal(Angle : Single);
var
  T : TVector4f;
  U : TAffineVector;
  TempVal : Single;


Begin
  If InvertHorizontalSteeringWhenUpsideDown and ((CurrentVAngle < -90) or (CurrentVAngle > 90)) then
    Angle := -Angle;

  FCurrentHAngle:=(FCurrentHAngle-Angle);

  If (FCurrentHAngle < 0) or (FCurrentHAngle > 360) then
  Begin
    TempVal := (FCurrentHAngle)/360;
    FCurrentHAngle :=  (TempVal-Floor(TempVal))*360;
  End;

  Angle := DegToRadian(Angle); {make it ready for Cos and Sin }
  If FUseVirtualUp Then
  Begin
    SetVector(U, VirtualUp.AsVector);
    T := FObject.Up.AsVector;
    RotateVector(T,U,Angle);
    FObject.Up.AsVector := T;

    T := FObject.Direction.AsVector;
    RotateVector(T,U,Angle);
    FObject.Direction.AsVector := T;
  End else FObject.Direction.AsVector := VectorCombine(FObject.Direction.AsVector,CalcRight,Cos(Angle),Sin(Angle));
End;

procedure   TgxNavigator.TurnVertical(Angle : Single);
var
  ExpectedAngle : Single;
  CosAngle, SinAngle : Single;
  TempVal : Single;
  Direction : TVector4f;

begin
  ExpectedAngle := FCurrentVAngle+Angle;
  If FAngleLock then
  Begin
    If ExpectedAngle > FMaxAngle then
    Begin
      If FCurrentVAngle = FMaxAngle then Exit;
      Angle := FMaxAngle-FCurrentVAngle;
      ExpectedAngle := FMaxAngle;
    End else
    Begin
      If ExpectedAngle < FMinAngle then
      Begin
        If FCurrentVAngle = FMinAngle then Exit;
        Angle := FMinAngle-FCurrentVAngle;
        ExpectedAngle := FMinAngle;
      End;
    End;
  End;
  FCurrentVAngle := ExpectedAngle;

  If (FCurrentVAngle < -180) or (FCurrentVAngle > 180) then
  Begin
    TempVal := (FCurrentVAngle+180)/360;
    FCurrentVAngle := (TempVal-Floor(TempVal))*360-180;
  End;

  Angle := DegToRadian(Angle); {make it ready for Cos and Sin }
  SinCosine(Angle,SinAngle,CosAngle);
  Direction := VectorCombine(MovingObject.Direction.AsVector,MovingObject.Up.AsVector,CosAngle,SinAngle);
  MovingObject.Up.AsVector := VectorCombine(MovingObject.Direction.AsVector,MovingObject.Up.AsVector,SinAngle,CosAngle);
  MovingObject.Direction.AsVector := Direction;
end;

procedure   TgxNavigator.MoveForward(Distance : Single);
Begin
  If (FUseVirtualUp and (not MoveUpWhenMovingForward)) Then
  Begin
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,VectorCrossProduct(FVirtualUp.AsVector,CalcRight),1,Distance);
  End else FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,FObject.Direction.AsVector,1,Distance);
End;

Procedure   TgxNavigator.StrafeHorizontal(Distance : Single);
Begin
  FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,CalcRight,1,Distance);
End;

Procedure   TgxNavigator.StrafeVertical(Distance : Single);
Begin
  If UseVirtualUp Then
  Begin
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,FVirtualUp.AsVector,1,Distance);
  End else FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,FObject.Up.AsVector,1,Distance);
End;

procedure TgxNavigator.FlyForward(Distance: single);
begin
  FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector, FObject.Direction.AsVector, 1, Distance);
end;

Procedure TgxNavigator.Straighten;

Var
  R : TVector4f;
  D : TVector4f;
  A : Single;

Begin
  FCurrentVAngle     := 0;
  FCurrentHAngle     := 0;

  R := CalcRight;
  A := VectorAngleCosine(AffineVectorMake(MovingObject.Up.AsVector), AffineVectorMake(VirtualUp.AsVector));
  MovingObject.Up.AsVector := VirtualUp.AsVector;

  VectorCrossProduct(R, FVirtualUp.AsVector, D);

  If A >= 0 then
    ScaleVector(D,-1/VectorLength(D))
  else
    ScaleVector(D,1/VectorLength(D));

  MovingObject.Direction.AsVector := D;
End;

Procedure   TgxNavigator.SetUseVirtualUp(UseIt : Boolean);

Begin
  FUseVirtualUp := UseIt;
  if csdesigning in componentstate then Exit;
  If FUseVirtualUp then FVirtualRight := CalcRight;
End;


Procedure   TgxNavigator.SetVirtualUp(Up : TgxCoordinates);
begin
  FVirtualUp.Assign(Up);
  if csdesigning in componentstate then Exit;
  If FUseVirtualUp then FVirtualRight := CalcRight;
end;

procedure   TgxNavigator.LoadState(Stream : TStream);
var
  Vector : TAffineVector;
  B : ByteBool;
  S : Single;

begin
  Stream.Read(Vector,SizeOf(TAffineVector));
  FObject.Position.AsAffineVector := Vector;
  Stream.Read(Vector,SizeOf(TAffineVector));
  FObject.Direction.AsAffineVector := Vector;
  Stream.Read(Vector,SizeOf(TAffineVector));
  FObject.Up.AsAffineVector := Vector;
  Stream.Read(B,SizeOf(ByteBool));
  UseVirtualUp := B;
  Stream.Read(B,SizeOf(ByteBool));
  FAngleLock := B;
  Stream.Read(S,SizeOf(Single));
  FMaxAngle := S;
  Stream.Read(S,SizeOf(Single));
  FMinAngle := S;
  Stream.Read(S,SizeOf(Single));
  FCurrentVAngle := S;
  Stream.Read(S,SizeOf(Single));
  FCurrentHAngle := S;
end;

procedure   TgxNavigator.SaveState(Stream : TStream);
var
  Vector : TAffineVector;
  B : ByteBool;
  S : Single;

begin
  Vector := FObject.Position.AsAffineVector;
  Stream.Write(Vector,SizeOf(TAffineVector));
  Vector := FObject.Direction.AsAffineVector;
  Stream.Write(Vector,SizeOf(TAffineVector));
  Vector := FObject.Up.AsAffineVector;
  Stream.Write(Vector,SizeOf(TAffineVector));
  B := UseVirtualUp;
  Stream.Write(B,SizeOf(ByteBool));
  B := FAngleLock;
  Stream.Write(B,SizeOf(ByteBool));
  S := FMaxAngle;
  Stream.Write(S,SizeOf(Single));
  S := FMinAngle;
  Stream.Write(S,SizeOf(Single));
  S := FCurrentVAngle;
  Stream.Write(S,SizeOf(Single));
  S := FCurrentHAngle;
  Stream.Write(S,SizeOf(Single));
end;

function TgxUserInterface.IsMouseLookOn: Boolean;
begin
   Result:=FMouseActive;
end;

Procedure   TgxUserInterface.TurnHorizontal(Angle : Double);

Begin
  GLNavigator.TurnHorizontal(Angle);
End;

Procedure   TgxUserInterface.TurnVertical(Angle : Double);

Begin
  If Assigned(GLVertNavigator) then GLVertNavigator.TurnVertical(Angle)
  else GLNavigator.TurnVertical(Angle);
End;

procedure TgxUserInterface.MouseLookActiveToggle;
begin
   if FMouseActive then
      MouseLookDeactivate
   else MouseLookActivate;
end;

procedure TgxUserInterface.MouseLookActivate;
begin
   if not FMouseActive then begin
      FMouseActive := True;
      MouseInitialize;
      GLShowCursor(False);
   end;
end;

procedure TgxUserInterface.MouseLookDeactivate;
begin
   if FMouseActive then begin
      FMouseActive := False;
      GLShowCursor(True);
   end;
end;

procedure TgxUserInterface.MouseInitialize;
begin
   midScreenX:=GLGetScreenWidth div 2;
   midScreenY:=GLGetScreenHeight div 2;

   FPrevPoint.x:=midScreenX; FPrevPoint.Y:=midScreenY;
   GLSetCursorPos(midScreenX, midScreenY);
end;

// SetMouseLookActive
//
procedure TgxUserInterface.SetMouseLookActive(const val : Boolean);
begin
   if val<>FMouseActive then
      if val then
         MouseLookActivate
      else MouseLookDeactivate;
end;

procedure TgxUserInterface.MouseUpdate;
begin
   if FMouseActive then
     GLGetCursorPos(FPrevPoint);
end;

function  TgxUserInterface.Mouselook : Boolean;
var
   deltaX, deltaY : Single;
begin
   Result := False;
   if not FMouseActive then exit;

   deltax:=(FPrevPoint.x-midscreenX)*mousespeed;
   deltay:=-(FPrevPoint.y-midscreenY)*mousespeed;
   If InvertMouse then deltay:=-deltay;

   if deltax <> 0 then begin
     TurnHorizontal(deltax*0.01);
     result := True;
   end;
   if deltay <> 0 then begin
     TurnVertical(deltay*0.01);
     result := True;
   end;

   if (FPrevPoint.x <> midScreenX) or (FPrevPoint.y <> midScreenY) then
      GLSetCursorPos(midScreenX, midScreenY);
end;

Constructor TgxUserInterface.Create(AOwner : TComponent);
Begin
  inherited;
  FMouseSpeed :=0;
  FMouseActive:=False;
  midScreenX:=GLGetScreenWidth div 2;
  midScreenY:=GLGetScreenHeight div 2;
  FPrevPoint.x:=midScreenX; FPrevPoint.Y:=midScreenY;
End;

Destructor  TgxUserInterface.Destroy;

begin
  if FMouseActive then MouseLookDeactivate; // added by JAJ
  inherited;
end;

procedure TgxUserInterface.Notification(AComponent: TComponent; operation:
    TOperation);
begin
     if operation = opRemove then begin
          if AComponent = FGLNavigator then
               setNavigator(nil);
          if AComponent = FGLVertNavigator then
               setVertNavigator(nil);
     end;
     inherited;
end;

procedure TgxUserInterface.setNavigator(val: TgxNavigator);
begin
     if assigned(FGLNavigator) then FGLNavigator.RemoveFreeNotification(self);
     FGLNavigator:= val;
     if assigned(val) then val.FreeNotification(self);
end;

procedure TgxUserInterface.setVertNavigator(val: TgxNavigator);
begin
     if assigned(FGLVertNavigator) then FGLVertNavigator.RemoveFreeNotification(self);
     FGLVertNavigator:= val;
     if assigned(val) then val.FreeNotification(self);
end;

end.
