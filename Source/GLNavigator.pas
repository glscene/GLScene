//
// This unit is part of the GLScene Engine, http://glscene.org
//

unit GLNavigator;

(*  Unit for navigating GLBaseObjects and GLSceneViewer. *)

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

  GLScene,
  GLObjects,
  GLGeomObjects,
  GLContext,
  GLBaseClasses,
  Scene.PersistentClasses,
  Scene.VectorGeometry,
  GLHUDObjects,
  GLCoordinates,
  GLScreen,
  GLKeyBoard,
  Scene.VectorTypes,
  GLMaterial,
  GLTexture,
  GLTextureFormat,
  GLSceneViewer,
  GLRenderContextInfo;

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
    FVirtualRight: TVector;
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
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetObject(NewObject: TGLBaseSceneObject); virtual;
    procedure SetUseVirtualUp(UseIt: boolean);
    procedure SetVirtualUp(Up: TGLCoordinates);
    function CalcRight: TVector;
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
    property VirtualUp: TGLCoordinates read FVirtualUp write SetVirtualUp;
    property MovingObject: TGLBaseSceneObject read FObject write SetObject;
    property UseVirtualUp: boolean read FUseVirtualUp write SetUseVirtualUp default False;
    property AutoUpdateObject: boolean read FAutoUpdateObject write FAutoUpdateObject default False;
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
    property GLNavigator: TGLNavigator read FGLNavigator write setNavigator;
    property GLVertNavigator: TGLNavigator read FGLVertNavigator write setVertNavigator;
  end;


  TGLNaviCube = class(TGLBaseSceneObject)
  private
    FDelta, FFps, FTimer, FInactiveTime: single;
    FCube: TGLDummyCube;
    FSel: Integer;
    FSelPos: TVector;
    FCam, FNaviCam: TGLCamera;
    FHud: TGLHUDSprite;
    FMem: TGLMemoryViewer;
    FViewer: TGLSceneViewer;
    FReady, FMouse: boolean;
    FMouseRotation: boolean;
    FMousePos: TPoint;
    FPosAnimationStart: TVector;
    FPosAnimationEnd: TVector;
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
  sW2, sH2: Integer;




//-------------------------------------------------------------  
implementation
//-------------------------------------------------------------  

constructor TGLNavigator.Create(AOwner : TComponent);
Begin
  inherited;
  FVirtualUp := TGLCoordinates.CreateInitialized(Self, ZHmgVector, csPoint);
  FCurrentVAngle := 0;
  FCurrentHAngle := 0;
end;

destructor  TGLNavigator.Destroy;

Begin
  FVirtualUp.Free;
  inherited;
End;


Procedure   TGLNavigator.SetObject(NewObject : TGLBaseSceneObject);
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

procedure   TGLNavigator.Notification(AComponent: TComponent; Operation: TOperation);

Begin
  If Operation = opRemove then
  If AComponent = FObject then
    MovingObject := Nil;

  inherited;
End;

Function    TGLNavigator.CalcRight : TVector;

Begin
  If Assigned(FObject) then
  If FUseVirtualUp Then
  Begin
    VectorCrossProduct(FObject.Direction.AsVector, FVirtualUp.AsVector, Result);
    ScaleVector(Result,1/VectorLength(Result));
  End else VectorCrossProduct(FObject.Direction.AsVector, FObject.Up.AsVector, Result); { automaticly length(1), if not this is a bug }
End;

procedure   TGLNavigator.TurnHorizontal(Angle : Single);

Var
  T : TVector;
  U : TAffineVector;
  TempVal : Single;


Begin
  If InvertHorizontalSteeringWhenUpsideDown and ((CurrentVAngle < -90) or (CurrentVAngle > 90)) then
    Angle := -Angle;

  FCurrentHAngle:=(FCurrentHAngle-Angle);

  If (FCurrentHAngle < 0) or (FCurrentHAngle > 360) then
  Begin
    TempVal := (FCurrentHAngle)/360;
    FCurrentHAngle :=  (TempVal - Floor(TempVal))*360;
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

Procedure   TGLNavigator.TurnVertical(Angle : Single);

Var
  ExpectedAngle : Single;
  CosAngle, SinAngle : Single;
  TempVal : Single;
  Direction : TVector;

Begin
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
End;

Procedure   TGLNavigator.MoveForward(Distance : Single);
Begin
  If (FUseVirtualUp and (not MoveUpWhenMovingForward)) Then
  Begin
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,VectorCrossProduct(FVirtualUp.AsVector,CalcRight),1,Distance);
  End else FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,FObject.Direction.AsVector,1,Distance);
End;

Procedure   TGLNavigator.StrafeHorizontal(Distance : Single);
Begin
  FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,CalcRight,1,Distance);
End;

Procedure   TGLNavigator.StrafeVertical(Distance : Single);
Begin
  If UseVirtualUp Then
  Begin
    FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,FVirtualUp.AsVector,1,Distance);
  End else FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector,FObject.Up.AsVector,1,Distance);
End;

procedure TGLNavigator.FlyForward(Distance: single);
begin
  FObject.Position.AsVector := VectorCombine(FObject.Position.AsVector, FObject.Direction.AsVector, 1, Distance);
end;

Procedure TGLNavigator.Straighten;

Var
  R : TVector;
  D : TVector;
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

Procedure   TGLNavigator.SetUseVirtualUp(UseIt : Boolean);

Begin
  FUseVirtualUp := UseIt;
  if csdesigning in componentstate then Exit;
  If FUseVirtualUp then FVirtualRight := CalcRight;
End;


Procedure   TGLNavigator.SetVirtualUp(Up : TGLCoordinates);
Begin
  FVirtualUp.Assign(Up);
  if csdesigning in componentstate then Exit;
  If FUseVirtualUp then FVirtualRight := CalcRight;
End;

Procedure   TGLNavigator.LoadState(Stream : TStream);

Var
  Vector : TAffineVector;
  B : ByteBool;
  S : Single;

Begin
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
End;

Procedure   TGLNavigator.SaveState(Stream : TStream);

Var
  Vector : TAffineVector;
  B : ByteBool;
  S : Single;

Begin
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
End;

function TGLUserInterface.IsMouseLookOn: Boolean;
begin
   Result:=FMouseActive;
end;

Procedure   TGLUserInterface.TurnHorizontal(Angle : Double);

Begin
  GLNavigator.TurnHorizontal(Angle);
End;

Procedure   TGLUserInterface.TurnVertical(Angle : Double);

Begin
  If Assigned(GLVertNavigator) then GLVertNavigator.TurnVertical(Angle)
  else GLNavigator.TurnVertical(Angle);
End;

procedure TGLUserInterface.MouseLookActiveToggle;
begin
   if FMouseActive then
      MouseLookDeactivate
   else MouseLookActivate;
end;

procedure TGLUserInterface.MouseLookActivate;
begin
   if not FMouseActive then begin
      FMouseActive := True;
      MouseInitialize;
      GLShowCursor(False);
   end;
end;

procedure TGLUserInterface.MouseLookDeactivate;
begin
   if FMouseActive then begin
      FMouseActive := False;
      GLShowCursor(True);
   end;
end;

procedure TGLUserInterface.MouseInitialize;
begin
   midScreenX:=GLGetScreenWidth div 2;
   midScreenY:=GLGetScreenHeight div 2;

   FPrevPoint.x:=midScreenX; FPrevPoint.Y:=midScreenY;
   GLSetCursorPos(midScreenX, midScreenY);
end;

procedure TGLUserInterface.SetMouseLookActive(const val : Boolean);
begin
   if val<>FMouseActive then
      if val then
         MouseLookActivate
      else MouseLookDeactivate;
end;

procedure TGLUserInterface.MouseUpdate;
begin
   if FMouseActive then
     GLGetCursorPos(FPrevPoint);
end;

function  TGLUserInterface.Mouselook : Boolean;
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

Constructor TGLUserInterface.Create(AOwner : TComponent);
Begin
  inherited;
  FMouseSpeed :=0;
  FMouseActive:=False;
  midScreenX:=GLGetScreenWidth div 2;
  midScreenY:=GLGetScreenHeight div 2;
  FPrevPoint.x:=midScreenX; FPrevPoint.Y:=midScreenY;
End;

Destructor  TGLUserInterface.Destroy;

Begin
  if FMouseActive then MouseLookDeactivate; // added by JAJ
  inherited;
End;

procedure TGLUserInterface.Notification(AComponent: TComponent; operation:
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

procedure TGLUserInterface.setNavigator(val: TGLNavigator);
begin
     if assigned(FGLNavigator) then FGLNavigator.RemoveFreeNotification(self);
     FGLNavigator:= val;
     if assigned(val) then val.FreeNotification(self);
end;

procedure TGLUserInterface.setVertNavigator(val: TGLNavigator);
begin
     if assigned(FGLVertNavigator) then FGLVertNavigator.RemoveFreeNotification(self);
     FGLVertNavigator:= val;
     if assigned(val) then val.FreeNotification(self);
end;

constructor TGLNaviCube.CreateAsChild(aParentOwner: TGLBaseSceneObject);

procedure genTex(s: string; mat: TGLMaterial);
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
    TextOut(32 - TextWidth(s) div 2, 24, s);
  end;
  mat.FrontProperties.Diffuse.SetColor(1, 1, 1);
  mat.Texture.Image.Assign(bmp);
  mat.Texture.Disabled := false;
  mat.Texture.FilteringQuality := tfAnisotropic;
  mat.Texture.TextureMode := tmModulate;
  bmp.Free;
end;

procedure SetColor(m: TGLMaterial; c: single);
begin
  m.FrontProperties.Diffuse.SetColor(c, c, 1);
end;

procedure addPlane(t: Integer; ttl: string; c, x, y, z, dx, dy, dz: single);
begin
  with TGLPlane.CreateAsChild(FCube) do
  begin
    tag := t;
    tagfloat := c;
    Position.SetPoint(x, y, z);
    Direction.SetVector(dx, dy, dz);
    genTex(ttl, Material);
  end;
end;

procedure addCube(t: Integer; c, x, y, z, sx, sy, sz: single);
begin
  with TGLCube.CreateAsChild(FCube) do
  begin
    tag := t;
    tagfloat := c;
    Position.SetPoint(x, y, z);
    Scale.SetVector(sx, sy, sz);
    SetColor(Material, c);
  end;
end;

begin
  inherited CreateAsChild(aParentOwner);
  FDelta := 2;
  FFps := 30;
  FTimer := 10;
  FMouse := true;
  FInactiveTime := 0;

  FHud := TGLHUDSprite.CreateAsChild(self);
  FHud.Width := 128;
  FHud.Height := 128;
  FHud.Material.BlendingMode := bmTransparency;
  with FHud.Material.Texture do
  begin
    Disabled := false;
    ImageClassName := 'TGLBlankImage';
    MinFilter := miNearest;
    TGLBlankImage(Image).Width := 128;
    TGLBlankImage(Image).Height := 128;
    TextureMode := tmReplace;
  end;
  FHud.Position.SetPoint(-200, 50, 0);

  FNaviCam := TGLCamera.CreateAsChild(aParentOwner);
  FNaviCam.FocalLength := 55;
  FNaviCam.TargetObject := self;

  FMem := TGLMemoryViewer.Create(aParentOwner);
  FMem.Width := 128;
  FMem.Height := 128;
  FMem.Camera := FNaviCam;
  with FMem.Buffer do
  begin
    BackgroundAlpha := 0;
    Antialiasing := aa6x;
    ContextOptions := [roDestinationAlpha];
    Lighting := false;
  end;

  FCube := TGLDummyCube.CreateAsChild(self);
  FCube.Visible := false;

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
  tb: array [0 .. 1] of array [0 .. 3] of TVector = (((x: 0; y: 20; z: 1;
    W: 0), (x: 1; y: 20; z: 0; W: 0), (x: 0; y: 20; z: - 1; W: 0), (x: - 1;
    y: 20; z: 0; W: 0)), ((x: 0; y: - 20; z: 1; W: 0), (x: 1; y: - 20; z: 0;
    W: 0), (x: 0; y: - 20; z: - 1; W: 0), (x: - 1; y: - 20; z: 0; W: 0)));
var
  mp: TPoint;
  mover: boolean;
  i: Integer;
  v0, v1, v2, v: TVector;
  obj: TGLBaseSceneObject;

procedure moveTo(trgv: TVector);
begin
  FPosAnimationStart := FCam.Position.AsVector;
  FPosAnimationEnd := FCam.TargetObject.AbsoluteToLocal
    (VectorScale(VectorNormalize(trgv), FCam.DistanceToTarget));
  FDelta := 0;
end;

begin
  mp := FViewer.ScreenToClient(Mouse.CursorPos);
  mover := (mp.x > FHud.Position.x - 64) and (mp.x < FHud.Position.x + 64) and
    (mp.y > FHud.Position.y - 64) and (mp.y < FHud.Position.y + 64);
  // mouse Down/Up
  if FDelta > 1 then
  begin
    if IsKeyDown(VK_LBUTTON) and (not FMouseRotation) then
    begin
      // selection > start auto rotation
      if mover and (FSel >= 0) then
      begin

        v := FCam.AbsoluteVectorToTarget;
        v.y := 0;
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
        FMouseRotation := true;
        FMousePos := Mouse.CursorPos;
        ShowCursor(false);
        Mouse.CursorPos := point(sW2, sH2);
        FInactiveTime := 0;
      end;
    end;
    // stop rotation, restore cursor
    if (not IsKeyDown(VK_LBUTTON)) and FMouseRotation and FMouse then
    begin
      ShowCursor(true);
      FMouseRotation := false;
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
      FCam.MoveAroundTarget((sH2 - mp.y) * 0.2, (sW2 - mp.x) * 0.2);
    FNaviCam.MoveAroundTarget((sH2 - mp.y) * 0.2, (sW2 - mp.x) * 0.2);
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
        round(FHud.Position.y) - mp.y + 64);
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
    FReady := true;
  end;
end;

procedure TGLNaviCube.DoRender(var ARci: TGLRenderContextInfo;
  ARenderSelf, ARenderChildren: boolean);
begin
  inherited;
  if (FCam = nil) and (scene.CurrentGLCamera <> nil) then
  begin
    FCam := scene.CurrentGLCamera;
    FNaviCam.Position.SetPoint
      (VectorScale(VectorNormalize(FCam.Position.AsVector), 10));
  end;
  if FViewer <> nil then
    FHud.Position.SetPoint(FViewer.Width - 80, 50, 0);
end;

//------------------------------------------------
initialization
//------------------------------------------------

  sW2 := Screen.Width div 2;
  sH2 := Screen.Height div 2;



end.
