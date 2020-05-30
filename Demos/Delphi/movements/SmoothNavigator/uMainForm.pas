unit uMainForm;

interface

uses
  Winapi.Windows,
  System.Classes, 
  System.SysUtils,
  Vcl.Graphics, 
  Vcl.Controls, 
  Vcl.Forms, 
  Vcl.ExtCtrls, 
  Vcl.StdCtrls,

  GLCadencer, 
  GLWin32Viewer, 
  GLKeyboard, 
  GLVectorGeometry, 
  GLGeomObjects,
  GLScene, 
  GLObjects, 
  GLGraph, 
  GLCrossPlatform, 
  GLSmoothNavigator,
  GLCoordinates, 
  GLBaseClasses, 
  GLScreen;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLCadencer1: TGLCadencer;
    GLCamera1: TGLCamera;
    scene: TGLDummyCube;
    FPSTimer: TTimer;
    GLSceneViewer1: TGLSceneViewer;
    Panel3: TPanel;
    MouseLookCheckBox: TCheckBox;
    GLLightSource1: TGLLightSource;
    GLSphere1: TGLSphere;
    GLXYZGrid1: TGLXYZGrid;
    GLArrowLine1: TGLArrowLine;
    GroupBox2: TGroupBox;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Panel1: TPanel;
    procedure GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure FPSTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MouseLookCheckBoxClick(Sender: TObject);
    procedure RadioButton6Click(Sender: TObject);
    procedure RadioButton7Click(Sender: TObject);
    procedure RadioButton8Click(Sender: TObject);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
     
    UI:  TGLSmoothUserInterface;
    Navigator: TGLSmoothNavigator;
    //  RealPos: TPoint;

    ShiftState: TShiftState;
    xx, yy: Integer;
    NewXX, NewYY: Integer;
    procedure CheckControls(DeltaTime, newTime: Double);
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Navigator := TGLSmoothNavigator.Create(Self);
  Navigator.AngleLock := False;
  Navigator.AutoUpdateObject := False;
  Navigator.InvertHorizontalSteeringWhenUpsideDown := True;
  Navigator.MoveUpWhenMovingForward := True;
  Navigator.UseVirtualUp := True;
  Navigator.VirtualUp.AsAffineVector := YVector;
  Navigator.MovingObject := GLCamera1;

  Navigator.InertiaParams.MovementAcceleration := 7;
  Navigator.InertiaParams.MovementInertia := 200;
  Navigator.InertiaParams.MovementSpeed := 200;

  Navigator.InertiaParams.TurnInertia := 150;
  Navigator.InertiaParams.TurnSpeed := 40;
  Navigator.InertiaParams.TurnMaxAngle := 0.5;

  Navigator.MoveAroundParams.TargetObject := GLArrowLine1;

  UI := TGLSmoothUserInterface.Create(Self);
//  UI.AutoUpdateMouse := False;
  UI.SmoothNavigator := Navigator;
end;

procedure TForm1.CheckControls(DeltaTime, newtime: Double);
var
  NeedToAccelerate: Boolean;
begin
  NeedToAccelerate := isKeyDown(VK_SHIFT);

  Navigator.StrafeVertical(isKeyDown('F'), isKeyDown('R'), DeltaTime, NeedToAccelerate);
  Navigator.MoveForward(isKeyDown('W'), isKeyDown('S'), DeltaTime, NeedToAccelerate);
  Navigator.StrafeHorizontal(isKeyDown('D'), isKeyDown('A'), DeltaTime, NeedToAccelerate);

//  GetCursorPos(RealPos);
  UI.MouseLook({RealPos, }DeltaTime);
//  if UI.MouseLookActive then
//    SetCursorPos(Round(UI.OriginalMousePos.X), Round(UI.OriginalMousePos.Y));
end;


procedure TForm1.GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  GLSceneViewer1.Invalidate;


  if UI.MouseLookActive then
    CheckControls(DeltaTime, newtime)
  else
  begin
    if (ssRight in ShiftState) and (ssLeft in ShiftState) then
    begin
      Navigator.MoveAroundTarget(0, 0, DelTaTime);
      Navigator.AdjustDistanceToTarget(yy - NewYY, DelTaTime)
    end
    else if (ssRight in ShiftState) or (ssLeft in ShiftState) then
    begin
      Navigator.MoveAroundTarget(yy - NewYY, xx - NewXX, DelTaTime);
      Navigator.AdjustDistanceToTarget(0, DelTaTime);
    end
    else
    begin
      Navigator.MoveAroundTarget(0, 0, DelTaTime);
      Navigator.AdjustDistanceToTarget(0, DelTaTime);
    end;

    xx := NewXX;
    yy := NewYY;
  end;
end;


procedure TForm1.FPSTimerTimer(Sender: TObject);
begin
  Caption := 'Smooth Navigator  -  ' + GLSceneViewer1.FramesPerSecondText;
  Navigator.AutoScaleParameters(GLSceneViewer1.FramesPerSecond);
  GLSceneViewer1.ResetPerformanceMonitor;
end;


procedure TForm1.MouseLookCheckBoxClick(Sender: TObject);
begin
  if MouseLookCheckBox.Checked then
  begin
    GLCamera1.TargetObject := nil;
    GLCamera1.PointTo(GLArrowLine1, YHmgVector);

    UI.MouseLookActive := True;
//    GetCursorPos(RealPos);
//    UI.OriginalMousePos.SetPoint2D(RealPos.X, RealPos.Y);
//    ShowCursor(False);
  end
  else
  begin
    UI.MouseLookActive := False;
//    ShowCursor(True);

    GLCamera1.Up.SetVector(0, 1, 0);
    GLCamera1.TargetObject := GLArrowLine1;
  end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = Char(VK_SPACE) then
    MouseLookCheckBoxClick(Self);
  if Key = Char(VK_ESCAPE) then
    Close;
end;


procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  GLSceneViewer1.Enabled := False;
  GLCadencer1.Enabled := False;
  FPSTimer.Enabled := False;

  FreeAndNil(UI);
  FreeAndNil(Navigator);
  GLShowCursor(True);
end;


procedure TForm1.RadioButton6Click(Sender: TObject);
begin
  GLCadencer1.FixedDeltaTime := 0;
end;

procedure TForm1.RadioButton7Click(Sender: TObject);
begin
  GLCadencer1.FixedDeltaTime := 0.01;
end;

procedure TForm1.RadioButton8Click(Sender: TObject);
begin
  GLCadencer1.FixedDeltaTime := 0.1;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  ShiftState :=  Shift;
  NewXX := X;
  NewYY := Y;
end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  xx := x;
  yy := y;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // (WheelDelta / Abs(WheelDelta) is used to deternime the sign.
  Navigator.AdjustDistanceParams.AddImpulse((WheelDelta / Abs(WheelDelta)));
end;

end.

