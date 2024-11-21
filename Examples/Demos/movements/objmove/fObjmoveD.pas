unit fObjmoveD;

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,

  GLS.Scene,
  GLS.Objects,
  GLS.Graph,
  GLS.Collision,
  GLS.Texture,
  Stage.VectorTypes,
  Stage.VectorGeometry,
  GLS.VectorFileObjects,
  GLS.SceneViewer,
  GLS.SpaceText,
  GLS.GeomObjects,
  GLS.Color,

  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.HUDObjects;

type
  TFormObjmove = class(TForm)
    GLScene1: TGLScene;
    Scene: TGLSceneViewer;
    Camera: TGLCamera;
    DummyCube: TGLDummyCube;
    ZArrow: TGLArrowLine;
    XArrow: TGLArrowLine;
    YArrow: TGLArrowLine;
    Cube1: TGLCube;
    TopLight: TGLLightSource;
    Cube2: TGLCube;
    Floor: TGLCube;
    Panel1: TPanel;
    SpaceTextX: TGLSpaceText;
    SpaceTextY: TGLSpaceText;
    SpaceTextZ: TGLSpaceText;
    HUDText: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    HUDTextObj: TGLHUDText;
    GroupBox1: TGroupBox;
    ShowAxes: TCheckBox;
    StatusBar1: TStatusBar;
    Button1: TButton;
    ButtonReset: TButton;
    procedure SceneMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SceneMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure ShowAxesClick(Sender: TObject);
    procedure ButtonResetClick(Sender: TObject);
  private
    lastMouseWorldPos: TGLVector;
    movingOnZ: Boolean;
    CurrentPick: TGLCustomSceneObject;
    SceneMouseMoveCnt: Integer;
    function MouseWorldPos(X, Y: Integer): TGLVector;
    procedure UpdateHUDText;
    procedure ProcessPick(pick: TGLBaseSceneObject);
  end;

const
  SelectionColor: TGLColorVector = (X : 0.243; Y : 0.243; Z: 0.243; W : 1.000);

var
  FormObjmove: TFormObjmove;

implementation

{$R *.DFM}

//------------------------------------------------------------------

procedure TFormObjmove.FormCreate(Sender: TObject);
begin
  UpdateHUDText;
end;

//------------------------------------------------------------------

function TFormObjmove.MouseWorldPos(X, Y: Integer): TGLVector;
var
  v: TGLVector;
begin
  Y := Scene.Height - Y;
  if Assigned(CurrentPick) then
  begin
    SetVector(v, X, Y, 0);
    if movingOnZ then
      Scene.Buffer.ScreenVectorIntersectWithPlaneXZ(v, CurrentPick.Position.Y, Result)
    else
      Scene.Buffer.ScreenVectorIntersectWithPlaneXY(v, CurrentPick.Position.Z, Result);
  end
  else
    SetVector(Result, NullVector);
end;

//------------------------------------------------------------------

procedure TFormObjmove.ProcessPick(pick: TGLBaseSceneObject);
begin
  if Assigned(pick) then
  begin
    // Only Cube1 and Cube2 can be selected
    if (pick.Name <> 'Cube1') and (pick.Name <> 'Cube2') then
      pick := nil;
  end;
  if pick <> CurrentPick then
  begin
    if Assigned(CurrentPick) then
    begin
      CurrentPick.ShowAxes := false;
      CurrentPick.Material.FrontProperties.Emission.Color := clrBlack;
    end;
    CurrentPick := TGLCustomSceneObject(pick);
    if Assigned(CurrentPick) then
    begin
      if ShowAxes.Checked then
        CurrentPick.ShowAxes := true;
      CurrentPick.Material.FrontProperties.Emission.Color := SelectionColor;
    end;
  end;
  UpdateHudText;
end;

//------------------------------------------------------------------

procedure TFormObjmove.SceneMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pick: TGLBaseSceneObject;
begin
  movingOnZ := (ssShift in Shift);
  // If an object is picked...
  pick := (Scene.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject);
  ProcessPick(Pick);

  // store mouse pos
  if Assigned(CurrentPick) then
    lastMouseWorldPos := MouseWorldPos(X, Y);
end;

//------------------------------------------------------------------

procedure TFormObjmove.SceneMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  newPos: TGLVector;
begin
  Inc(SceneMouseMoveCnt);
  Assert(SceneMouseMoveCnt < 2);
  if ssLeft in Shift then
  begin
    // handle hold/unhold of shift
    if (ssShift in Shift) <> movingOnZ then
    begin
      movingOnZ := (ssShift in Shift);
      lastMouseWorldPos := MouseWorldPos(X, Y);
    end;
    newPos := MouseWorldPos(X, Y);
    if Assigned(CurrentPick) and (VectorNorm(lastMouseWorldPos) <> 0) then
      CurrentPick.Position.Translate(VectorSubtract(newPos, lastMouseWorldPos));
    lastMouseWorldPos := newPos;

    UpdateHudText;
  end;
  Dec(SceneMouseMoveCnt);
end;

//------------------------------------------------------------------

procedure TFormObjmove.ShowAxesClick(Sender: TObject);
begin
  // Unselect all
  ProcessPick(nil);
end;

//------------------------------------------------------------------

procedure TFormObjmove.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio
  if WheelDelta <> 0 then
    Camera.AdjustDistanceToTarget(Power(1.1, -WheelDelta / 120));
end;

//------------------------------------------------------------------

procedure TFormObjmove.FormKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '1': Camera.MoveAroundTarget(3, 0);
    '2': Camera.MoveAroundTarget(-3, 0);
    '3': Camera.MoveAroundTarget(0, 3);
    '4': Camera.MoveAroundTarget(0, -3);
    '-': Camera.AdjustDistanceToTarget(1.1);
    '+': Camera.AdjustDistanceToTarget(1 / 1.1);
  end;
end;

//------------------------------------------------------------------

procedure TFormObjmove.UpdateHUDText;
var
  objPos, winPos: TAffineVector;
begin
  if Assigned(CurrentPick) then
  begin
    SetVector(objPos, CurrentPick.AbsolutePosition);
    HUDText.Text := Format('New Object Position: Xn: %4.3f, Yn: %4.3f, Zn: %4.3f',
      [objPos.X, objPos.Y, objPos.Z]);
    winPos := Scene.Buffer.WorldToScreen(objPos);

    HUDTextObj.Visible := True;
    HUDTextObj.Text := CurrentPick.Name;
    HUDTextObj.Position.X := winPos.X + 20;
    HUDTextObj.Position.Y := Scene.Height - winPos.Y + 20;
  end
  else
  begin
    HUDText.Text := 'No selected object';
    HUDTextObj.Visible := False;
  end;
end;

//------------------------------------------------------------------

procedure TFormObjmove.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(CurrentPick) then
    case Key of
      VK_UP:
        if ssShift in Shift then
          CurrentPick.Translate(0, 0, 0.3)
        else
          CurrentPick.Translate(-0.3, 0, 0);
      VK_DOWN:
        if ssShift in Shift then
          CurrentPick.Translate(0, 0, -0.3)
        else
          CurrentPick.Translate(0.3, 0, 0);
      VK_LEFT:
        CurrentPick.Translate(0, -0.3, 0);
      VK_RIGHT:
        CurrentPick.Translate(0, 0.3, 0);
    end;
end;

procedure TFormObjmove.ButtonResetClick(Sender: TObject);
begin
  Cube1.Position.X := 0.1;
  Cube1.Position.Y := 0.1;
  Cube1.Position.Z := -0.9;
  Cube2.Position.X := -0.4;
  Cube2.Position.Y := 0.4;
  Cube2.Position.Z := -0.5;
  UpdateHUDText;
end;

end.
