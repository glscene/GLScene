unit ObjmoveFm;

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
  GLS.VectorTypes,
  GLS.VectorGeometry,
  GLS.VectorFileObjects,
  GLS.SceneViewer,
  GLS.SpaceText,
  GLS.GeomObjects,
  GLS.Color,

  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.HUDObjects,
  GLS.SimpleNavigation,
  GLS.Navigator,
  GLS.SmoothNavigator;

type
  TFormObjmove = class(TForm)
    GLScene1: TGLScene;
    Scn: TGLSceneViewer;
    GLCamera: TGLCamera;
    DummyCube: TGLDummyCube;
    ZArrow: TGLArrowLine;
    XArrow: TGLArrowLine;
    YArrow: TGLArrowLine;
    Cube1: TGLCube;
    TopLight: TGLLightSource;
    Cube2: TGLCube;
    Floor: TGLCube;
    Panel1: TPanel;
    Button1: TButton;
    Label2: TLabel;
    TxtX: TGLSpaceText;
    TxtY: TGLSpaceText;
    Label3: TLabel;
    Label4: TLabel;
    TxtZ: TGLSpaceText;
    TopText: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    ObjText: TGLHUDText;
    GroupBox1: TGroupBox;
    ShowAxes: TCheckBox;
    StatusBar1: TStatusBar;
    GLSmoothNavigator1: TGLSmoothNavigator;
    procedure ScnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure ShowAxesClick(Sender: TObject);
  private
    lastMouseWorldPos: TGLVector;
    Cube: TGLCube;
    movingOnZ: Boolean;
    CurrentPick: TGLCustomSceneObject;
    ScnMouseMoveCnt: Integer;
    function MouseWorldPos(X, Y: Integer): TGLVector;
    procedure UpdateHudText;
    procedure ProcessPick(pick: TGLBaseSceneObject);
  end;

const
  SelectionColor: TColorVector = (X : 0.243; Y : 0.243; Z: 0.243; W : 1.000);

var
  FormObjmove: TFormObjmove;

implementation

{$R *.DFM}

procedure TFormObjmove.FormCreate(Sender: TObject);
begin
  UpdateHudText;
  Cube := TGLCube.CreateAsChild(DummyCube);
  Cube.CubeDepth := 0.2;
  Cube.CubeWidth := 0.2;
  Cube.CubeHeight := 0.2;
  Cube.Position.X := 1;
  Cube.Position.Y := 1;
  Cube.Position.Z := 1;
end;

function TFormObjmove.MouseWorldPos(X, Y: Integer): TGLVector;
var
  v: TGLVector;
begin
  Y := Scn.Height - Y;
  if Assigned(CurrentPick) then
  begin
    SetVector(v, X, Y, 0);
    if movingOnZ then
      Scn.Buffer.ScreenVectorIntersectWithPlaneXZ(v, CurrentPick.Position.Y,
        Result)
    else
      Scn.Buffer.ScreenVectorIntersectWithPlaneXY(v, CurrentPick.Position.Z,
        Result);
  end
  else
    SetVector(Result, NullVector);
end;

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

procedure TFormObjmove.ScnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pick: TGLBaseSceneObject;
begin
  movingOnZ := (ssShift in Shift);
  // If an object is picked...
  pick := (Scn.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject);
  ProcessPick(Pick);

  // store mouse pos
  if Assigned(CurrentPick) then
    lastMouseWorldPos := MouseWorldPos(X, Y);
end;

procedure TFormObjmove.ScnMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  newPos: TGLVector;
begin
  Inc(ScnMouseMoveCnt);
  Assert(ScnMouseMoveCnt < 2);
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
  Dec(ScnMouseMoveCnt);
end;

procedure TFormObjmove.ShowAxesClick(Sender: TObject);
begin
  // Unselect all
  ProcessPick(nil);
end;

procedure TFormObjmove.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio
  if WheelDelta <> 0 then
    GLCamera.AdjustDistanceToTarget(Power(1.1, -WheelDelta / 120));
end;

procedure TFormObjmove.FormKeyPress(Sender: TObject; var Key: Char);
begin
  with GLCamera do
    case Key of
      '2':  MoveAroundTarget(3, 0);
      '4':  MoveAroundTarget(0, -3);
      '6':  MoveAroundTarget(0, 3);
      '8':  MoveAroundTarget(-3, 0);
      '-':  AdjustDistanceToTarget(1.1);
      '+':  AdjustDistanceToTarget(1 / 1.1);
    end;
end;

procedure TFormObjmove.UpdateHudText;
var
  objPos, winPos: TAffineVector;
begin
  if Assigned(CurrentPick) then
  begin
    SetVector(objPos, CurrentPick.AbsolutePosition);

    TopText.Text := Format(
      'New Object Position: Xn: %4.4f, Yn: %4.4f, Zn: %4.4f',
      [objPos.X, objPos.Y, objPos.Z]);

    winPos := Scn.Buffer.WorldToScreen(objPos);

    with ObjText do
    begin
      Visible := true;
      Text := CurrentPick.Name;
      Position.X := winPos.X + 10;
      Position.Y := Scn.Height - winPos.Y + 10;
    end;
  end
  else
  begin
    TopText.Text := 'No selected object';
    ObjText.Visible := false;
  end;
end;

procedure TFormObjmove.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(CurrentPick) then
    with CurrentPick do
      case Key of
        VK_UP:
          if ssShift in Shift then
            Translate(0, 0, 0.3)
          else
            Translate(-0.3, 0, 0);
        VK_DOWN:
          if ssShift in Shift then
            Translate(0, 0, -0.3)
          else
            Translate(0.3, 0, 0);
        VK_LEFT:
          Translate(0, -0.3, 0);
        VK_RIGHT:
          Translate(0, 0.3, 0);
      end;
end;

end.
