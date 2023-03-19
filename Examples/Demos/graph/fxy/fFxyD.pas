unit fFxyD;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  GLS.Objects,
  GLS.Graph,
  GLS.Scene,
  GLS.VectorGeometry,
  GLS.VectorTypes,
  GLS.SceneViewer,

  GLS.Coordinates,
  GLS.BaseClasses, GLS.AsyncTimer;

type
  TFormFxy = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    HeightField1: TGLHeightField;
    YZGrid: TGLXYZGrid;
    XZGrid: TGLXYZGrid;
    XYGrid: TGLXYZGrid;
    Panel1: TPanel;
    CBCentered: TCheckBox;
    Label1: TLabel;
    TBXYPosition: TTrackBar;
    TBYZPosition: TTrackBar;
    TBXZPosition: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    GLAsyncTimer1: TGLAsyncTimer;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure CBCenteredClick(Sender: TObject);
    procedure TBXYPositionChange(Sender: TObject);
    procedure TBYZPositionChange(Sender: TObject);
    procedure TBXZPositionChange(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure HeightField1GetHeight(const X, Y: Single; var z: Single;
      var Color: TVector4f; var TexPoint: TTexPoint);
    procedure GLAsyncTimer1Timer(Sender: TObject);
  public
    mx, my: Integer;
  end;

var
  FormFxy: TFormFxy;

// ----------------------------------
implementation
// ----------------------------------

{$R *.DFM}

procedure TFormFxy.HeightField1GetHeight(const X, Y: Single; var z: Single;
  var Color: TVector4f; var TexPoint: TTexPoint);
begin
  z := VectorNorm(X, Y);
  z := cos(z * 12) / (2 * (z * 6.28 + 1));
end;

procedure TFormFxy.CBCenteredClick(Sender: TObject);
begin
  if CBCentered.Checked then
  begin
    XZGrid.YSamplingScale.Origin := 0;
    YZGrid.XSamplingScale.Origin := 0;
    XYGrid.ZSamplingScale.Origin := 0;
  end
  else
  begin
    XZGrid.YSamplingScale.Origin := -1;
    YZGrid.XSamplingScale.Origin := -1;
    XYGrid.ZSamplingScale.Origin := -1;
  end;
end;

procedure TFormFxy.TBXYPositionChange(Sender: TObject);
begin
  XYGrid.ZSamplingScale.Origin := -(TBXYPosition.Position / 10);
end;

procedure TFormFxy.TBYZPositionChange(Sender: TObject);
begin
  YZGrid.XSamplingScale.Origin := -(TBYZPosition.Position / 10);
end;

procedure TFormFxy.TBXZPositionChange(Sender: TObject);
begin
  XZGrid.YSamplingScale.Origin := -(TBXZPosition.Position / 10);
end;

// following code takes care of camera movement, see camera & movement demos
// for explanations and more samples

procedure TFormFxy.GLAsyncTimer1Timer(Sender: TObject);
begin
  HeightField1.StructureChanged;
end;

procedure TFormFxy.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormFxy.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if Shift <> [] then
  begin
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
    mx := X;
    my := Y;
  end;
end;

procedure TFormFxy.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1 := GLSceneViewer1.Camera;
  // Note that 1 wheel-step induces a WheelDelta of 120,
  // this code adjusts the distance to target with a 10% per wheel-step ratio
  GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
end;

end.
