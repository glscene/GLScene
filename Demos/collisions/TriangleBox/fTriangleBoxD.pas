unit fTriangleBoxD;

interface

uses
  Winapi.OpenGL,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,

  GLS.VectorTypes,
  GLS.VectorLists,
  GLS.Scene,
  GLS.Objects,
  GLS.Cadencer,
  GLS.VectorFileObjects,
  GLS.Material,
  GLS.Color,
  GLS.State,
  GLS.SceneViewer,
  GLS.VectorGeometry,
  GLS.Graph,
  GLS.GeomObjects,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TFormTriangleBox = class(TForm)
    Viewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLCadencer: TGLCadencer;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    DCCamTarget: TGLDummyCube;
    Panel2: TPanel;
    GLCube1: TGLCube;
    GLXYZGrid1: TGLXYZGrid;
    GLLines1: TGLLines;
    CheckBoxGrid: TCheckBox;
    ButtonFindIntersect: TButton;
    CheckBoxPosition: TCheckBox;
    CheckBoxScale: TCheckBox;
    CheckBoxTriangle: TCheckBox;
    ButtonNotFindIntersect: TButton;
    GLPolygon1: TGLPolygon;
    CheckBoxVisible: TCheckBox;
    GLPoints1: TGLPoints;
    CheckBoxAxis: TCheckBox;
    GLLines2: TGLLines;
    GLPolygon2: TGLPolygon;
    RadioGroupCoPolygon: TRadioGroup;
    procedure GLCadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure ButtonNotFindIntersectClick(Sender: TObject);
    procedure ButtonFindIntersectClick(Sender: TObject);
    procedure CheckBoxVisibleClick(Sender: TObject);
    procedure RadioGroupCoPolygonClick(Sender: TObject);
  private
    mdx, mdy : Integer;
    BoxPos, BoxScale,
    MinExtend, MaxExtend : TAffineVector;
    TriangePos           : array [0..2] of TAffineVector;
    PolygonPos           : array [0..4] of TAffineVector;
    procedure MakeRandomData;
    procedure DrawResult;
    procedure DrawCoplanarPolygon;
  public
  end;

var
  FormTriangleBox : TFormTriangleBox;

implementation

{$R *.DFM}

const
  SizePos   = 10;
  ScaleSize = 3;
  TrigRect  = 10;

procedure TFormTriangleBox.FormCreate(Sender: TObject);
var
  i:integer;
begin
  Randomize;
  BoxScale := XYZVector;
  DrawCoplanarPolygon;
end;

procedure TFormTriangleBox.DrawCoplanarPolygon;
begin
  GLPolygon2.Nodes.Clear;
  GLPolygon2.Material.FrontProperties.Ambient.RandomColor;
  PolygonPos[0] := AffineVectorMake(-5,-5,0);
  PolygonPos[1] := AffineVectorMake(5,-5,0);
  PolygonPos[2] := AffineVectorMake(5,5,0);
  PolygonPos[3] := AffineVectorMake(-5,5,0);
  PolygonPos[4] := AffineVectorMake(0,0,0);
  GLPolygon2.AddNode(PolygonPos[0]);
  GLPolygon2.AddNode(PolygonPos[1]);
  GLPolygon2.AddNode(PolygonPos[2]);
  GLPolygon2.AddNode(PolygonPos[3]);
  GLPolygon2.AddNode(PolygonPos[4]);
end;

procedure TFormTriangleBox.MakeRandomData;
var
  i : Integer;
begin
    // Change position.
  if CheckBoxPosition.Checked then
    BoxPos   := AffineVectorMake( Random*SizePos -SizePos/2,
                                  Random*SizePos -SizePos/2,
                                  Random*SizePos -SizePos/2  );
    // Change scale.
  if CheckBoxScale.Checked then
    BoxScale := AffineVectorMake( Random*ScaleSize +ScaleSize/2,
                                  Random*ScaleSize +ScaleSize/2,
                                  Random*ScaleSize +ScaleSize/2 );
    // Change triangle.
  if CheckBoxTriangle.Checked then
    for i := 0 to 2 do
      TriangePos[i] := AffineVectorMake( Random*TrigRect -TrigRect/2,
                                         Random*TrigRect -TrigRect/2,
                                         Random*TrigRect -TrigRect/2 );
   // Calc extends.
  MinExtend := VectorSubtract(BoxPos, VectorScale(BoxScale, 0.5));
  MaxExtend := VectorAdd(     BoxPos, VectorScale(BoxScale, 0.5));
end;

procedure TFormTriangleBox.RadioGroupCoPolygonClick(Sender: TObject);
begin
  case RadioGroupCoPolygon.ItemIndex of
     0: GLPolygon2.Material.PolygonMode := pmFill;
     1: GLPolygon2.Material.PolygonMode := pmLines;
     2: GLPolygon2.Material.PolygonMode := pmPoints;
  end;
end;

procedure TFormTriangleBox.DrawResult;
var
  i : Integer;
begin
  GLPolygon1.Nodes.Clear;
  GLPolygon1.Material.FrontProperties.Emission.Color := clrGreen;
  GLPolygon1.Material.BackProperties.Emission.Color := clrGreen;
  GLPolygon1.AddNode(TriangePos[0]);
  GLPolygon1.AddNode(TriangePos[1]);
  GLPolygon1.AddNode(TriangePos[2]);

  GLPoints1.Positions.Clear;
  GLPoints1.Colors.Add(1, 0, 1, 1); //magenta
  GLPoints1.Size := 5;
  for i := 0 to 2 do
    GLPoints1.Positions.Add(TriangePos[i]);

  GLLines2.Nodes.Clear;
  GLLines2.Nodes.AddNode(TriangePos[0]);
  GLLines2.Nodes.AddNode(TriangePos[1]);
  GLLines2.Nodes.AddNode(TriangePos[2]);
  GLLines2.Nodes.AddNode(TriangePos[0]);

  DCCamTarget.Position.SetPoint(BoxPos);
  DCCamTarget.Scale.SetVector(BoxScale);
  GLCube1.Position.SetPoint(BoxPos);
  GLCube1.Scale.SetVector(BoxScale);

  DrawCoplanarPolygon;
end;

// Find next with intersection
procedure TFormTriangleBox.ButtonFindIntersectClick(Sender: TObject);
var
  IterCnt : Integer;
  Res1    : Boolean;
begin
  IterCnt := 0;
  repeat
    MakeRandomData;
    Res1 := IntersectTriangleBox(TriangePos[0], TriangePos[1], TriangePos[2],
      MinExtend, MaxExtend);
    IterCnt := IterCnt + 1;
    if IterCnt >= 10000 then
    begin
      DrawResult;
      ShowMessage('Intersection not found!');
      exit;
    end;
  until Res1;
  DrawResult;
end;

// Find next without intersection.
procedure TFormTriangleBox.ButtonNotFindIntersectClick(Sender: TObject);
var
  IterCnt : Integer;
  Res1    : Boolean;
begin
  IterCnt := 0;
  repeat
    MakeRandomData;
    Res1 := IntersectTriangleBox(TriangePos[0], TriangePos[1], TriangePos[2],
      MinExtend, MaxExtend);
    IterCnt := IterCnt + 1;
    if IterCnt >= 10000 then
    begin
      DrawResult;
      ShowMessage('Intersection not found!');
      Exit;
    end;
  until not Res1;
  DrawResult;
end;

procedure TFormTriangleBox.CheckBoxVisibleClick(Sender: TObject);
begin
  GLCube1.Visible    := CheckBoxVisible.Checked;
  GLXYZGrid1.Visible := CheckBoxGrid.Checked;
  GLLines1.Visible   := CheckBoxAxis.Checked;
end;

procedure TFormTriangleBox.GLCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  if FormTriangleBox.Active then Viewer.Invalidate
end;

procedure TFormTriangleBox.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Shift = [ssLeft] then GLCamera1.MoveAroundTarget(mdy -y, mdx -x);
  mdx := x;
  mdy := y;
end;

procedure TFormTriangleBox.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(Power(1.02, WheelDelta/120));
end;

procedure TFormTriangleBox.FormResize(Sender: TObject);
begin
  GLCamera1.FocalLength := MinInteger(Height, Width) / 10;
end;

procedure TFormTriangleBox.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then close;
end;

end.
