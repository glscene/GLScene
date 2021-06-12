unit GizmoFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,

  
  GLS.Scene,
  GLS.Cadencer,
  GLS.PersistentClasses,
  GLS.Objects,
  GLS.SpaceText,
  GLS.SceneViewer,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.HUDObjects,
  GLS.GeomObjects,
  GLS.Gizmo,
 
  GLS.Coordinates,
  GLS.BaseClasses, GLS.VectorFileObjects;

type
  TFormGizmo = class(TForm)
    GLScene1: TGLScene;
    Viewer: TGLSceneViewer;
    Camera: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    RootGizmo: TGLDummyCube;
    Panel1: TPanel;
    Bevel1: TBevel;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CBXAxis: TComboBox;
    CheckBox3: TCheckBox;
    CBXOperation: TComboBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    ColorBox1: TColorBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    Label4: TLabel;
    edAutoZoomFactor: TEdit;
    Label5: TLabel;
    edZoomFactor: TEdit;
    CheckBox12: TCheckBox;
    Label6: TLabel;
    edMoveCoef: TEdit;
    edRotateCoef: TEdit;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    GLDodecahedron3: TGLDodecahedron;
    GLArrowLine3: TGLArrowLine;
    GLArrowLine4: TGLArrowLine;
    edScaleCoef: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    edGizmoThickness: TEdit;
    GLSphere1: TGLSphere;
    GLCube1: TGLCube;
    OptPickMode: TRadioGroup;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    Label7: TLabel;
    procedure GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure edAutoZoomFactorKeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox12Click(Sender: TObject);
    procedure CBXAxisChange(Sender: TObject);
    procedure CBXOperationChange(Sender: TObject);
    procedure edMoveCoefChange(Sender: TObject);
    procedure edRotateCoefChange(Sender: TObject);
    procedure edAutoZoomFactorChange(Sender: TObject);
    procedure edZoomFactorChange(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure edScaleCoefChange(Sender: TObject);
    procedure edGizmoThicknessChange(Sender: TObject);
    procedure OptPickModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure FillPickableObjectsList(root: TGLBaseSceneObject; doClearList: Boolean);
     
  public
     
    mx, my: Integer;
    //    gizmo: TGLGizmoEx;
    noMouseMotion: Boolean;

  end;

var
  FormGizmo: TFormGizmo;
  Gizmo: TGLGizmo;

implementation

{$R *.dfm}

procedure TFormGizmo.GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  Viewer.Invalidate;
end;

procedure TFormGizmo.OptPickModeClick(Sender: TObject);
begin
  Gizmo.PickMode := TGLGizmoPickMode(OptPickMode.ItemIndex);
end;

procedure TFormGizmo.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  Gizmo.ViewerMouseDown(X, Y);
end;

procedure TFormGizmo.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  //if noMouseMotion then exit;

  if (shift = [ssLeft]) and (gizmo.SelectedObj = nil) then
    Camera.MoveAroundTarget(Y - my, mx - X)
  else if (shift = [ssRight]) and (gizmo.SelectedObj = nil) then
  begin
    if my > Y then
      Camera.AdjustDistanceToTarget(1.05)
    else
      Camera.AdjustDistanceToTarget(0.95);
    Gizmo.MoveCoef := Camera.DistanceToTarget / 1000;
  end
  else
    Gizmo.ViewerMouseMove(X, Y);

  mx := X;
  my := Y;
end;

procedure TFormGizmo.ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Gizmo.ViewerMouseUp(X, Y);
end;

procedure TFormGizmo.FormShow(Sender: TObject);
begin
  Viewer.SetFocus;
  Gizmo.RootGizmo := rootGizmo;
  // Fill list of pickable objects when using PickMode=pmRaycast
  FillPickableObjectsList(GLDummyCube1, True);
end;


// Recurse root object to fill list of pickable objects when using PickMode=pmRaycast
procedure TFormGizmo.FillPickableObjectsList(root: TGLBaseSceneObject; doClearList: Boolean);
var
  t: Integer;
begin
  if doClearList then
    Gizmo.PickableObjectsWithRayCast.Clear;
  for t := 0 to root.Count - 1 do
  begin
    Gizmo.PickableObjectsWithRayCast.Add(root[t]);
    FillPickableObjectsList(root[t], False);
  end;
end;


procedure TFormGizmo.edAutoZoomFactorKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key,['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', ',']) then
    Key := #0;

end;

procedure TFormGizmo.CheckBox12Click(Sender: TObject);
var
  check: Boolean;
begin
  // (Sender as TCheckBox).Checked:=Not((Sender as TCheckBox).Checked);
  check := (Sender as TCheckBox).Checked;
  case (Sender as TCheckBox).Tag of
    0: Gizmo.Enabled := Check;
    1: Gizmo.ExcludeObjects := Check;
    2:
    begin
      Gizmo.ForceAxis := Check;
      CBXAxis.Enabled := Check;
    end;
    3:
    begin
      Gizmo.ForceOperation := Check;
      CBXOperation.Enabled := Check;
    end;
    4: Gizmo.ForceUniformScale := Check;
    5: if Check then
        Gizmo.GizmoElements := Gizmo.GizmoElements + [geAxisLabel]
      else
        Gizmo.GizmoElements := Gizmo.GizmoElements - [geAxisLabel];
    6:
    begin
      if Check then
      begin
        Gizmo.GizmoElements := Gizmo.GizmoElements + [geObjectInfos];
        CheckBox7.Enabled := Check;
        CheckBox8.Enabled := Check;
        CheckBox9.Enabled := Check;
      end
      else
      begin
        Gizmo.GizmoElements := Gizmo.GizmoElements - [geObjectInfos];
        CheckBox7.Enabled := Check;
        CheckBox8.Enabled := Check;
        CheckBox9.Enabled := Check;
      end;
    end;
    7: Gizmo.NoZWrite := Check;
    8:
    begin
      Gizmo.AutoZoom := Check;
      if Check then
      begin
        edAutoZoomFactor.Enabled := True;
        edZoomFactor.Enabled := False;
      end
      else
      begin
        edAutoZoomFactor.Enabled := False;
        edZoomFactor.Enabled := True;
      end;
    end;
    9: if Check then
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels + [vliName]
      else
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels - [vliName];
    10: if Check then
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels + [vliOperation]
      else
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels - [vliOperation];
    11: if Check then
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels + [vliCoords]
      else
        Gizmo.VisibleInfoLabels := Gizmo.VisibleInfoLabels - [vliCoords];
    12: if Check then
        Gizmo.GizmoElements := Gizmo.GizmoElements + [geMove]
      else
        Gizmo.GizmoElements := Gizmo.GizmoElements - [geMove];
    13: if Check then
        Gizmo.GizmoElements := Gizmo.GizmoElements + [geRotate]
      else
        Gizmo.GizmoElements := Gizmo.GizmoElements - [geRotate];
    14: if Check then
        Gizmo.GizmoElements := Gizmo.GizmoElements + [geScale]
      else
        Gizmo.GizmoElements := Gizmo.GizmoElements - [geScale];
  end;
end;

procedure TFormGizmo.CBXAxisChange(Sender: TObject);
begin
  case CBXAxis.ItemIndex of
    0: Gizmo.SelAxis := gaNone;
    1: Gizmo.SelAxis := gaX;
    2: Gizmo.SelAxis := gaXY;
    3: Gizmo.SelAxis := gaXZ;
    4: Gizmo.SelAxis := gaY;
    5: Gizmo.SelAxis := gaYZ;
    6: Gizmo.SelAxis := gaZ;
  end;
end;

procedure TFormGizmo.CBXOperationChange(Sender: TObject);
begin
  case CBXOperation.ItemIndex of
    0: Gizmo.Operation := gopNone;
    1: Gizmo.Operation := gopMove;
    2: Gizmo.Operation := gopRotate;
    3: Gizmo.Operation := gopScale;
  end;
end;

procedure TFormGizmo.edMoveCoefChange(Sender: TObject);
begin
  if edMoveCoef.Text <> '' then
    Gizmo.MoveCoef := StrToFloat(edMoveCoef.Text);
end;

procedure TFormGizmo.edRotateCoefChange(Sender: TObject);
begin
  if edRotateCoef.Text <> '' then
    Gizmo.RotationCoef := StrToFloat(edRotateCoef.Text);
end;

procedure TFormGizmo.edGizmoThicknessChange(Sender: TObject);
begin
  Gizmo.GizmoThickness := StrToFloat(edGizmoThickness.Text);
end;

procedure TFormGizmo.edScaleCoefChange(Sender: TObject);
begin
  if edScaleCoef.Text <> '' then
    Gizmo.ScaleCoef := StrToFloat(edScaleCoef.Text);
end;

procedure TFormGizmo.edAutoZoomFactorChange(Sender: TObject);
begin
  if edAutoZoomFactor.Text <> '' then
    Gizmo.AutoZoomFactor := StrToFloat(edAutoZoomFactor.Text);
end;

procedure TFormGizmo.edZoomFactorChange(Sender: TObject);
begin
  if edZoomFactor.Text <> '' then
    Gizmo.ZoomFactor := StrToFloat(edZoomFactor.Text);
end;

procedure TFormGizmo.ColorBox1Change(Sender: TObject);
begin
  case (Sender as TColorBox).Tag of
    0: Gizmo.BoundingBoxColor.AsWinColor := ColorBox1.Selected;
    1: Gizmo.VisibleInfoLabelsColor.AsWinColor := ColorBox2.Selected;
    2: Gizmo.SelectedColor.AsWinColor := ColorBox3.Selected;
  end;
end;

procedure TFormGizmo.FormCreate(Sender: TObject);
begin
  Gizmo := TGLGizmo.Create(Self);
  Gizmo.LabelFont := WindowsBitmapFont;
  Gizmo.Viewer := Viewer;
end;

procedure TFormGizmo.FormDestroy(Sender: TObject);
begin
  Gizmo.Free;
end;

end.
