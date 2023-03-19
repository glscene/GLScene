unit fGizmoExD;

interface

uses
  Winapi.Windows,
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Math,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Buttons,
  Vcl.ComCtrls,

  
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.Cadencer,
  GLS.Objects,
  GLS.VectorTypes,
  GLS.SceneViewer,
  GLS.Keyboard,

  GLS.Utils,
  GLS.GizmoEx,

  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.VectorGeometry,
  GLS.GeomObjects,
  GLS.BitmapFont,
  GLS.WindowsFont,
  GLS.HUDObjects,
  GLS.Graph,
  GLS.VectorFileObjects;

type
  TFormGizmoEx = class(TForm)
    GLScene1: TGLScene;
    GLRootObjects: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLLightSource1: TGLLightSource;
    GLLightSource2: TGLLightSource;
    RootGizmo: TGLDummyCube;
    GLDodecahedron3: TGLDodecahedron;
    GLArrowLine3: TGLArrowLine;
    GLArrowLine4: TGLArrowLine;
    Label9: TLabel;
    GLSphere1: TGLSphere;
    GLCube1: TGLCube;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    GLFrustrum1: TGLFrustrum;
    GLDisk1: TGLDisk;
    GLCube2: TGLCube;
    Panel2: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    Label12: TLabel;
    Viewer: TGLSceneViewer;
    SpeedButton6: TSpeedButton;
    Panel4: TPanel;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton10: TSpeedButton;
    RootTempObjects: TGLDummyCube;
    SpeedButton11: TSpeedButton;
    Label15: TLabel;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    edAutoZoomFactor: TEdit;
    edtGizmoThickness: TEdit;
    edtScaleCoef: TEdit;
    edzoomfactor: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    OptPickMode: TRadioGroup;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ColorBox4: TColorBox;
    Label1: TLabel;
    Label14: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label16: TLabel;
    ComboBox3: TComboBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    Panel1: TPanel;
    SpeedButton14: TSpeedButton;
    SpeedButton15: TSpeedButton;
    ComboBox4: TComboBox;
    CheckBox3: TCheckBox;
    edMoveCoef: TEdit;
    Label6: TLabel;
    edRotateCoef: TEdit;
    Label7: TLabel;
    CheckBox16: TCheckBox;
    TabSheet3: TTabSheet;
    Label8: TLabel;
    Edit1: TEdit;
    Label13: TLabel;
    Panel3: TPanel;
    TreeView1: TTreeView;
    GLRootUserInterface: TGLDummyCube;
    GLXYZGrid1: TGLXYZGrid;
    GLTargetCamera: TGLDummyCube;
    Camera: TGLCamera;
    SpeedButton17: TSpeedButton;
    SpeedButton18: TSpeedButton;
    Panel5: TPanel;
    GroupBox1: TGroupBox;
    SpeedButton16: TSpeedButton;
    SpeedButton19: TSpeedButton;
    SpeedButton20: TSpeedButton;
    Label17: TLabel;
    Timer1: TTimer;
    procedure GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure edAutoZoomFactorKeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox12Click(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure edtGizmoThicknessChange(Sender: TObject);
    procedure OptPickModeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ViewerMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    function MouseWorldPos(const X, Y: Integer; isy: boolean = false): TGLVector;
    procedure ComboBox4Change(Sender: TObject);
    procedure SpeedButton6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton7MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton17Click(Sender: TObject);
    procedure SpeedButton14Click(Sender: TObject);
    procedure SpeedButton16Click(Sender: TObject);
    function ObjectName(value: string): string;
    procedure Timer1Timer(Sender: TObject);
  private
     
  public
     
    mx, my: Integer;
    MousePos,LostMousePos: TGLVector;
    MouseMoving: boolean;
    pos: TGLVector;
    FObj: TGLBaseSceneObject;
    procedure UpdateTreeView;
  end;

var
  FormGizmoEx: TFormGizmoEx;
  Gizmo: TGLGizmoEx;
  FVectorLength: Single;
  FCreationScenarious: integer;

implementation

{$R *.dfm}

procedure SettingsObj(Obj: TGLBaseSceneObject; Step: Integer; Length: TGLVector);
begin
  if (Obj is TGLCube) then
  with (Obj as TGLCube) do
    case Step of
      0:
      begin
        CubeWidth := Length.X*2;
        CubeDepth := Length.Z*2;
      end;
      1: CubeHeight := FVectorLength;
      2:  FCreationScenarious := -1;
    end;

  if (Obj is TGLSphere) then
  with (Obj as TGLSphere) do
    case Step of
      0: Radius := FVectorLength;
      1: FCreationScenarious := -1;
    end;

  if (Obj is TGLPlane) then
  with (Obj as TGLPlane) do
    case Step of
      0:
      begin
        Width := Length.X*2;
        Height := Length.Z*2;
      end;
      1: FCreationScenarious := -1;
    end;
end;

function TFormGizmoEx.ObjectName(value: string): string;
var
  i: integer;
begin
   Result := value;
   i := 1;
   while glScene1.FindSceneObject(Result) <> nil do
   begin
      Result := value + IntToStr(i);
      Inc(i);
   end;
end;

function TFormGizmoEx.MouseWorldPos(const X, Y: Integer; isy: boolean = false): TGLVector;
var
  v: TGLVector;
  InvertedY: Integer;
begin
  InvertedY := Viewer.Height - Y;

  SetVector(v, X, InvertedY, 0);
 if not isy then
  Viewer.Buffer.ScreenVectorIntersectWithPlaneXZ
   (v, GLTargetCamera.AbsolutePosition.Y, Result)
   else
     Viewer.Buffer.ScreenVectorIntersectWithPlaneXY
       (v, GLTargetCamera.AbsolutePosition.Z, Result)
end;

procedure TFormGizmoEx.UpdateTreeView;
var
   I: Integer;
   ObjectNode: TTreeNode;
   CurrentNode: TTreeNode;

   function AddNodes(ANode: TTreeNode; AObject: TGLBaseSceneObject): TTreeNode;
   var
     I: Integer;
   begin
     if IsSubComponent(AObject) then
     begin
       Result := TreeView1.Selected;
       exit;
     end else
     begin
       Result := TreeView1.Items.AddChildObject(ANode, AObject.Name, AObject);
       CurrentNode := Result;
       for I := 0 to AObject.Count -1 do
         Result := AddNodes(CurrentNode, AObject[I]);
     end;
   end;

 begin
   TreeView1.Items.Clear;
    // -- add two root nodes --
   ObjectNode := TreeView1.Items.AddFirst(nil, 'RootTempObjects');
   // -- get the object's tree --
   TreeView1.Items.BeginUpdate();
     with RootTempObjects do
     begin
       // -- objects (with children too) --
       if Assigned(RootTempObjects) then
         begin
         ObjectNode.Data := RootTempObjects;
        with RootTempObjects do
           for I := 0 to Count -1 do
             AddNodes(ObjectNode, Children[I]);
         ObjectNode.Expand(True);
       end;
     end;
   TreeView1.Items.EndUpdate();

   // -- add two root nodes --
   ObjectNode := TreeView1.Items.AddFirst(nil, 'World');
   // -- get the object's tree --
   TreeView1.Items.BeginUpdate();
     with GLRootObjects do
     begin
       // -- objects (with children too) --
       if Assigned(GLRootObjects) then
         begin
         ObjectNode.Data := GLRootObjects;
        with GLRootObjects do
           for I := 0 to Count -1 do
             AddNodes(ObjectNode, Children[I]);
         ObjectNode.Expand(True);
       end;
     end;
   TreeView1.Items.EndUpdate();
end;


procedure TFormGizmoEx.GLCadencer1Progress(Sender: TObject; const DeltaTime, newTime: Double);
begin
  Viewer.Invalidate;
end;

procedure TFormGizmoEx.OptPickModeClick(Sender: TObject);
begin
  Gizmo.PickMode := TGLGizmoExPickMode(OptPickMode.ItemIndex);
end;

procedure TFormGizmoEx.SpeedButton14Click(Sender: TObject);
begin
  gizmo.Enabled := not ((SpeedButton18.Down or SpeedButton15.Down) or SpeedButton14.Down);
  CheckBox12.Checked := Gizmo.Enabled;
end;

procedure TFormGizmoEx.SpeedButton16Click(Sender: TObject);
begin
  Gizmo.Enabled := not (Sender as TSpeedButton).Down;
end;

procedure TFormGizmoEx.SpeedButton17Click(Sender: TObject);
begin
  GLTargetCamera.Position.SetPoint(0, 0, 0);
  GLTargetCamera.Direction.SetVector(0, 0, 1);
  GLTargetCamera.Up.SetVector(0, 1, 0);
end;

procedure TFormGizmoEx.SpeedButton1Click(Sender: TObject);
begin
  (Sender as TSpeedButton).Down:=false;
   case (Sender as TSpeedButton).Tag of
    0: Gizmo.OperationMode:=gomSelect;
    1: Gizmo.OperationMode:=gomMove;
    2: Gizmo.OperationMode:=gomRotate;
    3: Gizmo.OperationMode:=gomScale;
    4: Gizmo.OperationMode:=gomNone;
    5: Gizmo.Undo;
    6: Gizmo.Redo;
    7: Gizmo.RemoveSelectedObjects;
  end;
  UpdateTreeView;

  // Disable buttons Camera Translate
  SpeedButton18.Down := false;
  SpeedButton15.Down := false;
  SpeedButton14.Down := false;
  SpeedButton17.Down := false;
end;

procedure TFormGizmoEx.SpeedButton6MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Panel4.Visible:= not Panel4.Visible;
  Panel4.Left:=SpeedButton6.Left;
  Panel4.Top:= SpeedButton6.Top+SpeedButton6.Height;
end;

procedure TFormGizmoEx.SpeedButton7MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   case (Sender as TSpeedButton).Tag of
    20:
    begin
      Gizmo.SelectionRegion:= gsrRectangular;
      SpeedButton6.Glyph.Assign(SpeedButton7.Glyph);
    end;
    21:
    begin
      Gizmo.SelectionRegion:= gsrCircular;
      SpeedButton6.Glyph.Assign(SpeedButton8.Glyph);
    end;
    22:
    begin
      Gizmo.SelectionRegion:= gsrFence;
      SpeedButton6.Glyph.Assign(SpeedButton9.Glyph);
    end;
    23:
    begin
      Gizmo.SelectionRegion:= gsrLasso;
      SpeedButton6.Glyph.Assign(SpeedButton10.Glyph);
    end;
  end;
 Panel4.Visible:=false;
 SpeedButton6.Down:=false;
end;

procedure TFormGizmoEx.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
  Gizmo.viewerMouseDown(X, Y);
  if SpeedButton15.Down or SpeedButton18.Down  then
  begin
    if SpeedButton15.Down then
      LostMousePos := MouseWorldPos(x, y)
      else
        LostMousePos := MouseWorldPos(x, y, true);
    pos:=GLTargetCamera.Position.AsVector;
    MouseMoving:=true;
  end;


  //Create Cube With Mouse
  if SpeedButton16.Down then
  begin
    LostMousePos := MouseWorldPos(x, y);
    MouseMoving := true;

    if FCreationScenarious = -1 then
    begin
      FObj := TGLCube.CreateAsChild(GLRootObjects);
      (FObj as tglcube).CubeDepth := 0;
      (FObj as tglcube).CubeHeight := 0;
      (FObj as tglcube).CubeWidth := 0.1;
       FObj.Position.AsVector := LostMousePos;
       FObj.Name:= ObjectName('GLCube');
       UpdateTreeView;
       FCreationScenarious := 0;
    end;
  end;

    //Create Sphere With Mouse
  if SpeedButton19.Down then
  begin
    LostMousePos := MouseWorldPos(x, y);
    MouseMoving := true;

    if FCreationScenarious = -1 then
    begin
      FObj := TGLSphere.CreateAsChild(GLRootObjects);
      (FObj as tglSphere).Radius := 0;
       FObj.Position.AsVector := LostMousePos;
       FObj.Name:= ObjectName('GLSphere');
       UpdateTreeView;
       FCreationScenarious := 0;
    end;
  end;
    //Create Sphere With Mouse
  if SpeedButton20.Down then
  begin
    LostMousePos := MouseWorldPos(x, y);
    MouseMoving := true;

    if FCreationScenarious = -1 then
    begin
      FObj := TGLPlane.CreateAsChild(GLRootObjects);
      (FObj as TGLPlane).Height := 0;
      (FObj as TGLPlane).Width := 0;
       FObj.Position.AsVector := LostMousePos;
       FObj.PitchAngle := 90   ;
       FObj.Name:= ObjectName('GLPlane');
       UpdateTreeView;
       FCreationScenarious := 0;
    end;
  end;
end;

procedure TFormGizmoEx.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

begin
  if SpeedButton14.Down then
  begin
    if (shift = [ssLeft]) then
      GLTargetCamera.turn(mx - X)
    else if (shift = [ssRight])  then
    begin
      if my > Y then
        Camera.AdjustDistanceToTarget(1.05)
      else
        Camera.AdjustDistanceToTarget(0.95);
      gizmo.MoveCoef := Camera.DistanceToTarget / 1000;
    end;
  end
    else
      gizmo.viewerMouseMove(X, Y);

  if MouseMoving then
  begin
    if SpeedButton15.Down or SpeedButton18.Down then
    begin
      if SpeedButton15.Down then
        MousePos := MouseWorldPos(x, y)
        else
          MousePos := MouseWorldPos(x, y, true);
      MousePos := VectorSubtract(LostMousePos,MousePos);
      MousePos.X:= -MousePos.X*0.4 ;
      MousePos.Z:= -MousePos.Z*0.4 ;
      GLTargetCamera.Position.AsVector := Vectoradd(pos, MousePos);
    end;
    if SpeedButton16.Down or SpeedButton19.Down or SpeedButton20.Down then
    begin
      MousePos := MouseWorldPos(x, y);
      FVectorLength := VectorLength(VectorSubtract(LostMousePos,MousePos));
      pos := VectorSubtract(LostMousePos,MousePos);
      SettingsObj(Fobj, FCreationScenarious, pos)
    end;
  end;

  mx := X;
  my := Y;
end;

procedure TFormGizmoEx.ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 //
  MouseMoving:=false;
  gizmo.viewerMouseUp(X, Y);


  if TMouseButton(Button)=mbRight then
  begin
    if gizmo.CursorSelectingRegion then
    begin
      gizmo.LooseCursorSelection;
    end;
    if SpeedButton15.Down or SpeedButton14.Down or SpeedButton18.Down
    then
    begin
      SpeedButton15.Down := false;
      SpeedButton14.Down := false;
      SpeedButton18.Down := false;
      Gizmo.Enabled := true;
      CheckBox12.Checked := true;
    end;
    if SpeedButton16.Down or SpeedButton19.Down or
       SpeedButton20.Down  then
       begin
         if FCreationScenarious>=0 then
          FreeAndNil(Fobj);
         Gizmo.Enabled := true;
         FCreationScenarious :=-1;
         SpeedButton16.Down := false;
         SpeedButton19.Down := false;
         SpeedButton20.Down := false;
         UpdateTreeView;
       end;

  end;

  if SpeedButton16.Down or SpeedButton19.Down or
     SpeedButton20.Down  then
  begin
     If (FCreationScenarious = 0) and (VectorLength(VectorSubtract(LostMousePos,MousePos))<0.1) then
     begin
        FreeAndNil(Fobj);
        SpeedButton16.Down := false;
        FCreationScenarious :=-1;
     end;
     LostMousePos := MouseWorldPos(x, y);
     FCreationScenarious := FCreationScenarious + 1;
     MouseMoving := true;
  end;

end;

procedure TFormGizmoEx.ViewerMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
  Gizmo.UpdateGizmo;

end;

procedure TFormGizmoEx.FormShow(Sender: TObject);
begin
  Viewer.SetFocus;
  Gizmo.RootGizmo := rootGizmo;
  Gizmo.RootObjects:= GLRootObjects;
  Gizmo.GizmoTmpRoot := RootTempObjects;
  Gizmo.ExcludeObjects := true;
  Gizmo.ExcludeObjectsList.Add('GLXYZGrid1');
  Gizmo.ExcludeObjectsList.Add('GLHUDText1');
  Camera.TurnAngle:=45;
  Camera.PitchAngle:=-45;
  UpdateTreeView;
end;


procedure TFormGizmoEx.edAutoZoomFactorKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key,['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', ',']) then
    Key := #0;
end;

procedure TFormGizmoEx.CheckBox12Click(Sender: TObject);
var
  check: Boolean;
begin
  check := (Sender as TCheckBox).Checked;
  case (Sender as TCheckBox).Tag of
    0: Gizmo.Enabled := Check;
    1: Gizmo.ExcludeObjects := Check;
    2: Gizmo.ExcludeClassname:= Check;
    3: Gizmo.EnableLoopCursorMoving := Check;
    4: Gizmo.EnableMultiSelection:= Check;

    5: Gizmo.EnableActionHistory:= Check;
    6: Gizmo.ShowBoundingBox := Check;
    7: Gizmo.ShowAxisLabel := Check;
    8:
    begin
        Gizmo.ShowObjectInfos := not Gizmo.ShowObjectInfos;
        CheckBox7.Enabled := Check;
        CheckBox8.Enabled := Check;
        CheckBox9.Enabled := Check;
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

    12: Gizmo.NoZWrite := Check;
    13: Gizmo.AntiAliasedLines := Check;
    14: Gizmo.CanChangeWithChildren := Check;
    15:
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
  end;
end;

procedure TFormGizmoEx.edtGizmoThicknessChange(Sender: TObject);
var
  value: Extended;
begin
  TryStrToFloat((Sender as TEdit).Text, value);
  if value > 0 then
  case (Sender as TEdit).Tag of
    1: Gizmo.GizmoThickness := value;
    2: Gizmo.ScaleCoef := value;
    3: Gizmo.MoveCoef := value;
    4: Gizmo.RotationCoef := value;
    5: Gizmo.HistoryStepsCount:=Round(value);
    6: Gizmo.AutoZoomFactor := value;
    7: Gizmo.ZoomFactor := value;
  end;
end;

procedure TFormGizmoEx.ColorBox1Change(Sender: TObject);
begin
  case (Sender as TColorBox).Tag of
    0: Gizmo.BoundingBoxColor.AsWinColor := ColorBox1.Selected;
    1: Gizmo.VisibleInfoLabelsColor.AsWinColor := ColorBox2.Selected;
    2: Gizmo.SelectedColor.AsWinColor := ColorBox3.Selected;
    3: Gizmo.SelectionRegionColor.AsWinColor := ColorBox4.Selected;
  end;
end;

procedure TFormGizmoEx.ComboBox3Change(Sender: TObject);
begin
  case ComboBox3.ItemIndex of
    0: Gizmo.InfoLabelCoordType := ilcChanging;
    1: Gizmo.InfoLabelCoordType := ilcChangeRate;
  end;
end;

procedure TFormGizmoEx.ComboBox4Change(Sender: TObject);
begin
  Gizmo.ReferenceCoordSystem:=TGLGizmoExReferenceCoordinateSystem(ComboBox4.ItemIndex);
end;

procedure TFormGizmoEx.FormCreate(Sender: TObject);
begin
  Gizmo := TGLGizmoEx.Create(Self);
  Gizmo.LabelFont := WindowsBitmapFont;
  Gizmo.Viewer := Viewer;
  Gizmo.ExcludeClassnameList.Add('TGLSphere');
  FCreationScenarious := -1;
end;

procedure TFormGizmoEx.FormDestroy(Sender: TObject);
begin
  Gizmo.Free;
end;

procedure TFormGizmoEx.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Gizmo.CanAddObjToSelectionList:=(key=VK_Control);
  Gizmo. CanRemoveObjFromSelectionList:=(Key=VK_MENU);
end;

procedure TFormGizmoEx.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Gizmo.CanAddObjToSelectionList:=false;
  Gizmo. CanRemoveObjFromSelectionList:=false;
end;

procedure TFormGizmoEx.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
  gizmo.UpdateGizmo;
end;

procedure TFormGizmoEx.Timer1Timer(Sender: TObject);
begin
  Panel1.Caption := Viewer.FramesPerSecondText();
  Viewer.ResetPerformanceMonitor;

  if GLScene1.IsUpdating then UpdateTreeView;
end;

end.
