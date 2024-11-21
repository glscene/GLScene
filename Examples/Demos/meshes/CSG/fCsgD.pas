unit fCsgD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.Dialogs,
  Vcl.StdCtrls,


  GLS.Scene,
  GLS.PersistentClasses,
  GLS.VectorFileObjects,
  GLS.MeshBSP,
  GLS.MeshCSG,
  GLS.SceneViewer,
  GLS.Objects,
  GLS.Texture,
  GLS.File3DS,

  GLS.Material,
  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.State,
  Stage.VectorGeometry,
  Stage.Utils;

type
  TFormCsg = class(TForm)
    GLScene1: TGLScene;
    FF_A: TGLFreeForm;
    GLCamera1: TGLCamera;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    FF_B: TGLFreeForm;
    FF_C: TGLFreeForm;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
    PanelLeft: TPanel;
    chbSolidResult: TCheckBox;
    btnReset: TButton;
    gbVisibility: TGroupBox;
    chbA: TCheckBox;
    chbB: TCheckBox;
    chbC: TCheckBox;
    rgOperation: TRadioGroup;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure chbSolidResultClick(Sender: TObject);
    procedure chbClick(Sender: TObject);
    procedure rgOperationClick(Sender: TObject);
  private
  public
    mx : Integer;
    my : Integer;
    Drag : Boolean;
  end;

var
  FormCsg: TFormCsg;

implementation

{$R *.dfm}

procedure TFormCsg.FormCreate(Sender: TObject);
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\model');
  // scaled 40
  FF_A.LoadFromFile('polyhedron.3ds');

  // scaled 20, position.x = 16
  FF_B.LoadFromFile('polyhedron.3ds');
end;

//
// Boolean operations
//
procedure TFormCsg.rgOperationClick(Sender: TObject);
begin
  FF_C.MeshObjects.Clear;

  if FF_C.MeshObjects.Count = 0 then
    TGLMeshObject.CreateOwned(FF_C.MeshObjects).Mode := momFaceGroups;

  case rgOperation.ItemIndex of
    0: CSG_Operation(FF_A.MeshObjects.Items[0], FF_B.MeshObjects.Items[0],
           CSG_Union, FF_C.MeshObjects[0], '1', '2');
    1: CSG_Operation(FF_A.MeshObjects.Items[0], FF_B.MeshObjects.Items[0],
           CSG_Subtraction, FF_C.MeshObjects[0], '1', '2');
    2: CSG_Operation(FF_B.MeshObjects.Items[0], FF_A.MeshObjects.Items[0],
           CSG_Subtraction, FF_C.MeshObjects[0], '1', '2');
    3: CSG_Operation(FF_A.MeshObjects.Items[0], FF_B.MeshObjects.Items[0],
    CSG_Intersection, FF_C.MeshObjects[0],'1','2');
  end;

  FF_A.Material.PolygonMode := pmLines;
  FF_B.Material.PolygonMode := pmLines;
  FF_C.StructureChanged;
  GLSceneViewer1.Invalidate;
end;

procedure TFormCsg.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Drag := true;
end;

procedure TFormCsg.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Drag := false;
end;

procedure TFormCsg.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Drag then
  begin
    GLCamera1.MoveAroundTarget(my-Y,mx-X);
  end;
  mx := X;
  my := Y;
end;

procedure TFormCsg.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(1/1.1);
end;

procedure TFormCsg.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(1.1);
end;


procedure TFormCsg.chbClick(Sender: TObject);
begin
  FF_A.Visible := chbA.Checked;
  FF_B.Visible := chbB.Checked;
  FF_C.Visible := chbC.Checked;
end;

procedure TFormCsg.chbSolidResultClick(Sender: TObject);
begin
  if chbSolidResult.Checked then
  begin
    GLMaterialLibrary1.Materials[0].Material.PolygonMode := pmFill;
    GLMaterialLibrary1.Materials[1].Material.PolygonMode := pmFill;
  end
  else
  begin
    GLMaterialLibrary1.Materials[0].Material.PolygonMode := pmLines;
    GLMaterialLibrary1.Materials[1].Material.PolygonMode := pmLines;
  end;
  FF_C.StructureChanged;
  GLSceneViewer1.Invalidate;
end;

procedure TFormCsg.btnResetClick(Sender: TObject);
begin
  FF_C.MeshObjects.Clear;
  FF_C.StructureChanged;

  FF_A.Visible := True;  chbA.Checked := True;
  FF_B.Visible := True;  chbB.Checked := True;
  FF_C.Visible := True;  chbC.Checked := True;
  chbSolidResult.Checked := True;

  FF_A.Material.PolygonMode := pmFill;
  FF_B.Material.PolygonMode := pmFill;

  rgOperation.ItemIndex := 0;
  GLSceneViewer1.Invalidate;
end;

end.
