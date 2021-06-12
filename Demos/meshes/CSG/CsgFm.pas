unit CsgFm;

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
  GLS.VectorGeometry,
  GLS.Utils;

type
  TFormCsg = class(TForm)
    GLScene1: TGLScene;
    GLFreeForm1: TGLFreeForm;
    GLCamera1: TGLCamera;
    GLSceneViewer1: TGLSceneViewer;
    GLMaterialLibrary1: TGLMaterialLibrary;
    GLFreeForm2: TGLFreeForm;
    GLFreeForm3: TGLFreeForm;
    Panel1: TPanel;
    btnClear: TButton;
    btnUnion: TButton;
    btnSubtractAB: TButton;
    btnSubtractBA: TButton;
    btnIntersect: TButton;
    CheckBox1: TCheckBox;
    GLLightSource1: TGLLightSource;
    GLDummyCube1: TGLDummyCube;
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
// Demo starts here above is just navigation.
    procedure btnClearClick(Sender: TObject);
    procedure btnUnionClick(Sender: TObject);
    procedure btnSubtractABClick(Sender: TObject);
    procedure btnSubtractBAClick(Sender: TObject);
    procedure btnIntersectClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
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

// Demo starts here above is just navigation.
procedure TFormCsg.FormCreate(Sender: TObject);
begin
  SetGLSceneMediaDir();
  // scaled 40
  GLFreeForm1.LoadFromFile('polyhedron.3ds');

  // scaled 20, position.x = 16
  GLFreeForm2.LoadFromFile('polyhedron.3ds');
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

procedure TFormCsg.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(1.1);
end;

procedure TFormCsg.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  GLCamera1.AdjustDistanceToTarget(1/1.1);
end;

procedure TFormCsg.btnClearClick(Sender: TObject);
begin
  GLFreeForm3.MeshObjects.Clear;
  GLFreeForm3.StructureChanged;

  GLFreeForm1.Material.PolygonMode := pmFill;
  GLFreeForm2.Material.PolygonMode := pmFill;
end;

procedure TFormCsg.btnUnionClick(Sender: TObject);
var
  Mesh : TMeshObject;
begin
  btnClearClick(Sender);

  if GLFreeForm3.MeshObjects.Count = 0 then
    TMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;

  Mesh := GLFreeForm3.MeshObjects[0];

  CSG_Operation(GLFreeForm1.MeshObjects.Items[0],GLFreeForm2.MeshObjects.Items[0],CSG_Union,Mesh,'1','2');
  GLFreeForm3.StructureChanged;

  GLFreeForm1.Material.PolygonMode := pmLines;
  GLFreeForm2.Material.PolygonMode := pmLines;
end;

procedure TFormCsg.btnSubtractABClick(Sender: TObject);
var
  Mesh : TMeshObject;
begin
  btnClearClick(Sender);

  if GLFreeForm3.MeshObjects.Count = 0 then
    TMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;

  Mesh := GLFreeForm3.MeshObjects[0];

  CSG_Operation(GLFreeForm1.MeshObjects.Items[0],GLFreeForm2.MeshObjects.Items[0],CSG_Subtraction,Mesh,'1','2');
  GLFreeForm3.StructureChanged;

  GLFreeForm1.Material.PolygonMode := pmLines;
  GLFreeForm2.Material.PolygonMode := pmLines;
end;

procedure TFormCsg.btnSubtractBAClick(Sender: TObject);
var
  Mesh : TMeshObject;
begin
  btnClearClick(Sender);

  if GLFreeForm3.MeshObjects.Count = 0 then
    TMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;

  Mesh := GLFreeForm3.MeshObjects[0];

  CSG_Operation(GLFreeForm2.MeshObjects.Items[0],GLFreeForm1.MeshObjects.Items[0],CSG_Subtraction,Mesh,'1','2');
  GLFreeForm3.StructureChanged;

  GLFreeForm1.Material.PolygonMode := pmLines;
  GLFreeForm2.Material.PolygonMode := pmLines;
end;

procedure TFormCsg.btnIntersectClick(Sender: TObject);
var
  Mesh : TMeshObject;
begin
  btnClearClick(Sender);

  if GLFreeForm3.MeshObjects.Count = 0 then
    TMeshObject.CreateOwned(GLFreeForm3.MeshObjects).Mode := momFaceGroups;

  Mesh := GLFreeForm3.MeshObjects[0];

  CSG_Operation(GLFreeForm1.MeshObjects.Items[0],GLFreeForm2.MeshObjects.Items[0],CSG_Intersection,Mesh,'1','2');
  GLFreeForm3.StructureChanged;

  GLFreeForm1.Material.PolygonMode := pmLines;
  GLFreeForm2.Material.PolygonMode := pmLines;
end;

procedure TFormCsg.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    GLMaterialLibrary1.Materials[0].Material.PolygonMode := pmFill;
    GLMaterialLibrary1.Materials[1].Material.PolygonMode := pmFill;
  end
  else
  begin
    GLMaterialLibrary1.Materials[0].Material.PolygonMode := pmLines;
    GLMaterialLibrary1.Materials[1].Material.PolygonMode := pmLines;
  end;
  GLFreeForm3.StructureChanged;
end;

end.
