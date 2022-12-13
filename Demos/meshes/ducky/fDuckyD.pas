unit fDuckyD;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  Vcl.StdCtrls,

  
  GLS.Scene,
  GLS.VectorTypes,
  GLS.PersistentClasses,
  GLS.VectorFileObjects,
  GLS.Objects,
  GLS.SceneViewer,
  GLS.ParametricSurfaces,
  GLS.VectorGeometry,
  GLS.VectorLists,
  GLS.Texture,
 
  GLS.Coordinates,
  GLS.Material,
  GLS.State,
  GLS.BaseClasses,
  GLS.FileNurbs,
  GLS.Utils;

type
  TFormDucky = class(TForm)
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Panel1: TPanel;
    GLSceneViewer1: TGLSceneViewer;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    GLActor1: TGLActor;
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
     
 public

    mx, my: Integer;
  end;

var
  FormDucky: TFormDucky;

implementation

{$R *.dfm}

procedure TFormDucky.FormCreate(Sender: TObject);
var
  cp: TGLAffineVectorList;
begin
  var Path: TFileName := GetCurrentAssetPath();
  SetCurrentDir(Path  + '\model');

  // Load the nurbs data
  GLActor1.LoadFromFile('duck1.nurbs');
  GLActor1.AddDataFromFile('duck2.nurbs');
  GLActor1.AddDataFromFile('duck3.nurbs');

  { Translate Actor based on the first mesh object's average
    control point. Quick and dirty ... or maybe just dirty :P }
  cp := TMOParametricSurface(GLActor1.MeshObjects[0]).ControlPoints;
  GLActor1.Position.Translate(VectorNegate(VectorScale(cp.Sum, 1 / cp.Count)));
end;

procedure TFormDucky.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormDucky.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TFormDucky.TrackBar1Change(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 2 do
    TMOParametricSurface(GLActor1.MeshObjects[i]).Resolution :=
      TrackBar1.Position;
  GLActor1.StructureChanged;
end;

procedure TFormDucky.CheckBox1Click(Sender: TObject);
begin
  with GLActor1.Material do
  begin
    if CheckBox1.Checked then
    begin
      PolygonMode := pmLines;
      FaceCulling := fcNoCull;
    end
    else
    begin
      PolygonMode := pmFill;
      FaceCulling := fcBufferDefault;
    end;
  end;
end;

end.
