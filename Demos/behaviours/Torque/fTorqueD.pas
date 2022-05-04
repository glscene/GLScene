unit fTorqueD;

interface

uses
  Winapi.OpenGL,
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Imaging.jpeg,

  GLS.Objects,
  GLS.Scene,
  GLS.PersistentClasses,
  GLS.Cadencer,
  GLS.SceneViewer,

  GLS.Coordinates,
  GLS.BaseClasses,
  GLS.Behaviours,
  GLS.HUDObjects,
  GLS.Color,
  GLS.BitmapFont,
  GLS.GeomObjects,
  GLS.RenderContextInfo,
  GLS.Utils,
  GLS.Mesh,
  GLS.VectorFileObjects;

type
  TFormTorque = class(TForm)
    GLSceneViewer1: TGLSceneViewer;
    GLScene1: TGLScene;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    GLBitmapFont1: TGLBitmapFont;
    Panel1: TPanel;
    PanelBottom: TPanel;
    CheckBox1: TCheckBox;
    lHexahedron: TLabel;
    lDodecahedron: TLabel;
    lOctagedron: TLabel;
    lTetrahedron: TLabel;
    lIcosahedron: TLabel;
    Tetrahedron: TGLTetrahedron;
    Octahedron: TGLOctahedron;
    Dodecahedron: TGLDodecahedron;
    Cube: TGLCube;
    Icosahedron: TGLIcosahedron;
    Hexahedron: TGLHexahedron;
    Torus: TGLTorus;
    Teapot: TGLTeapot;
    Superellipsoid: TGLSuperellipsoid;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    lastTime: Double;
    pickedObject: TGLBaseSceneObject;
  public
    //
  end;

var
  FormTorque: TFormTorque;

implementation

{$R *.DFM}

procedure TFormTorque.FormCreate(Sender: TObject);
begin
  // Load the font bitmap from media dir
  SetGLSceneMediaDir();
  GLBitmapFont1.Glyphs.LoadFromFile('darkgold_font.bmp');
  // Initialize last time
  lastTime := Now * 3600 * 24;
  // Initialize rotation dampings...
  // ...using properties...
  with GetOrCreateInertia(Cube.Behaviours).RotationDamping do
  begin
    Constant := 1;
    Linear := 1;
    Quadratic := 0;
  end;
  // ...using helper function on the TGLBehaviours...
  GetOrCreateInertia(Tetrahedron.Behaviours).RotationDamping.SetDamping(0, 0, 0.01);
  // ...or using helper function directly on the TGLBaseSceneObject
  GetOrCreateInertia(Octahedron.Behaviours).RotationDamping.SetDamping(0, 0, 0.01);
  GetOrCreateInertia(Hexahedron.Behaviours).RotationDamping.SetDamping(5, 0, 0.01);
  GetOrCreateInertia(Dodecahedron.Behaviours).RotationDamping.SetDamping(10, 0, 0.01);
  GetOrCreateInertia(Icosahedron.Behaviours).RotationDamping.SetDamping(0, 0, 0.01);

  GetOrCreateInertia(Torus.Behaviours).RotationDamping.SetDamping(10, 0, 0.01);
  GetOrCreateInertia(Superellipsoid.Behaviours).RotationDamping.SetDamping(10, 0, 0.01);
  GetOrCreateInertia(Teapot.Behaviours).RotationDamping.SetDamping(0, 0, 0.01);
end;

procedure TFormTorque.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pickedObject: TGLCustomSceneObject;
  oldColor: TGLColorVector;
  rci: TGLRenderContextInfo;
begin
  // if an object is picked...
  pickedObject := (GLSceneViewer1.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject);
  if Assigned(pickedObject) then
  begin
    oldColor := pickedObject.Material.FrontProperties.Emission.Color;
    //...turn it to yellow and show its name
    pickedObject.Material.FrontProperties.Emission.Color := clrYellow;
    ShowMessage('You clicked the ' + pickedObject.Name);
    pickedObject.BuildList(rci);
///    HUDText.Text := 'Calculated Volume:+ '#13#10 + 'Vertices:'#13#10#13#10 + 'Faces:'#13#10#13#10 + 'Edges:';
    pickedObject.Material.FrontProperties.Emission.Color := oldColor;
  end;
end;

procedure TFormTorque.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // Mouse moved, get what's underneath
  pickedObject := GLSceneViewer1.Buffer.GetPickedObject(X, Y);
end;

procedure TFormTorque.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  // apply some "torque" to the pickedObject if any
  if Assigned(pickedObject) then
    GetOrCreateInertia(pickedObject).ApplyTorque(deltaTime, 200, 0, 0);
  GLSceneViewer1.Invalidate;
end;

procedure TFormTorque.CheckBox1Click(Sender: TObject);
var
  i: Integer;
  mass: Single;
begin
  if CheckBox1.Checked then
    mass := 2
  else
    mass := 1;
  // all our objects are child of the DummyCube1
  for i := 0 to DummyCube1.Count - 1 do
    GetOrCreateInertia(DummyCube1.Children[i]).mass := mass;
end;

end.
