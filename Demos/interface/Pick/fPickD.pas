unit fPickD;

interface

uses
  Winapi.OpenGL,
  System.Classes,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,

  GLS.VectorTypes,
  GLS.Scene,
  GLS.Objects,
  GLS.Texture,
  GLS.SceneViewer,
  GLS.GeomObjects,
  GLS.Color,

  GLS.Coordinates,
  GLS.BaseClasses, GLS.SimpleNavigation;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLLightSource1: TGLLightSource;
    Sphere: TGLSphere;
    Cylinder: TGLCylinder;
    Torus: TGLTorus;
    Cone: TGLCone;
    Points: TGLPoints;
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    Color: TVector3f;
    oldPick: TGLCustomSceneObject;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
  NumPoints: Integer;
  X, Y, Z: Single;

begin
  NumPoints := 10000;
  Points.Size := 5.0;
  Points.Style := psSmooth;
  for I := 0 to NumPoints - 1 do
  begin
    // add positions of Points
    X := Random(20) - 10;
    Y := Random(20) - 10;
    Z := Random(20) - 10;
    Points.Positions.Add(X * 0.05, Y * 0.05, Z * 0.05);
    // add colors of Points
    Color.X := Random();
    Color.Y := Random();
    Color.Z := Random();
    Points.Colors.AddPoint(Color);
  end;
end;


procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  pickedObject: TGLCustomSceneObject;
begin
  // find what's under the mouse
  pickedObject := (GLSceneViewer1.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject);
  // if it has changed since last MouseMove...
  if (pickedObject <> oldPick) then
  begin
    // ...turn to black previous "hot" object...
    if Assigned(oldPick) then
      oldPick.Material.FrontProperties.Emission.Color := clrBlack;
    // ...and heat up the new selection...
    if Assigned(pickedObject) then
      pickedObject.Material.FrontProperties.Emission.Color := clrRed;
    // ...and don't forget it !
    oldPick := pickedObject;
  end;

end;

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pickedObject: TGLCustomSceneObject;
begin
  // if an object is picked...
  pickedObject := (GLSceneViewer1.Buffer.GetPickedObject(X, Y) as TGLCustomSceneObject);
  if Assigned(pickedObject) then
  begin
    // ...turn it to yellow and show its name
    pickedObject.Material.FrontProperties.Emission.Color := clrYellow;
    if (pickedObject is TGLPoints) then
    begin
      Points.Colors.Clear;
      Points.Colors.Add(1,1,0,1);
    end;

    ShowMessage('You clicked the ' + pickedObject.Name);
  end;
end;

end.
