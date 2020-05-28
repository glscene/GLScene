unit Unit1;

interface

uses
  Winapi.OpenGL,
  System.Classes,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  
  GLScene,
  GLObjects,
  GLTexture,
  GLWin32Viewer,
  GLGeomObjects,
  GLColor,
  GLCrossPlatform,
  GLCoordinates,
  GLBaseClasses;

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
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
     
    oldPick: TGLCustomSceneObject;
  public
     
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

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
    ShowMessage('You clicked the ' + pickedObject.Name);
  end;
end;

end.
