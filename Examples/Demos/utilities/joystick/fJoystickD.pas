unit fJoystickD;

interface

uses
  System.SysUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  GLS.Scene,
  GLS.Objects,
  GLS.PersistentClasses,
  GLS.Joystick,
  GLS.GeomObjects,
  GLS.Cadencer,
  GLS.SceneViewer,
 
  GLS.Coordinates,
  GLS.BaseClasses;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    Joystick1: TGLJoystick;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    DummyCube1: TGLDummyCube;
    Cube1: TGLCube;
    Cylinder1: TGLCylinder;
    Sphere1: TGLSphere;
    DummyCube2: TGLDummyCube;
    Sphere2: TGLSphere;
    Sphere3: TGLSphere;
    Sphere4: TGLSphere;
    DummyCube3: TGLDummyCube;
    GLCadencer1: TGLCadencer;
    procedure FormCreate(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure Joystick1JoystickButtonChange(Sender: TObject; JoyID: TJoystickID;
      Buttons: TJoystickButtons; XDeflection, YDeflection: Integer);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
   if Joystick1.Capture=False then begin
      ShowMessage('Connect a Joystick!');
      Application.Terminate;
      Exit;
   end;
   // setup default sphere colors
   Joystick1JoystickButtonChange(Self, Joystick1.JoystickID, [], 0, 0);
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
   // Rotate our 3d stick (=cylinder), a dummycube is used as its rotation axis
   DummyCube1.PitchAngle:=-Joystick1.XPosition/3;
   DummyCube1.RollAngle:=Joystick1.YPosition/3;
end;

procedure TForm1.Joystick1JoystickButtonChange(Sender: TObject;
  JoyID: TJoystickID; Buttons: TJoystickButtons; XDeflection,
  YDeflection: Integer);
const
   cPressedColor : array [False..True] of Integer = (clGray, clWhite);
var
   button : TJoystickButton;
   i : Integer;
begin
   // Browse all buttons and adjusts matching spheres color
  // All the spheres are accessed in an arrayed fashion (I made them
  // child of a single dummycube)
  i := 0;
  for button := jbButton1 to jbButton4 do
  begin
    with TGLSphere(DummyCube2.Children[i]).Material.FrontProperties.Diffuse do
      AsWinColor := cPressedColor[button in Buttons];
    Inc(i);
  end;
end;

end.
