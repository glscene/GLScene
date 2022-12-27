unit fOdeMachineD;

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
  GLS.SceneViewer,
  Physics.ODEManager,
  GLS.VectorGeometry,
  GLS.Cadencer,
  GLS.GeomObjects,
  GLS.HUDObjects,
  GLS.BitmapFont,
  GLS.WindowsFont,
 
  GLS.Coordinates,
  GLS.BaseClasses,
  Physics.ODEImport;

type
  TFormOdeMachine = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCamera1: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    GLODEManager1: TGLODEManager;
    GLODEJointList1: TGLODEJointList;
    Machine: TGLDummyCube;
    Axle: TGLCylinder;
    GLLightSource1: TGLLightSource;
    Wheel: TGLCylinder;
    Pin1: TGLCylinder;
    Arm: TGLCube;
    Slider: TGLCube;
    Pin2: TGLCylinder;
    GLCadencer1: TGLCadencer;
    ODERenderPoint: TGLRenderPoint;
    GLHUDText1: TGLHUDText;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    procedure GLSceneViewer1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewer1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLCadencer1Progress(Sender: TObject; const deltaTime,
      newTime: Double);
  private

  public
    mx, my: Integer;
  end;

var
  FormOdeMachine: TFormOdeMachine;

//---------------------------------------
implementation
//---------------------------------------

{$R *.dfm}


procedure TFormOdeMachine.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mx := X;
  my := Y;
end;

procedure TFormOdeMachine.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    GLCamera1.MoveAroundTarget(my - Y, mx - X);
  mx := X;
  my := Y;
end;

procedure TFormOdeMachine.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
var
  velWheel,
  velPin2 : PdVector3;

begin
  GLODEManager1.Step(deltaTime);
  velWheel := dBodyGetAngularVel(TGLODEDynamic(Wheel.Behaviours[0]).Body);
  velPin2 := dBodyGetLinearVel(TGLODEDynamic(Pin2.Behaviours[0]).Body);
  GLHUDText1.Text := Format('Wheel Angular Velocity (Y-Axis) = %.1f' + #13#10 +
    'Pin2 Linear Velocity (X-Axis) = %.1f', [velWheel[1], velPin2[0]]);
end;

end.
