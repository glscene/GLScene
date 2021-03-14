unit NewtonDensityFm;

interface

uses
  Winapi.OpenGL,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Samples.Spin,
  
  GLS.Scene,
  GLS.VectorTypes,
  Physics.NGDManager,
  GLS.Objects,
  GLS.PersistentClasses,
  GLS.Coordinates,
  GLS.SimpleNavigation,
  GLS.Cadencer,
  GLS.SceneViewer,
 
  GLS.BaseClasses,
  GLS.VectorGeometry,
  GLS.HUDObjects,
  GLS.BitmapFont,
  GLS.WindowsFont,
  Imports.NGD,
  GLS.GeomObjects;

type
  TFormNewtonDensity = class(TForm)
    GLScene1: TGLScene;
    GLSceneViewer1: TGLSceneViewer;
    GLCadencer1: TGLCadencer;
    GLSimpleNavigation1: TGLSimpleNavigation;
    GLCamera1: TGLCamera;
    GLLightSource1: TGLLightSource;
    GLPlane1: TGLPlane;
    Mag: TGLDummyCube;
    SubMarine: TGLCube;
    GLPaperSphere: TGLSphere;
    GLLeadSphere: TGLSphere;
    GLCube1: TGLCube;
    SpinEdit1: TSpinEdit;
    HTLiquidDensity: TGLHUDText;
    GLNGDManager1: TGLNGDManager;
    SpinEdit2: TSpinEdit;
    HTLinearViscosity: TGLHUDText;
    HTAngularViscosity: TGLHUDText;
    SpinEdit3: TSpinEdit;
    GLCube2: TGLCube;
    GLCone1: TGLCone;
    GLCylinder1: TGLCylinder;
    obj: TGLDummyCube;
    GLCapsule1: TGLCapsule;
    GLWindowsBitmapFont1: TGLWindowsBitmapFont;
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
  public
    procedure MyForceAndTorqueDensity(const cbody: PNewtonBody;
      timestep: NGDFloat; threadIndex: Integer);
    procedure Shoot;
  end;

var
  FormNewtonDensity: TFormNewtonDensity;

implementation

{$R *.dfm}

function BuoyancyPlaneCallback(const collisionID: Integer; context: Pointer;
  const globalSpaceMatrix: PNGDFloat; globalSpacePlane: PNGDFloat): Integer;
  cdecl;
var
  BodyMatrix: TGLMatrix;
  PlaneEquation: TGLVector;
  pv: PGLVector;
  MyForm: TFormNewtonDensity;
begin
  // Get the matrix of the actual body
  BodyMatrix := PMatrix(globalSpaceMatrix)^;

  MyForm := TFormNewtonDensity(context);

  // this is the 4-value vector that represents the plane equation for
  // the buoyancy surface
  // This can be used to simulate boats and lighter than air vehicles etc..
  PlaneEquation := MyForm.GLPlane1.Direction.AsVector;
  // the distance along this normal, to the origin.
  PlaneEquation.W := MyForm.GLPlane1.Position.Y;
  PGLVector(globalSpacePlane)^ := PlaneEquation;
  Result := 1;
end;

procedure TFormNewtonDensity.Shoot;
var
  Ball: TGLCube;
  NGDDyn: TGLNGDDynamic;
begin
  Ball := TGLCube.CreateAsChild(Mag);
  Ball.CubeWidth := 0.5;
  Ball.CubeHeight := 0.5;
  Ball.CubeDepth := 0.5;
  Ball.AbsolutePosition := GLCamera1.AbsolutePosition;
  NGDDyn := GetOrCreateNGDDynamic(Ball);
  NGDDyn.Manager := GLNGDManager1;
  NGDDyn.Density := 10;
  NGDDyn.UseGravity := false;
  NGDDyn.LinearDamping := 0;

  // Add impulse in the camera direction
  NGDDyn.AddImpulse(VectorScale(GLCamera1.AbsoluteVectorToTarget, 100),
    Ball.AbsolutePosition);

end;

procedure TFormNewtonDensity.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  // To use Buoyancy effect, set a custom forceAndTorqueEvent were you can call
  // NewtonBodyAddBuoyancyForce API function
  for I := 0 to obj.Count - 1 do
    GetNGDDynamic(obj[I]).CustomForceAndTorqueEvent := MyForceAndTorqueDensity;

end;

procedure TFormNewtonDensity.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
begin
  GLNGDManager1.Step(deltaTime);
end;

procedure TFormNewtonDensity.GLSceneViewer1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton(mbMiddle) then
    Shoot;
end;

procedure TFormNewtonDensity.MyForceAndTorqueDensity(const cbody: PNewtonBody;
  timestep: NGDFloat; threadIndex: Integer);
var
  worldGravity: TGLVector;
  NGDDyn: TGLNGDDynamic;
  fluidDensity, fluidLinearViscosity, fluidAngularViscosity: Single;
begin
  worldGravity := GLNGDManager1.Gravity.AsVector;
  NGDDyn := TGLNGDDynamic(NewtonBodyGetUserData(cbody));

  // Add gravity to body: Weight= mass*gravity
  ScaleVector(worldGravity, NGDDyn.mass);
  NewtonBodyAddForce(cbody, @worldGravity);

  fluidDensity := SpinEdit1.Value;
  fluidLinearViscosity := SpinEdit2.Value / 10;
  fluidAngularViscosity := SpinEdit3.Value / 10;

  // We send Self as context for the callback
  NewtonBodyAddBuoyancyForce(cbody, fluidDensity / NGDDyn.mass,
    fluidLinearViscosity, fluidAngularViscosity, @worldGravity,
    @BuoyancyPlaneCallback, self);
end;

end.
