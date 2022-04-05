//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "fNewtonDensityC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.GeomObjects"
#pragma link "GLS.HUDObjects"
#pragma link "Physics.NGDManager"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SimpleNavigation"
#pragma link "GLS.SceneViewer"

#pragma link "GLS.BitmapFont"
#pragma resource "*.dfm"
TForm1 *Form1;



int __cdecl BuoyancyPlaneCallback(const int collisionID, void *context,
  const PdFloat globalSpaceMatrix, PdFloat globalSpacePlane)
{
  TGLMatrix *BodyMatrix;
  TGLVector PlaneEquation;
  PGLVector pv;
  TForm1 *MyForm;

  // Get the matrix of the actual body

  BodyMatrix = (PGLMatrix) globalSpaceMatrix;
  MyForm = (TForm1 *) context;

  // this is the 4-value vector that represents the plane equation for
  // the buoyancy surface
  // This can be used to simulate boats and lighter than air vehicles etc..
  PlaneEquation = MyForm->GLPlane1->Direction->AsVector;
  // the distance along this normal, to the origin.
  PlaneEquation.W = MyForm->GLPlane1->Position->Y;
  globalSpacePlane = PlaneEquation.V;

  return 1;
}

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  // To use Buoyancy effect, set a custom forceAndTorqueEvent were you can call
  // NewtonBodyAddBuoyancyForce API function
  for (int i = 0; i < obj->Count - 1; i++)
  {
	GetNGDDynamic(obj->Children[i])->CustomForceAndTorqueEvent =
	  NULL; //must be MyForceAndTorqueDensity();
  }
}
//---------------------------------------------------------------------------
void TForm1::Shoot(void)
{
  TGLCube *Ball;
  TGLNGDDynamic *NGDDyn;

  Ball = (TGLCube *)(Mag->AddNewChild(__classid(TGLCube)));
  Ball->CubeWidth = 0.5;
  Ball->CubeHeight = 0.5;
  Ball->CubeDepth = 0.5;
  Ball->AbsolutePosition = GLCamera1->AbsolutePosition;
  NGDDyn = GetOrCreateNGDDynamic(Ball);
  NGDDyn->Manager = GLNGDManager1;
  NGDDyn->Density = 10;
  NGDDyn->UseGravity = false;
  NGDDyn->LinearDamping = 0;

  // Add impulse in the camera direction
  NGDDyn->AddImpulse(VectorScale(GLCamera1->AbsoluteVectorToTarget(), 100),
	Ball->AbsolutePosition);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  GLNGDManager1->Step(deltaTime);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  if (Button = TMouseButton(mbMiddle))
	Shoot();
}
//---------------------------------------------------------------------------

void TForm1::MyForceAndTorqueDensity(const PNewtonBody cbody,
	  dFloat timestep, int threadIndex)
{
  TGLVector worldGravity;
  TGLNGDDynamic  *NGDDyn;
  float fluidDensity, fluidLinearViscosity, fluidAngularViscosity;

  worldGravity = GLNGDManager1->Gravity->AsVector;
  NGDDyn = (TGLNGDDynamic *)NewtonBodyGetUserData(cbody);

  // Add gravity to body: Weight= Mass*Gravity
  ScaleVector(worldGravity, NGDDyn->Mass);

  NewtonBodyAddForce(cbody, worldGravity.V);

  fluidDensity = SpinEdit1->Value;
  fluidLinearViscosity = SpinEdit2->Value / 10;
  fluidAngularViscosity = SpinEdit3->Value / 10;

  // We send Self as context for the callback

  NewtonBodyAddBuoyancyForce(cbody, fluidDensity / NGDDyn->Mass,
	fluidLinearViscosity, fluidAngularViscosity, worldGravity.V,
	BuoyancyPlaneCallback, Owner);

}





