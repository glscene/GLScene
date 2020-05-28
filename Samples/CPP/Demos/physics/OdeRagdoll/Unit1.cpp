//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLBitmapFont"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLHUDObjects"
#pragma link "GLMaterial"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLShadowPlane"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLWindowsFont"

#pragma resource "*.dfm"

TfRagDoll *fRagDoll;
//---------------------------------------------------------------------------
__fastcall TfRagDoll::TfRagDoll(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

__fastcall TWorld_ODE::TWorld_ODE(TObject *AOwner)
{
  TdMatrix3 R;
  ODEEnable = false;
  PhysTime = 0;
  //Create physic
  World = dWorldCreate();
  dWorldSetQuickStepNumIterations(World, 8);
  Space = dHashSpaceCreate(NULL);
///-  contactgroup = &dJointGroupCreate(0);
  dWorldSetGravity (World, 0, 0, -0.81);
  dWorldSetCFM (World, 1e-5);
  //Floor
  dCreatePlane (Space, 0, 0, 1, 0);
  //Box wall limit
  dCreatePlane (Space,  0, 1, 0, -50.00);
  dCreatePlane (Space,  1, 0, 0, -50.00);
  dCreatePlane (Space,  0,-1, 0, -50.00);
  dCreatePlane (Space, -1, 0, 0, -50.00);
  // Create 1 GLSCube and a box space.
  Ground_box = dCreateBox(Space,25,50,50);
  dRFromAxisAndAngle (R,0,1,0,0.95);
  dGeomSetPosition(Ground_box,32,5,0.5);
  dGeomSetRotation(Ground_box,R);
  Cube = (TGLCube*)(fRagDoll->ODEScene->AddNewChild(__classid(TGLCube)));
  PdxGeom(Ground_box)->data = Cube;
  CopyCubeSizeFromBox(Cube, Ground_box);
  PositionSceneObject((TGLBaseSceneObject*)(PdxGeom(Ground_box)->data), Ground_box);
  // Same Create 1 GLSCube and a box space.
  Ground_box2 = dCreateBox (Space,5,10,5);
  dRFromAxisAndAngle (R,0,1,0,0);
  dGeomSetPosition (Ground_box2,-12,-5,2.5);
  dGeomSetRotation (Ground_box2,R);
  Cube2 = (TGLCube*)(fRagDoll->ODEScene->AddNewChild(__classid(TGLCube)));
  PdxGeom(Ground_box2)->data = Cube2;
  CopyCubeSizeFromBox(Cube2, Ground_box2);
  PositionSceneObject((TGLBaseSceneObject*)(PdxGeom(Ground_box2)->data), Ground_box2);

  // Create now a sphere
  Ground_box2 = dCreateSphere (Space,5);
  dGeomSetPosition (Ground_box2,0,-15,2.5);
  PdxGeom(Ground_box2)->data = (TGLSphere*)(fRagDoll->ODEScene->AddNewChild(__classid(TGLSphere)));
///-  (TGLSphere*)(PdxGeom(Ground_box2)->data)->Radius = 5;
  PositionSceneObject((TGLSphere*)(PdxGeom(Ground_box2)->data), Ground_box2);
}
