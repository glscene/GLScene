//---------------------------------------------------------------------------

#include <vcl.h>
#include <stdlib.h>
#include <math.hpp>
#pragma hdrstop

#include "fOdeFurballC.h"

#pragma link "Stage.VectorGeometry"
#pragma link "Stage.Keyboard"
#pragma link "GLS.ODEManager"
#pragma link "GLS.Navigator"
#pragma link "GLS.ShadowPlane"
#pragma link "GLS.Extrusion"
#pragma link "GLS.Texture"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.VerletTypes"

//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------

#pragma link "GLS.BaseClasses"
#pragma link "GLS.Coordinates"

#pragma resource "*.dfm"
TFormFurBall *FormFurBall;


//---------------------------------------------------------------------------
float random(void)
{
  return (float)(rand() & 0xFFF) / (float)0xFFF;
}

//---------------------------------------------------------------------------
void __cdecl nearCallback(void *data, PdxGeom o1, PdxGeom o2)
{
  const int cCOL_MAX = 1;

  int i, numc;
  PdxBody b1, b2;
  TdContact contact[cCOL_MAX];
  TdJointID c;

  // exit without doing anything if the two bodies are connected by a joint
  b1 = dGeomGetBody(o1);
  b2 = dGeomGetBody(o2);
  if(b1 && b2 && (dAreConnected(b1, b2) != 0))
	return;

  for(i = 0; i <= cCOL_MAX - 1; i++)
  {
	contact[i].surface.mode = dContactBounce;

	// This determines friction, play around with it!
	contact[i].surface.mu = 3;  //10e9; //dInfinity; SHOULD BE INFINITY!
	contact[i].surface.mu2 = 0;
	contact[i].surface.bounce = 0.5;
	contact[i].surface.bounce_vel = 0.1;
  }

  numc = dCollide(o1, o2, cCOL_MAX, contact[0].geom, sizeof(TdContact));
  if(numc > 0)
  {
	for(i = 0; i <= numc - 1; i++)
	{
	  c = dJointCreateContact(FormFurBall->world, FormFurBall->contactgroup, &contact[i]);
	  dJointAttach(c, b1, b2);
	}
  }
}

//---------------------------------------------------------------------------
__fastcall TFormFurBall::TFormFurBall(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

const float cOffset = 0.03;

void __fastcall TFormFurBall::FormCreate(TObject *Sender)
{
  Show();

  Randomize();

  world = dWorldCreate();
  space = dHashSpaceCreate(NULL);
  contactgroup = dJointGroupCreate (1000000);
  dWorldSetGravity (world,0,0,-9.81);

  CreateODEPlaneFromGLPlane(GLShadowPlane_Floor, space);
  CreateODEPlaneFromGLPlane(GLShadowPlane_Floor2, space);
  CreateODEPlaneFromGLPlane(GLShadowPlane_Wall, space);
  CreateODEPlaneFromGLPlane(GLShadowPlane_Wall2, space);
  CreateODEPlaneFromGLPlane(GLShadowPlane_Wall3, space);
  // dCreatePlane (space,0,0,1,0);

  VerletWorld = new TGLVerletWorld;
  VerletWorld->Iterations = 2;
  VerletWorld->VerletNodeClass = __classid(TGLVerletNode);

  CheckBox_FurGravityClick(Sender);
  CheckBox_WindResistenceClick(Sender);

  CreateVerletPlaneFromGLPlane(GLShadowPlane_Floor, VerletWorld, cOffset);
  CreateVerletPlaneFromGLPlane(GLShadowPlane_Floor2, VerletWorld, cOffset);
  CreateVerletPlaneFromGLPlane(GLShadowPlane_Wall, VerletWorld, cOffset);
  CreateVerletPlaneFromGLPlane(GLShadowPlane_Wall2, VerletWorld, cOffset);
  CreateVerletPlaneFromGLPlane(GLShadowPlane_Wall3, VerletWorld, cOffset);

  HairList = new TList;

  CreateBall();
}

//---------------------------------------------------------------------------
void __fastcall TFormFurBall::FormClose(TObject *Sender, TCloseAction &Action)
{
  GLCadencer1->Enabled = false;
  dJointGroupDestroy(contactgroup);
  dSpaceDestroy(space);
  dWorldDestroy(world);

  CloseODE();
}

//---------------------------------------------------------------------------
double angle = 0;

void __fastcall TFormFurBall::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  const float cTIME_STEP = 0.01;

  int i, j;
  float Delta;
  TGLVerletHair *Hair;
  TGLLines *GLLines;

  Delta = deltaTime;
  angle = angle + Delta * 3;

  while(PhysicsTime < newTime)
  {
	PhysicsTime = PhysicsTime + cTIME_STEP;

	if(!CheckBox_LockBall->Checked)
	{
	  dSpaceCollide(space, NULL, nearCallback);
	  dWorldStep(world, cTIME_STEP);    //}
	  // remove all contact joints
	  dJointGroupEmpty(contactgroup);

	  if(IsKeyDown(VK_UP))
		dBodyAddForce(odeFurBallBody, 0, 0, 2.5);

	  else if(IsKeyDown(VK_DOWN))
		dBodyAddForce(odeFurBallBody, 0, 0, -2.5);

	  if(IsKeyDown('A'))
		dBodyAddForce(odeFurBallBody, 0, -1, 0);

	  else if(IsKeyDown('D'))
		dBodyAddForce(odeFurBallBody, 0, 1, 0);

	  if(IsKeyDown('W'))
		dBodyAddForce(odeFurBallBody, -1, 0, 0);

	  else if(IsKeyDown('S'))
		dBodyAddForce(odeFurBallBody, 1, 0, 0);
	}

	PositionSceneObject(FurBall, odeFurBallGeom);
	VCSphere->Location = FurBall->Position->AsAffineVector;
	VerletWorld->Progress(cTIME_STEP, PhysicsTime);
  }

  for(i = 0; i <= HairList->Count - 1; i++)
  {
	Hair = (TGLVerletHair *) HairList->Items[i];
	GLLines = (TGLLines *) Hair->Data;
	for(j = 1; j <= Hair->NodeList->Count - 1; j++)
	  GLLines->Nodes->Items[j - 1]->AsAffineVector =
		Hair->NodeList->Items[j]->Location;
  }
}

//---------------------------------------------------------------------------

void __fastcall TFormFurBall::DC_LightHolderProgress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  DC_LightHolder->Roll(deltaTime*M_PI*2*8);
}

//---------------------------------------------------------------------------

void __fastcall TFormFurBall::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  if (Shift.Contains(ssLeft))
	GLCamera1->MoveAroundTarget(mY-Y, mX-X);

  mX = X;
  mY = Y;
}

//---------------------------------------------------------------------------
const int cRadiusMultiplier = 5, cSegmentCount = 4, cHairCount =
  200, cRootDepth = 4;

void __fastcall TFormFurBall::CreateRandomHair()
{
  int i;
  TAffineVector Dir;
  TGLVerletHair *Hair;
  TGLLines *GLLines;

  Dir = AffineVectorMake(random() - 0.5, random() - 0.5, random() - 0.5);
  NormalizeVector(Dir);

  TGLStiffnessSetVH vhs;
  vhs << vhsSkip1Node;
  Hair =
	new TGLVerletHair(VerletWorld, FurBall->Radius * cRootDepth,
					FurBall->Radius * cRadiusMultiplier, cSegmentCount,
					VectorAdd(AffineVectorMake(FurBall->AbsolutePosition),
							  VectorScale(Dir, FurBall->Radius)), Dir, vhs);

  GLLines = (TGLLines *) (DCShadowCaster->AddNewChild(__classid(TGLLines)));
  GLLines->NodesAspect = lnaInvisible;
  GLLines->LineWidth = 2;
  GLLines->LineColor->Color = clrBlack;

  for(i = 0; i <= Hair->NodeList->Count - 1; i++)
	((TGLVerletNode *) Hair->NodeList->Items[i])->GLBaseSceneObject = FurBall;

  for(i = 1; i <= Hair->NodeList->Count - 1; i++)
	GLLines->AddNode(Hair->NodeList->Items[i]->Location);

  for(i = 0; i <= GLLines->Nodes->Count - 1; i++)
	((TGLLinesNode *) GLLines->Nodes->Items[i])->Color->Color = clrBlack;

  GLLines->ObjectStyle = GLLines->ObjectStyle << osDirectDraw;
  GLLines->SplineMode = lsmCubicSpline;

  Hair->Data = GLLines;
  HairList->Add(Hair);
}


//---------------------------------------------------------------------------

void __fastcall TFormFurBall::CreateFur()
{
  TGLVerletHair *Hair;
  int i;

  for(i = 0; i <= HairList->Count - 1; i++)
  {
	Hair = ((TGLVerletHair *) HairList->Items[i]);
	delete((TGLLines *) Hair->Data);
	delete Hair;
  }

  HairList->Clear();

  for(i = 0; i <= cHairCount - 1; i++)
	CreateRandomHair();
}

//---------------------------------------------------------------------------

void __fastcall TFormFurBall::CreateBall()
{
  TdMass m;

  dMassSetSphere(m, 1, FurBall->Radius);

  odeFurBallGeom = dCreateSphere(space, FurBall->Radius);
  odeFurBallBody = dBodyCreate(world);

  dGeomSetBody(odeFurBallGeom, odeFurBallBody);
  dBodySetMass(odeFurBallBody, &m);
  dBodySetLinearVel(odeFurBallBody, 0, 14, 0);

  dBodyAddTorque(odeFurBallBody, 0.1, 0.1, 0.1);

  // Add the GLScene object
  odeFurBallGeom->data = FurBall;

  CopyPosFromGeomToGL(odeFurBallGeom, FurBall);

  VCSphere = new TGLVerletFrictionSphere(VerletWorld);
  VCSphere->Radius = FurBall->Radius * 1.1;
  VCSphere->Location = AffineVectorMake(FurBall->AbsolutePosition);

  CreateFur();
}

//---------------------------------------------------------------------------

void __fastcall TFormFurBall::CheckBox_FurGravityClick(TObject *Sender)
{
  if(!CheckBox_FurGravity->Checked)
	delete Gravity;
  else
  {
	Gravity = new TGLVerletGravity(VerletWorld);
	Gravity->Gravity = AffineVectorMake(0, 0, -9.81);
  }
}

//---------------------------------------------------------------------------
const float cMaxWindMag = 8;
void __fastcall TFormFurBall::CheckBox_WindResistenceClick(TObject *Sender)
{
  if(!CheckBox_WindResistence->Checked)
	delete AirResistance;
  else
  {
	AirResistance = new TGLVerletAirResistance(VerletWorld);
	AirResistance->DragCoeff = 0.01;
	AirResistance->WindDirection = AffineVectorMake(1, 0, 0);
	AirResistance->WindMagnitude =
	  (float)(TrackBar_WindForce->Position) / 100.0 * cMaxWindMag;
	AirResistance->WindChaos = 0.4;
  }

  TrackBar_WindForce->Enabled = CheckBox_WindResistence->Checked;
}

//---------------------------------------------------------------------------

void __fastcall TFormFurBall::TrackBar_WindForceChange(TObject *Sender)
{
  if(AirResistance)
	AirResistance->WindMagnitude =
	  (float)(TrackBar_WindForce->Position) / 100.0 * cMaxWindMag;
}

//---------------------------------------------------------------------------

void __fastcall TFormFurBall::CheckBox_BaldClick(TObject *Sender)
{
  for(int i = 0; i <= HairList->Count - 1; i++)
  {
	TGLVerletHair *h = (TGLVerletHair *) (HairList->Items[i]);
	{
	  h->Anchor->NailedDown = !CheckBox_Bald->Checked;
	  h->Anchor->OldLocation = h->Anchor->Location;
	  h->Root->NailedDown = !CheckBox_Bald->Checked;
      h->Root->OldLocation = h->Root->Location;
    }
  }

  if(!CheckBox_Bald->Checked)
    VerletWorld->PauseInertia(5);
}
//---------------------------------------------------------------------------

void __fastcall TFormFurBall::Timer1Timer(TObject *Sender)
{
  Label_FPS->Caption = GLSceneViewer1->FramesPerSecondText(1);
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------

void __fastcall TFormFurBall::CheckBox_ShadowsClick(TObject *Sender)
{
  TGLLightSource *light;

  if(CheckBox_Shadows->Checked)
    light = GLLightSource1;
  else
    light = NULL;

  GLShadowPlane_Floor->ShadowedLight = light;
  GLShadowPlane_Floor2->ShadowedLight = light;
  GLShadowPlane_Wall->ShadowedLight = light;
  GLShadowPlane_Wall2->ShadowedLight = light;
  GLShadowPlane_Wall3->ShadowedLight = light;
}
//---------------------------------------------------------------------------

void __fastcall TFormFurBall::CheckBox_InertiaClick(TObject *Sender)
{
  VerletWorld->Inertia = CheckBox_Inertia->Checked;
}

//---------------------------------------------------------------------------

void __fastcall TFormFurBall::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
		  TPoint &MousePos, bool &Handled)
{
 GLCamera1-> AdjustDistanceToTarget(Power(1.1, WheelDelta / 120.0));
}
//---------------------------------------------------------------------------


