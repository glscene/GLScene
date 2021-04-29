//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Graph"
#pragma link "GLS.Objects"
#pragma link "Physics.ODEManager"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TForm1 *Form1;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormActivate(TObject *Sender)
{
  ComboBox1->ItemIndex = 0;
  ComboBox2->ItemIndex = 0;
  CheckBox2->Enabled = false;
  Label2->Enabled = false;
  TrackBar1->Enabled = false;
  ComboBox2Change(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
		  TShiftState Shift, int X, int Y)
{
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
		  int X, int Y)
{
  if (Shift.Contains(ssLeft))
	GLCamera1->MoveAroundTarget(my-Y,mx-X);
  mx = X;
  my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::SpawnClick(TObject *Sender)
{
  switch (ComboBox1->ItemIndex)
  {
    case 0 : DoSphere(); break;
    case 1 : DoBox(); break;
	case 2 : DoCapsule(); break;
	case 3 : DoCylinder(); break;
// CONE IS CURRENTLY UNSUPPOETED FOR ODE 0.9
// case 4 : DoCone(); break;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
 GLODEManager1->Step(deltaTime);
}
//---------------------------------------------------------------------------


void TForm1::DoSphere(void)
{
  TGLSphere *sphere;
  TGLODEDynamic *dyn;

  sphere = (TGLSphere *)(ODEObjects->AddNewChild(__classid(TGLSphere)));
  sphere->Position->SetPoint(5*Random()-2.5,2,5*Random()-2.5);
  sphere->Radius = 0.3*(Random()+1);
  sphere->Material->FrontProperties->Ambient->Color = clrRed;
  dyn = new TGLODEDynamic(sphere->Behaviours);
  dyn->Manager = GLODEManager1;
  ((TGLODEElementSphere *)(dyn->AddNewElement(__classid(TGLODEElementSphere))))->Radius = sphere->Radius;
}

void TForm1::DoBox(void)
{
  TGLCube *cube;
  TGLODEDynamic *dyn;
  TGLODEElementBox *elem;

  cube = (TGLCube *)(ODEObjects->AddNewChild(__classid(TGLCube)));
  cube->Position->SetPoint(5*Random()-2.5,2,5*Random()-2.5);
  cube->CubeWidth = 0.5*(Random()+1);
  cube->CubeHeight = 0.5*(Random()+1);
  cube->CubeDepth = 0.5*(Random()+1);
  cube->Material->FrontProperties->Ambient->Color = clrGreen;
  dyn = new TGLODEDynamic(cube->Behaviours);
  dyn->Manager = GLODEManager1;
  elem = (TGLODEElementBox *) dyn->AddNewElement(__classid(TGLODEElementBox));
  elem->BoxWidth = cube->CubeWidth;
  elem->BoxHeight = cube->CubeHeight;
  elem->BoxDepth = cube->CubeDepth;
}

void TForm1::DoCapsule(void)
{
  TGLCylinder *capsule;
  TGLODEDynamic *dyn;
  TGLSphere *sphere;
  TGLODEElementCapsule *elem;

  capsule = (TGLCylinder *)(ODEObjects->AddNewChild(__classid(TGLCylinder)));
  capsule->Position->SetPoint(5*Random()-2.5,2,5*Random()-2.5);
  capsule->BottomRadius = 0.25*(Random()+1);
  capsule->TopRadius = capsule->BottomRadius;
  capsule->Height = Random()+1;
  capsule->Parts << cySides;
  capsule->Material->FrontProperties->Ambient->Color = clrBlue;

  sphere = (TGLSphere *) capsule->AddNewChild(__classid(TGLSphere));
  sphere->Position->Y = 0.5*capsule->Height;
  sphere->Radius = capsule->BottomRadius;
  sphere->Bottom = 0;

  sphere = (TGLSphere *) capsule->AddNewChild(__classid(TGLSphere));
  sphere->Position->Y = -0.5*capsule->Height;
  sphere->Radius = capsule->BottomRadius;
  sphere->Top = 0;

  dyn = new TGLODEDynamic(capsule->Behaviours);
  dyn->Manager = GLODEManager1;
  elem = (TGLODEElementCapsule *) dyn->AddNewElement(__classid(TGLODEElementCapsule));
  elem->Radius = capsule->BottomRadius;
  elem->Length = capsule->Height;
  elem->Direction->SetVector(0,1,0);
  elem->Up->SetVector(0,0,1);
}

void TForm1::DoCylinder(void)
{
  TGLCylinder *cylinder;
  TGLODEDynamic *dyn;
  TGLODEElementCylinder *elem;

  cylinder = (TGLCylinder *)(ODEObjects->AddNewChild(__classid(TGLCylinder)));
  cylinder->Position->SetPoint(5*Random()-2.5,2,5*Random()-2.5);
  cylinder->BottomRadius = 0.25*(Random()+1);
  cylinder->TopRadius = cylinder->BottomRadius;
  cylinder->Height = Random()+1;
  cylinder->Material->FrontProperties->Ambient->Color = clrYellow;

  dyn = new TGLODEDynamic(cylinder->Behaviours);
  dyn->Manager = GLODEManager1;
  elem = (TGLODEElementCylinder *) dyn->AddNewElement(__classid(TGLODEElementCylinder));
  elem->Radius = cylinder->BottomRadius;
  elem->Length = cylinder->Height;
}


// CONE IS CURRENTLY UNSUPPOETED FOR ODE 0.9
//
/*
void TForm1::DoCone(void)
{
  TGLCone *cone;
  TGLODEDynamic *dyn;
  TODEElementCone *elem;

  cone = (TGLCone *)(ODEObjects->AddNewChild(__classid(TGLCone)));
  cone->Position->SetPoint(5*Random()-2.5,2,5*Random()-2.5);
  cone->BottomRadius = 0.25*(Random()+1);
  cone->Height = Random()+1;

  dyn = new TGLODEDynamic(cone->Behaviours);
  dyn->Manager = GLODEManager1;
  elem = (TODEElementCone *) dyn->AddNewElement(__classid(TODEElementCone));
  elem->Radius = cone->BottomRadius;
  elem->Length = cone->Height;
  elem->Direction->SetVector(0,1,0);
  elem->Up->SetVector(0,0,1);
  elem->Position->SetPoint(0,-cone->Height/2,0);
}
*/

//---------------------------------------------------------------------------


void __fastcall TForm1::GLHeightField1GetHeight(const float x, const float y, float &z,
          TVector4f &color, TTexPoint &texPoint)
{
  z = 0.5*cos(x)*sin(y);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox1Click(TObject *Sender)
{
  GLODEManager1->Visible = CheckBox1->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::CheckBox2Click(TObject *Sender)
{
 ((TGLODEHeightField *)(GLHeightField1->Behaviours->Behaviour[0]))->RenderContacts = CheckBox2->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ComboBox2Change(TObject *Sender)
{
  if (ComboBox2->ItemIndex == 0)
  {
	GLPlane1->Visible = true;
	{
	GetOdeStatic(GLPlane1)->Manager = GLODEManager1;
	CheckBox2->Enabled = false;
	Label2->Enabled = false;
	TrackBar1->Enabled = false;
	}
  } else {
	GLPlane1->Visible = false;
	CheckBox2->Enabled = true;
	Label2->Enabled = true;
	TrackBar1->Enabled = true;
	GetOdeStatic(GLPlane1)->Manager = NULL;

  }
  if (ComboBox2->ItemIndex == 1)
  {
	GLHeightField1->Visible = true;
	CheckBox2->Enabled = true;
	Label2->Enabled = true;
	TrackBar1->Enabled = true;
	GetODEHeightField(GLHeightField1)->Manager = GLODEManager1;
  } else {
	GLHeightField1->Visible = false;
	CheckBox2->Enabled = false;
	Label2->Enabled = false;
	TrackBar1->Enabled = false;
	GetODEHeightField(GLHeightField1)->Manager = NULL;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::TrackBar1Change(TObject *Sender)
{
  ((TGLODEHeightField *)(GLHeightField1->Behaviours->Behaviour[0]))->ContactResolution =
		0.25+(float)(10-TrackBar1->Position)/20;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
 GLCamera1-> AdjustDistanceToTarget(Power(1.1, WheelDelta / 120.0));
}
//---------------------------------------------------------------------------

