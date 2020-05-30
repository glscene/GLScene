//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLGeomObjects"
#pragma link "GLNavigator"
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLSkydome"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFile3DS"
#pragma link "GLFileMD2"

#pragma resource "*.dfm"
TForm1 *Form1;

const int
  cWalkStep = 6;   // this is our walking speed, in 3D units / second
const int
  cStrafeStep = 6; // this is our strafing speed, in 3D units / second
const int
  cRotAngle = 60;  // this is our turning speed, in degrees / second
const int
  cRunBoost = 2;   // speed boost when running
const int
  cSpread = 90;
const int
  cNbMushrooms = 15;

//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TForm1::AddMushrooms()
{
   int i;
   TGLProxyObject *proxy;
   Glvectorgeometry::TVector s;
   float f;

   // spawn some more mushrooms using proxy objects
   for (i=0; i < cNbMushrooms-1; i++)
   {
	// create a new proxy and set its MasterObject property
	proxy = (TGLProxyObject *)(DummyCube1->AddNewChild(__classid(TGLProxyObject)));

	proxy->ProxyOptions = proxy->ProxyOptions >> pooObjects;
	proxy->MasterObject = FreeForm1;
	 // retrieve reference attitude
	proxy->Direction = FreeForm1->Direction;
	proxy->Up = FreeForm1->Up;
	// randomize scale
	s = FreeForm1->Scale->AsVector;
	f = (1*Random()+1);
	ScaleVector(s, f);
	proxy->Scale->AsVector = s;
	// randomize position
	proxy->Position->SetPoint(Random(cSpread)-(cSpread/2.0),
				   FreeForm1->Position->Z+0.8*f,
				   Random(cSpread)-(cSpread/2.0));
	// randomize orientation
	proxy->RollAngle = Random(360.0);
	proxy->TransformationChanged();
	}
}

//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   SetGLSceneMediaDir();
   // Load mushroom mesh
   FreeForm1->LoadFromFile("mushroom.3ds");

   // Duplicate our reference mushroom (but not its mesh data !)
   AddMushrooms();

   // Load Actor into GLScene
   Actor1->LoadFromFile("waste.md2");
   Actor1->Material->Texture->Image->LoadFromFile("waste.jpg");
   Actor1->Animations->LoadFromFile("Quake2Animations.aaf");
   Actor1->Scale->SetVector(0.04, 0.04, 0.04, 0);
   // Load weapon model and texture
   Actor2->LoadFromFile("WeaponWaste.md2");
   Actor2->Material->Texture->Image->LoadFromFile("WeaponWaste.jpg");
   Actor2->Animations->Assign(Actor1->Animations);

   // Define animation properties
   Actor1->AnimationMode = aamLoop;
   Actor1->SwitchToAnimation("stand");
   Actor1->FrameInterpolation = afpLinear;
   Actor2->Synchronize(Actor1);

   // Load Texture for ground disk
   Disk1->Material->Texture->Image->LoadFromFile("clover.jpg");


   // F7 Third person
	GLSceneViewer1->Camera = GLCamera1;
	Actor1->Visible = true;
	Label4->Font->Style = Label4->Font->Style >> fsBold;
	Label3->Font->Style = Label3->Font->Style << fsBold;

}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBMouseLookClick(TObject *Sender)
{
   GLUserInterface1->MouseLookActive = CBMouseLook->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::HandleKeys(const double deltaTime)
{
   String moving;
   float boost;

   // This function uses asynchronous keyboard check (see Keyboard.pas)
   if (IsKeyDown(VK_ESCAPE)) Close();
   if (IsKeyDown('A'))
   {
	  CBMouseLook->Checked = true;
	  CBMouseLookClick(CBMouseLook);
   }
   if (IsKeyDown('D'))
   {
	  CBMouseLook->Checked = false;
	  CBMouseLookClick(CBMouseLook);
   }

   //Change Cameras
   if (IsKeyDown(VK_F7))
   {
	  GLSceneViewer1->Camera = GLCamera1;
	  Actor1->Visible = true;
	  Label4->Font->Style = Label4->Font->Style >> fsBold;
	  Label3->Font->Style = Label3->Font->Style << fsBold;
   }
   if (IsKeyDown(VK_F8))
   {
	  GLSceneViewer1->Camera = GLCamera2;
	  Actor1->Visible = false;
	  Label4->Font->Style = Label4->Font->Style << fsBold;
	  Label3->Font->Style = Label3->Font->Style >> fsBold;

   }
   // Move Actor in the scene

   // if nothing specified, we are standing
   moving = "stand";

   // first, are we running ? if yes give animation & speed a boost
   if (IsKeyDown(VK_SHIFT))
   {
	  Actor1->Interval = 100;
	  boost = cRunBoost*deltaTime;
   }
   else
   {
	  Actor1->Interval = 150;
	  boost = deltaTime;
   }
   Actor2->Interval = Actor1->Interval;

   // are we advaning/backpedaling ?
   if (IsKeyDown(VK_UP))
   {
	  GLNavigator1->MoveForward(cWalkStep*boost);
	  moving = "run";
   }
   if (IsKeyDown(VK_DOWN))
   {
	  GLNavigator1->MoveForward(-cWalkStep*boost);
	  moving = "run";
   }

   // slightly more complex, depending on CTRL key, we either turn or strafe
   if (IsKeyDown(VK_LEFT))
   {
	  if (IsKeyDown(VK_CONTROL))
		  GLNavigator1->StrafeHorizontal(-cStrafeStep*boost);
	  else
		  GLNavigator1->TurnHorizontal(-cRotAngle*boost);
	  moving = "run";
   }
   if (IsKeyDown(VK_RIGHT))
   {
	  if (IsKeyDown(VK_CONTROL))
		  GLNavigator1->StrafeHorizontal(cStrafeStep*boost);
	  else
	      GLNavigator1->TurnHorizontal(cRotAngle*boost);
	  moving = "run";
   }
   // update animation (if required)
   // you can use faster methods (such as storing the last value of "moving")
   // but this ones shows off the brand new "CurrentAnimation" function :)
   if (Actor1->CurrentAnimation() != moving)
   {
	  Actor1->SwitchToAnimation(moving);
	  Actor2->Synchronize(Actor1);
   }
}
void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
   HandleKeys(deltaTime);
   GLUserInterface1->MouseLook();

   GLSceneViewer1->Invalidate();
   GLUserInterface1->MouseUpdate();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   Caption = "Actor with Two Cameras " + Format("%.2f FPS",
	 ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();

}
//---------------------------------------------------------------------------
