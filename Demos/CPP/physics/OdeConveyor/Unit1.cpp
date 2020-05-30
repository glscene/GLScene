//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>

#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLBaseClasses"
#pragma link "GLCadencer"
#pragma link "GLCoordinates"
#pragma link "GLCrossPlatform"
#pragma link "GLObjects"
#pragma link "GLODEManager"
#pragma link "GLScene"
#pragma link "GLSimpleNavigation"
#pragma link "GLWin32Viewer"
#pragma resource "*.dfm"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  // Initialize default values from the one of DesignTime;
  TrackBarMotionSpeed->Position = Round(GetOrCreateOdeStatic(ConveyorBelt1)->Surface->Motion1);
  Friction->Text = FloatToStr(GetOrCreateOdeStatic(ConveyorBelt1)->Surface->Mu);

  FDirX->Text = "0";
  FDirY->Text = "0";
  FDirZ->Text = "1";

}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormShow(TObject *Sender)
{
  GLCadencer1->Enabled = true;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLCadencer1Progress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
  GLODEManager1->Step(deltaTime);
}
//---------------------------------------------------------------------------

void __fastcall TForm1::GLODEManager1Collision(TObject *Sender, TObject *Object1,
		  TObject *Object2, TdContact &Contact, bool &HandleCollision)
{
  if (Object2 = GetOrCreateOdeStatic(ConveyorBelt1))
  {
	Contact.fdir1[0] = FDirectionVector.X; // x
	Contact.fdir1[1] = FDirectionVector.Y; // y
	Contact.fdir1[2] = FDirectionVector.Z; // z
	Contact.fdir1[3] = FDirectionVector.W; // not used
  }

}
//---------------------------------------------------------------------------
void __fastcall TForm1::TrackBarMotionSpeedChange(TObject *Sender)
{
  TGLODEStatic *AODEStatic;
  AODEStatic = (TGLODEStatic*)(ConveyorBelt1->Behaviours->Items[0]);
  AODEStatic->Surface->Motion1 = TrackBarMotionSpeed->Position;
}

//---------------------------------------------------------------------------

void __fastcall TForm1::FrictionChange(TObject *Sender)
{
  TGLODEStatic *AODEStatic;
  AODEStatic = (TGLODEStatic*)(ConveyorBelt1->Behaviours->Items[0]);
  AODEStatic->Surface->Mu =
  	Glutils::StrToFloatDef(Friction->Text, GetOrCreateOdeStatic(ConveyorBelt1)->Surface->Mu);
  FrictionFeedback->Caption = Format("µs = %.2f", ARRAYOFCONST((GetOrCreateOdeStatic(ConveyorBelt1)->Surface->Mu)));
}

//---------------------------------------------------------------------------

void __fastcall TForm1::AddODECubeClick(TObject *Sender)
{
  TGLCube *ACube;
  TGLODEDynamic *AODEDynamic;
  TODEElementBox *AODEElementBox;

  // Create a new GLScene cube and add it to the current GLScene1
  ACube = (TGLCube *)(SpawnPoint->AddNewChild(__classid(TGLCube)));
  ACube->Parent = GLScene1->Objects;
  ACube->Position->Assign(SpawnPoint->Position);
  ACube->Material->FrontProperties->Diffuse->RandomColor();

  // Add ODE Dynamic behaviour on it
  AODEDynamic = new TGLODEDynamic(ACube->Behaviours);
  AODEDynamic->Manager = GLODEManager1;

  // Set µs value to 1 (default=1000), just uses the one from the conveyor
  AODEDynamic->Surface->Mu = 1;

  // Finally create physical data in this behaviour
  AODEElementBox = (TODEElementBox *) AODEDynamic->AddNewElement(__classid(TODEElementBox));
  if (AODEElementBox)
  {
	AODEElementBox->BoxWidth = ACube->CubeWidth;
	AODEElementBox->BoxHeight = ACube->CubeHeight;
	AODEElementBox->BoxDepth = ACube->CubeDepth;
   }

  // The new camera target is the last added cube
  GLCamera1->TargetObject = ACube;

  // The spawn position is increased
  SpawnPoint->Position->Y = SpawnPoint->Position->Y + 1;
}
//---------------------------------------------------------------------------

