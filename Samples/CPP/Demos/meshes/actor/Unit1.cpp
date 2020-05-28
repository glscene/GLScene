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
#pragma link "GLObjects"
#pragma link "GLScene"
#pragma link "GLVectorFileObjects"
#pragma link "GLWin32Viewer"
#pragma link "GLFileMD2"
#pragma resource "*.dfm"
TForm1 *Form1;
int mdx, mdy;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::FormCreate(TObject *Sender)
{
   SetGLSceneMediaDir();
   // Load Actor into GLScene
   Actor1->LoadFromFile("waste.md2");
   Actor1->Material->Texture->Image->LoadFromFile("waste.jpg");

   // Load Quake2 animations defaults, for "waste.md2", this is not required
   // since the author did not renamed the frames, and thus, GLScene can
   // recover them from the .MD2, but other authors just made a mess...
   // Loading the default animations takes care of that
   Actor1->Animations->LoadFromFile("Quake2Animations.aaf");

   // Scale Actor for put in the Scene
   Actor1->Scale->SetVector(0.04, 0.04, 0.04, 0);

   // Send animation names to the combo, to allow user selection
   Actor1->Animations->SetToStrings(CBAnimations->Items);
   // Force state to stand (first in list)
   CBAnimations->ItemIndex = 0;
   CBAnimationsChange(Sender);

   // Load Texture of ground disk for Persistant Image
   // Disk1->Material->Texture->Image->LoadFromFile("clover.jpg");
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SBPlayClick(TObject *Sender)
{
   // start playing
   Actor1->AnimationMode = aamLoop;
   Actor2->AnimationMode = aamLoop;

   // update buttons
   SBPlay->Enabled = False;
   SBStop->Enabled = True;
   SBFrameToFrame->Enabled = False;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SBStopClick(TObject *Sender)
{
   // stop playing
   Actor1->AnimationMode = aamNone;
   Actor2->AnimationMode = aamNone;

   // update buttons
   SBPlay->Enabled = True;
   SBStop->Enabled = False;
   SBFrameToFrame->Enabled = True;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::CBAnimationsChange(TObject *Sender)
{
   // Change animation
   Actor1->SwitchToAnimation(CBAnimations->Text, True);

   // Normally actors for Quake II Model have one number of frames
   // for all states 198 for actors and 172 for weapon,
   // frames 173 to 198 are for death
   // I use this for Hide and show weapon.
   Actor2->Visible = (Actor1->NextFrameIndex()<173);
   if (Actor2->Visible)
	  Actor2->Synchronize(Actor1);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::SBFrameToFrameClick(TObject *Sender)
{
 // Animate Frame to Frame
 Actor1->NextFrame();
 Actor2->NextFrame();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BBLoadWeaponClick(TObject *Sender)
{
   // Load weapon model and texture
   SetGLSceneMediaDir();
   Actor2->LoadFromFile("WeaponWaste.md2");
   Actor2->Material->Texture->Image->LoadFromFile("WeaponWaste.jpg");

   // Get animations frames from the main actor
   Actor2->Animations->Assign(Actor1->Animations);

   // Synch both actors
   Actor2->Synchronize(Actor1);
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
 // store mouse coordinates when a button went down
 mdx = X; mdy = Y;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
   LabelFPS->Caption = Format("%.1f FPS",
    ARRAYOFCONST((GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   // (we're moving around the parent and target dummycube)
   if (Shift.Contains(ssLeft))
	  GLCamera1->MoveAroundTarget(mdy-Y, mdx-X);
	mdx = X; mdy = Y;
}
//---------------------------------------------------------------------------
