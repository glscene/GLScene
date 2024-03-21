//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "fSkeletalC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Graph"
#pragma link "GLS.Material"
#pragma link "GLS.Objects"
#pragma link "GLS.Scene"
#pragma link "GLS.VectorFileObjects"
#pragma link "GLS.SceneViewer"
#pragma link "GLS.FileSMD"

#pragma resource "*.dfm"
TFormSkeletal *FormSkeletal;
//---------------------------------------------------------------------------
__fastcall TFormSkeletal::TFormSkeletal(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::FormCreate(TObject *Sender)
{
   TFileName Path = GetCurrentAssetPath();
	SetCurrentDir(Path + "\\modelext");
   // We load the SMD model here
   // Note the actor was linked to a material library, and textures are loaded
   // automatically (4 textures are used by this model)
   //
   // Kind thanks to ~A.u.s.t.i.n. & Neal 'Guplik' Corbett for the model
   // and allowing its use ;)
   Actor1->LoadFromFile("trinityRage.smd");
   // Now we load the walk & run animations and "fix" their translation
   // (HL walk/run animations have a built-in "slide" that we don't want here)
   Actor1->AddDataFromFile("walk.smd");
   Actor1->Animations->Items[1]->MakeSkeletalTranslationStatic();
   Actor1->AddDataFromFile("run.smd");
   Actor1->Animations->Items[2]->MakeSkeletalTranslationStatic();
   // Then load the two jumps
   Actor1->AddDataFromFile("long_jump.smd");
   Actor1->AddDataFromFile("jump.smd");
   // And the 'look_left_right' blending animations, that we immediately
   // assign to the controler. The MakeSkeletalRotationDelta removes absolute
   // information from the SMD (which HL may use, but GLScene doesn't)
   Actor1->AddDataFromFile("look_left_right.smd");
   Actor1->Animations->Items[5]->MakeSkeletalRotationDelta();
   AnimationControler1->AnimationName = "look_left_right";
   // Skeleton visible, and start with walk animation
   // (pseudo-animation 0 is for the static model in its default attitude)
   Actor1->OverlaySkeleton = true;
   baseAnimation = "walk";
   Actor1->SwitchToAnimation(baseAnimation);

}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::FormMouseWheel(TObject *Sender, TShiftState Shift, int WheelDelta,
          TPoint &MousePos, bool &Handled)
{
  GLCamera1 = GLSceneViewer1->Camera;
  GLCamera1->AdjustDistanceToTarget(Power(1.1, WheelDelta / 120));
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::RBWalkClick(TObject *Sender)
{
   // user requested 'walk'
   baseAnimation = "walk";
   Actor1->SwitchToAnimation(baseAnimation, true);
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::RBRunClick(TObject *Sender)
{
   // user requested 'run'
   baseAnimation = "run";
   Actor1->SwitchToAnimation(baseAnimation, true);
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::BULongJumpClick(TObject *Sender)
{
   // Smoothly switch to Long Jump
   Actor1->SwitchToAnimation(3, true);
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::BUHighJumpClick(TObject *Sender)
{
   // Smoothly switch to High Jump
   Actor1->SwitchToAnimation(4, true);
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::Actor1EndFrameReached(TObject *Sender)
{
   // If we weren't walking, switch back to walk
   if (Actor1->CurrentAnimation() != baseAnimation)
	  Actor1->SwitchToAnimation(baseAnimation, true);
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::CBBlendClick(TObject *Sender)
{
   // Enable/disable blending by binding or unbinding the animation controler
   // to the actor
   if (CBBlend->Checked)
   {
	  AnimationControler1->Actor = Actor1;
	  TrackBar1Change(this);
   }
   else
	 AnimationControler1->Actor = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::TrackBar1Change(TObject *Sender)
{
   // Blending along the controler's animation is just a matter of adjusting
   // the ratio, with 0 = first frame and 1 = last frame.
   AnimationControler1->Ratio = TrackBar1->Position*0.01;
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::CheckBox1Click(TObject *Sender)
{
   Actor1->OverlaySkeleton = CheckBox1->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TFormSkeletal::GLSceneViewer1MouseDown(TObject *Sender, TMouseButton Button,
          TShiftState Shift, int X, int Y)
{
   mx = X; my = Y;
}
//---------------------------------------------------------------------------

void __fastcall TFormSkeletal::GLSceneViewer1MouseMove(TObject *Sender, TShiftState Shift,
          int X, int Y)
{
   if (Shift.Contains(ssLeft) || Shift.Contains(ssRight))
   {
	  GLCamera1->MoveAroundTarget(my-Y, mx-X);
	  mx = X; my = Y;
   }
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::Timer1Timer(TObject *Sender)
{
   LabelFPS->Caption = Format("%.1f FPS",
	 ARRAYOFCONST ((GLSceneViewer1->FramesPerSecond())));
   GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TFormSkeletal::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  GLScene1->NotifyChange(NULL);
}
//---------------------------------------------------------------------------
