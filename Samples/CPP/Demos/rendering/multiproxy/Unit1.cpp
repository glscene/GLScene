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
#pragma link "GLMultiProxy"
#pragma link "GLObjects"
#pragma link "GLParticles"
#pragma link "GLScene"
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
   int i;
   // adjust settings to their default
   RBUseLODsClick(Sender); //MPSphere
   // replicate the multiproxy via a TGLParticles object
   for (i = 0; i < 35; i++) {
	  GLParticles->CreateParticle()->TagFloat = DegToRad((float)i*10);
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  LabelFPS->Caption = Format("%.1f FPS", ARRAYOFCONST((GLSceneViewer1->FramesPerSecond())));
  GLSceneViewer1->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::RBUseLODsClick(TObject *Sender)
{
   // adjust LOD on/off (by adjusting base sphere's detail)
   if (RBUseLODs->Checked)  {
	  SPHighRes->Slices = 32;
	  SPHighRes->Stacks = 32;
	  SPMedRes->Slices = 16;
	  SPMedRes->Stacks = 16;
	  SPLowRes->Slices = 8;
	  SPLowRes->Stacks = 8;
   }
   else
   if (RBHighRes->Checked)
   {
	  SPHighRes->Slices = 32;
	  SPHighRes->Stacks = 32;
	  SPMedRes->Slices = 32;
	  SPMedRes->Stacks = 32;
	  SPLowRes->Slices = 32;
	  SPLowRes->Stacks = 32;
   }
   else
   if (RBLowRes->Checked)
   {
	  SPHighRes->Slices = 8;
	  SPHighRes->Stacks = 8;
	  SPMedRes->Slices = 8;
	  SPMedRes->Stacks = 8;
	  SPLowRes->Slices = 8;
	  SPLowRes->Stacks = 8;
   }
   // colorize the LODs, to make them clearly visible
   CBColorize->Enabled = RBUseLODs->Checked;
   if (CBColorize->Checked && RBUseLODs->Checked)
   {
	  SPHighRes->Material->FrontProperties->Diffuse->Color = clrRed;
	  SPMedRes->Material->FrontProperties->Diffuse->Color = clrBlue;
	  SPLowRes->Material->FrontProperties->Diffuse->Color = clrYellow;
   }
   else
   {
	  SPHighRes->Material->FrontProperties->Diffuse->Color = clrGray80;
	  SPMedRes->Material->FrontProperties->Diffuse->Color = clrGray80;
	  SPLowRes->Material->FrontProperties->Diffuse->Color = clrGray80;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::MPSphereProgress(TObject *Sender, const double deltaTime,
		  const double newTime)
{
   // this is invoked for each of our MultiProxys, it makes them loop an ellipse
	  GLParticles->Position->X = Sin(newTime+GLParticles->TagFloat)*80-60;
	  GLParticles->Position->Z = Cos(newTime+GLParticles->TagFloat)*7;
}
//---------------------------------------------------------------------------

