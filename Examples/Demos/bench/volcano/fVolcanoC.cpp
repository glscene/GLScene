//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fVolcanoC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Behaviours"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.ParticleFX"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormVolcano *FormVolcano;
//---------------------------------------------------------------------------
__fastcall TFormVolcano::TFormVolcano(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormVolcano::FormCreate(TObject *Sender)
{
   RadioGroup1Click(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TFormVolcano::RadioGroup1Click(TObject *Sender)
{
   TGLSourcePFXEffect *source;

   source = GetOrCreateSourcePFX(DCVolcano);
   switch (RadioGroup1->ItemIndex) {
	 case 0: source->ParticleInterval =0.1; break;
	 case 1: source->ParticleInterval =0.5; break;
	 case 2: source->ParticleInterval =0.02; break;
	 case 3: source->ParticleInterval =0.01; break;
	 case 4: source->ParticleInterval =0.005; break;
	 case 5: source->ParticleInterval =0.001; break;
   default:
	   ;
   }

}
//---------------------------------------------------------------------------
void __fastcall TFormVolcano::GLCadencer1Progress(TObject *Sender, const double deltaTime,
          const double newTime)
{
   GLSceneViewer1->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TFormVolcano::Timer1Timer(TObject *Sender)
{
/*
   Panel1->Caption = Format("%.1f FPS - %3d Particles - Depth Sort: %.2f msec",
   ARRAYOFCONST((GLSceneViewer1->FramesPerSecond(), PFXVolcano->Particles->ItemCount()+PFXBlue->Particles->ItemCount(),
					 PFXRenderer->LastSortTime)));
*/
   Panel1->Caption = GLSceneViewer1->FramesPerSecond();
   GLSceneViewer1->ResetPerformanceMonitor();

}
//---------------------------------------------------------------------------
