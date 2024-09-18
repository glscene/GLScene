//---------------------------------------------------------------------------

#include <vcl.h>
#include <tchar.h>
#pragma hdrstop

#include "fSmokingC.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "GLS.BaseClasses"
#pragma link "GLS.Cadencer"
#pragma link "GLS.Coordinates"

#pragma link "GLS.Objects"
#pragma link "GLS.ParticleFX"
#pragma link "GLS.PerlinPFX"
#pragma link "GLS.Scene"
#pragma link "GLS.SceneViewer"
#pragma resource "*.dfm"
TFormSmoking *FormSmoking;
//---------------------------------------------------------------------------
__fastcall TFormSmoking::TFormSmoking(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TFormSmoking::GLCadencerProgress(TObject *Sender, const double deltaTime,
          const double newTime)
{
  SmokePFX->Rotation = newTime;
  GLSceneViewer->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TFormSmoking::TimerTimer(TObject *Sender)
{
   /*
   Panel1->Caption = Format("%.1f FPS",
	ARRAYOFCONST((GLSceneViewer->FramesPerSecond(), SmokePFX->ParticleCount()+FlamePFX->ParticleCount(),
					 ParticleFXRenderer->LastSortTime)));
   */
   Panel1->Caption = GLSceneViewer->FramesPerSecond();
   GLSceneViewer->ResetPerformanceMonitor();
}
//---------------------------------------------------------------------------
